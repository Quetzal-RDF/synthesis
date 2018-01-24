package sqlAnalysis;

import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.facebook.presto.sql.parser.SqlParser;
import com.facebook.presto.sql.tree.Statement;
import com.ibm.wala.util.collections.Pair;

public class GitHubGetExpressions {
	
	public static void main(String[] args) throws Exception {
		List<String> lines = Files.readAllLines(Paths.get(args[0]));
		StringBuffer buf = new StringBuffer();
		int i = 0;
		int numParses = 0;

		Map<String, String> expressions = new HashMap<String, String>();
		Map<String, Integer> functions = new HashMap<String, Integer>();
		Map<String, Integer> opsToCounts = new HashMap<String, Integer>();
		
		for (String line : lines) {
			line = line.trim();
			if (line.startsWith("SELECT") || line.startsWith("select") || line.startsWith("INSERT") || line.startsWith("insert")
					|| line.startsWith("UPDATE") || line.startsWith("update")
					|| line.startsWith("CREATE") || line.startsWith("create")
					|| line.startsWith("ALTER") || line.startsWith("alter")
					|| line.startsWith("WITH") || line.startsWith("with")
					|| line.startsWith("DELETE") || line.startsWith("delete")
					|| line.startsWith("--")
					) {
				
				String sql =  buf.toString().replace(';',' ');
				sql = sql.replaceAll("\n", " ");
				sql = sql.replace("`", "\"");
				sql = sql.replace("?", "42");
				sql = sql.replace(":", "");
				sql = sql.replace("[", "");
				sql = sql.replace("]", "");
				
				if (sql.startsWith("SELECT") || sql.startsWith("select")) {
					i++;				
					try {
												
						SExpressionWriter writer = new SExpressionWriter(i);
						SqlParser parser = new SqlParser();
						Statement st = parser.createStatement(sql);
						writer.process(st, null);
						
						List<Pair<String, String>> exps = writer.getExpressionsAndTypes();
//						System.out.println("GRAPH:" + writer.graph);
						
						for (Pair<String, String> p : exps) {
							if (!expressions.containsKey(p.fst)) {
							//	System.out.println("SQL:" + sql);
							//	System.out.println("Expression:" + p.fst + " " + p.snd);
								expressions.put(p.fst, p.snd);
							}
						}
						for (String f : writer.functions) {
							if (!functions.containsKey(f)) {
								functions.put(f, 1);
							} else {
								int j = functions.get(f) + 1;
								functions.put(f, j);
							}
						}
						
						for (Map.Entry<String, Integer> e : writer.getOpsToCounts().entrySet()) {
							if (!opsToCounts.containsKey(e.getKey())) {
								opsToCounts.put(e.getKey(), e.getValue());
							} else {
								int j = opsToCounts.get(e.getKey()) + e.getValue();
								opsToCounts.put(e.getKey(), j);
							}
						}
						numParses++;
					} catch (Throwable e) {
						// e.printStackTrace();
						// System.out.println("SQL that does not parse:" + sql);
						// debug++;
						// e.printStackTrace();
					}
				}
				buf = new StringBuffer();
				buf.append(line).append("\n");

			} else {
				buf.append(line).append("\n");
			}
		}
		System.out.println("num of expressions:" + expressions.size());
		System.out.println("num of statements:" + i);
		System.out.println("num of statements parsed:" + numParses);
		
		System.out.println("Functions");
		
		functions.entrySet().stream()
        .sorted(Map.Entry.<String, Integer>comparingByValue().reversed()) 
        .forEach(System.out::println); 

		System.out.println("Expressions");
		expressions.forEach((k, v) -> System.out.println("(" + k + " " + v + ")")); 
		
		System.out.println("OpsToCounts");
		opsToCounts.entrySet().stream()
        .sorted(Map.Entry.<String, Integer>comparingByValue().reversed()) 
        .forEach(System.out::println); 
	}

}
