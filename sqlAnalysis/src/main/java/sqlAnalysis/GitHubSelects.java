package sqlAnalysis;

import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.List;
import java.util.Map;

import com.ibm.wala.classLoader.ClassLoaderFactory;

public class GitHubSelects {
	
	public static void main(String[] args) throws Exception {
		List<String> lines = Files.readAllLines(Paths.get(args[0]));
		StringBuffer buf = new StringBuffer();
		int i = 0;
		int numParses = 0;
		int debug = 0;
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
				
				ClassLoaderFactory loaders = new SQLClassLoaderFactory();
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
						SQLToGraph.doPresto(sql, loaders);
						numParses++;
					} catch (Throwable e) {
						System.out.println("SQL that does not parse:" + sql);
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
		System.out.println("num of statements:" + i);
		System.out.println("num of statements parsed:" + numParses);
		
		
		System.out.println("dataflow graph" + SQLToGraph.dataflowGraph);
		System.out.println("dataflow edgecount" + SQLToGraph.dataflowEdgeCount);
		System.out.println("control dependence graph" + SQLToGraph.controlflowGraph);
		System.out.println("control dependence edgecount" + SQLToGraph.controlflowEdgeCount);
		System.out.println("DataFlow:");
		SQLToGraph.printEdgeCounts(SQLToGraph.dataflowEdgeCount);
		System.out.println("ControlFlow:");
		SQLToGraph.printEdgeCounts(SQLToGraph.controlflowEdgeCount);

	}


}
