package sqlAnalysis;

import java.io.StringReader;
import java.nio.file.Files;
import java.nio.file.Paths;

import com.facebook.presto.sql.parser.SqlParser;
import com.ibm.wala.classLoader.ClassLoaderFactory;

import net.sf.jsqlparser.parser.CCJSqlParserManager;

public class ParseStackOverflowData {
	static final String codeStart = "<code>";
	static final String codeEnd = "</code>";
	static final SqlParser SQL_PARSER = new SqlParser();
	
	public static void main(String[] args) throws Exception {
		String content = new String(
				Files.readAllBytes(Paths.get("../embeddingData/stackOverflowBody.doc")));
				
		
		int start = content.indexOf(codeStart);
		int end = content.indexOf(codeEnd);
		int count = 0;
		int failures = 0;
		
		while (start != -1 && end != -1 && start < end) {
			String code = content.substring(start + codeStart.length(), end);
			
			ClassLoaderFactory loaders = new SQLClassLoaderFactory();
			
			
			if (code.startsWith("select") || code.startsWith("SELECT")) {
				count++;
				code = code.replaceAll("&gt;", ">");
				code = code.replaceAll("&lt;", "<");
				code = code.replaceAll("\\[", "");
				code = code.replaceAll("]", "");
				code = code.replaceAll("[;#@]", "");
				try {
					SQLToGraph.doPresto(code, loaders);
				} catch (Throwable e) {
					e.printStackTrace();
					try {
						CCJSqlParserManager manager = new CCJSqlParserManager();
						net.sf.jsqlparser.statement.Statement st = manager.parse(new StringReader(code));
						JSQLVisitor.process(st, code);
					} catch (Throwable e2) {
						failures++;
					}
				}
			}
			content = content.substring(end + codeEnd.length());
			start = content.indexOf(codeStart);
			end = content.indexOf(codeEnd);
		}
		System.out.println("found:" + count + " selects" + " with successes:" + (count - failures));
		
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
