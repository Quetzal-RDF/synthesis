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
		for (String line : lines) {
			if (line.startsWith("SELECT") || line.startsWith("select")) {
				
				ClassLoaderFactory loaders = new SQLClassLoaderFactory();

				// System.out.println(buf.toString());
				try {
					SQLToGraph.doPresto(buf.toString(), loaders);
				} catch (Throwable e) {
					e.printStackTrace();
				}
				i++;
				buf = new StringBuffer();
				buf.append(line).append("\n");
			} else {
				buf.append(line).append("\n");
			}
		}
		System.out.println("num of statements:" + i);
		
		
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
