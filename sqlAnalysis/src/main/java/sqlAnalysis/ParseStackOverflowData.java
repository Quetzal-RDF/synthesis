package sqlAnalysis;

import java.io.StringReader;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Collections;

import com.facebook.presto.sql.parser.SqlParser;
import com.ibm.wala.classLoader.ClassLoaderFactory;
import com.ibm.wala.classLoader.SourceModule;
import com.ibm.wala.ipa.callgraph.AnalysisScope;
import com.ibm.wala.ipa.cha.ClassHierarchyException;
import com.ibm.wala.ipa.cha.IClassHierarchy;
import com.ibm.wala.ipa.cha.SeqClassHierarchyFactory;

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
		int prestoPasses = 0;
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
					prestoPasses = doPresto(prestoPasses, code, loaders);
				} catch (Throwable e) {
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
		System.out.println("presto passes:" + prestoPasses);

	}

	private static int doPresto(int prestoPasses, String code, ClassLoaderFactory loaders)
			throws ClassHierarchyException {
		SourceModule M = new SQLSourceModule(code);
		AnalysisScope scope = new AnalysisScope(Collections.singleton(SQL.sql)) {
			{
				loadersByName.put(SQLClassLoaderFactory.Sql.getName(), SQLClassLoaderFactory.Sql);
			}
		};
		scope.addToScope(SQLClassLoaderFactory.Sql, M);
		
		IClassHierarchy cha = SeqClassHierarchyFactory.make(scope, loaders);
		
		System.err.println(cha);
		
		prestoPasses++;
		return prestoPasses;
	}
}