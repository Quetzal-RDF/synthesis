package sqlAnalysis;

import java.io.StringReader;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Collections;
import java.util.Set;

import com.facebook.presto.sql.parser.SqlParser;
import com.ibm.wala.cast.ir.ssa.AstIRFactory;
import com.ibm.wala.classLoader.ClassLoaderFactory;
import com.ibm.wala.classLoader.IClass;
import com.ibm.wala.classLoader.IMethod;
import com.ibm.wala.classLoader.SourceModule;
import com.ibm.wala.ipa.callgraph.AnalysisScope;
import com.ibm.wala.ipa.callgraph.impl.Everywhere;
import com.ibm.wala.ipa.cha.ClassHierarchyException;
import com.ibm.wala.ipa.cha.IClassHierarchy;
import com.ibm.wala.ipa.cha.SeqClassHierarchyFactory;
import com.ibm.wala.ssa.DefUse;
import com.ibm.wala.ssa.IR;
import com.ibm.wala.ssa.IRFactory;
import com.ibm.wala.ssa.SSAInstruction;
import com.ibm.wala.ssa.SSAInvokeInstruction;
import com.ibm.wala.ssa.SSAOptions;
import com.ibm.wala.ssa.SSAPhiInstruction;
import com.ibm.wala.util.collections.HashSetFactory;

import net.sf.jsqlparser.parser.CCJSqlParserManager;

public class ParseStackOverflowData {
	static final String codeStart = "<code>";
	static final String codeEnd = "</code>";
	static final SqlParser SQL_PARSER = new SqlParser();

	public static void main(String[] args) throws Exception {
		String content = new String(
				Files.readAllBytes(Paths.get("../embeddingData/stackOverflowBody.doc")));
//		String content = new String(
//				Files.readAllBytes(Paths.get("/tmp/test")));

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
		System.out.println("presto passes:" + prestoPasses);

	}

	public static int doPresto(int prestoPasses, String code, ClassLoaderFactory loaders)
			throws ClassHierarchyException {
		
		SourceModule M = new SQLSourceModule(code);
		AnalysisScope scope = new AnalysisScope(Collections.singleton(SQL.sql)) {
			{
				loadersByName.put(SQLClassLoaderFactory.Sql.getName(), SQLClassLoaderFactory.Sql);
			}
		};
		scope.addToScope(SQLClassLoaderFactory.Sql, M);
		
		IClassHierarchy cha = SeqClassHierarchyFactory.make(scope, loaders);

		IRFactory<IMethod> irs = new AstIRFactory<IMethod>();
		for(IClass c : cha) {
			for(IMethod f : c.getDeclaredMethods()) {
				IR ir = irs.makeIR(f, Everywhere.EVERYWHERE, SSAOptions.defaultOptions());
				DefUse du = new DefUse(ir);
				for(int i = 0; i <= ir.getSymbolTable().getMaxValueNumber(); i++) {
					SSAInstruction d = du.getDef(i);
					if (d instanceof SSAInvokeInstruction) {
						Set<SSAInstruction> uses = HashSetFactory.make();
						du.getUses(i).forEachRemaining((SSAInstruction x) -> { uses.add(x); });
						while (! uses.isEmpty()) {
							SSAInstruction use = uses.iterator().next();
							uses.remove(use);
							if (use instanceof SSAInvokeInstruction) {
								System.err.println(
									((SSAInvokeInstruction) d).getCallSite().getDeclaredTarget().getName() + 
									" --> " +
									((SSAInvokeInstruction) use).getCallSite().getDeclaredTarget().getName());
							} else if (use instanceof SSAPhiInstruction) {
								du.getUses(use.getDef()).forEachRemaining((SSAInstruction x) -> { 
									uses.add(x);
								});
							}
						}
					}
				}
				System.out.println(ir);
			}
		}
		
		prestoPasses++;
		return prestoPasses;
	}
}
