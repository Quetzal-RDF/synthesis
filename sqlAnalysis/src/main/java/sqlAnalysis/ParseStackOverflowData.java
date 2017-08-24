package sqlAnalysis;

import java.io.StringReader;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import java.util.stream.Stream;

import com.facebook.presto.sql.parser.SqlParser;
import com.ibm.wala.cast.ir.ssa.AstIRFactory;
import com.ibm.wala.cfg.cdg.ControlDependenceGraph;
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
import com.ibm.wala.ssa.ISSABasicBlock;
import com.ibm.wala.ssa.SSABinaryOpInstruction;
import com.ibm.wala.ssa.SSAGetInstruction;
import com.ibm.wala.ssa.SSAInstruction;
import com.ibm.wala.ssa.SSAInvokeInstruction;
import com.ibm.wala.ssa.SSAOptions;
import com.ibm.wala.ssa.SSAPhiInstruction;
import com.ibm.wala.util.collections.HashSetFactory;
import com.ibm.wala.util.graph.Graph;
import com.ibm.wala.util.graph.impl.SlowSparseNumberedGraph;

import net.sf.jsqlparser.parser.CCJSqlParserManager;

public class ParseStackOverflowData {
	static final String codeStart = "<code>";
	static final String codeEnd = "</code>";
	static final SqlParser SQL_PARSER = new SqlParser();
	static SlowSparseNumberedGraph<String> dataflowGraph = new SlowSparseNumberedGraph<String>(1);
	static HashMap<String, Integer> dataflowEdgeCount = new HashMap<String, Integer>();
	static SlowSparseNumberedGraph<String> controlflowGraph = new SlowSparseNumberedGraph<String>(1);
	static HashMap<String, Integer> controlflowEdgeCount = new HashMap<String, Integer>();
	static Map<String, String> srcToTargetFunction  = new HashMap<String, String>();
	
	public static void main(String[] args) throws Exception {
		String content = new String(
				Files.readAllBytes(Paths.get("../embeddingData/stackOverflowBody.doc")));
		
		Stream<String> mappings = Files.lines(Paths.get("functions"));
		
		mappings.forEach(name -> { 
			System.out.println(name);
			String[] arr = name.split("[|]");
			assert arr.length == 2;
			srcToTargetFunction.put(arr[0], arr[1]);
		});
		mappings.close();
		
		
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
		
		System.out.println("dataflow graph" + dataflowGraph);
		System.out.println("dataflow edgecount" + dataflowEdgeCount);
		System.out.println("control dependence graph" + controlflowGraph);
		System.out.println("control dependence edgecount" + controlflowEdgeCount);
		Iterator<String> it = dataflowGraph.getNodeManager().iterator();
		while (it.hasNext()) {
			System.out.println(it.next());
		}

	}
	
	public static String getFunction(String src) {
		if (srcToTargetFunction.containsKey(src)) {
			return srcToTargetFunction.get(src);
		} else {
			return src;
		}
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
				
				ControlDependenceGraph<ISSABasicBlock> cdg = new ControlDependenceGraph<>(ir.getControlFlowGraph());
				for(SSAInstruction inst : ir.getInstructions()) {
					if (inst != null) {
						ISSABasicBlock bb = ir.getBasicBlockForInstruction(inst);
						if (bb != null) {
							for(Iterator<ISSABasicBlock> pbs = cdg.getPredNodes(bb); pbs.hasNext(); ) {
								ISSABasicBlock pb = pbs.next();
								SSAInstruction cond = pb.getLastInstruction();
								Set<SSAInstruction> vals = HashSetFactory.make();
								for(int i = 0; i < cond.getNumberOfUses(); i++) {
									vals.add(du.getDef(cond.getUse(i)));
								}
								while (! vals.isEmpty()) {
									SSAInstruction pred = vals.iterator().next();
									vals.remove(pred);
									if (pred instanceof SSAPhiInstruction) {
										for(int i = 0; i < pred.getNumberOfUses(); i++) {
											vals.add(du.getDef(pred.getUse(i)));
										}
									} else if (pred != null) {
										record(controlflowGraph, controlflowEdgeCount, getSourceName(pred), getSourceName(inst));
									}
								}
							}
						}
					}
				}
				
				for(int i = 0; i <= ir.getSymbolTable().getMaxValueNumber(); i++) {
					SSAInstruction d = du.getDef(i);
					if (d instanceof SSAInvokeInstruction || d instanceof SSAGetInstruction || d instanceof SSABinaryOpInstruction) {
						Set<SSAInstruction> uses = HashSetFactory.make();
						du.getUses(i).forEachRemaining((SSAInstruction x) -> { uses.add(x); });
						while (! uses.isEmpty()) {
							SSAInstruction use = uses.iterator().next();
							uses.remove(use);
							if (use instanceof SSAInvokeInstruction || use instanceof SSABinaryOpInstruction) {
								String src = getSourceName(d);
								String target = getTargetName(use);
								
								if (src == null || target == null) {
									continue;
								}
										
								record(dataflowGraph, dataflowEdgeCount, src, target);										
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

	private static void record(Graph<String> dataflowGraph, Map<String,Integer> dataflowEdgeCount, String src, String target) {
		if (src != null && target != null) {
			if (!dataflowGraph.containsNode(src)) {	
				dataflowGraph.addNode(src);	
			}
			if (!dataflowGraph.containsNode(target)) {
				dataflowGraph.addNode(target);
			}
			dataflowGraph.addEdge(src, target);
			String key = src + "-->" + target;
			if (!dataflowEdgeCount.containsKey(key)) {
				dataflowEdgeCount.put(key, 0);
			}
			int count = dataflowEdgeCount.get(key) + 1;
			dataflowEdgeCount.put(key, count);
		}
	}

	private static String getTargetName(SSAInstruction use) {
		String target = null;
		if (use instanceof SSAInvokeInstruction) {
			target = getFunction(((SSAInvokeInstruction) use).getCallSite().getDeclaredTarget().getName().toString());
		} else if (use instanceof SSABinaryOpInstruction) {
			target = getFunction(((SSABinaryOpInstruction) use).getOperator().toString());
		}
		return target;
	}

	private static String getSourceName(SSAInstruction d) {
		String src = null;
		if (d instanceof SSAInvokeInstruction) {
			src = getFunction(((SSAInvokeInstruction) d).getCallSite().getDeclaredTarget().getName().toString());
		} else if (d instanceof SSABinaryOpInstruction) {
			src = getFunction(((SSABinaryOpInstruction) d).getOperator().toString());
		} else if (d instanceof SSAGetInstruction) {
			src = getFunction("column_read");
		}
		return src;
	}
}
