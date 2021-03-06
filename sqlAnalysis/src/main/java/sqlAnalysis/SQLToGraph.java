package sqlAnalysis;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.json.JSONArray;
import org.json.JSONObject;
import com.ibm.wala.cast.ir.ssa.AstIRFactory;
import com.ibm.wala.cast.util.Util;
import com.ibm.wala.cfg.cdg.ControlDependenceGraph;
import com.ibm.wala.classLoader.ClassLoaderFactory;
import com.ibm.wala.classLoader.IClass;
import com.ibm.wala.classLoader.IMethod;
import com.ibm.wala.classLoader.SourceModule;
import com.ibm.wala.ipa.callgraph.AnalysisScope;
import com.ibm.wala.ipa.callgraph.impl.Everywhere;
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
import com.ibm.wala.ssa.SSAReturnInstruction;
import com.ibm.wala.util.collections.HashSetFactory;
import com.ibm.wala.util.collections.Pair;
import com.ibm.wala.util.graph.Graph;
import com.ibm.wala.util.graph.impl.SlowSparseNumberedGraph;

public class SQLToGraph {
  static SlowSparseNumberedGraph<String> dataflowGraph = new SlowSparseNumberedGraph<String>(1);
  static HashMap<Pair<String, String>, Integer> dataflowEdgeCount =
      new HashMap<Pair<String, String>, Integer>();
  static SlowSparseNumberedGraph<String> controlflowGraph = new SlowSparseNumberedGraph<String>(1);
  static HashMap<Pair<String, String>, Integer> controlflowEdgeCount =
      new HashMap<Pair<String, String>, Integer>();
  static Map<String, String> srcToTargetFunction = new HashMap<String, String>();

  static {
    Stream<String> mappings;
    try {
      mappings = Files.lines(Paths.get("functions"));
      mappings.forEach(name -> {
        System.out.println(name);
        String[] arr = name.split("[|]");
        assert arr.length == 2;
        srcToTargetFunction.put(arr[0], arr[1]);
      });
      mappings.close();
    } catch (IOException e) {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
  }

  public static String getFunction(String src) {
    if (srcToTargetFunction.containsKey(src)) {
      return srcToTargetFunction.get(src);
    } else {
      return src;
    }
  }

  public static void doPresto(String code, ClassLoaderFactory loaders) throws Exception {

    SourceModule M = new SQLSourceModule(code);
    AnalysisScope scope = new AnalysisScope(Collections.singleton(SQL.sql)) {
      {
        loadersByName.put(SQLClassLoaderFactory.Sql.getName(), SQLClassLoaderFactory.Sql);
      }
    };
    scope.addToScope(SQLClassLoaderFactory.Sql, M);

    IClassHierarchy cha = SeqClassHierarchyFactory.make(scope, loaders);

    Util.checkForFrontEndErrors(cha);

    IRFactory<IMethod> irs = new AstIRFactory<IMethod>();
    for (IClass c : cha) {
      for (IMethod f : c.getDeclaredMethods()) {
        IR ir = irs.makeIR(f, Everywhere.EVERYWHERE, SSAOptions.defaultOptions());
        DefUse du = new DefUse(ir);

        ControlDependenceGraph<ISSABasicBlock> cdg =
            new ControlDependenceGraph<>(ir.getControlFlowGraph());
        for (SSAInstruction inst : ir.getInstructions()) {
          if (inst != null) {
            ISSABasicBlock bb = ir.getBasicBlockForInstruction(inst);
            if (bb != null) {
              for (Iterator<ISSABasicBlock> pbs = cdg.getPredNodes(bb); pbs.hasNext();) {
                ISSABasicBlock pb = pbs.next();
                SSAInstruction cond = pb.getLastInstruction();
                Set<SSAInstruction> vals = HashSetFactory.make();
                for (int i = 0; i < cond.getNumberOfUses(); i++) {
                  vals.add(du.getDef(cond.getUse(i)));
                }
                while (!vals.isEmpty()) {
                  SSAInstruction pred = vals.iterator().next();
                  vals.remove(pred);
                  if (pred instanceof SSAPhiInstruction) {
                    for (int i = 0; i < pred.getNumberOfUses(); i++) {
                      vals.add(du.getDef(pred.getUse(i)));
                    }
                  } else if (pred != null) {
                    record(controlflowGraph, controlflowEdgeCount, getSourceName(pred),
                        getSourceName(inst));
                  } 
                }
              }
            }
          }
        }

        for (int i = 0; i <= ir.getSymbolTable().getMaxValueNumber(); i++) {
          SSAInstruction d = du.getDef(i);
          if (d instanceof SSAInvokeInstruction || d instanceof SSAGetInstruction
              || d instanceof SSABinaryOpInstruction || d == null) {
            Set<SSAInstruction> uses = HashSetFactory.make();
            du.getUses(i).forEachRemaining((SSAInstruction x) -> {
              uses.add(x);
            });
            while (!uses.isEmpty()) {
              SSAInstruction use = uses.iterator().next();
              uses.remove(use);
              if (use instanceof SSAInvokeInstruction || use instanceof SSABinaryOpInstruction || use instanceof SSAReturnInstruction) {
                String src = getSourceName(d);
                String target = getTargetName(use);

                if (target == null) {
                  System.out.println(src);
                  continue;
                }

                record(dataflowGraph, dataflowEdgeCount, src==null? "constant": src, target);
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

  }

  public static void printEdgeCounts(Map<Pair<String, String>, Integer> edgeCounts) {
    int count = 0;
    edgeCounts =
        edgeCounts.entrySet().stream().sorted(Map.Entry.comparingByValue(Comparator.reverseOrder()))
            .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue,
                (oldValue, newValue) -> oldValue, LinkedHashMap::new));
    for (Map.Entry<Pair<String, String>, Integer> entry : edgeCounts.entrySet()) {
      count += entry.getValue();
      System.out.print(entry.getKey() + " " + entry.getValue() + "\n");
    }

    System.out.println("Total edges:" + count);
  }

  public static void printDataFlowForRacket(Map<Pair<String, String>, Integer> edgeCounts,
      String fileName) throws Exception {
    edgeCounts =
        edgeCounts.entrySet().stream().sorted(Map.Entry.comparingByValue(Comparator.reverseOrder()))
            .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue,
                (oldValue, newValue) -> oldValue, LinkedHashMap::new));
    JSONObject json = new JSONObject();
    BufferedWriter writer = new BufferedWriter(new FileWriter(fileName));
    for (Map.Entry<Pair<String, String>, Integer> e : edgeCounts.entrySet()) {
      String key = SExpressionWriter.functionNameNormalizedMap.containsKey(e.getKey().snd)
          ? SExpressionWriter.functionNameNormalizedMap.get(e.getKey().snd) : e.getKey().snd;
      String value = SExpressionWriter.functionNameNormalizedMap.containsKey(e.getKey().fst)
          ? SExpressionWriter.functionNameNormalizedMap.get(e.getKey().fst) : e.getKey().fst;
      
      if (!json.has(key)) {
        json.put(key, new JSONArray());
      }
      JSONArray l = (JSONArray) json.get(key);
      l.put(value);
    }
    json.write(writer);
    writer.close();
  }

  public static void printEdges(Map<Pair<String, String>, Integer> edgeCounts, String fileName)
      throws Exception {
    edgeCounts =
        edgeCounts.entrySet().stream().sorted(Map.Entry.comparingByValue(Comparator.reverseOrder()))
            .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue,
                (oldValue, newValue) -> oldValue, LinkedHashMap::new));
    JSONObject json = new JSONObject();
    BufferedWriter writer = new BufferedWriter(new FileWriter(fileName));
    for (Map.Entry<Pair<String, String>, Integer> e : edgeCounts.entrySet()) {
      if (!json.has(e.getKey().fst)) {
        json.put(e.getKey().fst, new JSONArray());
      }
      JSONArray l = (JSONArray) json.get(e.getKey().fst);
      JSONObject obj = new JSONObject();
      obj.put("target", e.getKey().snd);
      obj.put("count", e.getValue());
      l.put(obj);
    }
    json.write(writer);
    writer.close();
  }

  private static void record(Graph<String> dataflowGraph,
      Map<Pair<String, String>, Integer> dataflowEdgeCount, String src, String target) {
    if (src != null && target != null) {
      if (!dataflowGraph.containsNode(src)) {
        dataflowGraph.addNode(src);
      }
      if (!dataflowGraph.containsNode(target)) {
        dataflowGraph.addNode(target);
      }
      dataflowGraph.addEdge(src, target);
      Pair<String, String> key = Pair.make(src, target);
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
      target = getFunction(
          ((SSAInvokeInstruction) use).getCallSite().getDeclaredTarget().getName().toString());
    } else if (use instanceof SSABinaryOpInstruction) {
      target = getFunction(((SSABinaryOpInstruction) use).getOperator().toString());
    } else if (use instanceof SSAReturnInstruction) {
      target = "root";
    }
    return target;
  }

  private static String getSourceName(SSAInstruction d) {
    String src = null;
    if (d instanceof SSAInvokeInstruction) {
      src = getFunction(
          ((SSAInvokeInstruction) d).getCallSite().getDeclaredTarget().getName().toString());
    } else if (d instanceof SSABinaryOpInstruction) {
      src = getFunction(((SSABinaryOpInstruction) d).getOperator().toString());
    } else if (d instanceof SSAGetInstruction) {
      src = getFunction("column_read");
    }
    return src;
  }

}
