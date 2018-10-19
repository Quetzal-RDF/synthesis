package sqlAnalysis;

import es.ucm.sexp.SexpFileParser;
import es.ucm.sexp.SexpParser;
import org.apache.commons.lang3.math.NumberUtils;
import org.json.simple.JSONObject;
import org.json.simple.JSONArray;
import org.json.simple.parser.JSONParser;
import org.json.simple.parser.ParseException;

import java.io.ByteArrayInputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.*;

public class BuildUsageGraph {

    Map<String, Integer> opToNumeric = new HashMap<String, Integer>();

    public static void main(String[] args) throws Exception {
        BuildUsageGraph g = new BuildUsageGraph();
        g.parseJSON(args[0]);
       // g.dumpNodes();

    }


    private void parseJSON(String file) throws IOException, ParseException, SexpParser.ParseException {
        JSONParser parser = new JSONParser();
        JSONArray arr = (JSONArray) parser.parse(new FileReader(file));

        for (Object obj : arr) {
            JSONObject o = (JSONObject) obj;
            JSONArray exp = (JSONArray) o.get("expressions");
            for (Object e : exp) {
                String expression = (String) ((JSONObject) e).get("expression");
                InputStream stream = new ByteArrayInputStream(expression.getBytes(StandardCharsets.UTF_8));
                SexpFileParser parse = new SexpFileParser(stream);
                SexpParser.Expr result = parse.parseExpr();
                StringBuffer buf = new StringBuffer();
                if (result.isList()) {
                    opToNumeric.clear();
                    buildPattern(result, "start", buf);
                }
 //               System.out.println(result);
                System.out.println(buf.toString());
            }
        }
    }


    private String generalizeAtom(String atom) {
        if (atom.startsWith("\"")) {
            return "constant";
        }
        if (NumberUtils.isNumber(atom)) {
            return "constant";
        }
        return atom;
    }

    private void buildPattern(SexpParser.Expr exp, String currentNode, StringBuffer buf) {

        if (exp == null) {
            // list iteration has completed
            return;
        }
        if (exp.isAtom()) {
            String atom = generalizeAtom(exp.getAtom().toString());

            int count = 0;
            if (opToNumeric.containsKey(atom)) {
                count = opToNumeric.get(atom);
                count++;
                opToNumeric.put(atom, count);
            } else {
                opToNumeric.put(atom, count);
            }
           // atom = getUniqueNodeNameForExpression(atom);
            // check if the current node has an atom already with the exact same name successor

            buf.append(currentNode).append("->").append(atom).append("|");

        }

        if (exp.isList()) {
            // check if this is a column
            if (exp.getList().car.toString().equals("in")) {
                buf.append(currentNode).append("->").append("column").append("|");
            } else {
                buildPattern(exp.getList().car, currentNode, buf);
                if (exp.getList().car.isAtom()) {
                    String atom = generalizeAtom(exp.getList().car.getAtom().toString());
                    // buildGraph(exp.getList().cdr, getUniqueNodeNameForExpression(atom), buf);
                    buildPattern(exp.getList().cdr, atom, buf);
                } else {
                    buildPattern(exp.getList().cdr, currentNode, buf);
                }
            }
        }

    }

}
