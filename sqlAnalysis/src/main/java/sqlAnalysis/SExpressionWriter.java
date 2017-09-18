package sqlAnalysis;

import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.facebook.presto.sql.tree.ArithmeticExpression;
import com.facebook.presto.sql.tree.BetweenPredicate;
import com.facebook.presto.sql.tree.BooleanLiteral;
import com.facebook.presto.sql.tree.Cast;
import com.facebook.presto.sql.tree.CoalesceExpression;
import com.facebook.presto.sql.tree.ComparisonExpression;
import com.facebook.presto.sql.tree.CurrentTime;
import com.facebook.presto.sql.tree.DefaultTraversalVisitor;
import com.facebook.presto.sql.tree.DoubleLiteral;
import com.facebook.presto.sql.tree.ExistsPredicate;
import com.facebook.presto.sql.tree.Expression;
import com.facebook.presto.sql.tree.Extract;
import com.facebook.presto.sql.tree.FunctionCall;
import com.facebook.presto.sql.tree.GenericLiteral;
import com.facebook.presto.sql.tree.IfExpression;
import com.facebook.presto.sql.tree.InListExpression;
import com.facebook.presto.sql.tree.InPredicate;
import com.facebook.presto.sql.tree.IntervalLiteral;
import com.facebook.presto.sql.tree.IsNotNullPredicate;
import com.facebook.presto.sql.tree.IsNullPredicate;
import com.facebook.presto.sql.tree.Join;
import com.facebook.presto.sql.tree.JoinCriteria;
import com.facebook.presto.sql.tree.JoinOn;
import com.facebook.presto.sql.tree.LikePredicate;
import com.facebook.presto.sql.tree.Literal;
import com.facebook.presto.sql.tree.LogicalBinaryExpression;
import com.facebook.presto.sql.tree.LogicalBinaryExpression.Type;
import com.facebook.presto.sql.tree.LongLiteral;
import com.facebook.presto.sql.tree.NegativeExpression;
import com.facebook.presto.sql.tree.Node;
import com.facebook.presto.sql.tree.NotExpression;
import com.facebook.presto.sql.tree.NullIfExpression;
import com.facebook.presto.sql.tree.NullLiteral;
import com.facebook.presto.sql.tree.QualifiedName;
import com.facebook.presto.sql.tree.QualifiedNameReference;
import com.facebook.presto.sql.tree.Query;
import com.facebook.presto.sql.tree.QuerySpecification;
import com.facebook.presto.sql.tree.Relation;
import com.facebook.presto.sql.tree.SearchedCaseExpression;
import com.facebook.presto.sql.tree.SelectItem;
import com.facebook.presto.sql.tree.SimpleCaseExpression;
import com.facebook.presto.sql.tree.SingleColumn;
import com.facebook.presto.sql.tree.StringLiteral;
import com.facebook.presto.sql.tree.SubqueryExpression;
import com.facebook.presto.sql.tree.TimeLiteral;
import com.facebook.presto.sql.tree.TimestampLiteral;
import com.facebook.presto.sql.tree.WhenClause;
import com.google.common.base.Optional;
import com.ibm.wala.cast.tree.CAstNode;
import com.ibm.wala.util.collections.Pair;
import com.ibm.wala.util.graph.impl.SlowSparseNumberedGraph;

public class SExpressionWriter extends DefaultTraversalVisitor<String, Void> {

	protected List<String> expressions = new LinkedList<String>();
	protected Set<String> functions = new HashSet<String>();
	protected int columnNumber = 0;

	HashMap<String, Integer> colNamesToInt = new HashMap<String, Integer>();
	protected SlowSparseNumberedGraph<TypedNode> graph = new SlowSparseNumberedGraph<TypedNode>(1);
	private Map<String, List<Integer>> functionNamesToTypes = new HashMap<String, List<Integer>>();
	private Map<String, String> functionNameMap = new HashMap<String, String>();

	public SExpressionWriter() {
		
		// what should we do with decode?
		// And field
		
		LinkedList<Integer> l;
		
		l = new LinkedList<Integer>();
		l.add(3);
		l.add(3);
		functionNamesToTypes.put("trim", l);
		functionNameMap.put("trim", "trim");
		functionNameMap.put("ltrim", "trim");
		
		l = new LinkedList<Integer>();
		l.add(4);
		l.add(3);
		functionNamesToTypes.put("strcmp", l);
		functionNameMap.put("strcmp", "strcmp");	
		
		l = new LinkedList<Integer>();
		l.add(1);
		l.add(6);
		functionNamesToTypes.put("count", l);
		functionNameMap.put("count", "count");

	
		l = new LinkedList<Integer>();
		l.add(1);
		l.add(3);
		functionNamesToTypes.put("length", l);
		functionNameMap.put("char_length", "length");
		functionNameMap.put("length", "length");
		functionNameMap.put("len", "length");
		
		
		functionNameMap.put("row_number", "row_number");

		l = new LinkedList<Integer>();
		l.add(3);
		l.add(3);
		l.add(3);
		functionNamesToTypes.put("concat", l);
		functionNameMap.put("concat", "concat");
	
		l = new LinkedList<Integer>();
		l.add(3);
		l.add(3);
		l.add(3);
		l.add(3);
		functionNamesToTypes.put("replace", l);
		functionNameMap.put("replace", "replace");
		
		
		l = new LinkedList<Integer>();
		l.add(3);
		l.add(3);
		l.add(3);
		l.add(3);
		functionNamesToTypes.put("regexp_replace", l);
		functionNameMap.put("regexp_replace", "regexp_replace");


		
		l = new LinkedList<Integer>();
		l.add(3);
		l.add(3);
		functionNamesToTypes.put("lower", l);
		functionNameMap.put("lower", "lower");
		functionNameMap.put("lcase", "lower");

		l = new LinkedList<Integer>();
		l.add(3);
		l.add(3);
		functionNamesToTypes.put("upper", l);
		functionNameMap.put("upper", "upper");
		functionNameMap.put("ucase", "upper");


		l = new LinkedList<Integer>();
		l.add(3);
		l.add(3);
		functionNamesToTypes.put("upper", l);
		functionNameMap.put("upper", "upper");
		
		l = new LinkedList<Integer>();
		l.add(3);
		l.add(3);
		functionNamesToTypes.put("reverse", l);
		functionNameMap.put("reverse", "reverse");
		
		l = new LinkedList<Integer>();
		l.add(3);
		l.add(3);
		functionNamesToTypes.put("group_concat", l);
		functionNameMap.put("group_concat", "group_concat");
		functionNameMap.put("string_agg", "group_concat");

		l = new LinkedList<Integer>();
		l.add(3);
		l.add(3);
		l.add(1);
		l.add(1);
		functionNamesToTypes.put("substring", l);
		functionNameMap.put("mid", "substring");
		functionNameMap.put("substring", "substring");
		functionNameMap.put("substr", "substring");
		
		l = new LinkedList<Integer>();
		l.add(1);
		l.add(1);
		functionNamesToTypes.put("sum", l);
		functionNameMap.put("sum", "sum");

		l = new LinkedList<Integer>();
		l.add(1);
		l.add(1);
		functionNamesToTypes.put("min", l);
		functionNameMap.put("min", "min");
		
		
		l = new LinkedList<Integer>();
		l.add(1);
		l.add(1);
		functionNamesToTypes.put("sign", l);
		functionNameMap.put("sign", "sign");

		
		l = new LinkedList<Integer>();
		l.add(1);
		l.add(1);
		functionNamesToTypes.put("abs", l);
		functionNameMap.put("abs", "abs");

		l = new LinkedList<Integer>();
		l.add(1);
		l.add(2);
		functionNamesToTypes.put("from_unixtime", l);
		functionNameMap.put("from_unixtime", "from_unixtime");
		

		l = new LinkedList<Integer>();
		l.add(1);
		l.add(2);
		functionNamesToTypes.put("extractMonth", l);
		functionNameMap.put("month", "extractMonth");
		
		l = new LinkedList<Integer>();
		l.add(1);
		l.add(2);
		functionNamesToTypes.put("extractYear", l);
		functionNameMap.put("year", "extractYear");


		l = new LinkedList<Integer>();
		l.add(2);
		
		functionNamesToTypes.put("curdate", l);
		functionNameMap.put("curdate", "curdate");
		functionNameMap.put("getdate", "curdate");
		functionNameMap.put("now", "curdate");

		l = new LinkedList<Integer>();
		l.add(1);
		l.add(3);
		l.add(2);
		functionNamesToTypes.put("extractFromDate", l);
		functionNameMap.put("date_part", "extractFromDate");
		functionNameMap.put("datepart", "extractFromDate");
		functionNameMap.put("extractFromDate", "extractFromDate");
		functionNameMap.put("datetrunc", "extractFromDate");
		
		
		l = new LinkedList<Integer>();
		l.add(1);
		l.add(2);
		functionNamesToTypes.put("extractDayOfWeek", l);
		functionNameMap.put("weekday", "extractDayOfWeek");

		l = new LinkedList<Integer>();
		l.add(1);
		l.add(2);
		functionNamesToTypes.put("extractDayOfMonth", l);
		functionNameMap.put("day", "extractDayOfMonth");

		
		l = new LinkedList<Integer>();
		l.add(2);
		l.add(2);
		l.add(1);
		functionNamesToTypes.put("date_sub", l);
		functionNameMap.put("date_sub", "date_sub");
		
		l = new LinkedList<Integer>();
		l.add(1);
		l.add(2);
		l.add(2);
		functionNamesToTypes.put("datediff", l);
		functionNameMap.put("datediff", "datediff");

		l = new LinkedList<Integer>();
		l.add(2);
		l.add(2);
		l.add(1);
		l.add(3);
		functionNamesToTypes.put("dateadd", l);
		functionNameMap.put("adddate", "dateadd");
		functionNameMap.put("date_add", "dateadd");

		l = new LinkedList<Integer>();
		l.add(2);
		l.add(2);
		l.add(1);
		functionNamesToTypes.put("date_add_months", l);
		functionNameMap.put("add_months", "date_add_months");

		l = new LinkedList<Integer>();
		l.add(2);
		l.add(2);
		l.add(1);
		l.add(3);
		functionNamesToTypes.put("date_sub_interval", l);
		functionNameMap.put("date_sub", "date_sub_interval");

		l = new LinkedList<Integer>();
		l.add(1);
		l.add(2);
		functionNamesToTypes.put("unix_timestamp", l);
		functionNameMap.put("unix_timestamp", "unix_timestamp");
		

		l = new LinkedList<Integer>();
		l.add(1);
		l.add(1);
		functionNamesToTypes.put("avg", l);
		functionNameMap.put("avg", "avg");
		
		functionNameMap.put("ifnull", "coalesce");
		functionNameMap.put("coalesce", "coalesce");
		functionNameMap.put("isnull", "coalesce");
		functionNameMap.put("nvl", "coalesce");

		l = new LinkedList<Integer>();
		l.add(1);
		l.add(3);
		l.add(3);
		l.add(1);
		functionNamesToTypes.put("index-of", l);
		functionNameMap.put("instr", "index-of");
		functionNameMap.put("locate", "index-of");
		functionNameMap.put("charindex", "index-of");
		functionNameMap.put("position", "index-of");

		l = new LinkedList<Integer>();
		l.add(1);
		l.add(5);
		functionNamesToTypes.put("ceiling", l);
		functionNameMap.put("ceil", "ceiling");
		functionNameMap.put("ceiling", "ceiling");

		l = new LinkedList<Integer>();
		l.add(1);
		l.add(5);
		l.add(1);
		functionNamesToTypes.put("truncate", l);
		functionNameMap.put("trunc", "truncate");

		l = new LinkedList<Integer>();
		l.add(5);
		l.add(5);
		functionNamesToTypes.put("atan", l);
		functionNameMap.put("atan", "atan");
		
		l = new LinkedList<Integer>();
		l.add(5);
		l.add(5);
		functionNamesToTypes.put("sin", l);
		functionNameMap.put("sin", "sin");


		l = new LinkedList<Integer>();
		l.add(5);
		l.add(5);
		functionNamesToTypes.put("cos", l);
		functionNameMap.put("cos", "cos");
		

		l = new LinkedList<Integer>();
		l.add(5);
		l.add(5);
		functionNamesToTypes.put("acos", l);
		functionNameMap.put("acos", "acos");


		l = new LinkedList<Integer>();
		l.add(5);
		l.add(5);
		functionNamesToTypes.put("log10", l);
		functionNameMap.put("log10", "log10");
		

		l = new LinkedList<Integer>();
		l.add(5);
		l.add(5);
		functionNamesToTypes.put("log", l);
		functionNameMap.put("log", "log");


		l = new LinkedList<Integer>();
		l.add(5);
		l.add(5);
		functionNamesToTypes.put("cot", l);
		functionNameMap.put("cot", "cot");

		l = new LinkedList<Integer>();
		l.add(5);
		l.add(5);
		functionNamesToTypes.put("tan", l);
		functionNameMap.put("tan", "tan");

		l = new LinkedList<Integer>();
		l.add(5);
		l.add(5);
		l.add(5);
		functionNamesToTypes.put("pow", l);
		functionNameMap.put("pow", "pow");
		functionNameMap.put("power", "pow");


		l = new LinkedList<Integer>();
		l.add(5);
		l.add(5);
		functionNamesToTypes.put("exp", l);
		functionNameMap.put("exp", "exp");

		
		l = new LinkedList<Integer>();
		l.add(5);
		l.add(5);
		functionNamesToTypes.put("degrees", l);
		functionNameMap.put("degrees", "degrees");

	
		l = new LinkedList<Integer>();
		l.add(5);
		l.add(5);
		functionNamesToTypes.put("radians", l);
		functionNameMap.put("radians", "radians");

		l = new LinkedList<Integer>();
		l.add(3);
		l.add(3);
		l.add(3);
		l.add(3);
		functionNamesToTypes.put("concat_ws", l);
		functionNameMap.put("concat_ws", "concat_ws");


		l = new LinkedList<Integer>();
		l.add(1);
		l.add(1);
		functionNamesToTypes.put("max", l);
		functionNameMap.put("max", "max");


		l = new LinkedList<Integer>();
		l.add(4);
		l.add(3);
		l.add(3);
		functionNamesToTypes.put("like", l);
		functionNameMap.put("like", "like");
		
		l = new LinkedList<Integer>();
		l.add(4);
		l.add(3);
		l.add(3);
		functionNamesToTypes.put("regexp_like", l);
		functionNameMap.put("regexp_like", "regexp_like");
	
		
		l = new LinkedList<Integer>();
		l.add(1);
		l.add(5);
		functionNamesToTypes.put("floor", l);
		functionNameMap.put("floor", "floor");

		l = new LinkedList<Integer>();
		l.add(1);
		l.add(5);
		functionNamesToTypes.put("round", l);
		functionNameMap.put("round", "round");
	}

	@Override
	protected String visitJoin(Join node, Void context) {
		Optional<JoinCriteria> join = node.getCriteria();
		if (join.isPresent()) {
			if (join.get() instanceof JoinOn) {
				try {
					expressions.add(process(((JoinOn) join.get()).getExpression(), context));
				} catch (UnsupportedOperationException e) {
					
				}
			}
		}
		return null;
	}

	@Override
	protected String visitSingleColumn(SingleColumn node, Void context) {
		if (!colNamesToInt.containsKey(node.getExpression().toString())) {
			columnNumber += 1;
			colNamesToInt.put(node.getExpression().toString(), columnNumber);
		}
		return "col" + colNamesToInt.get(node.getExpression().toString());

	}

	@Override
	protected String visitSimpleCaseExpression(SimpleCaseExpression node, Void context) {
		String result = process(node.getDefaultValue(), context);
		StringBuffer buf = new StringBuffer();
		List<WhenClause> whenclauses = node.getWhenClauses();
		for (int i = whenclauses.size() - 1; i >= 0; i--) {
			buf.append("(").append("if").append(" ").append(process(whenclauses.get(i).getOperand(), context))
					.append(process(whenclauses.get(i).getResult(), context)).append(result);
		}
		return buf.toString();
	}

	@Override
	protected String visitQuerySpecification(QuerySpecification node, Void context) {
		List<SelectItem> items = node.getSelect().getSelectItems();
		List<CAstNode> l = new LinkedList<CAstNode>();

		for (SelectItem i : items) {
			if (i instanceof SingleColumn) {
				Expression e = ((SingleColumn) i).getExpression();
				if (!(e instanceof QualifiedNameReference)) {
					try {
						expressions.add(process(e, context));
					} catch (UnsupportedOperationException e1) {
						
					}
				}
			}
		}

		if (node.getWhere().isPresent()) {
			try {
				expressions.add(process(node.getWhere().get(), context));
			} catch (UnsupportedOperationException e1) {
				
			}
		}

		for (Relation r : node.getFrom()) {
			if (r instanceof Join) {
				visitJoin((Join) r, null);
			}
		}
		return null;
	}

	protected static String processOp(String op) {
		if ("+".equals(op)) {
			return "+";
		} else if ("-".equals(op)) {
			return "-";
		} else if ("*".equals(op)) {
			return "*";
		} else if ("/".equals(op)) {
			return "/";
		} else if ("%".equals(op)) {
			return "remainder";
		} else if ("<".equals(op)) {
			return "<";
		} else if ("<=".equals(op)) {
			return "<=";
		} else if (">".equals(op)) {
			return ">";
		} else if (">=".equals(op)) {
			return ">=";
		} else if ("=".equals(op)) {
			return "=";
		} else if ("!=".equals(op) || "<>".equals(op)) {
			return "!=";
		} else if ("|".equals(op)) {
			return "|";
		} else if ("&".equals(op)) {
			return "&";
		} else if ("#".equals(op)) {
			return "#";
		} else if ("~".equals(op)) {
			return "~";
		} else {
			throw new UnsupportedOperationException("dont recognize:" + op);
		}
	}

	@Override
	protected String visitArithmeticExpression(ArithmeticExpression node, Void context) {
		// arithmetic expressions will automatically be parsed as integers
		record(node.getLeft(), getLiteralForType(1), context);
		return "(" + processOp(node.getType().getValue()) + " " + process(node.getLeft(), context) + " "
				+ process(node.getRight(), context) + ")";
	}

	@Override
	protected String visitBetweenPredicate(BetweenPredicate node, Void context) {
		record(node.getValue(), node.getMax(), context);
		Expression e1 = new ComparisonExpression(ComparisonExpression.Type.LESS_THAN, node.getValue(), node.getMax());
		Expression e2 = new ComparisonExpression(ComparisonExpression.Type.GREATER_THAN, node.getValue(),
				node.getMin());
		LogicalBinaryExpression e3 = new LogicalBinaryExpression(Type.AND, e1, e2);
		return process(e3, context);
	}

	@Override
	protected String visitQuery(Query node, Void context) {
		List<QuerySpecification> querySpec = new LinkedList<QuerySpecification>();
		PrestoVisitor.getQuerySpecifications(node, querySpec);
		assert querySpec.size() == 1;
		return visitQuerySpecification(querySpec.get(0), context);
	}

	@Override
	protected String visitSubqueryExpression(SubqueryExpression node, Void context) {
		return process(node.getQuery(), context);
	}

	@Override
	protected String visitExists(ExistsPredicate node, Void context) {
		return visitQuery(node.getSubquery(), context);
	}

	private TypedNode record(String v) {
		TypedNode val = new TypedNode(v);
		if (!graph.containsNode(val)) {
			graph.addNode(val);
		}
		return val;
	}
	
	private LocalDate tryDateParse(StringLiteral s) {
		LocalDate ret = null;
		
		try {
			ret = LocalDate.parse(s.getValue(), DateTimeFormatter.BASIC_ISO_DATE);
		} catch (Exception e1) {
			try {
				ret = LocalDate.parse(s.getValue(), DateTimeFormatter.ISO_DATE);
			} catch (Exception e2) {
				try {
					ret = LocalDate.parse(s.getValue(), DateTimeFormatter.ISO_DATE_TIME);
				} catch (Exception e3) {
					try {
						ret = LocalDate.parse(s.getValue(), DateTimeFormatter.BASIC_ISO_DATE);
					} catch (Exception e4) {
						try {
							ret = LocalDate.parse(s.getValue(), DateTimeFormatter.ISO_LOCAL_DATE);
						} catch (Exception e5) {
							try {
								ret = LocalDate.parse(s.getValue(), DateTimeFormatter.ISO_LOCAL_DATE_TIME);
							} catch (Exception e6) {
								try {
									ret = LocalDate.parse(s.getValue(), DateTimeFormatter.ISO_LOCAL_TIME);
								} catch (Exception e7) {
									try {
										ret = LocalDate.parse(s.getValue(), DateTimeFormatter.ISO_OFFSET_DATE);
									} catch (Exception e8) {
										try {
											ret = LocalDate.parse(s.getValue(), DateTimeFormatter.ISO_OFFSET_DATE_TIME);
										} catch (Exception e9) {
											try {
												ret = LocalDate.parse(s.getValue(), DateTimeFormatter.ISO_OFFSET_TIME);
											} catch (Exception e10) {
												try {
													ret = LocalDate.parse(s.getValue(), DateTimeFormatter.ISO_INSTANT);
												} catch (Exception e11) {
													try {
														ret = LocalDate.parse(s.getValue(), DateTimeFormatter.RFC_1123_DATE_TIME);
													} catch (Exception e12) {
														
													}
												}
											}
										}
									}
								}
							}
						}
					}
				}
			}
		}
		return ret;
		
	}
	
	public List<Pair<String, String>> getExpressionsAndTypes() {
		propagateTypes();
		
		StringBuffer types = new StringBuffer("(");
		Iterator<TypedNode> nodes = graph.iterator();
		while (nodes.hasNext()) {
			TypedNode n = nodes.next();
			StringBuffer t = new StringBuffer("(");
			for (Integer q : n.types) {
				t.append(q).append(" ");
			}
			t.append(")");
			
			if (n.name.startsWith("col")) {
				types.append("(").append("columnName").append(" ").append("\"")
				.append(n.name).append("\"").append(" ").append("primitiveTypes").append(" ")
				.append(t).append(")").append(" ");
			}
		}
		types.append(")");
		
		List<Pair<String, String>> ret = new LinkedList<Pair<String, String>>();
		
		for (String s : expressions) {
			if (s == null) {
				continue;
			}
			ret.add(Pair.make(s, types.toString()));
		}

		return ret;
	}
	
	private void propagateTypes() {
		boolean changed = true;
		
		while (changed) {
			changed = false;
			Iterator<TypedNode> nodesIt = graph.iterator();
			while (nodesIt.hasNext()) {
				TypedNode n = nodesIt.next();
				for (Integer t : n.types) {
					Iterator<TypedNode> it = graph.getSuccNodes(n);
					while (it.hasNext()) {
						TypedNode succ = it.next();
						if (!succ.types.contains(t)) {
							changed = true;
							succ.types.add(t);
						}
					}
				}
			}
		}
		
		// go back and add integer as a default type if types are empty for a node
		Iterator<TypedNode> nodesIt = graph.iterator();
		while (nodesIt.hasNext()) {
			TypedNode n = nodesIt.next();
			if (n.types.isEmpty()) {
				n.types.add(1);
			}
		}
	}

	private Pair<Integer, String> getType(Literal l) {
		int type = -1;
		if (l instanceof BooleanLiteral) {
			type = 4;
		} else if (l instanceof LongLiteral) {
			type = 1;
		} else if (l instanceof TimeLiteral || l instanceof TimestampLiteral) {
			type = 2;
		} else if (l instanceof DoubleLiteral) {
			type = 5;
		} else if (l instanceof StringLiteral) {
			StringLiteral s = (StringLiteral) l;
			LocalDate d = tryDateParse(s);
			if (d != null) {
				type = 2;
			} else {
				type = 3;
			}
		} else if (l instanceof GenericLiteral) {
			type = 6;
		}
		return Pair.make(type, getStringForType(type));
	}

	private String getStringForType(int type) {
		switch (type) {
		case 1:
			return "integer";
		case 2:
			return "date";
		case 3:
			return "string";
		case 4:
			return "boolean";
		case 5:
			return "real";
		case 6:
			return "any";
		default:
			return "integer";
		}
	}

	private Pair<Integer, String> getReturnType(FunctionCall f) {
		int type = functionNamesToTypes.get(f.getName().toString()).get(0);
		return Pair.make(type, getStringForType(type));
	}

	private void record(Node lhs, Node rhs, Void context) {
		if (lhs instanceof QualifiedNameReference && rhs instanceof QualifiedNameReference) {
			TypedNode target = record(process(lhs, context));
			TypedNode src = record(process(rhs, context));
			graph.addEdge(src, target);
			graph.addEdge(target, src);
		} else {
			Node column = null;
			if (lhs instanceof QualifiedNameReference) {
				column = lhs;
			} else if (rhs instanceof QualifiedNameReference) {
				column = rhs;
			}
			if (column == null) {
				return;
			}
			TypedNode target = record(process(column, context));

			// the other side can either be a literal or a function call
			Integer type = null;
			TypedNode src = null;
			if (lhs instanceof Literal || rhs instanceof Literal) {
				Literal l = null;
				if (lhs instanceof Literal) {
					l = (Literal) lhs;
				} else if (rhs instanceof Literal) {
					l = (Literal) rhs;
				}
				type = getType(l).fst;
				src = record(getType(l).snd);
			} else if (rhs instanceof FunctionCall || lhs instanceof FunctionCall) {
				FunctionCall f = null;
				if (!functionNameMap.containsKey(f.getName().toString())) {
					return;
				}
				if (rhs instanceof FunctionCall) {
					f = (FunctionCall) rhs;
				} else if (lhs instanceof FunctionCall) {
					f = (FunctionCall) lhs;
				}
				type = getReturnType(f).fst;
				src = record(getReturnType(f).snd);
			}

			src.types.add(type);
			graph.addEdge(src, target);
		}
	}

	@Override
	protected String visitComparisonExpression(ComparisonExpression node, Void context) {
		record(node.getLeft(), node.getRight(), context);

		return "(" + processOp(node.getType().getValue()) + " " + process(node.getLeft(), context) + " "
				+ process(node.getRight(), context) + ")";
	}

	@Override
	protected String visitQualifiedNameReference(QualifiedNameReference node, Void context) {
		if (!colNamesToInt.containsKey(node.getName().toString())) {
			columnNumber += 1;
			colNamesToInt.put(node.getName().toString(), columnNumber);
		}
		return "col" + colNamesToInt.get(node.getName().toString());

	}

	@Override
	protected String visitInPredicate(InPredicate node, Void context) {
		InListExpression l = (InListExpression) node.getValueList();
		record(node.getValue(), l.getValues().get(0), context);

		String args = process(node.getValueList(), context);
		return "(" + "in" + " " + process(node.getValue(), context) + " " + args + ")";
	}

	private Literal getLiteralForType(int type) {
		switch (type) {
		case 1:
			return new LongLiteral("0");
		case 2:
			return new TimeLiteral("0");
		case 3:
			return new StringLiteral("0");
		case 4:
			return new BooleanLiteral("true");
		case 5:
			return new DoubleLiteral("0");
		case 6:
			return new GenericLiteral("any", "0");
		default:
			return new LongLiteral("0");
		}
	}

	@Override
	protected String visitFunctionCall(FunctionCall node, Void context) {
		if (!functionNameMap.containsKey(node.getName().toString())) {
			throw new UnsupportedOperationException();
		}
		functions.add(node.getName().toString());
		List<Expression> l = node.getArguments();

		int pos = -1;
		Expression col = null;
		for (Expression e : l) {
			pos++;
			if (e instanceof QualifiedNameReference) {
				col = e;
			}
		}
		if (functionNameMap.containsKey(node.getName().toString())) {
			String key = functionNameMap.get(node.getName().toString());
			if (pos > -1 && col != null && functionNamesToTypes.containsKey(key)) {
				int type = functionNamesToTypes.get(key).get(pos);
				record(col, getLiteralForType(type), context);
			}
		}

		String args = createArgs(context, l);

		return "(" + node.getName().toString() + " " + args + ")";
	}

	@Override
	protected String visitCurrentTime(CurrentTime node, Void context) {
		return "(" + "currentTime" + ")";
	}

	@Override
	protected String visitIntervalLiteral(IntervalLiteral node, Void context) {
		return "(" + "interval" + " " + node.getValue() + " " + node.getStartField() + " " + node.getEndField() + ")";
	}

	private String createArgs(Void context, List<Expression> l) {
		StringBuffer buf = new StringBuffer();

		for (int i = 0; i < l.size(); i++) {
			buf.append(process(l.get(i), context)).append(" ");
		}
		return buf.toString();
	}

	@Override
	protected String visitInListExpression(InListExpression node, Void context) {
		String str = createArgs(context, node.getValues());
		return "(" + str + ")";
	}

	@Override
	protected String visitNullIfExpression(NullIfExpression node, Void context) {
		String lhs = process(node.getFirst(), context);
		String rhs = process(node.getSecond(), context);
		return "(" + "if" + " (not (= " + lhs + " " + rhs + ")) " + "lhs" + " " + "'())";
	}

	@Override
	protected String visitIfExpression(IfExpression node, Void context) {
		Optional<Expression> isFalse = node.getFalseValue();

		String rhsExpr;
		if (isFalse.isPresent()) {
			rhsExpr = process(isFalse.get(), context);
		} else {
			rhsExpr = "NULL";
		}
		return "(" + "if" + " " + process(node.getCondition(), context) + " " + process(node.getTrueValue(), context)
				+ " " + rhsExpr + ")";
	}

	@Override
	protected String visitNegativeExpression(NegativeExpression node, Void context) {
		return "-" + process(node.getValue(), context);
	}

	@Override
	protected String visitNotExpression(NotExpression node, Void context) {
		return "(" + "not" + " " + process(node.getValue(), context) + ")";
	}

	@Override
	protected String visitLikePredicate(LikePredicate node, Void context) {
		List<Expression> args = new LinkedList<Expression>();
		args.add(node.getValue());
		args.add(node.getPattern());
		if (node.getEscape() != null) {
			args.add(node.getEscape());
		}
		FunctionCall fc = new FunctionCall(new QualifiedName("like"), args);
		return visitFunctionCall(fc, context);
	}

	@Override
	protected String visitIsNotNullPredicate(IsNotNullPredicate node, Void context) {
		return "(" + "isNotNull" + " " + process(node.getValue(), context) + ")";

	}

	@Override
	protected String visitIsNullPredicate(IsNullPredicate node, Void context) {
		record(node.getValue(), getLiteralForType(6), context);
		return "(" + "isNull" + " " + process(node.getValue(), context) + ")";
	}

	@Override
	protected String visitExtract(Extract node, Void context) {

		String date = process(node.getExpression(), context);
		String field = node.getField().toString();
		record(node.getExpression(), getLiteralForType(2), context);
		
		return "(" + "extractFromDate" + " " + date + " " + field + ")";
	}

	@Override
	protected String visitCast(Cast node, Void context) {
		// Racket wont support casts
		return process(node.getExpression(), context);
	}

	@Override
	protected String visitCoalesceExpression(CoalesceExpression node, Void context) {
		FunctionCall fc = new FunctionCall(new QualifiedName("coalesce"), node.getOperands());
		return visitFunctionCall(fc, context);

	}

	@Override
	protected String visitSearchedCaseExpression(SearchedCaseExpression node, Void context) {
		String result = process(node.getDefaultValue(), context);
		List<WhenClause> whenclauses = node.getWhenClauses();
		for (int i = whenclauses.size() - 1; i >= 0; i--) {
			result = "(" + "if" + " " + process(whenclauses.get(i).getOperand(), context) + " "
					+ process(whenclauses.get(i).getResult(), context) + " " + result + ")";
		}
		return result;
	}

	@Override
	protected String visitDoubleLiteral(DoubleLiteral node, Void context) {
		// TODO Auto-generated method stub
		return "" + node.getValue();
	}

	@Override
	protected String visitGenericLiteral(GenericLiteral node, Void context) {
		// TODO Auto-generated method stub
		return "\"" + node.getValue() + "\"";
	}

	@Override
	protected String visitTimeLiteral(TimeLiteral node, Void context) {
		// TODO Auto-generated method stub
		return "\"" + node.getValue() + "\"";
	}

	@Override
	protected String visitTimestampLiteral(TimestampLiteral node, Void context) {
		// TODO Auto-generated method stub
		return "\"" + node.getValue() + "\"";
	}

	@Override
	protected String visitStringLiteral(StringLiteral node, Void context) {
		// TODO Auto-generated method stub
		return "\"" + node.getValue() + "\"";
	}

	@Override
	protected String visitBooleanLiteral(BooleanLiteral node, Void context) {
		// TODO Auto-generated method stub
		String str = node.getValue() ? "#t" : "#f";
		return str;
	}

	@Override
	protected String visitNullLiteral(NullLiteral node, Void context) {
		return "'()";
	}

	@Override
	protected String visitLongLiteral(LongLiteral node, Void context) {
		// TODO Auto-generated method stub
		return "" + node.getValue();
	}

	@Override
	protected String visitLogicalBinaryExpression(LogicalBinaryExpression node, Void context) {
		String op;
		if (node.getType().toString().equals("AND")) {
			op = "and";
		} else {
			op = "or";
		}
		return "(" + op + " " + process(node.getLeft(), context) + " " + process(node.getRight(), context) + ")";
	}

	public static final class TypedNode {
		protected String name;
		protected Set<Integer> types = new HashSet<Integer>();

		public TypedNode(String n) {
			this.name = n;
		}

		@Override
		public int hashCode() {
			return name.hashCode();
		}

		@Override
		public boolean equals(Object obj) {
			if (obj instanceof TypedNode) {
				return name.equals(((TypedNode) obj).name);
			}
			return false;
		}
		
		@Override
		public String toString() {
			return name + ":" + types;
		}

	}
}
