package sqlAnalysis;

import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import com.facebook.presto.sql.tree.AliasedRelation;
import com.facebook.presto.sql.tree.ArithmeticExpression;
import com.facebook.presto.sql.tree.BetweenPredicate;
import com.facebook.presto.sql.tree.BooleanLiteral;
import com.facebook.presto.sql.tree.Cast;
import com.facebook.presto.sql.tree.CoalesceExpression;
import com.facebook.presto.sql.tree.ComparisonExpression;
import com.facebook.presto.sql.tree.CurrentTime;
import com.facebook.presto.sql.tree.DefaultTraversalVisitor;
import com.facebook.presto.sql.tree.DoubleLiteral;
import com.facebook.presto.sql.tree.Except;
import com.facebook.presto.sql.tree.ExistsPredicate;
import com.facebook.presto.sql.tree.Expression;
import com.facebook.presto.sql.tree.Extract;
import com.facebook.presto.sql.tree.FunctionCall;
import com.facebook.presto.sql.tree.GenericLiteral;
import com.facebook.presto.sql.tree.IfExpression;
import com.facebook.presto.sql.tree.InListExpression;
import com.facebook.presto.sql.tree.InPredicate;
import com.facebook.presto.sql.tree.Intersect;
import com.facebook.presto.sql.tree.IntervalLiteral;
import com.facebook.presto.sql.tree.IsNotNullPredicate;
import com.facebook.presto.sql.tree.IsNullPredicate;
import com.facebook.presto.sql.tree.Join;
import com.facebook.presto.sql.tree.JoinCriteria;
import com.facebook.presto.sql.tree.JoinOn;
import com.facebook.presto.sql.tree.LikePredicate;
import com.facebook.presto.sql.tree.LogicalBinaryExpression;
import com.facebook.presto.sql.tree.LogicalBinaryExpression.Type;
import com.facebook.presto.sql.tree.LongLiteral;
import com.facebook.presto.sql.tree.NegativeExpression;
import com.facebook.presto.sql.tree.NotExpression;
import com.facebook.presto.sql.tree.NullIfExpression;
import com.facebook.presto.sql.tree.NullLiteral;
import com.facebook.presto.sql.tree.QualifiedName;
import com.facebook.presto.sql.tree.QualifiedNameReference;
import com.facebook.presto.sql.tree.Query;
import com.facebook.presto.sql.tree.QueryBody;
import com.facebook.presto.sql.tree.QuerySpecification;
import com.facebook.presto.sql.tree.Relation;
import com.facebook.presto.sql.tree.SearchedCaseExpression;
import com.facebook.presto.sql.tree.SelectItem;
import com.facebook.presto.sql.tree.SimpleCaseExpression;
import com.facebook.presto.sql.tree.SingleColumn;
import com.facebook.presto.sql.tree.Statement;
import com.facebook.presto.sql.tree.StringLiteral;
import com.facebook.presto.sql.tree.SubqueryExpression;
import com.facebook.presto.sql.tree.TableSubquery;
import com.facebook.presto.sql.tree.TimeLiteral;
import com.facebook.presto.sql.tree.TimestampLiteral;
import com.facebook.presto.sql.tree.Union;
import com.facebook.presto.sql.tree.WhenClause;
import com.google.common.base.Optional;
import com.ibm.wala.cast.tree.CAst;
import com.ibm.wala.cast.tree.CAstAnnotation;
import com.ibm.wala.cast.tree.CAstControlFlowMap;
import com.ibm.wala.cast.tree.CAstEntity;
import com.ibm.wala.cast.tree.CAstNode;
import com.ibm.wala.cast.tree.CAstNodeTypeMap;
import com.ibm.wala.cast.tree.CAstQualifier;
import com.ibm.wala.cast.tree.CAstSourcePositionMap;
import com.ibm.wala.cast.tree.CAstSourcePositionMap.Position;
import com.ibm.wala.cast.tree.CAstType;
import com.ibm.wala.cast.tree.impl.CAstControlFlowRecorder;
import com.ibm.wala.cast.tree.impl.CAstImpl;
import com.ibm.wala.cast.tree.impl.CAstOperator;
import com.ibm.wala.cast.tree.impl.CAstSourcePositionRecorder;
import com.ibm.wala.cast.tree.impl.CAstSymbolImpl;
import com.ibm.wala.cast.util.CAstPrinter;
import com.ibm.wala.util.collections.EmptyIterator;
import com.ibm.wala.util.collections.HashMapFactory;

public class PrestoVisitor {
	public static int expressionCount = 0;
	public static int statementCount = 0;
	public final static CAstOperator OP_AND = new SQLCAstOperator("and");
	public final static CAstOperator OP_OR = new SQLCAstOperator("or");

	protected static CAstOperator processOp(String op) {
		if ("+".equals(op)) {
			return CAstOperator.OP_ADD;
		} else if ("-".equals(op)) {
			return CAstOperator.OP_SUB;
		} else if ("*".equals(op)) {
			return CAstOperator.OP_MUL;
		} else if ("/".equals(op)) {
			return CAstOperator.OP_DIV;
		} else if ("%".equals(op)) {
			return CAstOperator.OP_MOD;
		} else if ("<".equals(op)) {
			return CAstOperator.OP_LT;
		} else if ("<=".equals(op)) {
			return CAstOperator.OP_LE;
		} else if (">".equals(op)) {
			return CAstOperator.OP_GT;
		} else if (">=".equals(op)) {
			return CAstOperator.OP_GE;
		} else if ("=".equals(op)) {
			return CAstOperator.OP_EQ;
		} else if ("!=".equals(op) || "<>".equals(op)) {
			return CAstOperator.OP_NE;
		} else if ("|".equals(op)) {
			return CAstOperator.OP_BIT_OR;
		} else if ("&".equals(op)) {
			return CAstOperator.OP_BIT_AND;
		} else if ("#".equals(op)) {
			return CAstOperator.OP_BIT_XOR;
		} else if ("~".equals(op)) {
			return CAstOperator.OP_BITNOT;
		} else {
			throw new UnsupportedOperationException("dont recognize:" + op);
		}
	}

	public static CAstEntity process(Statement st, String orig) {
		
		List<QuerySpecification> querySpec = new LinkedList<QuerySpecification>();
		getQuerySpecifications( ((Query) st), querySpec);

		List<CAstEntity> l = new LinkedList<CAstEntity>();

		ExpressionGatherer exp = new ExpressionGatherer();
		for (QuerySpecification q : querySpec) {
			CAstNode n = exp.process(q, new Context());
			if (n != null) {
				createEntity(l, n, exp.cfg());
			}

		}
		
		int myStatement = statementCount++;
		CAstEntity e = createFileEntity(l, myStatement);
		return e;
	}
	
	public static void getQuerySpecifications(Query query, List<QuerySpecification> sp) {
		QueryBody body = query.getQueryBody();
		List<Query> queries = new LinkedList<Query>();

		if (body instanceof TableSubquery) {
			getQuerySpecifications(((TableSubquery) body).getQuery(), sp);
		} else if (body instanceof QuerySpecification) {
			sp.add((QuerySpecification) body);	
			QuerySpecification b = (QuerySpecification) body;
			queries.addAll(getSubQueries(b));
		} else {
			if (body instanceof Intersect) {
				Intersect i = (Intersect) body;
				List<Relation> rels = i.getRelations();
				for (Relation r : rels) {
					getTableSubqueries(r, queries);
				}
			} else if (body instanceof Union) {
				Union u = (Union) body;
				List<Relation> rels = u.getRelations();
				for (Relation r : rels) {
					getTableSubqueries(r, queries);
				}
			} else if (body instanceof Except) {
				Except e = (Except) body;
				getTableSubqueries(e.getLeft(), queries);
				getTableSubqueries(e.getRight(), queries);
			}
		}
		for (Query q: queries) {
			getQuerySpecifications(q, sp);
		}
	}
	
	private static List<Query> getSubQueries(QuerySpecification qb) {
		List<Query> subs = new LinkedList<Query>();
		List<Relation> rels = qb.getFrom();
		for (Relation r : rels) {
			getTableSubqueries(r, subs);
		}
		return subs;
	}
	
	private static void getTableSubqueries(Relation r, List<Query> subs) {
		if (r instanceof Join) {
			getTableSubqueries(((Join) r).getLeft(), subs);
			getTableSubqueries(((Join) r).getRight(), subs);
		} else if (r instanceof AliasedRelation) {
			getTableSubqueries(((AliasedRelation) r).getRelation(), subs);
		} else if (r instanceof TableSubquery) {
			subs.add(((TableSubquery) r).getQuery());
		}
	}

	public static CAstEntity createFileEntity(List<CAstEntity> l, int myStatement) {
		return new CAstEntity() {

			@Override
			public int getKind() {
				return CAstEntity.FILE_ENTITY;
			}

			@Override
			public String getName() {
				return "statement" + myStatement;
			}

			@Override
			public String getSignature() {
				// TODO Auto-generated method stub
				return null;
			}

			@Override
			public String[] getArgumentNames() {
				return new String[0];
			}

			@Override
			public CAstNode[] getArgumentDefaults() {
				// TODO Auto-generated method stub
				return null;
			}

			@Override
			public int getArgumentCount() {
				// TODO Auto-generated method stub
				return 0;
			}

			@Override
			public Map<CAstNode, Collection<CAstEntity>> getAllScopedEntities() {
				Map<CAstNode, Collection<CAstEntity>> map = HashMapFactory.make();
				map.put(null, l);
				return map;
			}

			@Override
			public Iterator<CAstEntity> getScopedEntities(CAstNode construct) {
				if (construct == null) {
					return l.iterator();
				} else {
					return EmptyIterator.instance();
				}
			}

			@Override
			public CAstNode getAST() {
				// TODO Auto-generated method stub
				return null;
			}

			@Override
			public CAstControlFlowMap getControlFlow() {
				// TODO Auto-generated method stub
				return null;
			}

			@Override
			public CAstSourcePositionMap getSourceMap() {
				// TODO Auto-generated method stub
				return null;
			}

			@Override
			public Position getPosition() {
				// TODO Auto-generated method stub
				return null;
			}

			@Override
			public CAstNodeTypeMap getNodeTypeMap() {
				// TODO Auto-generated method stub
				return null;
			}

			@Override
			public Collection<CAstQualifier> getQualifiers() {
				// TODO Auto-generated method stub
				return null;
			}

			@Override
			public CAstType getType() {
				// TODO Auto-generated method stub
				return null;
			}

			@Override
			public Collection<CAstAnnotation> getAnnotations() {
				// TODO Auto-generated method stub
				return null;
			}

		};
	}

	public static void createEntity(List<CAstEntity> l, CAstNode n, CAstControlFlowMap cfg) {
		int myNumber = expressionCount++;
		l.add(new CAstEntity() {

			public CAstType getType() {
				return SQLCAstToIRTranslator.Any;
			}

			public CAstSourcePositionMap getSourceMap() {
				// TODO Auto-generated method stub
				return null;
			}

			@Override
			public String getSignature() {
				// TODO Auto-generated method stub
				return null;
			}

			@Override
			public Iterator<CAstEntity> getScopedEntities(CAstNode construct) {
				return EmptyIterator.instance();
			}

			@Override
			public Collection<CAstQualifier> getQualifiers() {
				return Collections.emptySet();
			}

			@Override
			public Position getPosition() {
				// TODO Auto-generated method stub
				return null;
			}

			@Override
			public CAstNodeTypeMap getNodeTypeMap() {
				// TODO Auto-generated method stub
				return null;
			}

			@Override
			public String getName() {
				return "expression" + myNumber;
			}

			@Override
			public int getKind() {
				return CAstEntity.FUNCTION_ENTITY;
			}

			@Override
			public CAstControlFlowMap getControlFlow() {
				return cfg;
			}

			@Override
			public String[] getArgumentNames() {
				return new String[0];
			}

			@Override
			public CAstNode[] getArgumentDefaults() {
				// TODO Auto-generated method stub
				return new CAstNode[0];
			}

			@Override
			public int getArgumentCount() {
				return 0;
			}

			@Override
			public Collection<CAstAnnotation> getAnnotations() {
				return Collections.emptySet();
			}

			@Override
			public Map<CAstNode, Collection<CAstEntity>> getAllScopedEntities() {

				return Collections.emptyMap();
			}

			@Override
			public CAstNode getAST() {
				return n;
			}
		});
	}
	
	protected static final class Context {
		
		protected Context parent = null;
		
		protected Boolean isExists = null;
		
		protected Boolean isIn = null;
		
		protected Boolean isComparison = null;
		
		protected Boolean isNegative = null;

		public Boolean getIsNegative() {
			return isNegative;
		}

		public void setIsNegative(Boolean isNegative) {
			this.isNegative = isNegative;
		}

		public Context() {
		}
		
		public Context(Context p) {
			this.parent = p;
		}

		public boolean isExists() {
			if (isExists != null) {
				return isExists;
			} else if (parent != null) {
				return parent.isExists();
			} else {
				return false;
			}
		}

		public void setExists(boolean isExists) {
			this.isExists = isExists;
		}

		public boolean isIn() {
			if (isIn != null) {
				return isIn;
			} else if (parent != null) {
				return parent.isIn();
			} else {
				return false;
			}
		}

		public void setIn(boolean isIn) {
			this.isIn = isIn;
		}

		public boolean isComparison() {
			if (isComparison != null) {
				return isComparison;
			} else if (parent != null) {
				return parent.isComparison();
			} else {
				return false;
			}
		}

		public void setComparison(boolean isComparison) {
			this.isComparison = isComparison;
		}

		public boolean isTop() {
			return parent == null;
		}
		
	}

	protected static final class ExpressionGatherer extends DefaultTraversalVisitor<CAstNode, Context> {

		protected final CAst factory = new CAstImpl();
		
		protected CAstNode NULL = factory.makeConstant(CAstNode.VOID);

		private final CAstSourcePositionRecorder pos = new CAstSourcePositionRecorder();

		private final CAstControlFlowRecorder rec = new CAstControlFlowRecorder(pos);
		
		// KAVITHA: This is a hack to synchronize function names between synthesis and SQL
		private SExpressionWriter writer = new SExpressionWriter(0);

		
		public ExpressionGatherer() {
		}

		public CAstControlFlowMap cfg() {
			return rec;
		}

		@Override
		protected CAstNode visitJoin(Join node, Context context) {
			Optional<JoinCriteria> join = node.getCriteria();
			if (join.isPresent()) {
				if (join.get() instanceof JoinOn) {
					System.out.println(((JoinOn) join.get()).getExpression());
					return process(((JoinOn) join.get()).getExpression(), context);
				}
			}
			return null;
		}
		@Override
		protected CAstNode visitSingleColumn(SingleColumn node, Context context) {
			String colName = node.getExpression().toString();
			return factory.makeNode(CAstNode.OBJECT_REF, factory.makeNode(CAstNode.VOID),
					factory.makeConstant(colName));
		}

		@Override
		protected CAstNode visitSimpleCaseExpression(SimpleCaseExpression node, Context context) {
			CAstNode result = process(node.getDefaultValue(), context);
			List<WhenClause> whenclauses = node.getWhenClauses();
			for (int i = whenclauses.size() - 1; i >= 0; i--) {
				result = factory.makeNode(CAstNode.IF_EXPR, process(whenclauses.get(i).getOperand(), context),
						process(whenclauses.get(i).getResult(), context), result);
			}
			return result;
		}
	

		@Override
		protected CAstNode visitQuerySpecification(QuerySpecification node, Context context) {
			List<SelectItem> items = node.getSelect().getSelectItems();
			List<CAstNode> l = new LinkedList<CAstNode>();
			
			for (SelectItem i : items) {
				if (i instanceof SingleColumn) {
					Expression e = ((SingleColumn) i).getExpression();
					if (!(e instanceof QualifiedNameReference)) {
						l.add(process(e, context));
					}
				}
			}
			CAstNode select = null;
			if (!l.isEmpty()) {
				select = factory.makeNode(CAstNode.ARRAY_LITERAL, l.toArray(new CAstNode[l.size()]));
			}
			
			CAstNode where = null;
			
			if (node.getWhere().isPresent()) {
				where = process(node.getWhere().get(), context);
			}
			
			for (Relation r : node.getFrom()) {
				if (r instanceof Join) {
					CAstNode join = visitJoin((Join) r, null);
					if (where == null) {
						where = join;
					} else {
						where = factory.makeNode(CAstNode.ANDOR_EXPR, OP_AND, where, join);
					}
				}
			}
			
			return createQueryNode(select, where, context);
		}
		
		private CAstNode createQueryNode(CAstNode select, CAstNode where, Context context) {
			if (context.isTop()) {
				CAstNode query = null;	
				if (where != null && select != null) {
					query = factory.makeNode(CAstNode.IF_EXPR, where, select);
				} else if (where != null && select == null) {
					query = factory.makeNode(CAstNode.IF_EXPR, where, factory.makeConstant(true), factory.makeConstant(false));
				} else if (select != null) {
					query = factory.makeNode(CAstNode.IF_EXPR, factory.makeConstant(true), select);
				}
				return query;
			} else if (context.isExists()) {
				return where;
			} else if (context.isComparison()) {
				assert select.getChildCount() == 1;
				return select.getChild(0);
			} else if (context.isIn()) {
				assert select.getChildCount() == 1;
				return select.getChild(0);
			}
			return null;
		}

		@Override
		protected CAstNode visitArithmeticExpression(ArithmeticExpression node, Context context) {
			return factory.makeNode(CAstNode.BINARY_EXPR, processOp(node.getType().getValue()),
					process(node.getLeft(), context), process(node.getRight(), context));
		}

		@Override
		protected CAstNode visitBetweenPredicate(BetweenPredicate node, Context context) {
			Expression e1 = new ComparisonExpression(ComparisonExpression.Type.LESS_THAN, node.getValue(),
					node.getMax());
			Expression e2 = new ComparisonExpression(ComparisonExpression.Type.GREATER_THAN, node.getValue(),
					node.getMin());
			LogicalBinaryExpression e3 = new LogicalBinaryExpression(Type.AND, e1, e2);
			return process(e3, context);
		}
		
		@Override
		protected CAstNode visitQuery(Query node, Context context) {
			List<QuerySpecification> querySpec = new LinkedList<QuerySpecification>();
			getQuerySpecifications(node, querySpec);
			assert querySpec.size() == 1;
			return visitQuerySpecification(querySpec.get(0), context);
		}


		@Override
		protected CAstNode visitSubqueryExpression(SubqueryExpression node, Context context) {
			Context c = new Context(context);
			return process(node.getQuery(), c);
		}

		@Override
		protected CAstNode visitExists(ExistsPredicate node, Context context) {
			Context c = new Context(context);
			c.setExists(true);
			return visitQuery(node.getSubquery(), c);
		}

		@Override
		protected CAstNode visitComparisonExpression(ComparisonExpression node, Context context) {
			Context c = new Context(context);
			c.setComparison(true);
			return factory.makeNode(CAstNode.BINARY_EXPR, processOp(node.getType().getValue()),
					process(node.getLeft(), c), process(node.getRight(), c));
		}

		@Override
		protected CAstNode visitQualifiedNameReference(QualifiedNameReference node, Context context) {
			String colName = node.getName().toString();
			return factory.makeNode(CAstNode.OBJECT_REF, factory.makeNode(CAstNode.VOID),
					factory.makeConstant(colName));
		}

		@Override
		protected CAstNode visitInPredicate(InPredicate node, Context context) {
			Context c = new Context(context);
			c.setIn(true);
			
			CAstNode args = process(node.getValueList(), c);
			CAstNode[] arr = new CAstNode[ 3 + args.getChildCount() ];
			arr[0] = factory.makeNode(CAstNode.VOID);
			arr[1] = factory.makeConstant("in-list");
			arr[2] = process(node.getValue(), c);
			for(int i = 0; i < args.getChildCount(); i++) {
				arr[i+3] = args.getChild(i);
			}
			
			return factory.makeNode(CAstNode.CALL, arr);

		}

		@Override
		protected CAstNode visitFunctionCall(FunctionCall node, Context context) {
			List<Expression> l = node.getArguments();
			CAstNode[] arr = createArgs(context, l);
			
			CAstNode[] allArgs = new CAstNode[ arr.length + 2 ];
			allArgs[0] = factory.makeConstant(CAstNode.VOID);
			
			String n = node.getName().toString();
			if (writer.functionNameMap.containsKey(n)) {
			  n = writer.functionNameMap.get(n);
			  assert (writer.functionNameNormalizedMap.containsKey(n));
			  n = writer.functionNameNormalizedMap.get(n);
			  
			}
			allArgs[1] = factory.makeConstant(n);
			System.arraycopy(arr, 0, allArgs, 2, arr.length);

			return factory.makeNode(CAstNode.CALL, allArgs);
		}

		@Override
		protected CAstNode visitCurrentTime(CurrentTime node, Context context) {
			CAstNode[] arr = new CAstNode[2];
			arr[0] = factory.makeConstant(CAstNode.VOID);
			arr[1] = factory.makeConstant("currentTime");
			return factory.makeNode(CAstNode.CALL, arr);
		}

		@Override
		protected CAstNode visitIntervalLiteral(IntervalLiteral node, Context context) {
			CAstNode[] arr = new CAstNode[2];
			arr[0] = factory.makeConstant(CAstNode.VOID);
			arr[1] = factory.makeConstant("interval");
			return factory.makeNode(CAstNode.CALL, arr);
		}

		private CAstNode[] createArgs(Context context, List<Expression> l) {
			CAstNode[] arr = new CAstNode[l.size()];
			for (int i = 0; i < l.size(); i++) {
				arr[i] = process(l.get(i), context);
			}
			return arr;
		}

		@Override
		protected CAstNode visitInListExpression(InListExpression node, Context context) {
			CAstNode[] arr = createArgs(context, node.getValues());
			return factory.makeNode(CAstNode.ARRAY_LITERAL, arr);
		}

		@Override
		protected CAstNode visitNullIfExpression(NullIfExpression node, Context context) {
			CAstNode lhs = process(node.getFirst(), context);
			CAstNode rhs = process(node.getSecond(), context);
			CAstNode decl = factory.makeNode(CAstNode.DECL_STMT,
					factory.makeConstant(new CAstSymbolImpl("if null temp", SQLCAstToIRTranslator.Any, lhs)));
			CAstNode var = factory.makeNode(CAstNode.VAR, factory.makeConstant("if null temp"));
			CAstNode l = factory.makeNode(CAstNode.BINARY_EXPR, CAstOperator.OP_EQ, var, NULL);
			return factory.makeNode(CAstNode.LOCAL_SCOPE,
					factory.makeNode(CAstNode.BLOCK_EXPR, decl, factory.makeNode(CAstNode.IF_EXPR, l, var, rhs)));
		}

		@Override
		protected CAstNode visitIfExpression(IfExpression node, Context context) {
			Optional<Expression> isFalse = node.getFalseValue();
			CAstNode rhsExpr = null;
			if (isFalse.isPresent()) {
				rhsExpr = process(isFalse.get(), context);
			} else {
				rhsExpr = NULL;
			}
			return factory.makeNode(CAstNode.IF_EXPR, process(node.getCondition(), context),
					process(node.getTrueValue(), context), rhsExpr);
		}
		
		
		@Override
		protected CAstNode visitNegativeExpression(NegativeExpression node, Context context) {
			// KAVITHA - the fact that this is a negative expression is not of consequence to us
			return process(node.getValue(), context);
		}

		@Override
		protected CAstNode visitNotExpression(NotExpression node, Context context) {
			return factory.makeNode(CAstNode.UNARY_EXPR, CAstOperator.OP_NOT, process(node.getValue(), context));
		}

		@Override
		protected CAstNode visitLikePredicate(LikePredicate node, Context context) {
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
		protected CAstNode visitIsNotNullPredicate(IsNotNullPredicate node, Context context) {
			return factory.makeNode(CAstNode.BINARY_EXPR, CAstOperator.OP_NE, process(node.getValue(), context), NULL);
		}

		@Override
		protected CAstNode visitIsNullPredicate(IsNullPredicate node, Context context) {
			return factory.makeNode(CAstNode.BINARY_EXPR, CAstOperator.OP_EQ, process(node.getValue(), context), NULL);
		}

		@Override
		protected CAstNode visitExtract(Extract node, Context context) {
			CAstNode[] arr = new CAstNode[2];
			arr[0] = factory.makeNode(CAstNode.VOID);
			arr[1] = factory.makeConstant("EXTRACT");
			arr[2] = process(node.getExpression(), context);
			arr[3] = factory.makeConstant(node.getField().toString());
			return factory.makeNode(CAstNode.CALL, arr);
		}

		@Override
		protected CAstNode visitCast(Cast node, Context context) {
			return factory.makeNode(CAstNode.CAST, process(node.getExpression(), context),
					factory.makeConstant(node.getType()));
		}

		@Override
		protected CAstNode visitCoalesceExpression(CoalesceExpression node, Context context) {
			FunctionCall fc = new FunctionCall(new QualifiedName("coalesce"), node.getOperands());
			return visitFunctionCall(fc, context);

		}

		@Override
		protected CAstNode visitSearchedCaseExpression(SearchedCaseExpression node, Context context) {
			CAstNode result = process(node.getDefaultValue(), context);
			List<WhenClause> whenclauses = node.getWhenClauses();
			for (int i = whenclauses.size() - 1; i >= 0; i--) {
				result = factory.makeNode(CAstNode.IF_EXPR, process(whenclauses.get(i).getOperand(), context),
						process(whenclauses.get(i).getResult(), context), result);
			}
			return result;
		}

		@Override
		protected CAstNode visitDoubleLiteral(DoubleLiteral node, Context context) {
			// TODO Auto-generated method stub
			return factory.makeConstant(node.getValue());
		}

		@Override
		protected CAstNode visitGenericLiteral(GenericLiteral node, Context context) {
			// TODO Auto-generated method stub
			return factory.makeConstant(node.getValue());
		}

		@Override
		protected CAstNode visitTimeLiteral(TimeLiteral node, Context context) {
			// TODO Auto-generated method stub
			return factory.makeConstant(node.getValue());
		}

		@Override
		protected CAstNode visitTimestampLiteral(TimestampLiteral node, Context context) {
			// TODO Auto-generated method stub
			return factory.makeConstant(node.getValue());
		}

		@Override
		protected CAstNode visitStringLiteral(StringLiteral node, Context context) {
			// TODO Auto-generated method stub
			return factory.makeConstant(node.getValue());
		}

		@Override
		protected CAstNode visitBooleanLiteral(BooleanLiteral node, Context context) {
			// TODO Auto-generated method stub
			return factory.makeConstant(node.getValue());
		}

		@Override
		protected CAstNode visitNullLiteral(NullLiteral node, Context context) {
			// TODO Auto-generated method stub
			return NULL;
		}

		@Override
		protected CAstNode visitLongLiteral(LongLiteral node, Context context) {
			// TODO Auto-generated method stub
			return factory.makeConstant(node.getValue());
		}

		@Override
		protected CAstNode visitLogicalBinaryExpression(LogicalBinaryExpression node, Context context) {
			CAstOperator op;
			if (node.getType().toString().equals("AND")) {
				op = OP_AND;
			} else {
				op = OP_OR;
			}
			return factory.makeNode(CAstNode.ANDOR_EXPR, op, process(node.getLeft(), context),
					process(node.getRight(), context));
		}
	}

	public static class SQLCAstOperator extends CAstOperator {

		protected SQLCAstOperator(String op) {
			super(op);
			// TODO Auto-generated constructor stub
		}

	}
}
