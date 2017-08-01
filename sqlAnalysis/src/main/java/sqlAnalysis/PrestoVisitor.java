package sqlAnalysis;

import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import com.facebook.presto.sql.tree.ArithmeticExpression;
import com.facebook.presto.sql.tree.BetweenPredicate;
import com.facebook.presto.sql.tree.BooleanLiteral;
import com.facebook.presto.sql.tree.Cast;
import com.facebook.presto.sql.tree.CoalesceExpression;
import com.facebook.presto.sql.tree.ComparisonExpression;
import com.facebook.presto.sql.tree.DefaultTraversalVisitor;
import com.facebook.presto.sql.tree.DoubleLiteral;
import com.facebook.presto.sql.tree.ExistsPredicate;
import com.facebook.presto.sql.tree.Expression;
import com.facebook.presto.sql.tree.Extract;
import com.facebook.presto.sql.tree.FunctionCall;
import com.facebook.presto.sql.tree.GenericLiteral;
import com.facebook.presto.sql.tree.IfExpression;
import com.facebook.presto.sql.tree.InListExpression;
import com.facebook.presto.sql.tree.IsNotNullPredicate;
import com.facebook.presto.sql.tree.IsNullPredicate;
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
import com.facebook.presto.sql.tree.QuerySpecification;
import com.facebook.presto.sql.tree.Relation;
import com.facebook.presto.sql.tree.SearchedCaseExpression;
import com.facebook.presto.sql.tree.SelectItem;
import com.facebook.presto.sql.tree.SimpleCaseExpression;
import com.facebook.presto.sql.tree.SingleColumn;
import com.facebook.presto.sql.tree.Statement;
import com.facebook.presto.sql.tree.StringLiteral;
import com.facebook.presto.sql.tree.SubqueryExpression;
import com.facebook.presto.sql.tree.TimeLiteral;
import com.facebook.presto.sql.tree.TimestampLiteral;
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
import com.ibm.wala.cast.tree.impl.CAstImpl;
import com.ibm.wala.cast.tree.impl.CAstOperator;
import com.ibm.wala.cast.tree.impl.CAstSymbolImpl;
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
		} else if ("!=".equals(op)) {
			return CAstOperator.OP_NE;
		} else if ("|".equals(op)) {
			return CAstOperator.OP_BIT_OR;
		} else if ("&".equals(op)) {
			return CAstOperator.OP_BIT_AND;
		} else if ("#".equals(op)) {
			return CAstOperator.OP_BIT_XOR;
		} else if ("~".equals(op)) {
			return CAstOperator.OP_BITNOT;
		} 
		else {
			throw new UnsupportedOperationException("dont recognize:" + op);
		}
	}
	
	public static CAstEntity process(Statement st, String orig) {
		Query query = (Query) st;
		System.out.println("Presto passed:" + orig);

		List<CAstEntity> l = new LinkedList<CAstEntity>();
		QuerySpecification qb = (QuerySpecification) query.getQueryBody();
		List<SelectItem> items = qb.getSelect().getSelectItems();
		for (SelectItem i : items) {
			if (i instanceof SingleColumn) {
				Expression e = ((SingleColumn) i).getExpression();
				if (!(e instanceof QualifiedNameReference)) {
					ExpressionGatherer exp = new ExpressionGatherer();
					CAstNode n = exp.process(e, null);
					createEntity(l, n);
				}
			}
		}
		Optional<Expression> where = qb.getWhere();
		if (where.isPresent()) {
			ExpressionGatherer exp = new ExpressionGatherer();
			System.out.println("WHERE CLAUSE PROCESSSED AS CAST NODE:" + exp.process(where.get(), null));
			createEntity(l, exp.process(where.get(), null));
		}
		int myStatement = statementCount++;
		return createFileEntity(l, myStatement);
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
				// TODO Auto-generated method stub
				return null;
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

	public static void createEntity(List<CAstEntity> l, CAstNode n) {
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
				// TODO Auto-generated method stub
				return null;
			}
			
			@Override
			public String[] getArgumentNames() {
				// TODO Auto-generated method stub
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
				return  Collections.emptySet();
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

	protected static final class ExpressionGatherer extends DefaultTraversalVisitor<CAstNode, Void> {

		protected final CAst factory = new CAstImpl();
		protected CAstNode NULL = factory.makeConstant(CAstNode.VOID);

		
		@Override
		protected CAstNode visitSingleColumn(SingleColumn node, Void context) {
			// TODO Auto-generated method stub
			String colName = node.getExpression().toString();
			return factory.makeNode(CAstNode.OBJECT_REF, factory.makeNode(CAstNode.VOID),
					factory.makeConstant(colName));
		}

		@Override
		protected CAstNode visitSimpleCaseExpression(SimpleCaseExpression node, Void context) {
			CAstNode result = process(node.getDefaultValue(), context);		
			List<WhenClause> whenclauses = node.getWhenClauses();
			for (int i = whenclauses.size() -1; i >= 0; i--) {
				result = factory.makeNode(CAstNode.IF_EXPR, process(whenclauses.get(i).getOperand(), context),
						process(whenclauses.get(i).getResult(), context), result);
			}
			return result;
		}

		

		@Override
		protected CAstNode visitArithmeticExpression(ArithmeticExpression node, Void context) {
			return factory.makeNode(CAstNode.BINARY_EXPR, processOp(node.getType().getValue()),
					process(node.getLeft(), context), process(node.getRight(), context));
		}

		@Override
		protected CAstNode visitBetweenPredicate(BetweenPredicate node, Void context) {
			Expression e1 = new ComparisonExpression(ComparisonExpression.Type.LESS_THAN, node.getValue(),
					node.getMax());
			Expression e2 = new ComparisonExpression(ComparisonExpression.Type.GREATER_THAN, node.getValue(),
					node.getMin());
			LogicalBinaryExpression e3 = new LogicalBinaryExpression(Type.AND, e1, e2);
			return process(e3, context);
		}

		protected CAstNode visitPartQuery(Query node, Void context) {
			CAstNode e = null;
			QuerySpecification qb = (QuerySpecification) node.getQueryBody();
	        process(qb.getSelect(), context);
	        if (qb.getWhere().isPresent()) {
	            e = process(qb.getWhere().get(), context);
	        }
	        
	        return e;
		}
		
		@Override
		protected CAstNode visitSubqueryExpression(SubqueryExpression node, Void context) {
			return visitQuery(node.getQuery(), context);
		}

		@Override
		protected CAstNode visitExists(ExistsPredicate node, Void context) {
			// TODO Auto-generated method stub
			return visitPartQuery(node.getSubquery(), context);
		}

		@Override
		protected CAstNode visitComparisonExpression(ComparisonExpression node, Void context) {
			return factory.makeNode(CAstNode.BINARY_EXPR, processOp(node.getType().getValue()),
					process(node.getLeft(), context), process(node.getRight(), context));
		}

		@Override
		protected CAstNode visitQualifiedNameReference(QualifiedNameReference node, Void context) {
			// TODO Auto-generated method stub
			String colName = node.getName().toString();
			return factory.makeNode(CAstNode.OBJECT_REF, factory.makeNode(CAstNode.VOID),
					factory.makeConstant(colName));
		}

		@Override
		protected CAstNode visitFunctionCall(FunctionCall node, Void context) {
			// how do we deal with a list of arguments in the wala AST?
			List<Expression> l = node.getArguments();
			CAstNode[] arr = createArgs(context, l);
			return factory.makeNode(CAstNode.CALL, factory.makeConstant(node.getName().toString()), arr);
		}

		private CAstNode[] createArgs(Void context, List<Expression> l) {
			CAstNode[] arr = new CAstNode[l.size()];
			for (int i = 0; i < l.size(); i++) {
				arr[i] = process(l.get(i), context);
			}
			return arr;
		}

		@Override
		protected CAstNode visitInListExpression(InListExpression node, Void context) {
			CAstNode[] arr = createArgs(context, node.getValues());
			return factory.makeNode(CAstNode.CALL, factory.makeConstant("IN_LIST"), arr);
		}

		@Override
		protected CAstNode visitNullIfExpression(NullIfExpression node, Void context) {
			CAstNode lhs = process(node.getFirst(), context);
			CAstNode rhs = process(node.getSecond(), context);
			CAstNode decl = factory.makeNode(CAstNode.DECL_STMT, factory.makeConstant(new CAstSymbolImpl("if null temp", SQLCAstToIRTranslator.Any, lhs)));
			CAstNode var = factory.makeNode(CAstNode.VAR, factory.makeConstant("if null temp"));
			CAstNode l = factory.makeNode(CAstNode.BINARY_EXPR, CAstOperator.OP_EQ, var, NULL);
			return factory.makeNode(CAstNode.LOCAL_SCOPE, factory.makeNode(CAstNode.BLOCK_EXPR, decl, factory.makeNode(CAstNode.IF_EXPR, l, var, rhs)));
		}

		@Override
		protected CAstNode visitIfExpression(IfExpression node, Void context) {
			Optional<Expression> isFalse = node.getFalseValue();
			CAstNode rhsExpr = null;
			if (isFalse.isPresent()) {
				rhsExpr = process(isFalse.get(), context);
			} else {
				rhsExpr = NULL;
			}
			return factory.makeNode(CAstNode.IF_EXPR, process(node.getCondition(), context), process(node.getTrueValue(), context), rhsExpr);
		}

		@Override
		protected CAstNode visitNegativeExpression(NegativeExpression node, Void context) {
			NotExpression exp = new NotExpression(node.getValue());
			return visitNotExpression(exp, context);
		}

		@Override
		protected CAstNode visitNotExpression(NotExpression node, Void context) {
			return super.visitNotExpression(node, context);
		}

		@Override
		protected CAstNode visitLikePredicate(LikePredicate node, Void context) {
			List<Expression> args = new LinkedList<Expression>();
			args.add(node.getValue());
			args.add(node.getPattern());
			if (node.getEscape() != null) {
				args.add(node.getEscape());
			}
			FunctionCall fc = new FunctionCall(new QualifiedName("LIKE"), args);
			return visitFunctionCall(fc, context);
		}

		@Override
		protected CAstNode visitIsNotNullPredicate(IsNotNullPredicate node, Void context) {
			// TODO Auto-generated method stub
			return factory.makeNode(CAstNode.BINARY_EXPR, CAstOperator.OP_NE, process(node.getValue(), context),
					NULL);
		}

		@Override
		protected CAstNode visitIsNullPredicate(IsNullPredicate node, Void context) {
			return factory.makeNode(CAstNode.BINARY_EXPR, CAstOperator.OP_EQ, process(node.getValue(), context),
					NULL);
		}

		@Override
		protected CAstNode visitExtract(Extract node, Void context) {
			CAstNode[] arr = new CAstNode[2];
			arr[0] = process(node.getExpression(), context);
			arr[1] = factory.makeConstant(node.getField().toString());
			return factory.makeNode(CAstNode.CALL, factory.makeConstant("EXTRACT"), arr);
		}

		@Override
		protected CAstNode visitCast(Cast node, Void context) {
			return factory.makeNode(CAstNode.CAST, process(node.getExpression(), context), factory.makeConstant(node.getType()));
		}

		@Override
		protected CAstNode visitCoalesceExpression(CoalesceExpression node, Void context) {
			FunctionCall fc = new FunctionCall(new QualifiedName("COALESCE"), node.getOperands());
			return visitFunctionCall(fc, context);
			
		}

		@Override
		protected CAstNode visitSearchedCaseExpression(SearchedCaseExpression node, Void context) {
			List<Expression> exp = new LinkedList<Expression>();
			exp.add(node.getDefaultValue());
			exp.addAll(node.getWhenClauses());
			FunctionCall fc = new FunctionCall(new QualifiedName("COALESCE"), exp);
			return visitFunctionCall(fc, context);
		}

		@Override
		protected CAstNode visitDoubleLiteral(DoubleLiteral node, Void context) {
			// TODO Auto-generated method stub
			return factory.makeConstant(node.getValue());
		}

		@Override
		protected CAstNode visitGenericLiteral(GenericLiteral node, Void context) {
			// TODO Auto-generated method stub
			return factory.makeConstant(node.getValue());
		}

		@Override
		protected CAstNode visitTimeLiteral(TimeLiteral node, Void context) {
			// TODO Auto-generated method stub
			return factory.makeConstant(node.getValue());
		}

		@Override
		protected CAstNode visitTimestampLiteral(TimestampLiteral node, Void context) {
			// TODO Auto-generated method stub
			return factory.makeConstant(node.getValue());
		}

		@Override
		protected CAstNode visitStringLiteral(StringLiteral node, Void context) {
			// TODO Auto-generated method stub
			return factory.makeConstant(node.getValue());
		}

		@Override
		protected CAstNode visitBooleanLiteral(BooleanLiteral node, Void context) {
			// TODO Auto-generated method stub
			return factory.makeConstant(node.getValue());
		}

		@Override
		protected CAstNode visitNullLiteral(NullLiteral node, Void context) {
			// TODO Auto-generated method stub
			return NULL;
		}

		@Override
		protected CAstNode visitLongLiteral(LongLiteral node, Void context) {
			// TODO Auto-generated method stub
			return factory.makeConstant(node.getValue());
		}

		@Override
		protected CAstNode visitLogicalBinaryExpression(LogicalBinaryExpression node, Void context) {
			CAstOperator op;
			if (node.getType().toString().equals("AND")) {
				op = OP_AND;
			} else {
				op = OP_OR;
			}
			return factory.makeNode(CAstNode.ANDOR_EXPR, op,
					process(node.getLeft(), context), process(node.getRight(), context));
		}
	}
	
	public static class SQLCAstOperator extends CAstOperator {

		protected SQLCAstOperator(String op) {
			super(op);
			// TODO Auto-generated constructor stub
		}
		
	}
}


