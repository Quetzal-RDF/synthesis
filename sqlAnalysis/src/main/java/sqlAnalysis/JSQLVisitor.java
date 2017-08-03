package sqlAnalysis;

import java.util.LinkedList;
import java.util.List;
import java.util.Stack;

import com.ibm.wala.cast.tree.CAst;
import com.ibm.wala.cast.tree.CAstControlFlowMap;
import com.ibm.wala.cast.tree.CAstEntity;
import com.ibm.wala.cast.tree.CAstNode;
import com.ibm.wala.cast.tree.impl.CAstControlFlowRecorder;
import com.ibm.wala.cast.tree.impl.CAstImpl;
import com.ibm.wala.cast.tree.impl.CAstOperator;
import com.ibm.wala.cast.tree.impl.CAstSourcePositionRecorder;
import com.ibm.wala.cast.tree.impl.CAstSymbolImpl;

import net.sf.jsqlparser.expression.AllComparisonExpression;
import net.sf.jsqlparser.expression.AnalyticExpression;
import net.sf.jsqlparser.expression.AnyComparisonExpression;
import net.sf.jsqlparser.expression.BinaryExpression;
import net.sf.jsqlparser.expression.CaseExpression;
import net.sf.jsqlparser.expression.CastExpression;
import net.sf.jsqlparser.expression.DateValue;
import net.sf.jsqlparser.expression.DoubleValue;
import net.sf.jsqlparser.expression.Expression;
import net.sf.jsqlparser.expression.ExpressionVisitorAdapter;
import net.sf.jsqlparser.expression.ExtractExpression;
import net.sf.jsqlparser.expression.Function;
import net.sf.jsqlparser.expression.IntervalExpression;
import net.sf.jsqlparser.expression.LongValue;
import net.sf.jsqlparser.expression.NullValue;
import net.sf.jsqlparser.expression.SignedExpression;
import net.sf.jsqlparser.expression.StringValue;
import net.sf.jsqlparser.expression.TimeValue;
import net.sf.jsqlparser.expression.TimestampValue;
import net.sf.jsqlparser.expression.WhenClause;
import net.sf.jsqlparser.expression.operators.arithmetic.Addition;
import net.sf.jsqlparser.expression.operators.arithmetic.BitwiseAnd;
import net.sf.jsqlparser.expression.operators.arithmetic.BitwiseOr;
import net.sf.jsqlparser.expression.operators.arithmetic.BitwiseXor;
import net.sf.jsqlparser.expression.operators.arithmetic.Concat;
import net.sf.jsqlparser.expression.operators.arithmetic.Division;
import net.sf.jsqlparser.expression.operators.arithmetic.Modulo;
import net.sf.jsqlparser.expression.operators.arithmetic.Multiplication;
import net.sf.jsqlparser.expression.operators.arithmetic.Subtraction;
import net.sf.jsqlparser.expression.operators.conditional.AndExpression;
import net.sf.jsqlparser.expression.operators.conditional.OrExpression;
import net.sf.jsqlparser.expression.operators.relational.Between;
import net.sf.jsqlparser.expression.operators.relational.EqualsTo;
import net.sf.jsqlparser.expression.operators.relational.ExistsExpression;
import net.sf.jsqlparser.expression.operators.relational.ExpressionList;
import net.sf.jsqlparser.expression.operators.relational.GreaterThan;
import net.sf.jsqlparser.expression.operators.relational.GreaterThanEquals;
import net.sf.jsqlparser.expression.operators.relational.InExpression;
import net.sf.jsqlparser.expression.operators.relational.IsNullExpression;
import net.sf.jsqlparser.expression.operators.relational.LikeExpression;
import net.sf.jsqlparser.expression.operators.relational.Matches;
import net.sf.jsqlparser.expression.operators.relational.MultiExpressionList;
import net.sf.jsqlparser.expression.operators.relational.NotEqualsTo;
import net.sf.jsqlparser.expression.operators.relational.RegExpMatchOperator;
import net.sf.jsqlparser.schema.Column;
import net.sf.jsqlparser.statement.Statement;
import net.sf.jsqlparser.statement.StatementVisitor;
import net.sf.jsqlparser.statement.Statements;
import net.sf.jsqlparser.statement.alter.Alter;
import net.sf.jsqlparser.statement.create.index.CreateIndex;
import net.sf.jsqlparser.statement.create.table.CreateTable;
import net.sf.jsqlparser.statement.create.view.CreateView;
import net.sf.jsqlparser.statement.delete.Delete;
import net.sf.jsqlparser.statement.drop.Drop;
import net.sf.jsqlparser.statement.insert.Insert;
import net.sf.jsqlparser.statement.replace.Replace;
import net.sf.jsqlparser.statement.select.AllColumns;
import net.sf.jsqlparser.statement.select.AllTableColumns;
import net.sf.jsqlparser.statement.select.PlainSelect;
import net.sf.jsqlparser.statement.select.Select;
import net.sf.jsqlparser.statement.select.SelectExpressionItem;
import net.sf.jsqlparser.statement.select.SelectItem;
import net.sf.jsqlparser.statement.select.SelectItemVisitor;
import net.sf.jsqlparser.statement.select.SelectVisitor;
import net.sf.jsqlparser.statement.select.SetOperationList;
import net.sf.jsqlparser.statement.select.WithItem;
import net.sf.jsqlparser.statement.truncate.Truncate;
import net.sf.jsqlparser.statement.update.Update;
import sqlAnalysis.JSQLVisit.ExpressionVis;

public class JSQLVisitor {
	
	public static CAstEntity process(Statement st, String orig) {
		System.out.println(orig);
		JSQLVisit v = new JSQLVisit();
		st.accept(v);
		
		List<CAstEntity> l = new LinkedList<CAstEntity>();
		for (Expression e : v.expressions) {
			ExpressionVis vis = new ExpressionVis();
			e.accept(vis);
			CAstNode n = vis.expStack.pop();
			PrestoVisitor.createEntity(l, n, vis.cfg());
		}
		
		int myStatement = PrestoVisitor.statementCount++;
		return PrestoVisitor.createFileEntity(l, myStatement);
	}
}

final class JSQLVisit implements StatementVisitor {
	List<Expression> expressions = new LinkedList<Expression>();
	

	public void visit(Statements arg0) {
		// TODO Auto-generated method stub
		
	}
	
	public void visit(Alter arg0) {
		// TODO Auto-generated method stub
		
	}
	
	public void visit(CreateView arg0) {
		// TODO Auto-generated method stub
		
	}
	
	public void visit(CreateTable arg0) {
		// TODO Auto-generated method stub
		
	}
	
	public void visit(CreateIndex arg0) {
		// TODO Auto-generated method stub
		
	}
	
	public void visit(Truncate arg0) {
		// TODO Auto-generated method stub
		
	}
	
	public void visit(Drop arg0) {
		// TODO Auto-generated method stub
		
	}
	
	public void visit(Replace arg0) {
		// TODO Auto-generated method stub
		
	}
	
	public void visit(Insert arg0) {
		// TODO Auto-generated method stub
		
	}
	
	public void visit(Update arg0) {
		// TODO Auto-generated method stub
		
	}
	
	public void visit(Delete arg0) {
		// TODO Auto-generated method stub
		
	}
	
	public void visit(Select arg0) {
		// TODO Auto-generated method stub
		arg0.getSelectBody().accept(new SelectVisitor() {
			
			public void visit(WithItem arg0) {
				// TODO Auto-generated method stub
				
			}
			
			public void visit(SetOperationList arg0) {
				// TODO Auto-generated method stub
				
			}
			
			public void visit(PlainSelect arg0) {
				// TODO Auto-generated method stub
				List<SelectItem> items = arg0.getSelectItems();
				for (SelectItem i : items) {
					i.accept(new SelectItemVisitor() {
						
						public void visit(SelectExpressionItem arg0) {
							Expression a = arg0.getExpression();
							if (!(a instanceof Column)) {
								expressions.add(a);
								arg0.getExpression().accept(new ExpressionVis());
							}
						}
						
						public void visit(AllTableColumns arg0) {
							// TODO Auto-generated method stub
							
						}
						
						public void visit(AllColumns arg0) {
							// TODO Auto-generated method stub
							
						}
					});
				}
				if (arg0.getWhere() != null) {
					expressions.add(arg0.getWhere());
					arg0.getWhere().accept(new ExpressionVis());;
				}
			}
		});
	}
	
	public static final class ExpressionVis extends ExpressionVisitorAdapter {

		protected final CAst factory = new CAstImpl();
		protected CAstNode NULL = factory.makeConstant(CAstNode.VOID);
		protected Stack<CAstNode> expStack = new Stack<CAstNode>();

		private final CAstSourcePositionRecorder pos = new CAstSourcePositionRecorder();
		
		private final CAstControlFlowRecorder rec = new CAstControlFlowRecorder(pos);
		
		public CAstControlFlowMap cfg() {
			return rec;
		}

		@Override
		public void visit(NullValue value) {
			expStack.push(NULL);
		}

		@Override
		public void visit(Function function) {	
			CAstNode[] arr;
			if (function.getParameters() == null) {
				arr = new CAstNode[0];
			} else {
				List<Expression> e = function.getParameters().getExpressions();
				arr = new CAstNode[e.size()];

				for (int i = 0; i < e.size() ; i++) {
					e.get(i).accept(this);	
					arr[i] = expStack.pop();
				}
			}
			expStack.push(factory.makeNode(CAstNode.CALL, factory.makeConstant(function.getName()), arr));
		}

		@Override
		public void visit(SignedExpression expr) {
			Expression lhs = expr.getExpression();
			if (expr.getSign() == '-') {
				Multiplication m = new Multiplication();
				m.setLeftExpression(lhs);
				m.setRightExpression(new LongValue(-1));
				super.visit(m);
			} else {
				lhs.accept(this);
			}
		}

		@Override
		public void visit(DoubleValue value) {
			expStack.push(factory.makeConstant(value.getValue()));
		}

		@Override
		public void visit(LongValue value) {
			expStack.push(factory.makeConstant(value.getValue()));
		}

		@Override
		public void visit(DateValue value) {
			expStack.push(factory.makeConstant(value.getValue()));
		}

		@Override
		public void visit(TimeValue value) {
			expStack.push(factory.makeConstant(value.getValue()));
		}

		@Override
		public void visit(TimestampValue value) {
			expStack.push(factory.makeConstant(value.getValue()));
		}

		@Override
		public void visit(StringValue value) {
			expStack.push(factory.makeConstant(value.getValue()));
		}

		private void doBinary(String op, BinaryExpression expr) {
			expr.getLeftExpression().accept(this);
			CAstNode left = expStack.pop();
			expr.getRightExpression().accept(this);
			CAstNode right = expStack.pop();
			expStack.push(factory.makeNode(CAstNode.BINARY_EXPR, PrestoVisitor.processOp(op), left, right));
		}
		
		@Override
		public void visit(Addition expr) {
			doBinary("+", expr);
		}

		@Override
		public void visit(Division expr) {
			doBinary("/", expr);
		}

		@Override
		public void visit(Multiplication expr) {
			doBinary("*", expr);
		}

		@Override
		public void visit(Subtraction expr) {
			doBinary("-", expr);
		}

		private void doAndOr(BinaryExpression expr) {
			expr.getLeftExpression().accept(this);
			CAstNode left = expStack.pop();
			expr.getRightExpression().accept(this);
			CAstNode right = expStack.pop();
			expStack.push(factory.makeNode(CAstNode.ANDOR_EXPR, factory.makeConstant(expr.getStringExpression()), left, right));
		}
		
		@Override
		public void visit(AndExpression expr) {
			doAndOr(expr);
		}

		@Override
		public void visit(OrExpression expr) {
			doAndOr(expr);
		}

		@Override
		public void visit(Between expr) {
			CAstNode[] arr = new CAstNode[3];
			expr.getLeftExpression().accept(this);
			arr[0] = expStack.pop();
			expr.getBetweenExpressionStart().accept(this);;
			arr[1] = expStack.pop();
			expr.getBetweenExpressionEnd().accept(this);
			arr[2] = expStack.pop();
			expStack.push(factory.makeNode(CAstNode.ANDOR_EXPR, factory.makeConstant("AND"), arr));
		}

		@Override
		public void visit(EqualsTo expr) {
			doBinary("=", expr);
		}

		@Override
		public void visit(GreaterThan expr) {
			doBinary(">", expr);
		}

		@Override
		public void visit(GreaterThanEquals expr) {
			doBinary(">=", expr);
		}

		@Override
		public void visit(InExpression expr) {
			expr.getRightItemsList().accept(this);
			expr.getLeftItemsList().accept(this);
			expr.getLeftExpression().accept(this);
			CAstNode arr [] = new CAstNode[expStack.size()];
			for (int i = 0; i < expStack.size(); i++) {
				arr[i] = expStack.pop();
			}
			expStack.push(factory.makeNode(CAstNode.CALL, factory.makeConstant("IN_LIST"), arr));
		}

		@Override
		public void visit(IsNullExpression expr) {
			expr.getLeftExpression().accept(this);
			factory.makeNode(CAstNode.BINARY_EXPR, CAstOperator.OP_EQ, expStack.pop(),
					NULL);
		}

		@Override
		public void visit(LikeExpression expr) {
			CAstNode[] args = new CAstNode[3];
			expr.getLeftExpression().accept(this);
			args[0] = expStack.pop();
			expr.getRightExpression().accept(this);
			args[1] = expStack.pop();
			args[2] = factory.makeConstant(expr.getEscape());
			expStack.push(factory.makeNode(CAstNode.CALL, factory.makeConstant("LIKE"), args));
		}

		@Override
		public void visit(NotEqualsTo expr) {
			doBinary("!=", expr);
		}

		@Override
		public void visit(Column column) {
			expStack.push(factory.makeNode(CAstNode.OBJECT_REF, factory.makeNode(CAstNode.VOID),
					factory.makeConstant(column.getColumnName())));
		}

		@Override
		public void visit(CaseExpression expr) {
			Expression e = expr.getSwitchExpression();
			CAstNode s = null;
			CAstNode decl = null;
			if (e != null) {
				e.accept(this);
				s = expStack.pop();
				decl = factory.makeNode(CAstNode.DECL_STMT, factory.makeConstant(new CAstSymbolImpl("case exp temp", SQLCAstToIRTranslator.Any, s)));
				s = factory.makeNode(CAstNode.VAR, factory.makeConstant("case exp temp"));

			}
			expr.getElseExpression().accept(this);
			CAstNode result = expStack.pop();
			if (s != null) {
				result = factory.makeNode(CAstNode.BINARY_EXPR, PrestoVisitor.processOp("="), s, result);
			}
			List<Expression> whenclauses = expr.getWhenClauses();
			for (int i = whenclauses.size() -1; i >= 0; i--) {
				whenclauses.get(i).accept(this);
				CAstNode when = expStack.pop();
				CAstNode then = expStack.pop();
				if (e != null) {
					when = factory.makeNode(CAstNode.BINARY_EXPR, PrestoVisitor.processOp("="), s, when);
				}
				result = expStack.push(factory.makeNode(CAstNode.IF_EXPR, when, then, result));
			}
			if (e != null) {
				result = factory.makeNode(CAstNode.LOCAL_SCOPE, factory.makeNode(CAstNode.BLOCK_EXPR, decl, result));
			}
			expStack.push(result);
		}

		@Override
		public void visit(WhenClause expr) {
			expr.getThenExpression().accept(this);
			expr.getWhenExpression().accept(this);
		}

		@Override
		public void visit(ExistsExpression expr) {
			// We just pass back up to do its thing because sql exists usually means a subselect
			super.visit(expr);
		}

		@Override
		public void visit(AllComparisonExpression expr) {
			// subselect - just pass back up
			super.visit(expr);
		}

		@Override
		public void visit(AnyComparisonExpression expr) {
			// subselect - just pass bak up
			super.visit(expr);
		}
		
		private void doBinaryFunction(String funcName, BinaryExpression expr) {
			CAstNode[] args = new CAstNode[2];
			expr.getLeftExpression().accept(this);
			args[0] = expStack.pop();
			expr.getRightExpression().accept(this);
			args[1] = expStack.pop();
			expStack.push(factory.makeNode(CAstNode.CALL, factory.makeConstant(funcName), args));
		}

		@Override
		public void visit(Concat expr) {
			doBinaryFunction("CONCAT", expr);
		}

		@Override
		public void visit(Matches expr) {
			// This is some very funky thing - NOT REGEX
			doBinaryFunction("MATCHES", expr);
		}

		@Override
		public void visit(BitwiseAnd expr) {
			doBinary("&", expr);
		}

		@Override
		public void visit(BitwiseOr expr) {
			doBinary("|", expr);
		}

		@Override
		public void visit(BitwiseXor expr) {
			doBinary("#", expr);
		}

		@Override
		public void visit(CastExpression expr) {
			expr.getLeftExpression().accept(this);
			expStack.push(factory.makeNode(CAstNode.CAST, expStack.pop(), factory.makeConstant(expr.getType().getDataType())));
		}

		@Override
		public void visit(Modulo expr) {
			doBinary("%", expr);
		}

		@Override
		public void visit(AnalyticExpression expr) {
			// TODO - model this as a function?
			super.visit(expr);
		}

		@Override
		public void visit(ExtractExpression expr) {
			CAstNode[] args = new CAstNode[2];
			args[0] = factory.makeConstant(expr.getName());
			expr.getExpression().accept(this);
			args[1] = expStack.pop();
			expStack.push(factory.makeNode(CAstNode.CALL, factory.makeConstant("EXTRACT"), args));
		}

		@Override
		public void visit(IntervalExpression expr) {
			expStack.push(factory.makeNode(CAstNode.CALL, factory.makeConstant("INTERVAL"), factory.makeConstant(expr.getParameter())));
		}

		@Override
		public void visit(RegExpMatchOperator expr) {

			doBinary("REGEX", expr);
		}

		@Override
		public void visit(ExpressionList expressionList) {
			/// Leave as is...
			super.visit(expressionList);
		}

		@Override
		public void visit(MultiExpressionList multiExprList) {
			// Leave these as is
			super.visit(multiExprList);
		}

		@Override
		protected void visitBinaryExpression(BinaryExpression expr) {
			doBinaryFunction(expr.getStringExpression(), expr);
		}
	}
}
