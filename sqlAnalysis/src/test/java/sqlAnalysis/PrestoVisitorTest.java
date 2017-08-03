package sqlAnalysis;

import java.util.Collection;

import org.junit.Test;

import com.facebook.presto.sql.parser.SqlParser;
import com.facebook.presto.sql.tree.Statement;
import com.ibm.wala.cast.tree.CAstEntity;
import com.ibm.wala.cast.tree.CAstNode;
import com.ibm.wala.cast.tree.impl.CAstOperator;

public class PrestoVisitorTest {
	static final SqlParser SQL_PARSER = new SqlParser();

	@Test
	public void testSimpleBinary() {
		String sql = "select column1, column2 from tblpatient a, tbldropdowns b where a.gender = b.id order by a.gender";
		CAstNode n = getWhere(process(sql));
		n = n.getChild(0);
		assert n.getKind() == CAstNode.BINARY_EXPR;
		assertBinaryExpression(n, CAstOperator.OP_EQ);

	}

	private void assertBinaryExpression(CAstNode n, CAstOperator op) {
		assert n.getChild(0).getKind() == op.getKind();
		assert n.getChild(1).getKind() == CAstNode.OBJECT_REF;
		assert n.getChild(2).getKind() == CAstNode.OBJECT_REF;
	}

	private CAstNode process(String sql) {
		Statement statement = SQL_PARSER.createStatement(sql);
		CAstEntity entity = PrestoVisitor.process(statement, sql);
		Collection<CAstEntity> l = entity.getAllScopedEntities().get(null);
		assert l.size() == 1;
		return l.iterator().next().getAST();
	}

	private CAstNode getWhere(CAstNode n) {
		return n.getChild(1);
	}

	private CAstNode getSelect(CAstNode n) {
		return n.getChild(0);
	}

	@Test
	public void testSimpleLike() {
		String sql = "SELECT name, capital FROM world WHERE name LIKE capital";
		System.out.println(process(sql));
		CAstNode n = getWhere(process(sql));
		n = n.getChild(0);
		assert n.getKind() == CAstNode.CALL;
		assert n.getChild(0).getValue().equals("like");
		assert n.getChild(1).getKind() == CAstNode.OBJECT_REF;
		assert n.getChild(2).getKind() == CAstNode.OBJECT_REF;

	}

	@Test
	public void testLike2() {
		String sql = "SELECT name, capital FROM world WHERE capital LIKE concat('%',name,'%')";
		CAstNode n = getWhere(process(sql));
		n = n.getChild(0);

		assert n.getKind() == CAstNode.CALL;
		assert n.getChild(0).getValue().equals("like");
		assert n.getChild(1).getKind() == CAstNode.OBJECT_REF;
		assert n.getChild(2).getKind() == CAstNode.CALL;
		assert n.getChild(2).getChild(0).getValue().equals("concat");
		assert n.getChild(2).getChild(1).getValue().equals("%");
		assert n.getChild(2).getChild(2).getKind() == CAstNode.OBJECT_REF;
		assert n.getChild(2).getChild(3).getValue().equals("%");

	}

	@Test
	public void testNotExists() {
		String sql = "SELECT * FROM Courses WHERE NOT EXISTS (SELECT * FROM Scores WHERE CourseID = Courses.CourseID AND UserID = userID)";
		CAstNode n = getWhere(process(sql));
		n = n.getChild(0);

		n = n.getChild(0).getChild(0).getChild(1).getChild(0);
		assert n.getKind() == CAstNode.ANDOR_EXPR;
		assert n.getChild(0).getValue().equals("and");
		assert n.getChild(1).getKind() == CAstNode.BINARY_EXPR;
		assert n.getChild(2).getKind() == CAstNode.BINARY_EXPR;
		assertBinaryExpression(n.getChild(1), CAstOperator.OP_EQ);
		assertBinaryExpression(n.getChild(2), CAstOperator.OP_EQ);

	}

	@Test
	public void testDateBetween() {
		String sql = "SELECT USER, COUNT(*) FROM ORDERS WHERE ENTRY_DATE BETWEEN SYSDATE - 5 AND SYSDATE GROUP BY USER";
		CAstNode n = process(sql);

		CAstNode sn = getSelect(n).getChild(0);
		assert sn.getKind() == CAstOperator.CALL;
		assert sn.getChild(0).getValue().equals("count");

		CAstNode wn = getWhere(n).getChild(0);
		assert wn.getKind() == CAstOperator.ANDOR_EXPR;
		assert wn.getChild(0).getValue().equals("and");
		assert wn.getChild(1).getKind() == CAstNode.BINARY_EXPR;
		assert wn.getChild(2).getKind() == CAstNode.BINARY_EXPR;
		assertBinaryExpression(wn.getChild(1), CAstOperator.OP_LT);
		wn = wn.getChild(2);
		assert wn.getChild(0).getKind() == CAstOperator.OP_GT.getKind();
		assert wn.getChild(1).getKind() == CAstNode.OBJECT_REF;
		assert wn.getChild(2).getKind() == CAstNode.BINARY_EXPR;
		wn = wn.getChild(2);
		assert wn.getChild(0).getKind() == CAstOperator.OP_SUB.getKind();
		assert wn.getChild(1).getKind() == CAstNode.OBJECT_REF;
		assert (Long) wn.getChild(2).getValue() == 5;

	}

	private void assertMax(CAstNode n) {
		assert n.getKind() == CAstOperator.CALL;
		assert n.getChild(0).getValue().equals("max");
		assert n.getChild(1).getKind() == CAstNode.OBJECT_REF;
	}

	@Test
	public void testMax() {
		String sql = "select max(AccessID) as AccessID,  max(AccessType) as AccessType from audit_log where Status in (335,66) group by EventTime";
		CAstNode n = process(sql);
		CAstNode sn = getSelect(n);
		
		assertMax(sn.getChild(0));
		assertMax(sn.getChild(1));
		n = getWhere(n).getChild(0);
		assert n.getKind() == CAstOperator.CALL;
		assert n.getChild(0).getValue().equals("in");
		assert n.getChild(1).getKind() == CAstNode.OBJECT_REF;

	}

	@Test
	public void testIsNull() {
		String sql = "SELECT p.Color FROM Parts p LEFT JOIN Catalog c ON c.pid = p.pid WHERE  c.sid IS NULL GROUP BY p.Color";
		CAstNode n = getWhere(process(sql));
		n = n.getChild(0);
		assert n.getKind() == CAstNode.BINARY_EXPR;
		assert n.getChild(0).getValue().equals("==");
		assert n.getChild(1).getKind() == CAstNode.OBJECT_REF;
		assert (int) n.getChild(2).getValue() == CAstNode.VOID;
	}

	@Test
	public void testSubQueryParse() {
		String sql = "SELECT name, height FROM people WHERE height = (SELECT MAX(height) FROM people)";
		System.out.println(process(sql));
		CAstNode n = getWhere(process(sql));
		n = n.getChild(0);
		assert n.getKind() == CAstNode.BINARY_EXPR;
		assert n.getChild(0).getValue().equals("==");
		assert n.getChild(1).getKind() == CAstNode.OBJECT_REF;
		assert n.getChild(2).getKind() == SQLCAstNode.QUERY_SELECT;
		n = n.getChild(2).getChild(0).getChild(0);
		assert n.getKind() == SQLCAstNode.QUERY_SELECT;
		n = n.getChild(0);
		assert n.getKind() == CAstNode.CALL;
		assert n.getChild(0).getValue().equals("max");
	}

	@Test
	public void testSubqueryNested() {
		String sql = "select g.* from (select Id, UserId, a.Time, (a.Time - (" + "select top(1) Time "
				+ " from access b " + " where a.UserId = b.UserId " + " and a.Time > b.Time " + " order by Time desc "
				+ ")) as gap from access a) as g join (" + "select UserId, max(gap) as max_gap from ("
				+ "select Id, UserId, a.Time, (a.Time - (select top(1) Time from access b "
				+ "where a.UserId = b.UserId and a.Time > b.Time order by Time desc)) as gap "
				+ "from access a) as cte_gap group by UserId) as m on m.UserId = g.UserId and m.max_gap = g.gap "
				+ " where g.UserId = 42";
		Statement statement = SQL_PARSER.createStatement(sql);
		CAstEntity entity = PrestoVisitor.process(statement, sql);
		Collection<CAstEntity> l = entity.getAllScopedEntities().get(null);
		int i = 0;
		for (CAstEntity e : l) {
			CAstNode n = e.getAST();
			System.out.println(n);
		}
	}

}
