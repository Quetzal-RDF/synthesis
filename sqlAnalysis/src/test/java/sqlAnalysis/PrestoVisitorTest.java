package sqlAnalysis;

import java.util.Collection;
import java.util.Iterator;

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
		CAstNode n = process(sql);
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

	@Test
	public void testSimpleLike() {
		String sql = "SELECT name, capital FROM world WHERE name LIKE capital";
		CAstNode n = process(sql);
		assert n.getKind() == CAstNode.CALL;
		assert n.getChild(0).getValue().equals("like");
		assert n.getChild(1).getKind() == CAstNode.OBJECT_REF;
		assert n.getChild(2).getKind() == CAstNode.OBJECT_REF;

	}

	@Test
	public void testLike2() {
		String sql = "SELECT name, capital FROM world WHERE capital LIKE concat('%',name,'%')";
		CAstNode n = process(sql);
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
		CAstNode n = process(sql);
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
		Statement statement = SQL_PARSER.createStatement(sql);
		CAstEntity entity = PrestoVisitor.process(statement, sql);
		Collection<CAstEntity> l = entity.getAllScopedEntities().get(null);
		Iterator<CAstEntity> it = l.iterator();
		while (it.hasNext()) {
			CAstNode n = it.next().getAST();
			if (n.getKind() == CAstOperator.CALL) {
				assert n.getChild(0).getValue().equals("count");
			}
			if (n.getKind() == CAstOperator.ANDOR_EXPR) {
				assert n.getChild(0).getValue().equals("and");
				assert n.getChild(1).getKind() == CAstNode.BINARY_EXPR;
				assert n.getChild(2).getKind() == CAstNode.BINARY_EXPR;
				assertBinaryExpression(n.getChild(1), CAstOperator.OP_LT);
				n  = n.getChild(2);
				assert n.getChild(0).getKind() == CAstOperator.OP_GT.getKind();
				assert n.getChild(1).getKind() == CAstNode.OBJECT_REF;
				assert n.getChild(2).getKind() == CAstNode.BINARY_EXPR;
				n = n.getChild(2);
				assert n.getChild(0).getKind() == CAstOperator.OP_SUB.getKind();
				assert n.getChild(1).getKind() == CAstNode.OBJECT_REF;
				assert (Long) n.getChild(2).getValue() == 5;

			}
		}
	}
	
	private void assertMax(CAstNode n) {
		assert n.getKind() == CAstOperator.CALL;
		assert n.getChild(0).getValue().equals("max");
		assert n.getChild(1).getKind() == CAstNode.OBJECT_REF;
	}
	
	@Test
	public void testMax() {
		String sql = "select max(AccessID) as AccessID,  max(AccessType) as AccessType from audit_log where Status in (335,66) group by EventTime";
		Statement statement = SQL_PARSER.createStatement(sql);
		CAstEntity entity = PrestoVisitor.process(statement, sql);
		Collection<CAstEntity> l = entity.getAllScopedEntities().get(null);
		int i = 0;
		for (CAstEntity e : l) {
			CAstNode n = e.getAST();
			if (i < 2) {
				assertMax(n);
			} else {
				System.out.println(n);
				assert n.getKind() == CAstOperator.CALL;
				assert n.getChild(0).getValue().equals("in");
				assert n.getChild(1).getKind() == CAstNode.OBJECT_REF;
			}
			i++;
		}
	}
	
	@Test
	public void testIsNull() {
		String sql = "SELECT p.Color FROM Parts p LEFT JOIN Catalog c ON c.pid = p.pid WHERE  c.sid IS NULL GROUP BY p.Color";
		CAstNode n = process(sql);
		assert n.getKind() == CAstNode.BINARY_EXPR;
		assert n.getChild(0).getValue().equals("==");
		assert n.getChild(1).getKind() == CAstNode.OBJECT_REF;
		assert (int) n.getChild(2).getValue() == CAstNode.VOID;
	}
	
	@Test
	public void testSubQueryParse() {
		String sql = "SELECT name, height FROM people WHERE height = (SELECT MAX(height) FROM people)";
		CAstNode n = process(sql);
		
	}

}
