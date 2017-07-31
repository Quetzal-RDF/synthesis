package sqlAnalysis;

import java.util.List;

import com.facebook.presto.sql.parser.SqlParser;
import com.facebook.presto.sql.tree.Expression;
import com.facebook.presto.sql.tree.Query;
import com.facebook.presto.sql.tree.QuerySpecification;
import com.facebook.presto.sql.tree.SelectItem;
import com.facebook.presto.sql.tree.SingleColumn;
import com.google.common.base.Optional;

public class TestExpressionGenerator {
	static final SqlParser SQL_PARSER = new SqlParser();

	public static void main(String[] args) {
		String code = "select count(*) from foo where col1 > col2";
		Query query = (Query) SQL_PARSER.createStatement(code);
		QuerySpecification qb = (QuerySpecification) query.getQueryBody();
		List<SelectItem> items = qb.getSelect().getSelectItems();
		for (SelectItem i : items) {
			if (i instanceof SingleColumn) {
				Expression e = ((SingleColumn) i).getExpression();
				System.out.println(e);
			}
		}
		Optional<Expression> where = qb.getWhere();
		if (where.isPresent()) {
			System.out.println(where.get());
		}
	}
}
