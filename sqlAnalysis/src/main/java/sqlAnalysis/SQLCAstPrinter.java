package sqlAnalysis;

import com.ibm.wala.cast.tree.CAstNode;
import com.ibm.wala.cast.util.CAstPrinter;

public class SQLCAstPrinter extends CAstPrinter {

	@Override
	public String getKindAsString(int kind) {
		if (kind >= CAstNode.SUB_LANGUAGE_BASE) {
			switch (kind) {
			case SQLCAstNode.IN_ARGS:
				return "IN_ARGS";
			case SQLCAstNode.SUBQUERY:
				return "SUBQUERY";
			case SQLCAstNode.SUBQUERY_SELECT:
				return "SUBQUERY_SELECT";
			case SQLCAstNode.SUBQUERY_WHERE:
				return "SUBQUERY_WHERE";
			case SQLCAstNode.EXISTS:
				return "EXISTS";
			case SQLCAstNode.NOT:
				return "NOT";

			}
		}
		// TODO Auto-generated method stub
		return super.getKindAsString(kind);
	}

}
