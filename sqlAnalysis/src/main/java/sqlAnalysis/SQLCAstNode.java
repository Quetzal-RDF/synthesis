package sqlAnalysis;

import com.ibm.wala.cast.tree.CAstNode;

public interface SQLCAstNode extends CAstNode {
	public static int IN_ARGS = SUB_LANGUAGE_BASE + 1;
	public static int SUBQUERY = SUB_LANGUAGE_BASE + 2;
	public static int SUBQUERY_SELECT = SUB_LANGUAGE_BASE + 3;
	public static int SUBQUERY_WHERE = SUB_LANGUAGE_BASE + 4;
	public static int EXISTS = SUB_LANGUAGE_BASE + 5;
	public static int NOT = SUB_LANGUAGE_BASE + 6;
}
