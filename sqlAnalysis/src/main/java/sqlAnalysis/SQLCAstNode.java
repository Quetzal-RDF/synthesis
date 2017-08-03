package sqlAnalysis;

import com.ibm.wala.cast.tree.CAstNode;

public interface SQLCAstNode extends CAstNode {
	public static int IN_ARGS = SUB_LANGUAGE_BASE + 1;
	public static int QUERY_SELECT = SUB_LANGUAGE_BASE + 2;
	public static int QUERY_WHERE = SUB_LANGUAGE_BASE + 3;
	public static int EXISTS = SUB_LANGUAGE_BASE + 4;
	public static int NOT = SUB_LANGUAGE_BASE + 5;
	public static int QUERY = SUB_LANGUAGE_BASE + 6;
	public static int CURRENT_TIME = SUB_LANGUAGE_BASE + 7;
}
