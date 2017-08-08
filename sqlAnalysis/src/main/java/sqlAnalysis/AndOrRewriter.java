package sqlAnalysis;

import java.util.Map;

import com.ibm.wala.cast.tree.CAst;
import com.ibm.wala.cast.tree.CAstControlFlowMap;
import com.ibm.wala.cast.tree.CAstNode;
import com.ibm.wala.cast.tree.rewrite.CAstCloner;
import com.ibm.wala.util.collections.Pair;

public class AndOrRewriter extends CAstCloner {
	private int counter = 0;
	
	public AndOrRewriter(CAst Ast, boolean recursive) {
		super(Ast, recursive);
	}

	@Override
	protected CAstNode copyNodes(CAstNode root, CAstControlFlowMap cfg, NonCopyingContext context,
			Map<Pair<CAstNode, NoKey>, CAstNode> nodeMap) {
		if (root.getKind() == CAstNode.BINARY_EXPR && root.getChild(0).equals(PrestoVisitor.OP_OR)) {
			String var = nextVarName();
			return Ast.makeNode(CAstNode.BLOCK_EXPR,
					Ast.makeNode(CAstNode.ASSIGN,
							Ast.makeNode(CAstNode.VAR, Ast.makeConstant(var)),
							copyNodes(root.getChild(1), cfg, context, nodeMap)),
					Ast.makeNode(CAstNode.IF_EXPR, 
							Ast.makeNode(CAstNode.VAR, Ast.makeConstant(var)),
							Ast.makeNode(CAstNode.VAR, Ast.makeConstant(var)),
							copyNodes(root.getChild(2), cfg, context, nodeMap)));
									
		} else if (root.getKind() == CAstNode.BINARY_EXPR && root.getChild(0).equals(PrestoVisitor.OP_AND)) {
			String var = nextVarName();
			return Ast.makeNode(CAstNode.BLOCK_EXPR,
					Ast.makeNode(CAstNode.ASSIGN,
							Ast.makeNode(CAstNode.VAR, Ast.makeConstant(var)),
							copyNodes(root.getChild(1), cfg, context, nodeMap)),
					Ast.makeNode(CAstNode.IF_EXPR, 
							Ast.makeNode(CAstNode.VAR, Ast.makeConstant(var)),
							copyNodes(root.getChild(2), cfg, context, nodeMap),
							Ast.makeNode(CAstNode.VAR, Ast.makeConstant(var))));
	
		} else {
			return super.copyNodes(root, cfg, context, nodeMap);
		}
	}

	private String nextVarName() {
		return "_andor_" + counter++;
	}

}
