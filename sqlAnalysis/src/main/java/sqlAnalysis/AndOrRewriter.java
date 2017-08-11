package sqlAnalysis;

import java.util.Map;

import com.ibm.wala.cast.tree.CAst;
import com.ibm.wala.cast.tree.CAstControlFlowMap;
import com.ibm.wala.cast.tree.CAstNode;
import com.ibm.wala.cast.tree.impl.CAstSymbolImpl;
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
		

		if ((root.getKind() == CAstNode.ANDOR_EXPR || root.getKind() == CAstNode.BINARY_EXPR) && root.getChild(0).equals(PrestoVisitor.OP_OR)) {
			String var1 = nextVarName();
			CAstNode decl1 = Ast.makeNode(CAstNode.DECL_STMT,
					Ast.makeConstant(new CAstSymbolImpl(var1, SQLCAstToIRTranslator.Any)), 
					copyNodes(root.getChild(1), cfg, context, nodeMap));

			String var2 = nextVarName();
			CAstNode decl2 = Ast.makeNode(CAstNode.DECL_STMT,
					Ast.makeConstant(new CAstSymbolImpl(var2, SQLCAstToIRTranslator.Any)), 
					copyNodes(root.getChild(2), cfg, context, nodeMap));

			CAstNode if1 = Ast.makeNode(CAstNode.IF_EXPR, 
					Ast.makeNode(CAstNode.VAR, Ast.makeConstant(var1)),
					Ast.makeNode(CAstNode.VAR, Ast.makeConstant(var1)),
					Ast.makeNode(CAstNode.VAR, Ast.makeConstant(var2)));
			
			return Ast.makeNode(CAstNode.BLOCK_EXPR,
					decl1, decl2, if1
					);
									
		} else if ((root.getKind() == CAstNode.ANDOR_EXPR || root.getKind() == CAstNode.BINARY_EXPR) && root.getChild(0).equals(PrestoVisitor.OP_AND)) {
			String var1 = nextVarName();
			CAstNode decl1 = Ast.makeNode(CAstNode.DECL_STMT,
					Ast.makeConstant(new CAstSymbolImpl(var1, SQLCAstToIRTranslator.Any)),
					copyNodes(root.getChild(1), cfg, context, nodeMap));

			String var2 = nextVarName();
			CAstNode decl2 = Ast.makeNode(CAstNode.DECL_STMT,
					Ast.makeConstant(new CAstSymbolImpl(var2, SQLCAstToIRTranslator.Any)),
					copyNodes(root.getChild(2), cfg, context, nodeMap));

			CAstNode if2 = Ast.makeNode(CAstNode.IF_EXPR, 
					Ast.makeNode(CAstNode.VAR, Ast.makeConstant(var1)),
					Ast.makeNode(CAstNode.VAR, Ast.makeConstant(var2)),
					Ast.makeNode(CAstNode.VAR, Ast.makeConstant(var1)));

			return Ast.makeNode(CAstNode.BLOCK_EXPR,
					decl1, decl2, if2
					);
		} else {
			return super.copyNodes(root, cfg, context, nodeMap);
		}
	}

	private String nextVarName() {
		return "_andor_" + counter++;
	}

}
