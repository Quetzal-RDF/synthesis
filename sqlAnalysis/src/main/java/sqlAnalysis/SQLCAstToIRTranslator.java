package sqlAnalysis;

import java.util.Collection;
import java.util.Collections;
import java.util.Map;

import com.ibm.wala.cast.ir.translator.AstTranslator;
import com.ibm.wala.cast.ir.translator.AstTranslator.WalkContext;
import com.ibm.wala.cast.loader.AstMethod.DebuggingInformation;
import com.ibm.wala.cast.tree.CAstEntity;
import com.ibm.wala.cast.tree.CAstNode;
import com.ibm.wala.cast.tree.CAstType;
import com.ibm.wala.cast.tree.visit.CAstVisitor;
import com.ibm.wala.cast.util.CAstPrinter;
import com.ibm.wala.cfg.AbstractCFG;
import com.ibm.wala.cfg.IBasicBlock;
import com.ibm.wala.classLoader.CallSiteReference;
import com.ibm.wala.classLoader.ModuleEntry;
import com.ibm.wala.shrikeBT.IInvokeInstruction;
import com.ibm.wala.ssa.SSAInstruction;
import com.ibm.wala.ssa.SymbolTable;
import com.ibm.wala.types.MethodReference;
import com.ibm.wala.types.TypeName;
import com.ibm.wala.types.TypeReference;

public class SQLCAstToIRTranslator extends AstTranslator {

	public static final CAstType Any = new CAstType() { 
	
		public String getName() {
			return "Any";
		}
	
		public Collection<CAstType> getSupertypes() {
			return Collections.emptySet();
		}
	};

	public SQLCAstToIRTranslator(SQLClassLoader loader) {
		super(loader);
	}

	@Override
	protected boolean useDefaultInitValues() {
		return false;
	}

	@Override
	protected boolean treatGlobalsAsLexicallyScoped() {
		return false;
	}

	@Override
	protected TypeReference defaultCatchType() {
		return SQLClassLoader.Any;
	}

	@Override
	protected TypeReference makeType(CAstType type) {
		return TypeReference.findOrCreate(SQLClassLoaderFactory.Sql, TypeName.findOrCreate(type.getName()));
	}

	@Override
	protected boolean defineType(CAstEntity type, WalkContext wc) {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	protected void declareFunction(CAstEntity N, WalkContext context) {
		// TODO Auto-generated method stub

	}

	@Override
	protected void defineFunction(CAstEntity N, WalkContext definingContext,
			AbstractCFG<SSAInstruction, ? extends IBasicBlock<SSAInstruction>> cfg, SymbolTable symtab,
			boolean hasCatchBlock, Map<IBasicBlock<SSAInstruction>, TypeReference[]> catchTypes, boolean hasMonitorOp,
			AstLexicalInformation lexicalInfo, DebuggingInformation debugInfo) {
		((SQLClassLoader)loader).defineFunction(N, definingContext, cfg, symtab, hasCatchBlock, catchTypes, hasMonitorOp, lexicalInfo, debugInfo);
	}

	@Override
	protected void defineField(CAstEntity topEntity, WalkContext context, CAstEntity fieldEntity) {
		assert false;
	}

	@Override
	protected String composeEntityName(WalkContext parent, CAstEntity f) {
		return f.getName();
	}

	@Override
	protected void doThrow(WalkContext context, int exception) {
		assert false;
	}

	@Override
	public void doArrayRead(WalkContext context, int result, int arrayValue, CAstNode arrayRef, int[] dimValues) {
		// TODO Auto-generated method stub

	}

	@Override
	public void doArrayWrite(WalkContext context, int arrayValue, CAstNode arrayRef, int[] dimValues, int rval) {
		// TODO Auto-generated method stub

	}

	@Override
	protected void doFieldRead(WalkContext context, int result, int receiver, CAstNode elt, CAstNode parent) {
		// TODO Auto-generated method stub

	}

	@Override
	protected void doFieldWrite(WalkContext context, int receiver, CAstNode elt, CAstNode parent, int rval) {
		// TODO Auto-generated method stub

	}

	@Override
	protected void doMaterializeFunction(CAstNode node, WalkContext context, int result, int exception, CAstEntity fn) {
		assert false;
	}

	@Override
	protected void doNewObject(WalkContext context, CAstNode newNode, int result, Object type, int[] arguments) {
		assert false;
	}

	@Override
	protected void doCall(WalkContext context, CAstNode call, int result, int exception, CAstNode name, int receiver,
			int[] params) {
		MethodReference ref= MethodReference.findOrCreate(SQLClassLoader.Any, name.getValue().toString(), "()LAny;");
		CallSiteReference cs = CallSiteReference.make(context.cfg().getCurrentInstruction(), ref, IInvokeInstruction.Dispatch.STATIC);
		insts.InvokeInstruction(context.cfg().getCurrentInstruction(), result, params, exception, cs, null);
	}

	@Override
	protected CAstType topType() {
		return Any;
	}

	@Override
	protected CAstType exceptionType() {
		return Any;
	}

	@Override
	protected void doPrimitive(int resultVal, WalkContext context, CAstNode primitiveCall) {
		assert false;
	}

	@Override
	protected boolean doVisit(CAstNode n, WalkContext context, CAstVisitor<WalkContext> visitor) {
		if (n.getKind() == SQLCAstNode.QUERY) {
			visitor.visit(n.getChild(0), context, visitor);
			return true;
		} else if (n.getKind() == SQLCAstNode.QUERY_SELECT) {
			visitor.visit(n.getChild(0), context, visitor);
			return true;
		} else {
			return super.doVisit(n, context, visitor);
		}
	}


	private static final boolean DEBUG_AST = false;
	@Override
	public void translate(CAstEntity N, ModuleEntry context) {
		if (DEBUG_AST) {
			System.out.println(CAstPrinter.print(N));
		} else {
			super.translate(N, context);
		}
	}

}
