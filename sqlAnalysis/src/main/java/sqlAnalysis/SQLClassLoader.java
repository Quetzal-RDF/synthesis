package sqlAnalysis;

import java.io.IOException;

import com.facebook.presto.sql.parser.SqlParser;
import com.facebook.presto.sql.tree.Statement;
import com.ibm.wala.cast.ir.translator.TranslatorToCAst;
import com.ibm.wala.cast.ir.translator.TranslatorToIR;
import com.ibm.wala.cast.loader.CAstAbstractModuleLoader;
import com.ibm.wala.cast.tree.CAst;
import com.ibm.wala.cast.tree.CAstEntity;
import com.ibm.wala.cast.tree.rewrite.CAstRewriter.CopyKey;
import com.ibm.wala.cast.tree.rewrite.CAstRewriter.RewriteContext;
import com.ibm.wala.cast.tree.rewrite.CAstRewriterFactory;
import com.ibm.wala.classLoader.Language;
import com.ibm.wala.classLoader.ModuleEntry;
import com.ibm.wala.ipa.cha.IClassHierarchy;
import com.ibm.wala.ssa.SSAInstructionFactory;
import com.ibm.wala.types.ClassLoaderReference;
import com.ibm.wala.types.TypeReference;
import com.ibm.wala.util.io.Streams;

public class SQLClassLoader extends CAstAbstractModuleLoader {

	static final SqlParser SQL_PARSER = new SqlParser();

	public static TypeReference Any = TypeReference.find(SQLClassLoaderFactory.Sql, "LAny");
	
	public SQLClassLoader(IClassHierarchy cha) {
		super(cha);
	}

	@Override
	public ClassLoaderReference getReference() {
		return SQLClassLoaderFactory.Sql;
	}

	@Override
	public Language getLanguage() {
		return SQL.sql;
	}

	@Override
	public SSAInstructionFactory getInstructionFactory() {
		return Language.JAVA.instructionFactory();
	}

	@Override
	protected TranslatorToCAst getTranslatorToCAst(CAst ast, ModuleEntry M) throws IOException {
		return new TranslatorToCAst() {

			@Override
			public <C extends RewriteContext<K>, K extends CopyKey<K>> void addRewriter(
					CAstRewriterFactory<C, K> factory, boolean prepend) {
				assert false;
			}

			@Override
			public CAstEntity translateToCAst() throws Error, IOException {
				String code = new String(Streams.inputStream2ByteArray(M.getInputStream()));
				Statement statement = SQL_PARSER.createStatement(code);
				return PrestoVisitor.process(statement, code);
			}
			
		};
	}

	@Override
	protected boolean shouldTranslate(CAstEntity entity) {
		return true;
	}

	@Override
	protected TranslatorToIR initTranslator() {
		return new SQLCAstToIRTranslator(this);
	}

}
