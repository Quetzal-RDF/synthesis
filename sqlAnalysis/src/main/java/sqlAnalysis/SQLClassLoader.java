package sqlAnalysis;

import java.io.IOException;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.Map;

import com.facebook.presto.sql.parser.SqlParser;
import com.facebook.presto.sql.tree.Statement;
import com.ibm.wala.cast.ir.translator.AstTranslator;
import com.ibm.wala.cast.ir.translator.AstTranslator.AstLexicalInformation;
import com.ibm.wala.cast.ir.translator.AstTranslator.WalkContext;
import com.ibm.wala.cast.ir.translator.RewritingTranslatorToCAst;
import com.ibm.wala.cast.ir.translator.TranslatorToCAst;
import com.ibm.wala.cast.ir.translator.TranslatorToCAst.Error;
import com.ibm.wala.cast.ir.translator.TranslatorToIR;
import com.ibm.wala.cast.loader.AstDynamicPropertyClass;
import com.ibm.wala.cast.loader.AstFunctionClass;
import com.ibm.wala.cast.loader.AstMethod;
import com.ibm.wala.cast.loader.AstMethod.DebuggingInformation;
import com.ibm.wala.cast.loader.AstMethod.Retranslatable;
import com.ibm.wala.cast.loader.CAstAbstractModuleLoader;
import com.ibm.wala.cast.tree.CAst;
import com.ibm.wala.cast.tree.CAstEntity;
import com.ibm.wala.cast.tree.rewrite.CAstRewriter;
import com.ibm.wala.cast.tree.rewrite.CAstRewriter.CopyKey;
import com.ibm.wala.cast.tree.rewrite.CAstRewriter.RewriteContext;
import com.ibm.wala.cast.tree.rewrite.CAstRewriterFactory;
import com.ibm.wala.cast.types.AstMethodReference;
import com.ibm.wala.cast.util.CAstPrinter;
import com.ibm.wala.cfg.AbstractCFG;
import com.ibm.wala.cfg.IBasicBlock;
import com.ibm.wala.classLoader.IClass;
import com.ibm.wala.classLoader.IClassLoader;
import com.ibm.wala.classLoader.Language;
import com.ibm.wala.classLoader.ModuleEntry;
import com.ibm.wala.ipa.cha.IClassHierarchy;
import com.ibm.wala.ssa.SSAInstruction;
import com.ibm.wala.ssa.SSAInstructionFactory;
import com.ibm.wala.ssa.SymbolTable;
import com.ibm.wala.types.ClassLoaderReference;
import com.ibm.wala.types.TypeName;
import com.ibm.wala.types.TypeReference;
import com.ibm.wala.types.annotations.Annotation;
import com.ibm.wala.util.io.Streams;

public class SQLClassLoader extends CAstAbstractModuleLoader {

	static final SqlParser SQL_PARSER = new SqlParser();

	public static TypeReference Any = TypeReference.findOrCreate(SQLClassLoaderFactory.Sql, "LAny");
	
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
		return getLanguage().instructionFactory();
	}

	/**
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
				CAstEntity e = PrestoVisitor.process(statement, code);
				System.out.println(code);
				return e;
			}
			
		};
	}
   */
	@Override
	protected TranslatorToCAst getTranslatorToCAst(CAst ast, ModuleEntry M) throws IOException {
		RewritingTranslatorToCAst xlator = new RewritingTranslatorToCAst(M, new TranslatorToCAst() {

			@Override
			public <C extends RewriteContext<K>, K extends CopyKey<K>> void addRewriter(
					CAstRewriterFactory<C, K> factory, boolean prepend) {
				assert false;
			}

			@Override
			public CAstEntity translateToCAst() throws Error, IOException {
				String code = new String(Streams.inputStream2ByteArray(M.getInputStream()));
				Statement statement = SQL_PARSER.createStatement(code);
				CAstEntity e = PrestoVisitor.process(statement, code);
				System.out.println(code);
				return e;
			}
			
		}) {

			@Override
			public CAstEntity translateToCAst() throws IOException, Error {
				
				CAstEntity e = super.translateToCAst();
				System.out.println(CAstPrinter.print(e));
				return e;
			} 
			
		};
		@SuppressWarnings("rawtypes")
		CAstRewriterFactory<?, ?> factory = new CAstRewriterFactory() {
			@Override
			public CAstRewriter<?, ?> createCAstRewriter(CAst ast) {
				return new AndOrRewriter(ast, true);
			}	
		};
		xlator.addRewriter(factory, false);
		return xlator;
	}
	
	@Override
	protected boolean shouldTranslate(CAstEntity entity) {
		return true;
	}

	@Override
	protected TranslatorToIR initTranslator() {
		return new SQLCAstToIRTranslator(this);
	}

	@Override
	public Iterator<IClass> iterateAllClasses() {
		return types.values().iterator();
	}

	public void defineFunction(CAstEntity n, WalkContext definingContext,
			AbstractCFG<SSAInstruction, ? extends IBasicBlock<SSAInstruction>> cfg, SymbolTable symtab,
			boolean hasCatchBlock, Map<IBasicBlock<SSAInstruction>, TypeReference[]> catchTypes, boolean hasMonitorOp,
			AstLexicalInformation lexicalInfo, DebuggingInformation debugInfo) {
		class SQLMethodObject extends AstMethod implements Retranslatable {

			SQLMethodObject(IClass cls) {
				super(cls, Collections.emptySet(), cfg, symtab, AstMethodReference.fnReference(cls.getReference()), hasCatchBlock, catchTypes,
						hasMonitorOp, lexicalInfo, debugInfo, null);
			}


			@Override
			public CAstEntity getEntity() {
				return n;
			}


			@Override
			public void retranslate(AstTranslator xlator) {
				xlator.translate(n, definingContext);
			}

			@Override
			public IClassHierarchy getClassHierarchy() {
				return cha;
			}

			@Override
			public String toString() {
				return "<Code body of " + cls + ">";
			}

			@Override
			public TypeReference[] getDeclaredExceptions() {
				return null;
			}

			@Override
			public LexicalParent[] getParents() {
				if (lexicalInfo() == null)
					return new LexicalParent[0];

				final String[] parents = lexicalInfo().getScopingParents();

				if (parents == null)
					return new LexicalParent[0];

				LexicalParent result[] = new LexicalParent[parents.length];

				for (int i = 0; i < parents.length; i++) {
					final int hack = i;
					final AstMethod method = (AstMethod) lookupClass(parents[i], cha).getMethod(AstMethodReference.fnSelector);
					result[i] = new LexicalParent() {
						@Override
						public String getName() {
							return parents[hack];
						}

						@Override
						public AstMethod getMethod() {
							return method;
						}
					};

					if (AstTranslator.DEBUG_LEXICAL) {
						System.err.println(("parent " + result[i].getName() + " is " + result[i].getMethod()));
					}
				}

				return result;
			}

			@Override
			public String getLocalVariableName(int bcIndex, int localNumber) {
				return null;
			}

			@Override
			public boolean hasLocalVariableTable() {
				return false;
			}

			@Override
			public TypeReference getParameterType(int i) {
				if (i == 0) {
					return getDeclaringClass().getReference();
				} else {
					return SQLClassLoader.Any;
				}
			}
		}
		
		class SQLCodeBody extends AstFunctionClass {

			public SQLCodeBody(TypeReference codeName) {
				super(codeName, SQLClassLoader.this, null);
			}

			
			@Override
			public IClass getSuperclass() {
				return types.get(SQL.sql.getRootType().getName());
			}


			@Override
			public IClassHierarchy getClassHierarchy() {
				return cha;
			}

			@Override
			public Collection<Annotation> getAnnotations() {
				return Collections.emptySet();
			}

			private void setCodeBody(AstMethod code) {
				this.functionBody = code;
			}
		}

		SQLCodeBody cls = new SQLCodeBody(TypeReference.findOrCreate(getReference(), TypeName.findOrCreate(n.getName())));
		SQLMethodObject code = new SQLMethodObject(cls);
		cls.setCodeBody(code);

		types.put(cls.getReference().getName(), cls);
	}
	
	public class SQLRootClass extends AstDynamicPropertyClass {

		private SQLRootClass(IClassLoader loader) {
			super(null, SQL.sql.getRootType().getName(), loader, (short) 0, Collections.emptyMap(), SQL.sql.getRootType());

			types.put(SQL.sql.getRootType().getName(), this);
		}

		@Override
		public IClassHierarchy getClassHierarchy() {
			return cha;
		}

		@Override
		public String toString() {
			return "SQL Root:" + getReference().toString();
		}

		@Override
		public Collection<IClass> getDirectInterfaces() {
			return Collections.emptySet();
		}

		@Override
		public IClass getSuperclass() {
			return null;
		}

		@Override
		public Collection<Annotation> getAnnotations() {
			return Collections.emptySet();
		}
	}

	{
		types.put(SQL.sql.getRootType().getName(), new SQLRootClass(this));
	}
}
