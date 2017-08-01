package sqlAnalysis;

import java.util.Collection;
import java.util.Collections;
import java.util.Set;

import com.ibm.wala.analysis.typeInference.PrimitiveType;
import com.ibm.wala.classLoader.Language;
import com.ibm.wala.ipa.cha.IClassHierarchy;
import com.ibm.wala.shrikeCT.InvalidClassFileException;
import com.ibm.wala.ssa.SSAInstructionFactory;
import com.ibm.wala.types.MethodReference;
import com.ibm.wala.types.TypeName;
import com.ibm.wala.types.TypeReference;
import com.ibm.wala.util.strings.Atom;

public class SQL implements Language {

	public static SQL sql = new SQL();
	
	private SQL() {

	}

	@Override
	public Atom getName() {
		return Atom.findOrCreateUnicodeAtom("SQL");
	}

	@Override
	public Language getBaseLanguage() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public void registerDerivedLanguage(Language l) {
		assert false;
	}

	@Override
	public Set<Language> getDerivedLanguages() {
		return Collections.emptySet();
	}

	@Override
	public TypeReference getRootType() {
		return SQLClassLoader.Any;
	}

	@Override
	public TypeReference getThrowableType() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public TypeReference getConstantType(Object o) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public boolean isNullType(TypeReference t) {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public boolean isIntType(TypeReference t) {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public boolean isLongType(TypeReference t) {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public boolean isVoidType(TypeReference t) {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public boolean isFloatType(TypeReference t) {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public boolean isDoubleType(TypeReference t) {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public boolean isStringType(TypeReference t) {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public boolean isMetadataType(TypeReference t) {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public boolean isCharType(TypeReference t) {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public boolean isBooleanType(TypeReference t) {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public Object getMetadataToken(Object value) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public TypeReference[] getArrayInterfaces() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public TypeName lookupPrimitiveType(String name) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public SSAInstructionFactory instructionFactory() {
		return JAVA.instructionFactory();
	}

	@Override
	public Collection<TypeReference> inferInvokeExceptions(MethodReference target, IClassHierarchy cha)
			throws InvalidClassFileException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public TypeReference getStringType() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public TypeReference getPointerType(TypeReference pointee) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public PrimitiveType getPrimitive(TypeReference reference) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public boolean methodsHaveDeclaredParameterTypes() {
		// TODO Auto-generated method stub
		return false;
	}

}
