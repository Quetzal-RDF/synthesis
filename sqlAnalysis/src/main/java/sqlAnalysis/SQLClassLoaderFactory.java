package sqlAnalysis;

import com.ibm.wala.cast.loader.SingleClassLoaderFactory;
import com.ibm.wala.classLoader.IClassLoader;
import com.ibm.wala.ipa.cha.IClassHierarchy;
import com.ibm.wala.types.ClassLoaderReference;
import com.ibm.wala.util.strings.Atom;

public class SQLClassLoaderFactory extends SingleClassLoaderFactory {

	public static final Atom loaderName = Atom.findOrCreateUnicodeAtom("SQL");
	
	public static final ClassLoaderReference Sql = new ClassLoaderReference(SQL.sql.getName(), loaderName, null);
	
	@Override
	public ClassLoaderReference getTheReference() {
		return Sql;
	}

	@Override
	protected IClassLoader makeTheLoader(IClassHierarchy cha) {
		return new SQLClassLoader(cha);
	}

}
