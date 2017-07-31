package sqlAnalysis;

import java.io.InputStream;
import java.io.Reader;
import java.io.StringReader;
import java.net.URL;
import java.util.Iterator;

import com.ibm.wala.classLoader.Module;
import com.ibm.wala.classLoader.ModuleEntry;
import com.ibm.wala.classLoader.SourceModule;
import com.ibm.wala.util.collections.NonNullSingletonIterator;

public class SQLSourceModule implements SourceModule {

	private final String code;
	
	public SQLSourceModule(String code) {
		this.code = code;
	}

	@Override
	public Iterator<? extends ModuleEntry> getEntries() {
		return new NonNullSingletonIterator<>(this);
	}

	@Override
	public String getName() {
		return "sql source";
	}

	@Override
	public boolean isClassFile() {
		return false;
	}

	@Override
	public boolean isSourceFile() {	
		return true;
	}

	@Override
	public InputStream getInputStream() {
		assert false;
		return null;
	}

	@Override
	public boolean isModuleFile() {
		return false;
	}

	@Override
	public Module asModule() {
		return this;
	}

	@Override
	public String getClassName() {
		return null;
	}

	@Override
	public Module getContainer() {
		return null;
	}

	@Override
	public Reader getInputReader() {
		return new StringReader(code);
	}

	@Override
	public URL getURL() {
		return null;
	}

}
