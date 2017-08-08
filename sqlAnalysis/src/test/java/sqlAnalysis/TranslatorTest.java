package sqlAnalysis;

import static org.junit.Assert.*;

import org.junit.Test;

import com.ibm.wala.classLoader.ClassLoaderFactory;

public class TranslatorTest {
	private ClassLoaderFactory loaders = new SQLClassLoaderFactory();

	@Test
	public void testAnd() throws Exception {
		String sql = "select a, b from c where a = b and b = c";
		int i = 0;
		ParseStackOverflowData.doPresto(i, sql, loaders);

	}

}
