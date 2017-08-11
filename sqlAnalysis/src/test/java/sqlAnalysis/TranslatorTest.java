package sqlAnalysis;

import org.junit.Test;

import com.ibm.wala.cast.tree.CAstNode;
import com.ibm.wala.cast.tree.impl.CAstOperator;
import com.ibm.wala.classLoader.ClassLoaderFactory;

public class TranslatorTest {
	private ClassLoaderFactory loaders = new SQLClassLoaderFactory();

	@Test
	public void testAnd() throws Exception {
		String sql = "select a, b from c where a = b and a = 6";
		int i = 0;
		ParseStackOverflowData.doPresto(i, sql, loaders);
	}
	

	@Test
	public void testNegative() throws Exception {
		String sql = "SELECT CASE WHEN  Invoice_Type_Code = 'C' THEN -1 ELSE 1 END * Invoice_Amount " +
				" FROM Forefront.dbo.VN_GL_DISTRIBUTION_HEADER_MC where Vendor_Code ='  UnitedEL' and " +
				" Date_List1  > '2011-12-31' and Date_List1 < '2012-02-01' and Company_Code = 'tmg'";
		int i = 0;
		ParseStackOverflowData.doPresto(i, sql, loaders);

	}
	
	@Test
	public void testMax() throws Exception {
		String sql = "select max(AccessID) as AccessID,  max(AccessType) as AccessType from audit_log where Status in (335,66) group by EventTime";
		int i = 0;
		ParseStackOverflowData.doPresto(i, sql, loaders);

	}

}
