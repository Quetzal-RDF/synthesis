package sqlAnalysis;

import org.junit.Test;

import com.ibm.wala.classLoader.ClassLoaderFactory;

public class TranslatorTest {
	private ClassLoaderFactory loaders = new SQLClassLoaderFactory();

	@Test
	public void testAnd() throws Exception {
		String sql = "select a, b from c where a = b and a = 6";
		
		SQLToGraph.doPresto(sql, loaders);
	}
	

	@Test
	public void testNegative() throws Exception {
		String sql = "SELECT CASE WHEN  Invoice_Type_Code = 'C' THEN -1 ELSE 1 END * Invoice_Amount " +
				" FROM Forefront.dbo.VN_GL_DISTRIBUTION_HEADER_MC where Vendor_Code ='  UnitedEL' and " +
				" Date_List1  > '2011-12-31' and Date_List1 < '2012-02-01' and Company_Code = 'tmg'";
		
		SQLToGraph.doPresto(sql, loaders);

	}
	
	@Test
	public void testMax() throws Exception {
		String sql = "select max(AccessID) as AccessID,  max(AccessType) as AccessType from audit_log where Status in (335,66) group by EventTime";
		
		SQLToGraph.doPresto(sql, loaders);
	}

	@Test
	public void testFuns() throws Exception {
		String sql = "select a, b from c where sqrt(abs(a)) = b";
		
		SQLToGraph.doPresto(sql, loaders);
	}

	@Test
	public void testExists() throws Exception {
		String sql = "SELECT * FROM Courses WHERE NOT EXISTS (SELECT * FROM Scores WHERE CourseID = Courses.CourseID AND UserID = userID)";
		
		SQLToGraph.doPresto(sql, loaders);
	}
	
	@Test
	public void testExists2() throws Exception {
		String sql = "select a.acc_ref, a.bill_no from table1 a where exists (select acc_ref, bill_no, SUM (tran_amount) from table2 b where a.acc_ref = b.acc_ref and a.bill_no = b.bill_no group by acc_ref)";
		
		SQLToGraph.doPresto(sql, loaders);
	}
	
	@Test
	public void testSubquery() throws Exception {
		String sql = "SELECT name, height FROM people WHERE height = (SELECT MAX(height) FROM people)";
		
		SQLToGraph.doPresto(sql, loaders);
	
	}
	
	@Test
	public void testExtract() throws Exception {
		String sql = "SELECT EXTRACT(MONTH FROM bus_date) AS month, SUM (sales) AS sales FROM wmw_st_bte GROUP BY EXTRACT(MONTH FROM bus_date) ORDER BY EXTRACT(MONTH FROM bus_date)";
		
		SQLToGraph.doPresto(sql, loaders);
		
	}
	
	@Test
	public void testFunctionCalls() throws Exception {
		String sql = "select t.corridor, s.corridor_code_rb,t.roadway, s.SVYLENG2012, round(cast(t.frfpost as float), 3) as frfpost_short, " +
				"s.FRFPOST, s.BEG_GN from SEC_FILE_IMPORT_2014 t join NORTH_VAN_DATA_VIEW_MOD_032015 s on round(cast(t.frfpost as float), 3) = s.FRFPOST and t.corridor = s.CORRIDOR_CODE";
		
		SQLToGraph.doPresto(sql, loaders);

	}
	
}
