package sqlAnalysis;

// Imports the Google Cloud client library
import com.google.cloud.bigquery.BigQuery;
import com.google.cloud.bigquery.BigQueryOptions;
import com.google.cloud.bigquery.Dataset;
import com.google.cloud.bigquery.DatasetId;
import com.google.cloud.bigquery.DatasetInfo;

public class AccessGitHub {
	public static void main(String... args) throws Exception {
		// Instantiates a client
		BigQuery bigquery = BigQueryOptions.getDefaultInstance().getService();

		String projectId = "quetzal-166513";
		String datasetName = "quetzal";
		DatasetId datasetId = DatasetId.of(projectId, datasetName);
		
		// Creates the dataset
		Dataset dataset = bigquery.getDataset(datasetId);
		
		System.out.printf("Dataset %s created.%n", dataset.getDatasetId().getDataset());
	}
}
