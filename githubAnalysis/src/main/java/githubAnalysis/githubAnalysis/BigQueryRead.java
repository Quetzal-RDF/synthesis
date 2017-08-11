/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package githubAnalysis.githubAnalysis;

import org.apache.beam.sdk.Pipeline;
import org.apache.beam.sdk.io.TextIO;
import org.apache.beam.sdk.io.gcp.bigquery.BigQueryIO;
import org.apache.beam.sdk.options.Description;
import org.apache.beam.sdk.options.PipelineOptions;
import org.apache.beam.sdk.options.PipelineOptionsFactory;
import org.apache.beam.sdk.options.Validation.Required;
import org.apache.beam.sdk.transforms.DoFn;
import org.apache.beam.sdk.transforms.ParDo;

import com.google.api.services.bigquery.model.TableRow;

/**
 * An example that reads 5 files from BigQuery - adapted from WordCount.
 *
 * <p>This class, {@link BigQueryRead}, is the second in a series of four successively more detailed
 * 'word count' examples. You may first want to take a look at {@link MinimalWordCount}.
 * After you've looked at this example, then see the {@link DebuggingWordCount}
 * pipeline, for introduction of additional concepts.
 *
 * <p>For a detailed walkthrough of this example, see
 *   <a href="https://beam.apache.org/get-started/wordcount-example/">
 *   https://beam.apache.org/get-started/wordcount-example/
 *   </a>
 *
 * <p>Basic concepts, also in the MinimalWordCount example:
 * Reading text files; counting a PCollection; writing to text files
 *
 * <p>New Concepts:
 * <pre>
 *   1. Executing a Pipeline both locally and using the selected runner
 *   2. Using ParDo with static DoFns defined out-of-line
 *   3. Building a composite transform
 *   4. Defining your own pipeline options
 * </pre>
 *
 * <p>Concept #1: you can execute this pipeline either locally or using by selecting another runner.
 * These are now command-line options and not hard-coded as they were in the MinimalWordCount
 * example.
 *
 * <p>To change the runner, specify:
 * <pre>{@code
 *   --runner=YOUR_SELECTED_RUNNER
 * }
 * </pre>
 *
 * <p>To execute this pipeline, specify a local output file (if using the
 * {@code DirectRunner}) or output prefix on a supported distributed file system.
 * <pre>{@code
 *   --output=[YOUR_LOCAL_FILE | YOUR_OUTPUT_PREFIX]
 * }</pre>
 *
 * <p>The input file defaults to a public data set containing the text of of King Lear,
 * by William Shakespeare. You can override it and choose your own input with {@code --inputFile}.
 */
public class BigQueryRead {

  /**
   * Concept #2: You can make your pipeline assembly code less verbose by defining your DoFns
   * statically out-of-line. This DoFn gets a sql statement and tries to parse it
   */
  static class ParseSQL extends DoFn<TableRow, String> {

    /**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	@ProcessElement
    public void processElement(ProcessContext c) {
      if (c.element().containsKey("content")) {
    	  c.output(c.element().get("content").toString());
      }
    }
  }


  /**
   * Options supported by {@link BigQueryRead}.
   *
   * <p>Concept #4: Defining your own configuration options. Here, you can add your own arguments
   * to be processed by the command-line parser, and specify default values for them. You can then
   * access the options values in your pipeline code.
   *
   * <p>Inherits standard configuration options.
   */
  public interface BigQueryReadOptions extends PipelineOptions {
    /**
     * Set this required option to specify where to write the output.
     */
    @Description("Path of the file to write to")
    @Required
    String getOutput();
    void setOutput(String value);
  }

  public static void main(String[] args) {
    BigQueryReadOptions options = PipelineOptionsFactory.fromArgs(args).withValidation()
      .as(BigQueryReadOptions.class);
    Pipeline p = Pipeline.create(options);

    // Concepts #2 and #3: Our pipeline applies the composite CountWords transform, and passes the
    // static FormatAsTextFn() to the ParDo transform.
    p.apply("ReadLines", BigQueryIO.read().fromQuery("SELECT content FROM `quetzal-166513.quetzal.github_sql_dump` LIMIT 2").usingStandardSql())
    .apply(ParDo.of(new ParseSQL()))
    .apply("WriteSQL", TextIO.write().to(options.getOutput()));;

    p.run().waitUntilFinish();
  }
}
