package AttributeSelection;

import java.io.FileWriter;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collections;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import utils.ToOrder;
import utils.WriteReadFromFile;
import weka.attributeSelection.ASEvaluation;
import weka.attributeSelection.ASSearch;
import weka.attributeSelection.AttributeSelection;
import weka.attributeSelection.CfsSubsetEval;
import weka.attributeSelection.CorrelationAttributeEval;
import weka.attributeSelection.GainRatioAttributeEval;
import weka.attributeSelection.GreedyStepwise;
import weka.attributeSelection.InfoGainAttributeEval;
import weka.attributeSelection.Ranker;
import weka.attributeSelection.ReliefFAttributeEval;
import weka.core.Instances;
import weka.core.Utils;
import weka.core.converters.ConverterUtils.DataSource;

public class AttRanking {

	int atts = 0;

	double average[];

	public AttRanking(String fileName, String outputPath, String evaluationMode) {

		try {

			DataSource wekaData= new DataSource(fileName);

			Instances dataset = wekaData.getDataSet();

			dataset.setClassIndex(dataset.numAttributes() - 1);

			/*Normalize normalize= new Normalize();

			normalize.setIgnoreClass(true);

			normalize.setInputFormat(dataset);

			dataset= Filter.useFilter(dataset, normalize);*/

			atts = dataset.numAttributes() - 1;

			ArrayList<ASEvaluation> evaluators = createListEvaluators();

			ArrayList<ASSearch> searchs = createListSearch();

			int count = 0;

			Files.createDirectories(Paths.get(outputPath));

			for (ASEvaluation evaluator : evaluators) {

				for (ASSearch search : searchs) {

					if (evaluator instanceof CfsSubsetEval && search instanceof Ranker)
						continue;

					if (!(evaluator instanceof CfsSubsetEval) && search instanceof GreedyStepwise)
						continue;

					// Perform attribute evaluation
					AttributeSelection sel = new AttributeSelection();

					// set seed
					sel.setSeed(1);

					// perfom cross validation
					sel.setXval(true);
					
					if(evaluationMode.equals("LeaveOneOut"))
					{
					// set num folds
					sel.setFolds(dataset.numInstances());
					}
					
					if(evaluationMode.equals("10Folds"))
					{
						// set num folds
						sel.setFolds(10);
					}
					

					// the search algorithm
					sel.setSearch(search);

					// the evaluator
					sel.setEvaluator(evaluator);

					// perform att
					sel.SelectAttributes(dataset);

					String fileNameT = evaluator.getClass().getSimpleName() + "-" + search.getClass().getSimpleName();

					if (evaluator instanceof ReliefFAttributeEval)
						fileNameT += "-" + ((ReliefFAttributeEval) evaluator).getNumNeighbours();

					fileNameT += ".txt";

					FileWriter writer = new FileWriter(outputPath + fileNameT);

					String content = sel.CVResultsString();

					writer.write(content);

					writer.close();

					double[] values = null;

					if (evaluator instanceof CfsSubsetEval)
						values = extractValues1(content);

					if (!(evaluator instanceof CfsSubsetEval))
						values = extractValues2(content);

					if (average == null)
						average = values;
					else
						add(values);

					count++;

				}
			}

			average(count);

			ArrayList<ToOrder> order= new ArrayList<>();

			for(int i=0;i<average.length;i++)
				order.add(new ToOrder(average[i], i));

			Collections.sort(order);
			
			Files.createDirectories(Paths.get(outputPath));

			FileWriter writer= new FileWriter(outputPath+"sortedfeatures.txt");

			StringBuilder bu= new StringBuilder();

			for(ToOrder o: order){
				bu.append(o.getPos()).append("\t").append(dataset.attribute(o.getPos()).name()).append("\t").append(o.getValue()).append("\n");
			}

			writer.write(bu.toString());
			writer.close();



		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

	}

	private void add(double[] values) {

		for (int i = 0; i < values.length; i++)
			average[i] += values[i];

	}

	private void average(int count) {

		for (int i = 0; i < average.length; i++)
			average[i] /= count;

	}

	public ArrayList<ASEvaluation> createListEvaluators() {

		ArrayList<ASEvaluation> evaluators = new ArrayList<>();

		//evaluators.add(new CfsSubsetEval());

		evaluators.add(new CorrelationAttributeEval());

		evaluators.add(new InfoGainAttributeEval());

		evaluators.add(new GainRatioAttributeEval());

		ReliefFAttributeEval relief1 = new ReliefFAttributeEval();

		relief1.setNumNeighbours(5);

		evaluators.add(relief1);

		//ReliefFAttributeEval relief2 = new ReliefFAttributeEval();

		//relief2.setNumNeighbours(10);

		//evaluators.add(relief2);

		//ReliefFAttributeEval relief3 = new ReliefFAttributeEval();

		//relief3.setNumNeighbours(15);

		//evaluators.add(relief3);

		return evaluators;
	}

	public ArrayList<ASSearch> createListSearch() {

		ArrayList<ASSearch> search = new ArrayList<>();

		search.add(new Ranker());

		search.add(new GreedyStepwise());

		return search;

	}

	// returns the value of each att
	public double[] extractValues1(String fileContent) {

		double[] values = new double[atts];

		String[] rows = fileContent.split("\n");

		for (int i = 5; i < rows.length; i++) {

			String row = rows[i];

			Pattern p1 = Pattern.compile("\\d+ %");

			Pattern p2 = Pattern.compile("\\d+ [\\w-/]+$");

			Matcher m = p1.matcher(row);

			int value = 0;
			int index = 0;

			if (m.find()) {
				value = Integer.parseInt(m.group(0).split(" ")[0]);
			}

			m = p2.matcher(row);

			if (m.find()) {
				String s = m.group(0).split(" ")[0];
				index = Integer.parseInt(s);
			}

			values[index - 1] = (100 - value) / (double) 100;
		}

		return values;

	}

	// returns the value of each att
	public double[] extractValues2(String fileContent) {

		double[] values = new double[atts];

		String[] rows = fileContent.split("\n");

		for (int i = 5; i < rows.length; i++) {

			String row = rows[i];

			Pattern p1 = Pattern.compile("\\s{2,}\\d+\\.?\\d*\\s+\\+-");

			Pattern p2 = Pattern.compile("\\d+ [\\w-/]+$");

			Matcher m = p1.matcher(row);

			double value = 0;
			int index = 0;

			if (m.find()) {
				String s[] = m.group(0).trim().split(" ");
				value = Double.parseDouble(s[0]);
			}

			m = p2.matcher(row);

			if (m.find()) {
				index = Integer.parseInt(m.group(0).split(" ")[0]);
			}

			values[index - 1] = value;
		}

		double min = values[Utils.minIndex(values)];
		double max = values[Utils.maxIndex(values)];

		for (int i = 0; i < values.length; i++)
			values[i] = (values[i] - min) / (max - min);

		return values;

	}

	public static void main(String args[]) throws Exception {

		WriteReadFromFile file= new WriteReadFromFile();

		String evaluationMode = Utils.getOption("eval", args);
		
		ArrayList<String> datasets= file.read("config/datasets.txt");
		
		for(String dataset:datasets){

			AttRanking eval = new AttRanking("datasets/"+dataset+".csv","reports/"+dataset+"/", evaluationMode);
		}
	}
}