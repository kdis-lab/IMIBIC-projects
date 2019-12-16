package AttributeSelection;

import java.io.File;
import java.io.FileWriter;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Random;
import utils.WriteReadFromFile;
import weka.classifiers.Evaluation;
import weka.core.Instances;
import weka.core.converters.ConverterUtils.DataSource;
import weka.filters.Filter;
import weka.filters.unsupervised.attribute.Normalize;
import weka.filters.unsupervised.attribute.Remove;

public class AttSelectionClassification {

	public AttSelectionClassification() {

		try {

			WriteReadFromFile file = new WriteReadFromFile();

			ArrayList<String> datasets = file.read("config/datasets.txt");

			ArrayList<String> algorithms = file.read("config/classification.txt");

			ArrayList<String> seeds = file.read("config/seeds.txt");

			// for each dataset
			for (String datasetName : datasets) {

				System.out.println("Begin dataset "+ datasetName);

				// Load the dataset

				DataSource wekaDataSource= new DataSource("datasets/" + datasetName + ".csv");
				Instances dataset = wekaDataSource.getDataSet();

				dataset.setClassIndex(dataset.numAttributes() - 1);

				// Load the ranking of attributes
				ArrayList<String> lines = file.read("reports/" + datasetName + "/sortedfeatures.txt");

				int indexes[] = new int[lines.size()];

				int pos = 0;

				// for each attribute
				for (String att : lines) {

					int index = Integer.parseInt(att.split("\t")[0]);

					indexes[pos] = index;

					pos++;
				}

				// for each algorithm
				for (String classifierName : algorithms) {

					String classifier = classifierName.substring(classifierName.lastIndexOf('.') + 1);

					System.out.println("Begin algorithm " + classifier);

					String reportDirectory = "reports/" + datasetName + "/" + classifier;

					Files.createDirectories(Paths.get(reportDirectory));

					File fileResults = new File(reportDirectory + "/bestResults.txt");

					StringBuilder stringSummary = new StringBuilder();

					// for each seed
					for (String seedName : seeds) {

						System.out.println("Begin seed " + seedName);

						int seed = Integer.parseInt(seedName);

						stringSummary.append(doSelection(indexes, dataset, classifierName, seed)).append("\n\n");

						System.out.println("End seed " + seedName);
					}

					FileWriter writer = new FileWriter(fileResults);

					writer.write(stringSummary.toString());

					writer.close();

					System.out.println("End algorithm " + classifier);
				}

				System.out.println("End dataset "+ datasetName);
			}

		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

	}

	public String doSelection(int[] indexes, Instances dataset, String algorithm, int seed) throws Exception {

		// Until the value of the classifier is lower than previous
		// iteration

		double bestAverageGlobal = Double.MIN_VALUE;
		ArrayList<Integer> attsSelected = null;
		// Instances datasetGlobal;

		for (int pivot = 0; pivot < indexes.length; pivot++) {

			ArrayList<Integer> attsSelectedTemp = new ArrayList<>();
			attsSelectedTemp.add(dataset.classIndex());

			double bestAverageTemp = Double.MIN_VALUE;
			// Instances datasetTemp = null;

			for (int i = -1; i < indexes.length; i++) {

				ArrayList<Integer> attsSelectedTemp2 = new ArrayList<>(attsSelectedTemp);

				// the first iteration, the pivot is added
				if (i == -1) {
					attsSelectedTemp2.add(0, indexes[pivot]);
				} else {

					if (i == pivot)
						continue;

					attsSelectedTemp2.add(0, indexes[i]);
				}

				// Remove the atts
				Instances datasetTemp2 = reduceDataset(attsSelectedTemp2, dataset);

				// run the classifier
				double average = evaluateModel(algorithm, datasetTemp2, seed);

				if (bestAverageTemp < average) {

					bestAverageTemp = average;
					attsSelectedTemp = attsSelectedTemp2;
					// datasetTemp = new Instances(datasetTemp2);
				}
			}

			if (bestAverageGlobal < bestAverageTemp) {

				// System.out.println("Less");

				bestAverageGlobal = bestAverageTemp;
				attsSelected = attsSelectedTemp;
				// datasetGlobal = new Instances(datasetTemp);

				// printSummary(attsSelected, dataset, bestAverageGlobal);

			} else {

				if (bestAverageGlobal == bestAverageTemp) {

					if (attsSelected.size() > attsSelectedTemp.size()) {

						// System.out.println("Equal");

						bestAverageGlobal = bestAverageTemp;
						attsSelected = attsSelectedTemp;
						// datasetGlobal = new Instances(datasetTemp);

						// printSummary(attsSelected, dataset,
						// bestAverageGlobal);

					}

				}

			}

		}

		// System.out.println();

		// System.out.println("///////////BEST////////////////////");

		// System.out.println();

		// printSummary(attsSelected, dataset, bestAverageGlobal);

		/*
		 * FileWriter writer = new FileWriter(nameDatasetReduced);
		 *
		 * writer.write(datasetGlobal.toString());
		 *
		 * writer.close();
		 */

		return summary(seed, attsSelected, dataset, bestAverageGlobal);

	}

	public void printSummary(ArrayList<Integer> array, Instances dataset, double average) {

		System.out.println("///////////////////////////////");

		System.out.println();

		System.out.println("Number of atts: " + (array.size() - 1));

		StringBuilder st = new StringBuilder();

		for (int ats = 0; ats < array.size() - 1; ats++) {
			st.append(dataset.attribute(array.get(ats)).name()).append(" ");
		}

		System.out.println("Attributes: " + st.toString());

		System.out.println("Average AUC: " + average);

		System.out.println();
	}

	public String summary(int seed, ArrayList<Integer> array, Instances dataset, double average) {

		StringBuilder string = new StringBuilder();

		string.append("Seed: ").append(seed).append("\n");

		string.append("Number of atts: ").append(array.size() - 1).append("\n");

		string.append("Attributes: ");

		for (int ats = 0; ats < array.size() - 1; ats++) {
			string.append(dataset.attribute(array.get(ats)).name()).append(" ");
		}

		string.append("\n");

		string.append("Average AUC: " + average).append("\n");

		return string.toString();
	}

	public double evaluateModel(String algorithm, Instances dataset, int seed) throws Exception {

		Evaluation eval = new Evaluation(dataset);

		eval.crossValidateModel(algorithm, dataset, dataset.numInstances(), new String[] { "" }, new Random(seed));

		return eval.weightedAreaUnderROC();
	}

	public Instances reduceDataset(ArrayList<Integer> array, Instances dataset) throws Exception {

		Instances datasetTemp = new Instances(dataset);

		// Remove the atts

		Remove remove = new Remove();

		remove.setAttributeIndicesArray(toIntArray(array));

		remove.setInvertSelection(true);

		remove.setInputFormat(datasetTemp);

		datasetTemp = Filter.useFilter(datasetTemp, remove);

		datasetTemp.setClassIndex(datasetTemp.numAttributes() - 1);

		// Normalize the atts

		Normalize normalize = new Normalize();

		normalize.setInputFormat(datasetTemp);

		datasetTemp = Filter.useFilter(datasetTemp, normalize);

		datasetTemp.setClassIndex(datasetTemp.numAttributes() - 1);

		return datasetTemp;

	}

	private int[] toIntArray(ArrayList<Integer> array) {

		int[] ar = new int[array.size()];

		for (int i = 0; i < array.size(); i++) {

			ar[i] = array.get(i);

		}

		return ar;
	}

	public static void main(String args[]) {

		AttSelectionClassification sel = new AttSelectionClassification();

	}
}
