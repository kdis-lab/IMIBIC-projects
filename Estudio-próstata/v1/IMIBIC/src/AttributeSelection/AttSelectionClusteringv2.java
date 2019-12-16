package AttributeSelection;

import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Random;

import utils.WriteReadFromFile;
import weka.classifiers.Evaluation;
import weka.classifiers.evaluation.NominalPrediction;
import weka.classifiers.evaluation.Prediction;
import weka.classifiers.evaluation.ThresholdCurve;
import weka.clusterers.ClusterEvaluation;
import weka.clusterers.Clusterer;
import weka.clusterers.HierarchicalClusterer;
import weka.clusterers.SimpleKMeans;
import weka.core.Instances;
import weka.core.SelectedTag;
import weka.core.converters.ConverterUtils.DataSource;
import weka.filters.Filter;
import weka.filters.unsupervised.attribute.Normalize;
import weka.filters.unsupervised.attribute.Remove;

public class AttSelectionClusteringv2 {

	public AttSelectionClusteringv2(String datasetName, String algorithmName, double threshold, String type) {

		try {

			algorithmName = algorithmName.substring(algorithmName.lastIndexOf(".") + 1);

			String outputPath = "reports/" + datasetName + "/";

			DataSource wekaDataSource = new DataSource("datasets/" + datasetName + ".csv");

			Instances originalDataset = wekaDataSource.getDataSet();

			originalDataset.setClassIndex(originalDataset.numAttributes() - 1);

			WriteReadFromFile file = new WriteReadFromFile();

			// Features
			ArrayList<String> lines = file.read(outputPath + "sortedfeatures.txt");

			int indexes[] = new int[lines.size()];

			int pos = 0;

			// for each attribute
			for (String line : lines) {

				int index = Integer.parseInt(line.split("\t")[0]);

				indexes[pos] = index;

				pos++;
			}

			// Load the files with significant factors

			ArrayList<String> significantFactors = file.read(outputPath + "SignificantFactors.txt");

			int numClusters = originalDataset.numClasses();

			ArrayList<Summary> summariesAllSignificants = new ArrayList<>();
			ArrayList<Summary> summariesNotAllSignificants = new ArrayList<>();

			ArrayList<Summary> summaries = new ArrayList<>();

			for (int pivot = 0; pivot < indexes.length; pivot++) {

				ArrayList<Integer> attsSelected = new ArrayList<>();

				attsSelected.add(indexes[pivot]);
				attsSelected.add(originalDataset.classIndex());

				double bestAUC = Double.MIN_VALUE;

				for (int i = 0; i < indexes.length; i++) {

					if (i == pivot)
						continue;

					ArrayList<Integer> attsSelectedTemp = new ArrayList<>(attsSelected);

					attsSelectedTemp.add(attsSelectedTemp.size() - 1, indexes[i]);

					// Remove the other atts
					Instances datasetTemp = reduceDataset(attsSelectedTemp, originalDataset);

					// evaluate the atts, only if this combination was not
					// already evaluated

					Summary sG = new Summary(0, 0, combineString(attsSelectedTemp, originalDataset), 0);

					if (!summaries.contains(sG)) {

						double auc=0;

						if (type.equals("clustering")) {
							// run the clustering algorithm
							auc = evaluateModelClustering(numClusters, datasetTemp, algorithmName, false);

							if (auc >= bestAUC) {

								attsSelected = attsSelectedTemp;
								bestAUC = auc;
							}
						}

						if (type.equals("classification")) {
							// run the clustering algorithm
							auc = evaluateModelClassification(algorithmName, datasetTemp);

							if (auc >= bestAUC) {

								attsSelected = attsSelectedTemp;
								bestAUC = auc;
							}
						}

						// Register in the file
						if (auc >= threshold) {

							Summary summ = summary(attsSelectedTemp, originalDataset, auc, significantFactors);

							if (summ.numAtt == summ.numberOfSignificants)
								summariesAllSignificants.add(summ);
							else
								summariesNotAllSignificants.add(summ);
						}

						summaries.add(sG);
					}
				}
			}

			Files.createDirectories(Paths.get(outputPath + algorithmName + "/"));

			FileWriter writer = new FileWriter(
					outputPath + algorithmName + "/" + "bestClusteringAllSignificantFactors.csv");

			writer.write(createCSV(summariesAllSignificants));

			writer.close();

			writer = new FileWriter(outputPath + algorithmName + "/" + "bestClusteringNotAllSignificantFactors.csv");

			writer.write(createCSV(summariesNotAllSignificants));

			writer.close();

		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	public String createCSV(ArrayList<Summary> summaries) {

		StringBuilder csv = new StringBuilder();

		csv.append("Number-Atts,Number-Significant,Atts,AUC\n");

		// order the summary by auc
		Collections.sort(summaries);

		for (Summary sum : summaries) {

			csv.append(sum).append("\n");
		}

		return csv.toString();
	}

	public int numberOfSignificants(String atts, ArrayList<String> set) {

		String args[] = atts.split(" ");

		int number = 0;

		for (String st : args) {
			if (set.contains(st)) {
				number++;
			}
		}

		return number;
	}

	public Summary summary(ArrayList<Integer> array, Instances dataset, double average,
			ArrayList<String> significantFactors) {

		String atts = combineString(array, dataset);

		int number = numberOfSignificants(atts, significantFactors);

		return new Summary(array.size() - 1, average, atts, number);
	}

	private String combineString(ArrayList<Integer> array, Instances dataset) {

		StringBuilder st = new StringBuilder();

		for (int ats = 0; ats < array.size() - 1; ats++) {
			st.append(dataset.attribute(array.get(ats)).name()).append(" ");
		}

		st.deleteCharAt(st.length() - 1);

		return st.toString();

	}

	public double evaluateModelClustering(int numClusters, Instances dataset, String algorithm,
			boolean printBadIndividuals) throws Exception {

		Instances dataTemp = new Instances(dataset);

		Remove filter = new Remove();

		// The class attribute is removed
		filter.setAttributeIndicesArray(new int[] { dataset.classIndex() });

		filter.setInputFormat(dataTemp);

		dataTemp = Filter.useFilter(dataTemp, filter);

		Clusterer clusterer = null;

		if (algorithm.contains("SimpleKMeans")) {

			clusterer = new SimpleKMeans();

			((SimpleKMeans) clusterer).setInitializationMethod(new SelectedTag(
					((SimpleKMeans) clusterer).KMEANS_PLUS_PLUS, ((SimpleKMeans) clusterer).TAGS_SELECTION));

			((SimpleKMeans) clusterer).setNumClusters(numClusters);

			((SimpleKMeans) clusterer).setPreserveInstancesOrder(true);
		}

		if (algorithm.contains("HierarchicalClusterer")) {

			clusterer = new HierarchicalClusterer();

			((HierarchicalClusterer) clusterer)
					.setLinkType(new SelectedTag(5, ((HierarchicalClusterer) clusterer).TAGS_LINK_TYPE));

			((HierarchicalClusterer) clusterer).setNumClusters(numClusters);

		}

		// build clusteres
		clusterer.buildClusterer(dataTemp);

		// Class evaluation cluster
		ClusterEvaluation ce = new ClusterEvaluation();

		ce.setClusterer(clusterer);
		ce.evaluateClusterer(dataset);

		int numIndi = dataset.numInstances();

		StringBuilder badIndis = new StringBuilder();

		ArrayList<Prediction> predictions = new ArrayList();

		int numInstancesPerClass[] = new int[numClusters];

		// for each individual
		for (int i = 0; i < numIndi; i++) {

			int clusterID = (int) ce.getClusterAssignments()[i];

			double assignedClass = ce.getClassesToClusters()[clusterID];
			double actualClass = dataset.instance(i).classValue();

			numInstancesPerClass[(int) actualClass]++;

			double[] distribution = new double[numClusters];

			if (assignedClass != -1) {

				distribution[(int) assignedClass] = 1.0;

				if (actualClass != assignedClass) {

					if (printBadIndividuals) {
						String trueClassString = dataset.attribute(dataset.classIndex()).value((int) actualClass);

						String assignClassString = dataset.attribute(dataset.classIndex()).value((int) assignedClass);

						badIndis.append("ID: ").append(i).append(", True class: ").append(trueClassString)
								.append(", Assigned class: ").append(assignClassString).append("\n");
					}
				}
			}

			NominalPrediction prediction = new NominalPrediction(actualClass, distribution);

			predictions.add(prediction);
		}

		// Compute the AUC for each class

		// to compute the AUC from cluster results
		ThresholdCurve curve = new ThresholdCurve();

		double weightedAverageAUC = 0;

		for (int c = 0; c < numClusters; c++) {

			Instances instancesCurve = curve.getCurve(predictions, c);

			weightedAverageAUC += curve.getROCArea(instancesCurve) * numInstancesPerClass[c];

		}

		weightedAverageAUC /= dataset.numInstances();

		if (printBadIndividuals) {
			System.out.println("Evaluation of subset");
			System.out.println("///////////////////////////");
			System.out.println(badIndis.toString());
			System.out.println("Average AUC: " + weightedAverageAUC);
			System.out.println("///////////////////////////");
			System.out.println();
		}

		// return incorrect;
		return weightedAverageAUC;
	}

	public double evaluateModelClassification(String algorithm, Instances dataset) throws Exception {

		Evaluation eval = new Evaluation(dataset);

		eval.crossValidateModel(algorithm, dataset, dataset.numInstances(), new String[] { "" }, new Random());

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

	public static void main(String args[]) throws IOException {

		WriteReadFromFile config = new WriteReadFromFile();

		ArrayList<String> datasets = config.read("config/datasets.txt");

		// Adding clustering algorithms
		ArrayList<String> algorithms = config.read("config/clustering.txt");

		int nClust = algorithms.size();

		// Adding classification algorithms
		algorithms.addAll(config.read("config/classification.txt"));

		// threshold to filter
		double threshold = 0.8;

		// for each dataset
		for (String dataset : datasets) {

			int n = 1;

			// for each algo
			for (String algorithm : algorithms) {

				AttSelectionClusteringv2 sel;

				if (n <= nClust)
					sel = new AttSelectionClusteringv2(dataset, algorithm, threshold, "clustering");
				else
					sel = new AttSelectionClusteringv2(dataset, algorithm, threshold, "classification");
				n++;
			}
		}
	}

	class Summary implements Comparable<Summary> {

		int numAtt;
		double auc;
		String atts;
		int numberOfSignificants;

		public Summary(int numAtt, double auc, String atts, int numberOfSignificants) {

			this.numAtt = numAtt;
			this.auc = auc;
			this.atts = atts;
			this.numberOfSignificants = numberOfSignificants;
		}

		@Override
		public int compareTo(Summary sum) {

			// First compare by number of significant factors

			if (auc > sum.auc)
				return -1;

			if (auc < sum.auc)
				return 1;

			return 0;
		}

		public String toString() {

			StringBuilder string = new StringBuilder();

			string.append(numAtt + ",");
			string.append(numberOfSignificants + ",");
			string.append(atts + ",");
			string.append(auc);

			return string.toString();
		}

		public boolean equals(Object sum) {

			Summary otherS = (Summary) sum;

			String attsOther[] = otherS.atts.split(" ");

			String attsOwn[] = atts.split(" ");

			if (attsOther.length != attsOwn.length)
				return false;

			// Compare attsOther with attsOwn
			for (String other : attsOther) {

				boolean found = false;

				for (String own : attsOwn) {
					if (own.equals(other)) {
						found = true;
						break;
					}
				}

				if (!found)
					return false;
			}

			return true;
		}
	}
}