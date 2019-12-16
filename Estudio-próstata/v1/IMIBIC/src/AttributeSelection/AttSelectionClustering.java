package AttributeSelection;

import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import utils.WriteReadFromFile;
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

public class AttSelectionClustering {

	public AttSelectionClustering(String fileNameReport, String datasetName, String algorithm) {

		try {

			DataSource wekaDataSource = new DataSource(datasetName+".csv");

			Instances originalDataset = wekaDataSource.getDataSet();

			originalDataset.setClassIndex(originalDataset.numAttributes() - 1);

			WriteReadFromFile file = new WriteReadFromFile();

			ArrayList<String> lines = file.read(fileNameReport);

			int indexes[] = new int[lines.size()];

			int pos = 0;

			// for each attribute
			for (String line : lines) {

				int index = Integer.parseInt(line.split("\t")[0]);

				indexes[pos] = index;

				pos++;
			}

			// until the value is lower than previous iteration

			int numClusters = originalDataset.numClasses();

			double bestAverageGlobal = Double.MIN_VALUE;

			ArrayList<Integer> attsSelected = null;

			for (int pivot = 0; pivot < indexes.length; pivot++) {

				ArrayList<Integer> attsSelectedTemp = new ArrayList<>();
				attsSelectedTemp.add(originalDataset.classIndex());

				double bestAverageTemp = Double.MIN_VALUE;

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
					Instances datasetTemp2 = reduceDataset(attsSelectedTemp2, originalDataset);

					// run the classifier
					double average = evaluateModel(numClusters, datasetTemp2, algorithm, false);

					if (bestAverageTemp < average) {

						bestAverageTemp = average;
						attsSelectedTemp = attsSelectedTemp2;
					}
				}

				if (bestAverageGlobal < bestAverageTemp) {

					bestAverageGlobal = bestAverageTemp;
					attsSelected = attsSelectedTemp;

					System.out.println(summary(attsSelected, originalDataset, bestAverageGlobal));

				} else {

					if (bestAverageGlobal == bestAverageTemp) {

						if (attsSelected.size() > attsSelectedTemp.size()) {

							bestAverageGlobal = bestAverageTemp;
							attsSelected = attsSelectedTemp;

							System.out.println(summary(attsSelected, originalDataset, bestAverageGlobal));

						}

					}

				}

			}

			System.out.println("///////////BEST////////////////////");

			System.out.println();

			String summ= summary(attsSelected, originalDataset, bestAverageGlobal);

			System.out.println(summ);

			System.out.println();

			/*Instances bestDataset = reduceDataset(attsSelected, originalDataset);

			evaluateModel(numClusters, bestDataset, true);*/

			String algorithmName= algorithm.substring(algorithm.lastIndexOf(".")+1);
			String datasetNameT= datasetName.substring(datasetName.indexOf("/")+1);

			String directory= "reports/"+datasetNameT+"/"+algorithmName+"/";

			Files.createDirectories(Paths.get(directory));

			FileWriter writer = new FileWriter(directory+"bestResults.txt");

			writer.write(summ);

			writer.close();

		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

	}

	public String summary(ArrayList<Integer> array, Instances dataset, double average) {

		StringBuilder string= new StringBuilder();

		string.append("Number of atts: " + (array.size() - 1)+"\n");

		StringBuilder st = new StringBuilder();

		for (int ats = 0; ats < array.size() - 1; ats++) {
			st.append(dataset.attribute(array.get(ats)).name()).append(" ");
		}

		string.append("Attributes: ").append(st.toString()+"\n");

		string.append("Average AUC: " + average);

		return string.toString();
	}

	public double evaluateModel(int numClusters, Instances dataset, String algorithm, boolean printBadIndividuals) throws Exception {

		Instances dataTemp = new Instances(dataset);

		Remove filter = new Remove();

		// The class attribute is removed
		filter.setAttributeIndicesArray(new int[] { dataset.classIndex() });

		filter.setInputFormat(dataTemp);

		dataTemp = Filter.useFilter(dataTemp, filter);

		Clusterer clusterer = null;

		if(algorithm.contains("SimpleKMeans")){

			clusterer = new SimpleKMeans();

			((SimpleKMeans) clusterer).setInitializationMethod(new SelectedTag(((SimpleKMeans) clusterer).KMEANS_PLUS_PLUS, ((SimpleKMeans) clusterer).TAGS_SELECTION));

			((SimpleKMeans) clusterer).setNumClusters(numClusters);

			((SimpleKMeans) clusterer).setPreserveInstancesOrder(true);
		}

		if(algorithm.contains("HierarchicalClusterer")){

			clusterer = new HierarchicalClusterer();

			((HierarchicalClusterer) clusterer).setLinkType(new SelectedTag(5, ((HierarchicalClusterer) clusterer).TAGS_LINK_TYPE));

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

		WriteReadFromFile config= new WriteReadFromFile();

		ArrayList<String> datasets= config.read("config/datasets.txt");

		ArrayList<String> algorithms= config.read("config/clustering.txt");

		//for each dataset
		for(String dataset: datasets){

			//for each algo
			for(String algorithm:algorithms){

				AttSelectionClustering sel = new AttSelectionClustering(
						"reports/"+dataset+"/sortedfeatures.txt",
						"datasets/"+dataset, algorithm);
			}
		}
	}
}
