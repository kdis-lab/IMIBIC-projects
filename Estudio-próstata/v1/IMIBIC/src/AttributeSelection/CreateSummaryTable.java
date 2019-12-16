package AttributeSelection;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Scanner;
import java.util.Set;
import java.util.TreeSet;
import utils.WriteReadFromFile;

public class CreateSummaryTable {

	public static void main(String args[]) throws IOException {

		WriteReadFromFile configFiles = new WriteReadFromFile();

		ArrayList<String> algorithms = configFiles.read("config/classification.txt");

		algorithms.addAll(configFiles.read("config/clustering.txt"));

		ArrayList<String> datasets = configFiles.read("config/datasets.txt");

		for (String dataset : datasets) {

			HashMap<String, Integer> genesIndex = new HashMap<>();

			String path = "reports/" + dataset;

			// Store the list of general genes without repetition
			Set<String> generalGenes = new TreeSet<>();
			ArrayList<String> executions = new ArrayList<>();
			ArrayList<Double> AUC = new ArrayList<>();

			// For each algorithm
			for (String algorithm : algorithms) {

				String algorithmT = algorithm.substring(algorithm.lastIndexOf(".") + 1);

				Scanner scan = new Scanner(new File(path + "/" + algorithmT + "/bestResults.txt"));

				while (scan.hasNext()) {

					String line = scan.nextLine();

					if (line.startsWith("Attributes: ")) {

						executions.add(algorithmT + "$" + line);

						String genes[] = line.split(" ");

						for (int i = 1; i < genes.length; i++) {

							generalGenes.add(genes[i]);

						}
					}

					if (line.startsWith("Average AUC: ")) {

						String[] items = line.split(" ");
						AUC.add(Double.parseDouble(items[items.length - 1].trim()));

					}

				}
			}

			createCSV(path, generalGenes, algorithms, executions, AUC);
		}
	}

	private static void createCSV(String pathCSV, Set<String> generalGenes, ArrayList<String> algorithms,
			ArrayList<String> executions, ArrayList<Double> AUC) throws IOException {

		// Generate a summary table
		StringBuilder table = new StringBuilder();

		DecimalFormat df = new DecimalFormat("0.000");

		// header
		table.append("Algorithm\tExecution");

		for (String gene : generalGenes) {
			table.append("\t").append(gene);
		}
		// end header
		table.append("\tAUC\n");

		String algPrevious = "";

		HashMap<String, Integer> average = new HashMap<>();

		int cant = 0;
		int e = 0;
		// for each execution
		for (String exec : executions) {

			StringBuilder line = new StringBuilder();

			String algCurrent = exec.substring(0, exec.indexOf('$'));

			// does not repeat the algorithm name
			if (!algCurrent.equals(algPrevious)) {

				algPrevious = algCurrent;

				line.append(algPrevious).append("\t");
				cant = 1;
			} else {
				cant++;
				line.append("\t");
			}

			line.append(cant);

			// for each general gene
			for (String gene : generalGenes) {

				line.append("\t");

				if (exec.contains(gene)) {
					line.append("1");
					average.put(gene, average.get(gene) == null ? 1 : average.get(gene) + 1);
				}
			}

			line.append("\t").append(df.format(AUC.get(e)));

			table.append(line).append("\n");
			e++;
		}

		table.append("\n");
		table.append("Average\t");

		// for each general gene
		for (String gene : generalGenes) {
			table.append("\t").append(df.format(average.get(gene)/(double)executions.size()));
		}

		FileWriter writer= new FileWriter(new File(pathCSV+"/"+"summary.csv"));

		writer.write(table.toString());

		writer.close();
	}
}