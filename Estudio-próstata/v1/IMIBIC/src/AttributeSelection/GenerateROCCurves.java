package AttributeSelection;

import java.awt.*;
import java.io.*;
import java.util.*;
import weka.core.*;
import weka.core.converters.ConverterUtils.DataSource;
import weka.classifiers.*;
import weka.classifiers.evaluation.Evaluation;
import weka.classifiers.evaluation.ThresholdCurve;
import weka.classifiers.functions.SimpleLogistic;
import weka.classifiers.trees.RandomForest;
import weka.gui.visualize.*;

/**
 * Generates and displays a ROC curve from a dataset. Uses a default NaiveBayes
 * to generate the ROC data.
 *
 * @author FracPete
 */
public class GenerateROCCurves {

	/**
	 * takes one argument: dataset in ARFF format (expects class to be last
	 * attribute)
	 */
	public static void main(String[] args) throws Exception {

		String dataName= "mejora-nomejora-RF-3";
		String dataPath = "datasets/genes-splicing-all/";

		DataSource wekaSource = new DataSource(dataPath+dataName + ".csv");

		// load data
		Instances data = wekaSource.getDataSet();

		data.setClassIndex(data.numAttributes() - 1);

		int numClasses = data.numClasses();

		// train classifier
		Classifier cl = new RandomForest();

		Evaluation eval = new Evaluation(data);

		// leave one out cross validation
		eval.crossValidateModel(cl, data, data.numInstances(), new Random(3));

		// generate curve
		ThresholdCurve tc = new ThresholdCurve();

		Color[] colours = new Color[3];

		colours[0] = Color.BLACK;
		colours[1] = Color.BLUE;
		colours[2] = Color.RED;

		// plot curve
		ThresholdVisualizePanel vmc = new ThresholdVisualizePanel();
		vmc.setROCString(" - Weighted Area under ROC = " + Utils.doubleToString(eval.weightedAreaUnderROC(), 3));

		vmc.setName(data.relationName());

		for (int classIndex = 0; classIndex < numClasses; classIndex++) {

			Instances result = tc.getCurve(eval.predictions(), classIndex);

			/*FileWriter writer = new FileWriter(
					new File("curves/"+dataName + "-curve-" + data.classAttribute().value(classIndex) + ".csv"));

			writer.write(result.toString());

			writer.close();*/

			PlotData2D tempd = new PlotData2D(result);
			tempd.setPlotName(data.classAttribute().value(classIndex));
			tempd.addInstanceNumberAttribute();
			tempd.setCustomColour(colours[classIndex]);

			// specify which points are connected
			boolean[] cp = new boolean[result.numInstances()];

			for (int n = 1; n < cp.length; n++)
				cp[n] = true;

			tempd.setConnectPoints(cp);

			// add plot
			vmc.addPlot(tempd);

		}

		// display curve
		final javax.swing.JFrame jf = new javax.swing.JFrame();
		jf.setSize(500, 400);
		jf.getContentPane().setLayout(new BorderLayout());
		jf.getContentPane().add(vmc, BorderLayout.CENTER);
		jf.addWindowListener(new java.awt.event.WindowAdapter() {
			public void windowClosing(java.awt.event.WindowEvent e) {
				jf.dispose();
			}
		});
		jf.setVisible(true);
	}
}