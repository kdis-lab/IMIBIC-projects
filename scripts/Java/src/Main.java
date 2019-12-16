import AttributeSelection.AttRanking;
import AttributeSelection.AttSelectionClusterinClassification;
import weka.core.Utils;

public class Main {

	public static void main(String[] args) throws Exception {
		
		boolean task1, task2;
		
		//check whether the flag s is specified, in that case the feature selection algorithms are executed.
		task1 = Utils.getFlag('s', args);
		
		//check whether the flag p is specified, in that case the clustering and classification algorithms are executed
	    task2 = Utils.getFlag('p', args);
	  	    
	    if(task1){
	    		AttRanking.main(args);
	    }
	    
	  //The possible values for 'eval' option  are 10Folds or LeaveOneOut
		
	  //The option 't' is to specify the threshold value
	    
	    if(task2){
    		AttSelectionClusterinClassification.main(args);
	    }
	}
}
