package sort;

import org.semanticweb.owl.model.OWLClass;
import org.mindswap.pellet.owlapi.Reasoner;

/**
 * 
 * 
 *
 */
public class ClsQuickSort {
	  private OWLClass[] classes;
	  private int number;
	  
	  public void sort(OWLClass[] values, Reasoner r) {
	    // Check for empty or null array
	    if (values ==null || values.length==0){
	      return;
	    }
	    this.classes = values;
	    this.number = values.length;
	    quicksort(0, this.number - 1, r);
	  }

	  private void quicksort(int low, int high, Reasoner r) {

		//printResult (this.classes);
		 
	    int i = low, j = high;
	    // Get the pivot element from the middle of the list
	    OWLClass pivot = this.classes[low + (high-low)/2];

	    // Divide into two lists
	    while (i <= j) {
	      // If the current value from the left list is smaller then the pivot
	      // element then get the next element from the left list
	      while (  (r.isSubClassOf(this.classes[i] , pivot)  ||
	    		  (  !r.isSubClassOf(this.classes[i] , pivot)  && !r.isSubClassOf( pivot, this.classes[i]))) 
	    		  &&  (this.classes[i] != pivot) ) {
	        i++;
	      }
	      // If the current value from the right list is larger then the pivot
	      // element then get the next element from the right list
	      while ( ( r.isSubClassOf(pivot, this.classes[j] ) ||
	    		  (  !r.isSubClassOf(this.classes[j] , pivot)  && !r.isSubClassOf( pivot, this.classes[j]))
	    		  ) && (this.classes[j] != pivot)) {
	        j--;
	      }

	      // If we have found a values in the left list which is larger then
	      // the pivot element and if we have found a value in the right list
	      // which is smaller then the pivot element then we exchange the
	      // values.
	      // As we are done we can increase i and j
	      if (i <= j) {
	        exchange(i, j);
	        i++;
	        j--;
	      }
	    }
	    // Recursion
	    if (low < j)
	      quicksort(low, j, r);
	    if (i < high)
	      quicksort(i, high, r);
	  }

	  private void exchange(int i, int j) {
	    OWLClass temp = this.classes[i];
	    this.classes[i] = this.classes[j];
	    this.classes[j] = temp;
	  }
	  
	  public void printResult() {
		    for (int i = 0; i < this.classes.length; i++) {
		      System.out.print(this.classes[i].getURI().getFragment() + " , ");
		    }
		    System.out.println();
		  }
}
