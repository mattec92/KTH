package ontology;


import static org.junit.Assert.*;

import java.util.HashMap;
import java.util.Vector;

import org.junit.BeforeClass;
import org.junit.Test;
import org.mindswap.pellet.owlapi.Reasoner;
import org.semanticweb.owl.model.OWLClass;
import org.semanticweb.owl.model.OWLObjectProperty;
import org.semanticweb.owl.model.OWLOntology;
import org.semanticweb.owl.model.OWLOntologyManager;

import sort.ClsQuickSort;


public class Test_MyOntManager {
	private static OWLOntologyManager manager = null;
	private static OWLOntology ontology = null;
	private static Reasoner reasoner = null;
	private static String ontLocation = "file:///media/D664BD5E64BD4253/PWS/2013/project/data/travel.owl";
	private static MyOntManager ontsum = null;


	@BeforeClass
	public static void initializeOntology() {
		ontsum = new  MyOntManager();
		manager = ontsum.initializeOntologyManager();
		ontology = ontsum.initializeOntology(manager, ontLocation); 
		reasoner = ontsum.initializeReasoner(ontology, manager);
	}
	
	@Test
	public  void test_loadOntology() {
		assertNotNull(ontsum);
		assertNotNull(ontology);
		assertNotNull(manager);
		assertNotNull(reasoner);
	}
	
	@Test
	public void test_loadClasses() {
		HashMap<String, OWLClass> mapName_OWLClass = ontsum.loadClasses(reasoner);
		assertTrue(mapName_OWLClass.size() == 34);
	}
	
	@Test
	public void test_NullAncestor() {
		OWLClass[] superClasses = ontsum.getAncestorClasses(null, reasoner);
		assertTrue(superClasses.length == 0);
	}
	
	@Test
	public void test_AncestorStandard() {
		String clsName= "City";
		HashMap<String, OWLClass> mapName_OWLClass = ontsum.loadClasses(reasoner);
		OWLClass cls = mapName_OWLClass.get(clsName.toLowerCase());
		assertNotNull(cls);
		
		OWLClass[] superClasses = ontsum.getAncestorClasses(cls, reasoner);
		assertTrue(superClasses.length == 3);
	}
	
	@Test
	public void test_subclassing_direct() {
		String clsName1= "City";
		String clsName2= "Capital";
		
		HashMap<String, OWLClass> mapName_OWLClass = ontsum.loadClasses(reasoner);
		OWLClass cls1 = mapName_OWLClass.get(clsName1.toLowerCase());
		assertNotNull(cls1);
		OWLClass cls2 = mapName_OWLClass.get(clsName2.toLowerCase());
		assertNotNull(cls2);
		assertTrue(reasoner.isSubClassOf(cls2, cls1));  // this is
	}
	
	@Test
	public void test_subclassing_indirect() {
		String clsName1= "Destination";
		String clsName2= "Town";
		
		HashMap<String, OWLClass> mapName_OWLClass = ontsum.loadClasses(reasoner);
		OWLClass cls1 = mapName_OWLClass.get(clsName1.toLowerCase());
		assertNotNull(cls1);
		OWLClass cls2 = mapName_OWLClass.get(clsName2.toLowerCase());
		assertNotNull(cls2);
		assertTrue(reasoner.isSubClassOf(cls2, cls1));  // it considers both immediate and ancestors of a class as its super concepts  
	}
	
	
	@Test
	public void test_Relationships() {
		String clsName1= "Destination";
		String clsName2= "Accommodation";
		
		HashMap<String, OWLClass> mapName_OWLClass = ontsum.loadClasses(reasoner);
		OWLClass cls1 = mapName_OWLClass.get(clsName1.toLowerCase());
		assertNotNull(cls1);
		OWLClass cls2 = mapName_OWLClass.get(clsName2.toLowerCase());
		assertNotNull(cls2);
		Vector<OWLObjectProperty> objprops =  ontsum.findRelationship (cls1, cls2, reasoner);
		assertTrue(objprops.size() == 1);
		assertTrue (objprops.get(0).getURI().getFragment().equalsIgnoreCase("hasAccommodation"));
	}
	  
	 @Test
	  public void testSorting() {
	    ClsQuickSort sorter = new ClsQuickSort();
	    
	    String clsName= "City";
		HashMap<String, OWLClass> mapName_OWLClass = ontsum.loadClasses(reasoner);
		OWLClass cls = mapName_OWLClass.get(clsName.toLowerCase());
		assertNotNull(cls);
	    
		OWLClass[] superClasses = ontsum.getAncestorClasses(cls, reasoner);
		//sort classes based on their semantic relations, (grand child, child, parent, grandfather,...)
	    sorter.sort(superClasses, reasoner);
	    if (!validate(superClasses)) {
	      fail("Should not happen");
	    }
	  }
	 
	 // find common super concepts of two classes
	  @Test
	  public void testLUB_Standard() {
	    ClsQuickSort sorter = new ClsQuickSort();
		HashMap<String, OWLClass> mapName_OWLClass = ontsum.loadClasses(reasoner);
		
		String clsName1= "City";
		OWLClass cls1 = mapName_OWLClass.get(clsName1.toLowerCase());
		assertNotNull(cls1);
		OWLClass[] superClasses1 = ontsum.getAncestorClassesPlusItself(cls1, reasoner);
	    sorter.sort(superClasses1, reasoner);
	    assertTrue(validate(superClasses1));
	    
	    String clsName2= "Capital";
		OWLClass cls2 = mapName_OWLClass.get(clsName2.toLowerCase());
		assertNotNull(cls2);
		OWLClass[] superClasses2 = ontsum.getAncestorClassesPlusItself(cls2, reasoner);
	    sorter.sort(superClasses2, reasoner);
	    assertTrue(validate(superClasses2));
	    
	    OWLClass[] common = ontsum.getCommon(superClasses1, superClasses2);
	    sorter.sort(common, reasoner);
	    
	    assertTrue(common.length == 4);
	    assertTrue(common[0] == cls1);
	  }
	  
	 private boolean validate(OWLClass[] classes) {
		    for (int i = 0; i < classes.length - 1; i++) {
		      if ( !reasoner.isSubClassOf(classes[i] , classes[i + 1]) &&  reasoner.isSubClassOf(classes[i+1] , classes[i] )) {
		        return false;
		      }
		    }
		    return true;
		  }
	 
	  public void printResult(OWLClass[] classes) {
		    for (int i = 0; i < classes.length; i++) {
		      System.out.print(classes[i].getURI().getFragment() + " , ");
		    }
		    System.out.println();
		  }
}
