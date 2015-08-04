package ontology;

import java.net.URI;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Set;
import java.util.Vector;

import org.mindswap.pellet.owlapi.Reasoner;
import org.semanticweb.owl.apibinding.OWLManager;
import org.semanticweb.owl.model.OWLClass;
import org.semanticweb.owl.model.OWLDescription;
import org.semanticweb.owl.model.OWLObjectProperty;
import org.semanticweb.owl.model.OWLOntology;
import org.semanticweb.owl.model.OWLOntologyCreationException;
import org.semanticweb.owl.model.OWLOntologyManager;


public class MyOntManager {

	public enum LUBType {
		All,  // incorporate all 
		Sibeling //
	}
	public OWLOntologyManager initializeOntologyManager() {
		OWLOntologyManager manager = OWLManager.createOWLOntologyManager();
		return manager;
	}
	
	/**
	 * Loading ontology at location <SrcOntLocation>
	 * @param SrcOntLocation
	 * @return
	 */
	public  OWLOntology initializeOntology(OWLOntologyManager manager, String SrcOntLocation) {
		System.out.print("Reading Ontology " + SrcOntLocation + "...");
		OWLOntology ontology = null;
		try {
			ontology = manager.loadOntology(URI.create(SrcOntLocation));
		} catch (OWLOntologyCreationException e) {
			e.printStackTrace();
		}
		return ontology;
	}
	
	public Reasoner initializeReasoner(OWLOntology ontology, OWLOntologyManager manager)  {
		Reasoner reasoner;
		System.out.print("loading ....." );
		reasoner = new Reasoner( manager);
		reasoner.loadOntology(ontology);
		System.out.print("Realising ....." );
		reasoner.getKB().realize();
		
		System.out.print("Classifying ....." );
		reasoner.getKB().classify();
		System.out.println("Done." );
		
		boolean consistent = reasoner.isConsistent();
        System.out.println("Ontology is Consistent?: " + consistent);
        System.out.println("\n");
         
        Set<OWLClass> bottomNode = reasoner.getInconsistentClasses();
        if (!bottomNode.isEmpty()) {
             System.out.println("The following classes are unsatisfiable: ");
             for(OWLClass cls : bottomNode) {
                 System.out.println("    " + cls);
             }
         }
        else {
             System.out.println("There are no unsatisfiable classes");
        }
        
        System.out.print("Preparing knowledge base........");
        reasoner.getKB().prepare();
        System.out.println("Done!.");
        
        return reasoner;
	}
	
	public String getConceptName(OWLClass cls) {
		if (cls.getURI().getFragment() != null)
			return cls.getURI().getFragment();
		int last_index = cls.getURI().getPath().lastIndexOf("/");
		String concept_name = cls.getURI().getPath().substring(last_index + 1);
		//String pos = cls.getURI().
		return concept_name;
	}
	
	/**
	 * This method loads all classes into a map and returns the collection
	 * @param reasoner
	 * @return
	 */
	public HashMap<String, OWLClass> loadClasses (Reasoner reasoner) {
		HashMap<String, OWLClass> mapName_OWLClass = new HashMap<String, OWLClass> (); 
		 for (OWLClass cls: reasoner.getClasses())
			 if (cls.getURI() != null) {
				 String concept_name = getConceptName (cls);
				 mapName_OWLClass.put(concept_name.toLowerCase(), cls);
			 }
			 else {
				 System.out.println ("Something is Wrong !!");
			 }
		 return mapName_OWLClass;
	 }
	
	/**
	 * get ancestors of class <cls> 
	 * @param cls
	 * @param reasoner
	 * @return
	 */
	public OWLClass[] getAncestorClasses (OWLClass cls, Reasoner reasoner) {
		
		if (cls == null)
			return new OWLClass[0];
		
		Set<Set<OWLClass>> ancestor =  reasoner.getAncestorClasses(cls);
		
		int size = 0; 
		for (Set<OWLClass> set : ancestor) 
			size += set.size();
		OWLClass[] arrayCls = new OWLClass[size];
	
		
		int i = 0;
		for (Set<OWLClass> set : ancestor)
			for (OWLClass c : set) 
				arrayCls[i++] = c;

		return arrayCls;
	}
	
	/**
	 * get ancestors of class <cls> including itself
	 * @param cls
	 * @param reasoner
	 * @return
	 */
	public OWLClass[] getAncestorClassesPlusItself (OWLClass cls, Reasoner reasoner) {
		
		if (cls == null)
			return new OWLClass[0];
		
		Set<Set<OWLClass>> ancestor =  reasoner.getAncestorClasses(cls);
		
		int size = 1; //count itself too
		for (Set<OWLClass> set : ancestor) 
			size += set.size();
		OWLClass[] arrayCls = new OWLClass[size];
	
		arrayCls[0] = cls; //put class itself as the first
		int i = 0;
		for (Set<OWLClass> set : ancestor)
			for (OWLClass c : set) 
				arrayCls[++i] = c;

		return arrayCls;
	}
	/**
	 * find common subset of classes between two given sets <set1> and <set2>
	 * @param set1
	 * @param set2
	 * @return
	 */
	public OWLClass[] getCommon(OWLClass[] set1, OWLClass[] set2) {
		HashSet<OWLClass> set = new HashSet<OWLClass> ();
		for (OWLClass cls1 : set1)
			for (OWLClass cls2 : set2) {
				if (cls1.getURI().toString().equalsIgnoreCase(cls2.getURI().toString())) 
					set.add(cls1);
			}
		OWLClass[] arr = new OWLClass[set.size()] ;
		set.toArray(arr);
		
		return arr;
	}
	
public Vector<OWLObjectProperty> findRelationship (OWLDescription clsX, OWLDescription clsY, Reasoner reasoner) {
		
		Vector<OWLObjectProperty> relationships = new Vector<OWLObjectProperty> ();
		
 		for (OWLObjectProperty prop1:reasoner.getObjectProperties()) {
 			for (OWLDescription rangeY: reasoner.getRanges(prop1))
				if (reasoner.isEquivalentClass(rangeY, clsY) ) {
					Set<Set<OWLDescription>>  setSetDomainX = reasoner.getDomains(prop1);
		 			for (Set<OWLDescription> setDomainX: setSetDomainX) 
		 				for (OWLDescription domainX: setDomainX) {
		 				/*	System.out.println("	" + prop1.getURI().getFragment() + " : " + descr1.asOWLClass().getURI().getFragment() +
		 							" <->  " + descr2.asOWLClass().getURI().getFragment()); */
		 					if (reasoner.isEquivalentClass(domainX, clsX) ) {
		 						System.out.println(clsX.asOWLClass().getURI().getFragment() + "(" + domainX.asOWLClass().getURI().getFragment() + " ) "
		 								+ " hasProperty<" 
		 								+ prop1.getURI().getFragment() + ">  " + 
		 								clsY.asOWLClass().getURI().getFragment() + " ( " + rangeY.asOWLClass().getURI().getFragment() + " ) ");
				 						//return prop1;
		 						relationships.add(prop1);
				 						
		 					}
		 				}
				}
 		}
		return relationships;
	} 
	
	
//	public MatchingScore getMatchingScore(String clsName1, String clsName2, HashMap<String, OWLClass> mapName_OWLClass,
//			Reasoner reasoner, LUBType type) {
//		OWLClass cls1 = mapName_OWLClass.get(clsName1.toLowerCase());
//		OWLClass cls2 = mapName_OWLClass.get(clsName2.toLowerCase());
//		return getMatchingScore(cls1, cls2, reasoner, type);
//	}
	
	
}
