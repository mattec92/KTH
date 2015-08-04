package se.mattec.id2208.project;

import java.util.List;
import java.util.Map;

import ontology.MyOntManager;

import org.mindswap.pellet.owlapi.Reasoner;
import org.semanticweb.owl.model.OWLClass;
import org.semanticweb.owl.model.OWLObjectProperty;
import org.semanticweb.owl.model.OWLOntology;
import org.semanticweb.owl.model.OWLOntologyManager;

public class SemanticMatcher extends Matcher
{

//	private static final String OWL_PATH = "D:/Dropbox/_Web_Services/Project/project/data/SUMO.owl";
	private static final String OWL_PATH = "/Users/mcederlund/Dropbox/_Web_Services/Project/project/data/SUMO.owl";

	private static final double EXACT = 1.0;
	private static final double SUBSUMPTION = 0.8;
	private static final double PLUGIN = 0.6;
	private static final double STRUCTURAL = 0.5;
	private static final double NOTMATCHED = 0.0;

	private MyOntManager ontManager;
	private OWLOntologyManager manager = null;
	private OWLOntology ontology = null;
	private Reasoner reasoner = null;
	private Map<String, OWLClass> OWLClassMap;


	public SemanticMatcher()
	{
		ontManager = new MyOntManager();
		manager = ontManager.initializeOntologyManager();
		ontology = ontManager.initializeOntology(manager, "file:///" + OWL_PATH);
		reasoner = ontManager.initializeReasoner(ontology, manager);
		OWLClassMap = ontManager.loadClasses(reasoner);

		performMatching("SAWSDL", 0.0, "OutputOntology.xml");
	}


	@Override
	protected double getMatchingScore(Parameter inputParameter, Parameter outputParameter)
	{
		return findMatching(inputParameter.ontology, outputParameter.ontology);
	}


	public double findMatching(String inputOntology, String outputOntology)
	{
		if (inputOntology == null || inputOntology.isEmpty() || outputOntology == null || outputOntology.isEmpty())
		{
			System.out.println("#################################################### Ontology matching failed, something null");
			return NOTMATCHED;
		}

		if (isSameAs(inputOntology, outputOntology))
		{
			return EXACT;
		}
		else if (isSubClassOf(outputOntology, inputOntology))
		{
			return SUBSUMPTION;
		}
		else if (isSubClassOf(inputOntology, outputOntology))
		{
			return PLUGIN;
		}
		else if (hasRelationWith(inputOntology, outputOntology))
		{
			return STRUCTURAL;
		}
		else
		{
			return NOTMATCHED;
		}
	}


	private boolean isSameAs(String input, String output)
	{
		if (input == null || output == null)
		{
			return false;
		}

		return input.equals(output);
	}


	private boolean isSubClassOf(String child, String parent)
	{
		OWLClass childClass = OWLClassMap.get(child.toLowerCase());
		OWLClass parentClass = OWLClassMap.get(parent.toLowerCase());

		if (childClass == null || parentClass == null)
		{
			return false;
		}

		return reasoner.isSubClassOf(childClass, parentClass);
	}


	private boolean hasRelationWith(String input, String output)
	{
		OWLClass inputClass = OWLClassMap.get(input.toLowerCase());
		OWLClass outputClass = OWLClassMap.get(output.toLowerCase());

		if (inputClass == null || outputClass == null)
		{
			return false;
		}

		List<OWLObjectProperty> properties = ontManager.findRelationship(inputClass, outputClass, reasoner);

		return properties.size() > 0;
	}

}
