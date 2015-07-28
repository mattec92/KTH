package se.mattec.id2208.project;


public class SyntanticMatcher extends Matcher
{

	public SyntanticMatcher()
	{
		performMatching("WSDLs", 0.0, "Output.xml");
	}


	@Override
	protected double getMatchingScore(Parameter inputParameter, Parameter outputParameter)
	{
		return EditDistance.getSimilarity(inputParameter.name, outputParameter.name);
	}

}
