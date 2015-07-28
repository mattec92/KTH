package se.mattec.id2209.hw3.agents;

import jade.core.Agent;
import se.mattec.id2209.hw3.behaviours.BuyerBehaviour;

@SuppressWarnings("serial")
public class BuyerAgent extends Agent {

	@Override
	protected void setup() {
		super.setup();

		Object[] arguments = getArguments();
		Double biddingStrategy = 1.0;
		try {
			biddingStrategy = Double.parseDouble((String) arguments[0]);
		}
		catch (Exception e) {
			System.out.println("Argument should be numeric. Using default value 1.0.");
		}

		addBehaviour(new BuyerBehaviour(this, biddingStrategy, true));
	}
}
