package se.mattec.id2209.hw3.queens;

import jade.core.ProfileImpl;
import jade.wrapper.AgentContainer;
import jade.wrapper.AgentController;
import jade.core.Runtime;

public class Controller {
	private static final int N_QUEENS = 8;

	public static void main(String[] args) {
		try {
			Runtime rt =  Runtime.instance();
			rt.setCloseVM(true);

			AgentContainer agentContainer = rt.createMainContainer(new ProfileImpl("localhost", 8080, null));

			AgentController rma = agentContainer.createNewAgent("rma", jade.tools.rma.rma.class.getName(), new Object[0]);
			rma.start();

			//Start N queens, initializing them with their index.
			for (int i = 0; i < N_QUEENS; i++) {
				Object[] arguments = new Object[2];
				arguments[0] = i;
				arguments[1] = N_QUEENS;
				AgentController queenAgentController = agentContainer.createNewAgent("q" + i, QueenAgent.class.getName(), arguments);
				queenAgentController.start();
			}
		}
		catch (Exception e) {
			e.printStackTrace();
		}
	}
}
