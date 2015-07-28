package se.mattec.id2209.hw3;

import se.mattec.id2209.hw3.agents.ArtistManagerAgent;
import se.mattec.id2209.hw3.agents.CuratorAgent;
import jade.core.ProfileImpl;
import jade.wrapper.AgentContainer;
import jade.wrapper.AgentController;
import jade.core.Runtime;

public class Controller {

	public static void main(String[] args) {
		try {
			Runtime rt =  Runtime.instance();
			rt.setCloseVM(true);

			AgentContainer agentContainer = rt.createMainContainer(new ProfileImpl("localhost", 8080, null));

			AgentController rma = agentContainer.createNewAgent("rma", jade.tools.rma.rma.class.getName(), new Object[0]);
			rma.start();
			
			AgentContainer hmContainer = rt.createAgentContainer(new ProfileImpl("localhost", 8080, "hmContainer"));
			AgentController hmAgentController = hmContainer.createNewAgent("hmCurator", CuratorAgent.class.getName(), new Object[0]);
			hmAgentController.start();
			
			AgentContainer galileoContainer = rt.createAgentContainer(new ProfileImpl("localhost", 8080, "galileoContainer"));
			AgentController galileoAgentController = galileoContainer.createNewAgent("galileoCurator", CuratorAgent.class.getName(), new Object[0]);
			galileoAgentController.start();
			
			AgentContainer artistManagerAgentContainer = rt.createAgentContainer(new ProfileImpl("localhost", 8080, "artistManagerContainer"));
			AgentController artistManagerAgentController = artistManagerAgentContainer.createNewAgent("artistManager", ArtistManagerAgent.class.getName(), new Object[0]);
			artistManagerAgentController.start();

		}
		catch (Exception e) {
			e.printStackTrace();
		}
	}
}
