package se.mattec.id2209.hw3.agents;

import jade.core.AID;
import jade.core.Agent;
import jade.core.behaviours.SequentialBehaviour;
import jade.domain.DFService;
import jade.domain.FIPAException;
import jade.domain.FIPANames;
import jade.domain.FIPAAgentManagement.DFAgentDescription;
import jade.domain.FIPAAgentManagement.SearchConstraints;
import jade.domain.FIPAAgentManagement.ServiceDescription;
import jade.lang.acl.ACLMessage;
import jade.lang.acl.UnreadableException;
import jade.proto.SimpleAchieveREInitiator;
import jade.proto.SubscriptionInitiator;

import java.awt.Frame;
import java.awt.Label;
import java.awt.List;
import java.awt.Panel;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.IOException;
import java.util.ArrayList;

import javax.swing.BoxLayout;

import se.mattec.id2209.hw3.models.Artifact;
import se.mattec.id2209.hw3.models.RequestIdentifiers;
import se.mattec.id2209.hw3.models.User;

@SuppressWarnings("serial")
public class ProfilerAgent extends Agent {
	User user;

	ArrayList<Long> artifactIds;
	ArrayList<Artifact> artifacts;
	ArrayList<AID> guides;
	
	List list;

	@Override
	protected void setup() {
		super.setup();
		System.out.println("ProfilerAgent started, name: " + getLocalName());

		//Create random user
		user = new User();
		
		//Get the guides
		guides = findGuides();

		createGuideSelectionGUI();

		//Add subscription to new guide services
		DFAgentDescription template = new DFAgentDescription();
		ServiceDescription serviceDescription = new ServiceDescription();
		serviceDescription.setType("guide");

		SearchConstraints constrains = new SearchConstraints();
		constrains.setMaxResults((long) 1);

		addBehaviour(new SubscriptionInitiator(this, DFService.createSubscriptionMessage(this, getDefaultDF(), template, constrains)) {

			@Override
			protected void handleInform(ACLMessage inform) {
				super.handleInform(inform);
				try {
					DFAgentDescription[] resultAgentDescriptions = DFService.decodeNotification(inform.getContent());
					if (resultAgentDescriptions.length > 0) {
						System.out.println("Profiler received subscription message from DFService");
						
						//Refresh the guides and update the list
						guides = findGuides();
						updateList();
					}
				} catch (FIPAException e) {
					e.printStackTrace();
				}
			}
		});
	}

	/**
	 * Creates a simple GUI to select a guide for a tour.
	 * */
	private void createGuideSelectionGUI() {
		final Frame frame = new Frame("Profiler");
		Panel panel = new Panel();

		BoxLayout boxLayout = new BoxLayout(panel, BoxLayout.PAGE_AXIS);
		panel.setLayout(boxLayout);

		Label label = new Label("Guides available");

		list = new List();
		list.addItemListener(new ItemListener() {

			@Override
			public void itemStateChanged(ItemEvent event) {
				getTour(list.getSelectedIndex());
			}
		});		

		if (guides != null) {
			for (AID guide : guides) {
				list.add(guide.getLocalName());
			}
		}

		panel.add(label);
		panel.add(list);

		frame.add(panel);
		frame.pack();
		frame.setVisible(true);
		frame.addWindowListener(new WindowAdapter(){
			public void windowClosing(WindowEvent we){
				doDelete();
				frame.dispose();
			}
		});
	}
	
	/**
	 * Updates the list of guides in the GUI.
	 * */
	private void updateList() {
		list.removeAll();

		if (guides != null) {
			for (AID guide : guides) {
				list.add(guide.getLocalName());
			}
		}
	}

	/**
	 * Creates a simple gui to show the list of artifacts in a tour.
	 * */
	private void buildTourGui() {
		final Frame frame = new Frame("Tour details");
		Panel panel = new Panel();

		BoxLayout boxLayout = new BoxLayout(panel, BoxLayout.PAGE_AXIS);
		panel.setLayout(boxLayout);

		Label label = new Label("Artifacts in tour");

		final List list = new List();

		for (Artifact artifact : artifacts) {
			list.add(artifact.getName() + ", " + artifact.getGenre().toString() + ", " + artifact.getCreationYear());
		}

		panel.add(label);
		panel.add(list);

		frame.add(panel);
		frame.pack();
		frame.setVisible(true);
		frame.addWindowListener(new WindowAdapter(){
			public void windowClosing(WindowEvent we){
				frame.dispose();
			}
		});
	}

	/**
	 * Requests a tour from the selected guide.
	 * */
	private void getTour(int selectedIndex) {
		//Request a tour from the guide
		ACLMessage requestTourMessage = new ACLMessage(ACLMessage.REQUEST);
		requestTourMessage.setProtocol(FIPANames.InteractionProtocol.FIPA_REQUEST); 
		requestTourMessage.addReceiver(guides.get(selectedIndex));

		try {
			requestTourMessage.setContentObject(user);
		} catch (IOException e) {
			e.printStackTrace();
		}

		requestTourMessage.setOntology(RequestIdentifiers.REQUEST_TOUR);
		RequestTourInitiator requestTourGuideBehaviour = new RequestTourInitiator(this, requestTourMessage);

		//Request artifact details from the curator.
		ACLMessage requestTourDetailsMessage = new ACLMessage(ACLMessage.REQUEST);
		requestTourDetailsMessage.setProtocol(FIPANames.InteractionProtocol.FIPA_REQUEST); 
		requestTourDetailsMessage.addReceiver(findCurator(guides.get(selectedIndex).getName()));
		requestTourDetailsMessage.setOntology(RequestIdentifiers.REQUEST_ARTIFACT_INFO);
		TourDetailsInitiator requestTourDetailsBehaviour = new TourDetailsInitiator(this, requestTourDetailsMessage);

		//These two behaviors should be executed sequentially,
		//requesting the tour first and then the artifact details.
		SequentialBehaviour sequentialBehaviour = new SequentialBehaviour();
		sequentialBehaviour.addSubBehaviour(requestTourGuideBehaviour);
		sequentialBehaviour.addSubBehaviour(requestTourDetailsBehaviour);
		addBehaviour(sequentialBehaviour);
	}

	/**
	 * Search for a curator based on the tour name.
	 * */
	private AID findCurator(String tourName) {
		DFAgentDescription description = new DFAgentDescription();
		ServiceDescription serviceDescription = new ServiceDescription();
		serviceDescription.setType("curator");
		description.addServices(serviceDescription);
		try {
			DFAgentDescription[] resultAgentDescriptions = DFService.search(this,  description);
			if (resultAgentDescriptions.length > 0) {
				return resultAgentDescriptions[0].getName();
			}
		} 
		catch (FIPAException e) {
			e.printStackTrace();
		}
		return null;
	}

	/**
	 * Find all registered guides.
	 * */
	private ArrayList<AID> findGuides() {
		DFAgentDescription description = new DFAgentDescription();
		ServiceDescription serviceDescription = new ServiceDescription();
		serviceDescription.setType("guide");
		description.addServices(serviceDescription);
		try {
			DFAgentDescription[] resultAgentDescriptions = DFService.search(this,  description);
			if (resultAgentDescriptions.length > 0) {
				ArrayList<AID> guides = new ArrayList<AID>();
				for (int i = 0; i < resultAgentDescriptions.length; i++)
				{
					guides.add(resultAgentDescriptions[i].getName());
				}
				return guides;
			}
		} 
		catch (FIPAException e) {
			e.printStackTrace();
		}
		return null;
	}

	/**
	 * Initiator for requesting a tour from the guide.
	 * */
	class RequestTourInitiator extends SimpleAchieveREInitiator {

		public RequestTourInitiator(Agent a, ACLMessage msg) {
			super(a, msg);
		}

		@SuppressWarnings("unchecked")
		@Override
		protected void handleInform(ACLMessage msg) {
			super.handleInform(msg);
			System.out.println("Profiler received tour response from guide");

			try {
				artifactIds = (ArrayList<Long>) msg.getContentObject();
			} 
			catch (UnreadableException e) {
				e.printStackTrace();
			}
		}
	}

	/**
	 * Initiator for communication with curator, requesting artifact details.
	 * Provides the artifact ids with request message.
	 * */
	class TourDetailsInitiator extends SimpleAchieveREInitiator {

		public TourDetailsInitiator(Agent a, ACLMessage msg) {
			super(a, msg);
		}

		@Override
		protected ACLMessage prepareRequest(ACLMessage msg) {
			try {
				msg.setContentObject(artifactIds);
			} 
			catch (IOException e) {
				e.printStackTrace();
			}

			return super.prepareRequest(msg);
		}

		@SuppressWarnings("unchecked")
		@Override
		protected void handleInform(ACLMessage msg) {
			super.handleInform(msg);
			System.out.println("Profiler received tour details response from curator");

			try {
				artifacts = (ArrayList<Artifact>) msg.getContentObject();
				buildTourGui();
			} 
			catch (UnreadableException e) {
				e.printStackTrace();
			}
		}
	}

}
