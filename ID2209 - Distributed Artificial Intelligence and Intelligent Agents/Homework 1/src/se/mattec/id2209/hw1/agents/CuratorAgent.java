package se.mattec.id2209.hw1.agents;

import jade.core.Agent;
import jade.core.behaviours.OneShotBehaviour;
import jade.core.behaviours.ParallelBehaviour;
import jade.core.behaviours.SequentialBehaviour;
import jade.domain.DFService;
import jade.domain.FIPAException;
import jade.domain.FIPAAgentManagement.DFAgentDescription;
import jade.domain.FIPAAgentManagement.FailureException;
import jade.domain.FIPAAgentManagement.ServiceDescription;
import jade.lang.acl.ACLMessage;
import jade.lang.acl.MessageTemplate;
import jade.lang.acl.UnreadableException;
import jade.proto.SimpleAchieveREResponder;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;

import se.mattec.id2209.hw1.models.Artifact;
import se.mattec.id2209.hw1.models.RequestIdentifiers;
import se.mattec.id2209.hw1.models.User;

@SuppressWarnings("serial")
public class CuratorAgent extends Agent {
	HashMap<Long, Artifact> artCollection;

	@Override
	protected void setup() {
		super.setup();
		System.out.println("CuratorAgent started, name: " + getLocalName());

		//Register curator services
		registerService();

		SequentialBehaviour sequentialBehaviour = new SequentialBehaviour();

		//Create art collection / Museum. Run as OneShotBehaviour.
		sequentialBehaviour.addSubBehaviour(new OneShotBehaviour() {
			
			@Override
			public void action() {
				artCollection = new HashMap<Long, Artifact>();
				for (int i = 0; i < 100; i++) {
					Artifact artifact = new Artifact();
					artCollection.put(artifact.getId(), artifact);
				}
				
			}
		});
		
		ParallelBehaviour parallelBehaviour = new ParallelBehaviour();
		
		//Add responder behaviours for building a tour and getting artifact details.
		MessageTemplate buildTourTemplate = MessageTemplate.MatchOntology(RequestIdentifiers.REQUEST_BUILD_TOUR);
		MessageTemplate requestTourDetailsTemplate = MessageTemplate.MatchOntology(RequestIdentifiers.REQUEST_ARTIFACT_INFO);
		
		parallelBehaviour.addSubBehaviour(new BuildTourResponder(this, buildTourTemplate));
		parallelBehaviour.addSubBehaviour(new TourDetailsResponder(this, requestTourDetailsTemplate));
		
		sequentialBehaviour.addSubBehaviour(parallelBehaviour);
		
		addBehaviour(sequentialBehaviour);
	}

	/**
	 * Registers curator service to DF.
	 * */
	private void registerService() {
		DFAgentDescription description = new DFAgentDescription();
		description.setName(getAID());
		ServiceDescription serviceDescription = new ServiceDescription();
		serviceDescription.setType("curator");
		serviceDescription.setName(getName());
		description.addServices(serviceDescription);
		try {
			DFService.register(this, description);
		} catch (FIPAException e) {
			e.printStackTrace();
		}
	}

	/**
	 * Responder for build tour requests. Returns a tour (List of artifact ids) 
	 * based on user provided in the message.
	 * */
	class BuildTourResponder extends SimpleAchieveREResponder {

		public BuildTourResponder(Agent a, MessageTemplate mt) {
			super(a, mt);
		}

		@Override
		protected ACLMessage prepareResultNotification(ACLMessage request,
				ACLMessage response) throws FailureException {
			System.out.println("Curator received build tour request from guide"); 

			ACLMessage informDone = request.createReply();
			informDone.setPerformative(ACLMessage.INFORM);
			
			try {
				User user = (User) request.getContentObject();
				ArrayList<Long> ids = getTourBasedOnUser(user);
				informDone.setContentObject(ids);
			} 
			catch (IOException | UnreadableException e) {
				e.printStackTrace();
			}
			
			return informDone;
		}
	}

	/**
	 * Responder for artifact details requests. Returns artifact details
	 * based on the list of ids provided in the message.
	 * */
	class TourDetailsResponder extends SimpleAchieveREResponder {

		public TourDetailsResponder(Agent a, MessageTemplate mt) {
			super(a, mt);
		}

		@Override
		protected ACLMessage prepareResultNotification(ACLMessage request,
				ACLMessage response) throws FailureException {
			System.out.println("Curator received tour details request from profiler"); 
			
			ACLMessage informDone = request.createReply();
			informDone.setPerformative(ACLMessage.INFORM);
			
			try {
				@SuppressWarnings("unchecked")
				ArrayList<Long> ids = (ArrayList<Long>) request.getContentObject();
				ArrayList<Artifact> artifacts = getArtifactDetailsFromIds(ids);
				informDone.setContentObject(artifacts);
			} 
			catch (IOException | UnreadableException e) {
				e.printStackTrace();
			}
			
			return informDone;
		}
	}

	/**
	 * Generates a list of artifact ids based on user interests.
	 * */
	private ArrayList<Long> getTourBasedOnUser(User user) {
		ArrayList<Long> ids = new ArrayList<Long>();
		
		for (Artifact artifact : artCollection.values()) {
			if (artifact.getGenre() == user.getInterestGenre() && 
					artifact.getCreationYear() > user.getInterestCentury() && 
					artifact.getCreationYear() < (user.getInterestCentury() + 100)) {
				ids.add(artifact.getId());
			}
		}
		
		return ids;
	}

	/**
	 * Generates a list of artifacts based on the provided list of artifact ids.
	 * */
	private ArrayList<Artifact> getArtifactDetailsFromIds(ArrayList<Long> ids) {
		ArrayList<Artifact> artifacts = new ArrayList<Artifact>();
		
		for (Long id : ids) {
			Artifact artifact = artCollection.get(id);
			if (artifact != null) {
				artifacts.add(artifact);
			}
		}
		
		return artifacts;
	}

}
