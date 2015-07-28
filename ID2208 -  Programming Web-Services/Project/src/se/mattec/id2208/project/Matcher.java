package se.mattec.id2208.project;

import java.io.File;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBElement;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;
import javax.xml.namespace.QName;

import org.apache.xerces.dom.ElementNSImpl;
import org.apache.xerces.dom.TextImpl;
import org.w3c.dom.Node;

import se.mattec.id2208.project.output.MatchedElementType;
import se.mattec.id2208.project.output.MatchedOperationType;
import se.mattec.id2208.project.output.MatchedWebServiceType;
import se.mattec.id2208.project.output.WSMatchingType;
import wsdl.TDefinitions;
import wsdl.TExtensibleAttributesDocumented;
import wsdl.TMessage;
import wsdl.TOperation;
import wsdl.TParam;
import wsdl.TPart;
import wsdl.TPortType;
import wsdl.TTypes;

public abstract class Matcher
{
	enum MessageType
	{
		INPUT, OUTPUT
	}


	protected abstract double getMatchingScore(Parameter inputParameter, Parameter outputParameter);

	protected List<TDefinitions> wsdls;
	protected Map<String, List<Operation>> webServicesOperations;
	protected Map<String, Parameter> elementParameters = new HashMap<String, Matcher.Parameter>();
	protected Map<String, Parameter> typeParameters = new HashMap<String, Matcher.Parameter>();


	protected void performMatching(String folder, double limit, String outputFilename)
	{
		wsdls = new ArrayList<>();
		webServicesOperations = new HashMap<>();

		List<String> fileNames = listFilesForFolder(new File(folder));

		// Parse the WSDLs and get the operations
		for (String fileName : fileNames)
		{
			TDefinitions definitions = parseWSDL(folder + "/" + fileName);
			wsdls.add(definitions);

			webServicesOperations.put(fileName, getWebServiceOperations(definitions));
		}

		// Do the matching
		WSMatchingType wsMatchingType = new WSMatchingType();

		List<String> webServicesOperationsKey = new ArrayList<String>(webServicesOperations.keySet());

		for (String inputOperationKey : webServicesOperationsKey)
		{
			for (String outputOperationKey : webServicesOperationsKey)
			{
				MatchedWebServiceType matchedWebServiceType = matchWebServiceOperations(webServicesOperations.get(inputOperationKey), webServicesOperations.get(outputOperationKey));
				matchedWebServiceType.setInputServiceName(inputOperationKey);
				matchedWebServiceType.setOutputServiceName(outputOperationKey);

				if (matchedWebServiceType.getWsScore() > limit && !inputOperationKey.equals(outputOperationKey))
				{
					wsMatchingType.getMacthing().add(matchedWebServiceType);
				}
			}
		}

		Collections.sort(wsMatchingType.getMacthing(), new Comparator<MatchedWebServiceType>()
		{

			@Override
			public int compare(MatchedWebServiceType o1, MatchedWebServiceType o2)
			{
				if (o1.getWsScore() > o2.getWsScore())
				{
					return -1;
				}
				else if (o1.getWsScore() < o2.getWsScore())
				{
					return 1;
				}
				else
				{
					return 0;
				}
			}
		});

		buildOutputDocument(wsMatchingType, outputFilename);
	}


	protected MatchedWebServiceType matchWebServiceOperations(List<Operation> inputOperations, List<Operation> outputOperations)
	{
		MatchedWebServiceType matchedWebServiceType = new MatchedWebServiceType();

		for (Operation inputOperation : inputOperations)
		{
			MatchedOperationType bestMatchedOperation = new MatchedOperationType();
			bestMatchedOperation.setInputOperationName(inputOperation.name);

			for (Operation outputOperation : outputOperations)
			{
				List<MatchedElementType> tempElements = new ArrayList<MatchedElementType>();

				if (inputOperation.getInputs().size() == outputOperation.getOutputs().size())
				{
					for (Parameter inputParameter : inputOperation.getInputs())
					{
						MatchedElementType bestMatchedElement = new MatchedElementType();
						bestMatchedElement.setInputElement(inputParameter.name);

						for (Parameter outputParameter : outputOperation.getOutputs())
						{
							double matchingScore = getMatchingScore(inputParameter, outputParameter);

							if (matchingScore > bestMatchedElement.getScore())
							{
								bestMatchedElement.setScore(matchingScore);
								bestMatchedElement.setOutputElement(outputParameter.name);
							}
						}

						tempElements.add(bestMatchedElement);
					}

					double operationScore = getOperationScore(tempElements);

					if (operationScore > bestMatchedOperation.getOpScore())
					{
						bestMatchedOperation.setOpScore(operationScore);
						bestMatchedOperation.setOutputOperationName(outputOperation.name);
						bestMatchedOperation.getMacthedElement().clear();
						bestMatchedOperation.getMacthedElement().addAll(tempElements);
					}
				}
			}

			if (bestMatchedOperation.getOutputOperationName() != null && !bestMatchedOperation.getOutputOperationName().isEmpty())
			{
				matchedWebServiceType.getMacthedOperation().add(bestMatchedOperation);
			}
		}

		double webServiceScore = getWebServiceScore(matchedWebServiceType.getMacthedOperation());
		matchedWebServiceType.setWsScore(webServiceScore);

		return matchedWebServiceType;
	}


	private double getOperationScore(List<MatchedElementType> elements)
	{
		double operationSum = 0.0;

		for (MatchedElementType matchedElementType : elements)
		{
			operationSum += matchedElementType.getScore();
		}

		return operationSum / Math.max(elements.size(), 1);
	}


	private double getWebServiceScore(List<MatchedOperationType> elements)
	{
		double serviceSum = 0.0;

		for (MatchedOperationType matchedElementType : elements)
		{
			serviceSum += matchedElementType.getOpScore();
		}

		return serviceSum / Math.max(elements.size(), 1);
	}


	protected List<Operation> getWebServiceOperations(TDefinitions inputService)
	{
		List<TOperation> inputServiceOperations = new ArrayList<>();

		Map<String, TMessage> inputServiceMessages = new HashMap<>();

		TTypes inputServiceTypes = null;

		// Get top level elements
		for (Object object : inputService.getAnyTopLevelOptionalElement())
		{
			if (object instanceof TPortType)
			{
				inputServiceOperations.addAll(((TPortType) object).getOperation());
			}
			else if (object instanceof TMessage)
			{
				TMessage message = (TMessage) object;
				inputServiceMessages.put(message.getName(), message);
			}
			else if (object instanceof TTypes)
			{
				inputServiceTypes = (TTypes) object;
			}
		}

		// Get elements for messages defined in schema.
		typeParameters.clear();
		elementParameters.clear();

		getServiceElementNodes(inputServiceTypes);

		// Get all operations in our own data structure.
		List<Operation> operations = new ArrayList<Operation>();

		for (TOperation tOperation : inputServiceOperations)
		{
			System.out.println("Operation: " + tOperation.getName());

			List<TMessage> inputMessages = getMessagesFromOperation(tOperation, inputServiceMessages, MessageType.INPUT);
			List<TMessage> outputMessages = getMessagesFromOperation(tOperation, inputServiceMessages, MessageType.OUTPUT);

			Operation operation = new Operation();
			operation.name = tOperation.getName();

			System.out.println("  Inputs:");
			operation.getInputs().addAll(getOperationParametersList(inputMessages));

			System.out.println("  Outputs:");
			operation.getOutputs().addAll(getOperationParametersList(outputMessages));

			if (!(operation.getInputs().isEmpty() && operation.getOutputs().isEmpty()))
			{
				operations.add(operation);
			}
		}

		return operations;
	}


	private List<Parameter> getOperationParametersList(List<TMessage> inputMessages)
	{
		List<Parameter> operationParts = new ArrayList<>();

		for (TMessage message : inputMessages)
		{
			for (TPart part : message.getPart())
			{
				if (part.getType() != null)
				{
					System.out.println("    Parameter: " + part.getName() + ", type: " + part.getType().getLocalPart());

					if (getAllowedTypes().contains(part.getType().getLocalPart()))
					{
						Parameter parameter = new Parameter(part.getName());
						for (QName qName : part.getOtherAttributes().keySet())
						{
							if (qName.getLocalPart().equals("modelReference"))
							{

								String ontology = part.getOtherAttributes().get(qName);

								String[] ontologySplit = ontology.split("#");

								if (ontologySplit.length > 1)
								{
									ontology = ontologySplit[1];
									parameter.ontology = ontology;
								}
							}
						}

						operationParts.add(parameter);
					}
					else
					{
						Parameter parameter = typeParameters.get(part.getType().getLocalPart());
						if (parameter != null)
						{
							operationParts.add(parameter);
						}
						else
						{
							System.out.println("#################################################### Cant find type parameter for " + part.getType().getLocalPart());
						}
					}
				}
				else if (part.getElement() != null)
				{
					Parameter parameter = elementParameters.get(part.getElement().getLocalPart());
					if (parameter != null)
					{
						if (parameter.type != null && !parameter.hasOntology())
						{
							Parameter nestedParameter = typeParameters.get(parameter.type);

							if (nestedParameter != null)
							{
								parameter = nestedParameter;
							}
						}
						else if (parameter.ref != null)
						{
							Parameter nestedParameter = elementParameters.get(parameter.ref);

							if (nestedParameter != null)
							{
								parameter = nestedParameter;
							}
						}

						operationParts.add(parameter);
					}
					else
					{
						System.out.println("#################################################### Cant find element parameter for " + part.getElement().getLocalPart());
					}
				}
			}
		}

		return operationParts;
	}


	private List<TMessage> getMessagesFromOperation(TOperation operation, Map<String, TMessage> messages, MessageType messageType)
	{
		List<TMessage> messagesList = new ArrayList<TMessage>();

		for (JAXBElement<? extends TExtensibleAttributesDocumented> element : operation.getRest())
		{
			if (element.getName().getLocalPart().equalsIgnoreCase(messageType.toString()))
			{
				if (element.getValue() instanceof TParam)
				{
					TParam param = (TParam) element.getValue();
					TMessage message = messages.get(param.getMessage().getLocalPart());

					if (message != null)
					{
						messagesList.add(message);
					}
				}
			}
		}

		return messagesList;
	}


	private void getServiceElementNodes(TTypes types)
	{
		for (Object object : types.getAny())
		{
			if (object instanceof ElementNSImpl)
			{
				ElementNSImpl element = (ElementNSImpl) object;

				Node node = element.getFirstChild();

				while (node != null && !(node instanceof TextImpl))
				{
					switch (node.getLocalName())
					{

					case "element":
					{
						Parameter parameter = new Parameter(node.getAttributes().getNamedItem("name").getNodeValue(), null);

						Node elementChild = node.getFirstChild();

						if (elementChild != null)
						{
							if (elementChild.getNodeName().contains("complexType"))
							{
								parameter.ontology = ontologyAttribute(node);

								Node complexTypeChild = elementChild.getFirstChild();

								if (complexTypeChild != null)
								{
									if (complexTypeChild.getNodeName().contains("sequence") || complexTypeChild.getNodeName().contains("all"))
									{
										Node sequenceChild = complexTypeChild.getFirstChild();

										if (sequenceChild != null)
										{
											parameter.nestedParameters = new ArrayList<>();

											while (sequenceChild != null && !(sequenceChild instanceof TextImpl))
											{
												String name = nameOrRefAttribute(sequenceChild);

												Parameter nestedParameter = new Parameter(name);

												nestedParameter.ontology = ontologyAttribute(sequenceChild);

												parameter.nestedParameters.add(nestedParameter);

												sequenceChild = sequenceChild.getNextSibling();
											}
										}
									}
								}
							}
						}
						else
						{
							Node typeNode = node.getAttributes().getNamedItem("type");

							if (typeNode != null)
							{
								System.out.println("#################################################### isTypedElemnent");

								String type = typeNode.getNodeValue();

								String[] splittedType = type.split(":");

								type = splittedType[0];

								if (splittedType.length > 1)
								{
									type = splittedType[1];
								}

								parameter.type = type;
							}

							Node refNode = node.getAttributes().getNamedItem("ref");

							if (refNode != null)
							{
								System.out.println("#################################################### isRefElement");

								String ref = refNode.getNodeValue();

								String[] splittedRef = ref.split(":");

								ref = splittedRef[0];

								if (splittedRef.length > 1)
								{
									ref = splittedRef[1];
								}

								parameter.ref = ref;
							}
						}

						elementParameters.put(parameter.name, parameter);

						break;
					}

					case "complexType":
					{
						Parameter parameter = new Parameter(node.getAttributes().getNamedItem("name").getNodeValue(), null);

						parameter.ontology = ontologyAttribute(node);

						Node complexTypeChild = node.getFirstChild();

						if (complexTypeChild != null)
						{
							if (complexTypeChild.getNodeName().contains("sequence") || complexTypeChild.getNodeName().contains("all"))
							{
								Node sequenceChild = complexTypeChild.getFirstChild();

								if (sequenceChild != null)
								{
									parameter.nestedParameters = new ArrayList<>();

									while (sequenceChild != null && !(sequenceChild instanceof TextImpl))
									{
										String name = nameOrRefAttribute(sequenceChild);

										Parameter nestedParameter = new Parameter(name);

										nestedParameter.ontology = ontologyAttribute(sequenceChild);

										parameter.nestedParameters.add(nestedParameter);

										sequenceChild = sequenceChild.getNextSibling();
									}
								}
							}
						}

						typeParameters.put(parameter.name, parameter);

						break;
					}

					case "simpleType":
					{
						String name = nameOrRefAttribute(node);

						Parameter parameter = new Parameter(name);

						parameter.ontology = ontologyAttribute(node);

						typeParameters.put(parameter.name, parameter);

						break;
					}

					default:
					{
						break;
					}

					}

					node = node.getNextSibling();
				}
			}
		}
	}


	private String nameOrRefAttribute(Node node)
	{
		Node nameAttribute = node.getAttributes().getNamedItem("name");

		if (nameAttribute != null)
		{
			String[] nameSplit = nameAttribute.getNodeValue().split(":");

			String name = nameSplit[0];
			if (nameSplit.length > 1)
			{
				name = nameSplit[1];
			}

			return name;
		}
		else
		{
			Node refAttribute = node.getAttributes().getNamedItem("ref");

			if (refAttribute != null)
			{
				String[] nameSplit = refAttribute.getNodeValue().split(":");

				String name = nameSplit[0];
				if (nameSplit.length > 1)
				{
					name = nameSplit[1];
				}

				return name;
			}

			System.out.println("#################################################### Unknown name");

			return "";
		}
	}


	private String ontologyAttribute(Node node)
	{
		Node modelReferenceAttribute = node.getAttributes().getNamedItem("sawsdl:modelReference");

		String ontology = "";
		if (modelReferenceAttribute != null)
		{
			String rawOntology = modelReferenceAttribute.getNodeValue();

			if (rawOntology != null)
			{
				String[] ontologySplit = rawOntology.split("#");

				if (ontologySplit.length > 1)
				{
					ontology = ontologySplit[1];
				}
			}
			else
			{
				System.out.println("#################################################### Empty modelreference");
			}
		}
		else
		{
			System.out.println("#################################################### No modelreference attribute");
		}

		return ontology;
	}


	// Output document generation

	protected void buildOutputDocument(WSMatchingType matchingType, String outputFileName)
	{
		try
		{
			JAXBContext outputJC = JAXBContext.newInstance("se.mattec.id2208.project.output");
			Marshaller m = outputJC.createMarshaller();
			m.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE);
			m.marshal(matchingType, new File("xml/" + outputFileName));
		}
		catch (JAXBException e)
		{
			e.printStackTrace();
		}
	}


	// Methods for parsing WSDLs

	@SuppressWarnings("unchecked")
	protected TDefinitions parseWSDL(String path)
	{
		System.out.println("Current path: " + path);
		try
		{
			JAXBContext jc = JAXBContext.newInstance("wsdl");
			Unmarshaller unmarshaller = jc.createUnmarshaller();
			JAXBElement<TDefinitions> definitions = (JAXBElement<TDefinitions>) unmarshaller.unmarshal(new File(path));

			return definitions.getValue();
		}
		catch (JAXBException e)
		{
			e.printStackTrace();
		}

		return null;
	}


	// Helpers

	protected List<String> listFilesForFolder(final File folder)
	{
		List<String> fileNames = new ArrayList<>();

		for (final File fileEntry : folder.listFiles())
		{
			if (fileEntry.isDirectory())
			{
				listFilesForFolder(fileEntry);
			}
			else
			{
				fileNames.add(fileEntry.getName());
			}
		}

		return fileNames;
	}


	private Set<String> getAllowedTypes()
	{
		Set<String> types = new HashSet<String>();

		types.add("int");
		types.add("double");
		types.add("decimal");
		types.add("string");
		types.add("long");
		types.add("boolean");
		types.add("float");
		types.add("base64Binary");

		return types;
	}

	// Helper classes

	protected static class Operation
	{
		String name;
		private List<Parameter> inputs;
		private List<Parameter> outputs;


		public String getName()
		{
			return name;
		}


		public void setName(String name)
		{
			this.name = name;
		}


		public List<Parameter> getInputs()
		{
			if (inputs == null)
			{
				inputs = new ArrayList<Parameter>();
			}
			else
			{
				List<Parameter> nestedParameters = new ArrayList<Matcher.Parameter>();
				for (Parameter parameter : inputs)
				{
					if (parameter.hasNestedParameters() && !parameter.hasOntology())
					{
						nestedParameters.addAll(parameter.nestedParameters);
					}
					else
					{
						nestedParameters.add(parameter);
					}
				}
				inputs = nestedParameters;
			}

			return inputs;
		}


		public List<Parameter> getOutputs()
		{
			if (outputs == null)
			{
				outputs = new ArrayList<Parameter>();
			}
			else
			{
				List<Parameter> nestedParameters = new ArrayList<Matcher.Parameter>();
				for (Parameter parameter : outputs)
				{
					if (parameter.hasNestedParameters() && !parameter.hasOntology())
					{
						nestedParameters.addAll(parameter.nestedParameters);
					}
					else
					{
						nestedParameters.add(parameter);
					}
				}
				outputs = nestedParameters;
			}

			return outputs;
		}

	}

	protected static class Parameter
	{
		String name;
		String ontology;

		List<Parameter> nestedParameters;

		String type;
		String ref;


		public Parameter(String name)
		{
			this.name = name;
		}


		public Parameter(String name, String ontology)
		{
			this.name = name;
			this.ontology = ontology;
		}


		public boolean hasNestedParameters()
		{
			return nestedParameters != null && !nestedParameters.isEmpty();
		}


		public boolean hasOntology()
		{
			return ontology != null && !ontology.isEmpty();
		}
	}

}
