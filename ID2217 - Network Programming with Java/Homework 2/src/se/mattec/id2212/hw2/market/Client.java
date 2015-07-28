package se.mattec.id2212.hw2.market;

import java.awt.BorderLayout;
import java.awt.Button;
import java.awt.Frame;
import java.awt.HeadlessException;
import java.awt.Label;
import java.awt.List;
import java.awt.Panel;
import java.awt.TextField;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.rmi.Naming;
import java.rmi.RemoteException;
import java.rmi.registry.LocateRegistry;
import java.rmi.server.UnicastRemoteObject;

import javax.swing.BoxLayout;

import se.mattec.id2212.hw2.bank.Account;
import se.mattec.id2212.hw2.bank.Bank;
import se.mattec.id2212.hw2.bank.RejectedException;

public class Client extends UnicastRemoteObject implements OfferCallback 
{
	private static final String MARKET_NAME = "Market";
	private static final String BANK_NAME = "Bank";
	private String clientId;
	private Market market;
	private Bank bank;
	private Account account;

	//Used by the GUI.
	private List list;
	Label balanceLabel;


	public static void main(String[] args) throws RemoteException 
	{
		new Client();
	}


	/**
	 * Constructor. Getting a reference to the market using the naming service.
	 * Starts the main gui.
	 * */
	public Client() throws RemoteException 
	{
		super();
		try {
			try 
			{
				LocateRegistry.getRegistry(1099).list();
			} 
			catch (RemoteException e) 
			{
				LocateRegistry.createRegistry(1099);
			}
			market = (Market) Naming.lookup(MARKET_NAME);
			bank = (Bank) Naming.lookup(BANK_NAME);
		} 
		catch (Exception e) 
		{
			System.out.println("The runtime failed: " + e.getMessage());
			System.exit(0);
		}

		System.out.println("Connected to market");

		clientId = String.valueOf((int) (Math.random() * 1000));

		//Create a new account in the bank
		try 
		{
			account = bank.newAccount(String.valueOf(clientId));
		} 
		catch (RejectedException e) 
		{
			e.printStackTrace();
		}

		buildMainGui();
	}


	/**
	 * Builds the main GUI with a list of items in the market and buttons
	 * for adding a buy/sell offer and refreshing the list.
	 * */
	private void buildMainGui() 
	{
		final Frame frame = new Frame("Market: " + clientId);

		Panel panel = new Panel(new BorderLayout());

		list = new List(10, false);
		panel.add(list, BorderLayout.NORTH);

		updateList();

		Button sellOfferButton = new Button("Add sell offer");

		sellOfferButton.addActionListener(new ActionListener() 
		{

			@Override
			public void actionPerformed(ActionEvent e) 
			{
				buildAddOfferGui(false);
			}
		});

		Button buyOfferButton = new Button("Add buy offer");

		buyOfferButton.addActionListener(new ActionListener() 
		{

			@Override
			public void actionPerformed(ActionEvent e) 
			{
				buildAddOfferGui(true);
			}
		});

		Button updateListButton = new Button("Refresh");

		updateListButton.addActionListener(new ActionListener() 
		{

			@Override
			public void actionPerformed(ActionEvent e) 
			{
				updateList();
			}
		});

		Button bankButton = new Button("Open bank");

		bankButton.addActionListener(new ActionListener() 
		{

			@Override
			public void actionPerformed(ActionEvent e) 
			{
				buildBankGui();
			}
		});

		Panel buttonPanel = new Panel();
		buttonPanel.add(sellOfferButton);
		buttonPanel.add(buyOfferButton);
		buttonPanel.add(updateListButton);
		buttonPanel.add(bankButton);

		panel.add(buttonPanel, BorderLayout.SOUTH);

		frame.add(panel);
		frame.pack();
		frame.setVisible(true);
		frame.addWindowListener(new WindowAdapter()
		{
			public void windowClosing(WindowEvent we)
			{
				frame.dispose();
				System.exit(0);
			}
		});
	}


	/**
	 * Updates the list in the main GUI with the latest offers.
	 * */
	private void updateList() 
	{
		//Reset list items and listeners
		list.removeAll();
		ItemListener[] listeners = list.getItemListeners();
		if (listeners.length > 0) {
			list.removeItemListener(listeners[0]);
		}

		try
		{
			java.util.List<Offer> offers = market.listOffers();

			for (Offer offer : offers)
			{
				list.add(offer.getStringRepresentation());
			}
		} 
		catch (RemoteException e) 
		{
			e.printStackTrace();
		}

		list.addItemListener(new ItemListener()
		{

			@Override
			public void itemStateChanged(ItemEvent e) 
			{
				java.util.List<Offer> offers = null;
				try 
				{
					offers = market.listOffers();
				} 
				catch (RemoteException e1) 
				{
					e1.printStackTrace();
				}

				if (offers != null)
				{
					buildDetailsGui(offers.get(list.getSelectedIndex()));
				}
			}
		});
	}


	/**
	 * Creates the details GUI containing the item information and
	 * buttons to remove or accept the offer.
	 * */
	private void buildDetailsGui(final Offer offer) 
	{
		final Frame frame = new Frame("Item details: " + clientId);
		Panel panel = new Panel(new BorderLayout());

		Label label = null;
		try 
		{
			label = new Label(offer.getStringRepresentation());
		} 
		catch (HeadlessException | RemoteException e2) 
		{
			e2.printStackTrace();
		}

		Panel buttonPanel = new Panel();
		
		try {
			//Only the client who created the offer may remove it.
			if (offer.getCallback().getClientId().equals(clientId)) 
			{
				Button removeButton = new Button("Remove offer");
				removeButton.addActionListener(new ActionListener() 
				{

					@Override
					public void actionPerformed(ActionEvent e) 
					{
						try 
						{
							market.removeOffer(offer);
							updateList();
							frame.dispose();
						} 
						catch (RemoteException e1) 
						{
							e1.printStackTrace();
						}
					}
				});

				buttonPanel.add(removeButton);
			}
		} 
		catch (HeadlessException | RemoteException e2) {
			e2.printStackTrace();
		}

		Button acceptButton = new Button("Accept offer");
		acceptButton.addActionListener(new ActionListener() 
		{

			@Override
			public void actionPerformed(ActionEvent e) 
			{
				try 
				{
					//If this is a sell offer, the one who accepts the offer need funds.
					//Funds are checked by using the rejected exception in the bank.
					if (offer.getType() == OfferTypeEnum.SELL_OFFER)
					{
						try
						{
							account.withdraw(offer.getPrice());
							String offerClientId = offer.getCallback().getClientId();
							bank.getAccount(offerClientId).deposit(offer.getPrice()); 							
							market.acceptOffer(offer);
						}
						catch (RejectedException e1) 
						{
							buildNotificationGui(null, offer, "You dont have enough funds in your bank account.");
						}
					}
					else if (offer.getType() == OfferTypeEnum.BUY_OFFER)
					{
						//They need funds
						try
						{
							account.deposit(offer.getPrice());
							String offerClientId = offer.getCallback().getClientId();
							bank.getAccount(offerClientId).withdraw(offer.getPrice()); 							
							market.acceptOffer(offer);
						}
						catch (RejectedException e1) 
						{
							buildNotificationGui(null, offer, "You dont have enough funds in your bank account.");
						}
					}

					updateList();
					frame.dispose();
				} 
				catch (RemoteException e1) 
				{
					e1.printStackTrace();
				}
			}
		});

		buttonPanel.add(acceptButton);

		panel.add(buttonPanel, BorderLayout.SOUTH);
		panel.add(label, BorderLayout.NORTH);

		frame.add(panel);
		frame.pack();
		frame.setSize(300, frame.getSize().height);
		frame.setVisible(true);
		frame.addWindowListener(new WindowAdapter()
		{
			public void windowClosing(WindowEvent we)
			{
				frame.dispose();
			}
		});
	}


	/**
	 * Creates a simple notification GUI containing a message and the
	 * offers the notification is about. (if any)
	 * */
	private void buildNotificationGui(Offer offer, Offer matchingOffer, String message)
	{
		final Frame frame = new Frame("Notification: " + clientId);

		Panel panel = new Panel();
		panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));

		Label messageLabel = new Label(message);
		panel.add(messageLabel);

		try 
		{
			if (offer != null) 
			{
				Label yourOfferLabel = new Label("Your offer: " + offer.getStringRepresentation());
				panel.add(yourOfferLabel);
			}

			if (matchingOffer != null) 
			{
				Label matchingOfferLabel = new Label("Their offer: " + matchingOffer.getStringRepresentation());
				panel.add(matchingOfferLabel);
			}
		} 
		catch (HeadlessException | RemoteException e) 
		{
			e.printStackTrace();
		}

		frame.add(panel);
		frame.pack();
		frame.setVisible(true);
		frame.addWindowListener(new WindowAdapter()
		{
			public void windowClosing(WindowEvent we)
			{
				frame.dispose();
			}
		});
	}


	/**
	 * Creates a GUI to add an item to the marketplace.
	 * */
	private void buildAddOfferGui(final boolean isBuyOffer) 
	{		
		final Frame frame = new Frame("Add new offer: " + clientId);
		Panel panel = new Panel();
		panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));

		Label nameLabel = new Label("Name");
		Label priceLabel = new Label("Price");

		final TextField nameTextField = new TextField();
		final TextField priceTextField = new TextField();

		Button button = new Button("Add offer");
		button.addActionListener(new ActionListener() 
		{

			@Override
			public void actionPerformed(ActionEvent e) 
			{
				String name = nameTextField.getText();
				String price = priceTextField.getText();
				int priceAsInteger = 0;
				try 
				{
					priceAsInteger = Integer.parseInt(price);
				}
				catch (NumberFormatException e1) 
				{
					e1.printStackTrace();
				}

				if (name.length() > 0 && priceAsInteger > 0) {
					OfferTypeEnum type = OfferTypeEnum.SELL_OFFER;

					if (isBuyOffer) 
					{
						type = OfferTypeEnum.BUY_OFFER;
					}

					try 
					{
						market.putOffer(name, priceAsInteger, type, Client.this);
					} 
					catch (RemoteException e1) 
					{
						e1.printStackTrace();
					}

					updateList();
					frame.dispose();
				}
			}
		});

		panel.add(nameLabel);
		panel.add(nameTextField);
		panel.add(priceLabel);
		panel.add(priceTextField);
		panel.add(button);

		frame.add(panel);
		frame.pack();
		frame.setSize(300, frame.getSize().height);
		frame.setVisible(true);
		frame.addWindowListener(new WindowAdapter()
		{
			public void windowClosing(WindowEvent we)
			{
				frame.dispose();
			}
		});
	}


	/**
	 * Creates a simple GUI for the bank.
	 * */
	private void buildBankGui() 
	{
		final Frame frame = new Frame("Bank: " + clientId);
		Panel panel = new Panel();
		panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));

		balanceLabel = new Label();

		updateBalance();

		Panel depositWithdrawPanel = new Panel();

		final TextField amountField = new TextField();
		amountField.setColumns(10);

		Button withdrawButton = new Button("Withdraw amount");
		withdrawButton.addActionListener(new ActionListener() 
		{

			@Override
			public void actionPerformed(ActionEvent e) 
			{
				try 
				{
					account.withdraw(Float.parseFloat(amountField.getText()));
					updateBalance();
				} 
				catch (NumberFormatException | RemoteException | RejectedException e1) 
				{
					e1.printStackTrace();
				}
			}
		});

		Button depositButton = new Button("Deposit amount");
		depositButton.addActionListener(new ActionListener() 
		{

			@Override
			public void actionPerformed(ActionEvent e)
			{
				try 
				{
					account.deposit(Float.parseFloat(amountField.getText()));
					updateBalance();
				} 
				catch (NumberFormatException | RemoteException | RejectedException e1) 
				{
					e1.printStackTrace();
				}
			}
		});

		depositWithdrawPanel.add(amountField);
		depositWithdrawPanel.add(withdrawButton);
		depositWithdrawPanel.add(depositButton);

		panel.add(balanceLabel);
		panel.add(depositWithdrawPanel);

		frame.add(panel);
		frame.pack();
		frame.setVisible(true);
		frame.addWindowListener(new WindowAdapter()
		{
			public void windowClosing(WindowEvent we)
			{
				frame.dispose();
			}
		});
	}


	/**
	 * Updates the balance label in the bank GUI.
	 * */
	private void updateBalance() 
	{
		try 
		{
			Float balance = bank.getAccount(clientId).getBalance();
			balanceLabel.setText("Balance: " + balance);
		} 
		catch (RemoteException e) 
		{
			e.printStackTrace();
		}
	}


	/**
	 * Callback method triggered when an offer is accepted or two
	 * offers match each other.
	 * */
	@Override
	public void notify(Offer offer, Offer matchingOffer, String message) throws RemoteException 
	{
		buildNotificationGui(offer, matchingOffer, message);
	}


	@Override
	public String getClientId() throws RemoteException 
	{
		return clientId;
	}

}
