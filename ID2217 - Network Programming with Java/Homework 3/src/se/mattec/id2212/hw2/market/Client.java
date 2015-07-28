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
	private String userId;
	private Market market;
	private Bank bank;
	private Account account;

	// Used by the GUI.
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
		try
		{
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

		buildLoginGUI();
	}


	/**
	 * Builds the main GUI with a list of items in the market and buttons for
	 * adding a buy/sell offer and refreshing the list.
	 * */
	private void buildMainGui()
	{
		final Frame frame = new Frame("Market: " + userId);

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

		Button userButton = new Button("User info");

		userButton.addActionListener(new ActionListener()
		{

			@Override
			public void actionPerformed(ActionEvent e)
			{
				buildUserInfoGUI();
			}
		});

		Panel buttonPanel = new Panel();
		buttonPanel.add(sellOfferButton);
		buttonPanel.add(buyOfferButton);
		buttonPanel.add(updateListButton);
		buttonPanel.add(bankButton);
		buttonPanel.add(userButton);

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
		// Reset list items and listeners
		list.removeAll();
		ItemListener[] listeners = list.getItemListeners();
		if (listeners.length > 0)
		{
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
	 * Creates the details GUI containing the item information and buttons to
	 * remove or accept the offer.
	 * */
	private void buildDetailsGui(final Offer offer)
	{
		final Frame frame = new Frame("Item details: " + userId);
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

		try
		{
			// Only the client who created the offer may remove it.
			if (offer.getuserId().equals(userId))
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
						catch (RemoteException | RejectedException e1)
						{
							e1.printStackTrace();
						}
					}
				});

				buttonPanel.add(removeButton);
			}
			else
			{
				Button acceptButton = new Button("Accept offer");
				acceptButton.addActionListener(new ActionListener()
				{

					@Override
					public void actionPerformed(ActionEvent e)
					{
						try
						{
							market.acceptOffer(offer, userId);
							updateList();
							frame.dispose();
						}
						catch (RemoteException e1)
						{
							e1.printStackTrace();
						}
						catch (RejectedException e1)
						{
							buildNotificationGui(null, null, e1.getMessage());
						}
					}
				});

				buttonPanel.add(acceptButton);
			}
		}
		catch (HeadlessException | RemoteException e2)
		{
			e2.printStackTrace();
		}

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
	 * Creates a simple notification GUI containing a message and the offers the
	 * notification is about. (if any)
	 * */
	private void buildNotificationGui(Offer offer, Offer matchingOffer, String message)
	{
		final Frame frame = new Frame("Notification: " + userId);

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
		final Frame frame = new Frame("Add new offer: " + userId);
		Panel panel = new Panel();
		panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));

		Label nameLabel = new Label("Name");
		Label priceLabel = new Label("Price");
		Label amountLabel = new Label("Amount");

		final TextField nameTextField = new TextField();
		final TextField priceTextField = new TextField();
		final TextField amountTextField = new TextField(1);

		Button button = new Button("Add offer");
		button.addActionListener(new ActionListener()
		{

			@Override
			public void actionPerformed(ActionEvent e)
			{
				String name = nameTextField.getText();
				String price = priceTextField.getText();
				String amount = amountTextField.getText();
				int priceAsInteger = 0;
				int amountAsInteger = 1;
				try
				{
					priceAsInteger = Integer.parseInt(price);

					if (amount != null && amount.isEmpty() == false)
					{
						amountAsInteger = Integer.parseInt(amount);
					}
				}
				catch (NumberFormatException e1)
				{
					buildNotificationGui(null, null, "Price and amount needs to be numbers.");
				}

				OfferTypeEnum type = OfferTypeEnum.SELL_OFFER;

				if (isBuyOffer)
				{
					type = OfferTypeEnum.BUY_OFFER;
				}

				try
				{
					market.putOffer(name, priceAsInteger, type, Client.this, userId, amountAsInteger);

					updateList();
					frame.dispose();
				}
				catch (RemoteException e1)
				{
					e1.printStackTrace();
				}
				catch (RejectedException e1)
				{
					buildNotificationGui(null, null, e1.getMessage());
				}
			}
		});

		panel.add(nameLabel);
		panel.add(nameTextField);
		panel.add(priceLabel);
		panel.add(priceTextField);
		panel.add(amountLabel);
		panel.add(amountTextField);
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
		final Frame frame = new Frame("Bank: " + userId);
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
			Float balance = bank.getAccount(userId).getBalance();
			balanceLabel.setText("Balance: " + balance);
		}
		catch (RemoteException e)
		{
			e.printStackTrace();
		}
	}


	private void buildLoginGUI()
	{
		final Frame frame = new Frame("Log in");
		Panel panel = new Panel();
		panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));

		Label usernameLabel = new Label("Username");
		Label passwordLabel = new Label("Password");

		final TextField usernameTextField = new TextField();
		final TextField passwordTextField = new TextField();

		Button loginButton = new Button("Log in");
		loginButton.addActionListener(new ActionListener()
		{

			@Override
			public void actionPerformed(ActionEvent e)
			{
				String username = usernameTextField.getText();
				String password = passwordTextField.getText();

				try
				{
					if (market.logIn(username, password))
					{
						// Set clientId and get the bank account.
						userId = username;
						account = bank.getAccount(userId);

						frame.dispose();
						buildMainGui();
					}
					else
					{
						buildNotificationGui(null, null, "Wrong username or password");
					}
				}
				catch (RemoteException | RejectedException e1)
				{
					e1.printStackTrace();
				}
			}
		});

		Button registerButton = new Button("Register");
		registerButton.addActionListener(new ActionListener()
		{

			@Override
			public void actionPerformed(ActionEvent e)
			{
				String username = usernameTextField.getText();
				String password = passwordTextField.getText();

				try
				{
					market.register(username, password);

					// Set clientId and get the bank account.
					userId = username;
					account = bank.getAccount(userId);

					frame.dispose();
					buildMainGui();
				}
				catch (RemoteException e1)
				{
					e1.printStackTrace();
				}
				catch (RejectedException e2)
				{
					buildNotificationGui(null, null, e2.getMessage());
				}
			}
		});

		panel.add(usernameLabel);
		panel.add(usernameTextField);
		panel.add(passwordLabel);
		panel.add(passwordTextField);
		panel.add(loginButton);
		panel.add(registerButton);

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


	private void buildUserInfoGUI()
	{
		try
		{
			int sellCount = market.getSellCount(userId);
			int buyCount = market.getBuyCount(userId);

			final Frame frame = new Frame("User stats: " + userId);
			Panel panel = new Panel();
			panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));

			Label itemsSold = new Label("Items sold: " + sellCount);
			Label itemsBought = new Label("Items bought: " + buyCount);

			panel.add(itemsSold);
			panel.add(itemsBought);

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
		catch (RemoteException e)
		{
			e.printStackTrace();
		}
	}


	/**
	 * Callback method triggered when an offer is accepted or two offers match
	 * each other.
	 * */
	@Override
	public void notify(Offer offer, Offer matchingOffer, String message) throws RemoteException
	{
		buildNotificationGui(offer, matchingOffer, message);
	}


	@Override
	public String getClientId() throws RemoteException
	{
		return userId;
	}

}
