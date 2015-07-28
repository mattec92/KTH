The source code is provided in an Eclipse project structure.
To run it, import it to Eclipse as a Java project.

Run the project to open the Agent Manager.

Start one CuratorAgent, one TourGuideAgent and one ProfilerAgent without any arguments. 
There is no requirements on naming.

Task #1: Click a guide name in the GUIs list of guides to get a virtual tour from the guide.
A new window should open with the artifacts of the virtual tour.

Task #2: Start a couple more CuratorAgents and provide a decimal number (0.0-2.0) as argument.
This number will decide how much the CuratorAgent will pay in auctions. Higher number = accepting higher prices.
Then start the ArtistManagerAgent, insert a price (Integer value) and start the auction.
Information about the progress can be found in the console and when the auction is finished a summary is also presented.

