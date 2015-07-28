The source code is provided in an Eclipse project structure.
To run it, import it to Eclipse as a Java project.

Run the project to open the Agent Manager.

Start one CuratorAgent, one TourGuideAgent and one ProfilerAgent without any arguments. 
There is no requirements on naming.

Task #1: Click a guide name in the GUIs list of guides to get a virtual tour from the guide.
A new window should open with the artifacts of the virtual tour.

Task #2: Start a new TourGuideAgent while the ProfilerAgent is running. 
The ProfilerAgent will get informed by the DF subscription and the new TourGuideAgent 
should be added to its list of available guides.