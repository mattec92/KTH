README Green Elevator Controller

*The RMI elevator project is not provided*

Compile:

javac -cp "reference/to/elevator.jar" Controller.java ElevatorHandler.java Job.java
Example: javac -cp ".;D:\Dropbox\_Parallell_programmering\Elevator\elevator\lib\elevator.jar;" Controller.java ElevatorHandler.java Job.java


Run:

java -cp "reference/to/elevator.jar" Controller
Example: java -cp ".;D:\Dropbox\_Parallell_programmering\Elevator\elevator\lib\elevator.jar;" Controller


All commands must be run from the directory where the files (Controller.java ElevatorHandler.java Job.java) are kept.
The Elevator application is not provided. It can be downloaded from http://www.imit.kth.se/courses/ID1217/labs/task1.html

The elevator application must be started with the flag -rmi for the controller to work.