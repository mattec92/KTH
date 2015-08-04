import java.rmi.RemoteException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.ReentrantLock;

import elevator.Elevators;
import elevator.rmi.Elevator;
import elevator.rmi.MakeAll;

public class ElevatorHandler extends Thread {
	Elevator elevator; 			//This elevator
	int elevatorIndex;			//This elevator index

	int destinationFloor = 0;	//Current destination floor
	int lastStop = -1;			//Latest floor the elevator stopped on
	int direction = 0;			//Current direction
	double currentPosition;		//Current position of elevator
	boolean isStopped = false;	//Used too keep track of stop button state
	boolean isJobDone = false;	//Used to keep track if there is any ongoing jobs that are not done

	//Used for calculating cost of adding a new job (floor job)
	int currentDirectionJobsCount = 0;
	int nextDirectionJobsCount = 0;
	int thirdDirectionJobsCount = 0;

	//Used to keep track of jobs
	Job currentJob;
	ArrayList<Job> jobList = new ArrayList<Job>();

	//Used to signal when a new job has been added and elevator should wake up
	ReentrantLock lock = new ReentrantLock();
	Condition joblistNotEmptyCondition = lock.newCondition();

	//Constants used for direction, to make code more readable
	static final int DIRECTION_STILL = 0;
	static final int DIRECTION_UP = 1;
	static final int DIRECTION_DOWN = -1;

	public ElevatorHandler(Elevator elevator, int elevatorIndex) {
		this.elevator = elevator;
		this.elevatorIndex = elevatorIndex;
		this.setName("Elevator " + elevatorIndex);
	}

	@Override
	public void run() {
		super.run();
		try {
			while (true) {
				//If the list of jobs is empty
				if (jobList.isEmpty()) {
					//Wait for signal
					synchronized (joblistNotEmptyCondition) {
						joblistNotEmptyCondition.wait();
					}
					//When signaled, continue with performing the first job
					isJobDone = false;
					currentJob = jobList.get(0);
					performJob(currentJob);
				}
				//If the current job is null (and the job list is non empty), perform first job
				else if (currentJob == null) {
					currentJob = jobList.get(0);
					performJob(currentJob);
					isJobDone = false;
				}

				currentPosition = elevator.whereIs();
				elevator.setScalePosition((int) (currentPosition + 0.5));
				//If the elevator is at its destination and job has not been marked as done yet
				if (isAtDestination(currentPosition) && isJobDone == false) {
					lastStop = destinationFloor;
					//Stop the elevator, open doors, remove job from list and current and mark as done
					elevator.stop();
					elevator.open();
					jobList.remove(currentJob);
					currentJob = null;
					isJobDone = true;
				}
			}
		} 
		catch (Exception e) {
			e.printStackTrace();
		}
	}

	//isAtDestination - Method that checks if elevator is at its destination (Well, if it just passed it)
	private boolean isAtDestination(double position) {
		//If elevator is going up and has passed the floor, return true
		if (direction == DIRECTION_UP && position >= (double) destinationFloor) {
			System.out.println("Elevator " + elevatorIndex + " stopped at floor: " + position + " Destination: " + destinationFloor + " Direction:" + ((direction == DIRECTION_DOWN) ? "down" : "up"));
			System.out.println(jobList + "\n");
			return true;
		} 
		//If elevator is going down and has passed the floor, return true
		else if (direction == DIRECTION_DOWN && position <= (double) destinationFloor) {
			System.out.println("Elevator " + elevatorIndex + " stopped at floor: " + position + " Destination: " + destinationFloor + " Direction: " + ((direction == DIRECTION_DOWN) ? "down" : "up"));
			System.out.println(jobList + "\n");
			return true;
		}
		return false;
	}

	//getDirection - Starts elevator and sets the correct direction
	public void getDirection() {
		try {
			//If current position is above destination, go down
			if (currentPosition >= (double) destinationFloor) {
				elevator.down();
				direction = DIRECTION_DOWN;
				System.out.println("Elevator " + elevatorIndex + " going down, pos: " + elevator.whereIs() + " dest: " + destinationFloor);
			} 
			//If current position is below destination, go up
			else if (currentPosition <= (double) destinationFloor) {
				elevator.up();
				direction = DIRECTION_UP;
				System.out.println("Elevator " + elevatorIndex + " going up, pos: " + elevator.whereIs() + " dest: " + destinationFloor);
			}
		} 
		catch (RemoteException e) {
			e.printStackTrace();
		}
	}

	//adJob - Adds a job to the job list and notifies elevator to wake up
	public void addJob(Job job) {
		boolean isDuplicate = false;
		//Find out if the job is a duplicate
		for (Job jobEntry : jobList) {
			if (jobEntry.destinationFloor == job.destinationFloor && job.direction == jobEntry.direction) {
				isDuplicate = true;
				break;
			}
		}

		//If job is not a duplicate add it to the job list
		if (isDuplicate == false) {
			jobList.add(job);

			//Prioritize the joblist appropriately
			jobList = prioritizeJobs(jobList);
			System.out.println("Elevator " + elevatorIndex + " added job to list: " + job.toString());
			System.out.println(jobList + "\n");

			//Notify elevator that there is a new job
			synchronized (joblistNotEmptyCondition) {
				joblistNotEmptyCondition.notify();
			}
			//Set current job to the new first prioritized job so the elevator get its target
			//updated even if it is already moving
			currentJob = jobList.get(0);
			destinationFloor = currentJob.destinationFloor;
		}
	}

	//performJob - Perform a job
	public void performJob(Job job) {
		destinationFloor = job.destinationFloor;
		//If we are not already at the floor the job is to, perform job
		if (lastStop != destinationFloor) {
			try {
				//Close doors (and wait for some time)
				Thread.sleep(1000);
				elevator.close();
				Thread.sleep(4 * (long) (Elevators.step / MakeAll.getVelocity()));
			} catch (Exception e) {
				e.printStackTrace();
			}
			//Start the elevator in the correct direction
			getDirection();
		}
	}

	//getCost - Get the cost for the job (Used to decide which elevator will serve a floor job)
	public double getCost(Job job) {		
		try {
			//Make a copy of the job list, add the job and prioritize
			ArrayList<Job> jobs = new ArrayList<Job>(jobList);
			//Measure ditance to end of jobs before adding the new job, used to calculate total extra distance
			double distanceToEndBeforeJob = getDistanceToJob(jobs, jobs.size() - 1);
			jobs.add(job);
			jobs = prioritizeJobs(jobs);
			//Measure distance to end of jobs after adding the new job, used to calculate total extra distance
			double distanceToEndAfterJob = getDistanceToJob(jobs, jobs.size() - 1);
			//Get index of job in joblist; how many stops before the new job
			int indexOfJob = jobs.indexOf(job);
			//Get the distance to the job; how many floors to pass before the new job
			double distanceToJob = getDistanceToJob(jobs, indexOfJob);
			//If this job is the only job in the joblist, use distance to floor instead
			if (distanceToJob == 0) {
				distanceToJob = Math.abs(elevator.whereIs() - job.destinationFloor);
			}
			//Calculations of time with the elevator velocity in mind (Pretty much exact milliseconds to job)
			double velocity = MakeAll.getVelocity();
			double tickLength =  Elevators.step / velocity;
			int stopDelay = (int) (indexOfJob * (4 * tickLength + 1000));
			int travelDelay = (int) (distanceToJob * (25 * tickLength));

			//If the elevator was already stopping at the new jobs floor, remove the extra stopping time from calculation. (It was added twice otherwise)
			if (indexOfJob > 0 && indexOfJob != (jobs.size() - 1)) {
				if (jobs.get(indexOfJob - 1).destinationFloor == job.destinationFloor || jobs.get(indexOfJob + 1).destinationFloor == job.destinationFloor) {
					stopDelay -= (4 * tickLength + 1000);
				}
			}

			//Number of delayed jobs and correpsonding delay time is calculated 
			int numDelayedJobs = (jobs.size() - 1) - indexOfJob;
			double delayTime = numDelayedJobs * (4 * tickLength + 1000);

			//Extra distance is calculated. If calculation gives 0, then this is the first job
			//and the extra distance then is the same as distance to job.
			double extraDistance = (distanceToEndAfterJob-distanceToEndBeforeJob);
			if (indexOfJob == 0) {
				extraDistance = distanceToJob;
			}

			System.out.println("Cost info for elevator " + elevatorIndex + ": Stoptime " + stopDelay + ", Traveltime: " + travelDelay);	
			System.out.println("Extra distance: " + extraDistance + " DelayTime: " + delayTime);
			System.out.println("Total cost for elevator " + elevatorIndex + ": " + (stopDelay + travelDelay + delayTime) * extraDistance);
			//Return the sum of travel time and stopping time
			return (stopDelay + travelDelay + delayTime) * extraDistance;
		} catch (Exception e) {
			e.printStackTrace();
			return Integer.MAX_VALUE;
		}
	}

	//getDistanceToJob - Returns the distance to the job in number of floors to travel.
	public double getDistanceToJob(ArrayList<Job> jobs, int indexOfJob) {
		try {
			//If there are any jobs in the list
			if (jobs.size() > 0) {
				double currentDirectionJobsLength = 0;
				double nextDirectionJobsLength = 0;
				double thirdDirectionJobsLength = 0;
				boolean foundJob = false;
				double position = elevator.whereIs();
				//Get distance of current direction before highest/lowest floor
				if (currentDirectionJobsCount > 0) {
					int endIndex = currentDirectionJobsCount - 1;
					if (indexOfJob < endIndex) {
						endIndex = indexOfJob;
						foundJob = true;
					}
					currentDirectionJobsLength = Math.abs(position - jobs.get(endIndex).destinationFloor);
				}
				//Get distance of next direction, when the elevator turned around, if job was not passed yet
				if (nextDirectionJobsCount > 0 && foundJob == false) {
					int startIndex = 0;
					if (currentDirectionJobsCount > 0) {
						startIndex = currentDirectionJobsCount - 1;
					}

					int endIndex = currentDirectionJobsCount + nextDirectionJobsCount - 1;
					//Endindex is indexOfJob if job is in this section of the joblist
					if (indexOfJob < endIndex) {
						endIndex = indexOfJob;
						foundJob = true;
					}
					//If there was a job in the current direction, then calculate from first to last job in next direction
					if (currentDirectionJobsLength > 0) {
						nextDirectionJobsLength = Math.abs(jobs.get(startIndex).destinationFloor - jobs.get(endIndex).destinationFloor);
					}
					//Otherwise calculate from current position
					else {
						nextDirectionJobsLength = Math.abs(position - jobs.get(endIndex).destinationFloor);
					}
				}
				//Get distance of third direction, after turning around and serving the floors that
				//the elevator already had passed in the first direction, if job was not passed yet
				if (thirdDirectionJobsCount > 0 && foundJob == false) {
					int startIndex = 0;
					if (currentDirectionJobsCount > 0 || nextDirectionJobsCount > 0) {
						startIndex = currentDirectionJobsCount + nextDirectionJobsCount - 1;
					}

					int endIndex = currentDirectionJobsCount + nextDirectionJobsCount + thirdDirectionJobsCount - 1;
					//Endindex is indexOfJob if job is in this section of the joblist
					if (indexOfJob < endIndex) {
						endIndex = indexOfJob;
					}
					//If there was a job in the current or next direction, then calculate from first to last job in third direction
					if (currentDirectionJobsLength > 0 || nextDirectionJobsLength > 0) {
						thirdDirectionJobsLength = Math.abs(jobs.get(startIndex).destinationFloor - jobs.get(endIndex).destinationFloor);
					}
					//Otherwise calculate from current position
					else {
						thirdDirectionJobsLength = Math.abs(position - jobs.get(endIndex).destinationFloor);
					}
				}
				//Return the sum of the three directions (There are always a maximum of 3 directions scheduled at any time)
				return (currentDirectionJobsLength + nextDirectionJobsLength + thirdDirectionJobsLength);
			}
		} catch (RemoteException e) {
			e.printStackTrace();
		}
		return 0;
	}

	//prioritizeJob - prioritizes the jobs to minimize traveling distance
	public ArrayList<Job> prioritizeJobs(ArrayList<Job> jobs) {
		ArrayList<Job> currentDirectionJobs = new ArrayList<Job>();
		ArrayList<Job> nextDirectionJobs = new ArrayList<Job>();
		ArrayList<Job> thirdDirectionJobs = new ArrayList<Job>();
		//Go through all jobs
		for (Job job : jobs) {
			try {
				//If direction is not set - this is a job from buttons inside of elevator
				if (job.direction == 0) {
					//If direction is up and elevator havent passed the floor yet, add it to current direction jobs
					if (direction == DIRECTION_UP && (double) job.destinationFloor > elevator.whereIs()) {
						currentDirectionJobs.add(job);
					}
					//If direction is up and elevator have passed the floor yet, add it to next direction jobs
					else if (direction == DIRECTION_UP){
						nextDirectionJobs.add(job);
					}
					//If direction is down and elevator havent passed the floor yet, add it to current direction jobs
					else if (direction == DIRECTION_DOWN && (double) job.destinationFloor < elevator.whereIs()) {
						currentDirectionJobs.add(job);
					}
					//If direction is down and elevator havent passed the floor yet, add it to next direction jobs
					else if (direction == DIRECTION_DOWN) {
						nextDirectionJobs.add(job);
					}
					//If direction is still (only on first press after starting controller), add it to current direction jobs
					else if (direction == DIRECTION_STILL) {
						currentDirectionJobs.add(job);
					}
				}
				//If direction is set - this is a floor job
				else {
					//If direction of job is the same as current direction
					if (direction == job.direction || direction == DIRECTION_STILL) {
						//If direction is up and elevator havent passed the floor yet, add it to current direction jobs
						if (direction == DIRECTION_UP && (double) job.destinationFloor > elevator.whereIs()) {
							currentDirectionJobs.add(job);
						}
						//If direction is down and elevator havent passed the floor yet, add it to current direction jobs
						else if (direction == DIRECTION_DOWN && (double) job.destinationFloor < elevator.whereIs()) {
							currentDirectionJobs.add(job);
						}
						else if (direction == DIRECTION_STILL) {
							currentDirectionJobs.add(job);
						}
						//If elevator already passed the floor, add job to third direction (Cant add it to next because that would be in the wrong direction)
						else {
							thirdDirectionJobs.add(job);
						}
					}
					//If direction of job is not the same as current direction, then its the next direction
					else {
						nextDirectionJobs.add(job);
					}
				}
			} catch (RemoteException e) {
				e.printStackTrace();
			}
		}
		//Sort the jobs appropriately, if going up or still, jobs should be ascending-descending-ascending
		if (direction == DIRECTION_UP || direction == DIRECTION_STILL) {
			Collections.sort(currentDirectionJobs, new Job.JobComparatorByFloorAscending());
			Collections.sort(nextDirectionJobs, new Job.JobComparatorByFloorDescending());
			Collections.sort(thirdDirectionJobs, new Job.JobComparatorByFloorAscending());			
		}
		//If going down, jobs should be descending-ascending-descending
		else if (direction == DIRECTION_DOWN) {
			Collections.sort(currentDirectionJobs, new Job.JobComparatorByFloorDescending());
			Collections.sort(nextDirectionJobs, new Job.JobComparatorByFloorAscending());
			Collections.sort(thirdDirectionJobs, new Job.JobComparatorByFloorDescending());
		}
		//Save number of jobs in each direction (Used to calulate travel distance)
		currentDirectionJobsCount = currentDirectionJobs.size();
		nextDirectionJobsCount = nextDirectionJobs.size();
		thirdDirectionJobsCount = thirdDirectionJobs.size();

		//Add the three lists of jobs to the same list and return it
		currentDirectionJobs.addAll(nextDirectionJobs);
		currentDirectionJobs.addAll(thirdDirectionJobs);
		return currentDirectionJobs;
	}

	//toggleStop - Used by main Controller to toggle stop on this elevator
	public void toggleStop() {
		try {
			if (isStopped) {						
				if (direction == DIRECTION_UP) {
					elevator.up();
				}
				else if (direction == DIRECTION_DOWN) {
					elevator.down();
				}
				isStopped = false;
			}
			else {
				elevator.stop();
				isStopped = true;
			}
		}
		catch (Exception e) {
			e.printStackTrace();
		}
	}

}
