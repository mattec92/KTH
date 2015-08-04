import java.util.Comparator;

public class Job{
	int destinationFloor;
	int direction = 0;

	public Job(int destinationFloor) {
		this.destinationFloor = destinationFloor;
	}
	
	//setDirection - Sets the direction (Only used on floor jobs)
	public void setDirection(int direction) {
		this.direction = direction;
	}

	//Comparator to sort jobs on destination floor ascending
	public static class JobComparatorByFloorAscending implements Comparator<Job> {

		@Override
		public int compare(Job lhs, Job rhs) {
			if (lhs.destinationFloor > rhs.destinationFloor) {
				return 1;
			}
			else if (lhs.destinationFloor < rhs.destinationFloor) {
				return -1;
			}
			else {
				return 0;
			}
		}
	}

	//Comparator to sort jobs on destination floor descending
	public static class JobComparatorByFloorDescending implements Comparator<Job> {

		@Override
		public int compare(Job lhs, Job rhs) {
			if (lhs.destinationFloor < rhs.destinationFloor) {
				return 1;
			}
			else if (lhs.destinationFloor > rhs.destinationFloor) {
				return -1;
			}
			else {
				return 0;
			}
		}
	}

	public String toString() {
		return "Destinationfloor: " + destinationFloor + " Direction: " + direction;
	}
}
