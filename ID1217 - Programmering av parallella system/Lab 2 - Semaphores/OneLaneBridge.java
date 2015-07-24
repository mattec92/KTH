/*
 * Homework 2
 * Mattias Cederlund
 * mcede@kth.se
 *
 */

import java.util.concurrent.Semaphore;


public class OneLaneBridge {
	static Semaphore delayNorthboundSemaphore;
	static Semaphore delaySouthboundSemaphore;
	static Semaphore mutexSemaphore;

	static int numCarsOnBridgeNorthbound = 0;
	static int numCarsOnBridgeSouthbound = 0;

	static int numCarsDelayedNorthbound = 0;
	static int numCarsDelayedSouthbound = 0;
	
	final static int NUM_CARS = 10;

	public static void main(String[] arg) {
		delayNorthboundSemaphore = new Semaphore(0, true);
		delaySouthboundSemaphore = new Semaphore(0, true);
		mutexSemaphore = new Semaphore(1, true);

		//Start NUM_CARS car processes
		for (int i = 0; i < NUM_CARS; i++) {
			new Car("" + i, 5).start();
		}
	}
	
	static class Car extends Thread {
		boolean goingSouth;
		int numTrips;

		public Car(String name, int numTrips) {
			super(name);
			//If car number is even, it will start south. Uneven numbers will start north.
			goingSouth = ((Integer.parseInt(name) % 2 == 0) ? true : false);
			this.numTrips = numTrips;
		}

		@Override
		public void run() {
			try {
				while (numTrips > 0) {
					//If car is going southbound
					if (goingSouth) {
						//Aquire the mutex semaphore
						mutexSemaphore.acquire();
						//If there are any cars on the bridge northbound, delay the southbound car
						if (numCarsOnBridgeNorthbound > 0) {
							numCarsDelayedSouthbound++;
							mutexSemaphore.release();
							delaySouthboundSemaphore.acquire();
						}
						numCarsOnBridgeSouthbound++;
						//If there are any more cars delayed southbound, release one more car delayed southbound
						if (numCarsDelayedSouthbound > 0) {
							numCarsDelayedSouthbound--;
							delaySouthboundSemaphore.release();
						}
						//The last delayed car will release the mutex semaphore
						else {
							mutexSemaphore.release();
						}
						System.out.println("Car no " + getName() + " entered the bridge southbound |-->");
						Thread.sleep((int) (Math.random() * 500)); //Go across bridge
						System.out.println("Car no " + getName() + " exited  the bridge southbound -->|");
						mutexSemaphore.acquire();
						numCarsOnBridgeSouthbound--;
						//If there are no cars on the bridge southbound, release the delayed northbound cars if any
						if (numCarsOnBridgeSouthbound == 0 && numCarsDelayedNorthbound > 0) {
							numCarsDelayedNorthbound--; 
							delayNorthboundSemaphore.release();
						}
						//If there are no more delayed cars northbound, release the mutex semaphore
						else {
							mutexSemaphore.release();
						}
						goingSouth = false;
					}
					//If car is going northbound
					else {
						mutexSemaphore.acquire();
						//If there are any cars on the bridge southbound, delay the northbound car
						if (numCarsOnBridgeSouthbound > 0) {
							numCarsDelayedNorthbound++;
							mutexSemaphore.release();
							delayNorthboundSemaphore.acquire();
						}
						numCarsOnBridgeNorthbound++;
						//If there are any more cars delayed north, release one more car delayed northbound
						if (numCarsDelayedNorthbound > 0) {
							numCarsDelayedNorthbound--;
							delayNorthboundSemaphore.release();
						}
						//The last delayed car will release the mutex semaphore
						else {
							mutexSemaphore.release();
						}
						System.out.println("Car no " + getName() + " entered the bridge northbound <--|");
						Thread.sleep((int) (Math.random() * 500)); //Go across bridge
						System.out.println("Car no " + getName() + " exited  the bridge northbound |<--");
						mutexSemaphore.acquire();
						numCarsOnBridgeNorthbound--;
						//If there are no cars on the bridge southbound, release the delayed southbound cars if any
						if (numCarsOnBridgeNorthbound == 0 && numCarsDelayedSouthbound > 0) {
							numCarsDelayedSouthbound--;
							delaySouthboundSemaphore.release();
						}
						//If there are no more delayed cars southbound, release the mutex semaphore
						else {
							mutexSemaphore.release();
						}
						goingSouth = true;
					}
					Thread.sleep((int) (Math.random() * 1000)); //Wait to turn around
					numTrips--;
				}
			}
			catch (InterruptedException e) {
				e.printStackTrace();
			}
		}
	}
}

