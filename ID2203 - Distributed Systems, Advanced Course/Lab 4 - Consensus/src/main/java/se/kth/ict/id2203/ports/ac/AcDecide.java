package se.kth.ict.id2203.ports.ac;

import se.sics.kompics.Event;

public class AcDecide extends Event {

	private final int value;

	public AcDecide(int value) {
		this.value = value;
	}
	
	public final int getValue() {
		return value;
	}
}
