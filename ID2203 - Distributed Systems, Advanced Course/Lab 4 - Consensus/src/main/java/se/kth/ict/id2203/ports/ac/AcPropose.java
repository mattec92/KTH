package se.kth.ict.id2203.ports.ac;

import se.sics.kompics.Event;

public class AcPropose extends Event
{

	private final int value;


	public AcPropose(int value)
	{
		this.value = value;
	}


	public final int getValue()
	{
		return value;
	}


	@Override
	public String toString()
	{
		return getClass().getSimpleName() + ", value: " + value;
	}

}
