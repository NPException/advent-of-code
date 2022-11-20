package de.npe.utils;

public class Box {
	private Object x;

	public Box(Object x) {
		this.x = x;
	}

	public Object get() {
		return x;
	}

	public void set(Object x) {
		this.x = x;
	}
}
