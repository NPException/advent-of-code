package de.npe.lab;

public final class Main {
	public static void main(String[] args) {
		var pr = SomeClass.createPrivateRecord();
		SomeClass.printContent(pr);
	}
}
