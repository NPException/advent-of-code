package de.npe.lab;

public final class SomeClass {
	private record PrivateRecord(String content) {}

	public static PrivateRecord createPrivateRecord() {
		return new PrivateRecord("Hello World!");
	}

	public static void printContent(PrivateRecord pr) {
		System.out.println(pr.content());
	}
}
