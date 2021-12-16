package de.npe.lab.throttle;

import java.util.function.Supplier;

public interface Throttle {
	<T> T fetch(Supplier<T> s);
}
