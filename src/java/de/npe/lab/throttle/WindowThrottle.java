package de.npe.lab.throttle;

import java.util.concurrent.TimeUnit;
import java.util.function.Supplier;

/**
 * Only allows x amount of calls per window.
 * A new window starts when the first call is made
 * after the last window ended.
 */
public final class WindowThrottle implements Throttle {
	private final long amount;
	private final long time;

	private long start;
	private long used;

	public WindowThrottle(long amount, long time, TimeUnit unit) {
		this.amount = amount;
		this.time = unit.toNanos(time);
		// ensure that the first call to `fetch` starts a new window
		start = System.nanoTime() - this.time;
	}

	@Override
	public <T> T fetch(Supplier<T> supplier) {
		var now = System.nanoTime();
		if (now - start > time) {
			start = now;
			used = 0;
		}
		if (used >= amount) {
			return null;
		}
		used++;
		return supplier.get();
	}
}
