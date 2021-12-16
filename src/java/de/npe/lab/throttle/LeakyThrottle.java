package de.npe.lab.throttle;

import java.util.concurrent.TimeUnit;
import java.util.function.Supplier;

/**
 * Decrements the used up amount continuously over time.
 * This means there is now fixed time window which resets the amount.
 * Instead, the entire amount can be exhausted in a burst,
 * and then can continue in the throttled manner.
 */
public final class LeakyThrottle implements Throttle {
	private final long amount;
	private final long decTime;

	private long start;
	private long used;

	public LeakyThrottle(long amount, long time, TimeUnit unit) {
		this.amount = amount;
		decTime = unit.toNanos(time) / amount;
		start = System.nanoTime();
	}

	public <T> T fetch(Supplier<T> supplier) {
		var now = System.nanoTime();
		var decrements = (now - start) / decTime;
		used = decrements > used ? 0 : used - decrements;
		start += decrements * decTime;
		if (used >= amount) {
			return null;
		}
		used++;
		return supplier.get();
	}
}
