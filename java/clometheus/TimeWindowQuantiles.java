package clometheus;

// Copied from https://raw.githubusercontent.com/prometheus/client_java/master/simpleclient/src/main/java/io/prometheus/client/TimeWindowQuantiles.java
// Revision 0275972817f602b5dac2636aae3e7abe137455a6 from Apr 28, 2017 with small modifications.

import java.util.concurrent.TimeUnit;

/**
 * Wrapper around CKMSQuantiles.
 * <p>
 * Maintains a ring buffer of CKMSQuantiles to provide quantiles over a sliding windows of time.
 */
public class TimeWindowQuantiles {

    public final CKMSQuantiles.Quantile[] quantiles;
    private final CKMSQuantiles[] ringBuffer;
    private int currentBucket;
    private long lastRotateTimestampMillis;
    private final long durationBetweenRotatesMillis;

    public TimeWindowQuantiles(CKMSQuantiles.Quantile[] quantiles, long maxAgeSeconds, int ageBuckets) {
        this.quantiles = quantiles;
        this.ringBuffer = new CKMSQuantiles[ageBuckets];
        for (int i = 0; i < ageBuckets; i++) {
            this.ringBuffer[i] = new CKMSQuantiles(quantiles);
        }
        this.currentBucket = 0;
        this.lastRotateTimestampMillis = System.currentTimeMillis();
        this.durationBetweenRotatesMillis = TimeUnit.SECONDS.toMillis(maxAgeSeconds) / ageBuckets;
    }

    public double get(double q) {
        CKMSQuantiles currentBucket = rotate();
        return currentBucket.get(q);
    }

    public void insert(double value) {
        rotate();
        for (CKMSQuantiles ckmsQuantiles : ringBuffer) {
            ckmsQuantiles.insert(value);
        }
    }

    private synchronized CKMSQuantiles rotate() {
        long timeSinceLastRotateMillis = System.currentTimeMillis() - lastRotateTimestampMillis;
        while (timeSinceLastRotateMillis > durationBetweenRotatesMillis) {
            ringBuffer[currentBucket] = new CKMSQuantiles(quantiles);
            if (++currentBucket >= ringBuffer.length) {
                currentBucket = 0;
            }
            timeSinceLastRotateMillis -= durationBetweenRotatesMillis;
            lastRotateTimestampMillis += durationBetweenRotatesMillis;
        }
        return ringBuffer[currentBucket];
    }
}
