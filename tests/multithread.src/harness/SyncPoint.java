import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.ReentrantLock;
import jp.osscons.opensourcecobol.libcobj.call.CobolRunnable;
import jp.osscons.opensourcecobol.libcobj.common.CobolModule;
import jp.osscons.opensourcecobol.libcobj.data.AbstractCobolField;
import jp.osscons.opensourcecobol.libcobj.data.CobolDataStorage;

/**
 * CALL "SyncPoint" USING STEP: a rendezvous between COBOL programs running on different threads of
 * one JVM.
 *
 * <p>The harness runs the threads in lock step: a program calls SyncPoint with the number of the
 * step it has just completed, and the call returns once every other thread has reported the same
 * step. This makes the interleaving of OPEN/READ/CLOSE between the threads deterministic.
 */
public class SyncPoint implements CobolRunnable {

    private static final ReentrantLock lock = new ReentrantLock();
    private static final Condition changed = lock.newCondition();
    private static int parties = 2;
    private static int currentStep = 0;
    private static int arrived = 0;

    /** Number of threads that take part in each step (set by the harness). */
    public static void setParties(int n) {
        lock.lock();
        try {
            parties = n;
            currentStep = 0;
            arrived = 0;
        } finally {
            lock.unlock();
        }
    }

    @Override
    public int run(CobolDataStorage... storages) {
        AbstractCobolField stepField = CobolModule.getCurrentModule().cob_procedure_parameters.get(0);
        int step = Integer.parseInt(stepField.getString().trim());
        lock.lock();
        try {
            if (step != currentStep + 1) {
                System.out.println("SyncPoint: unexpected step " + step);
                return 1;
            }
            arrived++;
            if (arrived == parties) {
                currentStep = step;
                arrived = 0;
                changed.signalAll();
                return 0;
            }
            while (currentStep < step) {
                if (!changed.await(60, TimeUnit.SECONDS)) {
                    throw new IllegalStateException(new TimeoutException("SyncPoint timed out"));
                }
            }
            return 0;
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            return 1;
        } finally {
            lock.unlock();
        }
    }

    @Override
    public void cancel() {}

    @Override
    public boolean isActive() {
        return false;
    }
}
