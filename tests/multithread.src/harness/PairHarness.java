import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import jp.osscons.opensourcecobol.libcobj.call.CobolRunnable;
import jp.osscons.opensourcecobol.libcobj.common.CobolRunUnit;

/**
 * Runs one generated COBOL program per thread, all at once, and waits for them to finish.
 *
 * <p>Usage: java PairHarness PROGRAM1 PROGRAM2 ...
 *
 * <p>The programs synchronise themselves with CALL "SyncPoint" and DISPLAY their own observations, so
 * the output order is deterministic as long as each program prefixes its lines with its name.
 * Lines are printed sorted after every thread has finished.
 */
public class PairHarness {
    public static void main(String[] args) throws Exception {
        SyncPoint.setParties(args.length);
        ThreadHarness.ThreadLocalOut capture = new ThreadHarness.ThreadLocalOut(System.out);
        java.io.PrintStream realOut = System.out;
        System.setOut(new java.io.PrintStream(capture, true, "UTF-8"));
        ExecutorService executor = Executors.newFixedThreadPool(args.length);
        List<Future<String>> futures = new ArrayList<>();
        for (final String program : args) {
            futures.add(
                    executor.submit(
                            new Callable<String>() {
                                @Override
                                public String call() throws Exception {
                                    capture.begin();
                                    try {
                                        CobolRunnable prog =
                                                (CobolRunnable)
                                                        Class.forName(program)
                                                                .getConstructor()
                                                                .newInstance();
                                        int rc = prog.run();
                                        System.out.println(program + " return-code " + rc);
                                    } catch (Throwable e) {
                                        System.out.println(program + " EXCEPTION " + e);
                                        e.printStackTrace(realOut);
                                    } finally {
                                        CobolRunUnit.end();
                                    }
                                    return capture.end();
                                }
                            }));
        }
        List<String> lines = new ArrayList<>();
        for (Future<String> f : futures) {
            for (String line : f.get(300, TimeUnit.SECONDS).split("\r?\n")) {
                if (!line.isEmpty()) {
                    lines.add(line);
                }
            }
        }
        executor.shutdown();
        System.setOut(realOut);
        java.util.Collections.sort(lines);
        for (String line : lines) {
            realOut.print(line + "\n");
        }
        realOut.print("alive\n");
    }
}
