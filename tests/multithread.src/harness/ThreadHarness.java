import java.io.ByteArrayOutputStream;
import java.io.OutputStream;
import java.io.PrintStream;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.concurrent.Callable;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import jp.osscons.opensourcecobol.libcobj.call.CobolRunnable;
import jp.osscons.opensourcecobol.libcobj.common.CobolRunUnit;
import jp.osscons.opensourcecobol.libcobj.data.CobolDataStorage;

/**
 * Runs a generated COBOL program from several threads at once.
 *
 * <p>Usage: java ThreadHarness PROGRAM THREADS ITERATIONS [ARG-SIZE]
 *
 * <p>Every thread creates its own instance of PROGRAM and calls it ITERATIONS times. When ARG-SIZE
 * is given, the thread number (1-based, zero padded to ARG-SIZE digits) is passed as the first
 * argument of the program. Everything the program DISPLAYs is captured per thread. At the end the
 * harness prints every distinct output line with the number of times it appeared across all
 * threads (sorted), the return code of every thread (sorted), and "alive" so that a STOP RUN that
 * killed the JVM would be noticed.
 */
public class ThreadHarness {

    /** A PrintStream whose bytes go to a buffer owned by the writing thread. */
    static final class ThreadLocalOut extends OutputStream {
        private final ThreadLocal<ByteArrayOutputStream> buffers = new ThreadLocal<>();
        private final PrintStream fallback;

        ThreadLocalOut(PrintStream fallback) {
            this.fallback = fallback;
        }

        void begin() {
            buffers.set(new ByteArrayOutputStream());
        }

        String end() {
            ByteArrayOutputStream buf = buffers.get();
            buffers.remove();
            return buf == null ? "" : new String(buf.toByteArray(), StandardCharsets.UTF_8);
        }

        @Override
        public void write(int b) {
            ByteArrayOutputStream buf = buffers.get();
            if (buf == null) {
                fallback.write(b);
            } else {
                buf.write(b);
            }
        }

        @Override
        public void write(byte[] b, int off, int len) {
            ByteArrayOutputStream buf = buffers.get();
            if (buf == null) {
                fallback.write(b, off, len);
            } else {
                buf.write(b, off, len);
            }
        }
    }

    static final class Result {
        final int thread;
        final int returnCode;
        final String output;

        Result(int thread, int returnCode, String output) {
            this.thread = thread;
            this.returnCode = returnCode;
            this.output = output;
        }
    }

    public static void main(String[] args) throws Exception {
        final String program = args[0];
        final int threads = Integer.parseInt(args[1]);
        final int iterations = Integer.parseInt(args[2]);
        final int argSize = args.length > 3 ? Integer.parseInt(args[3]) : 0;

        final PrintStream realOut = System.out;
        final ThreadLocalOut capture = new ThreadLocalOut(realOut);
        System.setOut(new PrintStream(capture, true, "UTF-8"));

        final Class<?> clazz = Class.forName(program);
        final CountDownLatch start = new CountDownLatch(1);
        ExecutorService executor = Executors.newFixedThreadPool(threads);
        List<Future<Result>> futures = new ArrayList<>();
        for (int t = 1; t <= threads; t++) {
            final int threadNo = t;
            futures.add(
                    executor.submit(
                            new Callable<Result>() {
                                @Override
                                public Result call() throws Exception {
                                    start.await();
                                    capture.begin();
                                    int rc = 0;
                                    try {
                                        CobolRunnable prog =
                                                (CobolRunnable)
                                                        clazz.getConstructor().newInstance();
                                        for (int i = 0; i < iterations; i++) {
                                            if (argSize > 0) {
                                                String id =
                                                        String.format("%0" + argSize + "d", threadNo);
                                                CobolDataStorage arg = new CobolDataStorage(argSize);
                                                arg.memcpy(id, argSize);
                                                rc = prog.run(arg);
                                            } else {
                                                rc = prog.run();
                                            }
                                        }
                                    } catch (Throwable e) {
                                        System.out.println("EXCEPTION " + e);
                                        e.printStackTrace(realOut);
                                        rc = -1;
                                    } finally {
                                        CobolRunUnit.end();
                                    }
                                    return new Result(threadNo, rc, capture.end());
                                }
                            }));
        }
        start.countDown();
        List<Result> results = new ArrayList<>();
        for (Future<Result> f : futures) {
            results.add(f.get(300, TimeUnit.SECONDS));
        }
        executor.shutdown();
        System.setOut(realOut);

        Map<String, Integer> lines = new TreeMap<>();
        Map<Integer, Integer> returnCodes = new TreeMap<>();
        for (Result r : results) {
            for (String line : r.output.split("\r?\n")) {
                if (line.isEmpty()) {
                    continue;
                }
                lines.merge(line, 1, Integer::sum);
            }
            returnCodes.merge(r.returnCode, 1, Integer::sum);
        }
        for (Map.Entry<String, Integer> e : lines.entrySet()) {
            realOut.print(e.getValue() + " " + e.getKey() + "\n");
        }
        for (Map.Entry<Integer, Integer> e : returnCodes.entrySet()) {
            realOut.print("return-code " + e.getKey() + " x" + e.getValue() + "\n");
        }
        realOut.print("alive\n");
        realOut.flush();
    }
}
