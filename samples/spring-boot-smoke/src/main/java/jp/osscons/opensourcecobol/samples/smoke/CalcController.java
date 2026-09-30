package jp.osscons.opensourcecobol.samples.smoke;

import java.nio.charset.StandardCharsets;
import jp.osscons.opensourcecobol.libcobj.common.CobolRunUnit;
import jp.osscons.opensourcecobol.libcobj.data.CobolDataStorage;
import jp.osscons.opensourcecobol.samples.smoke.cobol.calc;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

/**
 * Calls the COBOL program "calc" on the servlet request thread.
 *
 * <p>Each request thread keeps its own instance of the program, so the WORKING-STORAGE of "calc"
 * and of the programs it CALLs is private to the thread: this is the same model as one COBOL run
 * unit per thread.
 */
@RestController
public class CalcController {

    private static final ThreadLocal<calc> program = ThreadLocal.withInitial(calc::new);

    @GetMapping("/calc")
    public String calc(
            @RequestParam(name = "num", defaultValue = "1") int num,
            @RequestParam(name = "txt", defaultValue = "banana") String txt) {
        CobolDataStorage request = new CobolDataStorage(16);
        request.memcpy(String.format("%06d%-10s", num, txt), 16);
        CobolDataStorage response = new CobolDataStorage(100);
        response.memset((byte) ' ', 100);
        program.get().run(request, response);
        return new String(response.getByteArray(0, 100), StandardCharsets.US_ASCII).trim()
                + " thread=" + Thread.currentThread().getName() + "\n";
    }

    /** Ends the COBOL run unit of the calling thread (releases files, CALL cache, ...). */
    @GetMapping("/end")
    public String end() {
        program.remove();
        CobolRunUnit.end();
        return "ended\n";
    }
}
