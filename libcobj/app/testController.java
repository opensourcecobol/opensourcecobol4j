package com.example.restservice;
import java.util.concurrent.atomic.AtomicLong;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RestController;
import jp.osscons.opensourcecobol.libcobj.ui.*;

@RestController
@RequestMapping("/test")
public class testController {

    @GetMapping
    public testRecord testController(
        @RequestParam(value = "param1", defaultValue = "0") int param1,
        @RequestParam(value = "param2", defaultValue = "0") double param2,
        @RequestParam(value = "param3", defaultValue = "") String param3
    ) {
        return testExecute(param1, param2, param3);
    }

    @PostMapping
    public testRecord testControllerExecute(@RequestBody testRecord testRecord) {
        return testExecute(testRecord.param1(), testRecord.param2(), testRecord.param3());
    }

    private testRecord testExecute(int param1, double param2, String param3) {
        int statuscode = 200;
        test testCobol = new test();
        CobolResultSet result = testCobol.execute(param1, param2, param3);
        try {
            param1 = result.getInt(1);
            param2 = result.getDouble(2);
            param3 = result.getString(3);
        } catch (Exception e) {
            statuscode = 500;
        }
        return new testRecord(statuscode, param1, param2, param3);
    }
}
