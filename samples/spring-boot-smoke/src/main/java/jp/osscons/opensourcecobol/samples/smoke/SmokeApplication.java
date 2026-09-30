package jp.osscons.opensourcecobol.samples.smoke;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

/** Spring Boot application that serves COBOL programs compiled by cobj. */
@SpringBootApplication
public class SmokeApplication {
    public static void main(String[] args) {
        SpringApplication.run(SmokeApplication.class, args);
    }
}
