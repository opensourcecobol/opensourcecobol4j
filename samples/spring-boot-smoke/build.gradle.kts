// A minimal Spring Boot application that calls COBOL programs compiled by cobj
// from HTTP request threads. CI starts it and fires concurrent requests at it
// to check that the generated code and libcobj behave in a multi-threaded
// servlet container.
//
// Build (after `make install` of opensource COBOL 4J and a libcobj build):
//   ../../libcobj/gradlew -p . bootJar
// Override the locations with -PcobjBin=... and -PlibcobjJar=...

plugins {
    java
    id("org.springframework.boot") version "3.4.4"
    id("io.spring.dependency-management") version "1.1.7"
}

java {
    sourceCompatibility = JavaVersion.VERSION_17
    targetCompatibility = JavaVersion.VERSION_17
}

repositories {
    mavenCentral()
}

val cobjBin: String = (project.findProperty("cobjBin") as String?) ?: "cobj"
val libcobjJar: String =
    (project.findProperty("libcobjJar") as String?)
        ?: "${rootDir}/../../libcobj/app/build/libs/libcobj.jar"
val generatedCobol = layout.buildDirectory.dir("generated-cobol")

configurations.all {
    // libcobj.jar bundles slf4j-simple as its SLF4J provider, which conflicts with
    // Spring Boot's default Logback setup. Use slf4j-simple for everything.
    exclude(group = "org.springframework.boot", module = "spring-boot-starter-logging")
    exclude(group = "ch.qos.logback")
}

dependencies {
    implementation("org.springframework.boot:spring-boot-starter-web")
    implementation(files(libcobjJar))
    // libcobj's runtime dependencies
    implementation("org.xerial:sqlite-jdbc:3.53.2.1")
    implementation("org.slf4j:slf4j-api:2.0.18")
}

// Compile the COBOL sources with cobj into Java sources
val compileCobol by tasks.registering(Exec::class) {
    val sources = fileTree("cobol") { include("*.cbl") }
    inputs.files(sources)
    outputs.dir(generatedCobol)
    doFirst {
        generatedCobol.get().asFile.mkdirs()
    }
    workingDir = file("cobol")
    environment("CLASSPATH", libcobjJar)
    commandLine(
        listOf(
            cobjBin,
            "-C",
            "-j",
            generatedCobol.get().asFile.absolutePath,
            "-java-package=jp.osscons.opensourcecobol.samples.smoke.cobol"
        ) + sources.files.map { it.name }
    )
}

sourceSets {
    main {
        java {
            srcDir(generatedCobol)
        }
    }
}

tasks.named("compileJava") {
    dependsOn(compileCobol)
}
