import com.github.jengelman.gradle.plugins.shadow.tasks.ShadowJar

plugins {
    // Apply the application plugin to add support for building a CLI application in Java.
    application
    id("com.github.johnrengelman.shadow") version "8.1.1"
    id("java")
}

repositories {
    // Use Maven Central for resolving dependencies.
    mavenCentral()
}

dependencies {
    // Use JUnit Jupiter for testing.
    testImplementation("org.junit.jupiter:junit-jupiter:5.9.2")

    testRuntimeOnly("org.junit.platform:junit-platform-launcher")

    // This dependency is used by the application.
    implementation("com.google.guava:guava:31.1-jre")

    implementation("org.xerial:sqlite-jdbc:3.30.1")
}

// Apply a specific Java toolchain to ease working on different environments.
java {
    toolchain {
        languageVersion.set(JavaLanguageVersion.of(11))
    }
}


application {
    // Define the main class for the application.
    mainClass.set("jp.osscons.opensourcecobol.libcobj.MetaData")
    //mainClassName="jp.osscons.opensourcecobol.libcobj.MetaData"
}

tasks.named<Test>("test") {
    // Use JUnit Platform for unit tests.
    useJUnitPlatform()
}

tasks.withType<Jar>().configureEach {
    archiveBaseName.set("libcobj")
}

tasks.withType<ShadowJar> {
    archiveClassifier.set("")
}