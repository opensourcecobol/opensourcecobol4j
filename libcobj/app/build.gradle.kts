import com.github.jengelman.gradle.plugins.shadow.tasks.ShadowJar

plugins {
    application
    id("com.github.johnrengelman.shadow") version "8.1.1"
    id("java")
    id("com.github.sherter.google-java-format") version "0.9"
}

repositories {
    mavenCentral()
}

dependencies {
    implementation("com.google.guava:guava:31.1-jre")

    implementation("org.xerial:sqlite-jdbc:3.30.1")
}

java {
    toolchain {
        languageVersion.set(JavaLanguageVersion.of(11))
    }
}


application {
    mainClass.set("")
}

tasks.withType<Jar>().configureEach {
    archiveBaseName.set("libcobj")
}

tasks.withType<ShadowJar> {
    archiveClassifier.set("")
}