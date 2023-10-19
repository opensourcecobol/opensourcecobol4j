import com.github.jengelman.gradle.plugins.shadow.tasks.ShadowJar

plugins {
    application
    id("com.github.johnrengelman.shadow") version "8.1.1"
    id("java")
    id("com.github.sherter.google-java-format") version "0.9"
    id("maven-publish")
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

publishing {
    /*publications {
        create<MavenPublication>("mavenJava") {
            artifactId = "my-library"
            from(components["java"])
            //versionMapping {
            //    usage("java-api") {
            //        fromResolutionOf("runtimeClasspath")
            //    }
            //    usage("java-runtime") {
            //        fromResolutionResult()
            //    }
            //}
            //pom {
            //    name = "My Library"
            //    description = "A concise description of my library"
            //    url = "http://www.example.com/library"
            //    properties = mapOf(
            //        "myProp" to "value",
            //        "prop.with.dots" to "anotherValue"
            //    )
            //}
        }
    }*/
    repositories {
      maven {
        url = uri("https://maven.pkg.github.com/yutaro-sakamoto/opensourcecobol4j")
        credentials {
          username = System.getenv("GITHUB_ACTOR")
          password = System.getenv("GITHUB_TOKEN")
        }
      }
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