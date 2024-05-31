import com.github.jengelman.gradle.plugins.shadow.tasks.ShadowJar

plugins {
    application
    id("com.github.johnrengelman.shadow") version "8.1.1"
    id("java")
    id("com.github.sherter.google-java-format") version "0.9"
    id("maven-publish")
    pmd
    id("com.github.spotbugs") version "6.0.0-rc.2"
}

repositories {
    mavenCentral()
}

tasks {
    javadoc {
        options.encoding = "UTF-8"
    }
    compileJava {
        options.encoding = "UTF-8"
    }
    compileTestJava {
        options.encoding = "UTF-8"
    }
}

dependencies {
    implementation("com.google.guava:guava:31.1-jre")
    implementation("org.xerial:sqlite-jdbc:3.30.1")
    implementation("commons-cli:commons-cli:1.6.0")
    testImplementation("org.junit.jupiter:junit-jupiter:5.9.3")
    testRuntimeOnly("org.junit.platform:junit-platform-launcher")
    implementation("org.json:json:20231013")
    spotbugs("com.github.spotbugs:spotbugs:4.8.0")
}

java {
    toolchain {
        languageVersion.set(JavaLanguageVersion.of(8))
    }
}

pmd {
    isConsoleOutput = true
    ruleSets = listOf()
    ruleSetFiles = files("${rootDir}/config/pmdRuleSet.xml")
}

spotbugs {
    excludeFilter.set(project.file("${rootDir}/config/spotbugsFilter.xml"))
}

publishing {
    repositories {
        maven {
            name = "GitHubPackages"
            url = uri("https://maven.pkg.github.com/opensourcecobol/opensourcecobol4j")
            credentials {
                username = project.findProperty("gpr.user") as String? ?: System.getenv("GITHUB_ACTOR")
                password = project.findProperty("gpr.key") as String? ?: System.getenv("GITHUB_TOKEN")
            }
        }
    }
    publications {
        register<MavenPublication>("gpr") {
            groupId = "jp.osscons.opensourcecobol"
            artifactId = "libcobj"
            version = "1.1.0"
            from(components["java"])
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

tasks.test {
	useJUnitPlatform()
	testLogging {
		events("passed", "skipped", "failed")
	}
}

tasks.named<Test>("test") {
    // Use JUnit Platform for unit tests.
    useJUnitPlatform()
}

