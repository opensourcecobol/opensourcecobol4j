import com.github.jengelman.gradle.plugins.shadow.tasks.ShadowJar

plugins {
    application
    id("com.github.johnrengelman.shadow") version "8.1.1"
    id("com.diffplug.spotless") version "7.2.1"
    id("java")
    id("maven-publish")
    pmd
    id("com.github.spotbugs") version "6.4.5"
    jacoco
}

repositories {
    mavenCentral()
}

tasks {
    javadoc {
        options.encoding = "UTF-8"
        options {
            // It seems that the following line does not work.
            (this as CoreJavadocOptions).addStringOption("Xdoclint:missing")
        }
    }
    compileJava {
        options.encoding = "UTF-8"
    }
    compileTestJava {
        options.encoding = "UTF-8"
    }
}

dependencies {
    implementation("com.google.guava:guava:33.5.0-jre")
    implementation("org.xerial:sqlite-jdbc:3.51.0.0")
    implementation("commons-cli:commons-cli:1.10.0")
    testImplementation("org.junit.jupiter:junit-jupiter:5.14.4")
    testRuntimeOnly("org.junit.platform:junit-platform-launcher")
    testImplementation("org.testcontainers:testcontainers:1.21.0")
    testImplementation("org.testcontainers:junit-jupiter:1.21.0")
    testImplementation("org.testcontainers:postgresql:1.21.0")
    implementation("org.json:json:20250517")
    spotbugs("com.github.spotbugs:spotbugs:4.10.4")

    implementation("org.slf4j:slf4j-api:2.0.17")
    runtimeOnly("org.slf4j:slf4j-simple:2.0.17")
    testImplementation("com.github.valfirst:slf4j-test:3.0.1")
    implementation("org.postgresql:postgresql:42.7.5")
}

configurations {
    testRuntimeOnly {
        exclude(group = "org.slf4j", module = "slf4j-simple")
    }
}

java {
    sourceCompatibility = JavaVersion.VERSION_1_8
    targetCompatibility = JavaVersion.VERSION_1_8
}

pmd {
    isConsoleOutput = true
    ruleSets = listOf()
    ruleSetFiles = files("${rootDir}/config/pmdRuleSet.xml")
}


spotbugs {
    excludeFilter.set(project.file("${rootDir}/config/spotbugsFilter.xml"))
}

spotless {
  java {
    googleJavaFormat("1.17.0").aosp().reflowLongStrings().skipJavadocFormatting()
  }
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
            version = "2.1.0"
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
    mergeServiceFiles()
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
    finalizedBy(tasks.jacocoTestReport)
}

tasks.jacocoTestReport {
    dependsOn(tasks.test)
    reports {
        xml.required.set(true)
        html.required.set(true)
    }
}

tasks.jacocoTestCoverageVerification {
    dependsOn(tasks.jacocoTestReport)
    violationRules {
        rule {
            element = "PACKAGE"
            includes = listOf("jp.osscons.opensourcecobol.libcobj.sql")
            limit {
                counter = "BRANCH"
                value = "COVEREDRATIO"
                minimum = "0.80".toBigDecimal()
            }
        }
    }
}

tasks.named("check") {
    dependsOn(tasks.jacocoTestCoverageVerification)
}

