# Build a Spring Boot project using Visual Studio Code
1. Install the "Spring Boot Extension Pack", a Visual Studio Code extension. 
 ![alt text](../image/doc1.png)

1. Install the "Java Extension Pack", a Visual Studio Code extension.  
![alt text](../image/doc2.png)

1. Select "Explorer" in the left sidebar and click "Create Java Project".  
![alt text](../image/doc3.png)

1. Select Spring Boot as the project type.  
![alt text](../image/doc4.png)

1. Select the build tool from Maven or Gradle.

1. Select Spring Boot version.

1. Select Java as the language.  
![alt text](../image/doc5.png)

1. Enter any Group ID. In this step, enter "com.example".  
![alt text](../image/doc6.png)

1. Enter any Artifact ID. In this step, enter "cobj_api_test".  
![alt text](../image/doc7.png)

1. Select the packaging type from jar or war.
1. Select the Java version.

1. Select the dependent libraries. In this step, only Spring Web is selected, but other libraries can be selected as needed.  
![alt text](../image/doc8.png)

1. Select the directory to create the project in and press enter.  
The project will be created according to the above steps.  
![alt text](../image/doc9.png)

# Create an API using Spring Boot/cobj-api
1. Prepare a COBOL program with a `LINKAGE SECTION`. In this procedure, the following sample program will be used.

    sample.cbl
    ``` cobol
           IDENTIFICATION          DIVISION.
           PROGRAM-ID.             sample.
           ENVIRONMENT             DIVISION.
           CONFIGURATION           SECTION.
           DATA                    DIVISION.
           WORKING-STORAGE         SECTION.
           LINKAGE                 SECTION.
           01  DATA1               PIC 9(09).
           01  DATA2               PIC 9(09).
           01  DATA3               PIC 9(09).
           01  SUM-DATA            PIC 9(09).
           PROCEDURE               DIVISION
                                   USING  DATA1,
                                          DATA2,
                                          DATA3,
                                          SUM-DATA.
           MAIN-SECTION.
               ADD DATA1 DATA2 DATA3 TO SUM-DATA.
           GOBACK.

    ```

2. Generate a JSON file from the COBOL program using `cobj` command.
     ```
     cobj -info-json-dir=./ -java-package=com.example.cobj_api_test sample.cbl
     ```
   * `-info-json-dir`：Option to generate a JSON file from the COBOL program. Specify the destination directory after "=".
   * `-java-package`：Option to specify the package name of the generated Java file. Specify the package name after "=".
    
    This will generate sample.java and info_sample.json.

    info_sample.json describes the variables defined in the "LINKAGE SECTION" and their type names.
    ```
    {
      "opensourcecobol4j_version": "1.1.1",
      "program_id": "sample",
      "procedure_division_using_parameters": [
        {
          "variable_name": "DATA1",
          "java_type": "int"
        },
        {
          "variable_name": "DATA2",
          "java_type": "int"
        },
        {
          "variable_name": "DATA3",
          "java_type": "int"
        },
        {
          "variable_name": "SUM-DATA",
          "java_type": "int"
        }
      ]
    }    
    ```
3. Generate Spring Boot Java files from the JSON file using `cobj-api` command.
   ```
    cobj-api -java-package=com.example.cobj_api_test info_sample.json
    ```
   * `-java-package`: An option to specify the package name of the generated Java file.

    This will generate sampleController.java and sampleRecord.java.
4. Place sample.java, sampleController.java, and sampleRecord.java in the created Spring Boot project.   
In this procedure, they will be placed in "src/main/java/com/example/cobj_api_test".

    File structure
    ```
    .
    ├── HELP.md
    ├── mvnw
    ├── mvnw.cmd
    ├── pom.xml
    ├── src
    │   ├── main
    │   │   ├── java
    │   │   │   └── com
    │   │   │       └── example
    │   │   │           └── cobj_api_test
    │   │   │               ├── CobjApiTestApplication.java
    │   │   │               ├── sample.java
    │   │   │               ├── sampleController.java
    │   │   │               └── sampleRecord.java
    │   │   └── resources
    │   └── test
    └── target
    ```


5. Place libcobj.jar anywhere in the project.   

    File structure
    ```
    .
    ├── HELP.md
    |── lib
    |   └── libcobj.jar
    ├── mvnw
    ├── mvnw.cmd
    ├── pom.xml
    ├── src
    │   ├── main
    │   │   ├── java
    │   │   │   └── com
    │   │   │       └── example
    │   │   │           └── cobj_api_test
    │   │   │               ├── CobjApiTestApplication.java
    │   │   │               ├── sample.java
    │   │   │               ├── sampleController.java
    │   │   │               └── sampleRecord.java
    │   │   └── resources
    │   └── test
    └── target
    ```
    * libcobj.jar is a jar file that is generated under "opensourcecobol4j/libcobj/app/build/lib/" when opensource COBOL 4J is installed.

6. 依存関係の追加
* For Gradle   
  Modify build.gradle as follows.
   ```
    dependencies {
        implementation files('lib/libcobj.jar')
        implementation('org.springframework.boot:spring-boot-starter-web') {
        exclude group: 'org.springframework.boot', module: 'spring-boot-starter-logging' 
        }
    }
    ```
    * "spring-boot-starter-web" is a dependency that has been added by default in the previous steps.
    * The provider included in "spring-boot-starter-web" conflicts with the provider included in libocbj.jar, so it needs to be excluded with `exclude group`.
* For Maven   
  Add the following dependency to pom.xml.
    ```
    <dependency>
        	<groupId>libcobj</groupId>
			<artifactId>libcobj</artifactId>
			<version>1.1.0(version of cobj)</version>
        	<scope>system</scope>
        	<systemPath>${basedir}/lib/libcobj.jar</systemPath>
    </dependency>
    ```
7. Build and launch the Spring Boot project.   
   Run the following command in the root directory of the project.
* For Gradle
    ```
    ./gradlew bootRun
    ``` 
* For Maven
   ```
   mvn spring-boot:run
   ```

8. You can call the API by accessing `http://localhost:8080/<PROGRAM-ID>?<data name 1>=<data value 1>&<data name 2>=<data value 2>&...` in your browser.

   In the case of the sample program used this time, if you access `http://localhost:8080/sample?DATA1=1&DATA2=2&DATA3=3`, you will get the following JSON:
   ```
   {"statuscode":200,"DATA1":1,"DATA2":2,"DATA3":3,"SUM_DATA":6}
   ```

