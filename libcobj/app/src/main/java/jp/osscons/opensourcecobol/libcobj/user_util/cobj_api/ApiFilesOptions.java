package jp.osscons.opensourcecobol.libcobj.user_util.cobj_api;

import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.DefaultParser;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;

public class ApiFilesOptions {
  public static String packageName;
  public static String outputDir;

  public static void getOptions(String[] args) {
    Options options = new Options();
    options.addOption("h", "help", false, "Prints the help message");
    options.addOption(
        "jp", "java-package", true, "Specify the package name of the generated source code");
    options.addOption(
        "o",
        "output-dir",
        true,
        "Set the output destination of the java file to an arbitrary destination");
    options.addOption("v", "version", false, "Prints the version of the cobj-api");
    CommandLineParser parser = new DefaultParser();
    CommandLine cmd;

    try {
      cmd = parser.parse(options, args);
    } catch (ParseException e) {
      printHelpMessage();
      System.exit(1);
      return;
    }

    if (cmd.hasOption("h")) {
      printHelpMessage();
      System.exit(0);
      return;
    } else if (cmd.hasOption("v")) {
      System.out.println("1.0.22");
      System.exit(0);
      return;
    } else if (cmd.getOptionValue("java-package") != null) {
      String packageName = cmd.getOptionValue("java-package");
      setJavaPackage(packageName);
      return;
    } else if (cmd.getOptionValue("output-dir") != null) {
      String outputDir = cmd.getOptionValue("output-dir");
      setOutputDir(outputDir);
      return;
    }
  }

  private static void printHelpMessage() {
    System.out.println("Usage: cobj-api [options] <json-file>");
    System.out.println();
    System.out.println("Arguments:");
    System.out.println("  <json-file>: a json file generated by cobj with `-info-json-dir` option");
    System.out.println();
    System.out.println("Options:");
    System.out.println("  -h, --help\t\t\t\tDisplay this message");
    System.out.println(
        "  -java-package=<package name>\t\tSpecify the package name of the generated source code");
    System.out.println(
        "  -o=<dir>, --output-dir=<dir>\t\tSet the output destination of the java file to an arbitrary destination");
    System.out.println("  -v, --version\t\t\t\tPrints the version of the cobj-api");
  }

  public static void setJavaPackage(String packageName) {
    ApiFilesOptions.packageName = packageName;
  }

  public static void setOutputDir(String outputDir) {
    ApiFilesOptions.outputDir = outputDir;
  }
}
