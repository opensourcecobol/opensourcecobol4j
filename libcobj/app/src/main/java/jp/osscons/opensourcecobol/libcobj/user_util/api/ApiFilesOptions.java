package jp.osscons.opensourcecobol.libcobj.user_util.api;

import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.DefaultParser;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;

public class ApiFilesOptions {
  public static String package_name;
  public static String output_dir;

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
    } else if (cmd.getOptionValue("java-package") != null) {
      String package_name = cmd.getOptionValue("java-package");
      setJavaPackage(package_name);
      return;
    } else if (cmd.getOptionValue("output-dir") != null) {
      String output_dir = cmd.getOptionValue("output-dir");
      setOutputDir(output_dir);
      return;
    }
  }

  private static void printHelpMessage() {
    System.out.println("Usage: cobj-api [options] <json-file>");
    System.out.println("Options:");
    System.out.println("  -h, --help\t\t\t\tDisplay this message");
    System.out.println(
        "  -java-package=<package name>\t\tSpecify the package name of the generated source code");
    System.out.println(
        "  -o=<dir>, --output-dir=<dir>\t\tSet the output destination of the java file to an arbitrary destination");
  }

  public static void setJavaPackage(String package_name) {
    ApiFilesOptions.package_name = package_name;
  }

  public static void setOutputDir(String output_dir) {
    ApiFilesOptions.output_dir = output_dir;
  }
}
