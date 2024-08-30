package jp.osscons.opensourcecobol.libcobj.user_util.cobj_api;

import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.DefaultParser;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;

/** cobj-apiコマンドのオプションを定義するクラス */
class ApiFilesOptions {
  /** cobj-apiコマンドによって生成されるJavaファイルに記述されるパッケージ名 */
  static String packageName;
  /** cobj-apiコマンドによって生成されるJavaファイルが配置されるディレクトリ名 */
  static String outputDir;

  /**
   * 入力されたオプションを取得する
   *
   * @param args コマンドラインから入力された文字列
   */
  static void getOptions(String[] args) {
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
      System.out.println("1.1.2");
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

  /** cobj-apiコマンドのヘルプメッセージを出力する */
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

  /**
   * 生成されるJavaファイルに記述されるパッケージ名を設定する
   *
   * @param packageName 生成されるJavaファイルに記述されるパッケージ名
   */
  static void setJavaPackage(String packageName) {
    ApiFilesOptions.packageName = packageName;
  }

  /**
   * 生成されるJavaファイルが配置されるディレクトリ名を設定する
   *
   * @param outputDir 生成されるJavaファイルが配置されるディレクトリ名
   */
  static void setOutputDir(String outputDir) {
    ApiFilesOptions.outputDir = outputDir;
  }
}
