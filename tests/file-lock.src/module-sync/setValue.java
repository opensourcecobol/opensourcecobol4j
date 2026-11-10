import java.io.UnsupportedEncodingException;
import jp.osscons.opensourcecobol.libcobj.*;
import jp.osscons.opensourcecobol.libcobj.common.*;
import jp.osscons.opensourcecobol.libcobj.data.*;
import jp.osscons.opensourcecobol.libcobj.exceptions.*;
import jp.osscons.opensourcecobol.libcobj.termio.*;
import jp.osscons.opensourcecobol.libcobj.call.*;
import jp.osscons.opensourcecobol.libcobj.file.*;
import jp.osscons.opensourcecobol.libcobj.ui.*;
import java.io.BufferedWriter;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.util.HashMap;
import java.util.Map;
import java.io.FileNotFoundException;

public class setValue implements CobolRunnable {
  private static Map<String, BufferedWriter> namedPipeWriters = new HashMap<>();
  @Override
  public int run(CobolDataStorage... argStorages) {
    AbstractCobolField NamedPipeField = CobolModule.getCurrentModule().cob_procedure_parameters.get(0);
    AbstractCobolField valueField = CobolModule.getCurrentModule().cob_procedure_parameters.get(1);
    String namedPipe = NamedPipeField.getString().trim();
    String value = valueField.getString().trim();

    try(BufferedWriter writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(namedPipe)))){
        writer.write(value);
        writer.close();
    } catch (IOException e) {
        e.printStackTrace();
        return -1;
    }

    return 0;
  }

  @Override
  public void cancel() {
    return;
  }

  @Override
  public boolean isActive() {
    return false;
  }
}