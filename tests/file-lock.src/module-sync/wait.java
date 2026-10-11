import java.io.UnsupportedEncodingException;
import jp.osscons.opensourcecobol.libcobj.*;
import jp.osscons.opensourcecobol.libcobj.common.*;
import jp.osscons.opensourcecobol.libcobj.data.*;
import jp.osscons.opensourcecobol.libcobj.exceptions.*;
import jp.osscons.opensourcecobol.libcobj.termio.*;
import jp.osscons.opensourcecobol.libcobj.call.*;
import jp.osscons.opensourcecobol.libcobj.file.*;
import jp.osscons.opensourcecobol.libcobj.ui.*;
import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.HashMap;
import java.util.Map;
import java.io.FileNotFoundException;

public class wait implements CobolRunnable {
  private static Map<String, BufferedReader> namedPipeReaders = new HashMap<>();
  @Override
  public int run(CobolDataStorage... argStorages) {
    AbstractCobolField namedPipeField = CobolModule.getCurrentModule().cob_procedure_parameters.get(0);
    AbstractCobolField valueField = CobolModule.getCurrentModule().cob_procedure_parameters.get(1);
    String namedPipe = namedPipeField.getString().trim();
    String value = valueField.getString().trim();

    while(true) {
        try(BufferedReader reader = new BufferedReader(new InputStreamReader(new FileInputStream(namedPipe)))) {

            String line;
            while ((line = reader.readLine()) != null) {
                if (line.equals(value)) {
                    return 0;
                } else {
                    Thread.sleep(200);
                    continue;
                }
            }
        } catch (FileNotFoundException e) {
            try {
                Thread.sleep(200);
            } catch (InterruptedException ie) {
                ie.printStackTrace();
                Thread.currentThread().interrupt();
                return -1;
            }
            continue;
        } catch (IOException e) {
            e.printStackTrace();
            return -1;
        } catch (InterruptedException e) {
            e.printStackTrace();
            Thread.currentThread().interrupt();
            return -1;
        }
    }
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