package jp.osscons.opensourcecobol.libcobj.ui;

public class CobolResultString extends CobolCallResult {
  private String value;

  public CobolResultString(String s) {
    this.value = s;
  }

  public String getString() throws CobolResultSetException {
    return this.value;
  }
}
