package jp.osscons.opensourcecobol.libcobj.ui;

public class CobolResultInt extends CobolCallResult {
  private int value;

  public CobolResultInt(int i) {
    this.value = i;
  }

  public int getInt() throws CobolResultSetException {
    return this.value;
  }
}
