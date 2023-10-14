package jp.osscons.opensourcecobol.libcobj.ui;

public class CobolResultDouble extends CobolCallResult {
  private double value;

  public CobolResultDouble(double d) {
    this.value = d;
  }

  public double getDouble() throws CobolResultSetException {
    return this.value;
  }
}
