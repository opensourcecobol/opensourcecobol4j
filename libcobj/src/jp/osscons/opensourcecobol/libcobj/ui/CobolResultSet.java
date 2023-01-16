package jp.osscons.opensourcecobol.libcobj.ui;

public class CobolResultSet {
  private CobolCallResult results[];
  private int returnCode;

  public CobolResultSet(int returnCode, CobolCallResult... results) {
    this.returnCode = returnCode;
    this.results = results;
  }

  private void checkIndexInValidRange(int index) throws CobolResultSetException {
    if (results.length == 0 || index < 1 || this.results.length < index) {
      throw new CobolResultSetException("The index is out of range.");
    }
  }

  public int getReturnCode() {
    return this.returnCode;
  }

  public String getString(int index) throws CobolResultSetException {
    this.checkIndexInValidRange(index);
    return this.results[index - 1].getString();
  }

  public int getInt(int index) throws CobolResultSetException {
    this.checkIndexInValidRange(index);
    return this.results[index - 1].getInt();
  }

  public double getDouble(int index) throws CobolResultSetException {
    this.checkIndexInValidRange(index);
    return this.results[index - 1].getDouble();
  }
}
