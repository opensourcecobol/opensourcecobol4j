package jp.osscons.opensourcecobol.libcobj.ui;

public class CobolCallResult {
    public int getInt() throws CobolResultSetException {
        throw new CobolResultSetException("The result type is not an integer");
    }
    public double getDouble() throws CobolResultSetException {
        throw new CobolResultSetException("The result type is not a double");
    }
    public String getString() throws CobolResultSetException {
        throw new CobolResultSetException("The result type is not a String");
    }
}