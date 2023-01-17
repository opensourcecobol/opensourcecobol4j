import jp.osscons.opensourcecobol.libcobj.ui.*;

public class a {
	public static void main(String[] args) {
		try {
			sub s = new sub();
			CobolResultSet rs = s.execute("hello", 123);
			System.out.println("Argument 1: "+ rs.getString(1));
			System.out.println("Argument 2: "+ rs.getInt(2));
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
}
