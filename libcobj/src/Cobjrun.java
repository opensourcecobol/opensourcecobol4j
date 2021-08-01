import jp.osscons.opensourcecobol.libcobj.call.CobolResolve;
import jp.osscons.opensourcecobol.libcobj.call.CobolRunnable;
import jp.osscons.opensourcecobol.libcobj.exceptions.CobolCallException;

/**
 * コマンドライン引数で与えられた名前に対応するプログラムを動的に読み込んで実行する
 */
public class Cobjrun {

	public static void main(String[] args) {
		if(args.length <= 0) {
			return;
		}
		CobolRunnable runnable;
		try {
			runnable = CobolResolve.resolve(args[0]);
			runnable.run();
		} catch (CobolCallException e) {
			e.printStackTrace();
		}
	}
}
