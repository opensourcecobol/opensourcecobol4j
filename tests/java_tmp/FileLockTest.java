package test;

import java.io.IOException;
import java.nio.channels.FileChannel;
import java.nio.channels.FileLock;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;

public class FileLockTest {

	public static void main(String[] args) {
		FileChannel f1 = null, f2 = null;
		FileLock l1 = null, l2 = null;
		try {
			f1 = FileChannel.open(Paths.get("lock.txt"), StandardOpenOption.READ);
			l1 = f1.lock(0, Long.MAX_VALUE, true);
			f2 = FileChannel.open(Paths.get("lock.txt"), StandardOpenOption.WRITE);
			l2 = f2.lock(0, Long.MAX_VALUE, false);
		} catch (IOException e) {
			e.printStackTrace();
		} finally {
			try {
				if(l1 != null) {
					l1.release();
				}
				if(l2 != null) {
					l2.release();
				}
				if(f1 != null) {
					f1.close();
				}
				if(f2 != null) {
					f2.close();
				}
			} catch(IOException e) {

			}
		}
	}
}