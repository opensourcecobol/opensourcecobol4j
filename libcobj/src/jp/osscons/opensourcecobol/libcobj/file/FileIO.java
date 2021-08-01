package jp.osscons.opensourcecobol.libcobj.file;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintStream;
import java.nio.ByteBuffer;
import java.nio.channels.Channels;
import java.nio.channels.ClosedChannelException;
import java.nio.channels.FileChannel;
import java.nio.channels.FileLock;
import java.nio.channels.NonReadableChannelException;
import java.nio.channels.NonWritableChannelException;

import jp.osscons.opensourcecobol.libcobj.data.CobolDataStorage;

public class FileIO {

	private FileChannel fc;
	private FileLock fl = null;
	private PrintStream out = System.out;
	private InputStream in = System.in;
	private boolean useStdOut = true;
	private boolean useStdIn = true;
	private BufferedInputStream bis;
	private BufferedOutputStream bos;
	private boolean atEnd = false;

	private final static boolean USE_STD_BUFFER = false;
	private final static int STD_DEFAULT_BUFFER_SIZE = 1024;

	private final static boolean USE_READ_BUFFER = false;
	private final static int READ_BUFFER_SIZE = 1024;
	private int readBufferIndex;
	private byte[] readBuffer;
	private int readBufferEndIndex;

	public FileIO() {
		this.out = System.out;
		this.in = System.in;
		this.useStdOut = true;
		this.useStdIn = true;

		this.readBufferIndex = READ_BUFFER_SIZE;
		this.readBuffer = new byte[READ_BUFFER_SIZE];
		this.readBufferEndIndex = READ_BUFFER_SIZE;
	}
	
	public boolean isAtEnd() {
		return this.atEnd;
	}

	public void setChannel(FileChannel fc, FileLock fl) {
		this.fc = fc;
		this.useStdOut = false;
		this.useStdIn = false;

		if(USE_STD_BUFFER) {
			this.bis = new BufferedInputStream(Channels.newInputStream(this.fc));
			this.bos = new BufferedOutputStream(Channels.newOutputStream(this.fc));
		}
	}

	public void setOut(PrintStream out) {
		this.out = out;
		this.useStdOut = true;
	}

	public void setIn(InputStream in) {
		this.in = in;
		this.useStdIn = true;
	}

	public int read(byte[] bytes, int size) {
		if(useStdIn) {
			//標準入力を使う
			System.err.println("read stdin not implmented");
			return 0;
		} else {
			int readSize;
			if(USE_STD_BUFFER) {
				try {
					readSize = this.bis.read(bytes, 0, size);					
				} catch(ClosedChannelException e) {
					return 0;
				} catch (IOException e) {
					return 0;
				} catch(NonReadableChannelException e) {
					return 0;
				}
			} else {
				ByteBuffer data = ByteBuffer.wrap(bytes);
				try {
					readSize = this.fc.read(data);
				} catch(ClosedChannelException e) {
					return 0;
				} catch (IOException e) {
					return 0;
				} catch(NonReadableChannelException e) {
					return 0;
				}
			}
			
			this.atEnd = readSize == -1;
			if(readSize == -1) {
				return 0;
			}
		}
		return 1;
	}

	public int read(CobolDataStorage storage, int size) throws IOException {
		if(useStdIn) {
			return 0;
		} else {
			if(USE_STD_BUFFER) {
				int i = 0;
				try {
					byte[] b = new byte[1];
					for(i=0; i<size; ++i) {
						if(this.bis.read(b, 0, 1) != 1) {
							return i;
						}
						storage.setByte(i, b[0]);
					}
				} catch(ClosedChannelException e) {
					return i;
				} catch (IOException e) {
					return i;
				} catch(NonReadableChannelException e) {
					return i;
				}
				return size;
			} else {
				if(this.fc == null) {
					throw new IOException();
				}
				int i = 0;
				try {
					for(i=0; i<size; ++i) {
						byte[] b = new byte[1];
						ByteBuffer bb = ByteBuffer.wrap(b);
						if(this.fc.read(bb) != 1) {
							return i;
						}
						storage.setByte(i, b[0]);
					}
				} catch(ClosedChannelException e) {
					throw new IOException();
				} catch (IOException e) {
					throw new IOException();
				} catch(NonReadableChannelException e) {
					throw new IOException();
				}
				return size;
			}
		}
	}

	public void write(char c) {
		this.write(String.valueOf(c).getBytes());
	}

	public int write(byte[] bytes) {
		if(useStdOut) {
			//標準を使う
		} else {
			//通常のファイル書き込み
		}
		return 0;
	}

	public int write(byte[] bytes, int size, int n) {
		if(this.fc == null) {
			return 0;
		} else {
			if(USE_STD_BUFFER) {
				int i=0;
				try {
					for(i=0; i<n; ++i) {
						this.bos.write(bytes, 0, size);
					}
				} catch(ClosedChannelException e) {
					return i;
				} catch (IOException e) {
					return i;
				} catch(NonWritableChannelException e) {
					return i;
				}
				return i;

			} else {
				byte[] data = new byte[size];
				System.arraycopy(bytes, 0, data, 0, size);
				ByteBuffer bb = ByteBuffer.wrap(data);
				int i=0;
				try {
					for(i=0; i<n; ++i) {
						this.fc.write(bb);
					}
				} catch(ClosedChannelException e) {
					return i;
				} catch (IOException e) {
					return i;
				} catch(NonWritableChannelException e) {
					return i;
				}
				return i;
			}
		}
	}

	public int write(CobolDataStorage storage, int size, int n) {
		if(this.fc == null) {
			return 0;
		} else {
			if(USE_STD_BUFFER) {
				byte[] data = storage.getByteArray(0, size);
				int i=0;
				try {
					for(i=0; i<n; ++i) {
						this.bos.write(data, 0, size);
					}
				} catch(ClosedChannelException e) {
					return i;
				} catch (IOException e) {
					return i;
				} catch(NonWritableChannelException e) {
					return i;
				}
				return i;
			} else {
				ByteBuffer data = storage.getByteBuffer(size);
				int i=0;
				try {
					for(i=0; i<n; ++i) {
						this.fc.write(data);
					}
				} catch(ClosedChannelException e) {
					return i;
				} catch (IOException e) {
					return i;
				} catch(NonWritableChannelException e) {
					return i;
				}
				return i;
			}
		}
	}

	public byte putc(byte val) {
		if(this.fc == null) {
			return 0;
		}
		if(USE_STD_BUFFER) {
			byte[] arr = {val};
			try {
				this.bos.write(arr);
			} catch (IOException e) {
				return -1;
			}
			return val;
		} else {
			try {
				byte[] arr = {val};
				this.fc.write(ByteBuffer.wrap(arr));
			}catch(ClosedChannelException e) {
				return -1 ;
			} catch (IOException e) {
				return -1;
			} catch(NonWritableChannelException e) {
				return -1;
			}
			return val;
		}
	}

	public int getc() {
		if(this.fc == null) {
			return 0;
		} else {
			if(USE_STD_BUFFER) {
				try {
					return this.bis.read();
				} catch(IOException e) {
					return -1;
				}
			} else if(USE_READ_BUFFER) {
				if(readBufferIndex >= READ_BUFFER_SIZE) {
					this.readBufferIndex = 0;
					try {
						ByteBuffer bb = ByteBuffer.wrap(readBuffer);
						int readBytes = this.fc.read(bb);
						if(readBytes <= 0) {
							this.readBufferEndIndex = -1;
						} else {
							this.readBufferEndIndex = readBytes;
						}
					} catch(ClosedChannelException e) {
						return -1;
					} catch (IOException e) {
						return -1;
					} catch(NonReadableChannelException e) {
						return -1;
					}
				}

				if(this.readBufferIndex >= this.readBufferEndIndex) {
					return -1;
				}

				return readBuffer[readBufferIndex++];

			} else {
				try {
					byte[] b = new byte[1];
					ByteBuffer bb = ByteBuffer.wrap(b);
					if(this.fc.read(bb) == 1) {
						return b[0];
					} else {
						return -1;
					}
				} catch(ClosedChannelException e) {
					return -1;
				} catch (IOException e) {
					return -1;
				} catch(NonReadableChannelException e) {
					return -1;
				}
			}
		}
	}

	public void close() {
		if(!useStdOut && !useStdIn && this.fc != null) {
			if(USE_STD_BUFFER) {
				try {
					this.bos.flush();
					this.bis.close();
					this.bos.close();
				} catch (IOException e) {
				}
			}
			try {
				this.fc.close();
			} catch(IOException e) {
			}
		}
	}

	public void flush() {
		if(useStdOut) {
		} else {
			try {
				if(USE_STD_BUFFER) {
					this.bos.flush();
				}
				this.fc.force(false);
			} catch(IOException e) {
			}
		}
	}

	public static final int SEEK_SET = 0;
	public static final int SEEK_CUR = 1;
	public boolean seek(long offset, int origin) {
		if(!useStdOut && !useStdIn) {
			try {
				switch(origin) {				
				case FileIO.SEEK_SET:
					this.fc.position(offset);
					break;
				case FileIO.SEEK_CUR:
					this.fc.position(this.fc.position() + offset);
					break;
				default:
					return false;
				}
			} catch(IOException e) {
				return false;
			}
		}
		return true;
	}

	public void seekInit() {
		if(!useStdOut && !useStdIn) {
			/*try {
				this.fc.position(this.fc.position() + 0);
			} catch (IOException e) {
			}*/
		}
	}

	public void rewind() {
		if(!useStdOut && !useStdIn) {
			try {
				this.fc.position(0L);
			} catch (IOException e) {
			}
		}
	}

	public void releaseLock() {
		if((!useStdOut || !useStdIn) && this.fl != null) {
			try {
				this.fl.release();
			} catch (IOException e) {
			}
		}
	}	
}
