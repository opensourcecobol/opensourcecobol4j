/*
 * Copyright (C) 2021-2022 TOKYO SYSTEM HOUSE Co., Ltd.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation; either version 3.0,
 * or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; see the file COPYING.LIB.  If
 * not, write to the Free Software Foundation, 51 Franklin Street, Fifth Floor
 * Boston, MA 02110-1301 USA
 */
package jp.osscons.opensourcecobol.libcobj.file;

import java.io.IOException;
import java.io.InputStream;
import java.io.PrintStream;
import java.io.RandomAccessFile;
import java.nio.ByteBuffer;
import java.nio.channels.FileChannel;
import java.nio.channels.FileLock;
import java.nio.channels.NonReadableChannelException;
import java.nio.channels.NonWritableChannelException;
import jp.osscons.opensourcecobol.libcobj.data.CobolDataStorage;

public class FileIO {

  private FileChannel fc;
  private FileLock fl = null;
  private boolean useStdOut = true;
  private boolean useStdIn = true;
  private boolean atEnd = false;

  private static final boolean USE_READ_BUFFER = false;
  private static final int READ_BUFFER_SIZE = 1024;
  private int readBufferIndex;
  private byte[] readBuffer;
  private int readBufferEndIndex;

  private int writeBufferSize = 0;
  private int writeBufferEndIndex = 0;
  private byte[] writeBuffer;

  public FileIO() {
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
    this.fl = fl;
    this.useStdOut = false;
    this.useStdIn = false;
  }

  public void setRandomAccessFile(RandomAccessFile ra, FileLock fl) {
    this.useStdOut = false;
    this.useStdIn = false;
    this.fc = ra.getChannel();
  }

  public void setOut(PrintStream out) {
    this.useStdOut = true;
  }

  public void setIn(InputStream in) {
    this.useStdIn = true;
  }

  public void prepareWriteBuffer(int bufferSize) {
    if (bufferSize > 0) {
      this.writeBufferSize = bufferSize;
      this.writeBufferEndIndex = 0;
      if (this.writeBuffer == null || this.writeBuffer.length < bufferSize) {
        this.writeBuffer = new byte[bufferSize];
      }
    }
  }

  private void destroyWriteBuffer() {
    this.writeBufferSize = 0;
    this.writeBufferEndIndex = 0;
  }

  public int read(byte[] bytes, int size) {
    if (useStdIn) {
      // 標準入力を使う
      System.err.println("read stdin not implmented");
      return 0;
    } else {
      int readSize;
      ByteBuffer data = ByteBuffer.wrap(bytes);
      try {
        readSize = this.fc.read(data);
      } catch (IOException | NonReadableChannelException e) {
        return 0;
      }

      this.atEnd = readSize == -1;
      if (readSize == -1) {
        return 0;
      }
    }
    return 1;
  }

  public int read(CobolDataStorage storage, int size) throws IOException {
    if (useStdIn) {
      return 0;
    } else {
      if (this.fc == null) {
        throw new IOException();
      }
      int i = 0;
      try {
        for (i = 0; i < size; ++i) {
          byte[] b = new byte[1];
          ByteBuffer bb = ByteBuffer.wrap(b);
          if (this.fc.read(bb) != 1) {
            return i;
          }
          storage.setByte(i, b[0]);
        }
      } catch (IOException | NonReadableChannelException e) {
        throw e;
      }
      return size;
    }
  }

  public void write(char c) {
    this.write(String.valueOf(c).getBytes());
  }

  public int write(byte[] bytes) {
    return 0;
  }

  private boolean writeByteBuffer(ByteBuffer bb) {
    try {
      this.fc.write(bb);
    } catch (IOException | NonWritableChannelException e) {
      return false;
    }
    return true;
  }

  private boolean outputWriteBuffer() {
    if (writeBufferEndIndex > 0 && writeBufferSize > 0) {
      ByteBuffer bb = ByteBuffer.wrap(writeBuffer, 0, writeBufferEndIndex);
      if (!writeByteBuffer(bb)) {
        return false;
      }
      writeBufferEndIndex = 0;
    }
    return true;
  }

  public boolean write(byte[] bytes, int size) {
    if (this.fc == null) {
      return false;
    }
    if (writeBufferSize > 0 && size <= writeBufferSize - writeBufferEndIndex) {
      System.arraycopy(bytes, 0, writeBuffer, writeBufferEndIndex, size);
      writeBufferEndIndex += size;
      return true;
    }
    if (!outputWriteBuffer()) {
      return false;
    }
    if (writeBufferSize > 0 && size <= writeBufferSize - writeBufferEndIndex) {
      System.arraycopy(bytes, 0, writeBuffer, writeBufferEndIndex, size);
      writeBufferEndIndex += size;
      return true;
    }
    ByteBuffer bb = ByteBuffer.wrap(bytes, 0, size);
    return writeByteBuffer(bb);
  }

  public boolean write(CobolDataStorage storage, int size) {
    if (this.fc == null) {
      return false;
    }
    if (writeBufferSize > 0 && size <= writeBufferSize - writeBufferEndIndex) {
      for (int i = 0; i < size; ++i) {
        writeBuffer[writeBufferEndIndex + i] = storage.getByte(i);
      }
      writeBufferEndIndex += size;
      return true;
    }
    if (!outputWriteBuffer()) {
      return false;
    }
    if (writeBufferSize > 0 && size <= writeBufferSize - writeBufferEndIndex) {
      for (int i = 0; i < size; ++i) {
        writeBuffer[writeBufferEndIndex + i] = storage.getByte(i);
      }
      writeBufferEndIndex += size;
      return true;
    }
    ByteBuffer bb = storage.getByteBuffer(size);
    return writeByteBuffer(bb);
  }

  public byte putc(byte val) {
    if (this.fc == null) {
      return 0;
    }
    if (writeBufferSize > 0 && 1 <= writeBufferSize - writeBufferEndIndex) {
      writeBuffer[writeBufferEndIndex++] = val;
      return val;
    }
    if (!outputWriteBuffer()) {
      return -1;
    }
    if (writeBufferSize > 0 && 1 <= writeBufferSize - writeBufferEndIndex) {
      writeBuffer[writeBufferEndIndex++] = val;
      return val;
    }
    byte[] arr = {val};
    if (writeByteBuffer(ByteBuffer.wrap(arr))) {
      return val;
    } else {
      return -1;
    }
  }

  public int getc() {
    if (this.fc == null) {
      return 0;
    }
    if (USE_READ_BUFFER) {
      if (readBufferIndex >= READ_BUFFER_SIZE) {
        this.readBufferIndex = 0;
        try {
          ByteBuffer bb = ByteBuffer.wrap(readBuffer);
          int readBytes = this.fc.read(bb);
          if (readBytes <= 0) {
            this.readBufferEndIndex = -1;
          } else {
            this.readBufferEndIndex = readBytes;
          }
        } catch (IOException | NonReadableChannelException e) {
          return -1;
        }
      }

      if (this.readBufferIndex >= this.readBufferEndIndex) {
        return -1;
      }

      return readBuffer[readBufferIndex++];

    } else {
      try {
        byte[] b = new byte[1];
        ByteBuffer bb = ByteBuffer.wrap(b);
        if (this.fc.read(bb) == 1) {
          return b[0];
        } else {
          return -1;
        }
      } catch (IOException | NonReadableChannelException e) {
        return -1;
      }
    }
  }

  public void close() {
    if (!useStdOut && !useStdIn && this.fc != null) {
      try {
        outputWriteBuffer();
        destroyWriteBuffer();
        this.fc.close();
      } catch (IOException e) {
        return;
      }
    }
  }

  public void flush() {
    if (!useStdOut) {
      try {
        outputWriteBuffer();
        this.fc.force(false);
      } catch (IOException e) {
        return;
      }
    }
  }

  public static final int SEEK_SET = 0;
  public static final int SEEK_CUR = 1;

  public boolean seek(long offset, int origin) {
    if (!useStdOut && !useStdIn) {
      try {
        switch (origin) {
          case FileIO.SEEK_SET:
            this.fc.position(offset);
            break;
          case FileIO.SEEK_CUR:
            this.fc.position(this.fc.position() + offset);
            break;
          default:
            return false;
        }
      } catch (IOException e) {
        return false;
      }
    }
    return true;
  }

  public void seekInit() {}

  public void rewind() {
    if (!useStdOut && !useStdIn) {
      try {
        this.fc.position(0L);
      } catch (IOException e) {
        return;
      }
    }
  }

  public void releaseLock() {
    if ((!useStdOut || !useStdIn) && this.fl != null) {
      try {
        this.fl.release();
      } catch (IOException e) {
        return;
      }
    }
  }
}
