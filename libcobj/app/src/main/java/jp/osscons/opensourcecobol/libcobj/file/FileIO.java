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

/** FileChannelや標準入出力をラップし,COBOLファイルの低水準な読み書き・バッファ処理・ロックを担うクラス. */
class FileIO {

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

    /** 標準入出力を利用する状態でインスタンスを生成し,読み込みバッファを初期化する. */
    FileIO() {
        this.useStdOut = true;
        this.useStdIn = true;

        this.readBufferIndex = READ_BUFFER_SIZE;
        this.readBuffer = new byte[READ_BUFFER_SIZE];
        this.readBufferEndIndex = READ_BUFFER_SIZE;
    }

    /**
     * 直前の読み込みでファイル終端に達したかどうかを返す.
     *
     * @return ファイル終端に達している場合はtrue.
     */
    boolean isAtEnd() {
        return this.atEnd;
    }

    /**
     * 読み書き対象のFileChannelとファイルロックを設定し,標準入出力の使用を無効にする.
     *
     * @param fc 読み書きに使用するFileChannel.
     * @param fl ファイルに対して取得したロック(不要な場合はnull).
     */
    void setChannel(FileChannel fc, FileLock fl) {
        this.fc = fc;
        this.fl = fl;
        this.useStdOut = false;
        this.useStdIn = false;
    }

    /**
     * RandomAccessFileから得たFileChannelとファイルロックを設定し,標準入出力の使用を無効にする.
     *
     * @param ra 読み書きに使用するRandomAccessFile.
     * @param fl ファイルに対して取得したロック(不要な場合はnull).
     */
    void setRandomAccessFile(RandomAccessFile ra, FileLock fl) {
        this.useStdOut = false;
        this.useStdIn = false;
        this.fc = ra.getChannel();
        this.fl = fl;
    }

    /**
     * 標準出力を使用する状態に設定する.
     *
     * @param out 出力先のPrintStream.
     */
    void setOut(PrintStream out) {
        this.useStdOut = true;
    }

    /**
     * 標準入力を使用する状態に設定する.
     *
     * @param in 入力元のInputStream.
     */
    void setIn(InputStream in) {
        this.useStdIn = true;
    }

    /**
     * 指定したサイズの書き込みバッファを確保し初期化する.
     *
     * @param bufferSize 確保する書き込みバッファのサイズ(バイト数).
     */
    void prepareWriteBuffer(int bufferSize) {
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

    /**
     * FileChannelからbytesにデータを読み込む(標準入力使用時は未実装).
     *
     * @param bytes 読み込んだデータを格納するバイト配列.
     * @param size 読み込むバイト数.
     * @return 読み込みに成功した場合は1,ファイル終端または失敗の場合は0.
     */
    int read(byte[] bytes, int size) {
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

    /**
     * FileChannelから1バイトずつsizeバイトをstorageに読み込む.
     *
     * @param storage 読み込んだデータを格納する領域.
     * @param size 読み込むバイト数.
     * @return 実際に読み込めたバイト数.
     * @throws IOException FileChannelが未設定の場合,または読み込みに失敗した場合.
     */
    int read(CobolDataStorage storage, int size) throws IOException {
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

    /**
     * bytesの先頭sizeバイトをファイルに書き込む.
     * 書き込みバッファに空きがあればバッファに蓄積し,空きがなければバッファを出力してから書き込む.
     *
     * @param bytes 書き込むデータを格納したバイト配列.
     * @param size 書き込むバイト数.
     * @return 書き込みに成功した場合はtrue.
     */
    boolean write(byte[] bytes, int size) {
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

    /**
     * storageの先頭sizeバイトをファイルに書き込む.
     * 書き込みバッファに空きがあればバッファに蓄積し,空きがなければバッファを出力してから書き込む.
     *
     * @param storage 書き込むデータを格納した領域.
     * @param size 書き込むバイト数.
     * @return 書き込みに成功した場合はtrue.
     */
    boolean write(CobolDataStorage storage, int size) {
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

    /**
     * 1バイトをファイルに書き込む.書き込みバッファに空きがあればバッファに蓄積する.
     *
     * @param val 書き込むバイト値.
     * @return 成功した場合は書き込んだバイト値,FileChannelが未設定の場合は0,失敗した場合は-1.
     */
    byte putc(byte val) {
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

    /**
     * ファイルから1バイトを読み込む.
     *
     * @return 読み込んだバイト値,FileChannelが未設定の場合は0,ファイル終端または失敗の場合は-1.
     */
    int getc() {
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

    /** 書き込みバッファの内容を出力してからファイルを閉じる. */
    void close() {
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

    /** 書き込みバッファの内容を出力し,ファイルの内容をディスクへ強制的に反映する. */
    void flush() {
        if (!useStdOut) {
            try {
                outputWriteBuffer();
                this.fc.force(false);
            } catch (IOException e) {
                return;
            }
        }
    }

    /** seekの起点をファイル先頭とすることを表す定数. */
    static final int SEEK_SET = 0;

    /** seekの起点を現在の読み書き位置とすることを表す定数. */
    static final int SEEK_CUR = 1;

    /**
     * 指定した起点からoffsetバイトの位置へファイルの読み書き位置を移動する.
     *
     * @param offset 起点からの移動量(バイト数).
     * @param origin 移動の起点(SEEK_SETまたはSEEK_CUR).
     * @return 移動に成功した場合はtrue.
     */
    boolean seek(long offset, int origin) {
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

    /** シークに関する初期化を行う(現在の実装では何も行わない). */
    void seekInit() {}

    /** ファイルの読み書き位置を先頭に戻す. */
    void rewind() {
        if (!useStdOut && !useStdIn) {
            try {
                this.fc.position(0L);
            } catch (IOException e) {
                return;
            }
        }
    }

    /** ファイルに対して取得したロックを解放する. */
    void releaseLock() {
        if ((!useStdOut || !useStdIn) && this.fl != null) {
            try {
                this.fl.release();
            } catch (IOException e) {
                return;
            }
        }
    }
}
