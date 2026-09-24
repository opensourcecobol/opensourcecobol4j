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

/** TODO: 準備中 */
class FileIO {

    private FileChannel fc;
    private FileLock fl = null;
    private boolean useStdOut = true;
    private boolean useStdIn = true;
    private boolean atEnd = false;

    private int readBufferSize = 0;
    private int readBufferIndex = 0;
    private int readBufferEndIndex = 0;
    private byte[] readBuffer;
    private IOException readException = null;

    private int writeBufferSize = 0;
    private int writeBufferEndIndex = 0;
    private byte[] writeBuffer;

    /** TODO: 準備中 */
    FileIO() {
        this.useStdOut = true;
        this.useStdIn = true;
    }

    /**
     * TODO: 準備中
     *
     * @return TODO: 準備中
     */
    boolean isAtEnd() {
        return this.atEnd;
    }

    /**
     * TODO: 準備中
     *
     * @param fc TODO: 準備中
     * @param fl TODO: 準備中
     */
    void setChannel(FileChannel fc, FileLock fl) {
        this.fc = fc;
        this.fl = fl;
        this.useStdOut = false;
        this.useStdIn = false;
        this.atEnd = false;
        // 開き直した場合に前回の読み込みバッファの状態が残らないようにする
        this.destroyReadBuffer();
    }

    /**
     * TODO: 準備中
     *
     * @param ra TODO: 準備中
     * @param fl TODO: 準備中
     */
    void setRandomAccessFile(RandomAccessFile ra, FileLock fl) {
        this.useStdOut = false;
        this.useStdIn = false;
        this.fc = ra.getChannel();
        this.fl = fl;
        this.atEnd = false;
        // 開き直した場合に前回の読み込みバッファの状態が残らないようにする
        this.destroyReadBuffer();
    }

    /**
     * TODO: 準備中
     *
     * @param out TODO: 準備中
     */
    void setOut(PrintStream out) {
        this.useStdOut = true;
    }

    /**
     * TODO: 準備中
     *
     * @param in TODO: 準備中
     */
    void setIn(InputStream in) {
        this.useStdIn = true;
    }

    /**
     * TODO: 準備中
     *
     * @param bufferSize TODO: 準備中
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
     * 読み込みバッファを有効にする。<br>
     * 以降のread/getcは、ファイルからbufferSizeバイトずつまとめて読み込んだ内容を返す。
     *
     * @param bufferSize 読み込みバッファのバイト数。0以下の場合はバッファを有効にしない
     */
    void prepareReadBuffer(int bufferSize) {
        if (bufferSize > 0) {
            this.readBufferSize = bufferSize;
            this.readBufferIndex = 0;
            this.readBufferEndIndex = 0;
            this.readException = null;
            if (this.readBuffer == null || this.readBuffer.length < bufferSize) {
                this.readBuffer = new byte[bufferSize];
            }
        }
    }

    private void destroyReadBuffer() {
        this.readBufferSize = 0;
        this.readBufferIndex = 0;
        this.readBufferEndIndex = 0;
        this.readException = null;
        // 大きなバッファを確保したまま常駐させない
        this.readBuffer = null;
    }

    /** 読み込みバッファ内の未読バイト数を返す。 */
    private int unreadBufferedBytes() {
        return this.readBufferEndIndex - this.readBufferIndex;
    }

    /**
     * 読み込みバッファに未読データがあることを保証する。<br>
     * 未読データが残っていない場合はファイルからまとめて読み込む。
     *
     * @return 読み込みバッファ内の未読バイト数。ファイル終端またはエラーの場合は0
     */
    private int fillReadBuffer() {
        int remaining = this.unreadBufferedBytes();
        if (remaining > 0) {
            return remaining;
        }
        this.readBufferIndex = 0;
        this.readBufferEndIndex = 0;
        try {
            ByteBuffer bb = ByteBuffer.wrap(this.readBuffer, 0, this.readBufferSize);
            int readBytes = this.fc.read(bb);
            if (readBytes <= 0) {
                return 0;
            }
            this.readBufferEndIndex = readBytes;
            return readBytes;
        } catch (IOException e) {
            this.readException = e;
            return 0;
        } catch (NonReadableChannelException e) {
            this.readException = new IOException(e);
            return 0;
        }
    }

    /**
     * 読み込みバッファ経由で最大sizeバイトを読み込む。<br>
     * bytesとstorageはどちらか一方のみを指定する。
     *
     * @param bytes 読み込み先のバイト配列(未使用の場合はnull)
     * @param storage 読み込み先のデータストレージ(未使用の場合はnull)
     * @param size 読み込むバイト数
     * @return 実際に読み込んだバイト数
     */
    private int readWithBuffer(byte[] bytes, CobolDataStorage storage, int size) {
        this.readException = null;
        int readSize = 0;
        while (readSize < size) {
            int remaining = this.fillReadBuffer();
            if (remaining <= 0) {
                break;
            }
            int n = Math.min(remaining, size - readSize);
            if (bytes != null) {
                System.arraycopy(this.readBuffer, this.readBufferIndex, bytes, readSize, n);
            } else {
                System.arraycopy(
                        this.readBuffer,
                        this.readBufferIndex,
                        storage.getRefOfData(),
                        storage.getIndex() + readSize,
                        n);
            }
            this.readBufferIndex += n;
            readSize += n;
        }
        return readSize;
    }

    /**
     * TODO: 準備中
     *
     * @param bytes TODO: 準備中
     * @param size TODO: 準備中
     * @return TODO: 準備中
     */
    int read(byte[] bytes, int size) {
        if (useStdIn) {
            // 標準入力を使う
            System.err.println("read stdin not implmented");
            return 0;
        } else if (this.readBufferSize > 0) {
            int readSize = this.readWithBuffer(bytes, null, size);
            if (this.readException != null) {
                // バッファ無効時と同様に、読み込みエラーはファイル終端と区別する
                return 0;
            }
            if (readSize <= 0) {
                this.atEnd = true;
                return 0;
            }
            this.atEnd = false;
        } else {
            int readSize;
            ByteBuffer data = ByteBuffer.wrap(bytes, 0, size);
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
     * TODO: 準備中
     *
     * @param storage TODO: 準備中
     * @param size TODO: 準備中
     * @return TODO: 準備中
     * @throws IOException TODO: 準備中
     */
    int read(CobolDataStorage storage, int size) throws IOException {
        if (useStdIn) {
            return 0;
        } else {
            if (this.fc == null) {
                throw new IOException();
            }
            if (this.readBufferSize > 0) {
                int readSize = this.readWithBuffer(null, storage, size);
                if (this.readException != null) {
                    throw this.readException;
                }
                return readSize;
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
     * TODO: 準備中
     *
     * @param bytes TODO: 準備中
     * @param size TODO: 準備中
     * @return TODO: 準備中
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
     * TODO: 準備中
     *
     * @param storage TODO: 準備中
     * @param size TODO: 準備中
     * @return TODO: 準備中
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
     * TODO: 準備中
     *
     * @param val TODO: 準備中
     * @return TODO: 準備中
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
     * TODO: 準備中
     *
     * @return TODO: 準備中
     */
    int getc() {
        if (this.fc == null) {
            return 0;
        }
        if (this.readBufferSize > 0) {
            // getcは戻り値でファイル終端と読み込みエラーを区別できないため、
            // バッファ無効時と同様にどちらも-1を返す(readExceptionは次のreadに持ち越さない)
            this.readException = null;
            if (this.fillReadBuffer() <= 0) {
                return -1;
            }
            return this.readBuffer[this.readBufferIndex++];
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

    /** TODO: 準備中 */
    void close() {
        if (!useStdOut && !useStdIn && this.fc != null) {
            try {
                outputWriteBuffer();
                destroyWriteBuffer();
                destroyReadBuffer();
                this.fc.close();
            } catch (IOException e) {
                return;
            }
        }
    }

    /** TODO: 準備中 */
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

    /** TODO: 準備中 */
    static final int SEEK_SET = 0;

    /** TODO: 準備中 */
    static final int SEEK_CUR = 1;

    /**
     * TODO: 準備中
     *
     * <p>現状このメソッドの呼び出し元はREWRITE(OPEN I-O)だけであり、読み込みバッファはOPEN INPUTのときしか<br>
     * 有効にならないため、読み込みバッファを考慮していない。将来OPEN I-Oでも読み込みバッファを使うようにする<br>
     * 場合は、SEEK_CURの基準となるファイルチャネルの位置がバッファ内の未読データの分だけ論理的な現在位置より<br>
     * 先に進んでいることと、位置の変更後はバッファ内の未読データが無効になることに注意が必要。
     *
     * @param offset TODO: 準備中
     * @param origin TODO: 準備中
     * @return TODO: 準備中
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

    /** TODO: 準備中 */
    void seekInit() {}

    /**
     * TODO: 準備中
     *
     * <p>現状このメソッドの呼び出し元はCobolFileSortの作業ファイルだけであり、そこでは読み込みバッファを<br>
     * 有効にしないため、読み込みバッファを考慮していない。将来これらのファイルでも読み込みバッファを使う<br>
     * ようにする場合は、位置の変更後はバッファ内の未読データが無効になることに注意が必要。
     */
    void rewind() {
        if (!useStdOut && !useStdIn) {
            try {
                this.fc.position(0L);
            } catch (IOException e) {
                return;
            }
        }
    }

    /** TODO: 準備中 */
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
