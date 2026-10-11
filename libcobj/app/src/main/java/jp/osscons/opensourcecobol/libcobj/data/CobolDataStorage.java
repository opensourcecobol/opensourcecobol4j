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
package jp.osscons.opensourcecobol.libcobj.data;

import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.util.Arrays;
import jp.osscons.opensourcecobol.libcobj.common.CobolModule;

/**
 * COBOL変数のデータを保存するバイト配列を扱うクラス<br>
 * byte[]型のデータと,その配列中での相対位置を保持し,データの読み書きを行う
 */
public class CobolDataStorage {

    /** データを保存するバイト配列 */
    private byte[] data;

    /** このクラスの扱うデータが保存する領域のバイト配列中での相対位置 */
    private int index;

    /**
     * 引数で指定された長さ分のバイト配列を確保する.相対位置は0にする.
     *
     * @param size バイト配列の長さ
     */
    public CobolDataStorage(int size) {
        this.data = new byte[size];
        this.index = 0;
    }

    /**
     * バイト配列と相対位置を指定する
     *
     * @param data データを保存するバイト配列
     * @param index バイト配列中の相対位置
     */
    public CobolDataStorage(byte[] data, int index) {
        this.data = data;
        this.index = index;
    }

    /**
     * データを保存するバイト配列を引数で指定する.相対位置は0にする.
     *
     * @param data データを保存するバイト配列
     */
    public CobolDataStorage(byte[] data) {
        this(data, 0);
    }

    /**
     * 指定したバイト列を保持するCobolDataStorageのインスタンスを生成する.
     *
     * @param bytes 保持するバイト列
     * @return 生成したCobolDataStorageのインスタンス
     */
    public static CobolDataStorage makeCobolDataStorage(byte... bytes) {
        return new CobolDataStorage(bytes);
    }

    /** バイト配列を持たない空のインスタンスを生成する.相対位置は0にする. */
    public CobolDataStorage() {
        this.index = 0;
        this.data = null;
    }

    /**
     * 指定したCobolDataStorageのバイト配列への参照と相対位置をこのオブジェクトに設定する.
     *
     * @param data 参照元のCobolDataStorage
     * @param index 設定する相対位置
     */
    public void setDataRefAndIndex(CobolDataStorage data, int index) {
        this.data = data.data;
        this.index = index;
    }

    /**
     * このオブジェクトが保持する相対位置を返す.
     *
     * @return 相対位置
     */
    public int getIndex() {
        return this.index;
    }

    /**
     * このオブジェクトの相対位置を設定する.
     *
     * @param index 設定する相対位置
     */
    public void setIndex(int index) {
        this.index = index;
    }

    /**
     * このオブジェクトが保持するバイト配列への参照を返す.
     *
     * @return バイト配列への参照
     */
    public byte[] getRefOfData() {
        return this.data;
    }

    /**
     * このオブジェクトと同じバイト配列への参照と相対位置を持つ複製を生成する.
     *
     * @return 生成した複製のインスタンス
     */
    public CobolDataStorage copy() {
        CobolDataStorage ret = new CobolDataStorage();
        ret.index = this.index;
        ret.data = this.data;
        return ret;
    }

    /**
     * 相対位置から指定したサイズのByteBufferを生成して返す.
     *
     * @param size ByteBufferのサイズ(バイト数)
     * @return 生成したByteBuffer
     */
    public ByteBuffer getByteBuffer(int size) {
        return ByteBuffer.wrap(this.data, this.index, size);
    }

    /**
     * 相対位置に指定した値を加算する.
     *
     * @param n 加算するバイト数
     */
    public void addIndex(int n) {
        this.index += n;
    }

    /**
     * プリミティブ値をバイト配列に変換して保持するCobolDataStorageのインスタンスを生成する.
     *
     * @param n 変換する値
     * @return 生成したCobolDataStorageのインスタンス
     */
    public static CobolDataStorage primitiveToDataStorage(byte n) {
        byte[] bytes = new byte[1];
        bytes[0] = n;
        return new CobolDataStorage(bytes);
    }

    /**
     * プリミティブ値をバイト配列に変換して保持するCobolDataStorageのインスタンスを生成する.
     *
     * @param n 変換する値
     * @return 生成したCobolDataStorageのインスタンス
     */
    public static CobolDataStorage primitiveToDataStorage(short n) {
        byte[] bytes = new byte[2];
        ByteBuffer buffer = ByteBuffer.wrap(bytes);
        buffer.putShort(n);
        return new CobolDataStorage(bytes);
    }

    /**
     * プリミティブ値をバイト配列に変換して保持するCobolDataStorageのインスタンスを生成する.
     *
     * @param n 変換する値
     * @return 生成したCobolDataStorageのインスタンス
     */
    public static CobolDataStorage primitiveToDataStorage(int n) {
        byte[] bytes = new byte[4];
        ByteBuffer buffer = ByteBuffer.wrap(bytes);
        buffer.putInt(n);
        return new CobolDataStorage(bytes);
    }

    /**
     * プリミティブ値をバイト配列に変換して保持するCobolDataStorageのインスタンスを生成する.
     *
     * @param n 変換する値
     * @return 生成したCobolDataStorageのインスタンス
     */
    public static CobolDataStorage primitiveToDataStorage(long n) {
        byte[] bytes = new byte[8];
        ByteBuffer buffer = ByteBuffer.wrap(bytes);
        buffer.putLong(n);
        return new CobolDataStorage(bytes);
    }

    /**
     * プリミティブ値をバイト配列に変換して保持するCobolDataStorageのインスタンスを生成する.
     *
     * @param s 変換する文字列
     * @return 生成したCobolDataStorageのインスタンス
     */
    public static CobolDataStorage primitiveToDataStorage(String s) {
        return new CobolDataStorage(s);
    }

    /**
     * 引数の文字列をShift_JISでエンコードしたバイト配列を保持するインスタンスを生成する.相対位置は0にする.
     *
     * @param str 保持する文字列
     */
    public CobolDataStorage(String str) {
        try {
            byte[] bytes = str.getBytes(AbstractCobolField.charSetSJIS);
            this.data = bytes;
            this.index = 0;
        } catch (Exception e) {
            e.printStackTrace();
            this.data = new byte[0];
            this.index = 0;
        }
    }

    /**
     * 保持するバイト配列のコピーを返す
     *
     * @return 保持するバイト配列のコピー
     */
    public byte[] getData() {
        return this.getData(0);
    }

    /**
     * 引数で指定した開始位置から末尾までをコピーした配列を返す.
     *
     * @param index このオブジェクトの保持する相対位置を基準とした,コピーの開始位置
     * @return 開始位置から末尾までのデータをコピーしたバイト配列
     */
    public byte[] getData(int index) {
        byte[] result = new byte[this.data.length - this.index - index];

        System.arraycopy(this.data, this.index + index, result, 0, result.length);

        return result;
    }

    /**
     * 引数で指定した相対位置から指定した長さをコピーした配列を返す.
     *
     * @param index このオブジェクトの保持する相対位置を基準とした,コピーの開始位置
     * @param length コピーする長さ(バイト数)
     * @return 開始位置からlengthバイト分の配列
     */
    public byte[] getByteArray(int index, int length) {
        byte[] result = new byte[length];
        System.arraycopy(this.data, this.index + index, result, 0, length);
        return result;
    }

    /**
     * 相対位置から指定した範囲を参照するByteBufferの内部配列を返す.
     *
     * @param index 相対位置からの開始位置
     * @param length 範囲の長さ(バイト数)
     * @return 参照したバイト配列
     */
    public byte[] getByteArrayRef(int index, int length) {
        ByteBuffer buffer = ByteBuffer.wrap(this.data, this.index + index, length);
        return buffer.array();
    }

    /**
     * C言語のmemcpyに相当するメソッド
     *
     * @param buf コピー元のデータ
     * @param size コピーするバイト数
     */
    public void memcpy(CobolDataStorage buf, int size) {
        System.arraycopy(buf.data, buf.index, this.data, this.index, size);
    }

    /**
     * C言語のmemcpyに相当するメソッド
     *
     * @param buf コピー元のデータ
     * @param size コピーするバイト数
     */
    public void memcpy(byte[] buf, int size) {
        System.arraycopy(buf, 0, this.data, this.index, size);
    }

    /**
     * C言語のmemcpyに相当するメソッド
     *
     * @param str コピー元の文字列
     * @param size コピーするバイト数
     */
    public void memcpy(String str, int size) {
        this.memcpy(str.getBytes(AbstractCobolField.charSetSJIS), size);
    }

    /**
     * C言語のmemcpyに相当するメソッド
     *
     * @param offset コピー先のthis.indexからの相対位置
     * @param buf コピー元のデータ
     * @param size コピーするバイト数
     */
    public void memcpy(int offset, byte[] buf, int size) {
        System.arraycopy(buf, 0, this.data, this.index + offset, size);
    }

    /**
     * C言語のmemcpyに相当するメソッド
     *
     * @param offset コピー先のthis.indexからの相対位置
     * @param buf コピー元のデータ
     * @param size コピーするバイト数
     */
    public void memcpy(int offset, CobolDataStorage buf, int size) {
        System.arraycopy(buf.data, buf.index, this.data, this.index + offset, size);
    }

    /**
     * C言語のmemcpyに相当するメソッド
     *
     * @param buf コピー元のデータ
     * @param offset コピー元bufの先頭からの相対位置
     * @param size コピーするバイト数
     */
    public void memcpy(byte[] buf, int offset, int size) {
        System.arraycopy(buf, offset, this.data, this.index, size);
    }

    /**
     * C言語のmemcpyに相当するメソッド
     *
     * @param buf コピー元のデータ
     */
    public void memcpy(byte[] buf) {
        this.memcpy(buf, buf.length);
    }

    /**
     * C言語のmemsetに相当するメソッド
     *
     * @param ch 代入する値
     * @param size 代入先のバイト数
     */
    public void memset(byte ch, int size) {
        Arrays.fill(this.data, this.index, this.index + size, ch);
    }

    /**
     * C言語のmemsetに相当するメソッド
     *
     * @param ch 代入する値
     * @param size 代入先のバイト数
     */
    public void memset(int ch, int size) {
        this.memset((byte) ch, size);
    }

    /**
     * C言語のmemsetに相当するメソッド
     *
     * @param offset 代入先のthis.indexからの相対位置
     * @param ch 代入する値
     * @param size 代入先のバイト数
     */
    public void memset(int offset, byte ch, int size) {
        Arrays.fill(this.data, this.index + offset, this.index + offset + size, ch);
    }

    /**
     * C言語のmemsetに相当するメソッド
     *
     * @param offset 代入先のthis.indexからの相対位置
     * @param ch 代入する値
     * @param size 代入先のバイト数
     */
    public void memset(int offset, int ch, int size) {
        this.memset(offset, (byte) ch, size);
    }

    /**
     * C言語のmemcmpに相当するメソッド
     *
     * @param buf 比較対象のデータ
     * @param size 比較するバイト数
     * @return 先頭から異なるバイトの差(符号なし).すべて等しい場合は0
     */
    public int memcmp(byte[] buf, int size) {
        for (int i = 0; i < size; ++i) {
            if (this.getByte(i) != buf[i]) {
                int x = Byte.toUnsignedInt(this.getByte(i));
                int y = Byte.toUnsignedInt(buf[i]);
                return x - y;
            }
        }
        return 0;
    }

    /**
     * C言語のmemcmpに相当するメソッド
     *
     * @param buf 比較対象のデータ
     * @param size 比較するバイト数
     * @return 先頭から異なるバイトの差(符号なし).すべて等しい場合は0
     */
    public int memcmp(String buf, int size) {
        return this.memcmp(buf.getBytes(AbstractCobolField.charSetSJIS), size);
    }

    /**
     * C言語のmemcmpに相当するメソッド
     *
     * @param buf 比較対象のデータ
     * @param size 比較するバイト数
     * @return 先頭から異なるバイトの差(符号なし).すべて等しい場合は0
     */
    public int memcmp(CobolDataStorage buf, int size) {
        for (int i = 0; i < size; ++i) {
            if (this.getByte(i) != buf.getByte(i)) {
                int x = Byte.toUnsignedInt(this.getByte(i));
                int y = Byte.toUnsignedInt(buf.getByte(i));
                return x - y;
            }
        }
        return 0;
    }

    /**
     * C言語のmemcmpに相当するメソッド
     *
     * @param offset 比較開始のthis.indexからの相対位置
     * @param buf 比較対象のデータ
     * @param size 比較するバイト数
     * @return 先頭から異なるバイトの差(符号なし).すべて等しい場合は0
     */
    public int memcmp(int offset, CobolDataStorage buf, int size) {
        for (int i = 0; i < size; ++i) {
            byte x = this.getByte(offset + i);
            byte y = buf.getByte(i);
            if (x != y) {
                return Byte.toUnsignedInt(x) - Byte.toUnsignedInt(y);
            }
        }
        return 0;
    }

    /**
     * 引数で与えられたバイト配列に保持したデータをこのオブジェクトに書き込む
     *
     * @param data コピー元のバイト配列
     */
    public void setData(byte[] data) {
        setData(data, 0);
    }

    /**
     * 引数で与えられたバイト配列に保持したデータをこのオブジェクトに書き込む
     *
     * @param data コピー元のバイト配列
     * @param index コピー先のバイト配列中での開始位置
     */
    public void setData(byte[] data, int index) {
        int length =
                (data.length <= this.data.length - this.index - index)
                        ? data.length
                        : this.data.length - this.index - index;

        System.arraycopy(data, 0, this.data, this.index + index, length);
    }

    /**
     * 引数で指定された分だけ相対位置を変位させたCobolDataStorageクラスのインスタンスを作成する
     *
     * @param index 変位させるバイト数
     * @return 新たに作成したCobolDataStorageクラスのインスタンス
     */
    public CobolDataStorage getDataStorage(int index) {
        return new CobolDataStorage(this.data, this.index + index);
    }

    /**
     * 指定のバイト配列中での位置に指定の値を代入する
     *
     * @param index 代入先のバイト配列中の位置
     * @param value 代入する値
     */
    public void setByte(int index, byte value) {
        this.data[this.index + index] = value;
    }

    /**
     * 指定のバイト配列中での位置に指定の値を代入する
     *
     * @param index 代入先のバイト配列中の位置
     * @param value 代入する値
     */
    public void setByte(int index, int value) {
        this.setByte(index, (byte) value);
    }

    /**
     * 指定のバイト配列中での位置に指定の値を代入する
     *
     * @param index 代入先のバイト配列中の位置.
     * @param value 代入する値
     */
    public void setByte(int index, char value) {
        this.setByte(index, (byte) value);
    }

    /**
     * 先頭(相対位置0)に指定の値を代入する
     *
     * @param value 代入する値
     */
    public void setByte(byte value) {
        this.setByte(0, value);
    }

    /**
     * 先頭(相対位置0)に指定の値を代入する
     *
     * @param value 代入する値
     */
    public void setByte(int value) {
        this.setByte((byte) value);
    }

    /**
     * 先頭(相対位置0)に指定の値を代入する
     *
     * @param value 代入する値
     */
    public void setByte(char value) {
        this.setByte((byte) value);
    }

    /**
     * 指定のバイト配列中での位置に格納された値を返す
     *
     * @param index バイト配列中での位置.
     * @return 指定のバイト配列中での位置に格納された値
     */
    public byte getByte(int index) {
        return this.data[this.index + index];
    }

    /**
     * バイト配列のthis.indexバイト目からsizeバイトの範囲にvalueを代入する
     *
     * @param value 代入する値
     * @param size 代入先のバイト数
     */
    public void fillBytes(byte value, int size) {
        fillBytes(0, value, size);
    }

    /**
     * バイト配列のthis.indexバイト目からsizeバイトの範囲にvalueを代入する
     *
     * @param value 代入する値
     * @param size 代入先のバイト数
     */
    public void fillBytes(int value, int size) {
        fillBytes(0, value, size);
    }

    /**
     * バイト配列のthis.indexバイト目からsizeバイトの範囲にvalueを代入する
     *
     * @param value 代入する値
     * @param size 代入先のバイト数
     */
    public void fillBytes(char value, int size) {
        fillBytes(0, value, size);
    }

    /**
     * バイト配列の0バイト目からsizeバイトの範囲にvalueを代入する
     *
     * @param index コピーの開始位置
     * @param value 代入する値
     * @param size 代入先のバイト数
     */
    public void fillBytes(int index, byte value, int size) {
        for (int i = 0; i < size; ++i) {
            this.setByte(i + index, value);
        }
    }

    /**
     * バイト配列の0バイト目からsizeバイトの範囲にvalueを代入する
     *
     * @param index コピーの開始位置
     * @param value 代入する値
     * @param size 代入先のバイト数
     */
    public void fillBytes(int index, int value, int size) {
        this.fillBytes(index, (byte) value, size);
    }

    /**
     * バイト配列の0バイト目からsizeバイトの範囲にvalueを代入する
     *
     * @param index コピーの開始位置
     * @param value 代入する値
     * @param size 代入先のバイト数
     */
    public void fillBytes(int index, char value, int size) {
        this.fillBytes(index, (byte) value, size);
    }

    /**
     * 指定したデータをバイト配列にコピーする.
     *
     * @param bytes コピー元のバイト配列
     */
    public void setBytes(byte[] bytes) {
        for (int i = 0; i < bytes.length; ++i) {
            this.setByte(i, bytes[i]);
        }
    }

    /**
     * 指定したバイト配列をコピーし,残りの領域を半角空白で埋める.
     *
     * @param bytes コピー元のバイト配列
     * @param length コピーする最大バイト数
     */
    public void setByByteArrayAndPaddingSpaces(byte[] bytes, int length) {
        int copyLength = Math.min(bytes.length, length);
        int i;
        for (i = 0; i < copyLength; ++i) {
            this.setByte(i, bytes[i]);
        }
        for (; i < length; ++i) {
            this.setByte(i, (byte) ' ');
        }
    }

    /**
     * 指定したデータをバイト配列にコピーする.
     *
     * @param bytes コピー元の配列
     * @param length コピーするバイト数
     */
    public void setBytes(byte[] bytes, int length) {
        for (int i = 0; i < length; ++i) {
            this.setByte(i, bytes[i]);
        }
    }

    /**
     * 指定のバイト配列に格納された値を this.dataのthis.indexバイト目以降へlengthバイトだけコピーする.
     *
     * @param index コピー先のthis.indexからの相対位置
     * @param bytes コピー元の配列
     * @param length コピーするバイト数
     */
    public void setBytes(int index, byte[] bytes, int length) {
        for (int i = 0; i < length; ++i) {
            this.setByte(i + index, bytes[i]);
        }
    }

    /**
     * 指定した文字列をバイト配列にコピーする
     *
     * @param str コピー元の文字列
     * @param length コピーするバイト数
     */
    public void setBytes(String str, int length) {
        try {
            this.setBytes(str.getBytes(AbstractCobolField.charSetSJIS), length);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    /**
     * 指定されたCobolDataStorageのインスタンスから,lengthバイトだけデータをコピーする
     *
     * @param data コピー元のCobolDataStorage
     * @param length コピーするバイト数
     */
    public void setBytes(CobolDataStorage data, int length) {
        this.setBytes(data, length, 0, 0);
    }

    /**
     * 指定されたCobolDataStorageのインスタンスから,lengthバイトだけデータをコピーする
     *
     * @param data コピー元のCobolDataStorage
     * @param length コピーするバイト数
     * @param dstIndex コピー先のthis.indexからの相対位置
     */
    public void setBytes(CobolDataStorage data, int length, int dstIndex) {
        this.setBytes(data, length, dstIndex, 0);
    }

    /**
     * 指定されたCobolDataStorageのインスタンスから,lengthバイトだけデータをコピーする
     *
     * @param data コピー元のCobolDataStorage
     * @param length コピーするバイト数
     * @param dstIndex コピー先のthis.indexからの相対位置
     * @param srcIndex コピー元バイト配列中の相対位置
     */
    public void setBytes(CobolDataStorage data, int length, int dstIndex, int srcIndex) {
        for (int i = 0; i < length; ++i) {
            this.setByte(i + dstIndex, data.getByte(i + srcIndex));
        }
    }

    /**
     * TODO 暫定的な実装
     *
     * @param str 書き込む文字列
     */
    public void setString(String str) {
        this.fillBytes((byte) 0x20, this.data.length);
        try {
            this.setBytes(str.getBytes(AbstractCobolField.charSetSJIS));
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    /**
     * TODO 暫定的な実装(対応未定)
     *
     * @param pointer 設定するポインタ値
     */
    public void setPointer(int pointer) {
        System.err.println("setPointer is not implemented");
    }

    /**
     * 指定した値をこのオブジェクトのバイト配列に書き込む.
     *
     * @param bytes 書き込むバイト配列
     */
    public void set(byte[] bytes) {
        for (int i = 0; i < bytes.length && this.index + i < this.data.length; ++i) {
            this.setByte(i, bytes[i]);
        }
    }

    /**
     * 指定した値をこのオブジェクトのバイト配列に書き込む.
     *
     * @param value 書き込む値
     */
    public void set(byte value) {
        this.setByte(0, value);
    }

    /**
     * 2バイト整数値をこのオブジェクト時書き込む
     *
     * @param value 書き込む値
     */
    public void set(short value) {
        ByteBuffer.wrap(this.data, this.index, 2).putShort(value);
    }

    /**
     * 4バイト整数値をこのオブジェクト時書き込む
     *
     * @param value 書き込む値
     */
    public void set(int value) {
        ByteBuffer.wrap(this.data, this.index, 4).putInt(value);
    }

    /**
     * 8バイト整数値をこのオブジェクト時書き込む
     *
     * @param value 書き込む値
     */
    public void set(long value) {
        ByteBuffer.wrap(this.data, this.index, 8).putLong(value);
    }

    /**
     * double型の値をこのオブジェクト時書き込む
     *
     * @param value 書き込む値
     */
    public void set(double value) {
        ByteBuffer.wrap(this.data, this.index, 8).putDouble(value);
    }

    /**
     * 指定した値をこのオブジェクトのバイト配列に書き込む.
     *
     * @param other 書き込む値を持つCobolDataStorage
     */
    public void set(CobolDataStorage other) {
        this.set(other.intValue());
    }

    /**
     * コピーの開始位置を指定して,4バイト整数値を書き込む
     *
     * @param value 書き込む値
     * @param index 書き込み先のthis.indexからの相対位置
     */
    public void set(int value, int index) {
        ByteBuffer buffer = ByteBuffer.wrap(this.data, this.index + index, 4);
        buffer.putInt(value);
    }

    /**
     * このオブジェクトと引数が同じバイト配列と相対位置を参照しているか判定する.
     *
     * @param other 比較対象のCobolDataStorage
     * @return 同じバイト配列と相対位置を参照していればtrue
     */
    public boolean isSame(CobolDataStorage other) {
        return this.data == other.data && this.index == other.index;
    }

    /**
     * このオブジェクトの保持するバイト配列の先頭2バイトを読み込んでshort型として返す
     *
     * @return 読み込んだ2バイト整数値
     */
    public short shortValue() {
        return ByteBuffer.wrap(this.data, this.index, Short.BYTES).getShort();
    }

    /**
     * このオブジェクトの保持するバイト配列の先頭4バイトを読み込んでint型として返す
     *
     * @return 読み込んだ4バイト整数値
     */
    public int intValue() {
        return ByteBuffer.wrap(this.data, this.index, Integer.BYTES).getInt();
    }

    /**
     * このオブジェクトの保持するバイト配列の先頭4バイトを読み込んでlong型として返す
     *
     * @return 読み込んだ8バイト整数値
     */
    public long longValue() {
        return ByteBuffer.wrap(this.data, this.index, Long.BYTES).getLong();
    }

    /**
     * このオブジェクトの保持するバイト配列の先頭8バイトを読み込んでdouble型として返す
     *
     * @return 読み込んだdouble値
     */
    public double doubleValue() {
        return ByteBuffer.wrap(this.data, this.index, Double.BYTES).getDouble();
    }

    /**
     * 相対位置を加算したCobolDataStorageのインスタンスを返す
     *
     * @param index 相対位置に加算する値
     * @return 相対位置を加算したCobolDataStorageのインスタンス
     */
    public CobolDataStorage getSubDataStorage(int index) {
        return new CobolDataStorage(this.data, this.index + index);
    }

    /**
     * 相対位置を加算したCobolDataStorageのインスタンスを返す
     *
     * @param index 相対位置に加算する値
     * @return 相対位置を加算したCobolDataStorageのインスタンス
     */
    public CobolDataStorage getSubDataStorage(long index) {
        return this.getDataStorage((int) index);
    }

    private long toLong(int numOfBytes, boolean signed, boolean isBigEndian) {
        ByteBuffer buffer = ByteBuffer.wrap(this.data);
        buffer.order(isBigEndian ? ByteOrder.BIG_ENDIAN : ByteOrder.LITTLE_ENDIAN);
        if (numOfBytes == 1) {
            return buffer.get(this.index);
        } else if (numOfBytes == 2) {
            return buffer.getShort(this.index);
        } else if (numOfBytes == 4) {
            return buffer.getInt(this.index);
        } else {
            return buffer.getLong(this.index);
        }
    }

    private interface Cmpr {
        int run(long a, long b);
    }

    private static final Cmpr compareS =
            new Cmpr() {
                @Override
                public int run(long a, long b) {
                    if (a < b) {
                        return -1;
                    } else if (a > b) {
                        return 1;
                    } else {
                        return 0;
                    }
                }
            };

    private static final Cmpr compareU =
            new Cmpr() {
                @Override
                public int run(long a, long b) {
                    return Long.compareUnsigned(a, b);
                }
            };

    /**
     * このオブジェクトが保持するバイナリ整数を引数nと比較する.
     *
     * @param n 比較対象の値
     * @param numOfBytes 読み込むバイト数
     * @param signed 符号付きならtrue
     * @param isBigEndian ビッグエンディアンならtrue
     * @return 保持する値がnより小さい場合は負の値,大きい場合は正の値,等しい場合は0
     */
    public int compareToBinary(long n, int numOfBytes, boolean signed, boolean isBigEndian) {
        long val = this.toLong(numOfBytes, signed, isBigEndian);
        Cmpr comparator = signed ? compareS : compareU;
        return comparator.run(val, n);
    }

    private void fromLong(int numOfBytes, boolean isBigEndian, long n) {
        ByteBuffer buffer = ByteBuffer.wrap(this.data);
        buffer.order(isBigEndian ? ByteOrder.BIG_ENDIAN : ByteOrder.LITTLE_ENDIAN);
        if (numOfBytes <= 1) {
            buffer.put(this.index, (byte) n);
        } else if (numOfBytes <= 2) {
            buffer.putShort(this.index, (short) n);
        } else if (numOfBytes <= 4) {
            buffer.putInt(this.index, (int) n);
        } else if (numOfBytes <= 8) {
            buffer.putLong(this.index, n);
        }
    }

    /**
     * このオブジェクトが保持するバイナリ整数に引数nを加算する.
     *
     * @param n 加算する値
     * @param numOfBytes 対象のバイト数
     * @param signed 符号付きならtrue
     * @param isBigEndian ビッグエンディアンならtrue
     */
    public void addBinary(long n, int numOfBytes, boolean signed, boolean isBigEndian) {
        long x = this.toLong(numOfBytes, signed, isBigEndian);
        x += n;
        this.fromLong(numOfBytes, isBigEndian, x);
    }

    /**
     * このオブジェクトが保持するバイナリ整数から引数nを減算する.
     *
     * @param n 減算する値
     * @param numOfBytes 対象のバイト数
     * @param signed 符号付きならtrue
     * @param isBigEndian ビッグエンディアンならtrue
     */
    public void subBinary(long n, int numOfBytes, boolean signed, boolean isBigEndian) {
        long x = this.toLong(numOfBytes, signed, isBigEndian);
        x -= n;
        this.fromLong(numOfBytes, isBigEndian, x);
    }

    // n numofBytes signed isBigEndian
    /**
     * このオブジェクトが保持する符号なし8ビットバイナリ整数と引数nを比較する.
     *
     * @param n 比較対象の整数値
     * @return 保持する値がnより小さい場合は負の値,大きい場合は正の値,等しい場合は0
     */
    public int cmpU8Binary(long n) {
        return compareToBinary(n, 1, false, true);
    }

    /**
     * このオブジェクトが保持する符号付き8ビットバイナリ整数と引数nを比較する.
     *
     * @param n 比較対象の整数値
     * @return 保持する値がnより小さい場合は負の値,大きい場合は正の値,等しい場合は0
     */
    public int cmpS8Binary(long n) {
        return compareToBinary(n, 1, true, true);
    }

    /**
     * このオブジェクトが保持する符号なし16ビットバイナリ整数と引数nを比較する.
     *
     * @param n 比較対象の整数値
     * @return 保持する値がnより小さい場合は負の値,大きい場合は正の値,等しい場合は0
     */
    public int cmpU16Binary(long n) {
        return compareToBinary(n, 2, false, true);
    }

    /**
     * このオブジェクトが保持する符号付き16ビットバイナリ整数と引数nを比較する.
     *
     * @param n 比較対象の整数値
     * @return 保持する値がnより小さい場合は負の値,大きい場合は正の値,等しい場合は0
     */
    public int cmpS16Binary(long n) {
        return compareToBinary(n, 2, true, true);
    }

    /**
     * このオブジェクトが保持する符号なし24ビットバイナリ整数と引数nを比較する.
     *
     * @param n 比較対象の整数値
     * @return 保持する値がnより小さい場合は負の値,大きい場合は正の値,等しい場合は0
     */
    public int cmpU24Binary(long n) {
        return compareToBinary(n, 3, false, true);
    }

    /**
     * このオブジェクトが保持する符号付き24ビットバイナリ整数と引数nを比較する.
     *
     * @param n 比較対象の整数値
     * @return 保持する値がnより小さい場合は負の値,大きい場合は正の値,等しい場合は0
     */
    public int cmpS24Binary(long n) {
        return compareToBinary(n, 3, true, true);
    }

    /**
     * このオブジェクトが保持する符号なし32ビットバイナリ整数と引数nを比較する.
     *
     * @param n 比較対象の整数値
     * @return 保持する値がnより小さい場合は負の値,大きい場合は正の値,等しい場合は0
     */
    public int cmpU32Binary(long n) {
        return compareToBinary(n, 4, false, true);
    }

    /**
     * このオブジェクトが保持する符号付き32ビットバイナリ整数と引数nを比較する.
     *
     * @param n 比較対象の整数値
     * @return 保持する値がnより小さい場合は負の値,大きい場合は正の値,等しい場合は0
     */
    public int cmpS32Binary(long n) {
        return compareToBinary(n, 4, true, true);
    }

    /**
     * このオブジェクトが保持する符号なし40ビットバイナリ整数と引数nを比較する.
     *
     * @param n 比較対象の整数値
     * @return 保持する値がnより小さい場合は負の値,大きい場合は正の値,等しい場合は0
     */
    public int cmpU40Binary(long n) {
        return compareToBinary(n, 5, false, true);
    }

    /**
     * このオブジェクトが保持する符号付き40ビットバイナリ整数と引数nを比較する.
     *
     * @param n 比較対象の整数値
     * @return 保持する値がnより小さい場合は負の値,大きい場合は正の値,等しい場合は0
     */
    public int cmpS40Binary(long n) {
        return compareToBinary(n, 5, true, true);
    }

    /**
     * このオブジェクトが保持する符号なし48ビットバイナリ整数と引数nを比較する.
     *
     * @param n 比較対象の整数値
     * @return 保持する値がnより小さい場合は負の値,大きい場合は正の値,等しい場合は0
     */
    public int cmpU48Binary(long n) {
        return compareToBinary(n, 6, false, true);
    }

    /**
     * このオブジェクトが保持する符号付き48ビットバイナリ整数と引数nを比較する.
     *
     * @param n 比較対象の整数値
     * @return 保持する値がnより小さい場合は負の値,大きい場合は正の値,等しい場合は0
     */
    public int cmpS48Binary(long n) {
        return compareToBinary(n, 6, true, true);
    }

    /**
     * このオブジェクトが保持する符号なし56ビットバイナリ整数と引数nを比較する.
     *
     * @param n 比較対象の整数値
     * @return 保持する値がnより小さい場合は負の値,大きい場合は正の値,等しい場合は0
     */
    public int cmpU56Binary(long n) {
        return compareToBinary(n, 7, false, true);
    }

    /**
     * このオブジェクトが保持する符号付き56ビットバイナリ整数と引数nを比較する.
     *
     * @param n 比較対象の整数値
     * @return 保持する値がnより小さい場合は負の値,大きい場合は正の値,等しい場合は0
     */
    public int cmpS56Binary(long n) {
        return compareToBinary(n, 7, true, true);
    }

    /**
     * このオブジェクトが保持する符号なし64ビットバイナリ整数と引数nを比較する.
     *
     * @param n 比較対象の整数値
     * @return 保持する値がnより小さい場合は負の値,大きい場合は正の値,等しい場合は0
     */
    public int cmpU64Binary(long n) {
        return compareToBinary(n, 8, false, true);
    }

    /**
     * このオブジェクトが保持する符号付き64ビットバイナリ整数と引数nを比較する.
     *
     * @param n 比較対象の整数値
     * @return 保持する値がnより小さい場合は負の値,大きい場合は正の値,等しい場合は0
     */
    public int cmpS64Binary(long n) {
        return compareToBinary(n, 8, true, true);
    }

    /**
     * このオブジェクトが保持するアライメント済みの符号なし8ビットバイナリ整数と引数nを比較する. (現状の実装ではアライメントの指定は反映されず,対応する非修飾版と同一に動作する)
     *
     * @param n 比較対象の整数値
     * @return 保持する値がnより小さい場合は負の値,大きい場合は正の値,等しい場合は0
     */
    public int cmpAlignU8Binary(long n) {
        return compareToBinary(n, 1, false, true);
    }

    /**
     * このオブジェクトが保持するアライメント済みの符号付き8ビットバイナリ整数と引数nを比較する. (現状の実装ではアライメントの指定は反映されず,対応する非修飾版と同一に動作する)
     *
     * @param n 比較対象の整数値
     * @return 保持する値がnより小さい場合は負の値,大きい場合は正の値,等しい場合は0
     */
    public int cmpAlignS8Binary(long n) {
        return compareToBinary(n, 1, true, true);
    }

    /**
     * このオブジェクトが保持するアライメント済みの符号なし16ビットバイナリ整数と引数nを比較する. (現状の実装ではアライメントの指定は反映されず,対応する非修飾版と同一に動作する)
     *
     * @param n 比較対象の整数値
     * @return 保持する値がnより小さい場合は負の値,大きい場合は正の値,等しい場合は0
     */
    public int cmpAlignU16Binary(long n) {
        return compareToBinary(n, 2, false, true);
    }

    /**
     * このオブジェクトが保持するアライメント済みの符号付き16ビットバイナリ整数と引数nを比較する. (現状の実装ではアライメントの指定は反映されず,対応する非修飾版と同一に動作する)
     *
     * @param n 比較対象の整数値
     * @return 保持する値がnより小さい場合は負の値,大きい場合は正の値,等しい場合は0
     */
    public int cmpAlignS16Binary(long n) {
        return compareToBinary(n, 2, true, true);
    }

    /**
     * このオブジェクトが保持するアライメント済みの符号なし32ビットバイナリ整数と引数nを比較する. (現状の実装ではアライメントの指定は反映されず,対応する非修飾版と同一に動作する)
     *
     * @param n 比較対象の整数値
     * @return 保持する値がnより小さい場合は負の値,大きい場合は正の値,等しい場合は0
     */
    public int cmpAlignU32Binary(long n) {
        return compareToBinary(n, 4, false, true);
    }

    /**
     * このオブジェクトが保持するアライメント済みの符号付き32ビットバイナリ整数と引数nを比較する. (現状の実装ではアライメントの指定は反映されず,対応する非修飾版と同一に動作する)
     *
     * @param n 比較対象の整数値
     * @return 保持する値がnより小さい場合は負の値,大きい場合は正の値,等しい場合は0
     */
    public int cmpAlignS32Binary(long n) {
        return compareToBinary(n, 4, true, true);
    }

    /**
     * このオブジェクトが保持するアライメント済みの符号なし64ビットバイナリ整数と引数nを比較する. (現状の実装ではアライメントの指定は反映されず,対応する非修飾版と同一に動作する)
     *
     * @param n 比較対象の整数値
     * @return 保持する値がnより小さい場合は負の値,大きい場合は正の値,等しい場合は0
     */
    public int cmpAlignU64Binary(long n) {
        return compareToBinary(n, 8, false, true);
    }

    /**
     * このオブジェクトが保持するアライメント済みの符号付き64ビットバイナリ整数と引数nを比較する. (現状の実装ではアライメントの指定は反映されず,対応する非修飾版と同一に動作する)
     *
     * @param n 比較対象の整数値
     * @return 保持する値がnより小さい場合は負の値,大きい場合は正の値,等しい場合は0
     */
    public int cmpAlignS64Binary(long n) {
        return compareToBinary(n, 8, true, true);
    }

    // n numofBytes signed isBigEndian
    /**
     * このオブジェクトが保持する符号なし8ビットバイナリ整数に引数nを加算する.
     *
     * @param n 加算する値
     */
    public void addU8Binary(long n) {
        addBinary(n, 1, false, true);
    }

    /**
     * このオブジェクトが保持する符号付き8ビットバイナリ整数に引数nを加算する.
     *
     * @param n 加算する値
     */
    public void addS8Binary(long n) {
        addBinary(n, 1, true, true);
    }

    /**
     * このオブジェクトが保持する符号なし16ビットバイナリ整数に引数nを加算する.
     *
     * @param n 加算する値
     */
    public void addU16Binary(long n) {
        addBinary(n, 2, false, true);
    }

    /**
     * このオブジェクトが保持する符号付き16ビットバイナリ整数に引数nを加算する.
     *
     * @param n 加算する値
     */
    public void addS16Binary(long n) {
        addBinary(n, 2, true, true);
    }

    /**
     * このオブジェクトが保持する符号なし24ビットバイナリ整数に引数nを加算する.
     *
     * @param n 加算する値
     */
    public void addU24Binary(long n) {
        addBinary(n, 3, false, true);
    }

    /**
     * このオブジェクトが保持する符号付き24ビットバイナリ整数に引数nを加算する.
     *
     * @param n 加算する値
     */
    public void addS24Binary(long n) {
        addBinary(n, 3, true, true);
    }

    /**
     * このオブジェクトが保持する符号なし32ビットバイナリ整数に引数nを加算する.
     *
     * @param n 加算する値
     */
    public void addU32Binary(long n) {
        addBinary(n, 4, false, true);
    }

    /**
     * このオブジェクトが保持する符号付き32ビットバイナリ整数に引数nを加算する.
     *
     * @param n 加算する値
     */
    public void addS32Binary(long n) {
        addBinary(n, 4, true, true);
    }

    /**
     * このオブジェクトが保持する符号なし40ビットバイナリ整数に引数nを加算する.
     *
     * @param n 加算する値
     */
    public void addU40Binary(long n) {
        addBinary(n, 5, false, true);
    }

    /**
     * このオブジェクトが保持する符号付き40ビットバイナリ整数に引数nを加算する.
     *
     * @param n 加算する値
     */
    public void addS40Binary(long n) {
        addBinary(n, 5, true, true);
    }

    /**
     * このオブジェクトが保持する符号なし48ビットバイナリ整数に引数nを加算する.
     *
     * @param n 加算する値
     */
    public void addU48Binary(long n) {
        addBinary(n, 6, false, true);
    }

    /**
     * このオブジェクトが保持する符号付き48ビットバイナリ整数に引数nを加算する.
     *
     * @param n 加算する値
     */
    public void addS48Binary(long n) {
        addBinary(n, 6, true, true);
    }

    /**
     * このオブジェクトが保持する符号なし56ビットバイナリ整数に引数nを加算する.
     *
     * @param n 加算する値
     */
    public void addU56Binary(long n) {
        addBinary(n, 7, false, true);
    }

    /**
     * このオブジェクトが保持する符号付き56ビットバイナリ整数に引数nを加算する.
     *
     * @param n 加算する値
     */
    public void addS56Binary(long n) {
        addBinary(n, 7, true, true);
    }

    /**
     * このオブジェクトが保持する符号なし64ビットバイナリ整数に引数nを加算する.
     *
     * @param n 加算する値
     */
    public void addU64Binary(long n) {
        addBinary(n, 8, false, true);
    }

    /**
     * このオブジェクトが保持する符号付き64ビットバイナリ整数に引数nを加算する.
     *
     * @param n 加算する値
     */
    public void addS64Binary(long n) {
        addBinary(n, 8, true, true);
    }

    /**
     * このオブジェクトが保持するアライメント済みの符号なし8ビットバイナリ整数に引数nを加算する. (現状の実装ではアライメントの指定は反映されず,対応する非修飾版と同一に動作する)
     *
     * @param n 加算する値
     */
    public void addAlignU8Binary(long n) {
        addBinary(n, 1, false, true);
    }

    /**
     * このオブジェクトが保持するアライメント済みの符号付き8ビットバイナリ整数に引数nを加算する. (現状の実装ではアライメントの指定は反映されず,対応する非修飾版と同一に動作する)
     *
     * @param n 加算する値
     */
    public void addAlignS8Binary(long n) {
        addBinary(n, 1, true, true);
    }

    /**
     * このオブジェクトが保持するアライメント済みの符号なし16ビットバイナリ整数に引数nを加算する. (現状の実装ではアライメントの指定は反映されず,対応する非修飾版と同一に動作する)
     *
     * @param n 加算する値
     */
    public void addAlignU16Binary(long n) {
        addBinary(n, 2, false, true);
    }

    /**
     * このオブジェクトが保持するアライメント済みの符号付き16ビットバイナリ整数に引数nを加算する. (現状の実装ではアライメントの指定は反映されず,対応する非修飾版と同一に動作する)
     *
     * @param n 加算する値
     */
    public void addAlignS16Binary(long n) {
        addBinary(n, 2, true, true);
    }

    /**
     * このオブジェクトが保持するアライメント済みの符号なし32ビットバイナリ整数に引数nを加算する. (現状の実装ではアライメントの指定は反映されず,対応する非修飾版と同一に動作する)
     *
     * @param n 加算する値
     */
    public void addAlignU32Binary(long n) {
        addBinary(n, 4, false, true);
    }

    /**
     * このオブジェクトが保持するアライメント済みの符号付き32ビットバイナリ整数に引数nを加算する. (現状の実装ではアライメントの指定は反映されず,対応する非修飾版と同一に動作する)
     *
     * @param n 加算する値
     */
    public void addAlignS32Binary(long n) {
        addBinary(n, 4, true, true);
    }

    /**
     * このオブジェクトが保持するアライメント済みの符号なし64ビットバイナリ整数に引数nを加算する. (現状の実装ではアライメントの指定は反映されず,対応する非修飾版と同一に動作する)
     *
     * @param n 加算する値
     */
    public void addAlignU64Binary(long n) {
        addBinary(n, 8, false, true);
    }

    /**
     * このオブジェクトが保持するアライメント済みの符号付き64ビットバイナリ整数に引数nを加算する. (現状の実装ではアライメントの指定は反映されず,対応する非修飾版と同一に動作する)
     *
     * @param n 加算する値
     */
    public void addAlignS64Binary(long n) {
        addBinary(n, 8, true, true);
    }

    // n numofBytes signed isBigEndian
    /**
     * このオブジェクトが保持する符号なし8ビットバイナリ整数から引数nを減算する.
     *
     * @param n 減算する値
     */
    public void subU8Binary(long n) {
        subBinary(n, 1, false, true);
    }

    /**
     * このオブジェクトが保持する符号付き8ビットバイナリ整数から引数nを減算する.
     *
     * @param n 減算する値
     */
    public void subS8Binary(long n) {
        subBinary(n, 1, true, true);
    }

    /**
     * このオブジェクトが保持する符号なし16ビットバイナリ整数から引数nを減算する.
     *
     * @param n 減算する値
     */
    public void subU16Binary(long n) {
        subBinary(n, 2, false, true);
    }

    /**
     * このオブジェクトが保持する符号付き16ビットバイナリ整数から引数nを減算する.
     *
     * @param n 減算する値
     */
    public void subS16Binary(long n) {
        subBinary(n, 2, true, true);
    }

    /**
     * このオブジェクトが保持する符号なし24ビットバイナリ整数から引数nを減算する.
     *
     * @param n 減算する値
     */
    public void subU24Binary(long n) {
        subBinary(n, 3, false, true);
    }

    /**
     * このオブジェクトが保持する符号付き24ビットバイナリ整数から引数nを減算する.
     *
     * @param n 減算する値
     */
    public void subS24Binary(long n) {
        subBinary(n, 3, true, true);
    }

    /**
     * このオブジェクトが保持する符号なし32ビットバイナリ整数から引数nを減算する.
     *
     * @param n 減算する値
     */
    public void subU32Binary(long n) {
        subBinary(n, 4, false, true);
    }

    /**
     * このオブジェクトが保持する符号付き32ビットバイナリ整数から引数nを減算する.
     *
     * @param n 減算する値
     */
    public void subS32Binary(long n) {
        subBinary(n, 4, true, true);
    }

    /**
     * このオブジェクトが保持する符号なし40ビットバイナリ整数から引数nを減算する.
     *
     * @param n 減算する値
     */
    public void subU40Binary(long n) {
        subBinary(n, 5, false, true);
    }

    /**
     * このオブジェクトが保持する符号付き40ビットバイナリ整数から引数nを減算する.
     *
     * @param n 減算する値
     */
    public void subS40Binary(long n) {
        subBinary(n, 5, true, true);
    }

    /**
     * このオブジェクトが保持する符号なし48ビットバイナリ整数から引数nを減算する.
     *
     * @param n 減算する値
     */
    public void subU48Binary(long n) {
        subBinary(n, 6, false, true);
    }

    /**
     * このオブジェクトが保持する符号付き48ビットバイナリ整数から引数nを減算する.
     *
     * @param n 減算する値
     */
    public void subS48Binary(long n) {
        subBinary(n, 6, true, true);
    }

    /**
     * このオブジェクトが保持する符号なし56ビットバイナリ整数から引数nを減算する.
     *
     * @param n 減算する値
     */
    public void subU56Binary(long n) {
        subBinary(n, 7, false, true);
    }

    /**
     * このオブジェクトが保持する符号付き56ビットバイナリ整数から引数nを減算する.
     *
     * @param n 減算する値
     */
    public void subS56Binary(long n) {
        subBinary(n, 7, true, true);
    }

    /**
     * このオブジェクトが保持する符号なし64ビットバイナリ整数から引数nを減算する.
     *
     * @param n 減算する値
     */
    public void subU64Binary(long n) {
        subBinary(n, 8, false, true);
    }

    /**
     * このオブジェクトが保持する符号付き64ビットバイナリ整数から引数nを減算する.
     *
     * @param n 減算する値
     */
    public void subS64Binary(long n) {
        subBinary(n, 8, true, true);
    }

    /**
     * このオブジェクトが保持するアライメント済みの符号なし8ビットバイナリ整数から引数nを減算する. (現状の実装ではアライメントの指定は反映されず,対応する非修飾版と同一に動作する)
     *
     * @param n 減算する値
     */
    public void subAlignU8Binary(long n) {
        subBinary(n, 1, false, true);
    }

    /**
     * このオブジェクトが保持するアライメント済みの符号付き8ビットバイナリ整数から引数nを減算する. (現状の実装ではアライメントの指定は反映されず,対応する非修飾版と同一に動作する)
     *
     * @param n 減算する値
     */
    public void subAlignS8Binary(long n) {
        subBinary(n, 1, true, true);
    }

    /**
     * このオブジェクトが保持するアライメント済みの符号なし16ビットバイナリ整数から引数nを減算する. (現状の実装ではアライメントの指定は反映されず,対応する非修飾版と同一に動作する)
     *
     * @param n 減算する値
     */
    public void subAlignU16Binary(long n) {
        subBinary(n, 2, false, true);
    }

    /**
     * このオブジェクトが保持するアライメント済みの符号付き16ビットバイナリ整数から引数nを減算する. (現状の実装ではアライメントの指定は反映されず,対応する非修飾版と同一に動作する)
     *
     * @param n 減算する値
     */
    public void subAlignS16Binary(long n) {
        subBinary(n, 2, true, true);
    }

    /**
     * このオブジェクトが保持するアライメント済みの符号なし32ビットバイナリ整数から引数nを減算する. (現状の実装ではアライメントの指定は反映されず,対応する非修飾版と同一に動作する)
     *
     * @param n 減算する値
     */
    public void subAlignU32Binary(long n) {
        subBinary(n, 4, false, true);
    }

    /**
     * このオブジェクトが保持するアライメント済みの符号付き32ビットバイナリ整数から引数nを減算する. (現状の実装ではアライメントの指定は反映されず,対応する非修飾版と同一に動作する)
     *
     * @param n 減算する値
     */
    public void subAlignS32Binary(long n) {
        subBinary(n, 4, true, true);
    }

    /**
     * このオブジェクトが保持するアライメント済みの符号なし64ビットバイナリ整数から引数nを減算する. (現状の実装ではアライメントの指定は反映されず,対応する非修飾版と同一に動作する)
     *
     * @param n 減算する値
     */
    public void subAlignU64Binary(long n) {
        subBinary(n, 8, false, true);
    }

    /**
     * このオブジェクトが保持するアライメント済みの符号付き64ビットバイナリ整数から引数nを減算する. (現状の実装ではアライメントの指定は反映されず,対応する非修飾版と同一に動作する)
     *
     * @param n 減算する値
     */
    public void subAlignS64Binary(long n) {
        subBinary(n, 8, true, true);
    }

    // n numofBytes signed isBigEndian
    /**
     * このオブジェクトが保持するバイトスワップ対応の符号なし8ビットバイナリ整数と引数nを比較する.
     *
     * @param n 比較対象の整数値
     * @return 保持する値がnより小さい場合は負の値,大きい場合は正の値,等しい場合は0
     */
    public int cmpSwpU8Binary(long n) {
        return compareToBinary(n, 1, false, true);
    }

    /**
     * このオブジェクトが保持するバイトスワップ対応の符号付き8ビットバイナリ整数と引数nを比較する.
     *
     * @param n 比較対象の整数値
     * @return 保持する値がnより小さい場合は負の値,大きい場合は正の値,等しい場合は0
     */
    public int cmpSwpS8Binary(long n) {
        return compareToBinary(n, 1, true, true);
    }

    /**
     * このオブジェクトが保持するバイトスワップ対応の符号なし16ビットバイナリ整数と引数nを比較する.
     *
     * @param n 比較対象の整数値
     * @return 保持する値がnより小さい場合は負の値,大きい場合は正の値,等しい場合は0
     */
    public int cmpSwpU16Binary(long n) {
        return compareToBinary(n, 2, false, true);
    }

    /**
     * このオブジェクトが保持するバイトスワップ対応の符号付き16ビットバイナリ整数と引数nを比較する.
     *
     * @param n 比較対象の整数値
     * @return 保持する値がnより小さい場合は負の値,大きい場合は正の値,等しい場合は0
     */
    public int cmpSwpS16Binary(long n) {
        return compareToBinary(n, 2, true, true);
    }

    /**
     * このオブジェクトが保持するバイトスワップ対応の符号なし24ビットバイナリ整数と引数nを比較する.
     *
     * @param n 比較対象の整数値
     * @return 保持する値がnより小さい場合は負の値,大きい場合は正の値,等しい場合は0
     */
    public int cmpSwpU24Binary(long n) {
        return compareToBinary(n, 3, false, true);
    }

    /**
     * このオブジェクトが保持するバイトスワップ対応の符号付き24ビットバイナリ整数と引数nを比較する.
     *
     * @param n 比較対象の整数値
     * @return 保持する値がnより小さい場合は負の値,大きい場合は正の値,等しい場合は0
     */
    public int cmpSwpS24Binary(long n) {
        return compareToBinary(n, 3, true, true);
    }

    /**
     * このオブジェクトが保持するバイトスワップ対応の符号なし32ビットバイナリ整数と引数nを比較する.
     *
     * @param n 比較対象の整数値
     * @return 保持する値がnより小さい場合は負の値,大きい場合は正の値,等しい場合は0
     */
    public int cmpSwpU32Binary(long n) {
        return compareToBinary(n, 4, false, true);
    }

    /**
     * このオブジェクトが保持するバイトスワップ対応の符号付き32ビットバイナリ整数と引数nを比較する.
     *
     * @param n 比較対象の整数値
     * @return 保持する値がnより小さい場合は負の値,大きい場合は正の値,等しい場合は0
     */
    public int cmpSwpS32Binary(long n) {
        return compareToBinary(n, 4, true, true);
    }

    /**
     * このオブジェクトが保持するバイトスワップ対応の符号なし40ビットバイナリ整数と引数nを比較する.
     *
     * @param n 比較対象の整数値
     * @return 保持する値がnより小さい場合は負の値,大きい場合は正の値,等しい場合は0
     */
    public int cmpSwpU40Binary(long n) {
        return compareToBinary(n, 5, false, true);
    }

    /**
     * このオブジェクトが保持するバイトスワップ対応の符号付き40ビットバイナリ整数と引数nを比較する.
     *
     * @param n 比較対象の整数値
     * @return 保持する値がnより小さい場合は負の値,大きい場合は正の値,等しい場合は0
     */
    public int cmpSwpS40Binary(long n) {
        return compareToBinary(n, 5, true, true);
    }

    /**
     * このオブジェクトが保持するバイトスワップ対応の符号なし48ビットバイナリ整数と引数nを比較する.
     *
     * @param n 比較対象の整数値
     * @return 保持する値がnより小さい場合は負の値,大きい場合は正の値,等しい場合は0
     */
    public int cmpSwpU48Binary(long n) {
        return compareToBinary(n, 6, false, true);
    }

    /**
     * このオブジェクトが保持するバイトスワップ対応の符号付き48ビットバイナリ整数と引数nを比較する.
     *
     * @param n 比較対象の整数値
     * @return 保持する値がnより小さい場合は負の値,大きい場合は正の値,等しい場合は0
     */
    public int cmpSwpS48Binary(long n) {
        return compareToBinary(n, 6, true, true);
    }

    /**
     * このオブジェクトが保持するバイトスワップ対応の符号なし56ビットバイナリ整数と引数nを比較する.
     *
     * @param n 比較対象の整数値
     * @return 保持する値がnより小さい場合は負の値,大きい場合は正の値,等しい場合は0
     */
    public int cmpSwpU56Binary(long n) {
        return compareToBinary(n, 7, false, true);
    }

    /**
     * このオブジェクトが保持するバイトスワップ対応の符号付き56ビットバイナリ整数と引数nを比較する.
     *
     * @param n 比較対象の整数値
     * @return 保持する値がnより小さい場合は負の値,大きい場合は正の値,等しい場合は0
     */
    public int cmpSwpS56Binary(long n) {
        return compareToBinary(n, 7, true, true);
    }

    /**
     * このオブジェクトが保持するバイトスワップ対応の符号なし64ビットバイナリ整数と引数nを比較する.
     *
     * @param n 比較対象の整数値
     * @return 保持する値がnより小さい場合は負の値,大きい場合は正の値,等しい場合は0
     */
    public int cmpSwpU64Binary(long n) {
        return compareToBinary(n, 8, false, true);
    }

    /**
     * このオブジェクトが保持するバイトスワップ対応の符号付き64ビットバイナリ整数と引数nを比較する.
     *
     * @param n 比較対象の整数値
     * @return 保持する値がnより小さい場合は負の値,大きい場合は正の値,等しい場合は0
     */
    public int cmpSwpS64Binary(long n) {
        return compareToBinary(n, 8, true, true);
    }

    /**
     * このオブジェクトが保持するバイトスワップ対応のアライメント済みの符号なし8ビットバイナリ整数と引数nを比較する. (現状の実装ではバイトスワップとアライメントの指定は反映されず,対応する非修飾版と同一に動作する)
     *
     * @param n 比較対象の整数値
     * @return 保持する値がnより小さい場合は負の値,大きい場合は正の値,等しい場合は0
     */
    public int cmpSwpAlignU8Binary(long n) {
        return compareToBinary(n, 1, false, true);
    }

    /**
     * このオブジェクトが保持するバイトスワップ対応のアライメント済みの符号付き8ビットバイナリ整数と引数nを比較する. (現状の実装ではバイトスワップとアライメントの指定は反映されず,対応する非修飾版と同一に動作する)
     *
     * @param n 比較対象の整数値
     * @return 保持する値がnより小さい場合は負の値,大きい場合は正の値,等しい場合は0
     */
    public int cmpSwpAlignS8Binary(long n) {
        return compareToBinary(n, 1, true, true);
    }

    /**
     * このオブジェクトが保持するバイトスワップ対応のアライメント済みの符号なし16ビットバイナリ整数と引数nを比較する. (現状の実装ではバイトスワップとアライメントの指定は反映されず,対応する非修飾版と同一に動作する)
     *
     * @param n 比較対象の整数値
     * @return 保持する値がnより小さい場合は負の値,大きい場合は正の値,等しい場合は0
     */
    public int cmpSwpAlignU16Binary(long n) {
        return compareToBinary(n, 2, false, true);
    }

    /**
     * このオブジェクトが保持するバイトスワップ対応のアライメント済みの符号付き16ビットバイナリ整数と引数nを比較する. (現状の実装ではバイトスワップとアライメントの指定は反映されず,対応する非修飾版と同一に動作する)
     *
     * @param n 比較対象の整数値
     * @return 保持する値がnより小さい場合は負の値,大きい場合は正の値,等しい場合は0
     */
    public int cmpSwpAlignS16Binary(long n) {
        return compareToBinary(n, 2, true, true);
    }

    /**
     * このオブジェクトが保持するバイトスワップ対応のアライメント済みの符号なし32ビットバイナリ整数と引数nを比較する. (現状の実装ではバイトスワップとアライメントの指定は反映されず,対応する非修飾版と同一に動作する)
     *
     * @param n 比較対象の整数値
     * @return 保持する値がnより小さい場合は負の値,大きい場合は正の値,等しい場合は0
     */
    public int cmpSwpAlignU32Binary(long n) {
        return compareToBinary(n, 4, false, true);
    }

    /**
     * このオブジェクトが保持するバイトスワップ対応のアライメント済みの符号付き32ビットバイナリ整数と引数nを比較する. (現状の実装ではバイトスワップとアライメントの指定は反映されず,対応する非修飾版と同一に動作する)
     *
     * @param n 比較対象の整数値
     * @return 保持する値がnより小さい場合は負の値,大きい場合は正の値,等しい場合は0
     */
    public int cmpSwpAlignS32Binary(long n) {
        return compareToBinary(n, 4, true, true);
    }

    /**
     * このオブジェクトが保持するバイトスワップ対応のアライメント済みの符号なし64ビットバイナリ整数と引数nを比較する. (現状の実装ではバイトスワップとアライメントの指定は反映されず,対応する非修飾版と同一に動作する)
     *
     * @param n 比較対象の整数値
     * @return 保持する値がnより小さい場合は負の値,大きい場合は正の値,等しい場合は0
     */
    public int cmpSwpAlignU64Binary(long n) {
        return compareToBinary(n, 8, false, true);
    }

    /**
     * このオブジェクトが保持するバイトスワップ対応のアライメント済みの符号付き64ビットバイナリ整数と引数nを比較する. (現状の実装ではバイトスワップとアライメントの指定は反映されず,対応する非修飾版と同一に動作する)
     *
     * @param n 比較対象の整数値
     * @return 保持する値がnより小さい場合は負の値,大きい場合は正の値,等しい場合は0
     */
    public int cmpSwpAlignS64Binary(long n) {
        return compareToBinary(n, 8, true, true);
    }

    // n numofBytes signed isBigEndian
    /**
     * このオブジェクトが保持するバイトスワップ対応の符号なし8ビットバイナリ整数に引数nを加算する.
     *
     * @param n 加算する値
     */
    public void addSwpU8Binary(long n) {
        addBinary(n, 1, false, true);
    }

    /**
     * このオブジェクトが保持するバイトスワップ対応の符号付き8ビットバイナリ整数に引数nを加算する.
     *
     * @param n 加算する値
     */
    public void addSwpS8Binary(long n) {
        addBinary(n, 1, true, true);
    }

    /**
     * このオブジェクトが保持するバイトスワップ対応の符号なし16ビットバイナリ整数に引数nを加算する.
     *
     * @param n 加算する値
     */
    public void addSwpU16Binary(long n) {
        addBinary(n, 2, false, true);
    }

    /**
     * このオブジェクトが保持するバイトスワップ対応の符号付き16ビットバイナリ整数に引数nを加算する.
     *
     * @param n 加算する値
     */
    public void addSwpS16Binary(long n) {
        addBinary(n, 2, true, true);
    }

    /**
     * このオブジェクトが保持するバイトスワップ対応の符号なし24ビットバイナリ整数に引数nを加算する.
     *
     * @param n 加算する値
     */
    public void addSwpU24Binary(long n) {
        addBinary(n, 3, false, true);
    }

    /**
     * このオブジェクトが保持するバイトスワップ対応の符号付き24ビットバイナリ整数に引数nを加算する.
     *
     * @param n 加算する値
     */
    public void addSwpS24Binary(long n) {
        addBinary(n, 3, true, true);
    }

    /**
     * このオブジェクトが保持するバイトスワップ対応の符号なし32ビットバイナリ整数に引数nを加算する.
     *
     * @param n 加算する値
     */
    public void addSwpU32Binary(long n) {
        addBinary(n, 4, false, true);
    }

    /**
     * このオブジェクトが保持するバイトスワップ対応の符号付き32ビットバイナリ整数に引数nを加算する.
     *
     * @param n 加算する値
     */
    public void addSwpS32Binary(long n) {
        addBinary(n, 4, true, true);
    }

    /**
     * このオブジェクトが保持するバイトスワップ対応の符号なし40ビットバイナリ整数に引数nを加算する.
     *
     * @param n 加算する値
     */
    public void addSwpU40Binary(long n) {
        addBinary(n, 5, false, true);
    }

    /**
     * このオブジェクトが保持するバイトスワップ対応の符号付き40ビットバイナリ整数に引数nを加算する.
     *
     * @param n 加算する値
     */
    public void addSwpS40Binary(long n) {
        addBinary(n, 5, true, true);
    }

    /**
     * このオブジェクトが保持するバイトスワップ対応の符号なし48ビットバイナリ整数に引数nを加算する.
     *
     * @param n 加算する値
     */
    public void addSwpU48Binary(long n) {
        addBinary(n, 6, false, true);
    }

    /**
     * このオブジェクトが保持するバイトスワップ対応の符号付き48ビットバイナリ整数に引数nを加算する.
     *
     * @param n 加算する値
     */
    public void addSwpS48Binary(long n) {
        addBinary(n, 6, true, true);
    }

    /**
     * このオブジェクトが保持するバイトスワップ対応の符号なし56ビットバイナリ整数に引数nを加算する.
     *
     * @param n 加算する値
     */
    public void addSwpU56Binary(long n) {
        addBinary(n, 7, false, true);
    }

    /**
     * このオブジェクトが保持するバイトスワップ対応の符号付き56ビットバイナリ整数に引数nを加算する.
     *
     * @param n 加算する値
     */
    public void addSwpS56Binary(long n) {
        addBinary(n, 7, true, true);
    }

    /**
     * このオブジェクトが保持するバイトスワップ対応の符号なし64ビットバイナリ整数に引数nを加算する.
     *
     * @param n 加算する値
     */
    public void addSwpU64Binary(long n) {
        addBinary(n, 8, false, true);
    }

    /**
     * このオブジェクトが保持するバイトスワップ対応の符号付き64ビットバイナリ整数に引数nを加算する.
     *
     * @param n 加算する値
     */
    public void addSwpS64Binary(long n) {
        addBinary(n, 8, true, true);
    }

    /**
     * このオブジェクトが保持するバイトスワップ対応のアライメント済みの符号なし8ビットバイナリ整数に引数nを加算する. (現状の実装ではバイトスワップとアライメントの指定は反映されず,対応する非修飾版と同一に動作する)
     *
     * @param n 加算する値
     */
    public void addSwpAlignU8Binary(long n) {
        addBinary(n, 1, false, true);
    }

    /**
     * このオブジェクトが保持するバイトスワップ対応のアライメント済みの符号付き8ビットバイナリ整数に引数nを加算する. (現状の実装ではバイトスワップとアライメントの指定は反映されず,対応する非修飾版と同一に動作する)
     *
     * @param n 加算する値
     */
    public void addSwpAlignS8Binary(long n) {
        addBinary(n, 1, true, true);
    }

    /**
     * このオブジェクトが保持するバイトスワップ対応のアライメント済みの符号なし16ビットバイナリ整数に引数nを加算する. (現状の実装ではバイトスワップとアライメントの指定は反映されず,対応する非修飾版と同一に動作する)
     *
     * @param n 加算する値
     */
    public void addSwpAlignU16Binary(long n) {
        addBinary(n, 2, false, true);
    }

    /**
     * このオブジェクトが保持するバイトスワップ対応のアライメント済みの符号付き16ビットバイナリ整数に引数nを加算する. (現状の実装ではバイトスワップとアライメントの指定は反映されず,対応する非修飾版と同一に動作する)
     *
     * @param n 加算する値
     */
    public void addSwpAlignS16Binary(long n) {
        addBinary(n, 2, true, true);
    }

    /**
     * このオブジェクトが保持するバイトスワップ対応のアライメント済みの符号なし32ビットバイナリ整数に引数nを加算する. (現状の実装ではバイトスワップとアライメントの指定は反映されず,対応する非修飾版と同一に動作する)
     *
     * @param n 加算する値
     */
    public void addSwpAlignU32Binary(long n) {
        addBinary(n, 4, false, true);
    }

    /**
     * このオブジェクトが保持するバイトスワップ対応のアライメント済みの符号付き32ビットバイナリ整数に引数nを加算する. (現状の実装ではバイトスワップとアライメントの指定は反映されず,対応する非修飾版と同一に動作する)
     *
     * @param n 加算する値
     */
    public void addSwpAlignS32Binary(long n) {
        addBinary(n, 4, true, true);
    }

    /**
     * このオブジェクトが保持するバイトスワップ対応のアライメント済みの符号なし64ビットバイナリ整数に引数nを加算する. (現状の実装ではバイトスワップとアライメントの指定は反映されず,対応する非修飾版と同一に動作する)
     *
     * @param n 加算する値
     */
    public void addSwpAlignU64Binary(long n) {
        addBinary(n, 8, false, true);
    }

    /**
     * このオブジェクトが保持するバイトスワップ対応のアライメント済みの符号付き64ビットバイナリ整数に引数nを加算する. (現状の実装ではバイトスワップとアライメントの指定は反映されず,対応する非修飾版と同一に動作する)
     *
     * @param n 加算する値
     */
    public void addSwpAlignS64Binary(long n) {
        addBinary(n, 8, true, true);
    }

    // n numofBytes signed isBigEndian
    /**
     * このオブジェクトが保持するバイトスワップ対応の符号なし8ビットバイナリ整数から引数nを減算する.
     *
     * @param n 減算する値
     */
    public void subSwpU8Binary(int n) {
        subBinary(n, 1, false, true);
    }

    /**
     * このオブジェクトが保持するバイトスワップ対応の符号付き8ビットバイナリ整数から引数nを減算する.
     *
     * @param n 減算する値
     */
    public void subSwpS8Binary(int n) {
        subBinary(n, 1, true, true);
    }

    /**
     * このオブジェクトが保持するバイトスワップ対応の符号なし16ビットバイナリ整数から引数nを減算する.
     *
     * @param n 減算する値
     */
    public void subSwpU16Binary(int n) {
        subBinary(n, 2, false, true);
    }

    /**
     * このオブジェクトが保持するバイトスワップ対応の符号付き16ビットバイナリ整数から引数nを減算する.
     *
     * @param n 減算する値
     */
    public void subSwpS16Binary(int n) {
        subBinary(n, 2, true, true);
    }

    /**
     * このオブジェクトが保持するバイトスワップ対応の符号なし24ビットバイナリ整数から引数nを減算する.
     *
     * @param n 減算する値
     */
    public void subSwpU24Binary(int n) {
        subBinary(n, 3, false, true);
    }

    /**
     * このオブジェクトが保持するバイトスワップ対応の符号付き24ビットバイナリ整数から引数nを減算する.
     *
     * @param n 減算する値
     */
    public void subSwpS24Binary(int n) {
        subBinary(n, 3, true, true);
    }

    /**
     * このオブジェクトが保持するバイトスワップ対応の符号なし32ビットバイナリ整数から引数nを減算する.
     *
     * @param n 減算する値
     */
    public void subSwpU32Binary(int n) {
        subBinary(n, 4, false, true);
    }

    /**
     * このオブジェクトが保持するバイトスワップ対応の符号付き32ビットバイナリ整数から引数nを減算する.
     *
     * @param n 減算する値
     */
    public void subSwpS32Binary(int n) {
        subBinary(n, 4, true, true);
    }

    /**
     * このオブジェクトが保持するバイトスワップ対応の符号なし40ビットバイナリ整数から引数nを減算する.
     *
     * @param n 減算する値
     */
    public void subSwpU40Binary(int n) {
        subBinary(n, 5, false, true);
    }

    /**
     * このオブジェクトが保持するバイトスワップ対応の符号付き40ビットバイナリ整数から引数nを減算する.
     *
     * @param n 減算する値
     */
    public void subSwpS40Binary(int n) {
        subBinary(n, 5, true, true);
    }

    /**
     * このオブジェクトが保持するバイトスワップ対応の符号なし48ビットバイナリ整数から引数nを減算する.
     *
     * @param n 減算する値
     */
    public void subSwpU48Binary(int n) {
        subBinary(n, 6, false, true);
    }

    /**
     * このオブジェクトが保持するバイトスワップ対応の符号付き48ビットバイナリ整数から引数nを減算する.
     *
     * @param n 減算する値
     */
    public void subSwpS48Binary(int n) {
        subBinary(n, 6, true, true);
    }

    /**
     * このオブジェクトが保持するバイトスワップ対応の符号なし56ビットバイナリ整数から引数nを減算する.
     *
     * @param n 減算する値
     */
    public void subSwpU56Binary(int n) {
        subBinary(n, 7, false, true);
    }

    /**
     * このオブジェクトが保持するバイトスワップ対応の符号付き56ビットバイナリ整数から引数nを減算する.
     *
     * @param n 減算する値
     */
    public void subSwpS56Binary(int n) {
        subBinary(n, 7, true, true);
    }

    /**
     * このオブジェクトが保持するバイトスワップ対応の符号なし64ビットバイナリ整数から引数nを減算する.
     *
     * @param n 減算する値
     */
    public void subSwpU64Binary(int n) {
        subBinary(n, 8, false, true);
    }

    /**
     * このオブジェクトが保持するバイトスワップ対応の符号付き64ビットバイナリ整数から引数nを減算する.
     *
     * @param n 減算する値
     */
    public void subSwpS64Binary(int n) {
        subBinary(n, 8, true, true);
    }

    /**
     * このオブジェクトが保持するバイトスワップ対応のアライメント済みの符号なし8ビットバイナリ整数から引数nを減算する. (現状の実装ではバイトスワップとアライメントの指定は反映されず,対応する非修飾版と同一に動作する)
     *
     * @param n 減算する値
     */
    public void subSwpAlignU8Binary(int n) {
        subBinary(n, 1, false, true);
    }

    /**
     * このオブジェクトが保持するバイトスワップ対応のアライメント済みの符号付き8ビットバイナリ整数から引数nを減算する. (現状の実装ではバイトスワップとアライメントの指定は反映されず,対応する非修飾版と同一に動作する)
     *
     * @param n 減算する値
     */
    public void subSwpAlignS8Binary(int n) {
        subBinary(n, 1, true, true);
    }

    /**
     * このオブジェクトが保持するバイトスワップ対応のアライメント済みの符号なし16ビットバイナリ整数から引数nを減算する. (現状の実装ではバイトスワップとアライメントの指定は反映されず,対応する非修飾版と同一に動作する)
     *
     * @param n 減算する値
     */
    public void subSwpAlignU16Binary(int n) {
        subBinary(n, 2, false, true);
    }

    /**
     * このオブジェクトが保持するバイトスワップ対応のアライメント済みの符号付き16ビットバイナリ整数から引数nを減算する. (現状の実装ではバイトスワップとアライメントの指定は反映されず,対応する非修飾版と同一に動作する)
     *
     * @param n 減算する値
     */
    public void subSwpAlignS16Binary(int n) {
        subBinary(n, 2, true, true);
    }

    /**
     * このオブジェクトが保持するバイトスワップ対応のアライメント済みの符号なし32ビットバイナリ整数から引数nを減算する. (現状の実装ではバイトスワップとアライメントの指定は反映されず,対応する非修飾版と同一に動作する)
     *
     * @param n 減算する値
     */
    public void subSwpAlignU32Binary(int n) {
        subBinary(n, 4, false, true);
    }

    /**
     * このオブジェクトが保持するバイトスワップ対応のアライメント済みの符号付き32ビットバイナリ整数から引数nを減算する. (現状の実装ではバイトスワップとアライメントの指定は反映されず,対応する非修飾版と同一に動作する)
     *
     * @param n 減算する値
     */
    public void subSwpAlignS32Binary(int n) {
        subBinary(n, 4, true, true);
    }

    /**
     * このオブジェクトが保持するバイトスワップ対応のアライメント済みの符号なし64ビットバイナリ整数から引数nを減算する. (現状の実装ではバイトスワップとアライメントの指定は反映されず,対応する非修飾版と同一に動作する)
     *
     * @param n 減算する値
     */
    public void subSwpAlignU64Binary(int n) {
        subBinary(n, 8, false, true);
    }

    /**
     * このオブジェクトが保持するバイトスワップ対応のアライメント済みの符号付き64ビットバイナリ整数から引数nを減算する. (現状の実装ではバイトスワップとアライメントの指定は反映されず,対応する非修飾版と同一に動作する)
     *
     * @param n 減算する値
     */
    public void subSwpAlignS64Binary(int n) {
        subBinary(n, 8, true, true);
    }

    /**
     * このオブジェクトにバイトスワップ対応の符号なし16ビットバイナリ整数として引数nを書き込む.
     *
     * @param n 書き込む値
     */
    public void setSwpU16Binary(int n) {
        this.fromLong(2, true, n);
    }

    /**
     * このオブジェクトにバイトスワップ対応の符号付き16ビットバイナリ整数として引数nを書き込む.
     *
     * @param n 書き込む値
     */
    public void setSwpS16Binary(int n) {
        this.fromLong(2, true, n);
    }

    /**
     * このオブジェクトにバイトスワップ対応の符号なし24ビットバイナリ整数として引数nを書き込む.
     *
     * @param n 書き込む値
     */
    public void setSwpU24Binary(int n) {
        this.fromLong(3, true, n);
    }

    /**
     * このオブジェクトにバイトスワップ対応の符号付き24ビットバイナリ整数として引数nを書き込む.
     *
     * @param n 書き込む値
     */
    public void setSwpS24Binary(int n) {
        this.fromLong(3, true, n);
    }

    /**
     * このオブジェクトにバイトスワップ対応の符号なし32ビットバイナリ整数として引数nを書き込む.
     *
     * @param n 書き込む値
     */
    public void setSwpU32Binary(int n) {
        this.fromLong(4, true, n);
    }

    /**
     * このオブジェクトにバイトスワップ対応の符号付き32ビットバイナリ整数として引数nを書き込む.
     *
     * @param n 書き込む値
     */
    public void setSwpS32Binary(int n) {
        this.fromLong(4, true, n);
    }

    /**
     * このオブジェクトにバイトスワップ対応の符号なし40ビットバイナリ整数として引数nを書き込む.
     *
     * @param n 書き込む値
     */
    public void setSwpU40Binary(int n) {
        this.fromLong(5, true, n);
    }

    /**
     * このオブジェクトにバイトスワップ対応の符号付き40ビットバイナリ整数として引数nを書き込む.
     *
     * @param n 書き込む値
     */
    public void setSwpS40Binary(int n) {
        this.fromLong(5, true, n);
    }

    /**
     * このオブジェクトにバイトスワップ対応の符号なし48ビットバイナリ整数として引数nを書き込む.
     *
     * @param n 書き込む値
     */
    public void setSwpU48Binary(int n) {
        this.fromLong(6, true, n);
    }

    /**
     * このオブジェクトにバイトスワップ対応の符号付き48ビットバイナリ整数として引数nを書き込む.
     *
     * @param n 書き込む値
     */
    public void setSwpS48Binary(int n) {
        this.fromLong(6, true, n);
    }

    /**
     * このオブジェクトにバイトスワップ対応の符号なし56ビットバイナリ整数として引数nを書き込む.
     *
     * @param n 書き込む値
     */
    public void setSwpU56Binary(int n) {
        this.fromLong(7, true, n);
    }

    /**
     * このオブジェクトにバイトスワップ対応の符号付き56ビットバイナリ整数として引数nを書き込む.
     *
     * @param n 書き込む値
     */
    public void setSwpS56Binary(int n) {
        this.fromLong(7, true, n);
    }

    /**
     * このオブジェクトにバイトスワップ対応の符号なし64ビットバイナリ整数として引数nを書き込む.
     *
     * @param n 書き込む値
     */
    public void setSwpU64Binary(int n) {
        this.fromLong(8, true, n);
    }

    /**
     * このオブジェクトにバイトスワップ対応の符号付き64ビットバイナリ整数として引数nを書き込む.
     *
     * @param n 書き込む値
     */
    public void setSwpS64Binary(int n) {
        this.fromLong(8, true, n);
    }

    // libcob/numeric.cのcob_cmp_numdispの実装
    /**
     * 表示用数字(数字列)として保持する値を引数nと比較する.
     *
     * @param size 対象のバイト数(桁数)
     * @param n 比較対象の値
     * @return 保持する値がnより小さい場合は-1,大きい場合は1,等しい場合は0
     */
    public int cmpNumdisp(int size, long n) {
        int p = 0;
        int val = 0;
        for (int inc = 0; inc < size; ++inc, ++p) {
            val = (val * 10) + (this.getByte(p) - '0');
        }
        return (val < n) ? -1 : (val > n) ? 1 : 0;
    }

    // libcob/numeric.cのcob_cmp_long_numdispの実装
    /**
     * 表示用数字(数字列)として保持する値を引数nと比較する.
     *
     * @param size 対象のバイト数(桁数)
     * @param n 比較対象の値
     * @return 保持する値がnより小さい場合は-1,大きい場合は1,等しい場合は0
     */
    public int cmpLongNumdisp(int size, long n) {
        return this.cmpNumdisp(size, n);
    }

    /**
     * 符号付き表示用数字として保持する値を引数nと比較する.
     *
     * @param size 対象のバイト数(桁数)
     * @param n 比較対象の値
     * @return 保持する値がnより小さい場合は-1,大きい場合は1,等しい場合は0
     */
    public int cmpSignNumdisp(int size, long n) {
        int p = 0;
        int val = 0;
        for (int inc = 0; inc < size - 1; ++inc, ++p) {
            val = (val * 10) + (this.getByte(p) - '0');
        }
        val *= 10;
        if (this.getByte(p) >= '0' && this.getByte(p) <= '9') {
            val += (this.getByte(p) - '0');
        } else {
            if (CobolModule.getCurrentModule().display_sign != 0) {
                PairInt pair = this.getLongEbcdicSign(p, val);
                val = pair.first;
                if (pair.last != 0) {
                    val = -val;
                }
            } else {
                val += (this.getByte(p) - 'p');
                val = -val;
            }
        }
        return (val < n) ? -1 : (val > n) ? 1 : 0;
    }

    /**
     * 符号付き表示用数字として保持する値を引数nと比較する.
     *
     * @param size 対象のバイト数(桁数)
     * @param n 比較対象の値
     * @return 保持する値がnより小さい場合は-1,大きい場合は1,等しい場合は0
     */
    public int cmpLongSignNumdisp(int size, long n) {
        int p = 0;
        int val = 0;
        for (int inc = 0; inc < size - 1; ++inc, ++p) {
            val = (val * 10) + (this.getByte(p) - '0');
        }
        val *= 10;
        if (this.getByte(p) >= '0' && this.getByte(p) <= '9') {
            val += (this.getByte(p) - '0');
        } else {
            if (CobolModule.getCurrentModule().display_sign != 0) {
                PairInt pair = this.getLongEbcdicSign(p, val);
                val = pair.first;
                if (pair.last != 0) {
                    val = -val;
                }
            } else {
                val += (this.getByte(p) - ('0' + 0x40));
                val = -val;
            }
        }
        return (val < n) ? -1 : (val > n) ? 1 : 0;
    }

    /**
     * 表示用数字(数字列)として保持する値を整数値に変換して返す.
     *
     * @param size 対象のバイト数(桁数)
     * @return 変換した整数値
     */
    public int getNumdisp(int size) {
        int retval = 0;
        for (int n = 0; n < size; ++n) {
            retval *= 10;
            byte b = this.getByte(n);
            if (b > '9') {
                retval += 10;
            } else {
                retval += b - '0';
            }
        }
        return retval;
    }

    private static class PairInt {
        public int first;
        public int last;

        public PairInt(int a, int b) {
            first = a;
            last = b;
        }
    }

    /**
     * EBCDIC形式の符号付き最下位桁を解釈し,値と符号を返す.
     *
     * @param p 符号桁のバイト配列中の位置
     * @param val 符号桁を除いた値
     * @return 値(first)と符号(last,負なら1)を保持するPairInt
     */
    public PairInt getEbcdicSign(int p, int val) {
        switch (this.getByte(p)) {
            case '{':
                return new PairInt(val, 0);
            case 'A':
                return new PairInt(val + 1, 0);
            case 'B':
                return new PairInt(val + 2, 0);
            case 'C':
                return new PairInt(val + 3, 0);
            case 'D':
                return new PairInt(val + 4, 0);
            case 'E':
                return new PairInt(val + 5, 0);
            case 'F':
                return new PairInt(val + 6, 0);
            case 'G':
                return new PairInt(val + 7, 0);
            case 'H':
                return new PairInt(val + 8, 0);
            case 'I':
                return new PairInt(val + 9, 0);
            case '}':
                return new PairInt(val, 1);
            case 'J':
                return new PairInt(val + 1, 1);
            case 'K':
                return new PairInt(val + 2, 1);
            case 'L':
                return new PairInt(val + 3, 1);
            case 'M':
                return new PairInt(val + 4, 1);
            case 'N':
                return new PairInt(val + 5, 1);
            case 'O':
                return new PairInt(val + 6, 1);
            case 'P':
                return new PairInt(val + 7, 1);
            case 'Q':
                return new PairInt(val + 8, 1);
            case 'R':
                return new PairInt(val + 9, 1);
            default:
                return new PairInt(val, 0);
        }
    }

    /**
     * EBCDIC形式の符号付き最下位桁を解釈し,値と符号を返す.
     *
     * @param p 符号桁のバイト配列中の位置
     * @param val 符号桁を除いた値
     * @return 値(first)と符号(last,負なら1)を保持するPairInt
     */
    public PairInt getLongEbcdicSign(int p, int val) {
        switch (this.getByte(p)) {
            case '{':
                return new PairInt(val, 0);
            case 'A':
                return new PairInt(val + 1, 0);
            case 'B':
                return new PairInt(val + 2, 0);
            case 'C':
                return new PairInt(val + 3, 0);
            case 'D':
                return new PairInt(val + 4, 0);
            case 'E':
                return new PairInt(val + 5, 0);
            case 'F':
                return new PairInt(val + 6, 0);
            case 'G':
                return new PairInt(val + 7, 0);
            case 'H':
                return new PairInt(val + 8, 0);
            case 'I':
                return new PairInt(val + 9, 0);
            case '}':
                return new PairInt(val, 1);
            case 'J':
                return new PairInt(val + 1, 1);
            case 'K':
                return new PairInt(val + 2, 1);
            case 'L':
                return new PairInt(val + 3, 1);
            case 'M':
                return new PairInt(val + 4, 1);
            case 'N':
                return new PairInt(val + 5, 1);
            case 'O':
                return new PairInt(val + 6, 1);
            case 'P':
                return new PairInt(val + 7, 1);
            case 'Q':
                return new PairInt(val + 8, 1);
            case 'R':
                return new PairInt(val + 9, 1);
            default:
                return new PairInt(val, 0);
        }
    }

    // TODO 修正
    // opensource COBOLにはない実装 COB_BSWAP_32などの代替
    // 本家opensource COBOLのcobc/codegen.cの894行目付近を参照
    /**
     * 保持する2バイトをビッグエンディアンの整数として読み込んで返す.
     *
     * @return 読み込んだ2バイト整数値
     */
    public long bswap_16() {
        return this.toLong(2, true, true);
    }

    /**
     * 保持する4バイトをビッグエンディアンの整数として読み込んで返す.
     *
     * @return 読み込んだ4バイト整数値
     */
    public long bswap_32() {
        return this.toLong(4, true, true);
    }

    /**
     * 保持する8バイトをビッグエンディアンの整数として読み込んで返す.
     *
     * @return 読み込んだ8バイト整数値
     */
    public long bswap_64() {
        return this.toLong(8, true, true);
    }
}
