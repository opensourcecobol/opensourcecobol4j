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

import java.io.UnsupportedEncodingException;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import jp.osscons.opensourcecobol.libcobj.common.CobolModule;

/** COBOLのデータを保存するバイト配列を扱うクラス */
public class CobolDataStorage {

  /** データを保存するバイト配列 */
  private byte[] data;

  /** このクラスの扱うデータが保存する領域のバイト配列中での相対位置 */
  private int index;

  /**
   * コンストラクタ.引数で指定された長さ分のバイト配列を確保する.
   *
   * @param size バイト配列の長さ
   */
  public CobolDataStorage(int size) {
    this.data = new byte[size];
    this.index = 0;
  }

  /**
   * コンストラクタ.データを保存するバイト配列と保存する領域の相対位置を引数で指定する.
   *
   * @param data データを保存するバイト配列
   * @param index このクラスの扱うデータが保存する領域のバイト配列中での相対位置
   */
  public CobolDataStorage(byte[] data, int index) {
    this.data = data;
    this.index = index;
  }

  /**
   * コンストラクタ.データを保存するバイト配列を引数で指定する.
   *
   * @param data データを保存するバイト配列
   */
  public CobolDataStorage(byte[] data) {
    this(data, 0);
  }

  public static CobolDataStorage makeCobolDataStorage(byte... bytes) {
    return new CobolDataStorage(bytes);
  }

  public CobolDataStorage() {
    this.index = 0;
    this.data = null;
  }

  public void setDataRefAndIndex(CobolDataStorage data, int index) {
    this.data = data.data;
    this.index = index;
  }

  public int getIndex() {
    return this.index;
  }

  public void setIndex(int index) {
    this.index = index;
  }

  public byte[] getRefOfData() {
    return this.data;
  }

  public CobolDataStorage copy() {
    CobolDataStorage ret = new CobolDataStorage();
    ret.index = this.index;
    ret.data = this.data;
    return ret;
  }

  public ByteBuffer getByteBuffer(int size) {
    return ByteBuffer.wrap(this.data, this.index, size);
  }

  public void addIndex(int n) {
    this.index += n;
  }

  public static CobolDataStorage primitiveToDataStorage(int n) {
    byte[] bytes = new byte[4];
    ByteBuffer buffer = ByteBuffer.wrap(bytes);
    buffer.putInt(n);
    return new CobolDataStorage(bytes);
  }

  public static CobolDataStorage primitiveToDataStorage(String s) {
    return new CobolDataStorage(s);
  }

  /**
   * コンストラクタ.文字列からバイト配列を構成する.
   *
   * @param str
   */
  public CobolDataStorage(String str) {
    try {
      byte[] bytes = str.getBytes("SHIFT-JIS");
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
   * @return
   */
  public byte[] getData() {
    return this.getData(0);
  }

  /**
   * バイト配列の引数で指定した相対位置から末尾までをコピーした配列を返す。
   *
   * @param index コピーの開始位置(this.indexバイト目を基準とする)
   * @return 開始位置から末尾までのデータをコピーしたバイト配列
   */
  public byte[] getData(int index) {
    byte[] result = new byte[this.data.length - this.index - index];

    System.arraycopy(this.data, this.index + index, result, 0, result.length);

    return result;
  }

  /**
   * バイト配列の引数で指定した相対位置から指定した長さをコピーした配列を返す。
   *
   * @param index コピーの開始位置(this.indexバイト目を基準とする)
   * @param length コピーする長さ(バイト数)
   * @return 開始位置からlengthバイト分の配列
   */
  public byte[] getByteArray(int index, int length) {
    byte[] result = new byte[length];
    System.arraycopy(this.data, this.index + index, result, 0, length);
    return result;
  }

  /*
   * Returns a reference to the byte array holded by this object.
   * Modification of the returned array will affect the data holded by this
   * object.
   */
  public byte[] getByteArrayRef(int index, int length) {
    ByteBuffer buffer = ByteBuffer.wrap(this.data, this.index + index, length);
    return buffer.array();
  }

  /**
   * C言語のmemcpy
   *
   * @param buf
   * @param size
   */
  public void memcpy(CobolDataStorage buf, int size) {
    for (int i = 0; i < size; ++i) {
      this.setByte(i, buf.getByte(i));
    }
  }

  /**
   * C言語のmemcpy
   *
   * @param buf
   * @param size
   */
  public void memcpy(byte[] buf, int size) {
    for (int i = 0; i < size; ++i) {
      this.setByte(i, buf[i]);
    }
  }

  /**
   * C言語のmemcpy
   *
   * @param buf
   * @param size
   */
  public void memcpy(String str, int size) {
    try {
      this.memcpy(str.getBytes("SJIS"), size);
    } catch (UnsupportedEncodingException e) {
      this.memcpy(str.getBytes(), size);
    }
  }

  /**
   * C言語のmemcpy (offset指定あり)
   *
   * @param offset
   * @param buf
   * @param size
   */
  public void memcpy(int offset, byte[] buf, int size) {
    for (int i = 0; i < size; ++i) {
      this.setByte(offset + i, buf[i]);
    }
  }

  public void memcpy(int offset, CobolDataStorage buf, int size) {
    for (int i = 0; i < size; ++i) {
      this.setByte(offset + i, buf.getByte(i));
    }
  }

  public void memcpy(byte[] buf) {
    this.memcpy(buf, buf.length);
  }

  /**
   * C言語のmemset
   *
   * @param ch
   * @param size
   */
  public void memset(byte ch, int size) {
    for (int i = 0; i < size; ++i) {
      this.setByte(i, ch);
    }
  }

  /**
   * C言語のmemset
   *
   * @param ch
   * @param size
   */
  public void memset(int ch, int size) {
    this.memset((byte) ch, size);
  }

  /**
   * C言語のmemset
   *
   * @param ch
   * @param size
   */
  public void memset(int offset, byte ch, int size) {
    for (int i = 0; i < size; ++i) {
      this.setByte(offset + i, ch);
    }
  }

  /**
   * C言語のmemset
   *
   * @param ch
   * @param size
   */
  public void memset(int offset, int ch, int size) {
    this.memset(offset, (byte) ch, size);
  }

  /**
   * C言語のmemcmp
   *
   * @param buf
   * @param size
   * @return
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
   * C言語のmemcmp
   *
   * @param buf
   * @param size
   * @return
   */
  public int memcmp(String buf, int size) {
    return this.memcmp(buf.getBytes(), size);
  }

  /**
   * C言語のmemcmp
   *
   * @param buf
   * @param size
   * @return
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
   * 引数で与えられたバイト配列をthis.dataの先頭からコピーする
   *
   * @param data
   */
  public void setData(byte[] data) {
    setData(data, 0);
  }

  /**
   * 引数で指定されたバイト配列をthis.dataの引数で指定された位置からコピーする
   *
   * @param data コピー元のバイト配列
   * @param index this.byteのコピー開始位置(this.indexバイト目を基準とする)
   */
  public void setData(byte[] data, int index) {
    int length =
        (data.length <= this.data.length - this.index - index)
            ? data.length
            : this.data.length - this.index - index;

    System.arraycopy(data, 0, this.data, this.index + index, length);
  }

  /**
   * 引数で指定された分だけindexを変位させたCobolDataStorageクラスのインスタンスを作成する
   *
   * @param index 新たに作成するCobolDataStorageクラスのインスタンスのメンバ変数indexのthis.indexからの相対位置
   * @return 新たに作成したCobolDataStorageクラスのインスタンス
   */
  public CobolDataStorage getDataStorage(int index) {
    return new CobolDataStorage(this.data, this.index + index);
  }

  /**
   * 指定のバイト配列中での位置に指定の値を代入する
   *
   * @param index 代入先のバイト配列中の位置.this.indexを基準とする.
   * @param value 代入する値
   */
  public void setByte(int index, byte value) {
    this.data[this.index + index] = value;
  }

  /**
   * 指定のバイト配列中での位置に指定の値を代入する
   *
   * @param index 代入先のバイト配列中の位置.this.indexを基準とする.
   * @param value 代入する値
   */
  public void setByte(int index, int value) {
    this.setByte(index, (byte) value);
  }

  /**
   * 指定のバイト配列中での位置に指定の値を代入する
   *
   * @param index 代入先のバイト配列中の位置.this.indexを基準とする.
   * @param value 代入する値
   */
  public void setByte(int index, char value) {
    this.setByte(index, (byte) value);
  }

  public void setByte(byte value) {
    this.setByte(0, value);
  }

  public void setByte(int value) {
    this.setByte((byte) value);
  }

  public void setByte(char value) {
    this.setByte((byte) value);
  }

  /**
   * 指定のバイト配列中での位置に格納された値を返す
   *
   * @param index バイト配列中での位置.this.indexを基準とする.
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
   * @param index コピーの開始位置(this.indexが基準)
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
   * @param index コピーの開始位置(this.indexが基準)
   * @param value 代入する値
   * @param size 代入先のバイト数
   */
  public void fillBytes(int index, int value, int size) {
    this.fillBytes(index, (byte) value, size);
  }

  /**
   * バイト配列の0バイト目からsizeバイトの範囲にvalueを代入する
   *
   * @param index コピーの開始位置(this.indexが基準)
   * @param value 代入する値
   * @param size 代入先のバイト数
   */
  public void fillBytes(int index, char value, int size) {
    this.fillBytes(index, (byte) value, size);
  }

  /**
   * 指定のバイト配列に格納された値をthis.dataのthis.indexバイト目以降へコピーする.
   *
   * @param bytes コピー元のバイト配列
   */
  public void setBytes(byte[] bytes) {
    for (int i = 0; i < bytes.length; ++i) {
      this.setByte(i, bytes[i]);
    }
  }

  /**
   * 指定のバイト配列に格納された値を this.dataのthis.indexバイト目以降へlengthバイトだけコピーする.
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
   * 指定のバイト配列に格納された値を this.dataのthis.index+indexバイト目以降へlengthバイトだけコピーする.
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
      this.setBytes(str.getBytes("SHIFT-JIS"), length);
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
   * @param dstIndex コピー先(this.data)のthis.indexからの相対位置
   */
  public void setBytes(CobolDataStorage data, int length, int dstIndex) {
    this.setBytes(data, length, dstIndex, 0);
  }

  /**
   * 指定されたCobolDataStorageのインスタンスから,lengthバイトだけデータをコピーする
   *
   * @param data コピー元のCobolDataStorage
   * @param length コピーするバイト数
   * @param dstIndex コピー先(this.data)のthis.indexからの相対位置
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
   * @param str
   */
  public void setString(String str) {
    this.fillBytes((byte) 0x20, this.data.length);
    try {
      this.setBytes(str.getBytes("SHIFT-JIS"));
    } catch (Exception e) {
      e.printStackTrace();
    }
  }

  /**
   * TODO 暫定的な実装(対応未定)
   *
   * @param pointer
   */
  public void setPointer(int pointer) {
    System.err.println("setPointer is not implemented");
  }

  /**
   * this.dataにバイト配列を書き込む
   *
   * @param bytes 書き込むバイト配列
   */
  public void set(byte[] bytes) {
    for (int i = 0; i < bytes.length && this.index + i < this.data.length; ++i) {
      this.setByte(i, bytes[i]);
    }
  }

  public void set(byte value) {
    this.setByte(0, value);
  }

  /**
   * this.dataにshort型のvalueを2バイトで書き込む
   *
   * @param value this.dataに書き込むshort型の値
   */
  public void set(short value) {
    ByteBuffer.wrap(this.data, this.index, 2).putShort(value);
  }

  /**
   * this.dataにint型のvalueを4バイトで書き込む
   *
   * @param value this.dataに書き込むint型の値
   */
  public void set(int value) {
    ByteBuffer.wrap(this.data, this.index, 4).putInt(value);
  }

  /**
   * this.dataにlong型のvalueを8バイトで書き込む
   *
   * @param value this.dataに書き込むlong型の値
   */
  public void set(long value) {
    ByteBuffer.wrap(this.data, this.index, 8).putLong(value);
  }

  /**
   * this.dataにdouble型のvalueを4バイトで書き込む
   *
   * @param value this.dataに書き込むlong型の値
   */
  public void set(double value) {
    ByteBuffer.wrap(this.data, this.index, 8).putDouble(value);
  }

  public void set(CobolDataStorage other) {
    this.set(other.intValue());
  }

  /**
   * this.dataにindexバイト目から4バイトでvalueを書き込む
   *
   * @param value
   * @param index
   */
  public void set(int value, int index) {
    ByteBuffer buffer = ByteBuffer.wrap(this.data, this.index + index, 4);
    buffer.putInt(value);
  }

  public boolean isSame(CobolDataStorage other) {
    return this.data == other.data && this.index == other.index;
  }

  /**
   * this.dataから2バイトを読み込んでshort型として返す
   *
   * @return this.dataから読み込んだ2バイトデータをshortに変換したデータ
   */
  public short shortValue() {
    return ByteBuffer.wrap(this.data, this.index, Short.BYTES).getShort();
  }

  /**
   * this.dataから4バイトを読み込んでint型として返す
   *
   * @return this.dataから読み込んだ2バイトデータをintに変換したデータ
   */
  public int intValue() {
    return ByteBuffer.wrap(this.data, this.index, Integer.BYTES).getInt();
  }

  /**
   * this.dataから2バイトを読み込んでlong型として返す
   *
   * @return this.dataから読み込んだ4バイトデータをlongに変換したデータ
   */
  public long longValue() {
    return ByteBuffer.wrap(this.data, this.index, Long.BYTES).getLong();
  }

  public double doubleValue() {
    return ByteBuffer.wrap(this.data, this.index, Double.BYTES).getDouble();
  }

  /**
   * メンバ変数indexの値がthis.index+indexであるようなCobolDataStorageのインスタンスを返す
   * メンバ変数indexの値をthis.indexから引数indexだけ変位させたCobolDataStorageのインスタンスを返す
   *
   * @param index this.indexからの相対位置
   * @return メンバ変数indexの値がthis.index+indexであるようなCobolDataStorageのインスタンス
   */
  public CobolDataStorage getSubDataStorage(int index) {
    return new CobolDataStorage(this.data, this.index + index);
  }

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
        public int run(long a, long b) {
          return Long.compareUnsigned(a, b);
        }
      };

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

  public void addBinary(long n, int numOfBytes, boolean signed, boolean isBigEndian) {
    long x = this.toLong(numOfBytes, signed, isBigEndian);
    x += n;
    this.fromLong(numOfBytes, isBigEndian, x);
  }

  public void subBinary(long n, int numOfBytes, boolean signed, boolean isBigEndian) {
    long x = this.toLong(numOfBytes, signed, isBigEndian);
    x -= n;
    this.fromLong(numOfBytes, isBigEndian, x);
  }

  // n numofBytes signed isBigEndian
  public int cmpU8Binary(long n) {
    return compareToBinary(n, 1, false, true);
  }

  public int cmpS8Binary(long n) {
    return compareToBinary(n, 1, true, true);
  }

  public int cmpU16Binary(long n) {
    return compareToBinary(n, 2, false, true);
  }

  public int cmpS16Binary(long n) {
    return compareToBinary(n, 2, true, true);
  }

  public int cmpU24Binary(long n) {
    return compareToBinary(n, 3, false, true);
  }

  public int cmpS24Binary(long n) {
    return compareToBinary(n, 3, true, true);
  }

  public int cmpU32Binary(long n) {
    return compareToBinary(n, 4, false, true);
  }

  public int cmpS32Binary(long n) {
    return compareToBinary(n, 4, true, true);
  }

  public int cmpU40Binary(long n) {
    return compareToBinary(n, 5, false, true);
  }

  public int cmpS40Binary(long n) {
    return compareToBinary(n, 5, true, true);
  }

  public int cmpU48Binary(long n) {
    return compareToBinary(n, 6, false, true);
  }

  public int cmpS48Binary(long n) {
    return compareToBinary(n, 6, true, true);
  }

  public int cmpU56Binary(long n) {
    return compareToBinary(n, 7, false, true);
  }

  public int cmpS56Binary(long n) {
    return compareToBinary(n, 7, true, true);
  }

  public int cmpU64Binary(long n) {
    return compareToBinary(n, 8, false, true);
  }

  public int cmpS64Binary(long n) {
    return compareToBinary(n, 8, true, true);
  }

  public int cmpAlignU8Binary(long n) {
    return compareToBinary(n, 1, false, true);
  }

  public int cmpAlignS8Binary(long n) {
    return compareToBinary(n, 1, true, true);
  }

  public int cmpAlignU16Binary(long n) {
    return compareToBinary(n, 2, false, true);
  }

  public int cmpAlignS16Binary(long n) {
    return compareToBinary(n, 2, true, true);
  }

  public int cmpAlignU32Binary(long n) {
    return compareToBinary(n, 4, false, true);
  }

  public int cmpAlignS32Binary(long n) {
    return compareToBinary(n, 4, true, true);
  }

  public int cmpAlignU64Binary(long n) {
    return compareToBinary(n, 8, false, true);
  }

  public int cmpAlignS64Binary(long n) {
    return compareToBinary(n, 8, true, true);
  }

  // n numofBytes signed isBigEndian
  public void addU8Binary(long n) {
    addBinary(n, 1, false, true);
  }

  public void addS8Binary(long n) {
    addBinary(n, 1, true, true);
  }

  public void addU16Binary(long n) {
    addBinary(n, 2, false, true);
  }

  public void addS16Binary(long n) {
    addBinary(n, 2, true, true);
  }

  public void addU24Binary(long n) {
    addBinary(n, 3, false, true);
  }

  public void addS24Binary(long n) {
    addBinary(n, 3, true, true);
  }

  public void addU32Binary(long n) {
    addBinary(n, 4, false, true);
  }

  public void addS32Binary(long n) {
    addBinary(n, 4, true, true);
  }

  public void addU40Binary(long n) {
    addBinary(n, 5, false, true);
  }

  public void addS40Binary(long n) {
    addBinary(n, 5, true, true);
  }

  public void addU48Binary(long n) {
    addBinary(n, 6, false, true);
  }

  public void addS48Binary(long n) {
    addBinary(n, 6, true, true);
  }

  public void addU56Binary(long n) {
    addBinary(n, 7, false, true);
  }

  public void addS56Binary(long n) {
    addBinary(n, 7, true, true);
  }

  public void addU64Binary(long n) {
    addBinary(n, 8, false, true);
  }

  public void addS64Binary(long n) {
    addBinary(n, 8, true, true);
  }

  public void addAlignU8Binary(long n) {
    addBinary(n, 1, false, true);
  }

  public void addAlignS8Binary(long n) {
    addBinary(n, 1, true, true);
  }

  public void addAlignU16Binary(long n) {
    addBinary(n, 2, false, true);
  }

  public void addAlignS16Binary(long n) {
    addBinary(n, 2, true, true);
  }

  public void addAlignU32Binary(long n) {
    addBinary(n, 4, false, true);
  }

  public void addAlignS32Binary(long n) {
    addBinary(n, 4, true, true);
  }

  public void addAlignU64Binary(long n) {
    addBinary(n, 8, false, true);
  }

  public void addAlignS64Binary(long n) {
    addBinary(n, 8, true, true);
  }

  // n numofBytes signed isBigEndian
  public void subU8Binary(long n) {
    subBinary(n, 1, false, true);
  }

  public void subS8Binary(long n) {
    subBinary(n, 1, true, true);
  }

  public void subU16Binary(long n) {
    subBinary(n, 2, false, true);
  }

  public void subS16Binary(long n) {
    subBinary(n, 2, true, true);
  }

  public void subU24Binary(long n) {
    subBinary(n, 3, false, true);
  }

  public void subS24Binary(long n) {
    subBinary(n, 3, true, true);
  }

  public void subU32Binary(long n) {
    subBinary(n, 4, false, true);
  }

  public void subS32Binary(long n) {
    subBinary(n, 4, true, true);
  }

  public void subU40Binary(long n) {
    subBinary(n, 5, false, true);
  }

  public void subS40Binary(long n) {
    subBinary(n, 5, true, true);
  }

  public void subU48Binary(long n) {
    subBinary(n, 6, false, true);
  }

  public void subS48Binary(long n) {
    subBinary(n, 6, true, true);
  }

  public void subU56Binary(long n) {
    subBinary(n, 7, false, true);
  }

  public void subS56Binary(long n) {
    subBinary(n, 7, true, true);
  }

  public void subU64Binary(long n) {
    subBinary(n, 8, false, true);
  }

  public void subS64Binary(long n) {
    subBinary(n, 8, true, true);
  }

  public void subAlignU8Binary(long n) {
    subBinary(n, 1, false, true);
  }

  public void subAlignS8Binary(long n) {
    subBinary(n, 1, true, true);
  }

  public void subAlignU16Binary(long n) {
    subBinary(n, 2, false, true);
  }

  public void subAlignS16Binary(long n) {
    subBinary(n, 2, true, true);
  }

  public void subAlignU32Binary(long n) {
    subBinary(n, 4, false, true);
  }

  public void subAlignS32Binary(long n) {
    subBinary(n, 4, true, true);
  }

  public void subAlignU64Binary(long n) {
    subBinary(n, 8, false, true);
  }

  public void subAlignS64Binary(long n) {
    subBinary(n, 8, true, true);
  }

  // n numofBytes signed isBigEndian
  public int cmpSwpU8Binary(long n) {
    return compareToBinary(n, 1, false, true);
  }

  public int cmpSwpS8Binary(long n) {
    return compareToBinary(n, 1, true, true);
  }

  public int cmpSwpU16Binary(long n) {
    return compareToBinary(n, 2, false, true);
  }

  public int cmpSwpS16Binary(long n) {
    return compareToBinary(n, 2, true, true);
  }

  public int cmpSwpU24Binary(long n) {
    return compareToBinary(n, 3, false, true);
  }

  public int cmpSwpS24Binary(long n) {
    return compareToBinary(n, 3, true, true);
  }

  public int cmpSwpU32Binary(long n) {
    return compareToBinary(n, 4, false, true);
  }

  public int cmpSwpS32Binary(long n) {
    return compareToBinary(n, 4, true, true);
  }

  public int cmpSwpU40Binary(long n) {
    return compareToBinary(n, 5, false, true);
  }

  public int cmpSwpS40Binary(long n) {
    return compareToBinary(n, 5, true, true);
  }

  public int cmpSwpU48Binary(long n) {
    return compareToBinary(n, 6, false, true);
  }

  public int cmpSwpS48Binary(long n) {
    return compareToBinary(n, 6, true, true);
  }

  public int cmpSwpU56Binary(long n) {
    return compareToBinary(n, 7, false, true);
  }

  public int cmpSwpS56Binary(long n) {
    return compareToBinary(n, 7, true, true);
  }

  public int cmpSwpU64Binary(long n) {
    return compareToBinary(n, 8, false, true);
  }

  public int cmpSwpS64Binary(long n) {
    return compareToBinary(n, 8, true, true);
  }

  public int cmpSwpAlignU8Binary(long n) {
    return compareToBinary(n, 1, false, true);
  }

  public int cmpSwpAlignS8Binary(long n) {
    return compareToBinary(n, 1, true, true);
  }

  public int cmpSwpAlignU16Binary(long n) {
    return compareToBinary(n, 2, false, true);
  }

  public int cmpSwpAlignS16Binary(long n) {
    return compareToBinary(n, 2, true, true);
  }

  public int cmpSwpAlignU32Binary(long n) {
    return compareToBinary(n, 4, false, true);
  }

  public int cmpSwpAlignS32Binary(long n) {
    return compareToBinary(n, 4, true, true);
  }

  public int cmpSwpAlignU64Binary(long n) {
    return compareToBinary(n, 8, false, true);
  }

  public int cmpSwpAlignS64Binary(long n) {
    return compareToBinary(n, 8, true, true);
  }

  // n numofBytes signed isBigEndian
  public void addSwpU8Binary(long n) {
    addBinary(n, 1, false, true);
  }

  public void addSwpS8Binary(long n) {
    addBinary(n, 1, true, true);
  }

  public void addSwpU16Binary(long n) {
    addBinary(n, 2, false, true);
  }

  public void addSwpS16Binary(long n) {
    addBinary(n, 2, true, true);
  }

  public void addSwpU24Binary(long n) {
    addBinary(n, 3, false, true);
  }

  public void addSwpS24Binary(long n) {
    addBinary(n, 3, true, true);
  }

  public void addSwpU32Binary(long n) {
    addBinary(n, 4, false, true);
  }

  public void addSwpS32Binary(long n) {
    addBinary(n, 4, true, true);
  }

  public void addSwpU40Binary(long n) {
    addBinary(n, 5, false, true);
  }

  public void addSwpS40Binary(long n) {
    addBinary(n, 5, true, true);
  }

  public void addSwpU48Binary(long n) {
    addBinary(n, 6, false, true);
  }

  public void addSwpS48Binary(long n) {
    addBinary(n, 6, true, true);
  }

  public void addSwpU56Binary(long n) {
    addBinary(n, 7, false, true);
  }

  public void addSwpS56Binary(long n) {
    addBinary(n, 7, true, true);
  }

  public void addSwpU64Binary(long n) {
    addBinary(n, 8, false, true);
  }

  public void addSwpS64Binary(long n) {
    addBinary(n, 8, true, true);
  }

  public void addSwpAlignU8Binary(long n) {
    addBinary(n, 1, false, true);
  }

  public void addSwpAlignS8Binary(long n) {
    addBinary(n, 1, true, true);
  }

  public void addSwpAlignU16Binary(long n) {
    addBinary(n, 2, false, true);
  }

  public void addSwpAlignS16Binary(long n) {
    addBinary(n, 2, true, true);
  }

  public void addSwpAlignU32Binary(long n) {
    addBinary(n, 4, false, true);
  }

  public void addSwpAlignS32Binary(long n) {
    addBinary(n, 4, true, true);
  }

  public void addSwpAlignU64Binary(long n) {
    addBinary(n, 8, false, true);
  }

  public void addSwpAlignS64Binary(long n) {
    addBinary(n, 8, true, true);
  }

  // n numofBytes signed isBigEndian
  public void subSwpU8Binary(int n) {
    subBinary(n, 1, false, true);
  }

  public void subSwpS8Binary(int n) {
    subBinary(n, 1, true, true);
  }

  public void subSwpU16Binary(int n) {
    subBinary(n, 2, false, true);
  }

  public void subSwpS16Binary(int n) {
    subBinary(n, 2, true, true);
  }

  public void subSwpU24Binary(int n) {
    subBinary(n, 3, false, true);
  }

  public void subSwpS24Binary(int n) {
    subBinary(n, 3, true, true);
  }

  public void subSwpU32Binary(int n) {
    subBinary(n, 4, false, true);
  }

  public void subSwpS32Binary(int n) {
    subBinary(n, 4, true, true);
  }

  public void subSwpU40Binary(int n) {
    subBinary(n, 5, false, true);
  }

  public void subSwpS40Binary(int n) {
    subBinary(n, 5, true, true);
  }

  public void subSwpU48Binary(int n) {
    subBinary(n, 6, false, true);
  }

  public void subSwpS48Binary(int n) {
    subBinary(n, 6, true, true);
  }

  public void subSwpU56Binary(int n) {
    subBinary(n, 7, false, true);
  }

  public void subSwpS56Binary(int n) {
    subBinary(n, 7, true, true);
  }

  public void subSwpU64Binary(int n) {
    subBinary(n, 8, false, true);
  }

  public void subSwpS64Binary(int n) {
    subBinary(n, 8, true, true);
  }

  public void subSwpAlignU8Binary(int n) {
    subBinary(n, 1, false, true);
  }

  public void subSwpAlignS8Binary(int n) {
    subBinary(n, 1, true, true);
  }

  public void subSwpAlignU16Binary(int n) {
    subBinary(n, 2, false, true);
  }

  public void subSwpAlignS16Binary(int n) {
    subBinary(n, 2, true, true);
  }

  public void subSwpAlignU32Binary(int n) {
    subBinary(n, 4, false, true);
  }

  public void subSwpAlignS32Binary(int n) {
    subBinary(n, 4, true, true);
  }

  public void subSwpAlignU64Binary(int n) {
    subBinary(n, 8, false, true);
  }

  public void subSwpAlignS64Binary(int n) {
    subBinary(n, 8, true, true);
  }

  public void setSwpU16Binary(int n) {
    this.fromLong(2, true, n);
  }

  public void setSwpS16Binary(int n) {
    this.fromLong(2, true, n);
  }

  public void setSwpU24Binary(int n) {
    this.fromLong(3, true, n);
  }

  public void setSwpS24Binary(int n) {
    this.fromLong(3, true, n);
  }

  public void setSwpU32Binary(int n) {
    this.fromLong(4, true, n);
  }

  public void setSwpS32Binary(int n) {
    this.fromLong(4, true, n);
  }

  public void setSwpU40Binary(int n) {
    this.fromLong(5, true, n);
  }

  public void setSwpS40Binary(int n) {
    this.fromLong(5, true, n);
  }

  public void setSwpU48Binary(int n) {
    this.fromLong(6, true, n);
  }

  public void setSwpS48Binary(int n) {
    this.fromLong(6, true, n);
  }

  public void setSwpU56Binary(int n) {
    this.fromLong(7, true, n);
  }

  public void setSwpS56Binary(int n) {
    this.fromLong(7, true, n);
  }

  public void setSwpU64Binary(int n) {
    this.fromLong(8, true, n);
  }

  public void setSwpS64Binary(int n) {
    this.fromLong(8, true, n);
  }

  /**
   * libcob/numeric.cのcob_cmp_numdispの実装
   *
   * @param size
   * @param n
   * @return
   */
  public int cmpNumdisp(int size, long n) {
    int p = 0;
    int val = 0;
    for (int inc = 0; inc < size; ++inc, ++p) {
      val = (val * 10) + (this.getByte(p) - '0');
    }
    return (val < n) ? -1 : (val > n) ? 1 : 0;
  }

  /**
   * libcob/numeric.cのcob_cmp_long_numdispの実装
   *
   * @param size
   * @param n
   * @return
   */
  public int cmpLongNumdisp(int size, long n) {
    return this.cmpNumdisp(size, n);
  }

  /**
   * @param size
   * @param n
   * @return
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
  public long bswap_16() {
    return this.toLong(2, true, true);
  }

  public long bswap_32() {
    return this.toLong(4, true, true);
  }

  public long bswap_64() {
    return this.toLong(8, true, true);
  }
}
