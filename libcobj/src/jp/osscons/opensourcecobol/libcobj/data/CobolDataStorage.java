/*
 * Copyright (C) 2020 TOKYO SYSTEM HOUSE Co., Ltd.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, 51 Franklin Street, Fifth Floor
 * Boston, MA 02110-1301 USA
 */

package jp.osscons.opensourcecobol.libcobj.data;

import java.nio.ByteBuffer;

import jp.osscons.opensourcecobol.libcobj.common.CobolModule;
/**
 * COBOLのデータを保存するバイト配列を扱うクラス
 */
public class CobolDataStorage {

	/**
	 * データを保存するバイト配列
	 */
	private byte[] data;
	/**
	 * このクラスの扱うデータが保存する領域のバイト配列中での相対位置
	 */
	private int index;

	/**
	 * コンストラクタ.引数で指定された長さ分のバイト配列を確保する.
	 * @param size バイト配列の長さ
	 */
	public CobolDataStorage(int size) {
		this.data = new byte[size];
		this.index = 0;
	}

	/**
	 * コンストラクタ.データを保存するバイト配列と保存する領域の相対位置を引数で指定する.
	 * @param data データを保存するバイト配列
	 * @param index このクラスの扱うデータが保存する領域のバイト配列中での相対位置
	 */
	public CobolDataStorage(byte[] data, int index) {
		this.data = data;
		this.index = index;
	}

	/**
	 * コンストラクタ.データを保存するバイト配列を引数で指定する.
	 * @param data データを保存するバイト配列
	 */
	public CobolDataStorage(byte[] data) {
		this(data, 0);
	}

	public CobolDataStorage() {
	}

	public int getIndex() {
		return this.index;
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

	/**
	 * コンストラクタ.文字列からバイト配列を構成する.
	 * @param str
	 */
	public CobolDataStorage(String str) {
		try {
			byte[] bytes = str.getBytes("SHIFT-JIS");
			this.data = bytes;
			this.index = 0;
		} catch(Exception e) {
			e.printStackTrace();
			this.data = new byte[0];
			this.index = 0;
		}
	}

	/**
	 * 保持するバイト配列のコピーを返す
	 * @return
	 */
	public byte[] getData() {
		return this.getData(0);
	}

	/**
	 * バイト配列の引数で指定した相対位置から末尾までをコピーした配列を返す。
	 * @param index コピーの開始位置(this.indexバイト目を基準とする)
	 * @return 開始位置から末尾までのデータをコピーしたバイト配列
	 */
	public byte[] getData(int index) {
		byte[] result = new byte[this.data.length - this.index - index];

		System.arraycopy(this.data,  this.index + index, result, 0, result.length);

		return result;
	}

	/**
	 * バイト配列の引数で指定した相対位置から指定した長さをコピーした配列を返す。
	 * @param index  コピーの開始位置(this.indexバイト目を基準とする)
	 * @param length コピーする長さ(バイト数)
	 * @return 開始位置からlengthバイト分の配列
	 */
	public byte[] getByteArray(int index, int length) {
		byte[] result = new byte[length];
		System.arraycopy(this.data, this.index + index, result, 0, length);
		return result;
	}

	/**
	 * C言語のmemcpy
	 * @param buf
	 * @param size
	 */
	public void memcpy(CobolDataStorage buf, int size) {
		for(int i=0; i<size; ++i) {
			this.setByte(i, buf.getByte(i));
		}
	}

	/**
	 * C言語のmemcpy
	 * @param buf
	 * @param size
	 */
	public void memcpy(byte[] buf, int size) {
		for(int i=0; i<size; ++i) {
			this.setByte(i, buf[i]);
		}
	}
	
	/**
	 * C言語のmemcpy (offset指定あり)
	 * @param offset
	 * @param buf
	 * @param size
	 */
	public void memcpy(int offset, byte[] buf, int size) {
		for(int i=0; i<size; ++i) {
			this.setByte(offset + i, buf[i]);
		}
	}

	public void memcpy(byte[] buf) {
		this.memcmp(buf, buf.length);
	}

	/**
	 *  C言語のmemset
	 * @param ch
	 * @param size
	 */
	public void memset(byte ch, int size) {
		for(int i=0; i<size; ++i) {
			this.setByte(i, ch);
		}
	}

	/**
	 *  C言語のmemcmp
	 * @param buf
	 * @param size
	 * @return
	 */
	public int memcmp(byte[] buf, int size) {
		for(int i=0; i<size; ++i) {
			if(this.getByte(i) != buf[i]) {
				return this.getByte(i) - buf[i];
			}
		}
		return 0;
	}
	
	/**
	 *  C言語のmemcmp
	 * @param buf
	 * @param size
	 * @return
	 */
	public int memcmp(String buf, int size) {
		return this.memcmp(buf.getBytes(), size);
	}

	/**
	 *  C言語のmemcmp
	 * @param buf
	 * @param size
	 * @return
	 */
	public int memcmp(CobolDataStorage buf, int size) {
		for(int i=0; i<size; ++i) {
			if(this.getByte(i) != buf.getByte(i)) {
				return this.getByte(i) - buf.getByte(i);
			}
		}
		return 0;
	}

	/**
	 * 引数で与えられたバイト配列をthis.dataの先頭からコピーする
	 * @param data
	 */
	public void setData(byte[] data) {
		setData(data, 0);
	}

	/**
	 * 引数で指定されたバイト配列をthis.dataの引数で指定された位置からコピーする
	 * @param data コピー元のバイト配列
	 * @param index this.byteのコピー開始位置(this.indexバイト目を基準とする)
	 */
	public void setData(byte[] data, int index) {
		int length = (data.length <=  this.data.length - this.index - index) ?
				data.length : this.data.length - this.index - index;

		System.arraycopy(data, 0, this.data, this.index + index, length);
	}

	/**
	 * 引数で指定された分だけindexを変位させたCobolDataStorageクラスのインスタンスを作成する
	 * @param index 新たに作成するCobolDataStorageクラスのインスタンスのメンバ変数indexのthis.indexからの相対位置
	 * @return 新たに作成したCobolDataStorageクラスのインスタンス
	 */
	public CobolDataStorage getDataStorage(int index) {
		return new CobolDataStorage(this.data, this.index + index);
	}

	/**
	 * 指定のバイト配列中での位置に指定の値を代入する
	 * @param index 代入先のバイト配列中の位置.this.indexを基準とする.
	 * @param value 代入する値
	 */
	public void setByte(int index, byte value) {
		this.data[this.index + index]  = value;
	}

	public void setByte(byte value) {
		this.setByte(0, value);
	}

	/**
	 * 指定のバイト配列中での位置に格納された値を返す
	 * @param index バイト配列中での位置.this.indexを基準とする.
	 * @return 指定のバイト配列中での位置に格納された値
	 */
	public byte getByte(int index) {
		return this.data[this.index + index];
	}

	/**
	 * バイト配列のthis.indexバイト目からsizeバイトの範囲にvalueを代入する
	 * @param value 代入する値
	 * @param size 代入先のバイト数
	 */
	public void fillBytes(byte value, int size) {
		fillBytes(0, value, size);
	}

	/**
	 * バイト配列のthis.indexバイト目からsizeバイトの範囲にvalueを代入する
	 * @param value 代入する値
	 * @param size 代入先のバイト数
	 */
	public void fillBytes(int value, int size) {
		fillBytes(0, (byte)value, size);
	}

	/**
	 * バイト配列の0バイト目からsizeバイトの範囲にvalueを代入する
	 * @param index コピーの開始位置(this.indexが基準)
	 * @param value 代入する値
	 * @param size 代入先のバイト数
	 */
	public void fillBytes(int index, byte value, int size) {
		for(int i=0; i<size; ++i) {
			this.setByte(i + index, value);
		}
	}

	/**
	 * 指定のバイト配列に格納された値をthis.dataのthis.indexバイト目以降へコピーする.
	 * @param bytes コピー元のバイト配列
	 */
	public void setBytes(byte[] bytes) {
		for(int i=0; i<bytes.length; ++i) {
			this.setByte(i, bytes[i]);
		}
	}

	/**
	 *
	 * 指定のバイト配列に格納された値を
	 * this.dataのthis.indexバイト目以降へlengthバイトだけコピーする.
	 * @param bytes コピー元の配列
	 * @param length コピーするバイト数
	 */
	public void setBytes(byte[] bytes, int length) {
		for(int i=0; i<length; ++i) {
			this.setByte(i, bytes[i]);
		}
	}

	/**
	 *
	 * 指定のバイト配列に格納された値を
	 * this.dataのthis.index+indexバイト目以降へlengthバイトだけコピーする.
	 * @param index コピー先のthis.indexからの相対位置
	 * @param bytes コピー元の配列
	 * @param length コピーするバイト数
	 */
	public void setBytes(int index, byte[] bytes, int length) {
		for(int i=0; i<length; ++i) {
			this.setByte(i + index, bytes[i]);
		}
	}

	/**
	 * 指定した文字列をバイト配列にコピーする
	 * @param str コピー元の文字列
	 * @param length コピーするバイト数
	 */
	public void setBytes(String str, int length) {
		try {
			this.setBytes(str.getBytes("SHIFT-JIS"), length);
		} catch(Exception e) {
			e.printStackTrace();
		}
	}

	/**
	 * 指定されたCobolDataStorageのインスタンスから,lengthバイトだけデータをコピーする
	 * @param data コピー元のCobolDataStorage
	 * @param length コピーするバイト数
	 */
	public void setBytes(CobolDataStorage data, int length) {
		this.setBytes(data, length, 0, 0);
	}

	/**
	 * 指定されたCobolDataStorageのインスタンスから,lengthバイトだけデータをコピーする
	 * @param data コピー元のCobolDataStorage
	 * @param length コピーするバイト数
	 * @param dstIndex コピー先(this.data)のthis.indexからの相対位置
	 */
	public void setBytes(CobolDataStorage data, int length, int dstIndex) {
		this.setBytes(data, length, dstIndex, 0);
	}

	/**
	 * 指定されたCobolDataStorageのインスタンスから,lengthバイトだけデータをコピーする
	 * @param data コピー元のCobolDataStorage
	 * @param length コピーするバイト数
	 * @param dstIndex コピー先(this.data)のthis.indexからの相対位置
	 * @param srcIndex コピー元バイト配列中の相対位置
	 */
	public void setBytes(CobolDataStorage data, int length, int dstIndex, int srcIndex) {
		for(int i=0; i<length; ++i) {
			this.setByte(i + dstIndex, data.getByte(i + srcIndex));
		}
	}

	/**
	 * TODO 暫定的な実装
	 * @param str
	 */
	public void setString(String str) {
		this.fillBytes((byte)0x20, this.data.length);
		try {
			this.setBytes(str.getBytes("SHIFT-JIS"));
		} catch(Exception e) {
			e.printStackTrace();
		}
	}

	/**
	 * TODO 暫定的な実装(対応未定)
	 * @param pointer
	 */
	public void setPointer(int pointer) {

	}

	/**
	 * this.dataにバイト配列を書き込む
	 * @param bytes 書き込むバイト配列
	 */
	public void set(byte[] bytes) {
		for(int i=0; i < bytes.length; ++i) {
			this.setByte(i, bytes[i]);
		}
	}

	/**
	 * this.dataにshort型のvalueを2バイトで書き込む
	 * @param value this.dataに書き込むshort型の値
	 */
	public void set(short value) {
		byte bytes[] = ByteBuffer.allocate(Short.BYTES).putShort(value).array();
		set(bytes);
	}

	/**
	 * this.dataにint型のvalueを4バイトで書き込む
	 * @param value this.dataに書き込むint型の値
	 */
	public void set(int value) {
		byte bytes[] = ByteBuffer.allocate(Integer.BYTES).putInt(value).array();
		set(bytes);
	}

	/**
	 * this.dataにindexバイト目から4バイトでvalueを書き込む
	 * @param value
	 * @param index
	 */
	public void set(int value, int index) {
		ByteBuffer buffer = ByteBuffer.wrap(this.data, this.index + index, 4);
		buffer.putInt(value);
	}

	/**
	 * this.dataにlong型のvalueを8バイトで書き込む
	 * @param value this.dataに書き込むlong型の値
	 */
	public void set(long value) {
		byte bytes[] = ByteBuffer.allocate(Long.BYTES).putLong(value).array();
		set(bytes);
	}

	/**
	 * this.dataから2バイトを読み込んでshort型として返す
	 * @return this.dataから読み込んだ2バイトデータをshortに変換したデータ
	 */
	public short shortValue() {
		return ByteBuffer.wrap(this.data, this.index, Short.BYTES).getShort();
	}

	/**
	 * this.dataから4バイトを読み込んでint型として返す
	 * @return this.dataから読み込んだ2バイトデータをintに変換したデータ
	 */
	public int intValue() {
		return ByteBuffer.wrap(this.data, this.index, Integer.BYTES).getInt();
	}

	/**
	 * this.dataから2バイトを読み込んでlong型として返す
	 * @return this.dataから読み込んだ4バイトデータをlongに変換したデータ
	 */
	public long longValue() {
		return ByteBuffer.wrap(this.data, this.index, Long.BYTES).getLong();
	}

	/**
	 * メンバ変数indexの値がthis.index+indexであるようなCobolDataStorageのインスタンスを返す
	 * メンバ変数indexの値をthis.indexから引数indexだけ変位させたCobolDataStorageのインスタンスを返す
	 * @param index this.indexからの相対位置
	 * @return メンバ変数indexの値がthis.index+indexであるようなCobolDataStorageのインスタンス
	 */
	public CobolDataStorage getSubDataStorage(int index) {
		return new CobolDataStorage(this.data, this.index + index);
	}
	
	public CobolDataStorage getSubDataStorage(long index) {
		return this.getDataStorage((int)index);
	}

	/**
	 * libcob/codegen.hのcob_setswp_u24_binaryの実装
	 */
	public void setSwpU24Binary(int n) {
		for(int i=2; i>=0; --i) {
			byte x = (byte) (((n >>> (8 * i)) & 0xFF));
			this.setByte(2-i, x);
		}
	}

	/**
	 * libcob/codegen.hのcob_setswp_s24_binaryの実装
	 * @param n
	 */
	public void setSwpS24Binary(int n) {
		this.setSwpU24Binary(n);
	}

	//以下NumericBinaryのためのメソッド
	/**
	 * libcob/codegen.hのcob_setswp_u32_binaryの実装
	 * @param n
	 */
	public void setSwpU32Binary(int n) {

		byte x = (byte) (((n >> 24) & 0x000000FF));
		this.setByte(0, x);

		x = (byte) ((n >> 16) & 0x000000FF);
		this.setByte(1, x);

		x = (byte) ((n >> 8) & 0x000000FF) ;
		this.setByte(2, x);

		x = (byte) (n & 0x000000FF);
		this.setByte(3, x);
	}

	/**
	 * libcob/codegen.hのcob_setswp_s32_binaryの実装
	 * @param n
	 */
	public void setSwpS32Binary(int n) {
		this.setSwpU32Binary(n);
	}

	/**
	 * libcob/codegen.hのcob_setswp_u64_binaryの実装
	 */
	public void setSwpU64Binary(long n) {
		for(int i=7; i>=0; --i) {
			byte x = (byte) (((n >>> (8 * i)) & 0xFF));
			this.setByte(7-i, x);
		}
	}

	/**
	 * libcob/codegen.hのcob_setswp_u64_binaryの実装
	 */
	public void setSwpS64Binary(long n) {
		this.setSwpU64Binary(n);
	}

	/**
	 * libcob/codegen.hのcob_setswp_u16_binaryの実装
	 * @param n
	 */
	public void setSwpU16Binary(int n) {
		byte x = (byte) (((n >> 8) & 0x000000FF));
		this.setByte(0, x);

		x = (byte) (n & 0x000000FF);
		this.setByte(1, x);
	}

	/**
	 * libcob/codegen.hのcob_setswp_s16_binaryの実装
	 * @param n
	 */
	public void setSwpS16Binary(int n) {
		this.setSwpU16Binary(n);
	}

	/**
	 * libcob/codegen.hのcob_addswp_s16_binaryの実装
	 * @param val
	 */
	public void addSwpS16Binary(int val) {
		short n = (short)(this.getByte(0) << 8 | this.getByte(1));
		n += val;
		this.setByte(0, (byte)(n >> 8));
		this.setByte(1, (byte)(n));
	}

	/**
	 * COB_BSWAP_16マクロの実装
	 * @param val
	 * @return
	 */
	private static short BSWAP16(short val) {
		return (short)((val << 8) | (0x0f & (val >> 8)));
	}

	/**
	 * libcob/codegen.hのcob_cmpswp_s16_binaryの実装
	 * @param n
	 * @return
	 */
	public int cmpSwpS16Binary(int n) {
		ByteBuffer bb = ByteBuffer.wrap(this.getData());
		short val = bb.getShort();
		val = BSWAP16(val);
		if (val < n) {
			return -1;
		} else if(val == n) {
			return 0;
		} else {
			return 1;
		}
	}

	/**
	 * libcob/numeric.cのcob_cmp_numdispの実装
	 * @param size
	 * @param n
	 * @return
	 */
	public int cmpNumdisp(int size, int n) {
		int p=0;
		int val = 0;
		for(int inc=0; inc<size; ++inc, ++p) {
			val = (val * 10) + (this.getByte(p) - '0');
		}
		return (val < n) ? -1 : (val > n) ? 1 : 0;
	}

	/**
	 * libcob/numeric.cのcob_cmp_long_numdispの実装
	 * @param size
	 * @param n
	 * @return
	 */
	public int cmpLongNumdisp(int size, int n) {
		return this.cmpNumdisp(size, n);
	}

	/**
	 *
	 * @param size
	 * @param n
	 * @return
	 */
	public int cmpSignNumdisp(int size, int n) {
		int p = 0;
		int val = 0;
		for(int inc=0; inc<size-1; ++inc, ++p) {
			val = (val * 10) + (this.getByte(p) - '0');
		}
		val *= 10;
		if(this.getByte(p) >= '0' && this.getByte(p) <= '9') {
			val += (this.getByte(p) - '0');
		} else {
			if(CobolModule.getCurrentModule().display_sign != 0) {
				PairInt pair = this.getLongEbcdicSign(p, val);
				val = pair.first;
				if(pair.last != 0) {
					val = -val;
				}
			} else {
				val += (this.getByte(p) - '0');
				val = -val;
			}
		}
		return (val < n) ? -1 : (val > n) ? 1 : 0;
	}

	public int cmpLongSignNumdisp(int size, int n) {
		int p = 0;
		int val = 0;
		for(int inc=0; inc<size-1; ++inc, ++p) {
			val = (val * 10) + (this.getByte(p) - '0');
		}
		val *= 10;
		if(this.getByte(p) >= '0' && this.getByte(p) <= '9') {
			val += (this.getByte(p) - '0');
		} else {
			if(CobolModule.getCurrentModule().display_sign != 0) {
				PairInt pair = this.getLongEbcdicSign(p, val);
				val = pair.first;
				if(pair.last != 0) {
					val = -val;
				}
			} else {
				val += (this.getByte(p) - '0');
				val = -val;
			}
		}
		return (val < n) ? -1 : (val > n) ? 1 : 0;
	}

	/**
	 *
	 */
	private class PairInt {
		public int first;
		public int last;
		public PairInt(int a, int b) {
			first = a; last = b;
		}
	};

	public PairInt getEbcdicSign(int p, int val) {
		switch (this.getByte(p)) {
		case '{': return new PairInt(val, 0);
		case 'A': return new PairInt(val + 1, 0);
		case 'B': return new PairInt(val + 2, 0);
		case 'C': return new PairInt(val + 3, 0);
		case 'D': return new PairInt(val + 4, 0);
		case 'E': return new PairInt(val + 5, 0);
		case 'F': return new PairInt(val + 6, 0);
		case 'G': return new PairInt(val + 7, 0);
		case 'H': return new PairInt(val + 8, 0);
		case 'I': return new PairInt(val + 9, 0);
		case '}': return new PairInt(val, 1);
		case 'J': return new PairInt(val+1, 1);
		case 'K': return new PairInt(val+2, 1);
		case 'L': return new PairInt(val+3, 1);
		case 'M': return new PairInt(val+4, 1);
		case 'N': return new PairInt(val+5, 1);
		case 'O': return new PairInt(val+6, 1);
		case 'P': return new PairInt(val+7, 1);
		case 'Q': return new PairInt(val+8, 1);
		case 'R': return new PairInt(val+9, 1);
		}
		return new PairInt(val, 0);
	}


	public PairInt getLongEbcdicSign(int p, int val) {
		switch (this.getByte(p)) {
		case '{': return new PairInt(val, 0);
		case 'A': return new PairInt(val+1, 0);
		case 'B': return new PairInt(val+2, 0);
		case 'C': return new PairInt(val+3, 0);
		case 'D': return new PairInt(val+4, 0);
		case 'E': return new PairInt(val+5, 0);
		case 'F': return new PairInt(val+6, 0);
		case 'G': return new PairInt(val+7, 0);
		case 'H': return new PairInt(val+8, 0);
		case 'I': return new PairInt(val+9, 0);
		case '}': return new PairInt(val ,1);
		case 'J': return new PairInt(val+1 ,1);
		case 'K': return new PairInt(val+2 ,1);
		case 'L': return new PairInt(val+3 ,1);
		case 'M': return new PairInt(val+4 ,1);
		case 'N': return new PairInt(val+5 ,1);
		case 'O': return new PairInt(val+6, 1);
		case 'P': return new PairInt(val+7, 1);
		case 'Q': return new PairInt(val+8, 1);
		case 'R': return new PairInt(val+9, 1);
		}
		return new PairInt(val, 0);
	}

	//TODO 修正
	//opensource COBOLにはない実装 COB_BSWAP_32などの代替
	//本家opensource COBOLのcobc/codegen.cの894行目付近を参照
	public long getBinaryShort() {
		byte[] bytes = this.getByteArray(0, 2);
		return ByteBuffer.wrap(bytes).getShort();
	}

	public long getBinaryInt() {
		byte[] bytes = this.getByteArray(0, 4);
		return ByteBuffer.wrap(bytes).getInt();
	}

	public long getBinaryLong() {
		byte[] bytes = this.getByteArray(0, 8);
		return ByteBuffer.wrap(bytes).getLong();
	}
}