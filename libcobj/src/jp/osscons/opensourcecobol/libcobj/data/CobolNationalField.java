package jp.osscons.opensourcecobol.libcobj.data;

import java.io.UnsupportedEncodingException;
import java.math.BigDecimal;

import jp.osscons.opensourcecobol.libcobj.exceptions.CobolRuntimeException;

/**
 * PIC 文字列がN(5)やN(9)の変数を表現するクラス.
 */
public class CobolNationalField extends AbstractCobolField {
	
	public static int workReturnSize;

	/**
	 *  コンストラクタ
	 * @param size データを格納するバイト配列の長さ
	 * @param dataStorage データを格納するバイト配列を扱うオブジェクト
	 * @param attribute 変数に関する様々な情報を保持するオブジェクト
	 */
	public CobolNationalField(int size, CobolDataStorage dataStorage, CobolFieldAttribute attribute) {
		super(size, dataStorage, attribute);
	}

	/**
	 * this.dataの保持するバイト配列のコピーを返す
	 */
	@Override
	public byte[] getBytes() {
		return dataStorage.getData(size);
	}

	/**
	 * thisの文字列表現をかえす.(toStringだけで十分か?)
	 * @return thisの文字列表現
	 */
	@Override
	public String getString() {
		try {
			return new String(dataStorage.getData(), "SJIS");
		} catch (UnsupportedEncodingException e) {
			e.printStackTrace();
			throw new CobolRuntimeException(CobolRuntimeException.COBOL_FITAL_ERROR, "エンコードエラー");
		}
	}

	@Override
	public int getInt() {
		return 0;
	}

	@Override
	public double getDouble() {
		return 0;
	}

	@Override
	public CobolDecimal getDecimal() {
		return null;
	}

	/**
	 * 実装しないメソッド
	 */
	public int addPackedInt(int n) {
		throw new CobolRuntimeException(0, "実装しないコード");
	}

	/**
	 * 引数で与えらえられたデータからthisへの代入を行う
	 * @param field 代入元のデータ(AbstractCobolField型)
	 */
	@Override
	public void moveFrom(AbstractCobolField src) {

		byte[] pTmp;
		int size;
		AbstractCobolField src2;

		AbstractCobolField src1 = this.preprocessOfMoving(src);
		if(src1 == null) {
			return;
		}
		
		// Convert Numeric
		switch (src1.getAttribute().getType()) {
		case CobolFieldAttribute.COB_TYPE_NUMERIC_PACKED:
		case CobolFieldAttribute.COB_TYPE_NUMERIC_BINARY:
		case CobolFieldAttribute.COB_TYPE_NUMERIC_DOUBLE:
		case CobolFieldAttribute.COB_TYPE_NUMERIC_FLOAT:
			src1 = src1.getNumericField();
			break;
		}

		// HANKAKU TO ZENKAKU
		if (!src1.getAttribute().isTypeNational() && !src1.getAttribute().isTypeNationalEdited()) {
			workReturnSize = 0;
			pTmp = judge_hankakujpn_exist (src1);
			size = workReturnSize;
			CobolDataStorage pTmpStorage = new CobolDataStorage(size);
			pTmpStorage.setBytes(pTmp, size);
			CobolFieldAttribute attr =
					new CobolFieldAttribute(CobolFieldAttribute.COB_TYPE_ALPHANUMERIC, 0, 0, 0, null);
			src2 = new CobolAlphanumericField(size, pTmpStorage, attr);
		} else {
			src2 = src1;
		}

		cob_move_from_alphanum(src2);
	}

	/**
	 * CobolAlphanumericFieldからthisへの代入を行う
	 * @param field 代入元のデータ(AbstractCobolField型)
	 */
	private void cob_move_from_alphanum(AbstractCobolField field) {
		//TODO UTF8対応
		CobolDataStorage data1;
		CobolDataStorage data2;
		int size1;
		int size2;
		int len;
		//TODO
		final int COB_ZENCSIZ = 2;
		final byte[] COB_ZENBLK = {(byte) 0x81, (byte) 0x40};

		data1 = field.getDataStorage();
		size1 = field.getSize();
		data2 = this.getDataStorage();
		size2 = this.getSize();

		len = size2 - size1;

		if (size1 >= size2) {
			/* move string with truncation */
			if (this.getAttribute().isFlagJustified()) {
				data2.setBytes(data1, size2, 0, size1 - size2);
			} else {
				data2.setBytes(data1, size2);
			}
		} else {
			data2.fillBytes((byte) 0x20, size2);
			if (this.getAttribute().isFlagJustified()) {
				for (int i = 0; i < len; i += COB_ZENCSIZ) {
					if (len-i >= COB_ZENCSIZ) {
						data2.setBytes(i, COB_ZENBLK, COB_ZENCSIZ);
					}
				}
				data2.getSubDataStorage(len).memcpy(data1, size1);
			} else {
				for (int i = 0; i < len; i += COB_ZENCSIZ) {
					if (len-i >= COB_ZENCSIZ) {
						data2.setBytes(i + size1, COB_ZENBLK, COB_ZENCSIZ);
					}
				}
				data2.setBytes(data1, size1);
			}
		}
	}

	/**
	 * libcob/move.cのjudge_hankakujpn_existの実装
	 * @param src
	 * @return
	 */
	public static byte[] judge_hankakujpn_exist(AbstractCobolField src) {
		byte[] tmp_zenjpn_word = null;

		if(src.getSize() <= 0) {
			return null;
		}
		//TODO データのStrlenの差し替え
		if(src.getDataStorage().getByte(0) != 0) {
			tmp_zenjpn_word = han2zen(src.getDataStorage().getData(), src.getSize());
		}
		return tmp_zenjpn_word;
	}

	/**
	 * libcob/move.c han2zenの実装
	 * @param str
	 * @param size
	 * @return
	 */
	public static byte[] han2zen(byte[] str, int size) {
		byte[] buf;

		int c;
		int d = (byte) 0x00;
		int i, buf_index;

		buf = new byte[size * 2 + 1];

		for (i = 0, buf_index = 0; i < size; i++) {
			c = Byte.toUnsignedInt(str[i]);
			switch (c) {
			case 0x20:
				buf[buf_index] = (byte) 0x81;
				buf[buf_index + 1] = (byte) 0x40;
				buf_index += 2;
				break;
			case 0x21:
				buf[buf_index] = (byte) 0x81;
				buf[buf_index + 1] = (byte) 0x49;
				buf_index += 2;
				break;
			case 0x22:
				buf[buf_index] = (byte) 0x81;
				buf[buf_index + 1] = (byte) 0x68;
				buf_index += 2;
				break;
			case 0x23:
				buf[buf_index] = (byte) 0x81;
				buf[buf_index + 1] = (byte) 0x94;
				buf_index += 2;
				break;
			case 0x24:
				buf[buf_index] = (byte) 0x81;
				buf[buf_index + 1] = (byte) 0x90;
				buf_index += 2;
				break;
			case 0x25:
				buf[buf_index] = (byte) 0x81;
				buf[buf_index + 1] = (byte) 0x93;
				buf_index += 2;
				break;
			case 0x26:
				buf[buf_index] = (byte) 0x81;
				buf[buf_index + 1] = (byte) 0x95;
				buf_index += 2;
				break;
			case 0x27:
				buf[buf_index] = (byte) 0x81;
				buf[buf_index + 1] = (byte) 0x66;
				buf_index += 2;
				break;
			case 0x28:
				buf[buf_index] = (byte) 0x81;
				buf[buf_index + 1] = (byte) 0x69;
				buf_index += 2;
				break;
			case 0x29:
				buf[buf_index] = (byte) 0x81;
				buf[buf_index + 1] = (byte) 0x6A;
				buf_index += 2;
				break;
			case 0x2A:
				buf[buf_index] = (byte) 0x81;
				buf[buf_index + 1] = (byte) 0x96;
				buf_index += 2;
				break;
			case 0x2B:
				buf[buf_index] = (byte) 0x81;
				buf[buf_index + 1] = (byte) 0x7B;
				buf_index += 2;
				break;
			case 0x2C:
				buf[buf_index] = (byte) 0x81;
				buf[buf_index + 1] = (byte) 0x43;
				buf_index += 2;
				break;
			case 0x2D:
				buf[buf_index] = (byte) 0x81;
				buf[buf_index + 1] = (byte) 0x7C;
				buf_index += 2;
				break;
			case 0x2E:
				buf[buf_index] = (byte) 0x81;
				buf[buf_index + 1] = (byte) 0x44;
				buf_index += 2;
				break;
			case 0x2F:
				buf[buf_index] = (byte) 0x81;
				buf[buf_index + 1] = (byte) 0x5E;
				buf_index += 2;
				break;
			case 0x30:
				buf[buf_index] = (byte) 0x82;
				buf[buf_index + 1] = (byte) 0x4F;
				buf_index += 2;
				break;
			case 0x31:
				buf[buf_index] = (byte) 0x82;
				buf[buf_index + 1] = (byte) 0x50;
				buf_index += 2;
				break;
			case 0x32:
				buf[buf_index] = (byte) 0x82;
				buf[buf_index + 1] = (byte) 0x51;
				buf_index += 2;
				break;
			case 0x33:
				buf[buf_index] = (byte) 0x82;
				buf[buf_index + 1] = (byte) 0x52;
				buf_index += 2;
				break;
			case 0x34:
				buf[buf_index] = (byte) 0x82;
				buf[buf_index + 1] = (byte) 0x53;
				buf_index += 2;
				break;
			case 0x35:
				buf[buf_index] = (byte) 0x82;
				buf[buf_index + 1] = (byte) 0x54;
				buf_index += 2;
				break;
			case 0x36:
				buf[buf_index] = (byte) 0x82;
				buf[buf_index + 1] = (byte) 0x55;
				buf_index += 2;
				break;
			case 0x37:
				buf[buf_index] = (byte) 0x82;
				buf[buf_index + 1] = (byte) 0x56;
				buf_index += 2;
				break;
			case 0x38:
				buf[buf_index] = (byte) 0x82;
				buf[buf_index + 1] = (byte) 0x57;
				buf_index += 2;
				break;
			case 0x39:
				buf[buf_index] = (byte) 0x82;
				buf[buf_index + 1] = (byte) 0x58;
				buf_index += 2;
				break;
			case 0x3A:
				buf[buf_index] = (byte) 0x81;
				buf[buf_index + 1] = (byte) 0x46;
				buf_index += 2;
				break;
			case 0x3B:
				buf[buf_index] = (byte) 0x81;
				buf[buf_index + 1] = (byte) 0x47;
				buf_index += 2;
				break;
			case 0x3C:
				buf[buf_index] = (byte) 0x81;
				buf[buf_index + 1] = (byte) 0x83;
				buf_index += 2;
				break;
			case 0x3D:
				buf[buf_index] = (byte) 0x81;
				buf[buf_index + 1] = (byte) 0x81;
				buf_index += 2;
				break;
			case 0x3E:
				buf[buf_index] = (byte) 0x81;
				buf[buf_index + 1] = (byte) 0x84;
				buf_index += 2;
				break;
			case 0x3F:
				buf[buf_index] = (byte) 0x81;
				buf[buf_index + 1] = (byte) 0x48;
				buf_index += 2;
				break;
			case 0x40:
				buf[buf_index] = (byte) 0x81;
				buf[buf_index + 1] = (byte) 0x97;
				buf_index += 2;
				break;
			case 0x41:
				buf[buf_index] = (byte) 0x82;
				buf[buf_index + 1] = (byte) 0x60;
				buf_index += 2;
				break;
			case 0x42:
				buf[buf_index] = (byte) 0x82;
				buf[buf_index + 1] = (byte) 0x61;
				buf_index += 2;
				break;
			case 0x43:
				buf[buf_index] = (byte) 0x82;
				buf[buf_index + 1] = (byte) 0x62;
				buf_index += 2;
				break;
			case 0x44:
				buf[buf_index] = (byte) 0x82;
				buf[buf_index + 1] = (byte) 0x63;
				buf_index += 2;
				break;
			case 0x45:
				buf[buf_index] = (byte) 0x82;
				buf[buf_index + 1] = (byte) 0x64;
				buf_index += 2;
				break;
			case 0x46:
				buf[buf_index] = (byte) 0x82;
				buf[buf_index + 1] = (byte) 0x65;
				buf_index += 2;
				break;
			case 0x47:
				buf[buf_index] = (byte) 0x82;
				buf[buf_index + 1] = (byte) 0x66;
				buf_index += 2;
				break;
			case 0x48:
				buf[buf_index] = (byte) 0x82;
				buf[buf_index + 1] = (byte) 0x67;
				buf_index += 2;
				break;
			case 0x49:
				buf[buf_index] = (byte) 0x82;
				buf[buf_index + 1] = (byte) 0x68;
				buf_index += 2;
				break;
			case 0x4A:
				buf[buf_index] = (byte) 0x82;
				buf[buf_index + 1] = (byte) 0x69;
				buf_index += 2;
				break;
			case 0x4B:
				buf[buf_index] = (byte) 0x82;
				buf[buf_index + 1] = (byte) 0x6A;
				buf_index += 2;
				break;
			case 0x4C:
				buf[buf_index] = (byte) 0x82;
				buf[buf_index + 1] = (byte) 0x6B;
				buf_index += 2;
				break;
			case 0x4D:
				buf[buf_index] = (byte) 0x82;
				buf[buf_index + 1] = (byte) 0x6C;
				buf_index += 2;
				break;
			case 0x4E:
				buf[buf_index] = (byte) 0x82;
				buf[buf_index + 1] = (byte) 0x6D;
				buf_index += 2;
				break;
			case 0x4F:
				buf[buf_index] = (byte) 0x82;
				buf[buf_index + 1] = (byte) 0x6E;
				buf_index += 2;
				break;
			case 0x50:
				buf[buf_index] = (byte) 0x82;
				buf[buf_index + 1] = (byte) 0x6F;
				buf_index += 2;
				break;
			case 0x51:
				buf[buf_index] = (byte) 0x82;
				buf[buf_index + 1] = (byte) 0x70;
				buf_index += 2;
				break;
			case 0x52:
				buf[buf_index] = (byte) 0x82;
				buf[buf_index + 1] = (byte) 0x71;
				buf_index += 2;
				break;
			case 0x53:
				buf[buf_index] = (byte) 0x82;
				buf[buf_index + 1] = (byte) 0x72;
				buf_index += 2;
				break;
			case 0x54:
				buf[buf_index] = (byte) 0x82;
				buf[buf_index + 1] = (byte) 0x73;
				buf_index += 2;
				break;
			case 0x55:
				buf[buf_index] = (byte) 0x82;
				buf[buf_index + 1] = (byte) 0x74;
				buf_index += 2;
				break;
			case 0x56:
				buf[buf_index] = (byte) 0x82;
				buf[buf_index + 1] = (byte) 0x75;
				buf_index += 2;
				break;
			case 0x57:
				buf[buf_index] = (byte) 0x82;
				buf[buf_index + 1] = (byte) 0x76;
				buf_index += 2;
				break;
			case 0x58:
				buf[buf_index] = (byte) 0x82;
				buf[buf_index + 1] = (byte) 0x77;
				buf_index += 2;
				break;
			case 0x59:
				buf[buf_index] = (byte) 0x82;
				buf[buf_index + 1] = (byte) 0x78;
				buf_index += 2;
				break;
			case 0x5A:
				buf[buf_index] = (byte) 0x82;
				buf[buf_index + 1] = (byte) 0x79;
				buf_index += 2;
				break;
			case 0x5B:
				buf[buf_index] = (byte) 0x81;
				buf[buf_index + 1] = (byte) 0x6D;
				buf_index += 2;
				break;
			case 0x5C:
				buf[buf_index] = (byte) 0x81;
				buf[buf_index + 1] = (byte) 0x8F;
				buf_index += 2;
				break;
			case 0x5D:
				buf[buf_index] = (byte) 0x81;
				buf[buf_index + 1] = (byte) 0x6E;
				buf_index += 2;
				break;
			case 0x5E:
				buf[buf_index] = (byte) 0x81;
				buf[buf_index + 1] = (byte) 0x4F;
				buf_index += 2;
				break;
			case 0x5F:
				buf[buf_index] = (byte) 0x81;
				buf[buf_index + 1] = (byte) 0x51;
				buf_index += 2;
				break;
			case 0x60:
				buf[buf_index] = (byte) 0x81;
				buf[buf_index + 1] = (byte) 0x65;
				buf_index += 2;
				break;
			case 0x61:
				buf[buf_index] = (byte) 0x82;
				buf[buf_index + 1] = (byte) 0x81;
				buf_index += 2;
				break;
			case 0x62:
				buf[buf_index] = (byte) 0x82;
				buf[buf_index + 1] = (byte) 0x82;
				buf_index += 2;
				break;
			case 0x63:
				buf[buf_index] = (byte) 0x82;
				buf[buf_index + 1] = (byte) 0x83;
				buf_index += 2;
				break;
			case 0x64:
				buf[buf_index] = (byte) 0x82;
				buf[buf_index + 1] = (byte) 0x84;
				buf_index += 2;
				break;
			case 0x65:
				buf[buf_index] = (byte) 0x82;
				buf[buf_index + 1] = (byte) 0x85;
				buf_index += 2;
				break;
			case 0x66:
				buf[buf_index] = (byte) 0x82;
				buf[buf_index + 1] = (byte) 0x86;
				buf_index += 2;
				break;
			case 0x67:
				buf[buf_index] = (byte) 0x82;
				buf[buf_index + 1] = (byte) 0x87;
				buf_index += 2;
				break;
			case 0x68:
				buf[buf_index] = (byte) 0x82;
				buf[buf_index + 1] = (byte) 0x88;
				buf_index += 2;
				break;
			case 0x69:
				buf[buf_index] = (byte) 0x82;
				buf[buf_index + 1] = (byte) 0x89;
				buf_index += 2;
				break;
			case 0x6A:
				buf[buf_index] = (byte) 0x82;
				buf[buf_index + 1] = (byte) 0x8A;
				buf_index += 2;
				break;
			case 0x6B:
				buf[buf_index] = (byte) 0x82;
				buf[buf_index + 1] = (byte) 0x8B;
				buf_index += 2;
				break;
			case 0x6C:
				buf[buf_index] = (byte) 0x82;
				buf[buf_index + 1] = (byte) 0x8C;
				buf_index += 2;
				break;
			case 0x6D:
				buf[buf_index] = (byte) 0x82;
				buf[buf_index + 1] = (byte) 0x8D;
				buf_index += 2;
				break;
			case 0x6E:
				buf[buf_index] = (byte) 0x82;
				buf[buf_index + 1] = (byte) 0x8E;
				buf_index += 2;
				break;
			case 0x6F:
				buf[buf_index] = (byte) 0x82;
				buf[buf_index + 1] = (byte) 0x8F;
				buf_index += 2;
				break;
			case 0x70:
				buf[buf_index] = (byte) 0x82;
				buf[buf_index + 1] = (byte) 0x90;
				buf_index += 2;
				break;
			case 0x71:
				buf[buf_index] = (byte) 0x82;
				buf[buf_index + 1] = (byte) 0x91;
				buf_index += 2;
				break;
			case 0x72:
				buf[buf_index] = (byte) 0x82;
				buf[buf_index + 1] = (byte) 0x92;
				buf_index += 2;
				break;
			case 0x73:
				buf[buf_index] = (byte) 0x82;
				buf[buf_index + 1] = (byte) 0x93;
				buf_index += 2;
				break;
			case 0x74:
				buf[buf_index] = (byte) 0x82;
				buf[buf_index + 1] = (byte) 0x94;
				buf_index += 2;
				break;
			case 0x75:
				buf[buf_index] = (byte) 0x82;
				buf[buf_index + 1] = (byte) 0x95;
				buf_index += 2;
				break;
			case 0x76:
				buf[buf_index] = (byte) 0x82;
				buf[buf_index + 1] = (byte) 0x96;
				buf_index += 2;
				break;
			case 0x77:
				buf[buf_index] = (byte) 0x82;
				buf[buf_index + 1] = (byte) 0x97;
				buf_index += 2;
				break;
			case 0x78:
				buf[buf_index] = (byte) 0x82;
				buf[buf_index + 1] = (byte) 0x98;
				buf_index += 2;
				break;
			case 0x79:
				buf[buf_index] = (byte) 0x82;
				buf[buf_index + 1] = (byte) 0x99;
				buf_index += 2;
				break;
			case 0x7A:
				buf[buf_index] = (byte) 0x82;
				buf[buf_index + 1] = (byte) 0x9A;
				buf_index += 2;
				break;
			case 0x7B:
				buf[buf_index] = (byte) 0x81;
				buf[buf_index + 1] = (byte) 0x6F;
				buf_index += 2;
				break;
			case 0x7C:
				buf[buf_index] = (byte) 0x81;
				buf[buf_index + 1] = (byte) 0x62;
				buf_index += 2;
				break;
			case 0x7D:
				buf[buf_index] = (byte) 0x81;
				buf[buf_index + 1] = (byte) 0x70;
				buf_index += 2;
				break;
			case 0x7E:
				buf[buf_index] = (byte) 0x81;
				buf[buf_index + 1] = (byte) 0x60;
				buf_index += 2;
				break;
			case 0xA1:
				buf[buf_index] = (byte) 0x81;
				buf[buf_index + 1] = (byte) 0x42;
				buf_index += 2;
				break;
			case 0xA2:
				buf[buf_index] = (byte) 0x81;
				buf[buf_index + 1] = (byte) 0x75;
				buf_index += 2;
				break;
			case 0xA3:
				buf[buf_index] = (byte) 0x81;
				buf[buf_index + 1] = (byte) 0x76;
				buf_index += 2;
				break;
			case 0xA4:
				buf[buf_index] = (byte) 0x81;
				buf[buf_index + 1] = (byte) 0x41;
				buf_index += 2;
				break;
			case 0xA5:
				buf[buf_index] = (byte) 0x81;
				buf[buf_index + 1] = (byte) 0x45;
				buf_index += 2;
				break;
			case 0xA6:
				buf[buf_index] = (byte) 0x83;
				buf[buf_index + 1] = (byte) 0x92;
				buf_index += 2;
				break;
			case 0xA7:
				buf[buf_index] = (byte) 0x83;
				buf[buf_index + 1] = (byte) 0x40;
				buf_index += 2;
				break;
			case 0xA8:
				buf[buf_index] = (byte) 0x83;
				buf[buf_index + 1] = (byte) 0x42;
				buf_index += 2;
				break;
			case 0xA9:
				buf[buf_index] = (byte) 0x83;
				buf[buf_index + 1] = (byte) 0x44;
				buf_index += 2;
				break;
			case 0xAA:
				buf[buf_index] = (byte) 0x83;
				buf[buf_index + 1] = (byte) 0x46;
				buf_index += 2;
				break;
			case 0xAB:
				buf[buf_index] = (byte) 0x83;
				buf[buf_index + 1] = (byte) 0x48;
				buf_index += 2;
				break;
			case 0xAC:
				buf[buf_index] = (byte) 0x83;
				buf[buf_index + 1] = (byte) 0x83;
				buf_index += 2;
				break;
			case 0xAD:
				buf[buf_index] = (byte) 0x83;
				buf[buf_index + 1] = (byte) 0x85;
				buf_index += 2;
				break;
			case 0xAE:
				buf[buf_index] = (byte) 0x83;
				buf[buf_index + 1] = (byte) 0x87;
				buf_index += 2;
				break;
			case 0xAF:
				buf[buf_index] = (byte) 0x83;
				buf[buf_index + 1] = (byte) 0x62;
				buf_index += 2;
				break;
			case 0xB0:
				buf[buf_index] = (byte) 0x81;
				buf[buf_index + 1] = (byte) 0x5B;
				buf_index += 2;
				break;
			case 0xB1:
				buf[buf_index] = (byte) 0x83;
				buf[buf_index + 1] = (byte) 0x41;
				buf_index += 2;
				break;
			case 0xB2:
				buf[buf_index] = (byte) 0x83;
				buf[buf_index + 1] = (byte) 0x43;
				buf_index += 2;
				break;
			case 0xB3:
				buf[buf_index] = (byte) 0x83;
				buf[buf_index + 1] = (byte) 0x45;
				buf_index += 2;
				break;
			case 0xB4:
				buf[buf_index] = (byte) 0x83;
				buf[buf_index + 1] = (byte) 0x47;
				buf_index += 2;
				break;
			case 0xB5:
				buf[buf_index] = (byte) 0x83;
				buf[buf_index + 1] = (byte) 0x49;
				buf_index += 2;
				break;
			case 0xB6:
				buf[buf_index] = (byte) 0x83;
				buf[buf_index + 1] = (byte) 0x4A;
				buf_index += 2;
				break;
			case 0xB7:
				buf[buf_index] = (byte) 0x83;
				buf[buf_index + 1] = (byte) 0x4C;
				buf_index += 2;
				break;
			case 0xB8:
				buf[buf_index] = (byte) 0x83;
				buf[buf_index + 1] = (byte) 0x4E;
				buf_index += 2;
				break;
			case 0xB9:
				buf[buf_index] = (byte) 0x83;
				buf[buf_index + 1] = (byte) 0x50;
				buf_index += 2;
				break;
			case 0xBA:
				buf[buf_index] = (byte) 0x83;
				buf[buf_index + 1] = (byte) 0x52;
				buf_index += 2;
				break;
			case 0xBB:
				buf[buf_index] = (byte) 0x83;
				buf[buf_index + 1] = (byte) 0x54;
				buf_index += 2;
				break;
			case 0xBC:
				buf[buf_index] = (byte) 0x83;
				buf[buf_index + 1] = (byte) 0x56;
				buf_index += 2;
				break;
			case 0xBD:
				buf[buf_index] = (byte) 0x83;
				buf[buf_index + 1] = (byte) 0x58;
				buf_index += 2;
				break;
			case 0xBE:
				buf[buf_index] = (byte) 0x83;
				buf[buf_index + 1] = (byte) 0x5A;
				buf_index += 2;
				break;
			case 0xBF:
				buf[buf_index] = (byte) 0x83;
				buf[buf_index + 1] = (byte) 0x5C;
				buf_index += 2;
				break;
			case 0xC0:
				buf[buf_index] = (byte) 0x83;
				buf[buf_index + 1] = (byte) 0x5E;
				buf_index += 2;
				break;
			case 0xC1:
				buf[buf_index] = (byte) 0x83;
				buf[buf_index + 1] = (byte) 0x60;
				buf_index += 2;
				break;
			case 0xC2:
				buf[buf_index] = (byte) 0x83;
				buf[buf_index + 1] = (byte) 0x63;
				buf_index += 2;
				break;
			case 0xC3:
				buf[buf_index] = (byte) 0x83;
				buf[buf_index + 1] = (byte) 0x65;
				buf_index += 2;
				break;
			case 0xC4:
				buf[buf_index] = (byte) 0x83;
				buf[buf_index + 1] = (byte) 0x67;
				buf_index += 2;
				break;
			case 0xC5:
				buf[buf_index] = (byte) 0x83;
				buf[buf_index + 1] = (byte) 0x69;
				buf_index += 2;
				break;
			case 0xC6:
				buf[buf_index] = (byte) 0x83;
				buf[buf_index + 1] = (byte) 0x6A;
				buf_index += 2;
				break;
			case 0xC7:
				buf[buf_index] = (byte) 0x83;
				buf[buf_index + 1] = (byte) 0x6B;
				buf_index += 2;
				break;
			case 0xC8:
				buf[buf_index] = (byte) 0x83;
				buf[buf_index + 1] = (byte) 0x6C;
				buf_index += 2;
				break;
			case 0xC9:
				buf[buf_index] = (byte) 0x83;
				buf[buf_index + 1] = (byte) 0x6D;
				buf_index += 2;
				break;
			case 0xCA:
				buf[buf_index] = (byte) 0x83;
				buf[buf_index + 1] = (byte) 0x6E;
				buf_index += 2;
				break;
			case 0xCB:
				buf[buf_index] = (byte) 0x83;
				buf[buf_index + 1] = (byte) 0x71;
				buf_index += 2;
				break;
			case 0xCC:
				buf[buf_index] = (byte) 0x83;
				buf[buf_index + 1] = (byte) 0x74;
				buf_index += 2;
				break;
			case 0xCD:
				buf[buf_index] = (byte) 0x83;
				buf[buf_index + 1] = (byte) 0x77;
				buf_index += 2;
				break;
			case 0xCE:
				buf[buf_index] = (byte) 0x83;
				buf[buf_index + 1] = (byte) 0x7A;
				buf_index += 2;
				break;
			case 0xCF:
				buf[buf_index] = (byte) 0x83;
				buf[buf_index + 1] = (byte) 0x7D;
				buf_index += 2;
				break;
			case 0xD0:
				buf[buf_index] = (byte) 0x83;
				buf[buf_index + 1] = (byte) 0x7E;
				buf_index += 2;
				break;
			case 0xD1:
				buf[buf_index] = (byte) 0x83;
				buf[buf_index + 1] = (byte) 0x80;
				buf_index += 2;
				break;
			case 0xD2:
				buf[buf_index] = (byte) 0x83;
				buf[buf_index + 1] = (byte) 0x81;
				buf_index += 2;
				break;
			case 0xD3:
				buf[buf_index] = (byte) 0x83;
				buf[buf_index + 1] = (byte) 0x82;
				buf_index += 2;
				break;
			case 0xD4:
				buf[buf_index] = (byte) 0x83;
				buf[buf_index + 1] = (byte) 0x84;
				buf_index += 2;
				break;
			case 0xD5:
				buf[buf_index] = (byte) 0x83;
				buf[buf_index + 1] = (byte) 0x86;
				buf_index += 2;
				break;
			case 0xD6:
				buf[buf_index] = (byte) 0x83;
				buf[buf_index + 1] = (byte) 0x88;
				buf_index += 2;
				break;
			case 0xD7:
				buf[buf_index] = (byte) 0x83;
				buf[buf_index + 1] = (byte) 0x89;
				buf_index += 2;
				break;
			case 0xD8:
				buf[buf_index] = (byte) 0x83;
				buf[buf_index + 1] = (byte) 0x8A;
				buf_index += 2;
				break;
			case 0xD9:
				buf[buf_index] = (byte) 0x83;
				buf[buf_index + 1] = (byte) 0x8B;
				buf_index += 2;
				break;
			case 0xDA:
				buf[buf_index] = (byte) 0x83;
				buf[buf_index + 1] = (byte) 0x8C;
				buf_index += 2;
				break;
			case 0xDB:
				buf[buf_index] = (byte) 0x83;
				buf[buf_index + 1] = (byte) 0x8D;
				buf_index += 2;
				break;
			case 0xDC:
				buf[buf_index] = (byte) 0x83;
				buf[buf_index + 1] = (byte) 0x8F;
				buf_index += 2;
				break;
			case 0xDD:
				buf[buf_index] = (byte) 0x83;
				buf[buf_index + 1] = (byte) 0x93;
				buf_index += 2;
				break;
			case 0xDE:
				/* Dakuten */
				if ((d >= 0xB6 &&  d <= 0xC4) ||
						(d >= 0xCA && d <= 0xCE)) {
					buf[buf_index - 1]++;
				} else if (d == 0xB3) {
					buf[buf_index - 1] = (byte) 0x94;
				} else {
					buf[buf_index] = (byte) 0x81;
					buf[buf_index + 1] = (byte) 0x4A;
					buf_index += 2;
				}
				break;
			case 0xDF:
				/* Han-dakuten */
				if (d >= 0xCA && d <= 0xCE) {
					buf[buf_index - 1] += 2;
				} else {
					buf[buf_index] = (byte) 0x81;
					buf[buf_index + 1] = (byte) 0x4B;
					buf_index += 2;
				}
				break;
			case 0:
			case 255:
				buf[buf_index] = (byte)c;
				buf_index++;
				buf[buf_index] = (byte)c;
				buf_index++;
				if (i + 1 < size) {
					if (str[i + 1] == str[i]) {
						i++;
					}
				}
				break;
			default:
				if ((byte) 0 < c && c < (byte) 0x20) {
					//#define COB_SJSPC "\x81\x40"
					buf[buf_index] = (byte) 0x81;
					buf[buf_index + 1] = (byte) 0x40;
					buf_index += 2;
				} else {
					buf[buf_index] = str[i];
					buf_index++;
					i++;
					buf[buf_index] = str[i];
					buf_index++;
				}
				break;
			}
			d = c;

		}
		buf[buf_index] = (byte) 0x00;
		workReturnSize = buf_index;
		return buf;
	}

	/**
	 * 引数で与えらえられたデータからthisへの代入を行う
	 * @param field 代入元のデータ(dataStorage型)
	 */
	@Override
	public void moveFrom(CobolDataStorage dataStrage) {
	}

	/**
	 * 引数で与えらえられたデータからthisへの代入を行う
	 * @param field 代入元のデータ(byte[]型)
	 */
	@Override
	public void moveFrom(byte[] bytes) {
		dataStorage.setBytes(bytes);
	}

	/**
	 * 引数で与えらえられたデータからthisへの代入を行う
	 * @param field 代入元のデータ(String型)
	 */
	@Override
	public void moveFrom(String string) {
		try {
			byte[] bytes = string.getBytes("SJIS");
			CobolDataStorage data = new CobolDataStorage(bytes);
			CobolFieldAttribute a = new CobolFieldAttribute(CobolFieldAttribute.COB_TYPE_ALPHANUMERIC, 0, 0, 0, null);
			CobolAlphanumericField f = new CobolAlphanumericField(bytes.length, data, a);
			this.moveFrom(f);
		} catch (UnsupportedEncodingException e) {
			e.printStackTrace();
			throw new CobolRuntimeException(CobolRuntimeException.COBOL_FITAL_ERROR, "エンコードエラー");
		}
	}

	/**
	 * 引数で与えらえられたデータからthisへの代入を行う
	 * @param field 代入元のデータ(int型)
	 */
	@Override
	public void moveFrom(int number) {
	}

	/**
	 * 引数で与えらえられたデータからthisへの代入を行う
	 * @param field 代入元のデータ(int型)
	 */
	@Override
	public void moveFrom(double number) {

	}

	/**
	 * 引数で与えらえられたデータからthisへの代入を行う
	 * @param field 代入元のデータ(int型)
	 */
	@Override
	public void moveFrom(BigDecimal number) {

	}

	/**
	 * 引数で与えらえられたデータからthisへの代入を行う
	 * @param field 代入元のデータ(int型)
	 */
	@Override
	public void setDecimal(BigDecimal decimal) {
	}

	/**
	 * thisをCobolNumericFieldに変換する.
	 * indirect moveをするときに使用されることを想定している.
	 * @return thisからCobolNumericField型へ変換した値
	 */
	@Override
	public CobolNumericField getNumericField() {
		int size = this.getSize() / 2;
		int scale = this.getAttribute().getScale();
		CobolFieldAttribute attr = new CobolFieldAttribute(
			CobolFieldAttribute.COB_TYPE_NUMERIC_DISPLAY,
			size,
			scale,
			CobolFieldAttribute.COB_FLAG_HAVE_SIGN,
			null
		);
		CobolDataStorage data = new CobolDataStorage(size);
		CobolNumericField field  =new CobolNumericField(size, data, attr);
		field.moveFrom(this);
		return field;
	}
}