package jp.osscons.opensourcecobol.libcobj.data;

import java.math.BigDecimal;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;

import jp.osscons.opensourcecobol.libcobj.common.CobolConstant;

public class CobolAlphanumericEditedField extends AbstractCobolField {
	/**
	 * コンストラクタ
	 * @param size データを格納するバイト配列の長さ
	 * @param dataStorage データを格納するバイト配列を扱うオブジェクト
	 * @param attribute 変数に関する様々な情報を保持するオブジェクト
	 */
	public CobolAlphanumericEditedField(int size, CobolDataStorage dataStorage, CobolFieldAttribute attribute) {
		super(size, dataStorage, attribute);
	}
	
	@Override
	public byte[] getBytes() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public String getString() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public double getDouble() {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public CobolDecimal getDecimal() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public void setDecimal(BigDecimal decimal) {
		// TODO Auto-generated method stub

	}

	@Override
	public int addPackedInt(int n) {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public void moveFrom(AbstractCobolField src) {
		AbstractCobolField src1 = this.preprocessOfMoving(src);
		if(src1 == null) {
			return;
		}

		switch(src1.getAttribute().getType()) {
		case CobolFieldAttribute.COB_TYPE_NUMERIC_DISPLAY:
			CobolFieldAttribute attr = src1.getAttribute();
			int scale = src1.getAttribute().getScale();
			int digits = src1.getAttribute().getDigits();
			if(scale < 0 || scale > digits) {
				CobolFieldAttribute newAttr = new CobolFieldAttribute(
					attr.getType(),
					attr.getDigits(),
					Math.max(0, attr.getScale()),
					attr.getFlags(),
					attr.getPic());
				src1 = CobolFieldFactory.makeCobolField(Math.max(scale,  digits), src1.getDataStorage(), newAttr);
			}
			CobolAlphanumericEditedField.moveAlphanumToEdited(this, src1);
			break;
		case CobolFieldAttribute.COB_TYPE_NUMERIC_PACKED:
		case CobolFieldAttribute.COB_TYPE_NUMERIC_BINARY:
		case CobolFieldAttribute.COB_TYPE_NUMERIC_DOUBLE:
		case CobolFieldAttribute.COB_TYPE_NUMERIC_FLOAT:
			this.moveFrom(src1.getNumericField());
			break;
		case CobolFieldAttribute.COB_TYPE_GROUP:
			CobolAlphanumericField.moveAlphanumToAlphanum(this, src1);
			break;
		default:
			CobolAlphanumericEditedField.moveAlphanumToEdited(this, src1);
			break;
		}
	}
	
	public static void moveAlphanumToEdited(AbstractCobolField dst, AbstractCobolField src) {
		CobolDataStorage srcd = src.getDataStorage();
		CobolDataStorage dstd = dst.getDataStorage();
		int sign = src.getSign();
		int max = src.getFieldSize();
		int srcp = src.getFirstDataIndex();
		int dstp = 0;
		byte[] picBytes = dst.getAttribute().getPic().getBytes();
		final int sizeOfInt = 4;
		
		for(int p=0; p<picBytes.length; ) {
			byte c = picBytes[p++];
			ByteBuffer buf = ByteBuffer.wrap(picBytes, p, sizeOfInt);
			buf.order(ByteOrder.LITTLE_ENDIAN);
			int n = buf.getInt();
			p += sizeOfInt;

			for(;n > 0; --n) {
				switch(c) {
				case 'A': case 'X': case '9':
					dstd.setByte(dstp++, (srcp < max) ? srcd.getByte(srcp++) : (byte)' ');
					break;
				case '0': case '/':
					dstd.setByte(dstp++, c);
					break;
				case 'B':
					dstd.setByte(dstp++, (byte)' ');
					break;
				default:
					dstd.setByte(dstp++, (byte)'?'); /* invalid PIC */
					break;
				}
			}
		}
		src.putSign(sign);
	}

	@Override
	public void moveFrom(CobolDataStorage dataStrage) {
		// TODO Auto-generated method stub

	}

	@Override
	public void moveFrom(byte[] bytes) {
		// TODO Auto-generated method stub

	}

	@Override
	public void moveFrom(String string) {
		// TODO Auto-generated method stub
	}

	@Override
	public void moveFrom(int number) {
		// TODO Auto-generated method stub
	}

	@Override
	public void moveFrom(double number) {
		// TODO Auto-generated method stub

	}

	@Override
	public void moveFrom(BigDecimal number) {
		// TODO Auto-generated method stub
	}

}
