package jp.osscons.opensourcecobol.libcobj.data;

import java.math.BigDecimal;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;

import jp.osscons.opensourcecobol.libcobj.common.CobolConstant;

public class CobolNationalEditedField extends AbstractCobolField {
	/**
	   * コンストラクタ
	 * @param size データを格納するバイト配列の長さ
	 * @param dataStorage データを格納するバイト配列を扱うオブジェクト
	 * @param attribute 変数に関する様々な情報を保持するオブジェクト
	 */
	public CobolNationalEditedField(int size, CobolDataStorage dataStorage, CobolFieldAttribute attribute) {
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
		case CobolFieldAttribute.COB_TYPE_NUMERIC_PACKED:
		case CobolFieldAttribute.COB_TYPE_NUMERIC_BINARY:
		case CobolFieldAttribute.COB_TYPE_NUMERIC_FLOAT:
		case CobolFieldAttribute.COB_TYPE_NUMERIC_DOUBLE:
			this.moveFrom(src1.getNumericField());
			break;
		case CobolFieldAttribute.COB_TYPE_GROUP:
			CobolAlphanumericField.moveAlphanumToAlphanum(this, src1);
			break;
		default:
			this.moveAlphanumToNationalEdited(src1);
			break;
		}
	}
	
	private void moveAlphanumToNationalEdited(AbstractCobolField src) {
		CobolDataStorage srcd = src.getDataStorage();
		CobolDataStorage dstd = this.getDataStorage();
		int srcp = src.getFirstDataIndex();
		int dstp = 0;
		int max = src.getFieldSize();
		dstd.memset((byte)' ', this.getSize());
		
		final int sizeOfInt = 4;
		
		byte[] picBytes = this.getAttribute().getPic().getBytes();
		for(int p=0; p<picBytes.length;) {
			byte c = picBytes[p++];
			ByteBuffer buf = ByteBuffer.wrap(picBytes, p, sizeOfInt);
			buf.order(ByteOrder.LITTLE_ENDIAN);
			int n = buf.getInt();
			p += sizeOfInt;
			for(; n>0; --n) {
				byte x;
				switch(c) {
				case'N':
					if(srcp < max) {
						dstd.setByte(dstp++, srcd.getByte(srcp++));
						dstd.setByte(dstp++, srcd.getByte(srcp++));
					} else {
						dstd.memcpy(dstp, CobolConstant.ZENBLK, CobolConstant.ZENCSIZ);
						dstp += CobolConstant.ZENCSIZ;
					}
					break;
				case '/':
					dstd.memcpy(dstp, CobolConstant.ZENSLAS, CobolConstant.ZENCSIZ);
					dstp += CobolConstant.ZENCSIZ;
					break;
				case 'B':
					dstd.memcpy(dstp, CobolConstant.ZENSPC, CobolConstant.ZENCSIZ);
					dstp += CobolConstant.ZENCSIZ;
					break;
				case '0':
					dstd.memcpy(dstp, CobolConstant.ZENZERO, CobolConstant.ZENCSIZ);
					dstp += CobolConstant.ZENCSIZ;
					break;
				default:
					dstd.setByte(dstp++, (byte)'?');
					break;
				}
			}
		}
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
