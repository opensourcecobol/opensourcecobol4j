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

import java.math.BigDecimal;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;

import jp.osscons.opensourcecobol.libcobj.common.CobolConstant;
import jp.osscons.opensourcecobol.libcobj.common.CobolModule;

public class CobolNumericEditedField extends AbstractCobolField {
	/**
	 * コンストラクタ
	 * @param size データを格納するバイト配列の長さ
	 * @param dataStorage データを格納するバイト配列を扱うオブジェクト
	 * @param attribute 変数に関する様々な情報を保持するオブジェクト
	 */
	public CobolNumericEditedField(int size, CobolDataStorage dataStorage, CobolFieldAttribute attribute) {
		super(size, dataStorage, attribute);
	}
	
	@Override
	public byte[] getBytes() {
		return null;
	}

	@Override
	public String getString() {
		return null;
	}

	@Override
	public double getDouble() {
		return 0;
	}

	@Override
	public CobolDecimal getDecimal() {
		return null;
	}

	@Override
	public void setDecimal(BigDecimal decimal) {

	}

	@Override
	public int addPackedInt(int n) {
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
			CobolNumericEditedField.moveDisplayToEdited(this, src1);
			break;
		case CobolFieldAttribute.COB_TYPE_NUMERIC_PACKED:
		case CobolFieldAttribute.COB_TYPE_NUMERIC_BINARY:
		case CobolFieldAttribute.COB_TYPE_NUMERIC_EDITED:
		case CobolFieldAttribute.COB_TYPE_NUMERIC_FLOAT:
		case CobolFieldAttribute.COB_TYPE_NUMERIC_DOUBLE:
			this.moveFrom(src1.getNumericField());
			break;
		case CobolFieldAttribute.COB_TYPE_GROUP:
			CobolAlphanumericField.moveAlphanumToAlphanum(this, src1);
			break;
		default:
			this.moveFrom(src1.getNumericField());
			break;
		}
	}
	
	private static void moveDisplayToEdited(AbstractCobolField dst, AbstractCobolField src) {
		int decimalPoint = 0;
		int sign = src.getSign();
		src.putSign(+1);
		boolean neg = sign < 0;
		byte[] picBytes = dst.getAttribute().getPic().getBytes();
		int count = 0;
		int count_sign = 1;
		int count_curr = 1;
		boolean pIsLeft = false;

		final int sizeOfInt = 4;
		for (int p=0; p<picBytes.length; p +=5) {
			byte c = picBytes[p];
			ByteBuffer buf = ByteBuffer.wrap(picBytes, p+1, sizeOfInt);
			buf.order(ByteOrder.LITTLE_ENDIAN);
			int repeat = buf.getInt();
			if(c == '9' || c == 'Z' || c == '*') {
				count += repeat;
				count_sign = 0;
				count_curr = 0;
			} else if(count_curr != 0 && c == CobolModule.getCurrentModule().currency_symbol) {
				count += repeat;
			} else if(count_sign != 0 && (c == '+' || c == '-')) {
				count += repeat;
			} else if(c == 'P') {
				if(count == 0) {
					pIsLeft = true;
					break;
				} else {
					count += repeat;
					count_sign = 0;
					count_curr = 0;
				}
			} else if(c == 'V' || c == CobolModule.getCurrentModule().decimal_point) {
				break;
			}
		}

		// index
		int min;
		if(src.getAttribute().isFlagSignSeparate() && src.getAttribute().isFlagSignLeading()) {
			min = src.getDataStorage().getIndex() + 1;
		} else {			
			min = src.getDataStorage().getIndex();
		}
		int max = min + src.getFieldSize();
		int srcp = max - src.getAttribute().getScale() - count;

		int dstp = dst.getDataStorage().getIndex();
		int end = dstp + dst.getSize();
		
		int signFirst = 0;

		//byte array
		byte[] srcBytes = src.getDataStorage().getRefOfData();
		byte[] dstBytes = dst.getDataStorage().getRefOfData();
		
		//flag
		boolean suppressZero = true;
		boolean isZero = true;
		boolean trailingSign = false;
		boolean trailingCurr = false;

		//characters
		byte pad = ' ';
		byte x;
		byte signSymbol = 0;
		byte currSymbol = 0;

		for(int p=0; p<picBytes.length; ) {
			byte c = picBytes[p++];
			ByteBuffer buf = ByteBuffer.wrap(picBytes, p, sizeOfInt);
			buf.order(ByteOrder.LITTLE_ENDIAN);
			int n = buf.getInt();
			p += sizeOfInt;
			for(; n > 0; n--, ++dstp) {
				switch(c) {
				case '0':
				case '/':
					dstBytes[dstp] = c;
					break;
			
				case 'B':
					dstBytes[dstp] = suppressZero ? pad : (byte)'B';
					break;
					
				case 'P':
					if(pIsLeft) {
						++srcp;
						++dstp;
					}
					break;
				
				case '9':
					if(min <= srcp && srcp < max) {
						byte val = srcBytes[srcp++];						
						dstBytes[dstp] =  (byte)(val >= 0x70 ? val - 0x40 : val);
					} else {
						srcp++;
						dstBytes[dstp] = '0';
					}
					if(dstBytes[dstp] != '0') {
						isZero = suppressZero = false;
					}
					suppressZero = false;
					trailingSign = true;
					trailingCurr = true;
					break;

				case 'V':
					--dstp;
					decimalPoint = dstp;
					break;
				
				case '.':
				case ',':
					if(c == CobolModule.getCurrentModule().decimal_point) {
						dstBytes[dstp] = (byte)CobolModule.getCurrentModule().decimal_point;
						decimalPoint = dstp;
					} else {
						dstBytes[dstp] = suppressZero ? pad : c;
					}
					break;
					
				case 'C':
				case 'D':
					end = dstp;
					if(neg) {
						if(c == 'C') {
							dstBytes[dstp    ] = 'C';
							dstBytes[dstp + 1] = 'R';
						} else {
							dstBytes[dstp    ] = 'D';
							dstBytes[dstp + 1] = 'B';						
						}
					} else {
						dstBytes[dstp    ] = ' ';
						dstBytes[dstp + 1] = ' ';
					}
					dstp++;
					break;
					
				case 'Z':
				case '*':
					x = (min <= srcp && srcp < max) ? srcBytes[srcp] : (byte)'0';
					srcp++;
					if(x != '0') {
						isZero = suppressZero = false;
					}
					pad = (byte)((c == '*') ? '*' : ' ');
					dstBytes[dstp] = suppressZero ? pad : x;
					trailingSign = trailingCurr = true;
					break;

				case '+':
				case '-':
					x = (min <= srcp && srcp < max) ? srcBytes[srcp] : (byte)'0';
					srcp++;
					if(x != '0') {
						isZero = suppressZero = false;
					}
					if(trailingSign) {
						dstBytes[dstp] = (byte)(neg ? '-' : (c == '+') ? '+' : ' ');
						--end;
					} else if(dstp ==  dst.getDataStorage().getIndex() || suppressZero){
						dstBytes[dstp] = pad;
						signSymbol = (byte)(neg ? '-' : (c == '+') ? '+' : ' ');
						if(currSymbol != 0) {
							++signFirst;
						}
					} else {
						dstBytes[dstp] = x;
					}
					break;

				default:
					if(c == CobolModule.getCurrentModule().currency_symbol) {
						x = (byte) ((min <= srcp && srcp < max) ? srcBytes[srcp] : '0');
						srcp++;
						if(x != '0') {
							isZero = suppressZero = false;
						}
						if(trailingCurr) {
							dstBytes[dstp] = (byte)CobolModule.getCurrentModule().currency_symbol;
							--end;
						} else if(dstp == dst.getDataStorage().getIndex() || suppressZero) {
							dstBytes[dstp] = pad;
							currSymbol = (byte)CobolModule.getCurrentModule().currency_symbol;
						} else {
							dstBytes[dstp] = x;
						}
						break;
					}
					dstBytes[dstp] = '?';
				}
			}
		}

		if(suppressZero || (isZero && dst.getAttribute().isFlagBlankZero())) {
			if(pad == ' ' || dst.getAttribute().isFlagBlankZero()) {
				dst.getDataStorage().memset((byte)' ', dst.getSize());
			} else {
				int firstIndex = dst.getDataStorage().getIndex();
				for(dstp = firstIndex; dstp < firstIndex + dst.getSize(); ++dstp) {
					if(dstBytes[dstp] != CobolModule.getCurrentModule().decimal_point) {
						dstBytes[dstp] = pad;
					}
				}
			}
		} else {
			if(decimalPoint != 0) {
				for(dstp = decimalPoint + 1; dstp < end; ++dstp) {
					if(!Character.isDigit(dstBytes[dstp]) && ",+/B".lastIndexOf(dstBytes[dstp]) < 0) {
						dstBytes[dstp] = '0';
					}
				}
			}
			
			if(signSymbol != 0 || currSymbol != 0) {
				for(dstp = end - 1; dstp > dst.getDataStorage().getIndex(); --dstp) {
					if(dstBytes[dstp] == ' ') {
						break;
					}
				}
				if(signSymbol != 0 && currSymbol != 0) {
					if(signFirst != 0) {
						dstBytes[dstp] = currSymbol;
						--dstp;
						if(dstp >= dst.getDataStorage().getIndex()) {
							dstBytes[dstp] = signSymbol;
						}
					} else {
						dstBytes[dstp] = signSymbol;
						--dstp;
						if(dstp >= dst.getDataStorage().getIndex()) {
							dstBytes[dstp] = currSymbol;
						}
					}
				} else if(signSymbol > 0) {
					dstBytes[dstp] = signSymbol;
				} else {
					dstBytes[dstp] = currSymbol;
				}
			}
			
			count = 0;
			for(dstp = dst.getDataStorage().getIndex(); dstp < end; ++dstp) {
				if(dstBytes[dstp] == 'B') {
					dstBytes[dstp] = count == 0 ? pad : (byte)' ';
				} else {
					++count;
				}
			}
		}
		
		src.putSign(sign);
	}

	@Override
	public void moveFrom(CobolDataStorage dataStrage) {

	}

	@Override
	public void moveFrom(byte[] bytes) {

	}

	@Override
	public void moveFrom(String string) {

	}

	@Override
	public void moveFrom(int number) {

	}

	@Override
	public void moveFrom(double number) {

	}

	@Override
	public void moveFrom(BigDecimal number) {

	}
	
	/**
	 * thisをCobolNumericFieldに変換する.
	 * indirect moveをするときに使用されることを想定している.
	 * @return thisからCobolNumericField型へ変換した値
	 */
	public CobolNumericField getNumericField() {
		int size = 36;
		int scale = 18;
		CobolFieldAttribute attr = new CobolFieldAttribute(
			CobolFieldAttribute.COB_TYPE_NUMERIC_DISPLAY,
			size,
			scale,
			CobolFieldAttribute.COB_FLAG_HAVE_SIGN,
			null
		);
		CobolDataStorage data = new CobolDataStorage(64);
		CobolNumericField field = new CobolNumericField(size, data, attr);
		field.moveFrom(this);
		return field;
	}
}
