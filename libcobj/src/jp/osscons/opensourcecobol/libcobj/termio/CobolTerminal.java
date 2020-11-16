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

package jp.osscons.opensourcecobol.libcobj.termio;

import java.io.BufferedOutputStream;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.PrintStream;

import jp.osscons.opensourcecobol.libcobj.common.CobolModule;
import jp.osscons.opensourcecobol.libcobj.data.AbstractCobolField;
import jp.osscons.opensourcecobol.libcobj.data.CobolDataStorage;
import jp.osscons.opensourcecobol.libcobj.data.CobolFieldAttribute;

/**
 * 標準出力,標準エラー出力に関するメソッドを実装するクラス
 */
public class CobolTerminal {

	/**
	 * cob_displayの実装
	 * TODO 暫定実装
	 * @param outorerr trueなら標準出力に,それ以外は標準エラー出力に出力する.
	 * @param newline trueなら出力後に改行しない,それ以外の場合は改行する
	 * @param fields 出力する変数(可変長)
	 */
	public static void display(boolean dispStdout, boolean newline, AbstractCobolField ...fields) {
		PrintStream stream = dispStdout ? System.out : System.err;
		for(AbstractCobolField field: fields) {
			CobolFieldAttribute attr = field.getAttribute();
			if(attr.isTypeNumericBinary() && CobolModule.getCurrentModule().flag_pretty_display == 0) {
				stream.print(field);
			} else if(attr.isTypeNumeric()){
				stream.print(field);
			} else {
				displayAlnum(field, stream);
			}
		}
		if(newline) {
			stream.println("");
		}
	}

	private static void displayAlnum(AbstractCobolField f, PrintStream stream) {
		CobolDataStorage storage = f.getDataStorage();
		stream.write(storage.getRefOfData(), storage.getIndex(), f.getSize());
	}

	/**
	 * cob_displayの実装
	 * TODO 暫定実装
	 * @param outorerr 0なら標準出力に,それ以外は標準エラー出力に出力する.
	 * @param newline 0なら出力後に改行しない,それ以外の場合は改行する
	 * @param varcnt 出力する変数の数
	 * @param fields 出力する変数(可変長)
	 */
	public static void display(int outorerr, int newline, int varcnt, AbstractCobolField ...fields) {
		PrintStream stream = outorerr == 0 ? System.out : System.err;
		for(AbstractCobolField field: fields) {
			CobolFieldAttribute attr = field.getAttribute();
			if(attr.isTypeNumericBinary() && CobolModule.getCurrentModule().flag_pretty_display == 0) {
				stream.print(field);
			} else if(attr.isTypeNumeric()){
				stream.print(field);
			} else {
				displayAlnum(field, stream);
			}
		}
		if(newline == 1) {
			stream.println("");
		}
	}

	/**
	 * cob_acceptの実装(暫定)
	 * @param f
	 */
	public static void accept(AbstractCobolField f) {
		try {
			//Scanner scan = new Scanner(System.in);
			//scan.nextInt();
			//TODO ロックのテストのための暫定的な実装
			Thread.sleep(1000);
		} catch(Exception e) {

		}
	}
}
