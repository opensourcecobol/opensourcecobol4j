/*
 * Copyright (C) 2022-2022 TOKYO SYSTEM HOUSE Co., Ltd.
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
package jp.osscons.opensourcecobol.libcobj.common;
import java.util.ArrayList;
import java.util.Deque;
import java.util.LinkedList;
import java.util.List;
import java.util.Stack;

import jp.osscons.opensourcecobol.libcobj.data.AbstractCobolField;
import jp.osscons.opensourcecobol.libcobj.data.CobolDataStorage;

/**
 * libcob/common.hのcob_moduleに対応するクラス
 */
public class CobolModule {

	static private Stack<CobolModule> moduleStack = new Stack<CobolModule>();
	static private CobolModule currentModule;

	static public CobolModule getCurrentModule ()
	{
		return currentModule;
	}

	/**
	 * モジュールスタックに追加する
	 * @param module キューに追加するモジュール
	 */
	static public void push(CobolModule module) {
		currentModule = module;
		moduleStack.push(module);
	}

	/**
	 * モジュールスタックからモジュールを取り除く
	 */
	static public void pop() {
		currentModule = moduleStack.pop();
	}

	/**
	 * モジュールキューが空かどうか
	 * @return
	 */
	static public boolean isQueueEmpty() {
		return moduleStack.isEmpty();
	}


	public CobolModule	next;
	public CobolDataStorage		collating_sequence;
	public AbstractCobolField	cut_status;
	public AbstractCobolField	cursor_pos;
	public int			display_sign;
	public char		decimal_point;
	public char		currency_symbol;
	public char		numeric_separator;
	public int			flag_filename_mapping;
	public int			flag_binary_truncate;
	public int			flag_pretty_display;
	public int			spare8;
	public String      program_id;

	public List<AbstractCobolField> cob_procedure_parameters;

	/**
	 * コンストラクタ
	 * @param next
	 * @param collating_sequence
	 * @param cut_status
	 * @param cursor_pos
	 * @param display_sign
	 * @param decimal_point
	 * @param currency_symbol
	 * @param numeric_separator
	 * @param flag_filename_mapping
	 * @param flag_binary_truncate
	 * @param flag_pretty_display
	 * @param spare8
	 * @param program_id
	 */
	public CobolModule(
		CobolModule	next,
		CobolDataStorage		collating_sequence,
		AbstractCobolField	cut_status,
		AbstractCobolField	cursor_pos,
		int			display_sign,
		char		decimal_point,
		char		currency_symbol,
		char		numeric_separator,
		int			flag_filename_mapping,
		int			flag_binary_truncate,
		int			flag_pretty_display,
		int			spare8,
		String      program_id
	) {
		this.next = next;
		this.collating_sequence = collating_sequence;
		this.cut_status = cut_status;
		this.cursor_pos = cursor_pos;
		this.display_sign = display_sign;
		this.decimal_point = decimal_point;
		this.currency_symbol = currency_symbol;
		this.numeric_separator = numeric_separator;
		this.flag_filename_mapping = flag_filename_mapping;
		this.flag_binary_truncate = flag_binary_truncate;
		this.flag_pretty_display = flag_pretty_display;
		this.spare8 = spare8;
		this.program_id = program_id;

		this.cob_procedure_parameters = new ArrayList<AbstractCobolField>();
	}

	/**
	 * TODO 実装
	 * @param m
	 */
	public void setNext(CobolModule m) {

	}

	/**
	 * TODO 実装
	 * @return
	 */
	public boolean hasNext() {
		return false;
	}

	/**
	 * TODO 実装
	 * @param string
	 */
	public void setProgramID(String string) {

	}

	/**
	 * TODO 実装
	 * @return
	 */
	public CobolModule getNext() {
		return null;
	}

	/**
	 * TODO 実装
	 * @param programId
	 */
	public void setProgramId(String programId) {
	}

	/**
	 * パラメータリストのすべての要素を削除する
	 */
	public void clearParameter() {
		cob_procedure_parameters.clear();
	}

	/**
	 * パラメータリストに要素を追加する
	 * @param field 追加する要素
	 */
	public void addParameter(AbstractCobolField field) {
		cob_procedure_parameters.add(field);
	}

	/**
	 * パラメータリストの長さを返す
	 * @return パラメータリストの長さ
	 */
	public int lengthParameter() {
		return cob_procedure_parameters.size();
	}

	/**
	 * パラメータリストの指定の要素を取得する
	 * @param index
	 * @return
	 */
	public AbstractCobolField getParameter(int index) {
		return cob_procedure_parameters.get(index);
	}

}