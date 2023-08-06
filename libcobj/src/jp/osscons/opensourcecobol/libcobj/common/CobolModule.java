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
package jp.osscons.opensourcecobol.libcobj.common;

import java.util.ArrayList;
import java.util.List;
import java.util.Stack;
import jp.osscons.opensourcecobol.libcobj.data.AbstractCobolField;
import jp.osscons.opensourcecobol.libcobj.data.CobolDataStorage;

/** libcob/common.hのcob_moduleに対応するクラス */
public class CobolModule {

  private static Stack<CobolModule> moduleStack = new Stack<CobolModule>();
  private static CobolModule currentModule;

  public static CobolModule getCurrentModule() {
    return currentModule;
  }

  /**
   * モジュールスタックに追加する
   *
   * @param module キューに追加するモジュール
   */
  public static void push(CobolModule module) {
    currentModule = module;
    moduleStack.push(module);
  }

  /** モジュールスタックからモジュールを取り除く */
  public static void pop() {
    currentModule = moduleStack.pop();
  }

  /**
   * モジュールキューが空かどうか
   *
   * @return
   */
  public static boolean isQueueEmpty() {
    return moduleStack.isEmpty();
  }

  public CobolModule next;
  public CobolDataStorage collating_sequence;
  public AbstractCobolField cut_status;
  public AbstractCobolField cursor_pos;
  public int display_sign;
  public char decimal_point;
  public char currency_symbol;
  public char numeric_separator;
  public int flag_filename_mapping;
  public int flag_binary_truncate;
  public int flag_pretty_display;
  public int spare8;
  public String program_id;
  public String packageName;

  public List<AbstractCobolField> cob_procedure_parameters;

  /**
   * コンストラクタ
   *
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
      CobolModule next,
      CobolDataStorage collatingSequence,
      AbstractCobolField cutStatus,
      AbstractCobolField cursorPos,
      int displaySign,
      char decimalPoint,
      char currencySymbol,
      char numericSeparator,
      int flagFilenameMapping,
      int flagBinaryTruncate,
      int flagPrettyDisplay,
      int spare8,
      String programId) {
    this.next = next;
    this.collating_sequence = collatingSequence;
    this.cut_status = cutStatus;
    this.cursor_pos = cursorPos;
    this.display_sign = displaySign;
    this.decimal_point = decimalPoint;
    this.currency_symbol = currencySymbol;
    this.numeric_separator = numericSeparator;
    this.flag_filename_mapping = flagFilenameMapping;
    this.flag_binary_truncate = flagBinaryTruncate;
    this.flag_pretty_display = flagPrettyDisplay;
    this.spare8 = spare8;
    this.program_id = programId;

    this.cob_procedure_parameters = new ArrayList<AbstractCobolField>();
  }

  /**
   * TODO 実装
   *
   * @param m
   */
  public void setNext(CobolModule m) {}

  /**
   * TODO 実装
   *
   * @return
   */
  public boolean hasNext() {
    return false;
  }

  /**
   * TODO 実装
   *
   * @param string
   */
  public void setProgramID(String string) {}

  /**
   * TODO 実装
   *
   * @return
   */
  public CobolModule getNext() {
    return null;
  }

  /**
   * TODO 実装
   *
   * @param programId
   */
  public void setProgramId(String programId) {}

  /** パラメータリストのすべての要素を削除する */
  public void clearParameter() {
    cob_procedure_parameters.clear();
  }

  /**
   * パラメータリストに要素を追加する
   *
   * @param field 追加する要素
   */
  public void addParameter(AbstractCobolField field) {
    cob_procedure_parameters.add(field);
  }

  public void setParameters(AbstractCobolField... field) {
    cob_procedure_parameters.clear();
    for (AbstractCobolField f : field) {
      cob_procedure_parameters.add(f);
    }
  }

  /**
   * パラメータリストの長さを返す
   *
   * @return パラメータリストの長さ
   */
  public int lengthParameter() {
    return cob_procedure_parameters.size();
  }

  public void setPackageName(String packageName) {
    this.packageName = packageName;
  }

  public String getPackageName() {
    return packageName;
  }

  /**
   * パラメータリストの指定の要素を取得する
   *
   * @param index
   * @return
   */
  public AbstractCobolField getParameter(int index) {
    return cob_procedure_parameters.get(index);
  }
}
