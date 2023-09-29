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
package jp.osscons.opensourcecobol.libcobj.file;

import jp.osscons.opensourcecobol.libcobj.data.AbstractCobolField;

public class Linage {
  private AbstractCobolField linage;
  private AbstractCobolField linageCtr;
  private AbstractCobolField latfoot;
  private AbstractCobolField lattop;
  private AbstractCobolField latbot;
  private int linLines;
  private int linFoot;
  private int linTop;
  private int linBot;

  public AbstractCobolField getLinage() {
    return linage;
  }

  public void setLinage(AbstractCobolField linage) {
    this.linage = linage;
  }

  public AbstractCobolField getLinageCtr() {
    return linageCtr;
  }

  public void setLinageCtr(AbstractCobolField linageCtr) {
    this.linageCtr = linageCtr;
  }

  public AbstractCobolField getLatfoot() {
    return latfoot;
  }

  public void setLatfoot(AbstractCobolField latfoot) {
    this.latfoot = latfoot;
  }

  public AbstractCobolField getLattop() {
    return lattop;
  }

  public void setLattop(AbstractCobolField lattop) {
    this.lattop = lattop;
  }

  public AbstractCobolField getLatbot() {
    return latbot;
  }

  public void setLatbot(AbstractCobolField latbot) {
    this.latbot = latbot;
  }

  public int getLinLines() {
    return linLines;
  }

  public void setLinLines(int linLines) {
    this.linLines = linLines;
  }

  public int getLinFoot() {
    return linFoot;
  }

  public void setLinFoot(int linFoot) {
    this.linFoot = linFoot;
  }

  public int getLinTop() {
    return linTop;
  }

  public void setLinTop(int linTop) {
    this.linTop = linTop;
  }

  public int getLinBot() {
    return linBot;
  }

  public void setLinBot(int linBot) {
    this.linBot = linBot;
  }
}
