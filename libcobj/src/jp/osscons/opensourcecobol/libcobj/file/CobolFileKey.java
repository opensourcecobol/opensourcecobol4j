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

public class CobolFileKey {
	public static final int COB_MAX_KEY_COMPONENTS = 8;

	private AbstractCobolField field;
	private int flag;
	private int offset;
	private int countComponents;
	private KeyComponent[] component = new KeyComponent[COB_MAX_KEY_COMPONENTS];
	public AbstractCobolField getField() {
		return field;
	}
	public void setField(AbstractCobolField field) {
		this.field = field;
	}
	public int getFlag() {
		return flag;
	}
	public void setFlag(int flag) {
		this.flag = flag;
	}
	public int getOffset() {
		return offset;
	}
	public void setOffset(int offset) {
		this.offset = offset;
	}
	public int getCountComponents() {
		return countComponents;
	}
	public void setCountComponents(int countComponents) {
		this.countComponents = countComponents;
	}
	public KeyComponent[] getComponent() {
		return component;
	}
	public void setComponent(KeyComponent[] component) {
		this.component = component;
	}
	public static int getCobMaxKeyComponents() {
		return COB_MAX_KEY_COMPONENTS;
	}
}
