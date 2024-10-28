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

/** TODO: 準備中 */
public class CobolFileKey {
    /** TODO: 準備中 */
    public static final int COB_MAX_KEY_COMPONENTS = 8;

    private AbstractCobolField field;
    private int flag;
    private int offset;
    private int countComponents;
    private KeyComponent[] component = new KeyComponent[COB_MAX_KEY_COMPONENTS];

    /**
     * TODO: 準備中
     *
     * @return TODO: 準備中
     */
    public AbstractCobolField getField() {
        return field;
    }

    /**
     * TODO: 準備中
     *
     * @param field TODO: 準備中
     */
    public void setField(AbstractCobolField field) {
        this.field = field;
    }

    /**
     * TODO: 準備中
     *
     * @return TODO: 準備中
     */
    public int getFlag() {
        return flag;
    }

    /**
     * TODO: 準備中
     *
     * @param flag TODO: 準備中
     */
    public void setFlag(int flag) {
        this.flag = flag;
    }

    /**
     * TODO: 準備中
     *
     * @return TODO: 準備中
     */
    public int getOffset() {
        return offset;
    }

    /**
     * TODO: 準備中
     *
     * @param offset TODO: 準備中
     */
    public void setOffset(int offset) {
        this.offset = offset;
    }

    /**
     * TODO: 準備中
     *
     * @return TODO: 準備中
     */
    public int getCountComponents() {
        return countComponents;
    }

    /**
     * TODO: 準備中
     *
     * @param countComponents TODO: 準備中
     */
    public void setCountComponents(int countComponents) {
        this.countComponents = countComponents;
    }

    /**
     * TODO: 準備中
     *
     * @return TODO: 準備中
     */
    public KeyComponent[] getComponent() {
        return component;
    }

    /**
     * TODO: 準備中
     *
     * @param component TODO: 準備中
     */
    public void setComponent(KeyComponent[] component) {
        this.component = component;
    }

    /**
     * TODO: 準備中
     *
     * @return TODO: 準備中
     */
    public static int getCobMaxKeyComponents() {
        return COB_MAX_KEY_COMPONENTS;
    }
}
