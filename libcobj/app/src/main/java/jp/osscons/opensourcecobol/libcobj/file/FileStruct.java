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

/** SORT/MERGE のマージ処理で用いる 1 つの一時ファイルと,そこに残っているブロック数を束ねる構造. */
class FileStruct {
    private FileIO fp;
    private int count;

    /**
     * 一時ファイルへの入出力を行うFileIOを取得する.
     *
     * @return この一時ファイルの入出力を担うFileIO
     */
    FileIO getFp() {
        return fp;
    }

    /**
     * 一時ファイルへの入出力を行うFileIOを設定する.
     *
     * @param fp この一時ファイルの入出力を担うFileIO
     */
    void setFp(FileIO fp) {
        this.fp = fp;
    }

    /**
     * この一時ファイルに残っているブロック数を取得する.
     *
     * @return 一時ファイルに残っているブロック数
     */
    int getCount() {
        return count;
    }

    /**
     * この一時ファイルに残っているブロック数を設定する.
     *
     * @param count 一時ファイルに残っているブロック数
     */
    void setCount(int count) {
        this.count = count;
    }
}
