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

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

/** Represents a result of fetching a data from SQLite tables. */
class FetchResult {
    /** TODO: 準備中 */
    byte[] key;

    /** TODO: 準備中 */
    byte[] value;

    /** TODO: 準備中 */
    int dupNo;

    /**
     * TODO: 準備中
     *
     * @param key TODO: 準備中
     * @param value TODO: 準備中
     * @param dupNo TODO: 準備中
     */
    FetchResult(byte[] key, byte[] value, int dupNo) {
        this.key = key;
        this.value = value;
        this.dupNo = dupNo;
    }

    /**
     * TODO: 準備中
     *
     * @param key TODO: 準備中
     * @param value TODO: 準備中
     */
    FetchResult(byte[] key, byte[] value) {
        this.key = key;
        this.value = value;
        this.dupNo = 0;
    }
}

enum CursorReadOption {
    NEXT,
    PREV,
    FIRST,
    LAST
}

/** Emulates a cursor in SQLite */
final class IndexedCursor {
    /** a cursor to the top direction */
    private Optional<ResultSet> backwardCursor;

    /** a cursor to the bottom direction */
    private Optional<ResultSet> forwardCursor;

    /** a connection to the SQLite database */
    private Connection conn;

    /** firstFetch is true if and only if the cursor has not read any data yet */
    private boolean firstFetch;

    /** a position in buffers that stores the read data */
    private int cursorIndex;

    /** one of COB_EQ, COB_LT, COB_LE, COB_GT, COB_GE in CobolIndexedFile.java */
    private int comparator;

    /** isDuplicate is true if and only if the table key allows duplicates */
    private boolean isDuplicate;

    /** a key */
    private byte[] key;

    /** forwardBuffer stores data located to the bottom direction from the first read position */
    List<FetchResult> forwardBuffer;

    /** bakckwardBuffer stores data located to the first direction from the first read position */
    List<FetchResult> backwardBuffer;

    /** the index of the table */
    private int tableIndex;

    private boolean nextCursorFetchKeyDiffrent;
    private boolean prevCursorFetchKeyDiffrent;

    /**
     * TODO: 準備中
     *
     * @param comparator TODO: 準備中
     */
    void setComparator(int comparator) {
        this.comparator = comparator;
    }

    /**
     * TODO: 準備中
     *
     * @return TODO: 準備中
     */
    int getComparator() {
        return this.comparator;
    }

    private IndexedCursor(
            Connection conn, byte[] key, int tableIndex, boolean isDuplicate, int comparator) {
        this.conn = conn;
        this.key = new byte[key.length];
        System.arraycopy(key, 0, this.key, 0, key.length);
        this.cursorIndex = 0;
        this.comparator = comparator;
        this.firstFetch = true;
        this.isDuplicate = isDuplicate;
        this.tableIndex = tableIndex;
        this.nextCursorFetchKeyDiffrent = false;
        this.prevCursorFetchKeyDiffrent = false;
        this.forwardBuffer = new ArrayList<FetchResult>();
        this.backwardBuffer = new ArrayList<FetchResult>();
    }

    /**
     * TODO: 準備中
     *
     * @param conn TODO: 準備中
     * @param key TODO: 準備中
     * @param tableIndex TODO: 準備中
     * @param isDuplicate TODO: 準備中
     * @param comparator TODO: 準備中
     * @return TODO: 準備中
     */
    static Optional<IndexedCursor> createCursor(
            Connection conn, byte[] key, int tableIndex, boolean isDuplicate, int comparator) {
        IndexedCursor cursor = new IndexedCursor(conn, key, tableIndex, isDuplicate, comparator);
        cursor.forwardCursor = getCursor(conn, key, tableIndex, isDuplicate, comparator, true);
        cursor.backwardCursor = getCursor(conn, key, tableIndex, isDuplicate, comparator, false);
        if (!cursor.forwardCursor.isPresent() || !cursor.backwardCursor.isPresent()) {
            return Optional.empty();
        } else {
            return Optional.of(cursor);
        }
    }

    Optional<IndexedCursor> reloadCursor() {
        if (this.firstFetch) {
            return createCursor(
                    this.conn, this.key, this.tableIndex, this.isDuplicate, this.comparator);
        }

        int newComparator;
        if (this.comparator == CobolIndexedFile.COB_EQ) {
            newComparator = CobolIndexedFile.COB_EQ;
        } else {
            newComparator = CobolIndexedFile.COB_GE;
        }

        FetchResult result;
        if (this.cursorIndex < 0) {
            result = this.backwardBuffer.get(-this.cursorIndex - 1);
        } else {
            result = this.forwardBuffer.get(this.cursorIndex);
        }

        IndexedCursor newCursor =
                new IndexedCursor(
                        this.conn, this.key, this.tableIndex, this.isDuplicate, this.comparator);
        newCursor.forwardCursor =
                reloadedCursor(
                        this.conn,
                        this.tableIndex,
                        this.isDuplicate,
                        newComparator,
                        result,
                        this.comparator,
                        true);
        newCursor.backwardCursor =
                reloadedCursor(
                        this.conn,
                        this.tableIndex,
                        this.isDuplicate,
                        newComparator,
                        result,
                        this.comparator,
                        false);

        Optional<FetchResult> reFetchResult =
                reFetch(this.conn, this.tableIndex, this.isDuplicate, result);
        if (reFetchResult.isPresent()) {
            FetchResult r = reFetchResult.get();
            newCursor.forwardBuffer.add(r);
            newCursor.cursorIndex = 0;
            newCursor.firstFetch = false;
        }

        if (!newCursor.forwardCursor.isPresent() || !newCursor.backwardCursor.isPresent()) {
            return Optional.empty();
        } else {
            return Optional.of(newCursor);
        }
    }

    private static Optional<FetchResult> reFetch(
            Connection conn, int tableIndex, boolean isDuplicate, FetchResult result) {
        final boolean isPrimaryTable = tableIndex == 0;
        final String primaryTable = CobolIndexedFile.getTableName(0);
        final String subTable = CobolIndexedFile.getTableName(tableIndex);
        final String query;
        if (isPrimaryTable) {
            query = String.format("select key, value from %s where key == ?", primaryTable);
        } else if (isDuplicate) {
            query =
                    String.format(
                            "select %s.key, %s.value, %s.dupNo from "
                                    + "%s join %s on %s.value = %s.key "
                                    + "where %s.key == ? and %s.dupNo == ?",
                            subTable,
                            primaryTable,
                            subTable,
                            subTable,
                            primaryTable,
                            subTable,
                            primaryTable,
                            subTable,
                            subTable);
        } else {
            query =
                    String.format(
                            "select %s.key, %s.value from "
                                    + "%s join %s on %s.value = %s.key "
                                    + "where %s.key == ?",
                            subTable,
                            primaryTable,
                            subTable,
                            primaryTable,
                            subTable,
                            primaryTable,
                            subTable);
        }

        try {
            PreparedStatement statement = conn.prepareStatement(query);
            if (isDuplicate) {
                statement.setBytes(1, result.key);
                statement.setInt(2, result.dupNo);
            } else {
                statement.setBytes(1, result.key);
            }
            statement.setFetchSize(0);
            ResultSet rs = statement.executeQuery();
            if (rs.next()) {
                byte[] key = rs.getBytes(1);
                byte[] value = rs.getBytes(2);
                if (isDuplicate) {
                    int dupNo = rs.getInt(3);
                    return Optional.of(new FetchResult(key, value, dupNo));
                } else {
                    return Optional.of(new FetchResult(key, value));
                }
            }
            return Optional.empty();
        } catch (SQLException e) {
            return Optional.empty();
        }
    }

    private static Optional<ResultSet> reloadedCursor(
            Connection conn,
            int tableIndex,
            boolean isDuplicate,
            int comparator,
            FetchResult result,
            int comparaotr,
            boolean forward) {

        final String compOperator = forward ? ">" : "<";

        final String sortOrder = forward ? "" : "desc";

        final String queryPreffix =
                getReloadCursorQueryPrefix(tableIndex, isDuplicate, compOperator, sortOrder);

        final String querySuffix = getQuerySuffix(tableIndex, isDuplicate, sortOrder);

        final String query = queryPreffix + querySuffix;

        try {
            PreparedStatement statement = conn.prepareStatement(query);
            if (isDuplicate) {
                statement.setBytes(1, result.key);
                statement.setInt(2, result.dupNo);
                statement.setBytes(3, result.key);
            } else {
                statement.setBytes(1, result.key);
            }
            statement.setFetchSize(0);
            return Optional.ofNullable(statement.executeQuery());
        } catch (SQLException e) {
            return Optional.empty();
        }
    }

    private static String getReloadCursorQueryPrefix(
            int index, boolean isDuplicate, String compOperator, String sortOrder) {
        final boolean isPrimaryTable = index == 0;
        final String primaryTable = CobolIndexedFile.getTableName(0);
        final String subTable = CobolIndexedFile.getTableName(index);
        if (isPrimaryTable) {
            return String.format(
                    "select key, value from %s where key %s ? order by key %s",
                    primaryTable, compOperator, sortOrder);
        } else if (isDuplicate) {
            return String.format(
                    "select %s.key, %s.value, %s.dupNo from "
                            + "%s join %s on %s.value = %s.key "
                            + "where (%s.key == ? and %s.dupNo %s ?) or %s.key %s ? "
                            + "order by %s.key %s",
                    subTable,
                    primaryTable,
                    subTable,
                    subTable,
                    primaryTable,
                    subTable,
                    primaryTable,
                    subTable,
                    subTable,
                    compOperator,
                    subTable,
                    compOperator,
                    subTable,
                    sortOrder);
        } else {
            return String.format(
                    "select %s.key, %s.value from "
                            + "%s join %s on %s.value = %s.key "
                            + "where %s.key %s ? "
                            + "order by %s.key %s",
                    subTable,
                    primaryTable,
                    subTable,
                    primaryTable,
                    subTable,
                    primaryTable,
                    subTable,
                    compOperator,
                    subTable,
                    sortOrder);
        }
    }

    static boolean matchKeyHead(byte[] originalKey, byte[] fetchKey) {
        if (originalKey.length > fetchKey.length) {
            return false;
        }
        for (int i = 0; i < originalKey.length; ++i) {
            if (originalKey[i] != fetchKey[i]) {
                return false;
            }
        }
        return true;
    }

    private Optional<FetchResult> fetchNext(ResultSet cursor) {
        if (this.comparator == CobolIndexedFile.COB_EQ && this.nextCursorFetchKeyDiffrent) {
            return Optional.empty();
        }
        try {
            if (cursor.next()) {
                byte[] fetchKey = cursor.getBytes(1);
                byte[] fetchValue = cursor.getBytes(2);
                if (this.comparator == CobolIndexedFile.COB_EQ
                        && !matchKeyHead(this.key, fetchKey)) {
                    this.nextCursorFetchKeyDiffrent = true;
                    return Optional.empty();
                }
                FetchResult result;
                if (this.isDuplicate) {
                    int dupNo = cursor.getInt(3);
                    result = new FetchResult(fetchKey, fetchValue, dupNo);
                } else {
                    result = new FetchResult(fetchKey, fetchValue);
                }
                return Optional.of(result);
            } else {
                return Optional.empty();
            }
        } catch (SQLException e) {
            return Optional.empty();
        }
    }

    private Optional<FetchResult> fetchPrev(ResultSet cursor) {
        if (this.comparator == CobolIndexedFile.COB_EQ && this.prevCursorFetchKeyDiffrent) {
            return Optional.empty();
        }
        try {
            if (cursor.next()) {
                byte[] fetchKey = cursor.getBytes(1);
                byte[] fetchValue = cursor.getBytes(2);
                if (this.comparator == CobolIndexedFile.COB_EQ
                        && !matchKeyHead(this.key, fetchKey)) {
                    this.prevCursorFetchKeyDiffrent = true;
                    return Optional.empty();
                }
                FetchResult result;
                if (this.isDuplicate) {
                    int dupNo = cursor.getInt(3);
                    result = new FetchResult(fetchKey, fetchValue, dupNo);
                } else {
                    result = new FetchResult(fetchKey, fetchValue);
                }
                return Optional.of(result);
            } else {
                return Optional.empty();
            }
        } catch (SQLException e) {
            return Optional.empty();
        }
    }

    Optional<FetchResult> next() {
        if (!this.forwardCursor.isPresent()) {
            return Optional.empty();
        }
        ResultSet cursor = this.forwardCursor.get();
        if (this.firstFetch) {
            this.cursorIndex = 0;
            Optional<FetchResult> fetchResult = this.fetchNext(cursor);
            if (this.comparator == CobolIndexedFile.COB_GT) {
                while (fetchResult.isPresent()) {
                    FetchResult result = fetchResult.get();
                    if (!matchKeyHead(this.key, result.key)) {
                        break;
                    }
                    fetchResult = this.fetchNext(cursor);
                }
            }
            this.firstFetch = false;
            if (fetchResult.isPresent()) {
                this.forwardBuffer.add(fetchResult.get());
            }
            return fetchResult;
        } else if (this.cursorIndex >= this.forwardBuffer.size()) {
            return Optional.empty();
        } else if (-this.cursorIndex > this.backwardBuffer.size()) {
            ++this.cursorIndex;
            return Optional.empty();
        } else if (this.cursorIndex == this.forwardBuffer.size() - 1) {
            Optional<FetchResult> fetchResult = this.fetchNext(cursor);
            if (fetchResult.isPresent()) {
                this.forwardBuffer.add(fetchResult.get());
                ++this.cursorIndex;
            }
            return fetchResult;
        } else if (this.cursorIndex < -1) {
            ++this.cursorIndex;
            return Optional.of(backwardBuffer.get(-this.cursorIndex - 1));
        } else {
            ++this.cursorIndex;
            return Optional.of(forwardBuffer.get(this.cursorIndex));
        }
    }

    Optional<FetchResult> prev() {
        if (!this.backwardCursor.isPresent()) {
            return Optional.empty();
        }
        ResultSet cursor = this.backwardCursor.get();
        if (this.firstFetch) {
            this.cursorIndex = -1;
            Optional<FetchResult> fetchResult = this.fetchPrev(cursor);
            if (fetchResult.isPresent()) {
                this.firstFetch = false;
                this.backwardBuffer.add(fetchResult.get());
            }
            return fetchResult;
        } else if (this.cursorIndex > this.forwardBuffer.size()) {
            --this.cursorIndex;
            return Optional.empty();
        } else if (-this.cursorIndex - 1 >= this.backwardBuffer.size()) {
            return Optional.empty();
        } else if (-this.cursorIndex == this.backwardBuffer.size()) {
            Optional<FetchResult> fetchResult = this.fetchPrev(cursor);
            if (fetchResult.isPresent()) {
                this.backwardBuffer.add(fetchResult.get());
                --this.cursorIndex;
            }
            return fetchResult;
        } else if (this.cursorIndex > 0) {
            --this.cursorIndex;
            return Optional.of(forwardBuffer.get(this.cursorIndex));
        } else {
            --this.cursorIndex;
            return Optional.of(backwardBuffer.get(-this.cursorIndex - 1));
        }
    }

    void close() {
        try {
            if (backwardCursor.isPresent()) {
                backwardCursor.get().close();
            }
            if (forwardCursor.isPresent()) {
                forwardCursor.get().close();
            }
        } catch (SQLException e) {
            return;
        }
    }

    Optional<FetchResult> read(CursorReadOption opt) {
        if (opt == CursorReadOption.NEXT) {
            return this.next();
        } else if (opt == CursorReadOption.PREV) {
            return this.prev();
        } else {
            return Optional.empty();
        }
    }

    private static Optional<ResultSet> getCursor(
            Connection conn,
            byte[] key,
            int index,
            boolean isDuplicate,
            int comparator,
            boolean forward) {
        final Optional<String> optionalCompOperator = getCompOperator(comparator, forward);

        if (!optionalCompOperator.isPresent()) {
            return Optional.empty();
        }

        final String compOperator = optionalCompOperator.get();

        final String sortOrder = forward ? "" : "desc";

        final String queryPreffix = getQueryPrefix(index, compOperator, sortOrder, isDuplicate);

        final String querySuffix = getQuerySuffix(index, isDuplicate, sortOrder);

        final String query = queryPreffix + querySuffix;

        try {
            PreparedStatement statement = conn.prepareStatement(query);
            statement.setBytes(1, key);
            statement.setFetchSize(0);
            return Optional.ofNullable(statement.executeQuery());
        } catch (SQLException e) {
            return Optional.empty();
        }
    }

    private static Optional<String> getCompOperator(int comparator, boolean forward) {
        if (comparator == CobolIndexedFile.COB_EQ
                || comparator == CobolIndexedFile.COB_GE
                || comparator == CobolIndexedFile.COB_LT) {
            return Optional.of(forward ? ">=" : "<");
        } else if (comparator == CobolIndexedFile.COB_GT || comparator == CobolIndexedFile.COB_LE) {
            return Optional.of(forward ? ">" : "<=");
        } else {
            return Optional.empty();
        }
    }

    private static String getQueryPrefix(
            int index, String compOperator, String sortOrder, boolean isDuplicate) {
        final boolean isPrimaryTable = index == 0;
        final String primaryTable = CobolIndexedFile.getTableName(0);
        final String subTable = CobolIndexedFile.getTableName(index);
        if (isPrimaryTable) {
            return String.format(
                    "select key, value from %s where key %s ? order by key %s",
                    primaryTable, compOperator, sortOrder);
        } else if (isDuplicate) {
            return String.format(
                    "select %s.key, %s.value, %s.dupNo from "
                            + "%s join %s on %s.value = %s.key "
                            + "where %s.key %s ? order by %s.key %s",
                    subTable,
                    primaryTable,
                    subTable,
                    subTable,
                    primaryTable,
                    subTable,
                    primaryTable,
                    subTable,
                    compOperator,
                    subTable,
                    sortOrder);
        } else {
            return String.format(
                    "select %s.key, %s.value from "
                            + "%s join %s on %s.value = %s.key "
                            + "where %s.key %s ? order by %s.key %s",
                    subTable,
                    primaryTable,
                    subTable,
                    primaryTable,
                    subTable,
                    primaryTable,
                    subTable,
                    compOperator,
                    subTable,
                    sortOrder);
        }
    }

    private Optional<ResultSet> getCursorForFirstLast(
            int index, boolean isDuplicate, CursorReadOption option) {
        final String query = getQueryForFirstLast(index, isDuplicate, option);
        try {
            Statement statement = this.conn.createStatement();
            return Optional.ofNullable(statement.executeQuery(query));
        } catch (SQLException e) {
            return Optional.empty();
        }
    }

    /**
     * TODO: 準備中
     *
     * @return TODO: 準備中
     */
    boolean moveToFirst() {
        Optional<ResultSet> cursor =
                getCursorForFirstLast(this.tableIndex, this.isDuplicate, CursorReadOption.FIRST);
        if (!cursor.isPresent()) {
            return false;
        }
        this.forwardCursor = cursor;
        this.backwardCursor = Optional.empty();
        this.firstFetch = true;
        this.cursorIndex = 0;
        return true;
    }

    /**
     * TODO: 準備中
     *
     * @return TODO: 準備中
     */
    boolean moveToLast() {
        Optional<ResultSet> cursor =
                getCursorForFirstLast(this.tableIndex, this.isDuplicate, CursorReadOption.LAST);
        if (!cursor.isPresent()) {
            return false;
        }
        this.backwardCursor = cursor;
        this.forwardCursor = Optional.empty();
        this.firstFetch = true;
        this.cursorIndex = 0;
        return true;
    }

    private static String getQueryForFirstLast(
            int index, boolean isDuplicate, CursorReadOption option) {
        final String sortOrder;
        if (option == CursorReadOption.FIRST) {
            sortOrder = "";
        } else {
            sortOrder = "desc";
        }

        boolean isPrimaryTable = index == 0;
        final String primaryTable = CobolIndexedFile.getTableName(0);
        final String subTable = CobolIndexedFile.getTableName(index);

        if (isPrimaryTable) {
            return String.format(
                    "select key, value from %s order by key %s", primaryTable, sortOrder);
        } else {
            String optionColumn;
            if (isDuplicate) {
                optionColumn = String.format(", %s.dupNo", subTable);
            } else {
                optionColumn = "";
            }
            String query =
                    String.format(
                            "select %s.key, %s.value %s from "
                                    + "%s join %s on %s.value = %s.key "
                                    + "order by %s.key %s",
                            subTable,
                            primaryTable,
                            optionColumn,
                            subTable,
                            primaryTable,
                            subTable,
                            primaryTable,
                            subTable,
                            sortOrder);
            if (isDuplicate) {
                return query + String.format(", dupNo %s", sortOrder);
            } else {
                return query;
            }
        }
    }

    private static String getQuerySuffix(int index, boolean isDuplicate, String sortOrder) {
        if (isDuplicate) {
            final String tableName = CobolIndexedFile.getTableName(index);
            return String.format(", %s.dupNo %s", tableName, sortOrder);
        } else {
            return "";
        }
    }
}
