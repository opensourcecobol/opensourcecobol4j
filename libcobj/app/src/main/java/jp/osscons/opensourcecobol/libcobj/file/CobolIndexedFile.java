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

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.Optional;
import jp.osscons.opensourcecobol.libcobj.data.AbstractCobolField;
import jp.osscons.opensourcecobol.libcobj.data.CobolDataStorage;
import org.sqlite.SQLiteConfig;
import org.sqlite.SQLiteErrorCode;

/** TODO: 準備中 */
public class CobolIndexedFile extends CobolFile {
    private Optional<IndexedCursor> cursor;
    private boolean updateWhileReading = false;
    private boolean indexedFirstRead = true;
    private boolean callStart = false;
    private boolean commitOnModification = true;

    /** TODO: 準備中 */
    public static final int COB_EQ = 1;

    /** TODO: 準備中 */
    public static final int COB_LT = 2;

    /** TODO: 準備中 */
    public static final int COB_LE = 3;

    /** TODO: 準備中 */
    public static final int COB_GT = 4;

    /** TODO: 準備中 */
    public static final int COB_GE = 5;

    /** TODO: 準備中 */
    public static final int COB_NE = 6;

    /**
     * TODO: 準備中
     *
     * @param selectName TODO: 準備中
     * @param fileStatus TODO: 準備中
     * @param assign TODO: 準備中
     * @param record TODO: 準備中
     * @param recordSize TODO: 準備中
     * @param recordMin TODO: 準備中
     * @param recordMax TODO: 準備中
     * @param nkeys TODO: 準備中
     * @param keys TODO: 準備中
     * @param organization TODO: 準備中
     * @param accessMode TODO: 準備中
     * @param lockMode TODO: 準備中
     * @param openMode TODO: 準備中
     * @param flagOptional TODO: 準備中
     * @param lastOpenMode TODO: 準備中
     * @param special TODO: 準備中
     * @param flagNonexistent TODO: 準備中
     * @param flagEndOfFile TODO: 準備中
     * @param flagBeginOfFile TODO: 準備中
     * @param flagFirstRead TODO: 準備中
     * @param flagReadDone TODO: 準備中
     * @param flagSelectFeatures TODO: 準備中
     * @param flagNeedsNl TODO: 準備中
     * @param flagNeedsTop TODO: 準備中
     * @param fileVersion TODO: 準備中
     */
    public CobolIndexedFile(
            String selectName,
            byte[] fileStatus,
            AbstractCobolField assign,
            AbstractCobolField record,
            AbstractCobolField recordSize,
            int recordMin,
            int recordMax,
            int nkeys,
            CobolFileKey[] keys,
            char organization,
            char accessMode,
            char lockMode,
            char openMode,
            boolean flagOptional,
            char lastOpenMode,
            char special,
            boolean flagNonexistent,
            boolean flagEndOfFile,
            boolean flagBeginOfFile,
            char flagFirstRead,
            boolean flagReadDone,
            char flagSelectFeatures,
            boolean flagNeedsNl,
            boolean flagNeedsTop,
            char fileVersion) {
        super(
                selectName,
                fileStatus,
                assign,
                record,
                recordSize,
                recordMin,
                recordMax,
                nkeys,
                keys,
                organization,
                accessMode,
                lockMode,
                openMode,
                flagOptional,
                lastOpenMode,
                special,
                flagNonexistent,
                flagEndOfFile,
                flagBeginOfFile,
                flagFirstRead,
                flagReadDone,
                flagSelectFeatures,
                flagNeedsNl,
                flagNeedsTop,
                fileVersion);
    }

    private static String getIndexName(int index) {
        return String.format("index%d", index);
    }

    private static String getSubIndexName(int index) {
        return String.format("subindex%d", index);
    }

    /**
     * TODO: 準備中
     *
     * @param index TODO: 準備中
     * @return TODO: 準備中
     */
    public static String getTableName(int index) {
        return String.format("table%d", index);
    }

    /**
     * TODO: 準備中
     *
     * @param index TODO: 準備中
     * @return TODO: 準備中
     */
    public static String getCursorName(int index) {
        return String.format("cursor%d", index);
    }

    private static String getConstraintName(int index) {
        return String.format("constraint%d", index);
    }

    /**
     * TODO: 準備中
     *
     * @param commitOnModification TODO: 準備中
     */
    public void setCommitOnModification(boolean commitOnModification) {
        this.commitOnModification = commitOnModification;
    }

    /** Equivalent to DBT_SET in libcob/fileio.c */
    private byte[] DBT_SET(AbstractCobolField field) {
        return field.getDataStorage().getByteArray(0, field.getSize());
    }

    @Override
    public int open_(String filename, int mode, int sharing) {
        IndexedFile p = new IndexedFile();

        SQLiteConfig config = new SQLiteConfig();
        config.setReadOnly(mode == COB_OPEN_INPUT);

        if (mode == COB_OPEN_OUTPUT) {
            Path path = Paths.get(filename);
            try {
                Files.deleteIfExists(path);
            } catch (IOException e) {
                return COB_STATUS_30_PERMANENT_ERROR;
            }
        }

        boolean fileExists = new java.io.File(filename).exists();

        p.connection = null;
        try {
            p.connection =
                    DriverManager.getConnection("jdbc:sqlite:" + filename, config.toProperties());
            p.connection.setAutoCommit(false);
        } catch (SQLException e) {
            int errorCode = e.getErrorCode();
            if (errorCode == SQLiteErrorCode.SQLITE_BUSY.code) {
                return COB_STATUS_61_FILE_SHARING;
            } else {
                return ENOENT;
            }
        } catch (Exception e) {
            return COB_STATUS_30_PERMANENT_ERROR;
        }

        p.filenamelen = filename.length();
        p.last_dupno = new int[this.nkeys];
        p.rewrite_sec_key = new int[this.nkeys];

        int maxsize = 0;
        for (int i = 0; i < this.nkeys; ++i) {
            if (this.keys[i].getField().getSize() > maxsize) {
                maxsize = this.keys[i].getField().getSize();
            }
        }

        if (mode == COB_OPEN_OUTPUT
                || (!fileExists && (mode == COB_OPEN_EXTEND || mode == COB_OPEN_I_O))) {
            try {
                for (int i = 0; i < this.nkeys; ++i) {
                    String tableName = getTableName(i);
                    Statement statement = p.connection.createStatement();
                    if (i == 0) {
                        statement.execute(
                                String.format(
                                        "create table %s (key blob not null primary key, value blob"
                                                + " not null)",
                                        tableName));
                    } else {
                        if (this.keys[i].getFlag() == 0) {
                            statement.execute(
                                    String.format(
                                            "create table %s (key blob not null primary key, value"
                                                    + " blob not null, constraint %s foreign key"
                                                    + " (value) references %s (key))",
                                            tableName, getConstraintName(i), getTableName(0)));
                        } else {
                            statement.execute(
                                    String.format(
                                            "create table %s (key blob not null, value blob not"
                                                    + " null, dupNo integer not null, constraint %s"
                                                    + " foreign key (value) references %s (key))",
                                            tableName, getConstraintName(i), getTableName(0)));
                        }
                        statement.execute(
                                String.format(
                                        "create index %s on %s(value)",
                                        getSubIndexName(i), tableName));
                    }
                    statement.execute(
                            String.format(
                                    "create index %s on %s(key)", getIndexName(i), tableName));
                    statement.close();
                }
                this.writeMetaData(p);
                if (this.commitOnModification) {
                    p.connection.commit();
                }
            } catch (SQLException e) {
                return COB_STATUS_30_PERMANENT_ERROR;
            }
        }

        p.temp_key = new CobolDataStorage(maxsize + 4);
        this.filei = p;
        p.key_index = 0;
        p.last_key = null;

        p.filename = filename;
        p.write_cursor_open = false;
        p.record_locked = false;

        p.key = DBT_SET(this.keys[0].getField());
        this.updateWhileReading = false;
        this.indexedFirstRead = true;
        this.callStart = false;

        return 0;
    }

    // Write a metadata to the database
    private void writeMetaData(IndexedFile p) throws SQLException {
        Statement statement = p.connection.createStatement();
        // Create a table to store metadata
        statement.execute(
                "create table metadata_string_int (key text not null primary key, value integer not"
                        + " null)");
        statement.execute(
                "create table metadata_key (idx integer not null primary key, offset integer not"
                        + " null, size integer not null, duplicate boolean)");
        statement.close();

        // Store the size of a record
        PreparedStatement recordSizePreparedStmt =
                p.connection.prepareStatement(
                        "insert into metadata_string_int values ('record_size', ?)");
        recordSizePreparedStmt.setInt(1, this.record_max);
        recordSizePreparedStmt.execute();
        recordSizePreparedStmt.close();

        // Store information of all keys
        PreparedStatement keyPreparedStmt =
                p.connection.prepareStatement("insert into metadata_key values (?, ?, ?, ?)");
        for (int i = 0; i < this.nkeys; i++) {
            keyPreparedStmt.setInt(1, i);
            keyPreparedStmt.setInt(
                    2,
                    this.keys[i].getField().getDataStorage().getIndex()
                            - this.record.getDataStorage().getIndex());
            keyPreparedStmt.setInt(3, this.keys[i].getField().getSize());
            keyPreparedStmt.setBoolean(4, this.keys[i].getFlag() != 0);
            keyPreparedStmt.execute();
        }
        keyPreparedStmt.close();
    }

    @Override
    public int close_(int opt) {
        IndexedFile p = this.filei;

        this.closeCursor();

        try {
            p.connection.close();
        } catch (SQLException e) {
            return COB_STATUS_30_PERMANENT_ERROR;
        }
        return COB_STATUS_00_SUCCESS;
    }

    // Equivalent to indexed_start_internal in libcob/fileio.c
    /**
     * TODO: 準備中
     *
     * @param cond TODO: 準備中
     * @param key TODO: 準備中
     * @param readOpts TODO: 準備中
     * @param testLock TODO: 準備中
     * @return TODO: 準備中
     */
    public int indexed_start_internal(
            int cond, AbstractCobolField key, int readOpts, boolean testLock) {
        IndexedFile p = this.filei;
        for (p.key_index = 0; p.key_index < this.nkeys; p.key_index++) {
            if (this.keys[p.key_index].getField().getDataStorage().isSame(key.getDataStorage())) {
                break;
            }
        }

        p.key = DBT_SET(key);

        boolean isDuplicate = this.keys[p.key_index].getFlag() != 0;

        this.cursor =
                IndexedCursor.createCursor(p.connection, p.key, p.key_index, isDuplicate, cond);
        if (!this.cursor.isPresent()) {
            return COB_STATUS_30_PERMANENT_ERROR;
        }

        IndexedCursor cursor = this.cursor.get();
        Optional<FetchResult> optionalResult = cursor.next();
        if (optionalResult.isPresent()) {
            FetchResult result = optionalResult.get();
            p.key = result.key;
            p.data = result.value;
            this.indexedFirstRead = false;
            return COB_STATUS_00_SUCCESS;
        } else {
            return COB_STATUS_23_KEY_NOT_EXISTS;
        }
    }

    @Override
    public int start_(int cond, AbstractCobolField key) {
        int ret = indexed_start_internal(cond, key, 0, false);
        if (ret == COB_STATUS_00_SUCCESS) {
            this.callStart = true;
        }
        return ret;
    }

    @Override
    public int read_(AbstractCobolField key, int readOpts) {
        IndexedFile p = this.filei;
        boolean testLock = false;
        this.callStart = false;
        int ret = this.indexed_start_internal(COB_EQ, key, readOpts, testLock);
        if (ret != COB_STATUS_00_SUCCESS) {
            return ret;
        }

        this.record.setSize(p.data.length);
        this.record.getDataStorage().memcpy(p.data, p.data.length);

        return COB_STATUS_00_SUCCESS;
    }

    @Override
    public int readNext(int readOpts) {
        IndexedFile p = this.filei;
        if (this.callStart) {
            this.callStart = false;
            this.indexedFirstRead = false;
            this.record.setSize(p.data.length);
            if (this.cursor.isPresent()) {
                IndexedCursor cursor = this.cursor.get();
                if ((readOpts & CobolFile.COB_READ_PREVIOUS) != 0
                        && cursor.getComparator() == COB_LE
                        && this.record.getDataStorage().memcmp(p.data, p.data.length) != 0) {
                    this.callStart = false;
                    return this.readNext(readOpts);
                }
            }
            this.record.getDataStorage().memcpy(p.data, p.data.length);
            return COB_STATUS_00_SUCCESS;
        }

        boolean isDuplicate = this.keys[p.key_index].getFlag() != 0;
        if (this.indexedFirstRead || this.flag_begin_of_file) {
            this.cursor =
                    IndexedCursor.createCursor(
                            p.connection, p.key, p.key_index, isDuplicate, COB_GE);
            if (!this.cursor.isPresent()) {
                return COB_STATUS_10_END_OF_FILE;
            }
            this.cursor.get().moveToFirst();
        } else if (this.flag_end_of_file) {
            this.cursor =
                    IndexedCursor.createCursor(
                            p.connection, p.key, p.key_index, isDuplicate, COB_LE);
            if (!this.cursor.isPresent()) {
                return COB_STATUS_30_PERMANENT_ERROR;
            }
            this.cursor.get().moveToLast();
        } else if (this.updateWhileReading) {
            this.updateWhileReading = false;
            if (!this.cursor.isPresent()) {
                return COB_STATUS_30_PERMANENT_ERROR;
            }
            IndexedCursor oldCursor = this.cursor.get();
            Optional<IndexedCursor> newCursor = oldCursor.reloadCursor();
            if (!newCursor.isPresent()) {
                this.cursor = Optional.of(oldCursor);
            } else {
                oldCursor.close();
                this.cursor = newCursor;
            }
        }

        if (!this.cursor.isPresent()) {
            return COB_STATUS_30_PERMANENT_ERROR;
        }

        IndexedCursor cursor = this.cursor.get();

        final CursorReadOption cursorOpt;
        if ((readOpts & COB_READ_PREVIOUS) != 0) {
            cursorOpt = CursorReadOption.PREV;
            cursor.setComparator(COB_LE);
        } else {
            cursorOpt = CursorReadOption.NEXT;
            cursor.setComparator(COB_GE);
        }

        Optional<FetchResult> optionalResult = cursor.read(cursorOpt);

        if (!optionalResult.isPresent()) {
            this.indexedFirstRead = false;
            return COB_STATUS_10_END_OF_FILE;
        }

        FetchResult result = optionalResult.get();
        p.key = result.key;
        p.data = result.value;

        this.record.setSize(p.data.length);
        this.record.getDataStorage().memcpy(p.data, p.data.length);

        this.indexedFirstRead = false;
        return COB_STATUS_00_SUCCESS;
    }

    private void closeCursor() {
        if (this.cursor != null) {
            if (this.cursor.isPresent()) {
                this.cursor.get().close();
            }
        }
    }

    private boolean keyExistsInTable(IndexedFile p, int index, byte[] key) {
        String query = String.format("select * from %s where key = ?", getTableName(index));
        try (PreparedStatement selectStatement = p.connection.prepareStatement(query)) {
            selectStatement.setBytes(1, key);
            selectStatement.setFetchSize(0);
            ResultSet rs = selectStatement.executeQuery();
            return rs.next();
        } catch (SQLException e) {
            return false;
        }
    }

    private boolean isDuplicateColumn(int index) {
        return this.keys[index].getFlag() != 0;
    }

    private int getNextKeyDupNo(Connection conn, int index, byte[] key) {
        try {
            PreparedStatement selectStatement =
                    conn.prepareStatement(
                            String.format(
                                    "select ifnull(max(dupNo), -1) from %s", getTableName(index)));
            ResultSet rs = selectStatement.executeQuery();
            return rs.getInt(1) + 1;
        } catch (SQLException e) {
            return 0;
        }
    }

    private int returnWith(IndexedFile p, boolean closeCursor, int index, int returnCode) {
        if (closeCursor) {
            this.closeCursor();
            p.write_cursor_open = false;
        }
        return returnCode;
    }

    /** Equivalent to indexed_write_internal in libcob/fileio.c */
    private int indexed_write_internal(boolean rewrite, int opt) {
        IndexedFile p = this.filei;

        boolean closeCursor;
        p.write_cursor_open = true;
        closeCursor = true;

        if (this.nkeys > 1 && !rewrite) {
            if (this.check_alt_keys(false)) {
                return returnWith(p, closeCursor, 0, COB_STATUS_22_KEY_EXISTS);
            }
            p.key = DBT_SET(this.keys[0].getField());
        }

        if (keyExistsInTable(p, 0, p.key)) {
            return COB_STATUS_22_KEY_EXISTS;
        }

        // insert into the primary table
        p.data = DBT_SET(this.record);
        try (PreparedStatement insertStatement =
                p.connection.prepareStatement(
                        String.format("insert into %s values (?, ?)", getTableName(0)))) {
            insertStatement.setBytes(1, p.key);
            insertStatement.setBytes(2, p.data);
            insertStatement.execute();
            if (this.commitOnModification) {
                p.connection.commit();
            }
        } catch (SQLException e) {
            return returnWith(p, closeCursor, 0, COB_STATUS_51_RECORD_LOCKED);
        }

        p.data = p.key;

        // insert into sub tables
        for (int i = 1; i < this.nkeys; i++) {

            p.key = DBT_SET(this.keys[i].getField());
            try {
                if (!isDuplicateColumn(i) && keyExistsInTable(p, i, p.key)) {
                    return returnWith(p, closeCursor, 0, COB_STATUS_22_KEY_EXISTS);
                }

                PreparedStatement insertStatement;
                if (isDuplicateColumn(i)) {
                    int dupNo = getNextKeyDupNo(p.connection, i, p.key);
                    insertStatement =
                            p.connection.prepareStatement(
                                    String.format(
                                            "insert into %s values (?, ?, ?)", getTableName(i)));
                    insertStatement.setBytes(1, p.key);
                    insertStatement.setBytes(2, p.data);
                    insertStatement.setInt(3, dupNo);
                } else {
                    insertStatement =
                            p.connection.prepareStatement(
                                    String.format("insert into %s values (?, ?)", getTableName(i)));
                    insertStatement.setBytes(1, p.key);
                    insertStatement.setBytes(2, p.data);
                }
                insertStatement.execute();
                insertStatement.close();
                if (this.commitOnModification) {
                    p.connection.commit();
                }
            } catch (SQLException e) {
                return returnWith(p, closeCursor, 0, COB_STATUS_51_RECORD_LOCKED);
            }
        }

        this.updateWhileReading = true;

        return returnWith(p, closeCursor, 0, COB_STATUS_00_SUCCESS);
    }

    @Override
    public int write_(int opt) {
        IndexedFile p = this.filei;

        p.key = DBT_SET(this.keys[0].getField());
        if (p.last_key == null) {
            p.last_key = new CobolDataStorage(p.key.length);

        } else if (this.access_mode == COB_ACCESS_SEQUENTIAL) {
            byte[] keyBytes = p.key;
            if (p.last_key.memcmp(keyBytes, keyBytes.length) > 0) {
                return COB_STATUS_21_KEY_INVALID;
            }
        }

        byte[] keyBytes = p.key;
        p.last_key.memcpy(keyBytes, keyBytes.length);

        return indexed_write_internal(false, opt);
    }

    /** Equivalent to check_alt_keys in libcob/fileio.c */
    private boolean check_alt_keys(boolean rewrite) {
        IndexedFile p = this.filei;

        byte[] primaryKey = DBT_SET(this.keys[0].getField());
        for (int i = 1; i < this.nkeys; ++i) {
            if (this.keys[i].getFlag() == 0) {
                p.key = DBT_SET(this.keys[i].getField());
                if (rewrite) {
                    if (checkTable(p, i, p.key, primaryKey)) {
                        return true;
                    }
                } else {
                    if (keyExistsInTable(p, i, p.key)) {
                        return true;
                    }
                }
            }
        }
        return false;
    }

    private static boolean checkTable(IndexedFile p, int index, byte[] key, byte[] primaryKey) {
        String query =
                String.format(
                        "select key from %s " + "where key = ? and value = ?", getTableName(index));
        try (PreparedStatement selectStatement = p.connection.prepareStatement(query)) {
            selectStatement.setBytes(1, key);
            selectStatement.setBytes(2, primaryKey);
            selectStatement.setFetchSize(0);
            ResultSet rs = selectStatement.executeQuery();
            return rs.next();
        } catch (SQLException e) {
            return false;
        }
    }

    @Override
    /** Equivalent to indexed_rewrite in libcob/fileio.c */
    public int rewrite_(int opt) {
        IndexedFile p = this.filei;

        p.write_cursor_open = true;

        if (this.access_mode == COB_ACCESS_SEQUENTIAL
                && !IndexedCursor.matchKeyHead(p.key, DBT_SET(this.keys[0].getField()))) {
            return COB_STATUS_21_KEY_INVALID;
        }

        p.key = DBT_SET(this.keys[0].getField());

        int ret = this.indexed_delete_internal(true);

        if (ret != COB_STATUS_00_SUCCESS) {
            p.write_cursor_open = false;
            return ret;
        }

        return this.indexed_write_internal(true, opt);
    }

    /** Equivalent to indexed_delete_internal in libcob/fileio.c */
    private int indexed_delete_internal(boolean rewrite) {
        IndexedFile p = this.filei;
        boolean closeCursor;

        p.write_cursor_open = true;
        closeCursor = true;

        if (this.access_mode != COB_ACCESS_SEQUENTIAL) {
            p.key = DBT_SET(this.keys[0].getField());
        }

        if (this.access_mode != COB_ACCESS_SEQUENTIAL && !keyExistsInTable(p, 0, p.key)) {
            return returnWith(p, closeCursor, 0, COB_STATUS_23_KEY_NOT_EXISTS);
        }

        // delete data from the primary table
        String query = String.format("delete from %s where key = ?", getTableName(0));
        try (PreparedStatement statement = p.connection.prepareStatement(query)) {
            statement.setBytes(1, p.key);
            statement.execute();
        } catch (SQLException e) {
            return returnWith(p, closeCursor, 0, COB_STATUS_30_PERMANENT_ERROR);
        }

        // delete data from sub tables
        for (int i = 1; i < this.nkeys; ++i) {
            try (PreparedStatement statement =
                    p.connection.prepareStatement(
                            String.format("delete from %s where value = ?", getTableName(i)))) {
                statement.setBytes(1, p.key);
                statement.execute();
            } catch (SQLException e) {
                return returnWith(p, closeCursor, 0, COB_STATUS_30_PERMANENT_ERROR);
            }
        }

        if (this.commitOnModification) {
            try {
                p.connection.commit();
            } catch (SQLException e) {
                return returnWith(p, closeCursor, 0, COB_STATUS_30_PERMANENT_ERROR);
            }
        }

        this.updateWhileReading = true;

        return COB_STATUS_00_SUCCESS;
    }

    @Override
    public int delete_() {
        return this.indexed_delete_internal(false);
    }

    @Override
    public void unlock_() {
        System.err.println("Unlocking INDEXED file is not implemented");
    }

    /** TODO: 準備中 */
    public void commitJdbcTransaction() {
        IndexedFile p = this.filei;
        try {
            p.connection.commit();
        } catch (SQLException e) {
            System.err.println("Failed to commit a transaction");
        }
    }

    /**
     * Delete all records in the file
     *
     * @return true if all records are deleted successfully, otherwise false
     */
    public boolean deleteAllRecords() {
        IndexedFile p = this.filei;
        try {
            for (int i = this.nkeys - 1; i >= 0; --i) {
                Statement statement = p.connection.createStatement();
                statement.execute("delete from " + getTableName(i));
                statement.close();
            }
            if (this.commitOnModification) {
                p.connection.commit();
            }
            return true;
        } catch (SQLException e) {
            return false;
        }
    }
}
