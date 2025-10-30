package jp.osscons.opensourcecobol.libcobj.file;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.Statement;
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

/**
 * Represents a cursor for navigating through indexed data in a CobolIndexedFile.
 */
enum CursorPosition {
    BEFORE_FIRST,
    AFTER_LAST,
    IN_TABLE,
}

/** Emulates a cursor in SQLite */
final class IndexedCursor {
    private final Connection conn;
    private byte[] key;
    private final int tableIndex;
    private final boolean isDuplicate;
    private int comparator;

    // variables for cursor state
    private boolean firstFetch;
    private CursorPosition position;
    private Optional<FetchResult> previousFetchResult;

    private IndexedCursor(
            Connection conn, byte[] key, int tableIndex, boolean isDuplicate, int comparator) {
        this.conn = conn;
        this.key = key;
        this.tableIndex = tableIndex;
        this.isDuplicate = isDuplicate;
        this.comparator = comparator;

        this.firstFetch = true;
        this.position = CursorPosition.IN_TABLE;
        this.previousFetchResult = Optional.empty();
    }

    static Optional<IndexedCursor> createCursor(
            Connection conn, byte[] key, int tableIndex, boolean isDuplicate, int comparator) {
        return Optional.of(new IndexedCursor(conn, key, tableIndex, isDuplicate, comparator));
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

    Optional<IndexedCursor> reloadCursor() {
        return Optional.of(this);
    }

    void close() {}

    void setComparator(int comparator) {
        this.comparator = comparator;
    }

    int getComparator() {
        return this.comparator;
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

    private static String getCompOperator(int comparator) {
        if (comparator == CobolIndexedFile.COB_EQ) {
            // for partial key match, we use ">=" instead of "="
            return ">=";
        } else if (comparator == CobolIndexedFile.COB_GE) {
            return ">=";
        } else if (comparator == CobolIndexedFile.COB_LE) {
            return "<=";
        } else if (comparator == CobolIndexedFile.COB_GT) {
            return ">";
        } else if (comparator == CobolIndexedFile.COB_LT) {
            return "<";
        } else {
            return null;
        }
    }

    Optional<FetchResult> fetchFirstRecord() {
        this.previousFetchResult = Optional.empty();
        this.position = CursorPosition.IN_TABLE;

        final boolean isPrimaryTable = this.tableIndex == 0;
        final String primaryTable = CobolIndexedFile.getTableName(0);
        final String subTable = CobolIndexedFile.getTableName(this.tableIndex);

        String query;
        if (isPrimaryTable) {
            query = String.format("select key, value from %s order by key limit 1", primaryTable);
        } else if (this.isDuplicate) {
            query =
                    String.format(
                            "select key, value from %s order by key, dupNo limit 1", subTable);
        } else {
            query = String.format("select key, value from %s order by key limit 1", subTable);
        }
        try (Statement stmt = this.conn.createStatement();
                ResultSet rs = stmt.executeQuery(query)) {
            if (rs.next()) {
                byte[] key = rs.getBytes("key");
                byte[] value = rs.getBytes("value");
                if (isDuplicate) {
                    int dupNo = rs.getInt("dupNo");
                    return Optional.of(new FetchResult(key, value, dupNo));
                } else {
                    return Optional.of(new FetchResult(key, value, 0));
                }
            } else {
                return Optional.empty();
            }
        } catch (Exception e) {
            return Optional.empty();
        }
    }

    Optional<FetchResult> fetchLastRecord() {
        this.previousFetchResult = Optional.empty();
        this.position = CursorPosition.IN_TABLE;

        final boolean isPrimaryTable = this.tableIndex == 0;
        final String primaryTable = CobolIndexedFile.getTableName(0);
        final String subTable = CobolIndexedFile.getTableName(this.tableIndex);

        String query;
        if (isPrimaryTable) {
            query =
                    String.format(
                            "select key, value from %s order by key desc limit 1", primaryTable);
        } else if (this.isDuplicate) {
            query =
                    String.format(
                            "select key, value from %s order by key desc, dupNo desc limit 1",
                            subTable);
        } else {
            query = String.format("select key, value from %s order by key desc limit 1", subTable);
        }
        try (Statement stmt = this.conn.createStatement();
                ResultSet rs = stmt.executeQuery(query)) {
            if (rs.next()) {
                byte[] key = rs.getBytes("key");
                byte[] value = rs.getBytes("value");
                if (isDuplicate) {
                    int dupNo = rs.getInt("dupNo");
                    return Optional.of(new FetchResult(key, value, dupNo));
                } else {
                    return Optional.of(new FetchResult(key, value, 0));
                }
            } else {
                return Optional.empty();
            }
        } catch (Exception e) {
            return Optional.empty();
        }
    }

    Optional<FetchResult> forwardNextRecord() {
        this.position = CursorPosition.IN_TABLE;

        final boolean isPrimaryTable = this.tableIndex == 0;
        final String primaryTable = CobolIndexedFile.getTableName(0);
        final String subTable = CobolIndexedFile.getTableName(this.tableIndex);

        final String compOperator;
        byte[] key;
        if (this.previousFetchResult.isPresent()) {
            compOperator = ">";
            key = this.previousFetchResult.get().key;
        } else {
            compOperator = ">=";
            key = this.key;
        }

        String query;
        if (isPrimaryTable) {
            query =
                    String.format(
                            "select key, value from %s where key %s ? order by key limit 1",
                            primaryTable, compOperator);
        } else if (this.isDuplicate) {
            query =
                    String.format(
                            "select %s.key, %s.value, %s.dupNo from "
                                    + "%s join %s on %s.value = %s.key "
                                    + "where (%s.key == ? and %s.dupNo %s ?) or %s.key %s ? "
                                    + "order by %s.key, %s.dupNo limit 1",
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
                            subTable);
        } else {
            query =
                    String.format(
                            "select %s.key, %s.value from "
                                    + "%s join %s on %s.value = %s.key "
                                    + "where %s.key %s ? "
                                    + "order by %s.key limit 1",
                            subTable,
                            primaryTable,
                            subTable,
                            primaryTable,
                            subTable,
                            primaryTable,
                            subTable,
                            compOperator,
                            subTable);
        }

        try (PreparedStatement stmt = this.conn.prepareStatement(query)) {
            stmt.setBytes(1, key);
            if (this.isDuplicate) {
                int dupNo =
                        this.previousFetchResult.isPresent()
                                ? this.previousFetchResult.get().dupNo
                                : 0;
                stmt.setInt(2, dupNo);
                stmt.setBytes(3, key);
            }
            try (ResultSet rs = stmt.executeQuery()) {
                if (rs.next()) {
                    byte[] fetchedKey = rs.getBytes("key");
                    byte[] value = rs.getBytes("value");
                    if (isDuplicate) {
                        int dupNo = rs.getInt("dupNo");
                        return Optional.of(new FetchResult(fetchedKey, value, dupNo));
                    } else {
                        return Optional.of(new FetchResult(fetchedKey, value, 0));
                    }
                } else {
                    this.position = CursorPosition.AFTER_LAST;
                    return Optional.empty();
                }
            }
        } catch (Exception e) {
            return Optional.empty();
        }
    }

    Optional<FetchResult> backwardPrevRecord() {
        this.position = CursorPosition.IN_TABLE;

        final boolean isPrimaryTable = this.tableIndex == 0;
        final String primaryTable = CobolIndexedFile.getTableName(0);
        final String subTable = CobolIndexedFile.getTableName(this.tableIndex);

        final String compOperator;
        byte[] key;
        if (this.previousFetchResult.isPresent()) {
            compOperator = "<";
            key = this.previousFetchResult.get().key;
        } else {
            compOperator = "<=";
            key = this.key;
        }

        String query;
        if (isPrimaryTable) {
            query =
                    String.format(
                            "select key, value from %s where key %s ? order by key desc limit 1",
                            primaryTable, compOperator);
        } else if (this.isDuplicate) {
            query =
                    String.format(
                            "select %s.key, %s.value, %s.dupNo from "
                                    + "%s join %s on %s.value = %s.key "
                                    + "where (%s.key == ? and %s.dupNo %s ?) or %s.key %s ? "
                                    + "order by %s.key desc, %s.dupNo desc limit 1",
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
                            subTable);
        } else {
            query =
                    String.format(
                            "select %s.key, %s.value from "
                                    + "%s join %s on %s.value = %s.key "
                                    + "where %s.key %s ? "
                                    + "order by %s.key desc limit 1",
                            subTable,
                            primaryTable,
                            subTable,
                            primaryTable,
                            subTable,
                            primaryTable,
                            subTable,
                            compOperator,
                            subTable);
        }

        try (PreparedStatement stmt = this.conn.prepareStatement(query)) {
            stmt.setBytes(1, key);
            if (this.isDuplicate) {
                int dupNo =
                        this.previousFetchResult.isPresent()
                                ? this.previousFetchResult.get().dupNo
                                : 0;
                stmt.setInt(2, dupNo);
                stmt.setBytes(3, key);
            }
            try (ResultSet rs = stmt.executeQuery()) {
                if (rs.next()) {
                    byte[] fetchedKey = rs.getBytes("key");
                    byte[] value = rs.getBytes("value");
                    if (isDuplicate) {
                        int dupNo = rs.getInt("dupNo");
                        return Optional.of(new FetchResult(fetchedKey, value, dupNo));
                    } else {
                        return Optional.of(new FetchResult(fetchedKey, value, 0));
                    }
                } else {
                    this.position = CursorPosition.AFTER_LAST;
                    return Optional.empty();
                }
            }
        } catch (Exception e) {
            return Optional.empty();
        }
    }

    private String getSortOrderForStartStatement(int comparator) {
        if (comparator == CobolIndexedFile.COB_LT || comparator == CobolIndexedFile.COB_LE) {
            return "desc";
        } else {
            return "asc";
        }
    }

    Optional<FetchResult> fetchRecord() {
        this.previousFetchResult = Optional.empty();
        this.position = CursorPosition.IN_TABLE;

        final boolean isPrimaryTable = this.tableIndex == 0;
        final String primaryTable = CobolIndexedFile.getTableName(0);
        final String subTable = CobolIndexedFile.getTableName(this.tableIndex);
        final String compOperator = this.getCompOperator(this.comparator);
        final String sortOrder = getSortOrderForStartStatement(this.comparator);

        String query;
        if (isPrimaryTable) {
            query =
                    String.format(
                            "select key, value from %s where key %s ? order by key %s",
                            primaryTable, compOperator, sortOrder);
        } else if (this.isDuplicate) {
            query =
                    String.format(
                            "select %s.key, %s.value, %s.dupNo from "
                                    + "%s join %s on %s.value = %s.key "
                                    + "where %s.key %s ? "
                                    + "order by %s.key %s, %s.dupNo %s",
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
                            sortOrder,
                            subTable,
                            sortOrder);
        } else {
            query =
                    String.format(
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
        byte[] key;
        if (this.previousFetchResult.isPresent()) {
            key = this.previousFetchResult.get().key;
        } else {
            key = this.key;
        }

        try (PreparedStatement stmt = this.conn.prepareStatement(query)) {
            stmt.setBytes(1, key);
            try (ResultSet rs = stmt.executeQuery()) {
                if (rs.next()) {
                    byte[] fetchedKey = rs.getBytes("key");
                    if (this.comparator == CobolIndexedFile.COB_EQ
                            && !matchKeyHead(this.key, fetchedKey)) {
                        return Optional.empty();
                    } else if (this.comparator == CobolIndexedFile.COB_GT) {
                        // exclude records that partially match the key
                        while (matchKeyHead(this.key, fetchedKey)) {
                            if (!rs.next()) {
                                return Optional.empty();
                            }
                            fetchedKey = rs.getBytes("key");
                        }
                    }
                    byte[] value = rs.getBytes("value");
                    if (isDuplicate) {
                        int dupNo = rs.getInt("dupNo");
                        return Optional.of(new FetchResult(fetchedKey, value, dupNo));
                    } else {
                        return Optional.of(new FetchResult(fetchedKey, value));
                    }
                } else {
                    this.position = CursorPosition.AFTER_LAST;
                    return Optional.empty();
                }
            }
        } catch (Exception e) {
            return Optional.empty();
        }
    }

    Optional<FetchResult> next() {
        if (this.position == CursorPosition.AFTER_LAST) {
            return Optional.empty();
        }

        Optional<FetchResult> result;
        if (this.firstFetch) {
            if (this.position == CursorPosition.BEFORE_FIRST) {
                result = fetchFirstRecord();
            } else {
                result = fetchRecord();
            }
        } else {
            result = forwardNextRecord();
        }
        if (result.isPresent()) {
            this.firstFetch = false;
            this.position = CursorPosition.IN_TABLE;
        }
        this.previousFetchResult = result;
        return result;
    }

    Optional<FetchResult> prev() {
        if (this.position == CursorPosition.BEFORE_FIRST) {
            return Optional.empty();
        }

        Optional<FetchResult> result;
        if (this.firstFetch) {
            if (this.position == CursorPosition.AFTER_LAST) {
                result = fetchLastRecord();
            } else {
                result = fetchRecord();
            }
        } else {
            result = backwardPrevRecord();
        }

        if (result.isPresent()) {
            this.firstFetch = false;
            this.position = CursorPosition.IN_TABLE;
        }
        this.previousFetchResult = result;
        return result;
    }

    boolean moveToFirst() {
        this.firstFetch = true;
        this.position = CursorPosition.BEFORE_FIRST;
        return true;
    }

    boolean moveToLast() {
        this.firstFetch = true;
        this.position = CursorPosition.AFTER_LAST;
        return true;
    }
}
