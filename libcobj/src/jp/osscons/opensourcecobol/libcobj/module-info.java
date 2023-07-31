module jp.osscons.opensourcecobol.libcobj {
    requires org.xerial.sqlitejdbc;
    requires java.sql;
    requires java.management;

    exports jp.osscons.opensourcecobol.libcobj;
    exports jp.osscons.opensourcecobol.libcobj.call;
    exports jp.osscons.opensourcecobol.libcobj.common;
    exports jp.osscons.opensourcecobol.libcobj.data;
    exports jp.osscons.opensourcecobol.libcobj.exceptions;
    exports jp.osscons.opensourcecobol.libcobj.file;
    exports jp.osscons.opensourcecobol.libcobj.termio;
    exports jp.osscons.opensourcecobol.libcobj.ui;
}
