package jp.osscons.opensourcecobol.libcobj.common;

import jp.osscons.opensourcecobol.libcobj.data.AbstractCobolField;

public class CobolInspect {
	public static void init(AbstractCobolField var, int replacing) {}
	public static void start() {}
	public static void before (AbstractCobolField str) {}
	public static void after(AbstractCobolField str) {}
	public static void characters(AbstractCobolField f1) {}
	public static void all(AbstractCobolField f1, AbstractCobolField f2) {}
	public static void leading(AbstractCobolField f1, AbstractCobolField f2) {}
	public static void first(AbstractCobolField f1, AbstractCobolField f2) {}
	public static void trailing(AbstractCobolField f1, AbstractCobolField f2) {}
	public static void converting(AbstractCobolField f1, AbstractCobolField f2) {}
	public static void finish() {}
}
