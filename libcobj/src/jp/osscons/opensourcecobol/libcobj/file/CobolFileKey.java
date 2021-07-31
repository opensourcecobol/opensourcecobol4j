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
