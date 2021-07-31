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
