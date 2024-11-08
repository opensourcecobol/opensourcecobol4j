## Process this file with automake to produce Makefile.in
##
## Copyright (C) 2003-2009 Keisuke Nishida
## Copyright (C) 2008-2009 Roger While
##

MODULES = NC SM IC SQ IX ST SG OB IF RL
MODULES_MAKEFILES = NC/Makefile SM/Makefile IC/Makefile SQ/Makefile IX/Makefile ST/Makefile SG/Makefile OB/Makefile IF/Makefile RL/Makefile
EXTRA_MODULES = CM DB RW
EXTRA_MODULES_MAKEFILES = CM/Makefile DB/Makefile RW/Makefile

EXTRA_DIST = EXEC85.conf.in expand.pl report.pl summary.pl summary.txt \
	NC.txt SM.txt IC.txt SQ.txt RL.txt IX.txt ST.txt SG.txt OB.txt IF.txt CM.txt DB.txt RW.txt ifedit.sh
CLEANFILES = EXEC85 summary.log

COBOL = cobj -std=cobol85

COB85DIR = "`cd $(srcdir) && pwd`"

all: $(MODULES_MAKEFILES) $(EXTRA_MODULES_MAKEFILES)

test: $(MODULES_MAKEFILES)
	@for m in $(MODULES); do \
	  (cd $$m && make test) \
	done
	perl $(srcdir)/summary.pl $(MODULES) > summary.log
	@for MODULE in $(MODULES); do\
		(cat $$MODULE/report.txt) \
	done
	cat summary.log
	cat summary.log | tail -n 1 | awk '$$4 != 0 || $$5 != 0 || $$2 != $$3 {exit 1}'

test-extra: $(EXTRA_MODULES_MAKEFILES)
	@for m in $(EXTRA_MODULES); do \
	  (cd $$m && make test) \
	done
	perl $(srcdir)/summary.pl $(EXTRA_MODULES) > summary.log
	@for MODULE in $(EXTRA_MODULES); do\
		(cat $$MODULE/report.txt) \
	done
	cat summary.log

save:
	@for m in $(MODULES) $(EXTRA_MODULES); do \
	  (cp $$m/report.txt $$m.txt) \
	done
	cp summary.log summary.txt

diff:
	@for m in $(MODULES) $(EXTRA_MODULES); do \
	  (diff $$m.txt $$m/report.txt) \
	done
	diff summary.txt summary.log

clean-local:
	rm -rf copy copyalt $(MODULES_MAKEFILES) $(EXTRA_MODULES_MAKEFILES)

clean-debug:
	rm -rf */*.class */tmp.cbl */XXXXX*

clean-all:
	rm -rf */*.class */*.java */*.out */*.log */tmp.cbl */XXXXX* */report.txt summary.log

$(MODULES_MAKEFILES):
	@echo "test:"               > $@
	@echo "	. ../../atconfig; . ../../atlocal; export PATH; perl $(COB85DIR)/report.pl" >> $@
	@echo "	@cat report.txt" >> $@

$(EXTRA_MODULES_MAKEFILES):
	@echo "test:"               > $@
	@echo "	. ../../atconfig; . ../../atlocal; export PATH; perl $(COB85DIR)/report.pl" >> $@
	@echo "	@cat report.txt" >> $@
