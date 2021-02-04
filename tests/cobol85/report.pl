#
# Copyright (C) 2001-2009 Keisuke Nishida
# Copyright (C) 2007-2009 Roger While
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this software; see the file COPYING.  If not, write to
# the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
# Boston, MA 02111-1307 USA

my $opt = shift;

my $compile;
my $compile_module;

if ($opt) {
	#$compile = "ocobc -std=cobol85 -x $opt";
	#$compile_module = "ocobc -std=cobol85 -m $opt";
	$compile = "cobc -std=cobol85 -x $opt";
	$compile_module = "cobc -std=cobol85 -m $opt";
} else {
	#$compile = "ocobc -std=cobol85 -x";
	#$compile_module = "ocobc -std=cobol85 -m";
	$compile = "cobc -std=cobol85 -x";
	$compile_module = "cobc -std=cobol85 -m";
}

my $num_progs = 0;
my $test_skipped = 0;
my $compile_error = 0;
my $execute_error = 0;

my $total_all = 0;
my $total_pass = 0;
my $total_fail = 0;
my $total_deleted = 0;
my $total_inspect = 0;
my $total_ok = 0;
my $ret = 0;

$ENV{"COB_SWITCH_1"} = "ON";
$ENV{"COB_SWITCH_2"} = "OFF";

$skip{SQ207M} = 1;
$skip{SG102A} = 1;
$skip{SG103A} = 1;
$skip{SG201A} = 1;
$skip{SG202A} = 1;
$skip{SG203A} = 1;
$skip{OBNC1M} = 1;
$skip{OBNC2M} = 1;

# temp
#$skip{SQ101M}=1;
#$skip{SQ102A}=1;
#$skip{SQ103A}=1;
#$skip{SQ104A}=1;
#$skip{SQ105A}=1;
#$skip{SQ106A}=1;
#$skip{SQ107A}=1;
#$skip{SQ108A}=1;
#$skip{SQ109M}=1;
#$skip{SQ110M}=1;
#$skip{SQ111A}=1;
#$skip{SQ112A}=1;
#$skip{SQ113A}=1;
#$skip{SQ114A}=1;
#$skip{SQ115A}=1;
#$skip{SQ116A}=1;
#$skip{SQ117A}=1;
#$skip{SQ121A}=1;
#$skip{SQ122A}=1;
#$skip{SQ123A}=1;
#$skip{SQ124A}=1;
#$skip{SQ125A}=1;
# $skip{SQ126A}=1;
#$skip{SQ127A}=1;
#$skip{SQ128A}=1;
#$skip{SQ129A}=1;
#$skip{SQ130A}=1;
#$skip{SQ131A}=1;
#$skip{SQ132A}=1;
#$skip{SQ133A}=1;
#$skip{SQ134A}=1;
#$skip{SQ135A}=1;
#$skip{SQ136A}=1;
#$skip{SQ137A}=1;
#$skip{SQ138A}=1;
#$skip{SQ139A}=1;
#$skip{SQ140A}=1;
#$skip{SQ141A}=1;
#$skip{SQ142A}=1;
#$skip{SQ143A}=1;
#$skip{SQ144A}=1;
#$skip{SQ146A}=1;
#$skip{SQ147A}=1;
#$skip{SQ148A}=1;
#$skip{SQ149A}=1;
#$skip{SQ150A}=1;
#$skip{SQ151A}=1;
#$skip{SQ152A}=1;
#$skip{SQ153A}=1;
#$skip{SQ154A}=1;
#$skip{SQ155A}=1;
#$skip{SQ156A}=1;
#$skip{SQ201M}=1;
#$skip{SQ202A}=1;
#$skip{SQ203A}=1;
#$skip{SQ204A}=1;
#$skip{SQ205A}=1;
#$skip{SQ206A}=1;
#$skip{SQ207M}=1;
#$skip{SQ208M}=1;
#$skip{SQ209M}=1;
#$skip{SQ210M}=1;
#$skip{SQ211A}=1;
#$skip{SQ212A}=1;
#$skip{SQ213A}=1;
#$skip{SQ214A}=1;
#$skip{SQ215A}=1;
#$skip{SQ216A}=1;
#$skip{SQ217A}=1;
#$skip{SQ218A}=1;
#$skip{SQ219A}=1;
#$skip{SQ220A}=1;
#$skip{SQ221A}=1;
#$skip{SQ222A}=1;
#$skip{SQ223A}=1;
#$skip{SQ224A}=1;
#$skip{SQ225A}=1;
#$skip{SQ226A}=1;
#$skip{SQ227A}=1;
#$skip{SQ228A}=1;
#$skip{SQ229A}=1;
#$skip{SQ230A}=1;
#$skip{SQ302M}=1;
#$skip{SQ303M}=1;
#$skip{SQ401M}=1;

open (LOG, "> report.txt") or die;
print LOG "Filename    total pass fail deleted inspect\n";
print LOG "--------    ----- ---- ---- ------- -------\n";

foreach $in (glob("lib/*.CBL")) {
	print "$compile_module $in\n";
	system ("$compile_module $in");
}

foreach $in (sort (glob("*.{CBL,SUB}"))) {
  my $exe = $in;
  my $cmd;
  $exe =~ s/\.CBL//;
  $exe =~ s/\.SUB//;
  $cmd = "./$exe";
  if (-e "./$exe.DAT") {
    $cmd = "./$exe < $exe.DAT";
  }
  printf LOG "%-12s", $in;
  if ($skip{$exe} || $exe =~ /^..[34]0/) {
    $test_skipped++;
    print LOG "  ----- test skipped -----\n";
  } else {
    $num_progs++;
    $copy = ($exe =~ /^SM/) ? "-I ../copy" : "";
    print "$compile_module $copy $in && $cmd\n";
    if ($in eq "SM206A.CBL") {
      $ret = system ("$compile_module -fdebugging-line $copy $in");
    } else {
      $ret = system ("$compile_module $copy $in");
    }
    if ($ret != 0) {
      $compile_error++;
      print LOG "  ===== compile error =====\n";
    } else {
      if ($in =~ /\.CBL/) {
	if ($ENV{'DB_HOME'}) {
		system ("rm -rf XXXXX*; rm -rf $ENV{'DB_HOME'}/XXXXX*");
	} else {
		system ("rm -rf XXXXX*");
	}
      }
      ## if (system ("java $cmd > $exe.out") != 0) {
      if (system ("java $exe > $exe.out") != 0) {
      #if (system ("ocobcrun $exe > $exe.out.org") != 0) {
	$execute_error++;
	print LOG "  ***** execute error *****\n";
      } else {
	my $total   = 0;
	my $pass    = 0;
	my $fail    = 0;
	my $deleted = 0;
	my $inspect = 0;
	if (open (PRT, "report.log")) {
	  while (<PRT>) {
	    if (/^ *([0-9]+) *OF *([0-9]+) *TESTS WERE/) {
	      $total += $2;
	      $pass += $1;
	    } elsif (/^ *([0-9NO]+) *TEST\(S\) ([A-Z]+)/) {
	      my $num = $1 eq "NO" ? 0 : $1;
	      if ($2 eq "FAILED") {
		$fail += $num;
	      } elsif ($2 eq "DELETED") {
		$deleted += $num;
	      } elsif ($2 eq "REQUIRE") {
		$inspect += $num;
	      }
	    }
	  }
	}
	printf LOG ("%5s %4s %4s %7s %7s %s\n",
		    $total, $pass, $fail, $deleted, $inspect,
		    $fail == 0 ? "OK" : "");
	$total_all += $total;
	$total_pass += $pass;
	$total_fail += $fail;
	$total_deleted += $deleted;
	$total_inspect += $inspect;
	$total_ok++ if $fail == 0;
	rename ("report.log", "$exe.log");
	unlink "$exe.out" if (-s "$exe.out" == 0);
      }
    }
  }
}

print LOG "--------    ----- ---- ---- ------- -------\n";
printf LOG ("Total       %5s %4s %4s %7s %7s\n",
	    $total_all, $total_pass, $total_fail, $total_deleted,
	    $total_inspect);
printf LOG ("%%           100.0 %4.1f %4.1f    %4.1f    %4.1f\n\n",
	    100 * $total_pass / $total_all,
	    100 * $total_fail / $total_all,
	    100 * $total_deleted / $total_all,
	    100 * $total_inspect / $total_all) if $total_all;

printf LOG ("Number of programs:    %2s\n", $num_progs);
printf LOG ("Successfully executed: %2s (%5.2f%%)\n",
	    $total_ok, 100 * $total_ok / $num_progs);
printf LOG ("Compile error:         %2s (%5.2f%%)\n",
	    $compile_error, 100 * $compile_error / $num_progs);
printf LOG ("Execute error:         %2s (%5.2f%%)\n",
	    $execute_error, 100 * $execute_error / $num_progs);
