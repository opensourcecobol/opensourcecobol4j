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

$enable_c = 0;

if ($opt) {
    if($enable_c == 1) {
	    $compile = "ocobc --save-temp=csrc -std=cobol85 -x $opt";
	    $compile_module = "ocobc --save-temp=csrc -std=cobol85 -m $opt";
    } else {
	    $compile = "cobc -std=cobol85 -x $opt";
	    $compile_module = "cobc -std=cobol85 -m $opt";
    }
} else {
    if($enable_c == 1) {
	    $compile = "ocobc --save-temp=csrc -std=cobol85 -x";
	    $compile_module = "ocobc --save-temp=csrc -std=cobol85 -m";
    } else {
	    $compile = "cobc -std=cobol85 -x";
	    $compile_module = "cobc -std=cobol85 -m";
    }
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


$skip{IX101A}=1;
$skip{IX102A}=1;
$skip{IX103A}=1;
$skip{IX104A}=1;
$skip{IX105A}=1;
$skip{IX106A}=1;
$skip{IX107A}=1;
$skip{IX108A}=1;
$skip{IX109A}=1;
$skip{IX110A}=1;
$skip{IX111A}=1;
$skip{IX112A}=1;
$skip{IX113A}=1;
$skip{IX114A}=1;
$skip{IX115A}=1;
$skip{IX116A}=1;
$skip{IX117A}=1;
$skip{IX118A}=1;
$skip{IX119A}=1;
$skip{IX120A}=1;
$skip{IX121A}=1;
$skip{IX201A}=1;
$skip{IX202A}=1;
$skip{IX203A}=1;
$skip{IX204A}=1;
$skip{IX205A}=1;
$skip{IX206A}=1;
$skip{IX207A}=1;
$skip{IX208A}=1;
$skip{IX209A}=1;
$skip{IX210A}=1;
$skip{IX211A}=1;
$skip{IX212A}=1;
$skip{IX213A}=1;
$skip{IX214A}=1;
$skip{IX215A}=1;
$skip{IX216A}=1;
#$skip{IX217A}=1;
#$skip{IX218A}=1;


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
  $cmd = "";
  if($enable_c == 1) {
    $cmd = "cobcrun $exe";
  } else {
    $cmd = "java -Xss4m $exe";
  }
  if (-e "./$exe.DAT") {
    $cmd = "$cmd < $exe.DAT";
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
      system ("cp $in tmp.cbl");
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
      $exec_result = 0;
      if($enable_c == 1) {
          $exec_result = system ("$cmd > $exe.out.org");
      } else {
          $exec_result = system ("$cmd > $exe.out");
      }
      if ($exec_result != 0) {
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
