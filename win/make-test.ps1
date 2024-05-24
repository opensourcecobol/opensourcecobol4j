$MODULES = "NC", "SM", "IC", "SQ", "IX", "ST", "SG", "OB", "IF", "RL"

cd ../tests/cobol85;
foreach ($MODULE in $MODULES){
    cd ./$MODULE;
    perl ../report.pl
    type report.txt
    cd ../
}
perl ./summary.pl $MODULES > summary.log
type summary.log
cd ../../win