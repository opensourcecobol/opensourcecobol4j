$MODULES = "NC", "SM", "IC", "SQ", "IX", "ST", "SG", "OB", "IF", "RL"

cd ../tests/cobol85;
foreach ($MODULE in $MODULES){
    cd ./$MODULE;
    sh ../report.sh
    type report.txt
    cd ../
}
sh ./summary.sh $MODULES > summary.log
type summary.log
cd ../../win
