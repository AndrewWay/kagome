#!/bin/bash

#FOR L=12 ONLY

arccos() {
    scale=17
    if (( $(echo "$1 == 0" | bc -l) )); then
        echo "a(1)*2" | bc -l
    elif (( $(echo "(-1 <= $1) && ($1 < 0)" | bc -l) )); then
        echo "scale=${scale}; a(1)*4 - a(sqrt((1/($1^2))-1))" | bc -l
    elif (( $(echo "(0 < $1) && ($1 <= 1)" | bc -l) )); then
        echo "scale=${scale}; a(sqrt((1/($1^2))-1))" | bc -l
    else
        echo "input out of range"
        return 1
    fi
}

expTest(){
#Checks if number is in scientific notation. If it is, convert to standard notation.
ret=$1
#echo $ret >> expCheck.txt
test=`echo $1 | grep -o "E"`
if [[ $test =~ "E" ]];then
sig=`echo $1 | cut -f1 -d "E"`
exp=`echo $1 | egrep -o "E-[0-9]+" | egrep -o "[1-9]+"`
if [[ $1 =~ "+" ]];then
exp=0
fi
factor=`echo "10 ^ $exp" | bc -l`
ret=`echo "$sig / $factor" | bc -l`
fi
#echo $ret >> expCheck.txt
echo $ret
}


if [[ `cat conf0000.dat | wc -l` -ne 1296 ]];then
echo "Either the conf files are not from L=12 simulations, or conf0000.dat DNE"
echo "Script will fail. Exiting."
exit 1
fi
if [[ -s "debug.txt" ]];then
echo "Resetting debug.txt"
rm debug.txt
fi
PI=3.14159265359
Root3=1.73205080757
Normal="Normals.txt"
perpNorm="NormalPerpendiculars.txt"
AB="AB"
AC="AC"
AD="AD"
AE="AE"
AF="AF"
BC="BC"
BD="BD"
BE="BE"
BF="BF"
CD="CD"
CE="CE"
CF="CF"
DE="DE"
DF="DF"
EF="EF"
fnum=0
rm spinAngles.dat
touch spinAngles.dat
#printf "%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t\n" "Field" $AB $AC $AD $AE $AF $BC $BD $BE $BF $CD $CE $CF $DE $DF $EF >> spinAngles.dat
rm normAngles.dat
touch normAngles.dat
rm sumAngles.dat
touch sumAngles.dat
hcounter=1

for f in `ls | egrep -o "\<conf[0-9]+.dat"`
do
echo $f
#EXTRACT SPINS FROM FILE F
length=`cat $f | wc -l`
Spin1=`sed '1q;d' $f`
Spin2=`sed '37q;d' $f`
Spin3=`sed '433q;d' $f`
Spin4=`sed '469q;d' $f`
Spin5=`sed '865q;d' $f`
Spin6=`sed '901q;d' $f`

#DIVIDE FILE ROWS INTO INDIVIDUAL FORMATTED SPINS
spin1x=`echo $Spin1 | awk '{print $1}'`
spin1y=`echo $Spin1 | awk '{print $2}'`
spin1z=`echo $Spin1 | awk '{print $3}'`
spin2x=`echo $Spin2 | awk '{print $1}'`
spin2y=`echo $Spin2 | awk '{print $2}'`
spin2z=`echo $Spin2 | awk '{print $3}'`
spin3x=`echo $Spin3 | awk '{print $1}'`
spin3y=`echo $Spin3 | awk '{print $2}'`
spin3z=`echo $Spin3 | awk '{print $3}'`
spin4x=`echo $Spin4 | awk '{print $1}'`
spin4y=`echo $Spin4 | awk '{print $2}'`
spin4z=`echo $Spin4 | awk '{print $3}'`
spin5x=`echo $Spin5 | awk '{print $1}'`
spin5y=`echo $Spin5 | awk '{print $2}'`
spin5z=`echo $Spin5 | awk '{print $3}'`
spin6x=`echo $Spin6 | awk '{print $1}'`
spin6y=`echo $Spin6 | awk '{print $2}'`
spin6z=`echo $Spin6 | awk '{print $3}'`
#echo "Checkpoint 1"
echo $spin1x $spin1y $spin1z $spin2x $spin2y $spin2z $spin3x $spin3y $spin3z >> debug.txt
spin1x=`expTest $spin1x`
spin1y=`expTest $spin1y`
spin1z=`expTest $spin1z`
spin2x=`expTest $spin2x`
spin2y=`expTest $spin2y`
spin2z=`expTest $spin2z`
spin3x=`expTest $spin3x`
spin3y=`expTest $spin3y`
spin3z=`expTest $spin3z`
spin4x=`expTest $spin4x`
spin4y=`expTest $spin4y`
spin4z=`expTest $spin4z`
spin5x=`expTest $spin5x`
spin5y=`expTest $spin5y`
spin5z=`expTest $spin5z`
spin6x=`expTest $spin6x`
spin6y=`expTest $spin6y`
spin6z=`expTest $spin6z`
#echo "Checkpoint 1"

spin1x=`printf '%.17f' $spin1x`
spin1y=`printf '%.17f' $spin1y`
spin1z=`printf '%.17f' $spin1z`
spin2x=`printf '%.17f' $spin2x`
spin2y=`printf '%.17f' $spin2y`
spin2z=`printf '%.17f' $spin2z`
spin3x=`printf '%.17f' $spin3x`
spin3y=`printf '%.17f' $spin3y`
spin3z=`printf '%.17f' $spin3z`
spin4x=`printf '%.17f' $spin4x`
spin4y=`printf '%.17f' $spin4y`
spin4z=`printf '%.17f' $spin4z`
spin5x=`printf '%.17f' $spin5x`
spin5y=`printf '%.17f' $spin5y`
spin5z=`printf '%.17f' $spin5z`
spin6x=`printf '%.17f' $spin6x`
spin6y=`printf '%.17f' $spin6y`
spin6z=`printf '%.17f' $spin6z`

AppendedString=""
#CALCULATION OF ANGLES
#These are the vectors of reference for the zenith and azimuth angles 
normx=1
normy=1
normz=1
#Find the dot product between each spin and the normal vector 

DotAB=`echo "$spin1x*$spin2x+$spin1y*$spin2y+$spin1z*$spin2z" | bc -l`
DotAC=`echo "$spin1x*$spin3x+$spin1y*$spin3y+$spin1z*$spin3z" | bc -l`
DotAD=`echo "$spin1x*$spin4x+$spin1y*$spin4y+$spin1z*$spin4z" | bc -l`
DotAE=`echo "$spin1x*$spin5x+$spin1y*$spin5y+$spin1z*$spin5z" | bc -l`
DotAF=`echo "$spin1x*$spin6x+$spin1y*$spin6y+$spin1z*$spin6z" | bc -l`
DotBC=`echo "$spin2x*$spin3x+$spin2y*$spin3y+$spin2z*$spin3z" | bc -l`
DotBD=`echo "$spin2x*$spin4x+$spin2y*$spin4y+$spin2z*$spin4z" | bc -l`
DotBE=`echo "$spin2x*$spin5x+$spin2y*$spin5y+$spin2z*$spin5z" | bc -l`
DotBF=`echo "$spin2x*$spin6x+$spin2y*$spin6y+$spin2z*$spin6z" | bc -l`
DotCD=`echo "$spin3x*$spin4x+$spin3y*$spin4y+$spin3z*$spin4z" | bc -l`
DotCE=`echo "$spin3x*$spin5x+$spin3y*$spin5y+$spin3z*$spin5z" | bc -l`
DotCF=`echo "$spin3x*$spin6x+$spin3y*$spin6y+$spin3z*$spin6z" | bc -l`
DotDE=`echo "$spin4x*$spin5x+$spin4y*$spin5y+$spin4z*$spin5z" | bc -l`
DotDF=`echo "$spin4x*$spin6x+$spin4y*$spin6y+$spin4z*$spin6z" | bc -l`
DotEF=`echo "$spin5x*$spin6x+$spin5y*$spin6y+$spin5z*$spin6z" | bc -l`
echo $spin1x $spin1y $spin1z $spin2x $spin2y $spin2z >> debug.txt
echo $DotAB $DotBC $DotCD $DotDE $DotEF $DotAF >> debug.txt
DotAN=`echo "$spin1x*$normx+$spin1y*$normy+$spin1z*$normz" | bc -l`
DotBN=`echo "$spin2x*$normx+$spin2y*$normy+$spin2z*$normz" | bc -l`
DotCN=`echo "$spin3x*$normx+$spin3y*$normy+$spin3z*$normz" | bc -l`
DotDN=`echo "$spin4x*$normx+$spin4y*$normy+$spin4z*$normz" | bc -l`
DotEN=`echo "$spin5x*$normx+$spin5y*$normy+$spin5z*$normz" | bc -l`
DotFN=`echo "$spin6x*$normx+$spin6y*$normy+$spin6z*$normz" | bc -l`

#Find the length of each spin (should just be 1)
Length1=`echo "sqrt($spin1x*$spin1x+$spin1y*$spin1y+$spin1z*$spin1z)" | bc -l`
Length2=`echo "sqrt($spin2x*$spin2x+$spin2y*$spin2y+$spin2z*$spin2z)" | bc -l`
Length3=`echo "sqrt($spin3x*$spin3x+$spin3y*$spin3y+$spin3z*$spin3z)" | bc -l`
Length4=`echo "sqrt($spin4x*$spin4x+$spin4y*$spin4y+$spin4z*$spin4z)" | bc -l`
Length5=`echo "sqrt($spin5x*$spin5x+$spin5y*$spin5y+$spin5z*$spin5z)" | bc -l`
Length6=`echo "sqrt($spin6x*$spin6x+$spin6y*$spin6y+$spin6z*$spin6z)" | bc -l`
#Find the length of each normal vector.
normLen=`echo "sqrt($normx*$normx+$normy*$normy+$normz*$normz)" | bc -l`

tmpAB=`echo "$DotAB/($Length1*$Length2)" | bc -l`
tmpAC=`echo "$DotAC/($Length1*$Length3)" | bc -l`
tmpAD=`echo "$DotAD/($Length1*$Length4)" | bc -l`
tmpAE=`echo "$DotAE/($Length1*$Length5)" | bc -l`
tmpAF=`echo "$DotAF/($Length1*$Length6)" | bc -l`
tmpBC=`echo "$DotBC/($Length2*$Length3)" | bc -l`
tmpBD=`echo "$DotBD/($Length2*$Length4)" | bc -l`
tmpBE=`echo "$DotBE/($Length2*$Length5)" | bc -l`
tmpBF=`echo "$DotBF/($Length2*$Length6)" | bc -l`
tmpCD=`echo "$DotCD/($Length3*$Length4)" | bc -l`
tmpCE=`echo "$DotCE/($Length3*$Length5)" | bc -l`
tmpCF=`echo "$DotCF/($Length3*$Length6)" | bc -l`
tmpDE=`echo "$DotDE/($Length4*$Length5)" | bc -l`
tmpDF=`echo "$DotDF/($Length4*$Length6)" | bc -l`
tmpEF=`echo "$DotEF/($Length5*$Length6)" | bc -l`

tmpAN=`echo "$DotAN/($Length1*$normLen)" | bc -l`
tmpBN=`echo "$DotBN/($Length2*$normLen)" | bc -l`
tmpCN=`echo "$DotCN/($Length3*$normLen)" | bc -l`
tmpDN=`echo "$DotDN/($Length4*$normLen)" | bc -l`
tmpEN=`echo "$DotEN/($Length5*$normLen)" | bc -l`
tmpFN=`echo "$DotFN/($Length6*$normLen)" | bc -l`

#echo 'Dots/Length' ${Angle1:0:9} ${Angle2:0:9} ${Angle3:0:9}
echo "PRE-ARCCOS" >> debug.txt
echo $tmpAB $tmpBC $tmpCD $tmpDE $tmpEF $tmpAF >> debug.txt
#echo "Checkpoint 3"
AB=`arccos $tmpAB`
AC=`arccos $tmpAC`
AD=`arccos $tmpAD`
AE=`arccos $tmpAE`
AF=`arccos $tmpAF`
BC=`arccos $tmpBC`
BD=`arccos $tmpBD`
BE=`arccos $tmpBE`
BF=`arccos $tmpBF`
CD=`arccos $tmpCD`
CE=`arccos $tmpCE`
CF=`arccos $tmpCF`
DE=`arccos $tmpDE`
DF=`arccos $tmpDF`
EF=`arccos $tmpEF`
#echo "Checkpoint 4"
echo $AB $BC $CD $DE $EF $AF >> debug.txt
AN=`arccos $tmpAN`
BN=`arccos $tmpBN`
CN=`arccos $tmpCN`
DN=`arccos $tmpDN`
EN=`arccos $tmpEN`
FN=`arccos $tmpFN`
#echo "Checkpoint 5"
#echo "Angles $AB $BC $CD $DE $EF"  >> debug.txt
#Convert from radians to degrees
AB=`echo "$AB*180/$PI" | bc -l`
AC=`echo "$AC*180/$PI" | bc -l`
AD=`echo "$AD*180/$PI" | bc -l`
AE=`echo "$AE*180/$PI" | bc -l`
AF=`echo "$AF*180/$PI" | bc -l`
BC=`echo "$BC*180/$PI" | bc -l`
BD=`echo "$BD*180/$PI" | bc -l`
BE=`echo "$BE*180/$PI" | bc -l`
BF=`echo "$BF*180/$PI" | bc -l`
CD=`echo "$CD*180/$PI" | bc -l`
CE=`echo "$CE*180/$PI" | bc -l`
CF=`echo "$CF*180/$PI" | bc -l`
DE=`echo "$DE*180/$PI" | bc -l`
DF=`echo "$DF*180/$PI" | bc -l`
EF=`echo "$EF*180/$PI" | bc -l`

AN=`echo "$AN*180/$PI" | bc -l`
BN=`echo "$BN*180/$PI" | bc -l`
CN=`echo "$CN*180/$PI" | bc -l`
DN=`echo "$DN*180/$PI" | bc -l`
EN=`echo "$EN*180/$PI" | bc -l`
FN=`echo "$FN*180/$PI" | bc -l`
Asum=`echo "$AB + $AC + $AD + $AE + $AF" | bc -l`
Bsum=`echo "$AB + $BC + $BD + $BE + $BF" | bc -l`
Csum=`echo "$AC + $BC + $CD + $CE + $CF" | bc -l`
Dsum=`echo "$AD + $BD + $CD + $DE + $DF" | bc -l`
Esum=`echo "$AE + $BE + $CE + $DE + $EF" | bc -l`
Fsum=`echo "$AF + $BF + $CF + $DF + $EF" | bc -l`
AngleString=`printf "%3.3f %3.3f %3.3f %3.3f %3.3f %3.3f" $AB $BC $CD $DE $EF $AF` 
#echo "Checkpoint 6"
H=`cat fort.1 | head -n $hcounter | tail -n 1 | awk '{print $1}'`
hcounter=$((hcounter + 1))
H=`expTest $H`
echo $H $AngleString >> spinAngles.dat
echo $H $AN $BN $CN $DN $EN $FN  >> normAngles.dat
echo "$H"
#echo "checkpoint 7"
done
