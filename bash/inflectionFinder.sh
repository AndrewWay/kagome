#!/bin/bash
#Point Of Inflection Finder
#Written by: Andrew Way 
#Description: Finds points of inflections from discrete data

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

#output="inflections.dat"
#rm $output
#touch $output
file=$1

length=`cat $file | wc -l`
length=$(( $length - 2 ))

for j in `seq 1 $length`
do
	propLine1=`cat $file | head -n $j | tail -n 1`
	propLine2=`cat $file | head -n $(($j + 1)) | tail -n 1`
	propLine3=`cat $file | head -n $(($j + 2)) | tail -n 1`
	x1=`echo $propLine1 | awk '{print $1}'`
	x2=`echo $propLine2 | awk '{print $1}'`
	x3=`echo $propLine3 | awk '{print $1}'`
	y1=`echo $propLine1 | awk '{print $3}'`
	y2=`echo $propLine2 | awk '{print $3}'`
	y3=`echo $propLine3 | awk '{print $3}'`

	x1=`expTest $x1`
	x2=`expTest $x2`
	x3=`expTest $x3`
	y1=`expTest $y1`
	y2=`expTest $y2`
	y3=`expTest $y3`

	D1=`echo "( $y2 - $y1 ) / ( $x2 - $x1 )" | bc -l`
	D2=`echo "( $y3 - $y2 ) / ( $x3 - $x2 )" | bc -l`
	SecDeriv=`echo "( $D2 - $D1 ) / ( $x2 - $x1 )" | bc -l`
	#echo $x1 $y1 $x2 $y2 $D1 $D2 $SecDeriv >> $output
	negative=`echo $SecDeriv'<'0 | bc -l`
	if [[ $negative -eq 1 ]];then
		echo $x1
		break;
	fi
done
#echo "Program complete."
