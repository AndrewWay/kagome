#!/bin/bash
 
# clear the screen
tput clear
 
# Move cursor to screen location X,Y (top left is 0,0)
tput cup 3 15
 
# Set a foreground colour using ANSI escape
tput setaf 3
echo "Kago V0.0"
tput sgr0
 
tput cup 5 17
# Set reverse video mode
tput rev
echo "A N A L Y S I S" 
tput sgr0

tput cup 7 15
echo "1. custom_script1"
tput cup 8 15
echo "2. custom_script2"
tput cup 9 15
echo "3. custom_script3"
tput cup 10 15
echo "4. custom_script4"
tput cup 11 15
echo "5. custom_script5"
tput cup 12 15
echo "6. Main Menu"
 
# Set bold mode 
tput bold
tput cup 14 15
read -p "Enter your choice [1-6] " choice
#tput clear
tput sgr0
tput rc
#Store custom script addresses in a file, read the names from those files 
#input i will run the script in directory i on the data currently inside the workbench
#if [ $choice -eq 1 ]; then
#./jobmanagement.sh
#elif [ $choice -eq 2 ]; then
#./main.sh
#elif [ $choice -eq 3 ]; then
#./settings.sh
#elif [ $choice -eq 4 ]; then
#echo ""
#else
#./main.sh #Script re runs for any other output. Change this so there is a while loop
#fi

if [ $choice -eq 6 ];then
    ./main.sh
else
    tput cup 16 
    echo "Running script "$choice
    tput cup 17
    read -p "Press any key to repeat" choice
    ./analysis.sh
fi
