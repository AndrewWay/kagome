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
echo "M A I N - M E N U" 
tput sgr0
 
tput cup 7 15
echo "1. Job Management"
 
tput cup 8 15
echo "2. Analysis"
 
tput cup 9 15
echo "3. Settings"
 
tput cup 10 15
echo "4. Database"
 
# Set bold mode 
tput bold
tput cup 12 15
read -p "Enter your choice [1-4] " choice
tput clear
tput sgr0
tput rc
echo "CHOICE: " $choice
if [ $choice -eq 1 ]; then
./startjob.sh
elif [ $choice -eq 2 ]; then
./checkjob.sh
elif [ $choice -eq 3 ]; then
./settings.sh
else
./mainmenu.sh
fi
 
