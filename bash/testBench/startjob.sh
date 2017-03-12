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
echo "S T A R T - J O B" 
tput sgr0
 
tput cup 7 15
echo "1. Start Job"
 
tput cup 8 15
echo "2. Check Job"
 
tput cup 9 15
echo "3. View Code"

tput cup 10 15
echo "4. Job Management"
 
tput cup 11 15
echo "5. Main Menu"
 
# Set bold mode 
tput bold
tput cup 13 15
read -p "Enter your choice [1-5] " choice
tput clear
tput sgr0
tput rc
echo "CHOICE: " $choice
if [ $choice -eq 1 ]; then
#get the name of the file to be compiled and used
sim_code=`cat settings.txt | grep "sim_code" | awk '{print $2}'`
#get the parameters being used in the simulation

./jobmanagement.sh
elif [ $choice -eq 2 ]; then
./jobmanagement.sh
elif [ $choice -eq 3 ]; then
./jobmanagement.sh
elif [ $choice -eq 4 ]; then
./jobmanagement.sh
elif [ $choice -eq 5 ]; then
./main.sh
else
./jobmanagement.sh
fi
 
