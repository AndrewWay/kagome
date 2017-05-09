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
echo "1. Submit Job"
tput cup 8 15
echo "2. Set Parameters"
tput cup 9 15
echo "3. Job Management"
tput cup 10 15
echo "4. Main Menu"
 
# Set bold mode 
tput bold
tput cup 13 15
read -p "Enter your choice [1-5] " choice
tput sgr0

if [ $choice -eq 1 ]; then
    ./dp.sh
    stat=$?
    tput cup 14 15 
    if [ $stat == 1 ]; then
	tput setaf 1
	echo "Sim code does not exist"
    elif [ $stat == 2 ]; then
	tput setaf 3
        echo "Error: A job using these parameters already exists" 
    else
	tput setaf 2
	echo "Job running"
    fi
    tput cup 15 15
    tput setaf 7
    read -p "Press any key to continue" input
    ./startjob.sh
elif [ $choice -eq 2 ]; then
nano parameters.txt
./startjob.sh
elif [ $choice -eq 3 ]; then
./jobmanagement.sh
elif [ $choice -eq 4 ]; then
./main.sh
else
./startjob.sh
fi
 
