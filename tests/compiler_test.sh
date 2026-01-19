#!/bin/bash

make
if [[ $? -eq 1 ]]; then
    echo -e "\e[31mCompilation failed\e[0m"
fi
mapfile -t valid_test < <(find ./valid_test/ -maxdepth 1 -type f \( -name "*.rz" -o -name "*.scm" \) )
mapfile -t bad_test < <(find ./bad_test/ -maxdepth 1 -type f \( -name "*.rz" -o -name "*.scm" \) )
mapfile -t lib_rizz < <(find ./lib_rizz/ -maxdepth 1 -type f \( -name "*.rz" -o -name "*.scm" \) )

echo -e "\e[1;97m-----------------------------------------------\e[0m"
echo -e "\e[1;33m--- VALID FILES TESTS ---\e[0m"

for f in "${valid_test[@]}"; do
    echo -e "\e[1;97m-----------------------------------------------\e[0m"
    ../glados-compiler $f > /dev/null
    if [[ $? -eq 1 ]]; then
        echo -e "\e[31m${f##*/} KO\e[0m"
    else
        echo -e "\e[32m${f##*/} OK\e[0m"
    fi
done

echo -e "\e[1;97m-----------------------------------------------\e[0m"
echo -e "\n\e[1;97m-----------------------------------------------\e[0m"
echo -e "\e[1;33m--- LIB RIZZ TESTS ---\e[0m"

for f in "${lib_rizz[@]}"; do
    echo -e "\e[1;97m-----------------------------------------------\e[0m"
    ../glados-compiler $f > /dev/null
    if [[ $? -eq 1 ]]; then
        echo -e "\e[31m${f##*/} KO\e[0m"
    else
        echo -e "\e[32m${f##*/} OK\e[0m"
    fi
done

echo -e "\e[1;97m-----------------------------------------------\e[0m"
echo -e "\n\e[1;97m-----------------------------------------------\e[0m"
echo -e "\e[1;33m--- INVALID FILES TESTS ---\e[0m"

for f in "${bad_test[@]}"; do
    echo -e "\e[1;97m-----------------------------------------------\e[0m"
    ../glados-compiler $f 2> /dev/null
    if [[ $? -eq 1 ]]; then
        echo -e "\e[32m${f##*/} OK\e[0m"
    else
        echo -e "\e[31m${f##*/} KO\e[0m"
    fi
done

echo -e "\e[1;97m-----------------------------------------------\e[0m"
