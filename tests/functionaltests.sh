#!/bin/bash

BOLD="\e[1m"
RED="\e[31m"
GREEN="\e[32m"
RESET="\e[0m"

results=0

files=("lisp/builtins/builtins1" "lisp/builtins/builtins2" "lisp/builtins/builtins3"
    "lisp/conditions/if1" "lisp/conditions/if2" "lisp/conditions/if3"
    "lisp/errors/error1" "lisp/errors/error2"
    "lisp/functions/function1" "lisp/functions/function2"
)
echo -e "${BOLD}Building binary glados${RESET}"
make 1> /dev/null
# make re 1> /dev/null
if [ $? -ne 0 ]; then
    echo -e "${RED}Error: 'make re' failed.${RESET}"
    exit 1
fi
echo -e "${GREEN}'make re' success.${RESET}"
TMP_OUTPUT="tmp_output.txt"

for f in "${files[@]}"; do
    echo "--------------------------------------------------------------------------------------------"
    echo -e "${BOLD}Testing $f${RESET}"

    timeout 1.5 ./glados "tests/examples/files/${f}.scm" > "$TMP_OUTPUT"
    EXPECTED="tests/examples/output/${f}.scm"
    if [ ! -f "$EXPECTED" ]; then
        echo -e "${RED}Expected file $EXPECTED not found.${RESET}"
        results=1
        continue
    fi
    echo "--------------------------------------------------------------------------------------------"
    echo -e "${BOLD}Output:                                                         Expected: ${RESET}"
    diff "$TMP_OUTPUT" "$EXPECTED"
    if [ $? -eq 0 ]; then
        echo -e "${GREEN}$f: OK${RESET}"
    else
        echo -e "${RED}$f: KO${RESET}"
        results=1
    fi
done

rm -f "$TMP_OUTPUT"
exit ${results}
