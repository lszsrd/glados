#!/bin/bash

ARCHIVE_DIR="glados-extract"

getOs() {
    case "$(uname -s)" in
        "Linux")
            echo "linux";;
        "Darwin")
            echo "macos";;
        *)
            printf "OS %s not supported, please build from sources\n" "$OS" >&2
            exit 1
    esac
}

installBrew() {
    if ! command -v brew
    then
        read -r -p "brew not found, should I install it? [y/N] " response
        case "$response" in
            [yY]) 
                /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)";;
            *)
                exit 1
        esac
    fi
}

checkRequired() {
    if ! command -v $1
    then
        read -r -p "Binary $1 not found, should I install it? [y/N] " response
        case "$response" in
            [yY]) 
                if [[ getOs == "Linux" ]]
                then
                    apt install wget -y
                else
                    installBrew
                    brew install wget
                fi;;
            *)
                exit 1
        esac
    fi
}

main() {
    if [[ $EUID -ne 0 ]]; then
        echo "This script must be run as root" 
        exit 1
    fi
    local os=$(getOs)
    printf "Installing glados to /usr/local/bin, required binaries are: wget, unzip"
    checkRequired "wget"
    checkRequired "unzip"
    mkdir ${ARCHIVE_DIR}
    cd ${ARCHIVE_DIR}
    wget https://github.com/lszsrd/glados/releases/latest/download/glados.zip
    if [[ $? -ne 0 ]]
    then
        exit 1
    fi
    unzip glados.zip
    if [[ $? -ne 0 ]]
    then
        exit 1
    fi
    cp "glados-compiler-${os}" /usr/local/bin/glados-compiler
    cp "glados-vm-${os}" /usr/local/bin/glados-vm
    export PATH="/usr/local/bin:$PATH"
    cd ..
    rm -r ${ARCHIVE_DIR}
}

main
