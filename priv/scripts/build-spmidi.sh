#!/bin/sh

ROOT_DIR=$(pwd)
SP_VERS_NUM=4.5.1
SP_VERS=v${SP_VERS_NUM}
SP_URL=https://github.com/sonic-pi-net/sonic-pi/archive/refs/tags/${SP_VERS}.zip
SP_DL_DIR=$HOME/.cache/sonic-pi
SP_UNZIP_DIR=${SP_DL_DIR}/sonic-pi-${SP_VERS_NUM}
SP_DIR=sp_midi
SP_BUILD_DIR=${SP_DIR}/build
SP_METADATA_FILE=priv/sp_metadata.conf

download() {
    echo "MIDISERVER: Downloading Sonic Pi MIDI NIF source ..."
    mkdir -p $SP_DL_DIR
    cd $SP_DL_DIR

    if [ -f ${SP_VERS}.zip ]; then
        echo "** Sonic Pi archive already present; skipping download."
    else
        curl -L -O $SP_URL
    fi

    if [ -d $SP_UNZIP_DIR ]; then
        echo "** Sonic Pi directory already exists; skipping unzip."
    else
        unzip -q ${SP_VERS}.zip
    fi
    
    cd $ROOT_DIR
}

pre_build() {
    echo "MIDISERVER: Setting up MIDI NIF build dir ..."

    if [ -d $SP_DIR ]; then
        echo "** Project copy of Sonic Pi directory already exists."
    else
        cp -r ${SP_UNZIP_DIR}/app/external/sp_midi .
    fi
    
    mkdir -p $SP_BUILD_DIR
}

build() {
    echo "MIDISERVER: Building MIDI NIF ..."
    if [ -e src/libsp_midi.so ]; then
        echo "** Sonic Pi Erlang NIF library exists; skipping build."
    else
        cd $SP_BUILD_DIR && \
            cmake .. && \
            make
        cd $ROOT_DIR
    fi
}

install() {
    echo "MIDISERVER: Installing MIDI NIF ..."
    if [ -e src/libsp_midi.so ]; then
        echo "** Sonic Pi Erlang NIF library exists; skipping install."
    else
        rm -f src/libsp_midi.*
        cp ${SP_BUILD_DIR}/libsp_midi.* src
    fi
}

post_build() {
    echo "MIDISERVER: Cleaning up MIDI NIF temporary and build directories ..."
    rm -rf $SP_DIR
    if [ $(uname -s) == "Darwin" ]; then
        if [ -e src/libsp_midi.so ]; then
            echo "** Sonic Pi Erlang NIF library Darwin symlink exists; skipping."
        else
            cd src && ln -s libsp_midi.dylib libsp_midi.so
            cd $ROOT_DIR
        fi
    fi
    echo "{sonic_pi, [{version, \"$SP_VERS_NUM\"}]}." > $SP_METADATA_FILE
}

download
pre_build
build
install
post_build

