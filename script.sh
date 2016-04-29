#!/bin/bash

if [ -z "$1" ] || [ -z "$2" ]
then
    echo "usage $0 initialLength finalLength"
    exit 1
fi
mkdir -p out/
javac Java/src/*
mv Java/src/*.class out/
cd out
java Main $1 $2
