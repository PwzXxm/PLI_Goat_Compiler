#!/bin/sh
mkdir testGoat
cp -r testdata testGoat/
cp test.py testGoat/
tar zcf testGoat.tar.gz testGoat
rm -rf testGoat
