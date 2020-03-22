#!/bin/bash


git clone -b 0.12.0 https://github.com/libcheck/check.git /opt/check
cd /opt/check
autoreconf --install
./configure
make
make check
make install
ldconfig


apt install make autoconf automake pkg-config flex bison
apt install libpango1.0-dev libpangocairo-1.0-0 libcairo2-dev libglibmm-2.4-dev
apt install libxkbcommon-dev libxcb1-dev libxcb-util0-dev
apt install libxcb-xkb-dev libxkbcommon-x11-dev libxcb-ewmh-dev libxcb-icccm4-dev libxcb-xrm-dev libxcb-randr0-dev libxcb-xinerama0-dev
apt install libstartup-notification0-dev


git clone https://github.com/DaveDavenport/rofi /opt/rofi
cd /opt/rofi
git submodule update --init
autoreconf -i
mkdir build
../configure
make
make install
