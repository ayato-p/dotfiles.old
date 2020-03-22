#!/bin/bash

BRANCH_NAME='emacs-25.3'
BIN_DIR='/usr/local/bin'

su - -c "apt install autoconf automake libtool texinfo build-essential xorg-dev libgtk2.0-dev libjpeg-dev libncurses5-dev libdbus-1-dev libgif-dev libtiff-dev libm17n-dev libpng-dev librsvg2-dev libotf-dev libgnutls28-dev libxml2-dev && \
git clone --depth 1 -b ${BRANCH_NAME} git://git.sv.gnu.org/emacs.git /opt/${BRANCH_NAME} && \
cd /opt/${BRANCH_NAME} && \
./autogen.sh && \
./configure --prefix=${BIN_DIR}/${BRANCH_NAME}.d && \
make bootstrap && \
make install && \
ln -s ${BIN_DIR}/${BRANCH_NAME}.d/bin/emacs ${BIN_DIR}/${BRANCH_NAME}
"
