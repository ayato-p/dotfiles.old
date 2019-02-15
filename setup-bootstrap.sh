#!/bin/bash

USER=$1

BASE_DIR=$(dirname $0)

su - -c "$BASE_DIR/setup-debian.sh $USER"
