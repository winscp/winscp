#! /bin/bash
# Copyright (C) 2018 Sebastian Pipping <sebastian@pipping.org>
# Licensed under the MIT license
#
# Creates release tarball and detached GPG signature file for upload

set -e

PS4='# '
set -x

version="$(./conftools/get-version.sh lib/expat.h)"
archive=expat-${version}.tar.bz2

./buildconf.sh
./configure
make distcheck

gpg --armor --output ${archive}.asc --detach-sign ${archive}
gpg --verify ${archive}.asc ${archive}
