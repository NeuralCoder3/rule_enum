#!/bin/sh
# apt-snapshot.sh DATE PACKAGE...: install Debian packages as of the snapshot.debian.org DATE.
set -eu
date=$1; shift
sed -i -E "s#https?://deb.debian.org/(debian(-security)?)#http://snapshot.debian.org/archive/\1/$date#" \
  /etc/apt/sources.list.d/debian.sources
apt-get -o Acquire::Check-Valid-Until=false update
apt-get install -y --no-install-recommends "$@"
rm -rf /var/lib/apt/lists/*
