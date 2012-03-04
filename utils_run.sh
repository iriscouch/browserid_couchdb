#!/bin/bash

couchdb="../couchdb"
if [ ! -d "$couchdb" ]; then
  echo "I was expecting a checkout in $couchdb"
  exit 1
fi


if [ ! -L "$couchdb/etc/couchdb/default.d/browserid.ini" ]; then
  ln -sf "../../../../browserid_couchdb/etc/couchdb/default.d/browserid.ini" "$couchdb/etc/couchdb/default.d/browserid.ini"
fi

export ERL_COMPILER_OPTIONS='[{i, "../couchdb/src/couchdb"}]'
export ERL_ZFLAGS="-pz $PWD/ebin"

set -e
set -x

./rebar compile
cd "$couchdb"
make dev
./utils/run
