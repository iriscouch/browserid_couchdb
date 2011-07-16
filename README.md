# BrowserID for CouchDB

This is a plugin for CouchDB to support Mozilla's BrowserID standard.

## Building

Use Build CouchDB: https://github.com/iriscouch/build-couchdb

    rake plugin="git://github.com/iriscouch/browserid_couchdb origin/master"

## Installation

By using Build CouchDB, this is already done!

## Development

This is what I do. It's not perfect but in my view barely harder than building a fork of CouchDB, and it allows 1.0.x, 1.1.x, as well as trunk support.

First, teach rebar where to find CouchDB.

    export ERL_COMPILER_OPTIONS='[{i, "../couchdb/src/couchdb"}]'

Then run this every time you change the code:

    ./rebar compile && cp ebin/*.beam ../couchdb/src/couchdb && cp etc/couchdb/default.d/*.ini ../couchdb/etc/couchdb/default.d/ && ( cd ../couchdb && ./utils/run -i )
