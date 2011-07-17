# BrowserID for CouchDB

This is a plugin for CouchDB to support Mozilla's BrowserID standard.

## So Simple

BrowserID is easier than traditional CouchDB accounts.

Of course, when a user logs in, they automatically receive a standard Couch account in the `_users` database.

## Building

Use Build CouchDB: https://github.com/iriscouch/build-couchdb

    rake plugin="git://github.com/iriscouch/browserid_couchdb origin/master"

## Installation

Build CouchDB already took care of this step!

## Development

This is what I do. It's not perfect but barely harder than building a fork of CouchDB, and it allows 1.0.x, 1.1.x, as well as trunk support.

This example assumes three Git checkouts, side-by-side:

* `couchdb` - Perhaps a trunk checkout, but could be any tag or branch
* `build-couchdb` - For the boring Couch dependencies
* `browserid_couchdb` - This code

### Build dependencies plus couch

Feel free to skip this if you are Randall Leeds. (Hi, Randall!)

    cd couchdb
    rake -f ../build-couchdb/Rakefile couchdb:configure
    # Go get coffee. This builds the deps, then runs the boostrap and configure scripts
    make dev

Next, teach rebar where to find CouchDB, and teach Erlang and Couch where to find this plugin.

    cd ../browserid_couchdb
    export ERL_COMPILER_OPTIONS='[{i, "../couchdb/src/couchdb"}]'
    export ERL_ZFLAGS="-pz $PWD/ebin"
    ln -s "../../../../browserid_couchdb/etc/couchdb/default.d/browserid.ini" ../couchdb/etc/couchdb/default.d

You're ready. Run this every time you change the code:

    ./rebar compile && ../couchdb/utils/run -i
