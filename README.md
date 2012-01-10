# BrowserID for CouchDB

This is a plugin for CouchDB to support Mozilla's BrowserID standard.

## So Simple

First, for security reasons, BrowserID is disabled by default. Go to Futon's *Configuration* page. Look for the *browserid* section, and change the *enabled* option to `true`.

Good. Put this in your Couch app.

    <!DOCTYPE html>
    <html>
      <head>
        <script src="/_browserid/include.js" type="text/javascript"></script>

        <!-- Optional styles -->
        <link rel="stylesheet" type="text/css" href="/_browserid/style.css">
      </head>

      <body>
        <div id="browserid">
          <div class="login">
            <img src="/_browserid/sign_in_blue.png">
          </div>
          <div class="picture"></div>
        </div>

        Hello, BrowserID world!
      </body>

      <!-- Recommended convenience API -->
      <script src="/_utils/script/jquery.js" type="text/javascript"></script>
      <script src="/_browserid/main.js" type="text/javascript"></script>
    </html>

You're done!

That is a complete, working CouchDB login page.  All you need is a `#browserid` div a `.login` div inside.

CouchDB supports automatic new account creation and a traditional session cookie. User accounts haven't changed. They are still documents in the `_users` database. You can set roles and replicate them as before. The only changes are

* Users log in through BrowserID instead of passwords
* The first time a user logs in, CouchDB will create an account for them.

### obscuring email addresses (optional, but recommended)

For privacy, set `browserid/hash_secret` to a random string. (Use the *add a new section* link at the bottom of futon's *configuration* page.) This will give BrowserID users names that don't look like their email address, protecting them from harvesting, *at a cost* of generating different document IDs for the same user on different Couches.

  * The *hash_secret* string should be long and cryptographically random
    (for example, you can use one of the random strings that [https://api.wordpress.org/secret-key/1.1/](https://api.wordpress.org/secret-key/1.1/) generates).

  * Once you decide whether to use a *hash_secret* or not, and what value to use for it, you shouldn't change this setting,
    or existing BrowserID users will never be able to login as "the same person" again.

### Logging in and out programatically

If you use the convenience API, `/_browserid/main.js`, you can start the login or logout process from Javascript:

    // Start the login process, which pops up the BrowserID window.
    $.couch.browserid.login();

    // Start the logout process, which ends the user's session.
    $.couch.browserid.logout();

Upon login, the `.login` div will contain a welcome message for the user. Use jQuery and CSS to modify or style it as needed. It will look like this:

    <div class="login">
      <span class="greeting">Hi </span>
      <span class="username">me@example.com</span>
      <span class="farewell">.</span>
      <a class="logout" href="/">(logout)</a>
    </div>

Additionally, the `.picture` div will contain a Gravatar image for the user's email address.

### Login and logout events

To be notified when the login or logout phase completes, use the same functions with a callback.

    // This will run when login is done.
    $.couch.browserid.login(function(event, error, user) {
      if(error)
        return console.log("Something went wrong with login: " + error);

      console.log("Congratulations " + user.email + ", you now have an account on my couch");
    });

    // This will run when logout is done.
    $.couch.browserid.logout(function(event, error) {
      if(error)
        return console.log("Something went wrong with logout: " + error);

      console.log("Sorry to see you go!");
    })

### Advanced usage

If you are a BrowserID whiz and prefer to do your own thing, include only the `/_browserid/include.js` file and no others. (You can also use `https://browserid.org/include.js` if you don't trust us.) To verify your membership assertion, POST an `application/json` body to `/_browserid`, such as this:

    { "assertion": "<the assertion string goes here>"
    , "audience" : "example.com:80"
    }

The response will be the same as `https://browserid.org/verify`. That URL is currently used for verification. You can use a different one by changing `/_config/httpd/browserid_verify_url`.

## Building

If you hate Freedom, just sign up for an [Iris Couch][ic] account, since they run hosted CouchDB with this plugin.

To build for yourself, use Build CouchDB: https://github.com/iriscouch/build-couchdb

    rake plugin="git://github.com/iriscouch/browserid_couchdb origin/master"

[ic]: http://www.iriscouch.com/service

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

## The Test Suite Guarantee

We at Iris Couch prefer on unit tests and test-driven development. A test suite is just the grown-up thing to do. Unfortunately, this project lacks such tests.

If you are interested in CouchDB development and wish to join the community, we need Javascript unit tests! Ultimately, we hope for a suite to integrate into Futon, however at this time, anything will do! We can port it later.

NodeJS, jQuery, Rhino, Futon--whatever you want. Send us a pull request and we can get this work underway!
