Scrapey — standalone API server to fetch/scrape web content on demand
===============================================================================

How to build and run Scrapey
----------------------------

Clone a copy of the Scrapey git repo by running:

```bash
git clone https://github.com/grasswire/scrapey
cabal sandbox init
cabal install --only-dependencies
cabal build
./dist/build/scrapey/scrapey "path/to/my/scrapey.cfg"
```

### Configuration

Scrapey requires a config file in order to run. Ppass the fully qualified path of a .cfg file as a command line argument. 

If you don't plan on using the Twitter API, you can leave this config empty. Otherwise, the config should contain the following:

```
twitter_consumer_key = "my consumer key"
twitter_consumer_secret  = "my consumer secret"
twitter_access_secret = "my access secret"
twitter_access_token = "my access token"
```

Without these credentials, trying to use the Twitter API will result in a HTTP 500, which is fine if you don't want to use the Twitter functionality.


### API

Some API endpoints are:

- **GET /pagetitle?url={url}**: Title of a web page's `<title>` tag. Response:`{"url":"https://www.haskell.org/","title":"Haskell Language"}`.
- **GET /tweet?url={url}**: Retrieves a Twitter Tweet object (see [Twitter Tweets](https://dev.twitter.com/overview/api/tweets)) by extracting the Tweet id from a twitter link (e.g. `https://twitter.com/LeviNotik/status/582581545157959680`). Responds with a 404 if the supplied url is not a link to a Tweet.
- **GET /images?url={url}**: Value of src attribute of all `<img>` tags. Response: JSON string array.


Questions?
----------

[@LeviNotik](https://twitter.com/levinotik)