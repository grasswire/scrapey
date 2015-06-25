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
./dist/build/scrapey/scrapey 
```

### API

Some API endpoints are:

- **GET /pagetitle?url={url}**: Title of a web page's `<title>` tag. Response:`{"url":"https://www.haskell.org/","title":"Haskell Language"}`.
- **GET /tweet?url={url}**: Retrieves a Twitter Tweet object (see [Twitter Tweets](https://dev.twitter.com/overview/api/tweets)) by extracting the Tweet id from a twitter link (e.g. `https://twitter.com/LeviNotik/status/582581545157959680`). Responds with a 404 if the supplied url is not a link to a Tweet.
- **GET /images?url={url}**: Value of src attribute of all `<img>` tags. Response: JSON string array.


Questions?
----------

[@LeviNotik](https://twitter.com/levinotik)
