Scrapey — tiny webservice to get a link preview from a url
===============================================================================

How to build and run Scrapey
----------------------------

Clone this repo and start the server:

```bash
git clone https://github.com/grasswire/scrapey
cabal sandbox init
cabal install --only-dependencies
cabal run
```

### API

- **GET /link_preview?url={url}**: retrieve a page's title, a brief description of its content, and the images contained therein. 

```
$ curl "http://localhost:3000/link_preview?url=http://google.com"
{
  "images": [
    "google.com/images/icons/product/chrome-48.png",
    "google.com/images/srpr/logo9w.png"
  ],
  "url": "http://google.com",
  "canonicalUrl": "google.com",
  "title": "Google",
  "description": "Search the world's information, including webpages, images, videos and more. Google has many special features to help you find exactly what you're looking for."
}
```



Questions?
----------

[@LeviNotik](https://twitter.com/levinotik)
