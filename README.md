Scrapey — tiny webservice to get a link preview from a url
===============================================================================

Why?
----

It's sometimes useful to display the preview of a webpage, like Facebook does when you enter a URL in the text area. This webservice lets you retrieve that basic info so you can display the title, description, and images contained within a webpage.


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
$ curl "http://localhost:3000/link_preview?url=https://twitter.com/BarackObama/status/266031293945503744"
{
   "image":"https://pbs.twimg.com/media/A7EiDWcCYAAZT1D.jpg:large",
   "url":"https://twitter.com/BarackObama/status/266031293945503744",
   "canonicalUrl":"twitter.com",
   "title":"Barack Obama on Twitter",
   "description":"“Four more years.”"
}
```



Questions?
----------

[@LeviNotik](https://twitter.com/levinotik)
