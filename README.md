Scrapey 
=======

Standalone server/API for scraping content of various types from urls
===========================

Running
-------

cabal build this project and then run the program, providing the full path to your scrapey.cfg confile file. 

`./scrapy path/to/scrapey.cfg`

If you don't plan on using the Twitter API, you can leave this config empty. Otherwise, the config should contain the following:

```
twitter_consumer_key = "my consuker key"
twitter_consumer_secret  = "my consumer secret"
twitter_access_secret = "my access secret"
twitter_access_token = "my access token"
```


Page titles
-----------


###GET /pagetitle?url={url}

Response: the content of the `<title>` tag for the specified url

```
curl "localhost:3000/pagetitle?url=https://www.haskell.org/"

{"url":"https://www.haskell.org/","title":"Haskell Language"}

```

Tweets
------

###GET /tweet?url={url}

Response: the Tweet json for the tweet from a url in the form of **twitter.com/{screen_name}/status/{tweetId}**

```
curl "localhost:3000/tweet?url=https://twitter.com/LeviNotik/status/582581545157959680"

{"in_reply_to_status_id":null,"truncated":false,"possibly_sensitive":false,"retweeted_status":null,"withheld_scope":null,"in_reply_to_screen_name":null,"extended_entities":null,"entities":{"urls":[{"expanded_url":"https://github.com/grasswire/scrapey","url":"https://t.co/ZN5TnhviyC","indices":[94,117],"display_url":"github.com/grasswire/scra…"}],"media":....

```