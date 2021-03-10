# mocky


This is a little http mock tool for mocking JSON API's.

originally designed for mocking gateway responses, it is configured by a POST request to the `/set` endpoint with a JSON mapping from String to MockResponse where MockResponse is 

```
data MockResponse = MockResponse
  { responseCode :: Int
  , responseHeader :: Maybe [(Text,Text)]
  , responseBody :: Text
  }
```


## Example: 

1.   the Setup Request:
```
curl --header "Content-Type: application/json" \   
  --request POST \
  --data '{"username/thing": { "responseCode": 200, "responseHeader": null, "responseBody": "hullow"  }}' \
  http://localhost:3000/set
(edited)
```

note the leading `/` is removed on the endpoint `username/thing`.

2: `curl http://localhost:3000/username/thing`


you can now also curl to `/get` to see the current state

