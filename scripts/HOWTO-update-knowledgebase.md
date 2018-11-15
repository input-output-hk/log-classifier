How to Update the Knowledgebase
---
```
$ curl -G -H "Accept: application/json" -H "Authorization: Bearer `cat tmp-secrets/yt-token`" https://iohk.myjetbrains.com/youtrack/rest/issue/byproject/IOHKS --data-urlencode "filter=Zendesk-Debugger: Yes State: -Done" --data-urlencode "max=100" -o issues.json
```

Running this will create a JSON file containing YouTrack Issue


Next, you must transform the JSON into a CSV, which can be done with JQ:

```
$ jq -r '.[] | { (.id): {searchString: (.field[] | select(.name == "Zendesk Debugger Identifier Text").value[]) }} | to_entries | map(.value[]) + map(.key) | @csv' issues.json
```
