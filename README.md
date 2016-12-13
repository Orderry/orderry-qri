Orderry SSE server.
===================
This is only a part of our infrastructure for easy integration of push notifications between Server-side and Client-side. Our infrastructure are based on [SSE](http://www.w3.org/TR/eventsource/#abstract) interface.

The purpose for creating this library was the need to support data synchronization in cabinets of our customers. Our infrastructure must support synchronization between Server-side frameworks, also including mobile applications, and Client-side SPA.

Read more about other Qri components:

1. [JS interface](https://github.com/Orderry/qri-js);
2. [Python lib](https://github.com/Orderry/qri-py);

Infrastructure
==============

```
 |--------|     |---------|     |---------|
 | Python | ... | Clojure | ... | Haskell |
 |--------|     |---------|     |---------|
         \           |            /
          \          |           /
        |-------------------------|
        | Erlang SSE Proxy Server | <-- Orderry Qri
        |-------------------------| <
             |               |       \
             |               |        \ http
       |----------|     |----------|   \
       | Client_1 | ... | Client_N |    |----------------|
       |----------|     |----------|    | Management API |
                                        |----------------|
```

Quick Start
==============
```bash
    $ git clone https://github.com/Orderry/orderry-qri.git
    $ cd orderry-gri
    $ mv raven.config.example raven.config
    # In raven.config set your dsn.
    $ make update
    $ sh start.sh
 ```

Management API
==============
GET     %QRI_HOST%/backdoor {type: %MESSAGE_TYPE%} => 
            200 {type: %MESSAGE_TYPE%, message: [undefined|%THE_MESSAGE%]}
            
POST    %QRI_HOST%/backdoor {type: %MESSAGE_TYPE%, message: %THE_MESSAGE%} =>
            204

DELETE  %QRI_HOST%/backdoor {type: %MESSAGE_TYPE%} =>
            204
