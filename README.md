
# Orderry SSE server

This is only a part of our infrastructure for easy integration of push notifications
between Server-side and Client-side.
Our infrastructure are based on [SSE](http://www.w3.org/TR/eventsource/#abstract) interface.

The purpose for creating this library was the need to support data synchronization
in cabinets of our customers.
Our infrastructure must support synchronization between Server-side frameworks,
also including mobile applications, and Client-side SPA.

Read more about other QRI components:

1. [JS interface](https://github.com/Orderry/qri-js);
2. [Python lib](https://github.com/Orderry/qri-py);


# Infrastructure

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

# Prerequisites

Install Erlang runtime.
Install Python 3.6.5 under pyenv, install pipenv.


# Quick Start

Edit appropriate .config if needed.

```bash
$ make setup
$ make build
$ make run
```


# Management API

GET     %QRI_HOST%/backdoor {type: %MESSAGE_TYPE%} =>
            200 {type: %MESSAGE_TYPE%, message: [undefined|%THE_MESSAGE%]}

POST    %QRI_HOST%/backdoor {type: %MESSAGE_TYPE%, message: %THE_MESSAGE%} =>
            204

DELETE  %QRI_HOST%/backdoor {type: %MESSAGE_TYPE%} =>
            204
