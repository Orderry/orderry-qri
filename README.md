
# Orderry SSE Server

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


## Infrastructure

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


## Prerequisites

Install Erlang runtime. *TODO: Describe Erlang setup in details.*

Install [pyenv](https://github.com/pyenv/pyenv):
packages needed to build Python by [this guide](https://askubuntu.com/a/865644)
and then use [pyenv-installer](https://github.com/pyenv/pyenv-installer#installation--update--uninstallation).

Ensure that pyenv shims come first at PATH.
Place these lines

    export PATH="~/.pyenv/bin:$PATH"
    eval "$(pyenv init -)"
    eval "$(pyenv virtualenv-init -)"

as last ones in `.bashrc` for interactive sessions **and**
also in `.bash_profile` for non-interactive sessions.

Install Python 3.6.5: `pyenv install 3.6.5`.

Install [Pipenv](https://github.com/pypa/pipenv)
into 3.6.5 distribution: `pip install pipenv`.


## Getting started

Setup runtime and build environment:

    $ make setup
    $ make build

Start server for development in foreground:

    $ make run


## Management API

```
GET     %QRI_HOST%/backdoor {type: %MESSAGE_TYPE%} =>
            200 {type: %MESSAGE_TYPE%, message: [undefined|%THE_MESSAGE%]}

POST    %QRI_HOST%/backdoor {type: %MESSAGE_TYPE%, message: %THE_MESSAGE%} =>
            204

DELETE  %QRI_HOST%/backdoor {type: %MESSAGE_TYPE%} =>
            204
```
