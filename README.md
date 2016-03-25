## servant-elm: Generate Elm modules which talk to Haskell Servant APIs

### Notice: You probably want [Matt Bray's servant-elm](https://github.com/mattjbray/servant-elm) instead of this one

### About

If you've described a REST API with
[servant](https://haskell-servant.github.io), this package can
generate an [Elm](elm-lang.org) module for communicating with that API.

### Status

This is currently a scrappy **work in progress** created in the course
of our monthly Hack Days at work. We're using the not-yet-released
`servant-foreign` library, so we can't ship `servant-elm` yet. Check
out the
[milestones](https://github.com/purcell/servant-elm/milestones) for
more info about what works and what doesn't.

### Installation

Once published, you'll be able to install this from Hackage with `cabal` or `stack`.

### Working on this

To work on it locally, check out this repo, then `stack build` should
build the example app.

There's an example API and server defined in `example/Main.hs`, which
dumps an Elm module to a named file before starting to serve the API:

```
stack exec servant-elm-example example-elm/API.elm
```

Run `elm-reactor` and open `example-elm/Example.elm` to connect to the
API server from Elm using the generated bindings.


### Usage

TODO

### Authors

This software was written by
[Andy Newport](https://github.com/newporta),
[Steve Purcell](https://github.com/purcell) and
[Kieran Trezona-le Comte](https://github.com/trezona-lecomte) with the
support of our awesome employer
[Powershop](http://www.powershop.co.nz/), who have other cool stuff
[here on Github](https://github.com/powershop).

### License and copyright

Copyright Powershop NZ Ltd. BSD3 license.
