Purescript 2048
===============

This code is a Purescript port from h2048 implementation by Gregor Ulm in haskell

** Disclaimer ** I'm not familiar with Haskell nor Purescript, this was only done only for fun. Any feedback is welcome

How to run locally
------------------

1. `yarn install` (or `npm install` if you prefer)
2. npm start (this will build code and run a local server)
3. open your navigator at `localhost:8080`

Differences
-----------

Purescript and Haskell are different in many ways (for starters you could look [here](https://github.com/purescript/purescript/wiki/Differences-from-Haskell)) I had to force code to work in a different way to make this port possible, some of this changes are:

- Terminal output goes to a PRE tag
- IO and Eff are quite different so some changes in code were required to support Eff
- I'm not sure how Purescript can handle blocking functions, like the one used for reading user input, so I had to go into callback-hell to get this done

TODO
----

- [ ] Learn more about Aff and use it instead of callbacks
- [ ] Learn more about Haskell and Purescript and clean code
