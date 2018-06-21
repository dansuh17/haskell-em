# haskell-em

A simple experiment of implementing EM algorithm using Haskell and its `State` monad + hmatrix.

Read the [blog post](http://densuh.github.io/jekyll/update/2018/06/20/haskell-em.html) about this project for more information.

The final project code is `EmCoinState.hs and Main.hs`. All other files are intermediary experiments that led to these two.

## Running example code

requires stack

```
stack build && stack exec haskell-em-exe
```
