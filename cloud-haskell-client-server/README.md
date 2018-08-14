# README

## Development Notes

_2018-08-14 08:45:01_

Created the project
	cabal init
Created the stack
    stack init
        command failed, with latest resolver
Resolved the problem
    stack init --resolver lts-11.20
    Used an older resolver
Adding dependencies
resolved stack solver
    stack solver
cabal configure
    Could not resolve some dependencies
cabal install {{library-name}}
    reported that some packages will be broken
problem not reported by stack
Should read about cabal sandbox
Created a cabal sandbox
    $
        cabal sandbox init
            creates a cabal.sandbox.config file
Install dependencies
    $
        cabal install --only-dependencies
Rerun configuration
    $
        cabal configure
Deleting a cabal sandbox
    $
        cabal sandbox delete
            # built-in-command
        rm -rf .cabal-sandbox cabal.sandbox.config
            # alternative
Intro to Cabal sandbox <http://coldwa.st/e/blog/2013-08-20-Cabal-sandbox.html>
