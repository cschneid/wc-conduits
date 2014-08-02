Initial project setup was done with `hi`, and the rspec template.

`hi -m ConduitWordCount -p conduitwordcount -a "Chris Schneider" --email "chris@christopher-schneider.com" -r git@github.com:fujimura/hi-hspec.git`

Then `git init && git add . && git commit -m "Initial Commit"`

Then `cabal sandbox init`

I added the executable block to the cabal file, added Main.hs with a simple
putStrLn and made sure the executable ran.

Fired up cabal repl, to allow me to play with the types.

SCREENSHOT 1

After each new dependency is added to cabal:

1. `cabal install --only-dependencies`
2. restart the cabal repl
