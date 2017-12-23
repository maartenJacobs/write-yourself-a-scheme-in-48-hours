# Write yourself a Scheme in 48 hours

Repo for my version of [Write yourself a Scheme in 48 hours](https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours).

## Improvements

- Tests with HSpec. Always good to have tests, even for a little exercise.
- Cabal sandbox. Useful for running tests and managing dependencies, such as Parsec.

## TODO

- Switch to Stack from (explicit) Cabal sandbox. Sandboxing GHC could solve the
problem of not being able to install `hlint`. It's also the preferable build tool
in the community at this point.

- Create tests using QuickCheck. Need to do a bit more reading on properties.

- Use prisms of the lenses package to simplify type-checking functions.
