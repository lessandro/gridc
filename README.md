# GridC

GridC gives [GridLang](https://github.com/GridControl-Team/GridControl/tree/master/gridlang) a nice, C-ish syntax.

## Dependencies

- [Haskell Platform](http://www.haskell.org/platform/)

## Instructions

1. Run `cabal install`
2. The executable should be in `~/.cabal/gridc`
3. `./gridc input.gridc > output.gridlang`

## GridC

    int factorial(n)
    {
        if (n == 0) {
            return 1;
        }

        f = factorial(n - 1);

        return n * f;
    }

The language works as you expect it to work, with a few caveats:

- Type declarations don't matter.
- Everything is a number (int or float).
- There are no void functions.
- Functions without a return statement return 0.
- Variables are created on assignment.
- The compiler does not check function calls.

Have fun writing GridC!
