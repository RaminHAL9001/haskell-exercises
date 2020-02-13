# Haskell Exercise Problems by Ramin Honary
In the following exercises, we will learn how to create parsers for a
very simple arithmetic calculator language in Haskell.

In these exercises, students will learn about the basics of "parser
combinators" by creating our own simple parser combinator
library. This will give us a deep understanding of how industrial
parser combinator libraries such as [Attoparsec](
https://hackage.haskell.org/package/attoparsec ) or [Megaparsec](
https://hackage.haskell.org/package/megaparsec ) work internally.

We will begin by creating a naive parser, then iteratively upgrade our
program. After each iteration our program will begin to take the form
of a more proper and elegant Haskell parser. By "elegant" I mean we
will use more advanced concepts (e.g. Monads and Applicative Functors)
to make our program much shorter, and therefore easier to change and
improve.

My hope is that students following these exercises will be able to
understand the advanced concepts works "under the hood," because we
will (hopefully) understand how the elegant code is equivalent to the
more simplistic, naive code.

This document provides no instruction, rather students are expected to
search the Internet, or use a Haskell textbook which they already own,
for resources on how to solve these exercises. For a free online
textbook, I recommend [Learn You a Haskell for Great Good](
http://learnyouahaskell.com/ ).

We will be using functions defined in the Haskell [`Prelude`](
https://hackage.haskell.org/package/base-4.12.0.0/docs/Prelude.html )
module (which is imported by default), and in the standard
[`Data.Char`](
https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Char.html
) module which provides API functions compliant with the Haskell 2010
language standard. The API reference documentation for these modules
are here:

* https://hackage.haskell.org/package/base-4.12.0.0/docs/Prelude.html
* https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Char.html

We will use the `ghc --make` command to build our program. Please
create a new Haskell source file `Calculator.hs` and configure your
favorite text editor to execute `ghc --make` as it's build
command. After you fix all the errors, an executable program called
`Calculator` will be built, which you can run on the command line.

We will be making use of the standard character predicates module
`Data.Char`.

Open `Calculator.hs` in your editor declare it to be our Main program,
and import the `Data.Char` library

``` haskell
module Main where

import Data.Char
```

## A note about this Git repository
I will write the answers to exercises in a file called
`Calculator.hs`, which will **NOT** be on the `master` branch. For
example, to view the `Calculator.hs` program that answers the exercise
problem 3.1., check out the branch with the name `ex-3.1` using the
Git command line:

``` sh
git checkout ex-3.1
```

**NOTE** that this is a work in progress, not all the exercises may
have answers yet.

Check out the branch `all-answers` to see the final program. You can
use the `git log` command to view the history of changes to the final
`Calculator.hs` program, each commit in the history will be an answer
to an exercise problem. I hope that you can see how the naive program
is tranformed into an elegant program simply by reading each diff in
the commit history.
