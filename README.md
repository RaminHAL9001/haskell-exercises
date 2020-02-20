# Elegant Haskell
## Understanding monadic parsers from the ground-up
In the following exercises, we will learn how to create parsers for a
very simple arithmetic calculator language in Haskell.

Read `./Calculator.md` to begin this tutorial.

In this tutorial, students will learn about the basics of "parser
combinators" by creating our own simple parser combinator
library. This will give us a deep understanding of how industrial
parser combinator libraries such as [Attoparsec](
https://hackage.haskell.org/package/attoparsec ) or [Megaparsec](
https://hackage.haskell.org/package/megaparsec ) work internally.

We will begin by creating a naive parser, then in each exercise we
will refine our program a little bit, making our program gradually
take the form of a more proper and elegant Haskell parser. By
"elegant" I mean we will use more advanced concepts (e.g. Monads and
Applicative Functors) to make our program much shorter, and therefore
easier to change and improve.

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

- Chapter 1, 2
    - ["base" Prelude]( https://hackage.haskell.org/package/base/docs/Prelude.html )

- Chapter 3
    - ["base" Data.Char]( https://hackage.haskell.org/package/base/docs/Data-Char.html)

- Chapter 4
    - ["mtl" Control.Monad.Except]( https://hackage.haskell.org/package/mtl/docs/Control-Monad-Except.html )

- Chapter 5
	- ["mtl" Control.Monad.State]( https://hackage.haskell.org/package/mtl/docs/Control-Monad-State-Lazy.html )

- Chapter 6
    - ["megaparsec" Text.Megaparsec]( https://hackage.haskell.org/package/megaparsec/docs/Text-Megaparsec.html )

We will use the `cabal build` command to build our program. Before you
can run `cabal build` you must run the `cabal v2-configure` command in
the command line (make sure you have run `cd` to changed to the
directory containing this `README.md` file), which will check that you
have the dependent code packages installed, and set up a build
directory to contain the output of the compiler.

Open `./Calculator/Calculator.hs` in your favorite editor IDE, and set
the build command to `cabal build Calculator`.

# A note about this Git repository
I will write the answers to exercises in a file called
`./Calculator/Calculator.hs`, which is empty on the `master` branch.

The answers to all exercises, that is to say, the completed
`Calculator.hs` file, can be checked out from the Git `all-answers`
branch. The commit history of `all-answers` has exactly one commit
showing the changes that were made to solve the problems given in the
exercise. The comment of each commit matches the text of the exercise
section header.

### WARNING: this is a work in progress!
Not all the exercises have answers yet.

## Solutions to practice problems
There is also a commit for each in the history of `all-answers`
containing the solutions to each practice problem.

``` sh
git checkout 'ex-3.1';
```

## Please create a `homework` branch before writing any code
It is strongly recommended you create a new Git branch (maybe call it
the `homework` branch) before writing your code into `Calculator.hs`,
and commit your changes to this branch:

``` sh
git checkout -b 'homework;
```

If you do not, Git will usually fail if you try to check-out the
`all-answers` branch or any of the solution exercise branches.

## Getting a list of solutions to practice problems
Use the `git log` command to list all commits in the range of the
`master` branch to the `all-answers` branch. It is recommended you use
the `--reverse` and `--oneline` option:

``` sh
git log --reverse --oneline 'master'..'all-answers';
```

## Viewing the solution to a practice problem in the command line
Once you have listed all solutions (as explained above), you can see
what changes were made for that particular solution on the command
line using the `git diff` command, and git's syntax for specifying a
revision by the text of it's commit message (this is the curly-bracket
notation). The following example commands will view the changes made
in section 2.1, followed by the changes made in section 3.7.

``` sh
git diff 'all-answers^{/^2\.1\.}^-1';
git diff 'all-answers^{/^3\.7\.}^-1';
```
