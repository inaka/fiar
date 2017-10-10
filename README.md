![](http://upload.wikimedia.org/wikipedia/commons/a/ad/Connect_Four.gif)

# fiar

Four in a Row - A game to learn Erlang

## Contact Us

If you find any **bugs** or have a **problem** while using this library, please [open an issue](https://github.com/inaka/fiar/issues/new) in this repo (or a pull request :)).

And you can check all of our open-source projects at [inaka.github.io](http://inaka.github.io)

## The Game

Connect Four is a two-player connection game in which the players first choose a color and then take turns dropping colored discs from the top into a seven-column, seven-row vertically suspended grid. The pieces fall straight down, occupying the next available space within the column. The object of the game is to connect four of one's own discs of the same color next to each other vertically, horizontally, or diagonally before your opponent.

http://en.wikipedia.org/wiki/Connect_Four

## Purpose

This is a project oriented to learning the erlang language, in fiar you will find topics such as:

- [Modules][modules] and functions
- [List][lists] functions
- Basic Erlang Structures
- [Pattern Matching][pattern_matching]
- [gen_server][gen_server]
- [Supervisor][supervisor] estructure
- [Application][application] Module
- Data persistence whith [sumo_db][sumo_db]
- Basic RESTful API
- Authentication
- SSE

## Stages of the project

For a procedural learning, fiar is divided into small and separate issues, grouped by [iterations][iter].

- [Iteration 1][iter1]: learn the basic estructure of an erlang application, tests, process as a gen_server, and supervisor with the `simple_one_for_one` strategy.

- [Iteracion 2][iter2]: create the fiar application module, README.md and conecting to MySQL with sumo_db.

- [Iteration 3][iter3]: Provide a RESTful API to let users play, adding a basic authentication.

- [Iteration 4][iter4]: Add SSE support and create a basic website with standard SSE support to let users play the game using already existing RESTful API.

## How to use

#### Basic usage:

```bash
fiar:start().
ok

Match = fiar:start_match().
<0.163.0>

fiar:play(Match, Col).
next
```

play is called to make a move, and it is expected to return the atom `won` when the player won, `drawn` when de board is full and none won, and `next` when it is the next player's turn.

#### Example to vertically won:

```bash
fiar:start().
ok

Match = fiar:start_match().
<0.163.0>

fiar:play(Match, 1).
next

fiar:play(Match, 2).
next

fiar:play(Match, 1).
next

fiar:play(Match, 2).
next

fiar:play(Match, 1).
next

fiar:play(Match, 2).
next

fiar:play(Match, 1).
won
```

## Run tests

Every module has its your own test file created with [Common Test][common_test] And you can run the whole suite with:

```bash
make devtests
```

## Dependencies

- [Cowboy][cowboy]
- [Jiffy][jiffy]
- [Sumo_DB][sumo_db]
- [Synk][sync]
- [Lager][lager]
- [Lasse][lasse]
- [Eperl][eperl]
- [Katana][katana]
- [Shotgun][shotgun]

## Documentation

- erlang.org  http://www.erlang.org/doc.html
- learn you some erlang  http://learnyousomeerlang.com/contents
- Api  http://erldocs.com/


  [cowboy]: https://github.com/extend/cowboy
  [jiffy]: https://github.com/davisp/jiffy
  [sumo_db]: https://github.com/inaka/sumo_db
  [sync]: https://github.com/rustyio/sync
  [lager]: https://github.com/basho/lager
  [lasse]: https://github.com/inaka/lasse
  [eperl]: https://github.com/massemanet/eper
  [katana]: https://github.com/inaka/erlang-katana
  [shotgun]: https://github.com/inaka/shotgun
  [modules]: http://learnyousomeerlang.com/modules#what-are-modules
  [lists]: http://learnyousomeerlang.com/starting-out-for-real#lists
  [pattern_matching]: http://learnyousomeerlang.com/syntax-in-functions#pattern-matching
  [gen_server]: http://www.erlang.org/doc/man/gen_server.html
  [supervisor]: http://www.erlang.org/doc/man/supervisor.html
  [application]: http://www.erlang.org/doc/apps/kernel/application.html
  [iter]: https://github.com/inaka/fiar/milestones
  [iter1]: https://github.com/inaka/fiar/issues?q=milestone%3A%22Iteration+%231%22
  [iter2]: https://github.com/inaka/fiar/issues?q=milestone%3A%22Iteration+%232%22+is%3Aclosed
  [iter3]: https://github.com/inaka/fiar/issues?q=milestone%3A%22Iteration+%233%22+is%3Aclosed
  [iter4]: https://github.com/inaka/fiar/issues?q=milestone%3A%22Iteration+%234%22+is%3Aclosed
  [sumo_db]: https://github.com/inaka/sumo_db
  [common_test]: http://www.erlang.org/doc/apps/common_test/basics_chapter.html
