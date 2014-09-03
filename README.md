![](http://s2.playpickle.com/assets/four-in-a-row_704x451.jpg)

# fiar

For in a Row - A game to learn Erlang

## The Game

Connect Four is a two-player connection game in which the players first choose a color and then take turns dropping colored discs from the top into a seven-column, seven-row vertically suspended grid. The pieces fall straight down, occupying the next available space within the column. The object of the game is to connect four of one's own discs of the same color next to each other vertically, horizontally, or diagonally before your opponent.

http://en.wikipedia.org/wiki/Connect_Four

## Purpose

This is a project oriented to learning the erlang language, in fiar will find topics such as:

- Modules and functions
- List functions
- Basic Erlang Structures
- Pattern Matching
- gen_server
- Supervisor estructure
- Application Module
- Data persistence whith [sumo_db][sumo_db]
- Basic RESTful API
- Authentication
- SSE

## Stages of the project

For a procedural learning, fiar is divided into small and separate issues, grouped by iterations.

- Iteration 1: implement the initial setup, known the basic estructure of an erlang application, fiar_core with the logic of the game, test it, fiar_match as a process (gen_server) and supervisor with the `simple_one_for_one` strategy.

- Iteracion 2: create the fiar application module, README.md and conecting to MySQL with sumo_db.

- Iteration 3: Provide a RESTful API to let users play, adding a basic authentication.

- Iteration 4: Add SSE support and create a basic website with standard SSE support to let users play the game using already existing RESTful API.

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

play is called to make a move, and it is expected to return the `won` when the player won, `drawn` when de board is full and no wins, and `next` when is the next player's turn.

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

Every module has your own test file created with [Common Test][common_test] and you can run with:

```bash
make devtests
```



  [sumo_db]: https://github.com/inaka/sumo_db
  [common_test]: http://www.erlang.org/doc/apps/common_test/basics_chapter.html