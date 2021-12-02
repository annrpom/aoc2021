# Advent of Code 2021

### Motivation
It is my goal to learn more [Racket](https://racket-lang.org/). *Maybe* I will be able to complete a Haskell version at some point in my life...

### Credits
Credits to [Hazel](https://git.bicompact.space/hazel/aoc-2021) for providing a neat setup and answering my questions about such setup. They have credited [haskal](https://git.lain.faith/haskal/aoc2020/src/branch/aoc2020/scripts) as well.

### Setup and Workflow

First and foremost, you will need everything in `bin` and `lib`, along with an empty `data` directory. Use the provided template, `templ.rkt` as you see fit (if you do, make sure you replace the `N`s with the numerical value of whatever day you are solving).
Next, in your shell config, you will want to add the following lines:
```zsh
export AOC_YEAR="2021"
export AOC_SESSION="YOUR_SESSION_TOKEN_HERE"
export PATH="${PATH}:PATH_TO_AOC_2021_CODE/bin"
```
Replace `YOUR_SESSION_TOKEN_HERE` with your session cookie, which you can get by going to advent of code's website, logging in, and opening your browser's developer console. [Here](https://github.com/wimglenn/advent-of-code-wim/issues/1) is a resource for more thorough instructions, if needed.
Replace `PATH_TO_AOC_2021_CODE` with the file path where all of this good stuff is located.

For the following instructions, I am assuming you have the same directory structure as me; that is:
```
$ tree
.
├── bin
│   ├── README.md
│   ├── get-challenge
│   └── get-input
├── data
├── lib
│   ├── aoc.rkt
│   ├── common.rkt
│   └── compiled
│       └── drracket
│           └── errortrace
│               ├── aoc_rkt.dep
│               ├── aoc_rkt.zo
│               ├── common_rkt.dep
│               └── common_rkt.zo
└── tmpl.rkt
```
To start working, you will want the input for whatever day you are attempting to solve. For that, make sure you are in the same directory where `data` is located and run:
```
$ ./bin/get-input N
```
where `N` represents the numerical value of the day.

Then, get to solving! In order to turn in your submission:
```
$ racket dayN.rkt
```
will be your friend.


