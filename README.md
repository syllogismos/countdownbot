Countdown Twitter Bot:
=====================

This is a working twitter bot that will give you the solution to the numbersgame from
countdown, just mention the bot according to the format given below

@bot numbersgame {target_number} {n1} {n2} {n3} {n4} {n5} {n6}

Installation:
------------

Make a copy of config.template.json and rename it config.json 
and fill your secret twitter keys and nick in it
```sh
> npm install
> coffee countdownbot.coffee
```

You can compile NumbersGame.hs using ghc and get NumbersGame.exe,
or you can use the provided exe..

Using NumbersGame.exe
```sh
> ghc --make NumbersGame.hs

> NumbersGame 983 10 8 4 100 7 5
> One possible solution for 983 is: (10*(4+(100-5)))-7
```
