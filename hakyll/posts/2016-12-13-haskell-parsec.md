---
layout: post
author: Nicole Rauch
# meta: Meta Data Goes Here
title: Parsing Strings with Haskell's Parsec Library
# subtitle: 
lang: en
---

When I tried to walk my first steps with Haskell's Parsec library, I was unable to find a detailed tutorial aimed at non-experts in functional programming.
So, after I got my code working, I decided to write that tutorial myself.

## What am I Planning to Parse, Anyway?

I was writing a bot for a coding contest where the goal is to write a computer player for the drinking game ["Mia" or "Mäxchen"](https://github.com/NicoleRauch/maexchen).
This bot connects to a server (the game master) via UDP which sends simple string messages to the clients to inform them about the current state of the game.
These messages can be something like "A new round is about to start", "This is the current score" or "A player lost".

My first attempt, hacked together during an actual contest, was to compare those strings to expected constants all over the place, but
soon I found this to be extremely unelegant, especially since I was coding in Haskell, and of course I wanted my code to be more typesafe.

So I decided to learn Parsec and to use it for this task. I was well aware that applying Parsec to the problem might be a tad of an overkill,
but I wanted to learn about it anyway, and I always prefer to have a real-world use case when playing around with something new.

## Defining the Input and the Output

I decided to start with a very simple bot, one that would always [want to "see"](https://en.wikipedia.org/wiki/Mia_%28game%29)
whenever it was its turn. This reduced the number of relevant commands from the server to the following:

```
ROUND STARTING;some-token
YOUR TURN;some-token
```

All other server commands would be ignored for the moment.

We also need a data structure that will be the output of our parser:

```
data Command =
  RoundStarting String
  | YourTurn String
  | Unknown String
  deriving (Eq, Show)
```

Here, the two known commands will be identified by the matching data type constructors `RoundStarting` and `YourTurn`, where the string argument
will hold the token that was submitted as part of the command. In order to allow for a total parser function, I also added a third data type
constructor called `Unknown` whose string argument contains the full command that was submitted from the server.

## Defining the Language

First of all, we need to describe what elements our language contains. This description is done via
[the `LanguageDef` type](https://hackage.haskell.org/package/parsec-3.1.11/docs/Text-ParserCombinators-Parsec-Token.html).
When looking at this type, we can see that it is very much specialized to parsing programming languages; we can specify
different kinds of comments, identifiers, operators and reserved words. Clearly, our minimalistic Mia commands are not a programming language,
so we need not specify any of these. Luckily, there is a empty language definition available which perfectly serves our purpose.

The `Text.ParserCombinators.Parsec.Token` module contains a function called `makeTokenParser` which generates a scanner (also called lexer)
for us - in principle, a scanner is used to break the stream of characters into different tokens, to remove whitespace and comments.
As we do not define any language features, our scanner does not really do much work for us. But we need one anyways, so here goes:

```
lexer = T.makeTokenParser emptyDef
```

## Parsing Individual Command Strings

[The Parsec.Token module](https://hackage.haskell.org/package/parsec-3.1.11/docs/Text-ParserCombinators-Parsec-Token.html) provides us with numerous
small and helpful parsers which we can use to construct the parsers we actually need. First let's define some shortcuts for utility functions:

```
import qualified Text.ParserCombinators.Parsec.Token as T

semiP     = T.semi lexer          -- a semicolon
symbolP   = T.symbol lexer        -- a constant symbol
lineP     = many $ noneOf "\n"    -- a full line
tokenP    = many $ noneOf ";"     -- a token, i.e. everything up to the next semicolon
```

With these helpers, we can now construct the parsers for each of our commands:

```
roundStartingP = do
  try $ symbolP "ROUND STARTING"
  semiP
  token <- tokenP
  return $ RoundStarting token
```

The `roundStartingP` parser parses the first of our two commands, `ROUND STARTING`. It first reads the constant symbol, then a semicolon, and then the token,
 and finally it returns an element of type `Command` constructed by the `RoundStarting` data constructor which takes the parsed token as its argument.
 The `try` function tells Parsec not to consume any characters from our input string when the symbol does not match.

```
yourTurnP = do
  try $ symbolP "YOUR TURN"
  semiP
  token <- tokenP
  return $ YourTurn token
```

In `yourTurnP` we parse the second command, `YOUR TURN`. It is identical in structure to the first command, and unsurprisingly the parser is also identical
in structure.

```
unknownP = do
  unknownCommand <- lineP
  return $ Unknown unknownCommand
```

`unknownP` is our final parser, which simply reads the whole line and returns it as the argument of the `Unknown` data constructor.

## Putting It All Together

Parsec actually is a parser _combinator_, and it will hopefully become clear what this means when we look at our full parsing function:

```
commandParser :: Parser Command
commandParser = roundStartingP
            <|> yourTurnP
            <|> unknownP
            <?> "Parse error"
```

Parsec tries out in sequence each of the parser functions that are combined via `<|>`. If none matches, it raises a parse error.

Now, how to use this beast? We can define a generic function `runParser`:

```
runParser :: Parser a -> String -> a
runParser p str = case parse p "" str of
  Left err  -> error $ "parse error at " ++ (show err)
  Right val -> val
```

and define our actual parser function with its help:

```
parseCommand :: String -> Command
parseCommand = runParser commandParser
```

That's it!

You can have a look [at the full parser code in my Mia bot](https://github.com/NicoleRauch/maexchen/blob/master/client/haskell-bot/src/MessageParser.hs).

If you have any questions, comments or suggestions for improvement, please feel free to drop me a line (see below).

<hr/>

## Comments:

(please comment on this article <a href="mailto:info@nicole-rauch.de?Subject=Your blogpost 'CoolBeans - Dependency Injection for Node.js'">via email</a>)
