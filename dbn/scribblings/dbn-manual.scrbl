#lang scribble/manual

@(module test racket/base)

@(require (for-label racket))

@title{Design By Numbers}

@author[(author+email "Jeff Edgington" "jedgingt@cs.du.edu")
        (author+email "Chris GauthierDickey" "chrisg@cs.du.edu")]

Design by Numbers is a language created by John Maeda for the purpose
teaching artists how to program. It was the predecessor to Processing
which was created by his students and is no longer developed. This
version was created to allow examples from his book and those floating
around to still be used.

The language itself has a simple syntax and grammar with a few basic
concepts. All code draws to and reads from a simple 100x100 canvas, called
the paper and colors are only in greyscale. Different commands allow
you to draw lines, points, branch and create loops. In addition, the
language has procedures (which do not return a value) and functions.

@table-of-contents[]


@include-section["dbn-guide.scrbl"]


@section[#:tag"language-ref"]

@subsection[#:tag"paper"]{Paper}
The Paper statement creates a drawing canvas of 100 by 100 pixels. Each pixel of the
canvas is set to the specified @secref{color}.

Example:

@codeblock|{
  #lang dbn
  Paper 0
}|

@subsection[#:tag"pen"]{Pen}
The Pen statement specifies the @secref{color} of any following drawing statements
such as @secref{line} statements.

@codeblock{Pen 100}

@subsection[#:tag"set"]{Set}
There are two forms of Set statements: one associates a value to
a variable name and the other assigns a color value to a location
on the current paper.

The following code creates the variable named X and assigns the value 5 to it.
@codeblock{Set X 5}

In the other version of Set, you can change the coordinate value on the paper
to a specific color:
@codeblock{Set [25 50] 50}
This code sets the point at (25, 50) on the current @secref{paper} to the
@secref{color} 50.


@subsection[#:tag"line"]{Line}
Draw a line from one paper location to another using the current pen
color.
@codeblock{Paper 100
           Pen 0
           Line 0 0 100 100
}
This code draws a line from the bottom left corner of the paper to the top right
corner. 

@subsection[#:tag"repeat"]{Repeat}
Causes a sequence of statements to be repeated the
specified number of times. The statements are performed once
for each value of the repetition.

@codeblock{Repeat Y 25 50 {
             Line 10 Y 90 Y
           } }

This block of code draws a block of horizontal lines. The first
number is where the variable Y starts and the last number is where it
ends. Any expression which results in a number can be used as the
starting and ending values of the Repeat. In addition, the variable Y,
or the variable you chose to use for Repeat, is only valid within the
block of code between the curly-braces following Repeat.

@subsection[#:tag"forever"]{Forever}
Causes a sequence of statements to be repeated an infinite
number of times.

@codeblock{Forever {
             Set [<Mouse 1> <Mouse 2>] 100
           }
}
This code draws a point wherever it detects the mouse on the paper.

@subsection[#:tag"same"]{Same?}
Determines if two values are the same. If they are, the block of
code within curly-braces is executed.

@codeblock{Same? x 5
           {
             Line 0 0 100 100
           }
}

@subsection[#:tag"notsame"]{Not Same?}
Determines if two values are not the same. If they are not, the block of
code within curly-braces is executed.

@codeblock{NotSame? x 5
           {
             Line 0 0 100 100
           }
}

@subsection[#:tag"smaller"]{Smaller?}
Determines if one value is smaller than another value. If it is,
the block of code within the following curly-braces is executed.

@codeblock{Smaller? x 5 {
             Line 0 0 100 100
           }
}

@subsection[#:tag"notsmaller"]{Not Smaller?}
Determines if one value is not smaller than another value. If it is not,
the block of code within the following curly-braces is executed.

@codeblock{NotSmaller? x 5 {
             Line 0 0 100 100
           }
}

@subsection[#:tag"command"]{Command}
Groups a sequence of statements together and associates that
sequence of statements with a name. This name can then be used
to invoke the sequence of statements.

@codeblock{
Command HorizontalLine Y
{
   Line 0 Y 100 Y
}
}

@subsection[#:tag"number"]{Number}
Groups a sequence of statements together and associates that
sequence of statement with a name. The sequence of statements is
expected to produce a value using the @secref{value} statement.

@codeblock{
Number Add2 X
{
   Value X + 2
}
}
This Number definition can then be used in a statement as follows:

@codeblock{
// This sets the color for coordinate (50, 50) to
// 42 (since it executes
the Add2 function)
Set [50 50] <Add2 40>
}


@subsection[#:tag"value"]{Value}
This statement specifies the resulting value of a @secref{number}
statement. See @secref{number} for an example. Value may only
be executed within the definition of a Number.

@subsection[#:tag"load"]{Load}
Load another file containing Design By Numbers code into the
current Design By Numbers program.

@codeblock{
  Load dbngraphics.dbn
}

@subsection[#:tag"antialias"]{Antialias}
This statement attempts to "smooth" the resulting Design By Numbers
drawing. Valid values are 0, 1, and 2. 0 indicates no anti-aliases,
and both 1 and 2 give different kind of smoothings. By default,
DBN uses antialiasing 2.

@codeblock{
  Antialias 2
}

@subsection[#:tag"time"]{Time}
Time can be accessed through a Number expression:

@codeblock{
Set X <Time 1>
}
This sets X to be the current hour. Time takes
4 possible values, where 1 = hour, 2 = minute,
3 = second, and 4 = millisecond.

@subsection[#:tag"mouse"]{Mouse}
Mouse positions can also be accessed like a number by
passing a value 1 or 2 to the Mouse function:

@codeblock{
Set [<Mouse 1> <Mouse 2>] 100
}

Passing a 3 returns 0 if the left mouse button is up and 100 if it is down.

@subsection[#:tag"key"]{Key}
Access to keyboard strokes are through the Key function which takes a
number from 1 to 26, where 1 returns the state of the key 'A' and 26
returns the state of the key 'Z'. Each of these returns 0 when it is up and
100 when the key is down.

@codeblock{
Set X <Key 3>
}
This code sets X to be 100 if the 'C' key is being pressed. 

@section[#:tag"errors"]{Description of Errors}
Syntax errors generally indicate that the name of a command was mistyped. Usually
DBN will let you know where it doesn't understand something, so check there first
to see what it may be.

Parser errors generally indicate that you didn't use the right sentence
structure when telling DBN what to do. Check this document for examples
of using statements and such correctly.


