#lang scribble/manual
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

@section[#:tag"language-ref"]{Language Reference}

@subsection[#:tag"paper"]{Paper}
The Paper statement creates a drawing canvas of 100 by 100 pixels. Each pixel of the
canvas is set to the specified @secref{color}.

Example:

@codeblock{Paper 0}

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

@section[#:tag"language-guide"]{Language Guide}
To begin using Design by Numbers (DBN), you must
first install the design-by-numbers package. You can do
this by going to the Racket menu and selecting file, then
Package Manager. Select the 'Available from Catalog' tab,
click Update Package List to get the latest list of packages,
and then search for design-by-numbers. When you find this,
click 'Install' to install it. 

To create a program in DBN, you must put
@codeblock{#lang dbn} at the top of the Racket Definitions
area. Note, this is
only required because you need to let Racket know which kind of
language will follow. By doing this, you tell Racket to use the
DBN language. If you find examples on the web or in John Maeda's
book, you will @italic{not} see this, so you can simply add
it to the top of those examples.

When you Save the definitions in Racket, your code will be saved to
a file which will end in .rkt or .dbn (you can chose either). Most
examples you find on the web might end in .dbn, but it doesn't affect
the contents of the file. By clicking on the Run button in Racket,
you will run (or execute) the code you've typed in the Defintions.

Try this first program in DBN:

@codeblock{#lang dbn
           Paper 0}

You should see a small window pop up on the screen. This is the
@italic{paper} that your statements in your program will draw to.
Racket allows you to interact with the current program, so at the
bottom of Racket, you should find the @italic{Interactions} area
where you can type statements. Try typing the following:

@codeblock{Paper 100}

The window should turn black. This is because all color in DBN
is on the grey scale and goes from totally white to totally black.
Colors are represented by numbers that indicate what percentage of
ink is transfered to the paper. 0 means no ink while 100 means 100%
of the ink is drawn to the paper. A number of 50 will give you a
value half-way between no ink and all ink.

DBN has other commands that you can use to do more interesting things
than just coloring a page with a single color. Keep reading to
learn more!

@subsection[#:tag"syntax"]{Syntax}
In Design by Numbers (DBN), some words belong to the language and
we call these words the syntax. In addition to the words, the rules
for how they can be written are called the grammar (just like your
spoken language has a grammar, a programming language has a grammar,
albeit a pretty simple one). 

In DBN, the syntax and grammar can be broadly divided into @secref{statements},
@secref{definitions}, and @secref{expressions}. Both statements and definitions must be
separated by at least one new line character (i.e., pressing enter or
return on your keyboard). A statement usually tells DBN what you would
like it to do, while a definition creates new kinds of statements and
expressions. Expressions, on the other hand, are primarily used for
different kinds of calculations.

In DBN, case @italic{does not matter}, thus, you may use upper, lower
or mixed case when writing programs. 

@subsection[#:tag"statements"]{Statements}
DBN has the following built-in statements: @secref{paper}, @secref{pen}
@secref{set}, @secref{same}, @secref{notsame}, @secref{smaller},
@secref{notsmaller}, @secref{repeat}, @secref{forever}, @secref{load},
and @secref{antialias}. You can see the details of any of those
statements, but like with Paper commands, they must all be written
on their own line in a DBN program. For example (and remember
that you must have #lang dbn at the top of your file):

@codeblock{
 Paper 0
 Pen 100
 Line 0 0 100 100
}

This will draw a line from the bottom left of the paper to the top
right. Paper in DBN has @italic{coordinates}, which are numbers
that indicate where on the paper a color can go. The bottom left
corner is the coordinate (0, 0) and the top right corner is (100, 100).
When we execute the @secref{line} command above, we are telling
DBN to use the current @secref{pen} @secref{color} and draw a line
from coordinate (0, 0) to (100, 100). The first part of the coordinate,
which you may remember as 'x' from school, starts at 0 on the left
and goes to 100 on the right. The second part of the coordinate goes
from 0 on the bottom and up to 100 on the right.

Using the Interactions area, can you draw a line from the top left
to the bottom right? What would the command be?

@codeblock{Line 0 100 100 0}

DBN has many other kinds of commands which we suggest you explore!

@subsection[#:tag"definitions"]{Definitions}
DBN allows you to create @italic{procedures} and @italic{functions}.
A procedure defines a new kind of statement and you tell it exactly
how many arguments it needs to execute that statement. A function
defines a kind of calculation that will return a value when called
as part of @secref{expressions}.

To create a procedure, use @secref{command}. For example, to create
a command that draws a horizontal line, you could write it as
follows:

@codeblock{
           Command HorizontalLine Y
           {
             Line 0 y 100 Y
           }
                                    }

@subsection[#:tag"expressions"]{Expressions}
Design by numbers understands math expressions and allows the use of parenthesis
for clarity and to enforce a particular order of operation. For example,

@codeblock{
  Set X (17 * Y + 3)
}

Anywhere math is used, a Number may also be substituted.

@subsection[#:tag"color"]{Color}
All drawings in Design By Numbers use grey-scale colors. Color values are
specified using numeric values between 0 and 100 inclusive.

Color values are required for @secref{paper} and some types of
@secref{set} statements.
