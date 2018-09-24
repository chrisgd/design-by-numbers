#lang scribble/manual

@(require (for-label racket))

@title{Design By Numbers}

@table-of-contents[]

@section[#:tag"language-ref"]{Language Reference}



@subsection[#:tag"paper"]{Paper}
The Paper statement creates a drawing canvas of 100 by 100 pixels. Each pixel of the
canvas is set to the specified @secref{color}.

Example:

@codeblock{Paper 0}

@subsection[#:tag"pen"]{Pen}
The Pen statement specifies the color of any following drawing statements
such as @secref{line} statements.

@subsection[#:tag"set"]{Set}
There are two forms of Set statements: one associates a value to
a variable name and the other assigns a color value to a location
on the current paper.

@subsection[#:tag"line"]{Line}
Draw a line from one paper location to another using the current pen
color.

@subsection[#:tag"value"]{Value}
This statement specifies the resulting value of a @secref{number}
statement.

@subsection[#:tag"repeat"]{Repeat}
Causes a sequence of statements to be repeated the
specified number of times. The statements are performed once
for each value of the repetition.

@subsection[#:tag"forever"]{Forever}
Causes a sequence of statements to be repeated an infinite
number of times.

@subsection[#:tag"same"]{Same}
Determines if two values are the same.

@subsection[#:tag"notsame"]{Not Same}
Determines if two values are not the same.

@subsection[#:tag"smaller"]{Smaller}
Determines if one value is smaller than another value.

@subsection[#:tag"notsmaller"]{Not Smaller}
Determines if one value is not smaller than another value.

@subsection[#:tag"command"]{Command}
Groups a sequence of statements together and associates that
sequence of statements with a name. This name can then be used
to invoke the sequence of statements.

@subsection[#:tag"number"]{Number}
Groups a sequence of statements together and associates that
sequence of statement with a name. The sequence of statements is
expected to produce a value using the @secref{value} statement.

@subsection[#:tag"load"]{Load}
Load another file containing Design By Numbers code into the
current Design By Numbers program.

@subsection[#:tag"antialias"]{Antialias}
This statement attempts to "smooth" the resulting Design By Numbers
drawing.

@section[#:tag"errors"]{Description of Errors}

@section[#:tag"language-guide"]{Language Guide}

@subsection[#:tag"color"]{Color}
All drawings in Design By Numbers use grey-scale colors. Color values are
specified using numeric values between 0 and 100 inclusive.

Color values are required for @secref{paper} and some types of
@secref{set} statements.
