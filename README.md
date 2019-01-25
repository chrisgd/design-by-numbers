Design by Numbers for Racket
===
Design by Numbers (DBN) is both a language and a book by John Maeda that was
created to help people who were artists and designers understand how to
code and the purpose behind coding. The language itself is simple with
very few commands in an effort to make it easy to remember the syntax.

Today, finding working versions of DBN can be challenging so we created
a version that runs on top of the Racket ecosystem. To use this, you can
download Racket (http://racket-lang.org) and go to File | Package Manager. 
From here, you can download and install design-by-numbers (this repository)
and you can use the language by putting #lang racket at the top of your
file.  

For example, your first program should look like the following:
```racket
#lang dbn
Paper 50
```

When you hit 'Run' in Dr. Racket, you should get a window that is filled with grey.

Details on the language itself can be found in the documentation. From Dr. Racket,
go to the Help menu item, open the Racket documentation and search for Design
By Numbers (which should be under Languages). This documentation explains how to
use the language, what the commands are, and so forth.
