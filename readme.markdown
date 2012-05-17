
# Command line-using functions for Common lisp

These provide a bunch of functions of some commandline utilities, so their 
output can be more easily used..

For the [`wmctrl`](http://tomas.styblo.name/wmctrl/), `iwlist scanning`, 
[`pinot-search`](http://pinot.berlios.de/documentation.html), `ps` and `top` 
command. I use the `wmctrl` to control my the windows.(Maybe later that
code will end up here too)

## Dependencies

* [Alexandria](http://common-lisp.net/project/alexandria/).

* Various stuff from [j-basic](https://github.com/o-jasper/j-basic).
  iw-scan and cl-acpi-command depend on [regex](http://www.cliki.net/REGEX) 
  via that.

* If SBCL [trivial-shell](http://common-lisp.net/project/trivial-shell/)
 and [unix-options](https://github.com/astine/unix-options), if clisp it uses 
the inbuild ways to access the shell. In case of pinot-parse, 
 [external-program](http://common-lisp.net/project/external-program/)

* The program involved.

## Main functions and variables:

### cl-ps-command
Uses the `ps` commands, the defun `ps` takes as argument what aspects you want
`+ps-allowed+` contains the ones available. It defaultly returns a list of
 lists filled with the values as requested, but there is also a `:hook` 
keyword so you can take the information immediately.

Using cffi on the C interface would probably be superior.

**Warning:** some of the available options, like `:args` produce list, 
because how it works internally, tokenizing on whitespace instead of how the 
table works.

Also a `ps-do` where you just provide (non-keyword)variables it will fill the
variables with the associated keywords. For the reason as in the warnig
`ps-do` has `&rest` for the last element, for this reason all things to query
for in the list `+ps-produce-list+` *must* be last.
(this is unfortunate, probably a CFFI interface would be better)

### cl-wmctrl
Uses [`wmctrl`](http://tomas.styblo.name/wmctrl/) The function `wm-list` works
 the same as the `ps` one, you provide a list of stuff you want, 
`+wmctrl-allowed+` listing them, and there is also a `wm-list-do` that works 
the same.

`:command` is also allowed, `wmctrl` doesn't(afaik) do this, but `cl-wmctrl`
uses a `ps` command to get it anyway. (because it is useful)

### iw-scan, iw-scan-continuous
Uses `(sudo) iwlist wlan0 scanning` and looks at its output.

`iw-scan` is the main function, it defaultly returns a list for each interface
which then contains a list of plists with the different properties of the 
cells.

provides a `:cell-hook` that runs of each cell, and a `:hook` running on each
 interface.(Using latter makes the earlier moot.) `:sudo` is defaultly *on*.

`iw-cur` returns the current connection.

It is the reason why the read-tab-listing package exists in j-basic.

### cl-pinot-command
Function of the same name runs a `pinot-search` command. Turns the names of
different entries into keywords a list with keywords. Of course, pinot has to
be set up. Defaultly, the function uses the special variable contents, 
`*search-engine*` and `*search-db*`, defaulting to `:xapian` and
`(from-homedir ".pinot/")`.

`(:ran-query query time time-unit)` where `query` is the query string. 
 `time-unit` is a symbol, but for instance microseconds is `:|ms|`, because if
it were `:|Ms|` that would be different!(So symbol with lowercase.)

`(:showing-of shown-cnt total-cnt)`, number pinot-parse tells us we got and 
the number it says match the query.

`(:not-identified ...stuff..)`(hopefully none of that), 

and for the search results, `result-number ..plist..`
each of the words pinot lists is plist-ified: `:location`, `:date`, `:size`, 
`:score`, `:title`, `:type`, `:language`, `:extract`.

### cl-acpi-command
Commandline-acpi-user. Note that a CFFI based on the C-interface
to acpi would be superior. Earlier used the classes
because you can then have`:documentation`, but it is overkill.

It consists of an `acpi` command, all arguments are optional, defaultly it 
uses `acpi --everything`, otherwise, see the docstring.

### cl-top-command
Provides a function `top` that handles few lines of `top` output(badly) and
runs the lines through a function. Defaultly the function is `top-line` which
makes plists.(and `top` thus produces a list of plists)

### args-n-command
Basic functions providing access to commandline. If clisp it uses clisps 
facility, otherwise, trivial shell.

## TODO

* Would be better if cl-ps-command figured out the table instead of
  tokenizing, but feel hardly worth the effort for me.

* Use of streams, using external-program might be better, but probably hardly
  worth it.

## Copyright
Everything is under GPLv3, license included under `doc/`

## Author

Jasper den Ouden

