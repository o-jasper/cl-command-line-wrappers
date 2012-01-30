
# Command line-using functions for Common lisp

These provide a bunch of functions of some commandline utilities, so their 
output can be more easily used..

## Dependencies

* [Alexandria](http://common-lisp.net/project/alexandria/).

* Various stuff from [j-basic](https://github.com/o-jasper/j-basic).
  iw-scan and cl-acpi-command depend on [regex](http://www.cliki.net/REGEX) 
  via that.

* If SBCL [trivial-shell](http://common-lisp.net/project/trivial-shell/)
 and [unix-options](https://github.com/astine/unix-options), if clisp it uses 
the inbuild ways to access the shell.

* The program involved.

## Main functions and variables:

### args-ncommand
Basic functions providing access to commandline. If SBCL, depends on if clisp
 not.

### cl-ps-command
Uses the `ps` commands, the defun `ps` takes as argument what aspects you want
`+ps-allowed+` contains the ones available. It defaultly returns a list of
 lists filled with the values as requested, but there is also a `:hook` 
keyword so you can take the information immediately.

Also a `do-ps` where you just provide (non-keyword)variables it will fill the
variables with the associated keywords.

Warning todo: some of the things to get like `:args` actually return lists.

### cl-wmctrl
Uses [`wmctrl`](http://tomas.styblo.name/wmctrl/) `wm-list` works the same as
 the `ps` one, you provide a list of stuff you want, `+wmctrl-allowed+` 
listing them, and there is also a `do-wm-list` that works the same.

`:command` is also allowed, `wmctrl` doesn't(afaik) do this, but `cl-wmctrl`
uses a `ps` command to get it anyway. (because it is useful)

Warning todo: potentially the same problem as cl-ps-command.

### iw-scan, iw-scan-continuous
Uses `(sudo) iwlist wlan0 scanning` and looks at its output.

`iw-scan` is the main function, it defaultly returns a list for each interface
which then contains a list of plists with the different properties of the 
cells.

provides a `:cell-hook` that runs of each cell, and a `:hook` running on each
 interface.(Using latter makes the earlier moot.) `:sudo` is defaultly *on*.

`iw-cur` returns the current connection.

It is the reason why the read-tab-listing package exists.

### cl-acpi-command, cl-acpi-classes
Classes and commandline-acpi-user. Note that a CFFI based on the C-interface
to acpi would be *superior*, for this reason i won't do any work on this one.
 Used the classes because you can then have`:documentation`, but it is 
overkill.

### cl-top-command
Handles the first few lines of `top` output(badly) and runs the lines through
 a hook one by one, defaultly making a list of plists.

## TODO

* They don't respond well to non-ascii, pretty damningly, especially for if
 cl-wmctrl or cl-iw-scan see many cases.

* The potentially bad behavior of some of the things wmctrl and ps can suck.

* iw-scan is deviant from wmctrl and ps in behavior.

* Some examples.

* More proper use of streams, see if external-program is good.

* The implementation of regex-sequence underneath could probably be more 
  efficient.


##Copyright
Everything is under GPLv3, license included under `doc/`

##Author

Jasper den Ouden
