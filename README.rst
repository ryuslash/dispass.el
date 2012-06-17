dispass.el Emacs wrapper
******************************************************************************

dispass.el --- Generate and disperse/dispell passwords

dispass.el is an emacs wrapper around dispass (http://dispass.babab.nl).

Copyright (C) 2012 Tom Willemsen <tom@ryuslash.org>

:Author: Tom Willemsen <tom@ryuslash.org>
:Created: Jun 8, 2012
:Version: 0.1a7.2
:Keywords: encryption, security

Permission to use, copy, modify, and distribute this software for any
purpose with or without fee is hereby granted, provided that the
above copyright notice and this permission notice appear in all
copies.

THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL
WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE
AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR
CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS
OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.


Installation
==============================================================================

Place this file somewhere on your filesystem, either in your
`load-path` or somewhere else which you will have to add to your
`load-path`, like so::

    (add-to-list 'load-path "/location/of/dispass.el")

And then `load`, `require` or `autoload` it in your emacs init
file, for example::

    (require 'dispass)

*Or* if you have package.el you could use `package-install-file`.

Customization
==============================================================================

`dispass.el` only offers customization of the `dispass-executable`
variable for the moment. This is the location where the dispass
executable is located.

Usage
==============================================================================

Using dispass.el is simple, once installed. Either call `dispass`
to recall a priviously generated password or call `dispass-create`
to generate a new password.

The only real difference between the two is that `dispass-create`
asks to confirm the password. Both will ask for a label.

When a numeric argument is used when calling either `dispass-create`
or `dispass`, that argument is sent to the dispass program along with
the `-l` switch. This cuts the length of the password to that many
characters. For example::

    C-5 M-x dispass<RET> test<RET> test<RET>

will generate a password of 5 characters for label "test" and password
"test".

Once a password has been generated it is inserted into the kill
ring and the system's clipboard so it can be easily inserted into
password field, this makes the generated password easy to see in
plaintext in the `kill-ring` variable, though.
