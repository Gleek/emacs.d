My Emacs settings
==================
Includes ALL the packages installed from melpa.

A better way would have been to simply list package and install packages via `package.el` on load, but the current system saves my time, since all my systems have the same version of Emacs.

In case of problems `M-: (byte-recompile-directory package-user-dir nil 'force)` should help.

Usage
------
`git clone https://www.github.com/gleek/.emacs .emacs.d`

TODO
---------
- Organize all the settings properly
- Maintain different files for all the settings
