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
- Maintain different file for different modules
- Using use-package for a faster boot-time
- [Single File master Configuration](http://milkbox.net/note/single-file-master-emacs-configuration/)
