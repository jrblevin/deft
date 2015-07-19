Deft for Emacs
==============

<!-- Automatically generated from comments in deft.el. -->

Deft is an Emacs mode for quickly browsing, filtering, and editing
directories of plain text notes, inspired by Notational Velocity.
It was designed for increased productivity when writing and taking
notes by making it fast and simple to find the right file at the
right time and by automating many of the usual tasks such as
creating new files and saving files.

Deft is open source software and may be freely distributed and
modified under the BSD license.  Version 0.6 is the latest stable
version, released on June 26, 2015.  You may download it
directly here:

  * [deft.el](http://jblevins.org/projects/deft/deft.el)

To follow or contribute to Deft development, you can either
[browse](http://jblevins.org/git/deft.git) or clone the Git
repository:

    git clone git://jblevins.org/git/deft.git

![Deft 0.6 Screencast](http://jblevins.org/projects/deft/deft-v0.6.gif)

The Deft buffer is simply a file browser which lists the titles of
all text files in the Deft directory followed by short summaries
and last modified times.  The title is taken to be the first line
of the file and the summary is extracted from the text that
follows.  Files are, by default, sorted in terms of the last
modified date, from newest to oldest.

All Deft files or notes are simple plain text files where the first
line contains a title.  As an example, the following directory
structure generated the screenshot above.

    % ls ~/.deft
    about.txt    browser.txt     directory.txt   operations.txt
    ack.txt      completion.txt  extensions.org
    binding.txt  creation.txt    filtering.txt

    % cat ~/.deft/about.txt
    # About

    An Emacs mode for slicing and dicing plain text files.

Deft's primary operation is searching and filtering.  The list of
files can be limited or filtered using a search string, which will
match both the title and the body text.  To initiate a filter,
simply start typing.  Filtering happens on the fly.  As you type,
the file browser is updated to include only files that match the
current string.

To open the first matching file, simply press <kbd>RET</kbd>.  If no files
match your search string, pressing <kbd>RET</kbd> will create a new file
using the string as the title.  This is a very fast way to start
writing new notes.  The filename will be generated automatically.
If you prefer to provide a specific filename, use <kbd>C-RET</kbd> instead.

To open files other than the first match, navigate up and down
using <kbd>C-p</kbd> and <kbd>C-n</kbd> and press <kbd>RET</kbd> on the file you want to open.
When opening a file, Deft searches forward and leaves the point
at the end of the first match of the filter string.

You can also press <kbd>C-o</kbd> to open a file in another window, without
switching to the other window.  Issue the same command with a prefix
argument, <kbd>C-u C-o</kbd>, to open the file in another window and switch
to that window.

To edit the filter string, press `DEL` (backspace) to remove the
last character or <kbd>M-DEL</kbd> to remove the last "word".  To yank
(paste) the most recently killed (cut or copied) text into the
filter string, press <kbd>C-y</kbd>.  Press <kbd>C-c C-c</kbd> to clear the filter
string and display all files and <kbd>C-c C-g</kbd> to refresh the file
browser using the current filter string.

For more advanced editing operations, you can also edit the filter
string in the minibuffer by pressing <kbd>C-c C-l</kbd>.  While in the
minibuffer, the history of previous edits can be cycled through by
pressing <kbd>M-p</kbd> and <kbd>M-n</kbd>.  This form of static, one-time filtering
(as opposed to incremental, on-the-fly filtering) may be preferable
in some situations, such as over slow connections or on systems
where interactive filtering performance is poor.

By default, Deft filters files in incremental string search mode,
where "search string" will match all files containing both "search"
and "string" in any order.  Alternatively, Deft supports direct
regexp filtering, where the filter string is interpreted as a
formal regular expression.  For example, `^\(foo\|bar\)` matches
foo or bar at the beginning of a line.  Pressing <kbd>C-c C-t</kbd> will
toggle between incremental and regexp search modes.  Regexp
search mode is indicated by an "R" in the mode line.

Common file operations can also be carried out from within Deft.
Files can be renamed using <kbd>C-c C-r</kbd> or deleted using <kbd>C-c C-d</kbd>.
New files can also be created using <kbd>C-c C-n</kbd> for quick creation or
<kbd>C-c C-m</kbd> for a filename prompt.  You can leave Deft at any time
with <kbd>C-c C-q</kbd>.

Unused files can be archived by pressing <kbd>C-c C-a</kbd>. Files will be
moved to `deft-archive-directory`, which is a directory named
`archive` within your `deft-directory` by default.

Files opened with deft are automatically saved after Emacs has been
idle for a customizable number of seconds.  This value is a floating
point number given by `deft-auto-save-interval` (default: 1.0).

Getting Started
---------------

To start using it, place it somewhere in your Emacs load-path and
add the line

    (require 'deft)

in your `.emacs` file.  Then run <kbd>M-x deft</kbd> to start.  It is useful
to create a global keybinding for the `deft` function (e.g., a
function key) to start it quickly (see below for details).

When you first run Deft, it will complain that it cannot find the
`~/.deft` directory.  You can either create a symbolic link to
another directory where you keep your notes or run <kbd>M-x deft-setup</kbd>
to create the `~/.deft` directory automatically.

One useful way to use Deft is to keep a directory of notes in a
Dropbox folder.  This can be used with other applications and
mobile devices, for example, [nvALT][], [Notational Velocity][], or
[Simplenote][] on OS X or [Editorial][], [Byword][], or [1Writer][]
on iOS.

[nvALT]: http://brettterpstra.com/projects/nvalt/
[Notational Velocity]: http://notational.net/
[Simplenote]: http://simplenote.com/
[Editorial]: https://geo.itunes.apple.com/us/app/editorial/id673907758?mt=8&uo=6&at=11l5Vs&ct=deft
[Byword]: https://geo.itunes.apple.com/us/app/byword/id482063361?mt=8&uo=6&at=11l5Vs&ct=deft
[1Writer]: https://geo.itunes.apple.com/us/app/1writer-note-taking-writing/id680469088?mt=8&uo=6&at=11l5Vs&ct=deft

Basic Customization
-------------------

You can customize items in the `deft` group to change the default
functionality.

By default, Deft looks for notes by searching for files with the
extensions `.txt`, `.text`, `.md`, `.markdown`, or `.org` in the
`~/.deft` directory.  You can customize both the file extension and
the Deft directory by running <kbd>M-x customize-group</kbd> and typing
`deft`.  Alternatively, you can configure them in your `.emacs`
file:

    (setq deft-extensions '("txt" "tex" "org")
    (setq deft-directory "~/Dropbox/notes"))

The first element of `deft-extensions` (or in Lisp parlance, the
car) is the default extension used to create new files.

By default, Deft only searches for files in `deft-directory` but
not in any subdirectories. Set `deft-recursive` to a non-nil value
to enable recursive searching for files in subdirectories:

    (setq deft-recursive t)

You can easily set up a global keyboard binding for Deft.  For
example, to bind it to F8, add the following code to your `.emacs`
file:

    (global-set-key [f8] 'deft)

Reading Files
-------------

The displayed title of each file is taken to be the first line of
the file, with certain characters removed from the beginning. Hash
characters, as used in Markdown headers, and asterisks, as in Org
Mode headers, are removed.  Additionally, Org mode `#+TITLE:` tags,
MultiMarkdown `Title:` tags, LaTeX comment markers (<kbd>%</kbd>), and
Emacs mode-line declarations (e.g., `-*-mode-*-`) are stripped from
displayed titles.  This can be customized by changing
`deft-strip-title-regexp`.

More generally, the title post-processing function itself can be
customized by setting `deft-parse-title-function`, which accepts
the first line of the file as an argument and returns the parsed
title to display in the file browser.  The default function is
`deft-strip-title`, which removes all occurrences of
`deft-strip-title-regexp` as described above.

For compatibility with other applications which use the filename as
the title of a note (rather than the first line of the file), set the
`deft-use-filename-as-title` flag to a non-`nil` value. Deft will then
use note filenames to generate the displayed titles in the Deft
file browser. To enable this, add the following to your `.emacs` file:

    (setq deft-use-filename-as-title t)

Finally, the short summary that is displayed following the file
title can be customized by changing `deft-strip-summary-regexp`. By
default, this is set to remove certain org-mode metadata statements
such as `#+OPTIONS:` and `#+AUTHOR:`.

Creating Files
--------------

Filenames for newly created files are generated by Deft automatically.
The process for doing so is determined by the variables
`deft-use-filename-as-title` and `deft-use-filter-string-for-filename`
as well as the rules in the `deft-file-naming-rules` alist.
The possible cases are as follows:

1.  **Default** (`deft-use-filename-as-title` and
    `deft-use-filter-string-for-filename` are both `nil`):

    The filename will be automatically generated with prefix `deft-`
    and a numerical suffix as in `deft-0.ext`, `deft-1.ext`, ...
    The filter string will be inserted as the first line of the file
    (which is also used as the display title).

2.  **Filenames as titles** (`deft-use-filename-as-title` is non-`nil`):

    When `deft-use-filename-as-title` is non-`nil`, the filter string
    will be used as the filename for new files (with the appropriate
    file extension appended to the end).

3.  **Readable filenames** (`deft-use-filename-as-title` is
    `nil` but `deft-use-filter-string-for-filename` is non-`nil`):

    In this case you can choose to display the title as parsed from
    the first line of the file while also generating readable
    filenames for new files based on the filter string. The
    variable `deft-use-filter-string-for-filename` controls this
    behavior and decouples the title display
    (`deft-use-filename-as-title`) from the actual filename. New
    filenames will be generated from the filter string and
    processed according to the rules defined in the
    `deft-file-naming-rules` alist. By default, slashes are removed
    and replaced by hyphens, but many other options are possible
    (camel case, replacing spaces by hyphens, and so on). See the
    documentation for `deft-file-naming-rules` for additional
    details.

Titles inserted into files from the filter string can also be
customized for two common modes, `markdown-mode` and `org-mode`, by
setting the following variables:

* `deft-markdown-mode-title-level` - When set to a positive
  integer, determines how many hash marks will be added to titles
  in new Markdown files. In other words, setting
  `deft-markdown-mode-title-level` to <kbd>2</kbd> will result in new files
  being created with level-2 headings of the form `## Title`.

* `deft-org-mode-title-prefix` - When non-nil, automatically
  generated titles in new `org-mode` files will be prefixed with
  `#+TITLE:`.

Other Customizations
--------------------

Deft, by default, lists files from newest to oldest.  You can set
`deft-current-sort-method` to 'title to sort by file titles, case
ignored.  Or, you can toggle sorting method using
`deft-toggle-sort-method`.

Incremental string search is the default method of filtering on
startup, but you can set `deft-incremental-search` to nil to make
regexp search the default.

Deft also provides a function for opening files without using the
Deft buffer directly.  Calling `deft-find-file` will prompt for a
file to open, just like `find-file`, but starting from
`deft-directory`.  If the file selected is in `deft-directory`, it
is opened with the usual deft features (automatic saving, automatic
updating of the Deft buffer, etc.).  Otherwise, the file will be
opened by `find-file` as usual.  Therefore, you can set up a global
keybinding for this function to open Deft files anywhere.  For
example, to use <kbd>C-x C-g</kbd>, a neighbor of <kbd>C-x C-f</kbd>, use the
following:

    (global-set-key (kbd "C-x C-g") 'deft-find-file)

The faces used for highlighting various parts of the screen can
also be customized.  By default, these faces inherit their
properties from the standard font-lock faces defined by your current
color theme.

Acknowledgments
---------------

Thanks to Konstantinos Efstathiou for writing simplenote.el, from
which I borrowed liberally, and to Zachary Schneirov for writing
Notational Velocity, whose functionality and spirit I wanted to
bring to Emacs.

History
-------

Version 0.6 (2015-06-26):

* Recursive search in subdirectories (optional). Set
  `deft-recursive` to a non-nil value to enable.
* Support for multiple extensions via the `deft-extensions` list.
  As such, `deft-extension` is now deprecated.
* New variable `deft-create-file-from-filter-string` can enable
  generation of new filenames based on the filter string. This decouples
  the title display (`deft-use-filename-as-title`) from the actual filename
  generation.
* New variable `deft-file-naming-rules` allows customizing generation
  of filenames with regard to letter case and handling of spaces.
* New variables `deft-markdown-mode-title-level` and
  `deft-org-mode-title-prefix` for automatic insertion of title markup.
* Archiving of files in `deft-archive-directory`.
* Ability to sort by either title or modification time via
  `deft-current-sort-method`.
* Update default `deft-strip-title-regexp` to remove the following:
    - org-mode `#+TITLE:` tags
    - MultiMarkdown `Title:` tags
    - LaTeX comment markers (i.e., `%`)
    - Emacs mode-line declarations (e.g., `-*-mode-*-`)
* Remove leading and trailing whitespace from titles.
* Disable visual line mode to prevent lines from wrapping.
* Enable line truncation to avoid displaying truncation characters.
* Show the old filename as the default prompt when renaming a file.
* Call `hack-local-variables` to read file-local variables when
  opening files.
* Fixed several byte-compilation warnings.
* Bug fix: more robust handling of relative and absolute filenames.
* Bug fix: use width instead of length of strings for calculations.
* Bug fix: fix `string-width` error with empty file.

Version 0.5.1 (2013-01-28):

* Bug fix: creating files with <kbd>C-c C-n</kbd> when both the filter string and
  `deft-use-filename-as-title` are non-nil resulted in an invalid path.
* Bug fix: killed buffers would persist in `deft-auto-save-buffers`.

Version 0.5 (2013-01-25):

* Implement incremental string search (default) and regexp search.
  These search modes can be toggled by pressing <kbd>C-c C-t</kbd>.
* Default search method can be changed by setting `deft-incremental-search`.
* Support custom `deft-parse-title-function` for post-processing titles.
* The default `deft-parse-title-function` simply strips occurrences of
  `deft-strip-title-regexp`, which removes Markdown and Org headings.
* Open files in another window with <kbd>C-o</kbd>.  Prefix it with <kbd>C-u</kbd> to
  switch to the other window.
* For symbolic links, use modification time of taget for sorting.
* When opening files, move point to the end of the first match of
  the filter string.
* Improved filter editing: delete (`DEL`), delete word (<kbd>M-DEL</kbd>),
  and yank (<kbd>C-y</kbd>).
* Advanced filter editing in minibuffer (<kbd>C-c C-l</kbd>).

Version 0.4 (2011-12-11):

* Improved filtering performance.
* Optionally take title from filename instead of first line of the
  contents (see `deft-use-filename-as-title`).
* Dynamically resize width to fit the entire window.
* Customizable time format (see `deft-time-format`).
* Handle `deft-directory` properly with or without a trailing slash.

Version 0.3 (2011-09-11):

* Internationalization: support filtering with multibyte characters.

Version 0.2 (2011-08-22):

* Match filenames when filtering.
* Automatically save opened files (optional).
* Address some byte-compilation warnings.

Deft was originally written by [Jason Blevins](http://jblevins.org/).
The initial version, 0.1, was released on August 6, 2011.

