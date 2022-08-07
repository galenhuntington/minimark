##  Minimark

This is still mostly a personal project, or perhaps a work in progress
with minimal documentation.

Minimark is intended to be used with an HTML macro or templating
system, where tags can be defined to expand to other HTML.  It is
similar to Markdown, but defined more simply and far more flexibly.

Minimark translates some punctuation marks around simple text to tags.
E.g., `**bold**` expands to `<star2>bold</star2>`.  There is no
limit; one can have a `<caret5>`.  (Backslashes (`\`) can be used
for escaping.)  With macro expansion, these tags can then be given
any desired meaning.

Paragraphs are delineated with two newlines, and the `<para>` tag is
wrapped around them.

Remaining features are expanding two or three dashes to en- and
em-dashes, respectively, and tildes (`~`) to non-breaking spaces.
These were inspired by TeX.  I sometimes consider dropping these,
as good editors these days should provide effective Unicode support.

The predecessor to Minimark worked with text directly.  This version
operates on a document as defined with the `xmlhtml` package.
I have found this package adequate, and it is the basis of Heist,
which I often use for templating.

Normal HTML tags can be freely used, including Minimark-style ones.
E.g., a more complex piece of a document can be explicitly surrounded
with `<percent3>`, with the same effect one would get (for simple
content) by wrapping in `%%%`.  The latter is thus effectively
a shortcut.

The previous incarnation included a list item syntax, but I have not
ported it yet, and may not.

