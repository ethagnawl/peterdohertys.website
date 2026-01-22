---
post_type: blog-post
published: true
published_on: 7/19/2019
title: Vim Abbreviations - The Lightning Talk that Didn't Strike
---

## Introduction
I'd originally put some notes together about Vim's abbreviations in preparation
for a lightning talk I was planning to give at [NYC Vim](https://www.meetup.com/The-New-York-Vim-Meetup/). Unfortunately, I was
unable to attend the meeting and didn't get a chance to present my findings.
I thought it'd be a shame to let my notes sit unused, so I decided to put a
post together which attempted to summarize them.

Abbreviations are a pretty simple concept and this post should be useful to
Vim users of all levels.

As usual, the Vim help entry for abbreviations (`:help abbreviations`) is
both approachable and informative -- I suggest [having a look](https://vimhelp.org/map.txt.html#abbreviations) after
reading this post.

I'm surprised by how infrequently I see abbreviations used in the wild, which
is one of the reasons I wanted to give this talk. In my opinion, they are very
useful and can save Vim users a significant number of keystrokes, typos and
headaches. The one potential downside I can think of is that there is a
temptation to get carried away (C macros come to mind ...) and create tens or
hundreds of unwieldy mappings which attempt to do _everything_. This leads to a
scenario where users come to depend on them and are significantly less
productive when using Vim without their mappings, spend more time trying to
remember what the mappings are than it would take to write the full form,
maintaining them, etc. You _can_ use Vimscript expressions and functions in the
RHS of mappings and do operations on the LHS, insert dynamic data, etc. but I
haven't pushed this beyond doing simple conditional logic.  If you need do find
a need to operate on your LHS (e.g.  interpolate it, duplicate it, wrap it in a
`do ... end` block, etc.) or inject dynamic values into your expansion, you may
want to consider [UltiSnips](https://github.com/sirver/UltiSnips). (Also see
Vimcast links under Resources.) UltiSnips is very powerful and can be used in
conjunction with YouCompleteMe to do contextual code/text generation and the
like. There are also a number of [repositories](https://github.com/honza/vim-snippets) of snippets which prevent
users from having to reinvent the wheel.

## Overview
Abbreviations are simply mappings which, when evaluated, substitute one string
(LHS) with another (RHS). (I'm hand waving a bit here; see "types of
abbreviations" in help entry for what constitutes a valid LHS) They can be
defined at the CLI (`:iabbrev foo bar`) or from within a vimrc
(`iabbrev foo bar`). Abbreviations are available in insert mode, replace mode
and command-line mode. `abbrev` will create an abbreviation for all modes and
using mode-specific prefixes (i, r and c) will restrict the abbreviation to
the given mode. Abbreviations in all modes support the `<buffer>` and `<expr>`
[special arguments](https://vimhelp.org/map.txt.html#%3Amap-arguments).

Vim does not ship with any default abbreviations. This can be verified by
starting Vim with an empty vimrc (e.g. `vim -u NONE`) and running
`:abbreviate`, which will list all of the current abbreviations.

The most common use case for abbreviations is mapping an _abbreviation_ to its
full form. For example:

- `iabbrev nasa National Aeronautics and Space Administration`

- `iabbrev gnu GNU's not Unix`

Another common use case for abbreviations is autocorrecting common spelling
mistakes. For example, you could create an abbreviation which will
automatically correct a common misspelling of "GNU/Linux":

`iabbrev Linux GNU/Linux`

Basic substitutions and correcting spelling mistakes are just the tip of the
iceberg, though: Abbreviations can also be used to generate code snippets and
complete programs! The simplest usage would be to define an abbreviation which
generates a common programming construct, like an `if` statement. This could be
done using:

`iabbrev if if () {}`

It's worth noting that abbreviations _are not_ recursive, so the name of the
abbreviation (LHS) can be used in the body of the expansion (RHS). This is
likely not a good idea, though, as you'll inevitably want to use the name of a
common construct or overloaded word, like "if", in your programs without it
being replaced. You can work around this issue either by keying `^C-v` before
triggering the abbreviation expansion or by using a modified construct name as
the abbreviation trigger. I like to use an `a` prefix when defining my
abbreviations in order to prevent me from having to remember to escape the
mapping. For example:

`iabbrev aif if () {}`

Now that we know how to create abbreviations which generate code, the next
step is to use `autocmd FileType` to conditionally define abbreviations;
`aif` should be expanded differently when editing a C program and a Ruby
program. (I'll also introduce newlines via `<CR>` to help make the output more
user friendly.)

```
autocmd FileType c iabbrev aif if (x) {<CR>} else if (y) {<CR>} else {<CR>}
autocmd FileType ruby iabbrev aif if x<CR>else<CR>end " this may conflict with vim-endwise
```

### Examples

#### Debuggers
```
autocmd FileType c iabbrev adebugger printf("%s\n", x);
autocmd FileType clojure,clojurescript iabbrev adebugger (prn 0)
autocmd FileType css,sass,scss,stylesheet iabbrev adebugger color: #bada55;
autocmd FileType elm iabbrev adebugger Debug.log (Debug.toString model)
autocmd FileType haskell iabbrev adebugger print $
autocmd FileType javascript,coffee iabbrev adebugger debugger
autocmd FileType ruby iabbrev adebugger byebug
```

#### Program skeletons (these quickly become unwieldy ...)
```
autocmd FileType c iabbrev askeleton #include <stdlib.h>
                                     \<CR>
                                     \#include <stdio.h>
                                     \<CR>
                                     \#include <stdbool.h>
                                     \<CR>
                                     \<CR>
                                     \int main() {
                                     \<CR>
                                     \  printf("k\n");
                                     \<CR>
                                     \  return 0;
                                     \<CR>
                                     \}

autocmd FileType rust iabbrev askeleton fn main() {
                                        \<CR>
                                        \  println!("hello world!");
                                        \<CR>
                                        \}

autocmd FileType sh iabbrev askeleton #!/usr/bin/env bash
                                      \<CR>
                                      \<CR>
                                      \set -euo pipefail
                                      \<CR>
                                      \IFS=$'\n\t
                                      \<CR>
```

#### Emoji
```
iabbrev amonad 🌯
iabbrev asweatysmile 😅
```

#### ... and finally, a command-line mode example!
```
" maps qA (results in "E492: Not an editor command") to qa (quit all)
cnoreabbrev <expr> qA ((getcmdtype() is# ':' && getcmdline() is# 'qA') ? ('qa') : ('qA'))
```

## Resources
- https://vimhelp.org/map.txt.html#abbreviations
- https://github.com/sirver/UltiSnips
- http://vimcasts.org/episodes/meet-ultisnips/
- https://github.com/ethagnawl/dotfiles/blob/70776ce/.vimrc#L657-L695
- https://github.com/honza/vim-snippets
