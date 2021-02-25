# Sepo's Expression Language

Sepo's expression language operates on one datatype, collections of tracks. These collections come in two varieties, lists (a.k.a. ordered) and multisets (a.k.a. unordered).

## Atoms

These are where you get collections of tracks.

- An empty track set - `empty`, `ε`, `ø`
- A playlist name - quoted or unquoted
	e.g. `Christmas`, `"All Time Low"`, `'Hey Monday'`
- A playlist ID - `spotify:playlist:4aDoQ5a6T5lYqJNegpdqLg`
	shorter variants: `p:`, `pl:`, `playlist:`
	or the old format namespaced under a user, e.g.: `spotify:user:CoderPuppy-Oreo:playlist:4aDoQ5a6T5lYqJNegpdqLg`
- A track ID - `spotify:track:6REVQZzxEOHBC4LXuREQXg`
	shorter variants: `t:`, `tr:`, `track:`
- An album ID - `spotify:album:1oZPpBr6pivIxf8OYZh0w4`
	shorter variants: `al:`, `album:`
- An artist ID - `spotify:artist:4UXqAaa6dQYAk18Lv7PEgX`
	shorter variants: `ar:`, `artist:`
- The currently playing "context" (playlist, album or artist) - `playing`, `current`, `this`
- The currently playing track - `playing_song`, `current_song`, `this_song`, `playing_track`, `current_track`, `this_track`
- All of the user's playlists - `my_playlists`, `playlists`
- An alias - quoted or unquoted
	e.g. `_stdin`, `_"Bowling for Soup"`
	TODO
- A file - anything starting with `/` or `./`, may quote any part of it except the initial `/` or `./`
	e.g. `./playlists/4aDoQ5a6T5lYqJNegpdqLg` or `/'proc/se'lf/f"d/0"`

## Operators

These let you change a collection of tracks (e.g. remove all duplicates) or combine them (e.g. union, intersection, or subtraction).

### Unary

- Convert to a set - `unique`, `uniq`, `u`
- Shuffle - `shuffle`, `shuf`, `s`
	This is not yet implemented.
- Expand - `expand`, `exp`, `e`
	TODO
- Sort by track name - `sort_track`, `sortTrack`, `sort_t`, `sort_tr`, `str`, `st`
- Sort by album name - `sort_album`, `sortAlbum`, `sort_al`, `sal`
- Sort by artist names - `sort_artist`, `sortArtist`, `sort_ar`, `sar`

Example: `sort_artist sort_album unique 'All Time Low'`

### Binary

- Intersection - `&&`, `&`, `∩`, `∧`
	all the tracks which are in both
- Union/Concatenation - `++`, `+`, `||`, `|`, `∪`, `∨`
	append two collections together
- Subtraction/Difference - `--`, `-`, `\\`, `\`
	remove one collection from another
- Sequencing - `*>`
	perform the left then the right and return the tracks from the right
	TODO
- Sequencing with reverse return - `<*`
	perform the left then the right and return the tracks from the left
	TODO

## Assignments

Some of the atoms may be assigned to: playlists, aliases, files, and the currently playing context. These are known as fields.
Note: There are restrictions on what may be assigned to the currently playing context, it must be a playlist, album or artist. A common pattern is assigning to a playlist just before assigning to `playing`, i.e. `playing = test1 = …`

These take the form of `<atom> = <expression>` like `test1 = 'All Time Low' + 'Hey Monday'`. They return what is assigned so can be chained as in `playing = test1 = 'All Time Low' + 'Hey Monday'` (it is right associative).

There are also add to (`<field> += <expr>`) and remove from (`<field> -= <expr>`) operators, these are syntax sugar for `<field> = <field> + <expr>` and `<field> = <field> - <expr>` respectively. Any of the symbols for these operators may be used `++`, `+`, `||`, `|`, `∪`, `∨` for add to and `--`, `-`, `\\`, `\` for remove from.

Also any of the binary operators may be prefixed with an exclamation point (`!`) to make them assign, i.e. `<field> !+ <expr>` is syntax sugar for `<field> = <field> + <expr>`. These are left associative and have a higher precedence than add to and remove from.

Any of the unary operators can be postfixed with an exclamation point to do the same thing or prefixed with it to make it a postfix. For example `unique! test1` and `test1 !unique` are both syntax sugar for `test1 = unique test1`.

All these operators can be chained in the field as well. For example, `(test1 = 'Hey Monday') = 'All Time Low'` would first assign `Christmas` to `test1` and then assign `'All Time Low'` to it. That is a rather useless example, but chaining the modifying operators this way can be useful. For example, `(test1 += 'Hey Monday') += 'All Time Low'` would add both `'Hey Monday'` and `'All Time Low'` to `test1`. Using the assigning variants of binary operators make this even cleaner: `test1 !+ 'Hey Monday' !+ 'All Time Low'`.

## Compounds

TODO
