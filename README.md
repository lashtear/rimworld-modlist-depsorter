# Rimworld Mod-list Dependency Sorter

This tool looks at what mods are installed (either manually, in `SteamApps/common/RimWorld/Mods` or via Steam workshop in `SteamApps/workshop/content/294100`) and [topologically sorts](https://en.wikipedia.org/wiki/Topological_sorting) them based on supplied [dependency rules](rules.txt).

As mods come and go and the core game updates, keeping them automatically sorted in a working order is increasingly difficult.  In most cases, updating the game also disables all mods-- losing the very ordering that took so long to get right in the first place.  I found this intensely frustrating, as other games (e.g. Factorio) have their own hard and soft dependency system that automatically sorts the list.  So here's a start of what that might look like in Rimworld, implemented as an external tool.

## Rules-grammar

* Comments are line-based and start with `#`.
* Rules are subject-verb-object and end with a period (`.`).
* Subject and object describe what mods might match.
* Mods are identified by normalized name, and can be directly string-matched, or prefix/suffix/contains-matched.
* Strings are double-quoted.
* Subsets of mods can be composed via and/or/not clauses.
* Long lists of and/or can be composed with commas and enclosed within parentheses.
* Verb clauses may describe hard dependencies or soft dependencies.

## Usage

```
$ rimworld-modlist-depsorter rules.txt
```

### Installation

This project should be buildable with the Cabal packaging system for Haskell.

## To-Do

* Better error handling/reporting.
* Re-enable graphviz output.
* Automatic dependency discovery (e.g. for HugsLib) by examining patch XMLs and `.dll` dependencies.
* Support for versioned dependencies and associated (much more complex) solver.

