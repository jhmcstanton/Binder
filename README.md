# Binder

Binder is a tool that compiles notes into a single file.

## Current State

Binder currently compiles all markdown files with extension `.note` from the current directory and all subdirectories
into a single HTML file. In commit 8de0fb2 the library switched from the [http://www.haskell.org/hackage/package/markdown](markdown)
package to [http://www.haskell.org/hackage/package/pandoc](pandoc) to better support tables and math. 

The HTML output is sorted by directories and last modified date (directories are sorted by last modified date as well). 

## Next Steps

Since pandoc is now used, the meta data in pandocs should be leveraged to update sorting (for example, by creation date rather
than last modification date).

Support for diagrams, ala [http://www.haskell.org/hackage/package/diagrams](diagrams), is desirable, possibly using 
[http://www.haskell.org/hackage/package/pandoc-diagrams](pandoc-diagrams).

Additional tooling is to be added for a yaml config file to include things such as additional styles to include, target
directory, etc.