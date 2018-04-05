# file-embed-lzma

The [file-embed](http://hackage.haskell.org/package/file-embed) package let's
embed file and dir contents.

This package is similar, but also compresses the embedded contents with LZMA.
That makes resulting object files smaller, at the cost of start up decompression.
