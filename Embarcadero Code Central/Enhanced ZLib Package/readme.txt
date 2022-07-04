Enhanced zlib implementation
Gabriel Corneanu <gabrielcorneanu(AT)yahoo.com>

Key features:
Using last zlib library (1.2.3).

Moved all basic imported functions in a standalone unit, zlibpas.
A lot of other popular projects use zlib (pngimage, graphicEx) and they include 
their own version of the library. That means 2 or 3 instances of the C library 
included in an application! I hope one day this will be ended.

The compression stream can create different type of streams:
zlib, gzip and raw deflate (see constructors).

The decompression stream can read all type of streams (autodetect),
plus that the stream type and gzip info is available for public access.
If the stream is not zlib or gzip, it is assumed raw. An error will
occur during decompressing if the data format is not valid.

The DecompressStream function is using the InflateBack call together with direct
memory access on the source stream (if available, which means TStringStream or 
TCustomMemoryStream descendant).
It should be the fastest decompression routine!

The CompressStreamEx function is using direct memory access on both source and 
destination stream (if available).
It should be faster than CompressStream.

CompressString or CompressStream can be used to compress a http response.
