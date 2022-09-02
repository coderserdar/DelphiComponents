Unit zUnCompr;

{ uncompr.c -- decompress a memory buffer
  Copyright (C) 1995-1998 Jean-loup Gailly.

  Pascal tranlastion
  Copyright (C) 1998 by Jacques Nomssi Nzali
  For conditions of distribution and use, see copyright notice in readme.txt
}

interface

{$I zconf.inc}

uses
  zutil, zlib, zInflate;

{ ===========================================================================
     Decompresses the source buffer into the destination buffer.  sourceLen is
   the byte length of the source buffer. Upon entry, destLen is the total
   size of the destination buffer, which must be large enough to hold the
   entire uncompressed data. (The size of the uncompressed data must have
   been saved previously by the compressor and transmitted to the decompressor
   by some mechanism outside the scope of this compression library.)
   Upon exit, destLen is the actual size of the compressed buffer.
     This function can be used to decompress a whole file at once if the
   input file is mmap'ed.

     uncompress returns Z_OK if success, Z_MEM_ERROR if there was not
   enough memory, Z_BUF_ERROR if there was not enough room in the output
   buffer, or Z_DATA_ERROR if the input data was corrupted.
}

function uncompress (dest : pBytef;
                     var destLen : uLong;
                     const source : array of byte;
                     sourceLen : uLong) : int;

implementation

function uncompress (dest : pBytef;
                     var destLen : uLong;
                     const source : array of byte;
                     sourceLen : uLong) : int;
var
  stream : z_stream;
  err : int;
begin
  stream.next_in := pBytef(@source);
  stream.avail_in := uInt(sourceLen);
  { Check for source > 64K on 16-bit machine: }
  if (uLong(stream.avail_in) <> sourceLen) then
  begin
    uncompress := Z_BUF_ERROR;
    exit;
  end;

  stream.next_out := dest;
  stream.avail_out := uInt(destLen);
  if (uLong(stream.avail_out) <> destLen) then
  begin
    uncompress := Z_BUF_ERROR;
    exit;
  end;

  stream.zalloc := NIL;       { alloc_func(0); }
  stream.zfree := NIL;        { free_func(0); }

  err := inflateInit(stream);
  if (err <> Z_OK) then
  begin
    uncompress := err;
    exit;
  end;

  err := inflate(stream, Z_FINISH);
  if (err <> Z_STREAM_END) then
  begin
    inflateEnd(stream);
    if err = Z_OK then
      uncompress := Z_BUF_ERROR
    else
      uncompress := err;
    exit;
  end;
  destLen := stream.total_out;

  err := inflateEnd(stream);
  uncompress := err;
end;

end.