(*
 * This file is part of FFmpeg.
 *
 * FFmpeg is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * FFmpeg is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with FFmpeg; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 *)

(**
 * @file
 * a very simple circular buffer FIFO implementation
 *)

(*
 * FFVCL - Delphi FFmpeg VCL Components
 * http://www.DelphiFFmpeg.com
 *
 * Original file: libavutil/fifo.h
 * Ported by CodeCoolie@CNSW 2008/03/25 -> $Date:: 2019-08-12 #$
 *)

(*
FFmpeg Delphi/Pascal Headers and Examples License Agreement

A modified part of FFVCL - Delphi FFmpeg VCL Components.
Copyright (c) 2008-2022 DelphiFFmpeg.com
All rights reserved.
http://www.DelphiFFmpeg.com

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:
1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.

This source code is provided "as is" by DelphiFFmpeg.com without
warranty of any kind, either expressed or implied, including but not
limited to the implied warranties of merchantability and/or fitness
for a particular purpose.

Please also notice the License agreement of FFmpeg libraries.
*)

unit libavutil_fifo;

interface

{$I CompilerDefines.inc}

uses
  FFTypes;

{$I libversion.inc}

type
  PPAVFifoBuffer = ^PAVFifoBuffer;
  PAVFifoBuffer = ^TAVFifoBuffer;
  TAVFifoBuffer = record
    buffer: PByte;
    rptr, wptr, eend: PByte;
    rndx, wndx: Cardinal;
  end;

  TfifoCall = procedure(v1, v2: Pointer; i: Integer); cdecl;
  TwriteCall = function(p1, p2: Pointer; i: Integer): Integer; cdecl;

(**
 * Initialize an AVFifoBuffer.
 * @param size of FIFO
 * @return AVFifoBuffer or NULL in case of memory allocation failure
 *)
function av_fifo_alloc(size: Cardinal): PAVFifoBuffer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_fifo_alloc';

(**
 * Initialize an AVFifoBuffer.
 * @param nmemb number of elements
 * @param size  size of the single element
 * @return AVFifoBuffer or NULL in case of memory allocation failure
 *)
function av_fifo_alloc_array(nmemb, size: Size_t): PAVFifoBuffer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_fifo_alloc_array';

(**
 * Free an AVFifoBuffer.
 * @param f AVFifoBuffer to free
 *)
procedure av_fifo_free(f: PAVFifoBuffer); cdecl; external AVUTIL_LIBNAME name _PU + 'av_fifo_free';

(**
 * Free an AVFifoBuffer and reset pointer to NULL.
 * @param f AVFifoBuffer to free
 *)
procedure av_fifo_freep(f: PPAVFifoBuffer); cdecl; external AVUTIL_LIBNAME name _PU + 'av_fifo_freep';

(**
 * Reset the AVFifoBuffer to the state right after av_fifo_alloc, in particular it is emptied.
 * @param f AVFifoBuffer to reset
 *)
procedure av_fifo_reset(f: PAVFifoBuffer); cdecl; external AVUTIL_LIBNAME name _PU + 'av_fifo_reset';

(**
 * Return the amount of data in bytes in the AVFifoBuffer, that is the
 * amount of data you can read from it.
 * @param f AVFifoBuffer to read from
 * @return size
 *)
function av_fifo_size(const f: PAVFifoBuffer): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_fifo_size';

(**
 * Return the amount of space in bytes in the AVFifoBuffer, that is the
 * amount of data you can write into it.
 * @param f AVFifoBuffer to write into
 * @return size
 *)
function av_fifo_space(const f: PAVFifoBuffer): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_fifo_space';

(**
 * Feed data at specific position from an AVFifoBuffer to a user-supplied callback.
 * Similar as av_fifo_gereric_read but without discarding data.
 * @param f AVFifoBuffer to read from
 * @param offset offset from current read position
 * @param buf_size number of bytes to read
 * @param func generic read function
 * @param dest data destination
 *)
type
  TfifopeekCall = procedure(a, b: Pointer; c: Integer); cdecl;
function av_fifo_generic_peek_at(f: PAVFifoBuffer; dest: Pointer; offset, buf_size: Integer; func: TfifopeekCall): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_fifo_generic_peek_at';

(**
 * Feed data from an AVFifoBuffer to a user-supplied callback.
 * Similar as av_fifo_gereric_read but without discarding data.
 * @param f AVFifoBuffer to read from
 * @param buf_size number of bytes to read
 * @param func generic read function
 * @param dest data destination
 *)
function av_fifo_generic_peek(f: PAVFifoBuffer; dest: Pointer; buf_size: Integer; func: TfifoCall): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_fifo_generic_peek';

(**
 * Feed data from an AVFifoBuffer to a user-supplied callback.
 * @param f AVFifoBuffer to read from
 * @param buf_size number of bytes to read
 * @param func generic read function
 * @param dest data destination
 *)
function av_fifo_generic_read(f: PAVFifoBuffer; dest: Pointer; buf_size: Integer; func: TfifoCall): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_fifo_generic_read';

(**
 * Feed data from a user-supplied callback to an AVFifoBuffer.
 * @param f AVFifoBuffer to write to
 * @param src data source; non-const since it may be used as a
 * modifiable context by the function defined in func
 * @param size number of bytes to write
 * @param func generic write function; the first parameter is src,
 * the second is dest_buf, the third is dest_buf_size.
 * func must return the number of bytes written to dest_buf, or <= 0 to
 * indicate no more data available to write.
 * If func is NULL, src is interpreted as a simple byte array for source data.
 * @return the number of bytes written to the FIFO
 *)
function av_fifo_generic_write(f: PAVFifoBuffer; src: Pointer; size: Integer; func: TwriteCall): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_fifo_generic_write';

(**
 * Resize an AVFifoBuffer.
 * In case of reallocation failure, the old FIFO is kept unchanged.
 *
 * @param f AVFifoBuffer to resize
 * @param size new AVFifoBuffer size in bytes
 * @return <0 for failure, >=0 otherwise
 *)
function av_fifo_realloc2(f: PAVFifoBuffer; size: Cardinal): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_fifo_realloc2';

(**
 * Enlarge an AVFifoBuffer.
 * In case of reallocation failure, the old FIFO is kept unchanged.
 * The new fifo size may be larger than the requested size.
 *
 * @param f AVFifoBuffer to resize
 * @param additional_space the amount of space in bytes to allocate in addition to av_fifo_size()
 * @return <0 for failure, >=0 otherwise
 *)
function av_fifo_grow(f: PAVFifoBuffer; additional_space: Cardinal): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_fifo_grow';

(**
 * Read and discard the specified amount of data from an AVFifoBuffer.
 * @param f AVFifoBuffer to read from
 * @param size amount of data to read in bytes
 *)
procedure av_fifo_drain(f: PAVFifoBuffer; size: Integer); cdecl; external AVUTIL_LIBNAME name _PU + 'av_fifo_drain';

(**
 * Return a pointer to the data stored in a FIFO buffer at a certain offset.
 * The FIFO buffer is not modified.
 *
 * @param f    AVFifoBuffer to peek at, f must be non-NULL
 * @param offs an offset in bytes, its absolute value must be less
 *             than the used buffer size or the returned pointer will
 *             point outside to the buffer data.
 *             The used buffer size can be checked with av_fifo_size().
 *)
function av_fifo_peek2(const f: PAVFifoBuffer; offs: Integer): PByte; {$IFDEF USE_INLINE}inline;{$ENDIF}

implementation

function av_fifo_peek2(const f: PAVFifoBuffer; offs: Integer): PByte;
var
  ptr: PByte;
begin
  ptr := f^.rptr;
  Inc(ptr, offs);
{$IFNDEF FPC}
  {$IF CompilerVersion <= 20.0} // Delphi 2009 and and olders
    {$DEFINE NO_NATIVE_UINT}
  {$IFEND}
{$ENDIF}
{$IFDEF NO_NATIVE_UINT}
  if Cardinal(ptr) >= Cardinal(f^.eend) then
    Dec(ptr, Cardinal(f^.eend) - Cardinal(f^.buffer))
  else if Cardinal(ptr) < Cardinal(f^.buffer) then
    Inc(ptr, Cardinal(f^.eend) - Cardinal(f^.buffer));
{$ELSE}
  if NativeUInt(ptr) >= NativeUInt(f^.eend) then
    Dec(ptr, NativeUInt(f^.eend) - NativeUInt(f^.buffer))
  else if NativeUInt(ptr) < NativeUInt(f^.buffer) then
    Inc(ptr, NativeUInt(f^.eend) - NativeUInt(f^.buffer));
{$ENDIF}
  Result := ptr;
end;

end.
