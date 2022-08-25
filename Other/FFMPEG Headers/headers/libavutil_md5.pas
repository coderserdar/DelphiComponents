(*
 * copyright (c) 2006 Michael Niedermayer <michaelni@gmx.at>
 *
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
 * @ingroup lavu_md5
 * Public header for MD5 hash function implementation.
 *)

(*
 * FFVCL - Delphi FFmpeg VCL Components
 * http://www.DelphiFFmpeg.com
 *
 * Original file: libavutil/md5.h
 * Ported by CodeCoolie@CNSW 2014/08/27 -> $Date:: 2019-05-29 #$
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

unit libavutil_md5;

interface

{$I CompilerDefines.inc}

uses
  FFTypes;

{$I libversion.inc}

(**
 * @defgroup lavu_md5 MD5
 * @ingroup lavu_hash
 * MD5 hash function implementation.
 *
 * @{
 *)

//extern const int av_md5_size;

type
  PAVMD5 = ^TAVMD5;
  TAVMD5 = record
    // need {$ALIGN 8}
    // defined in libavutil/md5.c
  end;

(**
 * Allocate an AVMD5 context.
 *)
function av_md5_alloc(): PAVMD5; cdecl; external AVUTIL_LIBNAME name _PU + 'av_md5_alloc';

(**
 * Initialize MD5 hashing.
 *
 * @param ctx pointer to the function context (of size av_md5_size)
 *)
procedure av_md5_init(ctx: PAVMD5); cdecl; external AVUTIL_LIBNAME name _PU + 'av_md5_init';

(**
 * Update hash value.
 *
 * @param ctx hash function context
 * @param src input data to update hash with
 * @param len input data length
 *)
{$IFDEF FF_API_CRYPTO_SIZE_T}
procedure av_md5_update(ctx: PAVMD5; const src: PByte; len: Integer); cdecl; external AVUTIL_LIBNAME name _PU + 'av_md5_update';
{$ELSE}
procedure av_md5_update(ctx: PAVMD5; const src: PByte; len: Size_t); cdecl; external AVUTIL_LIBNAME name _PU + 'av_md5_update';
{$ENDIF}

(**
 * Finish hashing and output digest value.
 *
 * @param ctx hash function context
 * @param dst buffer where output digest value is stored
 *)
procedure av_md5_final(ctx: PAVMD5; dst: PByte); cdecl; external AVUTIL_LIBNAME name _PU + 'av_md5_final';

(**
 * Hash an array of data.
 *
 * @param dst The output buffer to write the digest into
 * @param src The data to hash
 * @param len The length of the data, in bytes
 *)
{$IFDEF FF_API_CRYPTO_SIZE_T}
procedure av_md5_sum(dst: PByte; const src: PByte; const len: Integer); cdecl; external AVUTIL_LIBNAME name _PU + 'av_md5_sum';
{$ELSE}
procedure av_md5_sum(dst: PByte; const src: PByte; len: Size_t); cdecl; external AVUTIL_LIBNAME name _PU + 'av_md5_sum';
{$ENDIF}

(**
 * @}
 *)

implementation

end.
