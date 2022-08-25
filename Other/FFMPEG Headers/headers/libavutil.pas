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
 * @ingroup lavu
 * Convenience header that includes @ref lavu "libavutil"'s core.
 *)

(*
 * FFVCL - Delphi FFmpeg VCL Components
 * http://www.DelphiFFmpeg.com
 *
 * Original file: libavutil/avutil.h
 * Ported by CodeCoolie@CNSW 2008/03/18 -> $Date:: 2018-05-22 #$
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

unit libavutil;

interface

{$I CompilerDefines.inc}

uses
  libavutil_rational;

{$I libversion.inc}

(**
 * @mainpage
 *
 * @section ffmpeg_intro Introduction
 *
 * This document describes the usage of the different libraries
 * provided by FFmpeg.
 *
 * @li @ref libavc "libavcodec" encoding/decoding library
 * @li @ref lavfi "libavfilter" graph-based frame editing library
 * @li @ref libavf "libavformat" I/O and muxing/demuxing library
 * @li @ref lavd "libavdevice" special devices muxing/demuxing library
 * @li @ref lavu "libavutil" common utility library
 * @li @ref lswr "libswresample" audio resampling, format conversion and mixing
 * @li @ref lpp  "libpostproc" post processing library
 * @li @ref libsws "libswscale" color conversion and scaling library
 *
 * @section ffmpeg_versioning Versioning and compatibility
 *
 * Each of the FFmpeg libraries contains a version.h header, which defines a
 * major, minor and micro version number with the
 * <em>LIBRARYNAME_VERSION_{MAJOR,MINOR,MICRO}</em> macros. The major version
 * number is incremented with backward incompatible changes - e.g. removing
 * parts of the public API, reordering public struct members, etc. The minor
 * version number is incremented for backward compatible API changes or major
 * new features - e.g. adding a new public function or a new decoder. The micro
 * version number is incremented for smaller changes that a calling program
 * might still want to check for - e.g. changing behavior in a previously
 * unspecified situation.
 *
 * FFmpeg guarantees backward API and ABI compatibility for each library as long
 * as its major version number is unchanged. This means that no public symbols
 * will be removed or renamed. Types and names of the public struct members and
 * values of public macros and enums will remain the same (unless they were
 * explicitly declared as not part of the public API). Documented behavior will
 * not change.
 *
 * In other words, any correct program that works with a given FFmpeg snapshot
 * should work just as well without any changes with any later snapshot with the
 * same major versions. This applies to both rebuilding the program against new
 * FFmpeg versions or to replacing the dynamic FFmpeg libraries that a program
 * links against.
 *
 * However, new public symbols may be added and new members may be appended to
 * public structs whose size is not part of public ABI (most public structs in
 * FFmpeg). New macros and enum values may be added. Behavior in undocumented
 * situations may change slightly (and be documented). All those are accompanied
 * by an entry in doc/APIchanges and incrementing either the minor or micro
 * version number.
 *)

(**
 * @defgroup lavu libavutil
 * Common code shared across all FFmpeg libraries.
 *
 * @note
 * libavutil is designed to be modular. In most cases, in order to use the
 * functions provided by one component of libavutil you must explicitly include
 * the specific header containing that feature. If you are only using
 * media-related components, you could simply include libavutil/avutil.h, which
 * brings in most of the "core" components.
 *
 * @{
 *
 * @defgroup lavu_crypto Crypto and Hashing
 *
 * @{
 * @}
 *
 * @defgroup lavu_math Mathematics
 * @{
 *
 * @}
 *
 * @defgroup lavu_string String Manipulation
 *
 * @{
 *
 * @}
 *
 * @defgroup lavu_mem Memory Management
 *
 * @{
 *
 * @}
 *
 * @defgroup lavu_data Data Structures
 * @{
 *
 * @}
 *
 * @defgroup lavu_video Video related
 *
 * @{
 *
 * @}
 *
 * @defgroup lavu_audio Audio related
 *
 * @{
 *
 * @}
 *
 * @defgroup lavu_error Error Codes
 *
 * @{
 *
 * @}
 *
 * @defgroup lavu_log Logging Facility
 *
 * @{
 *
 * @}
 *
 * @defgroup lavu_misc Other
 *
 * @{
 *
 * @defgroup preproc_misc Preprocessor String Macros
 *
 * @{
 *
 * @}
 *
 * @defgroup version_utils Library Version Macros
 *
 * @{
 *
 * @}
 *)


(**
 * @addtogroup lavu_ver
 * @{
 *)

(**
 * Return the LIBAVUTIL_VERSION_INT constant.
 *)
function avutil_version: Cardinal; cdecl; external AVUTIL_LIBNAME name _PU + 'avutil_version';

(**
 * Return an informative version string. This usually is the actual release
 * version number or a git commit description. This string has no fixed format
 * and can change any time. It should never be parsed by code.
 *)
function av_version_info: PAnsiChar; cdecl; external AVUTIL_LIBNAME name _PU + 'av_version_info';

(**
 * Return the libavutil build-time configuration.
 *)
function avutil_configuration: PAnsiChar; cdecl; external AVUTIL_LIBNAME name _PU + 'avutil_configuration';

(**
 * Return the libavutil license.
 *)
function avutil_license: PAnsiChar; cdecl; external AVUTIL_LIBNAME name _PU + 'avutil_license';

(**
 * @}
 *)

(**
 * @addtogroup lavu_media Media Type
 * @brief Media Type
 *)

type
  TAVMediaType = (
    AVMEDIA_TYPE_UNKNOWN = -1,  ///< Usually treated as AVMEDIA_TYPE_DATA
    AVMEDIA_TYPE_VIDEO,
    AVMEDIA_TYPE_AUDIO,
    AVMEDIA_TYPE_DATA,          ///< Opaque data information usually continuous
    AVMEDIA_TYPE_SUBTITLE,
    AVMEDIA_TYPE_ATTACHMENT,    ///< Opaque data information usually sparse
    AVMEDIA_TYPE_NB
  );

(**
 * Return a string describing the media_type enum, NULL if media_type
 * is unknown.
 *)
function av_get_media_type_string(media_type: TAVMediaType): PAnsiChar; cdecl; external AVUTIL_LIBNAME name _PU + 'av_get_media_type_string';

(**
 * @defgroup lavu_const Constants
 * @{
 *
 * @defgroup lavu_enc Encoding specific
 *
 * @note those definition should move to avcodec
 * @{
 *)

const
  // AVCodecContext.global_quality;
  FF_LAMBDA_SHIFT = 7;
  FF_LAMBDA_SCALE = (1 shl FF_LAMBDA_SHIFT);
  FF_QP2LAMBDA = 118; ///< factor to convert from H.263 QP to lambda
  FF_LAMBDA_MAX = (256*128-1);

  FF_QUALITY_SCALE = FF_LAMBDA_SCALE; //FIXME maybe remove

(**
 * @}
 * @defgroup lavu_time Timestamp specific
 *
 * FFmpeg internal timebase and timestamp definitions
 *
 * @{
 *)

(**
 * @brief Undefined timestamp value
 *
 * Usually reported by demuxer that work on containers that do not provide
 * either pts or dts.
 *)

  AV_NOPTS_VALUE: Int64    = Int64($8000000000000000);

(**
 * Internal time base represented as integer
 *)

  AV_TIME_BASE_I           = 1000000;
  AV_TIME_BASE: Int64      = AV_TIME_BASE_I;

(**
 * Internal time base represented as fractional value
 *)

  AV_TIME_BASE_Q: TAVRational = (num: 1; den: AV_TIME_BASE_I);
  AV_TIME_BASE_SUB: TAVRational = (num: 1; den: 1000);

(**
 * @}
 * @}
 * @defgroup lavu_picture Image related
 *
 * AVPicture types, pixel formats and basic image planes manipulation.
 *
 * @{
 *)

type
  TAVPictureType = (
    AV_PICTURE_TYPE_NONE = 0, ///< Undefined
    AV_PICTURE_TYPE_I,     ///< Intra
    AV_PICTURE_TYPE_P,     ///< Predicted
    AV_PICTURE_TYPE_B,     ///< Bi-dir predicted
    AV_PICTURE_TYPE_S,     ///< S(GMC)-VOP MPEG-4
    AV_PICTURE_TYPE_SI,    ///< Switching Intra
    AV_PICTURE_TYPE_SP,    ///< Switching Predicted
    AV_PICTURE_TYPE_BI     ///< BI type
  );

(**
 * Return a single letter to describe the given picture type
 * pict_type.
 *
 * @param[in] pict_type the picture type @return a single character
 * representing the picture type, '?' if pict_type is unknown
 *)
function av_get_picture_type_char(pict_type: TAVPictureType): AnsiChar; cdecl; external AVUTIL_LIBNAME name _PU + 'av_get_picture_type_char';

(**
 * @}
 *)

(**
 * Return x default pointer in case p is NULL.
 *)
//static inline void *av_x_if_null(const void *p, const void *x)
{
    return (void *)(intptr_t)(p ? p : x);
}

(**
 * Compute the length of an integer list.
 *
 * @param elsize  size in bytes of each list element (only 1, 2, 4 or 8)
 * @param term    list terminator (usually 0 or -1)
 * @param list    pointer to the list
 * @return  length of the list, in elements, not counting the terminator
 *)
function av_int_list_length_for_size(elsize: Cardinal;
                                     const list: Pointer; term: Int64): Cardinal; cdecl; external AVUTIL_LIBNAME name _PU + 'av_int_list_length_for_size';

(**
 * Compute the length of an integer list.
 *
 * @param term  list terminator (usually 0 or -1)
 * @param list  pointer to the list
 * @return  length of the list, in elements, not counting the terminator
 *)
//#define av_int_list_length(list, term) \
//    av_int_list_length_for_size(sizeof(*(list)), list, term)
function av_int_list_length(list: Pointer; item_size: Integer; term: Int64): Integer; {$IFDEF USE_INLINE}inline;{$ENDIF}

(**
 * Open a file using a UTF-8 filename.
 * The API of this function matches POSIX fopen(), errors are returned through
 * errno.
 *)
function av_fopen_utf8(const path, mode: PAnsiChar): Pointer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_fopen_utf8';

(**
 * Fill the provided buffer with a string containing a FourCC (four-character
 * code) representation.
 *
 * @param buf    a buffer with size in bytes of at least AV_FOURCC_MAX_STRING_SIZE
 * @param fourcc the fourcc to represent
 * @return the buffer in input
 *)
function av_fourcc_make_string(buf: PAnsiChar; fourcc: Cardinal): PAnsiChar; cdecl; external AVUTIL_LIBNAME name _PU + 'av_fourcc_make_string';

(**
 * Return the fractional representation of the internal time base.
 *)
function av_get_time_base_q: TAVRational; {$IFDEF USE_INLINE}inline;{$ENDIF}

const
  AV_FOURCC_MAX_STRING_SIZE = 32;

//#define av_fourcc2str(fourcc) av_fourcc_make_string((char[AV_FOURCC_MAX_STRING_SIZE]){0}, fourcc)

(**
 * Return x default pointer in case p is NULL.
 *)
function av_x_if_null(const p, x: PAnsiChar): PAnsiChar; {$IFDEF USE_INLINE}inline;{$ENDIF}

implementation


function av_int_list_length(list: Pointer; item_size: Integer; term: Int64): Integer;
begin
  Result := av_int_list_length_for_size(item_size, list, term);
end;

function av_get_time_base_q: TAVRational;
begin
  Result.num := 1;
  Result.den := AV_TIME_BASE;
end;

function av_x_if_null(const p, x: PAnsiChar): PAnsiChar;
begin
  if Assigned(p) then
    Result := p
  else
    Result := x;
end;

end.
