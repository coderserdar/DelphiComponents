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

(*
 * FFVCL - Delphi FFmpeg VCL Components
 * http://www.DelphiFFmpeg.com
 *
 * Original file: libavutil/log.h
 * Ported by CodeCoolie@CNSW 2008/03/18 -> $Date:: 2021-04-25 #$
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

unit libavutil_log;

interface

{$I CompilerDefines.inc}

uses
  libavutil_opt;

{$I libversion.inc}

type
  TAVClassCategory = (
    AV_CLASS_CATEGORY_NA = 0,
    AV_CLASS_CATEGORY_INPUT,
    AV_CLASS_CATEGORY_OUTPUT,
    AV_CLASS_CATEGORY_MUXER,
    AV_CLASS_CATEGORY_DEMUXER,
    AV_CLASS_CATEGORY_ENCODER,
    AV_CLASS_CATEGORY_DECODER,
    AV_CLASS_CATEGORY_FILTER,
    AV_CLASS_CATEGORY_BITSTREAM_FILTER,
    AV_CLASS_CATEGORY_SWSCALER,
    AV_CLASS_CATEGORY_SWRESAMPLER,
    AV_CLASS_CATEGORY_DEVICE_VIDEO_OUTPUT = 40,
    AV_CLASS_CATEGORY_DEVICE_VIDEO_INPUT,
    AV_CLASS_CATEGORY_DEVICE_AUDIO_OUTPUT,
    AV_CLASS_CATEGORY_DEVICE_AUDIO_INPUT,
    AV_CLASS_CATEGORY_DEVICE_OUTPUT,
    AV_CLASS_CATEGORY_DEVICE_INPUT,
    AV_CLASS_CATEGORY_NB  ///< not part of ABI/API
  );

{
#define AV_IS_INPUT_DEVICE(category) \
    (((category) == AV_CLASS_CATEGORY_DEVICE_VIDEO_INPUT) || \
     ((category) == AV_CLASS_CATEGORY_DEVICE_AUDIO_INPUT) || \
     ((category) == AV_CLASS_CATEGORY_DEVICE_INPUT))

#define AV_IS_OUTPUT_DEVICE(category) \
    (((category) == AV_CLASS_CATEGORY_DEVICE_VIDEO_OUTPUT) || \
     ((category) == AV_CLASS_CATEGORY_DEVICE_AUDIO_OUTPUT) || \
     ((category) == AV_CLASS_CATEGORY_DEVICE_OUTPUT))
}

(*
  PPAVOptionRanges = ^PAVOptionRanges;
  PAVOptionRanges = ^TAVOptionRanges;
  TAVOptionRanges = record
    // need {$ALIGN 8}
    // defined in libavutil/opt.h
  end;
*)

(**
 * Describe the class of an AVClass context structure. That is an
 * arbitrary struct of which the first field is a pointer to an
 * AVClass struct (e.g. AVCodecContext, AVFormatContext etc.).
 *)
  PPPAVClass = ^PPAVClass;
  PPAVClass = ^PAVClass;
  PAVClass = ^TAVClass;
  TAVClass = record
    (**
     * The name of the class; usually it is the same name as the
     * context structure type to which the AVClass is associated.
     *)
    class_name: PAnsiChar;

    (**
     * A pointer to a function which returns the name of a context
     * instance ctx associated with the class.
     *)
    item_name: function(ctx: Pointer): PAnsiChar; cdecl;

    (**
     * a pointer to the first option specified in the class if any or NULL
     *
     * @see av_set_default_options()
     *)
    option: PAVOption;

    (**
     * LIBAVUTIL_VERSION with which this structure was created.
     * This is used to allow fields to be added without requiring major
     * version bumps everywhere.
     *)
    version: Integer;

    (**
     * Offset in the structure where log_level_offset is stored.
     * 0 means there is no such variable
     *)
    log_level_offset_offset: Integer;

    (**
     * Offset in the structure where a pointer to the parent context for
     * logging is stored. For example a decoder could pass its AVCodecContext
     * to eval as such a parent context, which an av_log() implementation
     * could then leverage to display the parent context.
     * The offset can be NULL.
     *)
    parent_log_context_offset: Integer;

    (**
     * Return next AVOptions-enabled child or NULL
     *)
    child_next: function(obj, prev: Pointer): Pointer; cdecl;

{$IFDEF FF_API_CHILD_CLASS_NEXT}
    (**
     * Return an AVClass corresponding to the next potential
     * AVOptions-enabled child.
     *
     * The difference between child_next and this is that
     * child_next iterates over _already existing_ objects, while
     * child_class_next iterates over _all possible_ children.
     *)
    child_class_next: function(const prev: PAVClass): PAVClass; cdecl;
{$ENDIF}

    (**
     * Category used for visualization (like color)
     * This is only set if the category is equal for all objects using this class.
     * available since version (51 << 16 | 56 << 8 | 100)
     *)
    category: TAVClassCategory;

    (**
     * Callback to return the category.
     * available since version (51 << 16 | 59 << 8 | 100)
     *)
    get_category: function(ctx: Pointer): TAVClassCategory; cdecl;

    (**
     * Callback to return the supported/allowed ranges.
     * available since version (52.12)
     *)
    query_ranges: function(ranges: PPAVOptionRanges; obj: Pointer; const key: PAnsiChar; flags: Integer): Integer; cdecl;

    (**
     * Iterate over the AVClasses corresponding to potential AVOptions-enabled
     * children.
     *
     * @param iter pointer to opaque iteration state. The caller must initialize
     *             *iter to NULL before the first call.
     * @return AVClass for the next AVOptions-enabled child or NULL if there are
     *         no more such children.
     *
     * @note The difference between child_next and this is that child_next
     *       iterates over _already existing_ objects, while child_class_iterate
     *       iterates over _all possible_ children.
     *)
    child_class_iterate: function(iter: PPointer): PAVClass; cdecl;
  end;

(**
 * @addtogroup lavu_log
 *
 * @{
 *
 * @defgroup lavu_log_constants Logging Constants
 *
 * @{
 *)

(**
 * Print no output.
 *)
const
  AV_LOG_QUIET    = -8;

(**
 * Something went really wrong and we will crash now.
 *)
  AV_LOG_PANIC    = 0;

(**
 * Something went wrong and recovery is not possible.
 * For example, no header was found for a format which depends
 * on headers or an illegal combination of parameters is used.
 *)
  AV_LOG_FATAL    = 8;

(**
 * Something went wrong and cannot losslessly be recovered.
 * However, not all future data is affected.
 *)
  AV_LOG_ERROR    = 16;

(**
 * Something somehow does not look correct. This may or may not
 * lead to problems. An example would be the use of '-vstrict -2'.
 *)
  AV_LOG_WARNING  = 24;

(**
 * Standard information.
 *)
  AV_LOG_INFO     = 32;

(**
 * Detailed information.
 *)
  AV_LOG_VERBOSE  = 40;

(**
 * Stuff which is only useful for libav* developers.
 *)
  AV_LOG_DEBUG    = 48;

(**
 * Extremely verbose debugging, useful for libav* development.
 *)
  AV_LOG_TRACE    = 56;

  AV_LOG_MAX_OFFSET = (AV_LOG_TRACE - AV_LOG_QUIET);

(**
 * @}
 *)

(**
 * Sets additional colors for extended debugging sessions.
 * @code
   av_log(ctx, AV_LOG_DEBUG|AV_LOG_C(134), "Message in purple\n");
   @endcode
 * Requires 256color terminal support. Uses outside debugging is not
 * recommended.
 *)
//#define AV_LOG_C(x) ((x) << 8)
function AV_LOG_C(x: Integer): Integer; {$IFDEF USE_INLINE}inline;{$ENDIF}

(**
 * Send the specified message to the log if the level is less than or equal
 * to the current av_log_level. By default, all logging messages are sent to
 * stderr. This behavior can be altered by setting a different logging callback
 * function.
 * @see av_log_set_callback
 *
 * @param avcl A pointer to an arbitrary struct of which the first field is a
 *        pointer to an AVClass struct or NULL if general log.
 * @param level The importance level of the message expressed using a @ref
 *        lavu_log_constants "Logging Constant".
 * @param fmt The format string (printf-compatible) that specifies how
 *        subsequent arguments are converted to output.
 *)
procedure av_log(avcl: Pointer; level: Integer; const fmt: PAnsiChar); cdecl varargs; external AVUTIL_LIBNAME name _PU + 'av_log';

(**
 * Send the specified message to the log once with the initial_level and then with
 * the subsequent_level. By default, all logging messages are sent to
 * stderr. This behavior can be altered by setting a different logging callback
 * function.
 * @see av_log
 *
 * @param avcl A pointer to an arbitrary struct of which the first field is a
 *        pointer to an AVClass struct or NULL if general log.
 * @param initial_level importance level of the message expressed using a @ref
 *        lavu_log_constants "Logging Constant" for the first occurance.
 * @param subsequent_level importance level of the message expressed using a @ref
 *        lavu_log_constants "Logging Constant" after the first occurance.
 * @param fmt The format string (printf-compatible) that specifies how
 *        subsequent arguments are converted to output.
 * @param state a variable to keep trak of if a message has already been printed
 *        this must be initialized to 0 before the first use. The same state
 *        must not be accessed by 2 Threads simultaneously.
 *)
procedure av_log_once(avcl: Pointer; initial_level, subsequent_level: Integer; state: PInteger; fmt: PAnsiChar);cdecl varargs; external AVUTIL_LIBNAME name _PU + 'av_log_once';


(**
 * Send the specified message to the log if the level is less than or equal
 * to the current av_log_level. By default, all logging messages are sent to
 * stderr. This behavior can be altered by setting a different logging callback
 * function.
 * @see av_log_set_callback
 *
 * @param avcl A pointer to an arbitrary struct of which the first field is a
 *        pointer to an AVClass struct.
 * @param level The importance level of the message expressed using a @ref
 *        lavu_log_constants "Logging Constant".
 * @param fmt The format string (printf-compatible) that specifies how
 *        subsequent arguments are converted to output.
 * @param vl The arguments referenced by the format string.
 *)
type
  Tav_vlogCall = procedure(avcl: Pointer; level: Integer; const fmt: PAnsiChar; vl: PAnsiChar); cdecl;
procedure av_vlog(avcl: Pointer; level: Integer; const fmt: PAnsiChar; vl: PAnsiChar); cdecl; external AVUTIL_LIBNAME name _PU + 'av_vlog';

(**
 * Get the current log level
 *
 * @see lavu_log_constants
 *
 * @return Current log level
 *)
function av_log_get_level: Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_log_get_level';

(**
 * Set the log level
 *
 * @see lavu_log_constants
 *
 * @param level Logging level
 *)
procedure av_log_set_level(level: Integer); cdecl; external AVUTIL_LIBNAME name _PU + 'av_log_set_level';

(**
 * Set the logging callback
 *
 * @note The callback must be thread safe, even if the application does not use
 *       threads itself as some codecs are multithreaded.
 *
 * @see av_log_default_callback
 *
 * @param callback A logging function with a compatible signature.
 *)
procedure av_log_set_callback(callback: Tav_vlogCall); cdecl; external AVUTIL_LIBNAME name _PU + 'av_log_set_callback';

(**
 * Default logging callback
 *
 * It prints the message to stderr, optionally colorizing it.
 *
 * @param avcl A pointer to an arbitrary struct of which the first field is a
 *        pointer to an AVClass struct.
 * @param level The importance level of the message expressed using a @ref
 *        lavu_log_constants "Logging Constant".
 * @param fmt The format string (printf-compatible) that specifies how
 *        subsequent arguments are converted to output.
 * @param vl The arguments referenced by the format string.
 *)
procedure av_log_default_callback(avcl: Pointer; level: Integer; const fmt: PAnsiChar;
                             vl: PAnsiChar); cdecl; external AVUTIL_LIBNAME name _PU + 'av_log_default_callback';

(**
 * Return the context name
 *
 * @param  ctx The AVClass context
 *
 * @return The AVClass class_name
 *)
function av_default_item_name(ctx: Pointer): PAnsiChar; cdecl; external AVUTIL_LIBNAME name _PU + 'av_default_item_name';
function av_default_get_category(ptr: Pointer): TAVClassCategory; cdecl; external AVUTIL_LIBNAME name _PU + 'av_default_get_category';

(**
 * Format a line of log the same way as the default callback.
 * @param line          buffer to receive the formatted line
 * @param line_size     size of the buffer
 * @param print_prefix  used to store whether the prefix must be printed;
 *                      must point to a persistent integer initially set to 1
 *)
procedure av_log_format_line(ptr: Pointer; level: Integer; const fmt: PAnsiChar; vl: Pointer;{va_list}
                        line: PAnsiChar; line_size: Integer; print_prefix: PInteger); cdecl; external AVUTIL_LIBNAME name _PU + 'av_log_format_line';

(**
 * Format a line of log the same way as the default callback.
 * @param line          buffer to receive the formatted line;
 *                      may be NULL if line_size is 0
 * @param line_size     size of the buffer; at most line_size-1 characters will
 *                      be written to the buffer, plus one null terminator
 * @param print_prefix  used to store whether the prefix must be printed;
 *                      must point to a persistent integer initially set to 1
 * @return Returns a negative value if an error occurred, otherwise returns
 *         the number of characters that would have been written for a
 *         sufficiently large buffer, not including the terminating null
 *         character. If the return value is not less than line_size, it means
 *         that the log message was truncated to fit the buffer.
 *)
function av_log_format_line2(ptr: Pointer; level: Integer; const fmt: PAnsiChar; vl: Pointer;{va_list}
                        line: PAnsiChar; line_size: Integer; print_prefix: PInteger): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_log_format_line2';

procedure av_log_set_flags(arg: Integer); cdecl; external AVUTIL_LIBNAME name _PU + 'av_log_set_flags';
function av_log_get_flags: Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_log_get_flags';

const
(**
 * Skip repeated messages, this requires the user app to use av_log() instead of
 * (f)printf as the 2 would otherwise interfere and lead to
 * "Last message repeated x times" messages below (f)printf messages with some
 * bad luck.
 * Also to receive the last, "last repeated" line if any, the user app must
 * call av_log(NULL, AV_LOG_QUIET, "%s", ""); at the end
 *)
  AV_LOG_SKIP_REPEATED = 1;

(**
 * Include the log severity in messages originating from codecs.
 *
 * Results in messages such as:
 * [rawvideo @ 0xDEADBEEF] [error] encode did not produce valid pts
 *)
  AV_LOG_PRINT_LEVEL = 2;

(**
 * @}
 *)

implementation

function AV_LOG_C(x: Integer): Integer;
begin
  Result := x shl 8;
end;

end.
