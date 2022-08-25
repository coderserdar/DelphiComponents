(*
 * AVOptions
 * copyright (c) 2005 Michael Niedermayer <michaelni@gmx.at>
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
 * AVOptions
 *)

(*
 * FFVCL - Delphi FFmpeg VCL Components
 * http://www.DelphiFFmpeg.com
 *
 * Original file: libavutil/opt.h
 * Ported by CodeCoolie@CNSW 2010/10/03 -> $Date:: 2021-04-25 #$
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

unit libavutil_opt;

interface

{$I CompilerDefines.inc}

uses
  FFTypes,
  libavutil_dict,
  libavutil_pixfmt,
  libavutil_rational,
  libavutil_samplefmt;

{$I libversion.inc}

(**
 * @defgroup avoptions AVOptions
 * @ingroup lavu_data
 * @{
 * AVOptions provide a generic system to declare options on arbitrary structs
 * ("objects"). An option can have a help text, a type and a range of possible
 * values. Options may then be enumerated, read and written to.
 *
 * @section avoptions_implement Implementing AVOptions
 * This section describes how to add AVOptions capabilities to a struct.
 *
 * All AVOptions-related information is stored in an AVClass. Therefore
 * the first member of the struct should be a pointer to an AVClass describing it.
 * The option field of the AVClass must be set to a NULL-terminated static array
 * of AVOptions. Each AVOption must have a non-empty name, a type, a default
 * value and for number-type AVOptions also a range of allowed values. It must
 * also declare an offset in bytes from the start of the struct, where the field
 * associated with this AVOption is located. Other fields in the AVOption struct
 * should also be set when applicable, but are not required.
 *
 * The following example illustrates an AVOptions-enabled struct:
 * @code
 * typedef struct test_struct {
 *     const AVClass *class;
 *     int      int_opt;
 *     char    *str_opt;
 *     uint8_t *bin_opt;
 *     int      bin_len;
 * } test_struct;
 *
 * static const AVOption test_options[] = {
 *   { "test_int", "This is a test option of int type.", offsetof(test_struct, int_opt),
 *     AV_OPT_TYPE_INT, { .i64 = -1 }, INT_MIN, INT_MAX },
 *   { "test_str", "This is a test option of string type.", offsetof(test_struct, str_opt),
 *     AV_OPT_TYPE_STRING },
 *   { "test_bin", "This is a test option of binary type.", offsetof(test_struct, bin_opt),
 *     AV_OPT_TYPE_BINARY },
 *   { NULL },
 * };
 *
 * static const AVClass test_class = {
 *     .class_name = "test class",
 *     .item_name  = av_default_item_name,
 *     .option     = test_options,
 *     .version    = LIBAVUTIL_VERSION_INT,
 * };
 * @endcode
 *
 * Next, when allocating your struct, you must ensure that the AVClass pointer
 * is set to the correct value. Then, av_opt_set_defaults() can be called to
 * initialize defaults. After that the struct is ready to be used with the
 * AVOptions API.
 *
 * When cleaning up, you may use the av_opt_free() function to automatically
 * free all the allocated string and binary options.
 *
 * Continuing with the above example:
 *
 * @code
 * test_struct *alloc_test_struct(void)
 * {
 *     test_struct *ret = av_mallocz(sizeof(*ret));
 *     ret->class = &test_class;
 *     av_opt_set_defaults(ret);
 *     return ret;
 * }
 * void free_test_struct(test_struct **foo)
 * {
 *     av_opt_free(*foo);
 *     av_freep(foo);
 * }
 * @endcode
 *
 * @subsection avoptions_implement_nesting Nesting
 *      It may happen that an AVOptions-enabled struct contains another
 *      AVOptions-enabled struct as a member (e.g. AVCodecContext in
 *      libavcodec exports generic options, while its priv_data field exports
 *      codec-specific options). In such a case, it is possible to set up the
 *      parent struct to export a child's options. To do that, simply
 *      implement AVClass.child_next() and AVClass.child_class_iterate() in the
 *      parent struct's AVClass.
 *      Assuming that the test_struct from above now also contains a
 *      child_struct field:
 *
 *      @code
 *      typedef struct child_struct {
 *          AVClass *class;
 *          int flags_opt;
 *      } child_struct;
 *      static const AVOption child_opts[] = {
 *          { "test_flags", "This is a test option of flags type.",
 *            offsetof(child_struct, flags_opt), AV_OPT_TYPE_FLAGS, { .i64 = 0 }, INT_MIN, INT_MAX },
 *          { NULL },
 *      };
 *      static const AVClass child_class = {
 *          .class_name = "child class",
 *          .item_name  = av_default_item_name,
 *          .option     = child_opts,
 *          .version    = LIBAVUTIL_VERSION_INT,
 *      };
 *
 *      void *child_next(void *obj, void *prev)
 *      {
 *          test_struct *t = obj;
 *          if (!prev && t->child_struct)
 *              return t->child_struct;
 *          return NULL
 *      }
 *      const AVClass child_class_iterate(void **iter)
 *      {
 *          const AVClass *c = *iter ? NULL : &child_class;
 *          *iter = (void* )(uintptr_t)c;
 *          return c;
 *      }
 *      @endcode
 *      Putting child_next() and child_class_iterate() as defined above into
 *      test_class will now make child_struct's options accessible through
 *      test_struct (again, proper setup as described above needs to be done on
 *      child_struct right after it is created).
 *
 *      From the above example it might not be clear why both child_next()
 *      and child_class_iterate() are needed. The distinction is that child_next()
 *      iterates over actually existing objects, while child_class_iterate()
 *      iterates over all possible child classes. E.g. if an AVCodecContext
 *      was initialized to use a codec which has private options, then its
 *      child_next() will return AVCodecContext.priv_data and finish
 *      iterating. OTOH child_class_iterate() on AVCodecContext.av_class will
 *      iterate over all available codecs with private options.
 *
 * @subsection avoptions_implement_named_constants Named constants
 *      It is possible to create named constants for options. Simply set the unit
 *      field of the option the constants should apply to a string and
 *      create the constants themselves as options of type AV_OPT_TYPE_CONST
 *      with their unit field set to the same string.
 *      Their default_val field should contain the value of the named
 *      constant.
 *      For example, to add some named constants for the test_flags option
 *      above, put the following into the child_opts array:
 *      @code
 *      { "test_flags", "This is a test option of flags type.",
 *        offsetof(child_struct, flags_opt), AV_OPT_TYPE_FLAGS, { .i64 = 0 }, INT_MIN, INT_MAX, "test_unit" },
 *      { "flag1", "This is a flag with value 16", 0, AV_OPT_TYPE_CONST, { .i64 = 16 }, 0, 0, "test_unit" },
 *      @endcode
 *
 * @section avoptions_use Using AVOptions
 * This section deals with accessing options in an AVOptions-enabled struct.
 * Such structs in FFmpeg are e.g. AVCodecContext in libavcodec or
 * AVFormatContext in libavformat.
 *
 * @subsection avoptions_use_examine Examining AVOptions
 * The basic functions for examining options are av_opt_next(), which iterates
 * over all options defined for one object, and av_opt_find(), which searches
 * for an option with the given name.
 *
 * The situation is more complicated with nesting. An AVOptions-enabled struct
 * may have AVOptions-enabled children. Passing the AV_OPT_SEARCH_CHILDREN flag
 * to av_opt_find() will make the function search children recursively.
 *
 * For enumerating there are basically two cases. The first is when you want to
 * get all options that may potentially exist on the struct and its children
 * (e.g.  when constructing documentation). In that case you should call
 * av_opt_child_class_iterate() recursively on the parent struct's AVClass.  The
 * second case is when you have an already initialized struct with all its
 * children and you want to get all options that can be actually written or read
 * from it. In that case you should call av_opt_child_next() recursively (and
 * av_opt_next() on each result).
 *
 * @subsection avoptions_use_get_set Reading and writing AVOptions
 * When setting options, you often have a string read directly from the
 * user. In such a case, simply passing it to av_opt_set() is enough. For
 * non-string type options, av_opt_set() will parse the string according to the
 * option type.
 *
 * Similarly av_opt_get() will read any option type and convert it to a string
 * which will be returned. Do not forget that the string is allocated, so you
 * have to free it with av_free().
 *
 * In some cases it may be more convenient to put all options into an
 * AVDictionary and call av_opt_set_dict() on it. A specific case of this
 * are the format/codec open functions in lavf/lavc which take a dictionary
 * filled with option as a parameter. This makes it possible to set some options
 * that cannot be set otherwise, since e.g. the input file format is not known
 * before the file is actually opened.
 *)

type
  TAVOptionType = (
    AV_OPT_TYPE_FLAGS,
    AV_OPT_TYPE_INT,
    AV_OPT_TYPE_INT64,
    AV_OPT_TYPE_DOUBLE,
    AV_OPT_TYPE_FLOAT,
    AV_OPT_TYPE_STRING,
    AV_OPT_TYPE_RATIONAL,
    AV_OPT_TYPE_BINARY,  ///< offset must point to a pointer immediately followed by an int for the length
    AV_OPT_TYPE_DICT,
    AV_OPT_TYPE_UINT64,
    AV_OPT_TYPE_CONST,
    AV_OPT_TYPE_IMAGE_SIZE, ///< offset must point to two consecutive integers
    AV_OPT_TYPE_PIXEL_FMT,
    AV_OPT_TYPE_SAMPLE_FMT,
    AV_OPT_TYPE_VIDEO_RATE, ///< offset must point to AVRational
    AV_OPT_TYPE_DURATION,
    AV_OPT_TYPE_COLOR,
    AV_OPT_TYPE_CHANNEL_LAYOUT,
    AV_OPT_TYPE_BOOL
  );

const
  // const for TAVOption.flags
  AV_OPT_FLAG_ENCODING_PARAM  = 1;   ///< a generic parameter which can be set by the user for muxing or encoding
  AV_OPT_FLAG_DECODING_PARAM  = 2;   ///< a generic parameter which can be set by the user for demuxing or decoding
  AV_OPT_FLAG_AUDIO_PARAM     = 8;
  AV_OPT_FLAG_VIDEO_PARAM     = 16;
  AV_OPT_FLAG_SUBTITLE_PARAM  = 32;
(**
 * The option is intended for exporting values to the caller.
 *)
  AV_OPT_FLAG_EXPORT          = 64;
(**
 * The option may not be set through the AVOptions API, only read.
 * This flag only makes sense when AV_OPT_FLAG_EXPORT is also set.
 *)
  AV_OPT_FLAG_READONLY        = 128;
  AV_OPT_FLAG_BSF_PARAM       = (1 shl 8); ///< a generic parameter which can be set by the user for bit stream filtering
  AV_OPT_FLAG_RUNTIME_PARAM   = (1 shl 15); ///< a generic parameter which can be set by the user at runtime
  AV_OPT_FLAG_FILTERING_PARAM = (1 shl 16); ///< a generic parameter which can be set by the user for filtering
  AV_OPT_FLAG_DEPRECATED      = (1 shl 17); ///< set if option is deprecated, users should refer to AVOption.help text for more information
  AV_OPT_FLAG_CHILD_CONSTS    = (1 shl 18); ///< set if option constants can also reside in child objects
//FIXME think about enc-audio, ... style flags

(**
 * AVOption
 *)
type
  _Tdefault_val = record
    case Integer of
      0: (i64: Int64);
      1: (dbl: Double);
      2: (str: PAnsiChar);
      (* TODO those are unused now *)
      3: (q: TAVRational);
  end;

  PPAVOption = ^PAVOption;
  PAVOption = ^TAVOption;
  TAVOption = record
    name: PAnsiChar;

    (**
     * short English help text.
     * @todo What about other languages
     *)
    help: PAnsiChar;

    (**
     * The offset relative to the context structure where the option
     * value is stored. It should be 0 for named constants.
     *)
    offset: Integer;
    ttype: TAVOptionType;

    (**
     * the default value for scalar options
     *)
    default_val: _Tdefault_val;
    min: Double;                        ///< minimum valid value for the option
    max: Double;                        ///< maximum valid value for the option

    flags: Integer;
{
#define AV_OPT_FLAG_ENCODING_PARAM  1   ///< a generic parameter which can be set by the user for muxing or encoding
#define AV_OPT_FLAG_DECODING_PARAM  2   ///< a generic parameter which can be set by the user for demuxing or decoding
#define AV_OPT_FLAG_AUDIO_PARAM     8
#define AV_OPT_FLAG_VIDEO_PARAM     16
#define AV_OPT_FLAG_SUBTITLE_PARAM  32
/**
 * The option is intended for exporting values to the caller.
 */
#define AV_OPT_FLAG_EXPORT          64
/**
 * The option may not be set through the AVOptions API, only read.
 * This flag only makes sense when AV_OPT_FLAG_EXPORT is also set.
 */
#define AV_OPT_FLAG_READONLY        128
#define AV_OPT_FLAG_BSF_PARAM       (1<<8) ///< a generic parameter which can be set by the user for bit stream filtering
#define AV_OPT_FLAG_RUNTIME_PARAM   (1<<15) ///< a generic parameter which can be set by the user at runtime
#define AV_OPT_FLAG_FILTERING_PARAM (1<<16) ///< a generic parameter which can be set by the user for filtering
#define AV_OPT_FLAG_DEPRECATED      (1<<17) ///< set if option is deprecated, users should refer to AVOption.help text for more information
#define AV_OPT_FLAG_CHILD_CONSTS    (1<<18) ///< set if option constants can also reside in child objects
//FIXME think about enc-audio, ... style flags
}

    (**
     * The logical unit to which the option belongs. Non-constant
     * options and corresponding named constants share the same
     * unit. May be NULL.
     *)
    uunit: PAnsiChar; //const char *unit;
  end;

(**
 * A single allowed range of values, or a single allowed value.
 *)
  PPAVOptionRange = ^PAVOptionRange;
  PAVOptionRange = ^TAVOptionRange;
  TAVOptionRange = record
    str: PAnsiChar;
    (**
     * Value range.
     * For string ranges this represents the min/max length.
     * For dimensions this represents the min/max pixel count or width/height in multi-component case.
     *)
    value_min, value_max: Double;
    (**
     * Value's component range.
     * For string this represents the unicode range for chars, 0-127 limits to ASCII.
     *)
    component_min, component_max: Double;
    (**
     * Range flag.
     * If set to 1 the struct encodes a range, if set to 0 a single value.
     *)
    is_range: Integer;
  end;

(**
 * List of AVOptionRange structs.
 *)
  PPAVOptionRanges = ^PAVOptionRanges;
  PAVOptionRanges = ^TAVOptionRanges;
  TAVOptionRanges = record
    (**
     * Array of option ranges.
     *
     * Most of option types use just one component.
     * Following describes multi-component option types:
     *
     * AV_OPT_TYPE_IMAGE_SIZE:
     * component index 0: range of pixel count (width * height).
     * component index 1: range of width.
     * component index 2: range of height.
     *
     * @note To obtain multi-component version of this structure, user must
     *       provide AV_OPT_MULTI_COMPONENT_RANGE to av_opt_query_ranges or
     *       av_opt_query_ranges_default function.
     *
     * Multi-component range can be read as in following example:
     *
     * @code
     * int range_index, component_index;
     * AVOptionRanges *ranges;
     * AVOptionRange *range[3]; //may require more than 3 in the future.
     * av_opt_query_ranges(&ranges, obj, key, AV_OPT_MULTI_COMPONENT_RANGE);
     * for (range_index = 0; range_index < ranges->nb_ranges; range_index++) {
     *     for (component_index = 0; component_index < ranges->nb_components; component_index++)
     *         range[component_index] = ranges->range[ranges->nb_ranges * component_index + range_index];
     *     //do something with range here.
     * }
     * av_opt_freep_ranges(&ranges);
     * @endcode
     *)
    range: PPAVOptionRange;
    (**
     * Number of ranges per component.
     *)
    nb_ranges: Integer;
    (**
     * Number of componentes.
     *)
    nb_components: Integer;
  end;

(**
 * Show the obj options.
 *
 * @param req_flags requested flags for the options to show. Show only the
 * options for which it is opt->flags & req_flags.
 * @param rej_flags rejected flags for the options to show. Show only the
 * options for which it is !(opt->flags & req_flags).
 * @param av_log_obj log context to use for showing the options
 *)
function av_opt_show2(obj: Pointer; av_log_obj: Pointer; req_flags, rej_flags: Integer): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_opt_show2';

(**
 * Set the values of all AVOption fields to their default values.
 *
 * @param s an AVOption-enabled struct (its first member must be a pointer to AVClass)
 *)
procedure av_opt_set_defaults(s: Pointer); cdecl; external AVUTIL_LIBNAME name _PU + 'av_opt_set_defaults';

(**
 * Set the values of all AVOption fields to their default values. Only these
 * AVOption fields for which (opt->flags & mask) == flags will have their
 * default applied to s.
 *
 * @param s an AVOption-enabled struct (its first member must be a pointer to AVClass)
 * @param mask combination of AV_OPT_FLAG_*
 * @param flags combination of AV_OPT_FLAG_*
 *)
procedure av_opt_set_defaults2(s: Pointer; mask, flags: Integer); cdecl; external AVUTIL_LIBNAME name _PU + 'av_opt_set_defaults2';

(**
 * Parse the key/value pairs list in opts. For each key/value pair
 * found, stores the value in the field in ctx that is named like the
 * key. ctx must be an AVClass context, storing is done using
 * AVOptions.
 *
 * @param opts options string to parse, may be NULL
 * @param key_val_sep a 0-terminated list of characters used to
 * separate key from value
 * @param pairs_sep a 0-terminated list of characters used to separate
 * two pairs from each other
 * @return the number of successfully set key/value pairs, or a negative
 * value corresponding to an AVERROR code in case of error:
 * AVERROR(EINVAL) if opts cannot be parsed,
 * the error code issued by av_opt_set() if a key/value pair
 * cannot be set
 *)
function av_set_options_string(ctx: Pointer; const opts: PAnsiChar;
                          const key_val_sep: PAnsiChar; const pairs_sep: PAnsiChar): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_set_options_string';

(**
 * Parse the key-value pairs list in opts. For each key=value pair found,
 * set the value of the corresponding option in ctx.
 *
 * @param ctx          the AVClass object to set options on
 * @param opts         the options string, key-value pairs separated by a
 *                     delimiter
 * @param shorthand    a NULL-terminated array of options names for shorthand
 *                     notation: if the first field in opts has no key part,
 *                     the key is taken from the first element of shorthand;
 *                     then again for the second, etc., until either opts is
 *                     finished, shorthand is finished or a named option is
 *                     found; after that, all options must be named
 * @param key_val_sep  a 0-terminated list of characters used to separate
 *                     key from value, for example '='
 * @param pairs_sep    a 0-terminated list of characters used to separate
 *                     two pairs from each other, for example ':' or ','
 * @return  the number of successfully set key=value pairs, or a negative
 *          value corresponding to an AVERROR code in case of error:
 *          AVERROR(EINVAL) if opts cannot be parsed,
 *          the error code issued by av_set_string3() if a key/value pair
 *          cannot be set
 *
 * Options names must use only the following characters: a-z A-Z 0-9 - . / _
 * Separators must use characters distinct from option names and from each
 * other.
 *)
function av_opt_set_from_string(ctx: Pointer; const opts: PAnsiChar;
                           const shorthand: PPAnsiChar;//const char *const *shorthand,
                           const key_val_sep, pairs_sep: PAnsiChar): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_opt_set_from_string';
(**
 * Free all allocated objects in obj.
 *)
procedure av_opt_free(obj: Pointer); cdecl; external AVUTIL_LIBNAME name _PU + 'av_opt_free';

(**
 * Check whether a particular flag is set in a flags field.
 *
 * @param field_name the name of the flag field option
 * @param flag_name the name of the flag to check
 * @return non-zero if the flag is set, zero if the flag isn't set,
 *         isn't of the right type, or the flags field doesn't exist.
 *)
function av_opt_flag_is_set(obj: Pointer; const field_name, flag_name: PAnsiChar): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_opt_flag_is_set';

(**
 * Set all the options from a given dictionary on an object.
 *
 * @param obj a struct whose first element is a pointer to AVClass
 * @param options options to process. This dictionary will be freed and replaced
 *                by a new one containing all options not found in obj.
 *                Of course this new dictionary needs to be freed by caller
 *                with av_dict_free().
 *
 * @return 0 on success, a negative AVERROR if some option was found in obj,
 *         but could not be set.
 *
 * @see av_dict_copy()
 *)
function av_opt_set_dict(obj: Pointer; options: PPAVDictionary): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_opt_set_dict';

(**
 * Set all the options from a given dictionary on an object.
 *
 * @param obj a struct whose first element is a pointer to AVClass
 * @param options options to process. This dictionary will be freed and replaced
 *                by a new one containing all options not found in obj.
 *                Of course this new dictionary needs to be freed by caller
 *                with av_dict_free().
 * @param search_flags A combination of AV_OPT_SEARCH_*.
 *
 * @return 0 on success, a negative AVERROR if some option was found in obj,
 *         but could not be set.
 *
 * @see av_dict_copy()
 *)
function av_opt_set_dict2(obj: Pointer; options: PPAVDictionary; search_flags: Integer): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_opt_set_dict2';

(**
 * Extract a key-value pair from the beginning of a string.
 *
 * @param ropts        pointer to the options string, will be updated to
 *                     point to the rest of the string (one of the pairs_sep
 *                     or the final NUL)
 * @param key_val_sep  a 0-terminated list of characters used to separate
 *                     key from value, for example '='
 * @param pairs_sep    a 0-terminated list of characters used to separate
 *                     two pairs from each other, for example ':' or ','
 * @param flags        flags; see the AV_OPT_FLAG_* values below
 * @param rkey         parsed key; must be freed using av_free()
 * @param rval         parsed value; must be freed using av_free()
 *
 * @return  >=0 for success, or a negative value corresponding to an
 *          AVERROR code in case of error; in particular:
 *          AVERROR(EINVAL) if no key is present
 *
 *)
function av_opt_get_key_value(const ropts: PPAnsiChar;
                         const key_val_sep, pairs_sep: PAnsiChar;
                         flags: Cardinal;
                         rkey, rval: PPAnsiChar): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_opt_get_key_value';

const
  (**
   * Accept to parse a value without a key; the key will then be returned
   * as NULL.
   *)
  AV_OPT_FLAG_IMPLICIT_KEY = 1;

(**
 * @defgroup opt_eval_funcs Evaluating option strings
 * @{
 * This group of functions can be used to evaluate option strings
 * and get numbers out of them. They do the same thing as av_opt_set(),
 * except the result is written into the caller-supplied pointer.
 *
 * @param obj a struct whose first element is a pointer to AVClass.
 * @param o an option for which the string is to be evaluated.
 * @param val string to be evaluated.
 * @param *_out value of the string will be written here.
 *
 * @return 0 on success, a negative number on failure.
 *)
function av_opt_eval_flags(obj: Pointer; const o: PAVOption; const val: PAnsiChar; flags_out : PInteger)   : Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_opt_eval_flags';
function av_opt_eval_int(obj: Pointer; const o: PAVOption; const val: PAnsiChar; int_out   : PInteger)   : Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_opt_eval_int';
function av_opt_eval_int64(obj: Pointer; const o: PAVOption; const val: PAnsiChar; int64_out : PInt64)     : Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_opt_eval_int64';
function av_opt_eval_float(obj: Pointer; const o: PAVOption; const val: PAnsiChar; float_out : PSingle)    : Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_opt_eval_float';
function av_opt_eval_double(obj: Pointer; const o: PAVOption; const val: PAnsiChar; double_out: PDouble)    : Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_opt_eval_double';
function av_opt_eval_q(obj: Pointer; const o: PAVOption; const val: PAnsiChar; q_out     : PAVRational): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_opt_eval_q';
(**
 * @}
 *)

const
  AV_OPT_SEARCH_CHILDREN  = (1 shl 0); (**< Search in possible children of the
                                            given object first. *)
(**
 *  The obj passed to av_opt_find() is fake -- only a double pointer to AVClass
 *  instead of a required pointer to a struct containing AVClass. This is
 *  useful for searching for options without needing to allocate the corresponding
 *  object.
 *)
  AV_OPT_SEARCH_FAKE_OBJ  = (1 shl 1);

(**
 *  In av_opt_get, return NULL if the option has a pointer type and is set to NULL,
 *  rather than returning an empty string.
 *)
  AV_OPT_ALLOW_NULL       = (1 shl 2);

(**
 *  Allows av_opt_query_ranges and av_opt_query_ranges_default to return more than
 *  one component for certain option types.
 *  @see AVOptionRanges for details.
 *)
  AV_OPT_MULTI_COMPONENT_RANGE = (1 shl 12);

(**
 * Look for an option in an object. Consider only options which
 * have all the specified flags set.
 *
 * @param[in] obj A pointer to a struct whose first element is a
 *                pointer to an AVClass.
 *                Alternatively a double pointer to an AVClass, if
 *                AV_OPT_SEARCH_FAKE_OBJ search flag is set.
 * @param[in] name The name of the option to look for.
 * @param[in] unit When searching for named constants, name of the unit
 *                 it belongs to.
 * @param opt_flags Find only options with all the specified flags set (AV_OPT_FLAG).
 * @param search_flags A combination of AV_OPT_SEARCH_*.
 *
 * @return A pointer to the option found, or NULL if no option
 *         was found.
 *
 * @note Options found with AV_OPT_SEARCH_CHILDREN flag may not be settable
 * directly with av_opt_set(). Use special calls which take an options
 * AVDictionary (e.g. avformat_open_input()) to set options found with this
 * flag.
 *)
function av_opt_find(obj: Pointer; const name, unit_: PAnsiChar;
                            opt_flags, search_flags: Integer): PAVOption; cdecl; external AVUTIL_LIBNAME name _PU + 'av_opt_find';

(**
 * Look for an option in an object. Consider only options which
 * have all the specified flags set.
 *
 * @param[in] obj A pointer to a struct whose first element is a
 *                pointer to an AVClass.
 *                Alternatively a double pointer to an AVClass, if
 *                AV_OPT_SEARCH_FAKE_OBJ search flag is set.
 * @param[in] name The name of the option to look for.
 * @param[in] unit When searching for named constants, name of the unit
 *                 it belongs to.
 * @param opt_flags Find only options with all the specified flags set (AV_OPT_FLAG).
 * @param search_flags A combination of AV_OPT_SEARCH_*.
 * @param[out] target_obj if non-NULL, an object to which the option belongs will be
 * written here. It may be different from obj if AV_OPT_SEARCH_CHILDREN is present
 * in search_flags. This parameter is ignored if search_flags contain
 * AV_OPT_SEARCH_FAKE_OBJ.
 *
 * @return A pointer to the option found, or NULL if no option
 *         was found.
 *)
function av_opt_find2(obj: Pointer; const name, unit_: PAnsiChar;
                              opt_flags, search_flags: Integer; target_obj: PPointer): PAVOption; cdecl; external AVUTIL_LIBNAME name _PU + 'av_opt_find2';

(**
 * Iterate over all AVOptions belonging to obj.
 *
 * @param obj an AVOptions-enabled struct or a double pointer to an
 *            AVClass describing it.
 * @param prev result of the previous call to av_opt_next() on this object
 *             or NULL
 * @return next AVOption or NULL
 *)
function av_opt_next(const obj: Pointer; const prev: PAVOption): PAVOption; cdecl; external AVUTIL_LIBNAME name _PU + 'av_opt_next';

(**
 * Iterate over AVOptions-enabled children of obj.
 *
 * @param prev result of a previous call to this function or NULL
 * @return next AVOptions-enabled child or NULL
 *)
procedure av_opt_child_next(obj, prev: Pointer); cdecl; external AVUTIL_LIBNAME name _PU + 'av_opt_child_next';

{$IFDEF FF_API_CHILD_CLASS_NEXT}
(**
 * Iterate over potential AVOptions-enabled children of parent.
 *
 * @param prev result of a previous call to this function or NULL
 * @return AVClass corresponding to next potential child or NULL
 *
 * @deprecated use av_opt_child_class_iterate
 *)
function av_opt_child_class_next(const parent, prev: Pointer{PAVClass}): Pointer{PAVClass}; cdecl; external AVUTIL_LIBNAME name _PU + 'av_opt_child_class_next';
{$ENDIF}

(**
 * Iterate over potential AVOptions-enabled children of parent.
 *
 * @param iter a pointer where iteration state is stored.
 * @return AVClass corresponding to next potential child or NULL
 *)
function av_opt_child_class_iterate(const parent: Pointer{PAVClass}; iter: PPointer): Pointer{PAVClass}; cdecl; external AVUTIL_LIBNAME name _PU + 'av_opt_child_class_iterate';

(**
 * @defgroup opt_set_funcs Option setting functions
 * @{
 * Those functions set the field of obj with the given name to value.
 *
 * @param[in] obj A struct whose first element is a pointer to an AVClass.
 * @param[in] name the name of the field to set
 * @param[in] val The value to set. In case of av_opt_set() if the field is not
 * of a string type, then the given string is parsed.
 * SI postfixes and some named scalars are supported.
 * If the field is of a numeric type, it has to be a numeric or named
 * scalar. Behavior with more than one scalar and +- infix operators
 * is undefined.
 * If the field is of a flags type, it has to be a sequence of numeric
 * scalars or named flags separated by '+' or '-'. Prefixing a flag
 * with '+' causes it to be set without affecting the other flags;
 * similarly, '-' unsets a flag.
 * If the field is of a dictionary type, it has to be a ':' separated list of
 * key=value parameters. Values containing ':' special characters must be
 * escaped.
 * @param search_flags flags passed to av_opt_find2. I.e. if AV_OPT_SEARCH_CHILDREN
 * is passed here, then the option may be set on a child of obj.
 *
 * @return 0 if the value has been set, or an AVERROR code in case of
 * error:
 * AVERROR_OPTION_NOT_FOUND if no matching option exists
 * AVERROR(ERANGE) if the value is out of range
 * AVERROR(EINVAL) if the value is not valid
 *)
function av_opt_set(obj: Pointer; const name: PAnsiChar; val: PAnsiChar;   search_flags: Integer): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_opt_set';
function av_opt_set_int(obj: Pointer; const name: PAnsiChar; val: Int64;       search_flags: Integer): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_opt_set_int';
function av_opt_set_double(obj: Pointer; const name: PAnsiChar; val: Double;      search_flags: Integer): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_opt_set_double';
function av_opt_set_q(obj: Pointer; const name: PAnsiChar; val: TAVRational; search_flags: Integer): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_opt_set_q';
function av_opt_set_bin(obj: Pointer; const name: PAnsiChar; val: PByte; size, search_flags: Integer): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_opt_set_bin';
function av_opt_set_image_size(obj: Pointer; const name: PAnsiChar; w, h, search_flags: Integer): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_opt_set_image_size';
function av_opt_set_pixel_fmt(obj: Pointer; const name: PAnsiChar; fmt: TAVPixelFormat; search_flags: Integer): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_opt_set_pixel_fmt';
function av_opt_set_sample_fmt(obj: Pointer; const name: PAnsiChar; fmt: TAVSampleFormat; search_flags: Integer): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_opt_set_sample_fmt';
function av_opt_set_video_rate(obj: Pointer; const name: PAnsiChar; val: TAVRational; search_flags: Integer): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_opt_set_video_rate';
function av_opt_set_channel_layout(obj: Pointer; const name: PAnsiChar; ch_layout: Int64; search_flags: Integer): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_opt_set_channel_layout';
(**
 * @note Any old dictionary present is discarded and replaced with a copy of the new one. The
 * caller still owns val is and responsible for freeing it.
 *)
function av_opt_set_dict_val(obj: Pointer; const name: PAnsiChar; const val: PAVDictionary; search_flags: Integer): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_opt_set_dict_val';

(**
 * Set a binary option to an integer list.
 *
 * @param obj    AVClass object to set options on
 * @param name   name of the binary option
 * @param val    pointer to an integer list (must have the correct type with
 *               regard to the contents of the list)
 * @param term   list terminator (usually 0 or -1)
 * @param flags  search flags
 *)
//#define av_opt_set_int_list(obj, name, val, term, flags) \
//    (av_int_list_length(val, term) > INT_MAX / sizeof(*(val)) ? \
//     AVERROR(EINVAL) : \
//     av_opt_set_bin(obj, name, (const uint8_t *)(val), \
//                    av_int_list_length(val, term) * sizeof(*(val)), flags))
function av_opt_set_int_list(obj: Pointer; name: PAnsiChar; list: Pointer; item_size: Integer; term: Int64; flags: Integer): Integer;
(**
 * @}
 *)

(**
 * @defgroup opt_get_funcs Option getting functions
 * @{
 * Those functions get a value of the option with the given name from an object.
 *
 * @param[in] obj a struct whose first element is a pointer to an AVClass.
 * @param[in] name name of the option to get.
 * @param[in] search_flags flags passed to av_opt_find2. I.e. if AV_OPT_SEARCH_CHILDREN
 * is passed here, then the option may be found in a child of obj.
 * @param[out] out_val value of the option will be written here
 * @return >=0 on success, a negative error code otherwise
 *)
(**
 * @note the returned string will be av_malloc()ed and must be av_free()ed by the caller
 *
 * @note if AV_OPT_ALLOW_NULL is set in search_flags in av_opt_get, and the
 * option is of type AV_OPT_TYPE_STRING, AV_OPT_TYPE_BINARY or AV_OPT_TYPE_DICT
 * and is set to NULL, *out_val will be set to NULL instead of an allocated
 * empty string.
 *)
function av_opt_get(obj: Pointer; const name: PAnsiChar; search_flags: Integer; out_val: PPByte)     : Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_opt_get';
function av_opt_get_int(obj: Pointer; const name: PAnsiChar; search_flags: Integer; out_val: PInt64)     : Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_opt_get_int';
function av_opt_get_double(obj: Pointer; const name: PAnsiChar; search_flags: Integer; out_val: PDouble)    : Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_opt_get_double';
function av_opt_get_q(obj: Pointer; const name: PAnsiChar; search_flags: Integer; out_val: PAVRational): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_opt_get_q';
function av_opt_get_image_size(obj: Pointer; const name: PAnsiChar; search_flags: Integer; w_out, h_out: PInteger): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_opt_get_image_size';
function av_opt_get_pixel_fmt(obj: Pointer; const name: PAnsiChar; search_flags: Integer; out_fmt: PAVPixelFormat): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_opt_get_pixel_fmt';
function av_opt_get_sample_fmt(obj: Pointer; const name: PAnsiChar; search_flags: Integer; out_fmt: PAVPixelFormat): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_opt_get_sample_fmt';
function av_opt_get_video_rate(obj: Pointer; const name: PAnsiChar; search_flags: Integer; out_val: PAVRational): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_opt_get_video_rate';
function av_opt_get_channel_layout(obj: Pointer; const name: PAnsiChar; search_flags: Integer; ch_layout: PInt64): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_opt_get_channel_layout';
(**
 * @param[out] out_val The returned dictionary is a copy of the actual value and must
 * be freed with av_dict_free() by the caller
 *)
function av_opt_get_dict_val(obj: Pointer; const name: PAnsiChar; search_flags: Integer; out_val: PPAVDictionary): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_opt_get_dict_val';
(**
 * @}
 *)
(**
 * Gets a pointer to the requested field in a struct.
 * This function allows accessing a struct even when its fields are moved or
 * renamed since the application making the access has been compiled,
 *
 * @returns a pointer to the field, it can be cast to the correct type and read
 *          or written to.
 *)
function av_opt_ptr(const avclass: Pointer{PAVClass}; obj: Pointer; const name: PAnsiChar): Pointer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_opt_ptr';

(**
 * Free an AVOptionRanges struct and set it to NULL.
 *)
procedure av_opt_freep_ranges(ranges: PPAVOptionRanges); cdecl; external AVUTIL_LIBNAME name _PU + 'av_opt_freep_ranges';

(**
 * Get a list of allowed ranges for the given option.
 *
 * The returned list may depend on other fields in obj like for example profile.
 *
 * @param flags is a bitmask of flags, undefined flags should not be set and should be ignored
 *              AV_OPT_SEARCH_FAKE_OBJ indicates that the obj is a double pointer to a AVClass instead of a full instance
 *              AV_OPT_MULTI_COMPONENT_RANGE indicates that function may return more than one component, @see AVOptionRanges
 *
 * The result must be freed with av_opt_freep_ranges.
 *
 * @return number of compontents returned on success, a negative errro code otherwise
 *)
function av_opt_query_ranges(ranges: PPAVOptionRanges; obj: Pointer; const key: PAnsiChar; flags: Integer): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_opt_query_ranges';

(**
 * Copy options from src object into dest object.
 *
 * Options that require memory allocation (e.g. string or binary) are malloc'ed in dest object.
 * Original memory allocated for such options is freed unless both src and dest options points to the same memory.
 *
 * @param dest Object to copy from
 * @param src  Object to copy into
 * @return 0 on success, negative on error
 *)
function av_opt_copy(dest: Pointer; const src: Pointer): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_opt_copy';

(**
 * Get a default list of allowed ranges for the given option.
 *
 * This list is constructed without using the AVClass.query_ranges() callback
 * and can be used as fallback from within the callback.
 *
 * @param flags is a bitmask of flags, undefined flags should not be set and should be ignored
 *              AV_OPT_SEARCH_FAKE_OBJ indicates that the obj is a double pointer to a AVClass instead of a full instance
 *              AV_OPT_MULTI_COMPONENT_RANGE indicates that function may return more than one component, @see AVOptionRanges
 *
 * The result must be freed with av_opt_free_ranges.
 *
 * @return number of compontents returned on success, a negative errro code otherwise
 *)
function av_opt_query_ranges_default(ranges: PPAVOptionRanges; obj: Pointer; const key: PAnsiChar; flags: Integer): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_opt_query_ranges_default';

(**
 * Check if given option is set to its default value.
 *
 * Options o must belong to the obj. This function must not be called to check child's options state.
 * @see av_opt_is_set_to_default_by_name().
 *
 * @param obj  AVClass object to check option on
 * @param o    option to be checked
 * @return     >0 when option is set to its default,
 *              0 when option is not set its default,
 *             <0 on error
 *)
function av_opt_is_set_to_default(obj: Pointer; const o: PAVOption): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_opt_is_set_to_default';

(**
 * Check if given option is set to its default value.
 *
 * @param obj          AVClass object to check option on
 * @param name         option name
 * @param search_flags combination of AV_OPT_SEARCH_*
 * @return             >0 when option is set to its default,
 *                     0 when option is not set its default,
 *                     <0 on error
 *)
function av_opt_is_set_to_default_by_name(obj: Pointer; const name: PAnsiChar; search_flags: Integer): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_opt_is_set_to_default_by_name';


const
  AV_OPT_SERIALIZE_SKIP_DEFAULTS   = $00000001;  ///< Serialize options that are not set to default values only.
  AV_OPT_SERIALIZE_OPT_FLAGS_EXACT = $00000002;  ///< Serialize options that exactly match opt_flags only.

(**
 * Serialize object's options.
 *
 * Create a string containing object's serialized options.
 * Such string may be passed back to av_opt_set_from_string() in order to restore option values.
 * A key/value or pairs separator occurring in the serialized value or
 * name string are escaped through the av_escape() function.
 *
 * @param[in]  obj           AVClass object to serialize
 * @param[in]  opt_flags     serialize options with all the specified flags set (AV_OPT_FLAG)
 * @param[in]  flags         combination of AV_OPT_SERIALIZE_* flags
 * @param[out] buffer        Pointer to buffer that will be allocated with string containg serialized options.
 *                           Buffer must be freed by the caller when is no longer needed.
 * @param[in]  key_val_sep   character used to separate key from value
 * @param[in]  pairs_sep     character used to separate two pairs from each other
 * @return                   >= 0 on success, negative on error
 * @warning Separators cannot be neither '\\' nor '\0'. They also cannot be the same.
 *)
function av_opt_serialize(obj: Pointer; opt_flags, flags: Integer; buffer: PPAnsiChar;
                     const key_val_sep, pairs_sep: AnsiChar): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_opt_serialize';
(**
 * @}
 *)

implementation

uses
  libavutil,
  libavutil_error;

function av_opt_set_int_list(obj: Pointer; name: PAnsiChar; list: Pointer; item_size: Integer; term: Int64; flags: Integer): Integer;
begin
  if av_int_list_length(list, item_size, term) > MaxInt / item_size then
    Result := AVERROR_EINVAL
  else
    Result := av_opt_set_bin(obj, name, PByte(list),
                  av_int_list_length(list, item_size, term) * item_size, flags);
end;

end.
