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

(*
 * FFVCL - Delphi FFmpeg VCL Components
 * http://www.DelphiFFmpeg.com
 *
 * Original file: libavfilter/formats.h
 * Ported by CodeCoolie@CNSW 2014/07/21 -> $Date:: 2021-04-25 #$
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

unit libavfilter_formats;

interface

{$I CompilerDefines.inc}

uses
  libavfilter,
  libavutil;

{$I libversion.inc}

const
  //* format is software, non-planar with sub-sampling
  FF_PIX_FMT_FLAG_SW_FLAT_SUB = (1 shl 24);

type
(**
 * A list of supported formats for one end of a filter link. This is used
 * during the format negotiation process to try to pick the best format to
 * use to minimize the number of necessary conversions. Each filter gives a
 * list of the formats supported by each input and output pad. The list
 * given for each pad need not be distinct - they may be references to the
 * same list of formats, as is often the case when a filter supports multiple
 * formats, but will always output the same format as it is given in input.
 *
 * In this way, a list of possible input formats and a list of possible
 * output formats are associated with each link. When a set of formats is
 * negotiated over a link, the input and output lists are merged to form a
 * new list containing only the common elements of each list. In the case
 * that there were no common elements, a format conversion is necessary.
 * Otherwise, the lists are merged, and all other links which reference
 * either of the format lists involved in the merge are also affected.
 *
 * For example, consider the filter chain:
 * filter (a) --> (b) filter (b) --> (c) filter
 *
 * where the letters in parenthesis indicate a list of formats supported on
 * the input or output of the link. Suppose the lists are as follows:
 * (a) = {A, B}
 * (b) = {A, B, C}
 * (c) = {B, C}
 *
 * First, the first link's lists are merged, yielding:
 * filter (a) --> (a) filter (a) --> (c) filter
 *
 * Notice that format list (b) now refers to the same list as filter list (a).
 * Next, the lists for the second link are merged, yielding:
 * filter (a) --> (a) filter (a) --> (a) filter
 *
 * where (a) = {B}.
 *
 * Unfortunately, when the format lists at the two ends of a link are merged,
 * we must ensure that all links which reference either pre-merge format list
 * get updated as well. Therefore, we have the format list structure store a
 * pointer to each of the pointers to itself.
 *)
  PPPAVFilterFormats = ^PPAVFilterFormats;
  PPAVFilterFormats = ^PAVFilterFormats;
  PAVFilterFormats = ^TAVFilterFormats;
  TAVFilterFormats = record
    nb_formats: Cardinal;         ///< number of formats
    formats: PInteger;            ///< list of media formats

    refcount: Cardinal;           ///< number of references to this list
    refs: PPPAVFilterFormats;     ///< references to this list
  end;

(**
 * A list of supported channel layouts.
 *
 * The list works the same as AVFilterFormats, except for the following
 * differences:
 * - A list with all_layouts = 1 means all channel layouts with a known
 *   disposition; nb_channel_layouts must then be 0.
 * - A list with all_counts = 1 means all channel counts, with a known or
 *   unknown disposition; nb_channel_layouts must then be 0 and all_layouts 1.
 * - The list must not contain a layout with a known disposition and a
 *   channel count with unknown disposition with the same number of channels
 *   (e.g. AV_CH_LAYOUT_STEREO and FF_COUNT2LAYOUT(2).
 *)
  PPPAVFilterChannelLayouts = ^PPAVFilterChannelLayouts;
  PPAVFilterChannelLayouts = ^PAVFilterChannelLayouts;
  PAVFilterChannelLayouts = ^TAVFilterChannelLayouts;
  TAVFilterChannelLayouts = record
    channel_layouts: PInt64;          ///< list of channel layouts
    nb_channel_layouts: Integer;      ///< number of channel layouts
    all_layouts: AnsiChar;            ///< accept any known channel layout
    all_counts: AnsiChar;             ///< accept any channel layout or count

    refcount: Cardinal;               ///< number of references to this list
    refs: PPPAVFilterChannelLayouts;  ///< references to this list
  end;

(**
 * Encode a channel count as a channel layout.
 * FF_COUNT2LAYOUT(c) means any channel layout with c channels, with a known
 * or unknown disposition.
 * The result is only valid inside AVFilterChannelLayouts and immediately
 * related functions.
 *)
//#define FF_COUNT2LAYOUT(c) (0x8000000000000000ULL | (c))

(**
 * Decode a channel count encoded as a channel layout.
 * Return 0 if the channel layout was a real one.
 *)
//#define FF_LAYOUT2COUNT(l) (((l) & 0x8000000000000000ULL) ? \
//                           (int)((l) & 0x7FFFFFFF) : 0)

(**
 * Check the formats/samplerates lists for compatibility for merging
 * without actually merging.
 *
 * @return 1 if they are compatible, 0 if not.
 *)
function ff_can_merge_formats(const a, b: PAVFilterFormats;
                         type_: TAVMediaType): Integer; cdecl; external AVFILTER_LIBNAME name _PU + 'ff_can_merge_formats';
function ff_can_merge_samplerates(const a, b: PAVFilterFormats): Integer; cdecl; external AVFILTER_LIBNAME name _PU + 'ff_can_merge_samplerates';

(**
 * Merge the formats/channel layouts/samplerates lists if they are compatible
 * and update all the references of a and b to point to the combined list and
 * free the old lists as needed. The combined list usually contains the
 * intersection of the lists of a and b.
 *
 * Both a and b must have owners (i.e. refcount > 0) for these functions.
 *
 * @return 1 if merging succeeded, 0 if a and b are incompatible
 *         and negative AVERROR code on failure.
 *         a and b are unmodified if 0 is returned.
 *)
function ff_merge_channel_layouts(a, b: PAVFilterChannelLayouts): Integer; cdecl; external AVFILTER_LIBNAME name _PU + 'ff_merge_channel_layouts';
function ff_merge_formats(a, b: PAVFilterFormats; type_: TAVMediaType): Integer; cdecl; external AVFILTER_LIBNAME name _PU + 'ff_merge_formats';
function ff_merge_samplerates(a, b: PAVFilterFormats): Integer; cdecl; external AVFILTER_LIBNAME name _PU + 'ff_merge_samplerates';

(**
 * Construct an empty AVFilterChannelLayouts/AVFilterFormats struct --
 * representing any channel layout (with known disposition)/sample rate.
 *)
function ff_all_channel_layouts: PAVFilterChannelLayouts; cdecl; external AVFILTER_LIBNAME name _PU + 'ff_all_channel_layouts';

function ff_all_samplerates: PAVFilterFormats; cdecl; external AVFILTER_LIBNAME name _PU + 'ff_all_samplerates';

(**
 * Construct an AVFilterChannelLayouts coding for any channel layout, with
 * known or unknown disposition.
 *)
function ff_all_channel_counts: PAVFilterChannelLayouts; cdecl; external AVFILTER_LIBNAME name _PU + 'ff_all_channel_counts';

function ff_make_format64_list(const fmts: PInt64): PAVFilterChannelLayouts; cdecl; external AVFILTER_LIBNAME name _PU + 'ff_make_format64_list';

{$IFDEF LIBAVFILTER_VERSION_MAJOR < 8}
function avfilter_make_format64_list(const fmts: PInt64): PAVFilterChannelLayouts; cdecl; external AVFILTER_LIBNAME name _PU + 'avfilter_make_format64_list';
{$ENDIF}

(**
 * A helper for query_formats() which sets all links to the same list of channel
 * layouts/sample rates. If there are no links hooked to this filter, the list
 * is freed.
 *)
function ff_set_common_channel_layouts(ctx: PAVFilterContext;
                                   layouts: PAVFilterChannelLayouts): Integer; cdecl; external AVFILTER_LIBNAME name _PU + 'ff_set_common_channel_layouts';
function ff_set_common_samplerates(ctx: PAVFilterContext;
                               samplerates: PAVFilterFormats): Integer; cdecl; external AVFILTER_LIBNAME name _PU + 'ff_set_common_samplerates';

(**
 * A helper for query_formats() which sets all links to the same list of
 * formats. If there are no links hooked to this filter, the list of formats is
 * freed.
 *)
function ff_set_common_formats(ctx: PAVFilterContext; formats: PAVFilterFormats): Integer; cdecl; external AVFILTER_LIBNAME name _PU + 'ff_set_common_formats';

function ff_add_channel_layout(l: PPAVFilterChannelLayouts; channel_layout: Int64): Integer; cdecl; external AVFILTER_LIBNAME name _PU + 'ff_add_channel_layout';

(**
 * Add *ref as a new reference to f.
 *)
function ff_channel_layouts_ref(f: PAVFilterChannelLayouts;
                            ref: PPAVFilterChannelLayouts): Integer; cdecl; external AVFILTER_LIBNAME name _PU + 'ff_channel_layouts_ref';

(**
 * Remove a reference to a channel layouts list.
 *)
procedure ff_channel_layouts_unref(ref: PPAVFilterChannelLayouts); cdecl; external AVFILTER_LIBNAME name _PU + 'ff_channel_layouts_unref';

procedure ff_channel_layouts_changeref(oldref, newref: PPAVFilterChannelLayouts); cdecl; external AVFILTER_LIBNAME name _PU + 'ff_channel_layouts_changeref';

function ff_default_query_formats(ctx: PAVFilterContext): Integer; cdecl; external AVFILTER_LIBNAME name _PU + 'ff_default_query_formats';

(**
 * Create a list of supported formats. This is intended for use in
 * AVFilter->query_formats().
 *
 * @param fmts list of media formats, terminated by -1
 * @return the format list, with no existing references
 *)
function ff_make_format_list(const fmts: PInteger): PAVFilterFormats; cdecl; external AVFILTER_LIBNAME name _PU + 'ff_make_format_list';

(**
 * Add fmt to the list of media formats contained in *avff.
 * If *avff is NULL the function allocates the filter formats struct
 * and puts its pointer in *avff.
 *
 * @return a non negative value in case of success, or a negative
 * value corresponding to an AVERROR code in case of error
 *)
function ff_add_format(avff: PPAVFilterFormats; fmt: Int64): Integer; cdecl; external AVFILTER_LIBNAME name _PU + 'ff_add_format';

(**
 * Return a list of all formats supported by FFmpeg for the given media type.
 *)
function ff_all_formats(ttype: TAVMediaType): PAVFilterFormats; cdecl; external AVFILTER_LIBNAME name _PU + 'ff_all_formats';

(**
 * Construct a formats list containing all pixel formats with certain
 * properties
 *)
function ff_formats_pixdesc_filter(rfmts: PPAVFilterFormats; want, rej: Cardinal): Integer; cdecl; external AVFILTER_LIBNAME name _PU + 'ff_formats_pixdesc_filter';

//* format is software, non-planar with sub-sampling
//#define FF_PIX_FMT_FLAG_SW_FLAT_SUB (1 << 24)

(**
 * Construct a formats list containing all planar sample formats.
 *)
function ff_planar_sample_fmts: PAVFilterFormats; cdecl; external AVFILTER_LIBNAME name _PU + 'ff_planar_sample_fmts';

(**
 * Add *ref as a new reference to formats.
 * That is the pointers will point like in the ascii art below:
 *   ________
 *  |formats |<--------.
 *  |  ____  |     ____|___________________
 *  | |refs| |    |  __|_
 *  | |* * | |    | |  | |  AVFilterLink
 *  | |* *--------->|*ref|
 *  | |____| |    | |____|
 *  |________|    |________________________
 *)
function ff_formats_ref(formats: PAVFilterFormats; ref: PPAVFilterFormats): Integer; cdecl; external AVFILTER_LIBNAME name _PU + 'ff_formats_ref';

(**
 * If *ref is non-NULL, remove *ref as a reference to the format list
 * it currently points to, deallocates that list if this was the last
 * reference, and sets *ref to NULL.
 *
 *         Before                                 After
 *   ________                               ________         NULL
 *  |formats |<--------.                   |formats |         ^
 *  |  ____  |     ____|________________   |  ____  |     ____|________________
 *  | |refs| |    |  __|_                  | |refs| |    |  __|_
 *  | |* * | |    | |  | |  AVFilterLink   | |* * | |    | |  | |  AVFilterLink
 *  | |* *--------->|*ref|                 | |*   | |    | |*ref|
 *  | |____| |    | |____|                 | |____| |    | |____|
 *  |________|    |_____________________   |________|    |_____________________
 *)
procedure ff_formats_unref(ref: PPAVFilterFormats); cdecl; external AVFILTER_LIBNAME name _PU + 'ff_formats_unref';

(**
 *         Before                                 After
 *   ________                         ________
 *  |formats |<---------.            |formats |<---------.
 *  |  ____  |       ___|___         |  ____  |       ___|___
 *  | |refs| |      |   |   |        | |refs| |      |   |   |   NULL
 *  | |* *--------->|*oldref|        | |* *--------->|*newref|     ^
 *  | |* * | |      |_______|        | |* * | |      |_______|  ___|___
 *  | |____| |                       | |____| |                |   |   |
 *  |________|                       |________|                |*oldref|
 *                                                             |_______|
 *)
procedure ff_formats_changeref(oldref, newref: PPAVFilterFormats); cdecl; external AVFILTER_LIBNAME name _PU + 'ff_formats_changeref';

(**
 * Check that fmts is a valid pixel formats list.
 *
 * In particular, check for duplicates.
 *)
function ff_formats_check_pixel_formats(log: Pointer; const fmts: PAVFilterFormats): Integer; cdecl; external AVFILTER_LIBNAME name _PU + 'ff_formats_check_pixel_formats';

(**
 * Check that fmts is a valid sample formats list.
 *
 * In particular, check for duplicates.
 *)
function ff_formats_check_sample_formats(log: Pointer; const fmts: PAVFilterFormats): Integer; cdecl; external AVFILTER_LIBNAME name _PU + 'ff_formats_check_sample_formats';

(**
 * Check that fmts is a valid sample rates list.
 *
 * In particular, check for duplicates.
 *)
function ff_formats_check_sample_rates(log: Pointer; const fmts: PAVFilterFormats): Integer; cdecl; external AVFILTER_LIBNAME name _PU + 'ff_formats_check_sample_rates';

(**
 * Check that fmts is a valid channel layouts list.
 *
 * In particular, check for duplicates.
 *)
function ff_formats_check_channel_layouts(log: Pointer; const fmts: PAVFilterChannelLayouts): Integer; cdecl; external AVFILTER_LIBNAME name _PU + 'ff_formats_check_channel_layouts';

implementation

end.
