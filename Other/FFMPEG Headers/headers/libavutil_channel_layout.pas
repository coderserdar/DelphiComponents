(*
 * Copyright (c) 2006 Michael Niedermayer <michaelni@gmx.at>
 * Copyright (c) 2008 Peter Ross
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
 * audio channel layout utility functions
 *)

(*
 * FFVCL - Delphi FFmpeg VCL Components
 * http://www.DelphiFFmpeg.com
 *
 * Original file: libavcore/audioconvert.h
 * Ported by CodeCoolie@CNSW 2010/11/24 -> 2011-01-21
 * Original file: libavutil/audioconvert.h
 * Ported by CodeCoolie@CNSW 2011/07/02 -> 2013-01-19
 * Original file: libavutil/channel_layout.h
 * Ported by CodeCoolie@CNSW 2013/01/14 -> $Date:: 2021-04-25 #$
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

unit libavutil_channel_layout;

interface

{$I CompilerDefines.inc}

uses
{$IF Defined(BCB)}
  FFTypes,
{$IFEND}
  libavutil_bprint;

{$I libversion.inc}

(**
 * @addtogroup lavu_audio
 * @{
 *)

const
(**
 * @defgroup channel_masks Audio channel masks
 *
 * A channel layout is a 64-bits integer with a bit set for every channel.
 * The number of bits set must be equal to the number of channels.
 * The value 0 means that the channel layout is not known.
 * @note this data structure is not powerful enough to handle channels
 * combinations that have the same channel multiple times, such as
 * dual-mono.
 *
 * @{
 *)
  AV_CH_FRONT_LEFT             = $00000001;
  AV_CH_FRONT_RIGHT            = $00000002;
  AV_CH_FRONT_CENTER           = $00000004;
  AV_CH_LOW_FREQUENCY          = $00000008;
  AV_CH_BACK_LEFT              = $00000010;
  AV_CH_BACK_RIGHT             = $00000020;
  AV_CH_FRONT_LEFT_OF_CENTER   = $00000040;
  AV_CH_FRONT_RIGHT_OF_CENTER  = $00000080;
  AV_CH_BACK_CENTER            = $00000100;
  AV_CH_SIDE_LEFT              = $00000200;
  AV_CH_SIDE_RIGHT             = $00000400;
  AV_CH_TOP_CENTER             = $00000800;
  AV_CH_TOP_FRONT_LEFT         = $00001000;
  AV_CH_TOP_FRONT_CENTER       = $00002000;
  AV_CH_TOP_FRONT_RIGHT        = $00004000;
  AV_CH_TOP_BACK_LEFT          = $00008000;
  AV_CH_TOP_BACK_CENTER        = $00010000;
  AV_CH_TOP_BACK_RIGHT         = $00020000;
  AV_CH_STEREO_LEFT            = $20000000;  ///< Stereo downmix.
  AV_CH_STEREO_RIGHT           = $40000000;  ///< See AV_CH_STEREO_LEFT.
  AV_CH_WIDE_LEFT              = Int64($0000000080000000);
  AV_CH_WIDE_RIGHT             = Int64($0000000100000000);
  AV_CH_SURROUND_DIRECT_LEFT   = Int64($0000000200000000);
  AV_CH_SURROUND_DIRECT_RIGHT  = Int64($0000000400000000);
  AV_CH_LOW_FREQUENCY_2        = Int64($0000000800000000);
  AV_CH_TOP_SIDE_LEFT          = Int64($0000001000000000);
  AV_CH_TOP_SIDE_RIGHT         = Int64($0000002000000000);
  AV_CH_BOTTOM_FRONT_CENTER    = Int64($0000004000000000);
  AV_CH_BOTTOM_FRONT_LEFT      = Int64($0000008000000000);
  AV_CH_BOTTOM_FRONT_RIGHT     = Int64($0000010000000000);

(** Channel mask value used for AVCodecContext.request_channel_layout
    to indicate that the user requests the channel order of the decoder output
    to be the native codec channel order. *)
  AV_CH_LAYOUT_NATIVE          = Int64($8000000000000000);

(**
 * @}
 * @defgroup channel_mask_c Audio channel layouts
 * @{
 * *)
  AV_CH_LAYOUT_MONO            = (AV_CH_FRONT_CENTER);
  AV_CH_LAYOUT_STEREO          = (AV_CH_FRONT_LEFT or AV_CH_FRONT_RIGHT);
  AV_CH_LAYOUT_2POINT1         = (AV_CH_LAYOUT_STEREO or AV_CH_LOW_FREQUENCY);
  AV_CH_LAYOUT_2_1             = (AV_CH_LAYOUT_STEREO or AV_CH_BACK_CENTER);
  AV_CH_LAYOUT_SURROUND        = (AV_CH_LAYOUT_STEREO or AV_CH_FRONT_CENTER);
  AV_CH_LAYOUT_3POINT1         = (AV_CH_LAYOUT_SURROUND or AV_CH_LOW_FREQUENCY);
  AV_CH_LAYOUT_4POINT0         = (AV_CH_LAYOUT_SURROUND or AV_CH_BACK_CENTER);
  AV_CH_LAYOUT_4POINT1         = (AV_CH_LAYOUT_4POINT0 or AV_CH_LOW_FREQUENCY);
  AV_CH_LAYOUT_2_2             = (AV_CH_LAYOUT_STEREO or AV_CH_SIDE_LEFT or AV_CH_SIDE_RIGHT);
  AV_CH_LAYOUT_QUAD            = (AV_CH_LAYOUT_STEREO or AV_CH_BACK_LEFT or AV_CH_BACK_RIGHT);
  AV_CH_LAYOUT_5POINT0         = (AV_CH_LAYOUT_SURROUND or AV_CH_SIDE_LEFT or AV_CH_SIDE_RIGHT);
  AV_CH_LAYOUT_5POINT1         = (AV_CH_LAYOUT_5POINT0 or AV_CH_LOW_FREQUENCY);
  AV_CH_LAYOUT_5POINT0_BACK    = (AV_CH_LAYOUT_SURROUND or AV_CH_BACK_LEFT or AV_CH_BACK_RIGHT);
  AV_CH_LAYOUT_5POINT1_BACK    = (AV_CH_LAYOUT_5POINT0_BACK or AV_CH_LOW_FREQUENCY);
  AV_CH_LAYOUT_6POINT0         = (AV_CH_LAYOUT_5POINT0 or AV_CH_BACK_CENTER);
  AV_CH_LAYOUT_6POINT0_FRONT   = (AV_CH_LAYOUT_2_2 or AV_CH_FRONT_LEFT_OF_CENTER or AV_CH_FRONT_RIGHT_OF_CENTER);
  AV_CH_LAYOUT_HEXAGONAL       = (AV_CH_LAYOUT_5POINT0_BACK or AV_CH_BACK_CENTER);
  AV_CH_LAYOUT_6POINT1         = (AV_CH_LAYOUT_5POINT1 or AV_CH_BACK_CENTER);
  AV_CH_LAYOUT_6POINT1_BACK    = (AV_CH_LAYOUT_5POINT1_BACK or AV_CH_BACK_CENTER);
  AV_CH_LAYOUT_6POINT1_FRONT   = (AV_CH_LAYOUT_6POINT0_FRONT or AV_CH_LOW_FREQUENCY);
  AV_CH_LAYOUT_7POINT0         = (AV_CH_LAYOUT_5POINT0 or AV_CH_BACK_LEFT or AV_CH_BACK_RIGHT);
  AV_CH_LAYOUT_7POINT0_FRONT   = (AV_CH_LAYOUT_5POINT0 or AV_CH_FRONT_LEFT_OF_CENTER or AV_CH_FRONT_RIGHT_OF_CENTER);
  AV_CH_LAYOUT_7POINT1         = (AV_CH_LAYOUT_5POINT1 or AV_CH_BACK_LEFT or AV_CH_BACK_RIGHT);
  AV_CH_LAYOUT_7POINT1_WIDE    = (AV_CH_LAYOUT_5POINT1 or AV_CH_FRONT_LEFT_OF_CENTER or AV_CH_FRONT_RIGHT_OF_CENTER);
  AV_CH_LAYOUT_7POINT1_WIDE_BACK=(AV_CH_LAYOUT_5POINT1_BACK or AV_CH_FRONT_LEFT_OF_CENTER or AV_CH_FRONT_RIGHT_OF_CENTER);
  AV_CH_LAYOUT_OCTAGONAL       = (AV_CH_LAYOUT_5POINT0 or AV_CH_BACK_LEFT or AV_CH_BACK_CENTER or AV_CH_BACK_RIGHT);
  AV_CH_LAYOUT_HEXADECAGONAL   = (AV_CH_LAYOUT_OCTAGONAL or AV_CH_WIDE_LEFT or AV_CH_WIDE_RIGHT or AV_CH_TOP_BACK_LEFT or
    AV_CH_TOP_BACK_RIGHT or AV_CH_TOP_BACK_CENTER or AV_CH_TOP_FRONT_CENTER or AV_CH_TOP_FRONT_LEFT or AV_CH_TOP_FRONT_RIGHT);
  AV_CH_LAYOUT_STEREO_DOWNMIX  = (AV_CH_STEREO_LEFT or AV_CH_STEREO_RIGHT);
  AV_CH_LAYOUT_22POINT2        = (AV_CH_LAYOUT_5POINT1_BACK or AV_CH_FRONT_LEFT_OF_CENTER or AV_CH_FRONT_RIGHT_OF_CENTER or
    AV_CH_BACK_CENTER or AV_CH_LOW_FREQUENCY_2 or AV_CH_SIDE_LEFT or AV_CH_SIDE_RIGHT or AV_CH_TOP_FRONT_LEFT or
    AV_CH_TOP_FRONT_RIGHT or AV_CH_TOP_FRONT_CENTER or AV_CH_TOP_CENTER or AV_CH_TOP_BACK_LEFT or AV_CH_TOP_BACK_RIGHT or
    AV_CH_TOP_SIDE_LEFT or AV_CH_TOP_SIDE_RIGHT or AV_CH_TOP_BACK_CENTER or AV_CH_BOTTOM_FRONT_CENTER or
    AV_CH_BOTTOM_FRONT_LEFT or AV_CH_BOTTOM_FRONT_RIGHT);

type
  TAVMatrixEncoding = (
    AV_MATRIX_ENCODING_NONE,
    AV_MATRIX_ENCODING_DOLBY,
    AV_MATRIX_ENCODING_DPLII,
    AV_MATRIX_ENCODING_DPLIIX,
    AV_MATRIX_ENCODING_DPLIIZ,
    AV_MATRIX_ENCODING_DOLBYEX,
    AV_MATRIX_ENCODING_DOLBYHEADPHONE,
    AV_MATRIX_ENCODING_NB
  );

(**
 * Return a channel layout id that matches name, or 0 if no match is found.
 *
 * name can be one or several of the following notations,
 * separated by '+' or '|':
 * - the name of an usual channel layout (mono, stereo, 4.0, quad, 5.0,
 *   5.0(side), 5.1, 5.1(side), 7.1, 7.1(wide), downmix);
 * - the name of a single channel (FL, FR, FC, LFE, BL, BR, FLC, FRC, BC,
 *   SL, SR, TC, TFL, TFC, TFR, TBL, TBC, TBR, DL, DR);
 * - a number of channels, in decimal, followed by 'c', yielding
 *   the default channel layout for that number of channels (@see
 *   av_get_default_channel_layout);
 * - a channel layout mask, in hexadecimal starting with "0x" (see the
 *   AV_CH_* macros).
 *
 * Example: "stereo+FC" = "2c+FC" = "2c+1c" = "0x7"
 *)
function av_get_channel_layout(const name: PAnsiChar): Int64; cdecl; external AVUTIL_LIBNAME name _PU + 'av_get_channel_layout';

(**
 * Return a channel layout and the number of channels based on the specified name.
 *
 * This function is similar to (@see av_get_channel_layout), but can also parse
 * unknown channel layout specifications.
 *
 * @param[in]  name             channel layout specification string
 * @param[out] channel_layout   parsed channel layout (0 if unknown)
 * @param[out] nb_channels      number of channels
 *
 * @return 0 on success, AVERROR(EINVAL) if the parsing fails.
 *)
function av_get_extended_channel_layout(const name: PAnsiChar; channel_layout: PInt64; nb_channels: PInteger): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_get_extended_channel_layout';

(**
 * Return a description of a channel layout.
 * If nb_channels is <= 0, it is guessed from the channel_layout.
 *
 * @param buf put here the string containing the channel layout
 * @param buf_size size in bytes of the buffer
 *)
procedure av_get_channel_layout_string(buf: PAnsiChar; buf_size, nb_channels: Integer; channel_layout: Int64); cdecl; external AVUTIL_LIBNAME name _PU + 'av_get_channel_layout_string';

//  PAVBPrint = ^TAVBPrint;
//  TAVBPrint = record
    // need {$ALIGN 8}
    // defined in libavutil/bprint.h
//  end;

(**
 * Append a description of a channel layout to a bprint buffer.
 *)
procedure av_bprint_channel_layout(bp: PAVBPrint; nb_channels: Integer; channel_layout: Int64); cdecl; external AVUTIL_LIBNAME name _PU + 'av_bprint_channel_layout';

(**
 * Return the number of channels in the channel layout.
 *)
function av_get_channel_layout_nb_channels(channel_layout: Int64): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_get_channel_layout_nb_channels';

(**
 * Return default channel layout for a given number of channels.
 *)
function av_get_default_channel_layout(nb_channels: Integer): Int64; cdecl; external AVUTIL_LIBNAME name _PU + 'av_get_default_channel_layout';

(**
 * Get the index of a channel in channel_layout.
 *
 * @param channel a channel layout describing exactly one channel which must be
 *                present in channel_layout.
 *
 * @return index of channel in channel_layout on success, a negative AVERROR
 *         on error.
 *)
function av_get_channel_layout_channel_index(channel_layout, channel: Int64): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_get_channel_layout_channel_index';

(**
 * Get the channel with the given index in channel_layout.
 *)
function av_channel_layout_extract_channel(channel_layout: Int64; index: Integer): Int64; cdecl; external AVUTIL_LIBNAME name _PU + 'av_channel_layout_extract_channel';

(**
 * Get the name of a given channel.
 *
 * @return channel name on success, NULL on error.
 *)
function av_get_channel_name(channel: Int64): PAnsiChar; cdecl; external AVUTIL_LIBNAME name _PU + 'av_get_channel_name';

(**
 * Get the description of a given channel.
 *
 * @param channel  a channel layout with a single channel
 * @return  channel description on success, NULL on error
 *)
function av_get_channel_description(channel: Int64): PAnsiChar; cdecl; external AVUTIL_LIBNAME name _PU + 'av_get_channel_description';

(**
 * Get the value and name of a standard channel layout.
 *
 * @param[in]  index   index in an internal list, starting at 0
 * @param[out] layout  channel layout mask
 * @param[out] name    name of the layout
 * @return  0  if the layout exists,
 *          <0 if index is beyond the limits
 *)
function av_get_standard_channel_layout(index: Cardinal; layout: PInt64;
                                                  const name: PPAnsiChar): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_get_standard_channel_layout';

(**
 * @}
 * @}
 *)

implementation

end.
