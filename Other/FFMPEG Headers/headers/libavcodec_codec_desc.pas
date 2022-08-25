(*
 * Codec descriptors public API
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
 * Original file: libavcodec/codec_desc.h
 * Ported by CodeCoolie@CNSW 2020/11/11 -> $Date:: 2021-04-18 #$
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

unit libavcodec_codec_desc;

interface

{$I CompilerDefines.inc}

uses
  libavcodec_codec_id,
  libavutil;

{$I libversion.inc}

(**
 * @addtogroup lavc_core
 * @{
 *)

(**
 * This struct describes the properties of a single codec described by an
 * AVCodecID.
 * @see avcodec_descriptor_get()
 *)
type
  PAVCodecDescriptor = ^TAVCodecDescriptor;
  TAVCodecDescriptor = record
    id: TAVCodecID;
    type_: TAVMediaType;
    (**
     * Name of the codec described by this descriptor. It is non-empty and
     * unique for each codec descriptor. It should contain alphanumeric
     * characters and '_' only.
     *)
    name: PAnsiChar;
    (**
     * A more descriptive name for this codec. May be NULL.
     *)
    long_name: PAnsiChar;
    (**
     * Codec properties, a combination of AV_CODEC_PROP_* flags.
     *)
    props: Integer;
    (**
     * MIME type(s) associated with the codec.
     * May be NULL; if not, a NULL-terminated array of MIME types.
     * The first item is always non-NULL and is the preferred MIME type.
     *)
    //const char *const *mime_types;
    mime_types: PPAnsiChar;
    (**
     * If non-NULL, an array of profiles recognized for this codec.
     * Terminated with FF_PROFILE_UNKNOWN.
     *)
    profiles: Pointer; // PAVProfile;
  end;

const
(**
 * Codec uses only intra compression.
 * Video and audio codecs only.
 *)
  AV_CODEC_PROP_INTRA_ONLY    = (1 shl 0);
(**
 * Codec supports lossy compression. Audio and video codecs only.
 * @note a codec may support both lossy and lossless
 * compression modes
 *)
  AV_CODEC_PROP_LOSSY         = (1 shl 1);
(**
 * Codec supports lossless compression. Audio and video codecs only.
 *)
  AV_CODEC_PROP_LOSSLESS      = (1 shl 2);
(**
 * Codec supports frame reordering. That is, the coded order (the order in which
 * the encoded packets are output by the encoders / stored / input to the
 * decoders) may be different from the presentation order of the corresponding
 * frames.
 *
 * For codecs that do not have this property set, PTS and DTS should always be
 * equal.
 *)
  AV_CODEC_PROP_REORDER       = (1 shl 3);
(**
 * Subtitle codec is bitmap based
 * Decoded AVSubtitle data can be read from the AVSubtitleRect->pict field.
 *)
  AV_CODEC_PROP_BITMAP_SUB    = (1 shl 16);
(**
 * Subtitle codec is text based.
 * Decoded AVSubtitle data can be read from the AVSubtitleRect->ass field.
 *)
  AV_CODEC_PROP_TEXT_SUB      = (1 shl 17);

(**
 * @return descriptor for given codec ID or NULL if no descriptor exists.
 *)
function avcodec_descriptor_get(id: TAVCodecID): PAVCodecDescriptor; cdecl; external AVCODEC_LIBNAME name _PU + 'avcodec_descriptor_get';

(**
 * Iterate over all codec descriptors known to libavcodec.
 *
 * @param prev previous descriptor. NULL to get the first descriptor.
 *
 * @return next descriptor or NULL after the last descriptor
 *)
function avcodec_descriptor_next(const prev: PAVCodecDescriptor): PAVCodecDescriptor; cdecl; external AVCODEC_LIBNAME name _PU + 'avcodec_descriptor_next';

(**
 * @return codec descriptor with the given name or NULL if no such descriptor
 *         exists.
 *)
function avcodec_descriptor_get_by_name(const name: PAnsiChar): PAVCodecDescriptor; cdecl; external AVCODEC_LIBNAME name _PU + 'avcodec_descriptor_get_by_name';

(**
 * @}
 *)

implementation

end.
