(*
 * Codec parameters public API
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
 * Original file: libavcodec/codec_par.h
 * Ported by CodeCoolie@CNSW 2020/11/11 -> $Date:: 2021-03-03 #$
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

unit libavcodec_codec_par;

interface

{$I CompilerDefines.inc}

uses
  libavcodec_codec_id,
  libavutil,
  libavutil_pixfmt,
  libavutil_rational;

{$I libversion.inc}

(**
 * @addtogroup lavc_core
 *)
type
  TAVFieldOrder = (
    AV_FIELD_UNKNOWN,
    AV_FIELD_PROGRESSIVE,
    AV_FIELD_TT,          //< Top coded_first, top displayed first
    AV_FIELD_BB,          //< Bottom coded first, bottom displayed first
    AV_FIELD_TB,          //< Top coded first, bottom displayed first
    AV_FIELD_BT           //< Bottom coded first, top displayed first
  );

(**
 * This struct describes the properties of an encoded stream.
 *
 * sizeof(AVCodecParameters) is not a part of the public ABI, this struct must
 * be allocated with avcodec_parameters_alloc() and freed with
 * avcodec_parameters_free().
 *)
  PPAVCodecParameters = ^PAVCodecParameters;
  PAVCodecParameters = ^TAVCodecParameters;
  TAVCodecParameters = record
    (**
     * General type of the encoded data.
     *)
    codec_type: TAVMediaType;
    (**
     * Specific type of the encoded data (the codec used).
     *)
    codec_id: TAVCodecID;
    (**
     * Additional information about the codec (corresponds to the AVI FOURCC).
     *)
    //unsigned int codec_tag;
    //codec_tag: Cardinal;
    //codec_tag: array[0..3] of AnsiChar;
    codec_tag: packed record
      case Integer of
        0: (tag: Cardinal);
        1: (fourcc: array[0..3] of AnsiChar);
        2: (fourbb: array[0..3] of Byte);
      end;

    (**
     * Extra binary data needed for initializing the decoder, codec-dependent.
     *
     * Must be allocated with av_malloc() and will be freed by
     * avcodec_parameters_free(). The allocated size of extradata must be at
     * least extradata_size + AV_INPUT_BUFFER_PADDING_SIZE, with the padding
     * bytes zeroed.
     *)
    extradata: PByte;
    (**
     * Size of the extradata content in bytes.
     *)
    extradata_size: Integer;

    (**
     * - video: the pixel format, the value corresponds to enum AVPixelFormat.
     * - audio: the sample format, the value corresponds to enum AVSampleFormat.
     *)
    format: Integer;

    (**
     * The average bitrate of the encoded data (in bits per second).
     *)
    bit_rate: Int64;

    (**
     * The number of bits per sample in the codedwords.
     *
     * This is basically the bitrate per sample. It is mandatory for a bunch of
     * formats to actually decode them. It's the number of bits for one sample in
     * the actual coded bitstream.
     *
     * This could be for example 4 for ADPCM
     * For PCM formats this matches bits_per_raw_sample
     * Can be 0
     *)
    bits_per_coded_sample: Integer;

    (**
     * This is the number of valid bits in each output sample. If the
     * sample format has more bits, the least significant bits are additional
     * padding bits, which are always 0. Use right shifts to reduce the sample
     * to its actual size. For example, audio formats with 24 bit samples will
     * have bits_per_raw_sample set to 24, and format set to AV_SAMPLE_FMT_S32.
     * To get the original sample use "(int32_t)sample >> 8"."
     *
     * For ADPCM this might be 12 or 16 or similar
     * Can be 0
     *)
    bits_per_raw_sample: Integer;

    (**
     * Codec-specific bitstream restrictions that the stream conforms to.
     *)
    profile: Integer;
    level: Integer;

    (**
     * Video only. The dimensions of the video frame in pixels.
     *)
    width: Integer;
    height: Integer;

    (**
     * Video only. The aspect ratio (width / height) which a single pixel
     * should have when displayed.
     *
     * When the aspect ratio is unknown / undefined, the numerator should be
     * set to 0 (the denominator may have any value).
     *)
    sample_aspect_ratio: TAVRational;

    (**
     * Video only. The order of the fields in interlaced video.
     *)
    field_order: TAVFieldOrder;

    (**
     * Video only. Additional colorspace characteristics.
     *)
    color_range: TAVColorRange;
    color_primaries: TAVColorPrimaries;
    color_trc: TAVColorTransferCharacteristic;
    color_space: TAVColorSpace;
    chroma_location: TAVChromaLocation;

    (**
     * Video only. Number of delayed frames.
     *)
    video_delay: Integer;

    (**
     * Audio only. The channel layout bitmask. May be 0 if the channel layout is
     * unknown or unspecified, otherwise the number of bits set must be equal to
     * the channels field.
     *)
    channel_layout: Int64;
    (**
     * Audio only. The number of audio channels.
     *)
    channels: Integer;
    (**
     * Audio only. The number of audio samples per second.
     *)
    sample_rate: Integer;
    (**
     * Audio only. The number of bytes per coded audio frame, required by some
     * formats.
     *
     * Corresponds to nBlockAlign in WAVEFORMATEX.
     *)
    block_align: Integer;
    (**
     * Audio only. Audio frame size, if known. Required by some formats to be static.
     *)
    frame_size: Integer;

    (**
     * Audio only. The amount of padding (in samples) inserted by the encoder at
     * the beginning of the audio. I.e. this number of leading decoded samples
     * must be discarded by the caller to get the original audio without leading
     * padding.
     *)
    initial_padding: Integer;
    (**
     * Audio only. The amount of padding (in samples) appended by the encoder to
     * the end of the audio. I.e. this number of decoded samples must be
     * discarded by the caller from the end of the stream to get the original
     * audio without any trailing padding.
     *)
    trailing_padding: Integer;
    (**
     * Audio only. Number of samples to skip after a discontinuity.
     *)
    seek_preroll: Integer;
  end;

(**
 * Allocate a new AVCodecParameters and set its fields to default values
 * (unknown/invalid/0). The returned struct must be freed with
 * avcodec_parameters_free().
 *)
function avcodec_parameters_alloc: PAVCodecParameters; cdecl; external AVCODEC_LIBNAME name _PU + 'avcodec_parameters_alloc';

(**
 * Free an AVCodecParameters instance and everything associated with it and
 * write NULL to the supplied pointer.
 *)
procedure avcodec_parameters_free(par: PPAVCodecParameters); cdecl; external AVCODEC_LIBNAME name _PU + 'avcodec_parameters_free';

(**
 * Copy the contents of src to dst. Any allocated fields in dst are freed and
 * replaced with newly allocated duplicates of the corresponding fields in src.
 *
 * @return >= 0 on success, a negative AVERROR code on failure.
 *)
function avcodec_parameters_copy(dst: PAVCodecParameters; const src: PAVCodecParameters): Integer; cdecl; external AVCODEC_LIBNAME name _PU + 'avcodec_parameters_copy';

(**
 * @}
 *)

implementation

end.
