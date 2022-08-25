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
 * Original file: libavcore/samplefmt.h
 * Ported by CodeCoolie@CNSW 2010/11/24 -> 2010-11-30
 * Original file: libavutil/samplefmt.h
 * Ported by CodeCoolie@CNSW 2011/07/02 -> $Date:: 2018-05-22 #$
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

unit libavutil_samplefmt;

interface

{$I CompilerDefines.inc}

uses
  FFTypes;

{$I libversion.inc}

type
(**
 * @addtogroup lavu_audio
 * @{
 *
 * @defgroup lavu_sampfmts Audio sample formats
 *
 * Audio sample format enumeration and related convenience functions.
 * @{
 *)

(**
 * Audio sample formats
 *
 * - The data described by the sample format is always in native-endian order.
 *   Sample values can be expressed by native C types, hence the lack of a signed
 *   24-bit sample format even though it is a common raw audio data format.
 *
 * - The floating-point formats are based on full volume being in the range
 *   [-1.0, 1.0]. Any values outside this range are beyond full volume level.
 *
 * - The data layout as used in av_samples_fill_arrays() and elsewhere in FFmpeg
 *   (such as AVFrame in libavcodec) is as follows:
 *
 * @par
 * For planar sample formats, each audio channel is in a separate data plane,
 * and linesize is the buffer size, in bytes, for a single plane. All data
 * planes must be the same size. For packed sample formats, only the first data
 * plane is used, and samples for each channel are interleaved. In this case,
 * linesize is the buffer size, in bytes, for the 1 plane.
 *
 *)
  PAVSampleFormat = ^TAVSampleFormat;
  TAVSampleFormat = (
    AV_SAMPLE_FMT_NONE = -1,
    AV_SAMPLE_FMT_U8,          ///< unsigned 8 bits
    AV_SAMPLE_FMT_S16,         ///< signed 16 bits
    AV_SAMPLE_FMT_S32,         ///< signed 32 bits
    AV_SAMPLE_FMT_FLT,         ///< float
    AV_SAMPLE_FMT_DBL,         ///< double

    AV_SAMPLE_FMT_U8P,         ///< unsigned 8 bits, planar
    AV_SAMPLE_FMT_S16P,        ///< signed 16 bits, planar
    AV_SAMPLE_FMT_S32P,        ///< signed 32 bits, planar
    AV_SAMPLE_FMT_FLTP,        ///< float, planar
    AV_SAMPLE_FMT_DBLP,        ///< double, planar
    AV_SAMPLE_FMT_S64,         ///< signed 64 bits
    AV_SAMPLE_FMT_S64P,        ///< signed 64 bits, planar

    AV_SAMPLE_FMT_NB           ///< Number of sample formats. DO NOT USE if linking dynamically
  );

(**
 * Return the name of sample_fmt, or NULL if sample_fmt is not
 * recognized.
 *)
function av_get_sample_fmt_name(sample_fmt: TAVSampleFormat): PAnsiChar; cdecl; external AVUTIL_LIBNAME name _PU + 'av_get_sample_fmt_name';

(**
 * Return a sample format corresponding to name, or AV_SAMPLE_FMT_NONE
 * on error.
 *)
function av_get_sample_fmt(const name: PAnsiChar): TAVSampleFormat; cdecl; external AVUTIL_LIBNAME name _PU + 'av_get_sample_fmt';

(**
 * Return the planar<->packed alternative form of the given sample format, or
 * AV_SAMPLE_FMT_NONE on error. If the passed sample_fmt is already in the
 * requested planar/packed format, the format returned is the same as the
 * input.
 *)
function av_get_alt_sample_fmt(sample_fmt: TAVSampleFormat; planar: Integer): TAVSampleFormat; cdecl; external AVUTIL_LIBNAME name _PU + 'av_get_alt_sample_fmt';

(**
 * Get the packed alternative form of the given sample format.
 *
 * If the passed sample_fmt is already in packed format, the format returned is
 * the same as the input.
 *
 * @return  the packed alternative form of the given sample format or
            AV_SAMPLE_FMT_NONE on error.
 *)
function av_get_packed_sample_fmt(sample_fmt: TAVSampleFormat): TAVSampleFormat; cdecl; external AVUTIL_LIBNAME name _PU + 'av_get_packed_sample_fmt';

(**
 * Get the planar alternative form of the given sample format.
 *
 * If the passed sample_fmt is already in planar format, the format returned is
 * the same as the input.
 *
 * @return  the planar alternative form of the given sample format or
            AV_SAMPLE_FMT_NONE on error.
 *)
function av_get_planar_sample_fmt(sample_fmt: TAVSampleFormat): TAVSampleFormat; cdecl; external AVUTIL_LIBNAME name _PU + 'av_get_planar_sample_fmt';

(**
 * Generate a string corresponding to the sample format with
 * sample_fmt, or a header if sample_fmt is negative.
 *
 * @param buf the buffer where to write the string
 * @param buf_size the size of buf
 * @param sample_fmt the number of the sample format to print the
 * corresponding info string, or a negative value to print the
 * corresponding header.
 * @return the pointer to the filled buffer or NULL if sample_fmt is
 * unknown or in case of other errors
 *)
function av_get_sample_fmt_string(buf: PAnsiChar; buf_size: Integer; sample_fmt: TAVSampleFormat): PAnsiChar; cdecl; external AVUTIL_LIBNAME name _PU + 'av_get_sample_fmt_string';

(**
 * Return number of bytes per sample.
 *
 * @param sample_fmt the sample format
 * @return number of bytes per sample or zero if unknown for the given
 * sample format
 *)
function av_get_bytes_per_sample(sample_fmt: TAVSampleFormat): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_get_bytes_per_sample';

(**
 * Check if the sample format is planar.
 *
 * @param sample_fmt the sample format to inspect
 * @return 1 if the sample format is planar, 0 if it is interleaved
 *)
function av_sample_fmt_is_planar(sample_fmt: TAVSampleFormat): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_sample_fmt_is_planar';

(**
 * Get the required buffer size for the given audio parameters.
 *
 * @param[out] linesize calculated linesize, may be NULL
 * @param nb_channels   the number of channels
 * @param nb_samples    the number of samples in a single channel
 * @param sample_fmt    the sample format
 * @param align         buffer size alignment (0 = default, 1 = no alignment)
 * @return              required buffer size, or negative error code on failure
 *)
function av_samples_get_buffer_size(linesize: PInteger; nb_channels, nb_samples: Integer;
                               sample_fmt: TAVSampleFormat; align: Integer): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_samples_get_buffer_size';

(**
 * @}
 *
 * @defgroup lavu_sampmanip Samples manipulation
 *
 * Functions that manipulate audio samples
 * @{
 *)

(**
 * Fill plane data pointers and linesize for samples with sample
 * format sample_fmt.
 *
 * The audio_data array is filled with the pointers to the samples data planes:
 * for planar, set the start point of each channel's data within the buffer,
 * for packed, set the start point of the entire buffer only.
 *
 * The value pointed to by linesize is set to the aligned size of each
 * channel's data buffer for planar layout, or to the aligned size of the
 * buffer for all channels for packed layout.
 *
 * The buffer in buf must be big enough to contain all the samples
 * (use av_samples_get_buffer_size() to compute its minimum size),
 * otherwise the audio_data pointers will point to invalid data.
 *
 * @see enum AVSampleFormat
 * The documentation for AVSampleFormat describes the data layout.
 *
 * @param[out] audio_data  array to be filled with the pointer for each channel
 * @param[out] linesize    calculated linesize, may be NULL
 * @param buf              the pointer to a buffer containing the samples
 * @param nb_channels      the number of channels
 * @param nb_samples       the number of samples in a single channel
 * @param sample_fmt       the sample format
 * @param align            buffer size alignment (0 = default, 1 = no alignment)
 * @return                 >=0 on success or a negative error code on failure
 * @todo return minimum size in bytes required for the buffer in case
 * of success at the next bump
 *)
function av_samples_fill_arrays(audio_data: PPByte; linesize: PInteger;
                           const buf: PByte; nb_channels, nb_samples: Integer;
                           sample_fmt: TAVSampleFormat; align: Integer): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_samples_fill_arrays';

(**
 * Allocate a samples buffer for nb_samples samples, and fill data pointers and
 * linesize accordingly.
 * The allocated samples buffer can be freed by using av_freep(&audio_data[0])
 * Allocated data will be initialized to silence.
 *
 * @see enum AVSampleFormat
 * The documentation for AVSampleFormat describes the data layout.
 *
 * @param[out] audio_data  array to be filled with the pointer for each channel
 * @param[out] linesize    aligned size for audio buffer(s), may be NULL
 * @param nb_channels      number of audio channels
 * @param nb_samples       number of samples per channel
 * @param align            buffer size alignment (0 = default, 1 = no alignment)
 * @return                 >=0 on success or a negative error code on failure
 * @todo return the size of the allocated buffer in case of success at the next bump
 * @see av_samples_fill_arrays()
 * @see av_samples_alloc_array_and_samples()
 *)
function av_samples_alloc(audio_data: PPByte; linesize: PInteger;
                     nb_channels, nb_samples: Integer;
                     sample_fmt: TAVSampleFormat; align: Integer): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_samples_alloc';

(**
 * Allocate a data pointers array, samples buffer for nb_samples
 * samples, and fill data pointers and linesize accordingly.
 *
 * This is the same as av_samples_alloc(), but also allocates the data
 * pointers array.
 *
 * @see av_samples_alloc()
 *)
function av_samples_alloc_array_and_samples(audio_data: PPPByte; linesize: PInteger; nb_channels: Integer;
                                       nb_samples: Integer; sample_fmt: TAVSampleFormat; align: Integer): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_samples_alloc_array_and_samples';

(**
 * Copy samples from src to dst.
 *
 * @param dst destination array of pointers to data planes
 * @param src source array of pointers to data planes
 * @param dst_offset offset in samples at which the data will be written to dst
 * @param src_offset offset in samples at which the data will be read from src
 * @param nb_samples number of samples to be copied
 * @param nb_channels number of audio channels
 * @param sample_fmt audio sample format
 *)
function av_samples_copy(dst: PPByte; const src: PPByte;
                    dst_offset, src_offset, nb_samples, nb_channels: Integer;
                    sample_fmt: TAVSampleFormat): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_samples_copy';

(**
 * Fill an audio buffer with silence.
 *
 * @param audio_data  array of pointers to data planes
 * @param offset      offset in samples at which to start filling
 * @param nb_samples  number of samples to fill
 * @param nb_channels number of audio channels
 * @param sample_fmt  audio sample format
 *)
function av_samples_set_silence(audio_data: PPByte;
                            offset, nb_samples, nb_channels: Integer;
                            sample_fmt: TAVSampleFormat): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_samples_set_silence';

(**
 * @}
 * @}
 *)

implementation

end.
