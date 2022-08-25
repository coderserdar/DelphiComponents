(*
 * Audio FIFO
 * Copyright (c) 2012 Justin Ruggles <justin.ruggles@gmail.com>
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
 * Audio FIFO Buffer
 *)

(*
 * FFVCL - Delphi FFmpeg VCL Components
 * http://www.DelphiFFmpeg.com
 *
 * Original file: libavutil/audio_fifo.h
 * Ported by CodeCoolie@CNSW 2014/09/01 -> $Date:: 2016-08-29 #$
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

unit libavutil_audio_fifo;

interface

{$I CompilerDefines.inc}

uses
  libavutil_samplefmt;

{$I libversion.inc}

(**
 * @addtogroup lavu_audio
 * @{
 *
 * @defgroup lavu_audiofifo Audio FIFO Buffer
 * @{
 *)

(**
 * Context for an Audio FIFO Buffer.
 *
 * - Operates at the sample level rather than the byte level.
 * - Supports multiple channels with either planar or packed sample format.
 * - Automatic reallocation when writing to a full buffer.
 *)
type
  PAVAudioFifo = ^TAVAudioFifo;
  TAVAudioFifo = record
    // need {$ALIGN 8}
    // defined in libavutil/audio_fifo.c
  end;

(**
 * Free an AVAudioFifo.
 *
 * @param af  AVAudioFifo to free
 *)
procedure av_audio_fifo_free(af: PAVAudioFifo); cdecl; external AVUTIL_LIBNAME name _PU + 'av_audio_fifo_free';

(**
 * Allocate an AVAudioFifo.
 *
 * @param sample_fmt  sample format
 * @param channels    number of channels
 * @param nb_samples  initial allocation size, in samples
 * @return            newly allocated AVAudioFifo, or NULL on error
 *)
function av_audio_fifo_alloc(sample_fmt: TAVSampleFormat; channels, nb_samples: Integer): PAVAudioFifo; cdecl; external AVUTIL_LIBNAME name _PU + 'av_audio_fifo_alloc';

(**
 * Reallocate an AVAudioFifo.
 *
 * @param af          AVAudioFifo to reallocate
 * @param nb_samples  new allocation size, in samples
 * @return            0 if OK, or negative AVERROR code on failure
 *)
function av_audio_fifo_realloc(af: PAVAudioFifo; nb_samples: Integer): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_audio_fifo_realloc';

(**
 * Write data to an AVAudioFifo.
 *
 * The AVAudioFifo will be reallocated automatically if the available space
 * is less than nb_samples.
 *
 * @see enum AVSampleFormat
 * The documentation for AVSampleFormat describes the data layout.
 *
 * @param af          AVAudioFifo to write to
 * @param data        audio data plane pointers
 * @param nb_samples  number of samples to write
 * @return            number of samples actually written, or negative AVERROR
 *                    code on failure. If successful, the number of samples
 *                    actually written will always be nb_samples.
 *)
function av_audio_fifo_write(af: PAVAudioFifo; data: PPointer; nb_samples: Integer): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_audio_fifo_write';

(**
 * Peek data from an AVAudioFifo.
 *
 * @see enum AVSampleFormat
 * The documentation for AVSampleFormat describes the data layout.
 *
 * @param af          AVAudioFifo to read from
 * @param data        audio data plane pointers
 * @param nb_samples  number of samples to peek
 * @return            number of samples actually peek, or negative AVERROR code
 *                    on failure. The number of samples actually peek will not
 *                    be greater than nb_samples, and will only be less than
 *                    nb_samples if av_audio_fifo_size is less than nb_samples.
 *)
function av_audio_fifo_peek(af: PAVAudioFifo; data: PPointer; nb_samples: Integer): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_audio_fifo_peek';

(**
 * Peek data from an AVAudioFifo.
 *
 * @see enum AVSampleFormat
 * The documentation for AVSampleFormat describes the data layout.
 *
 * @param af          AVAudioFifo to read from
 * @param data        audio data plane pointers
 * @param nb_samples  number of samples to peek
 * @param offset      offset from current read position
 * @return            number of samples actually peek, or negative AVERROR code
 *                    on failure. The number of samples actually peek will not
 *                    be greater than nb_samples, and will only be less than
 *                    nb_samples if av_audio_fifo_size is less than nb_samples.
 *)
function av_audio_fifo_peek_at(af: PAVAudioFifo; data: Pointer; nb_samples, offset: Integer): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_audio_fifo_peek_at';

(**
 * Read data from an AVAudioFifo.
 *
 * @see enum AVSampleFormat
 * The documentation for AVSampleFormat describes the data layout.
 *
 * @param af          AVAudioFifo to read from
 * @param data        audio data plane pointers
 * @param nb_samples  number of samples to read
 * @return            number of samples actually read, or negative AVERROR code
 *                    on failure. The number of samples actually read will not
 *                    be greater than nb_samples, and will only be less than
 *                    nb_samples if av_audio_fifo_size is less than nb_samples.
 *)
function av_audio_fifo_read(af: PAVAudioFifo; data: PPointer; nb_samples: Integer): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_audio_fifo_read';

(**
 * Drain data from an AVAudioFifo.
 *
 * Removes the data without reading it.
 *
 * @param af          AVAudioFifo to drain
 * @param nb_samples  number of samples to drain
 * @return            0 if OK, or negative AVERROR code on failure
 *)
function av_audio_fifo_drain(af: PAVAudioFifo; nb_samples: Integer): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_audio_fifo_drain';

(**
 * Reset the AVAudioFifo buffer.
 *
 * This empties all data in the buffer.
 *
 * @param af  AVAudioFifo to reset
 *)
procedure av_audio_fifo_reset(af: PAVAudioFifo); cdecl; external AVUTIL_LIBNAME name _PU + 'av_audio_fifo_reset';

(**
 * Get the current number of samples in the AVAudioFifo available for reading.
 *
 * @param af  the AVAudioFifo to query
 * @return    number of samples available for reading
 *)
function av_audio_fifo_size(af: PAVAudioFifo): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_audio_fifo_size';

(**
 * Get the current number of samples in the AVAudioFifo available for writing.
 *
 * @param af  the AVAudioFifo to query
 * @return    number of samples available for writing
 *)
function av_audio_fifo_space(af: PAVAudioFifo): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_audio_fifo_space';

(**
 * @}
 * @}
 *)

implementation

end.
