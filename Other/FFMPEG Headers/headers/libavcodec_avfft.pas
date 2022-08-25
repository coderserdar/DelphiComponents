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

(**
 * @file
 * @ingroup lavc_fft
 * FFT functions
 *)

(*
 * FFVCL - Delphi FFmpeg VCL Components
 * http://www.DelphiFFmpeg.com
 *
 * Original file: libavcodec/avfft.h
 * Ported by CodeCoolie@CNSW 2010/03/14 -> $Date:: 2014-07-23 #$
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

unit libavcodec_avfft;

interface

{$I CompilerDefines.inc}

{$I libversion.inc}

(**
 * @defgroup lavc_fft FFT functions
 * @ingroup lavc_misc
 *
 * @{
 *)

type
  PFFTSample = ^TFFTSample;
  TFFTSample = Single;

  PFFTComplex = ^TFFTComplex;
  TFFTComplex = record
    re, im: TFFTSample;
  end;

  PFFTContext = ^TFFTContext;
  TFFTContext = record
    // need {$ALIGN 8}
    // defined in libavcodec/fft.h
  end;

(**
 * Set up a complex FFT.
 * @param nbits           log2 of the length of the input array
 * @param inverse         if 0 perform the forward transform, if 1 perform the inverse
 *)
function av_fft_init(nbits, inverse: Integer): PFFTContext; cdecl; external AVCODEC_LIBNAME name _PU + 'av_fft_init';

(**
 * Do the permutation needed BEFORE calling ff_fft_calc().
 *)
procedure av_fft_permute(s: PFFTContext; z: PFFTComplex); cdecl; external AVCODEC_LIBNAME name _PU + 'av_fft_permute';

(**
 * Do a complex FFT with the parameters defined in av_fft_init(). The
 * input data must be permuted before. No 1.0/sqrt(n) normalization is done.
 *)
procedure av_fft_calc(s: PFFTContext; z: PFFTComplex); cdecl; external AVCODEC_LIBNAME name _PU + 'av_fft_calc';

procedure av_fft_end(s: PFFTContext); cdecl; external AVCODEC_LIBNAME name _PU + 'av_fft_end';

function av_mdct_init(nbits, inverse: Integer; scale: Double): PFFTContext; cdecl; external AVCODEC_LIBNAME name _PU + 'av_mdct_init';
procedure av_imdct_calc(s: PFFTContext; output: PFFTSample; const input: PFFTSample); cdecl; external AVCODEC_LIBNAME name _PU + 'av_imdct_calc';
procedure av_imdct_half(s: PFFTContext; output: PFFTSample; const input: PFFTSample); cdecl; external AVCODEC_LIBNAME name _PU + 'av_imdct_half';
procedure av_mdct_calc(s: PFFTContext; output: PFFTSample; const input: PFFTSample); cdecl; external AVCODEC_LIBNAME name _PU + 'av_mdct_calc';
procedure av_mdct_end(s: PFFTContext); cdecl; external AVCODEC_LIBNAME name _PU + 'av_mdct_end';

(* Real Discrete Fourier Transform *)
type
  TRDFTransformType = (
      DFT_R2C,
      IDFT_C2R,
      IDFT_R2C,
      DFT_C2R
    );

  PRDFTContext = ^TRDFTContext;
  TRDFTContext = record
    // need {$ALIGN 8}
    // defined in libavcodec/rdft.h
  end;

(**
 * Set up a real FFT.
 * @param nbits           log2 of the length of the input array
 * @param trans           the type of transform
 *)
function av_rdft_init(nbits: Integer; trans: TRDFTransformType): PRDFTContext; cdecl; external AVCODEC_LIBNAME name _PU + 'av_rdft_init';
procedure av_rdft_calc(s: PRDFTContext; data: PFFTSample); cdecl; external AVCODEC_LIBNAME name _PU + 'av_rdft_calc';
procedure av_rdft_end(s: PRDFTContext); cdecl; external AVCODEC_LIBNAME name _PU + 'av_rdft_end';

(* Discrete Cosine Transform *)
type
  PDCTContext = ^TDCTContext;
  TDCTContext = record
    // need {$ALIGN 8}
    // defined in libavcodec/dct.h
  end;

  TDCTTransformType = (
    DCT_II = 0,
    DCT_III,
    DCT_I,
    DST_I
  );

(**
 * Set up DCT.
 *
 * @param nbits           size of the input array:
 *                        (1 << nbits)     for DCT-II, DCT-III and DST-I
 *                        (1 << nbits) + 1 for DCT-I
 * @param type            the type of transform
 *
 * @note the first element of the input of DST-I is ignored
 *)
function av_dct_init(nbits: Integer; ttype: TDCTTransformType): PDCTContext; cdecl; external AVCODEC_LIBNAME name _PU + 'av_dct_init';
procedure av_dct_calc(s: PDCTContext; data: PFFTSample); cdecl; external AVCODEC_LIBNAME name _PU + 'av_dct_calc';
procedure av_dct_end(s: PDCTContext); cdecl; external AVCODEC_LIBNAME name _PU + 'av_dct_end';

(**
 * @}
 *)

implementation

end.
