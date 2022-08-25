(*
 * Copyright (c) 2014 Vittorio Giovara <vittorio.giovara@gmail.com>
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
 * Display matrix
 *)

(*
 * FFVCL - Delphi FFmpeg VCL Components
 * http://www.DelphiFFmpeg.com
 *
 * Original file: libavutil/display.h
 * Ported by CodeCoolie@CNSW 2015/08/14 -> $Date:: 2018-02-22 #$
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

unit libavutil_display;

interface

{$I CompilerDefines.inc}

{$I libversion.inc}

(**
 * @addtogroup lavu_video
 * @{
 *
 * @defgroup lavu_video_display Display transformation matrix functions
 * @{
 *)

(**
 * @addtogroup lavu_video_display
 * The display transformation matrix specifies an affine transformation that
 * should be applied to video frames for correct presentation. It is compatible
 * with the matrices stored in the ISO/IEC 14496-12 container format.
 *
 * The data is a 3x3 matrix represented as a 9-element array:
 *
 * @code{.unparsed}
 *                                  | a b u |
 *   (a, b, u, c, d, v, x, y, w) -> | c d v |
 *                                  | x y w |
 * @endcode
 *
 * All numbers are stored in native endianness, as 16.16 fixed-point values,
 * except for u, v and w, which are stored as 2.30 fixed-point values.
 *
 * The transformation maps a point (p, q) in the source (pre-transformation)
 * frame to the point (p', q') in the destination (post-transformation) frame as
 * follows:
 *
 * @code{.unparsed}
 *               | a b u |
 *   (p, q, 1) . | c d v | = z * (p', q', 1)
 *               | x y w |
 * @endcode
 *
 * The transformation can also be more explicitly written in components as
 * follows:
 *
 * @code{.unparsed}
 *   p' = (a * p + c * q + x) / z;
 *   q' = (b * p + d * q + y) / z;
 *   z  =  u * p + v * q + w
 * @endcode
 *)

(**
 * Extract the rotation component of the transformation matrix.
 *
 * @param matrix the transformation matrix
 * @return the angle (in degrees) by which the transformation rotates the frame
 *         counterclockwise. The angle will be in range [-180.0, 180.0],
 *         or NaN if the matrix is singular.
 *
 * @note floating point numbers are inherently inexact, so callers are
 *       recommended to round the return value to nearest integer before use.
 *)
function av_display_rotation_get(const matrix: PInteger{int32_t matrix[9]}): Double; cdecl; external AVUTIL_LIBNAME name _PU + 'av_display_rotation_get';

(**
 * Initialize a transformation matrix describing a pure counterclockwise
 * rotation by the specified angle (in degrees).
 *
 * @param matrix an allocated transformation matrix (will be fully overwritten
 *               by this function)
 * @param angle rotation angle in degrees.
 *)
procedure av_display_rotation_set(matrix: PInteger{int32_t matrix[9]}; angle: Double); cdecl; external AVUTIL_LIBNAME name _PU + 'av_display_rotation_set';

(**
 * Flip the input matrix horizontally and/or vertically.
 *
 * @param matrix an allocated transformation matrix
 * @param hflip whether the matrix should be flipped horizontally
 * @param vflip whether the matrix should be flipped vertically
 *)
procedure av_display_matrix_flip(matrix: PInteger{int32_t matrix[9]}; hflip, vflip: Integer); cdecl; external AVUTIL_LIBNAME name _PU + 'av_display_matrix_flip';

(**
 * @}
 * @}
 *)

implementation

end.
