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
 * Original file: libavutil/motion_vector.h
 * Ported by CodeCoolie@CNSW 2014/12/09 -> $Date:: 2016-03-28 #$
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

unit libavutil_motion_vector;

interface

{$I CompilerDefines.inc}

{$I libversion.inc}

type
  PAVMotionVector = ^TAVMotionVector;
  TAVMotionVector = record
    (**
     * Where the current macroblock comes from; negative value when it comes
     * from the past, positive value when it comes from the future.
     * XXX: set exact relative ref frame reference instead of a +/- 1 "direction".
     *)
    source: LongInt;
    (**
     * Width and height of the block.
     *)
    w, h: Byte;
    (**
     * Absolute source position. Can be outside the frame area.
     *)
    src_x, src_y: SmallInt;
    (**
     * Absolute destination position. Can be outside the frame area.
     *)
    dst_x, dst_y: SmallInt;
    (**
     * Extra flag information.
     * Currently unused.
     *)
    flags: Int64;
    (**
     * Motion vector
     * src_x = dst_x + motion_x / motion_scale
     * src_y = dst_y + motion_y / motion_scale
     *)
    motion_x, motion_y: LongInt;
    motion_scale: SmallInt;
  end;

implementation

end.
