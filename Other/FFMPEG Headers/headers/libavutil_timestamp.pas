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
 * timestamp utils, mostly useful for debugging/logging purposes
 *)

(*
 * FFVCL - Delphi FFmpeg VCL Components
 * http://www.DelphiFFmpeg.com
 *
 * Original file: libavutil/timestamp.h
 * Ported by CodeCoolie@CNSW 2014/07/21 -> $Date:: 2017-02-09 #$
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

unit libavutil_timestamp;

interface

{$I CompilerDefines.inc}

uses
{$IFDEF VCL_XE2_OR_ABOVE}
  System.SysUtils,
{$ELSE}
  SysUtils,
{$ENDIF}
  libavutil,
  libavutil_rational;

{$I libversion.inc}


//#define AV_TS_MAX_STRING_SIZE 32

(**
 * Fill the provided buffer with a string containing a timestamp
 * representation.
 *
 * @param buf a buffer with size in bytes of at least AV_TS_MAX_STRING_SIZE
 * @param ts the timestamp to represent
 * @return the buffer in input
 *)
function av_ts_make_string(ts: Int64): string; {$IFDEF USE_INLINE}inline;{$ENDIF}
{
    if (ts == AV_NOPTS_VALUE) snprintf(buf, AV_TS_MAX_STRING_SIZE, "NOPTS");
    else                      snprintf(buf, AV_TS_MAX_STRING_SIZE, "%" PRId64, ts);
    return buf;
}

(**
 * Convenience macro, the return value should be used only directly in
 * function arguments but never stand-alone.
 *)
//#define av_ts2str(ts) av_ts_make_string((char[AV_TS_MAX_STRING_SIZE]){0}, ts)
function av_ts2str(ts: Int64): string; {$IFDEF USE_INLINE}inline;{$ENDIF}

(**
 * Fill the provided buffer with a string containing a timestamp time
 * representation.
 *
 * @param buf a buffer with size in bytes of at least AV_TS_MAX_STRING_SIZE
 * @param ts the timestamp to represent
 * @param tb the timebase of the timestamp
 * @return the buffer in input
 *)
function av_ts_make_time_string(ts: Int64; tb: PAVRational): string; {$IFDEF USE_INLINE}inline;{$ENDIF}
{
    if (ts == AV_NOPTS_VALUE) snprintf(buf, AV_TS_MAX_STRING_SIZE, "NOPTS");
    else                      snprintf(buf, AV_TS_MAX_STRING_SIZE, "%.6g", av_q2d(*tb) * ts);
    return buf;
}

(**
 * Convenience macro, the return value should be used only directly in
 * function arguments but never stand-alone.
 *)
//#define av_ts2timestr(ts, tb) av_ts_make_time_string((char[AV_TS_MAX_STRING_SIZE]){0}, ts, tb)
function av_ts2timestr(ts: Int64; tb: PAVRational): string; {$IFDEF USE_INLINE}inline;{$ENDIF}

implementation

function av_ts_make_string(ts: Int64): string;
begin
  if ts = AV_NOPTS_VALUE then
    Result := 'NOPTS'
  else
    Result := IntToStr(ts);
end;

function av_ts2str(ts: Int64): string;
begin
  if ts = AV_NOPTS_VALUE then
    Result := 'NOPTS'
  else
    Result := IntToStr(ts);
end;

function av_ts_make_time_string(ts: Int64; tb: PAVRational): string;
begin
  if ts = AV_NOPTS_VALUE then
    Result := 'NOPTS'
  else
    Result := Format('%.6g', [av_q2d(tb^) * ts]);
end;

function av_ts2timestr(ts: Int64; tb: PAVRational): string;
begin
  if ts = AV_NOPTS_VALUE then
    Result := 'NOPTS'
  else
    Result := Format('%.6g', [av_q2d(tb^) * ts]);
end;

end.
