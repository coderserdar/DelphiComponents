(*
 * Copyright (c) 2012 Stefano Sabatini
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 *)

(**
 * @file
 * libswscale API use example.
 * @example scaling_video.c
 *)

(*
 * FFVCL - Delphi FFmpeg VCL Components
 * http://www.DelphiFFmpeg.com
 *
 * Original file: doc/examples/scaling_video.c
 * Ported by CodeCoolie@CNSW 2014/08/30 -> $Date:: 2019-08-24 #$
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

program scaling_video;

{$APPTYPE CONSOLE}

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

uses
{$IFDEF FPC}
  {$IFDEF MSWINDOWS}
    Windows,
  {$ENDIF}
  SysUtils,
{$ELSE}
  {$IF CompilerVersion >= 23.0}
    {$IFDEF MSWINDOWS}
      Winapi.Windows,
    {$ENDIF}
    System.SysUtils,
  {$ELSE}
    {$IFDEF MSWINDOWS}
      Windows,
    {$ENDIF}
    SysUtils,
  {$IFEND}
{$ENDIF}

{$I headers.inc}
  FFUtils;

procedure fill_yuv_image(data: array of PByte; linesize: array of Integer;
  width, height, frame_index: Integer);
var
  x, y: Integer;
begin
  (* Y *)
  for y := 0 to height - 1 do
    for x := 0 to width - 1 do
      PByte(@PAnsiChar(data[0])[y * linesize[0] + x])^ := x + y + frame_index * 3;

  (* Cb and Cr *)
  for y := 0 to height div 2 - 1 do
    for x := 0 to width div 2 - 1 do
    begin
      PByte(@PAnsiChar(data[1])[y * linesize[1] + x])^ := 128 + y + frame_index * 2;
      PByte(@PAnsiChar(data[2])[y * linesize[2] + x])^ := 64 + x + frame_index * 5;
    end;
end;

function main(): Integer;
var
  src_data, dst_data: array[0..3] of PByte;
  src_linesize, dst_linesize: array[0..3] of Integer;
  src_w, src_h, dst_w, dst_h: Integer;
  src_pix_fmt, dst_pix_fmt: TAVPixelFormat;
  dst_size: string;
  dst_filename: string;
  dst_file: THandle;
  dst_bufsize: Integer;
  sws_ctx: PSwsContext;
  i, ret: Integer;
label
  the_end;
begin
  src_w := 320;
  src_h := 240;
  src_pix_fmt := AV_PIX_FMT_YUV420P;
  dst_pix_fmt := AV_PIX_FMT_RGB24;

  if ParamCount <> 2 then
  begin
    Writeln(ErrOutput, Format('Usage: %s output_file output_size' + sLineBreak +
            'API example program to show how to scale an image with libswscale.' + sLineBreak +
            'This program generates a series of pictures, rescales them to the given ' +
            'output_size and saves them to an output file named output_file.',
            [ExtractFileName(ParamStr(0))]));
    Result := 1;
    Exit;
  end;
  dst_filename := ParamStr(1);
  dst_size     := ParamStr(2);

  if av_parse_video_size(@dst_w, @dst_h, PAnsiChar(AnsiString(dst_size))) < 0 then
  begin
    Writeln(ErrOutput,
            Format('Invalid size ''%s'', must be in the form WxH or a valid size abbreviation',
            [dst_size]));
    Result := 1;
    Exit;
  end;

  dst_file := FileCreate(dst_filename);
  if dst_file = INVALID_HANDLE_VALUE then
  begin
    Writeln(ErrOutput, Format('Could not open destination file %s', [dst_filename]));
    Result := 1;
    Exit;
  end;

  (* create scaling context *)
  sws_ctx := sws_getContext(src_w, src_h, src_pix_fmt,
                           dst_w, dst_h, dst_pix_fmt,
                           SWS_BILINEAR, nil, nil, nil);
  if not Assigned(sws_ctx) then
  begin
    Writeln(ErrOutput,
            Format('Impossible to create scale context for the conversion ' +
            'fmt:%s s:%dx%d -> fmt:%s s:%dx%d',
           [av_get_pix_fmt_name(src_pix_fmt), src_w, src_h,
            av_get_pix_fmt_name(dst_pix_fmt), dst_w, dst_h]));
    ret := AVERROR_EINVAL;
    goto the_end;
  end;

  (* allocate source and destination image buffers *)
  ret := av_image_alloc(@src_data[0], @src_linesize[0],
                            src_w, src_h, src_pix_fmt, 16);
  if ret < 0 then
  begin
    Writeln(ErrOutput, 'Could not allocate source image');
    goto the_end;
  end;

  (* buffer is going to be written to rawvideo file, no alignment *)
  ret := av_image_alloc(@dst_data[0], @dst_linesize[0],
                            dst_w, dst_h, dst_pix_fmt, 1);
  if ret < 0 then
  begin
    Writeln(ErrOutput, 'Could not allocate destination image');
    goto the_end;
  end;
  dst_bufsize := ret;

  for i := 0 to 100 - 1 do
  begin
    (* generate synthetic video *)
    fill_yuv_image(src_data, src_linesize, src_w, src_h, i);

    (* convert to destination format *)
    sws_scale(sws_ctx, @src_data[0],
              @src_linesize[0], 0, src_h, @dst_data[0], @dst_linesize[0]);

    (* write scaled image to file *)
    FileWrite(dst_file, dst_data[0]^, dst_bufsize);
  end;

  Writeln(ErrOutput, Format('Scaling succeeded. Play the output file with the command:' + sLineBreak +
         'ffplay -f rawvideo -pix_fmt %s -video_size %dx%d %s',
         [av_get_pix_fmt_name(dst_pix_fmt), dst_w, dst_h, dst_filename]));

the_end:
  FileClose(dst_file);
  av_freep(@src_data[0]);
  av_freep(@dst_data[0]);
  sws_freeContext(sws_ctx);
  Result := Ord(ret < 0);
end;

begin
  try
    ExitCode := main();
  except
    on E: Exception do
      Writeln(ErrOutput, E.ClassName, ': ', E.Message);
  end;
end.
