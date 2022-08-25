(*
 * Copyright (c) 2014 Stefano Sabatini
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
 * libavformat AVIOContext API example.
 *
 * Make libavformat demuxer access media content through a custom
 * AVIOContext read callback.
 * @example avio_reading.c
 *)

(*
 * FFVCL - Delphi FFmpeg VCL Components
 * http://www.DelphiFFmpeg.com
 *
 * Original file: doc/examples/avio_reading.c
 * Ported by CodeCoolie@CNSW 2014/08/24 -> $Date:: 2021-02-22 #$
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

program avio_reading;

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

type
  Pbuffer_data = ^Tbuffer_data;
  Tbuffer_data = record
    ptr: PByte;
    size: Integer; //Cardinal; ///< size left in the buffer
  end;

function read_packet(opaque: Pointer; buf: PByte; buf_size: Integer): Integer; cdecl;
var
  bd: Pbuffer_data;
begin
  bd := Pbuffer_data(opaque);
  if buf_size > bd.size then
    buf_size := bd.size;

  if buf_size = 0 then
  begin
    Result := AVERROR_EOF;
    Exit;
  end;
  Writeln(Format('ptr:%p size:%d', [bd.ptr, bd.size]));

  (* copy internal buffer data to buf *)
  Move(bd.ptr^, buf^, buf_size);
  Inc(bd.ptr, buf_size);
  Dec(bd.size, buf_size);

  Result := buf_size;
end;

function main(): Integer;
var
  fmt_ctx: PAVFormatContext;
  avio_ctx: PAVIOContext;
  buffer, avio_ctx_buffer: PByte;
  buffer_size, avio_ctx_buffer_size: Cardinal;
  input_filename: string;
  ret: Integer;
  bd: Tbuffer_data;
label
  the_end;
begin
  fmt_ctx := nil;
  avio_ctx := nil;
  buffer := nil;
  avio_ctx_buffer_size := 4096;
  FillChar(bd, SizeOf(Tbuffer_data), 0);

  if ParamCount <> 1 then
  begin
    Writeln(ErrOutput, Format('usage: %s input_file' + sLineBreak +
              'API example program to show how to read from a custom buffer ' +
              'accessed through AVIOContext.', [ExtractFileName(ParamStr(0))]));
    Result := 1;
    Exit;
  end;
  input_filename := ParamStr(1);

  (* slurp file content into buffer *)
  ret := av_file_map(PAnsiChar(AnsiString(input_filename)), @buffer, @buffer_size, 0, nil);
  if ret < 0 then
    goto the_end;

  (* fill opaque structure used by the AVIOContext read callback *)
  bd.ptr  := buffer;
  bd.size := buffer_size;

  fmt_ctx := avformat_alloc_context();
  if not Assigned(fmt_ctx) then
  begin
    ret := AVERROR_ENOMEM;
    goto the_end;
  end;

  avio_ctx_buffer := av_malloc(avio_ctx_buffer_size);
  if not Assigned(avio_ctx_buffer) then
  begin
    ret := AVERROR_ENOMEM;
    goto the_end;
  end;
  avio_ctx := avio_alloc_context(avio_ctx_buffer, avio_ctx_buffer_size,
                                0, @bd, @read_packet, nil, nil);
  if not Assigned(avio_ctx) then
  begin
    ret := AVERROR_ENOMEM;
    goto the_end;
  end;
  fmt_ctx.pb := avio_ctx;

  ret := avformat_open_input(@fmt_ctx, nil, nil, nil);
  if ret < 0 then
  begin
    Writeln(ErrOutput, 'Could not open input');
    goto the_end;
  end;

  ret := avformat_find_stream_info(fmt_ctx, nil);
  if ret < 0 then
  begin
    Writeln(ErrOutput, 'Could not find stream information');
    goto the_end;
  end;

  av_dump_format(fmt_ctx, 0, PAnsiChar(AnsiString(input_filename)), 0);

the_end:
  avformat_close_input(@fmt_ctx);

  (* note: the internal buffer could have changed, and be != avio_ctx_buffer *)
  if Assigned(avio_ctx) then
    av_freep(@avio_ctx.buffer);
  avio_context_free(@avio_ctx);

  av_file_unmap(buffer, buffer_size);

  if ret < 0 then
  begin
    Writeln(ErrOutput, Format('Error occurred: %s', [av_err2str(ret)]));
    Result := 1;
    Exit;
  end;

  Result := 0;
end;

begin
  try
    ExitCode := main();
  except
    on E: Exception do
      Writeln(ErrOutput, E.ClassName, ': ', E.Message);
  end;
end.
