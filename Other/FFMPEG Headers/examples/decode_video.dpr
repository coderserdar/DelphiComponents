(*
 * Copyright (c) 2001 Fabrice Bellard
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
 * video decoding with libavcodec API example
 *
 * @example decode_video.c
 *)

(*
 * FFVCL - Delphi FFmpeg VCL Components
 * http://www.DelphiFFmpeg.com
 *
 * Original file: doc/examples/decode_video.c
 * Ported by CodeCoolie@CNSW 2017/12/17 -> $Date:: 2021-04-18 #$
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

program decode_video;

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

const
  INBUF_SIZE          = 4096;

procedure pgm_save(buf: PByte; wrap, xsize, ysize: Integer;
  filename: string);
var
  f: THandle;
  i: Integer;
  s: AnsiString;
begin
  f := FileCreate(filename);
  if f = INVALID_HANDLE_VALUE then
  begin
    Writeln(ErrOutput, Format('Could not open %s', [filename]));
    ExitCode := 1;
    Exit;
  end;
  s := AnsiString(Format('P5'#10'%d %d'#10'%d'#10, [xsize, ysize, 255]));
  FileWrite(f, s[1], Length(s));
  for i := 0 to ysize - 1 do
    FileWrite(f, PByte(PAnsiChar(buf) + i * wrap)^, xsize);
  FileClose(f);
end;

function decode(dec_ctx: PAVCodecContext; frame: PAVFrame; pkt: PAVPacket; const filename: string): Boolean;
var
  outfile: string;
  ret: Integer;
begin
  ret := avcodec_send_packet(dec_ctx, pkt);
  if ret < 0 then
  begin
    Writeln(ErrOutput, 'Error sending a packet for decoding');
    Result := False;
    Exit;
  end;

  while ret >= 0 do
  begin
    ret := avcodec_receive_frame(dec_ctx, frame);
    if (ret = AVERROR_EAGAIN) or (ret = AVERROR_EOF) then
    begin
      Result := True;
      Exit;
    end
    else if ret < 0 then
    begin
      Writeln(ErrOutput, 'Error during decoding');
      Result := False;
      Exit;
    end;

    Writeln(Format('saving frame %3d', [dec_ctx.frame_number]));
    //fflush(stdout);

    (* the picture is allocated by the decoder, no need to free it *)
    outfile := ChangeFileExt(filename, '');
    pgm_save(frame.data[0], frame.linesize[0],
             frame.width, frame.height, Format('%s-%d%s', [outfile, dec_ctx.frame_number, ExtractFileExt(filename)]));
  end;
  Result := True;
end;

function main(): Integer;
var
  filename, outfilename: string;
  codec: PAVCodec;
  parser: PAVCodecParserContext;
  c: PAVCodecContext;
  f: THandle;
  frame: PAVFrame;
  inbuf: array[0..INBUF_SIZE + AV_INPUT_BUFFER_PADDING_SIZE - 1] of Byte;
  data: PByte;
  data_size: Cardinal;
  ret: Integer;
  pkt: PAVPacket;
begin
  if ParamCount < 2 then
  begin
    Writeln(Format('Usage: %s <input file> <output file>', [ExtractFileName(ParamStr(0))]));
    Writeln('And check your input file is encoded by mpeg1video please.');
    Result := 1;
    Exit;
  end;
  filename := ParamStr(1);
  outfilename := ParamStr(2);

  pkt := av_packet_alloc();
  if not Assigned(pkt) then
  begin
    Result := 1;
    Exit;
  end;

  (* set end of buffer to 0 (this ensures that no overreading happens for damaged MPEG streams) *)
  FillChar(inbuf[INBUF_SIZE], AV_INPUT_BUFFER_PADDING_SIZE, 0);

  (* find the MPEG1 video decoder *)
  codec := avcodec_find_decoder(AV_CODEC_ID_MPEG1VIDEO);
  if not Assigned(codec) then
  begin
    Writeln(ErrOutput, 'Codec not found');
    Result := 1;
    Exit;
  end;

  parser := av_parser_init(Ord(codec.id));
  if not Assigned(parser) then
  begin
    Writeln(ErrOutput, 'parser not found');
    Result := 1;
    Exit;
  end;

  c := avcodec_alloc_context3(codec);
  if not Assigned(c) then
  begin
    Writeln(ErrOutput, 'Could not allocate video codec context');
    Result := 1;
    Exit;
  end;

  (* For some codecs, such as msmpeg4 and mpeg4, width and height
     MUST be initialized there because this information is not
     available in the bitstream. *)

  (* open it *)
  if avcodec_open2(c, codec, nil) < 0 then
  begin
    Writeln(ErrOutput, 'Could not open codec');
    Result := 1;
    Exit;
  end;

  f := FileOpen(filename, fmOpenRead);
  if f = INVALID_HANDLE_VALUE then
  begin
    Writeln(ErrOutput, Format('Could not open %s', [filename]));
    Result := 1;
    Exit;
  end;

  frame := av_frame_alloc();
  if not Assigned(frame) then
  begin
    Writeln(ErrOutput, 'Could not allocate video frame');
    Result := 1;
    Exit;
  end;

  while True do // not feof(f)
  begin
    (* read raw data from the input file *)
    data_size := FileRead(f, inbuf[0], INBUF_SIZE);
    if data_size = 0 then
      Break;

    (* use the parser to split the data into frames *)
    data := @inbuf[0];
    while data_size > 0 do
    begin
      ret := av_parser_parse2(parser, c, @pkt.data, @pkt.size,
                              data, data_size, AV_NOPTS_VALUE, AV_NOPTS_VALUE, 0);
      if ret < 0 then
      begin
        Writeln(ErrOutput, 'Error while parsing');
        Result := 1;
        Exit;
      end;
      Inc(data, ret);
      Dec(data_size, ret);

      if pkt.size <> 0 then
        if not decode(c, frame, pkt, outfilename) then
        begin
          Result := 1;
          Exit;
        end;
    end;
  end;
  Result := 0;

  (* flush the decoder *)
  if not decode(c, frame, nil, outfilename) then
    Result := 1;

  FileClose(f);

  av_parser_close(parser);
  avcodec_free_context(@c);
  av_frame_free(@frame);
  av_packet_free(@pkt);
end;

begin
  try
    ExitCode := main();
  except
    on E: Exception do
      Writeln(ErrOutput, E.ClassName, ': ', E.Message);
  end;
end.
