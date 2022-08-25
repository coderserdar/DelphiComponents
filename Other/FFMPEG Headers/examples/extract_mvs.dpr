(*
 * Copyright (c) 2012 Stefano Sabatini
 * Copyright (c) 2014 Clément Bœsch
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

(*
 * FFVCL - Delphi FFmpeg VCL Components
 * http://www.DelphiFFmpeg.com
 *
 * Original file: doc/examples/extract_mvs.c
 * Ported by CodeCoolie@CNSW 2014/12/08 -> $Date:: 2019-08-24 #$
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

program extract_mvs;

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

var
  fmt_ctx: PAVFormatContext = nil;
  video_dec_ctx: PAVCodecContext = nil;
  video_stream: PAVStream = nil;
  src_filename: PAnsiChar = nil;

  video_stream_idx: Integer = -1;
  frame: PAVFrame = nil;
  video_frame_count: Integer = 0;

function PtrIdx(P: PAVMotionVector; I: Integer): PAVMotionVector; overload;
begin
  Inc(P, I);
  Result := P;
end;

function decode_packet(const pkt: PAVPacket): Integer;
var
  ret: Integer;
  i: Integer;
  sd: PAVFrameSideData;
  mvs: PAVMotionVector;
  mv: PAVMotionVector;
begin
  ret := avcodec_send_packet(video_dec_ctx, pkt);
  if ret < 0 then
  begin
    Writeln(ErrOutput, Format('Error while sending a packet to the decoder (%s)', [av_err2str(ret)]));
    Result := ret;
    Exit;
  end;

  while ret >= 0 do
  begin
    ret := avcodec_receive_frame(video_dec_ctx, frame);
    if (ret = AVERROR_EAGAIN) or (ret = AVERROR_EOF) then
      Break
    else if ret < 0 then
    begin
      Writeln(ErrOutput, Format('Error while receiving a frame from the decoder: %s', [av_err2str(ret)]));
      Result := ret;
      Exit;
    end;

    if ret >= 0 then
    begin
      Inc(video_frame_count);
      sd := av_frame_get_side_data(frame, AV_FRAME_DATA_MOTION_VECTORS);
      if Assigned(sd) then
      begin
        mvs := PAVMotionVector(sd.data);
        for i := 0 to sd.size div SizeOf(mvs^) - 1 do
        begin
          mv := PtrIdx(mvs, i);
          Writeln(Format('%d,%2d,%2d,%2d,%4d,%4d,%4d,%4d,0x%x',
            [video_frame_count, mv.source,
             mv.w, mv.h, mv.src_x, mv.src_y,
             mv.dst_x, mv.dst_y, mv.flags]));
        end;
      end;
      av_frame_unref(frame);
    end;
  end;

  Result := 0;
end;

function open_codec_context(fmt_ctx: PAVFormatContext; type_: TAVMediaType): Integer;
var
  ret: Integer;
  st: PAVStream;
  dec_ctx: PAVCodecContext;
  avdec: PAVCodec;
  opts: PAVDictionary;
  stream_idx: Integer;
begin
  avdec := nil;
  opts := nil;

  ret := av_find_best_stream(fmt_ctx, type_, -1, -1, @avdec, 0);
  if ret < 0 then
  begin
    Writeln(ErrOutput, Format('Could not find %s stream in input file "%s"',
                [av_get_media_type_string(type_), src_filename]));
    Result := ret;
    Exit;
  end
  else
  begin
    stream_idx := ret;
    st := PPtrIdx(fmt_ctx.streams, stream_idx);

    dec_ctx := avcodec_alloc_context3(avdec);
    if not Assigned(dec_ctx) then
    begin
      Writeln(ErrOutput, 'Failed to allocate codec');
      Result := AVERROR_EINVAL;
      Exit;
    end;

    ret := avcodec_parameters_to_context(dec_ctx, st.codecpar);
    if ret < 0 then
    begin
      Writeln(ErrOutput, 'Failed to copy codec parameters to codec context');
      Result := ret;
      Exit;
    end;

    (* Init the video decoder *)
    av_dict_set(@opts, 'flags2', '+export_mvs', 0);
    ret := avcodec_open2(dec_ctx, avdec, @opts);
    if ret < 0 then
    begin
      Writeln(ErrOutput, Format('Failed to open %s codec',
              [av_get_media_type_string(type_)]));
      Result := ret;
      Exit;
    end;

    video_stream_idx := stream_idx;
    video_stream := PPtrIdx(fmt_ctx.streams, video_stream_idx);
    video_dec_ctx := dec_ctx;
  end;

  Result := 0;
end;

function main(): Integer;
var
  ret: Integer;
  pkt: TAVPacket;
label
  the_end;
begin
  FillChar(pkt, Sizeof(TAVPacket), 0);
  ret := 0;

  if ParamCount <> 1 then
  begin
    Writeln(ErrOutput, Format('Usage: %s <video>', [ExtractFileName(ParamStr(0))]));
    Result := 1;
    Exit;
  end;
  src_filename := PAnsiChar(AnsiString(ParamStr(1)));

  if avformat_open_input(@fmt_ctx, src_filename, nil, nil) < 0 then
  begin
    Writeln(ErrOutput, Format('Could not open source file %s', [src_filename]));
    Result := 1;
    Exit;
  end;

  if avformat_find_stream_info(fmt_ctx, nil) < 0 then
  begin
    Writeln(ErrOutput, 'Could not find stream information');
    Result := 1;
    Exit;
  end;

  open_codec_context(fmt_ctx, AVMEDIA_TYPE_VIDEO);

  av_dump_format(fmt_ctx, 0, src_filename, 0);

  if not Assigned(video_stream) then
  begin
    Writeln(ErrOutput, 'Could not find video stream in the input, aborting');
    ret := 1;
    goto the_end;
  end;

  frame := av_frame_alloc();
  if not Assigned(frame) then
  begin
    Writeln(ErrOutput, 'Could not allocate frame');
    ret := AVERROR_ENOMEM;
    goto the_end;
  end;

  Writeln('framenum,source,blockw,blockh,srcx,srcy,dstx,dsty,flags');

  (* read frames from the file *)
  while av_read_frame(fmt_ctx, @pkt) >= 0 do
  begin
    if pkt.stream_index = video_stream_idx then
      ret := decode_packet(@pkt);
    av_packet_unref(@pkt);
    if ret < 0 then
      Break;
  end;

  (* flush cached frames *)
  decode_packet(nil);

the_end:
  avcodec_free_context(@video_dec_ctx);
  avformat_close_input(@fmt_ctx);
  av_frame_free(@frame);

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
