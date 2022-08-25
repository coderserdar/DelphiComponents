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
 * audio encoding with libavcodec API example.
 *
 * @example encode_audio.c
 *)

(*
 * FFVCL - Delphi FFmpeg VCL Components
 * http://www.DelphiFFmpeg.com
 *
 * Original file: doc/examples/encode_audio.c
 * Ported by CodeCoolie@CNSW 2017/12/17 -> $Date:: 2019-08-24 #$
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

program encode_audio;

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

(* check that a given sample format is supported by the encoder *)
function check_sample_fmt(const codec: PAVCodec; sample_fmt: TAVSampleFormat): Integer;
var
  p: PAVSampleFormat;
begin
  p := codec.sample_fmts;

  while p^ <> AV_SAMPLE_FMT_NONE do
  begin
    if p^ = sample_fmt then
    begin
      Result := 1;
      Exit;
    end;
    Inc(p);
  end;
  Result := 0;
end;

(* just pick the highest supported samplerate *)
function select_sample_rate(const codec: PAVCodec): Integer;
var
  p: PInteger;
  best_samplerate: Integer;
begin
  best_samplerate := 0;

  if not Assigned(codec.supported_samplerates) then
  begin
    Result := 44100;
    Exit;
  end;

  p := codec.supported_samplerates;
  while p^ <> 0 do
  begin
    if (best_samplerate = 0) or (Abs(44100 - p^) < Abs(44100 - best_samplerate)) then
      best_samplerate := p^;
    Inc(p);
  end;
  Result := best_samplerate;
end;

(* select layout with the highest channel count *)
function select_channel_layout(const codec: PAVCodec): Integer;
var
  p: PInt64;
  best_ch_layout: Int64;
  best_nb_channels: Integer;
  nb_channels: Integer;
begin
  best_ch_layout := 0;
  best_nb_channels := 0;

  if not Assigned(codec.channel_layouts) then
  begin
    Result := AV_CH_LAYOUT_STEREO;
    Exit;
  end;

  p := codec.channel_layouts;
  while p^ <> 0 do
  begin
    nb_channels := av_get_channel_layout_nb_channels(p^);

    if nb_channels > best_nb_channels then
    begin
      best_ch_layout   := p^;
      best_nb_channels := nb_channels;
    end;
    Inc(p);
  end;
  Result := best_ch_layout;
end;

function encode(ctx: PAVCodecContext; frame: PAVFrame; pkt: PAVPacket; output: THandle): Boolean;
var
  ret: Integer;
begin
  (* send the frame for encoding *)
  ret := avcodec_send_frame(ctx, frame);
  if ret < 0 then
  begin
    Writeln(ErrOutput, 'Error sending the frame to the encoder');
    Result := False;
    Exit;
  end;

  (* read all the available output packets (in general there may be any
   * number of them *)
  while ret >= 0 do
  begin
    ret := avcodec_receive_packet(ctx, pkt);
    if (ret = AVERROR_EAGAIN) or (ret = AVERROR_EOF) then
    begin
      Result := True;
      EXit;
    end
    else if ret < 0 then
    begin
      Writeln(ErrOutput, 'Error encoding audio frame');
      Result := False;
      Exit;
    end;

    FileWrite(output, pkt.data^, pkt.size);
    av_packet_unref(pkt);
  end;
  Result := True;
end;

function main(): Integer;
var
  filename: string;
  codec: PAVCodec;
  c: PAVCodecContext;
  frame: PAVFrame;
  pkt: PAVPacket;
  i, j, k, ret: Integer;
  f: THandle;
  samples: PSmallInt;
  t, tincr: Single;
begin
  if ParamCount < 1 then
  begin
    Writeln(Format('Usage: %s <output file>', [ExtractFileName(ParamStr(0))]));
    Result := 1;
    Exit;
  end;
  filename := ParamStr(1);

  (* find the MP2 encoder *)
  codec := avcodec_find_encoder(AV_CODEC_ID_MP2);
  if not Assigned(codec) then
  begin
    Writeln(ErrOutput, 'Codec not found');
    Result := 1;
    Exit;
  end;

  c := avcodec_alloc_context3(codec);
  if not Assigned(c) then
  begin
    Writeln(ErrOutput, 'Could not allocate audio codec context');
    Result := 1;
    Exit;
  end;

  (* put sample parameters *)
  c.bit_rate := 64000;

  (* check that the encoder supports s16 pcm input *)
  c.sample_fmt := AV_SAMPLE_FMT_S16;
  if check_sample_fmt(codec, c.sample_fmt) = 0 then
  begin
    Writeln(ErrOutput, Format('Encoder does not support sample format %s',
              [string(av_get_sample_fmt_name(c.sample_fmt))]));
    Result := 1;
    Exit;
  end;

  (* select other audio parameters supported by the encoder *)
  c.sample_rate    := select_sample_rate(codec);
  c.channel_layout := select_channel_layout(codec);
  c.channels       := av_get_channel_layout_nb_channels(c.channel_layout);

  (* open it *)
  if avcodec_open2(c, codec, nil) < 0 then
  begin
    Writeln(ErrOutput, 'Could not open codec');
    Result := 1;
    Exit;
  end;

  f := FileCreate(filename);
  if f = INVALID_HANDLE_VALUE then
  begin
    Writeln(ErrOutput, Format('Could not open %s', [filename]));
    Result := 1;
    Exit;
  end;

  (* packet for holding encoded output *)
  pkt := av_packet_alloc();
  if not Assigned(pkt) then
  begin
    Writeln(ErrOutput, 'could not allocate the packet');
    Result := 1;
    Exit;
  end;

  (* frame containing input raw audio *)
  frame := av_frame_alloc();
  if not Assigned(frame) then
  begin
    Writeln(ErrOutput, 'Could not allocate audio frame');
    Result := 1;
    Exit;
  end;

  frame.nb_samples     := c.frame_size;
  frame.format         := Ord(c.sample_fmt);
  frame.channel_layout := c.channel_layout;

  (* allocate the data buffers *)
  ret := av_frame_get_buffer(frame, 0);
  if ret < 0 then
  begin
    Writeln(ErrOutput, 'Could not allocate audio data buffers');
    Result := 1;
    Exit;
  end;

  (* encode a single tone sound *)
  t := 0;
  tincr := 2 * M_PI * 440.0 / c.sample_rate;
  for i := 0 to 200 - 1 do
  begin
    (* make sure the frame is writable -- makes a copy if the encoder
     * kept a reference internally *)
    ret := av_frame_make_writable(frame);
    if ret < 0 then
    begin
      Writeln(ErrOutput, 'av_frame_make_writable() failed');
      Result := 1;
      Exit;
    end;
    samples := PSmallInt(frame.data[0]);

    for j := 0 to c.frame_size - 1 do
    begin
      PtrIdx(samples, 2*j)^ := Trunc((sin(t) * 10000));

      for k := 1 to c.channels - 1 do
        PtrIdx(samples, 2*j + k)^ := PtrIdx(samples, 2*j)^;
      t := t + tincr;
    end;
    if not encode(c, frame, pkt, f) then
    begin
      Result := 1;
      Exit;
    end;
  end;
  Result := 0;

  (* flush the encoder *)
  if not encode(c, nil, pkt, f) then
    Result := 1;
  FileClose(f);

  av_frame_free(@frame);
  av_packet_free(@pkt);
  avcodec_free_context(@c);
end;

begin
  try
    ExitCode := main();
  except
    on E: Exception do
      Writeln(ErrOutput, E.ClassName, ': ', E.Message);
  end;
end.
