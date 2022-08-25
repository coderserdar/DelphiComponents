(*
 * Copyright (c) 2017 Jun Zhao
 * Copyright (c) 2017 Kaixuan Liu
 *
 * HW Acceleration API (video decoding) decode sample
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
 * HW-Accelerated decoding example.
 *
 * @example hw_decode.c
 * This example shows how to do HW-accelerated decoding with output
 * frames from the HW video surfaces.
 *)

(*
 * FFVCL - Delphi FFmpeg VCL Components
 * http://www.DelphiFFmpeg.com
 *
 * Original file: doc/examples/hw_decode.c
 * Ported by CodeCoolie@CNSW 2018/03/02 -> $Date:: 2019-08-24 #$
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

program hw_decode;

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
  hw_device_ctx: PAVBufferRef = nil;
  hw_pix_fmt: TAVPixelFormat;
  output_file: THandle = INVALID_HANDLE_VALUE;

function hw_decoder_init(ctx: PAVCodecContext; const type_: TAVHWDeviceType): Integer;
var
  err: Integer;
begin
  err := av_hwdevice_ctx_create(@hw_device_ctx, type_, nil, nil, 0);
  if err < 0 then
  begin
    Writeln(ErrOutput, 'Failed to create specified HW device.');
    Result := err;
    Exit;
  end;
  ctx.hw_device_ctx := av_buffer_ref(hw_device_ctx);

  Result := err;
end;

function get_hw_format(ctx: PAVCodecContext; const pix_fmts: PAVPixelFormat): TAVPixelFormat; cdecl;
var
  p: PAVPixelFormat;
begin
  p := pix_fmts;
  while p^ <> AV_PIX_FMT_NONE do
  begin
    if p^ = hw_pix_fmt then
    begin
      Result := p^;
      EXit;
    end;
    Inc(p);
  end;

  Writeln(ErrOutput, 'Failed to get HW surface format.');
  Result := AV_PIX_FMT_NONE;
end;

function decode_write(avctx: PAVCodecContext; packet: PAVPacket): Integer;
var
  frame, sw_frame: PAVFrame;
  tmp_frame: PAVFrame;
  buffer: PByte;
  size: Integer;
  ret: Integer;
label
  fail;
begin
  frame := nil;
  sw_frame := nil;
  buffer := nil;

  ret := avcodec_send_packet(avctx, packet);
  if ret < 0 then
  begin
    Writeln(ErrOutput, 'Error during decoding');
    Result := ret;
    Exit;
  end;

  while True do
  begin
    frame := av_frame_alloc();
    if not Assigned(frame) then
    begin
      Writeln(ErrOutput, 'Can not alloc frame');
      ret := AVERROR_ENOMEM;
      goto fail;
    end;
    sw_frame := av_frame_alloc();
    if not Assigned(sw_frame) then
    begin
      Writeln(ErrOutput, 'Can not alloc frame');
      ret := AVERROR_ENOMEM;
      goto fail;
    end;

    ret := avcodec_receive_frame(avctx, frame);
    if (ret = AVERROR_EAGAIN) or (ret = AVERROR_EOF) then
    begin
      av_frame_free(@frame);
      av_frame_free(@sw_frame);
      Result := 0;
      Exit;
    end
    else if ret < 0 then
    begin
      Writeln(ErrOutput, 'Error while decoding');
      goto fail;
    end;

    if TAVPixelFormat(frame.format) = hw_pix_fmt then
    begin
      (* retrieve data from GPU to CPU *)
      ret := av_hwframe_transfer_data(sw_frame, frame, 0);
      if ret < 0 then
      begin
        Writeln(ErrOutput, 'Error transferring the data to system memory');
        goto fail;
      end;
      tmp_frame := sw_frame;
    end
    else
      tmp_frame := frame;

    size := av_image_get_buffer_size(TAVPixelFormat(tmp_frame.format), tmp_frame.width,
                                     tmp_frame.height, 1);
    buffer := av_malloc(size);
    if not Assigned(buffer) then
    begin
      Writeln(ErrOutput, 'Can not alloc buffer');
      ret := AVERROR_ENOMEM;
      goto fail;
    end;
    ret := av_image_copy_to_buffer(buffer, size,
                                  @tmp_frame.data[0],
                                  @tmp_frame.linesize[0], TAVPixelFormat(tmp_frame.format),
                                   tmp_frame.width, tmp_frame.height, 1);
    if ret < 0 then
    begin
      Writeln(ErrOutput, 'Can not copy image to buffer');
      goto fail;
    end;

    ret := FileWrite(output_file, buffer^, size);
    if ret < 0 then
    begin
      Writeln(ErrOutput, 'Failed to dump raw data.');
      goto fail;
    end;

fail:
    av_frame_free(@frame);
    av_frame_free(@sw_frame);
    av_freep(@buffer);
    if ret < 0 then
    begin
      Result := ret;
      Exit;
    end;
  end;

  Result := 0;
end;

function main(): Integer;
var
  input_ctx: PAVFormatContext;
  video_stream, ret: Integer;
  video: PAVStream;
  decoder_ctx: PAVCodecContext;
  decoder: PAVCodec;
  packet: TAVPacket;
  type_: TAVHWDeviceType;
  i: Integer;
  config: PAVCodecHWConfig;
begin
  input_ctx := nil;
  decoder_ctx := nil;
  decoder := nil;

  if ParamCount < 3 then
  begin
    Writeln(ErrOutput, Format('Usage: %s <device type> <input file> <output file>', [ExtractFileName(ParamStr(0))]));
    Result := 1;
    Exit;
  end;

  type_ := av_hwdevice_find_type_by_name(PAnsiChar(AnsiString(ParamStr(1))));
  if type_ = AV_HWDEVICE_TYPE_NONE then
  begin
    Writeln(ErrOutput, Format('Device type %s is not supported.', [ParamStr(1)]));
    Writeln(ErrOutput, 'Available device types:');
    type_ := av_hwdevice_iterate_types(type_);
    while type_ <> AV_HWDEVICE_TYPE_NONE do
    begin
      Write(ErrOutput, Format(' %s', [av_hwdevice_get_type_name(type_)]));
      type_ := av_hwdevice_iterate_types(type_);
    end;
    Writeln(ErrOutput, '');
    Result := 1;
    Exit;
  end;

  (* open the input file *)
  if avformat_open_input(@input_ctx, PAnsiChar(AnsiString(ParamStr(2))), nil, nil) <> 0 then
  begin
    Writeln(ErrOutput, Format('Cannot open input file "%s"', [ParamStr(2)]));
    Result := 1;
    Exit;
  end;

  if avformat_find_stream_info(input_ctx, nil) < 0 then
  begin
    Writeln(ErrOutput, 'Cannot find input stream information.');
    Result := 1;
    Exit;
  end;

  (* find the video stream information *)
  ret := av_find_best_stream(input_ctx, AVMEDIA_TYPE_VIDEO, -1, -1, @decoder, 0);
  if ret < 0 then
  begin
    Writeln(ErrOutput, 'Cannot find a video stream in the input file');
    Result := 1;
    Exit;
  end;
  video_stream := ret;

  i := 0;
  while True do
  begin
    config := avcodec_get_hw_config(decoder, i);
    if not Assigned(config) then
    begin
      Writeln(ErrOutput, Format('Decoder %s does not support device type %s.',
              [decoder.name, av_hwdevice_get_type_name(type_)]));
      Result := 1;
      Exit;
    end;
    if ((config.methods and AV_CODEC_HW_CONFIG_METHOD_HW_DEVICE_CTX) <> 0) and
      (config.device_type = type_) then
    begin
      hw_pix_fmt := config.pix_fmt;
      Break;
    end;
    Inc(i);
  end;

  decoder_ctx := avcodec_alloc_context3(decoder);
  if not Assigned(decoder_ctx) then
  begin
    Result := AVERROR_ENOMEM;
    Exit;
  end;

  video := PPtrIdx(input_ctx.streams, video_stream);
  if avcodec_parameters_to_context(decoder_ctx, video.codecpar) < 0 then
  begin
    Result := 1;
    Exit;
  end;

  decoder_ctx.get_format := get_hw_format;

  if hw_decoder_init(decoder_ctx, type_) < 0 then
  begin
    Result := 1;
    Exit;
  end;

  ret := avcodec_open2(decoder_ctx, decoder, nil);
  if ret < 0 then
  begin
    Writeln(ErrOutput, Format('Failed to open codec for stream #%u', [video_stream]));
    Result := 1;
    Exit;
  end;

  (* open the file to dump raw data *)
  output_file := FileCreate(ParamStr(3));
  if output_file = INVALID_HANDLE_VALUE then
  begin
    Writeln(ErrOutput, Format('Could not open %s', [ParamStr(3)]));
    Result := 1;
    Exit;
  end;

  (* actual decoding and dump the raw data *)
  while ret >= 0 do
  begin
    ret := av_read_frame(input_ctx, @packet);
    if ret < 0 then
      Break;

    if video_stream = packet.stream_index then
      ret := decode_write(decoder_ctx, @packet);

    av_packet_unref(@packet);
  end;

  (* flush the decoder *)
  packet.data := nil;
  packet.size := 0;
  {ret := }decode_write(decoder_ctx, @packet);
  av_packet_unref(@packet);

  if output_file <> INVALID_HANDLE_VALUE then
    FileClose(output_file);
  avcodec_free_context(@decoder_ctx);
  avformat_close_input(@input_ctx);
  av_buffer_unref(@hw_device_ctx);

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
