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
 * Demuxing and decoding example.
 *
 * Show how to use the libavformat and libavcodec API to demux and
 * decode audio and video data.
 * @example demuxing_decoding.c
 *)

(*
 * FFVCL - Delphi FFmpeg VCL Components
 * http://www.DelphiFFmpeg.com
 *
 * Original file: doc/examples/demuxing_decoding.c
 * Ported by CodeCoolie@CNSW 2014/08/26 -> $Date:: 2022-02-20 #$
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

program demuxing_decoding;

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
  audio_dec_ctx: PAVCodecContext = nil;
  width, height: Integer;
  pix_fmt: TAVPixelFormat;
  video_stream: PAVStream = nil;
  audio_stream: PAVStream = nil;
  src_filename: string = '';
  video_dst_filename: string = '';
  audio_dst_filename: string = '';
  video_dst_file: THandle = INVALID_HANDLE_VALUE;
  audio_dst_file: Thandle = INVALID_HANDLE_VALUE;

  video_dst_data: array[0..3] of PByte = (nil, nil, nil, nil);
  video_dst_linesize: array[0..3] of Integer;
  video_dst_bufsize: Integer;

  video_stream_idx: Integer = -1;
  audio_stream_idx: Integer = -1;
  frame: PAVFrame = nil;
  pkt: PAVPacket = nil;
  video_frame_count: Integer = 0;
  audio_frame_count: Integer = 0;

function output_video_frame(frame: PAVFrame): Integer;
begin
  if (frame.width <> width) or (frame.height <> height) or (frame.format <> Integer(pix_fmt)) then
  begin

    (* To handle this change, one could call av_image_alloc again and
     * decode the following frames into another rawvideo file. *)
    Writeln(Format('Error: Width, height and pixel format have to be ' +
                   'constant in a rawvideo file, but the width, height or ' +
                   'pixel format of the input video changed:' + sLineBreak +
                   'old: width = %d, height = %d, format = %s' + sLineBreak +
                   'new: width = %d, height = %d, format = %s',
                  [width, height, string(av_get_pix_fmt_name(pix_fmt)),
                   frame.width, frame.height, string(av_get_pix_fmt_name(TAVPixelFormat(frame.format)))]));
    Result := -1;
    Exit;
  end;

  Writeln(Format('video_frame n:%d coded_n:%d',
         [video_frame_count, frame.coded_picture_number]));
  Inc(video_frame_count);

  (* copy decoded frame to destination buffer:
   * this is required since rawvideo expects non aligned data *)
  av_image_copy(@video_dst_data[0], @video_dst_linesize[0],
                @frame.data[0], @frame.linesize[0],
                pix_fmt, width, height);

  (* write to rawvideo file *)
  FileWrite(video_dst_file, video_dst_data[0]^, video_dst_bufsize);

  Result := 0;
end;

function output_audio_frame(frame: PAVFrame): Integer;
var
  unpadded_linesize: Cardinal;
begin
  unpadded_linesize := frame.nb_samples * av_get_bytes_per_sample(TAVSampleFormat(frame.format));
  Writeln(Format('audio_frame n:%d nb_samples:%d pts:%s',
         [audio_frame_count, frame.nb_samples,
          string(av_ts2timestr(frame.pts, @audio_dec_ctx.time_base))]));
  Inc(audio_frame_count);

  (* Write the raw audio data samples of the first plane. This works
   * fine for packed formats (e.g. AV_SAMPLE_FMT_S16). However,
   * most audio decoders output planar audio, which uses a separate
   * plane of audio samples for each channel (e.g. AV_SAMPLE_FMT_S16P).
   * In other words, this code will write only the first audio channel
   * in these cases.
   * You should use libswresample or libavfilter to convert the frame
   * to packed data. *)
  FileWrite(audio_dst_file, frame.extended_data^^, unpadded_linesize);

  Result := 0;
end;

function decode_packet(dec: PAVCodecContext; const pkt: PAVPacket): Integer;
var
  ret: Integer;
begin
  ret := 0;

  // submit the packet to the decoder
  ret := avcodec_send_packet(dec, pkt);
  if ret < 0 then
  begin
    Writeln(ErrOutput, Format('Error submitting a packet for decoding (%s)', [string(av_err2str(ret))]));
    Result := ret;
    Exit;
  end;

  // get all the available frames from the decoder
  while ret >= 0 do
  begin
    ret := avcodec_receive_frame(dec, frame);
    if ret < 0 then
    begin
      // those two return values are special and mean there is no output
      // frame available, but there were no errors during decoding
      if (ret = AVERROR_EOF) or (ret = AVERROR_EAGAIN) then
      begin
        Result := 0;
        Exit;
      end;

      Writeln(ErrOutput, Format('Error during decoding (%s)', [string(av_err2str(ret))]));
      Result := ret;
      Exit;
    end;

    // write the frame data to output file
    if dec.codec.ttype = AVMEDIA_TYPE_VIDEO then
      ret := output_video_frame(frame)
    else
      ret := output_audio_frame(frame);

    av_frame_unref(frame);
    if ret < 0 then
    begin
      Result := ret;
      Exit;
    end;
  end;

  Result := 0;
end;

function open_codec_context(stream_idx: PInteger;
  dec_ctx: PPAVCodecContext; fmt_ctx: PAVFormatContext; type_: TAVMediaType): Integer;
var
  ret, stream_index: Integer;
  st: PAVStream;
  avdec: PAVCodec;
  opts: PAVDictionary;
begin
  opts := nil;

  ret := av_find_best_stream(fmt_ctx, type_, -1, -1, nil, 0);
  if ret < 0 then
  begin
    Writeln(ErrOutput, Format('Could not find %s stream in input file ''%s''',
            [string(av_get_media_type_string(type_)), src_filename]));
    Result := ret;
    Exit;
  end
  else
  begin
    stream_index := ret;
    st := PPtrIdx(fmt_ctx.streams, stream_index);

    (* find decoder for the stream *)
    avdec := avcodec_find_decoder(st.codecpar.codec_id);
    if not Assigned(avdec) then
    begin
      Writeln(ErrOutput, Format('Failed to find %s codec',
              [string(av_get_media_type_string(type_))]));
      Result := AVERROR_EINVAL;
      Exit;
    end;

    (* Allocate a codec context for the decoder *)
    dec_ctx^ := avcodec_alloc_context3(avdec);
    if not Assigned(dec_ctx^) then
    begin
      Writeln(ErrOutput, Format('Failed to allocate the %s codec context',
              [string(av_get_media_type_string(type_))]));
      Result := AVERROR_ENOMEM;
      Exit;
    end;

    (* Copy codec parameters from input stream to output codec context *)
    ret := avcodec_parameters_to_context(dec_ctx^, st.codecpar);
    if ret < 0 then
    begin
      Writeln(ErrOutput, Format('Failed to copy %s codec parameters to decoder context',
              [string(av_get_media_type_string(type_))]));
      Result := ret;
      Exit;
    end;

    (* Init the decoders *)
    ret := avcodec_open2(dec_ctx^, avdec, @opts);
    if ret < 0 then
    begin
      Writeln(ErrOutput, Format('Failed to open %s codec',
              [string(av_get_media_type_string(type_))]));
      Result := ret;
      Exit;
    end;
    stream_idx^ := stream_index;
  end;

  Result := 0;
end;

function get_format_from_sample_fmt(const fmt: PPAnsiChar;
  sample_fmt: TAVSampleFormat): Integer;
type
  Psample_fmt_entry = ^Tsample_fmt_entry;
  Tsample_fmt_entry = record
    sample_fmt: TAVSampleFormat;
    fmt_be, fmt_le: PAnsiChar;
  end;
const
  sample_fmt_entries: array[0..4] of Tsample_fmt_entry = (
      (sample_fmt: AV_SAMPLE_FMT_U8;  fmt_be: 'u8';    fmt_le: 'u8'    ),
      (sample_fmt: AV_SAMPLE_FMT_S16; fmt_be: 's16be'; fmt_le: 's16le' ),
      (sample_fmt: AV_SAMPLE_FMT_S32; fmt_be: 's32be'; fmt_le: 's32le' ),
      (sample_fmt: AV_SAMPLE_FMT_FLT; fmt_be: 'f32be'; fmt_le: 'f32le' ),
      (sample_fmt: AV_SAMPLE_FMT_DBL; fmt_be: 'f64be'; fmt_le: 'f64le' )
    );
var
  i: Integer;
  entry: Psample_fmt_entry;
begin
  fmt^ := nil;

  for i := 0 to High(sample_fmt_entries) do
  begin
    entry := @sample_fmt_entries[i];
    if sample_fmt = entry.sample_fmt then
    begin
      fmt^ := entry.fmt_le; //AV_NE(entry.fmt_be, entry.fmt_le);
      Result := 0;
      Exit;
    end;
  end;

  Writeln(ErrOutput,
          Format('sample format %s is not supported as output format',
          [string(av_get_sample_fmt_name(sample_fmt))]));
  Result := -1;
end;

function main(): Integer;
var
  ret, got_frame: Integer;
  sfmt: TAVSampleFormat;
  n_channels: Integer;
  fmt: PAnsiChar;
  packed_name: PAnsiChar;
  packed_str: string;
label
  the_end;
begin
  ret := 0;

  if ParamCount <> 3 then
  begin
    Writeln(ErrOutput, Format('usage: %s input_file video_output_file audio_output_file' + sLineBreak +
            'API example program to show how to read frames from an input file.' + sLineBreak +
            'This program reads frames from a file, decodes them, and writes decoded' + sLineBreak +
            'video frames to a rawvideo file named video_output_file, and decoded' + sLineBreak +
            'audio frames to a rawaudio file named audio_output_file.',
            [ExtractFileName(ParamStr(0))]));
    Result := 1;
    Exit;
  end;

  src_filename := ParamStr(1);
  video_dst_filename := ParamStr(2);
  audio_dst_filename := ParamStr(3);

  (* open input file, and allocate format context *)
  if avformat_open_input(@fmt_ctx, PAnsiChar(AnsiString(src_filename)), nil, nil) < 0 then
  begin
    Writeln(ErrOutput, Format('Could not open source file %s', [src_filename]));
    Result := 1;
    Exit;
  end;

  (* retrieve stream information *)
  if avformat_find_stream_info(fmt_ctx, nil) < 0 then
  begin
    Writeln(ErrOutput, 'Could not find stream information');
    Result := 1;
    Exit;
  end;

  if open_codec_context(@video_stream_idx, @video_dec_ctx, fmt_ctx, AVMEDIA_TYPE_VIDEO) >= 0 then
  begin
    video_stream := PPtrIdx(fmt_ctx.streams, video_stream_idx);

    video_dst_file := FileCreate(video_dst_filename);
    if video_dst_file = INVALID_HANDLE_VALUE then
    begin
      Writeln(ErrOutput, Format('Could not open destination file %s', [video_dst_filename]));
      ret := 1;
      goto the_end;
    end;

    (* allocate image where the decoded image will be put *)
    width := video_dec_ctx.width;
    height := video_dec_ctx.height;
    pix_fmt := video_dec_ctx.pix_fmt;
    ret := av_image_alloc(@video_dst_data[0], @video_dst_linesize[0],
                         width, height, pix_fmt, 1);
    if ret < 0 then
    begin
      Writeln(ErrOutput, 'Could not allocate raw video buffer');
      goto the_end;
    end;
    video_dst_bufsize := ret;
  end;

  if open_codec_context(@audio_stream_idx, @audio_dec_ctx, fmt_ctx, AVMEDIA_TYPE_AUDIO) >= 0 then
  begin
    audio_stream := PPtrIdx(fmt_ctx.streams, audio_stream_idx);
    audio_dst_file := FileCreate(audio_dst_filename);
    if audio_dst_file = INVALID_HANDLE_VALUE then
    begin
      Writeln(ErrOutput, Format('Could not open destination file %s', [audio_dst_filename]));
      ret := 1;
      goto the_end;
    end;
  end;

  (* dump input information to stderr *)
  av_dump_format(fmt_ctx, 0, PAnsiChar(AnsiString(src_filename)), 0);

  if not Assigned(audio_stream) and not Assigned(video_stream) then
  begin
    Writeln(ErrOutput, 'Could not find audio or video stream in the input, aborting');
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

  pkt := av_packet_alloc();
  if not Assigned(pkt) then
  begin
    Writeln(ErrOutput, 'Could not allocate packet');
    ret := AVERROR_ENOMEM;
    goto the_end;
  end;

  if Assigned(video_stream) then
    Writeln(Format('Demuxing video from file ''%s'' into ''%s''', [src_filename, video_dst_filename]));
  if Assigned(audio_stream) then
    Writeln(Format('Demuxing audio from file ''%s'' into ''%s''', [src_filename, audio_dst_filename]));

  (* read frames from the file *)
  while av_read_frame(fmt_ctx, pkt) >= 0 do
  begin
    // check if the packet belongs to a stream we are interested in, otherwise
    // skip it
    if pkt.stream_index = video_stream_idx then
      ret := decode_packet(video_dec_ctx, pkt)
    else if pkt.stream_index = audio_stream_idx then
      ret := decode_packet(audio_dec_ctx, pkt)
    else
      ret := 0;
    av_packet_unref(pkt);
    if ret < 0 then
      Break;
  end;

  (* flush the decoders *)
  if Assigned(video_dec_ctx) then
    decode_packet(video_dec_ctx, nil);
  if Assigned(audio_dec_ctx) then
    decode_packet(audio_dec_ctx, nil);

  Writeln('Demuxing succeeded.');

  if Assigned(video_stream) then
  begin
    Writeln(Format('Play the output video file with the command:' + sLineBreak +
           'ffplay -f rawvideo -pix_fmt %s -video_size %dx%d %s',
          [string(av_get_pix_fmt_name(pix_fmt)), width, height,
           video_dst_filename]));
  end;

  if Assigned(audio_stream) then
  begin
    sfmt := audio_dec_ctx.sample_fmt;
    n_channels := audio_dec_ctx.channels;

    if av_sample_fmt_is_planar(sfmt) <> 0 then
    begin
      packed_name := av_get_sample_fmt_name(sfmt);
      if Assigned(packed_name) then
        packed_str := string(packed_name)
      else
        packed_str := '?';
      Writeln(Format('Warning: the sample format the decoder produced is planar ' +
             '(%s). This example will output the first channel only.',
             [packed_str]));
      sfmt := av_get_packed_sample_fmt(sfmt);
      n_channels := 1;
    end;

    ret := get_format_from_sample_fmt(@fmt, sfmt);
    if ret < 0 then
      goto the_end;

    Writeln(Format('Play the output audio file with the command:' + sLineBreak +
           'ffplay -f %s -ac %d -ar %d %s',
          [string(fmt), n_channels, audio_dec_ctx.sample_rate,
           audio_dst_filename]));
  end;

the_end:
  avcodec_free_context(@video_dec_ctx);
  avcodec_free_context(@audio_dec_ctx);
  avformat_close_input(@fmt_ctx);
  if video_dst_file <> INVALID_HANDLE_VALUE then
    FileClose(video_dst_file);
  if audio_dst_file <> INVALID_HANDLE_VALUE then
    FileClose(audio_dst_file);
  av_packet_free(@pkt);
  av_frame_free(@frame);
  av_free(video_dst_data[0]);

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
