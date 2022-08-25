(*
 * Copyright (c) 2003 Fabrice Bellard
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
 * libavformat API example.
 *
 * Output a media file in any supported libavformat format. The default
 * codecs are used.
 * @example muxing.c
 *)

(*
 * FFVCL - Delphi FFmpeg VCL Components
 * http://www.DelphiFFmpeg.com
 *
 * Original file: doc/examples/muxing.c
 * Ported by CodeCoolie@CNSW 2014/08/30 -> $Date:: 2021-04-18 #$
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

program muxing;

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
  STREAM_DURATION   = 10;
  STREAM_FRAME_RATE = 25; (* 25 images/s *)
  STREAM_PIX_FMT    = AV_PIX_FMT_YUV420P; (* default pix_fmt *)

  SCALE_FLAGS = SWS_BICUBIC;

type
  // a wrapper around a single output AVStream
  POutputStream = ^TOutputStream;
  TOutputStream = record
    st: PAVStream;
    enc: PAVCodecContext;

    (* pts of the next frame that will be generated *)
    next_pts: Int64;
    samples_count: Integer;

    frame: PAVFrame;
    tmp_frame: PAVFrame;

    t, tincr, tincr2: Single;

    sws_ctx: PSwsContext;
    swr_ctx: PSwrContext;
  end;

procedure log_packet(const fmt_ctx: PAVFormatContext; const pkt: PAVPacket);
var
  time_base: PAVRational;
begin
  time_base := @PAVStream(PPtrIdx(fmt_ctx.streams, pkt.stream_index)).time_base;

  Writeln(Format('pts:%s pts_time:%s dts:%s dts_time:%s duration:%s duration_time:%s stream_index:%d',
        [av_ts2str(pkt.pts), av_ts2timestr(pkt.pts, time_base),
         av_ts2str(pkt.dts), av_ts2timestr(pkt.dts, time_base),
         av_ts2str(pkt.duration), av_ts2timestr(pkt.duration, time_base),
         pkt.stream_index]));
end;

function write_frame(fmt_ctx: PAVFormatContext; c: PAVCodecContext;
  st: PAVStream; frame: PAVFrame): Integer;
var
  ret: Integer;
  pkt: TAVPacket;
begin
  // send the frame to the encoder
  ret := avcodec_send_frame(c, frame);
  if ret < 0 then
  begin
    Writeln(ErrOutput, Format('Error sending a frame to the encoder: %s',
              [string(av_err2str(ret))]));
    Result := ret;
    Exit;
  end;

  while ret >= 0 do
  begin
    FillChar(pkt, SizeOf(TAVPacket), 0);

    ret := avcodec_receive_packet(c, @pkt);
    if (ret = AVERROR_EAGAIN) or (ret = AVERROR_EOF) then
      Break
    else if ret < 0 then
    begin
      Writeln(ErrOutput, Format('Error encoding a frame: %s', [string(av_err2str(ret))]));
      Result := ret;
      Exit;
    end;

    (* rescale output packet timestamp values from codec to stream timebase *)
    av_packet_rescale_ts(@pkt, c.time_base, st.time_base);
    pkt.stream_index := st.index;

    (* Write the compressed frame to the media file. *)
    log_packet(fmt_ctx, @pkt);
    ret := av_interleaved_write_frame(fmt_ctx, @pkt);
    av_packet_unref(@pkt);
    if ret < 0 then
    begin
      Writeln(ErrOutput, Format('Error while writing output packet: %s', [string(av_err2str(ret))]));
      Result := ret;
      Exit;
    end;
  end;

  if ret = AVERROR_EOF then
    Result := 1
  else
    Result := 0;
end;

(* Add an output stream. *)
function add_stream(ost: POutputStream; oc: PAVFormatContext; codec: PPAVCodec;
  codec_id: TAVCodecID): Integer;
var
  c: PAVCodecContext;
  i: Integer;
begin
  (* find the encoder *)
  codec^ := avcodec_find_encoder(codec_id);
  if not Assigned(codec^) then
  begin
    Writeln(ErrOutput, Format('Could not find encoder for ''%s''',
           [avcodec_get_name(codec_id)]));
    Result := -1;
    Exit;
  end;

  ost.st := avformat_new_stream(oc, nil);
  if not Assigned(ost.st) then
  begin
    Writeln(ErrOutput, 'Could not allocate stream');
    Result := -1;
    Exit;
  end;
  ost.st.id := oc.nb_streams-1;
  c := avcodec_alloc_context3(codec^);
  if not Assigned(c) then
  begin
    Writeln(ErrOutput, 'Could not alloc an encoding context');
    Result := -1;
    Exit;
  end;
  ost.enc := c;

  case codec^.ttype of
    AVMEDIA_TYPE_AUDIO:
    begin
      if Assigned(codec^.sample_fmts) then
        c.sample_fmt := codec^.sample_fmts^
      else
        c.sample_fmt := AV_SAMPLE_FMT_FLTP;
      c.bit_rate    := 64000;
      c.sample_rate := 44100;
      if Assigned(codec^.supported_samplerates) then
      begin
        c.sample_rate := codec^.supported_samplerates^;
        i := 0;
        while PPtrIdx(codec^.supported_samplerates, i) <> 0 do
        begin
          if PPtrIdx(codec^.supported_samplerates, i) = 44100 then
            c.sample_rate := 44100;
          Inc(i);
        end;
      end;
      c.channels       := av_get_channel_layout_nb_channels(c.channel_layout);
      c.channel_layout := AV_CH_LAYOUT_STEREO;
      if Assigned(codec^.channel_layouts) then
      begin
        c.channel_layout := codec^.channel_layouts^;
        i := 0;
        while PPtrIdx(codec^.channel_layouts, i) <> 0 do
        begin
          if PPtrIdx(codec^.channel_layouts, i) = AV_CH_LAYOUT_STEREO then
            c.channel_layout := AV_CH_LAYOUT_STEREO;
          Inc(i);
        end;
      end;
      c.channels       := av_get_channel_layout_nb_channels(c.channel_layout);
      ost.st.time_base.num := 1;
      ost.st.time_base.den := c.sample_rate;
    end;
    AVMEDIA_TYPE_VIDEO:
    begin
      c.codec_id := codec_id;

      c.bit_rate := 400000;
      (* Resolution must be a multiple of two. *)
      c.width    := 352;
      c.height   := 288;
      (* timebase: This is the fundamental unit of time (in seconds) in terms
       * of which frame timestamps are represented. For fixed-fps content,
       * timebase should be 1/framerate and timestamp increments should be
       * identical to 1. *)
      ost.st.time_base.num := 1;
      ost.st.time_base.den := STREAM_FRAME_RATE;
      c.time_base          := ost.st.time_base;

      c.gop_size      := 12; (* emit one intra frame every twelve frames at most *)
      c.pix_fmt       := STREAM_PIX_FMT;
      if c.codec_id = AV_CODEC_ID_MPEG2VIDEO then
        (* just for testing, we also add B-frames *)
        c.max_b_frames := 2;
      if c.codec_id = AV_CODEC_ID_MPEG1VIDEO then
        (* Needed to avoid using macroblocks in which some coeffs overflow.
         * This does not happen with normal video, it just happens here as
         * the motion of the chroma plane does not match the luma plane. *)
        c.mb_decision := 2;
    end;
  end;

  (* Some formats want stream headers to be separate. *)
  if (oc.oformat.flags and AVFMT_GLOBALHEADER) <> 0 then
    c.flags := c.flags or AV_CODEC_FLAG_GLOBAL_HEADER;

  Result := 0;
end;

(**************************************************************)
(* audio output *)

function alloc_audio_frame(sample_fmt: TAVSampleFormat; channel_layout: Int64;
  sample_rate, nb_samples: Integer): PAVFrame;
var
  frame: PAVFrame;
  ret: Integer;
begin
  frame := av_frame_alloc();

  if not Assigned(frame) then
  begin
    Writeln(ErrOutput, 'Error allocating an audio frame');
    Result := nil;
    Exit;
  end;

  frame.format := Ord(sample_fmt);
  frame.channel_layout := channel_layout;
  frame.sample_rate := sample_rate;
  frame.nb_samples := nb_samples;

  if nb_samples <> 0 then
  begin
    ret := av_frame_get_buffer(frame, 0);
    if ret < 0 then
    begin
      Writeln(ErrOutput, 'Error allocating an audio buffer');
      Result := nil;
      Exit;
    end;
  end;

  Result := frame;
end;

function open_audio(oc: PAVFormatContext; codec: PAVCodec; ost: POutputStream; opt_arg: PAVDictionary): Integer;
var
  c: PAVCodecContext;
  nb_samples: Integer;
  ret: Integer;
  opt: PAVDictionary;
begin
  opt := nil;
  c := ost.enc;

  (* open it *)
  av_dict_copy(@opt, opt_arg, 0);
  ret := avcodec_open2(c, codec, @opt);
  av_dict_free(@opt);
  if ret < 0 then
  begin
    Writeln(ErrOutput, Format('Could not open audio codec: %s', [av_err2str(ret)]));
    Result := -1;
    Exit;
  end;

  (* init signal generator *)
  ost.t     := 0;
  ost.tincr := 2 * M_PI * 110.0 / c.sample_rate;
  (* increment frequency by 110 Hz per second *)
  ost.tincr2 := 2 * M_PI * 110.0 / c.sample_rate / c.sample_rate;

  if (c.codec.capabilities and AV_CODEC_CAP_VARIABLE_FRAME_SIZE) <> 0 then
    nb_samples := 10000
  else
    nb_samples := c.frame_size;

  ost.frame     := alloc_audio_frame(c.sample_fmt, c.channel_layout, c.sample_rate, nb_samples);
  if not Assigned(ost.frame) then
  begin
    Result := -1;
    Exit;
  end;
  ost.tmp_frame := alloc_audio_frame(AV_SAMPLE_FMT_S16, c.channel_layout, c.sample_rate, nb_samples);
  if not Assigned(ost.tmp_frame) then
  begin
    Result := -1;
    Exit;
  end;

  (* copy the stream parameters to the muxer *)
  ret := avcodec_parameters_from_context(ost.st.codecpar, c);
  if ret < 0 then
  begin
    Writeln(ErrOutput, 'Could not copy the stream parameters');
    Result := -1;
    Exit;
  end;

  (* create resampler context *)
  ost.swr_ctx := swr_alloc();
  if not Assigned(ost.swr_ctx) then
  begin
    Writeln(ErrOutput, 'Could not allocate resampler context');
    Result := -1;
    Exit;
  end;

  (* set options *)
  av_opt_set_int       (ost.swr_ctx, 'in_channel_count',   c.channels,       0);
  av_opt_set_int       (ost.swr_ctx, 'in_sample_rate',     c.sample_rate,    0);
  av_opt_set_sample_fmt(ost.swr_ctx, 'in_sample_fmt',      AV_SAMPLE_FMT_S16, 0);
  av_opt_set_int       (ost.swr_ctx, 'out_channel_count',  c.channels,       0);
  av_opt_set_int       (ost.swr_ctx, 'out_sample_rate',    c.sample_rate,    0);
  av_opt_set_sample_fmt(ost.swr_ctx, 'out_sample_fmt',     c.sample_fmt,     0);

  (* initialize the resampling context *)
  ret := swr_init(ost.swr_ctx);
  if ret < 0 then
  begin
    Writeln(ErrOutput, 'Failed to initialize the resampling context');
    Result := -1;
    Exit;
  end;

  Result := 0;
end;

(* Prepare a 16 bit dummy audio frame of 'frame_size' samples and
 * 'nb_channels' channels. *)
function get_audio_frame(ost: POutputStream; frame: PPAVFrame): Integer;
var
  tmp_frame: PAVFrame;
  j, i, v: Integer;
  q: PSmallInt;
begin
  tmp_frame := ost.tmp_frame;
  q := PSmallInt(tmp_frame.data[0]);

  (* check if we want to generate more frames *)
  if av_compare_ts(ost.next_pts, ost.enc.time_base,
                    STREAM_DURATION, av_make_q(1, 1)) > 0 then
  begin
    frame^ := nil;
    Result := 0;
    Exit;
  end;

  for j := 0 to tmp_frame.nb_samples - 1 do
  begin
    v := Trunc(Sin(ost.t) * 10000);
    for i := 0 to ost.enc.channels - 1 do
    begin
      q^ := v;
      Inc(q);
    end;
    ost.t     := ost.t + ost.tincr;
    ost.tincr := ost.tincr + ost.tincr2;
  end;

  tmp_frame.pts := ost.next_pts;
  Inc(ost.next_pts, tmp_frame.nb_samples);

  frame^ := tmp_frame;
  Result := 0;
end;

(*
 * encode one audio frame and send it to the muxer
 * return 1 when encoding is finished, 0 otherwise
 *)
function write_audio_frame(oc: PAVFormatContext; ost: POutputStream): Integer;
var
  c: PAVCodecContext;
  frame: PAVFrame;
  ret: Integer;
  dst_nb_samples: Integer;
begin
  c := ost.enc;

  if get_audio_frame(ost, @frame) < 0 then
  begin
    Result := -1;
    Exit;
  end;

  if Assigned(frame) then
  begin
    (* convert samples from native format to destination codec format, using the resampler *)
    (* compute destination number of samples *)
    dst_nb_samples := av_rescale_rnd(swr_get_delay(ost.swr_ctx, c.sample_rate) + frame.nb_samples,
                                    c.sample_rate, c.sample_rate, AV_ROUND_UP);
    Assert(dst_nb_samples = frame.nb_samples);

    (* when we pass a frame to the encoder, it may keep a reference to it
     * internally;
     * make sure we do not overwrite it here
     *)
    ret := av_frame_make_writable(ost.frame);
    if ret < 0 then
    begin
      Writeln(ErrOutput, 'av_frame_make_writable() failed');
      Result := -1;
      Exit;
    end;

    (* convert to destination format *)
    ret := swr_convert(ost.swr_ctx,
                      @ost.frame.data[0], dst_nb_samples,
                      @frame.data[0], frame.nb_samples);
    if ret < 0 then
    begin
      Writeln(ErrOutput, 'Error while converting');
      Result := -1;
      Exit;
    end;
    frame := ost.frame;

    frame.pts := av_rescale_q(ost.samples_count, av_make_q(1, c.sample_rate), c.time_base);
    Inc(ost.samples_count, dst_nb_samples);
  end;

  Result := write_frame(oc, c, ost.st, frame);
end;

(**************************************************************)
(* video output *)

function alloc_picture(pix_fmt: TAVPixelFormat; width, height: Integer): PAVFrame;
var
  picture: PAVFrame;
  ret: Integer;
begin
  picture := av_frame_alloc();
  if not Assigned(picture) then
  begin
    Result := nil;
    Exit;
  end;

  picture.format := Ord(pix_fmt);
  picture.width  := width;
  picture.height := height;

  (* allocate the buffers for the frame data *)
  ret := av_frame_get_buffer(picture, 0);
  if ret < 0 then
  begin
    Writeln(ErrOutput, 'Could not allocate frame data.');
    Result := nil;
    Exit;
  end;

  Result := picture;
end;

function open_video(oc: PAVFormatContext; codec: PAVCodec; ost: POutputStream; opt_arg: PAVDictionary): Integer;
var
  ret: Integer;
  c: PAVCodecContext;
  opt: PAVDictionary;
begin
  c := ost.enc;
  opt := nil;

  av_dict_copy(@opt, opt_arg, 0);

  (* open the codec *)
  ret := avcodec_open2(c, codec, @opt);
  av_dict_free(@opt);
  if ret < 0 then
  begin
    Writeln(ErrOutput, Format('Could not open video codec: %s', [av_err2str(ret)]));
    Result := -1;
    Exit;
  end;

  (* allocate and init a re-usable frame *)
  ost.frame := alloc_picture(c.pix_fmt, c.width, c.height);
  if not Assigned(ost.frame) then
  begin
    Writeln(ErrOutput, 'Could not allocate video frame');
    Result := -1;
    Exit;
  end;

  (* If the output format is not YUV420P, then a temporary YUV420P
   * picture is needed too. It is then converted to the required
   * output format. *)
  ost.tmp_frame := nil;
  if c.pix_fmt <> AV_PIX_FMT_YUV420P then
  begin
    ost.tmp_frame := alloc_picture(AV_PIX_FMT_YUV420P, c.width, c.height);
    if not Assigned(ost.tmp_frame) then
    begin
      Writeln(ErrOutput, 'Could not allocate temporary picture');
      Result := -1;
      Exit;
    end;
  end;

  (* copy the stream parameters to the muxer *)
  ret := avcodec_parameters_from_context(ost.st.codecpar, c);
  if ret < 0 then
  begin
    Writeln(ErrOutput, 'Could not copy the stream parameters');
    Result := -1;
    Exit;
  end;

  Result := 0;
end;

(* Prepare a dummy image. *)
function fill_yuv_image(pict: PAVFrame; frame_index, width, height: Integer): Integer;
var
  x, y, i: Integer;
begin
  i := frame_index;

  (* Y *)
  for y := 0 to height - 1 do
    for x := 0 to width - 1 do
      PByte(@PAnsiChar(pict.data[0])[y * pict.linesize[0] + x])^ := x + y + i * 3;

  (* Cb and Cr *)
  for y := 0 to height div 2 - 1 do
    for x := 0 to width div 2 - 1 do
    begin
      PByte(@PAnsiChar(pict.data[1])[y * pict.linesize[1] + x])^ := 128 + y + i * 2;
      PByte(@PAnsiChar(pict.data[2])[y * pict.linesize[2] + x])^ := 64 + x + i * 5;
    end;

  Result := 0;
end;

function get_video_frame(ost: POutputStream; frame: PPAVFrame): Integer;
var
  c: PAVCodecContext;
begin
  c := ost.enc;

  (* check if we want to generate more frames *)
  if av_compare_ts(ost.next_pts, ost.enc.time_base,
                    STREAM_DURATION, av_make_q(1, 1)) > 0 then
  begin
    frame^ := nil;
    Result := 0;
    Exit;
  end;

  (* when we pass a frame to the encoder, it may keep a reference to it
   * internally; make sure we do not overwrite it here *)
  if av_frame_make_writable(ost.frame) < 0 then
  begin
    Writeln(ErrOutput, 'av_frame_make_writable() failed');
    Result := -1;
    Exit;
  end;

  if c.pix_fmt <> AV_PIX_FMT_YUV420P then
  begin
    (* as we only generate a YUV420P picture, we must convert it
     * to the codec pixel format if needed *)
    if not Assigned(ost.sws_ctx) then
    begin
      ost.sws_ctx := sws_getContext(c.width, c.height,
                                    AV_PIX_FMT_YUV420P,
                                    c.width, c.height,
                                    c.pix_fmt,
                                    SCALE_FLAGS, nil, nil, nil);
      if not Assigned(ost.sws_ctx) then
      begin
        Writeln(ErrOutput, 'Could not initialize the conversion context');
        Result := -1;
        Exit;
      end;
    end;
    if fill_yuv_image(ost.tmp_frame, ost.next_pts, c.width, c.height) < 0 then
    begin
      Result := -1;
      Exit;
    end;
    sws_scale(ost.sws_ctx,
                  @ost.tmp_frame.data[0], @ost.tmp_frame.linesize[0],
                  0, c.height, @ost.frame.data[0], @ost.frame.linesize[0]);
  end
  else
    if fill_yuv_image(ost.frame, ost.next_pts, c.width, c.height) < 0 then
    begin
      Result := -1;
      Exit;
    end;

  ost.frame.pts := ost.next_pts;
  Inc(ost.next_pts);

  frame^ := ost.frame;
  Result := 0;
end;

(*
 * encode one video frame and send it to the muxer
 * return 1 when encoding is finished, 0 otherwise
 *)
function write_video_frame(oc: PAVFormatContext; ost: POutputStream): Integer;
var
  frame: PAVFrame;
begin
  if get_video_frame(ost, @frame) < 0 then
  begin
    Result := -1;
    Exit;
  end;
  Result := write_frame(oc, ost.enc, ost.st, frame);
end;

procedure close_stream(oc: PAVFormatContext; ost: POutputStream);
begin
  avcodec_free_context(@ost.enc);
  av_frame_free(@ost.frame);
  av_frame_free(@ost.tmp_frame);
  sws_freeContext(ost.sws_ctx);
  swr_free(@ost.swr_ctx);
end;

(**************************************************************)
(* media file output *)

function main(): Integer;
var
  video_st, audio_st: TOutputStream;
  filename: string;
  fmt: PAVOutputFormat;
  oc: PAVFormatContext;
  audio_codec, video_codec: PAVCodec;
  ret: Integer;
  have_video, have_audio: Integer;
  encode_video, encode_audio: Integer;
  opt: PAVDictionary;
  i: Integer;
begin
  Set8087CW($133F); { Disable all fpu exceptions }
  opt := nil;

  FillChar(video_st, SizeOf(video_st), 0);
  FillChar(audio_st, SizeOf(audio_st), 0);
  have_video := 0;
  have_audio := 0;
  encode_video := 0;
  encode_audio := 0;

  if ParamCount < 1 then
  begin
    Writeln(Format('usage: %s output_file' + sLineBreak +
           'API example program to output a media file with libavformat.' + sLineBreak +
           'This program generates a synthetic audio and video stream, encodes and' + sLineBreak +
           'muxes them into a file named output_file.' + sLineBreak +
           'The output format is automatically guessed according to the file extension.' + sLineBreak +
           'Raw images can also be output by using ''%%d'' in the filename.',
           [ExtractFileName(ParamStr(0))]));
    Result := 1;
    Exit;
  end;

  filename := ParamStr(1);
  if (ParamCount > 2) and (ParamStr(2) = '-flags') then
    av_dict_set(@opt, PAnsiChar(AnsiString(Copy(ParamStr(2), 2, MaxInt))), PAnsiChar(AnsiString(ParamStr(3))), 0);
  i := 2;
  while i < ParamCount do
  begin
    if (ParamStr(i) = '-flags') or (ParamStr(i) = '-fflags') then
      av_dict_set(@opt, PAnsiChar(AnsiString(Copy(ParamStr(i), 2, MaxInt))), PAnsiChar(AnsiString(ParamStr(i + 1))), 0);
    Inc(i, 2);
  end;

  (* allocate the output media context *)
  avformat_alloc_output_context2(@oc, nil, nil, PAnsiChar(AnsiString(filename)));
  if not Assigned(oc) then
  begin
    Writeln('Could not deduce output format from file extension: using MPEG.');
    avformat_alloc_output_context2(@oc, nil, 'mpeg', PAnsiChar(AnsiString(filename)));
  end;
  if not Assigned(oc) then
  begin
    Result := 1;
    Exit;
  end;

  fmt := oc.oformat;

  (* Add the audio and video streams using the default format codecs
   * and initialize the codecs. *)
  if fmt.video_codec <> AV_CODEC_ID_NONE then
  begin
    if add_stream(@video_st, oc, @video_codec, fmt.video_codec) < 0 then
    begin
      Result := 1;
      Exit;
    end;
    have_video := 1;
    encode_video := 1;
  end;
  if fmt.audio_codec <> AV_CODEC_ID_NONE then
  begin
    if add_stream(@audio_st, oc, @audio_codec, fmt.audio_codec) < 0 then
    begin
      Result := 1;
      Exit;
    end;
    have_audio := 1;
    encode_audio := 1;
  end;

  (* Now that all the parameters are set, we can open the audio and
   * video codecs and allocate the necessary encode buffers. *)
  if have_video <> 0 then
    if open_video(oc, video_codec, @video_st, opt) < 0 then
    begin
      Result := 1;
      Exit;
    end;

  if have_audio <> 0 then
    if open_audio(oc, audio_codec, @audio_st, opt) < 0 then
    begin
      Result := 1;
      Exit;
    end;

  av_dump_format(oc, 0, PAnsiChar(AnsiString(filename)), 1);
  (* open the output file, if needed *)
  if (fmt.flags and AVFMT_NOFILE) = 0 then
  begin
    ret := avio_open(@oc.pb, PAnsiChar(AnsiString(filename)), AVIO_FLAG_WRITE);
    if ret < 0 then
    begin
      Writeln(ErrOutput, Format('Could not open ''%s'': %s', [filename,
              av_err2str(ret)]));
      Result := 1;
      Exit;
    end;
  end;

  (* Write the stream header, if any. *)
  ret := avformat_write_header(oc, @opt);
  if ret < 0 then
  begin
    Writeln(ErrOutput, Format('Error occurred when opening output file: %s',
            [av_err2str(ret)]));
    Result := 1;
    Exit;
  end;

  while (encode_video <> 0) or (encode_audio <> 0) do
  begin
    (* select the stream to encode *)
    if (encode_video <> 0) and
      ((encode_audio = 0) or (av_compare_ts(video_st.next_pts, video_st.enc.time_base,
                                            audio_st.next_pts, audio_st.enc.time_base) <= 0)) then
    begin
      encode_video := write_video_frame(oc, @video_st);
      if encode_video < 0 then
      begin
        Result := 1;
        Exit;
      end;
      encode_video := 1 - encode_video;
    end
    else
    begin
      encode_audio := write_audio_frame(oc, @audio_st);
      if encode_audio < 0 then
      begin
        Result := 1;
        Exit;
      end;
      encode_audio := 1 - encode_audio;
    end;
  end;

  (* Write the trailer, if any. The trailer must be written before you
   * close the CodecContexts open when you wrote the header; otherwise
   * av_write_trailer() may try to use memory that was freed on
   * av_codec_close(). *)
  av_write_trailer(oc);

  (* Close each codec. *)
  if have_video <> 0 then
    close_stream(oc, @video_st);
  if have_audio <> 0 then
    close_stream(oc, @audio_st);

  if (fmt.flags and AVFMT_NOFILE) = 0 then
    (* Close the output file. *)
    avio_closep(@oc.pb);

  (* free the stream *)
  avformat_free_context(oc);

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
