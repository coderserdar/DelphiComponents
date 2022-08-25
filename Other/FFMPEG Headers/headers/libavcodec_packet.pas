(*
 * AVPacket public API
 *
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
 * Original file: libavcodec/packet.h
 * Ported by CodeCoolie@CNSW 2020/11/11 -> $Date:: 2021-04-25 #$
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

unit libavcodec_packet;

interface

{$I CompilerDefines.inc}

uses
  FFTypes,
  libavutil_buffer,
  libavutil_dict,
  libavutil_rational;

{$I libversion.inc}

(**
 * @defgroup lavc_packet AVPacket
 *
 * Types and functions for working with AVPacket.
 * @{
 *)
type
  TAVPacketSideDataType = (
    (**
     * An AV_PKT_DATA_PALETTE side data packet contains exactly AVPALETTE_SIZE
     * bytes worth of palette. This side data signals that a new palette is
     * present.
     *)
    AV_PKT_DATA_PALETTE,

    (**
     * The AV_PKT_DATA_NEW_EXTRADATA is used to notify the codec or the format
     * that the extradata buffer was changed and the receiving side should
     * act upon it appropriately. The new extradata is embedded in the side
     * data buffer and should be immediately used for processing the current
     * frame or packet.
     *)
    AV_PKT_DATA_NEW_EXTRADATA,

    (**
     * An AV_PKT_DATA_PARAM_CHANGE side data packet is laid out as follows:
     * @code
     * u32le param_flags
     * if (param_flags & AV_SIDE_DATA_PARAM_CHANGE_CHANNEL_COUNT)
     *     s32le channel_count
     * if (param_flags & AV_SIDE_DATA_PARAM_CHANGE_CHANNEL_LAYOUT)
     *     u64le channel_layout
     * if (param_flags & AV_SIDE_DATA_PARAM_CHANGE_SAMPLE_RATE)
     *     s32le sample_rate
     * if (param_flags & AV_SIDE_DATA_PARAM_CHANGE_DIMENSIONS)
     *     s32le width
     *     s32le height
     * @endcode
     *)
    AV_PKT_DATA_PARAM_CHANGE,

    (**
     * An AV_PKT_DATA_H263_MB_INFO side data packet contains a number of
     * structures with info about macroblocks relevant to splitting the
     * packet into smaller packets on macroblock edges (e.g. as for RFC 2190).
     * That is, it does not necessarily contain info about all macroblocks,
     * as long as the distance between macroblocks in the info is smaller
     * than the target payload size.
     * Each MB info structure is 12 bytes, and is laid out as follows:
     * @code
     * u32le bit offset from the start of the packet
     * u8    current quantizer at the start of the macroblock
     * u8    GOB number
     * u16le macroblock address within the GOB
     * u8    horizontal MV predictor
     * u8    vertical MV predictor
     * u8    horizontal MV predictor for block number 3
     * u8    vertical MV predictor for block number 3
     * @endcode
     *)
    AV_PKT_DATA_H263_MB_INFO,

    (**
     * This side data should be associated with an audio stream and contains
     * ReplayGain information in form of the AVReplayGain struct.
     *)
    AV_PKT_DATA_REPLAYGAIN,

    (**
     * This side data contains a 3x3 transformation matrix describing an affine
     * transformation that needs to be applied to the decoded video frames for
     * correct presentation.
     *
     * See libavutil/display.h for a detailed description of the data.
     *)
    AV_PKT_DATA_DISPLAYMATRIX,

    (**
     * This side data should be associated with a video stream and contains
     * Stereoscopic 3D information in form of the AVStereo3D struct.
     *)
    AV_PKT_DATA_STEREO3D,

    (**
     * This side data should be associated with an audio stream and corresponds
     * to enum AVAudioServiceType.
     *)
    AV_PKT_DATA_AUDIO_SERVICE_TYPE,

    (**
     * This side data contains quality related information from the encoder.
     * @code
     * u32le quality factor of the compressed frame. Allowed range is between 1 (good) and FF_LAMBDA_MAX (bad).
     * u8    picture type
     * u8    error count
     * u16   reserved
     * u64le[error count] sum of squared differences between encoder in and output
     * @endcode
     *)
    AV_PKT_DATA_QUALITY_STATS,

    (**
     * This side data contains an integer value representing the stream index
     * of a "fallback" track.  A fallback track indicates an alternate
     * track to use when the current track can not be decoded for some reason.
     * e.g. no decoder available for codec.
     *)
    AV_PKT_DATA_FALLBACK_TRACK,

    (**
     * This side data corresponds to the AVCPBProperties struct.
     *)
    AV_PKT_DATA_CPB_PROPERTIES,

    (**
     * Recommmends skipping the specified number of samples
     * @code
     * u32le number of samples to skip from start of this packet
     * u32le number of samples to skip from end of this packet
     * u8    reason for start skip
     * u8    reason for end   skip (0=padding silence, 1=convergence)
     * @endcode
     *)
    AV_PKT_DATA_SKIP_SAMPLES,

    (**
     * An AV_PKT_DATA_JP_DUALMONO side data packet indicates that
     * the packet may contain "dual mono" audio specific to Japanese DTV
     * and if it is true, recommends only the selected channel to be used.
     * @code
     * u8    selected channels (0=mail/left, 1=sub/right, 2=both)
     * @endcode
     *)
    AV_PKT_DATA_JP_DUALMONO,

    (**
     * A list of zero terminated key/value strings. There is no end marker for
     * the list, so it is required to rely on the side data size to stop.
     *)
    AV_PKT_DATA_STRINGS_METADATA,

    (**
     * Subtitle event position
     * @code
     * u32le x1
     * u32le y1
     * u32le x2
     * u32le y2
     * @endcode
     *)
    AV_PKT_DATA_SUBTITLE_POSITION,

    (**
     * Data found in BlockAdditional element of matroska container. There is
     * no end marker for the data, so it is required to rely on the side data
     * size to recognize the end. 8 byte id (as found in BlockAddId) followed
     * by data.
     *)
    AV_PKT_DATA_MATROSKA_BLOCKADDITIONAL,

    (**
     * The optional first identifier line of a WebVTT cue.
     *)
    AV_PKT_DATA_WEBVTT_IDENTIFIER,

    (**
     * The optional settings (rendering instructions) that immediately
     * follow the timestamp specifier of a WebVTT cue.
     *)
    AV_PKT_DATA_WEBVTT_SETTINGS,

    (**
     * A list of zero terminated key/value strings. There is no end marker for
     * the list, so it is required to rely on the side data size to stop. This
     * side data includes updated metadata which appeared in the stream.
     *)
    AV_PKT_DATA_METADATA_UPDATE,

    (**
     * MPEGTS stream ID as uint8_t, this is required to pass the stream ID
     * information from the demuxer to the corresponding muxer.
     *)
    AV_PKT_DATA_MPEGTS_STREAM_ID,

    (**
     * Mastering display metadata (based on SMPTE-2086:2014). This metadata
     * should be associated with a video stream and contains data in the form
     * of the AVMasteringDisplayMetadata struct.
     *)
    AV_PKT_DATA_MASTERING_DISPLAY_METADATA,

    (**
     * This side data should be associated with a video stream and corresponds
     * to the AVSphericalMapping structure.
     *)
    AV_PKT_DATA_SPHERICAL,

    (**
     * Content light level (based on CTA-861.3). This metadata should be
     * associated with a video stream and contains data in the form of the
     * AVContentLightMetadata struct.
     *)
    AV_PKT_DATA_CONTENT_LIGHT_LEVEL,

    (**
     * ATSC A53 Part 4 Closed Captions. This metadata should be associated with
     * a video stream. A53 CC bitstream is stored as uint8_t in AVPacketSideData.data.
     * The number of bytes of CC data is AVPacketSideData.size.
     *)
    AV_PKT_DATA_A53_CC,

    (**
     * This side data is encryption initialization data.
     * The format is not part of ABI, use av_encryption_init_info_* methods to
     * access.
     *)
    AV_PKT_DATA_ENCRYPTION_INIT_INFO,

    (**
     * This side data contains encryption info for how to decrypt the packet.
     * The format is not part of ABI, use av_encryption_info_* methods to access.
     *)
    AV_PKT_DATA_ENCRYPTION_INFO,

    (**
     * Active Format Description data consisting of a single byte as specified
     * in ETSI TS 101 154 using AVActiveFormatDescription enum.
     *)
    AV_PKT_DATA_AFD,

    (**
     * Producer Reference Time data corresponding to the AVProducerReferenceTime struct,
     * usually exported by some encoders (on demand through the prft flag set in the
     * AVCodecContext export_side_data field).
     *)
    AV_PKT_DATA_PRFT,

    (**
     * ICC profile data consisting of an opaque octet buffer following the
     * format described by ISO 15076-1.
     *)
    AV_PKT_DATA_ICC_PROFILE,

    (**
     * DOVI configuration
     * ref:
     * dolby-vision-bitstreams-within-the-iso-base-media-file-format-v2.1.2, section 2.2
     * dolby-vision-bitstreams-in-mpeg-2-transport-stream-multiplex-v1.2, section 3.3
     * Tags are stored in struct AVDOVIDecoderConfigurationRecord.
     *)
    AV_PKT_DATA_DOVI_CONF,

    (**
     * Timecode which conforms to SMPTE ST 12-1:2014. The data is an array of 4 uint32_t
     * where the first uint32_t describes how many (1-3) of the other timecodes are used.
     * The timecode format is described in the documentation of av_timecode_get_smpte_from_framenum()
     * function in libavutil/timecode.h.
     *)
    AV_PKT_DATA_S12M_TIMECODE,

    (**
     * The number of side data types.
     * This is not part of the public API/ABI in the sense that it may
     * change when new side data types are added.
     * This must stay the last enum value.
     * If its value becomes huge, some code using it
     * needs to be updated as it assumes it to be smaller than other limits.
     *)
    AV_PKT_DATA_NB
  );

const
  AV_PKT_DATA_QUALITY_FACTOR = AV_PKT_DATA_QUALITY_STATS; //DEPRECATED

type
  PAVPacketSideData = ^TAVPacketSideData;
  TAVPacketSideData = record
    data: PByte;
{$IFDEF FF_API_BUFFER_SIZE_T}
    size: Integer;
{$ELSE}
    size: Size_t;
{$ENDIF}
    type_: TAVPacketSideDataType;
  end;

(**
 * This structure stores compressed data. It is typically exported by demuxers
 * and then passed as input to decoders, or received as output from encoders and
 * then passed to muxers.
 *
 * For video, it should typically contain one compressed frame. For audio it may
 * contain several compressed frames. Encoders are allowed to output empty
 * packets, with no compressed data, containing only side data
 * (e.g. to update some stream parameters at the end of encoding).
 *
 * AVPacket is one of the few structs in FFmpeg, whose size is a part of public
 * ABI. Thus it may be allocated on stack and no new fields can be added to it
 * without libavcodec and libavformat major bump.
 *
 * The semantics of data ownership depends on the buf field.
 * If it is set, the packet data is dynamically allocated and is
 * valid indefinitely until a call to av_packet_unref() reduces the
 * reference count to 0.
 *
 * If the buf field is not set av_packet_ref() would make a copy instead
 * of increasing the reference count.
 *
 * The side data is always allocated with av_malloc(), copied by
 * av_packet_ref() and freed by av_packet_unref().
 *
 * sizeof(AVPacket) being a part of the public ABI is deprecated. once
 * av_init_packet() is removed, new packets will only be able to be allocated
 * with av_packet_alloc(), and new fields may be added to the end of the struct
 * with a minor bump.
 *
 * @see av_packet_alloc
 * @see av_packet_ref
 * @see av_packet_unref
 *)
  PPAVPacket = ^PAVPacket;
  PAVPacket = ^TAVPacket;
  TAVPacket = record
    (**
     * A reference to the reference-counted buffer where the packet data is
     * stored.
     * May be NULL, then the packet data is not reference-counted.
     *)
    buf: PAVBufferRef;
    (**
     * Presentation timestamp in AVStream->time_base units; the time at which
     * the decompressed packet will be presented to the user.
     * Can be AV_NOPTS_VALUE if it is not stored in the file.
     * pts MUST be larger or equal to dts as presentation cannot happen before
     * decompression, unless one wants to view hex dumps. Some formats misuse
     * the terms dts and pts/cts to mean something different. Such timestamps
     * must be converted to true pts/dts before they are stored in AVPacket.
     *)
    pts: Int64;
    (**
     * Decompression timestamp in AVStream->time_base units; the time at which
     * the packet is decompressed.
     * Can be AV_NOPTS_VALUE if it is not stored in the file.
     *)
    dts: Int64;
    data: PByte;
    size: Integer;
    stream_index: Integer;
    (**
     * A combination of AV_PKT_FLAG values
     *)
    flags: Integer;
    (**
     * Additional packet data that can be provided by the container.
     * Packet can contain several types of side information.
     *)
    side_data: PAVPacketSideData;
    side_data_elems: Integer;

    (**
     * Duration of this packet in AVStream->time_base units, 0 if unknown.
     * Equals next_pts - this_pts in presentation order.
     *)
    duration: Int64;

    pos: Int64;             ///< byte position in stream, -1 if unknown

{$IFDEF FF_API_CONVERGENCE_DURATION}
    (**
     * @deprecated Same as the duration field, but as int64_t. This was required
     * for Matroska subtitles, whose duration values could overflow when the
     * duration field was still an int.
     *)
    convergence_duration: Int64;
{$ENDIF}
  end;

{$IFDEF FF_API_INIT_PACKET}
  PAVPacketList = ^TAVPacketList;
  TAVPacketList = record
    pkt: TAVPacket;
    next: PAVPacketList;
  end;
{$ENDIF}

const
  AV_PKT_FLAG_KEY     = $0001; ///< The packet contains a keyframe
  AV_PKT_FLAG_CORRUPT = $0002; ///< The packet content is corrupted
(**
 * Flag is used to discard packets which are required to maintain valid
 * decoder state but are not required for output and should be dropped
 * after decoding.
 **)
  AV_PKT_FLAG_DISCARD = $0004;
(**
 * The packet comes from a trusted source.
 *
 * Otherwise-unsafe constructs such as arbitrary pointers to data
 * outside the packet may be followed.
 *)
  AV_PKT_FLAG_TRUSTED = $0008;
(**
 * Flag is used to indicate packets that contain frames that can
 * be discarded by the decoder.  I.e. Non-reference frames.
 *)
  AV_PKT_FLAG_DISPOSABLE = $0010;

type
  TAVSideDataParamChangeFlags = (
    AV_SIDE_DATA_PARAM_CHANGE_CHANNEL_COUNT  = $0001,
    AV_SIDE_DATA_PARAM_CHANGE_CHANNEL_LAYOUT = $0002,
    AV_SIDE_DATA_PARAM_CHANGE_SAMPLE_RATE    = $0004,
    AV_SIDE_DATA_PARAM_CHANGE_DIMENSIONS     = $0008
  );

(**
 * Allocate an AVPacket and set its fields to default values.  The resulting
 * struct must be freed using av_packet_free().
 *
 * @return An AVPacket filled with default values or NULL on failure.
 *
 * @note this only allocates the AVPacket itself, not the data buffers. Those
 * must be allocated through other means such as av_new_packet.
 *
 * @see av_new_packet
 *)
function av_packet_alloc: PAVPacket; cdecl; external AVCODEC_LIBNAME name _PU + 'av_packet_alloc';

(**
 * Create a new packet that references the same data as src.
 *
 * This is a shortcut for av_packet_alloc()+av_packet_ref().
 *
 * @return newly created AVPacket on success, NULL on error.
 *
 * @see av_packet_alloc
 * @see av_packet_ref
 *)
function av_packet_clone(const src: PAVPacket): PAVPacket; cdecl; external AVCODEC_LIBNAME name _PU + 'av_packet_clone';

(**
 * Free the packet, if the packet is reference counted, it will be
 * unreferenced first.
 *
 * @param pkt packet to be freed. The pointer will be set to NULL.
 * @note passing NULL is a no-op.
 *)
procedure av_packet_free(pkt: PPAVPacket); cdecl; external AVCODEC_LIBNAME name _PU + 'av_packet_free';

{$IFDEF FF_API_INIT_PACKET}
(**
 * Initialize optional fields of a packet with default values.
 *
 * Note, this does not touch the data and size members, which have to be
 * initialized separately.
 *
 * @param pkt packet
 *
 * @see av_packet_alloc
 * @see av_packet_unref
 *
 * @deprecated This function is deprecated. Once it's removed,
               sizeof(AVPacket) will not be a part of the ABI anymore.
 *)
procedure av_init_packet(pkt: PAVPacket); cdecl; external AVCODEC_LIBNAME name _PU + 'av_init_packet';
{$ENDIF}

(**
 * Allocate the payload of a packet and initialize its fields with
 * default values.
 *
 * @param pkt packet
 * @param size wanted payload size
 * @return 0 if OK, AVERROR_xxx otherwise
 *)
function av_new_packet(pkt: PAVPacket; size: Integer): Integer; cdecl; external AVCODEC_LIBNAME name _PU + 'av_new_packet';

(**
 * Reduce packet size, correctly zeroing padding
 *
 * @param pkt packet
 * @param size new size
 *)
procedure av_shrink_packet(pkt: PAVPacket; size: Integer); cdecl; external AVCODEC_LIBNAME name _PU + 'av_shrink_packet';

(**
 * Increase packet size, correctly zeroing padding
 *
 * @param pkt packet
 * @param grow_by number of bytes by which to increase the size of the packet
 *)
function av_grow_packet(pkt: PAVPacket; grow_by: Integer): Integer; cdecl; external AVCODEC_LIBNAME name _PU + 'av_grow_packet';

(**
 * Initialize a reference-counted packet from av_malloc()ed data.
 *
 * @param pkt packet to be initialized. This function will set the data, size,
 *        and buf fields, all others are left untouched.
 * @param data Data allocated by av_malloc() to be used as packet data. If this
 *        function returns successfully, the data is owned by the underlying AVBuffer.
 *        The caller may not access the data through other means.
 * @param size size of data in bytes, without the padding. I.e. the full buffer
 *        size is assumed to be size + AV_INPUT_BUFFER_PADDING_SIZE.
 *
 * @return 0 on success, a negative AVERROR on error
 *)
function av_packet_from_data(pkt: PAVPacket; data: PByte; size: Integer): Integer; cdecl; external AVCODEC_LIBNAME name _PU + 'av_packet_from_data';

{$IFDEF FF_API_AVPACKET_OLD_API}
(**
 * @warning This is a hack - the packet memory allocation stuff is broken. The
 * packet is allocated if it was not really allocated.
 *
 * @deprecated Use av_packet_ref or av_packet_make_refcounted
 *)
function av_dup_packet(pkt: PAVPacket): Integer; cdecl; external AVCODEC_LIBNAME name _PU + 'av_dup_packet';

(**
 * Copy packet, including contents
 *
 * @return 0 on success, negative AVERROR on fail
 *
 * @deprecated Use av_packet_ref
 *)
function av_copy_packet(dst: PAVPacket; const src: PAVPacket): Integer; cdecl; external AVCODEC_LIBNAME name _PU + 'av_copy_packet';

(**
 * Copy packet side data
 *
 * @return 0 on success, negative AVERROR on fail
 *
 * @deprecated Use av_packet_copy_props
 *)
function av_copy_packet_side_data(dst: PAVPacket; const src: PAVPacket): Integer; cdecl; external AVCODEC_LIBNAME name _PU + 'av_copy_packet_side_data';

(**
 * Free a packet.
 *
 * @deprecated Use av_packet_unref
 *
 * @param pkt packet to free
 *)
procedure av_free_packet(pkt: PAVPacket); cdecl; external AVCODEC_LIBNAME name _PU + 'av_free_packet';
{$ENDIF}

(**
 * Allocate new information of a packet.
 *
 * @param pkt packet
 * @param type side information type
 * @param size side information size
 * @return pointer to fresh allocated data or NULL otherwise
 *)
function av_packet_new_side_data(pkt: PAVPacket; type_: TAVPacketSideDataType;
{$IFDEF FF_API_BUFFER_SIZE_T}
                                 size: Integer): PByte; cdecl; external AVCODEC_LIBNAME name _PU + 'av_packet_new_side_data';
{$ELSE}
                                 size: Size_t): PByte; cdecl;
{$ENDIF}

(**
 * Wrap an existing array as a packet side data.
 *
 * @param pkt packet
 * @param type side information type
 * @param data the side data array. It must be allocated with the av_malloc()
 *             family of functions. The ownership of the data is transferred to
 *             pkt.
 * @param size side information size
 * @return a non-negative number on success, a negative AVERROR code on
 *         failure. On failure, the packet is unchanged and the data remains
 *         owned by the caller.
 *)
function av_packet_add_side_data(pkt: PAVPacket; type_: TAVPacketSideDataType;
                            data: PByte; size: Size_t): Integer; cdecl; external AVCODEC_LIBNAME name _PU + 'av_packet_add_side_data';

(**
 * Shrink the already allocated side data buffer
 *
 * @param pkt packet
 * @param type side information type
 * @param size new side information size
 * @return 0 on success, < 0 on failure
 *)
function av_packet_shrink_side_data(pkt: PAVPacket; ttype: TAVPacketSideDataType;
{$IFDEF FF_API_BUFFER_SIZE_T}
                               size: Integer): Integer; cdecl; external AVCODEC_LIBNAME name _PU + 'av_packet_shrink_side_data';
{$ELSE}
                               size: Size_t): Integer; cdecl;
{$ENDIF}

(**
 * Get side information from packet.
 *
 * @param pkt packet
 * @param type desired side information type
 * @param size If supplied, *size will be set to the size of the side data
 *             or to zero if the desired side data is not present.
 * @return pointer to data if present or NULL otherwise
 *)
function av_packet_get_side_data(const pkt: PAVPacket; type_: TAVPacketSideDataType;
{$IFDEF FF_API_BUFFER_SIZE_T}
                                 size: PInteger): PByte; cdecl; external AVCODEC_LIBNAME name _PU + 'av_packet_get_side_data';
{$ELSE}
                                 size: PSize_t): PByte; cdecl;
{$ENDIF}

{$IFDEF FF_API_MERGE_SD_API}
function av_packet_merge_side_data(pkt: PAVPacket): Integer; cdecl; external AVCODEC_LIBNAME name _PU + 'av_packet_merge_side_data';

function av_packet_split_side_data(pkt: PAVPacket): Integer; cdecl; external AVCODEC_LIBNAME name _PU + 'av_packet_split_side_data';
{$ENDIF}

function av_packet_side_data_name(type_: TAVPacketSideDataType): PAnsiChar; cdecl; external AVCODEC_LIBNAME name _PU + 'av_packet_side_data_name';

(**
 * Pack a dictionary for use in side_data.
 *
 * @param dict The dictionary to pack.
 * @param size pointer to store the size of the returned data
 * @return pointer to data if successful, NULL otherwise
 *)
{$IFDEF FF_API_BUFFER_SIZE_T}
function av_packet_pack_dictionary(dict: PAVDictionary; size: PInteger): PByte; cdecl; external AVCODEC_LIBNAME name _PU + 'av_packet_pack_dictionary';
{$ELSE}
function av_packet_pack_dictionary(dict: PAVDictionary; size: PSize_t): PByte; cdecl; external AVCODEC_LIBNAME name _PU + 'av_packet_pack_dictionary';
{$ENDIF}

(**
 * Unpack a dictionary from side_data.
 *
 * @param data data from side_data
 * @param size size of the data
 * @param dict the metadata storage dictionary
 * @return 0 on success, < 0 on failure
 *)
{$IFDEF FF_API_BUFFER_SIZE_T}
function av_packet_unpack_dictionary(const data: PByte; size: Integer; dict: PPAVDictionary): Integer; cdecl; external AVCODEC_LIBNAME name _PU + 'av_packet_unpack_dictionary';
{$ELSE}
function av_packet_unpack_dictionary(const data: PByte; size: Size_t; dict: PPAVDictionary): Integer; cdecl; external AVCODEC_LIBNAME name _PU + 'av_packet_unpack_dictionary';
{$ENDIF}


(**
 * Convenience function to free all the side data stored.
 * All the other fields stay untouched.
 *
 * @param pkt packet
 *)
procedure av_packet_free_side_data(pkt: PAVPacket); cdecl; external AVCODEC_LIBNAME name _PU + 'av_packet_free_side_data';

(**
 * Setup a new reference to the data described by a given packet
 *
 * If src is reference-counted, setup dst as a new reference to the
 * buffer in src. Otherwise allocate a new buffer in dst and copy the
 * data from src into it.
 *
 * All the other fields are copied from src.
 *
 * @see av_packet_unref
 *
 * @param dst Destination packet. Will be completely overwritten.
 * @param src Source packet
 *
 * @return 0 on success, a negative AVERROR on error. On error, dst
 *         will be blank (as if returned by av_packet_alloc()).
 *)
function av_packet_ref(dst: PAVPacket; const src: PAVPacket): Integer; cdecl; external AVCODEC_LIBNAME name _PU + 'av_packet_ref';

(**
 * Wipe the packet.
 *
 * Unreference the buffer referenced by the packet and reset the
 * remaining packet fields to their default values.
 *
 * @param pkt The packet to be unreferenced.
 *)
procedure av_packet_unref(pkt: PAVPacket); cdecl; external AVCODEC_LIBNAME name _PU + 'av_packet_unref';

(**
 * Move every field in src to dst and reset src.
 *
 * @see av_packet_unref
 *
 * @param src Source packet, will be reset
 * @param dst Destination packet
 *)
procedure av_packet_move_ref(dst, src: PAVPacket); cdecl; external AVCODEC_LIBNAME name _PU + 'av_packet_move_ref';

(**
 * Copy only "properties" fields from src to dst.
 *
 * Properties for the purpose of this function are all the fields
 * beside those related to the packet data (buf, data, size)
 *
 * @param dst Destination packet
 * @param src Source packet
 *
 * @return 0 on success AVERROR on failure.
 *)
function av_packet_copy_props(dst: PAVPacket; const src: PAVPacket): Integer; cdecl; external AVCODEC_LIBNAME name _PU + 'av_packet_copy_props';

(**
 * Ensure the data described by a given packet is reference counted.
 *
 * @note This function does not ensure that the reference will be writable.
 *       Use av_packet_make_writable instead for that purpose.
 *
 * @see av_packet_ref
 * @see av_packet_make_writable
 *
 * @param pkt packet whose data should be made reference counted.
 *
 * @return 0 on success, a negative AVERROR on error. On failure, the
 *         packet is unchanged.
 *)
function av_packet_make_refcounted(pkt: PAVPacket): Integer; cdecl; external AVCODEC_LIBNAME name _PU + 'av_packet_make_refcounted';

(**
 * Create a writable reference for the data described by a given packet,
 * avoiding data copy if possible.
 *
 * @param pkt Packet whose data should be made writable.
 *
 * @return 0 on success, a negative AVERROR on failure. On failure, the
 *         packet is unchanged.
 *)
function av_packet_make_writable(pkt: PAVPacket): Integer; cdecl; external AVCODEC_LIBNAME name _PU + 'av_packet_make_writable';

(**
 * Convert valid timing fields (timestamps / durations) in a packet from one
 * timebase to another. Timestamps with unknown values (AV_NOPTS_VALUE) will be
 * ignored.
 *
 * @param pkt packet on which the conversion will be performed
 * @param tb_src source timebase, in which the timing fields in pkt are
 *               expressed
 * @param tb_dst destination timebase, to which the timing fields will be
 *               converted
 *)
procedure av_packet_rescale_ts(pkt: PAVPacket; tb_src, tb_dst: TAVRational); cdecl; external AVCODEC_LIBNAME name _PU + 'av_packet_rescale_ts';

(**
 * @}
 *)

implementation

end.
