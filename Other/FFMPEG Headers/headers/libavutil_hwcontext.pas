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

(*
 * FFVCL - Delphi FFmpeg VCL Components
 * http://www.DelphiFFmpeg.com
 *
 * Original file: libavutil/hwcontext.h
 * Ported by CodeCoolie@CNSW 2017/04/23 -> $Date:: 2020-11-15 #$
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

unit libavutil_hwcontext;

interface

{$I CompilerDefines.inc}

uses
  libavutil_buffer,
  libavutil_dict,
  libavutil_frame,
  libavutil_log,
  libavutil_pixfmt;

{$I libversion.inc}

type
  TAVHWDeviceType = (
    AV_HWDEVICE_TYPE_NONE,
    AV_HWDEVICE_TYPE_VDPAU,
    AV_HWDEVICE_TYPE_CUDA,
    AV_HWDEVICE_TYPE_VAAPI,
    AV_HWDEVICE_TYPE_DXVA2,
    AV_HWDEVICE_TYPE_QSV,
    AV_HWDEVICE_TYPE_VIDEOTOOLBOX,
    AV_HWDEVICE_TYPE_D3D11VA,
    AV_HWDEVICE_TYPE_DRM,
    AV_HWDEVICE_TYPE_OPENCL,
    AV_HWDEVICE_TYPE_MEDIACODEC,
    AV_HWDEVICE_TYPE_VULKAN
  );

  PAVHWDeviceInternal = ^TAVHWDeviceInternal;
  TAVHWDeviceInternal = record
    // need {$ALIGN 8}
    // defined in libutil/hwcontext_internal.h
  end;

(**
 * This struct aggregates all the (hardware/vendor-specific) "high-level" state,
 * i.e. state that is not tied to a concrete processing configuration.
 * E.g., in an API that supports hardware-accelerated encoding and decoding,
 * this struct will (if possible) wrap the state that is common to both encoding
 * and decoding and from which specific instances of encoders or decoders can be
 * derived.
 *
 * This struct is reference-counted with the AVBuffer mechanism. The
 * av_hwdevice_ctx_alloc() constructor yields a reference, whose data field
 * points to the actual AVHWDeviceContext. Further objects derived from
 * AVHWDeviceContext (such as AVHWFramesContext, describing a frame pool with
 * specific properties) will hold an internal reference to it. After all the
 * references are released, the AVHWDeviceContext itself will be freed,
 * optionally invoking a user-specified callback for uninitializing the hardware
 * state.
 *)
  PAVHWDeviceContext = ^TAVHWDeviceContext;
  TAVHWDeviceContext = record
    (**
     * A class for logging. Set by av_hwdevice_ctx_alloc().
     *)
    av_class: PAVClass;

    (**
     * Private data used internally by libavutil. Must not be accessed in any
     * way by the caller.
     *)
    internal: PAVHWDeviceInternal;

    (**
     * This field identifies the underlying API used for hardware access.
     *
     * This field is set when this struct is allocated and never changed
     * afterwards.
     *)
    type_: TAVHWDeviceType;

    (**
     * The format-specific data, allocated and freed by libavutil along with
     * this context.
     *
     * Should be cast by the user to the format-specific context defined in the
     * corresponding header (hwcontext_*.h) and filled as described in the
     * documentation before calling av_hwdevice_ctx_init().
     *
     * After calling av_hwdevice_ctx_init() this struct should not be modified
     * by the caller.
     *)
    hwctx: Pointer;

    (**
     * This field may be set by the caller before calling av_hwdevice_ctx_init().
     *
     * If non-NULL, this callback will be called when the last reference to
     * this context is unreferenced, immediately before it is freed.
     *
     * @note when other objects (e.g an AVHWFramesContext) are derived from this
     *       struct, this callback will be invoked after all such child objects
     *       are fully uninitialized and their respective destructors invoked.
     *)
    free: procedure(ctx: PAVHWDeviceContext); cdecl;

    (**
     * Arbitrary user data, to be used e.g. by the free() callback.
     *)
    user_opaque: Pointer;
  end;

  PAVHWFramesInternal = ^TAVHWFramesInternal;
  TAVHWFramesInternal = record
    // need {$ALIGN 8}
    // defined in libutil/hwcontext_internal.h
  end;

(**
 * This struct describes a set or pool of "hardware" frames (i.e. those with
 * data not located in normal system memory). All the frames in the pool are
 * assumed to be allocated in the same way and interchangeable.
 *
 * This struct is reference-counted with the AVBuffer mechanism and tied to a
 * given AVHWDeviceContext instance. The av_hwframe_ctx_alloc() constructor
 * yields a reference, whose data field points to the actual AVHWFramesContext
 * struct.
 *)
  PAVHWFramesContext = ^TAVHWFramesContext;
  TAVHWFramesContext = record
    (**
     * A class for logging.
     *)
    av_class: PAVClass;

    (**
     * Private data used internally by libavutil. Must not be accessed in any
     * way by the caller.
     *)
    internal: PAVHWFramesInternal;

    (**
     * A reference to the parent AVHWDeviceContext. This reference is owned and
     * managed by the enclosing AVHWFramesContext, but the caller may derive
     * additional references from it.
     *)
    device_ref: PAVBufferRef;

    (**
     * The parent AVHWDeviceContext. This is simply a pointer to
     * device_ref->data provided for convenience.
     *
     * Set by libavutil in av_hwframe_ctx_init().
     *)
    device_ctx: PAVHWDeviceContext;

    (**
     * The format-specific data, allocated and freed automatically along with
     * this context.
     *
     * Should be cast by the user to the format-specific context defined in the
     * corresponding header (hwframe_*.h) and filled as described in the
     * documentation before calling av_hwframe_ctx_init().
     *
     * After any frames using this context are created, the contents of this
     * struct should not be modified by the caller.
     *)
    hwctx: Pointer;

    (**
     * This field may be set by the caller before calling av_hwframe_ctx_init().
     *
     * If non-NULL, this callback will be called when the last reference to
     * this context is unreferenced, immediately before it is freed.
     *)
    free: procedure(ctx: PAVHWFramesContext); cdecl;

    (**
     * Arbitrary user data, to be used e.g. by the free() callback.
     *)
    user_opaque: Pointer;

    (**
     * A pool from which the frames are allocated by av_hwframe_get_buffer().
     * This field may be set by the caller before calling av_hwframe_ctx_init().
     * The buffers returned by calling av_buffer_pool_get() on this pool must
     * have the properties described in the documentation in the corresponding hw
     * type's header (hwcontext_*.h). The pool will be freed strictly before
     * this struct's free() callback is invoked.
     *
     * This field may be NULL, then libavutil will attempt to allocate a pool
     * internally. Note that certain device types enforce pools allocated at
     * fixed size (frame count), which cannot be extended dynamically. In such a
     * case, initial_pool_size must be set appropriately.
     *)
    pool: PAVBufferPool;

    (**
     * Initial size of the frame pool. If a device type does not support
     * dynamically resizing the pool, then this is also the maximum pool size.
     *
     * May be set by the caller before calling av_hwframe_ctx_init(). Must be
     * set if pool is NULL and the device type does not support dynamic pools.
     *)
    initial_pool_size: Integer;

    (**
     * The pixel format identifying the underlying HW surface type.
     *
     * Must be a hwaccel format, i.e. the corresponding descriptor must have the
     * AV_PIX_FMT_FLAG_HWACCEL flag set.
     *
     * Must be set by the user before calling av_hwframe_ctx_init().
     *)
    format: TAVPixelFormat;

    (**
     * The pixel format identifying the actual data layout of the hardware
     * frames.
     *
     * Must be set by the caller before calling av_hwframe_ctx_init().
     *
     * @note when the underlying API does not provide the exact data layout, but
     * only the colorspace/bit depth, this field should be set to the fully
     * planar version of that format (e.g. for 8-bit 420 YUV it should be
     * AV_PIX_FMT_YUV420P, not AV_PIX_FMT_NV12 or anything else).
     *)
    sw_format: TAVPixelFormat;

    (**
     * The allocated dimensions of the frames in this pool.
     *
     * Must be set by the user before calling av_hwframe_ctx_init().
     *)
    width, height: Integer;
  end;

(**
 * Look up an AVHWDeviceType by name.
 *
 * @param name String name of the device type (case-insensitive).
 * @return The type from enum AVHWDeviceType, or AV_HWDEVICE_TYPE_NONE if
 *         not found.
 *)
function av_hwdevice_find_type_by_name(const name: PAnsiChar): TAVHWDeviceType; cdecl; external AVUTIL_LIBNAME name _PU + 'av_hwdevice_find_type_by_name';

(** Get the string name of an AVHWDeviceType.
 *
 * @param type Type from enum AVHWDeviceType.
 * @return Pointer to a static string containing the name, or NULL if the type
 *         is not valid.
 *)
function av_hwdevice_get_type_name(type_: TAVHWDeviceType): PAnsiChar; cdecl; external AVUTIL_LIBNAME name _PU + 'av_hwdevice_get_type_name';

(**
 * Iterate over supported device types.
 *
 * @param type AV_HWDEVICE_TYPE_NONE initially, then the previous type
 *             returned by this function in subsequent iterations.
 * @return The next usable device type from enum AVHWDeviceType, or
 *         AV_HWDEVICE_TYPE_NONE if there are no more.
 *)
function av_hwdevice_iterate_types(prev: TAVHWDeviceType): TAVHWDeviceType; cdecl; external AVUTIL_LIBNAME name _PU + 'av_hwdevice_iterate_types';

(**
 * Allocate an AVHWDeviceContext for a given hardware type.
 *
 * @param type the type of the hardware device to allocate.
 * @return a reference to the newly created AVHWDeviceContext on success or NULL
 *         on failure.
 *)
function av_hwdevice_ctx_alloc(type_: TAVHWDeviceType): PAVBufferRef; cdecl; external AVUTIL_LIBNAME name _PU + 'av_hwdevice_ctx_alloc';

(**
 * Finalize the device context before use. This function must be called after
 * the context is filled with all the required information and before it is
 * used in any way.
 *
 * @param ref a reference to the AVHWDeviceContext
 * @return 0 on success, a negative AVERROR code on failure
 *)
function av_hwdevice_ctx_init(ref: PAVBufferRef): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_hwdevice_ctx_init';

(**
 * Open a device of the specified type and create an AVHWDeviceContext for it.
 *
 * This is a convenience function intended to cover the simple cases. Callers
 * who need to fine-tune device creation/management should open the device
 * manually and then wrap it in an AVHWDeviceContext using
 * av_hwdevice_ctx_alloc()/av_hwdevice_ctx_init().
 *
 * The returned context is already initialized and ready for use, the caller
 * should not call av_hwdevice_ctx_init() on it. The user_opaque/free fields of
 * the created AVHWDeviceContext are set by this function and should not be
 * touched by the caller.
 *
 * @param device_ctx On success, a reference to the newly-created device context
 *                   will be written here. The reference is owned by the caller
 *                   and must be released with av_buffer_unref() when no longer
 *                   needed. On failure, NULL will be written to this pointer.
 * @param type The type of the device to create.
 * @param device A type-specific string identifying the device to open.
 * @param opts A dictionary of additional (type-specific) options to use in
 *             opening the device. The dictionary remains owned by the caller.
 * @param flags currently unused
 *
 * @return 0 on success, a negative AVERROR code on failure.
 *)
function av_hwdevice_ctx_create(device_ctx: PPAVBufferRef; type_: TAVHWDeviceType;
                           const device: PAnsiChar; opts: PAVDictionary; flags: Integer): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_hwdevice_ctx_create';

(**
 * Create a new device of the specified type from an existing device.
 *
 * If the source device is a device of the target type or was originally
 * derived from such a device (possibly through one or more intermediate
 * devices of other types), then this will return a reference to the
 * existing device of the same type as is requested.
 *
 * Otherwise, it will attempt to derive a new device from the given source
 * device.  If direct derivation to the new type is not implemented, it will
 * attempt the same derivation from each ancestor of the source device in
 * turn looking for an implemented derivation method.
 *
 * @param dst_ctx On success, a reference to the newly-created
 *                AVHWDeviceContext.
 * @param type    The type of the new device to create.
 * @param src_ctx A reference to an existing AVHWDeviceContext which will be
 *                used to create the new device.
 * @param flags   Currently unused; should be set to zero.
 * @return        Zero on success, a negative AVERROR code on failure.
 *)
function av_hwdevice_ctx_create_derived(dst_ctx: PPAVBufferRef;
                                   type_: TAVHWDeviceType;
                                   src_ctx: PAVBufferRef; flags: Integer): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_hwdevice_ctx_create_derived';

(**
 * Create a new device of the specified type from an existing device.
 *
 * This function performs the same action as av_hwdevice_ctx_create_derived,
 * however, it is able to set options for the new device to be derived.
 *
 * @param dst_ctx On success, a reference to the newly-created
 *                AVHWDeviceContext.
 * @param type    The type of the new device to create.
 * @param src_ctx A reference to an existing AVHWDeviceContext which will be
 *                used to create the new device.
 * @param options Options for the new device to create, same format as in
 *                av_hwdevice_ctx_create.
 * @param flags   Currently unused; should be set to zero.
 * @return        Zero on success, a negative AVERROR code on failure.
 *)
function av_hwdevice_ctx_create_derived_opts(dst_ctx: PPAVBufferRef;
                                        type_: TAVHWDeviceType;
                                        src_ctx: PAVBufferRef;
                                        options: PAVDictionary; flags: Integer): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_hwdevice_ctx_create_derived_opts';

(**
 * Allocate an AVHWFramesContext tied to a given device context.
 *
 * @param device_ctx a reference to a AVHWDeviceContext. This function will make
 *                   a new reference for internal use, the one passed to the
 *                   function remains owned by the caller.
 * @return a reference to the newly created AVHWFramesContext on success or NULL
 *         on failure.
 *)
function av_hwframe_ctx_alloc(device_ctx: PAVBufferRef): PAVBufferRef; cdecl; external AVUTIL_LIBNAME name _PU + 'av_hwframe_ctx_alloc';

(**
 * Finalize the context before use. This function must be called after the
 * context is filled with all the required information and before it is attached
 * to any frames.
 *
 * @param ref a reference to the AVHWFramesContext
 * @return 0 on success, a negative AVERROR code on failure
 *)
function av_hwframe_ctx_init(ref: PAVBufferRef): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_hwframe_ctx_init';

(**
 * Allocate a new frame attached to the given AVHWFramesContext.
 *
 * @param hwframe_ctx a reference to an AVHWFramesContext
 * @param frame an empty (freshly allocated or unreffed) frame to be filled with
 *              newly allocated buffers.
 * @param flags currently unused, should be set to zero
 * @return 0 on success, a negative AVERROR code on failure
 *)
function av_hwframe_get_buffer(hwframe_ctx: PAVBufferRef; frame: PAVFrame; flags: Integer): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_hwframe_get_buffer';

(**
 * Copy data to or from a hw surface. At least one of dst/src must have an
 * AVHWFramesContext attached.
 *
 * If src has an AVHWFramesContext attached, then the format of dst (if set)
 * must use one of the formats returned by av_hwframe_transfer_get_formats(src,
 * AV_HWFRAME_TRANSFER_DIRECTION_FROM).
 * If dst has an AVHWFramesContext attached, then the format of src must use one
 * of the formats returned by av_hwframe_transfer_get_formats(dst,
 * AV_HWFRAME_TRANSFER_DIRECTION_TO)
 *
 * dst may be "clean" (i.e. with data/buf pointers unset), in which case the
 * data buffers will be allocated by this function using av_frame_get_buffer().
 * If dst->format is set, then this format will be used, otherwise (when
 * dst->format is AV_PIX_FMT_NONE) the first acceptable format will be chosen.
 *
 * The two frames must have matching allocated dimensions (i.e. equal to
 * AVHWFramesContext.width/height), since not all device types support
 * transferring a sub-rectangle of the whole surface. The display dimensions
 * (i.e. AVFrame.width/height) may be smaller than the allocated dimensions, but
 * also have to be equal for both frames. When the display dimensions are
 * smaller than the allocated dimensions, the content of the padding in the
 * destination frame is unspecified.
 *
 * @param dst the destination frame. dst is not touched on failure.
 * @param src the source frame.
 * @param flags currently unused, should be set to zero
 * @return 0 on success, a negative AVERROR error code on failure.
 *)
function av_hwframe_transfer_data(dst: PAVFrame; const src: PAVFrame; flags: Integer): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_hwframe_transfer_data';

type
  TAVHWFrameTransferDirection = (
    (**
     * Transfer the data from the queried hw frame.
     *)
    AV_HWFRAME_TRANSFER_DIRECTION_FROM,

    (**
     * Transfer the data to the queried hw frame.
     *)
    AV_HWFRAME_TRANSFER_DIRECTION_TO
  );

(**
 * Get a list of possible source or target formats usable in
 * av_hwframe_transfer_data().
 *
 * @param hwframe_ctx the frame context to obtain the information for
 * @param dir the direction of the transfer
 * @param formats the pointer to the output format list will be written here.
 *                The list is terminated with AV_PIX_FMT_NONE and must be freed
 *                by the caller when no longer needed using av_free().
 *                If this function returns successfully, the format list will
 *                have at least one item (not counting the terminator).
 *                On failure, the contents of this pointer are unspecified.
 * @param flags currently unused, should be set to zero
 * @return 0 on success, a negative AVERROR code on failure.
 *)
function av_hwframe_transfer_get_formats(hwframe_ctx: PAVBufferRef;
                                    dir: TAVHWFrameTransferDirection;
                                    formats: PPAVPixelFormat; flags: Integer): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_hwframe_transfer_get_formats';


type
(**
 * This struct describes the constraints on hardware frames attached to
 * a given device with a hardware-specific configuration.  This is returned
 * by av_hwdevice_get_hwframe_constraints() and must be freed by
 * av_hwframe_constraints_free() after use.
 *)
  PPAVHWFramesConstraints = ^PAVHWFramesConstraints;
  PAVHWFramesConstraints = ^TAVHWFramesConstraints;
  TAVHWFramesConstraints = record
    (**
     * A list of possible values for format in the hw_frames_ctx,
     * terminated by AV_PIX_FMT_NONE.  This member will always be filled.
     *)
    valid_hw_formats: PAVPixelFormat;

    (**
     * A list of possible values for sw_format in the hw_frames_ctx,
     * terminated by AV_PIX_FMT_NONE.  Can be NULL if this information is
     * not known.
     *)
    valid_sw_formats: PAVPixelFormat;

    (**
     * The minimum size of frames in this hw_frames_ctx.
     * (Zero if not known.)
     *)
    min_width: Integer;
    min_height: Integer;

    (**
     * The maximum size of frames in this hw_frames_ctx.
     * (INT_MAX if not known / no limit.)
     *)
    max_width: Integer;
    max_height: Integer;
  end;

(**
 * Allocate a HW-specific configuration structure for a given HW device.
 * After use, the user must free all members as required by the specific
 * hardware structure being used, then free the structure itself with
 * av_free().
 *
 * @param device_ctx a reference to the associated AVHWDeviceContext.
 * @return The newly created HW-specific configuration structure on
 *         success or NULL on failure.
 *)
function av_hwdevice_hwconfig_alloc(device_ctx: PAVBufferRef): Pointer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_hwdevice_hwconfig_alloc';

(**
 * Get the constraints on HW frames given a device and the HW-specific
 * configuration to be used with that device.  If no HW-specific
 * configuration is provided, returns the maximum possible capabilities
 * of the device.
 *
 * @param ref a reference to the associated AVHWDeviceContext.
 * @param hwconfig a filled HW-specific configuration structure, or NULL
 *        to return the maximum possible capabilities of the device.
 * @return AVHWFramesConstraints structure describing the constraints
 *         on the device, or NULL if not available.
 *)
function av_hwdevice_get_hwframe_constraints(ref: PAVBufferRef;
                                                           const hwconfig: Pointer): PAVHWFramesConstraints; cdecl; external AVUTIL_LIBNAME name _PU + 'av_hwdevice_get_hwframe_constraints';

(**
 * Free an AVHWFrameConstraints structure.
 *
 * @param constraints The (filled or unfilled) AVHWFrameConstraints structure.
 *)
procedure av_hwframe_constraints_free(constraints: PPAVHWFramesConstraints); cdecl; external AVUTIL_LIBNAME name _PU + 'av_hwframe_constraints_free';


type
(**
 * Flags to apply to frame mappings.
 *)
  TAVHWFrameMap = (
    (**
     * The mapping must be readable.
     *)
    AV_HWFRAME_MAP_READ      = (1 shl 0),
    (**
     * The mapping must be writeable.
     *)
    AV_HWFRAME_MAP_WRITE     = (1 shl 1),
    (**
     * The mapped frame will be overwritten completely in subsequent
     * operations, so the current frame data need not be loaded.  Any values
     * which are not overwritten are unspecified.
     *)
    AV_HWFRAME_MAP_OVERWRITE = (1 shl 2),
    (**
     * The mapping must be direct.  That is, there must not be any copying in
     * the map or unmap steps.  Note that performance of direct mappings may
     * be much lower than normal memory.
     *)
    AV_HWFRAME_MAP_DIRECT    = (1 shl 3)
  );

(**
 * Map a hardware frame.
 *
 * This has a number of different possible effects, depending on the format
 * and origin of the src and dst frames.  On input, src should be a usable
 * frame with valid buffers and dst should be blank (typically as just created
 * by av_frame_alloc()).  src should have an associated hwframe context, and
 * dst may optionally have a format and associated hwframe context.
 *
 * If src was created by mapping a frame from the hwframe context of dst,
 * then this function undoes the mapping - dst is replaced by a reference to
 * the frame that src was originally mapped from.
 *
 * If both src and dst have an associated hwframe context, then this function
 * attempts to map the src frame from its hardware context to that of dst and
 * then fill dst with appropriate data to be usable there.  This will only be
 * possible if the hwframe contexts and associated devices are compatible -
 * given compatible devices, av_hwframe_ctx_create_derived() can be used to
 * create a hwframe context for dst in which mapping should be possible.
 *
 * If src has a hwframe context but dst does not, then the src frame is
 * mapped to normal memory and should thereafter be usable as a normal frame.
 * If the format is set on dst, then the mapping will attempt to create dst
 * with that format and fail if it is not possible.  If format is unset (is
 * AV_PIX_FMT_NONE) then dst will be mapped with whatever the most appropriate
 * format to use is (probably the sw_format of the src hwframe context).
 *
 * A return value of AVERROR(ENOSYS) indicates that the mapping is not
 * possible with the given arguments and hwframe setup, while other return
 * values indicate that it failed somehow.
 *
 * @param dst Destination frame, to contain the mapping.
 * @param src Source frame, to be mapped.
 * @param flags Some combination of AV_HWFRAME_MAP_* flags.
 * @return Zero on success, negative AVERROR code on failure.
 *)
function av_hwframe_map(dst: PAVFrame; const src: PAVFrame; flags: Integer): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_hwframe_map';


(**
 * Create and initialise an AVHWFramesContext as a mapping of another existing
 * AVHWFramesContext on a different device.
 *
 * av_hwframe_ctx_init() should not be called after this.
 *
 * @param derived_frame_ctx  On success, a reference to the newly created
 *                           AVHWFramesContext.
 * @param derived_device_ctx A reference to the device to create the new
 *                           AVHWFramesContext on.
 * @param source_frame_ctx   A reference to an existing AVHWFramesContext
 *                           which will be mapped to the derived context.
 * @param flags  Some combination of AV_HWFRAME_MAP_* flags, defining the
 *               mapping parameters to apply to frames which are allocated
 *               in the derived device.
 * @return       Zero on success, negative AVERROR code on failure.
 *)
function av_hwframe_ctx_create_derived(derived_frame_ctx: PPAVBufferRef;
                                  format: TAVPixelFormat;
                                  derived_device_ctx: PAVBufferRef;
                                  source_frame_ctx: PAVBufferRef;
                                  flags: Integer): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_hwframe_ctx_create_derived';

implementation

end.
