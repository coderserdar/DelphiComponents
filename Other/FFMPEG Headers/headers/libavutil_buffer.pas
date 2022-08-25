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

(**
 * @file
 * @ingroup lavu_buffer
 * refcounted data buffer API
 *)

(*
 * FFVCL - Delphi FFmpeg VCL Components
 * http://www.DelphiFFmpeg.com
 *
 * Original file: libavutil/buffer.h
 * Ported by CodeCoolie@CNSW 2013/10/22 -> $Date:: 2021-04-25 #$
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

unit libavutil_buffer;

interface

{$I CompilerDefines.inc}

{$I libversion.inc}

(**
 * @defgroup lavu_buffer AVBuffer
 * @ingroup lavu_data
 *
 * @{
 * AVBuffer is an API for reference-counted data buffers.
 *
 * There are two core objects in this API -- AVBuffer and AVBufferRef. AVBuffer
 * represents the data buffer itself; it is opaque and not meant to be accessed
 * by the caller directly, but only through AVBufferRef. However, the caller may
 * e.g. compare two AVBuffer pointers to check whether two different references
 * are describing the same data buffer. AVBufferRef represents a single
 * reference to an AVBuffer and it is the object that may be manipulated by the
 * caller directly.
 *
 * There are two functions provided for creating a new AVBuffer with a single
 * reference -- av_buffer_alloc() to just allocate a new buffer, and
 * av_buffer_create() to wrap an existing array in an AVBuffer. From an existing
 * reference, additional references may be created with av_buffer_ref().
 * Use av_buffer_unref() to free a reference (this will automatically free the
 * data once all the references are freed).
 *
 * The convention throughout this API and the rest of FFmpeg is such that the
 * buffer is considered writable if there exists only one reference to it (and
 * it has not been marked as read-only). The av_buffer_is_writable() function is
 * provided to check whether this is true and av_buffer_make_writable() will
 * automatically create a new writable buffer when necessary.
 * Of course nothing prevents the calling code from violating this convention,
 * however that is safe only when all the existing references are under its
 * control.
 *
 * @note Referencing and unreferencing the buffers is thread-safe and thus
 * may be done from multiple threads simultaneously without any need for
 * additional locking.
 *
 * @note Two different references to the same buffer can point to different
 * parts of the buffer (i.e. their AVBufferRef.data will not be equal).
 *)

(**
 * A reference counted buffer type. It is opaque and is meant to be used through
 * references (AVBufferRef).
 *)
type
  PAVBuffer = ^TAVBuffer;
  TAVBuffer = record
    // need {$ALIGN 8}
    // defined libavutil/buffer_internal.h
  end;

(**
 * A reference to a data buffer.
 *
 * The size of this struct is not a part of the public ABI and it is not meant
 * to be allocated directly.
 *)
  PPAVBufferRef = ^PAVBufferRef;
  PAVBufferRef = ^TAVBufferRef;
  TAVBufferRef = record
    buffer: PAVBuffer;

    (**
     * The data buffer. It is considered writable if and only if
     * this is the only reference to the buffer, in which case
     * av_buffer_is_writable() returns 1.
     *)
    data: PByte;
    (**
     * Size of data in bytes.
     *)
{$IFDEF FF_API_BUFFER_SIZE_T}
    size: Integer;
{$ELSE}
    size: Size_t;
{$ENDIF}
  end;

(**
 * Allocate an AVBuffer of the given size using av_malloc().
 *
 * @return an AVBufferRef of given size or NULL when out of memory
 *)
{$IFDEF FF_API_BUFFER_SIZE_T}
function av_buffer_alloc(size: Integer): PAVBufferRef; cdecl; external AVUTIL_LIBNAME name _PU + 'av_buffer_alloc';
{$ELSE}
function av_buffer_alloc(size: Size_t): PAVBufferRef; cdecl; external AVUTIL_LIBNAME name _PU + 'av_buffer_alloc';
{$ENDIF}

(**
 * Same as av_buffer_alloc(), except the returned buffer will be initialized
 * to zero.
 *)
{$IFDEF FF_API_BUFFER_SIZE_T}
function av_buffer_allocz(size: Integer): PAVBufferRef; cdecl; external AVUTIL_LIBNAME name _PU + 'av_buffer_allocz';
{$ELSE}
function av_buffer_allocz(size: Size_t): PAVBufferRef; cdecl; external AVUTIL_LIBNAME name _PU + 'av_buffer_allocz';
{$ENDIF}

(**
 * Always treat the buffer as read-only, even when it has only one
 * reference.
 *)
const
  AV_BUFFER_FLAG_READONLY = (1 shl 0);

(**
 * Create an AVBuffer from an existing array.
 *
 * If this function is successful, data is owned by the AVBuffer. The caller may
 * only access data through the returned AVBufferRef and references derived from
 * it.
 * If this function fails, data is left untouched.
 * @param data   data array
 * @param size   size of data in bytes
 * @param free   a callback for freeing this buffer's data
 * @param opaque parameter to be got for processing or passed to free
 * @param flags  a combination of AV_BUFFER_FLAG_*
 *
 * @return an AVBufferRef referring to data on success, NULL on failure.
 *)
type
  TfreeCall = procedure(opaque: Pointer; data: PByte); cdecl;
{$IFDEF FF_API_BUFFER_SIZE_T}
function av_buffer_create(data: PByte; size: Integer; free: TfreeCall;
{$ELSE}
  Tav_buffer_createProc = function(data: PByte; size: Size_t; free: TfreeCall;
{$ENDIF}
                              opaque: Pointer; flags: Integer): PAVBufferRef; cdecl; external AVUTIL_LIBNAME name _PU + 'av_buffer_create';

(**
 * Default free callback, which calls av_free() on the buffer data.
 * This function is meant to be passed to av_buffer_create(), not called
 * directly.
 *)
procedure av_buffer_default_free(opaque: Pointer; data: PByte); cdecl; external AVUTIL_LIBNAME name _PU + 'av_buffer_default_free';

(**
 * Create a new reference to an AVBuffer.
 *
 * @return a new AVBufferRef referring to the same AVBuffer as buf or NULL on
 * failure.
 *)
function av_buffer_ref(buf: PAVBufferRef): PAVBufferRef; cdecl; external AVUTIL_LIBNAME name _PU + 'av_buffer_ref';

(**
 * Free a given reference and automatically free the buffer if there are no more
 * references to it.
 *
 * @param buf the reference to be freed. The pointer is set to NULL on return.
 *)
procedure av_buffer_unref(buf: PPAVBufferRef); cdecl; external AVUTIL_LIBNAME name _PU + 'av_buffer_unref';

(**
 * @return 1 if the caller may write to the data referred to by buf (which is
 * true if and only if buf is the only reference to the underlying AVBuffer).
 * Return 0 otherwise.
 * A positive answer is valid until av_buffer_ref() is called on buf.
 *)
function av_buffer_is_writable(const buf: PAVBufferRef): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_buffer_is_writable';

(**
 * @return the opaque parameter set by av_buffer_create.
 *)
function av_buffer_get_opaque(const buf: PAVBufferRef): Pointer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_buffer_get_opaque';

function av_buffer_get_ref_count(const buf: PAVBufferRef): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_buffer_get_ref_count';

(**
 * Create a writable reference from a given buffer reference, avoiding data copy
 * if possible.
 *
 * @param buf buffer reference to make writable. On success, buf is either left
 *            untouched, or it is unreferenced and a new writable AVBufferRef is
 *            written in its place. On failure, buf is left untouched.
 * @return 0 on success, a negative AVERROR on failure.
 *)
function av_buffer_make_writable(buf: PPAVBufferRef): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_buffer_make_writable';

(**
 * Reallocate a given buffer.
 *
 * @param buf  a buffer reference to reallocate. On success, buf will be
 *             unreferenced and a new reference with the required size will be
 *             written in its place. On failure buf will be left untouched. *buf
 *             may be NULL, then a new buffer is allocated.
 * @param size required new buffer size.
 * @return 0 on success, a negative AVERROR on failure.
 *
 * @note the buffer is actually reallocated with av_realloc() only if it was
 * initially allocated through av_buffer_realloc(NULL) and there is only one
 * reference to it (i.e. the one passed to this function). In all other cases
 * a new buffer is allocated and the data is copied.
 *)
{$IFDEF FF_API_BUFFER_SIZE_T}
function av_buffer_realloc(buf: PPAVBufferRef; size: Integer): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_buffer_realloc';
{$ELSE}
function av_buffer_realloc(buf: PPAVBufferRef; size: Size_t): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_buffer_realloc';
{$ENDIF}

(**
 * Ensure dst refers to the same data as src.
 *
 * When *dst is already equivalent to src, do nothing. Otherwise unreference dst
 * and replace it with a new reference to src.
 *
 * @param dst Pointer to either a valid buffer reference or NULL. On success,
 *            this will point to a buffer reference equivalent to src. On
 *            failure, dst will be left untouched.
 * @param src A buffer reference to replace dst with. May be NULL, then this
 *            function is equivalent to av_buffer_unref(dst).
 * @return 0 on success
 *         AVERROR(ENOMEM) on memory allocation failure.
 *)
function av_buffer_replace(dst: PPAVBufferRef; src: PAVBufferRef): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_buffer_replace';

(**
 * @}
 *)

(**
 * @defgroup lavu_bufferpool AVBufferPool
 * @ingroup lavu_data
 *
 * @{
 * AVBufferPool is an API for a lock-free thread-safe pool of AVBuffers.
 *
 * Frequently allocating and freeing large buffers may be slow. AVBufferPool is
 * meant to solve this in cases when the caller needs a set of buffers of the
 * same size (the most obvious use case being buffers for raw video or audio
 * frames).
 *
 * At the beginning, the user must call av_buffer_pool_init() to create the
 * buffer pool. Then whenever a buffer is needed, call av_buffer_pool_get() to
 * get a reference to a new buffer, similar to av_buffer_alloc(). This new
 * reference works in all aspects the same way as the one created by
 * av_buffer_alloc(). However, when the last reference to this buffer is
 * unreferenced, it is returned to the pool instead of being freed and will be
 * reused for subsequent av_buffer_pool_get() calls.
 *
 * When the caller is done with the pool and no longer needs to allocate any new
 * buffers, av_buffer_pool_uninit() must be called to mark the pool as freeable.
 * Once all the buffers are released, it will automatically be freed.
 *
 * Allocating and releasing buffers with this API is thread-safe as long as
 * either the default alloc callback is used, or the user-supplied one is
 * thread-safe.
 *)

(**
 * The buffer pool. This structure is opaque and not meant to be accessed
 * directly. It is allocated with av_buffer_pool_init() and freed with
 * av_buffer_pool_uninit().
 *)
type
  PPAVBufferPool = ^PAVBufferPool;
  PAVBufferPool = ^TAVBufferPool;
  TAVBufferPool = record
    // need {$ALIGN 8}
    // defined libavutil/buffer_internal.h
  end;

(**
 * Allocate and initialize a buffer pool.
 *
 * @param size size of each buffer in this pool
 * @param alloc a function that will be used to allocate new buffers when the
 * pool is empty. May be NULL, then the default allocator will be used
 * (av_buffer_alloc()).
 * @return newly created buffer pool on success, NULL on error.
 *)
{$IFDEF FF_API_BUFFER_SIZE_T}
  TallocCall = function(size: Integer): PAVBufferRef; cdecl;
function av_buffer_pool_init(size: Integer; alloc: TallocCall): PAVBufferPool; cdecl; external AVUTIL_LIBNAME name _PU + 'av_buffer_pool_init';
{$ELSE}
  TallocCall = function(size: Size_t): PAVBufferRef; cdecl;
function av_buffer_pool_init(size: Size_t; alloc: TallocCall): PAVBufferPool; cdecl; external AVUTIL_LIBNAME name _PU + 'av_buffer_pool_init';
{$ENDIF}

(**
 * Allocate and initialize a buffer pool with a more complex allocator.
 *
 * @param size size of each buffer in this pool
 * @param opaque arbitrary user data used by the allocator
 * @param alloc a function that will be used to allocate new buffers when the
 *              pool is empty. May be NULL, then the default allocator will be
 *              used (av_buffer_alloc()).
 * @param pool_free a function that will be called immediately before the pool
 *                  is freed. I.e. after av_buffer_pool_uninit() is called
 *                  by the caller and all the frames are returned to the pool
 *                  and freed. It is intended to uninitialize the user opaque
 *                  data. May be NULL.
 * @return newly created buffer pool on success, NULL on error.
 *)
type
{$IFDEF FF_API_BUFFER_SIZE_T}
  Talloc2Call = function(opaque: Pointer; size: Integer): PAVBufferRef; cdecl;
{$ELSE}
  Talloc2Call = function(opaque: Pointer; size: Size_t): PAVBufferRef; cdecl;
{$ENDIF}
  Tfree2Call = procedure(opaque: POinter); cdecl;
{$IFDEF FF_API_BUFFER_SIZE_T}
function av_buffer_pool_init2(size: Integer; opaque: Pointer;
{$ELSE}
  Tav_buffer_pool_init2Proc = function(size: Size_t; opaque: Pointer;
{$ENDIF}
                                   alloc: Talloc2Call;
                                   pool_free: Tfree2Call): PAVBufferPool; cdecl; external AVUTIL_LIBNAME name _PU + 'av_buffer_pool_init2';

(**
 * Mark the pool as being available for freeing. It will actually be freed only
 * once all the allocated buffers associated with the pool are released. Thus it
 * is safe to call this function while some of the allocated buffers are still
 * in use.
 *
 * @param pool pointer to the pool to be freed. It will be set to NULL.
 *)
procedure av_buffer_pool_uninit(pool: PPAVBufferPool); cdecl; external AVUTIL_LIBNAME name _PU + 'av_buffer_pool_uninit';

(**
 * Allocate a new AVBuffer, reusing an old buffer from the pool when available.
 * This function may be called simultaneously from multiple threads.
 *
 * @return a reference to the new buffer on success, NULL on error.
 *)
function av_buffer_pool_get(pool: PAVBufferPool): PAVBufferRef; cdecl; external AVUTIL_LIBNAME name _PU + 'av_buffer_pool_get';

(**
 * Query the original opaque parameter of an allocated buffer in the pool.
 *
 * @param ref a buffer reference to a buffer returned by av_buffer_pool_get.
 * @return the opaque parameter set by the buffer allocator function of the
 *         buffer pool.
 *
 * @note the opaque parameter of ref is used by the buffer pool implementation,
 * therefore you have to use this function to access the original opaque
 * parameter of an allocated buffer.
 *)
procedure av_buffer_pool_buffer_get_opaque(ref: PAVBufferRef); cdecl; external AVUTIL_LIBNAME name _PU + 'av_buffer_pool_buffer_get_opaque';

(**
 * @}
 *)

implementation

end.
