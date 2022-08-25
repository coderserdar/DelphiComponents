(*
 * filter layer
 * Copyright (c) 2007 Bobby Bingham
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
 * Original file: libavfilter/avfilter.h
 * Ported by CodeCoolie@CNSW 2008/03/19 -> $Date:: 2021-04-25 #$
 *)

(**
 * @file
 * @ingroup lavfi
 * Main libavfilter public API header
 *)

(**
 * @defgroup lavfi libavfilter
 * Graph-based frame editing library.
 *
 * @{
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

unit libavfilter;

interface

{$I CompilerDefines.inc}

uses
  FFTypes,
  libavutil,
  libavutil_buffer,
  libavutil_dict,
  libavutil_frame,
  libavutil_log,
  libavutil_pixfmt,
  libavutil_rational,
  libavutil_samplefmt;

{$I libversion.inc}

const
(**
 * The number of the filter inputs is not determined just by AVFilter.inputs.
 * The filter might add additional inputs during initialization depending on the
 * options supplied to it.
 *)
  AVFILTER_FLAG_DYNAMIC_INPUTS            = (1 shl  0);
(**
 * The number of the filter outputs is not determined just by AVFilter.outputs.
 * The filter might add additional outputs during initialization depending on
 * the options supplied to it.
 *)
  AVFILTER_FLAG_DYNAMIC_OUTPUTS           = (1 shl  1);
(**
 * The filter supports multithreading by splitting frames into multiple parts
 * and processing them concurrently.
 *)
  AVFILTER_FLAG_SLICE_THREADS             = (1 shl  2);
(**
 * Some filters support a generic "enable" expression option that can be used
 * to enable or disable a filter in the timeline. Filters supporting this
 * option have this flag set. When the enable expression is false, the default
 * no-op filter_frame() function is called in place of the filter_frame()
 * callback defined on each input pad, thus the frame is passed unchanged to
 * the next filters.
 *)
  AVFILTER_FLAG_SUPPORT_TIMELINE_GENERIC  = (1 shl 16);
(**
 * Same as AVFILTER_FLAG_SUPPORT_TIMELINE_GENERIC, except that the filter will
 * have its filter_frame() callback(s) called as usual even when the enable
 * expression is false. The filter will disable filtering within the
 * filter_frame() callback(s) itself, for example executing code depending on
 * the AVFilterContext->is_disabled value.
 *)
  AVFILTER_FLAG_SUPPORT_TIMELINE_INTERNAL = (1 shl 17);
(**
 * Handy mask to test whether the filter supports or no the timeline feature
 * (internally or generically).
 *)
  AVFILTER_FLAG_SUPPORT_TIMELINE          = (AVFILTER_FLAG_SUPPORT_TIMELINE_GENERIC or AVFILTER_FLAG_SUPPORT_TIMELINE_INTERNAL);

(**
 * Process multiple parts of the frame concurrently.
 *)
  AVFILTER_THREAD_SLICE  = (1 shl 0);

  AVFILTER_CMD_FLAG_ONE  = 1; ///< Stop once a filter understood the command (for target=all for example), fast filters are favored automatically
  AVFILTER_CMD_FLAG_FAST = 2; ///< Only execute command when its fast (like a video out that supports contrast adjustment in hw)

  AVFILTER_AUTO_CONVERT_NONE = -1; (**< all automatic conversions disabled *)
  AVFILTER_AUTO_CONVERT_ALL  =  0;  (**< all automatic conversions enabled *)

(**
 * Return the LIBAVFILTER_VERSION_INT constant.
 *)
function avfilter_version: Cardinal; cdecl; external AVFILTER_LIBNAME name _PU + 'avfilter_version';

(**
 * Return the libavfilter build-time configuration.
 *)
function avfilter_configuration: PAnsiChar; cdecl; external AVFILTER_LIBNAME name _PU + 'avfilter_configuration';

(**
 * Return the libavfilter license.
 *)
function avfilter_license: PAnsiChar; cdecl; external AVFILTER_LIBNAME name _PU + 'avfilter_license';

type
  PPAVFilterContext = ^PAVFilterContext;
  PAVFilterContext = ^TAVFilterContext;
  PPPAVFilterLink = ^PPAVFilterLink;
  PPAVFilterLink = ^PAVFilterLink;
  PAVFilterLink = ^TAVFilterLink;
  PAVFilterPad = ^TAVFilterPad;
  TAVFilterPad = record
    // need {$ALIGN 8}
    // defined in libavfilter/internal.h
  end;
  PAVFilterFormats = ^TAVFilterFormats;
  TAVFilterFormats = record
    // need {$ALIGN 8}
    // defined in libavfilter/formats.h
  end;
  PAVFilterChannelLayouts = ^TAVFilterChannelLayouts;
  TAVFilterChannelLayouts = record
    // need {$ALIGN 8}
    // defined in libavfilter/formats.h
  end;

(**
 * Filter definition. This defines the pads a filter contains, and all the
 * callback functions used to interact with the filter.
 *)
  PPAVFilter = ^PAVFilter;
  PAVFilter = ^TAVFilter;
  TAVFilter = record
    (**
     * Filter name. Must be non-NULL and unique among filters.
     *)
    name: PAnsiChar;

    (**
     * A description of the filter. May be NULL.
     *
     * You should use the NULL_IF_CONFIG_SMALL() macro to define it.
     *)
    description: PAnsiChar;

    (**
     * List of inputs, terminated by a zeroed element.
     *
     * NULL if there are no (static) inputs. Instances of filters with
     * AVFILTER_FLAG_DYNAMIC_INPUTS set may have more inputs than present in
     * this list.
     *)
    inputs: PAVFilterPad;
    (**
     * List of outputs, terminated by a zeroed element.
     *
     * NULL if there are no (static) outputs. Instances of filters with
     * AVFILTER_FLAG_DYNAMIC_OUTPUTS set may have more outputs than present in
     * this list.
     *)
    outputs: PAVFilterPad;

    (**
     * A class for the private data, used to declare filter private AVOptions.
     * This field is NULL for filters that do not declare any options.
     *
     * If this field is non-NULL, the first member of the filter private data
     * must be a pointer to AVClass, which will be set by libavfilter generic
     * code to this class.
     *)
    priv_class: PAVClass;

    (**
     * A combination of AVFILTER_FLAG_*
     *)
    flags: Integer;

    (*****************************************************************
     * All fields below this line are not part of the public API. They
     * may not be used outside of libavfilter and can be changed and
     * removed at will.
     * New public fields should be added right above.
     *****************************************************************
     *)

    (**
     * Filter pre-initialization function
     *
     * This callback will be called immediately after the filter context is
     * allocated, to allow allocating and initing sub-objects.
     *
     * If this callback is not NULL, the uninit callback will be called on
     * allocation failure.
     *
     * @return 0 on success,
     *         AVERROR code on failure (but the code will be
     *           dropped and treated as ENOMEM by the calling code)
     *)
    preinit: function(ctx: PAVFilterContext): Integer; cdecl;

    (**
     * Filter initialization function.
     *
     * This callback will be called only once during the filter lifetime, after
     * all the options have been set, but before links between filters are
     * established and format negotiation is done.
     *
     * Basic filter initialization should be done here. Filters with dynamic
     * inputs and/or outputs should create those inputs/outputs here based on
     * provided options. No more changes to this filter's inputs/outputs can be
     * done after this callback.
     *
     * This callback must not assume that the filter links exist or frame
     * parameters are known.
     *
     * @ref AVFilter.uninit "uninit" is guaranteed to be called even if
     * initialization fails, so this callback does not have to clean up on
     * failure.
     *
     * @return 0 on success, a negative AVERROR on failure
     *)
    init: function(ctx: PAVFilterContext): Integer; cdecl;

    (**
     * Should be set instead of @ref AVFilter.init "init" by the filters that
     * want to pass a dictionary of AVOptions to nested contexts that are
     * allocated during init.
     *
     * On return, the options dict should be freed and replaced with one that
     * contains all the options which could not be processed by this filter (or
     * with NULL if all the options were processed).
     *
     * Otherwise the semantics is the same as for @ref AVFilter.init "init".
     *)
    init_dict: function(ctx: PAVFilterContext; options: PPAVDictionary): Integer; cdecl;

    (**
     * Filter uninitialization function.
     *
     * Called only once right before the filter is freed. Should deallocate any
     * memory held by the filter, release any buffer references, etc. It does
     * not need to deallocate the AVFilterContext.priv memory itself.
     *
     * This callback may be called even if @ref AVFilter.init "init" was not
     * called or failed, so it must be prepared to handle such a situation.
     *)
    uninit: procedure(ctx: PAVFilterContext); cdecl;

    (**
     * Query formats supported by the filter on its inputs and outputs.
     *
     * This callback is called after the filter is initialized (so the inputs
     * and outputs are fixed), shortly before the format negotiation. This
     * callback may be called more than once.
     *
     * This callback must set AVFilterLink.outcfg.formats on every input link and
     * AVFilterLink.incfg.formats on every output link to a list of pixel/sample
     * formats that the filter supports on that link. For audio links, this
     * filter must also set @ref AVFilterLink.incfg.samplerates "in_samplerates" /
     * @ref AVFilterLink.outcfg.samplerates "out_samplerates" and
     * @ref AVFilterLink.incfg.channel_layouts "in_channel_layouts" /
     * @ref AVFilterLink.outcfg.channel_layouts "out_channel_layouts" analogously.
     *
     * This callback may be NULL for filters with one input, in which case
     * libavfilter assumes that it supports all input formats and preserves
     * them on output.
     *
     * @return zero on success, a negative value corresponding to an
     * AVERROR code otherwise
     *)
    query_formats: function(ctx: PAVFilterContext): Integer; cdecl;

    priv_size: Integer;  ///< size of private data to allocate for the filter

    flags_internal: Integer; ///< Additional flags for avfilter internal use only.

{$IFDEF FF_API_NEXT}
    (**
     * Used by the filter registration system. Must not be touched by any other
     * code.
     *)
    next: PAVFilter;
{$ENDIF}

    (**
     * Make the filter instance process a command.
     *
     * @param cmd    the command to process, for handling simplicity all commands must be alphanumeric only
     * @param arg    the argument for the command
     * @param res    a buffer with size res_size where the filter(s) can return a response. This must not change when the command is not supported.
     * @param flags  if AVFILTER_CMD_FLAG_FAST is set and the command would be
     *               time consuming then a filter should treat it like an unsupported command
     *
     * @returns >=0 on success otherwise an error code.
     *          AVERROR(ENOSYS) on unsupported commands
     *)
    process_command: function(ctx: PAVFilterContext; const cmd, arg: PAnsiChar; res: PAnsiChar; res_len, flags: Integer): Integer; cdecl;

    (**
     * Filter initialization function, alternative to the init()
     * callback. Args contains the user-supplied parameters, opaque is
     * used for providing binary data.
     *)
    init_opaque: function(ctx: PAVFilterContext; opaque: Pointer): Integer; cdecl;

    (**
     * Filter activation function.
     *
     * Called when any processing is needed from the filter, instead of any
     * filter_frame and request_frame on pads.
     *
     * The function must examine inlinks and outlinks and perform a single
     * step of processing. If there is nothing to do, the function must do
     * nothing and not return an error. If more steps are or may be
     * possible, it must use ff_filter_set_ready() to schedule another
     * activation.
     *)
    activate: function(ctx: PAVFilterContext): Integer; cdecl;
  end;

  PAVFilterInternal = ^TAVFilterInternal;
  TAVFilterInternal = record
    // need {$ALIGN 8}
    // defined in libavfilter/internal.h
  end;

(** An instance of a filter *)
  PPAVFilterGraph = ^PAVFilterGraph;
  PAVFilterGraph = ^TAVFilterGraph;
  TAVFilterContext = record
    av_class: PAVClass;         ///< needed for av_log() and filters common options

    filter: PAVFilter;          ///< the AVFilter of which this is an instance

    name: PAnsiChar;            ///< name of this filter instance

    input_pads: PAVFilterPad;   ///< array of input pads
    inputs: PPAVFilterLink;     ///< array of pointers to input links
    nb_inputs: Cardinal;        ///< number of input pads

    output_pads: PAVFilterPad;  ///< array of output pads
    outputs: PPAVFilterLink;    ///< array of pointers to output links
    nb_outputs: Cardinal;       ///< number of output pads

    priv: Pointer;              ///< private data for use by the filter

    graph: PAVFilterGraph;      ///< filtergraph this filter belongs to

    (**
     * Type of multithreading being allowed/used. A combination of
     * AVFILTER_THREAD_* flags.
     *
     * May be set by the caller before initializing the filter to forbid some
     * or all kinds of multithreading for this filter. The default is allowing
     * everything.
     *
     * When the filter is initialized, this field is combined using bit AND with
     * AVFilterGraph.thread_type to get the final mask used for determining
     * allowed threading types. I.e. a threading type needs to be set in both
     * to be allowed.
     *
     * After the filter is initialized, libavfilter sets this field to the
     * threading type that is actually used (0 for no multithreading).
     *)
    thread_type: Integer;

    (**
     * An opaque struct for libavfilter internal use.
     *)
    internal: PAVFilterInternal;

    command_queue: Pointer; //PAVFilterCommand;

    enable_str: PAnsiChar;      ///< enable expression string
    enable: Pointer;            ///< parsed expression (AVExpr*)
    var_values: PDouble;        ///< variable values for the enable expression
    is_disabled: Integer;       ///< the enabled state from the last expression evaluation

    (**
     * For filters which will create hardware frames, sets the device the
     * filter should create them in.  All other filters will ignore this field:
     * in particular, a filter which consumes or processes hardware frames will
     * instead use the hw_frames_ctx field in AVFilterLink to carry the
     * hardware context information.
     *)
    hw_device_ctx: PAVBufferRef;

    (**
     * Max number of threads allowed in this filter instance.
     * If <= 0, its value is ignored.
     * Overrides global number of threads set per filter graph.
     *)
    nb_threads: Integer;

    (**
     * Ready status of the filter.
     * A non-0 value means that the filter needs activating;
     * a higher value suggests a more urgent activation.
     *)
    ready: Cardinal;

    (**
     * Sets the number of extra hardware frames which the filter will
     * allocate on its output links for use in following filters or by
     * the caller.
     *
     * Some hardware filters require all frames that they will use for
     * output to be defined in advance before filtering starts.  For such
     * filters, any hardware frame pools used for output must therefore be
     * of fixed size.  The extra frames set here are on top of any number
     * that the filter needs internally in order to operate normally.
     *
     * This field must be set before the graph containing this filter is
     * configured.
     *)
    extra_hw_frames: Integer;
  end;

(**
 * Lists of formats / etc. supported by an end of a link.
 *
 * This structure is directly part of AVFilterLink, in two copies:
 * one for the source filter, one for the destination filter.

 * These lists are used for negotiating the format to actually be used,
 * which will be loaded into the format and channel_layout members of
 * AVFilterLink, when chosen.
 *)
  TAVFilterFormatsConfig = record
    (**
     * List of supported formats (pixel or sample).
     *)
    formats: PAVFilterFormats;

    (**
     * Lists of supported sample rates, only for audio.
     *)
    samplerates: PAVFilterFormats;

    (**
     * Lists of supported channel layouts, only for audio.
     *)
    channel_layouts: PAVFilterChannelLayouts;
  end;

(**
 * A link between two filters. This contains pointers to the source and
 * destination filters between which this link exists, and the indexes of
 * the pads involved. In addition, this link also contains the parameters
 * which have been negotiated and agreed upon between the filter, such as
 * image dimensions, format, etc.
 *
 * Applications must not normally access the link structure directly.
 * Use the buffersrc and buffersink API instead.
 * In the future, access to the header may be reserved for filters
 * implementation.
 *)
  Tinit_state = (
    AVLINK_UNINIT = 0,      ///< not started
    AVLINK_STARTINIT,       ///< started, but incomplete
    AVLINK_INIT             ///< complete
  );
  TAVFilterLink = record
    src: PAVFilterContext;    ///< source filter
    srcpad: PAVFilterPad;     ///< output pad on the source filter

    dst: PAVFilterContext;    ///< dest filter
    dstpad: PAVFilterPad;     ///< input pad on the dest filter

    type_: TAVMediaType;      ///< filter media type

    (* These parameters apply only to video *)
    w: Integer;               ///< agreed upon image width
    h: Integer;               ///< agreed upon image height
    sample_aspect_ratio: TAVRational; ///< agreed upon sample aspect ratio
    (* These parameters apply only to audio *)
    channel_layout: Int64;    ///< channel layout of current buffer (see libavutil/channel_layout.h)
    sample_rate: Integer;     ///< samples per second

    format: Integer;          ///< agreed upon media format

    (**
     * Define the time base used by the PTS of the frames/samples
     * which will pass through this link.
     * During the configuration stage, each filter is supposed to
     * change only the output timebase, while the timebase of the
     * input link is assumed to be an unchangeable property.
     *)
    time_base: TAVRational;

    (*****************************************************************
     * All fields below this line are not part of the public API. They
     * may not be used outside of libavfilter and can be changed and
     * removed at will.
     * New public fields should be added right above.
     *****************************************************************
     *)

    (**
     * Lists of supported formats / etc. supported by the input filter.
     *)
    incfg: TAVFilterFormatsConfig;

    (**
     * Lists of supported formats / etc. supported by the output filter.
     *)
    outcfg: TAVFilterFormatsConfig;

    (** stage of the initialization of the link properties (dimensions, etc) *)
    init_state: Tinit_state;

    (**
     * Graph the filter belongs to.
     *)
    graph: PAVFilterGraph;

    (**
     * Current timestamp of the link, as defined by the most recent
     * frame(s), in link time_base units.
     *)
    current_pts: Int64;

    (**
     * Current timestamp of the link, as defined by the most recent
     * frame(s), in AV_TIME_BASE units.
     *)
    current_pts_us: Int64;

    (**
     * Index in the age array.
     *)
    age_index: Integer;

    (**
     * Frame rate of the stream on the link, or 1/0 if unknown or variable;
     * if left to 0/0, will be automatically copied from the first input
     * of the source filter if it exists.
     *
     * Sources should set it to the best estimation of the real frame rate.
     * If the source frame rate is unknown or variable, set this to 1/0.
     * Filters should update it if necessary depending on their function.
     * Sinks can use it to set a default output frame rate.
     * It is similar to the r_frame_rate field in AVStream.
     *)
    frame_rate: TAVRational;

    (**
     * Buffer partially filled with samples to achieve a fixed/minimum size.
     *)
    partial_buf: PAVFrame;

    (**
     * Size of the partial buffer to allocate.
     * Must be between min_samples and max_samples.
     *)
    partial_buf_size: Integer;

    (**
     * Minimum number of samples to filter at once. If filter_frame() is
     * called with fewer samples, it will accumulate them in partial_buf.
     * This field and the related ones must not be changed after filtering
     * has started.
     * If 0, all related fields are ignored.
     *)
    min_samples: Integer;

    (**
     * Maximum number of samples to filter at once. If filter_frame() is
     * called with more samples, it will split them.
     *)
    max_samples: Integer;

    (**
     * Number of channels.
     *)
    channels: Integer;

    (**
     * Number of past frames sent through the link.
     *)
    frame_count_in, frame_count_out: Int64;

    (**
     * A pointer to a FFFramePool struct.
     *)
    frame_pool: Pointer;

    (**
     * True if a frame is currently wanted on the output of this filter.
     * Set when ff_request_frame() is called by the output,
     * cleared when a frame is filtered.
     *)
    frame_wanted_out: Integer;

    (**
     * For hwaccel pixel formats, this should be a reference to the
     * AVHWFramesContext describing the frames.
     *)
    hw_frames_ctx: PAVBufferRef;

{$IFNDEF FF_INTERNAL_FIELDS}

    (**
     * Internal structure members.
     * The fields below this limit are internal for libavfilter's use
     * and must in no way be accessed by applications.
     *)
    reserved: array[0..$F000-1] of AnsiChar;

{$ELSE} {* FF_INTERNAL_FIELDS *}

    (**
     * Queue of frames waiting to be filtered.
     *)
    fifo: TFFFrameQueue;

    (**
     * If set, the source filter can not generate a frame as is.
     * The goal is to avoid repeatedly calling the request_frame() method on
     * the same link.
     *)
    frame_blocked_in: Integer;

    (**
     * Link input status.
     * If not zero, all attempts of filter_frame will fail with the
     * corresponding code.
     *)
    status_in: Integer;

    (**
     * Timestamp of the input status change.
     *)
    status_in_pts: Int64;

    (**
     * Link output status.
     * If not zero, all attempts of request_frame will fail with the
     * corresponding code.
     *)
    status_out: Integer;

{$ENDIF} (* FF_INTERNAL_FIELDS *)
  end;

  PAVFilterGraphInternal = ^TAVFilterGraphInternal;
  TAVFilterGraphInternal = record
    // need {$ALIGN 8}
    // defined in libavfilter/internal.h
  end;

(**
 * A function pointer passed to the @ref AVFilterGraph.execute callback to be
 * executed multiple times, possibly in parallel.
 *
 * @param ctx the filter context the job belongs to
 * @param arg an opaque parameter passed through from @ref
 *            AVFilterGraph.execute
 * @param jobnr the index of the job being executed
 * @param nb_jobs the total number of jobs
 *
 * @return 0 on success, a negative AVERROR on error
 *)
  Tavfilter_action_func = function(ctx: PAVFilterContext; arg: Pointer; jobnr, nb_jobs: Integer): Integer; cdecl;

(**
 * A function executing multiple jobs, possibly in parallel.
 *
 * @param ctx the filter context to which the jobs belong
 * @param func the function to be called multiple times
 * @param arg the argument to be passed to func
 * @param ret a nb_jobs-sized array to be filled with return values from each
 *            invocation of func
 * @param nb_jobs the number of jobs to execute
 *
 * @return 0 on success, a negative AVERROR on error
 *)
  Tavfilter_execute_func = function(ctx: PAVFilterContext; func: Tavfilter_action_func;
                                    arg: Pointer; ret: PInteger; nb_jobs: Integer): Integer; cdecl;

  TAVFilterGraph = record
    av_class: PAVClass;
    filters: PPAVFilterContext;
    nb_filters: Cardinal;

    scale_sws_opts: PAnsiChar; ///< sws options to use for the auto-inserted scale filters
{$IFDEF FF_API_LAVR_OPTS}
    resample_lavr_opts: PAnsiChar; ///< libavresample options to use for the auto-inserted resample filters
{$ENDIF}

    (**
     * Type of multithreading allowed for filters in this graph. A combination
     * of AVFILTER_THREAD_* flags.
     *
     * May be set by the caller at any point, the setting will apply to all
     * filters initialized after that. The default is allowing everything.
     *
     * When a filter in this graph is initialized, this field is combined using
     * bit AND with AVFilterContext.thread_type to get the final mask used for
     * determining allowed threading types. I.e. a threading type needs to be
     * set in both to be allowed.
     *)
    thread_type: Integer;

    (**
     * Maximum number of threads used by filters in this graph. May be set by
     * the caller before adding any filters to the filtergraph. Zero (the
     * default) means that the number of threads is determined automatically.
     *)
    nb_threads: Integer;

    (**
     * Opaque object for libavfilter internal use.
     *)
    internal: PAVFilterGraphInternal;

    (**
     * Opaque user data. May be set by the caller to an arbitrary value, e.g. to
     * be used from callbacks like @ref AVFilterGraph.execute.
     * Libavfilter will not touch this field in any way.
     *)
    opaque: Pointer;

    (**
     * This callback may be set by the caller immediately after allocating the
     * graph and before adding any filters to it, to provide a custom
     * multithreading implementation.
     *
     * If set, filters with slice threading capability will call this callback
     * to execute multiple jobs in parallel.
     *
     * If this field is left unset, libavfilter will use its internal
     * implementation, which may or may not be multithreaded depending on the
     * platform and build options.
     *)
    execute: Tavfilter_execute_func;

    aresample_swr_opts: PAnsiChar; ///< swr options to use for the auto-inserted aresample filters, Access ONLY through AVOptions

    (**
     * Private fields
     *
     * The following fields are for internal use only.
     * Their type, offset, number and semantic can change without notice.
     *)

    sink_links: PPAVFilterLink;
    sink_links_count: Integer;

    disable_auto_convert: Cardinal;
  end;

(**
 * Get the number of elements in a NULL-terminated array of AVFilterPads (e.g.
 * AVFilter.inputs/outputs).
 *)
function avfilter_pad_count(const pads: PAVFilterPad): Integer; cdecl; external AVFILTER_LIBNAME name _PU + 'avfilter_pad_count';

(**
 * Get the name of an AVFilterPad.
 *
 * @param pads an array of AVFilterPads
 * @param pad_idx index of the pad in the array; it is the caller's
 *                responsibility to ensure the index is valid
 *
 * @return name of the pad_idx'th pad in pads
 *)
function avfilter_pad_get_name(const pads: PAVFilterPad; pad_idx: Integer): PAnsiChar; cdecl; external AVFILTER_LIBNAME name _PU + 'avfilter_pad_get_name';

(**
 * Get the type of an AVFilterPad.
 *
 * @param pads an array of AVFilterPads
 * @param pad_idx index of the pad in the array; it is the caller's
 *                responsibility to ensure the index is valid
 *
 * @return type of the pad_idx'th pad in pads
 *)
function avfilter_pad_get_type(const pads: PAVFilterPad; pad_idx: Integer): TAVMediaType; cdecl; external AVFILTER_LIBNAME name _PU + 'avfilter_pad_get_type';

(**
 * Link two filters together.
 *
 * @param src    the source filter
 * @param srcpad index of the output pad on the source filter
 * @param dst    the destination filter
 * @param dstpad index of the input pad on the destination filter
 * @return       zero on success
 *)
function avfilter_link(src: PAVFilterContext; srcpad: Cardinal;
                  dst: PAVFilterContext; dstpad: Cardinal): Integer; cdecl; external AVFILTER_LIBNAME name _PU + 'avfilter_link';

(**
 * Free the link in *link, and set its pointer to NULL.
 *)
procedure avfilter_link_free(link: PPAVFilterLink); cdecl; external AVFILTER_LIBNAME name _PU + 'avfilter_link_free';

{$IFDEF FF_API_FILTER_GET_SET}
(**
 * Get the number of channels of a link.
 * @deprecated Use av_buffersink_get_channels()
 *)
function avfilter_link_get_channels(link: PAVFilterLink): Integer; cdecl; external AVFILTER_LIBNAME name _PU + 'avfilter_link_get_channels';
{$ENDIF}

{$IFDEF FF_API_FILTER_LINK_SET_CLOSED}
(**
 * Set the closed field of a link.
 * @deprecated applications are not supposed to mess with links, they should
 * close the sinks.
 *)
procedure avfilter_link_set_closed(link: PAVFilterLink; closed: Integer); cdecl; external AVFILTER_LIBNAME name _PU + 'avfilter_link_set_closed';
{$ENDIF}

(**
 * Negotiate the media format, dimensions, etc of all inputs to a filter.
 *
 * @param filter the filter to negotiate the properties for its inputs
 * @return       zero on successful negotiation
 *)
function avfilter_config_links(filter: PAVFilterContext): Integer; cdecl; external AVFILTER_LIBNAME name _PU + 'avfilter_config_links';

(**
 * Make the filter instance process a command.
 * It is recommended to use avfilter_graph_send_command().
 *)
function avfilter_process_command(filter: PAVFilterContext; const cmd, arg: PAnsiChar; res: PAnsiChar; res_len, flags: Integer): Integer; cdecl; external AVFILTER_LIBNAME name _PU + 'avfilter_process_command';

(**
 * Iterate over all registered filters.
 *
 * @param opaque a pointer where libavfilter will store the iteration state. Must
 *               point to NULL to start the iteration.
 *
 * @return the next registered filter or NULL when the iteration is
 *         finished
 *)
function av_filter_iterate(opaque: PPointer): PAVFilter; cdecl; external AVFILTER_LIBNAME name _PU + 'av_filter_iterate';

{$IFDEF FF_API_NEXT}
(** Initialize the filter system. Register all builtin filters. *)
procedure avfilter_register_all; cdecl; external AVFILTER_LIBNAME name _PU + 'avfilter_register_all';

(**
 * Register a filter. This is only needed if you plan to use
 * avfilter_get_by_name later to lookup the AVFilter structure by name. A
 * filter can still by instantiated with avfilter_graph_alloc_filter even if it
 * is not registered.
 *
 * @param filter the filter to register
 * @return 0 if the registration was successful, a negative value
 * otherwise
 *)
function avfilter_register(filter: PAVFilter): Integer; cdecl; external AVFILTER_LIBNAME name _PU + 'avfilter_register';

(**
 * Iterate over all registered filters.
 * @return If prev is non-NULL, next registered filter after prev or NULL if
 * prev is the last filter. If prev is NULL, return the first registered filter.
 *)
function avfilter_next(const prev: PAVFilter): PAVFilter; cdecl; external AVFILTER_LIBNAME name _PU + 'avfilter_next';
{$ENDIF}

(**
 * Get a filter definition matching the given name.
 *
 * @param name the filter name to find
 * @return     the filter definition, if any matching one is registered.
 *             NULL if none found.
 *)
function avfilter_get_by_name(const name: PAnsiChar): PAVFilter; cdecl; external AVFILTER_LIBNAME name _PU + 'avfilter_get_by_name';

(**
 * Initialize a filter with the supplied parameters.
 *
 * @param ctx  uninitialized filter context to initialize
 * @param args Options to initialize the filter with. This must be a
 *             ':'-separated list of options in the 'key=value' form.
 *             May be NULL if the options have been set directly using the
 *             AVOptions API or there are no options that need to be set.
 * @return 0 on success, a negative AVERROR on failure
 *)
function avfilter_init_str(ctx: PAVFilterContext; const args: PAnsiChar): Integer; cdecl; external AVFILTER_LIBNAME name _PU + 'avfilter_init_str';

(**
 * Initialize a filter with the supplied dictionary of options.
 *
 * @param ctx     uninitialized filter context to initialize
 * @param options An AVDictionary filled with options for this filter. On
 *                return this parameter will be destroyed and replaced with
 *                a dict containing options that were not found. This dictionary
 *                must be freed by the caller.
 *                May be NULL, then this function is equivalent to
 *                avfilter_init_str() with the second parameter set to NULL.
 * @return 0 on success, a negative AVERROR on failure
 *
 * @note This function and avfilter_init_str() do essentially the same thing,
 * the difference is in manner in which the options are passed. It is up to the
 * calling code to choose whichever is more preferable. The two functions also
 * behave differently when some of the provided options are not declared as
 * supported by the filter. In such a case, avfilter_init_str() will fail, but
 * this function will leave those extra options in the options AVDictionary and
 * continue as usual.
 *)
function avfilter_init_dict(ctx: PAVFilterContext; options: PPAVDictionary): Integer; cdecl; external AVFILTER_LIBNAME name _PU + 'avfilter_init_dict';

(**
 * Free a filter context. This will also remove the filter from its
 * filtergraph's list of filters.
 *
 * @param filter the filter to free
 *)
procedure avfilter_free(filter: PAVFilterContext); cdecl; external AVFILTER_LIBNAME name _PU + 'avfilter_free';

(**
 * Insert a filter in the middle of an existing link.
 *
 * @param link the link into which the filter should be inserted
 * @param filt the filter to be inserted
 * @param filt_srcpad_idx the input pad on the filter to connect
 * @param filt_dstpad_idx the output pad on the filter to connect
 * @return     zero on success
 *)
function avfilter_insert_filter(link: PAVFilterLink; filt: PAVFilterContext;
                           filt_srcpad_idx, filt_dstpad_idx: Cardinal): Integer; cdecl; external AVFILTER_LIBNAME name _PU + 'avfilter_insert_filter';

(**
 * @return AVClass for AVFilterContext.
 *
 * @see av_opt_find().
 *)
function avfilter_get_class: PAVClass; cdecl; external AVFILTER_LIBNAME name _PU + 'avfilter_get_class';

(**
 * Allocate a filter graph.
 *
 * @return the allocated filter graph on success or NULL.
 *)
function avfilter_graph_alloc: PAVFilterGraph; cdecl; external AVFILTER_LIBNAME name _PU + 'avfilter_graph_alloc';

(**
 * Create a new filter instance in a filter graph.
 *
 * @param graph graph in which the new filter will be used
 * @param filter the filter to create an instance of
 * @param name Name to give to the new instance (will be copied to
 *             AVFilterContext.name). This may be used by the caller to identify
 *             different filters, libavfilter itself assigns no semantics to
 *             this parameter. May be NULL.
 *
 * @return the context of the newly created filter instance (note that it is
 *         also retrievable directly through AVFilterGraph.filters or with
 *         avfilter_graph_get_filter()) on success or NULL on failure.
 *)
function avfilter_graph_alloc_filter(graph: PAVFilterGraph;
                                              const filter: PAVFilter;
                                              const name: PAnsiChar): PAVFilterContext; cdecl; external AVFILTER_LIBNAME name _PU + 'avfilter_graph_alloc_filter';

(**
 * Get a filter instance identified by instance name from graph.
 *
 * @param graph filter graph to search through.
 * @param name filter instance name (should be unique in the graph).
 * @return the pointer to the found filter instance or NULL if it
 * cannot be found.
 *)
function avfilter_graph_get_filter(graph: PAVFilterGraph; const name: PAnsiChar): PAVFilterContext; cdecl; external AVFILTER_LIBNAME name _PU + 'avfilter_graph_get_filter';

(**
 * Create and add a filter instance into an existing graph.
 * The filter instance is created from the filter filt and inited
 * with the parameter args. opaque is currently ignored.
 *
 * In case of success put in *filt_ctx the pointer to the created
 * filter instance, otherwise set *filt_ctx to NULL.
 *
 * @param name the instance name to give to the created filter instance
 * @param graph_ctx the filter graph
 * @return a negative AVERROR error code in case of failure, a non
 * negative value otherwise
 *)
function avfilter_graph_create_filter(filt_ctx: PPAVFilterContext; const filt: PAVFilter;
                                 const name: PAnsiChar; const args: PAnsiChar; opaque: Pointer;
                                 graph_ctx: PAVFilterGraph): Integer; cdecl; external AVFILTER_LIBNAME name _PU + 'avfilter_graph_create_filter';

(**
 * Enable or disable automatic format conversion inside the graph.
 *
 * Note that format conversion can still happen inside explicitly inserted
 * scale and aresample filters.
 *
 * @param flags  any of the AVFILTER_AUTO_CONVERT_* constants
 *)
procedure avfilter_graph_set_auto_convert(graph: PAVFilterGraph; flags: Integer{Cardinal}); cdecl; external AVFILTER_LIBNAME name _PU + 'avfilter_graph_set_auto_convert';

//  TAVFilterConverter = (
//    AVFILTER_AUTO_CONVERT_NONE = -1, (**< all automatic conversions disabled *)
//    AVFILTER_AUTO_CONVERT_ALL  =  0  (**< all automatic conversions enabled *)
//  );

(**
 * Check validity and configure all the links and formats in the graph.
 *
 * @param graphctx the filter graph
 * @param log_ctx context used for logging
 * @return >= 0 in case of success, a negative AVERROR code otherwise
 *)
function avfilter_graph_config(graphctx: PAVFilterGraph; log_ctx: Pointer): Integer; cdecl; external AVFILTER_LIBNAME name _PU + 'avfilter_graph_config';

(**
 * Free a graph, destroy its links, and set *graph to NULL.
 * If *graph is NULL, do nothing.
 *)
procedure avfilter_graph_free(graph: PPAVFilterGraph); cdecl; external AVFILTER_LIBNAME name _PU + 'avfilter_graph_free';

(**
 * A linked-list of the inputs/outputs of the filter chain.
 *
 * This is mainly useful for avfilter_graph_parse() / avfilter_graph_parse2(),
 * where it is used to communicate open (unlinked) inputs and outputs from and
 * to the caller.
 * This struct specifies, per each not connected pad contained in the graph, the
 * filter context and the pad index required for establishing a link.
 *)
type
  PPAVFilterInOut = ^PAVFilterInOut;
  PAVFilterInOut = ^TAVFilterInOut;
  TAVFilterInOut = record
    (** unique name for this input/output in the list *)
    name: PAnsiChar;

    (** filter context associated to this input/output *)
    filter_ctx: PAVFilterContext;

    (** index of the filt_ctx pad to use for linking *)
    pad_idx: Integer;

    (** next input/input in the list, NULL if this is the last *)
    next: PAVFilterInOut;
  end;

(**
 * Allocate a single AVFilterInOut entry.
 * Must be freed with avfilter_inout_free().
 * @return allocated AVFilterInOut on success, NULL on failure.
 *)
function avfilter_inout_alloc: PAVFilterInOut; cdecl; external AVFILTER_LIBNAME name _PU + 'avfilter_inout_alloc';

(**
 * Free the supplied list of AVFilterInOut and set *inout to NULL.
 * If *inout is NULL, do nothing.
 *)
procedure avfilter_inout_free(inout: PPAVFilterInOut); cdecl; external AVFILTER_LIBNAME name _PU + 'avfilter_inout_free';

(**
 * Add a graph described by a string to a graph.
 *
 * @note The caller must provide the lists of inputs and outputs,
 * which therefore must be known before calling the function.
 *
 * @note The inputs parameter describes inputs of the already existing
 * part of the graph; i.e. from the point of view of the newly created
 * part, they are outputs. Similarly the outputs parameter describes
 * outputs of the already existing filters, which are provided as
 * inputs to the parsed filters.
 *
 * @param graph   the filter graph where to link the parsed graph context
 * @param filters string to be parsed
 * @param inputs  linked list to the inputs of the graph
 * @param outputs linked list to the outputs of the graph
 * @return zero on success, a negative AVERROR code on error
 *)
function avfilter_graph_parse(graph: PAVFilterGraph; const filters: PAnsiChar;
                          inputs, outputs: PAVFilterInOut; log_ctx: Pointer): Integer; cdecl; external AVFILTER_LIBNAME name _PU + 'avfilter_graph_parse';

(**
 * Add a graph described by a string to a graph.
 *
 * In the graph filters description, if the input label of the first
 * filter is not specified, "in" is assumed; if the output label of
 * the last filter is not specified, "out" is assumed.
 *
 * @param graph   the filter graph where to link the parsed graph context
 * @param filters string to be parsed
 * @param inputs  pointer to a linked list to the inputs of the graph, may be NULL.
 *                If non-NULL, *inputs is updated to contain the list of open inputs
 *                after the parsing, should be freed with avfilter_inout_free().
 * @param outputs pointer to a linked list to the outputs of the graph, may be NULL.
 *                If non-NULL, *outputs is updated to contain the list of open outputs
 *                after the parsing, should be freed with avfilter_inout_free().
 * @return non negative on success, a negative AVERROR code on error
 *)
function avfilter_graph_parse_ptr(graph: PAVFilterGraph; const filters: PAnsiChar;
                              inputs, outputs: PPAVFilterInOut; log_ctx: Pointer): Integer; cdecl; external AVFILTER_LIBNAME name _PU + 'avfilter_graph_parse_ptr';

(**
 * Add a graph described by a string to a graph.
 *
 * @param[in]  graph   the filter graph where to link the parsed graph context
 * @param[in]  filters string to be parsed
 * @param[out] inputs  a linked list of all free (unlinked) inputs of the
 *                     parsed graph will be returned here. It is to be freed
 *                     by the caller using avfilter_inout_free().
 * @param[out] outputs a linked list of all free (unlinked) outputs of the
 *                     parsed graph will be returned here. It is to be freed by the
 *                     caller using avfilter_inout_free().
 * @return zero on success, a negative AVERROR code on error
 *
 * @note This function returns the inputs and outputs that are left
 * unlinked after parsing the graph and the caller then deals with
 * them.
 * @note This function makes no reference whatsoever to already
 * existing parts of the graph and the inputs parameter will on return
 * contain inputs of the newly parsed part of the graph.  Analogously
 * the outputs parameter will contain outputs of the newly created
 * filters.
 *)
function avfilter_graph_parse2(graph: PAVFilterGraph; const filters: PAnsiChar;
                          inputs, outputs: PPAVFilterInOut): Integer; cdecl; external AVFILTER_LIBNAME name _PU + 'avfilter_graph_parse2';

(**
 * Send a command to one or more filter instances.
 *
 * @param graph  the filter graph
 * @param target the filter(s) to which the command should be sent
 *               "all" sends to all filters
 *               otherwise it can be a filter or filter instance name
 *               which will send the command to all matching filters.
 * @param cmd    the command to send, for handling simplicity all commands must be alphanumeric only
 * @param arg    the argument for the command
 * @param res    a buffer with size res_size where the filter(s) can return a response.
 *
 * @returns >=0 on success otherwise an error code.
 *              AVERROR(ENOSYS) on unsupported commands
 *)
function avfilter_graph_send_command(graph: PAVFilterGraph; const target, cmd, arg: PAnsiChar; res: PAnsiChar; res_len, flags: Integer): Integer; cdecl; external AVFILTER_LIBNAME name _PU + 'avfilter_graph_send_command';

(**
 * Queue a command for one or more filter instances.
 *
 * @param graph  the filter graph
 * @param target the filter(s) to which the command should be sent
 *               "all" sends to all filters
 *               otherwise it can be a filter or filter instance name
 *               which will send the command to all matching filters.
 * @param cmd    the command to sent, for handling simplicity all commands must be alphanumeric only
 * @param arg    the argument for the command
 * @param ts     time at which the command should be sent to the filter
 *
 * @note As this executes commands after this function returns, no return code
 *       from the filter is provided, also AVFILTER_CMD_FLAG_ONE is not supported.
 *)
function avfilter_graph_queue_command(graph: PAVFilterGraph; const target, cmd, arg: PAnsiChar; flags: Integer; ts: Double): Integer; cdecl; external AVFILTER_LIBNAME name _PU + 'avfilter_graph_queue_command';


(**
 * Dump a graph into a human-readable string representation.
 *
 * @param graph    the graph to dump
 * @param options  formatting options; currently ignored
 * @return  a string, or NULL in case of memory allocation failure;
 *          the string must be freed using av_free
 *)
function avfilter_graph_dump(graph: PAVFilterGraph; const options: PAnsiChar): PAnsiChar; cdecl; external AVFILTER_LIBNAME name _PU + 'avfilter_graph_dump';

(**
 * Request a frame on the oldest sink link.
 *
 * If the request returns AVERROR_EOF, try the next.
 *
 * Note that this function is not meant to be the sole scheduling mechanism
 * of a filtergraph, only a convenience function to help drain a filtergraph
 * in a balanced way under normal circumstances.
 *
 * Also note that AVERROR_EOF does not mean that frames did not arrive on
 * some of the sinks during the process.
 * When there are multiple sink links, in case the requested link
 * returns an EOF, this may cause a filter to flush pending frames
 * which are sent to another sink link, although unrequested.
 *
 * @return  the return value of ff_request_frame(),
 *          or AVERROR_EOF if all links returned AVERROR_EOF
 *)
function avfilter_graph_request_oldest(graph: PAVFilterGraph): Integer; cdecl; external AVFILTER_LIBNAME name _PU + 'avfilter_graph_request_oldest';

(**
 * @}
 *)

implementation

end.
