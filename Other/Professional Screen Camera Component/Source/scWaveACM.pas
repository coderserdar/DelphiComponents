{------------------------------------------------------------------------------}
{                                                                              }
{  WaveACM - A subset of Microsoft Audio Compression Manager (ACM) API         }
{  by Kambiz R. Khojasteh                                                      }
{                                                                              }
{  kambiz@delphiarea.com                                                       }
{  http://www.delphiarea.com                                                   }
{                                                                              }
{------------------------------------------------------------------------------}
{ http://msdn2.microsoft.com/en-us/library/ms705349.aspx                       }
{------------------------------------------------------------------------------}

{$I DELPHIAREA.INC}

unit scWaveACM;

interface

uses
  Windows, MMSystem;

const
  // acmStreamConvert flags
  ACM_STREAMCONVERTF_BLOCKALIGN     = $00000004;
  ACM_STREAMCONVERTF_START          = $00000010;
  ACM_STREAMCONVERTF_END            = $00000020;

  // acmStreamOpen flags
  ACM_STREAMOPENF_QUERY             = $00000001;
  ACM_STREAMOPENF_ASYNC             = $00000002;
  ACM_STREAMOPENF_NONREALTIME       = $00000004;

  // acmStreamSize flags
  ACM_STREAMSIZEF_SOURCE            = $00000000;
  ACM_STREAMSIZEF_DESTINATION       = $00000001;

  // acmFormatSuggest flags
  ACM_FORMATSUGGESTF_WFORMATTAG     = $00010000;
  ACM_FORMATSUGGESTF_NCHANNELS      = $00020000;
  ACM_FORMATSUGGESTF_NSAMPLESPERSEC = $00040000;
  ACM_FORMATSUGGESTF_WBITSPERSAMPLE = $00080000;


type
  // ACM Driver Handle
  HACMDRIVER = DWORD;

  // ACM Stream Handle
  HACMSTREAM = DWORD;

  // ACM Stream Header
  PACMSTREAMHEADER = ^TACMSTREAMHEADER;
  TACMSTREAMHEADER = packed record
    cbStruct: DWORD;
    fdwStatus: DWORD;
    dwUser: DWORD;
    pbSrc: PBYTE;
    cbSrcLength: DWORD;
    cbSrcLengthUsed: DWORD;
    dwSrcUser: DWORD;
    pbDst: PBYTE;
    cbDstLength: DWORD;
    cbDstLengthUsed: DWORD;
    dwDstUser: DWORD;
    dwReservedDriver: array[0..9] of DWORD;
  end;

  // ACM Wave Filter
  PWAVEFILTER = ^TWAVEFILTER;
  TWAVEFILTER = packed record
    cbStruct: DWORD;
    dwFilterTag: DWORD;
    fdwFilter: DWORD;
    dwReserved: array[0..4] of DWORD;
  end;

function acmStreamOpen(var phas: HACMSTREAM; had: HACMDRIVER;
  pwfxSrc: PWAVEFORMATEX; pwfxDst: PWAVEFORMATEX; pwfltr: PWAVEFILTER;
  dwCallback: DWORD; dwInstance: DWORD; fdwOpen: DWORD): MMRESULT; stdcall;

function acmStreamClose(has: HACMSTREAM; fdwClose: DWORD): MMRESULT; stdcall;

function acmStreamPrepareHeader(has: HACMSTREAM; var pash: TACMSTREAMHEADER;
  fdwPrepare: DWORD): MMRESULT; stdcall;

function acmStreamUnprepareHeader(has: HACMSTREAM; var pash: TACMSTREAMHEADER;
  fdwUnprepare: DWORD): MMRESULT; stdcall;

function acmStreamConvert(has: HACMSTREAM; var pash: TACMSTREAMHEADER;
  fdwConvert: DWORD): MMRESULT; stdcall;

function acmStreamSize(has: HACMSTREAM; cbInput: DWORD;
  var pdwOutputBytes: DWORD; fdwSize: DWORD): MMRESULT; stdcall;

function acmFormatSuggest(had: HACMDRIVER; pwfxSrc: PWAVEFORMATEX;
  pwfxDst: PWAVEFORMATEX; cbwfxDst: DWORD; fdwSuggest: DWORD): MMRESULT; stdcall;


implementation

const
  msacm32 = 'msacm32.dll';

function acmStreamOpen; external msacm32;
function acmStreamClose; external msacm32;
function acmStreamPrepareHeader; external msacm32;
function acmStreamUnprepareHeader; external msacm32;
function acmStreamConvert; external msacm32;
function acmStreamSize; external msacm32;
function acmFormatSuggest; external msacm32;

end.
