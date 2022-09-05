(*
 * This unit is  free software; you can redistribute it  and modify it
 * under the terms of the GNU Library General Public License as published
 * by the Free Software Foundation; either version 2  of the license or
 * (at your option) any later version.
 *
 * Author of CPP header file : Olivier Lapicque <olivierl@jps.net>
 * Author of Delphi conversion : Dean Ellis <Dean_Ellis@sillex.freeserve.co.uk>
 *
 * NOTE : The Origonal C++ Class declarations anc constants have been left in place
 *
*)
unit MpSndSys;

interface

uses Windows,Classes;

const
//#ifndef MPP_SND_SYS_H
//#define MPP_SND_SYS_H

//#ifndef MPPDLLEXPORT
//#define MPPDLLEXPORT
//#endif

//#define MPPAPI			__stdcall
//#define MPPCDECL		__cdecl
//#define MPPAPI_VERSION	0x0140

//#define MPPAPI_VERSION	0x0140

   MPPAPI_VERSION	= $0141;

// Version-specific functions
//#define MPPVERSION_HAS_NAVIGATION	0x0139
//#define MPPVERSION_HAS_SONGTIME 0x0141

   MPPVERSION_HAS_NAVIGATION =	$0139;
   MPPVERSION_HAS_SONGTIME	  =	$0141;

// Error codes
//typedef long MPPERR;
//
//enum {
//	MPPERR_NOERROR=0,
//	MPPERR_FAILED,
//	MPPERR_INVALIDPARAM,
//};

//enum {
// MPPSONG_INVALID=0,
//	MPPSONG_MOD,
//	MPPSONG_S3M,
//	MPPSONG_XM,
//	MPPSONG_IT,
//	MPPSONG_MDL,
//	MPPSONG_UNKNOWN=100
//};
   MPPERR_NOERROR       = 0;
	MPPERR_FAILED        = 1;
	MPPERR_INVALIDPARAM  = 2;

	MPPSONG_INVALID   = 0;
	MPPSONG_MOD       = 1;
	MPPSONG_S3M       = 2;
	MPPSONG_XM        = 3;
	MPPSONG_IT        = 4;
	MPPSONG_MDL       = 5;
	MPPSONG_UNKNOWN   = 100;

// Mixer Options MPPMIX_XXXX
//#define MPPMIX_NORESAMPLING		0x01	// Faster, but crappy quality
//#define MPPMIX_BASSEXPANSION	0x02	// Bass Expansion
//#define MPPMIX_SURROUND			0x04	// Surround Encoding
//#define MPPMIX_REVERB			0x08	// Reverb
//#define MPPMIX_LOOP				0x10	// Loop the song (backward jumps will be enabled)
//// v1.40+ flags
//#define MPPMIX_HIGHQUALITY		0x20	// HQ mixing (better resampling, dithering enabled)
//#define MPPMIX_GAINCONTROL		0x40	// Automatic Gain Control
//#define MPPMIX_NOISEREDUCTION	0x80	// Noise reduction (-6dB 22kHz lowpass filter)

   MPPMIX_NORESAMPLING	  = $01;	// Faster, but crappy quality
   MPPMIX_BASSEXPANSION	  = $02;	// Bass Expansion
   MPPMIX_SURROUND		  = $04;	// Surround Encoding
   MPPMIX_REVERB			  = $08;	// Reverb
   MPPMIX_LOOP				  = $10;	// Loop the song (backward jumps will be enabled)
   // v1.40+ flags
   MPPMIX_HIGHQUALITY	  = $20;	// HQ mixing (better resampling, dithering enabled)
   MPPMIX_GAINCONTROL	  = $40;	// Automatic Gain Control
   MPPMIX_NOISEREDUCTION  = $80;	// Noise reduction (-6dB 22kHz lowpass filter)

type
  //==========================
  //class MPPDLLEXPORT IModMixer
  //==========================
(*  {
public:
	// Reference count: the initial reference count is 1, so you shouldn't have to call AddRef()
	virtual unsigned long MPPAPI AddRef() = 0;
	virtual unsigned long MPPAPI Release() = 0;

	// API Version: you should refuse to continue if the returned value is smaller than MPPAPI_VERSION
	virtual unsigned long MPPAPI GetVersion() = 0;

	// Basic I/O Functions
	virtual MPPERR MPPAPI LoadSong(const void *pmemfile, long len) = 0;
						// Songs are always loaded from memory. The pointer pmemfile can be destroyed
						// after the call to LoadSong. You can use memory-mapped files or a pointer returned
						// by LockResource(), or whatever file in memory.
	virtual MPPERR MPPAPI FreeSong() = 0;			// Free the memory used by the song

	// Audio Rendering Functions: example: (44100, 2, 16) for 44.1kHz, stereo, 16-bit
	virtual MPPERR MPPAPI SetWaveFormat(long samplespersec, long channels, long bitspersample) = 0;
	// return # of SAMPLES that have been written to the buffer, 0 if end has been reached
	// Note: protect calls to Render() and SetMixerOptions() by a critical section, if they
	//       are used in different threads.
	virtual long MPPAPI Render(void *pbuffer, unsigned long bufsize) = 0;

	// Player Configuration: set of MPPMIX_XXXX
	virtual MPPERR MPPAPI SetMixerOptions(unsigned long dwOptions) = 0;
	virtual unsigned long MPPAPI GetMixerOptions() = 0;

	// Song Information
	virtual long MPPAPI GetSongType() = 0;				// Return MPPSONG_XXXX
	virtual void MPPAPI GetSongName(char *pszbuf) = 0;	// pszbuf must be at least 32-bytes

	//////////////////////////////////////////////////////////////////////////////////////
	// v1.39+: Navigation functions
	// The order is the position in the pattern sequence list: this allows you to
	// jump to a specific part of a song. It can be useful in a game with a song that
	// uses pattern position jump effects (or pattern loops).
	// These function will not be available if GetVersion() returns a value smaller than 0x139 (MPPVERSION_HAS_NAVIGATION)
	virtual unsigned long MPPAPI GetNumOrders() = 0;
	virtual unsigned long MPPAPI GetCurrentOrder() = 0;
	virtual MPPERR MPPAPI SetCurrentOrder(unsigned long neworder) = 0;

};
*)
   MppError = longint;

   PModMixer = ^IModMixer;
   IModMixer = class
      public
         function AddRef:longint; virtual; stdcall; abstract;
         function Release:longint; virtual; stdcall; abstract;
         function GetVersion:longint; virtual; stdcall; abstract;
         function LoadSong(const MemFile:pointer;Length:longint):MppError;virtual; stdcall; abstract;
         function FreeSong:MppError;virtual; stdcall; abstract;
         function SetWaveFormat(SamplesPerSec,channels,bitsPerSample:longint):MppError;virtual; stdcall; abstract;
         function Render(Buffer:pointer;BufferSize:longint):MppError;virtual; stdcall; abstract;
         function SetMixerOptions(dwOptions:longint):MppError;virtual; stdcall; abstract;
         function GetMixerOptions:longint;virtual; stdcall; abstract;
         function GetSongType:longint;virtual; stdcall; abstract;
         procedure GetSongName(Buffer:PChar);virtual; stdcall; abstract;
         function GetNumOrders:longint;virtual; stdcall; abstract;
         function SetCurrentOrder(newOrder:longint):MppError;virtual; stdcall; abstract;
         function GetSongLength:longint; virtual; stdcall; abstract;
   end;

var
   MppSdkLibLoaded:Boolean;
   ModMixer:IModMixer;

implementation

const MppSdkLibrary = 'mppsdk.dll';

var MppSdkLibHandle:THandle;

//#define MPP_GETMODAPIFUNCNAME				"MPP_GetModAPI"
//typedef MPPERR (MPPCDECL * MPP_GETMODAPIFUNC)(IModMixer **);
    GetModAPI : function(MODMixer:PModMixer):MppError;cdecl;

(*
 *
 * To get a pointer to the IModMixer interface, you can use the following functions:
 *
 *   HMODULE hMPPSDK = (HMODULE)LoadLibrary("mppsdk.dll");
 *   MPP_GETMODAPIFUNC pFunc = (MPP_GETMODAPIFUNC)GetProcAddress(hMPPSDK, MPP_GETMODAPIFUNCNAME);
 *   IModMixer *pMPPAPI;
 *   pFunc(&pMPPAPI);
 *   ...
 * You can then access the MPP SDK API through the IModMixer interface
 *
 * When you are done:
 *
 *   pMPPAPI->Release();
 *   FreeLibrary(hMPPSDK);
 *
 *)

//#endif // MPP_SND_SYS_H


{----------------------------------------------------------------------------}
{InitLibrary - will try to load the Mppsdk.dll and get the ModMixer object}
{----------------------------------------------------------------------------}
function InitLibrary:Boolean;
begin
   Result := False;

   MppSdkLibHandle := LoadLibrary(MppSdkLibrary);

   if MppSdkLibHandle = 0 then
   begin
      Exit;
   end;

   try
      GetModAPI := GetProcAddress(MppSdkLibHandle,'MPP_GetModAPI');
      GetModAPI(@ModMixer);
      Result := True;
   except
      Result := False;
   end;
end;
{----------------------------------------------------------------------------}
{UnLoadLibrary - Will unload the Mppsdk.dll if it was loaded}
{----------------------------------------------------------------------------}
procedure UnLoadLibrary;
begin
  if MppSdkLibLoaded then
  begin
     FreeLibrary(MppSdkLibHandle);
  end;
end;

{----------------------------------------------------------------------------}
{Automatic initialization and finalization - comment out if you want to
 do this manually}
{----------------------------------------------------------------------------}
initialization
  MppSdkLibLoaded:=InitLibrary;
finalization
  UnloadLibrary;
end.
