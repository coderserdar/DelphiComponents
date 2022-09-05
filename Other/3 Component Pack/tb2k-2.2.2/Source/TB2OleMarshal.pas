unit TB2OleMarshal;

{
  Toolbar2000
  Copyright (C) 1998-2008 by Jordan Russell
  All rights reserved.

  The contents of this file are subject to the "Toolbar2000 License"; you may
  not use or distribute this file except in compliance with the
  "Toolbar2000 License". A copy of the "Toolbar2000 License" may be found in
  TB2k-LICENSE.txt or at:
    http://www.jrsoftware.org/files/tb2k/TB2k-LICENSE.txt

  Alternatively, the contents of this file may be used under the terms of the
  GNU General Public License (the "GPL"), in which case the provisions of the
  GPL are applicable instead of those in the "Toolbar2000 License". A copy of
  the GPL may be found in GPL-LICENSE.txt or at:
    http://www.jrsoftware.org/files/tb2k/GPL-LICENSE.txt
  If you wish to allow use of your version of this file only under the terms of
  the GPL and not to allow others to use your version of this file under the
  "Toolbar2000 License", indicate your decision by deleting the provisions
  above and replace them with the notice and other provisions required by the
  GPL. If you do not delete the provisions above, a recipient may use your
  version of this file under either the "Toolbar2000 License" or the GPL.

  $jrsoftware: tb2k/Source/TB2OleMarshal.pas,v 1.4 2008/09/17 18:04:09 jr Exp $

  This unit implements the TTBStandardOleMarshalObject class, an exact clone of
  .NET Framework 2.0's StandardOleMarshalObject class, which isn't available
  on the .NET Framework 1.1-based Delphi 2006.
  On Delphi 2007, I had planned to switch to StandardOleMarshalObject, but it
  turns out there's a bug that causes it raise AV's on x64 & IA-64 (seen as
  E_POINTER on the client side). Coincidentally, TTBStandardOleMarshalObject
  does not suffer from this bug (even though it was intended to be an exact
  clone!).

  The class "replaces the standard common language runtime (CLR) free-threaded
  marshaler with the standard OLE STA marshaler." It "prevents calls made into
  a hosting object by OLE from entering threads other than the UI thread."
  For more information, see:
  http://msdn2.microsoft.com/system.runtime.interopservices.standardolemarshalobject.aspx
}

interface

{$I TB2Ver.inc}

uses
  System.Runtime.InteropServices, Windows;

type
  { Our declaration for IMarshal }
  [ComImport,
  GuidAttribute('00000003-0000-0000-C000-000000000046'),
  InterfaceTypeAttribute(ComInterfaceType.InterfaceIsIUnknown)]
  ITBMarshal = interface
    [PreserveSig]
    function GetUnmarshalClass([MarshalAs(UnmanagedType.LPStruct)] riid: Guid;
      pv: IntPtr; dwDestContext: Longint; pvDestContext: IntPtr;
      mshlflags: Longint; out pCid: Guid): HRESULT;
    [PreserveSig]
    function GetMarshalSizeMax([MarshalAs(UnmanagedType.LPStruct)] riid: Guid;
      pv: IntPtr; dwDestContext: Longint; pvDestContext: IntPtr;
      mshlflags: Longint; out pSize: Longint): HRESULT;
    [PreserveSig]
    function MarshalInterface([in, MarshalAs(UnmanagedType.Interface)] pStm: TObject;
      [MarshalAs(UnmanagedType.LPStruct)] riid: Guid; pv: IntPtr;
      dwDestContext: Longint; pvDestContext: IntPtr; mshlflags: Longint): HRESULT;
    [PreserveSig]
    function UnmarshalInterface([in, MarshalAs(UnmanagedType.Interface)] pStm: TObject;
      [MarshalAs(UnmanagedType.LPStruct)] riid: Guid; out ppv: IntPtr): HRESULT;
    [PreserveSig]
    function ReleaseMarshalData([in, MarshalAs(UnmanagedType.Interface)] pStm: TObject): HRESULT;
    [PreserveSig]
    function DisconnectObject(dwReserved: Longint): HRESULT;
  end;

  TTBStandardOleMarshalObject = class(System.MarshalByRefObject, ITBMarshal)
  private
    function GetStdMarshaller(const riid: Guid; const dwDestContext: Longint;
      const mshlflags: Longint): IntPtr;
    { IMarshal }
    function GetUnmarshalClass(riid: Guid; pv: IntPtr; dwDestContext: Longint;
      pvDestContext: IntPtr; mshlflags: Longint; out pCid: Guid): HRESULT;
    function GetMarshalSizeMax(riid: Guid; pv: IntPtr; dwDestContext: Longint;
      pvDestContext: IntPtr; mshlflags: Longint; out pSize: Longint): HRESULT;
    function MarshalInterface(pStm: TObject; riid: Guid; pv: IntPtr;
      dwDestContext: Longint; pvDestContext: IntPtr; mshlflags: Longint): HRESULT;
    function UnmarshalInterface(pStm: TObject; riid: Guid; out ppv: IntPtr): HRESULT;
    function ReleaseMarshalData(pStm: TObject): HRESULT;
    function DisconnectObject(dwReserved: Longint): HRESULT;
  end;

implementation

{ Note: According to http://blogs.msdn.com/cbrumme/archive/2003/04/15/51335.aspx
  the Marshal.ReleaseComObject(pStm) calls are needed to work around a "quirk
  of OLE32 on some versions of the operating system". }

uses
  System.Security;

const
  ole32 = 'ole32.dll';

[SuppressUnmanagedCodeSecurity, DllImport(ole32, CharSet = CharSet.Unicode, EntryPoint = 'CoGetMarshalSizeMax')]
function _CoGetMarshalSizeMax(out pulSize: Longint;
  [in, MarshalAs(UnmanagedType.LPStruct)] riid: Guid; pUnk: IntPtr;
  dwDestContext: Longint; pvDestContext: IntPtr;
  mshlflags: Longint): HRESULT; external;
[SuppressUnmanagedCodeSecurity, DllImport(ole32, CharSet = CharSet.Unicode, EntryPoint = 'CoGetStandardMarshal')]
function _CoGetStandardMarshal([in, MarshalAs(UnmanagedType.LPStruct)] iid: Guid;
  pUnk: IntPtr; dwDestContext: Longint; pvDestContext: IntPtr;
  mshlflags: Longint; out ppMarshal: IntPtr): HRESULT; external;
[SuppressUnmanagedCodeSecurity, DllImport(ole32, CharSet = CharSet.Unicode, EntryPoint = 'CoMarshalInterface')]
function _CoMarshalInterface([in, MarshalAs(UnmanagedType.Interface)] pStm: TObject;
  [in, MarshalAs(UnmanagedType.LPStruct)] riid: Guid; pUnk: IntPtr;
  dwDestContext: Longint; pvDestContext: IntPtr;
  mshlflags: Longint): HRESULT; external;

function TTBStandardOleMarshalObject.GetStdMarshaller(const riid: Guid;
  const dwDestContext: Longint; const mshlflags: Longint): IntPtr;
var
  V_1: IntPtr;
begin
  Result := nil;
  V_1 := Marshal.GetIUnknownForObject(Self);
  if V_1 <> nil then begin
    try
      if _CoGetStandardMarshal(riid, V_1, dwDestContext, nil, mshlflags, Result) = S_OK then
        Exit;
    finally
      Marshal.Release(V_1);
    end;
  end;
  { Note: Localizing this message isn't necessary because a user will never
    see it; the .NET runtime will catch it and translate it into a
    COR_E_EXCEPTION HRESULT. }
  raise InvalidOperationException.Create('TTBStandardOleMarshalObject.GetStdMarshaller failed');
end;

function TTBStandardOleMarshalObject.GetUnmarshalClass(riid: Guid; pv: IntPtr;
  dwDestContext: Longint; pvDestContext: IntPtr; mshlflags: Longint;
  out pCid: Guid): HRESULT;
begin
  { StandardOleMarshalObject does "pCid := TypeOf(IStdMarshal).GUID" here, but
    we haven't declared IStdMarshal anywhere, so create a fresh Guid }
  pCid := Guid.Create('00000017-0000-0000-C000-000000000046');  { CLSID_StdMarshal }
  Result := S_OK;
end;

function TTBStandardOleMarshalObject.GetMarshalSizeMax(riid: Guid; pv: IntPtr;
  dwDestContext: Longint; pvDestContext: IntPtr; mshlflags: Longint;
  out pSize: Longint): HRESULT;
var
  V_0: IntPtr;
begin
  V_0 := GetStdMarshaller(riid, dwDestContext, mshlflags);
  try
    Result := _CoGetMarshalSizeMax(pSize, riid, V_0, dwDestContext, pvDestContext, mshlflags);
  finally
    Marshal.Release(V_0);
  end;
end;

function TTBStandardOleMarshalObject.MarshalInterface(pStm: TObject; riid: Guid;
  pv: IntPtr; dwDestContext: Longint; pvDestContext: IntPtr;
  mshlflags: Longint): HRESULT;
var
  V_0: IntPtr;
begin
  V_0 := GetStdMarshaller(riid, dwDestContext, mshlflags);
  try
    Result := _CoMarshalInterface(pStm, riid, V_0, dwDestContext, pvDestContext, mshlflags);
  finally
    Marshal.Release(V_0);
    if pStm <> nil then
      Marshal.ReleaseComObject(pStm);
  end;
end;

function TTBStandardOleMarshalObject.UnmarshalInterface(pStm: TObject;
  riid: Guid; out ppv: IntPtr): HRESULT;
begin
  ppv := nil;
  if pStm <> nil then
    Marshal.ReleaseComObject(pStm);
  Result := E_NOTIMPL;
end;

function TTBStandardOleMarshalObject.ReleaseMarshalData(pStm: TObject): HRESULT;
begin
  if pStm <> nil then
    Marshal.ReleaseComObject(pStm);
  Result := E_NOTIMPL;
end;

function TTBStandardOleMarshalObject.DisconnectObject(dwReserved: Longint): HRESULT;
begin
  Result := E_NOTIMPL;
end;

end.
