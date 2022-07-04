{*********************************************************}
{* FlashFiler: Replacement Memory Manger used in the     *}
{* FF1 to FF2 application. This is used to prevent the   *}
{* problems associated with passing string types between *}
{* an application. We decide not to use ShareMem because *}
{* of the size of its required DLL.                      *}
{*********************************************************}

(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TurboPower FlashFiler
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1996-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

unit FFMemMgr;

interface

function FFMMGetMem(Size : Integer) : Pointer;
  { Allocates memory using the DLL's memory manager. }
function FFMMFreeMem(P : Pointer) : integer;
  { Deallocates memory using the DLL's memory manger.}
function FFMMReallocMem(P : Pointer; Size : integer) : Pointer;
  { Reallocates memory using the DLL's memory manager.}
function LoadFF1DLL : boolean;
  { Dynamically loads the FF1 DLL.}

implementation

uses
  Windows, SysUtils;

type
  { These are exported functions from the FlashFiler 1 DLL. These
    functions are used to let the DLL's memory manager manage the
    memory for the conversion application also.  We are doing this
    to prevent the inherent problems caused by passing strings
    between an application and a DLL. This also prevents a requirement
    on the ShareMem DLL.}
  TFF1GetMemFunc = procedure (var P : pointer; aSize : integer);
  TFF1FreeMemFunc = procedure (P : pointer);
  TFF1ReallocMemFunc = procedure (var P : pointer; aSize : integer);

var
  FOldMemMgr : TMemoryManager;
  FNewMemMgr : TMemoryManager;
  FDLLHandle : THandle;

  { Functions mapped to FF1 DLL}
  FF1GetMem     : TFF1GetMemFunc;
  FF1FreeMem    : TFF1FreeMemFunc;
  FF1ReallocMem : TFF1ReallocMemFunc;

{====================================================================}
function FFMMGetMem(Size : Integer) : Pointer;
begin
  FF1GetMem(Result, Size);
end;
{--------}
function FFMMFreeMem(P : Pointer) : integer;
begin
  FF1FreeMem(P);
  Result := 0;
end;
{--------}
function FFMMReallocMem(P : Pointer; Size : integer) : Pointer;
begin
  FF1ReallocMem(P, Size);
  Result := P;
end;
{--------}
function  LoadFF1DLL : boolean;
var
  Msg,Msg2   : string;
  ErrorMode  : word;
begin
  { Use setErrorMode to prohibit the Windows error dialog that appears
    if the DLL is not found.  Load the DLL dynamically. }
  ErrorMode := SetErrorMode(SEM_NoOpenFileErrorBox);
  FDllHandle := LoadLibrary('FF1Intfc.DLL');
  SetErrorMode(ErrorMode);
  if FDllHandle = 0 then begin
     Msg := 'Unable to load FF1Intfc.DLL. ';
     case GetLastError of
        0 : Msg2 := 'System out of memory, executable corrupt, ' +
                    'or relocations invalid.';
        2 : Msg2 := 'File not found.';
        3 : Msg2 := 'Path not found.';
        8 : Msg2 := 'There is insufficient memory to load the DLL.';
       10 : Msg2 := 'The Windows version of the DLL is incorrect.';
       else
         Msg2 := '';
     end;  { case }
       raise Exception.Create(Msg + Msg2 + ' Unable to run conversion.');
     Result := False;
  end
  else begin
    @FF1GetMem := GetProcAddress(FDLLHandle, 'FF1GetMem');
    @FF1FreeMem := GetProcAddress(FDLLHandle, 'FF1FreeMem');
    @FF1ReallocMem := GetProcAddress(FDLLHandle, 'FF1ReallocMem');
    Result := True;
  end;
end;
{--------}
procedure InitializeUnit;
begin
  {setup our heap manager}
  FNewMemMgr.GetMem := FFMMGetMem;
  FNewMemMgr.FreeMem := FFMMFreeMem;
  FNewMemMgr.ReallocMem := FFMMReallocMem;

  {load FF1 DLL}
  try
    if LoadFF1DLL then begin
      {get the original manager, replace with ours}
      GetMemoryManager(FOldMemMgr);
      SetMemoryManager(FNewMemMgr);
    end;
  except
    on E: Exception do begin
      MessageBox( 0, PChar(E.message),
                  'Critical Error!',
                  MB_ICONSTOP + MB_OK);
      raise; 
    end;
  end;
end;
{--------}
procedure FinalizeUnit;
begin
  {restore the original manager}
  SetMemoryManager(FOldMemMgr);

  {unload the DLL}
  if FDllHandle <> 0 then
    FreeLibrary(FDllHandle);
end;
{====================================================================}
initialization
  InitializeUnit;
{--------}
finalization
  FinalizeUnit;
{====================================================================}
end.
