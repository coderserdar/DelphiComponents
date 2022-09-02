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
 * The Original Code is TurboPower B-Tree Filer
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1996-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Based in part on code written by Ralf Nagel
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

function IsamCheckLockError : Boolean;
begin
  IsamCheckLockError := False;
  if IsamDriveNotReadyError or (IOResult = 152) then begin
    IsamDriveNotReadyError := False;
    IsamLockError := False;
    IsamOK := False;
    IsamError := 10110;
    IsamCheckLockError := True;
    Exit;
  end;
  if IsamLockError then begin
    IsamLockError := False;
    IsamOK := False;
    IsamError := 10140;
    IsamCheckLockError := True;
  end;
end;


procedure IsamClearOK;
begin
  IsamOK := True;
  IsamError := 0;
  IsamLockError := False;
  IsamDriveNotReadyError := False;
  IsamDOSError := 0;
  IsamDOSFunc := 0;
end;


procedure SaveErrorStat(var ErrSt : ErrStatSaveRec);
begin
  with ErrSt do begin
    OK := IsamOK;
    Error := IsamError;
    DError := IsamDOSError;
    DFunc := IsamDOSFunc;
  end;
end;


procedure RestoreErrorStat(ErrSt : ErrStatSaveRec);
begin
  with ErrSt do begin
    IsamOK := OK;
    IsamError := Error;
    IsamDOSError := DError;
    IsamDOSFunc := DFunc;
  end;
end;


function IsamSetDosRetry(NrOfRetries, WaitTime : Integer) : Boolean;
{$IFDEF Win32}
  begin
    {do nothing}
    Result := true;
  end;
{$ELSE}
var
  IRR : GenRegisters;
begin
  DefaultRegisters(IRR);
  with IRR do begin
    AX := $440B;
    BX := NrOfRetries;
    CX := WaitTime;
    DX := NrOfRetries;
    if IsamDOSError = 0 then
      IsamDOSFunc := AX;
    CallMsDos(IRR);
    if odd(Flags) and (IsamDOSError = 0) then
      IsamDOSError := AX;
    IsamSetDosRetry := not odd(Flags);
  end;
end;
{$ENDIF}


function IsamGetFileName(var F : IsamFile) : IsamFileName;
var
  FName : IsamFileName;
begin
  FName [0] := Char (Pred (SizeOf (IsamFileName)));
  Move(F.Name, FName [1], Byte (FName [0]));
  FName [0] := Char (Pred (Pos (#0, FName)));
  IsamGetFileName := FName;
end;


function IsamExistsIsamFile(var IsamF : IsamFile) : Boolean;
{$IFDEF Win32}
var
  FA : DWORD;                                                  {!!.54}
begin
  if (IsamDOSError = 0) then
    IsamDOSFunc := $4300;
  FA := Windows.GetFileAttributes(IsamF.Name);
  if (FA = $FFFFFFFF) then begin
    Result := false;
    if (IsamDOSError = 0) then
      IsamDOSError := GetLastError;
  end
  else
    Result := true;
end;
{$ELSE}
var
  IRR : GenRegisters;
begin
  DefaultRegisters(IRR);
  with IRR, IsamF do begin
    AX := $4300;           {get file attribute}
    DS := Seg (Name);
    DX := Ofs (Name);
    if IsamDOSError = 0 then
      IsamDOSFunc := AX;
    CallMsDos(IRR);
    IsamExistsIsamFile := False;
    if IsamCheckLockError then
      Exit;
    if odd(Flags) and (IsamDOSError = 0) then
      IsamDOSError := AX;
    IsamExistsIsamFile := not odd(Flags);
  end;
end;
{$ENDIF}


function IsamExists(Name : IsamFileName) : Boolean;
var
  IsamF : IsamFile;
begin
  IsamAssign(IsamF, Name);
  IsamExists := IsamExistsIsamFile(IsamF);
end;


procedure IsamPutDummyBlock(var F : IsamFile; Ref, Len : LongInt);
                                                     {!!.52 rewritten}
const
  EmptyArrLen = 1024;
var
  EmptyArr  : Array [1..EmptyArrLen] of Char;
  BufPtr    : Pointer;
  GotMem    : boolean;
begin
  {$IFDEF Win32}
  if (Len > EmptyArrLen) then
  {$ELSE}
  if (Len > EmptyArrLen) and (Len < $FFF0) then
  {$ENDIF}
    GotMem := IsamGetMem(BufPtr, Len)
  else
    GotMem := false;
  if GotMem then begin
    FillChar(BufPtr^, Len, 0);
    IsamPutBlock(F, Ref, Len, BufPtr^);
    FreeMem(BufPtr, Len);
  end
  else begin
    FillChar(EmptyArr, sizeof(EmptyArr), 0);
    IsamLongSeek(F, Ref);
    if not IsamOK then
      Exit;
    while (Len > EmptyArrLen) do begin
      IsamBlockWrite(F, EmptyArr, sizeof(EmptyArr));
      if not IsamOK then
        Exit;
      Len := Len - sizeof(EmptyArr);
    end;
    IsamBlockWrite(F, EmptyArr, Word(Len));
  end;
end;


procedure IsamExtractFileNames(    FNameComp : IsamFileBlockName;
                                 var FNameD,
                                     FNameI    : IsamFileBlockName);
var
  SP : Word;
  {------}
  procedure Trim(var S : IsamFileBlockName);
  var
    P : Word;
  begin
    while True do begin
      P := Pos(' ', S);
      if P = 0 then
        Exit;
      Delete(S, P, 1);
    end;
  end;
  {------}
begin
  {Trim(FNameComp);}                                           {!!.53}
  SP := Pos(';', FNameComp);
  if SP = 0 then begin
    FNameD := FNameComp;
    FNameI := FNameComp;
  end
  else begin
    FNameD := Copy(FNameComp, 1, Pred (SP));
    FNameI := Copy(FNameComp, Succ (SP), Length (FNameComp) - SP);
  end;
end;


procedure IsamCopyFile(Source,
                       Dest                  : IsamFileBlockName;
                       DeleteSourceAfterCopy : Boolean);
                                                     {!!.52 rewritten}
{$IFDEF Win32}
var
  SourceZ : array [0..MAX_PATH] of char;
  DestZ   : array [0..MAX_PATH] of char;
  SourceF : IsamFile;
begin
  StrPCopy(SourceZ, Source);
  StrPCopy(DestZ, Dest);
  if not Windows.CopyFile(SourceZ, DestZ, True) then begin
    IsamOK := false;
    IsamError := 10075;
    Exit;
  end;
  if DeleteSourceAfterCopy then begin
    IsamAssign(SourceF, Source);
    IsamDelete(SourceF);
  end;
end;
{$ELSE}
var
  SourceF,
  DestF     : IsamFile;
  BufPtr    : Pointer;
  BufSize,
  BytesRead : Longint;
  {----}
  procedure UnDo(Err : Integer; Level : integer);
    begin
      if Level >= 3 then
        FreeMem(BufPtr, BufSize);
      if Level >= 2 then
        IsamClose(DestF);
      if Level >= 1 then
        IsamClose(SourceF);
      IsamOK := False;
      IsamError := Err;
    end;
  {----}
  procedure GetCopyBuffer(var BufPtr : pointer; var Size : longint);
  const
    MinBufSize = 1024;
    MaxBufSize = 32768;
  begin
    Size := MaxBufSize;
    repeat
      if IsamGetMem(BufPtr, Size) then
        Exit;
      dec(Size, MinBufSize);
    until (Size < MinBufSize);
  end;
  {----}
begin
  IsamAssign(SourceF, Source);
  IsamAssign(DestF, Dest);
  IsamReset(SourceF, False, True);
  if not IsamOK then
    Exit;
  IsamRewrite(DestF);
  if not IsamOK then begin
    UnDo(IsamError, 1);
    Exit;
  end;
  GetCopyBuffer(BufPtr, BufSize);
  if (BufPtr = nil) then begin
    UnDo(10420, 2);
    Exit;
  end;
  repeat
    IsamBlockReadRetLen(SourceF, BufPtr^, BufSize, BytesRead);
    if not IsamOK then begin
      UnDo(IsamError, 3);
      Exit;
    end;
    if (BytesRead <> 0) then begin
      IsamBlockWrite(DestF, BufPtr^, BytesRead);
      if not IsamOK then begin
        UnDo(IsamError, 3);
        Exit;
      end;
    end;
  until (BytesRead = 0);
  FreeMem(BufPtr, BufSize);
  IsamClose(DestF);
  if not IsamOK then begin
    UnDo(IsamError, 1);
    Exit;
  end;
  IsamClose(SourceF);
  if DeleteSourceAfterCopy then IsamDelete(SourceF);
end;
{$ENDIF}


function IsamForceExtension(Name, Ext : IsamFileName) : IsamFileName;
var
  DotPos : Word;
  {------}
  function HasExtension(Name : IsamFileBlockName; var DotPos : Word)
      : Boolean;
  var
    W : Word;
  begin
    DotPos := 0;
    For W := Length (Name) DownTo 1 do begin
      if (Name [W] = '.') and (DotPos = 0) then begin
        DotPos := W;
      end;
    end;
    HasExtension := (DotPos > 0) and
        (Pos ('\', Copy (Name, Succ (DotPos), 255)) = 0);
  end;
  {------}
begin
  if HasExtension(Name, DotPos) then begin
    IsamForceExtension := Copy(Name, 1, DotPos) + Ext;
  end
  else begin
    IsamForceExtension := Name + '.' + Ext;
  end;
end;


{$IFDEF Heap6}
function RoundToGranul(Value : Word) : Word;
  {-Rounds Value up to the next multiple of 8}
begin
  RoundToGranul := (Value + 7) and $FFF8;
end;
{$ENDIF}


