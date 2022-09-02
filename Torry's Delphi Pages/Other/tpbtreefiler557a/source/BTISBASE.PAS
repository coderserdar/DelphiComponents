{********************************************************************}
{* BTIsBase.PAS - B-Tree Filer ISAMBASE as unit                     *}
{********************************************************************}

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
 * Contributor(s):
 *   Rob Roberts  robr@pcisys.net
 *
 * ***** END LICENSE BLOCK ***** *)

{$I BTDEFINE.INC}

unit BTIsBase;

interface

uses
  {$IFDEF DPMI}
  WinAPI,
  {$ENDIF}
  {$IFDEF Win32}                                               {!!.54}
  Windows,                                                     {!!.54}
  {$ENDIF}                                                     {!!.54}
  {$IFDEF Windows}
  WinTypes, WinProcs,
  {$ENDIF}
  {$IFDEF VER80}
  SysUtils,
  {$ENDIF}
  {$IFNDEF Win32}
  BaseSupp,
  {$ENDIF}
  {$IFDEF SupportVLM}                                          {!!.54}
  NWBase, NWFile,                                              {!!.54}
  {$ENDIF}                                                     {!!.54}
  BTBase,
  BTFileIO;

const
  IsamFileNameLen = 64;

type
  IsamFileName = string[IsamFileNameLen];
  IsamHandle = TbtfHandle;

  IsamFile = packed record
    Handle : TbtfHandle;
    Name   : array [0..IsamFileNameLen] of char;
    IsLocal: boolean;                                          {!!.54}
  end;

  NetSupportType = (NoNet, Novell, MsNet); {Supported networks}

var
  IsamOK       : boolean;        {status of last operation}
  IsamError    : TbtfErrorCode;  {error code of last operation}
  IsamDOSFunc  : word;           {DOS function of last operation, if DOS was called}
  IsamDOSError : TbtfErrorCode;  {DOS error code of last operation, if failed}
  IsamLockError: boolean;
  IsamDefNrOfWS : Word;                                        {!!.42}

{$I FILER.CFG} {configuration data}

function IsamGetMem(var P; Size : TbtfMemSize) : boolean;
  {-allocate memory, return true if successful}
procedure IsamDelay(MilliSecs : longint);
  {-sleep a given amount of time}
function IsamInitNet(ExpectedNet : NetSupportType) : Boolean;
  {-start up a network interface}
function IsamDoneNet : Boolean;
  {-close down a network interface}


procedure IsamAssign(var F : IsamFile; FName : IsamFileName);
  {-assigns FName to the file F}
procedure IsamBlockRead(var F : IsamFile; var Dest; Len  : longint);
  {-reads from the current position in a file}
procedure IsamBlockReadRetLen(var F         : IsamFile;
                              var Dest;
                                  Len       : longint;
                              var BytesRead : longint);
  {-reads from the current position in a file and returns the number
    of bytes read}
procedure IsamBlockWrite(var F : IsamFile; var Source; Len : longint);
  {-writes a block to the current position in a file}
procedure IsamClose(var F : IsamFile);
  {-closes the file F}
procedure IsamDelete(var F : IsamFile);
  {-Deletes the file F}
procedure IsamFlush(var F        : IsamFile;
                    var WithDUP  : Boolean;
                        NetUsed  : Boolean);
  {-Flushes the file F}
procedure IsamGetBlock(var F        : IsamFile;
                           Ref, Len : LongInt;
                       var Dest);
  {-reads from a given offset of a file}
function IsamGetPosition(var F : IsamFile) : LongInt;
  {-returns the current position of the file F}
procedure IsamLongSeek(var F : IsamFile; Ref : LongInt);
  {-seeks the position Ref in the file F}
procedure IsamLongSeekEOF(var F : IsamFile; var Len : DWORD);
  {-seeks the end of file position in the file F and returns its length}
procedure IsamPutBlock(var F        : IsamFile;
                           Ref, Len : LongInt;
                       var Source);
  {-writes a block to the given offset in a file}
procedure IsamRename(var F : IsamFile; FName : IsamFileName);
  {-Renames the file F to FName}
procedure IsamReset(var F : IsamFile; NetUsed, ReadOnly : Boolean);
  {-opens the file F in the specified mode}
procedure IsamRewrite(var F : IsamFile);
  {-creates the file F}
procedure IsamSetEOF(var F : IsamFile);
  {-truncates the file F at the current file position}


{---Exported Lock Manager routines---}
function btfLockMgrAcqLock(aHandle     : integer;
                           aOffset     : longint;
                           aLength     : longint;
                           aTimeOut    : longint;
                           aTotalDelay : longint) : Boolean;
  {-acquire a lock on a file}
function btfLockMgrRelLock(aHandle : integer;
                           aOffset : longint;
                           aLength : longint) : Boolean;
  {-release a lock on a file}

implementation

{$IFDEF Win32}
uses
  SysUtils;
{$ENDIF}

type
  IsamLockRecFunc = function (Start     : Longint;
                              Len       : LongInt;
                              Handle    : IsamHandle;
                              TimeOut   : word;
                              DelayTime : Word ) : Boolean;
  IsamUnlockRecFunc = function (Start  : LongInt;
                                Len    : LongInt;
                                Handle : IsamHandle) : Boolean;
  IsamExitNetFunc = function : Boolean;

var
  IsamLockRecord : IsamLockRecFunc;
  IsamUnlockRecord : IsamUnlockRecFunc;
  IsamExitNet : IsamExitNetFunc;
  {$IFNDEF Win32}
  ExitSave : pointer;
  {$ENDIF}

{===Helper routines==================================================}
{$IFNDEF UsingDelphi}
type                                                           {!!.52}
  THeapFunc = function (Size : word) : integer;                {!!.52}
var                                                            {!!.52}
  SaveHeapFunc : THeapFunc;                                    {!!.52}
function IsamHeapFunc(Size : word) : integer; far;
  begin
    if Size <> 0 then
      IsamHeapFunc := 1
    else                                                       {!!.52}
      IsamHeapFunc := SaveHeapFunc(Size);                      {!!.52}
  end;
{$ENDIF}
{--------}
procedure SetIsamDOSError;
begin
  if (IsamDOSError = 0) then begin
    IsamDOSFunc := btfDOSFunc;
    IsamDOSError := btfDOSError;
  end;
end;
{====================================================================}


{===Network support==================================================}
{$I ISNETSUP.INC}
{====================================================================}


{===Lock Manager=====================================================}
{$I BTLCKMGR.INC}
{====================================================================}


{===Primitive routines===============================================}
procedure IsamBlockReadRetLenPrim(var F         : IsamFile;
                                  var Dest;
                                      Len       : longint;
                                  var BytesRead : longint);
begin
  if not btfReadFile(F.Handle, Len, Dest, BytesRead) then begin
    IsamOK := false;
    case btfDOSError of
      6 : IsamError := 9904;
    else
      IsamError := 10140;
    end;{case}
    SetIsamDOSError;
    BytesRead := 0;
  end;
end;
{--------}
procedure IsamBlockReadPrim(var F : IsamFile; var Dest; Len : longint);
var
  BytesRead : longint;
begin
  IsamBlockReadRetLenPrim(F, Dest, Len, BytesRead);
  if IsamOK and (BytesRead <> Len) then begin
    IsamOK := false;
    IsamError := 10070;
  end;
end;
{--------}
procedure IsamGetBlockPrim(var F : IsamFile; Ref, Len : LongInt; var Dest);
begin
  IsamLongSeek(F, Ref);
  if not IsamOK then
    Exit;
  IsamBlockReadPrim(F, Dest, Len);
end;
{--------}
procedure IsamBlockWritePrim(var F     : IsamFile;
                             var Source;
                                 Len   : longint);
var
  BytesWritten : longint;
begin
  if not btfWriteFile(F.Handle, Len, Source, BytesWritten) then begin
    IsamOK := false;
    case btfDOSError of
      6 : IsamError := 9904;
    else
      IsamError := 10140;
    end;{case}
    SetIsamDOSError;
  end
  else begin
    if (BytesWritten <> Len) then begin
      IsamOK := false;
      IsamError := 10075;
    end;
  end;
end;
{--------}
procedure IsamPutBlockPrim(var F : IsamFile; Ref, Len : LongInt; var Source);
begin
  IsamLongSeek(F, Ref);
  if not IsamOK then Exit;
  if (Len = 0) then                                            {!!.53}
    IsamSetEOF(F)                                              {!!.53}
  else                                                         {!!.53}
    IsamBlockWritePrim(F, Source, Len);
end;
{====================================================================}


{===Non-file interfaced routines=====================================}
function IsamGetMem(var P; Size : TbtfMemSize) : boolean;
{$IFDEF Win32}
var
  Pt : pointer absolute P;
begin
  if (Size <= 0) then
    Pt := nil
  else
    try
      GetMem(Pt, Size);
    except
      on EOutOfMemory do
        Pt := nil;
    end;{try..except}
  Result := Pt <> nil;
end;
{$ELSE}
{$IFDEF UsingDelphi}
var
  Pt : pointer absolute P;
begin
  if (Size = 0) then
    Pt := nil
  else
    try
      GetMem(Pt, Size);
    except
      on EOutOfMemory do
        Pt := nil;
    end;{try..except}
  Result := Pt <> nil;
end;
{$ELSE}
var
  Pt : pointer absolute P;
begin
  SaveHeapFunc := THeapFunc(HeapError);                        {!!.52}
  HeapError := @IsamHeapFunc;
  GetMem(Pt, Size);
  THeapFunc(HeapError) := SaveHeapFunc;                        {!!.52}
  IsamGetMem := Pt <> nil;
end;
{$ENDIF}
{$ENDIF}
{--------}
procedure IsamDelay(MilliSecs : longint);
{$IFDEF Win32}
begin
  Sleep(MilliSecs);
end;
{$ELSE}
const
  NoCallCounter = -1;
  CallCounter : longint = NoCallCounter;
  cOneSecond = 1000;
  cTenSeconds = 10000;
  cOneDay = 1000 * 60 * 60 * 24;
var
  StartTime : longint;
  EndTime   : longint;
  ThisTime  : longint;
  ToCall    : longint;
  Counter   : longint;
  {------}
  function GetTimeNow : longint;
  var
    H, M, S, T : byte;
  begin
    asm
      mov ah, $2C
      {$IFDEF DPMIOrWnd}
      call DOS3Call
      {$ELSE}
      int $21
      {$ENDIF}
      mov T, dl
      mov S, dh
      mov M, cl
      mov H, ch
    end;
    GetTimeNow := ((((((longint(H) * 60) + M) * 60) + S) * 100) + T) * 10;
  end;
  {------}
  function EndTimeIsLessThan(CompareTime : longint) : boolean;
  begin
    if (EndTime < cOneDay) then
      EndTimeIsLessThan := EndTime < CompareTime
    else if (CompareTime > StartTime) then
      EndTimeIsLessThan := false
    else
      EndTimeIsLessThan := (EndTime - cOneDay) < CompareTime;
  end;
  {------}
begin
  {do not delay beyond 10 seconds with this method}
  if (MilliSecs > cTenSeconds) then
    MilliSecs := cTenSeconds
  {exit immediately if requested time is zero or less}
  else if (MilliSecs <= 0) then
    Exit;
  {if we are initialising, get the count/tick}
  if (CallCounter = NoCallCounter) then begin
    {get start time}
    StartTime := GetTimeNow;
    {for initialisation, don't do more than 2 ticks}
    MilliSecs := 80;
    {get a time change}
    repeat
      ThisTime := GetTimeNow;
    until (StartTime <> ThisTime);
    {set start and end times}
    StartTime := ThisTime;
    EndTime := StartTime + MilliSecs;
    Counter := 0;
    repeat
      inc(Counter);
      if (Counter >= CallCounter) then
        {Dummy to adjust speed};
    until EndTimeIsLessThan(GetTimeNow);
    CallCounter := Counter;
  end
  {delay for the required time}
  else begin
    StartTime := GetTimeNow;
    EndTime := StartTime + MilliSecs;
    if (Millisecs > cOneSecond) then begin
      repeat
        {nothing}
      until EndTimeIsLessThan(GetTimeNow);
    end
    else begin
      ToCall := (Millisecs * CallCounter) div 110;
      Counter := 0;
      repeat
        inc(Counter);
        if EndTimeIsLessThan(GetTimeNow) then
          {Dummy to delay};
      until Counter >= ToCall;
    end;
  end;
end;
{$ENDIF}
{====================================================================}


{===File interfaced routines=========================================}
procedure IsamAssign(var F : IsamFile; FName : IsamFileName);
begin
  with F do begin
    Handle := INVALID_HANDLE_VALUE;
    Move(FName[1], Name[0], length(FName));
    Name[length(FName)] := #0;
  end;
end;
{--------}
procedure IsamBlockRead(var F : IsamFile; var Dest; Len  : longint);
var
  Offset : longint;
begin
  {work out the current file pointer}
  Offset := IsamGetPosition(F);
  if not IsamOK then Exit;
  {call IsamGetBlock to read the data}
  IsamGetBlock(F, Offset, Len, Dest);
end;
{--------}
procedure IsamBlockReadRetLen(var F         : IsamFile;
                              var Dest;
                                  Len       : longint;
                              var BytesRead : longint);
{$IFNDEF NoNet}
var
  Offset   : longint;
  Offset1  : longint;
  Offset2  : longint;
  Length1  : longint;
  Length2  : longint;
  LockedIt : boolean;
  UnlockedIt : boolean;
{$ENDIF}
begin
  {$IFNDEF NoNet}
  LockedIt := false;
  if DefeatLocalCache and not F.IsLocal then begin             {!!.54}
    Offset := IsamGetPosition(F);
    if not IsamOK then Exit;
    if btfLockMgrIsLockReq(F.Handle,
                           Offset, Len,
                           Offset1, Length1,
                           Offset2, Length2) then begin
      if btfLockMgrAcqLock(F.Handle, Offset1, Length1,
                           IsamLockTimeOut,
                           IsamDelayBetwLocks) then begin
        if (Length2 = 0) then begin
          LockedIt := true;
        end
        else begin
          if btfLockMgrAcqLock(F.Handle, Offset2, Length2,
                               IsamLockTimeOut,
                               IsamDelayBetwLocks) then begin
            LockedIt := true
          end
          else begin
            if btfLockMgrRelLock(F.Handle, Offset1, Length1) then
              {do nothing};
          end;
        end;
      end;
      if not LockedIt then begin
        IsamOk := false;
        IsamError := 10335;
        Exit;
      end;
    end;
  end;
  {$ENDIF}
  IsamBlockReadRetLenPrim(F, Dest, Len, BytesRead);
  {$IFNDEF NoNet}
  if LockedIt then begin
    UnlockedIt := true;
    if not btfLockMgrRelLock(F.Handle, Offset1, Length1) then
      UnlockedIt := false;
    if (Length2 <> 0) then
      if not btfLockMgrRelLock(F.Handle, Offset2, Length2) then
        UnlockedIt := false;
    if not UnlockedIt then begin
      if IsamOk then begin
        IsamOk := false;
        IsamError := 10345;
      end;
    end;
  end;
  {$ENDIF}
end;
{--------}
procedure IsamBlockWrite(var F : IsamFile; var Source; Len : longint);
var
  Offset : longint;
begin
  {work out the current file pointer}
  Offset := IsamGetPosition(F);
  if not IsamOK then Exit;
  {call IsamPutBlock to write the data}
  IsamPutBlock(F, Offset, Len, Source);
end;
{--------}
procedure IsamClose(var F : IsamFile);
begin
  btfLockMgrRelAllLocks(F.Handle);
  if not btfCloseFile(F.Handle) then begin
    IsamOK := false;
    case btfDOSError of
      6 : IsamError := 9904;
    else
      IsamError := 10140;
    end;{case}
    SetIsamDOSError;
  end
  else
    F.Handle := INVALID_HANDLE_VALUE;
end;
{--------}
procedure IsamDelete(var F : IsamFile);
begin
  if not btfDeleteFile(F.Name) then begin
    IsamOK := false;
    case btfDOSError of
      2  : IsamError := 9903;
      3  : IsamError := 9900;
      53 : IsamError := 9900;                                  {!!.57}
    else
      IsamError := 10140;
    end;{case}
    SetIsamDOSError;
  end;
end;
{--------}
procedure IsamFlush(var F        : IsamFile;
                    var WithDUP  : Boolean;
                        NetUsed  : Boolean);
begin
  WithDUP := true;
  if not btfFlushFile(F.Handle) then begin
    if (btfDOSError = 4) then begin
      {a dup was attempted, but there's too many open files}
      WithDUP := false;
      if NetUsed then begin
        IsamOK := False;
        IsamError := 10150;
        SetIsamDOSError;
      end
      else begin
        IsamClose(F);
        if not IsamOK then
          Exit;
        IsamReset(F, False, False);
      end;
    end
    else begin
      IsamOK := false;
      case btfDOSError of
        6 : IsamError := 9904;
      else
        IsamError := 10140;
      end;{case}
      SetIsamDOSError;
    end;
  end;
end;
{--------}
procedure IsamGetBlock(var F        : IsamFile;
                           Ref, Len : LongInt;
                       var Dest);
{$IFNDEF NoNet}
var
  Offset1  : longint;
  Offset2  : longint;
  Length1  : longint;
  Length2  : longint;
  LockedIt : boolean;
  UnlockedIt : boolean;
{$ENDIF}
begin
  {$IFNDEF NoNet}
  LockedIt := false;
  if DefeatLocalCache and not F.IsLocal then begin             {!!.54}
    if btfLockMgrIsLockReq(F.Handle,
                           Ref, Len,
                           Offset1, Length1,
                           Offset2, Length2) then begin
      if btfLockMgrAcqLock(F.Handle, Offset1, Length1,
                           IsamLockTimeOut,
                           IsamDelayBetwLocks) then begin
        if (Length2 = 0) then begin
          LockedIt := true;
        end
        else begin
          if btfLockMgrAcqLock(F.Handle, Offset2, Length2,
                               IsamLockTimeOut,
                               IsamDelayBetwLocks) then begin
            LockedIt := true
          end
          else begin
            if btfLockMgrRelLock(F.Handle, Offset1, Length1) then
              {do nothing};
          end;
        end;
      end;
      if not LockedIt then begin
        IsamOk := false;
        IsamError := 10335;
        Exit;
      end;
    end;
  end;
  {$ENDIF}
  IsamGetBlockPrim(F, Ref, Len, Dest);
  {$IFNDEF NoNet}
  if LockedIt then begin
    UnlockedIt := true;
    if not btfLockMgrRelLock(F.Handle, Offset1, Length1) then
      UnlockedIt := false;
    if (Length2 <> 0) then
      if not btfLockMgrRelLock(F.Handle, Offset2, Length2) then
        UnlockedIt := false;
    if not UnlockedIt then begin
      if IsamOk then begin
        IsamOk := false;
        IsamError := 10345;
      end;
    end;
  end;
  {$ENDIF}
end;
{--------}
function IsamGetPosition(var F : IsamFile) : LongInt;
var
  Offset : DWORD;                                              {!!.54}
begin
  if not btfGetPositionFile(F.Handle, Offset) then begin
    IsamOK := false;
    case btfDOSError of
      6 : IsamError := 9904;
    else
      IsamError := 10140;
    end;{case}
    SetIsamDOSError;
    IsamGetPosition := -1;
  end
  else
    IsamGetPosition := Offset;
end;
{--------}
procedure IsamLongSeek(var F : IsamFile; Ref : LongInt);
begin
  if not btfPositionFile(F.Handle, Ref) then begin
    IsamOK := false;
    case btfDOSError of
      6 : IsamError := 9904;
    else
      IsamError := 10140;
    end;{case}
    SetIsamDOSError;
  end;
end;
{--------}
procedure IsamLongSeekEOF(var F : IsamFile; var Len : DWORD);
begin
  if not btfPositionFileEOF(F.Handle, Len) then begin
    IsamOK := false;
    case btfDOSError of
      6 : IsamError := 9904;
    else
      IsamError := 10140;
    end;{case}
    SetIsamDOSError;
  end;
end;
{--------}
procedure IsamPutBlock(var F        : IsamFile;
                           Ref, Len : LongInt;
                       var Source);
{$IFNDEF NoNet}
var
  Offset1  : longint;
  Offset2  : longint;
  Length1  : longint;
  Length2  : longint;
  LockedIt : boolean;
  UnlockedIt : boolean;
{$ENDIF}
begin
  {$IFNDEF NoNet}
  LockedIt := false;
  if DefeatLocalCache and (Len <> 0) and not F.IsLocal then begin {!!.54}
    if btfLockMgrIsLockReq(F.Handle,
                           Ref, Len,
                           Offset1, Length1,
                           Offset2, Length2) then begin
      if btfLockMgrAcqLock(F.Handle, Offset1, Length1,
                           IsamLockTimeOut,
                           IsamDelayBetwLocks) then begin
        if (Length2 = 0) then begin
          LockedIt := true;
        end
        else begin
          if btfLockMgrAcqLock(F.Handle, Offset2, Length2,
                               IsamLockTimeOut,
                               IsamDelayBetwLocks) then begin
            LockedIt := true
          end
          else begin
            if btfLockMgrRelLock(F.Handle, Offset1, Length1) then
              {do nothing};
          end;
        end;
      end;
      if not LockedIt then begin
        IsamOk := false;
        IsamError := 10335;
        Exit;
      end;
    end;
  end;
  {$ENDIF}
  IsamPutBlockPrim(F, Ref, Len, Source);
  {$IFNDEF NoNet}
  if LockedIt then begin
    UnlockedIt := true;
    if not btfLockMgrRelLock(F.Handle, Offset1, Length1) then
      UnlockedIt := false;
    if (Length2 <> 0) then
      if not btfLockMgrRelLock(F.Handle, Offset2, Length2) then
        UnlockedIt := false;
    if not UnlockedIt then begin
      if IsamOk then begin
        IsamOk := false;
        IsamError := 10345;
      end;
    end;
  end;
  {$ENDIF}
end;
{--------}
procedure IsamRename(var F : IsamFile; FName : IsamFileName);
var
  NameZ : array [0..IsamFileNameLen] of char;
begin
  Move(FName[1], NameZ[0], length(FName));
  NameZ[length(FName)] := #0;
  if not btfRenameFile(F.Name, NameZ) then begin
    IsamOK := false;
    case btfDOSError of
      2  : IsamError := 9903;
      3  : IsamError := 9900;
      53 : IsamError := 9900;                                  {!!.57}
    else
      IsamError := 10140;
    end;{case}
    SetIsamDOSError;
  end;
end;
{--------}
procedure IsamReset(var F : IsamFile; NetUsed, ReadOnly : Boolean);
var
  OpenMode  : TbtfOpenMode;
  ShareMode : TbtfShareMode;
begin
  if ReadOnly then
    OpenMode := bomReadOnly
  else
    OpenMode := bomReadWrite;
  if NetUsed then
    ShareMode := bsmShared
  else
    ShareMode := bsmExclusive;
  if not btfOpenFile(F.Name, OpenMode, ShareMode,
                     false, false, InheritFileHandles, F.Handle) then begin
    IsamOK := false;
    case btfDOSError of
      2  : IsamError := 9903;
      3  : IsamError := 9900;
      4  : IsamError := 9901;
      12 : IsamError := 9908;
      53 : IsamError := 9900;                                  {!!.57}
    else
      IsamError := 10140;
    end;{case}
    SetIsamDOSError;
  end
  else begin                                                   {!!.54}
    F.IsLocal := not NetUsed;                                  {!!.54}
  end;                                                         {!!.54}
end;
{--------}
procedure IsamRewrite(var F : IsamFile);
begin
  if not btfOpenFile(F.Name, bomReadWrite, bsmExclusive,
                     false, true, InheritFileHandles, F.Handle) then begin
    IsamOK := false;
    case btfDOSError of
      3  : IsamError := 9900;
      4  : IsamError := 9901;
      53 : IsamError := 9900;                                  {!!.57}
    else
      IsamError := 10140;
    end;{case}
    SetIsamDOSError;
  end                                                          {!!.54}
  else begin                                                   {!!.54}
    F.IsLocal := true;                                         {!!.54}
  end;
end;
{--------}
procedure IsamSetEOF(var F : IsamFile);
var
  Offset : longint;
begin
  Offset := IsamGetPosition(F);
  if not IsamOK then Exit;
  if not btfSetEOF(F.Handle, Offset) then begin
    IsamOK := false;
    case btfDOSError of
      6 : IsamError := 9904;
    else
      IsamError := 10140;
    end;{case}
    SetIsamDOSError;
  end;
end;

procedure BTIsBaseUnitDone; far;
begin
  btfLockMgrDestroy;
  {$IFNDEF Win32}
  ExitProc := ExitSave;
  {$ENDIF}
end;

procedure BTIsBaseUnitInit;
begin
  btfLockMgrCreate;
  {$IFNDEF Win32}
  ExitSave := ExitProc;
  ExitProc := @BTIsBaseUnitDone;
  {$ENDIF}
end;

{$IFDEF Win32}
initialization
  BTIsBaseUnitInit;

finalization
  BTIsBaseUnitDone;
{$ELSE}
begin
  BTIsBaseUnitInit;
{$ENDIF}

end.
