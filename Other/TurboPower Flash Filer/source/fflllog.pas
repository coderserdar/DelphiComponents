{*********************************************************}
{* FlashFiler: Logging facility                          *}
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

{$I ffdefine.inc}
unit fflllog;

interface

uses
  Classes,
  ExtCtrls,                                                            {!!.06}
  SysUtils,
  Windows,
  ffllbase;

type
  { Base class for event logs. }
  TffBaseLog = class(TffComponent)
  protected  { private }
    { Property variables }
    FCache : Boolean;                                                  {!!.06}
    FCacheLimit : Integer;                                             {!!.06}
    FEnabled : Boolean;
    FFileName : TFileName;

    { Internal variables }
    blLogCS : TRTLCriticalSection;
{Begin !!.06}
    blTimer : TTimer;
      { When caching, flushes cache during periods of inactivity. The timer
        is enabled only when caching is enabled and something is written to
        the log. The timer is reset as more stuff is added to the log. }
{End !!.06}
    { Property methods }
    function blGetFileName : TFileName;
  protected
    procedure blLockLog;
    procedure blUnlockLog;
    function blGetEnabled : Boolean;
    procedure blOnTimer(Sender : TObject); virtual;                      {!!.06}
    procedure blSetEnabled(const Value : Boolean); virtual;
    procedure blSetFileName(const Value : TFileName); virtual;
    procedure Clear; virtual;
  public
    constructor Create(aOwner : TComponent); override;
    destructor Destroy; override;

    procedure Flush; virtual;                                          {!!.06}

    procedure WriteBlock(const S : string; Buf : pointer;
                         BufLen : TffMemSize); virtual; abstract;
      { Use this method to write a block of data to the event log. }

    procedure WriteString(const aMsg : string); virtual; abstract;
      { Used to write a string to the event log. }

    procedure WriteStringFmt(const aMsg : string; args : array of const); virtual; abstract;
      { Used to write a formatted string to the event log. }

    procedure WriteStrings(const Msgs : array of string); virtual; abstract;
      { Used to write a block of strings to the event log. }
      
    { Properties }
{Begin !!.06}
    property CacheEnabled : Boolean
      read FCache
      write FCache
      default True;
      { If True then log lines are cached in memory and flushed to
        disk once the CacheLimit has been reached. }

    property CacheLimit : Integer
      read FCacheLimit
      write FCacheLimit
      default 500;
      { The maximum number of log lines that may be retained in
        memory. Not used if CacheEnabled is set to False. }
{End !!.06}

    property Enabled : Boolean
       read blGetEnabled
       write blSetEnabled
       default False;                                                  {!!.01}
      { Enable/disable event logging. }

    property FileName : TFileName
      read blGetFileName write blSetFileName;
      { The file to which the event log is written. }
  end;

  TffEventLog = class(TffBaseLog)
  protected
    FLog            : TStringList;                                     {!!.06}
    FLogSize        : Integer;                                         {!!.06}
    FTruncateSize   : Integer;                                         {!!.06}
    FMaxSize        : Integer;                                         {!!.06}
    FWriteBlockData : Boolean;                                         {!!.06}

    procedure elTruncateCheck(const Stream : TStream);                 {!!.06}
    procedure elWritePrim(const LogStr : string); virtual;             {!!.05}
  public
    constructor Create(aOwner : TComponent); override;
    destructor Destroy; override;

    procedure Flush; override;                                         {!!.06}
      { Flushes the contents of the cache to the log. }                {!!.06}

    procedure WriteBlock(const S : string; Buf : pointer;
                         BufLen : TffMemSize); override;
    procedure WriteString(const aMsg : string); override;
    procedure WriteStringFmt(const aMsg : string; args : array of const); override;
    procedure WriteStrings(const Msgs : array of string); override;

  published

    { Inherited properties }
    property CacheEnabled;                                             {!!.06}
    property CacheLimit;                                               {!!.06}
    property Enabled;
    property FileName;

{Begin !!.06}
    property MaxSize : Integer
      read FMaxSize
      write FMaxSize
      default 50;
      { Max size (in megabytes) of the log file. Once the log file
        reaches this size it will be truncated to TruncateSize. By
        default, the log is truncated at 50MB. }

   property TruncateSize : Integer
     read FTruncateSize
     write FTruncateSize
     default ffcl_1KB;
     { Kilobytes of log kept when truncated. By default, 1MB is kept
       when the log is truncated. See MaxSize. }

    property WriteBlockData : Boolean
      read FWriteBlockData
      write FWriteBlockData
      default False;
      { If set to False then data passed to WriteBlock is *not*
        written to the log. }
{End !!.06}
  end;

{Begin !!.06}
const
  ffc_FlushTimerInterval : Cardinal = 1000;
{End !!.06}

implementation

const
  ffcsSpaces13 = '             ';
  ffcsSpaces44 = ffcsSpaces13 + ffcsSpaces13 + ffcsSpaces13 + '     ';
  ffcsFormat   = '%s %12d %8d %s' + ffcCRLF;

{===TffBaseLog=======================================================}

constructor TffBaseLog.Create(aOwner : TComponent);
begin
  inherited Create(aOwner);
  InitializeCriticalSection(blLogCS);
  FCache := True;
  FCacheLimit := 500;
{Begin !!.06}
  blTimer := TTimer.Create(nil);
  blTimer.Enabled := False;
  blTimer.Interval := ffc_FlushTimerInterval;
  blTimer.OnTimer := blOnTimer;
{End !!.06}
end;
{--------}
destructor TffBaseLog.Destroy;
begin
  FFNotifyDependents(ffn_Destroy);                                     {!!.11}
  blTimer.Free;                                                        {!!.05}
  DeleteCriticalSection(blLogCS);
  inherited Destroy;
end;
{--------}
function TffBaseLog.blGetEnabled : Boolean;
begin
  blLockLog;
  try
    Result := FEnabled;
  finally
    blUnlockLog;
  end;
end;
{--------}
function TffBaseLog.blGetFileName : TFileName;
begin
  blLockLog;
  try
    Result := FFileName;
  finally
    blUnlockLog;
  end;
end;
{--------}
procedure TffBaseLog.blLockLog;
begin
  if IsMultiThread then
    EnterCriticalSection(blLogCS);
end;
{Begin !!.06}
{--------}
procedure TffBaseLog.blOnTimer(Sender : TObject);
begin
  blLockLog;
  try
    blTimer.Enabled := False;
    Flush;
  finally
    blUnlockLog;
  end;
end;
{End !!.06}
{--------}
procedure TffBaseLog.blSetEnabled(const Value : Boolean);
begin
  blLockLog;
  try
    FEnabled := Value;
  finally
    blUnlockLog;
  end;
end;
{--------}
procedure TffBaseLog.blSetFileName(const Value : TFileName);
begin
  blLockLog;
  try
    FFileName := Value;
  finally
    blUnlockLog;
  end;
end;
{--------}
procedure TffBaseLog.blUnlockLog;
begin
  if IsMultiThread then
    LeaveCriticalSection(blLogCS);
end;
{Begin !!.06}
{--------}
procedure TffBaseLog.Clear;
begin
  { Do nothing }
end;
{--------}
procedure TffBaseLog.Flush;
begin
  { Do nothing }
end;
{End !!.06}

{====================================================================}

{===TffEventLog======================================================}
{Begin !!.06}
constructor TffEventLog.Create(aOwner : TComponent);
begin
  inherited Create(aOwner);
  FLog := TStringList.Create;
  FLogSize := 0;
  FWriteBlockData := False;
  FMaxSize := 50;
  FTruncateSize := ffcl_1KB;
end;
{--------}
destructor TffEventLog.Destroy;
begin
  Flush;
  FLog.Free;
  inherited;
end;
{--------}
procedure TffEventLog.elTruncateCheck(const Stream : TStream);
var
  TruncBytes,
  MaxBytes   : Integer;
  TempStr    : string;
begin
  { Convert MaxSize to Bytes. }
  MaxBytes := (FMaxSize * ffcl_1MB);

  { Is it time to truncate this log file? }
  if ((FMaxSize <> 0) and
      (FLogSize > MaxBytes)) then begin

    { Convert the truncate size to bytes. }
    TruncBytes := (FTruncateSize * ffcl_1KB);

    { Position the log to the portion we want to keep. }
    Stream.Seek(TruncBytes * -1, soFromEnd);
    { Preserve the part we want to keep. }
    SetLength(TempStr, TruncBytes);
    Stream.Read(TempStr[1], TruncBytes);
    { Truncate the file. }
    Stream.Size := TruncBytes;
    { Position to the beginning of the file and write the preserved
      portion of the log. }
    Stream.Position := 0;
    Stream.Write(TempStr[1], TruncBytes);

    { Reset the log's size. }
    FLogSize := TruncBytes;
  end;
end;
{--------}
{End !!.06}
procedure TffEventLog.elWritePrim(const LogStr : string);
{Rewritten !!.06}
var
  FileStm : TFileStream;
  LogMode : Word;
begin
  { Assumption: Log file locked for use by this thread. }

  if FCache then begin
    blTimer.Enabled := False;
    if FLog.Count = FCacheLimit then
      Flush;
    blTimer.Enabled := True;
    FLog.Add(LogStr);
  end
  else begin
    { Check whether file exists, set flags appropriately }
    if FileExists(FFileName) then
      LogMode := (fmOpenReadWrite or fmShareDenyWrite)
    else
      LogMode := (fmCreate or fmShareDenyWrite);

    { Open file, write string, close file }
    FileStm := TFileStream.Create(FFileName, LogMode);
    try
      elTruncateCheck(FileStm);
      FileStm.Seek(0, soFromEnd);
      FLogSize := FLogSize +
                  FileStm.Write(LogStr[1], Length(LogStr));
    finally
      FileStm.Free;
    end;
  end;
end;
{Begin !!.06}
{--------}
procedure TffEventLog.Flush;
var
  Inx : Integer;
  aStr : string;
  FileStm : TFileStream;
  LogMode : Word;
begin
  { Assumption: Log file locked for use by this thread. }

  if FCache and (FLog.Count > 0) and (FFileName <> '') then begin
    { Check whether file exists, set flags appropriately }
    if FileExists(FFileName) then
      LogMode := (fmOpenReadWrite or fmShareDenyWrite)
    else
      LogMode := (fmCreate or fmShareDenyWrite);

    { Open file, write string, close file }
    FileStm := TFileStream.Create(FFileName, LogMode);
    try
      elTruncateCheck(FileStm);
      FileStm.Seek(0, soFromEnd);
      for Inx := 0 to Pred(FLog.Count) do begin
        aStr := FLog.Strings[Inx];
        FLogSize := FLogSize +
                    FileStm.Write(aStr[1], Length(aStr));
      end;
    finally
      FileStm.Free;
    end;
    FLog.Clear;
  end;
end;
{End !!.06}
{--------}
procedure TffEventLog.WriteBlock(const S : string; Buf : pointer;
                                 BufLen : TffMemSize);
const
  HexPos : array [0..15] of byte =
    (1, 4, 7, 10, 14, 17, 20, 23, 27, 30, 33, 36, 40, 43, 46, 49);
  HexChar : array [0..15] of char =
    '0123456789abcdef';
var
  B : PffByteArray absolute Buf;
  ThisWidth,
  i, j : integer;
  Line : string[70];
  Work : byte;
begin
{Begin !!.06}
  if FWriteBlockData then begin
    blLockLog;
    try
      WriteStringFmt('%s  (Size: %d)', [S, BufLen]);
      if (BufLen = 0) or (Buf = nil) then
        elWritePrim(ffcsSpaces13 + 'buffer is nil' + ffcCRLF)
      else begin
        if (BufLen > 1024) then begin
          elWritePrim(ffcsSpaces13 + '(writing first 1K of buffer only)' + ffcCRLF);
          BufLen := 1024;
        end;
        for i := 0 to ((BufLen-1) shr 4) do begin
          FillChar(Line, 70, ' ');
          Line[0] := #70;
          Line[53] := '['; Line[70] := ']';
          if (BufLen >= 16) then
            ThisWidth := 16
          else
            ThisWidth := BufLen;
          for j := 0 to ThisWidth-1 do begin
            Work := B^[(i shl 4) + j];
            Line[HexPos[j]] := HexChar[Work shr 4];
            Line[HexPos[j]+1] := HexChar[Work and $F];
            if (Work < 32) or (Work >= $80) then
              Work := ord('.');
            Line[54+j] := char(Work);
          end;
          elWritePrim(ffcsSpaces13 + Line + ffcCRLF);
          dec(BufLen, ThisWidth);
        end;
      end;
    finally
      blUnlockLog;
    end;
  end;  { if }
{End !!.06}
end;
{--------}
procedure TffEventLog.WriteString(const aMsg : string);
var
  LogStr : string;
begin

  { Bail if logging isn't turned on }
  if not FEnabled then Exit;

  blLockLog;
  try
    { Create appropriate string for log }
    LogStr := format(ffcsFormat,
                     [DateTimeToStr(Now), getTickCount,
                      getCurrentThreadID, aMsg]);

    elWritePrim(LogStr);

  finally
    blUnlockLog;
  end;
end;
{--------}
procedure TffEventLog.WriteStringFmt(const aMsg : string; args : array of const);
var
  LogStr : string;
begin

  { Bail if logging isn't turned on }
  if not FEnabled then Exit;

  blLockLog;
  try
    { Create appropriate string for log }
    LogStr := format(ffcsFormat,
                     [DateTimeToStr(Now), getTickCount,
                      getCurrentThreadID, format(aMsg, args)]);

    elWritePrim(LogStr);

  finally
    blUnlockLog;
  end;
end;
{--------}
procedure TffEventLog.WriteStrings(const Msgs : array of string);
var
  Index : longInt;
  LogStr : string;
  MsgStr : string;
begin

  { Bail if logging isn't turned on }
  if not FEnabled then Exit;

  blLockLog;
  try

    for Index := 0 to high(Msgs) do begin

      { Create appropriate string for log }
      MsgStr := Msgs[Index];
      if (length(MsgStr) = 0) then
        LogStr := ffcCRLF
      else if(MsgStr[1] = ' ') then
        LogStr := ffcsSpaces44 + MsgStr + ffcCRLF
      else
        LogStr := format(ffcsFormat,
                         [DateTimeToStr(Now), getTickCount,
                          getCurrentThreadID, MsgStr]);

      elWritePrim(LogStr);

    end;

  finally
    blUnlockLog;
  end;
end;
{====================================================================}

end.
