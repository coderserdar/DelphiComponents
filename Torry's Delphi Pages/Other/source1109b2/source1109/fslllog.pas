{$I fsdefine.inc}
Unit fslllog;

Interface

Uses
  Classes,
  ExtCtrls, {!!.06}
  SysUtils,
  Windows,
  fsllbase;

Type
  { Base class for event logs. }
  TFSBaseLog = Class(TFSSpecComp)
  Protected { private }
    { Property variables }
    FCache: Boolean; {!!.06}
    FCacheLimit: Integer; {!!.06}
    FEnabled: Boolean;
    FFileName: TFileName;

    { Internal variables }
    blLogCS: TRTLCriticalSection;
    {Begin !!.06}
    blTimer: TTimer;
    { When caching, flushes cache during periods of inactivity. The timer
      is enabled only when caching is enabled and something is written to
      the log. The timer is reset as more stuff is added to the log. }
{End !!.06}
  { Property methods }
    Function blGetFileName: TFileName;
  Protected
    Procedure blLockLog;
    Procedure blUnlockLog;
    Function blGetEnabled: Boolean;
    Procedure blOnTimer(Sender: TObject); Virtual; {!!.06}
    Procedure blSetEnabled(Const Value: Boolean); Virtual;
    Procedure blSetFileName(Const Value: TFileName); Virtual;
    Procedure Clear; Virtual;
  Public
    Constructor Create(aOwner: TComponent); Override;
    Destructor Destroy; Override;

    Procedure Flush; Virtual; {!!.06}

    Procedure WriteBlock(Const S: String; Buf: pointer;
      BufLen: TffMemSize); Virtual; Abstract;
    { Use this method to write a block of data to the event log. }

    Procedure WriteString(Const aMsg: String); Virtual; Abstract;
    { Used to write a string to the event log. }

    Procedure WriteStringFmt(Const aMsg: String; args: Array Of Const); Virtual; Abstract;
    { Used to write a formatted string to the event log. }

    Procedure WriteStrings(Const Msgs: Array Of String); Virtual; Abstract;
    { Used to write a block of strings to the event log. }

  { Properties }
{Begin !!.06}
    Property CacheEnabled: Boolean
      Read FCache
      Write FCache
      Default True;
    { If True then log lines are cached in memory and flushed to
      disk once the CacheLimit has been reached. }

    Property CacheLimit: Integer
      Read FCacheLimit
      Write FCacheLimit
      Default 500;
    { The maximum number of log lines that may be retained in
      memory. Not used if CacheEnabled is set to False. }
{End !!.06}

    Property Enabled: Boolean
      Read blGetEnabled
      Write blSetEnabled
      Default False; {!!.01}
    { Enable/disable event logging. }

    Property FileName: TFileName
      Read blGetFileName Write blSetFileName;
    { The file to which the event log is written. }
  End;

  TFSEventLog = Class(TFSBaseLog)
  Protected
    FLog: TStringList; {!!.06}
    FLogSize: Integer; {!!.06}
    FTruncateSize: Integer; {!!.06}
    FMaxSize: Integer; {!!.06}
    FWriteBlockData: Boolean; {!!.06}

    Procedure elTruncateCheck(Const Stream: TStream); {!!.06}
    Procedure elWritePrim(Const LogStr: String); Virtual; {!!.05}
  Public
    Constructor Create(aOwner: TComponent); Override;
    Destructor Destroy; Override;

    Procedure Flush; Override; {!!.06}
    { Flushes the contents of the cache to the log. }{!!.06}

    Procedure WriteBlock(Const S: String; Buf: pointer;
      BufLen: TffMemSize); Override;
    Procedure WriteString(Const aMsg: String); Override;
    Procedure WriteStringFmt(Const aMsg: String; args: Array Of Const); Override;
    Procedure WriteStrings(Const Msgs: Array Of String); Override;

  Published

    { Inherited properties }
    Property CacheEnabled; {!!.06}
    Property CacheLimit; {!!.06}
    Property Enabled;
    Property FileName;

    {Begin !!.06}
    Property MaxSize: Integer
      Read FMaxSize
      Write FMaxSize
      Default 50;
    { Max size (in megabytes) of the log file. Once the log file
      reaches this size it will be truncated to TruncateSize. By
      default, the log is truncated at 50MB. }

    Property TruncateSize: Integer
      Read FTruncateSize
      Write FTruncateSize
      Default fscl_1KB;
    { Kilobytes of log kept when truncated. By default, 1MB is kept
      when the log is truncated. See MaxSize. }

    Property WriteBlockData: Boolean
      Read FWriteBlockData
      Write FWriteBlockData
      Default False;
    { If set to False then data passed to WriteBlock is *not*
      written to the log. }
{End !!.06}
  End;

  {Begin !!.06}
Const
  ffc_FlushTimerInterval: Cardinal = 1000;
  {End !!.06}

Implementation

Const
  ffcsSpaces13 = '             ';
  ffcsSpaces44 = ffcsSpaces13 + ffcsSpaces13 + ffcsSpaces13 + '     ';
  ffcsFormat = '%s %12d %8d %s' + ffcCRLF;

  {===TFSBaseLog=======================================================}

Constructor TFSBaseLog.Create(aOwner: TComponent);
Begin
  Inherited Create(aOwner);
  InitializeCriticalSection(blLogCS);
  FCache := True;
  FCacheLimit := 500;
  {Begin !!.06}
  blTimer := TTimer.Create(Nil);
  blTimer.Enabled := False;
  blTimer.Interval := ffc_FlushTimerInterval;
  blTimer.OnTimer := blOnTimer;
  {End !!.06}
End;
{--------}

Destructor TFSBaseLog.Destroy;
Begin
  FFNotifyDependents(ffn_Destroy); {!!.11}
  blTimer.Free; {!!.05}
  DeleteCriticalSection(blLogCS);
  Inherited Destroy;
End;
{--------}

Function TFSBaseLog.blGetEnabled: Boolean;
Begin
  blLockLog;
  Try
    Result := FEnabled;
  Finally
    blUnlockLog;
  End;
End;
{--------}

Function TFSBaseLog.blGetFileName: TFileName;
Begin
  blLockLog;
  Try
    Result := FFileName;
  Finally
    blUnlockLog;
  End;
End;
{--------}

Procedure TFSBaseLog.blLockLog;
Begin
  If IsMultiThread Then
    EnterCriticalSection(blLogCS);
End;
{Begin !!.06}
{--------}

Procedure TFSBaseLog.blOnTimer(Sender: TObject);
Begin
  blLockLog;
  Try
    blTimer.Enabled := False;
    Flush;
  Finally
    blUnlockLog;
  End;
End;
{End !!.06}
{--------}

Procedure TFSBaseLog.blSetEnabled(Const Value: Boolean);
Begin
  blLockLog;
  Try
    FEnabled := Value;
  Finally
    blUnlockLog;
  End;
End;
{--------}

Procedure TFSBaseLog.blSetFileName(Const Value: TFileName);
Begin
  blLockLog;
  Try
    FFileName := Value;
  Finally
    blUnlockLog;
  End;
End;
{--------}

Procedure TFSBaseLog.blUnlockLog;
Begin
  If IsMultiThread Then
    LeaveCriticalSection(blLogCS);
End;
{Begin !!.06}
{--------}

Procedure TFSBaseLog.Clear;
Begin
  { Do nothing }
End;
{--------}

Procedure TFSBaseLog.Flush;
Begin
  { Do nothing }
End;
{End !!.06}

{====================================================================}

{===TFSEventLog======================================================}
{Begin !!.06}

Constructor TFSEventLog.Create(aOwner: TComponent);
Begin
  Inherited Create(aOwner);
  FLog := TStringList.Create;
  FLogSize := 0;
  FWriteBlockData := False;
  FMaxSize := 50;
  FTruncateSize := fscl_1KB;
End;
{--------}

Destructor TFSEventLog.Destroy;
Begin
  Flush;
  FLog.Free;
  Inherited;
End;
{--------}

Procedure TFSEventLog.elTruncateCheck(Const Stream: TStream);
Var
  TruncBytes,
    MaxBytes: Integer;
  TempStr: String;
Begin
  { Convert MaxSize to Bytes. }
  MaxBytes := (FMaxSize * fscl_1MB);

  { Is it time to truncate this log file? }
  If ((FMaxSize <> 0) And
    (FLogSize > MaxBytes)) Then
    Begin

      { Convert the truncate size to bytes. }
      TruncBytes := (FTruncateSize * fscl_1KB);

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
    End;
End;
{--------}
{End !!.06}

Procedure TFSEventLog.elWritePrim(Const LogStr: String);
{Rewritten !!.06}
Var
  FileStm: TFileStream;
  LogMode: Word;
Begin
  { Assumption: Log file locked for use by this thread. }

  If FCache Then
    Begin
      blTimer.Enabled := False;
      If FLog.Count = FCacheLimit Then
        Flush;
      blTimer.Enabled := True;
      FLog.Add(LogStr);
    End
  Else
    Begin
      { Check whether file exists, set flags appropriately }
      If FileExists(FFileName) Then
        LogMode := (fmOpenReadWrite Or fmShareDenyWrite)
      Else
        LogMode := (fmCreate Or fmShareDenyWrite);

      { Open file, write string, close file }
      FileStm := TFileStream.Create(FFileName, LogMode);
      Try
        elTruncateCheck(FileStm);
        FileStm.Seek(0, soFromEnd);
        FLogSize := FLogSize +
          FileStm.Write(LogStr[1], Length(LogStr));
      Finally
        FileStm.Free;
      End;
    End;
End;
{Begin !!.06}
{--------}

Procedure TFSEventLog.Flush;
Var
  Inx: Integer;
  aStr: String;
  FileStm: TFileStream;
  LogMode: Word;
Begin
  { Assumption: Log file locked for use by this thread. }

  If FCache And (FLog.Count > 0) And (FFileName <> '') Then
    Begin
      { Check whether file exists, set flags appropriately }
      If FileExists(FFileName) Then
        LogMode := (fmOpenReadWrite Or fmShareDenyWrite)
      Else
        LogMode := (fmCreate Or fmShareDenyWrite);

      { Open file, write string, close file }
      FileStm := TFileStream.Create(FFileName, LogMode);
      Try
        elTruncateCheck(FileStm);
        FileStm.Seek(0, soFromEnd);
        For Inx := 0 To Pred(FLog.Count) Do
          Begin
            aStr := FLog.Strings[Inx];
            FLogSize := FLogSize +
              FileStm.Write(aStr[1], Length(aStr));
          End;
      Finally
        FileStm.Free;
      End;
      FLog.Clear;
    End;
End;
{End !!.06}
{--------}

Procedure TFSEventLog.WriteBlock(Const S: String; Buf: pointer;
  BufLen: TffMemSize);
Const
  HexPos: Array[0..15] Of Byte =
  (1, 4, 7, 10, 14, 17, 20, 23, 27, 30, 33, 36, 40, 43, 46, 49);
  HexChar: Array[0..15] Of char =
  '0123456789abcdef';
Var
  B: PffByteArray Absolute Buf;
  ThisWidth,
    i, j: Integer;
  Line: String[70];
  Work: Byte;
Begin
  {Begin !!.06}
  If FWriteBlockData Then
    Begin
      blLockLog;
      Try
        WriteStringFmt('%s  (Size: %d)', [S, BufLen]);
        If (BufLen = 0) Or (Buf = Nil) Then
          elWritePrim(ffcsSpaces13 + 'buffer is nil' + ffcCRLF)
        Else
          Begin
            If (BufLen > 1024) Then
              Begin
                elWritePrim(ffcsSpaces13 + '(writing first 1K of buffer only)' + ffcCRLF);
                BufLen := 1024;
              End;
            For i := 0 To ((BufLen - 1) Shr 4) Do
              Begin
                FillChar(Line, 70, ' ');
                Line[0] := #70;
                Line[53] := '[';
                Line[70] := ']';
                If (BufLen >= 16) Then
                  ThisWidth := 16
                Else
                  ThisWidth := BufLen;
                For j := 0 To ThisWidth - 1 Do
                  Begin
                    Work := B^[(i Shl 4) + j];
                    Line[HexPos[j]] := HexChar[Work Shr 4];
                    Line[HexPos[j] + 1] := HexChar[Work And $F];
                    If (Work < 32) Or (Work >= $80) Then
                      Work := ord('.');
                    Line[54 + j] := char(Work);
                  End;
                elWritePrim(ffcsSpaces13 + Line + ffcCRLF);
                dec(BufLen, ThisWidth);
              End;
          End;
      Finally
        blUnlockLog;
      End;
    End; { if }
  {End !!.06}
End;
{--------}

Procedure TFSEventLog.WriteString(Const aMsg: String);
Var
  LogStr: String;
Begin

  { Bail if logging isn't turned on }
  If Not FEnabled Then Exit;

  blLockLog;
  Try
    { Create appropriate string for log }
    LogStr := format(ffcsFormat,
      [DateTimeToStr(Now), getTickCount,
      getCurrentThreadID, aMsg]);

    elWritePrim(LogStr);

  Finally
    blUnlockLog;
  End;
End;
{--------}

Procedure TFSEventLog.WriteStringFmt(Const aMsg: String; args: Array Of Const);
Var
  LogStr: String;
Begin

  { Bail if logging isn't turned on }
  If Not FEnabled Then Exit;

  blLockLog;
  Try
    { Create appropriate string for log }
    LogStr := format(ffcsFormat,
      [DateTimeToStr(Now), getTickCount,
      getCurrentThreadID, format(aMsg, args)]);

    elWritePrim(LogStr);

  Finally
    blUnlockLog;
  End;
End;
{--------}

Procedure TFSEventLog.WriteStrings(Const Msgs: Array Of String);
Var
  Index: Longint;
  LogStr: String;
  MsgStr: String;
Begin

  { Bail if logging isn't turned on }
  If Not FEnabled Then Exit;

  blLockLog;
  Try

    For Index := 0 To high(Msgs) Do
      Begin

        { Create appropriate string for log }
        MsgStr := Msgs[Index];
        If (length(MsgStr) = 0) Then
          LogStr := ffcCRLF
        Else If (MsgStr[1] = ' ') Then
          LogStr := ffcsSpaces44 + MsgStr + ffcCRLF
        Else
          LogStr := format(ffcsFormat,
            [DateTimeToStr(Now), getTickCount,
            getCurrentThreadID, MsgStr]);

        elWritePrim(LogStr);

      End;

  Finally
    blUnlockLog;
  End;
End;
{====================================================================}

End.

