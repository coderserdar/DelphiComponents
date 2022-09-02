{$I fsdefine.inc}
{.$DEFINE Tracing}

Unit fsfile;

Interface

Uses
  Windows,
  SysUtils,
  fsconst,
  fsllbase,
  fssrmgr,
  fsllexcp,
  fssrbase;

Procedure FileProcsInitialize;

{$IFDEF Tracing}
{---File Access Tracing---}
Type
  TffTraceString = String[59];
Procedure FFStartTracing(BufferSize: Longint);
Procedure FFDumpTrace(FileName: String);
Procedure FFAddUserTrace(Const ParamRec; PRSize: Word);
Procedure FFAddUserTraceStr(Const S: TffTraceString);
{$ENDIF}

Implementation

{$IFDEF Tracing}
Type
  TffFileOp = (foUnknown, foClose, foFlush, foLock, foOpen, foRead,
    foSeek, foSetEOF, foUnlock, foWrite, foGeneral,
    foUserTrace, foUserTraceStr);

Procedure FFAddTrace(Op: TffFileOp; Const ParamRec; PRSize: Word); Forward;
{$ENDIF}

{===File Access Primitives===========================================}
{$I FsFile.INC}
{====================================================================}

{$IFDEF Tracing}
{===File Access Tracing==============================================}
Type
  PTraceBuffer = ^TTraceBuffer;
  TTraceBuffer = Array[0..32767] Of Byte;
  TTraceEntry = Record
    teWhat: Word;
    teSize: Word;
    teTime: TffWord32;
  End;
Var
  TraceBuffer: PTraceBuffer;
  TBSize: Longint;
  TBHead: Longint;
  TBTail: Longint;
  TracePadlock: TffPadlock;
  {--------}

Procedure FFStartTracing(BufferSize: Longint);
Const
  MaxBufferSize = 64 * 1024;
Begin
  If (TraceBuffer = Nil) Then
    Begin
      If (BufferSize <= 0) Then
        TBSize := 1024
      Else If (BufferSize > MaxBufferSize) Then
        TBSize := MaxBufferSize
      Else
        TBSize := (BufferSize + 1023) And (Not 1023);
      GetMem(TraceBuffer, TBSize);
    End;
  TBHead := 0;
  TBTail := 0;
  TracePadLock := TffPadlock.Create;
End;
{--------}

Procedure FFDumpTrace(FileName: String);
Type
  PHandyBuffer = ^THandyBuffer;
  THandyBuffer = Record
    Case Byte Of
      0: (L: Array[0..127] Of Longint);
      1: (B: Array[0..511] Of Byte);
      2: (C: Array[0..511] Of AnsiChar);
      3: (S: String[255]);
  End;
  {------}

  Procedure Read4Bytes(Var B);
  Begin
    Move(TraceBuffer^[TBTail], B, 4);
    inc(TBTail, 4);
    If (TBTail >= TBSize) Then
      dec(TBTail, TBSize);
  End;
  {------}

  Procedure GrowBuffer(Var GB: PHandyBuffer; Var CurSize: Word; NewSize: Word);
  Begin
    If (NewSize > CurSize) Then
      Begin
        If (GB <> Nil) Then
          FreeMem(GB, CurSize);
        GetMem(GB, NewSize);
        CurSize := NewSize;
      End;
  End;
  {------}

  Procedure PrintEntry(Var F: text; Const TE: TTraceEntry; GB: PHandyBuffer);
  Var
    FileName: TffMaxPathZ;
    Offset: Integer;
    RemBytes: Integer;
    i, j: Integer;
  Begin
    {print the time in hex}
    write(F, Format('%x8', [TE.teTime]));
    {print the rest}
    Case TffFileOp(TE.teWhat) Of
      foUnknown:
        Begin
          If (((TE.teSize + 3) And $FFFC) = 4) Then
            writeln(F, Format('  ..(result): %d ($%0:x)', [GB^.L[0]]))
          Else
            writeln(F, '  [unknown]');
        End;
      foGeneral:
        Begin
          writeln(F, '  [general]');
        End;
      foOpen:
        Begin
          writeln(F, '  [open file]');
          StrCopy(FileName, @GB^.L[0]);
          writeln(F, Format('          ..name: %s', [FileName]));
        End;
      foSeek:
        Begin
          writeln(F, '  [position file]');
          writeln(F, Format('          ..handle: %d ($%0:x)', [GB^.L[0]]));
          If (GB^.L[1] = -1) Then
            writeln(F, '          ..position: End-Of-File')
          Else
            writeln(F, Format('          ..position: %d ($%0:x)', [GB^.L[1]]));
        End;
      foSetEOF:
        Begin
          writeln(F, '  [truncate file]');
          writeln(F, Format('          ..handle: %d ($%0:x)', [GB^.L[0]]));
          writeln(F, Format('          ..position: %d ($%0:x)', [GB^.L[1]]));
        End;
      foFlush:
        Begin
          writeln(F, '  [flush file]');
          writeln(F, Format('          ..handle: %d ($%0:x)', [GB^.L[0]]));
        End;
      foRead:
        Begin
          writeln(F, '  [read file]');
          writeln(F, Format('          ..handle: %d ($%0:x)', [GB^.L[0]]));
          writeln(F, Format('          ..bytes to read: %d ($%0:x)', [GB^.L[1]]));
        End;
      foWrite:
        Begin
          writeln(F, '  [write file]');
          writeln(F, Format('          ..handle: %d ($%0:x)', [GB^.L[0]]));
          writeln(F, Format('          ..bytes to write: %d ($%0:x)', [GB^.L[1]]));
        End;
      foLock:
        Begin
          writeln(F, '  [lock file]');
          writeln(F, Format('          ..handle: %d ($%0:x)', [GB^.L[0]]));
          writeln(F, Format('          ..offset: %d ($%0:x)', [GB^.L[1]]));
          writeln(F, Format('          ..bytes to lock: %d ($%0:x)', [GB^.L[2]]));
        End;
      foUnlock:
        Begin
          writeln(F, '  [unlock file]');
          writeln(F, Format('          ..handle: %d ($%0:x)', [GB^.L[0]]));
          writeln(F, Format('          ..offset: %d ($%0:x)', [GB^.L[1]]));
          writeln(F, Format('          ..bytes to unlock: %d ($%0:x)', [GB^.L[2]]));
        End;
      foClose:
        Begin
          writeln(F, '  [close file]');
          writeln(F, Format('          ..handle: %d ($%0:x)', [GB^.L[0]]));
        End;
      foUserTrace:
        Begin
          writeln(F, Format('  [user trace entry], %d bytes', [TE.teSize]));
          Offset := 0;
          If (TE.teSize >= 8) Then
            For i := 0 To pred(TE.teSize Div 8) Do
              Begin
                write(F, '          ');
                For j := 0 To 7 Do
                  write(F, Format('%.2x ', [GB^.B[Offset + j]]));
                write(F, '  [');
                For j := 0 To 7 Do
                  write(F, Format('%s', [GB^.C[Offset + j]]));
                writeln(F, ']');
                inc(Offset, 8);
              End;
          RemBytes := TE.teSize Mod 8;
          If (RemBytes > 0) Then
            Begin
              write(F, '          ');
              For j := 0 To pred(RemBytes) Do
                write(F, Format('%.2x ', [GB^.B[Offset + j]]));
              For j := RemBytes To 7 Do
                write(F, '   ');
              write(F, '  [');
              For j := 0 To pred(RemBytes) Do
                write(F, Format('%s', [GB^.C[Offset + j]]));
              For j := RemBytes To 7 Do
                write(F, ' ');
              writeln(F, ']');
            End;
        End;
      foUserTraceStr:
        Begin
          writeln(F, Format('  [USER: %s]', [GB^.S]));
        End;
    End; {case}
  End;
  {------}
Var
  F: text;
  GenBuf: PHandyBuffer;
  GenBufSize: Word;
  TraceEntry: TTraceEntry;
  AdjSize: Word;
  i: Word;
Begin
  If (TraceBuffer <> Nil) Then
    Begin
      {..write it to file..}
      GenBuf := Nil;
      GenBufSize := 0;
      System.Assign(F, FileName);
      System.Rewrite(F);
      If (TBTail = TBHead) Then
        writeln(F, '***no entries***')
      Else
        Repeat
          Read4Bytes(TraceEntry);
          Read4Bytes(TraceEntry.teTime);
          AdjSize := (TraceEntry.teSize + 3) And $FFFC;
          GrowBuffer(GenBuf, GenBufSize, AdjSize);
          For i := 0 To pred(AdjSize Div 4) Do
            Read4Bytes(GenBuf^.L[i]);
          PrintEntry(F, TraceEntry, GenBuf);
        Until TBTail = TBHead;
      System.Close(F);
      FreeMem(GenBuf, GenBufSize);
      FreeMem(TraceBuffer, TBSize);
      TraceBuffer := Nil;
      TracePadLock.Free;
    End;
End;
{--------}

Procedure FFAddTrace(Op: TffFileOp; Const ParamRec; PRSize: Word);
{------}

Procedure Write4Bytes(Const B);
  Begin
    Move(B, TraceBuffer^[TBHead], 4);
    inc(TBHead, 4);
    If (TBHead >= TBSize) Then
      dec(TBHead, TBSize);
  End;
  {------}

  Procedure WriteXBytes(Const B; Size: Word);
  Begin
    FillChar(TraceBuffer^[TBHead], 4, 0);
    Move(B, TraceBuffer^[TBHead], Size);
    inc(TBHead, 4);
    If (TBHead >= TBSize) Then
      dec(TBHead, TBSize);
  End;
  {------}
Var
  TraceEntry: TTraceEntry;
  AdjSize: Word;
  i: Word;
  BytesFree: Longint;
  PRasLongints: Array[1..128] Of Longint Absolute ParamRec;
Begin
  If (TraceBuffer <> Nil) Then
    Begin
      {calc the size rounded to nearest 4 bytes}
      AdjSize := (PRSize + 3) And $FFFC;
      {make sure that there's enough space in the trace buffer}
      Repeat
        {calculate the number of bytes free in the trace buffer}
        If (TBTail = TBHead) Then
          BytesFree := TBSize
        Else If (TBTail < TBHead) Then
          BytesFree := (TBSize - TBHead) + TBTail
        Else
          BytesFree := TBTail - TBHead;
        {if not enough room for this entry..}
        If (BytesFree <= AdjSize + sizeof(TraceEntry)) Then
          Begin
            {..advance TBTail over oldest entry}
            Move(TraceBuffer^[TBTail], TraceEntry, 4);
            inc(TBTail, ((TraceEntry.teSize + 3) And $FFFC) + sizeof(TraceEntry));
            If (TBTail >= TBSize) Then
              dec(TBTail, TBSize);
          End;
      Until (BytesFree > AdjSize + sizeof(TraceEntry));
      With TraceEntry Do
        Begin
          teWhat := ord(Op);
          teSize := PRSize;
          teTime := GetTickCount;
        End;
      Write4Bytes(TraceEntry);
      Write4Bytes(TraceEntry.teTime);
      For i := 1 To pred(AdjSize Div 4) Do
        Write4Bytes(PRasLongints[i]);
      If (AdjSize = PRSize) Then
        Write4Bytes(PRasLongints[AdjSize Div 4])
      Else
        WriteXBytes(PRasLongints[AdjSize Div 4], 4 + PRSize - AdjSize);
    End;
End;
{--------}

Procedure FFGetTraceAccess;
Begin
  TracePadLock.Locked := True;
End;
{--------}

Procedure FFFreeTraceAccess;
Begin
  TracePadLock.Locked := False;
End;
{--------}

Procedure FFAddUserTrace(Const ParamRec; PRSize: Word);
Begin
  If (TraceBuffer <> Nil) Then
    Begin
      FFGetTraceAccess;
      If (PRSize > 128) Then
        PRSize := 128;
      FFAddTrace(foUserTrace, ParamRec, PRSize);
      FFFreeTraceAccess;
    End;
End;
{--------}

Procedure FFAddUserTraceStr(Const S: TffTraceString);
Begin
  If (TraceBuffer <> Nil) Then
    Begin
      FFGetTraceAccess;
      FFAddTrace(foUserTraceStr, S, length(S) + 1);
      FFFreeTraceAccess;
    End;
End;
{====================================================================}
{$ENDIF}

{===Unit initialization==============================================}

Procedure FileProcsInitialize;
Begin
  FFCloseFilePrim := FFCloseFilePrim32;
  FFFlushFilePrim := FFFlushFilePrim32;
  FFGetPositionFilePrim := FFGetPositionFilePrim32;
  //  FFLockFilePrim := FFLockFilePrim32;
  FFOpenFilePrim := FFOpenFilePrim32;
  FFPositionFilePrim := FFPositionFilePrim32;
  FFPositionFileEOFPrim := FFPositionFileEOFPrim32;
  FFReadFilePrim := FFReadFilePrim32;
  FFSetEOFPrim := FFSetEOFPrim32;
  FFSleepPrim := FFSleepPrim32;
  //  FFUnlockFilePrim := FFUnlockFilePrim32;
  FFWriteFilePrim := FFWriteFilePrim32;
  {$IFDEF Tracing}
  TraceBuffer := Nil;
  {$ENDIF}
End;

End.

