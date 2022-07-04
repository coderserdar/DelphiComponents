{*********************************************************}
{* FlashFiler: Low level file I/O routines               *}
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
{.$DEFINE Tracing}

unit fffile;

interface

uses
  Windows,
  SysUtils,
  ffconst,
  ffllbase,
  ffsrmgr,
  ffllexcp,
  ffsrbase;

procedure FileProcsInitialize;

{$IFDEF Tracing}
{---File Access Tracing---}
type
  TffTraceString = string[59];
procedure FFStartTracing(BufferSize : longint);
procedure FFDumpTrace(FileName : string);
procedure FFAddUserTrace(const ParamRec; PRSize : word);
procedure FFAddUserTraceStr(const S : TffTraceString);
{$ENDIF}

implementation

{$IFDEF Tracing}
type
  TffFileOp = (foUnknown, foClose, foFlush, foLock, foOpen, foRead,
               foSeek, foSetEOF, foUnlock, foWrite, foGeneral,
               foUserTrace, foUserTraceStr);

procedure FFAddTrace(Op : TffFileOp; const ParamRec; PRSize : word); forward;
{$ENDIF}


{===File Access Primitives===========================================}
{$I FFFile.INC}
{====================================================================}


{$IFDEF Tracing}
{===File Access Tracing==============================================}
type
  PTraceBuffer = ^TTraceBuffer;
  TTraceBuffer = array [0..32767] of byte;
  TTraceEntry = record
    teWhat : word;
    teSize : word;
    teTime : TffWord32;
  end;
var
  TraceBuffer : PTraceBuffer;
  TBSize : longint;
  TBHead : longint;
  TBTail : longint;
  TracePadlock : TffPadlock;
{--------}
procedure FFStartTracing(BufferSize : longint);
  const
    MaxBufferSize = 64*1024;
  begin
    if (TraceBuffer = nil) then
      begin
        if (BufferSize <= 0) then
          TBSize := 1024
        else if (BufferSize > MaxBufferSize) then
          TBSize := MaxBufferSize
        else
          TBSize := (BufferSize + 1023) and (not 1023);
        GetMem(TraceBuffer, TBSize);
      end;
    TBHead := 0;
    TBTail := 0;
    TracePadLock := TffPadlock.Create;
  end;
{--------}
procedure FFDumpTrace(FileName : string);
  type
    PHandyBuffer = ^THandyBuffer;
    THandyBuffer = record
      case byte of
        0 : (L : array [0..127] of longint);
        1 : (B : array [0..511] of byte);
        2 : (C : array [0..511] of AnsiChar);
        3 : (S : string[255]);
    end;
  {------}
  procedure Read4Bytes(var B);
    begin
      Move(TraceBuffer^[TBTail], B, 4);
      inc(TBTail, 4);
      if (TBTail >= TBSize) then
        dec(TBTail, TBSize);
    end;
  {------}
  procedure GrowBuffer(var GB : PHandyBuffer; var CurSize : word; NewSize : word);
    begin
      if (NewSize > CurSize) then
        begin
          if (GB <> nil) then
            FreeMem(GB, CurSize);
          GetMem(GB, NewSize);
          CurSize := NewSize;
        end;
    end;
  {------}
  procedure PrintEntry(var F : text; const TE : TTraceEntry; GB : PHandyBuffer);
    var
      FileName : TffMaxPathZ;
      Offset   : integer;
      RemBytes : integer;
      i, j     : integer;
    begin
      {print the time in hex}
      write(F, Format('%x8', [TE.teTime]));
      {print the rest}
      case TffFileOp(TE.teWhat) of
        foUnknown :
          begin
            if (((TE.teSize+3) and $FFFC) = 4) then
              writeln(F, Format('  ..(result): %d ($%0:x)', [GB^.L[0]]))
            else
              writeln(F, '  [unknown]');
          end;
        foGeneral :
          begin
            writeln(F, '  [general]');
          end;
        foOpen :
          begin
            writeln(F, '  [open file]');
            StrCopy(FileName, @GB^.L[0]);
            writeln(F, Format('          ..name: %s', [FileName]));
          end;
        foSeek :
          begin
            writeln(F, '  [position file]');
            writeln(F, Format('          ..handle: %d ($%0:x)', [GB^.L[0]]));
            if (GB^.L[1] = -1) then
              writeln(F, '          ..position: End-Of-File')
            else
              writeln(F, Format('          ..position: %d ($%0:x)', [GB^.L[1]]));
          end;
        foSetEOF :
          begin
            writeln(F, '  [truncate file]');
            writeln(F, Format('          ..handle: %d ($%0:x)', [GB^.L[0]]));
            writeln(F, Format('          ..position: %d ($%0:x)', [GB^.L[1]]));
          end;
        foFlush :
          begin
            writeln(F, '  [flush file]');
            writeln(F, Format('          ..handle: %d ($%0:x)', [GB^.L[0]]));
          end;
        foRead :
          begin
            writeln(F, '  [read file]');
            writeln(F, Format('          ..handle: %d ($%0:x)', [GB^.L[0]]));
            writeln(F, Format('          ..bytes to read: %d ($%0:x)', [GB^.L[1]]));
          end;
        foWrite :
          begin
            writeln(F, '  [write file]');
            writeln(F, Format('          ..handle: %d ($%0:x)', [GB^.L[0]]));
            writeln(F, Format('          ..bytes to write: %d ($%0:x)', [GB^.L[1]]));
          end;
        foLock :
          begin
            writeln(F, '  [lock file]');
            writeln(F, Format('          ..handle: %d ($%0:x)', [GB^.L[0]]));
            writeln(F, Format('          ..offset: %d ($%0:x)', [GB^.L[1]]));
            writeln(F, Format('          ..bytes to lock: %d ($%0:x)', [GB^.L[2]]));
          end;
        foUnlock :
          begin
            writeln(F, '  [unlock file]');
            writeln(F, Format('          ..handle: %d ($%0:x)', [GB^.L[0]]));
            writeln(F, Format('          ..offset: %d ($%0:x)', [GB^.L[1]]));
            writeln(F, Format('          ..bytes to unlock: %d ($%0:x)', [GB^.L[2]]));
          end;
        foClose :
          begin
            writeln(F, '  [close file]');
            writeln(F, Format('          ..handle: %d ($%0:x)', [GB^.L[0]]));
          end;
        foUserTrace :
          begin
            writeln(F, Format('  [user trace entry], %d bytes', [TE.teSize]));
            Offset := 0;
            if (TE.teSize >= 8) then
              for i := 0 to pred(TE.teSize div 8) do
                begin
                  write(F, '          ');
                  for j := 0 to 7 do
                    write(F, Format('%.2x ', [GB^.B[Offset+j]]));
                  write(F, '  [');
                  for j := 0 to 7 do
                    write(F, Format('%s', [GB^.C[Offset+j]]));
                  writeln(F, ']');
                  inc(Offset, 8);
                end;
            RemBytes := TE.teSize mod 8;
            if (RemBytes > 0) then
              begin
                write(F, '          ');
                for j := 0 to pred(RemBytes) do
                  write(F, Format('%.2x ', [GB^.B[Offset+j]]));
                for j := RemBytes to 7 do
                  write(F, '   ');
                write(F, '  [');
                for j := 0 to pred(RemBytes) do
                  write(F, Format('%s', [GB^.C[Offset+j]]));
                for j := RemBytes to 7 do
                  write(F, ' ');
                writeln(F, ']');
              end;
          end;
        foUserTraceStr :
          begin
            writeln(F, Format('  [USER: %s]', [GB^.S]));
          end;
      end;{case}
    end;
  {------}
  var
    F : text;
    GenBuf : PHandyBuffer;
    GenBufSize : word;
    TraceEntry : TTraceEntry;
    AdjSize    : word;
    i          : word;
  begin
    if (TraceBuffer <> nil) then
      begin
        {..write it to file..}
        GenBuf := nil;
        GenBufSize := 0;
        System.Assign(F, FileName);
        System.Rewrite(F);
        if (TBTail = TBHead) then
          writeln(F, '***no entries***')
        else
          repeat
            Read4Bytes(TraceEntry);
            Read4Bytes(TraceEntry.teTime);
            AdjSize := (TraceEntry.teSize + 3) and $FFFC;
            GrowBuffer(GenBuf, GenBufSize, AdjSize);
            for i := 0 to pred(AdjSize div 4) do
              Read4Bytes(GenBuf^.L[i]);
            PrintEntry(F, TraceEntry, GenBuf);
          until TBTail = TBHead;
        System.Close(F);
        FreeMem(GenBuf, GenBufSize);
        FreeMem(TraceBuffer, TBSize);
        TraceBuffer := nil;
        TracePadLock.Free;
      end;
  end;
{--------}
procedure FFAddTrace(Op : TffFileOp; const ParamRec; PRSize : word);
  {------}
  procedure Write4Bytes(const B);
    begin
      Move(B, TraceBuffer^[TBHead], 4);
      inc(TBHead, 4);
      if (TBHead >= TBSize) then
        dec(TBHead, TBSize);
    end;
  {------}
  procedure WriteXBytes(const B; Size : word);
    begin
      FillChar(TraceBuffer^[TBHead], 4, 0);
      Move(B, TraceBuffer^[TBHead], Size);
      inc(TBHead, 4);
      if (TBHead >= TBSize) then
        dec(TBHead, TBSize);
    end;
  {------}
  var
    TraceEntry : TTraceEntry;
    AdjSize : word;
    i       : word;
    BytesFree : longint;
    PRasLongints : array [1..128] of longint absolute ParamRec;
  begin
    if (TraceBuffer <> nil) then
      begin
        {calc the size rounded to nearest 4 bytes}
        AdjSize := (PRSize + 3) and $FFFC;
        {make sure that there's enough space in the trace buffer}
        repeat
          {calculate the number of bytes free in the trace buffer}
          if (TBTail = TBHead) then
            BytesFree := TBSize
          else if (TBTail < TBHead) then
            BytesFree := (TBSize - TBHead) + TBTail
          else
            BytesFree := TBTail - TBHead;
          {if not enough room for this entry..}
          if (BytesFree <= AdjSize + sizeof(TraceEntry)) then
            begin
              {..advance TBTail over oldest entry}
              Move(TraceBuffer^[TBTail], TraceEntry, 4);
              inc(TBTail, ((TraceEntry.teSize + 3) and $FFFC) + sizeof(TraceEntry));
              if (TBTail >= TBSize) then
                dec(TBTail, TBSize);
            end;
        until (BytesFree > AdjSize + sizeof(TraceEntry));
        with TraceEntry do
          begin
            teWhat := ord(Op);
            teSize := PRSize;
            teTime := GetTickCount;
          end;
        Write4Bytes(TraceEntry);
        Write4Bytes(TraceEntry.teTime);
        for i := 1 to pred(AdjSize div 4) do
          Write4Bytes(PRasLongints[i]);
        if (AdjSize = PRSize) then
          Write4Bytes(PRasLongints[AdjSize div 4])
        else
          WriteXBytes(PRasLongints[AdjSize div 4], 4 + PRSize - AdjSize);
      end;
  end;
{--------}
procedure FFGetTraceAccess;
  begin
    TracePadLock.Locked := true;
  end;
{--------}
procedure FFFreeTraceAccess;
  begin
    TracePadLock.Locked := false;
  end;
{--------}
procedure FFAddUserTrace(const ParamRec; PRSize : word);
  begin
    if (TraceBuffer <> nil) then
      begin
        FFGetTraceAccess;
        if (PRSize > 128) then
          PRSize := 128;
        FFAddTrace(foUserTrace, ParamRec, PRSize);
        FFFreeTraceAccess;
      end;
  end;
{--------}
procedure FFAddUserTraceStr(const S : TffTraceString);
  begin
    if (TraceBuffer <> nil) then
      begin
        FFGetTraceAccess;
        FFAddTrace(foUserTraceStr, S, length(S)+1);
        FFFreeTraceAccess;
      end;
  end;
{====================================================================}
{$ENDIF}


{===Unit initialization==============================================}
procedure FileProcsInitialize;
begin
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
  TraceBuffer := nil;
  {$ENDIF}
end;

end.
