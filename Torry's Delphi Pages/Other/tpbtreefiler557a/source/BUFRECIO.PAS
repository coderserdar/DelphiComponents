{********************************************************************}
{* BUFRECIO.PAS - Buffered record I/O                               *}
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
 * Based in part on code written by Ralf Nagel
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

{--Conditional defines and compiler options}
{$I BTDefine.Inc}
{$IFDEF CanAllowOverlays}
  {$O+,F+}
{$ENDIF}


Unit BufRecIO;                                                       {!!.50}

Interface

Uses
  BTBase,
  BTIsBase,
  Filer,     {!!.TP}
  VRec;      {!!.TP}


  Procedure CreateRecBuffers ( ReadDatSLen : Word;
                               AddIFBPtr   : IsamFileBlockPtr );
    {-Allocates memory (if available) for the buffers and initializes
      private data}

  Procedure DoneRecBuffers;
    {-Flushes all write buffered data to disk and deallocates memory}

  Function DecreaseBufferMem : Boolean;
    {-Returns True, if memory consumption could be decreased}

  Procedure BufGetBlock ( Var F : IsamFile; Ref, Len : LongInt; Var Dest );
    {-Acts like IsamGetBlock, but can use a buffer}

  Procedure BufAddRec (     IFBPtr  : IsamFileBlockPtr;
                        Var Source );
    {-Acts like BTAddRec, but can use a buffer and doesn't return a
      reference}

  Procedure BufGetVariableRecRebuilt ( Var F        : IsamFile;
                                           DatSLen  : Word;
                                       Var RefNr    : LongInt;
                                           DestPtr  : Pointer;
                                           DestSize : Word;
                                       Var RecRead  : Boolean;
                                       Var Len      : Word );
    {-Reads the next (following to RefNr) variable record into the buffer
      pointed to by DestPtr; if an error 10070 or 10415 occurs, RefNr is set
      appropriately to read the next record; other errors mean abort
      reading; if no error occurs and Len returnes 0, there is no more
      record to read; if no error occurs and RecRead contains False, then
      the buffer was not large enough to hold the record; in this case Len
      contains the needed buffer size}

  Procedure BufAddVariableRecRebuilt ( IFBPtr    : IsamFileBlockPtr;
                                       SourcePtr : Pointer;
                                       Len       : Word );
    {-Acts like BTAddVariableRec, but can use a buffer and doesn't return a
      reference}

  Procedure MarkReadBufRecDeleted ( Ref : LongInt );
    {-Marks the reference Ref as deleted, if it's present in the buffer}

  Procedure RewindReadBuffer;
    {-Forces the read buffer to start at 1}

Implementation

  Const
    MaxBufferedRecLen = 16384;
    MaxBufferSize = 32768;
    MinRecsInBuffer = 2;

  type
    PioBuffer = ^TioBuffer;
    TioBuffer = array [0..MaxBufferSize-1] of byte;

  Type
    ReadBufferDescr = packed Record
      DatSLen      : Word;
      BufPtr       : PioBuffer;
      BufSize      : Word;
      LogBufSize   : Word;
        {-Number of records that can be stored in the buffer}
      LogBufStart  : LongInt;
        {-The reference belonging to the first record in the buffer}
      LogBufEnd    : Word;
        {-The number (zero based) of the first not filled record in the
          buffer; if LogBufEnd = LogBufSize, the buffer is full;
          if LogBufEnd = 0, the buffer is empty}
        {-LogBufStart + LogBufEnd is the reference of the first record
          not present in the buffer}
    End;

    AddBufferDescr = packed Record
      IFBPtr       : IsamFileBlockPtr;
        {-Used to store header information}
      DatSLen      : Word;
      BufPtr       : PioBuffer;
      BufSize      : Word;
      LogBufSize   : Word;
        {-Number of records that can be stored in the buffer}
      LogBufStart  : LongInt;
        {-The reference belonging to the first record in the buffer}
      LogBufPos    : Word;
        {-The number (zero based) of the next free record in the buffer;
          if LogBufPos = LogBufSize, the buffer is full;
          if LogBufPos = 0, the buffer is empty}
    End;

  Const
    ReadBufDescrPtr : ^ReadBufferDescr = Nil;
    AddBufDescrPtr  : ^AddBufferDescr  = Nil;


  Procedure FlushAddBuffer;

  Type
    PLongInt = ^LongInt;

  Var
    LPtr : PLongInt;

  Begin
    If AddBufDescrPtr^.LogBufPos > 0 Then Begin
      IsamPutBlock ( AddBufDescrPtr^.IFBPtr^.DatF,
          AddBufDescrPtr^.LogBufStart * AddBufDescrPtr^.DatSLen,
          AddBufDescrPtr^.DatSLen * AddBufDescrPtr^.LogBufPos,
          AddBufDescrPtr^.BufPtr^ );
      If IsamOK Then Begin
        LPtr := IsamGetNumRecAddress ( AddBufDescrPtr^.IFBPtr );
        If Not IsamOK Then Exit;
        LPtr^:= LPtr^ + AddBufDescrPtr^.LogBufPos;
        AddBufDescrPtr^.LogBufStart := AddBufDescrPtr^.LogBufStart
            + AddBufDescrPtr^.LogBufPos;
        AddBufDescrPtr^.LogBufPos := 0;
      End;
    End;
  End;


  function GetBuffer(var BufPtr : PioBuffer; AddDatSLen : longint) : integer;
                                                                {!!.52 new}
    var
      i : integer;
    begin
      i := MaxBufferSize div AddDatSLen;
      repeat
        if IsamGetMem(BufPtr, (i * AddDatSLen)) then
          begin
          GetBuffer := i;
          Exit;
          end;
        dec(i);
      until (i < MinRecsInBuffer);
      GetBuffer := 0;
    end;


  Procedure CreateRecBuffers ( ReadDatSLen : Word;
                               AddIFBPtr   : IsamFileBlockPtr );
                                                          {!!.52 rewritten}
  Var
    AddDatSLen : LongInt;
    NrRecs     : Word;

  begin
    if AddIFBPtr <> Nil then
      begin
      AddDatSLen := BTDatRecordSize ( AddIFBPtr );
      if IsamOK and (AddDatSLen <= MaxBufferedRecLen) and
         (AddDatSLen > 0) and (BTFileLen(AddIFBPtr) = 1) then
        begin
        if IsamGetMem(AddBufDescrPtr, sizeof(AddBufferDescr)) then
          begin
          NrRecs := GetBuffer(AddBufDescrPtr^.BufPtr, AddDatSLen);
          if (NrRecs <> 0) then
            with AddBufDescrPtr^ do
              begin
              BufSize := NrRecs * AddDatSLen;
              IFBPtr := AddIFBPtr;
              DatSLen := AddDatSLen;
              LogBufSize := NrRecs;
              LogBufStart := 1;
              LogBufPos := 0;
              end
          else
            begin
            FreeMem(AddBufDescrPtr, sizeof(AddBufferDescr));
            AddBufDescrPtr := nil;
            end;
          end;
        end;
      end;

    if (ReadDatSLen > 0) and (ReadDatSLen <= MaxBufferedRecLen) then
      begin
      if IsamGetMem(ReadBufDescrPtr, sizeof(ReadBufferDescr)) then
        begin
        NrRecs := GetBuffer(ReadBufDescrPtr^.BufPtr, ReadDatSLen);
        if (NrRecs <> 0) then
          with ReadBufDescrPtr^ do
            begin
            BufSize := NrRecs * ReadDatSLen;
            DatSLen := ReadDatSLen;
            LogBufSize := NrRecs;
            LogBufStart := 1;
            LogBufEnd := 0;
            end
        else
          begin
          FreeMem(ReadBufDescrPtr, sizeof(ReadBufferDescr));
          ReadBufDescrPtr := Nil;
          end;
        end;
      end;
  end;


  Procedure DoneRecBuffers;

  Begin
    IsamClearOK;
    If ReadBufDescrPtr <> Nil Then Begin
      FreeMem ( ReadBufDescrPtr^.BufPtr, ReadBufDescrPtr^.BufSize );
      FreeMem ( ReadBufDescrPtr, SizeOf (ReadBufferDescr) );
      ReadBufDescrPtr := Nil;
    End;
    If AddBufDescrPtr <> Nil Then Begin
      FlushAddBuffer;
      FreeMem ( AddBufDescrPtr^.BufPtr, AddBufDescrPtr^.BufSize );
      FreeMem ( AddBufDescrPtr, SizeOf (AddBufferDescr) );
      AddBufDescrPtr := Nil;
    End;
  End;


  Function DecreaseBufferMem : Boolean;                   {!!.52 rewritten}

  Const
    MinDecrease = 1024;

  Var
    NrRecs : Word;

  Begin
    DecreaseBufferMem := True;
    IsamClearOK;
    If ReadBufDescrPtr <> Nil Then Begin
      NrRecs := Succ (Pred (MinDecrease) Div ReadBufDescrPtr^.DatSLen);
        {-Number of records to decrease}
      If (LongInt (ReadBufDescrPtr^.LogBufSize) - NrRecs)
          * ReadBufDescrPtr^.DatSLen < MinDecrease Then Begin
        {-Too less buffer left}
        NrRecs := ReadBufDescrPtr^.LogBufSize;
      End;
      NrRecs := ReadBufDescrPtr^.LogBufSize - NrRecs;
        {-Number of records to newly allocate}
      If NrRecs < MinRecsInBuffer Then NrRecs := 0;
      FreeMem ( ReadBufDescrPtr^.BufPtr, ReadBufDescrPtr^.BufSize );
      if (NrRecs > 0) Then
        with ReadBufDescrPtr^ do
          begin
          Inc(LogBufStart, Succ(LogBufSize - NrRecs));
          {-Ensure we will not loose contact}
          BufSize := NrRecs * DatSLen;
          if IsamGetMem(BufPtr, BufSize) then
            begin
            LogBufSize := NrRecs;
            LogBufEnd := 0;
            end
          else
            NrRecs := 0;
          end;
      if (NrRecs <= 0) then
        begin
        FreeMem ( ReadBufDescrPtr, SizeOf (ReadBufferDescr) );
        ReadBufDescrPtr := Nil;
        end;
    End Else Begin
      If AddBufDescrPtr <> Nil Then Begin
        FlushAddBuffer;
        If Not IsamOK Then Exit;
        NrRecs := Succ (Pred (MinDecrease) Div AddBufDescrPtr^.DatSLen);
          {-Number of records to decrease}
        If (LongInt (AddBufDescrPtr^.LogBufSize) - NrRecs)
            * AddBufDescrPtr^.DatSLen < MinDecrease Then Begin
          {-Too less buffer left}
          NrRecs := AddBufDescrPtr^.LogBufSize;
        End;
        NrRecs := AddBufDescrPtr^.LogBufSize - NrRecs;
          {-Number of records to newly allocate}
        If NrRecs < MinRecsInBuffer Then NrRecs := 0;
        FreeMem ( AddBufDescrPtr^.BufPtr, AddBufDescrPtr^.BufSize );
        if (NrRecs > 0) then
          with AddBufDescrPtr^ do
            begin
            BufSize := NrRecs * DatSLen;
            if IsamGetMem(BufPtr, BufSize) then
              LogBufSize := NrRecs
            else
              NrRecs := 0;
            end;
        if (NrRecs <= 0) then
          begin
          FreeMem(AddBufDescrPtr, sizeof(AddBufferDescr));
          AddBufDescrPtr := Nil;
          end;
      End Else Begin
        DecreaseBufferMem := False;
      End;
    End;
  End;


  Procedure BufGetBlock ( Var F : IsamFile; Ref, Len : LongInt; Var Dest );
                                                          {!!.51 rewritten}
  Var
    BytesRead : longint;
    ReqLen,
    ReadLen      : longint;
    ReadBuffered : Boolean;

  Begin
    IsamClearOK;
    ReadBuffered := ReadBufDescrPtr <> Nil;
    If ReadBuffered Then Begin
    {--Do we have it in the buffer?}
      If ReadBufDescrPtr^.LogBufStart * ReadBufDescrPtr^.DatSLen <= Ref
          Then Begin
        {-May be there or loadable}
        If (ReadBufDescrPtr^.LogBufStart + ReadBufDescrPtr^.LogBufEnd)
            * ReadBufDescrPtr^.DatSLen > Ref Then Begin
          {-The start of the required block is in the buffer}
          If (ReadBufDescrPtr^.LogBufStart + ReadBufDescrPtr^.LogBufEnd)
              * ReadBufDescrPtr^.DatSLen <= (Ref + Len) Then Begin
            {-The end of the required block is not in the buffer; so we
              cannot have this part and the needed following part in the
              buffer at the same time; this does normally not occure;
              else everything is ok}
            ReadBuffered := False;
          End;
        End Else Begin
          {-The start of the required block is not in the buffer}
          {--Will the start of the required block be in the next buffer?}
          If (ReadBufDescrPtr^.LogBufStart + ReadBufDescrPtr^.LogBufEnd
              + ReadBufDescrPtr^.LogBufSize)
              * ReadBufDescrPtr^.DatSLen <= Ref Then Begin
            {-No, so read from disk without forwarding the buffer}
            ReadBuffered := False;
          End Else Begin
            {-Yes, so forward the buffer}
            IsamLongSeek ( F, (ReadBufDescrPtr^.LogBufStart
                + ReadBufDescrPtr^.LogBufEnd) * ReadBufDescrPtr^.DatSLen );
            If Not IsamOK Then Exit;
            ReqLen := ReadBufDescrPtr^.BufSize;
            IsamBlockReadRetLen ( F, ReadBufDescrPtr^.BufPtr^, ReqLen,
                BytesRead );
            If Not IsamOK Then Exit;
            Inc (ReadBufDescrPtr^.LogBufStart, ReadBufDescrPtr^.LogBufEnd);
            ReadBufDescrPtr^.LogBufEnd := BytesRead
                Div ReadBufDescrPtr^.DatSLen;
            If (ReadBufDescrPtr^.LogBufStart + ReadBufDescrPtr^.LogBufEnd)
                * ReadBufDescrPtr^.DatSLen < (Ref + Len) Then Begin
              {-The end of the required block is not in the buffer; so we
                failed to fill the buffer with the required data; this does
                normally only occure at the end of the file; so let the
                original reading routine set any errors;
                else everything is ok}
              ReadBuffered := False;
            End;
          End;
        End;
      End Else Begin
        {-Since we do not fill the buffer by reading backward,
          read the record from disk}
        ReadBuffered := False;
      End;
    End;
    If ReadBuffered Then
      with ReadBufDescrPtr^ do
        Move(BufPtr^[Ref - LogBufStart * DatSLen], Dest, Len)
    Else
      IsamGetBlock ( F, Ref, Len, Dest );
  End;


  Procedure BufAddRec (    IFBPtr  : IsamFileBlockPtr;
                       Var Source );

  Var
    DummyRef : LongInt;

  Begin
    If AddBufDescrPtr <> Nil Then Begin
      IsamClearOK;
      If AddBufDescrPtr^.LogBufPos = AddBufDescrPtr^.LogBufSize Then Begin
        {-Buffer is full}
        FlushAddBuffer;
        If Not IsamOK Then Exit;
      End;
      with AddBufDescrPtr^ do
        begin
        Move(Source,
             BufPtr^[LogBufPos * DatSLen],
             DatSLen);
        Inc(LogBufPos);
        end;
    End Else Begin
      IsamAddRec ( IFBPtr, DummyRef, Source );
    End;
  End;


  Procedure BufGetVariableRecRebuilt ( Var F        : IsamFile;
                                           DatSLen  : Word;
                                       Var RefNr    : LongInt;
                                           DestPtr  : Pointer;
                                           DestSize : Word;
                                       Var RecRead  : Boolean;
                                       Var Len      : Word );


    Function FindNextValidRecStart ( Var NRef : LongInt ) : Boolean;

    Var
      DelMark : LongInt;

    Begin
      FindNextValidRecStart := False;
      Repeat
        Inc (NRef);
        BufGetBlock ( F, NRef * DatSLen, SizeOf (LongInt), DelMark );
        If Not IsamOK Then Begin
          If IsamError = 10070 Then IsamClearOK;
          Exit;
        End;
      Until DelMark = 0;
      FindNextValidRecStart := True;
    End;


  Var
    Pos,
    NrOfRecs,
    IVRBPos,
    MaxRecs  : Word;
    CPtr     : ^Word;
    LPtr     : ^LongInt;
    TRef     : LongInt;

  Begin
    IsamClearOK;
    RecRead := False;

    TRef := RefNr;
    If Not FindNextValidRecStart ( TRef ) Then Begin
      {-No further data or error}
      Len := 0;
      Exit;
    End;

    RecRead := True;

    RefNr := Pred (TRef);
      {-In case we fail because of too less memory, the next trial starts
        at the best position}

    CPtr := @IVRBPtr^ [DatSLen-6];
    LPtr := @IVRBPtr^ [DatSLen-4];
    MaxRecs := (MaxVariableRecLengthM1 - (DatSLen - 6))
        Div (DatSLen - 6) + 2;
    NrOfRecs := 0;
    Pos := 0;
    LPtr^ := TRef;
    IVRBPos := 0;

    Repeat
      BufGetBlock ( F, LPtr^ * DatSLen, DatSLen, IVRBPtr^ );
      If Not IsamOK Then Begin
        If IsamError = 10070 Then RefNr := TRef;
        Exit;
      End;
      If (LongInt (Pos) + CPtr^) > DestSize Then RecRead := False;
      If RecRead Then Move ( IVRBPtr^ [IVRBPos],
          PIsamVRecBuf (DestPtr)^ [Pos], CPtr^ );
      If Pos = 0 Then Inc (IVRBPos);
      Inc (Pos, CPtr^);
      Inc (NrOfRecs);
    Until (LPtr^ = 0) Or (NrOfRecs > MaxRecs);

    If (NrOfRecs > MaxRecs) Or (Pos = 0) Then Begin
      IsamOK := False;
      IsamError := 10415;
      RefNr := TRef;
      Exit;
    End;
    Len := Pos;
    If RecRead Then RefNr := TRef;
  End;


  Procedure BufAddVariableRecRebuilt ( IFBPtr    : IsamFileBlockPtr;
                                       SourcePtr : Pointer;
                                       Len       : Word );


    Function GetNextNewLogRecNr : LongInt;

    Begin
      If AddBufDescrPtr <> Nil Then Begin
        GetNextNewLogRecNr := AddBufDescrPtr^.LogBufStart
            + AddBufDescrPtr^.LogBufPos;
      End Else Begin
        GetNextNewLogRecNr := BTFileLen ( IFBPtr );
      End;
    End;


    Function GetDatSLen : Word;

    Begin
      If AddBufDescrPtr <> Nil Then Begin
        GetDatSLen := AddBufDescrPtr^.DatSLen;
      End Else Begin
        GetDatSLen := BTDatRecordSize ( IFBPtr );
      End;
    End;


  Var
    NextLogRec : LongInt;
    Pos,
    CurCont,
    IVRBPos,
    AddDatSLen : Word;
    CPtr       : ^Word;
    LPtr       : ^LongInt;

  Begin
    IsamClearOK;

    AddDatSLen := GetDatSLen;
    If Not IsamOK Then Exit;

    CPtr := @IVRBPtr^ [AddDatSLen-6];
    LPtr := @IVRBPtr^ [AddDatSLen-4];
    Pos := 0;
    IVRBPos := 0;

    While Len > 0 Do Begin
      If Pos = 0 Then Begin
        CurCont := AddDatSLen - 6;
      End Else Begin
        CurCont := AddDatSLen - 7;
        IVRBPtr^ [0] := 1;
      End;
      If CurCont > Len Then CurCont := Len;
      Move ( PIsamVRecBuf (SourcePtr)^ [Pos], IVRBPtr^ [IVRBPos], CurCont );
      CPtr^ := CurCont;
      Dec (Len, CurCont);
      If Len = 0 Then Begin
        LPtr^ := 0;
      End Else Begin
        LPtr^ := Succ (GetNextNewLogRecNr);
        If Not IsamOK Then Exit;
      End;

      BufAddRec ( IFBPtr, IVRBPtr^ );
      If Not IsamOK Then Exit;

      If Pos = 0 Then Inc (IVRBPos);
      Inc (Pos, CurCont);
    End;
  End;


  Procedure MarkReadBufRecDeleted ( Ref : LongInt );

  Type
    PByte = ^Byte;

  Begin
    If ReadBufDescrPtr <> Nil Then Begin
      If (Ref >= ReadBufDescrPtr^.LogBufStart)
          And (Ref < (ReadBufDescrPtr^.LogBufStart
          + ReadBufDescrPtr^.LogBufEnd)) Then Begin
        {-The record is buffered}
        with ReadBufDescrPtr^ do
          BufPtr^[(Ref - LogBufStart) * DatSLen] := 1;
      End;
    End;
  End;


  Procedure RewindReadBuffer;

  Begin
    If ReadBufDescrPtr <> Nil Then Begin
      If ReadBufDescrPtr^.LogBufStart <> 1 Then Begin
        ReadBufDescrPtr^.LogBufStart := 1;
        ReadBufDescrPtr^.LogBufEnd := 0;
      End
    End;
  End;


{$IFDEF InitAllUnits}
Begin
{$ENDIF}
End.