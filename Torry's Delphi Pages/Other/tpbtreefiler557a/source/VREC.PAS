{********************************************************************}
{* VREC.PAS - B-Tree Filer variable-length record support           *}
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

Unit
  VRec;

interface

uses
  BTBase,
  BTIsBase,
  {$IFDEF UsingDelphi}
  SysUtils,
  {$ENDIF}
  Filer;     {!!.TP}

const
  MaxVariableRecLength = $FFF0;
{--Internal use only}                                            {!!.50mov}
  MaxVariableRecLengthM1 = MaxVariableRecLength - 1;             {!!.50mov}

{--Internal use only}                                            {!!.50mov}
type                                                             {!!.50mov}
  IsamVRecBuf  = Array [0..MaxVariableRecLengthM1] of Byte;      {!!.50mov}
  PIsamVRecBuf = ^IsamVRecBuf;                                      {!!.50}

{--Internal use only}                                            {!!.50mov}
var
  IVRBPtr         : PIsamVRecBuf;                                {!!.50mov}
  IsamVRecBufSize : Word;


function BTCreateVariableRecBuffer(IFBPtr : IsamFileBlockPtr) : Boolean;
  {-Create a variable record buffer for the fileblock with the greatest
    data record length of all variable record fileblocks}

procedure BTReleaseVariableRecBuffer;
  {-Release the variable record buffer}

function BTSetVariableRecBuffer(Size : Word) : Boolean;
  {-Create a variable record buffer with Size bytes as an alternative to
    CreateVariableRecBuffer}

procedure BTAddVariableRec(IFBPtr : IsamFileBlockPtr;
                       var RefNr  : LongInt;
                       var Source;
                           Len    : Word);
  {-Adds a record with variable length}

procedure BTDeleteVariableRec(IFBPtr : IsamFileBlockPtr;
                              RefNr  : LongInt);
  {-Deletes a record with variable length}

procedure BTPutVariableRec(IFBPtr : IsamFileBlockPtr;
                           RefNr  : LongInt;
                       var Source;
                           Len    : Word);
  {-Puts a record with variable length to its old place}

procedure BTGetVariableRecPart(IFBPtr : IsamFileBlockPtr;
                               RefNr  : LongInt;
                           var Dest;
                           var Len    : Word);
  {-Reads at maximum Len bytes of a variable record}

procedure BTGetVariableRec(IFBPtr : IsamFileBlockPtr;
                           RefNr  : LongInt;
                       var Dest;
                       var Len    : Word);
  {-Reads a variable record and returns the read number of bytes in Len}

procedure BTGetVRecPartReadOnly(IFBPtr : IsamFileBlockPtr;
                                RefNr  : LongInt;
                            var Dest;
                            var Len    : Word);
  {-Reads a variable record and returns the read number of bytes in Len;
    needs either a record lock or another lock, else will do a readlock}

procedure BTGetVRecReadOnly(IFBPtr : IsamFileBlockPtr;
                            RefNr  : LongInt;
                        var Dest;
                        var Len    : Word);
  {-Reads at maximum Len bytes of a variable record;
    needs either a record lock or another lock, else will do a readlock}

procedure BTGetVariableRecLength(IFBPtr : IsamFileBlockPtr;
                                 RefNr  : LongInt;
                             var Len    : Word);
  {-Gets the length of a variable record and returns it in Len}


function BTAdjustVariableRecBuffer(Size : Word) : Boolean;     {!!.50}
  {-Increases the variable record buffer if necessary}

                                       {!!.50del BTGetVariableRecRebuilt}


implementation

{-Types, constants and variables moved to the interface}       {!!.50}


function CreateVariableRecBuffer(S : Word) : Boolean;   {!!.52 rewritten}
begin
  if not IsamGetMem(IVRBPtr, S) then
    CreateVariableRecBuffer := False
  else begin
    IsamVRecBufSize := S;
    CreateVariableRecBuffer := True;
  end;
end;


function CreateVariableRecBufferIFB(IFBPtr : IsamFileBlockPtr) : Boolean;
begin
  CreateVariableRecBufferIFB := CreateVariableRecBuffer
     (Word (ILI (IFBPtr^.DIDPtr^[0]^.LenRec).Lo));
end;


function BTCreateVariableRecBuffer(IFBPtr : IsamFileBlockPtr) : Boolean;
begin
  IsamEntryCode(IFBPtr, NoOptions);                            {!!.50}
  if IsamOK then begin                                         {!!.50}
    BTCreateVariableRecBuffer := CreateVariableRecBufferIFB(IFBPtr);
  end
  else begin                                                   {!!.50}
    BTCreateVariableRecBuffer := False;                        {!!.50}
  end;                                                         {!!.50}
  IsamExitCode(IFBPtr);                                        {!!.50}
end;


procedure ReleaseVariableRecBuffer;
begin
  if IsamVRecBufSize <> 0 then begin
    FreeMem(IVRBPtr, IsamVRecBufSize);
    IsamVRecBufSize := 0;
  end;
end;


procedure BTReleaseVariableRecBuffer;
begin
  IsamEntryCode(Pointer (NotAFileBlockPtr), NoOptions);        {!!.50}
  if IsamOK then
    ReleaseVariableRecBuffer;                                  {!!.50}
  IsamExitCode(Pointer (NotAFileBlockPtr));                    {!!.50}
end;


function BTSetVariableRecBuffer(Size : Word) : Boolean;
begin
  IsamEntryCode(Pointer (NotAFileBlockPtr), NoOptions);        {!!.50}
  if IsamOK then begin                                         {!!.50}
    BTSetVariableRecBuffer := CreateVariableRecBuffer(Size);
  end
  else begin                                                   {!!.50}
    BTSetVariableRecBuffer := False;                           {!!.50}
  end;                                                         {!!.50}
  IsamExitCode(Pointer (NotAFileBlockPtr));                    {!!.50}
end;


function AdjustVariableRecBuffer(Size : Word) : Boolean;       {!!.50}
begin
  if IsamVRecBufSize < Size then begin
    ReleaseVariableRecBuffer;
    AdjustVariableRecBuffer := CreateVariableRecBuffer(Size);
  end
  else begin
    AdjustVariableRecBuffer := True;
  end;
end;


function BTAdjustVariableRecBuffer(Size : Word) : Boolean;
begin
  IsamEntryCode(Pointer (NotAFileBlockPtr), NoOptions);        {!!.50}
  if IsamOK then begin                                         {!!.50}
    BTAdjustVariableRecBuffer := AdjustVariableRecBuffer(Size);{!!.50}
  end
  else begin                                                   {!!.50}
    BTAdjustVariableRecBuffer := False;                        {!!.50}
  end;                                                         {!!.50}
  IsamExitCode(Pointer (NotAFileBlockPtr));                    {!!.50}
end;


procedure IsamAddRestVarRec(IFBPtr : IsamFileBlockPtr;
                        var RefNr  : LongInt;
                        var Source;
                            Len    : Word);
var
  SPtr   : PIsamVRecBuf;                                       {!!.50}
  CPtr   : ^Word;
  LPtr   : ^LongInt;
  SetLen,
  Pos,
  Number : Word;
  Prev   : LongInt;
begin
  SetLen := Word(ILI (IFBPtr^.DIDPtr^[0]^.LenRec).Lo) - 7;
  Number := Len Div SetLen;
  if Number * SetLen <> Len then begin
    Inc (Number);
  end;
  Pos := Pred (Number) * SetLen;
  Prev := 0;
  CPtr := Addr(IVRBPtr^ [Succ (SetLen)]);
  LPtr := Addr(IVRBPtr^ [SetLen+3]);
  SPtr := Addr(Source);
  while Number > 0 do begin
    IVRBPtr^ [0] := 1;
    if Prev = 0 then begin
      CPtr^ := Len-Pos;
    end
    else begin
      CPtr^ := SetLen;
    end;
    Move(SPtr^ [Pos], IVRBPtr^ [1], CPtr^);
    LPtr^ := Prev;
    IsamAddRec(IFBPtr, Prev, IVRBPtr^);                        {!!.50}
    if not IsamOK then Exit;
    Dec (Number);
    if Number <> 0 then
      Pos := Pos-SetLen;
  end;
  RefNr := Prev;
end;


procedure BTAddVariableRec(IFBPtr : IsamFileBlockPtr;
                       var RefNr  : LongInt;
                       var Source;
                           Len    : Word);
  {------}
  procedure IsamAddVariableRec;                                {!!.50}
  var
    S    : Word;
    SPtr : PIsamVRecBuf;                                       {!!.50}
    CPtr : ^Word;
    LPtr : ^LongInt;
  begin
    S := Word(ILI (IFBPtr^.DIDPtr^[0]^.LenRec).Lo);
    if not AdjustVariableRecBuffer(S) then begin
      IsamOK := False;
      IsamError := 10040;
      Exit;
    end;
    SPtr := Addr (Source);
    CPtr := Addr (IVRBPtr^ [S-6]);
    LPtr := Addr (IVRBPtr^ [S-4]);
    if Len > (S-6) then begin
      IsamAddRestVarRec(IFBPtr, RefNr, SPtr^ [S-6], Len-(S-6));
      if not IsamOK then Exit;
      Len := S-6;
    end
    else begin
      RefNr := 0;
    end;
    Move(SPtr^, IVRBPtr^, Len);                                {!!.50}
    CPtr^ := Len;
    LPtr^ := RefNr;
    IsamAddRec(IFBPtr, RefNr, IVRBPtr^);                       {!!.50}
  end;
  {------}
begin
  IsamEntryCode(IFBPtr, OptCheckLock or OptWriteRoutine);      {!!.50}
  if IsamOK then begin                                         {!!.50}
    IFBPtr^.CharConvProc(@Source, Len, False,                  {!!.50}
        IFBPtr^.CCHookPtr);                                    {!!.50}
    IsamAddVariableRec;                                        {!!.50}
    if not IFBPtr^.CCDestrWrite then begin                     {!!.50}
      IFBPtr^.CharConvProc(@Source, Len, True,                 {!!.50}
          IFBPtr^.CCHookPtr);                                  {!!.50}
    end;                                                       {!!.50}
  end;                                                         {!!.50}
  IsamExitCode(IFBPtr);                                        {!!.50}
end;


procedure IsamDeleteVariableRec(IFBPtr : IsamFileBlockPtr;     {!!.50}
                                  RefNr  : LongInt);
var
  NextRefNr : LongInt;
  S         : Word;
begin
  S := ILI (IFBPtr^.DIDPtr^[0]^.LenRec).Lo;
  repeat
    IsamGetBlock(IFBPtr^.DatF, LongInt (S) * RefNr + LongInt (S-4),
                   SizeOf (LongInt), NextRefNr);
    if not IsamOK then Exit;
    IsamDeleteRec(IFBPtr, RefNr);                              {!!.50}
    if not IsamOK then Exit;
    RefNr := NextRefNr;
  until NextRefNr = 0;
end;


procedure BTDeleteVariableRec(IFBPtr : IsamFileBlockPtr;
                                RefNr  : LongInt);
begin
  IsamEntryCode(IFBPtr, OptCheckLock or OptWriteRoutine);       {!!.50}
  if IsamOK then
    IsamDeleteVariableRec(IFBPtr, RefNr);                       {!!.50}
  IsamExitCode(IFBPtr);                                         {!!.50}
end;


procedure BTPutVariableRec(    IFBPtr : IsamFileBlockPtr;
                                 RefNr  : LongInt;
                             var Source;
                                 Len    : Word);
  {------}
  procedure IsamPutVariableRec;                                {!!.50}
  var
    S        : Word;
    DelRefNr,
    AddRefNr : LongInt;
    SPtr     : PIsamVRecBuf;                                   {!!.50}
    CPtr     : ^Word;
    LPtr     : ^LongInt;
  begin
    S := Word (ILI (IFBPtr^.DIDPtr^[0]^.LenRec).Lo);
    if not AdjustVariableRecBuffer(S) then begin
      IsamOK := False;
      IsamError := 10040;
      Exit;
    end;
    SPtr := Addr (Source);
    CPtr := Addr (IVRBPtr^ [S-6]);
    LPtr := Addr (IVRBPtr^ [S-4]);
    if Len > (S-6) then begin
      IsamAddRestVarRec(IFBPtr, AddRefNr, SPtr^ [S-6], Len - (S-6));
      if not IsamOK then Exit;
      Len := S-6;
    end
    else begin
      AddRefNr := 0;
    end;
    IsamGetBlock(IFBPtr^.DatF, LongInt (S) * RefNr + LongInt (S-4),
                   SizeOf (LongInt), DelRefNr);
    if not IsamOK then Exit;
    if DelRefNr <> 0 then begin
      IsamDeleteVariableRec(IFBPtr, DelRefNr);
      if not IsamOK then Exit;
    end;
    Move(SPtr^, IVRBPtr^, Len);
    CPtr^ := Len;
    LPtr^ := AddRefNr;
    IsamPutRec(IFBPtr, RefNr, IVRBPtr^);
  end;
  {------}
begin
  IsamEntryCode(IFBPtr, OptCheckLock or OptWriteRoutine);      {!!.50}
  if IsamOK then begin                                         {!!.50}
    IFBPtr^.CharConvProc(@Source, Len, False,                  {!!.50}
        IFBPtr^.CCHookPtr);                                    {!!.50}
    IsamPutVariableRec;                                        {!!.50}
    if not IFBPtr^.CCDestrWrite then begin                     {!!.50}
      IFBPtr^.CharConvProc(@Source, Len, True,                 {!!.50}
          IFBPtr^.CCHookPtr);                                  {!!.50}
    end;                                                       {!!.50}
  end;                                                         {!!.50}
  IsamExitCode(IFBPtr);                                        {!!.50}
end;


procedure IsamGetVariableRecPart(IFBPtr : IsamFileBlockPtr;
                                 RefNr  : LongInt;
                             var Dest;
                             var Len    : Word;
                                 UseRO  : Boolean);
  {------}
  function GetVRec : Boolean;
  var
    SecSize    : Word;
    DestPos    : Word;
    SrcPos     : Word;
    LeftToRead : Word;
    LenToMove  : Word;
    DestPtr    : PIsamVRecBuf;                                 {!!.50}
    LenPtr     : ^Word;
    RefPtr     : ^LongInt;
  begin
    GetVRec := True;
    SecSize := Word (ILI(IFBPtr^.DIDPtr^[0]^.LenRec).Lo);
    LenPtr := @IVRBPtr^ [SecSize-6]; {Points to actual length of record
                                        section}
    RefPtr := @IVRBPtr^ [SecSize-4]; {Points to next segment reference}
    DestPtr := @Dest;                  {Points to base of output area}
    DestPos := 0;                      {Position of data in output area}
    SrcPos := 0;                       {Position of data in input section}
    RefPtr^ := RefNr;                  {First section to read}
    LeftToRead := Len;                 {Bytes left to read}
    Len := 0;                          {in case we exit with error}

    repeat
    {--Read the next section}
      if (DestPos = 0) and UseRO then begin
        IsamGetRecReadOnly(IFBPtr, RefPtr^, IVRBPtr^);         {!!.50}
        if IsamError = 10205 then begin
          GetVRec := False;
          IsamClearOK;
        end;
      end
      else begin
        IsamGetRec(IFBPtr, RefPtr^, IVRBPtr^);                 {!!.50}
      end;
      if not IsamOK then Exit;

    {--Transfer section to destination}
      if LenPtr^ > LeftToRead then begin
        LenToMove := LeftToRead;
      end
      else begin
        LenToMove := LenPtr^;
      end;
      Move(IVRBPtr^ [SrcPos], DestPtr^ [DestPos], LenToMove);

    {--Move to next area of destination}
      Inc(DestPos, LenToMove);
      Dec(LeftToRead, LenToMove);

    {--Data starts at position 1 for all sections but the first}
      SrcPos := 1;
    until (RefPtr^ = 0) or (LeftToRead = 0);

  {--Return the actual length}
    Len := DestPos;
  end;
  {------}
var
  Options : Word;                                              {!!.50}
begin
  if BTRecIsLocked(IFBPtr, RefNr) then begin                   {!!.50}
    Options := NoOptions;                                      {!!.50}
  end
  else begin                                                   {!!.50}
    Options := OptReadPrefix;                                  {!!.50}
  end;                                                         {!!.50}
  IsamEntryCode(IFBPtr, Options);                              {!!.50}
  if AdjustVariableRecBuffer                                   {!!.50}
     (Word (ILI (IFBPtr^.DIDPtr^[0]^.LenRec).Lo)) then begin
    if not GetVRec and IsamOK then begin
      IsamOK := False;
      IsamError := 10205;
    end;
    if IsamOK or (IsamError = 10205) then begin                {!!.50}
      IFBPtr^.CharConvProc(@Dest, Len, True,                   {!!.50}
          IFBPtr^.CCHookPtr);                                  {!!.50}
    end;                                                       {!!.50}
  end
  else begin                                                   {!!.50}
    IsamOK := False;                                           {!!.50}
    IsamError := 10040;                                        {!!.50}
  end;                                                         {!!.50}
  IsamExitCode(IFBPtr);                                        {!!.50}
end;


procedure BTGetVariableRecPart(IFBPtr : IsamFileBlockPtr;
                               RefNr  : LongInt;
                           var Dest;
                           var Len    : Word);
begin
  IsamGetVariableRecPart(IFBPtr, RefNr, Dest, Len, False);
end;


procedure BTGetVariableRec(IFBPtr : IsamFileBlockPtr;
                           RefNr  : LongInt;
                       var Dest;
                       var Len    : Word);
begin
  Len := MaxVariableRecLength;
  IsamGetVariableRecPart(IFBPtr, RefNr, Dest, Len, False);
end;


procedure BTGetVRecPartReadOnly(IFBPtr : IsamFileBlockPtr;
                                RefNr  : LongInt;
                            var Dest;
                            var Len    : Word);
begin
  IsamGetVariableRecPart(IFBPtr, RefNr, Dest, Len, True);
end;


procedure BTGetVRecReadOnly(IFBPtr : IsamFileBlockPtr;
                            RefNr  : LongInt;
                        var Dest;
                        var Len    : Word);
begin
  Len := MaxVariableRecLength;
  IsamGetVariableRecPart(IFBPtr, RefNr, Dest, Len, True);
end;


procedure BTGetVariableRecLength(IFBPtr : IsamFileBlockPtr;
                                 RefNr  : LongInt;
                             var Len    : Word);
  {------}
  procedure IsamGetVariableRecLength;                          {!!.50}
  var
    S   : Word;
    Inf : packed Record
      L         : Word;
      NextRefNr : LongInt;
    end;
  begin
    S := Word(ILI (IFBPtr^.DIDPtr^[0]^.LenRec).Lo);
    if not AdjustVariableRecBuffer(S) then begin
      IsamOK := False;
      IsamError := 10040;
      Exit;
    end;
    Len := 0;
    repeat
      IsamGetBlock(IFBPtr^.DatF, LongInt (S) * RefNr + LongInt (S-6),
                     SizeOf (Inf), Inf);
      if not IsamOK then Exit;
      with Inf do begin
        if Len > MaxVariableRecLength - L then begin
          IsamOK := False;
          IsamError := 10415;
          Exit;
        end;
        RefNr := NextRefNr;
        Inc (Len, L);
      end;
    until RefNr = 0;
  end;
  {------}
var
  Options : Word;                                              {!!.50}
begin
  if BTRecIsLocked(IFBPtr, RefNr) then begin                   {!!.50}
    Options := NoOptions;                                      {!!.50}
  end
  else begin                                                   {!!.50}
    Options := OptReadPrefix;                                  {!!.50}
  end;                                                         {!!.50}
  IsamEntryCode(IFBPtr, Options);                              {!!.50}
  if IsamOK then
    IsamGetVariableRecLength;                                  {!!.50}
  IsamExitCode(IFBPtr);                                        {!!.50}
end;

end.
