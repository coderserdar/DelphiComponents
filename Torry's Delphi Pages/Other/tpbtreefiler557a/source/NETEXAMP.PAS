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
 *
 * ***** END LICENSE BLOCK ***** *)

program NetExample;
  {-Example calls for basic database operations on a network}

uses
  Filer;

type
  PersonDef =
    record
      Del : LongInt;
      FirstName : String[20];
      LastName : String[25];
      Street : String[30];
      City : String[30];
      State : String[2];
      ZipCode : String[9];
      Telephone : String[15];
      Age : Integer;
    end;

var
  PF : IsamFileBlockPtr;          {Symbolic access to the database}

  MaxError : Integer;             {Lock status variables}
  RecordLocked : Boolean;
  FileBlockLocked : Boolean;

const
  Key1Len = 30;                   {First and last name}
  Key2Len = 5;                    {ZipCode}
  Key3Len = 15;                   {Telephone}

  SevereError = 20;               {User-defined error classes}
  KeyError = 15;
  DeletedError = 12;
  ModifiedError = 10;

  APerson : PersonDef =
  (Del : 0;
   FirstName : 'George';
   LastName : 'Bush';
   Street : '1 Capitol Ave';
   City : 'Washington';
   State : 'DC';
   ZipCode : '10011-0001';
   Telephone : '301-222-1111';
   Age : 62);

  procedure AllocatePageBuffer(HeapToRemain : LongInt);
  var
    NumberOfPages : Word;
  begin
    NumberOfPages := BTInitIsam(NoNet, HeapToRemain, 0);
    if not IsamOK then begin
      {Insufficient memory}
      Halt;
    end;
  end;

  function CreateFile : Boolean;
  var
    IID : IsamIndDescr;
  begin
    IID[1].KeyL := Key1Len; IID[1].AllowDupK := False;
    IID[2].KeyL := Key2Len; IID[2].AllowDupK := True;
    IID[3].KeyL := Key3Len; IID[3].AllowDupK := True;
    BTCreateFileBlock('TEST', SizeOf(PersonDef), 3, IID);
    CreateFile := IsamOK;
  end;

  function OpenFile : Boolean;
  begin
    BTOpenFileBlock(PF, 'TEST', False, False, False, BTNetSupported <> NoNet);
    if not IsamOK then begin
      OpenFile := False;
      {Error reporting code that examines
       <IsamError> can go here. Corrective action may
       be taken, for example by reconstructing a defective
       index file as described in Section 6.D.}
      Exit;
    end else
      OpenFile := True;
  end;

  function CloseFile : Boolean;
  begin
    BTCloseFileBlock(PF);
    if not IsamOK then begin
      CloseFile := False;
      {Error handling}
      Exit;
    end else
      CloseFile := True;
  end;

  function StUpcase(S : String) : String;
  var
    I : Integer;
  begin
    for I := 1 to Length(S) do
      S[I] := Upcase(S[I]);
    StUpcase := S;
  end;

  function Pad(S : String; Len : Byte) : String;
  var
    SLen : Byte absolute S;
  begin
    if SLen > Len then
      SLen := Len
    else
      while SLen < Len do
        S := S+' ';
    Pad := S;
  end;

  {$F+} {Routine should be global}
  function CreateKey(var P; KeyNr : Integer) : IsamKeyStr;
  begin
    with PersonDef(P) do
      case KeyNr of
        1 : CreateKey := StUpcase(Pad(LastName, 20)+Pad(FirstName, 10));
        2 : CreateKey := Copy(ZipCode, 1, 5);
        3 : CreateKey := Copy(Telephone, 1, 15);
      else
        CreateKey := '';
      end;
  end;

  function YesNo(Prompt : String) : Boolean;
  var
    Done : Boolean;
    S : string[1];
  begin
    Done := False;
    repeat
      Write(Prompt);
      ReadLn(S);
      if Length(S) > 0 then
        case Upcase(S[1]) of
          'Y' : begin
                  Done := True;
                  YesNo := True;
                end;
          'N' : begin
                  Done := True;
                  YesNo := False;
                end;
        end;
    until Done;
  end;

  const
    MaxRetries = 10;
    RetryCount : Integer = 0;

  function IsLockError(Ask : Boolean) : Boolean;
  begin
    if IsamOK or (BTIsamErrorClass <> 2) then begin
      {No error, or non-locking error}
      IsLockError := False;
      {Reset retry count}
      RetryCount := 0;
    end else begin
      {Lock error}
      IsLockError := True;
      inc(RetryCount);
      if RetryCount > MaxRetries then begin
        {Out of retries}
        if not Ask then
          {Abort the operation without even asking}
          IsLockError := False
        else if not YesNo('Lock error. Try again?') then
          {Abort the operation if the user says to do so}
          IsLockError := False;
        {Reset retry count}
        RetryCount := 0;
      end;
    end;
  end;

  procedure UndoAdd(P : PersonDef; RefNr : LongInt; LastKey : Integer);
  var
    KeyNr : Integer;
    Key : IsamKeyStr;
  begin
    for KeyNr := 1 to LastKey do begin
      Key := CreateKey(P, KeyNr);
      BTDeleteKey(PF, KeyNr, RefNr, Key);
      if not IsamOK then
        {Abort: too many errors}
        Halt;
    end;
  end;

  function AddRecord(P : PersonDef) : Boolean;
  var
    KeyNr : Integer;
    RefNr : LongInt;
    Key : IsamKeyStr;
  begin
    AddRecord := False;

    {Lock the database for a safe add}
    repeat
      BTLockFileBlock(PF);
    until not IsLockError(True);

    BTAddRec(PF, RefNr, P);
    if not IsamOK then begin
      {Error handling}
      BTUnLockFileBlock(PF);
      Exit;
    end;
    for KeyNr := 1 to BTNrOfKeys(PF) do begin
      Key := CreateKey(P, KeyNr);
      BTAddKey(PF, KeyNr, RefNr, Key);
      if not IsamOK then begin
        {Remove keys added so far}
        UndoAdd(P, RefNr, KeyNr-1);
        {Remove the new record}
        BTDeleteRec(PF, RefNr);
        {Error handling}
        BTUnLockFileBlock(PF);
        Exit;
      end;
    end;

    BTUnLockFileBlock(PF);
    AddRecord := IsamOK;
  end;

  procedure SetMaxError;
  var
    ErrorClass : Integer;
  begin
    ErrorClass := BTIsamErrorClass;
    if ErrorClass > MaxError then
      MaxError := ErrorClass;
  end;

  procedure UpdateUnlock(RefNr : LongInt);
  begin
    if BTFileBlockIsLocked(PF) then begin
      BTUnlockFileBlock(PF);
      if not IsamOK then
        {Hardware failure? shouldn't happen}
        SetMaxError;
    end;
    if BTRecIsLocked(PF, RefNr) then begin
      BTUnlockRec(PF, RefNr);
      if not IsamOK then
        {Hardware failure? shouldn't happen}
        SetMaxError;
    end;
  end;

  function UpdateLock(RefNr : LongInt) : Boolean;
  begin
    UpdateLock := False;
    {Record locking is needed only if the application uses
     record locks elsewhere}
    BTLockRec(PF, RefNr);
    if not IsamOK then begin
      {Record could not be locked}
      SetMaxError;
      Exit;
    end;
    {Lock the fileblock}
    BTLockFileBlock(PF);
    if not IsamOK then begin
      {FileBlock could not be locked}
      SetMaxError;
      UpdateUnlock(RefNr);
      Exit;
    end;
    UpdateLock := True;
  end;

  function MatchRecord(P1, P2 : PersonDef) : Boolean;
  begin
    {Return true if P1 and P2 are the same}
    MatchRecord := True;
  end;

  function DeleteRecord(P : PersonDef; RefNr : LongInt) : Integer;
  var
    KeyNr : Integer;
    TempP : PersonDef;
  begin
    {At this point, the record to be deleted has already been read into
     record P, and the user has verified that a deletion is to occur.}

    {MaxError is the highest error class encountered}
    MaxError := 0;

    {Lock the record and the fileblock}
    if not UpdateLock(RefNr) then begin
      {Couldn't lock}
      DeleteRecord := MaxError;
      Exit;
    end;

    {Get the record again to see if it still matches the original}
    BTGetRec(PF, RefNr, TempP, False);
    if not IsamOK then begin
      {Shouldn't happen}
      SetMaxError;
      UpdateUnlock(RefNr);
      DeleteRecord := MaxError;
      Exit;
    end;

    if TempP.Del <> 0 then begin
      {Record was already deleted. That's ok since we wanted to delete it also}
      UpdateUnlock(RefNr);
      DeleteRecord := MaxError;
      Exit;
    end;

    if not MatchRecord(P, TempP) then begin
      {Record was modified in the meantime. Return a "warning" error class}
      MaxError := 1;
      UpdateUnlock(RefNr);
      DeleteRecord := MaxError;
      Exit;
    end;

    {Finally perform the deletion}
    for KeyNr := 1 to BTNrOfKeys(PF) do begin
      BTDeleteKey(PF, KeyNr, RefNr, CreateKey(P, KeyNr));
      if not IsamOK then
        if IsamError = 10220 then
          {Key already deleted. Shouldn't happen, but it's still ok if so}
        else begin
          {Error handling}
          SetMaxError;
          UpdateUnlock(RefNr);
          DeleteRecord := MaxError;
          Exit;
        end;
    end;
    BTDeleteRec(PF, RefNr);
    if not IsamOK then
      SetMaxError;

    {Unlock the record and the fileblock}
    UpdateUnlock(RefNr);
    DeleteRecord := MaxError;
  end;

  function CheckKeys(P : PersonDef) : Boolean;
  begin
    {Verify that new record has valid keys}
    CheckKeys := True;
  end;

  function ModifyRecord(P, POld : PersonDef; RefNr : LongInt) : Integer;
  var
    KeyNr : Integer;
    TempP : PersonDef;
  begin
    {At this point, the user has modified the record. The new value is
     stored in P and its previous value is stored in POld.}

    ModifyRecord := 0;

    if MatchRecord(P, POld) then
      {No change, just exit}
      Exit;
    if not CheckKeys(P) then begin
      {Invalid keys, perhaps empty name fields}
      ModifyRecord := KeyError;
      Exit;
    end;

    {MaxError is the highest error class encountered}
    MaxError := 0;

    {Lock the record and the Fileblock}
    if not UpdateLock(RefNr) then begin
      {Couldn't lock}
      ModifyRecord := MaxError;
      Exit;
    end;

    {Get the record again to see if it still matches the original}
    BTGetRec(PF, RefNr, TempP, False);
    if not IsamOK then begin
      {Shouldn't happen, but just in case}
      SetMaxError;
      UpdateUnlock(RefNr);
      ModifyRecord := MaxError;
      Exit;
    end;

    if TempP.Del <> 0 then begin
      {Record was deleted in the meantime}
      MaxError := DeletedError;
      UpdateUnlock(RefNr);
      ModifyRecord := MaxError;
      Exit;
    end;

    if not MatchRecord(POld, TempP) then begin
      {Record was modified in the meantime}
      MaxError := ModifiedError;
      UpdateUnlock(RefNr);
      ModifyRecord := MaxError;
      Exit;
    end;

    {Finally perform the update}
    BTPutRec(PF, RefNr, P, False);
    if not IsamOK then begin
      {Error handling}
      SetMaxError;
      UpdateUnlock(RefNr);
      ModifyRecord := MaxError;
      Exit;
    end;

    for KeyNr := 1 to BTNrOfKeys(PF) do begin
      {Update modified keys}
      if CreateKey(P, KeyNr) <> CreateKey(POld, KeyNr) then begin
        BTDeleteKey(PF, KeyNr, RefNr, CreateKey(POld, KeyNr));
        if IsamOK then
          BTAddKey(PF, KeyNr, RefNr, CreateKey(P, KeyNr));
        if not IsamOK then begin
          {Error handling}
          SetMaxError;
          UpdateUnlock(RefNr);
          ModifyRecord := MaxError;
          Exit;
        end;
      end;
    end;

    UpdateUnlock(RefNr);
    ModifyRecord := MaxError;
  end;

  function FindRecord(var P : PersonDef;
                      var RefNr : LongInt;
                      KeyNr : Integer;
                      var Key : IsamKeyStr) : Boolean;
  begin
    FindRecord := False;
    repeat
      BTSearchKey(PF, KeyNr, RefNr, Key);
    until not IsLockError(True);
    if not IsamOK then begin
      {Key not found, program error, or abortive locking error}
      Exit;
    end;
    repeat
      BTGetRec(PF, RefNr, P, False);
    until not IsLockError(True);
    if not IsamOK then begin
      {Error handling}
      Exit;
    end;
    FindRecord := True;
  end;

  function NextPrevRecord(var P : PersonDef;
                          var RefNr : LongInt;
                          KeyNr : Integer;
                          var Key : IsamKeyStr;
                          Next : Boolean) : Boolean;
  begin
    NextPrevRecord := False;
    if Next then begin
      repeat
        BTNextKey(PF, KeyNr, RefNr, Key);
      until not IsLockError(True);
      if not IsamOK and (IsamError = 10250) then
        {There was no next key. Move to first key in the file}
        BTNextKey(PF, KeyNr, RefNr, Key);
    end else begin
      repeat
        BTPrevKey(PF, KeyNr, RefNr, Key);
      until not IsLockError(True);
      if not IsamOK and (IsamError = 10260) then
        {There was no previous key. Move to last key in file}
        BTPrevKey(PF, KeyNr, RefNr, Key);
    end;
    if not IsamOK then
      Exit;
    repeat
      BTGetRec(PF, RefNr, P, False);
    until not IsLockError(True);
    if not IsamOK then begin
      {Error handling}
      Exit;
    end;
    NextPrevRecord := True;
  end;

begin
  WriteLn('This program is just a collection of example routines');
  WriteLn('See SIMPDEMO.PAS or NETDEMO.PAS for working demo programs');
end.
