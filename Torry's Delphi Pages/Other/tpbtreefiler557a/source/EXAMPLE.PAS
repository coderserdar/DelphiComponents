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

program Example;
  {-Example calls for basic database operations}

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

const
  Key1Len = 30;                   {First and last name}
  Key2Len = 5;                    {ZipCode}
  Key3Len = 15;                   {Telephone}

  APerson : PersonDef =
  (Del : 0;
   FirstName : 'George';
   LastName : 'Bush';
   Street : '1 Capitol Ave';
   City : 'Washington';
   State : 'DC';
   ZipCode : '10011-0001';
   Telephone : '301-222-1111';
   Age : 64);

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
    BTOpenFileBlock(PF, 'TEST', False, False, False, False);
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
    BTAddRec(PF, RefNr, P);
    if not IsamOK then begin
      {Error handling}
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
        Exit;
      end;
    end;
    AddRecord := True;
  end;

  procedure UndoDel(P : PersonDef; RefNr : LongInt; LastKey : Integer);
  var
    KeyNr : Integer;
    Key : IsamKeyStr;
  begin
    for KeyNr := 1 to LastKey do begin
      Key := CreateKey(P, KeyNr);
      BTAddKey(PF, KeyNr, RefNr, Key);
      if not IsamOK then
        {Abort: too many errors}
        Halt;
    end;
  end;

  function DeleteRecord(P : PersonDef; RefNr : LongInt) : Boolean;
  var
    KeyNr : Integer;
    Key : IsamKeyStr;
  begin
    DeleteRecord := False;
    {Assure record not already deleted}
    if P.Del <> 0 then
      Exit;
    for KeyNr := 1 to BTNrOfKeys(PF) do begin
      Key := CreateKey(P, KeyNr);
      BTDeleteKey(PF, KeyNr, RefNr, Key);
      if not IsamOK then begin
        {Add keys that have been deleted so far}
        UndoDel(P, RefNr, KeyNr-1);
        {Error handling}
        Exit;
      end;
    end;
    BTDeleteRec(PF, RefNr);
    if IsamOK then
      DeleteRecord := True;
  end;

  function CheckRecord(P, POld : PersonDef) : Boolean;
  begin
    {Verify that: new record has valid keys,
                  new record differs from old}
    CheckRecord := True;
  end;

  function ModifyRecord(P, POld : PersonDef; RefNr : LongInt) : Boolean;
  var
    KeyNr : Integer;
  begin
    ModifyRecord := False;
    if not CheckRecord(P, POld) then
      Exit;
    for KeyNr := 1 to BTNrOfKeys(PF) do begin
      {Update modified keys}
      if CreateKey(P, KeyNr) <> CreateKey(POld, KeyNr) then begin
        BTDeleteKey(PF, KeyNr, RefNr, CreateKey(POld, KeyNr));
        if not IsamOK then
          if IsamError = 10220 then
            {Key already deleted, ignore the error}
          else begin
            UndoAdd(P, RefNr, KeyNr-1);
            UndoDel(POld, RefNr, KeyNr-1);
            Exit;
          end;
        BTAddKey(PF, KeyNr, RefNr, CreateKey(P, KeyNr));
        if not IsamOK then begin
          UndoAdd(P, RefNr, KeyNr-1);
          UndoDel(POld, RefNr, KeyNr);
          Exit;
        end;
      end;
    end;

    BTPutRec(PF, RefNr, P, False);
    if not IsamOK then begin
      UndoAdd(P, RefNr, BTNrOfKeys(PF));
      UndoDel(POld, RefNr, BTNrOfKeys(PF));
      Exit;
    end;

    ModifyRecord := True;
  end;

  function NextPrevRecord(var P : PersonDef;
                          var RefNr : LongInt;
                          KeyNr : Integer;
                          var Key : IsamKeyStr;
                          Next : Boolean) : Boolean;
  begin
    NextPrevRecord := False;
    if Next then begin
      BTNextKey(PF, KeyNr, RefNr, Key);
      if not IsamOK and (IsamError = 10250) then
        {There was no next key. Move to first key in the file}
        BTNextKey(PF, KeyNr, RefNr, Key);
    end else begin
      BTPrevKey(PF, KeyNr, RefNr, Key);
      if not IsamOK and (IsamError = 10260) then
        {There was no previous key. Move to last key in file}
        BTPrevKey(PF, KeyNr, RefNr, Key);
    end;
    if not IsamOK then
      Exit;
    BTGetRec(PF, RefNr, P, False);
    if not IsamOK then begin
      {Error handling}
      Exit;
    end;
    NextPrevRecord := True;
  end;

  function FindRecord(var P : PersonDef;
                      var RefNr : LongInt;
                      KeyNr : Integer;
                      var Key : IsamKeyStr) : Boolean;
  begin
    FindRecord := False;
    BTSearchKey(PF, KeyNr, RefNr, Key);
    if not IsamOK then begin
     {Determine why SearchKey failed, for example:
        IsamError = 10210 Neither the key nor any larger was found.}
      Exit;
    end;
    BTGetRec(PF, RefNr, P, False);
    if not IsamOK then begin
      {Error handling}
      Exit;
    end;
    FindRecord := True;
  end;

  function MatchedRecord(P, Q : PersonDef) : Boolean;
  begin
    {Return True if P and Q match based on some criteria, for example...}
    MatchedRecord := (StUpcase(P.City) = StUpcase(Q.City));
  end;

  function ScanForRecord(var P : PersonDef; KeyNr : Integer;
                         var RefNr : LongInt) : Boolean;
  var
    Done : Boolean;
    Goal : PersonDef;
    Key : IsamKeyStr;
  begin
    ScanForRecord := False;
    Goal := P;
    Done := False;
    repeat
      BTNextKey(PF, KeyNr, RefNr, Key);
      if not IsamOK then
        {Probably reached the largest key}
        Done := True
      else begin
        BTGetRec(PF, RefNr, P, False);
        if not IsamOK then begin
          {Error handling}
          Done := True;
        end else if MatchedRecord(P, Goal) then begin
          {Found a match}
          Done := True;
          ScanForRecord := True;
        end;
      end;
    until Done;
  end;

begin
  WriteLn('This program is just a collection of example routines');
  WriteLn('See SIMPDEMO.PAS or NETDEMO.PAS for working demo programs');
end.
