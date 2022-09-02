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

program Convert;
uses
  {$IFDEF Windows}
  WinCRT,
  {$ENDIF}
  Filer, Reorg;

type
  DataRecord =                    {Leave the record name unchanged}
    record                        {!!}
      RecDeleted : LongInt;       {!! All records should start with this}
      LastName : string[20];      {!!}
      CustNum : string[10];       {!!}
      {...}                       {!! Other fields follow}
    end;
const
  DataFileName = 'MYDATA';        {!! Root name for data file. Extension
                                  will always be .DAT}
  NrOfKeys = 2;                   {!! Number of indexes for the data file}
var
  IID : IsamIndDescr;             {Describes the indexes}
  Pages : LongInt;                {Number of index pages}
  RecordsAdded : LongInt;         {Status counter for records added}
  RecordsKeyed : LongInt;         {Status counter for records keyed}

{$F+}
function BuildKey(var DatS; KeyNr : Integer) : IsamKeyStr;
begin
  if RecordsKeyed = 0 then
    WriteLn;
  with DataRecord(DatS) do
    case KeyNr of                 {!! Customize this to return each key}
      1 : BuildKey := CustNum;    {!!}
      2 : BuildKey := LastName;   {!!}
      {..}
    end;
  {Keep status counter running}
  inc(RecordsKeyed);
  if RecordsKeyed and 15 = 0 then
    Write(^M, RecordsKeyed);
end;

function ChangeDat(var DatSOld; var DatSNew; Len : Word) : Boolean; {!!.22}
begin
  if LongInt(DatSOld) = 0 then begin
    {Record hasn't been deleted}
    ChangeDat := True;
    DataRecord(DatSNew) := DataRecord(DatSOld);
    {Keep status counter running}
    inc(RecordsAdded);
    if RecordsAdded and 15 = 0 then
      Write(^M, RecordsAdded);
  end else
    {Record is deleted, don't add it}
    ChangeDat := False;
end;
{$F-}

procedure InitIID;
begin
  {!! Specify each index type here}
  IID[1].KeyL := 10;              {Maximum length of key string}
  IID[1].AllowDupK := False;      {False for a primary key}
  IID[2].KeyL := 20;
  IID[2].AllowDupK := True;       {True for a secondary key}
end;

begin
  Pages := BTInitIsam(NoNet, 10000, 0);
  if not IsamOK then begin
    WriteLn('BTInitIsam failed. Not enough memory available');
    Halt;
  end;
  RecordsAdded := 0;
  RecordsKeyed := 0;
  InitIID;     {!!.01}
  ReorgFileBlock(DataFileName, SizeOf(DataRecord), NrOfKeys,
                 IID, SizeOf(DataRecord),
                 @BuildKey, @ChangeDat);
  WriteLn;
  if IsamOK then
    WriteLn(RecordsAdded, ' records converted')
  else
    WriteLn('Convert failed. IsamError = ', IsamError);
end.
