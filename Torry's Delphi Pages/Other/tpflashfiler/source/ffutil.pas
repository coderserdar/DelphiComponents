{*********************************************************}
{* FlashFiler: Client utility routines                   *}
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

unit ffutil;

interface
uses
  DB,
  ffdb,
  Windows,
  Messages,
  Classes,
  SysUtils,
  ffllprot,
  fflldict,
  ffllbase,
  ffstdate;

type
  TffCopyTableProgressEvent = procedure (Index : Integer);


procedure KillExternalServer(const aBlocking : Boolean);
  { - Kill the server process running locally }

function FFGetMaxAutoInc(aTable : TffTable) : Longint;
  { - Retrieve the MaxAutoInc value used in a table. This routine does
      not get the last one used, instead it queries each record and
      returns the highest key currently in the table. }

function FFGetProtocolString (Protocol : TffProtocolType) : string;
  { - Converts a TffProtocolType value to a string value }

function FFGetProtocolType (ProtocolStr : string) : TffProtocolType;
  { - Converts the specified string to a valid TffProtocolType }

procedure FFRetrieveLiveServers(const Protocol: TffProtocolType; ServerNames : TStringList);
  { - Fills ServerName with a list of servers for the specified protocol.
      Care is taken to remove the local server if it cannot be found. }

procedure FFSeparateAddress(const Original : string;
                              var Name : string;
                              var Address : string);
  { - Breaks the specifid address into Name & address parts }

function FFTransferRecord(Source, Dest : TDataSet) : Boolean;
  { - Transfers the current record from Source to Dest, matching fields by
      name. The result will be true if the routine was successful. }

procedure FFCopyTableData(SourceTable, DestTable : TDataset);
  { - Transfers the records from SourceTable to DestTable. SourceTable's
      cursor will be First and finish at EOF. Ranges and Filters will not
      be disturbed by this routine }

procedure FFCopyTableDataEx(SourceTable, DestTable : TDataset; ProgressEvent: TffCopyTableProgressEvent);
  { - Transfers the records from SourceTable to DestTable. SourceTable's
      cursor will be First and finish at EOF. Ranges and Filters will not
      be disturbed by this routine. This routine has the ability to
      call a progress event }

procedure FFStringToVCheckVal(const aStr  : string;
                              const aType : TffFieldType;
                                var aVal  : TffVCheckValue);
{ Converts a string to a TffVCheckValue. Used to set the default
  for a field in the data dictionary }

function FFVCheckValToString(const aVal  : TffVCheckValue;
                             const aType : TffFieldType) : String;
{ Converts a TffVCheckValue to a string. Used to retrieve a string
  representation of the default value for a field in a data
  dictionary }

implementation

uses
  FFClCfg,
  FFCLconv;

procedure KillExternalServer(const aBlocking : Boolean);
var
  Handle : THandle;
begin
  Handle := FindWindowEx(0, 0, 'TfrmFFServer', nil);
  if Handle > 0 then
    if aBlocking then
      SendMessage(Handle, WM_Close, 0, 0)
    else
      PostMessage(Handle, WM_Close, 0, 0);
end;
{--------}
function FFGetMaxAutoInc(aTable : TffTable) : Longint;
var
  MaxSeed : Longint;
  AField : TAutoIncField;
begin
  AField := nil;
  for MaxSeed := 0 to Pred(aTable.FieldCount) do
    if aTable.Fields[MaxSeed] is TAutoIncField then begin
      AField := TAutoIncField(aTable.Fields[MaxSeed]);
      Break;
    end;
  if not Assigned(AField) then begin
    Result := 0;
    Exit;
  end;

  MaxSeed := 0;
  aTable.First;
  while not aTable.EOF do begin
    if AField.Value > MaxSeed then
      MaxSeed := AField.Value;
    aTable.Next;
  end;
  Result := MaxSeed;
end;
{--------}
function FFGetProtocolString (Protocol : TffProtocolType) : string;
begin
  case Protocol of
    ptIPXSPX     : Result := ffc_IPXSPX;
    ptTCPIP      : Result := ffc_TCPIP;
  else
    Result := ffc_SingleUser;
  end;
end;
{--------}
function FFGetProtocolType (ProtocolStr : string) : TffProtocolType;
begin
  if ProtocolStr = ffc_IPXSPX then
    Result := ptIPXSPX
  else if ProtocolStr = ffc_TCPIP then
    Result := ptTCPIP
  else
    Result := ptSingleUser;
end;
{--------}
procedure FFRetrieveLiveServers(const Protocol: TffProtocolType; ServerNames : TStringList);
var
  CE : TffCommsEngine;
  SE : TffSession;
begin
  CE := TffCommsEngine.Create(nil);
  try
    CE.CommsEngineName := 'TEST';
    CE.Protocol := Protocol;
    CE.Open;
    ServerNames.Clear;
    if Protocol <> ptSingleUser then begin
      CE.GetServerNames(ServerNames);
    end else begin
      SE := TffSession.Create(nil);
      SE.SessionName := 'TEST';
      try
        SE.CommsEngineName := 'TEST';
        try
          SE.Open;
          ServerNames.Add('local');
        except
        end;
      finally
        SE.Free;
      end;
    end;
  finally
    CE.Free;
  end;
end;
{--------}
procedure FFSeparateAddress(const Original: string; var Name,
  Address: string);
var
  SepPlace : Integer;
  ServerName : string;
begin
  ServerName := Original;
  SepPlace := Pos('@', ServerName);
  if SepPlace > 0 then begin
    Name := Copy(ServerName, 1, pred(SepPlace));
    Delete(ServerName,1,SepPlace);
    Address := ServerName;
  end else begin
    Name := ServerName;
  end;
end;
{--------}
function FFTransferRecord(Source, Dest : TDataSet) : Boolean;
var
  i, nErr : integer;
  f1, f2 : TField;
begin
  nErr := 0;
  if (dest.state in [dsEdit, dsInsert]) {= dsBrowse} then begin
  end else begin
    Dest.Edit;
  end;
  for I := 0 to (Dest.FieldCount - 1) do begin
    f1 := Dest.FindField(Dest.Fields[I].FieldName);
    f2 := Source.FindField(Dest.Fields[I].FieldName);
    if (((f1 <> nil) and (f2 <> nil)) and (dest.Fields[I].FieldName <> 'RefNum') and (dest.Fields[I].FieldName <> 'AutoInc')) then begin
      try
        f1.value := f2.value;
      except
        inc(nErr);
      end;
    end else begin
    end;
  end;
  Dest.Post;
  Result := (nErr < Dest.FieldCount);
end;
{--------}
procedure FFCopyTableData(SourceTable, DestTable : TDataset);
begin
  SourceTable.First;
  while not SourceTable.EOF do begin
    DestTable.Insert;
    FFTransferRecord(SourceTable, DestTable);
    SourceTable.Next;
  end;
end;

procedure FFCopyTableDataEx(SourceTable, DestTable : TDataset; ProgressEvent: TffCopyTableProgressEvent);
var
  Idx : Integer;
begin
  SourceTable.First;
  Idx := 0;
  while not SourceTable.EOF do begin
    DestTable.Insert;
    FFTransferRecord(SourceTable, DestTable);
    inc(Idx);
    if Assigned(ProgressEvent) then
      ProgressEvent(Idx);
    SourceTable.Next;
  end;
end;

procedure FFStringToVCheckVal(const aStr  : string;
                              const aType : TffFieldType;
                                var aVal  : TffVCheckValue);
var
  TempStr     : String[255];
  TempInt     : Longint;
  TempExtend  : Extended;
  TempCurrency: Currency;
  TempSingle  : Single;
  TempDouble  : Double;
  TempStDate  : TStDate;
  TempStTime  : TStTime;
  TempDT      : TDateTime;
  TempTS      : TTimeStamp;
  TempComp    : Comp;
  TempWideStr : WideString;

begin
  if (aStr <> '') then begin
    FillChar(aVal, SizeOf(TffVCheckValue), #0);
    case aType of
      fftStDate :
        begin
          TempStDate := FFStringToStDate(aStr);
          Move(TempStDate, aVal, sizeof(TStDate));
        end;
      fftStTime :
        begin
          TempStTime := FFStringToStTime(aStr);
          Move(TempStTime, aVal, sizeof(TStTime));
        end;
      fftWord16 :
        begin
          TempInt := StrToInt(aStr);
          MapBDEDataToFF(fftWord16, sizeof(Word), @TempInt, @aVal);
        end;
      fftWord32 :
        begin
          TempInt := StrToInt(aStr);
          MapBDEDataToFF(fftWord32, sizeof(TffWord32), @TempInt, @aVal);
        end;
      fftInt8 :
        begin
          TempInt := StrToInt(aStr);
          MapBDEDataToFF(fftInt8, sizeof(Shortint), @TempInt, @aVal);
        end;
      fftInt16 :
        begin
          TempInt := StrToInt(aStr);
          MapBDEDataToFF(fftInt16, sizeof(Smallint), @TempInt, @aVal);
        end;
      fftInt32 :
        begin
          TempInt := StrToInt(aStr);
          MapBDEDataToFF(fftInt32, sizeof(Longint), @TempInt, @aVal);
        end;
      fftChar :
        begin
          TempStr := aStr;
          MapBDEDataToFF(fftChar, sizeof(Char), @TempStr[1], @aVal);
        end;
      fftWideChar :
        begin
          StringToWideChar(aStr, @TempStr, sizeof(WideChar));
          MapBDEDataToFF(fftWideChar, sizeof(WideChar), @TempStr, @aVal);
        end;
      fftByte :
        begin
          TempInt := StrToInt(aStr);
          MapBDEDataToFF(fftByte, sizeof(Byte), @TempInt, @aVal);
        end;
      fftSingle :
        begin
          TempSingle := StrToFloat(aStr);
          Move(TempSingle, aVal, sizeof(Single));
        end;
      fftDouble :
        begin
          TempDouble := StrToFloat(aStr);
          MapBDEDataToFF(fftDouble, sizeof(Double), @TempDouble, @aVal);
        end;
      fftExtended :
        begin
          TempExtend := StrToFloat(aStr);
          Move(TempExtend, aVal, sizeof(Extended));
        end;
      fftComp :
        begin
          TempComp := StrToFloat(aStr);
          Move(TempComp, aVal, sizeof(Comp));
        end;
      fftCurrency :
        begin
          TempCurrency := StrToFloat(aStr);
          Move(TempCurrency, aVal, sizeof(Currency));
        end;
      fftDateTime :
        begin
          TempDT := StrToDateTime(aStr);
          TempTS := DateTimeToTimeStamp(TempDT);
          TempDT := TimeStampToMSecs(TempTS);
          MapBDEDataToFF(fftDateTime, sizeof(TDateTime), @TempDT, @aVal);
        end;
      fftBoolean :
        begin
          if ((UpperCase(aStr) = 'TRUE') or (UpperCase(aStr) = 'T')) then
            TempInt := 1
          else if ((UpperCase(aStr) = 'FALSE') or (UpperCase(aStr) = 'F')) then
            TempInt := 0;
          MapBDEDataToFF(fftBoolean, sizeof(Boolean), @TempInt, @aVal);
        end;
      fftByteArray :
        begin
          TempStr := aStr;
          MapBDEDataToFF(fftByteArray, sizeof(ffcl_MaxVCheckLength), @TempStr, @aVal);
        end;
      fftShortAnsiStr :
        begin
          TempStr := aStr;
          Move(TempStr, aVal, Succ(Length(aStr)));
        end;
      fftShortString :
        begin
          TempStr := aStr;
          Move(TempStr, aVal, Succ(Length(aStr)));
        end;
      fftNullString :
        begin
          TempStr := aStr;
          MapBDEDataToFF(fftNullString, succ(Length(TempStr)), @TempStr[1], @aVal);
        end;
      fftNullAnsiStr :
        begin
          TempStr := aStr;
          MapBDEDataToFF(fftNullString, succ(Length(TempStr)), @TempStr[1], @aVal);
        end;
      fftWideString :
        begin
          StringToWideChar(aStr, @TempWideStr, (Length(aStr) * 2));
          Move(TempWideStr, aVal, (Length(aStr) * 2));
        end;
    end;
 end;
end;

function FFVCheckValToString(const aVal  : TffVCheckValue;
                             const aType : TffFieldType)
                                         : string;

var
  TempStr     : string[255];
//  TempInt8    : ShortInt;                                            {!!.07}
  TempInt16   : SmallInt;
  TempInt64   : TffInt64;                                              {!!.13}
  TempInt     : Longint;
  TempExtend  : Extended;
  TempCurrency: Currency;
  TempSingle  : Single;
  TempDouble  : Double;
  TempStDate  : TStDate;
  TempStTime  : TStTime;
  TempDT      : TDateTime;
  TempTS      : TTimeStamp;
  TempComp    : Comp;
  TempWideStr : WideString;
  i           : Integer;

begin
  case aType of
    fftWord16 :
      begin
        MapFFDataToBDE(fftWord16, sizeof(Word), @aVal, @TempInt);
        TempStr := IntToStr(TempInt);
      end;
    fftWord32 :
      begin
        MapFFDataToBDE(fftWord32, sizeof(TffWord32), @aVal, @TempInt);
        TempStr := IntToStr(TempInt);
      end;
    fftInt8 :
      begin
        {NOTE: Int8 mapped to 16-bit integer because the VCL does not
               have a 8-bit integer field type. }
        MapFFDataToBDE(fftInt8, SizeOf(ShortInt), @aVal, @TempInt16);  {!!.07}
        TempStr := IntToStr(TempInt16);                                {!!.07}
      end;
    fftInt16 :
      begin
        MapFFDataToBDE(fftInt16, sizeof(Smallint), @aVal, @TempInt16);
        TempStr := IntToStr(TempInt16);
      end;
    fftInt32 :
      begin
        MapFFDataToBDE(fftInt32, sizeof(Longint), @aVal, @TempInt);
        TempStr := IntToStr(TempInt);
      end;
    fftStDate :
      begin
        Move(aVal, TempStDate, sizeof(TStDate));
        TempStr := FFStDateToString(TempStDate);
      end;
    fftStTime :
      begin
        Move(aVal, TempStTime, sizeof(TStTime));
        TempStr := FFStTimeToString(TempStTime);
      end;
    fftChar :
      begin
        TempInt := 1;
        Move(TempInt, TempStr[0], 1);
        MapFFDataToBDE(fftChar, sizeof(Char), @aVal, @TempStr[1]);
      end;
    fftWideChar :
      begin
        MapFFDataToBDE(fftWideChar, sizeof(Widechar), @aVal, @TempStr);
        TempStr := WideCharToString(@TempStr);;
      end;
    fftByte :
      begin
        MapFFDataToBDE(fftByte, sizeof(Byte), @aVal, @TempInt);
        TempStr := IntToStr(TempInt);
      end;
    fftSingle :
      begin
        Move(aVal, TempSingle, sizeof(Single));
        TempStr := FloatToStr(TempSingle);
      end;
    fftDouble :
      begin
        MapFFDataToBDE(fftDouble, sizeof(Double), @aVal, @TempDouble);
        TempStr := FloatToStr(TempDouble);
      end;
    fftExtended :
      begin
        Move(aVal, TempExtend, sizeof(Extended));
        TempStr := FloatToStr(TempExtend);
      end;
    fftCurrency :
      begin
        Move(aVal, TempCurrency, sizeof(Currency));
        TempStr := FloatToStr(TempCurrency);
      end;
    fftComp :
      begin
        Move(aVal, TempComp, sizeof(Comp));
        TempStr := FloatToStr(TempComp);
      end;
    fftDateTime :
      begin
        MapFFDataToBDE(fftDateTime, sizeof(TDateTime), @aVal, @TempDT);
        TempTS := MSecsToTimeStamp(TempDT);
        TempDT := TimeStampToDateTime(TempTS);
        TempStr := DateTimeToStr(TempDT);
      end;
    fftBoolean :
      begin
        MapFFDataToBDE(fftBoolean, sizeof(Boolean), @aVal, @TempInt);
//      if (TempInt = 0) then                                          {!!.12}
        if Byte(TempInt) = 0 then                                      {!!.12}
          TempStr := 'False'
        else
          TempStr := 'True';
      end;
    fftByteArray :
      begin
        MapFFDataToBDE(fftByteArray, ffcl_MaxVCheckLength, @aVal, @TempStr);
      end;
    fftShortAnsiStr :
      begin
        MapFFDataToBDE(fftShortAnsiStr, ffcl_MaxVCheckLength, @aVal, @TempStr);
        MapFFDataToBDE(fftShortString, ffcl_MaxVCheckLength, @aVal, @TempStr[1]);
        i := 0;
        while TempStr[succ(i)] <> #0 do
          inc(i);
        SetLength(TempStr, i);
      end;
    fftShortString :
      begin
        MapFFDataToBDE(fftShortString, ffcl_MaxVCheckLength, @aVal, @TempStr[1]);
        i := 0;
        while TempStr[succ(i)] <> #0 do
          inc(i);
        SetLength(TempStr, i);
      end;
    fftNullString :
      begin
        MapFFDataToBDE(fftNullString, pred(ffcl_MaxVCheckLength), @aVal, @TempStr[1]);
        i := 0;
        while TempStr[succ(i)] <> #0 do
          inc(i);
        SetLength(TempStr, i);
      end;
    fftNullAnsiStr :
      begin
        MapFFDataToBDE(fftNullString, pred(ffcl_MaxVCheckLength), @aVal, @TempStr[1]);
        i := 0;
        while TempStr[succ(i)] <> #0 do
          inc(i);
        SetLength(TempStr, i);
      end;
    fftWideString :
      begin
        i := 0;
        while ((char(aVal[i])) +
               (char(aVal[succ(i)]))) <> #0#0 do
          inc(i);
        Move(aVal, TempWideStr, succ(i));
        TempStr := WideCharToString(@TempWideStr);
      end;
{Begin !!.13}
    fftBLOB..fftBLOBTypedBin :
      begin
        Move(aVal, TempInt64, SizeOf(TempInt64));
        TempStr := IntToStr(TempInt64.iHigh) + ':' + IntToStr(TempInt64.iLow);
      end;
{End !!.13}
    else
      begin
        TempStr :=  '';
      end;
  end;
  Result  :=  TempStr;
end;

end.
