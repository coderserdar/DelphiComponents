{*********************************************************}
{* FlashFiler: DLL used to perform FlashFiler v1.5x      *}
{* in the conversion program that is used to convert     *}
{* v1.5x tables to v2.x tables                           *}
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

unit uFF1Data;

interface

uses Windows, Classes;

{ The conversion program is designed to use a single memory manager
  for the application and the FlashFiler 1.5x DLL. This is to prevent
  the inherrent problems when using string types between an
  an application and a DLL. We also did not want to require our users
  to have to ship the ShareMem DLL with their applications. The three
  following procedures allow the application to manage its memory
  with the DLL's memory manager.}
procedure FF1GetMem(var P : Pointer; aSize : Integer);
procedure FF1FreeMem(P : Pointer);
procedure FF1ReallocMem(var P : Pointer; aSize : Integer);

procedure FF1TableDataDictionary(var aDict : TStream); stdcall;
  { retrieves a FF1 data dictionary into a TStream}
procedure FF1TableFirst; stdcall;
  { moves to the first record in a FF1 table}
procedure FF1TableNext; stdcall;
  { moves to the next record in a FF1 table}
function  FF1TableFieldValue(aFieldNo : Integer): Variant; stdcall;
  { retrieves the value of aField into a variant}
procedure FF1DirOpen(aPath : PChar); stdcall;
  { opens a FF1 database }
function  FF1GetAutoInc : Longint; stdcall;
  { retrieves the auto-increment seed}
function  FF1IsFileBLOB(aFieldNo : Integer;
                    var aBuffer  : array of Byte) : Boolean; stdcall;
  { determines if a BLOB Field is a file BLOB. If so, it copies the
    file name into aBuffer.}
function  FF1TableOpen(aTableName: PChar): Integer; stdcall;
  { opens a FF1 table.}
procedure FF1TableClose; stdcall;
  { Closes a FF1 table.}
function  FF1TableEOF : Boolean; stdcall;
  { Checks if a FF1 table is positioned at the end of file.}
function  FF1TableRecordCount : Integer; stdcall;
  { Retrieves the record count for a FF1 table}

implementation

uses
 SysUtils, FFLLDict, FFLLBase, FFSrEng, FFSrBase, FFSrCmd, FFSrHlpr,
 FFSrComm, FFLLProt, FFTbBLOB, FFTbData, FFSTDate, FFSrMisc;

{$I FFCONST.INC}

type
  PDateTime = ^TDateTime;

var
  OurServerEngine  : TffServerEngine = nil;
  DB               : TffSrDatabase = nil;
  FCursor          : TffSrCursor = nil;
  FDatabase        : string = '';
  FTableName       : string = '';
  RecordBuf        : PffByteArray = nil;
  CursorTableRefNr : TffWord32;

const ffc_AliasClientID = -1;
{====================================================================}
procedure FF1GetMem(var P : pointer; aSize : Integer);
begin
 GetMem(P, aSize);
end;
{--------}
procedure FF1FreeMem(P : Pointer);
begin
  FreeMem(P);
end;
{--------}
procedure FF1ReallocMem(var P : pointer; aSize : Integer);
begin
  ReallocMem(P, aSize);
end;
{--------}
function FF1GetAutoInc : Longint;
var
  FileBlock : PffBlock;
begin
  FileBlock := OurServerEngine.BufferManager.GetBlock(FCursor.Table.Files[0] , 0, False);
  Move(FileBlock^[80], Result, SizeOf(Result));
end;
{--------}
function FF1IsFileBLOB(aFieldNo : Integer; var aBuffer : array of byte) : Boolean;
var
  FileName   : TffFullFileName;
  BLOBNr     : TffWord32;
  BLOBIsNull : Boolean;
begin {Assumption: this is only being called for TBLOBFields}
  with FCursor.Table do
    begin
      Dictionary.GetRecordField(aFieldNo, RecordBuf, BLOBIsNull,
                                PffByteArray(@BLOBNr));
      result := (not BLOBIsNull) and
                FFTblGetFileNameBLOB(Files[Dictionary.BLOBFileNumber],
                                     BLOBNr, FileName);
     end;
  if result then
    begin
      Move(FileName[0], aBuffer[0], succ(byte(FileName[0])));
    end;
end;
{--------}
procedure FF1TableDataDictionary(var aDict: TStream);
begin
  OurServerEngine.TableGetDictionary(FCursor.Database.DatabaseID,
                                     FTableName, false, aDict);
end;
{--------}
procedure FF1TableFirst;
begin
  CursorTableRefNr := 0;
  FF1TableNext;
end;
{--------}
procedure FF1TableNext;
begin
  FCursor.Table.GetNextRecordSeq(CursorTableRefNr, RecordBuf);
end;
{--------}
function FF1TableFieldValue(aFieldNo : Integer): Variant;
var
  FldIsNull : Boolean;
  Buffer    : array[0..8192] of Char; { 8192=dsMaxStringSize in DB.pas}
  BufferW   : array[0..8192] of WideChar;                              {!!.11}

   {--------}
   function GetSTDate : TDateTime;
   var
     STD : TStDate;
   begin
     FCursor.Table.Dictionary.GetRecordField(aFieldNo, RecordBuf, FldIsNull, @STD);
     result := StDateToDateTime(STD);
   end;
   {--------}
   function GetSTTime : TDateTime;
   var
     STT: TStTime;
   begin
     FCursor.Table.Dictionary.GetRecordField(aFieldNo, RecordBuf, FldIsNull, @STT);
     result:= StTimeToDateTime(STT);
   end;
   {--------}
   function GetBLOBAsVariant: Variant;
   var
     SourceBLOBNr : TffWord32;
     BLOBLen      : Longint;
     Err          : DWORD;
     Buff         : PChar;
     s            : string;
       {--------}
       function GetBLOBSize : Longint;
       begin
         Err:= OurServerEngine.BLOBGetLength(FCursor.CursorID,
                                             SourceBLOBNr, result);
       if Err <> 0 then
         result := 0;
       end;
       {--------}
       procedure ReadBLOB;
       var
         BytesRead : Longint;
       begin
         {fetch BLOB Len BlobLen into s}
         Err := OurServerEngine.BLOBRead(FCursor.CursorID,
                                         SourceBLOBNr, 0,
                                         BLOBLen, Buff^, BytesRead);
       end;
       {--------}
   begin
     with FCursor.Table.Dictionary do
       begin
         GetRecordField(aFieldNo, RecordBuf, FldIsNull, @SourceBLOBNr);
         BLOBLen := GetBLOBSize;
         if (BLOBLen > 0) and (SourceBLOBNr <> 0) then begin
           GetMem(Buff, BLOBLen+1);
           try
             ReadBLOB;
             Buff[BLOBLen] := #0;
             SetString(s, Buff, BLOBLen);
             result := s;
           finally
             FreeMem(Buff, BLOBLen + 1);
           end;
         end else
           Result:= Null;
       end;
   end;
   {--------}
   function GetByteArrayAsVariant : Variant;
   var
     Data : Pointer;
   begin
     with FCursor.Table.Dictionary do
       begin
         Result := VarArrayCreate([0, FieldLength[aFieldNo] - 1], varByte);
         Data:= VarArrayLock(Result);
         try
           GetRecordField(aFieldNo, RecordBuf, FldIsNull, Data);       {!!.02}
         finally
           VarArrayUnlock(Result);
         end;
       end;
   end;
   {--------}
   function GetShortStringAsVariant : Variant;
   var
     S : string[255];
   begin
     with FCursor.Table.Dictionary do
       begin
         GetRecordField(aFieldNo, RecordBuf, FldIsNull, @S);
         Result:= S;
       end;
   end;
   {--------}
   function GetStringAsVariant : Variant;
   var
     S : string;
   begin
     with FCursor.Table.Dictionary do
       begin
         SetLength(S, FieldLength[aFieldNo]);
         GetRecordField(aFieldNo, RecordBuf, FldIsNull, @Buffer);
         S := Buffer;
         Result := S;
       end;
   end;
   {--------}
{Begin !!.11}
   function GetWideStringAsVariant : Variant;
   var
     S : Widestring;
   begin
     with FCursor.Table.Dictionary do
       begin
         SetLength(S, FieldLength[aFieldNo]);
         GetRecordField(aFieldNo, RecordBuf, FldIsNull, @BufferW);
         S := BufferW;
         Result := S;
       end;
   end;
{End !!.11}
   {--------}

type
  PBoolean = ^Boolean;
var
  P    : PChar;
  Wide : array [0..1] of WideChar;
begin
  with FCursor.Table.Dictionary do
    begin
      GetRecordField(aFieldNo, RecordBuf, FldIsNull, nil);
      if FldIsNull then begin
        Result:= Null;
        exit;
      end;
      P := PChar(RecordBuf) + FieldOffset[aFieldNo];
      case FieldType[aFieldNo] of
        fftBoolean : result:= PBoolean(p)^;
        fftChar :
          begin
            result:= P^;
          end;
        fftWideChar :
          begin
            StringToWideChar(StrPas(P), Wide, 2);
            result := Wide[0];
          end;
        fftByte       : result := PByte(P)^;
        fftWord16     : result := PWord(P)^;
        fftWord32     : result := PffWord32(P)^;
        fftInt8       : result := Shortint(P^);
        fftInt16      : result := PSmallint(P)^;
        fftInt32      : result := PLongint(P)^;
        fftAutoInc    : result := PLongint(P)^;
        fftSingle     : result := PSingle(P)^;
        fftDouble     : result := PDouble(P)^;
        fftExtended   : result := PExtended(P)^;
        fftComp       : result := Comp(Pointer(P)^);
        fftCurrency   : result := PCurrency(P)^;
        fftStDate     : result := VarFromDateTime(GetSTDate);
        fftStTime     : result := VarFromDateTime(GetSTTime);
        fftDateTime   : result := PDateTime(P)^ - 693594;
        fftBLOB..fftBLOBTypedBin : result := GetBLOBAsVariant;
        fftByteArray  : result := GetByteArrayAsVariant;
        fftShortString, fftShortAnsiStr :
                        result := GetShortStringAsVariant;
        fftNullString, fftNullAnsiStr :
                        result := GetStringAsVariant;
        fftWideString : result := GetWideStringAsVariant;              {!!.11}
      end;
    end;
end;
{--------}
procedure FF1TableClose;
begin
  if RecordBuf <> nil then
    ReAllocMem(RecordBuf, 0);
  if (OurServerEngine<>nil) and (FCursor<>nil) then
     OurServerEngine.CursorClose(FCursor.CursorID);
  FCursor:= nil;
  DB.free;
  DB:= nil;
  OurServerEngine.Free;
  OurServerEngine := nil;
end;
{--------}
function FF1TableEOF : Boolean;
begin
  Result := CursorTableRefNr = 0;
end;
{--------}
function FF1TableRecordCount : Integer;
var
  RecordInfo: TffRecordInfo;
begin
  FFTblGetRecordInfo(FCursor.Table.Files[0],RecordInfo);
  Result := RecordInfo.riRecCount;
end;
{--------}
procedure FF1DirOpen(aPath: PChar);
begin
  FDatabase:= aPath;
end;
{--------}
function FF1TableOpen(aTableName : PChar) : Integer;
var
  Hash, Err : TffWord32;
  CursorID  : Longint;
begin
  if RecordBuf <> nil then
    FF1TableClose;
  Result := -1;
  FTableName := aTableName;
  try
    if OurServerEngine = nil then begin
      OurServerEngine:= TffServerEngine.Create;
      { do not create FF server tables}
      OurServerEngine.Configuration.GeneralInfo^.giReadOnly:= true;
      if OurServerEngine.Configuration.UserList.Count=0 then
        FFCreateAdminUser;
      FFProcessAliasScript;
      {create a client}
      Err:= OurServerEngine.ClientAdd(ffc_AliasClientID, '', 'admin', Hash);
      if Err <> 0 then
        Exit;
      {open a no alias database}
      Err := OurServerEngine.DatabaseOpenNoAlias(ffc_AliasClientID,
                                                 FDatabase,
                                                 omReadWrite,
                                                 smExclusive,
                                                 DB);
      if Err <> 0 then begin
        OurServerEngine.ClientRemove(ffc_AliasClientID);
        Exit;
      end;
    end;
    Err := OurServerEngine.TableOpen(DB.DatabaseID,
                                     ChangeFileExt(aTableName, ''),
                                     False,
                                     '',
                                     0,
                                     omReadOnly,                      {!!.01}
                                     smShared,
                                     CursorID,
                                      nil);
                                                                      {Start !!.01}
    { If we receive an error about a bad stream block, we need to see
      if this is an encrypted server table. We do this by telling
      the server engine to open the table for the server (3rd
      parameter). NOTE: This error always comes about this way because
      the stream block is always the first encrypted block in an
      encrypted table.}
    if Err = DBIERR_FF_BadStreamBlock then
      Err := OurServerEngine.TableOpen(DB.DatabaseID,
                                       ChangeFileExt(aTableName, ''),
                                       True,
                                       '',
                                       0,
                                       omReadOnly,                    {!!.01}
                                       smShared,
                                       CursorID,
                                       nil);                          {End !!.01}
    if Err <> 0 then
      exit;
    OurServerEngine.CheckCursorIDAndGet(CursorID, FCursor);
    ReAllocMem(RecordBuf, FCursor.Table.Dictionary.RecordLength);
    Result:= 0;
  except
  end;
end;
{====================================================================}
end.
