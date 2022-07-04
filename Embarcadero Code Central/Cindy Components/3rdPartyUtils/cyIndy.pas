{   Unit cyImageEn3

    Description:
    Unit with functions to use with Indy components.

    $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
    $  €€€ Accept any PAYPAL DONATION $$$  €
    $      to: mauricio_box@yahoo.com      €
    €€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€

    * ***** BEGIN LICENSE BLOCK *****
    *
    * Version: MPL 1.1
    *
    * The contents of this file are subject to the Mozilla Public License Version
    * 1.1 (the "License"); you may not use this file except in compliance with the
    * License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
    *
    * Software distributed under the License is distributed on an "AS IS" basis,
    * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
    * the specific language governing rights and limitations under the License.
    *
    * The Initial Developer of the Original Code is Mauricio
    * (https://sourceforge.net/projects/tcycomponents/).
    *
    * Donations: see Donation section on Description.txt
    *
    * Alternatively, the contents of this file may be used under the terms of
    * either the GNU General Public License Version 2 or later (the "GPL"), or the
    * GNU Lesser General Public License Version 2.1 or later (the "LGPL"), in which
    * case the provisions of the GPL or the LGPL are applicable instead of those
    * above. If you wish to allow use of your version of this file only under the
    * terms of either the GPL or the LGPL, and not to allow others to use your
    * version of this file under the terms of the MPL, indicate your decision by
    * deleting the provisions above and replace them with the notice and other
    * provisions required by the LGPL or the GPL. If you do not delete the
    * provisions above, a recipient may use your version of this file under the
    * terms of any one of the MPL, the GPL or the LGPL.
    *
    * ***** END LICENSE BLOCK *****}

unit cyIndy;

interface

uses Classes, SysUtils, idGlobal, idHttp, idComponent, idCoderHeader, IdCoderMIME, IdHashMessageDigest, IdFTP, idFtpCommon, IdAllFTPListParsers,
       IdSSLOpenSSL, IdSSLOpenSSLHeaders,
       cyStrUtils, Dialogs;

// Functions to encode/Decode base64:
function Base64_EncodeString(Value: String; const aByteEncoding: IIdTextEncoding = Nil): string;  // Descodificar com Base64_DecodeToString()
function Base64_DecodeToString(Value: String; const aByteEncoding: IIdTextEncoding = nil): String; overload;  // Descodifica o que foi codificado com Base64_EncodeString()

function Base64_DecodeToBytes(Value: String): TidBytes; overload;

function IdHttp_DownloadFile(aSrcUrlFile, aDestFile: String; const OnWorkEvent: TWorkEvent = Nil): Boolean;
function Get_MD5(const aFileName: string): string;
function Get_MD5FromString(const aString: string): string;


// *** Ftp utils *** //
function FTP_GetFiles(AIdFTP: TIdFTP; ADirectory: String; AList: TStrings): Boolean;
function FTP_GetDirectories(AIdFTP: TIdFTP; ADirectory: String; AList: TStrings): Boolean;
function FTP_DirectoryExists(AIdFTP: TIdFTP; ADirectory: String): Boolean;
function FTP_ForceAndChangeDirectories(AIdFTP: TIdFTP; Directories: String): Boolean;

// *** Certification utils *** //

implementation

function Base64_EncodeString(Value: String; const aByteEncoding: IIdTextEncoding = Nil): string;  // IIdTextEncoding since Indy 10.6 ...
var
  Encoder: TIdEncoderMIME;
begin
  Encoder := TIdEncoderMIME.Create(nil);
  try
    Result := Encoder.EncodeString(Value, aByteEncoding);
  finally
    Encoder.Free;
  end;
end;

function Base64_DecodeToString(Value: String; const aByteEncoding: IIdTextEncoding = nil): String; overload;
var
  Decoder: TIdDecoderMIME;
begin
  Decoder := TIdDecoderMIME.Create(nil);
  try
    Result := Decoder.DecodeString(Value, aByteEncoding);
  finally
    Decoder.Free;
  end;
end;

function Base64_DecodeToBytes(Value: String): TidBytes; overload;
var
  Decoder: TIdDecoderMIME;
begin
  Decoder := TIdDecoderMIME.Create(nil);
  try
    Result := Decoder.DecodeBytes(Value);
  finally
    Decoder.Free;
  end;
end;

function IdHttp_DownloadFile(aSrcUrlFile, aDestFile: String; const OnWorkEvent: TWorkEvent = Nil): Boolean;
var
  Http: TIdHTTP;
  MS: TMemoryStream;
begin
  Result := false;
  Http := TIdHTTP.Create(nil);
  try
    MS := TMemoryStream.Create;
    try
      if Assigned(OnWorkEvent) then
        Http.OnWork := OnWorkEvent;

      Http.Get(aSrcUrlFile, MS);
      MS.SaveToFile(aDestFile);
      Result := true;
    finally
      MS.Free;
    end;
  finally
    Http.Free;
  end;
end;

function Get_MD5(const aFileName: string): string;
var
  idmd5: TIdHashMessageDigest5;
  fs: TFileStream;
begin
  idmd5 := TIdHashMessageDigest5.Create;
  fs := TFileStream.Create(aFileName, fmOpenRead OR fmShareDenyWrite) ;
  try
    Result := idmd5.HashStreamAsHex(fs);
  finally
    fs.Free;
    idmd5.Free;
  end;
end;

function Get_MD5FromString(const aString: string): string;
var
  idmd5: TIdHashMessageDigest5;
begin
  idmd5 := TIdHashMessageDigest5.Create;
  try
    Result := idmd5.HashStringAsHex(aString);
  finally
    idmd5.Free;
  end;
end;

// *** Ftp utils *** //
function FTP_GetFiles(AIdFTP: TIdFTP; ADirectory: String; AList: TStrings): Boolean;
var
  TmpList: TStrings;
  StrLine, aFilename: String;
  i, c: Integer;
begin
  Result := false;
  AList.Clear;

  if not AIdFTP.UseMLIS then  // Does not anderstand MLSD command in AIdFTP.ExtListDir() call ...
  begin
    try
      AIdFTP.List(ADirectory, false);
      for i := 0 to AIdFTP.DirectoryListing.Count-1 do
        if (AIdFTP.DirectoryListing[i].FileName <> '.') and (AIdFTP.DirectoryListing[i].FileName <> '..') then
        // not working ...   if AIdFTP.DirectoryListing[i].Size <> 0 then // if AIdFTP.DirectoryListing[i].ItemType = ditFile then
            AList.Add(AIdFTP.DirectoryListing[i].FileName);

      Result := true;
    except
    end;

    Exit;
  end;



  TmpList := TStringList.Create;
  try
    AIdFTP.ExtListDir(TmpList, ADirectory);

    for i := 0 to TmpList.Count-1 do
    begin
      StrLine := TmpList[i];         // type=file;size=3977;modify=20140529111949;UNIX.mode=0644;UNIX.uid=1005;UNIX.gid=1003;unique=805g1fc05a7; w13-231.jpg

      if SubString_Get(StrLine, ';', 1) = 'type=file' then
      begin
        aFilename := '';
        for c := length(StrLine) downto 1 do
          if StrLine[c] <> ';'
          then aFilename := StrLine[c] + aFilename
          else Break;

        aFilename := Trim(aFilename);
        AList.Add(aFilename);
      end;
    end;

    Result := true;
  except

  end;
  TmpList.Free;
end;

function FTP_GetDirectories(AIdFTP: TIdFTP; ADirectory: String; AList: TStrings): Boolean;
var
  TmpList: TStrings;
  StrLine, aFilename: String;
  i, c: Integer;
begin
  Result := false;
  AList.Clear;

  if not AIdFTP.UseMLIS then  // Does not anderstand MLSD command in AIdFTP.ExtListDir() call ...
  begin
    try
      AIdFTP.List(ADirectory, false);
      for i := 0 to AIdFTP.DirectoryListing.Count-1 do
        if (AIdFTP.DirectoryListing[i].FileName <> '.') and (AIdFTP.DirectoryListing[i].FileName <> '..') then
        // not working ...   if AIdFTP.DirectoryListing[i].Size <> 0 then // if AIdFTP.DirectoryListing[i].ItemType = ditFile then
            AList.Add(AIdFTP.DirectoryListing[i].FileName);

      Result := true;
    except
    end;

    Exit;
  end;



  TmpList := TStringList.Create;
  try
    AIdFTP.ExtListDir(TmpList, ADirectory);      // !!! RHR RSCAN Error !!!

    for i := 0 to TmpList.Count-1 do
    begin
      StrLine := TmpList[i];        // type=dir;size=3977;modify=20140529111949;UNIX.mode=0644;UNIX.uid=1005;UNIX.gid=1003;unique=805g1fc05a7; test

      if SubString_Get(StrLine, ';', 1) = 'type=dir' then
      begin
        aFilename := '';
        for c := length(StrLine) downto 1 do
          if StrLine[c] <> ';'
          then aFilename := StrLine[c] + aFilename
          else Break;

        aFilename := Trim(aFilename);
        AList.Add(aFilename);
      end;
    end;

    Result := true;
  except
    on E: Exception do
      ShowMessage('Indy error: ' + E.Message);
  end;
  TmpList.Free;
end;

// Use this function in try except/finally block !
function FTP_DirectoryExists(AIdFTP: TIdFTP; ADirectory: String): Boolean;
var
  ListDirs: TStrings;
  i: Integer;
begin
  Result := false;
  ADirectory := AnsiUpperCase(ADirectory);
  ListDirs := TStringlist.Create;

  if not FTP_GetDirectories(AIdFTP, '', ListDirs) then  // !!! Error dentro de FTP_GetDirectories()
    raise Exception.Create('Could not retrieve list of directories with FTP_GetDirectories() !')
  else
    for i := 0 to ListDirs.Count-1 do
      if AnsiUpperCase(ListDirs[i]) = ADirectory then
      begin
        Result := true;
        Break;
      end;

  ListDirs.Free;
end;

function FTP_ForceAndChangeDirectories(AIdFTP: TIdFTP; Directories: String): Boolean;
var
  i: Integer;
  SubDir: String;
begin
  Result := false;

  try
    for i := 1 to SubString_Count(Directories, '/') do
    begin
      SubDir := SubString_Get(Directories, '/', i);
      if SubDir <> '' then
      begin
        if not FTP_DirectoryExists(AIdFTP, SubDir) then
          AIdFTP.MakeDir(SubDir);

        AIdFTP.ChangeDir(SubDir);
      end;
    end;

    Result := true;
  except

  end;
end;

end.
