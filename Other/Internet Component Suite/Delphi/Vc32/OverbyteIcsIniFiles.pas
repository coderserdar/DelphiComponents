{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Arno Garrels <arno.garrels@gmx.de>
Creation:     Nov 10, 2008
Description:  Classes and little helpers for use with the ICS demo applications.
Version:      7.00
EMail:        http://www.overbyte.be       francois.piette@overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 2008 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium. Fax: +32-4-365.74.56
              <francois.piette@overbyte.be>

              This software is provided 'as-is', without any express or
              implied warranty.  In no event will the author be held liable
              for any  damages arising from the use of this software.

              Permission is granted to anyone to use this software for any
              purpose, including commercial applications, and to alter it
              and redistribute it freely, subject to the following
              restrictions:

              1. The origin of this software must not be misrepresented,
                 you must not claim that you wrote the original software.
                 If you use this software in a product, an acknowledgment
                 in the product documentation would be appreciated but is
                 not required.

              2. Altered source versions must be plainly marked as such, and
                 must not be misrepresented as being the original software.

              3. This notice may not be removed or altered from any source
                 distribution.

              4. You must register this software by sending a picture postcard
                 to Francois PIETTE. Use a nice stamp and mention your name,
                 street address, EMail address and any comment you like to say.

        Contains TIcsUtf8IniFile a variant of TMemIniFile that is capable to
        handle UTF-8 encoded INI files. By default it attempts to preserve ANSI
        format.

History:


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsIniFiles;

interface

{$Q-}           { Disable overflow checking           }
{$B-}           { Enable partial boolean evaluation   }
{$T-}           { Untyped pointers                    }
{$X+}           { Enable extended syntax              }
{$H+}           { Use long Strings                    }
{$J+}           { Allow typed constant to be modified }
{$I OverbyteIcsDefs.inc}
{$IFDEF DELPHI6_UP}
    {$WARN SYMBOL_PLATFORM   OFF}
    {$WARN SYMBOL_LIBRARY    OFF}
    {$WARN SYMBOL_DEPRECATED OFF}
{$ENDIF}
{$IFDEF BCB3_UP}
    {$ObjExportAll On}
{$ENDIF}

uses
  Windows, SysUtils, Classes, IniFiles,
  OverbyteIcsStreams,
  OverbyteIcsUtils,
  OverbyteIcsTypes;  // for TBytes

type
  EIcsIniFile = class(Exception);
  THackHashedStringList = class(THashedStringList);
  TIcsUtf8IniFile = class(TCustomIniFile)
  private
    FFileName: String;
    FSections: TStringList;
    FPreserveAnsi: Boolean;
    function  AddSection(const Section: String): TStrings;
    function  GetCaseSensitive: Boolean;
    procedure SetCaseSensitive(Value: Boolean);
    function  GetEncoding(Stream: TStream): Cardinal;
  protected
    procedure LoadValues; virtual;  
  public
    constructor Create(const FileName: String;
      PreserveAnsi: Boolean = TRUE);
    destructor Destroy; override;
    procedure Clear;
    procedure DeleteKey(const Section, Ident: String); override;
    procedure EraseSection(const Section: String); override;
    procedure GetStrings(List: TStrings);
    procedure ReadSection(const Section: String; Strings: TStrings); override;
    procedure ReadSections(Strings: TStrings); override;
    procedure ReadSectionValues(const Section: String; Strings: TStrings); override;
    function  ReadString(const Section, Ident, Default: String): String; override;
    procedure Rename(const FileName: String; Reload: Boolean);
    procedure SetStrings(List: TStrings);
    procedure UpdateFile; override;
    procedure WriteString(const Section, Ident, Value: String); override;
    property  CaseSensitive: Boolean read GetCaseSensitive write SetCaseSensitive;
  end;

  TIcsIniFile = TIcsUtf8IniFile;

  function GetIcsIniFileName: String;
  
implementation

{ TIcsUtf8IniFile }

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TIcsUtf8IniFile.Create(const FileName: String; PreserveAnsi: Boolean = TRUE);
begin
    inherited Create(FileName);
    FPreserveAnsi := PreserveAnsi;
    FSections := THackHashedStringList.Create;
    LoadValues;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TIcsUtf8IniFile.Destroy;
begin
    if FSections <> nil then
        Clear;
    FSections.Free;
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsUtf8IniFile.AddSection(const Section: String): TStrings;
begin
    Result := THackHashedStringList.Create;
    try
        THackHashedStringList(Result).CaseSensitive := CaseSensitive;
        FSections.AddObject(Section, Result);
    except
        Result.Free;
        raise;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsUtf8IniFile.Clear;
var
    I: Integer;
begin
    for I := 0 to FSections.Count - 1 do
        TObject(FSections.Objects[I]).Free;
    FSections.Clear;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsUtf8IniFile.DeleteKey(const Section, Ident: String);
var
    I, J: Integer;
    Strings: TStrings;
begin
    I := FSections.IndexOf(Section);
    if I >= 0 then begin
        Strings := TStrings(FSections.Objects[I]);
        J := Strings.IndexOfName(Ident);
        if J >= 0 then
            Strings.Delete(J);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsUtf8IniFile.EraseSection(const Section: String);
var
    I: Integer;
begin
    I := FSections.IndexOf(Section);
    if I >= 0 then begin
        TStrings(FSections.Objects[I]).Free;
        FSections.Delete(I);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsUtf8IniFile.GetCaseSensitive: Boolean;
begin
    Result := FSections.CaseSensitive;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsUtf8IniFile.GetStrings(List: TStrings);
var
    I, J: Integer;
    Strings: TStrings;
begin
    List.BeginUpdate;
    try
        for I := 0 to FSections.Count - 1 do begin
            List.Add('[' + FSections[I] + ']');
            Strings := TStrings(FSections.Objects[I]);
            for J := 0 to Strings.Count - 1 do List.Add(Strings[J]);
                List.Add('');
        end;
    finally
        List.EndUpdate;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
const
    CP_UTF16Le  = 1200;
    CP_UTF16Be  = 1201;

function TIcsUtf8IniFile.GetEncoding(Stream: TStream): Cardinal;
var
    Len : Integer;
    Bom : array [0..2] of Byte;
begin
    Stream.Seek(0, soBeginning);
    Len := Stream.Read(Bom[0], SizeOf(BOM));
    if (Len = 3) and (Bom[0] = $EF) and (Bom[1] = $BB) and (Bom[2] = $BF) then
        Result := CP_UTF8
    else if (Len >= 2) and (Bom[0] = $FF) and (Bom[1] = $FE) then
        Result := CP_UTF16Le
    else if (Len >= 2) and (Bom[0] = $FE) and (Bom[1] = $FF) then
        Result := CP_UTF16Be
    else begin
        Stream.Seek(0, soBeginning);
        Result := CP_ACP;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsUtf8IniFile.LoadValues;
var
    List: TStringList;
    Stream : TFileStream;
    TxtStream : TTextStream;
    S : AnsiString;
    Enc : Cardinal;
begin
    if (FileName <> '') and FileExists(FileName) then
    begin
        List := TStringList.Create;
        try
            Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
            try
                Enc := GetEncoding(Stream);
                if Enc = CP_UTF16Le then
                    raise EIcsIniFile.Create('UTF-16 LE is unsupported')
                else if Enc = CP_UTF16Be then
                    raise EIcsIniFile.Create('UTF-16 BE is unsupported');
                TxtStream := TTextStream.Create(Stream, 512);
                try
                    while TxtStream.ReadLn(S) do begin
                        if (Enc = CP_UTF8) then
                        {$IFDEF UNICODE}
                            List.Add(Utf8ToStringW(S))
                        {$ELSE}
                            List.Add(Utf8ToStringA(S))
                        {$ENDIF}
                        else
                            List.Add(String(S)); // Assume ANSI, current CP
                    end;
                finally
                    TxtStream.Free;
                end;
            finally
                Stream.Free;
            end;
            SetStrings(List);
        finally
            List.Free;
        end;
    end
    else
        Clear;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsUtf8IniFile.ReadSection(const Section: String;
  Strings: TStrings);
var
    I, J: Integer;
    SectionStrings: TStrings;
begin
    Strings.BeginUpdate;
    try
        Strings.Clear;
        I := FSections.IndexOf(Section);
        if I >= 0 then begin
            SectionStrings := TStrings(FSections.Objects[I]);
            for J := 0 to SectionStrings.Count - 1 do
                Strings.Add(SectionStrings.Names[J]);
        end;
    finally
        Strings.EndUpdate;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsUtf8IniFile.ReadSections(Strings: TStrings);
begin
    Strings.Assign(FSections);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsUtf8IniFile.ReadSectionValues(const Section: String;
  Strings: TStrings);
var
    I: Integer;
begin
    Strings.BeginUpdate;
    try
        Strings.Clear;
        I := FSections.IndexOf(Section);
        if I >= 0 then
            Strings.Assign(TStrings(FSections.Objects[I]));
    finally
        Strings.EndUpdate;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsUtf8IniFile.ReadString(const Section, Ident,
  Default: String): String;
var
    I: Integer;
    Strings: TStrings;
begin
    I := FSections.IndexOf(Section);
    if I >= 0 then begin
        Strings := TStrings(FSections.Objects[I]);
        I := Strings.IndexOfName(Ident);
        if I >= 0 then begin
            Result := Copy(Strings[I], Length(Ident) + 2, Maxint);
            Exit;
        end;
    end;
    Result := Default;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsUtf8IniFile.Rename(const FileName: String; Reload: Boolean);
begin
  FFileName := FileName;
  if Reload then
    LoadValues;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsUtf8IniFile.SetCaseSensitive(Value: Boolean);
var
    I: Integer;
begin
    if Value <> FSections.CaseSensitive then begin
        FSections.CaseSensitive := Value;
        for I := 0 to FSections.Count - 1 do
            with THackHashedStringList(FSections.Objects[I]) do begin
                CaseSensitive := Value;
                Changed;
            end;
        THackHashedStringList(FSections).Changed;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsUtf8IniFile.SetStrings(List: TStrings);
var
    I, J: Integer;
    S: String;
    Strings: TStrings;
begin
    Clear;
    Strings := nil;
    for I := 0 to List.Count - 1 do begin
        S := Trim(List[I]);
        if (S <> '') and (S[1] <> ';') then
            if (S[1] = '[') and (S[Length(S)] = ']') then begin
                Delete(S, 1, 1);
                SetLength(S, Length(S)-1);
                Strings := AddSection(Trim(S));
            end
            else
            if Strings <> nil then begin
                J := Pos('=', S);
                if J > 0 then // remove spaces before and after '='
                    Strings.Add(Trim(Copy(S, 1, J-1)) + '=' + Trim(Copy(S, J+1, MaxInt)) )
                else
                    Strings.Add(S);
            end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsUtf8IniFile.UpdateFile;
var
    List: TStringList;
    Stream : TFileStream;
    TxtStream : TTextStream;
    I : Integer;
    IsUtf8 : Boolean;
    Bom : TBytes;
    procedure SetBom;
    begin
        SetLength(Bom, 3);
        Bom[0] := $EF;
        Bom[1] := $BB;
        Bom[2] := $BF;
    end;
begin
    List := TStringList.Create;
    try
        GetStrings(List);
        Stream := TFileStream.Create(FileName, fmCreate);
        try
            if not FPreserveAnsi then begin
                { If plain ASCII text (<= #127) do not convert to UTF-8 }
                for I := 0 to List.Count - 1 do
                    if not IsUsAscii(List[I]) then begin
                        SetBom;
                        Break;
                    end;
            end
            else begin
            {$IFDEF UNICODE}
                { If the Unicode String can be represented in current ANSI }
                { code page do not convert to UTF-8.                       }
                for I := 0 to List.Count - 1 do
                    if not CheckUnicodeToAnsi(List[I]) then begin
                        SetBom;
                        Break;
                    end;
            {$ENDIF}
            end;
            if Length(Bom) > 0 then begin
                Stream.Write(Bom[0], Length(Bom));
                IsUtf8 := TRUE;
            end
            else
                IsUtf8 := FALSE;
            TxtStream := TTextStream.Create(Stream, 512);
            try
                for I := 0 to List.Count -1 do
                    if IsUtf8 then
                        TxtStream.WriteLn(StringToUtf8(List[I]))
                    else
                        TxtStream.WriteLn(AnsiString(List[I]));
            finally
                TxtStream.Free;
            end;
        finally
            Stream.Free;
        end;
    finally
        List.Free;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsUtf8IniFile.WriteString(const Section, Ident, Value: String);
var
    I: Integer;
    S: String;
    Strings: TStrings;
begin
    I := FSections.IndexOf(Section);
    if I >= 0 then
        Strings := TStrings(FSections.Objects[I])
    else
        Strings := AddSection(Section);
    S := Ident + '=' + Value;
    I := Strings.IndexOfName(Ident);
    if I >= 0 then
        Strings[I] := S
    else
        Strings.Add(S);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function GetCommonAppDataFolder(const SubPath: String): String;
var
    hSHFolderDLL: HMODULE;
    f_SHGetFolderPath: function(hwndOwner: HWND; nFolder: Integer;
        hToken: THandle; dwFlags: DWORD; pszPath: PChar): HRESULT; stdcall;
    Buf: array[0..MAX_PATH - 1] of Char;
const
    CSIDL_LOCAL_APPDATA = $001C;
    SHGFP_TYPE_CURRENT  = 0;
begin
    Result := '';
    hSHFolderDLL := LoadLibrary('shfolder.dll');
    if hSHFolderDLL = 0 then
        Exit;
    try
    {$IFDEF UNICODE}
        @f_SHGetFolderPath := GetProcAddress(hSHFolderDLL, 'SHGetFolderPathW');
    {$ELSE}
        @f_SHGetFolderPath := GetProcAddress(hSHFolderDLL, 'SHGetFolderPathA');
    {$ENDIF}
        if @f_SHGetFolderPath = nil then
            Exit;
        if Succeeded(f_SHGetFolderPath(0, CSIDL_LOCAL_APPDATA, 0,
                                       SHGFP_TYPE_CURRENT, Buf)) then begin
            Result := ExpandFileName(Buf);
            Result := IncludeTrailingPathDelimiter(Result) + SubPath;
            try
                if not ForceDirectories(Result) then
                    Result := '';
            except
                Result := '';
            end;
        end;
    finally
        FreeLibrary(hSHFolderDLL);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function GetIcsIniFileName: String;
begin
    Result := GetCommonAppDataFolder('ICS');
    if Result = '' then
        Result := ChangeFileExt(ParamStr(0), '.ini')
    else
        Result := IncludeTrailingPathDelimiter(Result) +
                        ExtractFileName(ChangeFileExt(ParamStr(0), '.ini'));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
end.
