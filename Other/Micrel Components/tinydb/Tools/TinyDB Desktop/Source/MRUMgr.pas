
{**********************************************************}
{                                                          }
{  MRU Manager Class                                       }
{  Last Modified Date: 8/28/2001                           }
{                                                          }
{**********************************************************}

unit MRUMgr;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, IniFiles{$IFDEF UNICODE}, shlwapi{$ENDIF};

//API declare
{$IFNDEF UNICODE}
function PathCompactPathEx(pszOut: LPSTR; pszSrc: LPCSTR; cchMax: UINT; dwFlags: DWORD): BOOL; stdcall; external 'shlwapi.dll' name 'PathCompactPathExA';
{$ENDIF}
const
  MRU_DefaultDispLength = 30;
  MRU_IniSectionName = 'MRU';

type
  TMRUMgr = class(TObject)
  private
    FIniFileName: string;
    FItems: TStrings;

  public
    constructor Create(IniFileName: string);
    destructor Destroy; override;

    property Items: TStrings read FItems;

    procedure AddMRU(Item: string);
    procedure ReadMRU(var Items: TStrings);
    procedure WriteMRU(Items: TStrings);

    function GetDisplayString(Index: Integer; MaxLen: Integer = MRU_DefaultDispLength): string;
  end;

var
  MRUManager: TMRUMgr;

implementation

uses Misc;

constructor TMRUMgr.Create(IniFileName: string);
begin
  FIniFileName := IniFileName;
  FItems := TStringList.Create;
  ReadMRU(FItems);
end;

destructor TMRUMgr.Destroy;
begin
  FItems.Free;
end;

procedure TMRUMgr.AddMRU(Item: string);
var
  i: Integer;
begin
  for i := 0 to FItems.Count - 1 do
  begin
    if UpperCase(FItems[i]) = UpperCase(Item) then
    begin
      FItems[i] := FItems[0];
      FItems[0] := Item;
      WriteMRU(FItems);
      Exit;
    end;
  end;

  if FItems.Count < 4 then FItems.Add('');
  for i := FItems.Count - 1 downto 1 do
    FItems[i] := FItems[i - 1];
  FItems[0] := Item;
  WriteMRU(FItems);
end;

procedure TMRUMgr.ReadMRU(var Items: TStrings);
var
  Ini: TIniFile;
  i: Integer;
  S: string;
begin
  Ini := TIniFile.Create(FIniFileName);
  Items.Clear;
  for i := 1 to 4 do
  begin
    S := Ini.ReadString(MRU_IniSectionName, 'File' + IntToStr(i), '');
    if S <> '' then Items.Add(S);
  end;
  Ini.Free;
end;

procedure TMRUMgr.WriteMRU(Items: TStrings);
var
  Ini: TIniFile;
  i: Integer;
begin
  Ini := TIniFile.Create(FIniFileName);
  Ini.EraseSection(MRU_IniSectionName);
  for i := 0 to Items.Count - 1 do
    Ini.WriteString(MRU_IniSectionName, 'File' + IntToStr(i + 1), Items[i]);
  Ini.Free;
end;

function TMRUMgr.GetDisplayString(Index: Integer; MaxLen: Integer): string;
var
  DstBuf: array[0..255] of Char;
begin
  PathCompactPathEx(DstBuf, PChar(Items[Index]), MaxLen, 0);
  Result := DstBuf;
end;

end.
