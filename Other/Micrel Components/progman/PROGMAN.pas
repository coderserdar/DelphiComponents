{-------------------------------------------------------}
{                                                       }
{Delphi VCL to add a program group and add program items}
{ by Andrew McLean                                      }
{                                                       }
{ Version 1.0a - 7th Jan 1996                           }
{ Initial release.                                      }
{                                                       }
{ Version 1.1a - 18th Jan 1996                          }
{ Added two properties: programGroup                    }
{                       programItems                    }
{ There is now just one method to make the group:       }
{   makeGroup                                           }
{                                                       }
{               THIS IS A FREE COMPONENT!!              }
{                                                       }
{  If you modify this please let me know.               }
{                                                       }
{  My web: http://www.aerosoft.com.au                   }
{          http://www.aerosoft.com.au/delphi            }
{          http://www.aerosoft.com.au/delphi/progman    }
{                                                       }
{  You can contact me via the 'contact aerosoft' link   }
{                                                       }
{  If anyone knows how to hide the other DDE properties }
{  I'd be most interested in hearing from you.          }
{                                                       }
{-------------------------------------------------------}

{-------------------------------------------------------}
{                                                       }
{ Reference Material:                                   }
{ PC Magazine - Turbo Pascal for Windows                }
{ Neil J. Rubenking                                     }
{ Ziff-Davis Press                                      }
{                                                       }
{ Page: 743-753                                         }
{                                                       }
{-------------------------------------------------------}

unit Progman;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, DdeMan, ShellAPI;

type
  {  TActivateType  }

  TActivateType = (activateNormalSize, activateAndIconize, activateMaximize, iconizeNoActivate);

  {  EItemError  }

  EItemError = class(Exception);

  {  TProgMan  }

  TProgMan = class(TDdeClientConv)
  private
    { Private declarations }
    FOnGroupError: TNotifyEvent;
    FOnItemError: TNotifyEvent;
    FProgramGroup: AnsiString;
    FProgramItems: TstringList;
    FShowGroup: TActivateType;
    procedure SetItems(TS: TStringList);

    procedure CreateGroup(name: AnsiString);
    procedure AddItem(cmdLine, itemName, iconPath: AnsiString);
    procedure OpenProgramManager;
    procedure CloseProgramManager;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure MakeGroup;
    procedure EraseGroup(const name: string);
    function FileRegister(const Ext: string; Assoc: Boolean; const TypeName, Hint, Progr: string; IcoPos: Integer): Boolean;
    procedure FileUnregister(const Ext: string);
  published
    {properties and events}

    property ProgramItems: TStringList read FProgramItems write SetItems;
    property ProgramGroup: AnsiString read FProgramGroup write FProgramGroup;

    property ShowGroup: TActivateType read FshowGroup write FshowGroup;
    property OnGroupError: TNotifyEvent read FOnGroupError write FOnGroupError;
    property OnItemError: TNotifyEvent read FOnItemError write FOnItemError;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Support', [TProgMan]);
end;

{  TProgMan  }

constructor TProgMan.Create(AOwner: TComponent);
begin
  inherited Create(aOwner);
  FshowGroup := activateNormalSize;
  FProgramItems := TStringList.Create;
end;

destructor TProgMan.Destroy;
begin
  CloseLink;
  FProgramItems.Free;
  inherited Destroy;
end;

procedure TProgMan.SetItems(TS: TStringList);
begin
  {We must assign a copy of the string because the 'write' method
  in the property declaration only assigns points to the strings and
  not a copy of them.}
  FprogramItems.Assign(TS);
end;

procedure TProgMan.OpenProgramManager;
begin
  ConnectMode := ddeAutomatic;
  DDEservice := 'progman'; {Delphi BUG: DDEservice is never set (or is it my fault!)}
  SetLink('progman', DDETopic);
  OpenLink;
end;

procedure TProgMan.closeProgramManager;
begin
  CloseLink;
end;

procedure TProgMan.CreateGroup(name: AnsiString);
var
  macro: AnsiString;
  cmd: array[0..255] of AnsiChar;
  i: Integer;
begin
  {create a program group using DDE}
  macro := AnsiString(Format('[CreateGroup(%s)]', [name])) + #13#10;
  StrPCopy(cmd, macro);

  if not ExecuteMacro(cmd, False) then
  begin
    if Assigned(FOnGroupError) then
      FOnGroupError(Self);
  end;

  {Setup display state}
  case FShowGroup of
    activateNormalSize: i := 1;
    activateAndIconize: i := 2;
    activateMaximize: i := 3;
    iconizeNoActivate: i := 7;
  else
    i := 1;
  end;

  macro := AnsiString(Format('[ShowGroup(%s,%d)]', [name, i])) + #13#10;
  StrPCopy(cmd, macro);
  ExecuteMacro(cmd, False);
end;

procedure TProgMan.AddItem(cmdLine, itemName, iconPath: AnsiString);
var
  macro: AnsiString;
  cmd: array[0..255] of AnsiChar;
begin
  {add program items using DDE}

  macro := '[AddItem(' + cmdLine;

  if itemName <> '' then
    macro := macro + ',' + itemName;

  if iconPath <> '' then
    macro := macro + ',' + iconPath;

  macro := macro + ')]' + #13#10;

  StrPCopy(cmd, macro);

  if not ExecuteMacro(cmd, false) then
    if Assigned(FOnItemError) then
      FOnItemError(Self);
end;

procedure TProgMan.MakeGroup;
var
  i: integer;
  s1, s2, s3: AnsiString;

  procedure ParseLine(sx: AnsiString; var s1, s2, s3: AnsiString);
  var
    s: array[1..3] of AnsiString;
    i: Integer;
    done: Boolean;

    function Comma(st: AnsiString): Integer;
    begin
      Result := Pos(',', string(st));
    end;

  begin
    for i := 1 to 3 do
      s[i] := '';

    i := 1;
    done := False;
    repeat
      if Comma(sx) <> 0 then
      begin
        s[i] := Copy(sx, 1, Comma(sx) - 1);
        Delete(sx, 1, Comma(sx));
      end
      else
      begin
        s[i] := sx;
        done := True;
      end;
      Inc(i);
    until done;

    s1 := s[1];
    s2 := s[2];
    s3 := s[3];
  end;

begin
  OpenProgramManager;
  CreateGroup(FProgramGroup);

  with FprogramItems do
    for i := 0 to count - 1 do
    begin
      ParseLine(AnsiString(strings[i]), s1, s2, s3);
      AddItem(s1, s2, s3);
    end;

  CloseProgramManager;
end;

(* this part is addapted by me *)
{Jaro Benes}
{mailto:JBenes@micrel.cz}

procedure TProgMan.EraseGroup; {J.B.}
var
  macro: AnsiString;
  cmd: array[0..255] of AnsiChar;
begin
  openProgramManager;
  {delete a program group using DDE}
  macro := AnsiString(Format('[DeleteGroup(%s)]', [name])) + #13#10;
  strPCopy(cmd, macro);

  if not ExecuteMacro(cmd, false) then
  begin
    if assigned(FOnGroupError) then
      FOnGroupError(Self);
  end;
  CloseProgramManager;
end;

{tahle sekce je pridana jako servisni}
{FileRegister('.mt',true,'MTWorkFile','Pracovní soubor pro MTarif','C:\MTARIF\MTarif.Exe',7);}

function TProgMan.FileRegister(const Ext: string; Assoc: Boolean; const TypeName, Hint, Progr: string; IcoPos: Integer): Boolean;
{const
  extension: PChar = '.mt';
  typename: PChar = 'MT_Work_File';
  HintOfTypeName: PChar = 'Pracovní soubor pro MTarif';
  commandKey: PChar = 'MT_Work_File\shell\open\command';
  iconKey: PChar = 'MT_Work_File\DefaultIcon';
  iconID: PChar = 'C:\MTARIF\MTarif.Exe,7';
  command: PChar = 'C:\MTARIF\MTarif.Exe %1';}
var
  key: HKey;
  A: array[0..255] of Char;
begin
  Result := True; {- kdyz je vsechno OK}
  {vytvoreni polozky extense jako tridy}
  if RegCreateKey(HKEY_CLASSES_ROOT, StrPCopy(A, ext), key) = ERROR_SUCCESS then
    {vytvoreni slozky typoveho jmena/tridy}
    if RegSetValue(key, nil, REG_SZ, StrPCopy(A, typename), 0) = ERROR_SUCCESS then
    begin
      RegCloseKey(key);
      {popisek souboru, ktery se objevi v prirazenych ext jako nazev reg.polozky}
      if RegCreateKey(HKEY_CLASSES_ROOT, StrPCopy(A, typename), key) = ERROR_SUCCESS then
      begin
        if RegSetValue(key, nil, REG_SZ, StrPCopy(A, hint), 0) = ERROR_SUCCESS then RegCloseKey(key)
        else
        begin
          Result := False;
          RegCloseKey(key);
          RegDeleteKey(HKEY_CLASSES_ROOT, StrPCopy(A, ext));
        end
      end;
      {ikona prirazena tomuto typu souboru}
      if RegCreateKey(HKEY_CLASSES_ROOT, StrPCopy(A, typename + '\DefaultIcon'), key) = ERROR_SUCCESS then
      begin
        if RegSetValue(key, nil, REG_SZ, StrPCopy(A, Progr + ',' + IntToStr(IcoPos)), 0) = ERROR_SUCCESS then RegCloseKey(key)
        else
        begin
          Result := False;
          RegCloseKey(key);
          RegDeleteKey(HKEY_CLASSES_ROOT, StrPCopy(A, ext));
        end
      end
      else
      begin
        Result := False;
        RegDeleteKey(HKEY_CLASSES_ROOT, StrPCopy(A, ext));
      end;
      {prikaz pro otevreni souboru zadanym programem}
      if Assoc then
      begin
        if RegCreateKey(HKEY_CLASSES_ROOT, StrPCopy(A, typename + '\shell\open\command'), key) = ERROR_SUCCESS then
        begin
          if RegSetValue(key, nil, REG_SZ, StrPCopy(A, progr + ' %1'), 0) = ERROR_SUCCESS then RegCloseKey(key)
          else
          begin
            Result := False;
            RegCloseKey(key);
            RegDeleteKey(HKEY_CLASSES_ROOT, StrPCopy(A, ext));
          end
        end
        else
        begin
          Result := False;
          RegDeleteKey(HKEY_CLASSES_ROOT, StrPCopy(A, ext));
        end;
      end;
      {---}
    end
    else
    begin
      Result := False;
      RegCloseKey(key);
      RegDeleteKey(HKEY_CLASSES_ROOT, StrPCopy(A, ext));
    end
  else
    Result := False;
end;


{FileUnregister('.mt');}

procedure TProgMan.FileUnregister(const Ext: string);
const eSize = 256;
var
  key: HKey;
  A: array[0..255] of Char;
  P: PChar;
  L: LongInt;
begin
  GetMem(P, eSize); L := eSize - 1;
  try
    if RegOpenKey(HKEY_CLASSES_ROOT, StrPCopy(A, ext), key) = ERROR_SUCCESS then
    begin
      if RegQueryValue(key, nil, P, L) = ERROR_SUCCESS then
      begin
        RegDeleteKey(HKEY_CLASSES_ROOT, P);
      end;
      RegDeleteKey(HKEY_CLASSES_ROOT, StrPCopy(A, ext));
    end;
  finally
    FreeMem(P, eSize)
  end;
end;

end.
