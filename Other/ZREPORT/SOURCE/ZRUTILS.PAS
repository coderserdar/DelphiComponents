unit ZRUtils;

interface

{$I ZRDefine.inc}

uses
  Windows,                                         // WinAPI
  SysUtils,                                        // Delphi RTL
  Classes;                                         // Delphi VCL

function TempFileName : String;
function ValidFileName(const FileName: string): Boolean;

procedure SetComponentName(Component: TComponent; const Name: TComponentName);
procedure RenameComponent(Component: TComponent; const OldPrefix, NewPrefix: String);

procedure ZRError(Error: Integer; Args: array of const);

implementation

function TempFileName : String;
var
  aName,
  aDir : array[0..255] of Char;
begin
  GetTempPath(SizeOf(aDir), aDir);
  GetTempFileName(aDir, PChar('ZRP'), 0, aName);
  Result := StrPas(aName);
end;

function ValidFileName(const FileName: string): Boolean;
  function HasAny(const Str, Substr: string): Boolean;
  var
    I: Integer;
  begin
    Result := False;
    for I := 1 to Length(Substr) do begin
      if Pos(Substr[I], Str) > 0 then begin
        Result := True;
        Break;
      end;
    end;
  end;
begin
  Result := (FileName <> '') and (not HasAny(FileName, ';,=+<>"[]|'));
  if Result then Result := Pos('\', ExtractFileName(FileName)) = 0;
end;

type
  EZRError = class(Exception);

procedure ZRError(Error: Integer; Args: array of const);
begin
  raise EZRError.CreateResFmt(Error, Args);
end;

procedure SetComponentName(Component: TComponent; const Name: TComponentName);
var
  i : Integer;
  S : String;
begin
  i := 0;
  S := '';
  while Component.Owner.FindComponent(Name + S) <> nil do begin
    S := IntToStr(i);
    Inc(i);
  end;
  Component.Name := Name + S;
end;

procedure RenameComponent(Component: TComponent; const OldPrefix, NewPrefix: String);
begin
  if (Component <> nil) and (OldPrefix = copy(Component.Name, 1, Length(OldPrefix))) then
    try
      Component.Name:= NewPrefix + copy(Component.Name, Length(OldPrefix)+1, Length(Component.Name));
    except
      on EComponentError do {Ignore rename errors };
    end;
end;

end.

