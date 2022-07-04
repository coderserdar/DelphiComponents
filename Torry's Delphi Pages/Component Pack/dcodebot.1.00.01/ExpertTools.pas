
(********************************************************)
(*                                                      *)
(*  Codebot Class Library @ www.codebot.org/delphi      *)
(*                                                      *)
(*  1.00.01 Open Source Released 2006                   *)
(*                                                      *)
(********************************************************)

unit ExpertTools;

interface

{$I STD.INC}

uses
  Classes, FileCtrl, Forms, SysUtils, ToolsAPI, Windows, Menus;

function CreateMenuItem(const Parent: string): TMenuItem;

function GetNewProjectPath: string;

function GetNewProjectFileName: string;

implementation

function CreateMenuItem(const Parent: string): TMenuItem;
var
  Form: TForm;
  Menu: TMenuItem;
  P: PChar;
  StartPos: PChar;
  MenuName: string;
  j: Integer;
begin
  Form := Application.MainForm;
  Menu := nil;
  for j := 0 to Form.ComponentCount - 1 do
    if Form.Components[j] is TMainMenu then
    begin
      Menu := TMainMenu(Form.Components[j]).Items;
      P := PChar(Parent);
      while (P^ <> #0) and (Menu <> nil)  do
      begin
        while P^ = #124 do
          Inc(P);
        StartPos := P;
        repeat
          Inc(P);
        until P^ in [#0, #124];
        SetString(MenuName, StartPos, P - StartPos);
        Menu := Menu.Find(MenuName);
      end;
      Break;
    end;
  Result := nil;
  if Menu = nil then
    Exit;
  Result := TMenuItem.Create(Form);
  Menu.Add(Result);
end;

function ExtractDrive(const Path: string): string;
var
  P: PChar;
begin
  Result := '';
  if Path = '' then
    Exit;
  P := PChar(Path);
  while P^ = '\' do
    Inc(P);
  while (P^ <> #0) or (P^ <> '\')  do
    Inc(P);
  SetString(Result, PChar(Path), PChar(Path) - P);
  Result := Result + '\';
end;

function GetNewProjectPath: string;
const
  DelphiPath = 'Bin\Delphi32.exe';
  ProjectPath = 'Projects';
begin
  Result := Application.ExeName;
  Result := Copy(Result, 1, Length(Result) - Length(DelphiPath)) + ProjectPath;
  if DirectoryExists(Result) then
    Result := Result + '\'
  else
    Result := ExtractDrive(Result);
end;

function GetNewProjectFileName: string;
var
  ModuleServices: IOTAModuleServices;
  Module:  IOTAModule;
  ProjectGroup: IOTAProjectGroup;
  Strings: TStrings;
  j: Integer;
begin
  ModuleServices := BorlandIDEServices as IOTAModuleServices;
  for j := 0 to ModuleServices.ModuleCount - 1 do
  begin
    Module := ModuleServices.Modules[j];
    if Module.QueryInterface(IOTAProjectGroup, ProjectGroup) = S_OK then
      Break;
  end;
  Result := '';
  j := 0;
  if ProjectGroup <> nil then
  begin
    Strings := TStringList.Create;
    try
      for j := 0 to ProjectGroup.ProjectCount - 1 do
        Strings.Add(UpperCase(ExtractFileName(ProjectGroup.Projects[j].FileName)));
      for j := 0 to ProjectGroup.ProjectCount - 1 do
        if Strings.IndexOf(Format('PROJECT%d.DPR', [j + 1])) = -1 then
          Break;
     finally
       Strings.Free;
     end;
  end;
  Inc(j);
  Result := Format('%sProject%d.dpr', [GetNewProjectPath, j]);
end;

end.
