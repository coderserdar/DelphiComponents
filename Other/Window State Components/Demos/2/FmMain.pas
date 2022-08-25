{
 * FmMain.pas
 *
 * Main form for the Window State Components StandAloneDemo demo program.
 *
 * $Rev: 1383 $
 * $Date: 2013-04-26 17:49:14 +0100 (Fri, 26 Apr 2013) $
 *
 * Any copyright in this file is dedicated to the Public Domain.
 * http://creativecommons.org/publicdomain/zero/1.0/
}

unit FmMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, PJWdwState, StdCtrls, ExtCtrls;

type
  TForm1 = class(TForm)
    CheckBox1: TCheckBox;
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
  private
    fWdwState: TPJWdwState;
    function IsInWorkArea: Boolean;
    procedure SetInWorkArea(Flag: Boolean);
  end;

var
  Form1: TForm1;

implementation

uses
  IniFiles;

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  Memo1.WordWrap := True;
  
  // Create stand-alone window state component
  // DO NOT use TPJWdwState.Create() constructor for this
  fWdwState := TPJWdwState.CreateStandAlone(Self);

  // Set up component properties: all have default values

  // specify ini file, its root directory, and section in it
  // we will also use the same ini file to store whether we want to keep
  // window in work space: see IsInWorkArea and SetInWorkArea methods below
  fWdwState.IniRootDir := rdAppDataDir;
  fWdwState.IniFileName := 'DelphiDabbler\Lib\PJWdwState\StandAloneDemo.ini';
  fWdwState.Section := 'Window';

  // we want component to work automatically
  fWdwState.AutoSaveRestore := True;

  // we may want component to keep window in work area: depends on ini file
  // setting
  if IsInWorkArea then
  begin
    // We keep in work area if recorded in ini file
    fWdwState.Options := [woFitWorkArea];
    CheckBox1.Checked := True;
  end;
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  // Record value in ini file for future reference
  SetInWorkArea(CheckBox1.Checked);
end;

function TForm1.IsInWorkArea: Boolean;
  {This method only used to check whether component should be set to keep in
  work area on next run: uses same ini file as window state component but uses
  [Options] section.}
var
  Ini: TIniFile;
begin
  // make sure the required directory exists
  // NOTE: use IniFilePath function instead IniFileName here since IniFileName
  // may not be found if it is a relative file name that is not on the path
  ForceDirectories(ExtractFileDir(fWdwState.IniFilePath));
  Ini := TIniFile.Create(fWdwState.IniFilePath);
  try
    Result := Ini.ReadBool('Options', 'WorkArea', False);
  finally
    Ini.Free;
  end;
end;

procedure TForm1.SetInWorkArea(Flag: Boolean);
  {This method only used to record whether component should be set to keep in
  work area on next run: uses same ini file as window state component but uses
  [Options] section.}
var
  Ini: TIniFile;
begin
  // NOTE: use IniFilePath function instead IniFileName here since IniFileName
  // may not be found if it is a relative file name that is not on the path
  Ini := TIniFile.Create(fWdwState.IniFilePath);
  try
    Ini.WriteBool('Options', 'WorkArea', Flag);
  finally
    Ini.Free;
  end;
end;

end.
