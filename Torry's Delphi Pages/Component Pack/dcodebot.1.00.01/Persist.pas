
(********************************************************)
(*                                                      *)
(*  Codebot Class Library @ www.codebot.org/delphi      *)
(*                                                      *)
(*  1.00.01 Open Source Released 2006                   *)
(*                                                      *)
(********************************************************)

unit Persist;

interface

{$I STD.INC}

uses
  Classes, SysUtils, Forms, Windows, ActiveX, Dialogs, Controls, Storage,
  StrTools, Grip, IniFiles;

{ TPersistSettings }

type
  TPersistSettings = class(TObject)
  private
    FRoot: TFolderStructure;
    FForms: TFolderStructure;
  public
    constructor Create;
    destructor Destroy; override;
    function CreateForm(FormClass: TFormClass; Owner: TComponent): TForm;
    function ExecuteForm(FormClass: TFormClass): Boolean;
    procedure LoadForm(Form: TCustomForm);
  end;

var
  Settings: TPersistSettings;

implementation

{ TPersistSettings }

function GetLocalPath: string;
var
  Mutex: THandle;
  IniFile: TIniFile;
begin
  Mutex := CreateMutex(nil, False, 'GoodieMutex');
  try
    WaitForSingleObject(Mutex, INFINITE);
    try
      IniFile := TIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'));
      try
        Result := IniFile.ReadString('USER', 'PATH',
          ExtractPath(Application.ExeName));
      finally
        IniFile.Free;
      end;
    finally
      ReleaseMutex(Mutex);
    end;
  finally
    CloseHandle(Mutex);
  end;
  if (Result <> '') and (Result[Length(Result)] <> '\') then
    Result := Result + '\';
end;

constructor TPersistSettings.Create;
var
  Path: string;
begin
  inherited Create;
  Path := GetLocalPath + GetUserName;
  CreateDir(Path);
  FRoot := TFolderStructure.Create(Path + '\config.dat');
  FForms := FRoot.OpenFolder('Forms');
end;

destructor TPersistSettings.Destroy;
begin
  FRoot.Free;
  inherited Destroy;
end;

function TPersistSettings.CreateForm(FormClass: TFormClass;
  Owner: TComponent): TForm;
var
  GripForm: TGripForm;
begin
  Result := FormClass.Create(Owner);
  if Result is TGripForm then
  begin
    GripForm := Result as TGripForm;
    GripForm.Stream := FForms.OpenStream(FormClass.ClassName).AsStream;
  end;
end;

function TPersistSettings.ExecuteForm(FormClass: TFormClass): Boolean;
begin
  with CreateForm(FormClass, nil) do
  try
    Result := ShowModal = mrOK;
  finally
    Free;
  end;
end;

procedure TPersistSettings.LoadForm(Form: TCustomForm);
begin
  if Form is TGripForm then
    (Form as TGripForm).Stream := FForms.OpenStream(Form.ClassName).AsStream;
end;

end.
