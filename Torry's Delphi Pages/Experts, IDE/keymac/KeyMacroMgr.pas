{

  Keyboard Macro Manager for Borland Delphi

  By Daniel Cunningham (dcunningham@email.com)

}

unit KeyMacroMgr;

interface

procedure Register;

implementation

uses Windows, Classes, SysUtils, ToolsAPI, Menus, Forms, Dialogs, Controls,
  ActiveX, Registry;

type
  TKeyboardMacroManager = class(TNotifierObject, IUnknown, IOTANotifier,
    IOTAKeyboardBinding)
    procedure KeyboardMacroSave(const Context: IOTAKeyContext; KeyCode: TShortCut;
      var BindingResult: TKeyBindingResult);
    procedure KeyboardMacroLoad(const Context: IOTAKeyContext; KeyCode: TShortCut;
      var BindingResult: TKeyBindingResult);
    function GetBindingType: TBindingType;
    function GetDisplayName: string;
    function GetName: string;
    procedure BindKeyboard(const BindingServices: IOTAKeyBindingServices);
  private
    function CreateFileOpenDialog: TOpenDialog;
    function CreateFileSaveDialog: TSaveDialog;
    procedure SavePath(Path: string);
    function LoadPath: string;
  end;

resourcestring
  sKeyboardMacroManager = 'Keyboard Macro Manager';

{ TKeyboardMacroManager }

procedure TKeyboardMacroManager.BindKeyboard(
  const BindingServices: IOTAKeyBindingServices);
begin
  BindingServices.AddKeyBinding([ShortCut(Ord('S'), [ssCtrl, ssShift])], KeyboardMacroSave, nil);
  BindingServices.AddKeyBinding([ShortCut(Ord('O'), [ssCtrl, ssShift])], KeyboardMacroLoad, nil);
end;

function TKeyboardMacroManager.GetBindingType: TBindingType;
begin
  Result := btPartial;
end;

function TKeyboardMacroManager.GetDisplayName: string;
begin
  Result := sKeyboardMacroManager;
end;

function TKeyboardMacroManager.GetName: string;
begin
  Result := 'x42.KeyboardMacroManager';  //do not localize
end;

function TKeyboardMacroManager.CreateFileOpenDialog: TOpenDialog;
begin
  Result := TOpenDialog.Create(Application);
  with Result do
  begin
    Filter := 'Borland Key Macros (*.bkm)|*.bkm|Any File (*.*)|*.*';
    Options := [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing];
    Title := 'Load Key Macro';
    DefaultExt := 'bkm';
    InitialDir := LoadPath;
    Options := [ofNoChangeDir];
  end;
end;

function TKeyboardMacroManager.CreateFileSaveDialog: TSaveDialog;
begin
  Result := TSaveDialog.Create(Application);
  with Result do
  begin
    Filter := 'Borland Key Macros (*.bkm)|*.bkm|Any File (*.*)|*.*';
    Title := 'Save Key Macro';
    DefaultExt := 'bkm';
    InitialDir := LoadPath;
    Options := [ofNoChangeDir];
  end;
end;

procedure TKeyboardMacroManager.KeyboardMacroLoad(
  const Context: IOTAKeyContext; KeyCode: TShortCut;
  var BindingResult: TKeyBindingResult);
var
  KS: IOTAKeyboardServices;
  Stream: IStream;
begin
  KS := Context.KeyboardServices;
  if not KS.CurrentPlayback.IsPlaying and not KS.CurrentRecord.IsRecording then
  begin
    with CreateFileOpenDialog do
    try
      if Execute then
      try
        Stream := TStreamAdapter.Create(TFileStream.Create(FileName,
          fmOpenRead or fmShareDenyWrite), soOwned);
        KS.CurrentPlayback.Clear;
        KS.CurrentPlayback.ReadFromStream(Stream);
        SavePath(ExtractFilePath(FileName));
      except
        Application.HandleException(Self);
      end;
    finally
      Free;
    end;
  end;
  BindingResult := krHandled;
end;

procedure TKeyboardMacroManager.KeyboardMacroSave(
  const Context: IOTAKeyContext; KeyCode: TShortCut;
  var BindingResult: TKeyBindingResult);
var
  KS: IOTAKeyboardServices;
  Stream: IStream;
begin
  KS := Context.KeyboardServices;
  if not KS.CurrentPlayback.IsPlaying and not KS.CurrentRecord.IsRecording then
  begin
    with CreateFileSaveDialog do
    try
      if Execute then
      try
        Stream := TStreamAdapter.Create(TFileStream.Create(FileName,
          fmCreate or fmShareExclusive), soOwned);
        KS.CurrentPlayback.WriteToStream(Stream);
        SavePath(ExtractFilePath(FileName));
      except
        Application.HandleException(Self);
      end;
    finally
      Free;
    end;
  end;
  BindingResult := krHandled;
end;

procedure Register;
begin
  (BorlandIDEServices as IOTAKeyBoardServices).AddKeyboardBinding(TKeyboardMacroManager.Create);
end;

// registry location constants
const
  RegPathKey = '\SOFTWARE\X42\Keyboard Macro Manager\';
  RegPathName = 'LastPath';

function TKeyboardMacroManager.LoadPath: string;
var
  Reg: TRegistry;
begin
  Reg := TRegistry.Create;
  try
    if Reg.OpenKey(RegPathKey, True) then
      Result := Reg.ReadString(RegPathName);
  finally
    Reg.Free;
  end;
end;

procedure TKeyboardMacroManager.SavePath(Path: string);
var
  Reg: TRegistry;
begin
  Reg := TRegistry.Create;
  try
    if Reg.OpenKey(RegPathKey, True) then
      Reg.WriteString(RegPathName, Path);
  finally
    Reg.Free;
  end;
end;

end.
