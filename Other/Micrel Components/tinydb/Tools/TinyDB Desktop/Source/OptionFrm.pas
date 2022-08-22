unit OptionFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics,
  Controls, Forms, Dialogs, IniFiles, StdCtrls,
  ExtCtrls, MainFrm, Registry, BaseFrm;

type

  TOptions = record
    LanguageName: string;
    FormBounds: TRect;
    OpenTableMode: TOpenTableMode;
    RememberSize: Boolean;
    AddToMenu: Boolean;
  end;

  TOptionForm = class(TBaseForm)
    OpenTableModeRadioGroup: TRadioGroup;
    MiscGroupBox: TGroupBox;
    RemSizeCheckBox: TCheckBox;
    OkButton: TButton;
    CancelButton: TButton;
    AddToMenuCheckBox: TCheckBox;
    procedure OkButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
  private
    { Private declarations }
    procedure AddToMenu(Add: Boolean);
  public
    { Public declarations }
    procedure TransLanguage; override;
    procedure SetData(Value: TOptions);
    procedure GetData(var Value: TOptions);
  end;

var
  OptionForm: TOptionForm;
  Options: TOptions;

procedure LoadOptions;
procedure SaveOptions;

function ShowOptionForm: Boolean;

implementation

uses Misc, LangMgr;

{$R *.DFM}

function ShowOptionForm: Boolean;
var
  Frm: TOptionForm;
begin
  Frm := TOptionForm.Create(Application);
  Frm.SetData(Options);
  Result := Frm.ShowModal = mrOk;
  if Result then Frm.GetData(Options);
  Frm.Free;
end;

procedure SetDefaultOptions(var Opt: TOptions);
const
  DefaultWidth = 640;
  DefaultHeight = 480;
var
  R: TRect;
begin
  Opt.LanguageName := ''; //Auto detect language
  R.Left := (Screen.Width - DefaultWidth) div 2;
  R.Top := (Screen.Height - DefaultHeight) div 2;
  R.Right := R.Left + DefaultWidth;
  R.Bottom := R.Top + DefaultHeight;
  Opt.FormBounds := R;
  Opt.OpenTableMode := otGrid;
  Opt.RememberSize := True;
  Opt.AddToMenu := False;
end;

procedure LoadOptions;
var
  IniFile: TIniFile;
  Section: string;
begin
  SetDefaultOptions(Options); ;
  IniFile := TIniFile.Create(GetIniFileName);
  try
    Section := 'Options';
    Options.LanguageName := IniFile.ReadString(Section, 'LanguageName', Options.LanguageName);
    Options.FormBounds.Left := IniFile.ReadInteger(Section, 'FormLeft', Options.FormBounds.Left);
    Options.FormBounds.Top := IniFile.ReadInteger(Section, 'FormTop', Options.FormBounds.Top);
    Options.FormBounds.Right := IniFile.ReadInteger(Section, 'FormRight', Options.FormBounds.Right);
    Options.FormBounds.Bottom := IniFile.ReadInteger(Section, 'FormBottom', Options.FormBounds.Bottom);
    Options.OpenTableMode := TOpenTableMode(IniFile.ReadInteger(Section, 'OpenTableMode', Byte(Options.OpenTableMode)));
    Options.RememberSize := IniFile.ReadBool(Section, 'RememberSize', Options.RememberSize);
    Options.AddToMenu := IniFile.ReadBool(Section, 'AddToMenu', Options.AddToMenu);
  finally
    IniFile.Free;
  end;
end;

procedure SaveOptions;
var
  IniFile: TIniFile;
  Section: string;
begin
  IniFile := TIniFile.Create(GetIniFileName);
  try
    Section := 'Options';
    IniFile.WriteString(Section, 'LanguageName', Options.LanguageName);
    IniFile.WriteInteger(Section, 'FormLeft', Options.FormBounds.Left);
    IniFile.WriteInteger(Section, 'FormTop', Options.FormBounds.Top);
    IniFile.WriteInteger(Section, 'FormRight', Options.FormBounds.Right);
    IniFile.WriteInteger(Section, 'FormBottom', Options.FormBounds.Bottom);
    IniFile.WriteInteger(Section, 'OpenTableMode', Byte(Options.OpenTableMode));
    IniFile.WriteBool(Section, 'RememberSize', Options.RememberSize);
    IniFile.WriteBool(Section, 'AddToMenu', Options.AddToMenu);
  finally
    IniFile.Free;
  end;
end;

procedure TOptionForm.TransLanguage;
begin
  inherited;
  OpenTableModeRadioGroup.Items[0] := AppLangMgr.Trans('Grid');
  OpenTableModeRadioGroup.Items[1] := AppLangMgr.Trans('Card');
end;

procedure TOptionForm.SetData(Value: TOptions);
begin
  OpenTableModeRadioGroup.ItemIndex := Byte(Value.OpenTableMode);
  RemSizeCheckBox.Checked := Value.RememberSize;
  AddToMenuCheckBox.Checked := Value.AddToMenu;
end;

procedure TOptionForm.GetData(var Value: TOptions);
begin
  Value.OpenTableMode := TOpenTableMode(OpenTableModeRadioGroup.ItemIndex);
  Value.RememberSize := RemSizeCheckBox.Checked;
  Value.AddToMenu := AddToMenuCheckBox.Checked;
end;

procedure TOptionForm.AddToMenu(Add: Boolean);
var
  R: TRegistry;
  S: string;
begin
  R := TRegistry.Create;
  R.RootKey := HKEY_CLASSES_ROOT;

  R.OpenKey('\.tdb', True);
  S := 'tdbfile';
  R.WriteString('', S);
  if Add then
  begin
    R.OpenKey('\' + S, True);
    R.WriteString('', 'TinyDB File');
    R.OpenKey('\' + S + '\Shell\Open\Command', True);
    R.WriteString('', Application.ExeName + ' "%1"');
    R.OpenKey('\' + S + '\DefaultIcon', True);
    R.WriteString('', Application.ExeName + ',0');
  end
  else
    R.DeleteKey('\' + S + '\Shell\Open');

  R.Free;
end;

procedure TOptionForm.OkButtonClick(Sender: TObject);
begin
  ModalResult := mrOk;
  AddToMenu(AddToMenuCheckBox.Checked);
end;

procedure TOptionForm.CancelButtonClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

end.
