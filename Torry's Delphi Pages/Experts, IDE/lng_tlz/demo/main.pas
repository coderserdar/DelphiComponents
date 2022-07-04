unit main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, smLngCmp, StdCtrls, smComponents, smADOPersistence, smLanguage,
  ActnList, Mask, DBCtrls, DB;

type
  Tmainform = class(TsmForm)
    smADODriver: TsmADODriver;
    MainMenu: TMainMenu;
    smLanguageOptions: TsmLanguageOptions;
    ActionList: TActionList;
    File1: TMenuItem;
    OPEN1: TMenuItem;
    SAVE1: TMenuItem;
    EXIT1: TMenuItem;
    LANGUAGE1: TMenuItem;
    LOWERCASE1: TMenuItem;
    UPPERCASE1: TMenuItem;
    smFile: TsmAction;
    smExit: TsmAction;
    smLowercase: TsmAction;
    smUppercase: TsmAction;
    smLanguage: TsmAction;
    smOpenWithParentLanguage: TsmAction;
    smLabel: TsmLabel;
    lLabel: TLabel;
    smOpenWithoutParent: TsmAction;
    smDBForm: TsmAction;
    OPENDBAWAREFORM1: TMenuItem;
    procedure smFileExecute(Sender: TObject);
    procedure smLanguageExecute(Sender: TObject);
    procedure smLowercaseExecute(Sender: TObject);
    procedure smUppercaseExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure smOpenWithParentLanguageExecute(Sender: TObject);
    procedure smOpenWithoutParentExecute(Sender: TObject);
    procedure smExitExecute(Sender: TObject);
    procedure smDBFormExecute(Sender: TObject);
  private
    { Private declarations }
    procedure LanguageChange(Sender: TObject; Language: TsmLanguage);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
  end;

var
  mainform: Tmainform;

implementation
uses smPersistence, smLanguageUtils, smPersistenceUtils, editor, dbform;

{$R *.dfm}

procedure Tmainform.smFileExecute(Sender: TObject);
begin
//
end;

procedure Tmainform.smLanguageExecute(Sender: TObject);
begin
//
end;

procedure Tmainform.smLowercaseExecute(Sender: TObject);
begin
  Language:=0;
  smLowercase.Checked:=true;
end;

procedure Tmainform.smUppercaseExecute(Sender: TObject);
begin
  Language:=1;
  smUppercase.Checked:=true;
end;

procedure Tmainform.FormCreate(Sender: TObject);
begin
  case Language of
    0: smLowercase.Checked:=true;
    1: smUppercase.Checked:=true;
  end;
end;

procedure Tmainform.smOpenWithParentLanguageExecute(Sender: TObject);
begin
  with TeditorForm.Create(Self) do
  try
    Show;
  finally
  end;
end;

procedure Tmainform.LanguageChange(Sender: TObject; Language: TsmLanguage);
begin
  case Language of
    0: lLabel.Caption:='mainForm language change event: lowercase';
    1: lLabel.Caption:=AnsiUpperCase('mainForm language change event: uppercase');
  end;
end;

constructor Tmainform.Create(AOwner: TComponent);
begin
  inherited;
  OnLanguageChange:=LanguageChange;
end;

procedure Tmainform.smOpenWithoutParentExecute(Sender: TObject);
begin
  with TeditorForm.Create(nil) do
  try
    Show;
  finally
  end;
end;

procedure Tmainform.smExitExecute(Sender: TObject);
begin
  Application.Terminate;
end;

procedure Tmainform.smDBFormExecute(Sender: TObject);
begin
  with Tadbform.Create(nil) do
  try
    ADOTable.Connection := smADODriver.ADOConnection;
    ADOTable.Active := true;
    Show;
  finally
  end;
end;

end.
