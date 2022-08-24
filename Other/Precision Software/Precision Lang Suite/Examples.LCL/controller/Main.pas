unit Main;

interface

uses
  LCLIntf, LCLType, lresources, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, Menus, DM, plsController;

type
  TfrmMain = class(TForm)
    sbChildWindow: TButton;
    mmMain: TMainMenu;
    miFile: TMenuItem;
    miLanguages: TMenuItem;
    N1: TMenuItem;
    miExit: TMenuItem;
    sbOpenTest: TButton;
    plsController1: TplsController;
    sbTestMessage: TButton;
    procedure sbChildWindowClick(Sender: TObject);
    procedure sbOpenTestClick(Sender: TObject);
    procedure miExitClick(Sender: TObject);
    procedure plsController1BeforeLangChange(Sender: TObject);
    procedure sbTestMessageClick(Sender: TObject);
    procedure plsController1LanguageChanged(Sender: TObject);
    procedure plsController1Error(Sender: TObject);
    procedure miChangeLanguageClick(Sender:TObject);
    procedure plsController1InitLangManager(Sender: TObject);
  private
    { Private declarations }
    procedure ReadAvailableLanguages;
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

uses
  Child, plsDialogs, LangConsts;

////////////////////////// plsController events /////////////////////////////////////////////
procedure TfrmMain.plsController1InitLangManager(Sender: TObject);
begin
  { LanguageManager Initialization }
  { Here you can setup miscellaneous options for loading localization files
    via the main TplsController (and its associated LanguageManager) }

  // TplsController(Sender).LangManager.FileNames.CommaText:='DemoMain,Comm*';  // set this, if you want to filter language files to those names (masks supported)
  // TplsController(Sender).LangManager.RecursiveFolder:=True;                  // set this, if you want to get language files from subfolders too
  // TplsController(Sender).LangManager.Folder:='..\..\Langs';                  // set this, if your language files are in different folder then default (AppExe\Langs)
  // TplsController(Sender).DefaultLangCode:='';                            // instead of default language code, you can set a language code loaded from your ini/config here

  { End LanguageManager Initialization }

  ReadAvailableLanguages;
end;

procedure TfrmMain.plsController1BeforeLangChange(Sender: TObject);
begin
  plsDialogs.LanguageChanged; // update constants (variables) in plsDialogs unit
  LangConsts.LanguageChanged; // update user defined constants (variables/resources)
  TplsController(Sender).LangManager.Clear(True, False);  // clear localization strings table for constants (speeds up searching for properties a little)
end;

procedure TfrmMain.plsController1LanguageChanged(Sender: TObject);
var
  j:Integer;
begin
  Application.Title := Self.Caption;
  for j := 0 to miLanguages.Count - 1 do
    if miLanguages.Items[j].Hint=plsController1.LanguageCode then
    begin
      miLanguages.Items[j].Checked:=True;
      break;
    end;
end;

procedure TfrmMain.plsController1Error(Sender: TObject);
begin
  with TplsController(Sender) do
    MessageDlg('', LanguageName+' ('+LanguageCode+'): ['+Owner.Name+' ('+Owner.ClassName+')] '+LastError, mtWarning);
end;

//////////////////////////// Language selection methods and events ///////////////////////////
procedure TfrmMain.ReadAvailableLanguages;
var
  MI:TMenuItem;
  i:Integer;
begin
  // fill menu with available languages
  miLanguages.Clear;
  for i := 0 To plsController1.LanguagesCount - 1 do
  begin
    MI:=TMenuItem.Create(self);
    MI.Hint:=plsController1.LanguageCodes[i];
    MI.Caption:=plsController1.LanguageNames[i];
    MI.OnClick:=@miChangeLanguageClick;
    MI.RadioItem:=True;
    MI.GroupIndex:=1;
    miLanguages.Add(MI);
  end;
end;

procedure TfrmMain.miChangeLanguageClick(Sender:TObject);
begin
  // load another translation
  plsController1.LanguageCode:=TMenuItem(Sender).Hint;
end;

//////////////////////////// Other stuff /////////////////////////////////////////////////////
procedure TfrmMain.sbChildWindowClick(Sender: TObject);
begin
  TfrmChild.Create(nil).Show;
end;

procedure TfrmMain.sbOpenTestClick(Sender: TObject);
begin
  frmDM.dlgOpen.Execute;
end;

procedure TfrmMain.sbTestMessageClick(Sender: TObject);
begin
  MessageDlg('', str_test_msg, mtConfirmation, mbYesNoCancel);
end;

procedure TfrmMain.miExitClick(Sender: TObject);
begin
  Close;
end;

initialization
  {$I Main.lrs}

end.
