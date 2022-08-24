unit Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Rtti, System.Classes,
  System.Variants, FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Menus,
  DM, FMX.plsController
  {$IF CompilerVersion >= 25.0 }
  , FMX.StdCtrls
  {$IFEND}
  ;

type
  TfrmMain = class(TForm)
    mmMain: TMainMenu;
    miFile: TMenuItem;
    miLanguages: TMenuItem;
    miN1: TMenuItem;
    miExit: TMenuItem;
    sbChildWindow: TButton;
    sbOpenTest: TButton;
    sbTestMessage: TButton;
    plsController1: TplsController;
    procedure miExitClick(Sender: TObject);
    procedure sbTestMessageClick(Sender: TObject);
    procedure sbOpenTestClick(Sender: TObject);
    procedure sbChildWindowClick(Sender: TObject);
    procedure miChangeLanguageClick(Sender:TObject);
    procedure plsController1BeforeLangChange(Sender: TObject);
    procedure plsController1Error(Sender: TObject);
    procedure plsController1InitLangManager(Sender: TObject);
    procedure plsController1LanguageChanged(Sender: TObject);
  private
    { Private declarations }
    procedure ReadAvailableLanguages;
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.fmx}

uses
  Child, LangConsts;

////////////////////////// plsController events /////////////////////////////////////////////
procedure TfrmMain.plsController1InitLangManager(Sender: TObject);
begin
  { LanguageManager Initialization }
  { Here you can setup miscellaneous options for loading localization files
    via the main TplsController (and its associated LanguageManager) }

  // TplsController(Sender).LangManager.FileNames.CommaText:='DemoMain,Comm*';  // set this, if you want to filter language files to those names (masks supported)
  // TplsController(Sender).LangManager.RecursiveFolder:=True;                  // set this, if you want to get language files from subfolders too
  TplsController(Sender).LangManager.Folder:='..\..\Langs';                     // set this, if your language files are in different folder then default (AppExe\Langs)
  // TplsController(Sender).DefaultLangCode:='';                            // instead of default language code, you can set a language code loaded from your ini/config here

  { End LanguageManager Initialization }

  ReadAvailableLanguages;
end;

procedure TfrmMain.plsController1BeforeLangChange(Sender: TObject);
begin
  LangConsts.LanguageChanged; // update user defined constants (variables/resources)
  TplsController(Sender).LangManager.Clear(True, False);  // clear localization strings table for constants (speeds up searching for properties a little)
end;

procedure TfrmMain.plsController1LanguageChanged(Sender: TObject);
var
  j:Integer;
begin
  Application.Title := Self.Caption;
  for j := 0 to miLanguages.ItemsCount - 1 do
    if miLanguages.Items[j].TagString=plsController1.LanguageCode then
    begin
      miLanguages.Items[j].IsChecked:=True;
      break;
    end;
end;

procedure TfrmMain.plsController1Error(Sender: TObject);
var
  lm: string;
begin
  with TplsController(Sender) do
  begin
    if LanguageCode='' then lm := ''
    else lm :=LanguageName+' ('+LanguageCode+'): ';
    MessageDlg(lm+'['+Owner.Name+' ('+Owner.ClassName+')] '+LastError, TMsgDlgType.mtWarning, [TMsgDlgBtn.mbOK], 0);
  end;
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
    MI.TagString:=plsController1.LanguageCodes[i];
    MI.Text:=plsController1.LanguageNames[i];
    MI.OnClick:=miChangeLanguageClick;
    MI.RadioItem:=True;
    MI.GroupIndex:=1;
    miLanguages.AddObject(MI);
  end;
end;

procedure TfrmMain.miChangeLanguageClick(Sender:TObject);
begin
  // load another translation
  plsController1.LanguageCode:=TMenuItem(Sender).TagString;
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
  MessageDlg(str_test_msg, TMsgDlgType.mtConfirmation, mbYesNoCancel, 0);
end;

procedure TfrmMain.miExitClick(Sender: TObject);
begin
  Close;
end;

end.
