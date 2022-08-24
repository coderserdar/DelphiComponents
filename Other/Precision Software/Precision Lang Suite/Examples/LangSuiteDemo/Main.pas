{-----------------------------------------------------------------------------
  Precision Language Suite for VCL

  written by Precision software & consulting
            copyright ©  2008 - 2013
            Email : info@be-precision.com
            Web : http://www.be-precision.com

  Purpose: Main demo unit

  The source code is given as is. The author is not responsible
  for any possible damage done due to the use of this code.
  This unit can be freely used in any application. The complete
  source code remains property of the author and may not be distributed,
  published, given or sold in any form as such. No parts of the source
  code can be included in any other component or application without
  written authorization of the author.
------------------------------------------------------------------------------}

unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Menus, ComCtrls, Consts;

type
  TForm1 = class(TForm)
    lbField1: TLabel;
    pumLanguages: TPopupMenu;
    lbLabelName: TLabel;
    lbName: TLabel;
    sbMsgWarn: TButton;
    sbMsgInfo: TButton;
    sbProperty: TButton;
    mmMain: TMainMenu;
    miLanguage: TMenuItem;
    miFile: TMenuItem;
    N1: TMenuItem;
    miExit: TMenuItem;
    mTest: TMemo;
    lvTest: TListView;
    sbChildWindow: TButton;
    edField2: TEdit;
    lbField2: TLabel;
    sbMessage1: TButton;
    sbMessage2: TButton;
    dlgFont: TFontDialog;
    sbFont: TButton;
    sbResStr: TButton;
    procedure FormCreate(Sender: TObject);
    procedure pumLanguagesClick(Sender:TObject);
    procedure LanguageChanged(Sender:TObject);
    procedure sbMsgWarnClick(Sender: TObject);
    procedure sbMsgInfoClick(Sender: TObject);
    procedure sbPropertyClick(Sender: TObject);
    procedure miExitClick(Sender: TObject);
    procedure sbChildWindowClick(Sender: TObject);
    procedure sbMessage1Click(Sender: TObject);
    procedure sbMessage2Click(Sender: TObject);
    procedure sbFontClick(Sender: TObject);
    procedure sbResStrClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  Child,
  plsLangMan,
  plsDialogs,
  LangConsts;


{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
var
  i,j:Integer;
  MI,MI2:TMenuItem;
  AL:TStringList;
begin
  LanguageManager.OnLanguageChanged:=LanguageChanged; // assign language changed event
  if LanguageManager.LanguageCode<>'' then
    LanguageChanged(LanguageManager);                 // if language files are already loaded, then perform the event now

  AL:=TStringList.Create;                             // fill menu items with available languages
  pumLanguages.Items.Clear;
  miLanguage.Clear;
  try
    LanguageManager.GetAvailableLanguages(AL);
    for i := 0 to AL.Count - 1 do
    begin
      MI:=TMenuItem.Create(self);
      MI2:=TMenuItem.Create(self);
      MI.Hint:=AL.Names[i];
      MI2.Hint:=MI.Hint;
      MI.Caption:=AL.ValueFromIndex[i];
      MI2.Caption:=MI.Caption;
      MI.OnClick:=pumLanguagesClick;
      MI2.OnClick:=MI.OnClick;
      for j := 0 to pumLanguages.Items.Count - 1 do
        if MI.Caption<pumLanguages.Items[j].Caption then
        begin
          pumLanguages.Items.Insert(j,MI);
          miLanguage.Insert(j,MI2);
          Break;
        end;
      if MI.Parent=nil then
      begin
        pumLanguages.Items.Add(MI);
        miLanguage.Add(MI2);
      end;
    end;
  finally
    AL.Free;
  end;
end;

procedure TForm1.pumLanguagesClick(Sender:TObject);
begin
  LanguageManager.LanguageCode:=TMenuItem(Sender).Hint; // load another language files
  if Length(LanguageManager.LastError)>0 then
    MessageDlg(Caption,LanguageManager.LastError,mtWarning,[mbOK]);
end;

procedure TForm1.LanguageChanged(Sender:TObject);
var
  i:Integer;
begin
  plsDialogs.LanguageChanged;               // update constants (variables) in plsDialogs unit
  LangConsts.LanguageChanged;               // update user defined constants (variables)
  LanguageManager.Clear(True,False);        // clear localization strings table for constants
  if not LanguageManager.LangVCL(Self) then // update vcl properties for this form
    MessageDlg(Caption,LanguageManager.LastError,mtWarning,[mbOK]);
  lbName.Caption:=LanguageManager.LanguageName+' ('+LanguageManager.LanguageCode+')'; // display the current language name and code if you want

  Application.Title:=Self.Caption;          // update application title if you want

  for i := 0 to Screen.FormCount - 1 do                         // inform other forms/frames/modules that language changed
    SendMessage(Screen.Forms[i].Handle,WM_LANGUAGECHANGED,0,0); // main form must not have WMLanguageChanged method
end;

procedure TForm1.miExitClick(Sender: TObject);
begin
  Close;
end;

procedure TForm1.sbChildWindowClick(Sender: TObject);
begin
  TfrmChild.Create(nil).Show;
end;

procedure TForm1.sbFontClick(Sender: TObject);
var
  i:Integer;
begin
  dlgFont.Font.Assign(Font);
  if dlgFont.Execute then
  begin
    Font.Assign(dlgFont.Font);
    for i:=0 To Screen.FormCount-1 do
      if Screen.Forms[i] is TfrmChild then
        TfrmChild(Screen.Forms[i]).Font.Assign(Font);
  end;
end;

procedure TForm1.sbMsgInfoClick(Sender: TObject);
begin
  MessageDlg(Caption,MSG_TEST1,mtInformation,mbYesAllNoAllCancel);
end;

procedure TForm1.sbMsgWarnClick(Sender: TObject);
begin
  MessageDlg(Caption,MSG_TEST2,mtWarning,mbYesNoCancel);
end;

procedure TForm1.sbPropertyClick(Sender: TObject);
begin
  MessageDlg(Caption,LanguageManager.LangText('TForm1.edField2.Text'),mtInformation,[mbOK]);
end;

procedure TForm1.sbResStrClick(Sender: TObject);
begin
  MessageDlg(Caption,SSelectDirCap,mtInformation,[mbOK]);
end;

procedure TForm1.sbMessage1Click(Sender: TObject);
begin
  try
    lvTest.Items[maxint].Caption:='test'; // this will raise an AV exception
  except
    on E:Exception do
      MessageDlg(Caption,LanguageManager.LangMess(e.message),mtError);  // see Langs\errors.*.lng files
  end;
end;

procedure TForm1.sbMessage2Click(Sender: TObject);
begin
  try
    raise Exception.Create('Test exception'); // this will raise testing exception
  except
    on E:Exception do
      MessageDlg(Caption,LanguageManager.LangMess(e.message),mtError);  // see Langs\errors.*.lng files
  end;
end;

end.
