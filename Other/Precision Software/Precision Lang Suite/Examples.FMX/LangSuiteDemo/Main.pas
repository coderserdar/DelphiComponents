unit Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Rtti, System.Classes,
  System.Variants, FMX.Forms, FMX.Types, FMX.Menus, FMX.Controls, FMX.Grid,
  FMX.Layouts, FMX.Memo, FMX.Edit, FMX.Ani
  {$IF CompilerVersion >= 25.0 }
  , FMX.StdCtrls
  {$IFEND}
  ;

type
  TForm1 = class(TForm)
    lbLabelName: TLabel;
    lbName: TLabel;
    lbField1: TLabel;
    lbField2: TLabel;
    edField2: TEdit;
    mTest: TMemo;
    grdTest: TGrid;
    colTest0: TColumn;
    colTest1: TColumn;
    colTest2: TColumn;
    sbMsgInfo: TButton;
    sbMsgWarn: TButton;
    sbProperty: TButton;
    sbMessage1: TButton;
    sbMessage2: TButton;
    sbChildWindow: TButton;
    sbResStr: TButton;
    mmMain: TMainMenu;
    miFile: TMenuItem;
    miLanguage: TMenuItem;
    miExit: TMenuItem;
    MenuItem1: TMenuItem;
    pumLanguages: TPopupMenu;
    procedure FormCreate(Sender: TObject);
    procedure LanguageChanged(Sender:TObject);
    procedure pumLanguagesClick(Sender:TObject);
    procedure miExitClick(Sender: TObject);
    procedure sbChildWindowClick(Sender: TObject);
    procedure sbMsgInfoClick(Sender: TObject);
    procedure sbMsgWarnClick(Sender: TObject);
    procedure sbPropertyClick(Sender: TObject);
    procedure sbMessage1Click(Sender: TObject);
    procedure sbMessage2Click(Sender: TObject);
    procedure sbResStrClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure Assign(Source: TPersistent); override;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  Child,
  FMX.plsLangMan,
  FMX.Dialogs,
  FMX.Consts,
  LangConsts;

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
  pumLanguages.Clear;
  miLanguage.Clear;
  try
    LanguageManager.GetAvailableLanguages(AL);
    for i := 0 to AL.Count - 1 do
    begin
      MI:=TMenuItem.Create(self);
      MI2:=TMenuItem.Create(self);
      MI.TagString:=AL.Names[i];
      MI2.TagString:=MI.TagString;
      MI.Text:=AL.ValueFromIndex[i];
      MI2.Text:=MI.Text;
      MI.OnClick:=pumLanguagesClick;
      MI2.OnClick:=MI.OnClick;
      for j := 0 to pumLanguages.ItemsCount - 1 do
        if MI.Text<pumLanguages.Items[j].Text then
        begin
          pumLanguages.InsertObject(j,MI);
          miLanguage.InsertObject(j,MI2);
          Break;
        end;
      if MI.Parent=nil then
      begin
        pumLanguages.AddObject(MI);
        miLanguage.AddObject(MI2);
      end;
    end;
  finally
    AL.Free;
  end;
end;

procedure TForm1.pumLanguagesClick(Sender:TObject);
begin
  LanguageManager.LanguageCode:=TMenuItem(Sender).TagString; // load another language files
end;

procedure TForm1.Assign(Source: TPersistent);
begin
  if not (Source is TLanguageManager) then
    inherited;
end;

procedure TForm1.LanguageChanged(Sender:TObject);
var
  i:Integer;
begin
  LangConsts.LanguageChanged;               // update user defined constants (variables)

  LanguageManager.Clear(True,False);        // clear localization strings table for constants
  if not LanguageManager.LangVCL(Self) then // update vcl properties for this form
    MessageDlg(LanguageManager.LanguageName+' ('+LanguageManager.LanguageCode+'): ['+
        Self.Name+' ('+Self.ClassName+')] '+LanguageManager.LastError, TMsgDlgType.mtWarning, [TMsgDlgBtn.mbOK], 0);
  lbName.Text:=LanguageManager.LanguageName+' ('+LanguageManager.LanguageCode+')'; // display the current language name and code if you want

  Application.Title:=Self.Caption;          // update application title if you want

  for i := 0 to Screen.FormCount - 1 do        // Inform other forms/frames/modules that language changed.
    Screen.Forms[i].Assign(LanguageManager);   // See differences in overriden Assign method of the main form and the child form.
end;

procedure TForm1.miExitClick(Sender: TObject);
begin
  Exit;
end;

procedure TForm1.sbChildWindowClick(Sender: TObject);
begin
  TfrmChild.Create(nil).Show;
end;

procedure TForm1.sbMsgInfoClick(Sender: TObject);
begin
  MessageDlg(MSG_TEST1,TMsgDlgType.mtInformation,mbYesAllNoAllCancel,0);
end;

procedure TForm1.sbMsgWarnClick(Sender: TObject);
begin
  MessageDlg(MSG_TEST2,TMsgDlgType.mtWarning,mbYesNoCancel,0);
end;

procedure TForm1.sbPropertyClick(Sender: TObject);
begin
  MessageDlg(LanguageManager.LangText('TForm1.edField2.Text'),TMsgDlgType.mtInformation,[TMsgDlgBtn.mbOK],0);
end;

procedure TForm1.sbResStrClick(Sender: TObject);
begin
  MessageDlg(FMX.Consts.StrInvalidThePosition,TMsgDlgType.mtInformation,[TMsgDlgBtn.mbOK],0);
end;

procedure TForm1.sbMessage1Click(Sender: TObject);
begin
  try
    grdTest.columns[maxint].Header:='test'; // this will raise an AV exception
  except
    on E:Exception do
      MessageDlg(LanguageManager.LangMess(e.message),TMsgDlgType.mtError,[TMsgDlgBtn.mbOK],0);  // see Langs\errors.*.lng files
  end;
end;

procedure TForm1.sbMessage2Click(Sender: TObject);
begin
  try
    raise Exception.Create('Test exception'); // this will raise testing exception
  except
    on E:Exception do
      MessageDlg(LanguageManager.LangMess(e.message),TMsgDlgType.mtError,[TMsgDlgBtn.mbOK],0);  // see Langs\errors.*.lng files
  end;
end;

end.
