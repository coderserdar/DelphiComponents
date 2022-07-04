{
  This demo is based on TLanguage demo application

  TLanguage is a translator component written by Serge Sushko

  TLanguage component and included .lng files is copyright (c) by
    Serge Sushko, 1998
    E-mail: sushko@iname.com,
    WWW:    http://members.tripod.com/~sushko/
}

unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, StdCtrls, vgNLS;

type
  TMainForm = class(TForm)
    FruitsLabel: TLabel;
    FruitList: TListBox;
    MainMenu: TMainMenu;
    FileMenuItem: TMenuItem;
    ExitMenuItem: TMenuItem;
    LanguageCombo: TComboBox;
    AboutMenuItem: TMenuItem;
    HelpMenuItem: TMenuItem;
    N1: TMenuItem;
    LanguageLabel: TLabel;
    MessageBtn: TButton;
    tl: TvgTranslator;
    procedure ExitMenuItemClick(Sender: TObject);
    procedure MessageBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LanguageComboChange(Sender: TObject);
  private
    { Private declarations }
    function  GetLangFileName(sLangName : String) : String;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.DFM}
procedure TMainForm.FormCreate(Sender: TObject);
begin
  LanguageCombo.ItemIndex := 0;
  LanguageComboChange(Sender);
end;

procedure TMainForm.ExitMenuItemClick(Sender: TObject);
begin
  Close;
end;

function  TMainForm.GetLangFileName(sLangName : String) : String;
var
  sDir : String;
begin
  sDir := ExtractFilePath(Application.ExeName);
  if (sDir[Length(sDir)] <> '\') then sDir := sDir + '\';
  Result := sDir + sLangName + '.lng';
end;

procedure TMainForm.LanguageComboChange(Sender: TObject);
begin
  tl.LanguageFile := GetLangFileName(LanguageCombo.Text);
  tl.Translate;
end;

procedure TMainForm.MessageBtnClick(Sender: TObject);
var
  sMessage : String;
begin
  if (FruitList.ItemIndex > -1) then
    sMessage := tl.TMsg('Selected fruit is') + ' ' +
                FruitList.Items[FruitList.ItemIndex]
  else
    sMessage := tl.TMsg('No one fruit is selected');

  MessageDlg(sMessage, mtInformation, [mbOK, mbHelp], 0);
end;

end.
