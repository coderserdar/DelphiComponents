unit unit_Trayicon;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, CHButton, CHAdvancedLabel, ShellApi;

type
  TfrmCHTrayIcon = class(TForm)
    MainMenu1: TMainMenu;
    close1: TMenuItem;
    Info1: TMenuItem;
    CHAdvancedLabel1: TCHAdvancedLabel;
    CHAdvancedLabel2: TCHAdvancedLabel;
    CHButton1: TCHButton;
    procedure close1Click(Sender: TObject);
    procedure Info1Click(Sender: TObject);
    procedure CHButton1Click(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  frmCHTrayIcon: TfrmCHTrayIcon;

implementation

uses unit_About;

{$R *.dfm}

procedure TfrmCHTrayIcon.close1Click(Sender: TObject);
begin
  Close;
end;

procedure TfrmCHTrayIcon.Info1Click(Sender: TObject);
begin
  frmAbout := TfrmAbout.Create(Self);
  frmAbout.ShowModal;
end;

procedure TfrmCHTrayIcon.CHButton1Click(Sender: TObject);
begin
  ShellExecute(0, 'open', PChar('CHTrayiconDemo.exe'), nil, nil, SW_ShowNormal);
  Close;
end;

end.
