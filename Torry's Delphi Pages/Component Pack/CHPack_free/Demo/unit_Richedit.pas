unit unit_Richedit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, ComCtrls, StdCtrls,
  CHRichedit;

type
  TfrmCHRichedit = class(TForm)
    MainMenu1: TMainMenu;
    close1: TMenuItem;
    Info1: TMenuItem;
    CHRichedit1: TCHRichedit;
    CHRichedit2: TCHRichedit;
    CHRichedit3: TCHRichedit;
    procedure close1Click(Sender: TObject);
    procedure Info1Click(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  frmCHRichedit: TfrmCHRichedit;

implementation

uses unit_About, CHUnit;

{$R *.dfm}

procedure TfrmCHRichedit.close1Click(Sender: TObject);
begin
  Close;
end;

procedure TfrmCHRichedit.Info1Click(Sender: TObject);
begin
  frmAbout := TfrmAbout.Create(Self);
  frmAbout.ShowModal;
end;

end.
