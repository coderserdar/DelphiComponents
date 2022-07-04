unit unit_CHLed;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, CHLed, StdCtrls;

type
  TfrmCHLed = class(TForm)
    MainMenu1: TMainMenu;
    close1: TMenuItem;
    Info1: TMenuItem;
    CHLed1: TCHLed;
    CHLed2: TCHLed;
    CHLed3: TCHLed;
    CHLed4: TCHLed;
    CHLed5: TCHLed;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    CHLed6: TCHLed;
    Label7: TLabel;
    CHLed7: TCHLed;
    CHLed8: TCHLed;
    CHLed9: TCHLed;
    Label8: TLabel;
    Label9: TLabel;
    CHLed10: TCHLed;
    Label10: TLabel;
    Label11: TLabel;
    CHLed11: TCHLed;
    CHLed12: TCHLed;
    Label12: TLabel;
    procedure close1Click(Sender: TObject);
    procedure Info1Click(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  frmCHLed: TfrmCHLed;

implementation

uses unit_About;

{$R *.DFM}

procedure TfrmCHLed.close1Click(Sender: TObject);
begin
  Close;
end;

procedure TfrmCHLed.Info1Click(Sender: TObject);
begin
  frmAbout := TfrmAbout.Create(Self);
  frmAbout.ShowModal;
end;

end.
