unit unit_Movecontainer;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, CHButton, CHCheckBox, StdCtrls, CHLabel, CHMoveContainer,
  ExtCtrls, Menus;

type
  TfrmCHMoveContainer = class(TForm)
    MainMenu1: TMainMenu;
    close1: TMenuItem;
    Info1: TMenuItem;
    Panel1: TPanel;
    CHMoveContainer1: TCHMoveContainer;
    Panel2: TPanel;
    CHMoveContainer2: TCHMoveContainer;
    CHMoveContainer3: TCHMoveContainer;
    CHLabel1: TCHLabel;
    CHLabel2: TCHLabel;
    CHCheckBox1: TCHCheckBox;
    CHButton1: TCHButton;
    CHCheckBox2: TCHCheckBox;
    CHLabel3: TCHLabel;
    CHLabel4: TCHLabel;
    CHLabel5: TCHLabel;
    CHLabel6: TCHLabel;
    CHLabel7: TCHLabel;
    CHLabel8: TCHLabel;
    Image1: TImage;
    CHLabel9: TCHLabel;
    CHLabel10: TCHLabel;
    CHLabel11: TCHLabel;
    CHLabel12: TCHLabel;
    CHLabel13: TCHLabel;
    CHLabel14: TCHLabel;
    CHLabel15: TCHLabel;
    CHLabel16: TCHLabel;
    procedure FormActivate(Sender: TObject);
    procedure close1Click(Sender: TObject);
    procedure Info1Click(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  frmCHMoveContainer: TfrmCHMoveContainer;

implementation

uses unit_About;

{$R *.dfm}

procedure TfrmCHMoveContainer.FormActivate(Sender: TObject);
begin
  CHMoveContainer1.Left := Panel1.Width;
  CHMoveContainer3.Left := -CHMoveContainer3.Width;
  CHMoveContainer2.Top := Panel2.Height;

  CHMoveContainer1.Active := True;
  CHMoveContainer2.Active := True;
  CHMoveContainer3.Active := True;
end;

procedure TfrmCHMoveContainer.close1Click(Sender: TObject);
begin
  Close;
end;

procedure TfrmCHMoveContainer.Info1Click(Sender: TObject);
begin
  frmAbout := TfrmAbout.Create(Self);
  frmAbout.ShowModal;
end;

end.
