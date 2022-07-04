unit unit_Mouse;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, ExtCtrls,

  CHRadioButton, CHMouse, CHButton;

type
  TfrmCHMouse = class(TForm)
    CHButton3: TCHButton;
    CHButton4: TCHButton;
    CHMouse1: TCHMouse;
    CHMouse2: TCHMouse;
    CHButton1: TCHButton;
    CHButton2: TCHButton;
    rbCenter: TCHRadioButton;
    rbLeftTop: TCHRadioButton;
    rbLeftBottom: TCHRadioButton;
    rbRightTop: TCHRadioButton;
    rbRightBottom: TCHRadioButton;
    Panel2: TPanel;
    CHButton5: TCHButton;
    Panel1: TPanel;
    MainMenu1: TMainMenu;
    close1: TMenuItem;
    Info1: TMenuItem;
    CHMouse3: TCHMouse;
    CHMouse4: TCHMouse;
    procedure CHButton3Click(Sender: TObject);
    procedure Panel1Click(Sender: TObject);
    procedure CHButton4Click(Sender: TObject);
    procedure CHButton5Click(Sender: TObject);
    procedure CHButton1Click(Sender: TObject);
    procedure CHButton2Click(Sender: TObject);
    procedure rbCenterClick(Sender: TObject);
    procedure rbLeftTopClick(Sender: TObject);
    procedure rbLeftBottomClick(Sender: TObject);
    procedure rbRightTopClick(Sender: TObject);
    procedure rbRightBottomClick(Sender: TObject);
    procedure Info1Click(Sender: TObject);
    procedure close1Click(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  frmCHMouse: TfrmCHMouse;

implementation

uses _CHTypes, unit_About;

{$R *.dfm}

procedure TfrmCHMouse.CHButton3Click(Sender: TObject);
begin
  CHMouse1.DoMouseArea(True);
  Panel1.Caption := 'Click for free';
end;

procedure TfrmCHMouse.Panel1Click(Sender: TObject);
begin
  CHMouse1.DoMouseArea(False);
  Panel1.Caption := 'Control';
end;

procedure TfrmCHMouse.CHButton4Click(Sender: TObject);
begin
  CHMouse2.DoMouseArea(True);
  CHButton5.Visible := True;
end;

procedure TfrmCHMouse.CHButton5Click(Sender: TObject);
begin
  CHMouse2.DoMouseArea(False);
  CHButton5.Visible := False;
end;

procedure TfrmCHMouse.CHButton1Click(Sender: TObject);
begin
  CHMouse3.DoMousePositioner;
end;

procedure TfrmCHMouse.CHButton2Click(Sender: TObject);
begin
  CHMouse4.DoMousePositioner;
end;

procedure TfrmCHMouse.rbCenterClick(Sender: TObject);
begin
  CHMouse3.Positioner.Position := mpCenter;
  CHMouse4.Positioner.Position := mpCenter;
end;

procedure TfrmCHMouse.rbLeftTopClick(Sender: TObject);
begin
  CHMouse3.Positioner.Position := mpTopLeft;
  CHMouse4.Positioner.Position := mpTopLeft;
end;

procedure TfrmCHMouse.rbLeftBottomClick(Sender: TObject);
begin
  CHMouse3.Positioner.Position := mpBottomLeft;
  CHMouse4.Positioner.Position := mpBottomLeft;
end;

procedure TfrmCHMouse.rbRightTopClick(Sender: TObject);
begin
  CHMouse3.Positioner.Position := mpTopRight;
  CHMouse4.Positioner.Position := mpTopRight;
end;

procedure TfrmCHMouse.rbRightBottomClick(Sender: TObject);
begin
  CHMouse3.Positioner.Position := mpBottomRight;
  CHMouse4.Positioner.Position := mpBottomRight;
end;

procedure TfrmCHMouse.Info1Click(Sender: TObject);
begin
  frmAbout := TfrmAbout.Create(Self);
  frmAbout.ShowModal;
end;

procedure TfrmCHMouse.close1Click(Sender: TObject);
begin
  Close;
end;

end.
