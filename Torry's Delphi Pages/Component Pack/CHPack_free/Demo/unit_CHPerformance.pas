unit unit_CHPerformance;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, StdCtrls, CHEdit, CHAdvancedLabel, CHThreadTimer,
  CHPerformance, CHButton;

type
  TfrmCHPerformance = class(TForm)
    MainMenu1: TMainMenu;
    close1: TMenuItem;
    Info1: TMenuItem;
    CHPerformance1: TCHPerformance;
    CHThreadTimer1: TCHThreadTimer;
    CHAdvancedLabel1: TCHAdvancedLabel;
    CHAdvancedLabel2: TCHAdvancedLabel;
    CHAdvancedLabel3: TCHAdvancedLabel;
    CHAdvancedLabel4: TCHAdvancedLabel;
    CHAdvancedLabel5: TCHAdvancedLabel;
    CHAdvancedLabel6: TCHAdvancedLabel;
    CHEdit1: TCHEdit;
    CHAdvancedLabel7: TCHAdvancedLabel;
    CHAdvancedLabel8: TCHAdvancedLabel;
    CHAdvancedLabel9: TCHAdvancedLabel;
    CHAdvancedLabel10: TCHAdvancedLabel;
    CHAdvancedLabel11: TCHAdvancedLabel;
    CHAdvancedLabel12: TCHAdvancedLabel;
    CHButton1: TCHButton;
    CHAdvancedLabel13: TCHAdvancedLabel;
    CHAdvancedLabel14: TCHAdvancedLabel;
    Label1: TLabel;
    procedure close1Click(Sender: TObject);
    procedure Info1Click(Sender: TObject);
    procedure CHThreadTimer1Timer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CHButton1Click(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  frmCHPerformance: TfrmCHPerformance;

implementation

uses unit_About;

{$R *.dfm}

procedure TfrmCHPerformance.FormCreate(Sender: TObject);
begin
  CHThreadTimer1.Enabled := True;
end;

procedure TfrmCHPerformance.close1Click(Sender: TObject);
begin
  CHThreadTimer1.Enabled := False;
  Close;
end;

procedure TfrmCHPerformance.Info1Click(Sender: TObject);
begin
  frmAbout := TfrmAbout.Create(Self);
  frmAbout.ShowModal;
end;

procedure TfrmCHPerformance.CHThreadTimer1Timer(Sender: TObject);
begin
  CHAdvancedLabel2.Caption := CHPerformance1.GetCPUUsage + ' %';
end;

{ Performance Counter }
procedure TfrmCHPerformance.CHButton1Click(Sender: TObject);
var
  nCount, I : Integer;
begin
  Screen.Cursor := crHandPoint;
  I := 0;
  CHPerformance1.SetPerformanceCounter;
  for nCount := 0 to StrToInt(CHEdit1.Text) do
  begin
    Inc(I);
  end;
  CHAdvancedLabel14.Caption := FloatToStr(CHPerformance1.GetPerformanceCounter) + ' ms';
  Screen.Cursor := crDefault;
end;

end.
