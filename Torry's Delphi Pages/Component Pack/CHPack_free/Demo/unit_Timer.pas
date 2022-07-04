unit unit_Timer;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, StdCtrls, CHEdit, CHAdvancedTimer, ExtCtrls,
  CHThreadTimer, CHHighResTimer, CHLabel, CHAdvancedLabel, CHCheckBox,
  CHButton;

type
  TfrmCHTimer = class(TForm)
    MainMenu1: TMainMenu;
    close1: TMenuItem;
    Info1: TMenuItem;
    CHLabel5: TCHLabel;
    CHLabel6: TCHLabel;
    CHLabel7: TCHLabel;
    CHLabel8: TCHLabel;
    lblAdvancedTimer: TCHLabel;
    lblTTimer: TCHLabel;
    lblThreadTimer: TCHLabel;
    lblHighResTimer: TCHLabel;
    CHHighResTimer1: TCHHighResTimer;
    CHThreadTimer1: TCHThreadTimer;
    Timer1: TTimer;
    CHAdvancedTimer1: TCHAdvancedTimer;
    CHLabel9: TCHLabel;
    CHLabel10: TCHLabel;
    CHLabel11: TCHLabel;
    Button1: TButton;
    Button2: TButton;
    CHEdit1: TCHEdit;
    CHEdit2: TCHEdit;
    CHEdit3: TCHEdit;
    CHEdit4: TCHEdit;
    CHEdit5: TCHEdit;
    CHCheckBox1: TCHCheckBox;
    CHAdvancedLabel1: TCHAdvancedLabel;
    CHHighResTimer2: TCHHighResTimer;
    CHButton1: TCHButton;
    procedure close1Click(Sender: TObject);
    procedure Info1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure CHHighResTimer1Timer(Sender: TObject);
    procedure CHThreadTimer1Timer(Sender: TObject);
    procedure CHAdvancedTimer1Timer(Sender: TObject);
    procedure CHHighResTimer2Timer(Sender: TObject);
    procedure CHButton1Click(Sender: TObject);
  private
    a, b, c, d : Cardinal;
    procedure DoStop;
  public
    { Public-Deklarationen }
  end;

var
  frmCHTimer: TfrmCHTimer;

implementation

uses unit_About;

{$R *.dfm}

procedure TfrmCHTimer.close1Click(Sender: TObject);
begin
  Close;
end;

procedure TfrmCHTimer.Info1Click(Sender: TObject);
begin
  frmAbout := TfrmAbout.Create(Self);
  frmAbout.ShowModal;
end;

procedure TfrmCHTimer.Button1Click(Sender: TObject);
begin
  a := 0;
  b := 0;
  c := 0;
  d := 0;

  Screen.Cursor := crHourGlass;

  if CHCheckBox1.Checked then
  begin
    CHHighResTimer2.Interval := StrToInt(CHEdit5.Text);
    CHHighResTimer2.Enabled := True;
  end;

  Timer1.Enabled := True;
  CHThreadTimer1.Enabled := True;
  CHHighResTimer1.Enabled := True;
  CHAdvancedTimer1.Enabled := True;
end;

procedure TfrmCHTimer.Button2Click(Sender: TObject);
begin
  DoStop;
end;

procedure TfrmCHTimer.Timer1Timer(Sender: TObject);
begin
  Inc(a);
end;

procedure TfrmCHTimer.CHHighResTimer1Timer(Sender: TObject);
begin
  Inc(b);
end;

procedure TfrmCHTimer.CHThreadTimer1Timer(Sender: TObject);
begin
  Inc(c);
end;

procedure TfrmCHTimer.CHAdvancedTimer1Timer(Sender: TObject);
begin
  Inc(d);
end;

procedure TfrmCHTimer.CHHighResTimer2Timer(Sender: TObject);
begin
  // self deaktivate
  CHHighResTimer2.Enabled := False;

  DoStop;
end;

procedure TfrmCHTimer.CHButton1Click(Sender: TObject);
begin
  a := 0;
  b := 0;
  c := 0;
  d := 0;
  lblTTimer.Caption := IntToStr(a);
  lblHighResTimer.Caption := IntToStr(b);
  lblThreadTimer.Caption := IntToStr(c);
  lblAdvancedTimer.Caption := IntToStr(d);
end;

procedure TfrmCHTimer.DoStop;
begin
  Timer1.Enabled := False;
  CHThreadTimer1.Enabled := False;
  CHHighResTimer1.Enabled := False;
  CHAdvancedTimer1.Enabled := False;

  lblTTimer.Caption := IntToStr(a);
  lblHighResTimer.Caption := IntToStr(b);
  lblThreadTimer.Caption := IntToStr(c);
  lblAdvancedTimer.Caption := IntToStr(d);

  Screen.Cursor := crDefault;
end;

end.
