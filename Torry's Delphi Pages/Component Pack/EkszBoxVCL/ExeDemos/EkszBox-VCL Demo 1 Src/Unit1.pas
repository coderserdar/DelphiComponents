unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  EkDirBrowse, ExtCtrls, EkImgProgressBar, EkGradientProgressBar,
  EkImgBtn, EkImgFadeBase, EkImgCheckBox, StdCtrls, EkImgForm, Dialogs;


type
  TForm1 = class(TForm)
    IProgressBar1: TEkImgProgressBar;
    Timer1: TTimer;
    ProgressBarSpeed: TEkImgProgressBar;
    GProgressBar1: TEkGradientProgressBar;
    GProgressBar3: TEkGradientProgressBar;
    GProgressBar2: TEkGradientProgressBar;
    IProgressBar5: TEkImgProgressBar;
    IProgressBar2: TEkImgProgressBar;
    GroupBox1: TGroupBox;
    EkImgCheckBox1: TEkImgCheckBox;
    GroupBox2: TGroupBox;
    EkImgBtn12: TEkImgBtn;
    EkImgBtn13: TEkImgBtn;
    EkImgBtn14: TEkImgBtn;
    EkImgBtn15: TEkImgBtn;
    EkImgBtn16: TEkImgBtn;
    EkImgBtn17: TEkImgBtn;
    EkImgBtn18: TEkImgBtn;
    EkImgBtn19: TEkImgBtn;
    EkImgBtn20: TEkImgBtn;
    EkImgBtn21: TEkImgBtn;
    EkImgBtn22: TEkImgBtn;
    GroupBox3: TGroupBox;
    EkImgBtn1: TEkImgBtn;
    EkDirBrowse1: TEkDirBrowse;
    EkImgCheckBox2: TEkImgCheckBox;
    IProgressBar3: TEkImgProgressBar;
    IProgressBar4: TEkImgProgressBar;
    Timer2: TTimer;
    EkImgCheckBox3: TEkImgCheckBox;
    EkImgForm1: TEkImgForm;
    BtnClose: TEkImgBtn;
    BtnMinimize: TEkImgBtn;
    procedure Timer1Timer(Sender: TObject);
    procedure EkImgBtn1Click(Sender: TObject);
    procedure EkImgCheckBox1Click(Sender: TObject);
    procedure ProgressBarSpeedChange(Sender: TObject);
    procedure Timer2Timer(Sender: TObject);
    procedure EkImgCheckBox2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure EkImgCheckBox3Click(Sender: TObject);
    procedure BtnMinimizeClick(Sender: TObject);
    procedure BtnCloseClick(Sender: TObject);
    procedure EkImgForm1Activate(Sender: TObject);
    procedure EkImgForm1Deactivate(Sender: TObject);
  private
    mForward : Boolean;
    procedure StopBars();
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}
{$R XpLook.res}

//==============================================================================

procedure TForm1.BtnMinimizeClick(Sender: TObject);
begin
Application.Minimize;
end;

//==============================================================================

procedure TForm1.BtnCloseClick(Sender: TObject);
begin
Close();
end;

//==============================================================================

procedure TForm1.EkImgBtn1Click(Sender: TObject);
begin
  EkDirBrowse1.Title := 'DirBrowse Demo';
  if EkDirBrowse1.Execute then
    ShowMessage('Selected: ' + EkDirBrowse1.Directory)
  else
    ShowMessage('Nothing Selected');
end;


//==============================================================================

procedure TForm1.EkImgCheckBox1Click(Sender: TObject);
begin

ProgressBarSpeed.Animation := not ProgressBarSpeed.Animation;
IProgressBar1.Animation := not IProgressBar1.Animation;
IProgressBar2.Animation := not IProgressBar2.Animation;
IProgressBar3.Animation := not IProgressBar3.Animation;
IProgressBar4.Animation := not IProgressBar4.Animation;

end;

//==============================================================================

procedure TForm1.EkImgCheckBox2Click(Sender: TObject);
begin
  IProgressBar5.ScrollText := not IProgressBar5.ScrollText;
end;

//==============================================================================

procedure TForm1.EkImgCheckBox3Click(Sender: TObject);
begin
  StopBars();
end;

//==============================================================================

procedure TForm1.EkImgForm1Activate(Sender: TObject);
begin
GroupBox1.Invalidate;
GroupBox2.Invalidate;
GroupBox3.Invalidate;
end;

//==============================================================================

procedure TForm1.EkImgForm1Deactivate(Sender: TObject);
begin
GroupBox1.Invalidate;
GroupBox2.Invalidate;
GroupBox3.Invalidate;
end;

//==============================================================================

procedure TForm1.FormCreate(Sender: TObject);
begin
  ProgressBarSpeed.Position := -Timer1.Interval + ProgressBarSpeed.Max +1;
end;

//==============================================================================

procedure TForm1.ProgressBarSpeedChange(Sender: TObject);
begin
  Timer1.Interval := -ProgressBarSpeed.Position + ProgressBarSpeed.Max +1;
  Timer2.Interval := -ProgressBarSpeed.Position + ProgressBarSpeed.Max +1;
end;

//==============================================================================

procedure TForm1.StopBars();
begin
  Timer1.Enabled := not Timer1.Enabled;
  Timer2.Enabled := not Timer2.Enabled;
end;

//==============================================================================

procedure TForm1.Timer1Timer(Sender: TObject);
begin

IProgressBar1.Position := IProgressBar1.Position+1;
IProgressBar2.Position := IProgressBar2.Position+1;
GProgressBar1.Position := GProgressBar1.Position-1;
GProgressBar3.Position := GProgressBar3.Position-1;

if IProgressBar1.Position > 99 then
begin
  IProgressBar1.Position := 0;
  IProgressBar2.Position := 0;
  GProgressBar1.Position := 100;
  GProgressBar3.Position := 100;
end;


if mForward then
  IProgressBar4.BlockWidth := IProgressBar4.BlockWidth+1
else
  IProgressBar4.BlockWidth := IProgressBar4.BlockWidth-1;

if IProgressBar4.BlockWidth > 40 then
mForward := not mForward;

if IProgressBar4.BlockWidth < 5 then
mForward := not mForward;

end;

//==============================================================================

procedure TForm1.Timer2Timer(Sender: TObject);
begin

IProgressBar3.Position := IProgressBar3.Position+1;
GProgressBar2.Position := GProgressBar2.Position+3;

if IProgressBar3.Position > 99 then
begin
  IProgressBar3.Position := 0;
  GProgressBar2.Position := 0;
end;

end;

//==============================================================================

end.
