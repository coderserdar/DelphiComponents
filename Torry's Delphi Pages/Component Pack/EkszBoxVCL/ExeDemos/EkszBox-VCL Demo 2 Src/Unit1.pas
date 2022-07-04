unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  EkImgFade, StdCtrls, EkImgBtn, EkImgFadeBase, EkImgCheckBox,
  EkGradientProgressBar, EkLabel, EkImgForm, EkTypes, jpeg;

type
  TForm1 = class(TForm)
    EkImgCheckBox3: TEkImgCheckBox;
    BtnFade: TEkImgBtn;
    ProgressBarStates: TEkGradientProgressBar;
    EkImgCheckBox1: TEkImgCheckBox;
    ComboBox1: TComboBox;
    ProgressBarUpdateInterval: TEkGradientProgressBar;
    EkLabel1: TEkLabel;
    EkLabel2: TEkLabel;
    EkImgForm1: TEkImgForm;
    BtnClose: TEkImgBtn;
    BtnMinimize: TEkImgBtn;
    EkImgFade1: TEkImgFade;
    procedure EkImgCheckBox3Click(Sender: TObject);
    procedure EkImgCheckBox1Click(Sender: TObject);
    procedure BtnFadeClick(Sender: TObject);
    procedure ProgressBarStatesChange(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure ProgressBarUpdateIntervalChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BtnMinimizeClick(Sender: TObject);
    procedure BtnCloseClick(Sender: TObject);
    procedure EkImgFade1FadeStartStop(Sender: TObject);
  private
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

procedure TForm1.ComboBox1Change(Sender: TObject);
begin
  EkImgFade1.FadeFx := TEkFadeFx(ComboBox1.ItemIndex);
end;

//==============================================================================

procedure TForm1.ProgressBarStatesChange(Sender: TObject);
begin
  EkImgFade1.States := ProgressBarStates.Position+2;
end;

//==============================================================================

procedure TForm1.ProgressBarUpdateIntervalChange(Sender: TObject);
begin
  EkImgFade1.UpdateInterval := ProgressBarUpdateInterval.Position;
end;

//==============================================================================

procedure TForm1.BtnFadeClick(Sender: TObject);
begin
  EkImgFade1.Fade := not EkImgFade1.Fade;
end;

//==============================================================================

procedure TForm1.EkImgCheckBox1Click(Sender: TObject);
begin
  EkImgFade1.AutoReverse := not EkImgFade1.AutoReverse;
end;

//==============================================================================

procedure TForm1.EkImgCheckBox3Click(Sender: TObject);
begin
  EkImgFade1.Loop := not EkImgFade1.Loop;
end;

//==============================================================================

procedure TForm1.EkImgFade1FadeStartStop(Sender: TObject);
begin
if EkImgFade1.Fade then
  BtnFade.Caption := 'Stop'
else
  BtnFade.Caption := 'Start';
end;

//==============================================================================

procedure TForm1.FormCreate(Sender: TObject);
begin
  EkImgForm1.LoadNewImage('Graphics\WMP11-2-3.bmp');
  EkImgFade1.LoadImgFrom('Graphics\1.jpg');
  EkImgFade1.LoadImgTo('Graphics\2.jpg');
  ProgressBarStates.Position := EkImgFade1.States;
  ProgressBarUpdateInterval.Position := EkImgFade1.UpdateInterval;
  EkImgFade1.Fade := True;
  ComboBox1.ItemIndex := 0;
end;

//==============================================================================

end.
