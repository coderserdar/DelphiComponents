unit MainUnit;

interface

uses
{$ifndef LINUX}
  Windows, Messages, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, Buttons,
{$else}
  QExtCtrls,
  QControls, QStdCtrls, QButtons,
  QForms,QDialogs,
{$endif}
  SysUtils, Classes,
  bvFormSaver,bvCursorUnit;

type
  TMainForm = class(TForm)
    bvFormSaver1: TbvFormSaver;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton5: TSpeedButton;
    Label1: TLabel;
    Bevel1: TBevel;
    Label2: TLabel;
    Label3: TLabel;
    SpeedButton4: TSpeedButton;
    SpeedButton6: TSpeedButton;
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure SpeedButton5Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton4Click(Sender: TObject);
    procedure SpeedButton6Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

uses bvDBGridDemoUnit, ColorEditUnit, FormSaverDemoUnit, StandardGrid,
  Grid2Unit, Grid3Unit;

{$ifndef LINUX}
{$R *.DFM}
{$else}
{$R *.xfm}
{$endif}

procedure TMainForm.SpeedButton1Click(Sender: TObject);
begin
  with TWaitCursor.create do // initialization of wait-cursor (bvCursorUnit)
  try
    TbvDBGriddemoform.Create(application).Show;
  finally
    free
  end;

end;

procedure TMainForm.SpeedButton3Click(Sender: TObject);
begin
  TColorEditForm.create(Application).Show;
end;

procedure TMainForm.SpeedButton5Click(Sender: TObject);
begin
   TFormSaverDemo.create(application).show;
end;

procedure TMainForm.SpeedButton2Click(Sender: TObject);
begin
   TStandardGridDemoForm.create(application).Show;
end;

procedure TMainForm.SpeedButton4Click(Sender: TObject);
begin
  with TWaitCursor.create do // initialization of wait-cursor (bvCursorUnit)
  try
    TbvDBGriddemoform2.Create(application).Show;
  finally
    free
  end;

end;

procedure TMainForm.SpeedButton6Click(Sender: TObject);
begin
  with TWaitCursor.create do // initialization of wait-cursor (bvCursorUnit)
  try
    TbvDBGriddemoform3.Create(application).Show;
  finally
    free
  end;

end;

end.
