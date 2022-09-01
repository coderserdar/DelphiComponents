unit bvWaitunitF2;

interface

uses
{$ifndef LINUX}
  Windows, Messages, Graphics, Controls, Forms, Dialogs,
  Buttons,ExtCtrls,StdCtrls,
  bvwaitGlobeunit,
  ComCtrls,
{$else}
  QGraphics,
  QControls,
  QForms,
  QStdCtrls,
  QButtons,
  QExtCtrls,
  QDBGrids,
  QMenus,
  Qt,
  QDialogs,
  QComCtrls,
{$endif}

  SysUtils, Classes,
  bvCursorUnit,
  bvlocalization;


{$ifndef LINUX}
type
  TWaitForm3 = class(TForm)
    Panel1: TPanel;
    Panel: TPanel;
    LabText: TLabel;
    ProgressBar: TProgressBar;
    AnimateGlobe: TAnimate;
    Bevel: TBevel;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  protected
    FirstHeight:integer;
    procedure MakePos; virtual;
  public
    { Public declarations }
    procedure ShowProgress; virtual;
    procedure HideProgress; virtual;
//    constructor CreateMSG(AOwner:TComponent;MSG :String); virtual;
    constructor Create(AOwner:TComponent); override;
  end;

  {$else}
type
  TWaitForm3 = class(TForm)
    Panel1: TPanel;
    Panel: TPanel;
    LabText: TLabel;
    ProgressBar: TProgressBar;
    //AnimateGlobe: TAnimate;
    Bevel: TBevel;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  protected
    FirstHeight:integer;
    procedure MakePos; virtual;
  public
    { Public declarations }
    procedure ShowProgress; virtual;
    procedure HideProgress; virtual;
//    constructor CreateMSG(AOwner:TComponent;MSG :String); virtual;
    constructor Create(AOwner:TComponent); override;
  end;
{$endif}

var
  WaitForm3: TWaitForm3;

  AviActive:boolean;

implementation

{$ifndef LINUX}
{$R *.DFM}
{$else}
{$R *.xfm}
{$endif}



constructor TWaitForm3.Create(AOwner:TComponent);
begin
 inherited Create(AOwner);
// RXLabel1.Caption:=MSG;
 FirstHeight:=Height;

// AnimateGlobe.ResName:='WAITGLOBE';
// AnimateGlobe.Transparent:=true;

{$ifndef LINUX}
 AnimateGlobe.ResName:=bvwaitglobeunit.FINDFILERES;
{$endif}

// AnimateGlobe.Transparent:=false;
{$ifndef LINUX}
 if AviActive then  AnimateGlobe.Active:=true;
{$endif}

 HideProgress;
 MakePos;
end;

procedure TWaitForm3.ShowProgress;
begin
//  ClientHeight:=ProgressBar.Top+ProgressBar.Height*2;
  LabTExt.Height := Progressbar.Top - LabText.top -3;
  ProgressBar.Visible:=true;
  //ProgressBar.Position:=0;
  Refresh;
end;

procedure TWaitForm3.HideProgress;
begin
//  Height:=FirstHeight;
  ProgressBar.Visible:=false;
  LabTExt.Height := ProgressBar.Height + Progressbar.Top - LabText.top ;
  if visible then Refresh;
end;

procedure TWaitForm3.MakePos;
begin
;  // Не трогать - здесь переопределяется старая ненужная функция
end;

procedure TWaitForm3.FormDestroy(Sender: TObject);
begin
{$ifndef LINUX}
   if AnimateGlobe.Active then AnimateGlobe.Active:=true;
{$endif}
end;

procedure TWaitForm3.FormCreate(Sender: TObject);
begin
  LabText.caption:=StrOneMoment;
  self.caption:=StrOneMoment;
end;

initialization

  AviActive:=true;

end.
