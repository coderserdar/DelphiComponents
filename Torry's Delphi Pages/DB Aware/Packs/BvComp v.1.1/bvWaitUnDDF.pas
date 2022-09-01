unit bvWaitUnDDF;

interface

uses
{$ifndef LINUX}
  Windows, Messages, Graphics, Controls, Forms, Dialogs,
  Buttons,ExtCtrls,StdCtrls,
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
  bvLocalization;


type
  TWaitDDForm = class(TForm)
    Panel1: TPanel;
    Panel: TPanel;
    Lab: TLabel;
    ProgressBar: TProgressBar;
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

var
  WaitDDForm: TWaitDDForm;

  AviActive:boolean;

implementation

{$ifndef LINUX}
{$R *.DFM}
{$else}
{$R *.xfm}
{$endif}


constructor TWaitDDForm.Create(AOwner:TComponent);
begin
 inherited Create(AOwner);
// RXLabel1.Caption:=MSG;
 FirstHeight:=Height;

// AnimateGlobe.ResName:='WAITGLOBE';
// AnimateGlobe.Transparent:=true;

// AnimateGlobe.ResName:='WAITGLOBE1';
// AnimateGlobe.Transparent:=false;

// if AviActive then  AnimateGlobe.Active:=true;

 MakePos;
 Visible:=true;
 Update; 
end;

procedure TWaitDDForm.ShowProgress;
begin
//  ClientHeight:=ProgressBar.Top+ProgressBar.Height*2;
  ProgressBar.Visible:=true;
  Refresh;
end;

procedure TWaitDDForm.HideProgress;
begin
//  Height:=FirstHeight;
  ProgressBar.Visible:=false;
  Refresh;
end;

procedure TWaitDDForm.MakePos;
begin
;  // Не трогать - здесь переопределяется старая ненужная функция
end;

procedure TWaitDDForm.FormDestroy(Sender: TObject);
begin
   //if AnimateGlobe.Active then AnimateGlobe.Active:=true;
end;

procedure TWaitDDForm.FormCreate(Sender: TObject);
begin
 Lab.caption:=StrOneMoment;
 self.caption:=StrOneMoment;
end;

initialization

  AviActive:=true;

end.
