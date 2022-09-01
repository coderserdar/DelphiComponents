unit bvMessageUnit;

interface

uses
{$ifndef LINUX}
  Windows, Messages, SysUtils, Graphics, Controls, Forms, Dialogs,
  DBGrids,Grids, StdCtrls, Buttons,ExtCtrls,  Menus, ComCtrls,
{$else}
  QForms,
  QStdCtrls,
  QButtons,
  QExtCtrls,
  QControls, QGraphics,
{$endif}
  Classes,
  bvLocalization;

type
  TMessageForm = class(TForm)
    PanelBottom: TPanel;
    BitBtnOk: tBitBtn;
    Bevel1: TBevel;
    Bevel2: TBevel;
    PanelLeft: TPanel;
    StopImage: TImage;
    StopImage1: TImage;
    InfoImage: TImage;
    WarningImage: TImage;
    ScrollBox1: TScrollBox;
    Labmess: TLabel;
    procedure FormResize(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure CheckSizes;
  end;

procedure bvMessageError(Mess:string);
procedure bvMessage(Mess:string;CapString:string='!!!');
procedure bvMessageWarning(Mess:string;CapString:string='!!!');


var
  MessageForm: TMessageForm;

implementation

{$ifndef LINUX}
{$R *.DFM}
{$else}
{$R *.xfm}
{$endif}

procedure TMessageForm.CheckSizes;
begin
;;
end;


procedure bvMessage(Mess:string;CapString:string='!!!');
//var thheight:integer;
begin
  with TMessageForm.create(Application) do
  try
    Caption:=CapString;
    LabMess.CAption:=Mess;

{    B:=labmess.ClientRect;
    DrawText(LabMess.canvas.Handle, PChar(Mess), Length(Mess), B,
        DT_WORDBREAK+DT_CENTER+ DT_CALCRECT);

    if b.Bottom>labmess.clientrect.bottom
    then labmess.height:=labmess.height+B.Bottom-labmess.clientrect.bottom;
    //thheight:=Labmess.Canvas.
    }
    StopImage.Visible:=false;
    StopImage1.Visible:=false;
    InfoImage.Visible:=true;
    warningImage.Visible:=false;
    CheckSizes;
    ShowModal;
  finally
    free;
  end;
end;

procedure bvMessageWarning(Mess:string;CapString:string='!!!');
begin
  with TMessageForm.create(Application) do
  try
    Caption:=CapString;
    LabMess.CAption:=Mess;
    StopImage.Visible:=false;
    StopImage1.Visible:=false;
    InfoImage.Visible:=false;
    warningImage.Visible:=true;
    CheckSizes;
    ShowModal;
  finally
    free;
  end;
end;

procedure bvMessageError(Mess:string);
begin
  with TMessageForm.create(Application) do
  try
    Caption:=StrError;
    LabMess.CAption:=Mess;
    StopImage.Visible:=true;
    StopImage1.Visible:=false;
    InfoImage.Visible:=false;
    warningImage.Visible:=false;
    CheckSizes;
    ShowModal;
  finally
    free;
  end;
end;

procedure TMessageForm.FormResize(Sender: TObject);
begin
  BitBtnOk.Left:=(Self.Width -BitBtnOk.width) div 2;
  //BitBtnCancel.Left:=BitBtnOk.Left;
  Self.Left:=(Screen.Width-Self.Width) div 2;
  Self.top:=(Screen.Height-Self.Height) div 2;
end;

end.
