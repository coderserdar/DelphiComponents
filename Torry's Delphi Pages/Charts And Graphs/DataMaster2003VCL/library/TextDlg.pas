///////////////////////////////////////
//        Data Master 2003           //
//   Copyright (c) 1993-2003 RRR     //
///////////////////////////////////////

unit TextDlg;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ComCtrls;

type
  TTextForm = class(TForm)
    Memo: TMemo;
    OkBitBtn: TBitBtn;
    CancelBitBtn: TBitBtn;
  private
    { Private declarations }
    FHWND: THandle;
  public
    { Public declarations }
    procedure CreateParams(var Params: TCreateParams); override;
  end;

function InputText(var S: string; Ed: TWinControl): boolean;

implementation

{$R *.DFM}

var TextForm: TTextForm;

function InputText(var S: string; Ed: TWinControl): boolean;
var
  I: integer;
  P: TPoint;
begin
  TextForm:=TTextForm.Create(Application);
  if Application.Handle=0 // ActiveX?
  then TextForm.FHWND:=Ed.Parent.Handle; // not TextForm.SetParent()!
  try
    TextForm.Memo.Text:=S;
    if Assigned(Ed) then
    begin
      P:=Ed.ClientToScreen(Point(0,0));
      TextForm.Top:=P.Y+Ed.Height;
      TextForm.Left:=P.X;
      I:=Ed.Width-TextForm.{Memo.}Width;
      TextForm.Width:=TextForm.Width+I;
      TextForm.Memo.Width:=TextForm.Memo.Width+I+6;
      TextForm.OkBitBtn.Left:=TextForm.OkBitBtn.Left+I+6;
      TextForm.CancelBitBtn.Left:=TextForm.CancelBitBtn.Left+I+6;
    end;
    Result:=TextForm.ShowModal=mrOk;
    if Result then
    begin
      S:='';
      for I:=0 to TextForm.Memo.Lines.Count-1 do
      S:=S+TextForm.Memo.Lines[I];
    end;
  finally
    TextForm.Free;
  end;
end;

{ TTextForm }

procedure TTextForm.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.Style:=WS_POPUP or WS_THICKFRAME; // hide title
  if FHWND<>0
  then Params.WndParent:=FHWND;
  {Params.ExStyle:=WS_EX_TOOLWINDOW; // remove from taskbar (for ActiveX)
  floating if Application.Handle=0 fixed by using FHWND!!!}
end;

end.
