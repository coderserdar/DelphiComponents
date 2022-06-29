
{*****************************************}
{                                         }
{             FastReport v2.3             }
{           Select Band dialog            }
{                                         }
{  Copyright (c) 1998-99 by Tzyganenko A. }
{                                         }
{*****************************************}

unit FR_BTyp;

interface

{$I FR.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, FR_Class, FR_Const;

type
  TfrBandTypesForm = class(TForm)
    Button1: TButton;
    GB1: TGroupBox;
    Button2: TButton;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    procedure bClick(Sender: TObject);
  public
    { Public declarations }
    SelectedTyp: TfrBandType;
  end;

var
  frBandTypesForm: TfrBandTypesForm;

implementation

uses FR_Desgn;

{$R *.DFM}


procedure TfrBandTypesForm.FormCreate(Sender: TObject);
var
  b: TRadioButton;
  bt: TfrBandType;
  First: Boolean;
begin
  First := True;
  for bt := btReportTitle to btCrossFooter do
  begin
    b := TRadioButton.Create(GB1);
    b.Parent := GB1;
    if Integer(bt) > 10 then
    begin
      b.Left := 130;
      b.Top := (Integer(bt) - 11) * 20 + 20;
    end
    else
    begin
      b.Left := 8;
      b.Top := Integer(bt) * 20 + 20;
    end;
    b.Tag := Integer(bt);
    b.Caption := frBandNames[Integer(bt)];
    b.OnClick := bClick;
    b.Enabled := (bt in [btMasterHeader..btSubDetailFooter,
      btGroupHeader, btGroupFooter]) or not frCheckBand(bt);
    if b.Enabled and First then
    begin
      b.Checked := True;
      SelectedTyp := bt;
      First := False;
    end;
  end;
  Caption := LoadStr(frRes + 510);
  GB1.Caption := LoadStr(frRes + 511);
  Button1.Caption := LoadStr(SOk);
  Button2.Caption := LoadStr(SCancel);
end;

procedure TfrBandTypesForm.bClick(Sender: TObject);
begin
  SelectedTyp := TfrBandType((Sender as TComponent).Tag);
end;

end.

