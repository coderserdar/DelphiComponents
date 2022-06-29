
{*****************************************}
{                                         }
{             FastReport v2.3             }
{            Variables editor             }
{                                         }
{  Copyright (c) 1998-99 by Tzyganenko A. }
{                                         }
{*****************************************}

unit FR_Vared;

interface

{$I FR.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, ComCtrls, FR_Class, FR_Const;

type
  TfrVaredForm = class(TForm)
    Button4: TButton;
    Button5: TButton;
    Memo1: TMemo;
    Label1: TLabel;
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Doc: TfrReport;
  end;

var
  frVaredForm: TfrVaredForm;

implementation

{$R *.DFM}

procedure TfrVaredForm.FormActivate(Sender: TObject);
begin
  Memo1.Lines.Assign(Doc.Variables);
end;

procedure TfrVaredForm.FormCreate(Sender: TObject);
begin
  Caption := LoadStr(frRes + 360);
  Label1.Caption := LoadStr(frRes + 361);
  Button4.Caption := LoadStr(SOk);
  Button5.Caption := LoadStr(SCancel);
end;

end.

