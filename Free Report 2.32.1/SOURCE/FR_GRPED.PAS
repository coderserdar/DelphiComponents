
{*****************************************}
{                                         }
{             FastReport v2.3             }
{            Group band editor            }
{                                         }
{  Copyright (c) 1998-99 by Tzyganenko A. }
{                                         }
{*****************************************}

unit FR_GrpEd;

interface

{$I FR.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, FR_Class, FR_Ctrls, FR_Const, ExtCtrls;

type
  TfrGroupEditorForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    GB1: TGroupBox;
    Edit1: TEdit;
    Panel1: TPanel;
    frSpeedButton1: TfrSpeedButton;
    procedure frSpeedButton1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure ShowEditor(t: TfrView);
  end;

var
  frGroupEditorForm: TfrGroupEditorForm;

implementation

{$R *.DFM}

uses FR_Flds;

procedure TfrGroupEditorForm.ShowEditor(t: TfrView);
begin
  Edit1.Text := (t as TfrBandView).GroupCondition;
  if ShowModal = mrOk then
  begin
    frDesigner.BeforeChange;
    (t as TfrBandView).GroupCondition := Edit1.Text;
  end;
end;

procedure TfrGroupEditorForm.frSpeedButton1Click(Sender: TObject);
begin
  frFieldsForm := TfrFieldsForm.Create(nil);
  with frFieldsForm do
  if ShowModal = mrOk then
    Edit1.Text := DBField;
  frFieldsForm.Free;
end;

procedure TfrGroupEditorForm.FormCreate(Sender: TObject);
begin
  Caption := LoadStr(frRes + 490);
  GB1.Caption := LoadStr(frRes + 491);
  frSpeedButton1.Hint := LoadStr(frRes + 492);
  Button1.Caption := LoadStr(SOk);
  Button2.Caption := LoadStr(SCancel);
end;

end.

