
{*****************************************}
{                                         }
{             FastReport v2.3             }
{              Search dialog              }
{                                         }
{  Copyright (c) 1998-99 by Tzyganenko A. }
{                                         }
{*****************************************}

unit FR_Srch;

interface

{$I FR.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, FR_Const;

type
  TfrPreviewSearchForm = class(TForm)
    Label1: TLabel;
    Edit1: TEdit;
    Button1: TButton;
    Button2: TButton;
    GroupBox1: TGroupBox;
    CB1: TCheckBox;
    GroupBox2: TGroupBox;
    RB1: TRadioButton;
    RB2: TRadioButton;
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frPreviewSearchForm: TfrPreviewSearchForm;

implementation

{$R *.DFM}

procedure TfrPreviewSearchForm.FormActivate(Sender: TObject);
begin
  Edit1.SetFocus;
  Edit1.SelectAll;
end;

procedure TfrPreviewSearchForm.FormCreate(Sender: TObject);
begin
  Caption := LoadStr(frRes + 000);
  Label1.Caption := LoadStr(frRes + 001);
  GroupBox1.Caption := LoadStr(frRes + 002);
  CB1.Caption := LoadStr(frRes + 003);
  GroupBox2.Caption := LoadStr(frRes + 004);
  RB1.Caption := LoadStr(frRes + 005);
  RB2.Caption := LoadStr(frRes + 006);
  Button1.Caption := LoadStr(SOk);
  Button2.Caption := LoadStr(SCancel);
end;

end.

