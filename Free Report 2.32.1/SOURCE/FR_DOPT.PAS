
{*****************************************}
{                                         }
{             FastReport v2.3             }
{            Document options             }
{                                         }
{  Copyright (c) 1998-99 by Tzyganenko A. }
{                                         }
{*****************************************}

unit FR_Dopt;

interface

{$I FR.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, FR_Const;

type
  TfrDocOptForm = class(TForm)
    GroupBox1: TGroupBox;
    ComB1: TComboBox;
    CB1: TCheckBox;
    GroupBox2: TGroupBox;
    CB2: TCheckBox;
    Button1: TButton;
    Button2: TButton;
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frDocOptForm: TfrDocOptForm;

implementation

{$R *.DFM}

uses FR_Prntr;

procedure TfrDocOptForm.FormActivate(Sender: TObject);
begin
  ComB1.Items.Assign(Prn.Printers);
  ComB1.ItemIndex := Prn.PrinterIndex;
end;

procedure TfrDocOptForm.FormCreate(Sender: TObject);
begin
  Caption := LoadStr(frRes + 370);
  GroupBox1.Caption := LoadStr(frRes + 371);
  CB1.Caption := LoadStr(frRes + 372);
  GroupBox2.Caption := LoadStr(frRes + 373);
  CB2.Caption := LoadStr(frRes + 374);
  Button1.Caption := LoadStr(SOk);
  Button2.Caption := LoadStr(SCancel);
end;

end.

