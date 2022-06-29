
{*****************************************}
{                                         }
{             FastReport v2.3             }
{          Insert fields dialog           }
{                                         }
{  Copyright (c) 1998-99 by Tzyganenko A. }
{                                         }
{*****************************************}

unit FR_IFlds;

interface

{$I FR.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, FR_DBRel;

type
  TfrInsertFieldsForm = class(TForm)
    FieldsL: TListBox;
    DatasetCB: TComboBox;
    Label1: TLabel;
    GroupBox1: TGroupBox;
    HorzRB: TRadioButton;
    VertRB: TRadioButton;
    Button1: TButton;
    Button2: TButton;
    GroupBox2: TGroupBox;
    HeaderCB: TCheckBox;
    BandCB: TCheckBox;
    procedure DatasetCBChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    procedure GetFields;
  public
    { Public declarations }
    DataSet: TfrTDataSet;
  end;

var
  frInsertFieldsForm: TfrInsertFieldsForm;

implementation

uses FR_Class, FR_Const, FR_Utils
{$IFDEF IBO}
  , IB_Components
{$ELSE}
  , DB
{$ENDIF};

{$R *.DFM}

procedure TfrInsertFieldsForm.FormShow(Sender: TObject);
begin
  DataSet := nil;
{$IFDEF IBO}
  frGetComponents(CurReport.Owner, TIB_DataSet, DatasetCB.Items, nil);
{$ELSE}
  frGetComponents(CurReport.Owner, TDataSet, DatasetCB.Items, nil);
{$ENDIF}
  if DatasetCB.Items.Count > 0 then
    DatasetCB.ItemIndex := 0;
  GetFields;
end;

procedure TfrInsertFieldsForm.DatasetCBChange(Sender: TObject);
begin
  GetFields;
end;

procedure TfrInsertFieldsForm.GetFields;
begin
  FieldsL.Items.Clear;
  DataSet := frGetDataSet(DatasetCB.Items[DatasetCB.ItemIndex]);
  if DataSet <> nil then
    frGetFieldNames(DataSet, FieldsL.Items);
end;

procedure TfrInsertFieldsForm.FormCreate(Sender: TObject);
begin
  Caption := LoadStr(frRes + 630);
  Label1.Caption := LoadStr(frRes + 631);
  GroupBox1.Caption := LoadStr(frRes + 632);
  HorzRB.Caption := LoadStr(frRes + 633);
  VertRB.Caption := LoadStr(frRes + 634);
  HeaderCB.Caption := LoadStr(frRes + 635);
  BandCB.Caption := LoadStr(frRes + 636);
  Button1.Caption := LoadStr(SOk);
  Button2.Caption := LoadStr(SCancel);
end;

end.

