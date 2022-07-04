
{*****************************************}
{                                         }
{             FastReport v2.3             }
{               Fields list               }
{                                         }
{  Copyright (c) 1998-99 by Tzyganenko A. }
{                                         }
{*****************************************}

unit FR_Flds;

interface

{$I FR.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TfrFieldsForm = class(TForm)
    ValCombo: TComboBox;
    ValList: TListBox;
    Label1: TLabel;
    procedure ValComboClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure ValListKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ValListMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
    procedure FillValCombo;
  public
    { Public declarations }
    DBField: String;
  end;

var
  frFieldsForm: TfrFieldsForm;

implementation

{$R *.DFM}

uses FR_Class, FR_Const, FR_Utils, FR_DBRel
{$IFDEF IBO}
  , IB_Components
{$ELSE}
  , DB
{$ENDIF};

var
  LastDB: String;


procedure TfrFieldsForm.ValListKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = vk_Return then ModalResult := mrOk;
end;

procedure TfrFieldsForm.FillValCombo;
var
  s: TStringList;
begin
  s := TStringList.Create;
{$IFDEF IBO}
  frGetComponents(CurReport.Owner, TIB_DataSet, s, nil);
{$ELSE}
  frGetComponents(CurReport.Owner, TDataSet, s, nil);
{$ENDIF}
  s.Sort;
  ValCombo.Items.Assign(s);
  s.Free;
end;

procedure TfrFieldsForm.ValComboClick(Sender: TObject);
var
  DataSet: TfrTDataSet;
begin
  ValList.Items.Clear;
  DataSet := frGetDataSet(ValCombo.Items[ValCombo.ItemIndex]);
  if DataSet <> nil then
  try
    frGetFieldNames(DataSet, ValList.Items);
  except
  end;
end;

procedure TfrFieldsForm.ValListMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ModalResult := mrOk;
end;

procedure TfrFieldsForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = vk_Escape then
    ModalResult := mrCancel;
end;

procedure TfrFieldsForm.FormCreate(Sender: TObject);
begin
  Caption := LoadStr(frRes + 450);
  Label1.Caption := LoadStr(frRes + 451);
end;

procedure TfrFieldsForm.FormActivate(Sender: TObject);
begin
  FillValCombo;
  if ValCombo.Items.IndexOf(LastDB) <> -1 then
    ValCombo.ItemIndex := ValCombo.Items.IndexOf(LastDB) else
    ValCombo.ItemIndex := 0;
  ValComboClick(nil);
end;

procedure TfrFieldsForm.FormDeactivate(Sender: TObject);
begin
  LastDB := ValCombo.Items[ValCombo.ItemIndex];
  if ValList.ItemIndex <> -1 then
    DBField := LastDB + '."' + ValList.Items[ValList.ItemIndex] + '"';
end;

end.
