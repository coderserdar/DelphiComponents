
{*****************************************}
{                                         }
{             FastReport v2.3             }
{             Variables form              }
{                                         }
{  Copyright (c) 1998-99 by Tzyganenko A. }
{                                         }
{*****************************************}

unit FR_Var;

interface

{$I FR.inc}

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, FR_Const;

type

  TfrVarForm = class(TForm)
    ValList: TListBox;
    ValCombo: TComboBox;
    Label1: TLabel;
    procedure ValListKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ValComboClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ValListMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormDeactivate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
    function CurVal: String;
    function CurDataSet: String;
    procedure GetVariables;
    procedure GetSpecValues;
    procedure GetFRVariables;
    procedure FillValCombo;
  public
    { Public declarations }
    SelectedItem: String;
  end;

var
  frVarForm: TfrVarForm;

implementation

{$R *.DFM}

uses FR_Class;

var
  LastCategory: String;

function TfrVarForm.CurVal: String;
begin
  Result := '';
  if ValList.ItemIndex <> -1 then
    Result := ValList.Items[ValList.ItemIndex];
end;

function TfrVarForm.CurDataSet: String;
begin
  Result := '';
  if ValCombo.ItemIndex <> -1 then
    Result := ValCombo.Items[ValCombo.ItemIndex];
end;

procedure TfrVarForm.FillValCombo;
var
  s: TStringList;
begin
  s := TStringList.Create;
  CurReport.GetCategoryList(s);
  s.Add(LoadStr(SSpecVal));
  s.Add(LoadStr(SFRVariables));
  ValCombo.Items.Assign(s);
  s.Free;
end;

procedure TfrVarForm.ValComboClick(Sender: TObject);
begin
  if CurDataSet = LoadStr(SFRVariables) then
    GetFRVariables
  else if CurDataSet = LoadStr(SSpecVal) then
    GetSpecValues else
    GetVariables;
end;

procedure TfrVarForm.GetVariables;
begin
  CurReport.GetVarList(ValCombo.ItemIndex, ValList.Items);
end;

procedure TfrVarForm.GetSpecValues;
var
  i: Integer;
begin
  with ValList.Items do
  begin
    Clear;
    for i := 0 to frSpecCount-1 do
      if i <> 1 then
        Add(frSpecArr[i]);
  end;
end;

procedure TfrVarForm.GetFRVariables;
var
  i: Integer;
begin
  with ValList.Items do
  begin
    Clear;
    for i := 0 to frVariables.Count - 1 do
      Add(frVariables.Name[i]);
  end;
end;

procedure TfrVarForm.ValListKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = vk_Return then ModalResult := mrOk;
end;

procedure TfrVarForm.ValListMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ModalResult := mrOk;
end;

procedure TfrVarForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = vk_Escape then
    ModalResult := mrCancel;
end;

procedure TfrVarForm.FormCreate(Sender: TObject);
begin
  Caption := LoadStr(frRes + 440);
  Label1.Caption := LoadStr(frRes + 441);
end;

procedure TfrVarForm.FormActivate(Sender: TObject);
begin
  FillValCombo;
  if ValCombo.Items.IndexOf(LastCategory) <> -1 then
    ValCombo.ItemIndex := ValCombo.Items.IndexOf(LastCategory) else
    ValCombo.ItemIndex := 0;
  ValComboClick(nil);
end;

procedure TfrVarForm.FormDeactivate(Sender: TObject);
begin
  if ModalResult = mrOk then
    if CurDataSet <> LoadStr(SSpecVal) then
      SelectedItem := CurVal
    else
      if ValList.ItemIndex > 0 then
        SelectedItem := frSpecFuncs[ValList.ItemIndex + 1] else
        SelectedItem := frSpecFuncs[0];
  LastCategory := ValCombo.Items[ValCombo.ItemIndex];
end;

end.

