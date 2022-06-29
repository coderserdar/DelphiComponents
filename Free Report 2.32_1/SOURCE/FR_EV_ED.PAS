
{*****************************************}
{                                         }
{             FastReport v2.3             }
{        'Values' property editor         }
{                                         }
{  Copyright (c) 1998-99 by Tzyganenko A. }
{                                         }
{*****************************************}

unit FR_Ev_ed;

interface

{$I FR.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, FR_Class, ExtCtrls, Buttons, FR_Ctrls;

type
  TfrEvForm = class(TForm)
    VarCombo: TComboBox;
    VarList: TListBox;
    ValCombo: TComboBox;
    ValList: TListBox;
    Label1: TLabel;
    Label2: TLabel;
    Button1: TButton;
    Edit1: TEdit;
    Label3: TLabel;
    Button2: TButton;
    Button3: TButton;
    SB1: TfrSpeedButton;
    SB2: TfrSpeedButton;
    Bevel1: TBevel;
    procedure VarComboClick(Sender: TObject);
    procedure ValComboClick(Sender: TObject);
    procedure VarListClick(Sender: TObject);
    procedure ValListClick(Sender: TObject);
    procedure Edit1Exit(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure SB1Click(Sender: TObject);
    procedure SB2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    function CurVar: String;
    function CurVal: String;
    function CurDataSet: String;
    procedure GetFields(Value: String);
    procedure GetSpecValues;
    procedure GetFRVariables;
    procedure FillVarCombo;
    procedure FillValCombo;
    procedure ShowVarValue(Value: String);
    procedure SetValTo(Value: String);
    procedure CheckForExpr;
    procedure PostVal;
  public
    { Public declarations }
    Doc: TfrReport;
    Str: TMemoryStream;
    Sl: TStringList;
    procedure Init;
    procedure RefreshVarList(Memo: TStrings);
    procedure CancelChanges;
  end;


function ShowEvEditor(Component: TfrReport): Boolean;

implementation

{$R *.DFM}

uses FR_Vared, FR_Const, FR_Utils, FR_DBRel
{$IFDEF IBO}
  , IB_Components
{$ELSE}
  , DB
{$ENDIF};

var
  SMemo: TStringList;
  VarClipbd: TMemoryStream;

function ShowEvEditor(Component: TfrReport): Boolean;
begin
  Result := False;
  with TfrEvForm.Create(nil) do
  try
    Doc := Component;
    Str := TMemoryStream.Create;
    Sl := TStringList.Create;
    Doc.Values.WriteBinaryData(Str);
    Doc.Values.Items.Sorted := False;
    Sl.Assign(Doc.Variables);
    Init;
    SB2.Enabled := VarClipbd.Size <> 0;
    if ShowModal = mrOk then
      Result := True else
      CancelChanges;
  finally
    Str.Free;
    Sl.Free;
    Free;
  end
end;

procedure TfrEvForm.Button3Click(Sender: TObject);
begin
  with TfrVaredForm.Create(nil) do
  try
    Doc := Self.Doc;
    if ShowModal = mrOk then
      RefreshVarList(Memo1.Lines);
  finally
    Free;
  end
end;

procedure TfrEvForm.Init;
begin
  FillVarCombo;
  FillValCombo;
  VarCombo.ItemIndex := 0;
  ValCombo.ItemIndex := 0;
  VarComboClick(nil);
  ValComboClick(nil);
  CheckForExpr;
end;

procedure TfrEvForm.RefreshVarList(Memo: TStrings);
var
  i, j, n: Integer;
  l: TStringList;
begin
  l := TStringList.Create;
  Doc.Variables.Assign(Memo);
  with Doc.Values do
  for i := Items.Count-1 downto 0 do
    if Doc.FindVariable(Items[i]) = -1 then
    begin
      Objects[i].Free;
      Items.Delete(i);
    end;
  Doc.GetCategoryList(l);
  n := l.Count;
  for i := 0 to n-1 do
  begin
    Doc.GetVarList(i, l);
    for j := 0 to l.Count-1 do
      with Doc.Values do
      if FindVariable(l[j]) = nil then
        Items[AddValue] := l[j];
  end;
  FillVarCombo;
  VarCombo.ItemIndex := 0;
  VarComboClick(nil);
  l.Free;
end;

procedure TfrEvForm.CancelChanges;
begin
  Str.Position := 0;
  Doc.Values.ReadBinaryData(Str);
  Doc.Variables.Assign(Sl);
end;

function TfrEvForm.CurVar: String;
begin
  Result := '';
  if VarList.ItemIndex <> -1 then
    Result := VarList.Items[VarList.ItemIndex];
end;

function TfrEvForm.CurVal: String;
begin
  Result := '';
  if ValList.ItemIndex <> -1 then
    Result := ValList.Items[ValList.ItemIndex];
end;

function TfrEvForm.CurDataSet: String;
begin
  Result := '';
  if ValCombo.ItemIndex <> -1 then
    Result := ValCombo.Items[ValCombo.ItemIndex];
end;

procedure TfrEvForm.FillVarCombo;
begin
  Doc.GetCategoryList(VarCombo.Items);
end;

procedure TfrEvForm.FillValCombo;
var
  s: TStringList;
begin
  s := TStringList.Create;
{$IFDEF IBO}
  frGetComponents(Doc.Owner, TIB_DataSet, s, nil);
{$ELSE}
  frGetComponents(Doc.Owner, TDataSet, s, nil);
{$ENDIF}
  s.Sort;
  s.Add(LoadStr(SSpecVal));
  s.Add(LoadStr(SFRVariables));
  ValCombo.Items.Assign(s);
  s.Free;
end;

procedure TfrEvForm.VarComboClick(Sender: TObject);
begin
  Doc.GetVarList(VarCombo.ItemIndex, VarList.Items);
end;

procedure TfrEvForm.ValComboClick(Sender: TObject);
begin
  if CurDataSet = LoadStr(SFRVariables) then
    GetFRVariables
  else if CurDataSet <> LoadStr(SSpecVal) then
    GetFields(CurDataSet) else
    GetSpecValues;
end;

procedure TfrEvForm.VarListClick(Sender: TObject);
begin
  ShowVarValue(CurVar);
end;

procedure TfrEvForm.GetFields(Value: String);
var
  DataSet: TfrTDataSet;
begin
  ValList.Items.Clear;
  CurReport := Doc;
  DataSet := frGetDataSet(Value);
  if DataSet <> nil then
  try
    frGetFieldNames(DataSet, ValList.Items);
  except
  end;
  ValList.Items.Insert(0, LoadStr(SNotAssigned));
end;

procedure TfrEvForm.GetSpecValues;
var
  i: Integer;
begin
  with ValList.Items do
  begin
    Clear;
    Add(LoadStr(SNotAssigned));
    for i := 0 to frSpecCount - 1 do
      Add(frSpecArr[i]);
  end;
end;

procedure TfrEvForm.GetFRVariables;
var
  i: Integer;
begin
  with ValList.Items do
  begin
    Clear;
    Add(LoadStr(SNotAssigned));
    for i := 0 to frVariables.Count - 1 do
      Add(frVariables.Name[i]);
  end;
end;

procedure TfrEvForm.ShowVarValue(Value: String);
begin
  with Doc.Values.FindVariable(Value) do
    case Typ of
      vtNotAssigned:
        SetValTo(CurDataSet + '.' + LoadStr(SNotAssigned));
      vtDBField:
        SetValTo(DataSet + '.' + Field);
      vtFRVar:
        SetValTo(LoadStr(SFRVariables) + '.' + Field);
      vtOther:
        begin
          SetValTo(LoadStr(SSpecVal) + '.' + frSpecArr[OtherKind]);
          if OtherKind = 1 then
            Edit1.Text := Field;
        end;
    end;
end;

procedure TfrEvForm.SetValTo(Value: String);
var
  s1, s2, s3: String;
  i, j: Integer;
begin
  s1 := Copy(Value, 1, Pos('.', Value) - 1);
  s2 := Copy(Value, Pos('.', Value) + 1, 255);
  if Pos('.', s2) <> 0 then
  begin
    s3 := Copy(s2, Pos('.', s2) + 1, 255);
    s2 := Copy(s2, 1, Pos('.', s2) - 1);
    if AnsiCompareText(s1, Doc.Owner.Name) = 0 then
      s1 := s2 else
      s1 := s1 + '.' + s2;
    s2 := s3;
  end;
  with ValCombo do
  for i := 0 to Items.Count-1 do
    if Items[i] = s1 then
    begin
      if ItemIndex <> i then
      begin
        ItemIndex := i;
        ValComboClick(nil);
      end;
      with ValList do
      for j := 0 to Items.Count-1 do
        if Items[j] = s2 then
        begin
          ItemIndex := j;
          break;
        end;
      break;
    end;
  CheckForExpr;
end;

procedure TfrEvForm.ValListClick(Sender: TObject);
begin
  if VarList.ItemIndex < 0 then Exit;
  CheckForExpr;
end;

procedure TfrEvForm.CheckForExpr;
begin
  Edit1.Enabled := (CurDataSet = LoadStr(SSpecVal)) and
    (CurVal = frSpecArr[1]);
  Label3.Enabled := Edit1.Enabled;
  if not Edit1.Enabled then
  begin
    Edit1.Text := '';
    Edit1.Color := clBtnFace;
  end
  else
    Edit1.Color := clWindow;
end;

procedure TfrEvForm.Edit1Exit(Sender: TObject);
begin
  PostVal;
end;

procedure TfrEvForm.PostVal;
var
  Val: TfrValue;
  i: Integer;
  s: String;
begin
  Val := Doc.Values.FindVariable(CurVar);
  if Val <> nil then
  with Val do
  begin
    if CurVal = LoadStr(SNotAssigned) then
      Typ := vtNotAssigned
    else if CurDataSet = LoadStr(SSpecVal) then
    begin
      Typ := vtOther;
      s := CurVal;
      for i := 0 to frSpecCount - 1 do
        if s = frSpecArr[i] then
        begin
          OtherKind := i;
          if i = 1 then // SExpr
            Field := Edit1.Text;
          break;
        end;
    end
    else if CurDataSet = LoadStr(SFRVariables) then
    begin
      Typ := vtFRVar;
      Field := CurVal;
    end
    else
    begin
      Typ := vtDBField;
      DataSet := CurDataSet;
      Field := CurVal;
      OtherKind := 0;
    end;
  end;
end;

procedure TfrEvForm.SB1Click(Sender: TObject);
begin
  VarClipbd.Position := 0;
  Doc.Values.WriteBinaryData(VarClipbd);
  SMemo.Assign(Doc.Variables);
  frWriteMemo(VarClipbd, SMemo);
  SB2.Enabled := True;
end;

procedure TfrEvForm.SB2Click(Sender: TObject);
begin
  VarClipbd.Position := 0;
  Doc.Values.ReadBinaryData(VarClipbd);
  frReadMemo(VarClipbd, SMemo);
  Doc.Variables.Assign(SMemo);
  Init;
end;

procedure TfrEvForm.Button1Click(Sender: TObject);
begin
  PostVal;
end;

procedure TfrEvForm.FormCreate(Sender: TObject);
begin
  Caption := LoadStr(frRes + 340);
  Label1.Caption := LoadStr(frRes + 341);
  Label2.Caption := LoadStr(frRes + 342);
  Label3.Caption := LoadStr(frRes + 343);
  SB1.Hint := LoadStr(frRes + 344);
  SB2.Hint := LoadStr(frRes + 345);
  Button3.Caption := LoadStr(frRes + 346);
  Button1.Caption := LoadStr(SOk);
  Button2.Caption := LoadStr(SCancel);
end;

initialization
  SMemo := TStringList.Create;
  VarClipbd := TMemoryStream.Create;

finalization
  SMemo.Free;
  VarClipbd.Free;

end.

