
{*****************************************}
{                                         }
{             FastReport v2.3             }
{   Memo editor with syntax highlight     }
{                                         }
{  Copyright (c) 1998-99 by Tzyganenko A. }
{                                         }
{*****************************************}

unit FR_SntxE;

interface

{$I FR.inc}

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  Forms, StdCtrls, Buttons, ClipBrd, FR_Class, FR_Insp, ExtCtrls, SyntaxEd,
  SynParse;

type
  TfrEditorForm = class(TPropEditor)
    Panel2: TPanel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    ScriptPanel: TPanel;
    Label2: TLabel;
    M2: TSyntaxMemo;
    MemoPanel: TPanel;
    M1: TMemo;
    Label1: TLabel;
    Panel5: TPanel;
    CB1: TCheckBox;
    CB2: TCheckBox;
    CB3: TCheckBox;
    Splitter: TPanel;
    SyntaxMemoParser1: TSyntaxMemoParser;
    procedure Button3Click(Sender: TObject);
    procedure M1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Button4Click(Sender: TObject);
    procedure M1Enter(Sender: TObject);
    procedure CB1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure CB2Click(Sender: TObject);
    procedure CB3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure SplitterMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SplitterMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure SplitterMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { Private declarations }
    FActiveMemo: TObject;
    FDown: Boolean;
    FLastY: Integer;
    procedure WMGetMinMaxInfo(var Msg: TWMGetMinMaxInfo); message WM_GETMINMAXINFO;
    procedure AdjustMemoSizes;
  public
    { Public declarations }
    function ShowEditor: TModalResult; override;
  end;


implementation

{$R *.DFM}

uses FR_Desgn, FR_Fmted, FR_Var, FR_Flds, FR_Const;

function TfrEditorForm.ShowEditor: TModalResult;
begin
  Result := mrCancel;
  if View <> nil then
    Result := inherited ShowEditor;
end;

procedure TfrEditorForm.FormShow(Sender: TObject);
begin
  CB1Click(nil);
  CB2Click(nil);
  CB3Click(nil);
  M1.Lines.Assign(View.Memo);
  M1.SetFocus;
  FActiveMemo := M1;
  M2.Lines.Assign(View.Script);
  Button5.Visible := View is TfrMemoView;
{$IFNDEF Delphi2}
  M1.Font.Charset := frCharset;
  M2.Font.Charset := frCharset;
{$ENDIF}
end;

procedure TfrEditorForm.FormHide(Sender: TObject);
begin
  if ModalResult = mrOk then
  begin
    frDesigner.BeforeChange;
    M1.WordWrap := False;
    View.Memo.Text := M1.Text;
    View.Script.Text := M2.Text;
  end;
end;

procedure TfrEditorForm.WMGetMinMaxInfo(var Msg: TWMGetMinMaxInfo);
begin
  with Msg.MinMaxInfo^ do
  begin
    ptMinTrackSize.x := Button2.Left + Button2.Width + 4 + 8;
    ptMinTrackSize.y := 200;
  end;
end;

procedure TfrEditorForm.AdjustMemoSizes;
begin
  M1.Height := MemoPanel.Height - M1.Top - 2;
  M1.Width :=  MemoPanel.Width - M1.Left - 4;
  M2.Height := ScriptPanel.Height - M2.Top - 6;
  M2.Width :=  ScriptPanel.Width - M2.Left - 4;
  Panel5.Width := MemoPanel.Width - Panel5.Left - 4;
end;

procedure TfrEditorForm.Button3Click(Sender: TObject);
begin
  frVarForm := TfrVarForm.Create(nil);
  with frVarForm do
  if ShowModal = mrOk then
    if SelectedItem <> '' then
    begin
      ClipBoard.Clear;
      ClipBoard.AsText := '[' + SelectedItem + ']';
      if FActiveMemo = M1 then
        M1.PasteFromClipboard else
        M2.PasteFromClipboard
    end;
  frVarForm.Free;
  TCustomControl(FActiveMemo).SetFocus;
end;

procedure TfrEditorForm.M1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = vk_Insert) and (Shift = []) then Button3Click(Self);
  if Key = vk_Escape then ModalResult := mrCancel;
end;

procedure TfrEditorForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Chr(Key) = 'F') and (ssCtrl in Shift) and Button5.Visible then
  begin
    Button5Click(nil);
    Key := 0;
  end;
  if (Key = vk_Return) and (ssCtrl in Shift) then
  begin
    ModalResult := mrOk;
    Key := 0;
  end;
end;

procedure TfrEditorForm.Button4Click(Sender: TObject);
begin
  frFieldsForm := TfrFieldsForm.Create(nil);
  with frFieldsForm do
  if ShowModal = mrOk then
    if DBField <> '' then
    begin
      ClipBoard.Clear;
      ClipBoard.AsText := '[' + DBField + ']';
      if FActiveMemo = M1 then
        M1.PasteFromClipboard else
        M2.PasteFromClipboard
    end;
  frFieldsForm.Free;
  TCustomControl(FActiveMemo).SetFocus;
end;

procedure TfrEditorForm.M1Enter(Sender: TObject);
begin
  FActiveMemo := Sender;
end;

procedure TfrEditorForm.CB1Click(Sender: TObject);
begin
  if CB1.Checked then
    ScriptPanel.Top := MemoPanel.Top + 1;
  ScriptPanel.Visible := CB1.Checked;
  Splitter.Top := MemoPanel.Top + 1;
  if ScriptPanel.Visible then
    Splitter.Cursor := crVSplit else
    Splitter.Cursor := crDefault;
  AdjustMemoSizes;
end;

procedure TfrEditorForm.CB2Click(Sender: TObject);
begin
  if CB2.Checked then
    M1.Font.Size := 12 else
    M1.Font.Size := 10;
  M2.Font.Size := M1.Font.Size;
end;

procedure TfrEditorForm.CB3Click(Sender: TObject);
begin
  M1.WordWrap := CB3.Checked;
end;

procedure TfrEditorForm.FormCreate(Sender: TObject);
begin
  Caption := LoadStr(frRes + 060);
  Label1.Caption := LoadStr(frRes + 061);
  CB1.Caption := LoadStr(frRes + 062);
  CB2.Caption := LoadStr(frRes + 063);
  CB3.Caption := LoadStr(frRes + 064);
  Label2.Caption := LoadStr(frRes + 065);
  Button3.Caption := LoadStr(frRes + 066);
  Button4.Caption := LoadStr(frRes + 067);
  Button5.Caption := LoadStr(frRes + 068);
  Button1.Caption := LoadStr(SOk);
  Button2.Caption := LoadStr(SCancel);
end;

procedure TfrEditorForm.Button5Click(Sender: TObject);
var
  t: TfrMemoView;
begin
  t := TfrMemoView(View);
  frFmtForm := TfrFmtForm.Create(nil);
  with frFmtForm do
  begin
    Format := t.Format;
    Edit1.Text := t.FormatStr;
    if ShowModal = mrOk then
    begin
      frDesigner.BeforeChange;
      t.Format := Format;
      t.FormatStr := Edit1.Text;
    end;
  end;
  frFmtForm.Free;
end;

procedure TfrEditorForm.FormResize(Sender: TObject);
begin
  AdjustMemoSizes;
end;

procedure TfrEditorForm.SplitterMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FDown := True;
  FLastY := Y;
end;

procedure TfrEditorForm.SplitterMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if FDown then
  begin
    ScriptPanel.Height := ScriptPanel.Height - (Y - FLastY);
    Splitter.Top := Splitter.Top + Y - FLastY;
    AdjustMemoSizes;
  end;
end;

procedure TfrEditorForm.SplitterMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FDown := False;
end;


end.

