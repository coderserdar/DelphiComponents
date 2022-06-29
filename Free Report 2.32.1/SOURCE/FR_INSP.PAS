
{*****************************************}
{                                         }
{             FastReport v2.3             }
{             Object Inspector            }
{                                         }
{  Copyright (c) 1998-99 by Tzyganenko A. }
{                                         }
{*****************************************}

unit FR_insp;

interface

{$I FR.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, Buttons, FR_Class, FR_Ctrls, FR_Const;

type
  TModifyEvent = procedure(Item: Integer; var EditText: String) of object;

  TCtrlStyle = (csEdit, csDefEditor);

  TPropEditor = class(TForm)
  public
    View: TfrView;
    function ShowEditor: TModalResult; virtual;
  end;

  TProp = class
    Addr: PChar;
    Style: TCtrlStyle;
    Editor: TPropEditor;
    Enabled: Boolean;
    constructor Create(a: PChar; st: TCtrlStyle; de: TPropEditor); virtual;
  end;

  TfrInspForm = class(TForm)
    Panel1: TPanel;
    PaintBox1: TPaintBox;
    SpeedButton1: TfrSpeedButton;
    Edit1: TEdit;
    procedure PaintBox1Paint(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Edit1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure SpeedButton1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Edit1DblClick(Sender: TObject);
    procedure Edit1KeyPress(Sender: TObject; var Key: Char);
    procedure FormResize(Sender: TObject);
  private
    { Private declarations }
    FItems: TStringList;
    FItemIndex: Integer;
    FOnModify: TModifyEvent;
    FRowHeight: Integer;
    w, w1: Integer;
    b: TBitmap;
    procedure SetItems(Value: TStringList);
    procedure SetItemIndex(Value: Integer);
    function GetCount: Integer;
    procedure DrawOneLine(i: Integer; a: Boolean);
    procedure SetItemValue(Value: String);
    function GetItemValue(i: Integer):String;
    function CurItem: TProp;
    procedure WMNCLButtonDblClk(var Message: TMessage); message WM_NCLBUTTONDBLCLK;
  public
    { Public declarations }
    V: TfrView;
    HideProperties: Boolean;
    DefHeight, DefWidth: Integer;
    procedure ClearItems;
    procedure ItemsChanged;
    procedure EnableItem(Index: Integer; Enable: Boolean);
    property Items: TStringList read FItems write SetItems;
    property ItemIndex: Integer read FItemIndex write SetItemIndex;
    property Count: Integer read GetCount;
    property OnModify: TModifyEvent read FOnModify write FOnModify;
  end;


implementation

{$R *.DFM}

constructor TProp.Create(a: PChar; st: TCtrlStyle; de: TPropEditor);
begin
  inherited Create;
  Addr := a;
  Style := st;
  Editor := de;
  Enabled := True;
end;

function TPropEditor.ShowEditor:TModalResult;
begin
  Result := ShowModal;
end;

function TfrInspForm.CurItem: TProp;
begin
  Result := nil;
  if (FItemIndex <> -1) and (Count > 0) then
    Result := TProp(FItems.Objects[FItemIndex]);
end;

procedure TfrInspForm.SetItems(Value: TStringList);
begin
  FItems.Assign(Value);
  FItemIndex := -1;
  PaintBox1.Repaint;
  ItemIndex := 0;
end;

procedure TfrInspForm.SetItemValue(Value: String);
var
  p: TProp;
  s: String[255];
begin
  if HideProperties then Exit;
  p := TProp(FItems.Objects[FItemIndex]);
  s := Value;
  Move(s[0], p.Addr^, Ord(s[0]) + 1);
  if Assigned(FOnModify) then FOnModify(FItemIndex, Value);
  Edit1.Text := Value;
  Edit1.SelectAll;
  Edit1.Modified := False;
end;

function TfrInspForm.GetItemValue(i: Integer): String;
var
  p: TProp;
  s: String[255];
begin
  Result := '';
  p := TProp(FItems.Objects[i]);
  if p = nil then Exit;
  Move(p.Addr^, s[0], Ord(p.Addr^) + 1);
  Result := s;
end;

procedure TfrInspForm.SetItemIndex(Value: Integer);
var
  ww: Integer;
begin
  if Value > Count - 1 then
    Value := Count - 1;
  if not TProp(FItems.Objects[Value]).Enabled then
    Exit;
  Edit1.Visible := (Count > 0) and not HideProperties;
  if (Count = 0) or (FItemIndex = Value) then
    Exit;
  if FItemIndex <> -1 then
    if Edit1.Modified then
      SetItemValue(Edit1.Text);
  FItemIndex := Value;
  SpeedButton1.Visible := (CurItem.Style = csDefEditor) and not HideProperties;
  Edit1.ReadOnly := CurItem.Style = csDefEditor;
  ww := w - w1 - 4;
  if SpeedButton1.Visible then
  begin
    SpeedButton1.SetBounds(w - 16, 2 + FItemIndex * FRowHeight + 1, 14, FRowHeight - 2);
    Dec(ww, 15);
    Edit1.Text := '(' + FItems[FItemIndex] + ')';
  end
  else
    Edit1.Text := GetItemValue(FItemIndex);
  Edit1.SetBounds(w1 + 2, 2 + FItemIndex * FRowHeight + 1, ww, FRowHeight - 2);
  Edit1.SelectAll;
  Edit1.Modified := False;
  PaintBox1Paint(nil);
end;

function TfrInspForm.GetCount: Integer;
begin
  Result := FItems.Count;
end;

procedure TfrInspForm.ItemsChanged;
begin
  FItemIndex := -1;
  ItemIndex := 0;
end;

procedure TfrInspForm.EnableItem(Index: Integer; Enable: Boolean);
begin
  TProp(FItems.Objects[Index]).Enabled := Enable;
  PaintBox1Paint(nil);
end;

procedure TfrInspForm.DrawOneLine(i: Integer; a: Boolean);
  procedure Line(x, y, dx, dy: Integer);
  begin
    b.Canvas.MoveTo(x, y);
    b.Canvas.LineTo(x + dx, y + dy);
  end;
begin
  if not TProp(FItems.Objects[i]).Enabled then Exit;
  if Count > 0 then
  with b.Canvas do
  begin
    Brush.Color := clBtnFace;
    Pen.Color := clBtnShadow;
    Font.Name := 'MS Sans Serif';
    Font.Size := 8;
    Font.Style := [];
    Font.Color := clBlack;
    if a then
    begin
      Pen.Color := clBtnShadow;
      Line(2, 0 + i * FRowHeight, w - 4, 0);
      Line(w1 - 1, 2 + i * FRowHeight, 0, FRowHeight);
      Pen.Color := clBlack;
      Line(2, 1 + i * FRowHeight, w - 4, 0);
      Line(2, 1 + i * FRowHeight, 0, FRowHeight + 1);
      Pen.Color := clBtnHighlight;
      Line(3, FRowHeight + 1 + i * FRowHeight, w - 5, 0);
      Line(Edit1.Left, 2 + i * FRowHeight, Edit1.Width, 0);
      Line(w1, 2 + i * FRowHeight, 0, FRowHeight);
      Line(w1 + 1, 2 + i * FRowHeight, 0, FRowHeight);
      TextOut(7, 3 + i * FRowHeight, FItems[i]);
    end
    else
    begin
      Line(2, FRowHeight + 1 + i * FRowHeight, w - 4, 0);
      Line(w1 - 1, 2 + i * FRowHeight, 0, FRowHeight);
      Pen.Color := clBtnHighlight;
      Line(w1, 2 + i * FRowHeight, 0, FRowHeight);
      TextOut(7, 3 + i * FRowHeight, FItems[i]);
      Font.Color := clNavy;
      if TProp(FItems.Objects[i]).Style = csEdit then
        TextOut(w1 + 2, 3 + i * FRowHeight, GetItemValue(i)) else
        TextOut(w1 + 2, 3 + i * FRowHeight, '(' + FItems[i] + ')');
    end;
  end;
end;

procedure TfrInspForm.PaintBox1Paint(Sender: TObject);
var
  i: Integer;
  r: TRect;
begin
  r := PaintBox1.BoundsRect;
  b.Canvas.Brush.Color := clBtnFace;
  b.Canvas.FillRect(r);
  if not HideProperties then
  begin
    for i := 0 to Count-1 do
      if i <> FItemIndex then
        DrawOneLine(i, False);
    if FItemIndex <> -1 then DrawOneLine(FItemIndex, True);
  end;
  DrawEdge(b.Canvas.Handle, r, EDGE_SUNKEN, BF_RECT);
  PaintBox1.Canvas.Draw(0, 0, b);
end;

procedure TfrInspForm.FormCreate(Sender: TObject);
begin
  w := PaintBox1.Width;
  w1 := w div 2;
  b := TBitmap.Create;
  b.Width := w;
  b.Height := PaintBox1.Height;
  SpeedButton1.Visible := False;
  FItemIndex := -1;
  FItems := TStringList.Create;
  Caption := LoadStr(frRes + 050);
  DefHeight := Height - 3;
  DefWidth := Width;
  FRowHeight := -Font.Height + 5;
  FormResize(nil);
end;

procedure TfrInspForm.FormDestroy(Sender: TObject);
begin
  b.Free;
  ClearItems;
  FItems.Free;
end;

procedure TfrInspForm.ClearItems;
var
  i: Integer;
begin
  for i := 0 to FItems.Count - 1 do
    TProp(FItems.Objects[i]).Free;
  FItems.Clear;
end;

procedure TfrInspForm.PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if HideProperties then Exit;
  ItemIndex := y div FRowHeight;
  Edit1.SetFocus;
end;

procedure TfrInspForm.Edit1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if HideProperties then Exit;
  if Key = vk_Up then
  begin
    if ItemIndex > 0 then
      ItemIndex := ItemIndex - 1;
    Key := 0;
  end
  else if Key = vk_Down then
  begin
    if ItemIndex < Count - 1 then
      ItemIndex := ItemIndex + 1;
    Key := 0;
  end;
end;

procedure TfrInspForm.Edit1KeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
  begin
    if CurItem.Style = csEdit then
    begin
      if Edit1.Modified then SetItemValue(Edit1.Text);
      Edit1.Modified := False;
    end
    else
      SpeedButton1Click(nil);
    Edit1.SelectAll;
    Key := #0;
  end;
end;

procedure TfrInspForm.SpeedButton1Click(Sender: TObject);
var
  s: String;
begin
  if HideProperties then Exit;
  with CurItem.Editor do
  begin
    View := V;
    s := '';
    if ShowEditor = mrOk then
      if Assigned(FOnModify) then FOnModify(FItemIndex, s);
  end;
end;

procedure TfrInspForm.Edit1DblClick(Sender: TObject);
begin
  if CurItem.Style = csDefEditor then
    SpeedButton1Click(nil);
end;

procedure TfrInspForm.FormShow(Sender: TObject);
begin
  SetWindowPos(Handle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOMOVE or
    SWP_NOSIZE or SWP_NOACTIVATE);
end;

procedure TfrInspForm.FormDeactivate(Sender: TObject);
begin
  if CurItem = nil then Exit;
  if CurItem.Style = csEdit then
  begin
    if Edit1.Modified then SetItemValue(Edit1.Text);
    Edit1.Modified := False;
  end;
end;

procedure TfrInspForm.WMNCLButtonDblClk(var Message: TMessage);
begin
  if Height = DefHeight then
  begin
    Height := 0;
    Width := DefWidth div 2;
    Panel1.Hide;
  end
  else
  begin
    Height := DefHeight;
    Width := DefWidth;
    Panel1.Show;
  end;
end;

procedure TfrInspForm.FormResize(Sender: TObject);
begin
  Panel1.Width := ClientWidth - 4;
  Panel1.Height := ClientHeight - 4;
  w := PaintBox1.Width;
  b.Width := w;
  b.Height := PaintBox1.Height;
  Edit1.Width := w - w1 - 4;
end;

end.

