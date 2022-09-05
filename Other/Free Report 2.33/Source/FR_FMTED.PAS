
{*****************************************}
{                                         }
{             FastReport v2.3             }
{              Format editor              }
{                                         }
{  Copyright (c) 1998-99 by Tzyganenko A. }
{                                         }
{*****************************************}

unit FR_fmted;

interface

{$I FR.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, FR_Utils;

type
  TfrFmtForm = class(TForm)
    GroupBox2: TGroupBox;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    Panel1: TPanel;
    Label5: TLabel;
    Label6: TLabel;
    SplEdit: TEdit;
    Button1: TButton;
    Button2: TButton;
    Panel2: TPanel;
    Edit1: TEdit;
    Label1: TLabel;
    Edit3: TEdit;
    procedure FormActivate(Sender: TObject);
    procedure ComboBox1Click(Sender: TObject);
    procedure DesEditChange(Sender: TObject);
    procedure SplEditChange(Sender: TObject);
    procedure ComboBox2Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure SplEditEnter(Sender: TObject);
    procedure ShowPanel1;
    procedure ShowPanel2;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Format: Integer;
  end;

var
  frFmtForm: TfrFmtForm;

implementation

{$R *.DFM}

uses FR_Class, FR_Const;

const
  CategCount = 5;

{$WARNINGS OFF}
procedure TfrFmtForm.FormActivate(Sender: TObject);
var
  i: Integer;
begin
  Panel1.Hide;
  Panel2.Hide;
  ComboBox1.Items.Clear;
  for i := 0 to CategCount-1 do
    ComboBox1.Items.Add(LoadStr(SCateg1 + i));
  ComboBox1.ItemIndex := (Format and $0F000000) div $01000000;
  ComboBox1Change(nil);
  ComboBox2.ItemIndex := (Format and $00FF0000) div $00010000;
  ShowPanel2;
  ShowPanel1;
end;

procedure TfrFmtForm.ShowPanel1;
begin
  Panel1.Visible := (ComboBox1.ItemIndex = 1) and (not Panel2.Visible);
  if Panel1.Visible then
  begin
    Edit3.Text := IntToStr((Format and $0000FF00) div $00000100);
    SplEdit.Text := Chr(Format and $000000FF);
  end;
end;

procedure TfrFmtForm.ShowPanel2;
begin
  Panel2.Visible := ComboBox2.ItemIndex = 4;
end;

procedure TfrFmtForm.ComboBox1Change(Sender: TObject);
var
  i, k: Integer;
  s: String;
begin
  k := ComboBox1.ItemIndex;
  if k = -1 then Exit;
  ComboBox2.Items.Clear;
  for i := 0 to 4 do
  begin
    s := LoadStr(SFormat11 + k * 5 + i);
    if s <> '' then ComboBox2.Items.Add(s);
  end;
  ComboBox2.ItemIndex := 0;
  if Sender <> nil then
  begin
    ComboBox2Click(nil);
    ShowPanel1;
    Edit1.Text := '';
  end;
end;

procedure TfrFmtForm.ComboBox1Click(Sender: TObject);
begin
  Format := (Format and $F0FFFFFF) + ComboBox1.ItemIndex * $01000000;
end;

procedure TfrFmtForm.ComboBox2Click(Sender: TObject);
begin
  Format := (Format and $FF00FFFF) + ComboBox2.ItemIndex * $00010000;
  ShowPanel2;
  ShowPanel1;
end;

procedure TfrFmtForm.DesEditChange(Sender: TObject);
begin
  Format := (Format and $FFFF00FF) + StrToInt(Edit3.Text) * $00000100;
end;

procedure TfrFmtForm.SplEditChange(Sender: TObject);
var
  c: Char;
begin
  c := ',';
  if SplEdit.Text <> '' then c := SplEdit.Text[1];
  Format := (Format and $FFFFFF00) + Ord(c);
end;

procedure TfrFmtForm.SplEditEnter(Sender: TObject);
begin
  SplEdit.SelectAll;
end;
{$WARNINGS ON}

procedure TfrFmtForm.FormCreate(Sender: TObject);
begin
  Caption := LoadStr(frRes + 420);
  GroupBox2.Caption := LoadStr(frRes + 421);
  Label5.Caption := LoadStr(frRes + 422);
  Label6.Caption := LoadStr(frRes + 423);
  Label1.Caption := LoadStr(frRes + 424);
  Button1.Caption := LoadStr(SOk);
  Button2.Caption := LoadStr(SCancel);
end;

end.
