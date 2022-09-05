
{*****************************************}
{                                         }
{             FastReport v2.3             }
{     Select Band datasource dialog       }
{                                         }
{  Copyright (c) 1998-99 by Tzyganenko A. }
{                                         }
{*****************************************}

unit FR_VBnd;

interface

{$I FR.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, FR_Class, FR_Intrp;

type
  TfrVBandEditorForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    CB1: TComboBox;
    Edit1: TEdit;
    LB1: TListBox;
    procedure FormCreate(Sender: TObject);
    procedure CB1Click(Sender: TObject);
    procedure LB1Click(Sender: TObject);
    procedure CB1Exit(Sender: TObject);
  private
    { Private declarations }
    Band: TfrBandView;
    List: TfrVariables;
    procedure FillCombo;
  public
    { Public declarations }
    procedure ShowEditor(t: TfrView);
  end;

var
  frVBandEditorForm: TfrVBandEditorForm;

implementation

{$R *.DFM}

uses FR_DSet, FR_Const, FR_Utils;

procedure TfrVBandEditorForm.ShowEditor(t: TfrView);
var
  i, j, n: Integer;
  s: String;
  t1: TfrView;
  b: Boolean;
begin
  Band := t as TfrBandView;
  List := TfrVariables.Create;

  s := Band.Dataset;
  b := False;
  if Pos(';', s) = 0 then
    b := True;

  with frDesigner.Page do
  for i := 0 to Objects.Count - 1 do
  begin
    t1 := Objects[i];
    if (t1.Typ = gtBand) and not (TfrBandView(t1).BandType in
      [btReportTitle..btPageFooter, btOverlay, btCrossHeader..btCrossFooter]) then
    begin
      LB1.Items.Add(t1.Name + ': ' + frBandNames[t1.FrameTyp]);
      n := Pos(AnsiUpperCase(t1.Name) + '=', AnsiUpperCase(s));
      if n <> 0 then
      begin
        n := n + Length(t1.Name) + 1;
        j := n;
        while s[j] <> ';' do Inc(j);
        List[t1.Name] := Copy(s, n, j - n);
      end
      else
        if b then
          List[t1.Name] := s else
          List[t1.Name] := '0';
    end;
  end;
  if LB1.Items.Count = 0 then
  begin
    List.Free;
    Exit;
  end;
  FillCombo;
  LB1.ItemIndex := 0;
  LB1Click(nil);

  if ShowModal = mrOk then
  begin
    CB1Exit(nil);
    frDesigner.BeforeChange;
    s := '';
    for i := 0 to List.Count - 1 do
      s := s + List.Name[i] + '=' + List.Value[i] + ';';
    Band.DataSet := s;
  end;
  List.Free;
end;

procedure TfrVBandEditorForm.FillCombo;
begin
  frGetComponents(CurReport.Owner, TfrDataset, CB1.Items, nil);
  CB1.Items.Insert(0, LoadStr(SVirtualDataset));
  CB1.Items.Insert(0, LoadStr(SNotAssigned));
end;

procedure TfrVBandEditorForm.FormCreate(Sender: TObject);
begin
  Caption := LoadStr(frRes + 485);
  GroupBox1.Caption := LoadStr(frRes + 486);
  GroupBox2.Caption := LoadStr(frRes + 487);
  Label1.Caption := LoadStr(frRes + 488);
  Button1.Caption := LoadStr(SOk);
  Button2.Caption := LoadStr(SCancel);
end;

procedure TfrVBandEditorForm.CB1Click(Sender: TObject);
begin
  frEnableControls([Label1, Edit1], CB1.ItemIndex = 1);
end;

procedure TfrVBandEditorForm.LB1Click(Sender: TObject);
var
  i: Integer;
  s: String;
begin
  s := LB1.Items[LB1.ItemIndex];
  s := Copy(s, 1, Pos(':', s) - 1);
  s := List[s];
  if (s <> '') and (s[1] in ['1'..'9']) then
  begin
    i := 1;
    Edit1.Text := s;
  end
  else
  begin
    i := CB1.Items.IndexOf(s);
    if i = -1 then
      i := CB1.Items.IndexOf(LoadStr(SNotAssigned));
  end;
  CB1.ItemIndex := i;
  CB1Click(nil);
end;

procedure TfrVBandEditorForm.CB1Exit(Sender: TObject);
var
  s: String;
begin
  s := LB1.Items[LB1.ItemIndex];
  s := Copy(s, 1, Pos(':', s) - 1);
  if CB1.ItemIndex = 1 then
    List[s] := Edit1.Text else
    List[s] := CB1.Items[CB1.ItemIndex];
end;

end.

