{ This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

  ---------------------------------------------------------------------------

    Author : Luc DAVID Email: luckylazarus@free.fr
    2006 - 2007

    Major changes are indicated in the \Documentation\Changes.pdf file
    Last update 20.07.2007

  --------------------------------------------------------------------------- }

unit SqlitePassMasterDetailFieldsDialog;
{$i ..\..\Sources\SqlitePassDbo.inc}

interface

uses
 {$IFDEF FPC}
  LResources,
 {$ELSE}
  Windows,
  Messages,
 {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Grids, Buttons, ExtCtrls, SqlitePassDbo, SqlitePassDesignErrorLang;

type
  TSqlitePassMasterDetailDialog = class(TForm)
    BtOk: TButton;
    BtCancel: TButton;
    BtnReset: TButton;
    Panel1: TPanel;
    Panel2: TPanel;
    GridJoinedFields: TDrawGrid;
    BtnDelete: TButton;
    BtnClear: TButton;
    Panel3: TPanel;
    Label2: TLabel;
    ListBoxMasterFields: TListBox;
    SbAddField: TSpeedButton;
    ListBoxDetailFields: TListBox;
    Label1: TLabel;
    Label3: TLabel;
    ImageArrowRight: TImage;
    LabelMasterSource: TLabel;
    LabelDetailSource: TLabel;
    procedure SbAddFieldClick(Sender: TObject);
    procedure ListBoxDetailFieldsClick(Sender: TObject);
    procedure ListBoxMasterFieldsClick(Sender: TObject);
    procedure BtnClearClick(Sender: TObject);
    procedure BtnDeleteClick(Sender: TObject);
    procedure GridJoinedFieldsDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure BtOkClick(Sender: TObject);
  private
    DetailDataset: TSqlitePassDataset;
    MasterFieldsList: TStringList;
    DetailFieldsList: TStringList;
    JoinedFields: TStringList;
    Procedure ParseDatasetMasterFields(JoinedFieldsStr: String);
    Procedure RefreshButtons;
  public
    Constructor Create(AOwner: TComponent; Dataset: TSqlitePassDataset); reintroduce;
    Destructor Destroy; override;
  end;

var
  SqlitePassMasterDetailDialog: TSqlitePassMasterDetailDialog;

implementation
{$IFNDEF FPC}
 {$R *.DFM}
{$ENDIF}

constructor TSqlitePassMasterDetailDialog.Create(AOwner: TComponent; Dataset: TSqlitePassDataset);
var
IsDetailDatasetActive, IsMasterDatasetActive: Boolean;
begin
  Inherited Create(AOwner);
  DetailDataset := Dataset;
  LabelDetailSource.Caption := 'From ' + DetailDataset.DatasetName;
  LabelMasterSource.Caption := 'From ' + TSqlitePassDataset(Dataset.MasterSource.DataSet).DatasetName;
  IsDetailDatasetActive := DetailDataset.Active;
  IsMasterDatasetActive := Dataset.MasterSource.DataSet.Active;
  If Not IsDetailDatasetActive
     then DetailDataset.Open;
  If (Not IsMasterDatasetActive) then
     if DetailDataset.MasterSourceAutoActivate
     then Dataset.MasterSource.DataSet.Open
     else begin
          MessageDlg(Msg1005, mtInformation, [mbOk], 0);
          Abort;
          end;

  DetailFieldsList := TStringList.Create;
  DetailDataset.GetFieldNames(ListBoxDetailFields.Items);
  DetailDataset.Active := IsDetailDatasetActive;

  MasterFieldsList := TStringList.Create;
  Dataset.MasterSource.DataSet.GetFieldNames(ListBoxMasterFields.Items);
  Dataset.MasterSource.DataSet.Active := IsMasterDatasetActive;

  GridJoinedFields.ColWidths[0] := 160;
  GridJoinedFields.ColWidths[1] := 20;
  GridJoinedFields.ColWidths[2] := 160;
  JoinedFields := TStringList.Create;
  ParseDatasetMasterFields(DetailDataset.MasterFields);
  GridJoinedFields.Refresh;
  RefreshButtons;
end;

destructor TSqlitePassMasterDetailDialog.Destroy;
begin
 JoinedFields.Free;
 MasterFieldsList.Free;
 DetailFieldsList.Free;
 Inherited Destroy;
end;


Procedure TSqlitePassMasterDetailDialog.ParseDatasetMasterFields(JoinedFieldsStr: String);
var
 i, d, m, EndPos: Integer;
 S: String;
begin
 JoinedFields.Clear;
 JoinedFields.Add('Master Fields=Details Fields');

 if JoinedFieldsStr <> '' then
 Repeat
 EndPos := AnsiPos(';',JoinedFieldsStr);

 If EndPos = 0
    then EndPos := Length(JoinedFieldsStr)+1;

 if EndPos > 0 then
    begin
    S := System.Copy(JoinedFieldsStr, 1, EndPos-1);
    System.Delete(JoinedFieldsStr, 1, EndPos);
    JoinedFields.Add(S);
    end;
 Until JoinedFieldsStr = '';

 For i := 1 to Pred(JoinedFields.count) do
     begin
     m := ListBoxMasterFields.Items.IndexOf(JoinedFields.Names[i]);
     d := ListBoxDetailFields.Items.IndexOf(JoinedFields.Values[JoinedFields.Names[i]]);
     if (m > -1) and (d > -1) then
        begin
        ListBoxMasterFields.Items.Delete(m);
        ListBoxDetailFields.Items.Delete(d);
        end;
     end;
 end;

procedure TSqlitePassMasterDetailDialog.ListBoxDetailFieldsClick(Sender: TObject);
begin
  RefreshButtons;
end;

procedure TSqlitePassMasterDetailDialog.ListBoxMasterFieldsClick(Sender: TObject);
begin
  RefreshButtons;
end;

procedure TSqlitePassMasterDetailDialog.SbAddFieldClick(Sender: TObject);
var
  i, j:Integer;
begin
  i := ListBoxMasterFields.ItemIndex;
  j := ListBoxDetailFields.ItemIndex;
  JoinedFields.Add(ListBoxMasterFields.Items[i]+'='+ListBoxDetailFields.Items[j]);
  ListBoxMasterFields.Items.Delete(i);
  ListBoxDetailFields.Items.Delete(j);
  GridJoinedFields.Refresh;
  RefreshButtons;
end;

procedure TSqlitePassMasterDetailDialog.BtnDeleteClick(Sender: TObject);
var
  Row: Integer;
begin
  Row := GridJoinedFields.Row;
  ListBoxDetailFields.Items.Add(JoinedFields.Names[Row]);
  ListBoxMasterFields.Items.Add(JoinedFields.Values[JoinedFields.Names[Row]]);
  JoinedFields.Delete(Row);
  GridJoinedFields.Refresh;
  RefreshButtons;
end;

procedure TSqlitePassMasterDetailDialog.BtnClearClick(Sender: TObject);
begin
  Repeat
    BtnDelete.Click;
  Until JoinedFields.Count = 1;
end;

procedure TSqlitePassMasterDetailDialog.GridJoinedFieldsDrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
var
  Grid: TDrawGrid;
begin
  Grid := Sender as TDrawGrid;
  If ARow < JoinedFields.Count
     then with Grid do
          begin
          Case ACol of
          0: Canvas.TextRect(Rect,Rect.Left+4,Rect.Top+4, JoinedFields.Names[ARow]);
          1: Canvas.Draw(Rect.Left+3,Rect.Top+1, ImageArrowRight.Picture.Graphic);
          2: Canvas.TextRect(Rect,Rect.Left+4,Rect.Top+4, JoinedFields.Values[JoinedFields.Names[ARow]]);
          end;
          if gdFocused in State then Canvas.DrawFocusRect(Rect);
          end;

end;

procedure TSqlitePassMasterDetailDialog.RefreshButtons;
begin
  SbAddField.Enabled := (ListBoxDetailFields.ItemIndex <> -1) And (ListBoxMasterFields.ItemIndex <> -1);
  BtnDelete.Enabled := (JoinedFields.Count > 1);
  BtnClear.Enabled := (JoinedFields.Count > 1);
end;
procedure TSqlitePassMasterDetailDialog.BtOkClick(Sender: TObject);
var
i: Integer;
JoinedFieldsStr: String;
begin
JoinedFieldsStr := '';
For i := 1 to Pred(JoinedFields.Count) do
    JoinedFieldsStr := JoinedFieldsStr + JoinedFields[i] + ';';

If JoinedFieldsStr <> ''
   then System.Delete(JoinedFieldsStr, Length(JoinedFieldsStr), 1);

DetailDataset.MasterFields := JoinedFieldsStr;
end;

initialization
{$IFDEF FPC}
  {$I SqlitePassMasterDetailFieldsDialog.lrs}
{$ENDIF}

end.
