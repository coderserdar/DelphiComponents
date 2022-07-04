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
    2007-2010
    Last update : 26-01-2010

  --------------------------------------------------------------------------- }

unit SqlitePassSortByDialog;
{$i ..\..\Sources\SqlitePassDbo.inc}

interface

uses
  {$IFDEF FPC}
  LResources,
  {$ELSE}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Math, Buttons, ExtCtrls, DB, SqlitePassConst, SqlitePassDbo;

type

  { TSqlitePassSortedByDialog }

  TSqlitePassSortedByDialog = class(TForm)
    Label2: TLabel;
    PanelBottom: TPanel;
    Label1: TLabel;
    ScrollboxSelectedFields: TScrollBox;
    SbUp: TSpeedButton;
    SbDown: TSpeedButton;
    SbRemoveField: TSpeedButton;
    SbRemoveAllFields: TSpeedButton;
    PanelAvailableFields: TPanel;
    PanelSelectedFields: TPanel;
    LbAvailableFields: TListBox;
    SbAddAllFields: TSpeedButton;
    SbAddField: TSpeedButton;
    BtOk: TButton;
    BtCancel: TButton;
    ImageDescending: TImage;
    ImageAscending: TImage;
    BtnReset: TButton;
    procedure SbAddFieldClick(Sender: TObject);
    procedure SbAddAllFieldsClick(Sender: TObject);
    procedure SbRemoveFieldClick(Sender: TObject);
    procedure DrawSelectedFields(Index: Integer);
    procedure SbRemoveAllFieldsClick(Sender: TObject);
    procedure SbUpClick(Sender: TObject);
    procedure SbDownClick(Sender: TObject);
    procedure BtOkClick(Sender: TObject);
    procedure LbAvailableFieldsDblClick(Sender: TObject);
    procedure BtnResetClick(Sender: TObject);
  private
    SelectedFields: TList;
    SelectedField: TSpeedButton;
    Procedure AddSelectedField(FieldName: String; Asc: Boolean);
    Procedure DisplaySelectedFields(SortedBySql: String);
    procedure RefreshButtons;
    procedure RemoveSelectedField;
    procedure OnSbSelectedFieldClick(Sender: TObject);
  public
    OriginalSQL: String;
    SQL: String;
    constructor Create(AOwner: TComponent; SortedBy: String; Fields: TFields); reintroduce;
    destructor Destroy; override;
  end;

var
  SqlitePassSortedByDialog: TSqlitePassSortedByDialog;

implementation

{$IFNDEF FPC}
 {$R *.DFM}
{$ENDIF}

{ Constructor and Destructor }

constructor TSqlitePassSortedByDialog.Create(AOwner: TComponent;
  SortedBy: String; Fields: TFields);
var
i: Integer;
begin
inherited Create(AOwner);
OriginalSQL := SortedBy;
SelectedFields := TList.Create;
LbAvailableFields.Items.Clear;
for i := 0 to Pred(Fields.count)
    do LbAvailableFields.Items.Add(Fields[i].FieldName);
if Fields.Count > 0
   then LbAvailableFields.ItemIndex := 0;
DisplaySelectedFields(OriginalSql);
end;

destructor TSqlitePassSortedByDialog.Destroy;
var
i: Integer;
begin
For i := 0 to Pred(SelectedFields.Count)
    do TSpeedButton(SelectedFields[i]).Free;

SelectedFields.Free;
inherited Destroy;
end;


{ Add one or more fields to the selection }

procedure TSqlitePassSortedByDialog.AddSelectedField(FieldName: String; Asc: Boolean);
begin
SelectedFields.Add(TSpeedButton.Create(ScrollboxSelectedFields));
With TSpeedButton(SelectedFields[Pred(SelectedFields.Count)]) do
  begin
  Parent := ScrollboxSelectedFields;
  Caption:= FieldName;
  Left := 2;
  Width := ScrollboxSelectedFields.Width -4;
  Height := 34;
  Flat := True;
  Transparent := False;
  GroupIndex := SelectedFields.Count+1;
  AllowAllUp := True;
  Layout := blGlyphBottom;
  if Asc
     then Glyph := ImageAscending.Picture.Bitmap
     else Glyph := ImageDescending.Picture.Bitmap;
  OnClick := OnSbSelectedFieldClick;
  OnDblClick := SbRemoveFieldClick;
  Down := Not Asc;
  end;
DrawSelectedFields(Pred(SelectedFields.Count));
LbAvailableFields.Items.Delete(LbAvailableFields.Items.IndexOf(FieldName));
if LbAvailableFields.Items.Count > 0
   then LbAvailableFields.ItemIndex := 0;
if SelectedFields.Count = 1
   then begin
        SelectedField := TSpeedButton(SelectedFields[0]);
        SelectedField.Font.Style := [fsBold];
        end;
RefreshButtons;
end;

procedure TSqlitePassSortedByDialog.SbAddFieldClick(Sender: TObject);
begin
AddSelectedField(LbAvailableFields.Items[LbAvailableFields.ItemIndex], False);
RefreshButtons;
end;

procedure TSqlitePassSortedByDialog.LbAvailableFieldsDblClick(
  Sender: TObject);
begin
SbAddFieldClick(Sender);
end;

procedure TSqlitePassSortedByDialog.SbAddAllFieldsClick(Sender: TObject);
begin
LbAvailableFields.ItemIndex := 0;
While LbAvailableFields.Items.Count > 0
    do AddSelectedField(LbAvailableFields.Items[0], False);
end;


{ Remove one or more fields from the selection }

procedure TSqlitePassSortedByDialog.SbRemoveFieldClick(Sender: TObject);
begin
RemoveSelectedField;
end;

procedure TSqlitePassSortedByDialog.RemoveSelectedField;
var
i: integer;
begin
LbAvailableFields.Items.Add(SelectedField.Caption);
i := SelectedFields.IndexOf(SelectedField);
SelectedFields.Remove(SelectedField);
SelectedField.Free;
if SelectedFields.Count > 0
   then begin
   SelectedField := TSpeedButton(SelectedFields[Min(i, Pred(SelectedFields.Count))]);
   SelectedField.Font.Style := [fsBold];
   DrawSelectedFields(-1);
   end;
RefreshButtons;
end;

procedure TSqlitePassSortedByDialog.SbRemoveAllFieldsClick(
  Sender: TObject);
begin
While SelectedFields.Count > 0
    do RemoveSelectedField;
end;


{ Mark the current selection et set the sort direction }

procedure TSqlitePassSortedByDialog.OnSbSelectedFieldClick(Sender: TObject);
begin
{ Clear the previously selected button }
if Assigned(SelectedField)
   then SelectedField.Font.Style := [];
{ Mark current selected button as Bold }
With TSpeedButton(Sender) do
     begin
     Font.Style := [fsBold];
     if Down
        then Glyph := ImageDescending.Picture.Bitmap
        else Glyph := ImageAscending.Picture.Bitmap;
     end;
SelectedField := TSpeedButton(Sender);
RefreshButtons;
end;


{ Update buttons state id necesary }

procedure TSqlitePassSortedByDialog.RefreshButtons;
begin
SbAddField.Enabled := (LbAvailableFields.Items.Count > 0);
SbAddAllFields.Enabled := (LbAvailableFields.Items.Count > 0);
SbRemoveField.Enabled := (SelectedFields.Count > 0);
SbRemoveAllFields.Enabled := (SelectedFields.Count > 0);
SbUp.Enabled := (SelectedFields.Count > 1) and (SelectedField <> SelectedFields[0]) ;
SbDown.Enabled := (SelectedFields.Count > 1) and (SelectedField <> SelectedFields[Pred(SelectedFields.Count)]);
if (LbAvailableFields.Items.Count > 0) and (LbAvailableFields.ItemIndex = -1)
   then LbAvailableFields.ItemIndex := 0;
end;


{ Draw the scrollbox content : selected fields }

procedure TSqlitePassSortedByDialog.DrawSelectedFields(Index: Integer);
var
i: integer;
begin
if Index < 0 then
   begin
   for i := 0 to Pred(SelectedFields.Count)
       do TSpeedButton(SelectedFields[i]).Top  := (i*36)+2;
   end
   else TSpeedButton(SelectedFields[Index]).Top := (Index*36)+2;
end;

{ Move the selected field up or down }

procedure TSqlitePassSortedByDialog.SbUpClick(Sender: TObject);
var
i: Integer;
begin
i := SelectedFields.IndexOf(SelectedField);
if i = -1 then exit;
SelectedFields.Exchange(i, Pred(i));
SelectedField := SelectedFields[Pred(i)];
DrawSelectedFields(-1);
RefreshButtons;
end;

procedure TSqlitePassSortedByDialog.SbDownClick(Sender: TObject);
var
i: Integer;
begin
i := SelectedFields.IndexOf(SelectedField);
if i = -1 then exit;
SelectedFields.Exchange(i,Succ(i));
SelectedField := SelectedFields[Succ(i)];
DrawSelectedFields(-1);
RefreshButtons;
end;

{ Create the SQL result string }

procedure TSqlitePassSortedByDialog.BtOkClick(Sender: TObject);
var
i: Integer;
begin
for i := 0 to Pred(SelectedFields.Count) do
    begin
    SQL := SQL + '[' + TSpeedButton(SelectedFields[i]).Caption + ']';
    if TSpeedButton(SelectedFields[i]).Down
       then SQL := SQL + ' DESC, '
       else SQL := SQL + ' ASC, ';
    end;
i := Length(SQL);
if i > 0 then
   begin
   System.Delete(SQL, i-1, 2);
   SQL := SQL + ';';
   end;
end;

procedure TSqlitePassSortedByDialog.BtnResetClick(Sender: TObject);
begin
 SbRemoveAllFieldsClick(Sender);
 DisplaySelectedFields(OriginalSql);
end;

procedure TSqlitePassSortedByDialog.DisplaySelectedFields(SortedBySql: String);
var
FieldName, SortDirectionText: String;
SortDirection: Boolean;
SqlStmt: TSqlitePassSqlStmt;
begin
SqlStmt := TSqlitePassSqlStmt.Create(TSqlitePassDatabase(Nil));
SqlStmt.Tokenizer.Text := SortedBySql;
Try
  Try
  While SqlStmt.Tokenizer.Next(ttIdentifier) do
   begin
   FieldName := SqlStmt.Tokenizer.Token.Text;
   SqlStmt.UnquoteString(FieldName);
   if FieldName <> '' then
      begin
      SqlStmt.Tokenizer.Next(ttIdentifier);
      SortDirectionText := Uppercase(SqlStmt.Tokenizer.Token.Text);
      SortDirection := (SortDirectionText = 'ASC');
      AddSelectedField(FieldName, SortDirection);
      end;
   end;
  Except
   On E: Exception do;
  end;
finally
SqlStmt.Free;
RefreshButtons;
end;
end;


initialization
 {$IFDEF FPC}
  {$I SqlitePassSortByDialog.lrs}
 {$ENDIF}
end.
