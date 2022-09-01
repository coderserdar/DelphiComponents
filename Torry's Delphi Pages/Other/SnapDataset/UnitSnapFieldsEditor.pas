unit UnitSnapFieldsEditor;

{*******************************************************************}
{                                                                   }
{       SnapObjectDataset Field Editor                              }
{                                                                   }
{       Copyright (c) 2006 by Cosimo De Michele.                    }
{       ALL RIGHTS RESERVED                                         }
{                                                                   }
{   The entire contents of this file is protected by                }
{   International Copyright Laws. Unauthorized reproduction,        }
{   reverse-engineering, and distribution of all or any portion of  }
{   the code contained in this file is strictly prohibited and may  }
{   result in severe civil and criminal penalties and will be       }
{   prosecuted to the maximum extent possible under the law.        }
{                                                                   }
{   RESTRICTIONS                                                    }
{                                                                   }
{   THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED      }
{   FILES OR ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE        }
{   COPIED, TRANSFERRED, SOLD, DISTRIBUTED, OR OTHERWISE MADE       }
{   AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT  }
{   AND PERMISSION FROM THE AUTHOR.                                 }
{                                                                   }
{*******************************************************************}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, SnapObjectDataset, DesignIntf, DB,
  SnapBaseDataset, ExtCtrls, Menus;

type
  IFormDesigner = IDesigner;

  TFormFieldEditor = class(TForm)
    ListBox: TListBox;
    Panel1: TPanel;
    btnAdd: TButton;
    btnDelete: TButton;
    btnMoveUp: TButton;
    btnMoveDown: TButton;
    btnImport: TButton;
    PopupMenu: TPopupMenu;
    mnuAdd: TMenuItem;
    mnuDelete: TMenuItem;
    N1: TMenuItem;
    mnuImport: TMenuItem;
    N2: TMenuItem;
    mnuMoveUp: TMenuItem;
    mnuMoveDown: TMenuItem;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ListBoxClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnMoveUpClick(Sender: TObject);
    procedure btnMoveDownClick(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnImportClick(Sender: TObject);
  private
    procedure MoveField(ADirection: Integer);
    procedure FillList;
    procedure GetSelection(AList: TList);
    procedure SetSelection(AList: TList);
  public
    Data: TSnapObjectDataset;
    FormDesigner: IFormDesigner;
  end;

  function ShowSnapFieldEditor(AData: TSnapObjectDataset; FormDesigner: IFormDesigner): Boolean;

implementation

uses
  UnitSnapAddFieldEditor,
  UnitSnapImportFieldEditor;

{$R *.dfm}

var
  FormList: TList;

type
  TSnapDataSetDesigner = class(TDataSetDesigner)
  private
   AForm: TFormFieldEditor;
   FDestroying: Boolean;
  public
    destructor Destroy; override;
    procedure DataEvent(Event: TDataEvent; Info: Longint); override;
  end;


function ShowSnapFieldEditor(AData: TSnapObjectDataset; FormDesigner: IFormDesigner): Boolean;
var
  AForm: TFormFieldEditor;
  i: Integer;
begin
  AForm := nil;
  for i := 0 to FormList.Count - 1 do
    if TFormFieldEditor(FormList[i]).Data = AData then
    begin
      AForm := TFormFieldEditor(FormList[i]);
      Break;
    end;
  if AForm = nil then
  begin
    AForm := TFormFieldEditor.Create(Application);
    AForm.Data := AData;
    AForm.FormDesigner := FormDesigner;

    TSnapDataSetDesigner.Create(AData);
    TSnapDataSetDesigner(AData.Designer).AForm := AForm;

    FormList.Add(AForm);
  end;
  with AForm do
  begin
    FillList;
    Caption := 'Editing ' + Data.Name + '.Fields';
    Show;
    Result := True;
  end;
end;

{ TSnapDataSetDesigner }

procedure TSnapDataSetDesigner.DataEvent(Event: TDataEvent; Info: Integer);
var
  i, j: Integer;
begin
  if AForm <> nil then
    with AForm do
    begin
      ListBox.Items.BeginUpdate;
      for i := 0 to Data.FieldCount - 1 do
        if Data.Fields[i].Owner = Data.Owner then
        begin
          j := ListBox.Items.IndexOfObject(Data.Fields[i]);
          if j > -1 then
            ListBox.Items[j] := Data.Fields[i].FieldName;
        end;
      ListBox.Items.EndUpdate;
    end;
end;

destructor TSnapDataSetDesigner.Destroy;
begin
  FDestroying := True;
  if AForm <> nil then
    AForm.Close;
  inherited Destroy;
end;

procedure TFormFieldEditor.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if (Data <> nil) and
     Not (csDestroying in Data.ComponentState) and
     (Data.Designer <> nil) and
     not TSnapDataSetDesigner(Data.Designer).FDestroying then
  begin
    TSnapDataSetDesigner(Data.Designer).AForm := nil;
    Data.Designer.Free;
  end;

  FormList.Remove(self);
  Action := caFree;
end;

procedure TFormFieldEditor.FillList;
Var
  i: Integer;
begin
  ListBox.Items.BeginUpdate;
  ListBox.Items.Clear;
  for i := 0 to Data.FieldCount - 1 do
    if Data.Fields[i].Owner = Data.Owner then
      ListBox.Items.AddObject(Data.Fields[i].FieldName, Data.Fields[i]);
  ListBox.Items.EndUpdate;
  ListBoxClick(nil);
end;

procedure TFormFieldEditor.ListBoxClick(Sender: TObject);
var
  List: IDesignerSelections;
  i: Integer;
begin
  if (csDesigning in Data.ComponentState) then
  begin
    List := CreateSelectionList;
    try
      for i := 0 to Listbox.Items.Count - 1 do
        if Listbox.Selected[i] then
          List.Add(TComponent(Listbox.Items.Objects[i]));
        if List.Count > 0 then
          FormDesigner.SetSelections(List)
        else
          FormDesigner.SelectComponent(Data);
    finally
      List := nil;
    end;
  end;
  btnDelete.Enabled := Listbox.SelCount > 0;
  btnMoveUp.Enabled := Listbox.SelCount > 0;
  btnMoveDown.Enabled := Listbox.SelCount > 0;
end;

procedure TFormFieldEditor.btnDeleteClick(Sender: TObject);
var
  i, OldIndex: Integer;
  List: TList;
begin
  if Data <> nil then
  begin
    OldIndex := ListBox.ItemIndex;
    List := TList.Create;
    for i := 0 to ListBox.Items.Count - 1 do
     if ListBox.Selected[i] then
        List.Add(ListBox.Items.Objects[i]);
    for i := 0 to List.Count - 1 do
       TField(List[i]).Free;
    List.Free;
    FillList;
    if OldIndex >= ListBox.Items.Count then
      OldIndex := ListBox.Items.Count - 1;
    if (OldIndex <> -1) and (ListBox.Items.Count > 0) then
      ListBox.Selected[OldIndex] := True;
    ListBox.SetFocus;
    ListBoxClick(nil);
  end;
end;

procedure TFormFieldEditor.btnMoveUpClick(Sender: TObject);
begin
  MoveField(-1);
end;

procedure TFormFieldEditor.btnMoveDownClick(Sender: TObject);
begin
  MoveField(1);
end;

procedure TFormFieldEditor.btnAddClick(Sender: TObject);
var
  AField: TField;
  P: TPoint;
begin
  P := Point(btnAdd.Left + btnAdd.Width, btnAdd.Top);
  P := ClientToScreen(P);
  Data.Close;
  AField := GetSnapDatasetNewFieldType(Data, P.X, P.Y, FormDesigner);
  if AField <> nil then
  begin
    FillList;
    if ListBox.Items.Count>0 then
    begin
      ListBox.Selected[ListBox.Items.Count-1] := True;
      ListBox.ItemIndex := ListBox.Items.Count-1;
      ListBox.SetFocus;
      ListBoxClick(nil);
    end;
  end;
end;

procedure TFormFieldEditor.MoveField(ADirection: Integer);
var
  i: Integer;
  List: TList;
begin
  if Data <> nil then
  begin
    List := TList.Create;
    try
      GetSelection(List);
      for i := 0 to List.Count - 1 do
        TField(List[i]).Index := TField(List[i]).Index + ADirection;
      if FormDesigner <> nil then
        FormDesigner.Modified;
      FillList;
      SetSelection(List);
    finally
      List.Free;
    end;
    ListBoxClick(nil);
  end;
end;

procedure TFormFieldEditor.GetSelection(AList: TList);
var
  I: Integer;
begin
  for I := 0 to ListBox.Items.Count - 1 do
    if ListBox.Selected[I] then
      AList.Add(ListBox.Items.Objects[i]);
end;

procedure TFormFieldEditor.SetSelection(AList: TList);
var
  I: Integer;
begin
  for I := 0 to ListBox.Items.Count - 1 do
    ListBox.Selected[I] := AList.IndexOf(ListBox.Items.Objects[I]) <> -1;
end;


procedure TFormFieldEditor.btnImportClick(Sender: TObject);
var
  AField: TField;
  P: TPoint;
begin
  P := Point(btnAdd.Left + btnAdd.Width, btnAdd.Top);
  P := ClientToScreen(P);
  Data.Close;
  if GetSnapDatasetImportField(Data, P.X, P.Y, FormDesigner) then
  begin
    FillList;
    if ListBox.Items.Count>0 then
    begin
      ListBox.Selected[ListBox.Items.Count-1] := True;
      ListBox.ItemIndex := ListBox.Items.Count-1;
      ListBox.SetFocus;
      ListBoxClick(nil);
    end;
  end;
end;

initialization
  FormList := TList.Create;

finalization
  FormList.Free;

end.
