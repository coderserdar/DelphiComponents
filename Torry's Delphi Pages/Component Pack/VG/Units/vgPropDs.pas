{*******************************************************}
{                                                       }
{         Vladimir Gaitanoff Delphi VCL Library         }
{         Properties storage                            }
{                                                       }
{         Copyright (c) 1997, 2000                      }
{         Copyright (c) 1995, 1997 RX Library           }
{                                                       }
{*******************************************************}

{$I VG.INC }
{$D-,L-}

unit vgPropDs;

interface

uses
  SysUtils, Messages, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls,
  Buttons, ExtCtrls, Consts, DesignIntf, fDlgStd, Contnrs, DesignWindows, DesignEditors;

{$IFDEF _D4_}
type
{ TPropStorageForm }
  TPropStorageForm = class(TDialogForm)
    Label30: TLabel;
    Label31: TLabel;
    Label2: TLabel;
    UpBtn: TSpeedButton;
    DownBtn: TSpeedButton;
    StoredList: TListBox;
    PropertiesList: TListBox;
    ComponentsList: TListBox;
    AddButton: TButton;
    DeleteButton: TButton;
    ClearButton: TButton;
    procedure AddButtonClick(Sender: TObject);
    procedure ClearButtonClick(Sender: TObject);
    procedure ListClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure DeleteButtonClick(Sender: TObject);
    procedure StoredListClick(Sender: TObject);
    procedure UpBtnClick(Sender: TObject);
    procedure DownBtnClick(Sender: TObject);
    procedure StoredListDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure StoredListDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure PropertiesListDblClick(Sender: TObject);
  private
    { Private declarations }
    FCompOwner: TComponent;
    FDesigner: IDesigner;
    procedure ListToIndex(List: TCustomListBox; Idx: Integer);
    procedure UpdateCurrent;
    procedure DeleteProp(I: Integer);
    function FindProp(const CompName, PropName: string; var IdxComp,
      IdxProp: Integer): Boolean;
    procedure ClearLists;
    procedure CheckAddItem(const CompName, PropName: string);
    procedure AddItem(IdxComp, IdxProp: Integer; AUpdate: Boolean);
    procedure BuildLists(StoredProps: TStrings);
    procedure CheckButtons;
    procedure SetStoredList(AList: TStrings);
  public
    { Public declarations }
  end;

{ TPropStorageEditor }
  TPropStorageEditor = class(TComponentEditor)
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

{ TStoredPropsProperty }
  TStoredPropsProperty = class(TClassProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure Edit; override;
  end;

{ Show component editor }
function ShowStorageDesigner(ACompOwner: TComponent;
  ADesigner: IDesigner; AStoredList: TStrings): Boolean;

{$ENDIF}
implementation

{$IFDEF _D4_}

uses Windows, TypInfo, vgVCLUtl, vgStndrt, vgSystem;

{$R *.DFM}

procedure BoxMoveFocusedItem(List: TListBox; DstIndex: Integer);
begin
  if (DstIndex >= 0) and (DstIndex < List.Items.Count) then
    if DstIndex <> List.ItemIndex then
    begin
      List.Items.Move(List.ItemIndex, DstIndex);
      List.ItemIndex := DstIndex;
    end;
end;

{ TPropStorageEditor }
procedure TPropStorageEditor.ExecuteVerb(Index: Integer);
var
  Storage: TPropStorage;
begin
  Storage := Component as TPropStorage;
  if Index = 0 then
  begin
    ShowStorageDesigner(TComponent(Storage.Owner), Designer,
      Storage.StoredProps);
  end;
end;

function TPropStorageEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'Designer...';
  else
    Result := '';
  end;
end;

function TPropStorageEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

{ TStoredPropsProperty }
function TStoredPropsProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog] - [paSubProperties];
end;

function TStoredPropsProperty.GetValue: string;
begin
  if TStrings(GetOrdValue).Count > 0 then
    Result := inherited GetValue else
    Result := '(none)';
end;

procedure TStoredPropsProperty.Edit;
var
  Storage: TPropStorage;
begin
  Storage := GetComponent(0) as TPropStorage;
  ShowStorageDesigner(Storage.Owner as TComponent, Designer,
    Storage.StoredProps);
end;

{ Show component editor }
function ShowStorageDesigner(ACompOwner: TComponent;
  ADesigner: IDesigner; AStoredList: TStrings): Boolean;
begin
  with TPropStorageForm.Create(Application) do
  try
    FCompOwner := ACompOwner;
    FDesigner := ADesigner;
    AppSetCursor(crHourGlass);
    try
      SetStoredList(AStoredList);
    finally
      AppRestoreCursor;
    end;
    Result := ShowModal = mrOk;
    if Result then
    begin
      AStoredList.Assign(StoredList.Items);
      if Assigned(Designer) then
        Designer.Modified;
    end;
  finally
    Free;
  end;
end;

{ TPropStorageForm }
procedure TPropStorageForm.ListToIndex(List: TCustomListBox; Idx: Integer);

  procedure SetItemIndex(Index: Integer);
  begin
    if TListBox(List).MultiSelect then
      TListBox(List).Selected[Index] := True;
    List.ItemIndex := Index;
  end;

begin
  if Idx < List.Items.Count then
    SetItemIndex(Idx)
  else if Idx - 1 < List.Items.Count then
    SetItemIndex(Idx - 1)
  else if (List.Items.Count > 0) then
    SetItemIndex(0);
end;

procedure TPropStorageForm.UpdateCurrent;
var
  IdxProp: Integer;
  List: TStrings;
begin
  IdxProp := PropertiesList.ItemIndex;
  if IdxProp < 0 then IdxProp := 0;
  if ComponentsList.Items.Count <= 0 then
  begin
    PropertiesList.Clear;
    Exit;
  end;
  if (ComponentsList.ItemIndex < 0) then
    ComponentsList.ItemIndex := 0;
  List := TStrings(ComponentsList.Items.Objects[ComponentsList.ItemIndex]);
  if List.Count > 0 then PropertiesList.Items := List
  else PropertiesList.Clear;
  ListToIndex(PropertiesList, IdxProp);
  CheckButtons;
end;

procedure TPropStorageForm.DeleteProp(I: Integer);
var
  CompName, PropName: string;
  IdxComp, IdxProp, Idx: Integer;
  StrList: TStringList;
begin
  Idx := StoredList.ItemIndex;
  if ParseStoredItem(FCompOwner, StoredList.Items[I], CompName, PropName) then
  begin
    StoredList.Items.Delete(I);
    ListToIndex(StoredList, Idx);
    if not FindProp(CompName, PropName, IdxComp, IdxProp) then begin
      if IdxComp < 0 then begin
        StrList := TStringList.Create;
        try
          StrList.Add(PropName);
          ComponentsList.Items.AddObject(CompName, StrList);
          ComponentsList.ItemIndex := ComponentsList.Items.IndexOf(CompName);
        except
          StrList.Free;
          raise;
        end;
      end
      else begin
        TStrings(ComponentsList.Items.Objects[IdxComp]).Add(PropName);
      end;
      UpdateCurrent;
    end;
  end;
end;

function TPropStorageForm.FindProp(const CompName, PropName: string; var IdxComp,
  IdxProp: Integer): Boolean;
begin
  Result := False;
  IdxComp := ComponentsList.Items.IndexOf(CompName);
  if IdxComp >= 0 then begin
    IdxProp := TStrings(ComponentsList.Items.Objects[IdxComp]).IndexOf(PropName);
    if IdxProp >= 0 then Result := True;
  end;
end;

procedure TPropStorageForm.ClearLists;
var
  I: Integer;
begin
  for I := 0 to ComponentsList.Items.Count - 1 do begin
    ComponentsList.Items.Objects[I].Free;
  end;
  ComponentsList.Items.Clear;
  ComponentsList.Clear;
  PropertiesList.Clear;
  StoredList.Clear;
end;

procedure TPropStorageForm.AddItem(IdxComp, IdxProp: Integer; AUpdate: Boolean);
var
  Idx: Integer;
  StrList: TStringList;
  CompName, PropName: string;
begin
  CompName := ComponentsList.Items[IdxComp];
  StrList := TStringList(ComponentsList.Items.Objects[IdxComp]);
  PropName := StrList[IdxProp];
  StrList.Delete(IdxProp);
  if StrList.Count = 0 then
  begin
    Idx := ComponentsList.ItemIndex;
    StrList.Free;
    ComponentsList.Items.Delete(IdxComp);
    ListToIndex(ComponentsList, Idx);
  end;
  StoredList.Items.Add(CreateStoredItem(CompName, PropName));
  StoredList.ItemIndex := StoredList.Items.Count - 1;
  if AUpdate then UpdateCurrent;
end;

procedure TPropStorageForm.CheckAddItem(const CompName, PropName: string);
var
  IdxComp, IdxProp: Integer;
begin
  if FindProp(CompName, PropName, IdxComp, IdxProp) then
    AddItem(IdxComp, IdxProp, True);
end;

procedure TPropStorageForm.BuildLists(StoredProps: TStrings);
var
  I, J, Components: Integer;
  C: TComponent;
  List: TPropInfoList;
  StrList: TStrings;
  CompName, PropName: string;
begin
  ClearLists;
  if FCompOwner <> nil then
  begin
    Components := FCompOwner.ComponentCount;
    for I := 0 to Components do
    begin
      if I = Components then
        C := FCompOwner else
        C := FCompOwner.Components[I];

      if (C is TPropStorage) or (C.Name = '') then Continue;
      List := TPropInfoList.Create(C, tkProperties);
      try
        StrList := TStringList.Create;
        try
          TStringList(StrList).Sorted := True;
          for J := 0 to List.Count - 1 do
            StrList.Add(List.Items[J]^.Name);
          ComponentsList.Items.AddObject(C.Name, StrList);
        except
          StrList.Free;
          raise;
        end;
      finally
        List.Free;
      end;
    end;
    if StoredProps <> nil then
    begin
      for I := 0 to StoredProps.Count - 1 do begin
        if ParseStoredItem(FCompOwner, StoredProps[I], CompName, PropName) then
          CheckAddItem(CompName, PropName);
      end;
      ListToIndex(StoredList, 0);
    end;
  end else
    StoredList.Items.Clear;
  UpdateCurrent;
end;

procedure TPropStorageForm.SetStoredList(AList: TStrings);
begin
  BuildLists(AList);
  if ComponentsList.Items.Count > 0 then
    ComponentsList.ItemIndex := 0;
  CheckButtons;
end;

procedure TPropStorageForm.CheckButtons;
var
  Enable: Boolean;
begin
  AddButton.Enabled := (ComponentsList.ItemIndex >= 0) and
    (PropertiesList.ItemIndex >= 0);
  Enable := (StoredList.Items.Count > 0) and
    (StoredList.ItemIndex >= 0);
  DeleteButton.Enabled := Enable;
  ClearButton.Enabled := Enable;
  UpBtn.Enabled := Enable and (StoredList.ItemIndex > 0);
  DownBtn.Enabled := Enable and (StoredList.ItemIndex < StoredList.Items.Count - 1);
end;

procedure TPropStorageForm.AddButtonClick(Sender: TObject);
var
  I: Integer;
begin
  if PropertiesList.SelCount > 0 then
  begin
    for I := PropertiesList.Items.Count - 1 downto 0 do
    begin
      if PropertiesList.Selected[I] then
        AddItem(ComponentsList.ItemIndex, I, False);
    end;
    UpdateCurrent;
  end else
    AddItem(ComponentsList.ItemIndex, PropertiesList.ItemIndex, True);
  CheckButtons;
end;

procedure TPropStorageForm.ClearButtonClick(Sender: TObject);
begin
  if StoredList.Items.Count > 0 then
    SetStoredList(nil);
end;

procedure TPropStorageForm.DeleteButtonClick(Sender: TObject);
begin
  DeleteProp(StoredList.ItemIndex);
end;

procedure TPropStorageForm.ListClick(Sender: TObject);
begin
  if Sender = ComponentsList then UpdateCurrent
  else CheckButtons;
end;

procedure TPropStorageForm.FormDestroy(Sender: TObject);
begin
  ClearLists;
end;

procedure TPropStorageForm.StoredListClick(Sender: TObject);
begin
  CheckButtons;
end;

procedure TPropStorageForm.UpBtnClick(Sender: TObject);
begin
  BoxMoveFocusedItem(StoredList, StoredList.ItemIndex - 1);
  CheckButtons;
end;

procedure TPropStorageForm.DownBtnClick(Sender: TObject);
begin
  BoxMoveFocusedItem(StoredList, StoredList.ItemIndex + 1);
  CheckButtons;
end;

procedure TPropStorageForm.StoredListDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := Source = StoredList;
end;

procedure TPropStorageForm.StoredListDragDrop(Sender, Source: TObject; X,
  Y: Integer);
begin
  BoxMoveFocusedItem(StoredList, StoredList.ItemAtPos(Point(X, Y), True));
  CheckButtons;
end;

procedure TPropStorageForm.PropertiesListDblClick(Sender: TObject);
begin
  if AddButton.Enabled then AddButtonClick(nil);
end;
{$ENDIF}

end.