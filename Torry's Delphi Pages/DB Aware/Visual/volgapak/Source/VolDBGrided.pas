//---------------------------------------------------------------------------
//  TVolgaDBGridColumnsEditor - columns editor for TVolgaDBGrid
//---------------------------------------------------------------------------
//  Copyright © 2000, Olga Vlasova, Russia
//  http://volgatable.chat.ru
//  E-mail: volgatable@chat.ru
//---------------------------------------------------------------------------
unit VolDBGridEd;

interface

{$I Volga.INC}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ToolWin, Menus, VolDBGrid, DB,
  {$IFDEF VER140} DesignIntf, DesignEditors, DesignWindows,
  {$ELSE} LibIntf, DsgnIntf, DsgnWnds,{$ENDIF}
  ImgList;

type
  TVolgaDBGridColumnsEditor = class(TDesignWindow)
    ToolBar1: TToolBar;
    tbAddNew: TToolButton;
    tbDeleteSelected: TToolButton;
    ToolButton3: TToolButton;
    tbAddAllFields: TToolButton;
    tbRestoreDefaults: TToolButton;
    ImageList1: TImageList;
    PopupMenu1: TPopupMenu;
    miAddNew: TMenuItem;
    miDeleteSelected: TMenuItem;
    miAddAllFields: TMenuItem;
    miRestoreDefaults: TMenuItem;
    miSelectAll: TMenuItem;
    N1: TMenuItem;
    ListView1: TListView;
    ToolButton1: TToolButton;
    tbUp: TToolButton;
    tbDown: TToolButton;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure tbAddNewClick(Sender: TObject);
    procedure tbDeleteSelectedClick(Sender: TObject);
    procedure tbAddAllFieldsClick(Sender: TObject);
    procedure tbRestoreDefaultsClick(Sender: TObject);
    procedure ListView1Change(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure FormResize(Sender: TObject);
    procedure miAddNewClick(Sender: TObject);
    procedure miDeleteSelectedClick(Sender: TObject);
    procedure miSelectAllClick(Sender: TObject);
    procedure miAddAllFieldsClick(Sender: TObject);
    procedure miRestoreDefaultsClick(Sender: TObject);
    procedure tbDownClick(Sender: TObject);
    procedure tbUpClick(Sender: TObject);
  private
    FVolgaDBGrid:TVolgaDBGrid;
    function CheckCollection: Boolean;
    procedure SetDBGrid(const Value: TVolgaDBGrid);
    procedure UpdateData;
    procedure Copy;
    procedure Cut;
    procedure Paste;
  protected
    function UniqueName(Component: TComponent): string; override;
    procedure Activated; override;
    { Private declarations }
  public
{$IFDEF VER140}
    function EditAction(Action: TEditAction): Boolean; override;
{$ELSE}
    procedure EditAction(Action: TEditAction); override;
{$ENDIF}
    procedure FormModified; override;
    procedure FormClosed(Form: TCustomForm); override;
    function GetEditState: TEditState; override;
    procedure ComponentDeleted(Component: IPersistent); override;

    property VolgaDBGrid: TVolgaDBGrid read FVolgaDBGrid
         write SetDBGrid;
    { Public declarations }
  end;


{ TVolgaDBGridColumnsProperty }

  TVolgaDBGridColumnsProperty = class(TPropertyEditor)
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure Edit; override;
  end;

{ TVolgaDBGridEditor }

  TVolgaDBGridEditor = class(TComponentEditor)
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

{ TVolgaColumnDataFieldProperty }

  TVolgaColumnDataFieldProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValueList(List: TStrings); virtual;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

{ TVolgaColumnLookupKeyProperty }

  TVolgaColumnLookupKeyProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValueList(List: TStrings); virtual;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

var
  VolgaDBGridColumnsEditor: TVolgaDBGridColumnsEditor;

//procedure ShowItemsEditor({Designer: TDesigner;}
//  AVolgaDBGrid: TVolgaDBGrid);

implementation

{$R *.DFM}

type
  TDesigner = IDesigner;
  TFormDesigner = IFormDesigner;

function FindEditor(AVolgaDBGrid: TVolgaDBGrid): TVolgaDBGridColumnsEditor;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Screen.FormCount - 1 do begin
    if Screen.Forms[I] is TVolgaDBGridColumnsEditor then begin
      if TVolgaDBGridColumnsEditor(Screen.Forms[I]).VolgaDBGrid = AVolgaDBGrid then
      begin
        Result := TVolgaDBGridColumnsEditor(Screen.Forms[I]);
        Break;
      end;
    end;
  end;
end;

procedure ShowItemsEditor(Designer: TDesigner;
  AVolgaDBGrid: TVolgaDBGrid);
var
  Editor: TVolgaDBGridColumnsEditor;
begin
  if AVolgaDBGrid = nil then Exit;
  Editor := FindEditor(AVolgaDBGrid);
  if Editor = nil then begin
    Editor := TVolgaDBGridColumnsEditor.Create(Application);
    try
      Editor.Designer := TFormDesigner(Designer);
      Editor.VolgaDBGrid := AVolgaDBGrid;
      Editor.Show;
    except
      Editor.Free;
      raise;
    end;
  end
  else begin
    Editor.Show;
    if Editor.WindowState = wsMinimized then
      Editor.WindowState := wsNormal;
  end;
end;


{ TDBGridColumnsEditor }

function TVolgaDBGridColumnsEditor.CheckCollection: Boolean;
begin
  Result := (VolgaDBGrid <> nil) and (VolgaDBGrid.Columns <> nil)
    and (Designer.Form <> nil);
end;

procedure TVolgaDBGridColumnsEditor.SetDBGrid(const Value: TVolgaDBGrid);
begin
  if FVolgaDBGrid <> Value then begin
    FVolgaDBGrid := Value;
    UpdateData;
  end;
end;

procedure TVolgaDBGridColumnsEditor.UpdateData;
var
  Empty,Modified: Boolean;
  i:Integer;
  ListItem: TListItem;
begin
  if CheckCollection then begin
    Caption := Format('%s.Columns', [VolgaDBGrid.Name]);
    Empty := VolgaDBGrid.Columns.Count = 0;
  end
  else Empty := True;
  if Empty then begin
    ListView1.Items.Clear;
  end
  else begin
    Modified := False;
    if (VolgaDBGrid.Columns.Count <> ListView1.Items.Count) then Modified := True;
    if (Modified = False) then
      for i := 0 to VolgaDBGrid.Columns.Count - 1 do
        if (ListView1.Items[i].Data <> VolgaDBGrid.Columns[i]) then begin
          Modified := True;
          Break;
        end;
    if (Modified = True) then begin
      ListView1.Items.BeginUpdate;
      ListView1.Items.Clear;
      try
        if (VolgaDBGrid.Columns.State = csCustomized) then begin
           for i := 0 to VolgaDBGrid.Columns.Count - 1 do begin
             ListItem := ListView1.Items.Add;
             ListItem.Caption := IntToStr(i) + ' - ' + VolgaDBGrid.Columns[i].DisplayName;
             ListItem.Data := VolgaDBGrid.Columns[i];
           end;
        end;
      finally
        ListView1.Items.EndUpdate;
      end;
    end else // Заголовок может поменяться когда меняют FieldName
      for i := 0 to VolgaDBGrid.Columns.Count - 1 do
        ListView1.Items[i].Caption := IntToStr(i) + ' - ' + VolgaDBGrid.Columns[i].DisplayName;
  end;
  ListView1Change(nil,nil,ctState);
end;

procedure TVolgaDBGridColumnsEditor.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TVolgaDBGridColumnsEditor.FormCreate(Sender: TObject);
begin
  VolgaDBGrid := nil;
  if NewStyleControls then Font.Style := [];
end;

procedure TVolgaDBGridColumnsEditor.tbAddNewClick(Sender: TObject);
begin
  VolgaDBGrid.Columns.Add;
  UpdateData;
  Designer.Modified;
  ListView1.Items[ListView1.Items.Count-1].Selected := True;
  ListView1Change(nil,nil,ctState);
end;

procedure TVolgaDBGridColumnsEditor.tbDeleteSelectedClick(Sender: TObject);
var i,sel:Integer;
{$IFDEF Delphi4}
  FComponents: TComponentList;
{$ELSE}
  FComponents: TDesignerSelectionList;
{$ENDIF}
begin
  sel := -1;
  if (ListView1.SelCount > 0) then begin
{$IFDEF Delphi4}
    FComponents := TComponentList.Create;
{$ELSE}
    FComponents := TDesignerSelectionList.Create;
{$ENDIF}
    try
      VolgaDBGrid.Columns.BeginUpdate;
      ListView1.Items.BeginUpdate;
      sel := ListView1.Items.IndexOf(ListView1.Selected);
      for i := ListView1.Items.Count - 1 downto 0 do
       if (ListView1.Items[i].Selected = True) and Assigned(ListView1.Items[i].Data) then begin
{$IFDEF Delphi4}
         FComponents.Add(TVolgaColumn(ListView1.Items[i].Data));
{$ELSE}
         FComponents.Add(ListView1.Items[i].Data);
{$ENDIF}
       end;
      ListView1.Items.Clear;

      for i := 0 to FComponents.Count - 1 do FComponents[i].Free;
    finally
      ListView1.Items.EndUpdate;
      VolgaDBGrid.Columns.EndUpdate;
      FComponents.Free;
      Designer.Modified;
      if (ListView1.Items.Count > 0) then
        if (sel > ListView1.Items.Count - 1) then
           ListView1.Items[ListView1.Items.Count-1].Selected := True
        else
           ListView1.Items[sel].Selected := True;
      ListView1Change(nil,nil,ctState);
    end;
   end;
end;

procedure TVolgaDBGridColumnsEditor.tbAddAllFieldsClick(Sender: TObject);
var msgValue:Word;
    i:Integer;
    Col:TVolgaColumn;
begin
  if (VolgaDBGrid.Columns.State = csDefault) then
    VolgaDBGrid.Columns.State := csCustomized
  else begin
   if (VolgaDBGrid.Columns.Count > 0) then begin
     msgValue := MessageDlg('Delete all columns?',
       mtConfirmation, [mbYes, mbNo, mbCancel], 0);
     case msgValue of
       mrYes: VolgaDBGrid.Columns.Clear;
       mrCancel: Exit;
     end;
   end;
   for i := 0 to VolgaDBGrid.DataSource.DataSet.FieldCount - 1 do begin
     Col := VolgaDBGrid.Columns.Add;
     Col.FieldName := VolgaDBGrid.DataSource.DataSet.Fields[i].FieldName;
   end;
  end;
  UpdateData;
  Designer.Modified;
end;

procedure TVolgaDBGridColumnsEditor.tbRestoreDefaultsClick(Sender: TObject);
var i:Integer;
  ListItem: TListItem;
begin
  if (ListView1.SelCount > 0) then begin
    ListItem := ListView1.Selected;
    for i := 0 to ListView1.SelCount - 1 do begin
      TVolgaColumn(ListItem.Data).RestoreDefaults;
      ListItem := ListView1.GetNextItem(ListItem,sdBelow,[isSelected]);
    end;
    ListView1Change(nil,nil,ctState);
    Designer.Modified;
  end;
end;

procedure TVolgaDBGridColumnsEditor.ListView1Change(Sender: TObject;
  Item: TListItem; Change: TItemChange);
var
{$IFDEF Delphi4}
  FComponents: TComponentList;
{$ELSE}
  FComponents: TDesignerSelectionList;
{$ENDIF}
  i: Integer;
begin
  if (Change <> ctState)  then Exit;
  tbDeleteSelected.Enabled := ListView1.SelCount > 0;
  tbRestoreDefaults.Enabled := ListView1.Items.Count > 0;

  miDeleteSelected.Enabled := ListView1.SelCount > 0;
  miRestoreDefaults.Enabled := ListView1.Items.Count > 0;
  miSelectAll.Enabled := ListView1.Items.Count > 0;
  tbAddAllFields.Enabled := Assigned(VolgaDBGrid) and Assigned(VolgaDBGrid.DataSource) and
    Assigned(VolgaDBGrid.Datasource.Dataset) and (VolgaDBGrid.Datasource.Dataset.FieldCount > 0);

  if CheckCollection and Active then begin
{$IFDEF Delphi4}
     FComponents := TComponentList.Create;
{$ELSE}
     FComponents := TDesignerSelectionList.Create;
{$ENDIF}
    if (ListView1.SelCount > 0) then begin
      for i := ListView1.Items.Count - 1 downto 0 do
       if (ListView1.Items[i].Selected = True) and Assigned(ListView1.Items[i].Data) then
{$IFDEF Delphi4}
         FComponents.Add(TVolgaColumn(ListView1.Items[i].Data));
{$ELSE}
         FComponents.Add(ListView1.Items[i].Data);
{$ENDIF}
    end
    else FComponents.Add(VolgaDBGrid.Columns);
    SetSelection(FComponents);
  end;
end;

procedure TVolgaDBGridColumnsEditor.Activated;
begin
  ListView1Change(nil,nil,ctState);
end;

function TVolgaDBGridColumnsEditor.UniqueName(Component: TComponent): string;
var
  Temp: string;
begin
  if (Component <> nil) then Temp := Component.ClassName
  else Temp := TVolgaColumn.ClassName;
  if (UpCase(Temp[1]) = 'T') and (Length(Temp) > 1) then
    System.Delete(Temp, 1, 1);
  Result := Designer.UniqueName(Temp);
end;

{$IFDEF VER140}
function TVolgaDBGridColumnsEditor.EditAction(Action: TEditAction): Boolean;
{$ELSE}
procedure TVolgaDBGridColumnsEditor.EditAction(Action: TEditAction);
{$ENDIF}
begin
  case Action of
    eaCut: Cut;
    eaCopy: Copy;
    eaPaste: Paste;
    eaDelete: tbDeleteSelectedClick(Self);
  end;
{$IFDEF VER140}
  REsult := true;
{$ENDIF}
end;

procedure TVolgaDBGridColumnsEditor.Copy;
var
{$IFDEF Delphi4}
  CompList: TComponentList;
{$ELSE}
  CompList: TDesignerSelectionList;
{$ENDIF}
  ListItem: TListItem;
  i:Integer;
begin
{$IFDEF Delphi4}
  CompList := TComponentList.Create;
{$ELSE}
  CompList := TDesignerSelectionList.Create;
{$ENDIF}
  try
    if (ListView1.SelCount > 0) then begin
      ListItem := ListView1.Selected;
      for i := 0 to ListView1.SelCount - 1 do begin
{$IFDEF Delphi4}
        CompList.Add(TVolgaColumn(ListItem.Data));
{$ELSE}
        CompList.Add(ListItem.Data);
{$ENDIF}
        ListItem := ListView1.GetNextItem(ListItem,sdBelow,[isSelected]);
      end;
    end;
  finally
    CompList.Free;
  end;
end;

procedure TVolgaDBGridColumnsEditor.Cut;
begin
  Copy;
  tbDeleteSelectedClick(Self);
end;

procedure TVolgaDBGridColumnsEditor.Paste;
{$IFDEF Delphi4}
var CompList: TComponentList;
{$ELSE}
var CompList: TDesignerSelectionList;
{$ENDIF}
begin
  if CheckCollection then begin
{$IFDEF Delphi4}
    CompList := TComponentList.Create;
{$ELSE}
    CompList := TDesignerSelectionList.Create;
{$ENDIF}
    try
      UpdateData;
    finally
      CompList.Free;
    end;
  end;
end;

procedure TVolgaDBGridColumnsEditor.FormModified;
begin
  if not (csDestroying in ComponentState) then UpdateData;
end;

procedure TVolgaDBGridColumnsEditor.FormClosed(Form: TCustomForm);
begin
  if Form = Designer.Form then Close;
end;

procedure TVolgaDBGridColumnsEditor.ComponentDeleted(Component: IPersistent);
begin
  if ExtractPersistent(Component) = VolgaDBGrid then begin
    VolgaDBGrid := nil;
    Close;
  end;
end;

function TVolgaDBGridColumnsEditor.GetEditState: TEditState;
begin
  Result := [];
  if tbDeleteSelected.Enabled then Result := [esCanDelete];
//  if tbDeleteSelected.Enabled then Result := [esCanDelete, esCanCut, esCanCopy];
//  if ClipboardComponents then Include(Result, esCanPaste);
end;

procedure TVolgaDBGridColumnsEditor.FormResize(Sender: TObject);
begin
  ListView1.Columns[0].Width := ListView1.ClientWidth;
end;

procedure TVolgaDBGridColumnsEditor.miAddNewClick(Sender: TObject);
begin
  tbAddNewClick(Sender);
end;

procedure TVolgaDBGridColumnsEditor.miDeleteSelectedClick(Sender: TObject);
begin
  tbDeleteSelectedClick(Sender);
end;

procedure TVolgaDBGridColumnsEditor.miSelectAllClick(Sender: TObject);
var i:Integer;
begin
  for i := 0 to ListView1.Items.Count - 1 do ListView1.Items[i].Selected := True;
end;

procedure TVolgaDBGridColumnsEditor.miAddAllFieldsClick(Sender: TObject);
begin
  tbAddAllFieldsClick(Sender);
end;

procedure TVolgaDBGridColumnsEditor.miRestoreDefaultsClick(Sender: TObject);
begin
  tbRestoreDefaultsClick(Sender);
end;

procedure TVolgaDBGridColumnsEditor.tbDownClick(Sender: TObject);
var
  I, InsPos, sel: Integer;
begin
  if (ListView1.SelCount = 0) or (ListView1.SelCount > 1) then Exit;
  InsPos := ListView1.Items.Count - 1;
  sel := ListView1.Items.IndexOf(ListView1.Selected)+1;
  while not ListView1.Items[InsPos].Selected do
    Dec(InsPos);
  if InsPos < (ListView1.Items.Count -1) then Inc(InsPos);
  VolgaDBGrid.Columns.BeginUpdate;
  try
     for I := ListView1.Items.Count - 1 downto 0 do
       if ListView1.Items[I].Selected then
       begin
         VolgaDBGrid.Columns.Items[I].Index := InsPos;
         Dec(InsPos);
       end;
  finally
    VolgaDBGrid.Columns.EndUpdate;
  end;
  Designer.Modified;
  if (sel > ListView1.Items.Count - 1) then
     ListView1.Items[ListView1.Items.Count-1].Selected := True
  else
     ListView1.Items[sel].Selected := True;
  ListView1Change(nil,nil,ctState);
end;

procedure TVolgaDBGridColumnsEditor.tbUpClick(Sender: TObject);
var
  I, InsPos, sel: Integer;
begin
  if (ListView1.SelCount = 0) or (ListView1.SelCount > 1) then Exit;
  InsPos := 0;
  sel := ListView1.Items.IndexOf(ListView1.Selected)-1;
  while not ListView1.Items[InsPos].Selected do
    Inc(InsPos);
  if InsPos > 0 then Dec(InsPos);
  VolgaDBGrid.Columns.BeginUpdate;
  try
     for I := 0 to ListView1.Items.Count - 1 do
       if ListView1.Items[I].Selected then
       begin
         VolgaDBGrid.Columns.Items[I].Index := InsPos;
         Inc(InsPos);
       end;
  finally
    VolgaDBGrid.Columns.EndUpdate;
  end;
  Designer.Modified;
  if (sel < 0) then
     ListView1.Items[0].Selected := True
  else
     ListView1.Items[sel].Selected := True;
  ListView1Change(nil,nil,ctState);
end;

{ TVolgaDBGridColumnsProperty }

procedure TVolgaDBGridColumnsProperty.Edit;
begin
  ShowItemsEditor(Designer, TVolgaDBGrid(GetComponent(0)));
end;

function TVolgaDBGridColumnsProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

function TVolgaDBGridColumnsProperty.GetValue: string;
begin
  FmtStr(Result, '(%s)', [GetPropType^.Name]);
end;


{ TVolgaDBGridEditor }

procedure TVolgaDBGridEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0: ShowItemsEditor(Designer, TVolgaDBGrid(Component));
  end;
end;

function TVolgaDBGridEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'Columns Editor ...';
  end;
end;

function TVolgaDBGridEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

{ TVolgaColumnDataFieldProperty }

function TVolgaColumnDataFieldProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paMultiSelect];
end;

procedure TVolgaColumnDataFieldProperty.GetValueList(List: TStrings);
var
  Grid: TVolgaCustomDBGrid;
  DataSource: TDataSource;
begin
  Grid := (GetComponent(0) as TVolgaColumn).Grid;
  if (Grid = nil) then Exit;
  DataSource := Grid.DataSource;
  if (DataSource <> nil) and (DataSource.DataSet <> nil)
  and (DataSource.DataSet.FieldCount > 0) then
    DataSource.DataSet.GetFieldNames(List);
end;

procedure TVolgaColumnDataFieldProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
  Values: TStringList;
begin
  Values := TStringList.Create;
  try
    GetValueList(Values);
    for I := 0 to Values.Count - 1 do
      Proc(Values[I]);
  finally
    Values.Free;
  end;
end;

{ TVolgaColumnLookupKeyProperty }

procedure TVolgaColumnLookupKeyProperty.GetValueList(List: TStrings);
var
  Column: TVolgaColumn;
begin
  Column := GetComponent(0) as TVolgaColumn;
  if (Column = nil) then Exit;
  if (Column.LookupDataSet <> nil) and (Column.LookupDataSet.FieldCount>0) then
    Column.LookupDataSet.GetFieldNames(List);
end;

function TVolgaColumnLookupKeyProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paMultiSelect];
end;

procedure TVolgaColumnLookupKeyProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
  Values: TStringList;
begin
  Values := TStringList.Create;
  try
    GetValueList(Values);
    for I := 0 to Values.Count - 1 do
      Proc(Values[I]);
  finally
    Values.Free;
  end;
end;

end.
