{*******************************************************}
{File:      NCDBTree.PAS                                }
{Revision:  2.00.01 / 02.08.2000                        }
{Comment:   DataBase TreeView                           }
{Copyright: (c) 1997-2000, Dmitry Arefiev               }
{Author:    Dmitry Arefiev, dmitrya@inthink.com         }
{*******************************************************}
{$I NCOciDef.inc}

unit NCDBTree;

interface

Uses ComCtrls, DB, NCOciDB, NCOciParams, Controls, Classes, ExtCtrls, Windows,
     ImgList
{$IFDEF OCI_D6}
    , Variants
{$ENDIF}
    ;

Const
    DeferedTimerResolution: Integer = 100;
    DBTRQMark: String = 'NCQ_';

Type
    TNCDBTreeKeySegment = class;
    TNCDBTreeKeySegments = class;
    TNCDBTreeNode = class;
    TNCDBTreeView = class;

    TNCDBTreeOption = (toMinResources, toMaxFreshed, toHideLeafs,
        toDeferedChange, toDefaultImages, toSQLPreview, toDefaultImageIndexes,
        toUseConst);
    TNCDBTreeOptions = set of TNCDBTreeOption;
    TNCHierarchyType = (htOwnerPointers, htKeySegments);
    TNCDBTreeDescribeNodeEvent = procedure(ASender: TObject; ANode: TTreeNode;
        var AIsItemNode: Boolean) of object;
    TNCDBTreeQueryDataEvent = procedure(ASender: TObject; var AValue: Variant;
        AIsOwnerValue: Boolean) of object;

    TNCDBTreeKeySegment = class(TCollectionItem)
    private
        FEmpty: String;
        FLength: Integer;
        procedure SetEmpty(const AValue: String);
        procedure SetLength(AValue: Integer);
    protected
        function GetDisplayName: string; override;
    public
        procedure Assign(Source: TPersistent); override;
    published
        property Length: Integer read FLength write SetLength;
        property Empty: String read FEmpty write SetEmpty;
    end;

    TNCDBTreeKeySegments = class(TCollection)
    private
        FOwner: TPersistent;
        function GetItem(Index: Integer): TNCDBTreeKeySegment;
        procedure SetItem(Index: Integer; Value: TNCDBTreeKeySegment);
        function GetRootValue: String;
        function GetFullLength: Integer;
    protected
        procedure Update(Item: TCollectionItem); override;
        function GetOwner: TPersistent; override;
    public
        constructor Create(AOwner: TPersistent);
        function Add: TNCDBTreeKeySegment;
        function FillRestEmpty(const s: String; i: Integer): String;
        function FirstEmpty(const s: String; var Len: Integer): Integer;
        function SegQParams(const s: String; var p1, p2: String): Integer;
        property Items[Index: Integer]: TNCDBTreeKeySegment read GetItem
            write SetItem; default;
        property RootValue: String read GetRootValue;
        property FullLength: Integer read GetFullLength;
    end;

    TNCDBTreeNode = class(TTreeNode)
    private
        FOwnerValue: Variant;
        FKeyValue: Variant;
        FDisplayValue: Variant;
        FDataValue: Variant;
        FIsItem: Boolean;
    public
        property OwnerValue: Variant read FOwnerValue;
        property KeyValue: Variant read FKeyValue;
        property DisplayValue: Variant read FDisplayValue;
        property DataValue: Variant read FDataValue;
        property IsItem: Boolean read FIsItem;
    end;

    TNCDBTreeView = class(TCustomTreeView)
    private
        FSkipExpand: Boolean;
        FDisplayFields: String;
        FDataFields: String;
        FKeyFields: String;
        FOwnerFields: String;
        FDisplayFormat: String;
        FOptions: TNCDBTreeOptions;
        FRootValue: Variant;
        FOnDeletion: TTVExpandedEvent;
        FRootName: String;
        FDatabaseName: String;
        FSQLFrom: String;
        FSQLWhere: String;
        FSQLOrderBy: String;
        FSQLParams: TOCIParams;
        FMacros: TStrings;
        FQuery: TOciQuery;
        FLocked: Integer;
        FDeferedTimer: TTimer;
        FDeferedStart, FDeferedDelay: DWORD;
        FHierarchyType: TNCHierarchyType;
        FKeySegments: TNCDBTreeKeySegments;
        FSyncDataSource: TDataSource;
        FOnDescribeNode: TNCDBTreeDescribeNodeEvent;
        FBeforeQueryData, FAfterQueryData: TNCDBTreeQueryDataEvent;
        function QueryData(const AValue: Variant; AIsOwnerValue: Boolean): Boolean;
        procedure NodeSet2Value(const AOwnerValue, AKeyValue: Variant);
        function GetOwnerValue: Variant;
        function GetKeyValue: Variant;
        procedure SetOwnerValue(const AValue: Variant);
        procedure SetKeyValue(const AValue: Variant);
        function GetDisplayValue: Variant;
        function GetDataValue: Variant;
        function BuildNode(AParentNode: TTreeNode; const AOwnerValue,
            AKeyValue, ADisplayValue, ADataValue: Variant): TTreeNode;
        procedure SetSQLParams(AValue: TOCIParams);
        function GetStrProp(AIndex: Integer): String;
        procedure SetStrProp(AIndex: Integer; const AValue: String);
        function GetRootValue: Variant;
        procedure SetRootValue(const AValue: Variant);
        procedure DeferedTimerExpired(ASender: TObject);
        procedure SetOptions(AValue: TNCDBTreeOptions);
        function GetImages: TCustomImageList;
        procedure SetImages(AValue: TCustomImageList);
        procedure SetKeySegments(AValue: TNCDBTreeKeySegments);
        procedure SetHierarchyType(AValue: TNCHierarchyType);
        function RetriveKeyValue: Variant;
        function RetriveOwnerValue: Variant;
        function RetriveDisplayValue: Variant;
        function RetriveDataValue: Variant;
        function GetActive: Boolean;
        procedure SetActive(AValue: Boolean);
        function IsRVS: Boolean;
        procedure SetSyncDataSource(AValue: TDataSource);
        procedure DoSyncDataSource;
        function CanSyncDataSource: Integer;
    protected
        procedure Loaded; override;
        procedure CreateWnd; override;
        procedure DestroyWnd; override;
        procedure Collapse(Node: TTreeNode); override;
        function CanChange(Node: TTreeNode): Boolean; override;
        function CanExpand(Node: TTreeNode): Boolean; override;
        procedure Change(ANode: TTreeNode); override;
        procedure DeferedChange(ANode: TTreeNode); dynamic;
        function DoDescribeNode(ANode: TTreeNode): Boolean; dynamic;
        function IsDefined: Boolean;
        function CreateNode: TTreeNode; override;
    public
        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;
        procedure ExpandNode(ANode: TTreeNode);
        procedure CollapseNode(ANode: TTreeNode);
        procedure RefreshChilds(ANode: TTreeNode);
        procedure RefreshCurrent;
        procedure RefreshTree;
        procedure ClearTree;
        function GoToRoot: TTreeNode;
        procedure BeginUpdate;
        procedure EndUpdate;
        function GetNodeIsItem(ANode: TTreeNode): Boolean;
        function GetNodeOwnerValue(ANode: TTreeNode): Variant;
        function GetNodeDisplayValue(ANode: TTreeNode): Variant;
        function GetNodeDataValue(ANode: TTreeNode): Variant;
        function GetNodeKeyValue(ANode: TTreeNode): Variant;
        procedure MakeNodeVisible(ANode: TTreeNode);
        property DisplayValue: Variant read GetDisplayValue;
        property DataValue: Variant read GetDataValue;
        property KeyValue: Variant read GetKeyValue write SetKeyValue;
        property OwnerValue: Variant read GetOwnerValue write SetOwnerValue;
        property Items;
    published
        property Active: Boolean read GetActive write SetActive default True;
        property RootValue: Variant read GetRootValue write SetRootValue stored IsRVS;
        property RootName: String index 0 read GetStrProp write SetStrProp;
        property DisplayFields: String index 1 read GetStrProp write SetStrProp;
        property DataFields: String index 11 read GetStrProp write SetStrProp;
        property KeyFields: String index 2 read GetStrProp write SetStrProp;
        property OwnerFields: String index 3 read GetStrProp write SetStrProp;
        property DisplayFormat: String index 4 read GetStrProp write SetStrProp;
        property Options: TNCDBTreeOptions read FOptions write SetOptions
            default [toDeferedChange, toMinResources, toMaxFreshed, toDefaultImages,
                     toDefaultImageIndexes];
        property DeferedDelay: DWORD read FDeferedDelay write FDeferedDelay
            default 300;
        property OnDeletion: TTVExpandedEvent read FOnDeletion write FOnDeletion;
        property OnDescribeNode: TNCDBTreeDescribeNodeEvent read FOnDescribeNode
            write FOnDescribeNode;
        property BeforeQueryData: TNCDBTreeQueryDataEvent read FBeforeQueryData write FBeforeQueryData;
        property AfterQueryData: TNCDBTreeQueryDataEvent read FAfterQueryData write FAfterQueryData;
        property Images: TCustomImageList read GetImages write SetImages;
        property HierarchyType: TNCHierarchyType read FHierarchyType
            write SetHierarchyType default htOwnerPointers;
        property KeySegments: TNCDBTreeKeySegments read FKeySegments
            write SetKeySegments;
        property DatabaseName: String index 12 read GetStrProp write SetStrProp;
        property SQLSelect: String index 6 read GetStrProp write SetStrProp;
        property SQLFrom: String index 7 read GetStrProp write SetStrProp;
        property SQLWhere: String index 8 read GetStrProp write SetStrProp;
        property SQLOrderBy: String index 9 read GetStrProp write SetStrProp;
        property SQLParams: TOCIParams read FSQLParams write SetSQLParams;
        property SyncDataSource: TDataSource read FSyncDataSource write SetSyncDataSource;

        property Anchors;
        property AutoExpand;
        property BiDiMode;
        property BorderWidth;
        property Constraints;
        property DragKind;
        property HotTrack;
        property ParentBiDiMode;
        property RightClickSelect;
        property RowSelect;
        property ToolTips;
        property OnCustomDraw;
        property OnCustomDrawItem;
        property OnEndDock;
        property OnStartDock;
        property ShowRoot;
        property ShowButtons;
        property BorderStyle;
        property DragCursor;
        property ShowLines;
        property DragMode;
        property HideSelection default False;
        property Indent;
        property OnExpanding;
        property OnExpanded;
        property OnCollapsing;
        property OnCollapsed;
        property OnChanging;
        property OnChange;
        property OnGetImageIndex;
        property OnGetSelectedIndex;
        property Align;
        property Enabled;
        property Font;
        property Color;
        property ParentColor;
        property ParentCtl3D;
        property Ctl3D;
        property TabOrder;
        property TabStop default True;
        property Visible;
        property OnClick;
        property OnEnter;
        property OnExit;
        property OnDragDrop;
        property OnDragOver;
        property OnStartDrag;
        property OnEndDrag;
        property OnMouseDown;
        property OnMouseMove;
        property OnMouseUp;
        property OnDblClick;
        property OnKeyDown;
        property OnKeyPress;
        property OnKeyUp;
        property PopupMenu;
        property ParentFont;
        property ParentShowHint;
        property ShowHint default False;
        property StateImages;
    end;

    procedure LoadDefaultImages(AImages: TImageList);

implementation

{$R NCDBTree.res}

{---------------------------------------------------------------------}
{---------------------------------------------------------------------}

Uses SysUtils, NCDBUtil, NCUtil, NCUIUtil, Graphics, NCStrs, Forms;

function Var2TextConcat(const AValue: Variant): String;
var
    i: Integer;
begin
    if not VarIsArray(AValue) then
        Result := Var2Text(AValue)
    else begin
        Result := '';
        for i := 0 to VarArrayHighBound(AValue, 1) do
            Result := Result + Var2Text(AValue[i]);
    end;
end;

{---------------------------------------------------------------------}
{---------------------------------------------------------------------}

var
    DefImageList: TImageList;
    DefImageUse: Integer;

procedure LoadDefaultImages(AImages: TImageList);
var
    Icon: TIcon;
begin
    Icon := TIcon.Create;
    try
        Icon.Handle := LoadIcon(HInstance, 'TV_ROOTCLOSE');
        AImages.AddIcon(Icon);
        Icon.Handle := LoadIcon(HInstance, 'TV_ROOTOPEN');
        AImages.AddIcon(Icon);
        Icon.Handle := LoadIcon(HInstance, 'TV_FLDCLOSE');
        AImages.AddIcon(Icon);
        Icon.Handle := LoadIcon(HInstance, 'TV_FLDOPEN');
        AImages.AddIcon(Icon);
        Icon.Handle := LoadIcon(HInstance, 'TV_ITEM');
        AImages.AddIcon(Icon);
    finally
        Icon.Free;
    end;
end;

function CreateDefImageList: TImageList;
begin
    if DefImageUse = 0 then begin
        DefImageList := TImageList.CreateSize(16, 16);
        LoadDefaultImages(DefImageList);
    end;
    Inc(DefImageUse);
    Result := DefImageList;
end;

procedure DestroyDefImageList;
begin
    if DefImageUse > 0 then begin
        Dec(DefImageUse);
        if DefImageUse = 0 then
            DefImageList.Free;
    end;
end;

{---------------------------------------------------------------------}
{---------------------------------------------------------------------}

procedure TNCDBTreeKeySegment.Assign(Source: TPersistent);
begin
    if Source is TNCDBTreeKeySegment then begin
        Empty := TNCDBTreeKeySegment(Source).Empty;
        Length := TNCDBTreeKeySegment(Source).Length;
    end
    else
        inherited Assign(Source);
end;

function TNCDBTreeKeySegment.GetDisplayName: string;
begin
    Result := '''' + Empty + '''';
end;

procedure TNCDBTreeKeySegment.SetEmpty(const AValue: String);
begin
    if FEmpty <> AValue then begin
        FEmpty := AValue;
        if Length < System.Length(FEmpty) then
            FLength := System.Length(FEmpty);
        Changed(False);
    end;
end;

procedure TNCDBTreeKeySegment.SetLength(AValue: Integer);
begin
    if FLength <> AValue then begin
        FLength := AValue;
        if FLength < System.Length(Empty) then
            FEmpty := Copy(FEmpty, 1, FLength);
        Changed(False);
    end;
end;

constructor TNCDBTreeKeySegments.Create(AOwner: TPersistent);
begin
    inherited Create(TNCDBTreeKeySegment);
    FOwner := AOwner;
end;

function TNCDBTreeKeySegments.GetOwner: TPersistent;
begin
    Result := FOwner;
end;

function TNCDBTreeKeySegments.Add: TNCDBTreeKeySegment;
begin
    Result := TNCDBTreeKeySegment(inherited Add);
end;

function TNCDBTreeKeySegments.GetItem(Index: Integer): TNCDBTreeKeySegment;
begin
    Result := TNCDBTreeKeySegment(inherited GetItem(Index));
end;

procedure TNCDBTreeKeySegments.SetItem(Index: Integer; Value: TNCDBTreeKeySegment);
begin
    inherited SetItem(Index, Value);
end;

procedure TNCDBTreeKeySegments.Update(Item: TCollectionItem);
begin
    if (FOwner <> nil) and (FOwner is TNCDBTreeView) then
        TNCDBTreeView(FOwner).RefreshTree;
end;

// Len - позиция первого пустого сегмента
// Result - индекс первого пустого сегмента
function TNCDBTreeKeySegments.FirstEmpty(const s: String; var Len: Integer): Integer;
begin
    len := 1;
    Result := 0;
    while (Result < Count) and
          (Items[Result].Empty <> Copy(s, len, Items[Result].Length)) do begin
        Inc(len, Items[Result].Length);
        Inc(Result);
    end;
end;

// Дополнить пустыми сегментами, начиная с i-ого сегмента
function TNCDBTreeKeySegments.FillRestEmpty(const s: String; i: Integer): String;
begin
    Result := s;
    while i < Count do begin
        Result := Result + Items[i].Empty;
        Inc(i);
    end;
end;

// Получить корневое значение = <seg1.empty> [|| <seg2.empty> [...]]
function TNCDBTreeKeySegments.GetRootValue: String;
begin
    Result := FillRestEmpty('', 0);
end;

// Получить суммарную длину кода = <seg1.length> [ + <seg2.length> [...]]
function TNCDBTreeKeySegments.GetFullLength: Integer;
var
    i: Integer;
begin
    Result := 0;
    for i := 0 to Count - 1 do
        Inc(Result, Items[i].Length);
end;

// Для значения кода узла S возвращает
// 1) Result - индекс первого пустого сегмента
// 2) и два параметра p1, p2 для запроса типа
//    select * from T where KOD like :p1 and KOD not like :p2,
//    позволяющего получить детей узла
function TNCDBTreeKeySegments.SegQParams(const s: String; var p1, p2: String): Integer;
var
    l: Integer;
begin
    Result := FirstEmpty(s, l);
    if Result < Count then begin
        p1 := Stuff(s, l, Items[Result].Length, Replicate('_', Items[Result].Length));
        p2 := Stuff(s, l + Items[Result].Length, Length(s), '%');
    end;
end;

{---------------------------------------------------------------------}
{---------------------------------------------------------------------}

constructor TNCDBTreeView.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    inherited ReadOnly := True;
    inherited SortType := stNone;
    FOptions := [toDeferedChange, toMinResources, toMaxFreshed, toDefaultImages,
                 toDefaultImageIndexes];
    inherited Images := CreateDefImageList;
    FSQLParams := TOCIParams.Create(Self);
    FMacros := TStringList.Create;
    FRootValue := Null;
    FQuery := TOciQuery.Create(nil);
    FQuery.Unidirectional := True;
    FQuery.FetchParams.RowsetSize := 50;
    FQuery.DisableControls;
    FDeferedDelay := 300;
    FKeySegments := TNCDBTreeKeySegments.Create(Self);
    FHierarchyType := htOwnerPointers;
    HideSelection := False;
    ShowHint := False;
end;

destructor TNCDBTreeView.Destroy;
begin
    FQuery.Free;
    FSQLParams.Free;
    FMacros.Free;
    FDeferedTimer.Free;
    FKeySegments.Free;
    Options := [];
    inherited Destroy;
end;

function TNCDBTreeView.CreateNode: TTreeNode;
begin
    Result := TNCDBTreeNode.Create(Items);
end;

procedure TNCDBTreeView.SetSyncDataSource(AValue: TDataSource);
begin
    if AValue <> FSyncDataSource then begin
        FSyncDataSource := AValue;
        if CanSyncDataSource = 1 then
            DoSyncDataSource;
    end;
end;

function TNCDBTreeView.CanSyncDataSource: Integer;
begin
    Result := 0;
    if (FSyncDataSource <> nil) and not (FSyncDataSource.State in [dsInactive, dsSetKey, dsCalcFields]) and
       (Selected <> nil) and (Selected.Parent <> nil) then
        try
            FSyncDataSource.DataSet.CheckBrowseMode;
            Result := 1;
        except
            Application.HandleException(Self);
            Result := -1;
        end;
end;

procedure TNCDBTreeView.DoSyncDataSource;
begin
    if not VarCmp(FSyncDataSource.DataSet[KeyFields], KeyValue, False, False) then
        FSyncDataSource.DataSet.Locate(KeyFields, KeyValue, []);
        // Filter, Range, Query
end;

procedure TNCDBTreeView.SetKeySegments(AValue: TNCDBTreeKeySegments);
begin
    FKeySegments.Assign(AValue);
end;

procedure TNCDBTreeView.SetHierarchyType(AValue: TNCHierarchyType);
begin
    if FHierarchyType <> AValue then begin
        FHierarchyType := AValue;
        RefreshTree;
    end;
end;

function TNCDBTreeView.DoDescribeNode(ANode: TTreeNode): Boolean;
begin
    Result := False;
    if Assigned(OnDescribeNode) then
        OnDescribeNode(Self, ANode, Result);
end;

function TNCDBTreeView.BuildNode(AParentNode: TTreeNode; const AOwnerValue,
    AKeyValue, ADisplayValue, ADataValue: Variant): TTreeNode;
var
    s: String;
begin
    if Items.Count = 0 then
        s := RootName
    else
        s := VarFormat(DisplayFormat, ADisplayValue, Name);
    Result := Items.AddChild(AParentNode, s);
    with Result as TNCDBTreeNode do begin
        FOwnerValue := AOwnerValue;
        FKeyValue := AKeyValue;
        FDisplayValue := ADisplayValue;
        FDataValue := ADataValue;
        ImageIndex := -1;
        SelectedIndex := -1;
        FIsItem := DoDescribeNode(Result);
        if AParentNode = nil then begin
            if toDefaultImageIndexes in Options then begin
                if ImageIndex = -1 then
                    ImageIndex := 0;
                if SelectedIndex = -1 then
                    SelectedIndex := 1;
            end;
            HasChildren := True;
        end
        else if IsItem then begin
            if toDefaultImageIndexes in Options then begin
                if ImageIndex = -1 then
                    ImageIndex := 4;
                if SelectedIndex = -1 then
                    SelectedIndex := 4;
            end;
            HasChildren := False;
        end
        else begin
            if toDefaultImageIndexes in Options then begin
                if ImageIndex = -1 then
                    ImageIndex := 2;
                if SelectedIndex = -1 then
                    SelectedIndex := 3;
            end;
            HasChildren := True;
        end;
    end;
end;

procedure TNCDBTreeView.CreateWnd;
begin
    inherited CreateWnd;
    RefreshTree;
end;

procedure TNCDBTreeView.DestroyWnd;
begin
    ClearTree;
    inherited DestroyWnd;
end;

procedure TNCDBTreeView.SetSQLParams(AValue: TOCIParams);
begin
    FSQLParams.Assign(AValue);
end;

function TNCDBTreeView.GetStrProp(AIndex: Integer): String;
begin
    case AIndex of
    0: if (csDesigning in ComponentState) and (FRootName = '') then
        Result := Name
       else
        Result := FRootName;
    1: Result := FDisplayFields;
    2: Result := FKeyFields;
    3: Result := FOwnerFields;
    4: Result := FDisplayFormat;
    6: Result := DUAliases2SQL(FMacros);
    7: Result := FSQLFrom;
    8: Result := FSQLWhere;
    9: Result := FSQLOrderBy;
   10: Result := Var2Text(RootValue);
   11: Result := FDataFields;
   12: Result := FDatabaseName;
    end;
end;

procedure TNCDBTreeView.SetStrProp(AIndex: Integer; const AValue: String);
    
    procedure FillParams;
    begin
        TOCICustomQuery.FillParams(FSQLParams, SQLFrom + ' ' + SQLWhere + ' ' + SQLSelect);
    end;
    
begin
    if GetStrProp(AIndex) = AValue then
        Exit;
    case AIndex of
    0: FRootName := AValue;
    1: FDisplayFields := AValue;
    2: FKeyFields := AValue;
    3: FOwnerFields := AValue;
    4: FDisplayFormat := AValue;
    6: begin
        FMacros.Clear;
        DUSQL2Aliases(AValue, FMacros);
        FillParams;
       end;
    7: begin
        FSQLFrom := AValue;
        FillParams;
       end;
    8: begin
        FSQLWhere := AValue;
        FillParams;
       end;
    9: FSQLOrderBy := AValue;
   10: if AValue = '' then
        RootValue := Null
       else
        RootValue := AValue;
   11: FDataFields := AValue;
   12: FDatabaseName := AValue;
    end;
    RefreshTree;
end;

function TNCDBTreeView.IsRVS: Boolean;
begin
    Result := not VarIsNull(FRootValue);
end;

function TNCDBTreeView.GetNodeIsItem(ANode: TTreeNode): Boolean;
begin
    if (ANode <> nil) and (ANode is TNCDBTreeNode) then
        Result := TNCDBTreeNode(ANode).IsItem
    else
        Result := False;
end;

function TNCDBTreeView.GetNodeOwnerValue(ANode: TTreeNode): Variant;
begin
    if (ANode <> nil) and (ANode is TNCDBTreeNode) then
        Result := TNCDBTreeNode(ANode).OwnerValue;
end;

function TNCDBTreeView.GetNodeDisplayValue(ANode: TTreeNode): Variant;
begin
    if (ANode <> nil) and (ANode is TNCDBTreeNode) then
        Result := TNCDBTreeNode(ANode).DisplayValue;
end;

function TNCDBTreeView.GetNodeDataValue(ANode: TTreeNode): Variant;
begin
    if (ANode <> nil) and (ANode is TNCDBTreeNode) then
        Result := TNCDBTreeNode(ANode).DataValue;
end;

function TNCDBTreeView.GetNodeKeyValue(ANode: TTreeNode): Variant;
begin
    if (ANode <> nil) and (ANode is TNCDBTreeNode) then
        Result := TNCDBTreeNode(ANode).KeyValue;
end;

function TNCDBTreeView.GetOwnerValue: Variant;
begin
    Result := GetNodeOwnerValue(Selected);
end;

function TNCDBTreeView.GetDisplayValue: Variant;
begin
    Result := GetNodeDisplayValue(Selected);
end;

function TNCDBTreeView.GetDataValue: Variant;
begin
    Result := GetNodeDataValue(Selected);
end;

function TNCDBTreeView.GetKeyValue: Variant;
begin
    Result := GetNodeKeyValue(Selected);
end;

procedure TNCDBTreeView.SetOwnerValue(const AValue: Variant);
begin
    if not VarCmp(OwnerValue, AValue, False, False) then
        try
            Items.BeginUpdate;
            SetKeyValue(AValue);
            if Selected <> nil then begin
                ExpandNode(Selected);
                if Selected.Count > 0 then
                    Selected.Item[0].Selected := True;
            end;
        finally
            Items.EndUpdate;
        end;
end;

procedure TNCDBTreeView.SetKeyValue(const AValue: Variant);
begin
    if not VarCmp(KeyValue, AValue, False, False) then
        try
            Items.BeginUpdate;
            if QueryData(AValue, False) then
                NodeSet2Value(RetriveOwnerValue, RetriveKeyValue)
            else
                GoToRoot;
        finally
            Items.EndUpdate;
            FQuery.Close;
        end;
end;

procedure TNCDBTreeView.SetRootValue(const AValue: Variant);
begin
    if not VarCmp(FRootValue, AValue, False, False) then begin
        FRootValue := AValue;
        RefreshTree;
    end;
end;

function TNCDBTreeView.GetRootValue: Variant;
begin
    if (HierarchyType = htKeySegments) and VarIsNull(FRootValue) then
        Result := KeySegments.RootValue
    else
        Result := FRootValue;
end;

function TNCDBTreeView.GoToRoot: TTreeNode;
begin
    Result := Items.GetFirstNode;
    if Result = nil then
        Result := BuildNode(nil, RootValue, RootValue, RootValue, RootValue);
    Result.Selected := True;
end;

function TNCDBTreeView.RetriveOwnerValue: Variant;
var
    s: String;
    i, l: Integer;
begin
    if HierarchyType = htOwnerPointers then
        Result := FQuery[OwnerFields]
    else begin
        s := RetriveKeyValue;
        i := KeySegments.FirstEmpty(s, l);
        if i = 0 then
            Result := RootValue
        else begin
            Dec(i);
            Dec(l, KeySegments[i].Length);
            Result := KeySegments.FillRestEmpty(Copy(s, 1, l - 1), i);
        end;
    end;
end;

function TNCDBTreeView.RetriveKeyValue: Variant;
begin
    Result := FQuery[KeyFields];
    if HierarchyType <> htOwnerPointers then
        Result := Var2TextConcat(Result);
end;

function TNCDBTreeView.RetriveDisplayValue: Variant;
begin
    Result := FQuery[DisplayFields];
end;

function TNCDBTreeView.RetriveDataValue: Variant;
begin
    Result := Unassigned;
    if DataFields <> '' then
        Result := FQuery[DataFields];
end;

procedure TNCDBTreeView.NodeSet2Value(const AOwnerValue, AKeyValue: Variant);
var
    OKNode: TTreeNode;
    function NodeSet2Value0(const AOwnerValue, AKeyValue: Variant): TTreeNode;
    var
        ChildNode, Node: TTreeNode;
    begin
        Result := nil;
        Node := nil;
        if not VarCmp(RootValue, AOwnerValue, False, False) then begin
            if QueryData(AOwnerValue, False) then
                Node := NodeSet2Value0(RetriveOwnerValue, AOwnerValue);
        end
        else
            Node := Items.GetFirstNode;
        if Node <> nil then begin
            ExpandNode(Node);
            ChildNode := Node.GetFirstChild;
            while ChildNode <> nil do begin
                if VarCmp(GetNodeKeyValue(ChildNode), AKeyValue, False, False) then begin
                    Result := ChildNode;
                    Exit;
                end;
                ChildNode := Node.GetNextChild(ChildNode);
            end;
        end;
    end;
begin
    Items.BeginUpdate;
    try
        OKNode := NodeSet2Value0(AOwnerValue, AKeyValue);
        if OKNode <> nil then begin
            OKNode.Selected := True;
            MakeNodeVisible(OKNode);
        end;
    finally
        Items.EndUpdate;
        FQuery.Close;
    end;
end;

function TNCDBTreeView.GetActive: Boolean;
begin
    Result := not (csLoading in ComponentState) and HandleAllocated and
        (FLocked = 0) and (IsDefined or Assigned(FBeforeQueryData));
end;

function TNCDBTreeView.IsDefined: Boolean;
begin
    Result := 
        (SQLFrom <> '') and (DisplayFields <> '') and (KeyFields <> '') and
        ((OwnerFields <> '') and (HierarchyType = htOwnerPointers) or
         (KeySegments.Count > 0) and (HierarchyType = htKeySegments));
end;

procedure TNCDBTreeView.SetActive(AValue: Boolean);
begin
    if (csLoading in ComponentState) or (Active <> AValue) then
        if AValue then
            try
                EndUpdate;
            except
                BeginUpdate;
                raise;
            end
        else
            BeginUpdate;
end;

procedure TNCDBTreeView.BeginUpdate;
begin
    Inc(FLocked);
end;

procedure TNCDBTreeView.EndUpdate;
begin
    if FLocked > 0 then begin
        Dec(FLocked);
        if FLocked = 0 then
            RefreshTree;
    end;
end;

function TNCDBTreeView.QueryData(const AValue: Variant; AIsOwnerValue: Boolean): Boolean;
var
    p1, p2, joinFields, pref: String;
    i: Integer;
    wasWhere: Boolean;
    V: Variant;
begin
    Inc(FLocked);
    try
        V := AValue;
        if Assigned(FBeforeQueryData) then
            FBeforeQueryData(Self, V, AIsOwnerValue);
        if not IsDefined then begin
            Result := False;
            Exit;
        end;
        wasWhere := False;
        FQuery.Disconnect;
        FQuery.DatabaseName := DatabaseName;
        with FQuery, FQuery.SQL do
        try
            BeginUpdate;
            Clear;
            Add('select ' + DUFields2SQL(
                DUJoinFields(DUJoinFields(DUJoinFields(DisplayFields, Self.KeyFields),
                OwnerFields), DataFields), '', True, FMacros));
            Add('from ' + SQLFrom);
            if SQLWhere <> '' then
                DUContWhere(SQL, wasWhere, SQLWhere);
            if HierarchyType = htOwnerPointers then begin
                pref := DBTRQMark;
                joinFields := OwnerFields;
                if not AIsOwnerValue then
                    joinFields := Self.KeyFields;
                DUContWhere(SQL, wasWhere, DUParamJoin2SQL(joinFields, '',
                    FMacros, V, False, False, pref, False));
                if AIsOwnerValue and (toHideLeafs in Options) then begin
                    DUContWhere(SQL, wasWhere, '');
                    Add('((' + DUFields2SQL(Self.KeyFields, '', False, FMacros) + ') in');
                    Add('(select ' + DUFields2SQL(OwnerFields, '', False, FMacros));
                    Add('from ' + SQLFrom);
                    if SQLWhere <> '' then begin
                        wasWhere := False;
                        DUContWhere(SQL, wasWhere, SQLWhere);
                    end;
                    Add('))');
                end;
            end
            else begin
                pref := '';
                joinFields := '';
                i := 1;
                while i <= Length(Self.KeyFields) do begin
                    if joinFields <> '' then
                        joinFields := joinFields + ' || ';
                    joinFields := joinFields + DUFields2SQL(ExtractFieldName(Self.KeyFields, i),
                        '', False, FMacros);
                end;
                p1 := Var2TextConcat(V);
                if AIsOwnerValue then begin
                    i := KeySegments.SegQParams(p1, p1, p2);
                    if (i = KeySegments.Count) or (toHideLeafs in Options) and
                       (i = KeySegments.Count - 1) then begin
                        Result := False;
                        Exit;
                    end;
                    DUContWhere(SQL, wasWhere, joinFields + ' like :' + DBTRQMark + '0_');
                    DUContWhere(SQL, wasWhere, joinFields + ' not like :' + DBTRQMark + '1_');
                end
                else
                    DUContWhere(SQL, wasWhere, joinFields + ' = :' + DBTRQMark + '0_');
            end;
            if SQLOrderBy <> '' then
                Add('order by ' + DUFields2SQL(SQLOrderBy, '', False, FMacros));
            ViewStrings(toSQLPreview in Options, SQL, Self.Name + '.FQuery.SQL');
        finally
            EndUpdate;
        end;
        with FQuery do begin
            if HierarchyType = htOwnerPointers then
                Params.SetValuesExt(joinFields, V, pref)
            else begin
                ParamByName(DBTRQMark + '0_').AsString := p1;
                if AIsOwnerValue then
                    ParamByName(DBTRQMark + '1_').AsString := p2;
            end;
            Params.AssignValues(SQLParams);
            Open;
            Result := not FQuery.IsEmpty;
        end;
        if Assigned(FAfterQueryData) then
            FAfterQueryData(Self, V, AIsOwnerValue);
    finally
        Dec(FLocked);
    end;
end;

function TNCDBTreeView.CanExpand(Node: TTreeNode): Boolean;
begin
    Result := False;
    if FSkipExpand or not Active then
        Node.HasChildren := True
    else begin
        Result := inherited CanExpand(Node);
        if Result and
           ((toMaxFreshed in Options) or (Node.GetFirstChild = nil)) and
           not VarIsEmpty(GetNodeKeyValue(Node)) then begin
            Items.BeginUpdate;
            try
                Node.DeleteChildren;
                Node.HasChildren := QueryData(GetNodeKeyValue(Node), True);
                if Node.HasChildren then begin
                    while not FQuery.EOF do begin
                        BuildNode(Node, RetriveOwnerValue, RetriveKeyValue,
                            RetriveDisplayValue, RetriveDataValue);
                        FQuery.Next;
                    end;
                end;
            finally
                FQuery.Close;
                Items.EndUpdate;
            end;
        end;
    end;
end;

procedure TNCDBTreeView.Collapse(Node: TTreeNode);
begin
    inherited Collapse(Node);
    if toMinResources in Options then
    try
        Items.BeginUpdate;
        Node.DeleteChildren;
        Node.HasChildren := not GetNodeIsItem(Node);
    finally
        Items.EndUpdate;
    end;
end;

procedure TNCDBTreeView.Loaded;
begin
    FSkipExpand := True;
    try
        inherited Loaded;
    finally
        FSkipExpand := False;
    end;
    RefreshTree;
end;

procedure TNCDBTreeView.ExpandNode(ANode: TTreeNode);
begin
    if ANode.Parent = nil then
        ANode.Expanded := True
    else if not ANode.Expanded and ANode.HasChildren and CanExpand(ANode) then begin
        ANode.Expanded := True;
        Expand(ANode);
    end;
end;

procedure TNCDBTreeView.CollapseNode(ANode: TTreeNode);
begin
    if ANode.Parent = nil then
        ANode.Expanded := False
    else if ANode.Expanded then begin
        ANode.Expanded := False;
        Collapse(ANode);
    end;
end;

procedure TNCDBTreeView.RefreshCurrent;
begin
    if Selected <> nil then
        RefreshChilds(Selected.Parent);
end;

procedure TNCDBTreeView.RefreshTree;
begin
    RefreshChilds(nil);
end;

procedure TNCDBTreeView.RefreshChilds(ANode: TTreeNode);
var
    prevOptions: TNCDBTreeOptions;
    lastKeyVal: Variant;
    skipGotoKey: Boolean;
begin
    if Active then
    try
        lastKeyVal := KeyValue;
        skipGotoKey := False;
        Items.BeginUpdate;
        if (Items.Count = 0) or (ANode = nil) or (ANode = Items[0]) then begin
            Items.Clear;
            ANode := GoToRoot;
            skipGotoKey := True;
        end
        else
            try
                prevOptions := FOptions;
                FOptions := FOptions - [toMinResources];
                CollapseNode(ANode);
            finally
                FOptions := prevOptions;
            end;
        ExpandNode(ANode);
        if not VarIsEmpty(lastKeyVal) and not skipGotoKey then
            KeyValue := lastKeyVal;
    finally
        Items.EndUpdate;
    end;
end;

procedure TNCDBTreeView.ClearTree;
begin
    Items.BeginUpdate;
    BeginUpdate;
    try
        Items.Clear;
        FQuery.Disconnect;
    finally
        Items.EndUpdate;
        EndUpdate;
    end;
end;

function TNCDBTreeView.CanChange(Node: TTreeNode): Boolean;
begin
    Result := inherited CanChange(Node) and (CanSyncDataSource <> -1); 
end;

procedure TNCDBTreeView.Change(ANode: TTreeNode);
begin
    if Showing and (FLocked = 0) {and (Assigned(OnChange) or (CanSyncDataSource = 1))} then
        if (toDeferedChange in Options) then begin
            if FDeferedTimer = nil then begin
                FDeferedTimer := TTimer.Create(Self);
                FDeferedTimer.Interval := DeferedTimerResolution;
                FDeferedTimer.OnTimer := DeferedTimerExpired;
            end;
            FDeferedStart := GetTickCount;
            FDeferedTimer.Enabled := True;
        end
        else
            DeferedChange(ANode);
end;

procedure TNCDBTreeView.DeferedTimerExpired(ASender: TObject);
begin
    if (FLocked = 0) and (GetTickCount - FDeferedStart >= FDeferedDelay) {and
       (Assigned(OnChange) or (CanSyncDataSource = 1))} then begin
        FDeferedTimer.Enabled := False;
        DeferedChange(Selected);
    end;
end;

procedure TNCDBTreeView.DeferedChange(ANode: TTreeNode);
begin
    Inc(FLocked);
    try
        if CanSyncDataSource = 1 then
            DoSyncDataSource;
        inherited Change(ANode);
    finally
           Dec(FLocked);
    end;
end;

procedure TNCDBTreeView.SetOptions(AValue: TNCDBTreeOptions);
begin
    if AValue <> FOptions then begin
        if (toDefaultImages in AValue) <> (toDefaultImages in FOptions) then begin
            if toDefaultImages in FOptions then begin
                inherited Images := nil;
                DestroyDefImageList;
            end
            else if toDefaultImages in AValue then begin
                inherited Images := CreateDefImageList;
                AValue := AValue + [toDefaultImageIndexes];
            end;
        end;
        FOptions := AValue;
    end;
end;

function TNCDBTreeView.GetImages: TCustomImageList;
begin
    Result := nil;
    if not (toDefaultImages in FOptions) then
        Result := inherited Images;
end;

procedure TNCDBTreeView.SetImages(AValue: TCustomImageList);
begin
    Options := Options - [toDefaultImages];
    inherited Images := AValue;
end;

procedure TNCDBTreeView.MakeNodeVisible(ANode: TTreeNode);
var
    R, R1, R2: TRect;
begin
    if ANode <> nil then begin
        R1 := ANode.DisplayRect(False);
        R2 := ClientRect;
        IntersectRect(R, R1, R2);
        if IsRectEmpty(R1) or not EqualRect(R, R1) then
            TopItem := ANode;
    end;
end;

end.
