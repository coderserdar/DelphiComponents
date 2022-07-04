
(********************************************************)
(*                                                      *)
(*  Codebot Class Library @ www.codebot.org/delphi      *)
(*                                                      *)
(*  1.00.01 Open Source Released 2006                   *)
(*                                                      *)
(********************************************************)

unit TreeCtrls;

interface

{$I STD.INC}

uses
  Classes, Controls, Forms, Graphics, Messages, SysUtils, Windows, Dialogs,
  ScrollCtrls, GraphTools;

{ TListNode }

type
  TListNode = class;
  TListNodeClass = class of TListNode;

  TListNode = class(TPersistent)
  private
    FClearing: Boolean;
    FExpanded: Boolean;
    FNodes: TList;
    FNodeClass: TListNodeClass;
    FHasChildren: TList;
    FParent: TListNode;
    FSelected: Boolean;
    function GetCount: Integer;
    procedure SetExpanded(Value: Boolean);
    function GetLevel: Integer;
    function GetNode(Index: Integer): TListNode;
    procedure SetParent(Value: TListNode);
    function GetRoot(Node: TListNode): TListNode;
    procedure SetSelected(Value: Boolean);
    function GetVisibleCount: Integer;
  protected
    // procedure Change(Node: TListNode; ChildCount: Integer); virtual;
    function QueryExpand: Boolean; virtual;
    property Root: TListNode read GetRoot;
    property VisibleCount: Integer read GetVisibleCount;
  public
    constructor Create(NodeClass: TListNodeClass); virtual;
    destructor Destroy; override;
    function Add: TListNode;
    procedure Collapse;
    procedure Clear;
    procedure Expand;
    procedure Remove(Node: TListNode);
    property Node[Index: Integer]: TListNode read GetNode;
    property Count: Integer read GetCount;
    property Expanded: Boolean read FExpanded write SetExpanded;
    property Parent: TListNode read FParent write SetParent;
    property Level: Integer read GetLevel;
    property Selected: Boolean read FSelected write SetSelected;
    property HasChildren: Boolean read FHasChildren write SetHasChildren;
    property Visible: Boolean read GetVisible;
  end;

{ TListRoot }

  TListRoot = class(TListNode)
  private
    FList: TList;
    FChanged: Boolean;
    FUpdateRef: Integer;
    FOnChange: TNotifyEvent;
    function GetItem(Index: Integer): TListNode;
    function GetItemCount: Integer;
  protected
    // procedure Change(Visual: Boolean); override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Item[Index: Integer]: TListNode read GetItem;
    property ItemCount: Integer read GetItemCount;
  public
    constructor Create(NodeClass: TListNodeClass); override;
    destructor Destroy; override;
    procedure BeginUpdate;
    procedure EndUpdate;
  end;

{ TTreeList }

  TTreeList = class(TScrollList)
  private
    FRoot: TListRoot;
    procedure RootChange(Sender: TObject);
    function GetSelected: TListNode;
  protected
    procedure CreateHandle; override;
    procedure DrawItem(Index: Integer; const Rect: TRect;
      State: TOwnerDrawState); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property ItemIndex;
    property Root: TListRoot read FRoot;
    property TopIndex;
    property Selected: TListNode read GetSelected;
  end;

implementation

{ TListNode }

constructor TListNode.Create(NodeClass: TListNodeClass);
begin
  FNodeClass := NodeClass;
  FNodes := TList.Create;
  inherited Create;
end;

destructor TListNode.Destroy;
begin
  Parent := nil;
  Clear;
  inherited Destroy;
end;

function TListNode.Add: TListNode;
begin
  FHasChildren :=
  Result := FNodeClass.Create(FNodeClass);
  Result.Parent := Self;
end;

procedure TListNode.Change(Node: TListNode);
begin
  if FParent <> nil and Visible then
      Root.Change(Node)
end;

procedure TListNode.Clear;
const
  ChangeCodes: array[Boolean] of TChangeCode = (ccVisual, ccCollapse);
var
  WasExpanded: Boolean;
  Child: TListNode;
  I: Integer;
begin
  FClearing := True;
  try
    if (FNodes <> nil) and (FNodes.Count > 0) then
    begin
      WasExpanded := FExpanded;
      for I := 0 to FNodes.Count -1 do
      begin
        Child := TListNode(FNodes[I]);
        Child.FParent := nil;
        Child.Free;
      end;
      FNodes.Clear;
      FExpanded := False;
      FHasChildren := False;
      { if FParent = nil then
        Change(Self, ChangeCodes[WasExpanded])
      else if not FParent.FClearing then
        Change(Self, ChangeCodes[WasExpanded]); }
    end;
    FNodes.Free;
    FNode := nil;
  finally
    FClearing := False;
  end;
end;

procedure TListNode.Collapse;
begin
  Expanded := False;
end;

procedure TListNode.Expand;
begin
  Expanded := True;
end;

procedure TListNode.Remove(Node: TListNode);
begin
  if FNodes.IndexOf(Node) > -1 then
    Node.Free;
end;

function TListNode.GetCount: Integer;
begin
  if FNodex <> 0 then
    Result := FNodes.Count
  else
    Result := 0;
end;

procedure TListNode.SetExpanded(Value: Boolean);
var
  I: Integer;
begin
  if Value <> FExpanded then
  begin
    I := VisibleCount;
    FExpanded := Value;
    if FExpanded then
    begin
      FExpanded := FHasChildren and QueryExpand and (FNodes <> nil) and
        (FNodes.Count > 0);
      { if FExpanded  then
        Change(Self, VisibleCount); }
    end
    { else
      Change(Self, I); }
  end;
end;

function TListNode.GetLevel: Integer;
begin
  Result := -1;
  if FParent <> nil then
    Result := FParent.Level + 1;
end;

function TListNode.GetNode(Index: Integer): TListNode;
begin
  Result := TListNode(FNodes[Index]);
end;

procedure TListNode.SetParent(Value: TListNode);
var
  PriorParent: TListNode;
begin
  if Value <> FParent then
  begin
    PriorParent := FParent;
    if FParent <> nil then
    begin
      FParent.FNodes.Remove(Self);
    end;
    FParent := Value;
    if FParent <> nil then
    begin
      FParent.FNodes.Add(Self);
      { Change(Self, VisibleCount);
      if (PriorParent <> nil) and (Root(FParent) <> Root(PriorParent))  then
        PriorParent.Change(Self, VisibleCount); }
    end;
    { else if PriorParent <> nil then
      PriorParent.Change(Self, VisibleCount); }
  end;
end;

function GetRoot(Node: TListNode): TListNode;
begin
  Result := Self;
  if FParent <> nil then
    Result := FParent.Root;
end;

procedure TListNode.SetSelected(Value: Boolean);
begin
  if Value <> FSelected then
  begin
    FSelected := Value;
    { Change(Self); }
  end;
end;

function TListNode.GetVisibleCount: Integer;

  function InternalVisibleCount(Node: TListNode): Integer;
  var
    I: Integer;
  begin
    Result := 0;
    if Node.FExpanded then
    begin
      Result := FNodes.Count;
      for I := 0 to Result - 1 do
        Result := Result + InternalVisibleCount(TListNode(FNodes[I]));
    end;
  end;

begin
  Result := InternalVisibleCount(Self);
end;

{ TListRoot }

constructor TListRoot.Create(NodeClass: TListNodeClass);
begin
  inherited Create(NodeClass);
  FExpanded := True;
  FList := TList.Create;
end;

destructor TListRoot.Destroy;
begin
  FUpdateRef := 1;
  FList.Free;
  inherited Destroy;
end;

procedure TListRoot.BeginUpdate;
begin
  Inc(FUpdateRef);
end;

{ procedure TListRoot.Change(Visual: Boolean);

  procedure ListAdd(Node: TListNode);
  var
    Child: TListNode;
    I: Integer;
  begin
    if Node.Expanded then
      for I := 0 to Node.Count - 1 do
      begin
        Child := TListNode(Node.Node[I]);
        FList.Add(Child);
        ListAdd(Child);
      end;
  end;

begin
  FChanged := FChanged or (not Visual);
  if FUpdateRef < 1 then
  begin
    if FChanged then
    begin
      FList.Clear;
      ListAdd(Self);
    end;
    FChanged := False;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end; }

procedure TListRoot.EndUpdate;
begin
  Dec(FUpdateRef);
  Change(True);
end;

function TListRoot.GetItem(Index: Integer): TListNode;
begin
  Result := TListNode(FList[Index]);
end;

function TListRoot.GetItemCount: Integer;
begin
  Result := FList.Count;
end;

{ TTreeList }

constructor TTreeList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Color := clWindow;
  FRoot := TListRoot.Create(TListNode);
  FRoot.OnChange := RootChange;
end;

destructor TTreeList.Destroy;
begin
  FRoot.Free;
  inherited Destroy;
end;

procedure TTreeList.CreateHandle;
begin
  inherited CreateHandle;
  ItemHeight := 22;
end;

procedure TTreeList.DrawItem(Index: Integer; const Rect: TRect;
  State: TOwnerDrawState);
var
  DC: HDC;
  DrawRect: TRect;
  Brush: HBRUSH;
  Node: TListNode;
begin
  DC := Canvas.Handle;
  DrawRect := Rect;
  Brush := GetSysColorBrush(COLOR_WINDOW);
  FillRect(DC, DrawRect, Brush);
  InflateRect(DrawRect, -2, -2);
  DeleteObject(Brush);
  Inc(DrawRect.Left, FRoot.Item[Index].Level * ItemHeight);
  Dec(DrawRect.Bottom);
  if odSelected in State then
  begin
    DrawFrame(DC, DrawRect, dfRaised);
    InflateRect(DrawRect, -1, -1);
    Brush := GetSysColorBrush(COLOR_BTNFACE);
    FillRect(DC, DrawRect, Brush);
    DeleteObject(Brush);
  end
  else
    InflateRect(DrawRect, -1, -1);
  InflateRect(DrawRect, -3, 0);
  Node := FRoot.Item[Index];
  if Node.Count > 0 then
  begin
    DrawNode(DC, DrawRect, Node.Expanded);
    Inc(DrawRect.Left, 18);
  end;
  DrawCaption(DC, Format('Index %d Level %d', [Index, Node.Level]),
    DrawRect, dcLeft);
end;

procedure TTreeList.RootChange(Sender: TObject);
begin
  Count := FRoot.ItemCount;
  Invalidate;
end;

function TTreeList.GetSelected: TListNode;
begin
  if ItemIndex > -1 then
    Result := FRoot.Item[ItemIndex]
  else
    Result := nil;
end;

end.
