unit jsontreeview;

// ***********************************************************************
//
//   JSON TreeView VCL Component
//
//   pawel.glowacki@embarcadero.com
//   July 2010
//
// ***********************************************************************

interface

uses
  Classes, ComCtrls, DBXJSON, jsondoc;

type
  TJSONTreeView = class(TTreeView)
  private
    FJSONDocument: TJSONDocument;
    FVisibleChildrenCounts: boolean;
    FVisibleByteSizes: boolean;
    procedure SetJSONDocument(const Value: TJSONDocument);
    procedure SetVisibleChildrenCounts(const Value: boolean);
    procedure SetVisibleByteSizes(const Value: boolean);
    procedure ProcessElement(currNode: TTreeNode; arr: TJSONArray;
      aIndex: integer);
    procedure ProcessPair(currNode: TTreeNode; obj: TJSONObject;
      aIndex: integer);
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ClearAll;
    procedure LoadJson;
  published
    property JSONDocument: TJSONDocument
      read FJSONDocument write SetJSONDocument;
    property VisibleChildrenCounts: boolean
      read FVisibleChildrenCounts write SetVisibleChildrenCounts;
    property VisibleByteSizes: boolean
      read FVisibleByteSizes write SetVisibleByteSizes;
  end;

implementation

uses
  SysUtils;

{ TJSONTreeView }

procedure TJSONTreeView.ClearAll;
begin
  Items.Clear;
end;

constructor TJSONTreeView.Create(AOwner: TComponent);
begin
  inherited;
  FVisibleChildrenCounts := true;
  FVisibleByteSizes := false;
end;

procedure TJSONTreeView.LoadJson;
var v: TJSONValue; currNode: TTreeNode; i, aCount: integer; s: string;
begin
  ClearAll;

  if (JSONDocument <> nil) and JSONDocument.IsActive then
  begin
    v := JSONDocument.RootValue;
    Items.Clear;

    if TJSONDocument.IsSimpleJsonValue(v) then
      Items.AddChild(nil, TJSONDocument.UnQuote(v.Value))

    else
    if v is TJSONObject then
    begin
      aCount := TJSONObject(v).Size;
      s := '{}';
      if VisibleChildrenCounts then
        s := s + ' (' + IntToStr(aCount) + ')';
      if VisibleByteSizes then
        s := s + ' (size: ' + IntToStr(v.EstimatedByteSize) + ' bytes)';
      currNode := Items.AddChild(nil, s);
      for i := 0 to aCount - 1 do
        ProcessPair(currNode, TJSONObject(v), i)
    end

    else
    if v is TJSONArray then
    begin
      aCount := TJSONArray(v).Size;
      s := '[]';
      if VisibleChildrenCounts then
        s := s + ' (' + IntToStr(aCount) + ')';
      if VisibleByteSizes then
        s := s + ' (size: ' + IntToStr(v.EstimatedByteSize) + ' bytes)';
      currNode := Items.AddChild(nil, s);
      for i := 0 to aCount - 1 do
        ProcessElement(currNode, TJSONArray(v), i)
    end

    else
      raise EUnknownJsonValueDescendant.Create;

    FullExpand;
  end;
end;

procedure TJSONTreeView.ProcessPair(currNode: TTreeNode; obj: TJSONObject; aIndex: integer);
var p: TJSONPair; s: string; n: TTreeNode; i, aCount: integer;
begin
  p := obj.Get(aIndex);

  s := TJSONDocument.UnQuote(p.JsonString.ToString) + ' : ';

  if TJSONDocument.IsSimpleJsonValue(p.JsonValue) then
  begin
    Items.AddChild(currNode, s + p.JsonValue.ToString);
    exit;
  end;

  if p.JsonValue is TJSONObject then
  begin
    aCount := TJSONObject(p.JsonValue).Size;
    s := s + ' {}';
    if VisibleChildrenCounts then
      s := s + ' (' + IntToStr(aCount) + ')';
    if VisibleByteSizes then
        s := s + ' (size: ' + IntToStr(p.EstimatedByteSize) + ' bytes)';
    n := Items.AddChild(currNode, s);
    for i := 0 to aCount - 1 do
      ProcessPair(n, TJSONObject(p.JsonValue), i);
  end

  else if p.JsonValue is TJSONArray then
  begin
    aCount := TJSONArray(p.JsonValue).Size;
    s := s + ' []';
    if VisibleChildrenCounts then
      s := s + ' (' + IntToStr(aCount) + ')';
    if VisibleByteSizes then
        s := s + ' (size: ' + IntToStr(p.EstimatedByteSize) + ' bytes)';
    n := Items.AddChild(currNode, s);
    for i := 0 to aCount - 1 do
      ProcessElement(n, TJSONArray(p.JsonValue), i);
  end
  else
    raise EUnknownJsonValueDescendant.Create;
end;

procedure TJSONTreeView.ProcessElement(currNode: TTreeNode; arr: TJSONArray; aIndex: integer);
var v: TJSONValue; s: string; n: TTreeNode; i, aCount: integer;
begin
  v := arr.Get(aIndex);
  s := '[' + IntToStr(aIndex) + '] ';

  if TJSONDocument.IsSimpleJsonValue(v) then
  begin
    Items.AddChild(currNode, s + v.ToString);
    exit;
  end;

  if v is TJSONObject then
  begin
    aCount := TJSONObject(v).Size;
    s := s + ' {}';
    if VisibleChildrenCounts then
      s := s + ' (' + IntToStr(aCount) + ')';
    if VisibleByteSizes then
        s := s + ' (size: ' + IntToStr(v.EstimatedByteSize) + ' bytes)';
    n := Items.AddChild(currNode, s);
    for i := 0 to aCount - 1 do
      ProcessPair(n, TJSONObject(v), i);
  end

  else if v is TJSONArray then
  begin
    aCount := TJSONArray(v).Size;
    s := s + ' []';
    n := Items.AddChild(currNode, s);
    if VisibleChildrenCounts then
      s := s + ' (' + IntToStr(aCount) + ')';
    if VisibleByteSizes then
        s := s + ' (size: ' + IntToStr(v.EstimatedByteSize) + ' bytes)';
    for i := 0 to aCount - 1 do
      ProcessElement(n, TJSONArray(v), i);
  end
  else
    raise EUnknownJsonValueDescendant.Create;

end;

procedure TJSONTreeView.SetJSONDocument(const Value: TJSONDocument);
begin
  if FJSONDocument <> Value then
  begin
    FJSONDocument := Value;
    ClearAll;
    if FJSONDocument <> nil then
    begin
      if FJSONDocument.IsActive then
        LoadJson;
    end;
  end;
end;

procedure TJSONTreeView.SetVisibleByteSizes(const Value: boolean);
begin
  if FVisibleByteSizes <> Value then
  begin
    FVisibleByteSizes := Value;
    LoadJson;
  end;
end;

procedure TJSONTreeView.SetVisibleChildrenCounts(const Value: boolean);
begin
  if FVisibleChildrenCounts <> Value then
  begin
    FVisibleChildrenCounts := Value;
    LoadJson;
  end;
end;

procedure TJSONTreeView.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;

  if Operation = opRemove then
    if FJSONDocument <> nil then
      if AComponent = FJSONDocument then
      begin
        FJSONDocument := nil;
        ClearAll;
      end;
end;

end.
