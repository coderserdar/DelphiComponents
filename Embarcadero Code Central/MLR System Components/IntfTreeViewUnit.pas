unit IntfTreeViewUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls;

type
  TMLRCustomIntfTreeView = class(TTreeView)
  private
    function GetNodeData(Node: TTreeNode): Pointer;
    procedure SetNodeData(Node: TTreeNode; const Value: Pointer);
    function GetInterface(Node: TTreeNode): IUnknown;
    procedure SetInterface(Node: TTreeNode; const Value: IUnknown);
    { Private declarations }
  protected
    { Protected declarations }
    procedure Delete(Node: TTreeNode); override;
    property Interfaces[Node: TTreeNode]: IUnknown read GetInterface write SetInterface;
    property NodeData[Node: TTreeNode]: Pointer read GetNodeData write SetNodeData;
  public
    { Public declarations }
  published
    { Published declarations }
  end;

  TMLRIntfTreeView = class(TMLRCustomIntfTreeView)
  public
    property Interfaces[Node: TTreeNode]: IUnknown read GetInterface write SetInterface;
    property NodeData[Node: TTreeNode]: Pointer read GetNodeData write SetNodeData;
  end;

  TMLRCustomObjTreeView = class(TMLRCustomIntfTreeView)
  private
    FAutoFree: Boolean;
    function GetObject(Node: TTreeNode): TObject;
    procedure SetObject(Node: TTreeNode; const Value: TObject);
  protected
    property AutoFree: Boolean read FAutoFree write FAutoFree default True;
    procedure Delete(Node: TTreeNode); override;
    property Objects[Node: TTreeNode]: TObject read GetObject write SetObject;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TMLRObjTreeView = class(TMLRCustomObjTreeView)
  public
    property Interfaces[Node: TTreeNode]: IUnknown read GetInterface write SetInterface;
    property Objects[Node: TTreeNode]: TObject read GetObject write SetObject;
  end;

procedure Register;

implementation

type
  PNodeData = ^TNodeData;
  TNodeData = record
    Data: Pointer;
    Intf: IUnknown;
  end;

procedure Register;
begin
  RegisterComponents('MLR Sysco', [TMLRCustomIntfTreeView, TMLRObjTreeView]);
end;

{ TMLRCustomIntfTreeView }

procedure TMLRCustomIntfTreeView.Delete(Node: TTreeNode);
var P: PNodeData;
begin
  if Assigned(Node.Data) then begin
    P := PNodeData(Node.Data);
    P^.Intf := nil;
    Dispose(P);
  end;
  inherited Delete(Node);
end;

function TMLRCustomIntfTreeView.GetInterface(Node: TTreeNode): IUnknown;
begin
  if Assigned(Node.Data) then
    Result := PNodeData(Node.Data)^.Intf
  else
    Result := nil;
end;

function TMLRCustomIntfTreeView.GetNodeData(Node: TTreeNode): Pointer;
begin
  if Assigned(Node.Data) then
    Result := PNodeData(Node.Data)^.Data
  else
    Result := nil;
end;

procedure TMLRCustomIntfTreeView.SetInterface(Node: TTreeNode;
  const Value: IUnknown);
var P: PNodeData;
begin
  if not Assigned(Node.Data) then begin
    New(P);
    FillChar(P^, sizeof(P^), 0);
    Node.Data := P;
  end else
    P := PNodeData(Node.Data);
  P^.Intf := Value;
end;

procedure TMLRCustomIntfTreeView.SetNodeData(Node: TTreeNode;
  const Value: Pointer);
var P: PNodeData;
begin
  if not Assigned(Node.Data) then begin
    New(P);
    FillChar(P^, sizeof(P^), 0);
    Node.Data := P;
  end else
    P := PNodeData(Node.Data);
  P^.Data := Value;
end;

{ TMLRCustomObjTreeView }

constructor TMLRCustomObjTreeView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAutoFree := True;
end;

procedure TMLRCustomObjTreeView.Delete(Node: TTreeNode);
begin
  if FAutoFree then
    Objects[Node].Free;
  inherited Delete(Node);
end;

function TMLRCustomObjTreeView.GetObject(Node: TTreeNode): TObject;
begin
  Result := TObject(NodeData[Node]);
end;

procedure TMLRCustomObjTreeView.SetObject(Node: TTreeNode;
  const Value: TObject);
begin
  NodeData[Node] := Value;
end;

end.
