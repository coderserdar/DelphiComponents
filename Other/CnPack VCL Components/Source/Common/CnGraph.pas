{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     �й����Լ��Ŀ���Դ�������������                         }
{                   (C)Copyright 2001-2020 CnPack ������                       }
{                   ------------------------------------                       }
{                                                                              }
{            ���������ǿ�Դ��������������������� CnPack �ķ���Э������        }
{        �ĺ����·�����һ����                                                }
{                                                                              }
{            ������һ��������Ŀ����ϣ�������ã���û���κε���������û��        }
{        �ʺ��ض�Ŀ�Ķ������ĵ���������ϸ���������� CnPack ����Э�顣        }
{                                                                              }
{            ��Ӧ���Ѿ��Ϳ�����һ���յ�һ�� CnPack ����Э��ĸ��������        }
{        ��û�У��ɷ������ǵ���վ��                                            }
{                                                                              }
{            ��վ��ַ��http://www.cnpack.org                                   }
{            �����ʼ���master@cnpack.org                                       }
{                                                                              }
{******************************************************************************}

unit CnGraph;
{* |<PRE>
================================================================================
* ������ƣ�CnPack ������Ԫ
* ��Ԫ���ƣ�ʵ��ͼ�ĵ�Ԫ
* ��Ԫ���ߣ���Х (liuxiao@cnpack.org)
* ��    ע��
* ����ƽ̨��Win 7 + Delphi 5.01
* ���ݲ��ԣ�PWin9X/2000/XP + Delphi 5/6/7 + C++Builder 5/6
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2019.03.19 V1.0
*               ������Ԫ��ʵ�ֹ���
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, Contnrs;

type
  TCnGraph = class;

  ECnGraphException = class(Exception);

  TCnMatrix = array of array of Integer;

  TCnAdjacencyMatrix = TCnMatrix;
  {* �ڽӾ��󣬶����붥��}

  TCnIncidenceMatrix = TCnMatrix;
  {* �������󣬶������}

  TCnVertex = class(TObject)
  {* ����ʵ����}
  private
    FOutNeighbours: TObjectList;
    FWeights: TList;
    FInNeighbours: TObjectList;
    FText: string;
    FOwner: TCnGraph;
    FData: TObject;
    FVisited: Boolean;
    function GetOutNeighbourCount: Integer;
    function GetInNeighbourCount: Integer;
    function GetInNeighbour(Index: Integer): TCnVertex;
    function GetOutNeighbour(Index: Integer): TCnVertex;
    function GetWeight(Index: Integer): Integer;
  protected
    procedure AddInNeighbour(PrevRef: TCnVertex);
    {* �������һ�����㵽 InNeighbour}
    procedure RemoveInNeighbour(PrevRef: TCnVertex);
    {* ������ InNeighbour ��ɾ��һ������}
    property Visited: Boolean read FVisited write FVisited;
    {* �Ƿ񱻷��ʹ��ı�ǣ�����ͼ�ı���}
  public
    constructor Create(AOwner: TCnGraph); virtual;
    destructor Destroy; override;

    procedure AddOutNeighbour(NextRef: TCnVertex; Weight: Integer = 1);
    {* ��ӳ������ڵ���Ȩ�أ��ڲ���� Self ��ӵ� NextRef �� InNeighbour ��}
    procedure RemoveOutNeighbour(NextRef: TCnVertex);
    {* ɾ���������ڵ�}
    procedure ClearNeighbours;
    {* ������г������ڵĶ���}

    property Owner: TCnGraph read FOwner;
    {* ������ͼ}

    property Text: string read FText write FText;
    {* �洢�ı�����}
    property Data: TObject read FData write FData;
    {* �洢���ݶ�������}

    property Weight[Index: Integer]: Integer read GetWeight;
    {* �Դ˶���Ϊ���ıߵ�Ȩ��}
    property OutNeighbour[Index: Integer]: TCnVertex read GetOutNeighbour;
    {* �Դ˶���Ϊ�������ڶ���}
    property OutNeighbourCount: Integer read GetOutNeighbourCount;
    {* �Դ˶���Ϊ���ıߵ�������Ҳ������}

    property InNeighbour[Index: Integer]: TCnVertex read GetInNeighbour;
    {* �Դ˶���Ϊ�յ�����ڶ���}
    property InNeighbourCount: Integer read GetInNeighbourCount;
    {* �Դ˶���Ϊ�յ�ıߵ�������Ҳ�����}
  end;

  TCnGraphTravelEvent = procedure(Vertex: TCnVertex) of object;
  {* ����ͼʱ��������ĳ������¼���Vertex �Ƕ���}

  TCnGraph = class(TObject)
  {* ͼʵ����}
  private
    FVertexes: TObjectList;
    FDirected: Boolean;
    FEdgeCount: Integer;
    FOnDepthFirstTravelVertex: TCnGraphTravelEvent;
    FOnWidthFirstTravelVertex: TCnGraphTravelEvent;
    function GetVertexCount: Integer;
    function GetVertex(Index: Integer): TCnVertex;
  protected
    procedure DoDepthFirstTravel(Vertex: TCnVertex);
    procedure DoWidthFirstTravel(Vertex: TCnVertex);
  public
    constructor Create(ADirected: Boolean = True); virtual;
    {* ���캯����Ĭ��������ͼ}
    destructor Destroy; override;
    {* ��������}

    function HasVertex(Vertex: TCnVertex): Boolean;
    {* ͼ���Ƿ���ָ������}

    function AddVertex(const Text: string): TCnVertex;
    {* ��ӹ�������}
    function FindVertex(const Text: string): TCnVertex;
    {* ���� Text Ϊָ�����ݵĶ��㣬�����ظ���ֻ���ص�һ��}

    function AddEdge(Vertex1, Vertex2: TCnVertex; Weight: Integer = 1): Boolean;
    {* ���һ���ߡ����������ͼ������� Vertex1 ָ�� Vertex2 �ıߣ�
      ����ͼ��� Vertex1 ָ�� Vertex2 ���� Vertex2 ָ�� Vertex1 �� }
    function AddVertexesEdge(const Text1, Text2: string; Weight: Integer = 1): Boolean;
    {* �� Text ���һ���ߡ�����Ѵ��� Text ��ͬ�Ķ������Զ���Ϊ׼����������Ӷ�������ӱ�}
    function RemoveEdge(Vertex1, Vertex2: TCnVertex): Boolean;
    {* ɾ��һ���ߡ����������ͼ����ɾ�� Vertex1 ָ�� Vertex2 �ıߣ�
      ����ͼɾ�� Vertex1 ָ�� Vertex2 ���� Vertex2 ָ�� Vertex1 ��}
    function RemoveVertex(Vertex: TCnVertex): Boolean;
    {* ɾ��һ�������Լ��������������б߲� Free �������}

    function GetVertexOutDegree(Vertex: TCnVertex): Integer;
    {* ����ͼ����µõ�ĳ����ĳ���}
    function GetVertexInDegree(Vertex: TCnVertex): Integer;
    {* ����ͼ����µõ�ĳ��������}
    function GetVertexDegree(Vertex: TCnVertex): Integer;
    {* �õ�ĳ����Ķȣ�����ͼ�����Ҳ�������֮��}

    procedure ClearVertexes;
    {* ������ж��㣬˳��Ҳ����������б�}
    procedure ClearEdges;
    {* ������б�}
    procedure ClearVisited;
    {* ������ʱ��}

    function DumpToAdjacencyMatrix: TCnAdjacencyMatrix;
    {* ������������ڽӾ���}
    function DumpToIncidenceMatrix: TCnIncidenceMatrix;
    {* �������������������}
    procedure DepthFirstTravel(Vertex: TCnVertex);
    {* ��ĳ�����������������ȱ��� }
    procedure WidthFirstTravel(Vertex: TCnVertex);
    {* ��ĳ����������й�����ȱ��� }

    property Directed: Boolean read FDirected;
    {* �Ƿ�������ͼ}
    property EdgeCount: Integer read FEdgeCount;
    {* ������}
    property Vertex[Index: Integer]: TCnVertex read GetVertex;
    {* �����б�}
    property VertexCount: Integer read GetVertexCount;
    {* ��������}

    property OnDepthFirstTravelVertex: TCnGraphTravelEvent
      read FOnDepthFirstTravelVertex write FOnDepthFirstTravelVertex;
    {* ������ȱ���ʱ������һ������ʱ�Ĵ����¼���Sender �Ǵ˶��� }
    property OnWidthFirstTravelVertex: TCnGraphTravelEvent
      read FOnWidthFirstTravelVertex write FOnWidthFirstTravelVertex;
    {* ������ȱ���ʱ������һ������ʱ�Ĵ����¼���Sender �Ǵ˶��� }
  end;

procedure CnMatrixToStrings(Matrix: TCnMatrix; List: TStrings);
{* ������ת��Ϊ�ַ����б�������ʾ}

implementation

procedure CnMatrixToStrings(Matrix: TCnMatrix; List: TStrings);
var
  I, J: Integer;
  S: string;
begin
  if (Matrix <> nil) and (List <> nil) then
  begin
    List.Clear;
    for I := Low(Matrix) to High(Matrix) do
    begin
      S := '';
      for J := Low(Matrix[I]) to High(Matrix[I]) do
        S := S + ' ' + Format('%2d', [Matrix[I, J]]);
      List.Add(S);
    end;
  end;
end;

{ TCnVertex }

procedure TCnVertex.AddInNeighbour(PrevRef: TCnVertex);
begin
  if PrevRef <> nil then
    FInNeighbours.Add(PrevRef);
end;

procedure TCnVertex.AddOutNeighbour(NextRef: TCnVertex; Weight: Integer);
begin
  if NextRef <> nil then
  begin
    FOutNeighbours.Add(NextRef);
    FWeights.Add(Pointer(Weight));
    NextRef.AddInNeighbour(Self);
  end;
end;

procedure TCnVertex.ClearNeighbours;
begin
  FOutNeighbours.Clear;
  FInNeighbours.Clear;
  FWeights.Clear;
end;

constructor TCnVertex.Create(AOwner: TCnGraph);
begin
  inherited Create;
  FOwner := AOwner;
  FOutNeighbours := TObjectList.Create(False);
  FWeights := TList.Create;
  FInNeighbours := TObjectList.Create(False);
end;

destructor TCnVertex.Destroy;
begin
  FInNeighbours.Free;
  FWeights.Free;
  FOutNeighbours.Free;
  inherited;
end;

function TCnVertex.GetInNeighbour(Index: Integer): TCnVertex;
begin
  Result := FInNeighbours[Index] as TCnVertex;
end;

function TCnVertex.GetInNeighbourCount: Integer;
begin
  Result := FInNeighbours.Count;
end;

function TCnVertex.GetOutNeighbour(Index: Integer): TCnVertex;
begin
  Result := FOutNeighbours[Index] as TCnVertex;
end;

function TCnVertex.GetOutNeighbourCount: Integer;
begin
  Result := FOutNeighbours.Count;
end;

function TCnVertex.GetWeight(Index: Integer): Integer;
begin
  Result := Integer(FWeights[Index]);
end;

procedure TCnVertex.RemoveInNeighbour(PrevRef: TCnVertex);
begin
  if PrevRef <> nil then
    FInNeighbours.Remove(PrevRef);
end;

procedure TCnVertex.RemoveOutNeighbour(NextRef: TCnVertex);
var
  WeightIndex: Integer;
begin
  if NextRef <> nil then
  begin
    WeightIndex := FOutNeighbours.IndexOf(NextRef);
    if WeightIndex >= 0 then
    begin
      FOutNeighbours.Delete(WeightIndex);
      FWeights.Delete(WeightIndex);
      NextRef.RemoveInNeighbour(Self);
    end;
  end;
end;

{ TCnGraph }

function TCnGraph.AddEdge(Vertex1, Vertex2: TCnVertex; Weight: Integer): Boolean;
begin
  Result := False;

  if not HasVertex(Vertex1) or not HasVertex(Vertex2) then
    Exit;

  Vertex1.AddOutNeighbour(Vertex2, Weight);
  if not FDirected and (Vertex1 <> Vertex2) then
    Vertex2.AddOutNeighbour(Vertex1, Weight);

  Inc(FEdgeCount);
  Result := True;
end;

function TCnGraph.AddVertex(const Text: string): TCnVertex;
begin
  Result := TCnVertex.Create(Self);
  Result.Text := Text;
  FVertexes.Add(Result);
end;

function TCnGraph.AddVertexesEdge(const Text1, Text2: string;
  Weight: Integer): Boolean;
var
  V1, V2: TCnVertex;
begin
  V1 := FindVertex(Text1);
  if V1 = nil then
    V1 := AddVertex(Text1);

  V2 := FindVertex(Text2);
  if V2 = nil then
    V2 := AddVertex(Text2);

  Result := AddEdge(V1, V2, Weight);
end;

procedure TCnGraph.ClearEdges;
var
  I: Integer;
begin
  for I := 0 to FVertexes.Count - 1 do
    TCnVertex(FVertexes[I]).ClearNeighbours;
  FEdgeCount := 0;
end;

procedure TCnGraph.ClearVertexes;
begin
  FVertexes.Clear;
  FEdgeCount := 0;
end;

procedure TCnGraph.ClearVisited;
var
  I: Integer;
begin
  for I := 0 to FVertexes.Count - 1 do
    TCnVertex(FVertexes[I]).Visited := False;
end;

constructor TCnGraph.Create(ADirected: Boolean);
begin
  inherited Create;
  FDirected := ADirected;
  FVertexes := TObjectList.Create(True);
end;

procedure TCnGraph.DepthFirstTravel(Vertex: TCnVertex);
begin
  if HasVertex(Vertex) then
  begin
    ClearVisited;
    DoDepthFirstTravel(Vertex);
  end;
end;

destructor TCnGraph.Destroy;
begin
  FVertexes.Free;
  inherited;
end;

procedure TCnGraph.DoDepthFirstTravel(Vertex: TCnVertex);
var
  I: Integer;
begin
  Vertex.Visited := True;
  if Assigned(FOnDepthFirstTravelVertex) then
    FOnDepthFirstTravelVertex(Vertex);

  for I := 0 to Vertex.OutNeighbourCount - 1 do
    if not Vertex.OutNeighbour[I].Visited then
      DoDepthFirstTravel(Vertex.OutNeighbour[I]);
end;

procedure TCnGraph.DoWidthFirstTravel(Vertex: TCnVertex);
var
  Queue: TQueue;
  I: Integer;
  V: TCnVertex;
begin
  Queue := TQueue.Create;
  try
    Vertex.Visited := True;
    Queue.Push(Vertex);

    while Queue.Count > 0 do
    begin
      V := TCnVertex(Queue.Pop);
      if Assigned(FOnWidthFirstTravelVertex) then
        FOnWidthFirstTravelVertex(V);

      for I := 0 to V.OutNeighbourCount - 1 do
      begin
        if not V.OutNeighbour[I].Visited then
        begin
          V.OutNeighbour[I].Visited := True;
          Queue.Push(V.OutNeighbour[I]);
        end;
      end;
    end;
  finally
    Queue.Free;
  end;   
end;

function TCnGraph.DumpToAdjacencyMatrix: TCnAdjacencyMatrix;
var
  Row, Col, Idx: Integer;
  VR, VC: TCnVertex;
begin
  if VertexCount = 0 then
    raise ECnGraphException.Create('NO Vertexes.');

  SetLength(Result, VertexCount, VertexCount);

  for Row := 0 to FVertexes.Count - 1 do
  begin
    VR := TCnVertex(FVertexes[Row]);
    for Col := 0 to VR.OutNeighbourCount - 1 do
    begin
      VC := VR.OutNeighbour[Col];
      Idx := FVertexes.IndexOf(VC);
      if Idx >= 0 then
        Result[Row, Idx] := VR.Weight[Col];
    end;
  end;
end;

function TCnGraph.DumpToIncidenceMatrix: TCnIncidenceMatrix;
var
  Row, Col, I, Idx: Integer;
begin
  if VertexCount = 0 then
    raise ECnGraphException.Create('NO Vertexes.');
  if EdgeCount = 0 then
    raise ECnGraphException.Create('NO Edges.');

  SetLength(Result, VertexCount, EdgeCount);

  // ����������������Ȩ�أ��޷���������ͼ�еĶ����Ի�
  Col := 0;
  for Row := 0 to FVertexes.Count - 1 do
  begin
    for I := 0 to Vertex[Row].OutNeighbourCount - 1 do
    begin
      // �ҵ�һ���ߣ���ʼ���� Vertex[Row] �Լ� Vertex[Row].OutNeighbour[I]
      Idx := FVertexes.IndexOf(Vertex[Row].OutNeighbour[I]);
      if Idx > 0 then
      begin
        Result[Row, Col] := 1;  // �����
        if FDirected then
        begin
          if Result[Idx, Col] = 0 then // ������ָ��
            Result[Idx, Col] := -1     // ���յ�
        end
        else
          Result[Idx, Col] := 1;  // ����ͼ���� 1
      end;
      Inc(Col);
    end;
  end;
end;

function TCnGraph.FindVertex(const Text: string): TCnVertex;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to FVertexes.Count - 1 do
  begin
    if TCnVertex(FVertexes[I]).Text = Text then
    begin
      Result := TCnVertex(FVertexes[I]);
      Exit;
    end;
  end;
end;

function TCnGraph.GetVertex(Index: Integer): TCnVertex;
begin
  Result := TCnVertex(FVertexes[Index]);
end;

function TCnGraph.GetVertexCount: Integer;
begin
  Result := FVertexes.Count;
end;

function TCnGraph.GetVertexDegree(Vertex: TCnVertex): Integer;
begin
  if HasVertex(Vertex) then
  begin
    if FDirected then
      Result := Vertex.InNeighbourCount + Vertex.OutNeighbourCount
    else // ����ͼ�Գ���Ϊ׼
      Result := Vertex.OutNeighbourCount;
  end
  else
    Result := -1;
end;

function TCnGraph.GetVertexInDegree(Vertex: TCnVertex): Integer;
begin
  if not FDirected then
    raise ECnGraphException.Create('NO InDegree for Undirected Graph.');

  if HasVertex(Vertex) then
    Result := Vertex.InNeighbourCount
  else
    Result := -1;
end;

function TCnGraph.GetVertexOutDegree(Vertex: TCnVertex): Integer;
begin
  if not FDirected then
    raise ECnGraphException.Create('NO OutDegree for Undirected Graph.');

  if HasVertex(Vertex) then
    Result := Vertex.OutNeighbourCount
  else
    Result := -1;
end;

function TCnGraph.HasVertex(Vertex: TCnVertex): Boolean;
begin
  Result := (Vertex <> nil) and (Vertex.Owner = Self) and (FVertexes.IndexOf(Vertex) >= 0);
end;

function TCnGraph.RemoveEdge(Vertex1, Vertex2: TCnVertex): Boolean;
begin
  Result := False;
  if not HasVertex(Vertex1) or not HasVertex(Vertex2) then
    Exit;

  Vertex1.RemoveOutNeighbour(Vertex2);
  if not FDirected and (Vertex1 <> Vertex2) then
    Vertex2.RemoveOutNeighbour(Vertex1);

  Dec(FEdgeCount);
  Result := True;
end;

function TCnGraph.RemoveVertex(Vertex: TCnVertex): Boolean;
var
  I: Integer;
begin
  Result := False;
  if not HasVertex(Vertex) then
    Exit;

  // ɾ OutNeighbours ��ÿһ����� InNeighbours ����Լ�
  for I := 0 to Vertex.OutNeighbourCount - 1 do
    Vertex.OutNeighbour[I].RemoveInNeighbour(Vertex);

  // ɾ InNeighbours ��ÿһ����� OutNeighbours ����Լ�
  for I := 0 to Vertex.InNeighbourCount - 1 do
    Vertex.InNeighbour[I].RemoveOutNeighbour(Vertex);

  Vertex.ClearNeighbours;
  FVertexes.Remove(Vertex);
  Vertex.Free;
  Result := True;
end;

procedure TCnGraph.WidthFirstTravel(Vertex: TCnVertex);
begin
  if HasVertex(Vertex) then
  begin
    ClearVisited;
    DoWidthFirstTravel(Vertex);
  end;
end;

end.
