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

unit CnDancingLinks;
{* |<PRE>
================================================================================
* ������ƣ�CnPack ������Ԫ
* ��Ԫ���ƣ�ʵ�ֻ���ʮ��˫��ѭ�������ϡ����󣬲��һ��ڴ�ʵ���赸����
* ��Ԫ���ߣ���Х (liuxiao@cnpack.org)
* ��    ע���õ�ԪΪ����ϡ�����ʵ����ʮ��˫��ѭ������
*           ���ڴ˻�����ʵ�����赸����Ŀ���ɾ��/�ָ����еĻ��ơ�
*           ����δ��������ݵݹ������
* ����ƽ̨��PWinXP + Delphi 5.01
* ���ݲ��ԣ�PWin9X/2000/XP + Delphi 5/6/7 + C++Builder 5/6
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2015.05.29 V1.0 by LiuXiao
*               ������Ԫ��ʵ�ֹ���
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Classes, Windows, SysUtils, Contnrs;

type
  ECnCrossLinkedMatrixException = class(Exception);

//==============================================================================
// ʮ��˫������ڵ�ʵ����
//==============================================================================

  TCnCrossLinkedNode = class(TObject)
  {* ʮ��˫������ڵ�ʵ����}
  private
    FUp: TCnCrossLinkedNode;
    FLeft: TCnCrossLinkedNode;
    FRight: TCnCrossLinkedNode;
    FDown: TCnCrossLinkedNode;
    FColumn: Integer;
    FRow: Integer;
    FData: Integer;
    FText: string;
  public
    property Left: TCnCrossLinkedNode read FLeft write FLeft;
    {* ���ڵ㱾����ߵĽڵ㣬�籾��ֻ�б��ڵ㣬���ָ��ڵ�����}
    property Right: TCnCrossLinkedNode read FRight write FRight;
    {* ���ڵ㱾���ұߵĽڵ㣬�籾��ֻ�б��ڵ㣬���ָ��ڵ�����}
    property Up: TCnCrossLinkedNode read FUp write FUp;
    {* ���ڵ㱾���ϱߵĽڵ㣬�籾��ֻ�б��ڵ㣬���ָ��ڵ�����}
    property Down: TCnCrossLinkedNode read FDown write FDown;
    {* ���ڵ㱾���±ߵĽڵ㣬�籾��ֻ�б��ڵ㣬���ָ��ڵ�����}

    property Column: Integer read FColumn;
    {* ���ڵ����ڵ��У��� 0 ��ʼ}
    property Row: Integer read FRow;
    {* ���ڵ����ڵ��У��� 0 ��ʼ}

    property Data: Integer read FData write FData;
    {* ���Ա���һ���������ԣ������� Tag}
    property Text: string read FText write FText;
    {* ���Ա���һ�ַ���������}
  end;

  TCnCrossLinkedNodeClass = class of TCnCrossLinkedNode;

//==============================================================================
// ʮ��˫��ѭ������ʵ�ֵ�ϡ�������
//==============================================================================

  TCnCrossLinkedMatrix = class(TObject)
  {* ʮ��˫��ѭ������ʵ�ֵ�ϡ�������}
  private
    FCount: Integer;  
    FColCount: Integer;
    FRowCount: Integer;
    FNodeClass: TCnCrossLinkedNodeClass;

    FColumnHeads: TObjectList;  // ��ͷָ��
    FRowHeads: TObjectList;     // ��ͷָ��
    FOnTravelNode: TNotifyEvent;

    function GetCells(Col, Row: Integer): TCnCrossLinkedNode;
    function CreateNode: TCnCrossLinkedNode;
    function GetColumnHead(Col: Integer): TCnCrossLinkedNode;
    function GetRowHead(Row: Integer): TCnCrossLinkedNode;
  protected
    procedure DoTravelNode(Node: TCnCrossLinkedNode); virtual;
  public
    constructor Create(AColCount: Integer; ARowCount: Integer;
      NodeClass: TCnCrossLinkedNodeClass = nil); virtual;
    destructor Destroy; override;

    function InsertNode(ACol, ARow: Integer): TCnCrossLinkedNode;
    {* ��ϡ������ָ������λ�ò���һ�ڵ㲢���ش˽ڵ㣬���λ���Ѵ����򷵻� nil}
    function ExtractNode(ACol, ARow: Integer): TCnCrossLinkedNode;
    {* ��ϡ������ָ������λ�ý���ڵ㲢���ش˽ڵ㣬���λ���޽ڵ��򷵻� nil}
    procedure RemoveNode(ACol, ARow: Integer);
    {* ɾ��ϡ�����ָ������λ�õĽڵ�}

    procedure TravelByRow;
    {* �����б���}
    procedure TravelByCol;
    {* �����б���}
    procedure ExpandRow(ExpandCount: Integer = 1);
    {* ��̬��������}
    procedure ExpandCol(ExpandCount: Integer = 1);
    {* ��̬��������}

    property RowCount: Integer read FRowCount;
    {* ϡ����������}
    property ColCount: Integer read FColCount;
    {* ϡ����������}
    property Cells[Col, Row: Integer]: TCnCrossLinkedNode read GetCells;
    {* �����С���������ϡ�����ĵ�Ԫ�����о��� 0 ��ʼ}
    property RowHead[Row: Integer]: TCnCrossLinkedNode read GetRowHead;
    {* �����з�����ͷԪ��}
    property ColumnHead[Col: Integer]: TCnCrossLinkedNode read GetColumnHead;
    {* �����з�����ͷԪ��}
    property Count: Integer read FCount;
    {* Ԫ�ظ���}
    property OnTravelNode: TNotifyEvent read FOnTravelNode write FOnTravelNode;
    {* ����ʱ�������¼�}
  end;

//==============================================================================
// �赸����ʵ����
//==============================================================================

  TCnDancingLinks = class(TCnCrossLinkedMatrix)
  {* �赸����ʵ���࣬�����˿���ɾ��/��ԭ���еķ���}
  public
    function ExtractRow(ARow: Integer): TCnCrossLinkedNode;
    {* ���һ�У����ظ��е���ͷԪ�أ���������Ԫ���������������ϵ}
    function RestoreRow(RowHead: TCnCrossLinkedNode): Boolean;
    {* �� ExtractRow ����������·��û�ԭλ}
    function ExtractColumn(ACol: Integer): TCnCrossLinkedNode;
    {* ���һ�У����ظ��е���ͷԪ�أ���������Ԫ���������������ϵ}
    function RestoreColumn(ColHead: TCnCrossLinkedNode): Boolean;
    {* �� ExtractColumn ����������·��û�ԭλ}
  end;

implementation

{ TCnCrossLinkedMatrix }

constructor TCnCrossLinkedMatrix.Create(AColCount, ARowCount: Integer;
  NodeClass: TCnCrossLinkedNodeClass);
var
  I: Integer;
begin
  inherited Create;
  if (AColCount <= 0) or (ARowCount <= 0) then
    raise ECnCrossLinkedMatrixException.Create('Error Column/Row Count.');

  FColCount := AColCount;
  FRowCount := ARowCount;
  if NodeClass = nil then
    FNodeClass := TCnCrossLinkedNode
  else
    FNodeClass := NodeClass;

  FColumnHeads := TObjectList.Create(False);
  FRowHeads := TObjectList.Create(False);

  for I := 0 to RowCount - 1 do
    FRowHeads.Add(nil);
  for I := 0 to ColCount - 1 do
    FColumnHeads.Add(nil);
end;

function TCnCrossLinkedMatrix.CreateNode: TCnCrossLinkedNode;
begin
  try
    Result := TCnCrossLinkedNode(FNodeClass.NewInstance);
    Result.Create;
  except
    Result := nil;
  end;
end;

destructor TCnCrossLinkedMatrix.Destroy;
var
  I: Integer;
  P, Q, Head: TCnCrossLinkedNode;
begin
  if FColumnHeads <> nil then
  begin
    for I := 0 to FColumnHeads.Count - 1 do
    begin
      Head := TCnCrossLinkedNode(FColumnHeads[I]);
      if Head <> nil then
      begin
        // �ͷű���
        P := Head;
        repeat
          Q := P.Down;
          P.Free;
          P := Q;
        until (P = Head) or (P = nil);
      end;
    end;
  end;
  inherited;
end;

procedure TCnCrossLinkedMatrix.DoTravelNode(Node: TCnCrossLinkedNode);
begin
  if Assigned(FOnTravelNode) then
    FOnTravelNode(Node);
end;

procedure TCnCrossLinkedMatrix.ExpandCol(ExpandCount: Integer);
var
  I: Integer;
begin
  if ExpandCount <= 0 then
    raise ECnCrossLinkedMatrixException.Create('Invalid Expand Count.');

  Inc(FColCount, ExpandCount);
  for I := 1 to ExpandCount do
    FColumnHeads.Add(nil);
end;

procedure TCnCrossLinkedMatrix.ExpandRow(ExpandCount: Integer);
var
  I: Integer;
begin
  if ExpandCount <= 0 then
    raise ECnCrossLinkedMatrixException.Create('Invalid Expand Count.');

  Inc(FRowCount, ExpandCount);
  for I := 1 to ExpandCount do
    FRowHeads.Add(nil);
end;

function TCnCrossLinkedMatrix.ExtractNode(ACol, ARow: Integer): TCnCrossLinkedNode;
var
  P, Head: TCnCrossLinkedNode;
begin
  Result := nil;
  if FColumnHeads[ACol] <> nil then  // ��ָ��������
  begin
    Head := TCnCrossLinkedNode(FColumnHeads[ACol]);
    P := Head;
    repeat
      if P.Row = ARow then
      begin
        Result := P;

        // ��ʼ�⿪�з���� P
        if (P = Head) and (P.Up = P) and (P.Down = P) then
        begin
          // ����ֻ�� Head һ����ֱ����ͷ���
          FColumnHeads[ACol] := nil;
        end
        else
        begin
          if P = Head then // P ����ͷ����Ҫ������ͷ
            FColumnHeads[ACol] := P.Down;
          P.Up.Down := P.Down;
          P.Down.Up := P.Up;
        end;

        // ��ʼ�⿪�з���� P
        Head := TCnCrossLinkedNode(FRowHeads[ARow]);
        if (P = Head) and (P.Left = P) and (P.Right = P) then
        begin
          // ����ֻ�� Head һ����ֱ����ͷ���
          FRowHeads[ARow] := nil;
        end
        else
        begin
          if P = Head then // P ����ͷ����Ҫ������ͷ
            FRowHeads[ARow] := P.Right;
          P.Left.Right := P.Right;
          P.Right.Left := P.Left;
        end;

        Dec(FCount);
        Exit;
      end;
      P := P.Down;
    until (P = Head) or (P = nil);
  end;
end;

function TCnCrossLinkedMatrix.GetCells(Col, Row: Integer): TCnCrossLinkedNode;
var
  P, Head: TCnCrossLinkedNode;
begin
  Result := nil;
  if FColumnHeads[Col] <> nil then  // ��ָ��������
  begin
    Head := TCnCrossLinkedNode(FColumnHeads[Col]);
    P := Head;
    repeat
      if P.Row = Row then
      begin
        Result := P;
        Exit;
      end;
      P := P.Down;
    until (P = Head) or (P = nil);
  end;
end;

function TCnCrossLinkedMatrix.GetColumnHead(Col: Integer): TCnCrossLinkedNode;
begin
  if (Col < 0) or (Col >= FColCount) then
    raise ECnCrossLinkedMatrixException.Create('Invalid Column Index.');

  Result := TCnCrossLinkedNode(FColumnHeads[Col]);
end;

function TCnCrossLinkedMatrix.GetRowHead(Row: Integer): TCnCrossLinkedNode;
begin
  if (Row < 0) or (Row >= FColCount) then
    raise ECnCrossLinkedMatrixException.Create('Invalid Row Index.');

  Result := TCnCrossLinkedNode(FRowHeads[Row]);
end;

function TCnCrossLinkedMatrix.InsertNode(ACol, ARow: Integer): TCnCrossLinkedNode;
var
  P, Head: TCnCrossLinkedNode;
  InsertColSuccess, InsertRowSuccess: Boolean;
begin
  Result := nil;
  if (ACol < 0) or (ARow < 0) or (ACol >= FColCount) or (ARow >= FRowCount) then
    raise ECnCrossLinkedMatrixException.Create('Error Column/Row Index.');

  if Cells[ACol, ARow] <> nil then // �Ѿ�����
    Exit;

  InsertRowSuccess := False;
  InsertColSuccess := False;
  Result := CreateNode;
  Result.FColumn := ACol;
  Result.FRow := ARow;

  try
    if FRowHeads[ARow] = nil then // ����Ϊ�գ�ֱ�����������Լ�
    begin
      Result.Left := Result;
      Result.Right := Result;
      FRowHeads[ARow] := Result;
    end
    else // ���в�Ϊ�գ��ҵ�����ص�
    begin
      Head := TCnCrossLinkedNode(FRowHeads[ARow]);
      P := Head;
      repeat
        if P.Column = ACol then
        begin
          // ���еĴ���λ���ѱ�ռ�ã�ֱ���ͷ��˳�
          Exit;
        end
        else if P.Column > ACol then
          Break;

        P := P.Right;
      until (P = Head) or (P = nil);

      if (P = Head) and (Head.Column < ACol) then
      begin
        // �ҵ�ͷ�˶�û�ҵ�����������Ҫ������еģ����ڱ�����ĩ
        P := P.Left; // P ��ʱ�ƻ�ȥ����β�ڵ�
        P.Right := Result;
        Result.Left := P;
        Result.Right := Head;
        Head.Left := Result;
      end
      else
      begin
        // P ������������ ARow �ĵ�һ����Ӧ�ò��� P ����� P ֮��
        if Head = P then // P ����ͷ��������ͷ
          FRowHeads[ARow] := Result;

        // �� P ���
        Result.Left := P.Left;
        Result.Right := P;
        P.Left.Right := Result;
        P.Left := Result;
      end;
    end;
    InsertRowSuccess := True;

    if FColumnHeads[ACol] = nil then // ����Ϊ�գ�ֱ�����������Լ�
    begin
      Result.Up := Result;
      Result.Down := Result;
      FColumnHeads[ACol] := Result;
    end
    else // ���в�Ϊ�գ��ҵ�����ص�
    begin
      Head := TCnCrossLinkedNode(FColumnHeads[ACol]);
      P := Head;
      repeat
        if P.Row = ARow then
        begin
          // ���еĴ���λ���ѱ�ռ�ã�Ҫ�ָ���ɾ�� Result �Ѿ��������
          // ���������˼���жϣ����Դ˴������Ͻ����������迼�Ǹ��ӵ��ͷ�������
          Exit;
        end
        else if P.Row > ARow then
          Break;

        P := P.Down;
      until (P = Head) or (P = nil);

      if (P = Head) and (P.Row < ARow) then
      begin
        // �ҵ�ͷ�˶�û�ҵ�����������Ҫ������еģ����ڱ�����ĩ
        P := P.Up; // P ��ʱ�ƻ�ȥ����β�ڵ�
        P.Down := Result;
        Result.Up := P;
        Result.Down := Head;
        Head.Up := Result;
      end
      else
      begin
        // P ������������ ACol �ĵ�һ����Ӧ�ò��� P ����� P ֮��
        if P = Head then // P ����ͷ��������ͷ
          FColumnHeads[ACol] := Result;

        // �� P �ϱ�
        Result.Up := P.Up;
        Result.Down := P;
        P.Up.Down := Result;
        P.Up := Result;
      end;
    end;
    InsertColSuccess := True;
  finally
    if not InsertColSuccess and not InsertRowSuccess then
      FreeAndNil(Result)
    else
      Inc(FCount);
  end;
end;

procedure TCnCrossLinkedMatrix.RemoveNode(ACol, ARow: Integer);
begin
  ExtractNode(ACol, ARow).Free;
end;

procedure TCnCrossLinkedMatrix.TravelByCol;
var
  I: Integer;
  P, Q, Head: TCnCrossLinkedNode;
begin
  for I := 0 to FColumnHeads.Count - 1 do
  begin
    Head := TCnCrossLinkedNode(FColumnHeads[I]);
    if Head <> nil then
    begin
      P := Head;
      repeat
        Q := P.Down;
        DoTravelNode(P);
        P := Q;
      until (P = Head) or (P = nil);
    end;
  end;
end;

procedure TCnCrossLinkedMatrix.TravelByRow;
var
  I: Integer;
  P, Q, Head: TCnCrossLinkedNode;
begin
  for I := 0 to FRowHeads.Count - 1 do
  begin
    Head := TCnCrossLinkedNode(FRowHeads[I]);
    if Head <> nil then
    begin
      P := Head;
      repeat
        Q := P.Right;
        DoTravelNode(P);
        P := Q;
      until (P = Head) or (P = nil);
    end;
  end

end;

{ TCnDancingLinks }

function TCnDancingLinks.ExtractColumn(ACol: Integer): TCnCrossLinkedNode;
var
  P: TCnCrossLinkedNode;
  Row: Integer;
begin
  if (ACol < 0) or (ACol >= ColCount) then
    raise ECnCrossLinkedMatrixException.Create('Error Column Index.');

  Result := TCnCrossLinkedNode(FColumnHeads[ACol]);
  if Result = nil then
    Exit;

  // ���� Result ָ�����Ԫ�أ����������⿪
  P := Result;
  repeat
    Row := P.Row;
    if (P.Left = P) and (P.Right = P) and (FRowHeads[Row] = P) then // ����ֻ��һ����ֱ������ͷ
      FRowHeads[Row] := nil
    else
    begin
      // P ����ͷ��������ͷָ����һ��
      if FRowHeads[Row] = P then
        FRowHeads[Row] := P.Right;
      // �⿪ P
      P.Left.Right := P.Right;
      P.Right.Left := P.Left;
    end;
    
    Dec(FCount);
    P := P.Down;
  until (P = Result) or (P = nil);
  // ��ժ������
  FColumnHeads[ACol] := nil;
end;

function TCnDancingLinks.ExtractRow(ARow: Integer): TCnCrossLinkedNode;
var
  P: TCnCrossLinkedNode;
  Col: Integer;
begin
  if (ARow < 0) or (ARow >= RowCount) then
    raise ECnCrossLinkedMatrixException.Create('Error Row Index.');

  Result := TCnCrossLinkedNode(FRowHeads[ARow]);
  if Result = nil then
    Exit;

  // ���� Result ָ�����Ԫ�أ����������⿪
  P := Result;
  repeat
    Col := P.Column;
    if (P.Up = P) and (P.Down = P) and (FColumnHeads[Col] = P) then // ����ֻ��һ����ֱ������ͷ
      FColumnHeads[Col] := nil
    else
    begin
      // P ����ͷ��������ͷָ����һ��
      if FColumnHeads[Col] = P then
        FColumnHeads[Col] := P.Down;
      // �⿪ P
      P.Up.Down := P.Down;
      P.Down.Up := P.Up;
    end;

    Dec(FCount);
    P := P.Right;
  until (P = Result) or (P = nil);
  // ��ժ������
  FRowHeads[ARow] := nil;
end;

function TCnDancingLinks.RestoreColumn(ColHead: TCnCrossLinkedNode): Boolean;
var
  Row, Col: Integer;
  P: TCnCrossLinkedNode;
begin
  Result := False;
  if ColHead = nil then
    Exit;

  Col := ColHead.Column;
  if (Col < 0) or (Col >= ColCount) then
    Exit;

  if FColumnHeads[Col] <> nil then // �����Ѵ��ڣ��޷��ٴβ���
    Exit;

  // ���½�����Ԫ�ز���
  FColumnHeads[Col] := ColHead; // ��ͷָ�������Ԫ��
  P := ColHead;
  repeat
    // ��ÿһ����Ԫ�أ��ؽ������е����ӹ�ϵ
    Row := P.Row;
    if FRowHeads[Row] = nil then
    begin
      // ������Ԫ�أ�ֱ��������ͷ
      FRowHeads[Row] := P;
      P.Left := P;
      P.Right := P;
    end
    else
    begin
      // �ָ� P
      P.Left.Right := P;
      P.Right.Left := P;
      // ��� P �����ף��������ͷָ��
      if P.Column < TCnCrossLinkedNode(FRowHeads[Row]).Column then
        FRowHeads[Row] := P;
    end;

    Inc(FCount);
    P := P.Down;
  until (P = ColHead) or (P = nil);
end;

function TCnDancingLinks.RestoreRow(RowHead: TCnCrossLinkedNode): Boolean;
var
  Row, Col: Integer;
  P: TCnCrossLinkedNode;
begin
  Result := False;
  if RowHead = nil then
    Exit;

  Row := RowHead.Row;
  if (Row < 0) or (Row >= RowCount) then
    Exit;

  if FRowHeads[Row] <> nil then // �����Ѵ��ڣ��޷��ٴβ���
    Exit;

  // ���½�����Ԫ�ز���
  FRowHeads[Row] := RowHead; // ��ͷָ�������Ԫ��
  P := RowHead;
  repeat
    // ��ÿһ����Ԫ�أ��ؽ������е����ӹ�ϵ
    Col := P.Column;
    if FColumnHeads[Col] = nil then
    begin
      // ������Ԫ�أ�ֱ��������ͷ
      FColumnHeads[Col] := P;
      P.Up := P;
      P.Down := P;
    end
    else
    begin
      // �ָ� P
      P.Up.Down := P;
      P.Down.Up := P;
      // ��� P �����ף��������ͷָ��
      if P.Row < TCnCrossLinkedNode(FColumnHeads[Col]).Row then
        FColumnHeads[Col] := P;
    end;

    Inc(FCount);
    P := P.Right;
  until (P = RowHead) or (P = nil);
end;

end.
