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

unit CnSkipList;
{* |<PRE>
================================================================================
* ������ƣ�������������
* ��Ԫ���ƣ���Ծ���� SkipList ʵ�ֵ�Ԫ
* ��Ԫ���ߣ���Х
* ��    ע���ο�ά���ٿ��Լ� ftp://ftp.cs.umd.edu/pub/skipLists/
*           ������Ծ���������ʱ�临�Ӷ����ڵ��������ӽ���
* ����ƽ̨��Win 7 + Delphi 5.0
* ���ݲ��ԣ���δ����
* �� �� �����õ�Ԫ���豾�ػ�����
* �޸ļ�¼��2015.05.22 V1.0
*               ������Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, Windows, Contnrs;

const
  CN_SKIPLIST_MAX_LEVEL = 16; // 0 ~ 15

type
  ECnSkipListLevelError = Exception;
  ECnSkipListCompareError = Exception;
  ECnSkipListRandomError = Exception;

  TCnSkipListNodeList = class;

  TCnSkipListNode = class
  {* һ���洢���ݽڵ�Ľṹ������һ������ָ��}
  private
    FLevel: Integer;
    FForwards: TCnSkipListNodeList;
    FData: Pointer;
    FText: string;
    FOnDestroy: TNotifyEvent;
  protected
    procedure DoDestroy; virtual;
  public
    constructor Create(ALevel: Integer);
    destructor Destroy; override;

    property Forwards: TCnSkipListNodeList read FForwards;
    {* ����ָ�룬0 �������������ϸ����һ��}
    property Level: Integer read FLevel write FLevel;
    {* �� Node ӵ�е���Ч��Σ��� 0 �� Level}
    
    property Data: Pointer read FData write FData;
    property Text: string read FText write FText;

    property OnDestroy: TNotifyEvent read FOnDestroy write FOnDestroy;
    {* �ͷŵĻص��¼���������ͷ� Data ��}
  end;

  TCnSkipListValueCompareFunc = function (const Value: Pointer; const Node: TCnSkipListNode): Integer;
  {* ���ڱȽϴ�����������ڵ���С�� Function��>=< �ֱ𷵻� 1��0��-1��
     ���������߼��ѷ�װ�� SkipList ��ʵ�֣�Head �ڵ�ҪС���κ�ֵ���κ�ֵС�� nil �ڵ�}

  TCnSkipListNodeList = class(TObjectList)
  {* һ���ڵ�ṹ�е�����ָ��}
  private
    function GetItem(Index: Integer): TCnSkipListNode;
    procedure SetItem(Index: Integer; const Value: TCnSkipListNode);
  public
    constructor Create; 
    destructor Destroy; override;

    function Add(ANode: TCnSkipListNode): Integer;
    property Items[Index: Integer]: TCnSkipListNode read GetItem write SetItem; default;
  end;

  TCnLevelProbability = (lpDot5, lpDot25, lpDot125); // �� Level �ĸ���

  TCnSkipList = class
  {* ��Ծ���� SkipList ��ʵ����}
  private
    FRandomBits: DWORD;
    FAllowDuplicates: Boolean;
    FCompareFunc: TCnSkipListValueCompareFunc;
    FMaxLevel: Integer;
    FHead: TCnSkipListNode;
    FLevelProbability: TCnLevelProbability;
    function GetMaxLevel: Integer;
    function GetCount: Integer;

  protected
    function ValueGreaterThanNode(const Value: Pointer; const Node: TCnSkipListNode): Boolean;
    function ValueEqualsNode(const Value: Pointer; const Node: TCnSkipListNode): Boolean;
    function ValueLessThanNode(const Value: Pointer; const Node: TCnSkipListNode): Boolean;

    function GenerateLevel: Integer;
  public
    constructor Create(CompareFunc: TCnSkipListValueCompareFunc; ALevelProbability:
      TCnLevelProbability = lpDot5; AllowDuplicates: Boolean = False);
    {* ���캯����
       CompareFunc��ʹ���߱����ṩһ�ȽϺ���������Ծ����ص������ж������ҵ�ֵ��ڵ�Ĵ�С��ϵ
       ALevelProbability������ĸ��ʣ�Ĭ��Ϊ 0.5
       AllowDuplicates���Ƿ���������ظ�ֵ���Ƿ������ CompareFunc ���ص�ֵ����
    }
    destructor Destroy; override;
    {* �������������ͷ�ȫ���ڵ㣬�ڵ�� OnDestroy �¼��ɹ��ͷ��丽������}

    function Delete(AValue: Pointer): Boolean;
    {* ���Ҳ�ɾ��һֵ�������Ƿ�ɾ���ɹ�}
    function Search(AValue: Pointer): TCnSkipListNode;
    {* ����һֵ�������ҵ��Ľڵ㣬���򷵻� nil}
    function Insert(AValue: Pointer): TCnSkipListNode;
    {* ����һֵ�����ز���Ľڵ㣬�ⲿ����ʹ�ô˽ڵ�洢 AValue������ʧ�ܷ��� nil}

    property AllowDuplicates: Boolean read FAllowDuplicates;
    {* �Ƿ���������ظ��ڵ�}
    property MaxLevel: Integer read GetMaxLevel;
    {* ���������нڵ������Σ�����δ� 0 �� MaxLevel}
    property Head: TCnSkipListNode read FHead;
    {* ͷ�ڵ㣬�����������ε�����ָ��}
    property Count: Integer read GetCount;
    {* �ڵ�����ͨ�������õ�}
    property LevelProbability: TCnLevelProbability read FLevelProbability;
    {* �� Level �ĸ��ʣ�ֻ�������̶�ֵ}
  end;

implementation

const
  CN_BITS_IN_RANDOM = 31;
  PROV_RSA_FULL = 1;

function CryptAcquireContext(phProv: PULONG; pszContainer: PAnsiChar;
  pszProvider: PAnsiChar; dwProvType: DWORD; dwFlags: DWORD): BOOL;
  stdcall; external ADVAPI32 name 'CryptAcquireContextA';

function CryptReleaseContext(hProv: ULONG; dwFlags: DWORD): BOOL;
  stdcall; external ADVAPI32 name 'CryptReleaseContext';

function CryptGenRandom(hProv: ULONG; dwLen: DWORD; pbBuffer: PAnsiChar): BOOL;
  stdcall; external ADVAPI32 name 'CryptGenRandom';

// ʹ�� Windows API ʵ�������� 32 bit
function Rand32: DWORD;
var
  HProv: Cardinal;
  Res: DWORD;
begin
  HProv := 0;
  if not CryptAcquireContext(@HProv, nil, nil, PROV_RSA_FULL, 0) then
    raise ECnSkipListRandomError.Create('SkipList Error Random.');

  if HProv <> 0 then
  begin
    try
      CryptGenRandom(HProv, SizeOf(DWORD), @Res);
    finally
      CryptReleaseContext(HProv, 0);
    end;
  end
  else
    raise ECnSkipListRandomError.Create('SkipList Error Random.');
  
  Result := Res;
end;

{ TCnSkipListNodeList }

function TCnSkipListNodeList.Add(ANode: TCnSkipListNode): Integer;
begin
  Result := inherited Add(ANode);
end;

constructor TCnSkipListNodeList.Create;
begin
  inherited Create(False);
end;

destructor TCnSkipListNodeList.Destroy;
begin
  inherited;

end;

function TCnSkipListNodeList.GetItem(Index: Integer): TCnSkipListNode;
begin
  Result := TCnSkipListNode(inherited GetItem(Index));
end;

procedure TCnSkipListNodeList.SetItem(Index: Integer;
  const Value: TCnSkipListNode);
begin
  inherited SetItem(Index, Value);
end;

{ TCnSkipListNode }

constructor TCnSkipListNode.Create(ALevel: Integer);
var
  I: Integer;
begin
  if (ALevel < 0) or (ALevel >= CN_SKIPLIST_MAX_LEVEL) then
    raise ECnSkipListLevelError.Create('SkipList Error Level.');

  FLevel := ALevel;
  FForwards := TCnSkipListNodeList.Create;

  for I := 0 to CN_SKIPLIST_MAX_LEVEL - 1 do
    FForwards.Add(nil);
end;

destructor TCnSkipListNode.Destroy;
begin
  DoDestroy;
  FForwards.Free;
  inherited;
end;

procedure TCnSkipListNode.DoDestroy;
begin
  if Assigned(FOnDestroy) then
    FOnDestroy(Self);
end;

{ TCnSkipList }

constructor TCnSkipList.Create(CompareFunc: TCnSkipListValueCompareFunc;
  ALevelProbability: TCnLevelProbability; AllowDuplicates: Boolean);
begin
  inherited Create;
  if not Assigned(CompareFunc) then
    raise ECnSkipListCompareError.Create('SkipList No Compare Function.');

  FLevelProbability := ALevelProbability;
  FAllowDuplicates := AllowDuplicates;
  FCompareFunc := CompareFunc;

  FHead := TCnSkipListNode.Create(0); // 0 ~ CN_SKIPLIST_MAX_LEVEL
end;

function TCnSkipList.Delete(AValue: Pointer): Boolean;
var
  K, M: Integer;
  P, PreNode, Q: TCnSkipListNode;
  Prevs: array[0..CN_SKIPLIST_MAX_LEVEL - 1] of TCnSkipListNode;
begin
  Result := False;

  PreNode := FHead;
  K := FMaxLevel;
  M := FMaxLevel;
  FillChar(Prevs, SizeOf(Prevs), 0);

  repeat
    Q := PreNode.Forwards[K];
    while ValueGreaterThanNode(AValue, Q) and (Q <> nil) do
    begin
      PreNode := Q;
      Q := PreNode.Forwards[K];
    end;
    Prevs[K] := PreNode;
    Dec(K);
  until K < 0; // ��ͬ�� Search������¼���м���

  if ValueEqualsNode(AValue, Q) then // �ҵ���Ҫɾ����
  begin
    K := 0;
    P := Prevs[K];
    while (K <= M) and (P.Forwards[K] = Q) do
    begin
      P.Forwards[K] := Q.Forwards[K];
      Inc(K);
      P := Prevs[K];
    end;

    Q.Free; // �ͷű�ɾ�ڵ�

    while (FHead.Forwards[M] = nil) and (M > 0) do
      Dec(M);
    FMaxLevel := M; // ����������
    Result := True;
  end;
end;

destructor TCnSkipList.Destroy;
var
  P, Q: TCnSkipListNode;
begin
  P := FHead;
  repeat
    Q := P.Forwards[0];
    P.Free;
    P := Q;
  until P = nil;
  // ������ײ� Node ���ͷ�

  inherited;
end;

function TCnSkipList.GenerateLevel: Integer;
var
  C, B: DWORD;
begin
  Result := 0;
  C := 1;
  case FLevelProbability of
    lpDot5: C := 1;        // 1
    lpDot25: C := 3;       // 11
    lpDot125: C := 7;      // 111
  end;

  repeat
    FRandomBits := Rand32;
    B := FRandomBits and C;
    if B = 0 then    // ��һ��ĸ����� FLevelProbability
      Inc(Result);
  until B <> 0;

  if Result >= CN_SKIPLIST_MAX_LEVEL then
    Result := CN_SKIPLIST_MAX_LEVEL - 1;
end;

function TCnSkipList.GetCount: Integer;
var
  P: TCnSkipListNode;
begin
  P := FHead;
  Result := 0;
  while P <> nil do
  begin
    Inc(Result);
    P := P.Forwards[0];
  end;
end;

function TCnSkipList.GetMaxLevel: Integer;
begin
  Result := FMaxLevel;
end;

function TCnSkipList.Insert(AValue: Pointer): TCnSkipListNode;
var
  K: Integer;
  P, Q, PreNode, NewNode: TCnSkipListNode;
  Prevs: array[0..CN_SKIPLIST_MAX_LEVEL - 1] of TCnSkipListNode;
begin
  Result := nil;

  PreNode := FHead;
  K := FMaxLevel;

  FillChar(Prevs, SizeOf(Prevs), 0);
  repeat
    Q := PreNode.Forwards[K];
    while ValueGreaterThanNode(AValue, Q) and (Q <> nil) do
    begin
      PreNode := Q;
      Q := PreNode.Forwards[K];
    end;
    Prevs[K] := PreNode;
    Dec(K);
  until K < 0; // ��ͬ�� Search����Ҳ��¼���м���

  if (not FAllowDuplicates) and ValueEqualsNode(AValue, Q) then
    Exit;
  // �½ڵ�Ҫ�� P ���棬��δ���ǲ� FHead ǰ������
  
  K := GenerateLevel;
  if K > FMaxLevel then
  begin
    Inc(FMaxLevel);
    K := FMaxLevel;
    Prevs[K] := FHead;

    // FHead �� Level ��Ҫ��
    if FHead.Level < K then
      FHead.Level := K;
  end;  // Update �Ǵ�����ڵ�����һ�Ÿ����ڵ㣬��Ҫ�����ָ�������ڵ�

  NewNode := TCnSkipListNode.Create(K);
  Result := NewNode;
  repeat
    P := Prevs[K];                        // ȡһ���� K �����߽ڵ�
    NewNode.Forwards[K] := P.Forwards[K];  // �� K ���½ڵ�ָ��ԭ����ڵ����һ��
    P.Forwards[K] := NewNode;              // ��ڵ�� K ��ָ�������ڵ�
    Dec(K);
  until K < 0;
end;

function TCnSkipList.Search(AValue: Pointer): TCnSkipListNode;
var
  I: Integer;
  P, Q: TCnSkipListNode;
begin
  Result := nil;
  P := FHead;
  I := FMaxLevel;

  repeat
    Q := P.Forwards[I];
    while ValueGreaterThanNode(AValue, Q) and (Q <> nil) do
    begin
      P := Q;
      Q := P.Forwards[I];
    end;
    Dec(I);
  until I < 0;

  if not ValueEqualsNode(AValue, Q) then
    Exit;

  Result := Q;
end;

function TCnSkipList.ValueEqualsNode(const Value: Pointer;
  const Node: TCnSkipListNode): Boolean;
begin
  if Node = FHead then
    Result := False
  else if Node = nil then
    Result := False
  else
    Result := FCompareFunc(Value, Node) = 0;
end;

function TCnSkipList.ValueGreaterThanNode(const Value: Pointer;
  const Node: TCnSkipListNode): Boolean;
begin
  if Node = FHead then     // �κ��ӽڵ���Զ���� Head
    Result := True
  else if Node = nil then  // �κ��ӽڵ���ԶС�� nil
    Result := False
  else
    Result := FCompareFunc(Value, Node) > 0;
end;

function TCnSkipList.ValueLessThanNode(const Value: Pointer;
  const Node: TCnSkipListNode): Boolean;
begin
  if Node = FHead then     // Head ��ԶС���κ��ӽڵ�
    Result := False
  else if Node = nil then  // nil ��Զ�����κνڵ�
    Result := True
  else
    Result := FCompareFunc(Value, Node) < 0;
end;

end.
