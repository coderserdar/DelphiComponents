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

unit CnDockHashTable;
{* |<PRE>
================================================================================
* ������ƣ������ӹ��������ͣ����Ԫ
* ��Ԫ���ƣ�����ͣ����HashTable��Ԫ 
* ��Ԫ���ߣ�CnPack������ ���沨��³С�ࣩ
* ��    ע������Ԫ��ԭ������ȨCnPack��������ֲ���ѱ���ԭ���߰�Ȩ��Ϣ
* ����ƽ̨��
* ���ݲ��ԣ�PWin9X/2000/XP + Delphi 5/6/7
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2007.07.13 V1.0
*                ��ֲ��Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Classes, Controls;

const DefaultHashSize = 20;

type
  { ɢ�еĽڵ��� }
  TCnDockClientHashNode = class
  private
    FKeyName: string;                 //������
    FKeyData: Pointer;                //����
    FPrevNode,                        //��һ��ָ��
    FNextNode: TCnDockClientHashNode; //��һ��ָ��
    FListIndex: Integer;              //�����ĸ�Ͱ
  public
    property KeyName: string read FKeyName write FKeyName;
    property KeyData: Pointer read FKeyData write FKeyData;
    property PrevNode: TCnDockClientHashNode
        read FPrevNode write FPrevNode;
    property NextNode: TCnDockClientHashNode
        read FNextNode write FNextNode;
    property ListIndex: Integer read FListIndex write FListIndex;
  end;

  { ɢ���� }
  TCnDockControlHashTable = class
  private
    FCurrentSize,            //��ǰͰ��
    FTableSize: Integer;     //���Ͱ��
    FEntryList: TList;       //Ͱ����
    FRiseException: Boolean; //�����ҵ���ͬ������ʱ�Ƿ񴥷��쳣
    procedure SetTableSize(const Value: Integer);     //Ϊɢ�б����ռ�
  protected
    function HashProc(Name: string): Integer; virtual;//ɢ�к���������Name�ĳ�ʼͰ��
    procedure DeleteListIndex(Index: Integer);        //ɾ������ΪIndex��Ͱ
    function CreateKeyNode(KeyName: string; KeyData: Pointer;
      ListIndex: Integer): TCnDockClientHashNode;     //����ɢ�нڵ�
    function CompareKey(Key1, Key2: string): Integer; //�Ƚ������ڵ�
  public
    constructor Create(Size: Integer = DefaultHashSize; RiseExcept: Boolean = True); virtual;
    destructor Destroy; override;
    procedure CreateDictionary(Size: Integer); virtual;//�����ֵ�
    function IsIn(Name: string): Boolean; virtual;     //�ж�Name�Ƿ����ֵ���
    function FindNode(Name: string): TCnDockClientHashNode; virtual;//���ҽڵ�
    function Find(Name: string): Pointer; virtual;     //����
    function Insert(Name: string; Data: Pointer): Integer; virtual;//����
    procedure Remove(Name: string); virtual;           //ɾ��
    procedure MakeEmpty;                               //��ɢ�б�Ϊ��
    property CurrentSize: Integer read FCurrentSize;
    property TableSize: Integer read FTableSize write SetTableSize;
  end;

implementation

uses
  CnDockGlobal, SysUtils;

{ TCnDockControlHashTable }

function TCnDockControlHashTable.CompareKey(Key1, Key2: string): Integer;
begin
  Result := AnsiStrComp(PChar(Key1), PChar(Key2));
end;

constructor TCnDockControlHashTable.Create(Size: Integer; RiseExcept: Boolean);
begin
  { ���ȴ���Ͱ }
  CreateDictionary(Size);
  FRiseException := RiseExcept;
end;

procedure TCnDockControlHashTable.CreateDictionary(Size: Integer);
begin
  FEntryList := TList.Create;
  FEntryList.Count := Size;
  FTableSize := Size;
end;

function TCnDockControlHashTable.CreateKeyNode(KeyName: string;
  KeyData: Pointer; ListIndex: Integer): TCnDockClientHashNode;
begin
  Result := TCnDockClientHashNode.Create;
  Result.KeyName := KeyName;
  Result.KeyData := KeyData;
  Result.ListIndex := ListIndex;
end;

procedure TCnDockControlHashTable.DeleteListIndex(Index: Integer);
var Node, NextNode: TCnDockClientHashNode;
begin
  Node := FEntryList[Index];
  while Node <> nil do
  begin
    NextNode := Node.NextNode;
    Node.Free;
    Node := NextNode;
  end;
  FEntryList.Delete(Index);
end;

destructor TCnDockControlHashTable.Destroy;
begin
  MakeEmpty;
  FEntryList.Free;
  inherited;
end;

function TCnDockControlHashTable.Find(Name: string): Pointer;
var Node: TCnDockClientHashNode;
begin
  Node := FindNode(Name);
  if Node <> nil then
    Result := Node.KeyData
  else Result := nil;
end;

function TCnDockControlHashTable.FindNode(
  Name: string): TCnDockClientHashNode;
var Value: Integer;
  ListIndex: Integer;
begin
  ListIndex := HashProc(Name);
  Assert((ListIndex >= 0) and (ListIndex < FTableSize), gs_CnTableIndexError);
  Result := FEntryList[ListIndex];
  if Result = nil then Exit;
  repeat
    Value := CompareKey(Name, Result.FKeyName);
    if Value = 0 then Exit;
    Result := Result.FNextNode;
  until Result = nil;
end;

function TCnDockControlHashTable.HashProc(Name: string): Integer;
var i: Integer;
begin
  Result := 0;
  for i := 1 to Length(Name) do
    Inc(Result, Ord(Name[i]));
  Result := Result mod FTableSize;
end;

function TCnDockControlHashTable.Insert(Name: string;
  Data: Pointer): Integer;
var Index: Integer;
  Value: Integer;
  Node, ParentNode: TCnDockClientHashNode;
begin
  Result := -1;
  Index := HashProc(Name);
  Assert((Index >= 0) and (Index < FTableSize), gs_CnTableIndexError);
  { ���Ȳ�����Ͱ�����Ƿ������� }
  if FEntryList[Index] = nil then
    FEntryList[Index] := CreateKeyNode(Name, Data, Index)
  else
  begin
    Node := FEntryList[Index];
    repeat
      { �ж���ɢ�������Ƿ�����ͬ�ļ�ֵ }
      Value := CompareKey(Name, Node.FKeyName);
      { �����쳣 }
      if FRiseException then
        Assert(Value <> 0, gs_CnNodeExistedError)
      else if Value = 0 then
        Exit;
      ParentNode := Node;
      Node := Node.FNextNode;
    until Node = nil;
    { �����ڵ� }
    Node := CreateKeyNode(Name, Data, Index);
    Node.FPrevNode := ParentNode;
    ParentNode.NextNode := Node;
  end;
  Result := Index;
end;

function TCnDockControlHashTable.IsIn(Name: string): Boolean;
begin
  Result := FindNode(Name) <> nil;
end;

procedure TCnDockControlHashTable.MakeEmpty;
var i: Integer;
begin
  for i := FEntryList.Count - 1 downto 0 do
    DeleteListIndex(i);
end;

procedure TCnDockControlHashTable.Remove(Name: string);
var Node: TCnDockClientHashNode;
begin
  Node := FindNode(Name);
  if Node <> nil then
  begin
    if Node.FPrevNode <> nil then
      Node.FPrevNode.FNextNode := Node.FNextNode
    else FEntryList[Node.ListIndex] := Node.FNextNode;
    if Node.FNextNode <> nil then
      Node.FNextNode.FPrevNode := Node.FPrevNode;
    Node.Free;
  end;
end;

procedure TCnDockControlHashTable.SetTableSize(const Value: Integer);
begin
  FEntryList.Count := Value;
end;

end.
