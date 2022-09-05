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

unit CnIocpSimpleMemPool;
{* |<PRE>
================================================================================
* ������ƣ�����ͨѶ�����
* ��Ԫ���ƣ�Windows ��ɶ˿ڣ�IOCP�����ʹ�õļ��ڴ��ʵ�ֵ�Ԫ
* ��Ԫ���ߣ�cnwinds
*           ����(cxmld@126.com) ��ֲ�޸�
* ��    ע��
*   1.TCnMemPoolMgr ���ǲ�ͬ�ߴ���ڴ�ع����ʵ�֡�
*     CnMemPoolMgr �� TCnMemPoolMgr ���ȫ�ֶ������������ͨ���ö���ʹ���ڴ�ء�
*     ������С��MemorySize����ͬ���ڴ�飨TCnMemoryBlockItem������һ����TCnMemoryTypeItem���н��й���
*     TCnMemPoolMgr ���й�������ͬ�ߴ���ڴ����Ϳ飨TCnMemoryTypeItem����
*     һ���ڴ����Ϳ飨TCnMemoryTypeItem���а����˶���ڴ�飨TCnMemoryBlockItem����
*     ��ֵ��Threshold��������һ�� TMemoryTypeItem ���ڴ��ĸ�����
*       ��ϵͳƵ�������ڴ���ʱ���ܸ����������ֵ��
*       ��ϵͳ���ڴ��Ĳ���ʹ����������ֵ��ʱ���ͷ��ڴ�飬���ܸ���������ֵ��
*       ������Կ��Ա��ⷱæ��ʱ��Ƶ�����롢�ͷ��ڴ棬���߿��е�ʱ���˷��ڴ档
*   2.TCnMemoryPool ��һ���ؼ���Ϊ���ܿ��ӻ��������������ࡣ
*     ���ܳ��ֶ���ؼ����ڴ���С��ͬ����������Ӧ��ͬһ���ڴ����Ϳ飨TCnMemoryTypeItem��
*     ���������������ؼ��������ڴ����Ϳ飨TCnMemoryTypeItem���е��ڴ�顣��ֵ��ȡ�������õ����ֵ��
*   ��֪���⣺����ʱ��Ҫ������Ч�ʽϵ�
TODO >>>
*   1.TCnIocpMemPool ��������һ��������GetFreeMemoryType����ȡһ�����е��ڴ�����
*   2.TCnIocpMemPool ������ڴ��С�ǹ̶�,���ֵ��ÿһ�η������
*   3.���� TCnIocpSimpleMemPool����װ TCnIocpMemPool�Ĺ���, �������ڴ�͹黹�ڴ�
*   4.ʹ��"����"��"�黹"��Ϊ������������"�����ڴ�"��"�ͷ��ڴ�"
*   5.TCnIocpSimpleMemPool��Ӧһ���ڴ�����, ÿ���ڴ����͵�����ֵ���Զ���ȡ
*   6.�����Զ�������(����������ò�����):"�ڴ��"��"�ڴ����Ϳ�"
*     ÿ�� TCnIocpSimpleMemPool ��Ӧһ���ڴ����Ϳ�, ���ɶ��"�ڴ��"���.ÿ���û�
*     ���þ��ǵõ�һ������"�ڴ��", ��С�ɵ�һ������ʱȷ��.
*     TCnIocpMemPool�����˶��  "�ڴ����Ϳ�", ��ÿע��һ��ʱ,�ͷ���һ��"�ڴ����Ϳ�"
TODO >>>
*
* ����ƽ̨��PWin2000Pro + Delphi 7.01
* ���ݲ��ԣ�PWin9X/2000/XP + Delphi 5/6/7 + C++Builder 5/6
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2008.09.16 V1.0
*                ������Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, SyncObjs, Windows, Controls;

const
  SCnErrorNotRegister = 'û��ע����ڴ�����(%d)!';
  SCnErrorBlockNotRent = '�ڴ��û�б������';
  SCnErrorBlockUnknow = 'û�и��ڴ�飡';

type
  TCreateMemoryEvent = procedure(Sender: TObject; var MemoryPtr: Pointer) of object;

  TFreeMemoryEvent = procedure(Sender: TObject; MemoryPtr: Pointer) of object;

  TCnMemoryBlockItem = record
  {* �ڴ��ͷ}
    MemoryBlockPtr: Pointer;              // ���ظ��û����ڴ��ָ��
    RentTime: Cardinal;                   // ���õ�ʱ�� TickCount
    IsRent: Boolean;                      // �Ƿ��ѱ����ȥ
    RentCount: Cardinal;                  // �ÿ����õ��ܴ���
    Size: Cardinal;                       // �ڴ���С����Ӧ TypeItem �еĿ��С
  end;
  PCnMemoryBlockItem = ^TCnMemoryBlockItem;

  TCnMemoryTypeItem = record
  {* �ڴ����Ϳ�ͷ}
    RefCount: Cardinal;                   // �����Ϳ�����ô�������˵ע�����
    MemorySize: Cardinal;                 // �������ڴ��Ĵ�С
    CreateMemoryProc: TCreateMemoryEvent; // �����ڴ�ķ���ָ��
    FreeMemoryProc: TFreeMemoryEvent;     // �ͷ��ڴ�ķ���ָ��  -- ��С�������������ͷŷ������Ψһ��ʶ���
    Threshold: Cardinal;                  // �ڴ���������ֵ
                                          // �������Ŀ������ڸ�ֵ��Ҫ��������
    IdelCount: Cardinal;                  // �����Ϳ�ӵ�еĿ����ڴ��ĸ���
    Lock: TCriticalSection;               // ������
    MemoryBlockList: TList;               // �ڴ���б������� PCnMemoryBlockItem
  end;
  PCnMemoryTypeItem = ^TCnMemoryTypeItem;

  TCnSimpleMemPoolMgr = class
  private
    FLock: TCriticalSection;
    FMemoryTypeList: TList;
    function RegisterMemoryTypeItem(MemorySize: Cardinal; CreateMemoryProc:
      TCreateMemoryEvent; FreeMemoryProc: TFreeMemoryEvent): PCnMemoryTypeItem;
    {* ע���ڴ����Ϳ飨�ڲ����߳�����}

    procedure UnregisterMemoryTypeItem(MemoryTypeItem: PCnMemoryTypeItem);
    {* ע���ڴ����Ϳ�}

    function CreateMemoryBlockItem(MemoryTypeItem: PCnMemoryTypeItem): PCnMemoryBlockItem;
    {* ��}
    procedure FreeMemoryBlockItem(MemoryTypeItem: PCnMemoryTypeItem;
      MemoryBlockItem: PCnMemoryBlockItem);

    function FindMemoryTypeItem(MemorySize: Cardinal; CreateMemoryProc: TCreateMemoryEvent;
      FreeMemoryProc: TFreeMemoryEvent): PCnMemoryTypeItem;

    procedure Clear;
  public
    constructor Create;
    destructor Destroy; override;

    function RegisterMemoryType(MemorySize: Cardinal;
                                CreateMemoryProc: TCreateMemoryEvent;
                                FreeMemoryProc: TFreeMemoryEvent): PCnMemoryTypeItem;
    {* ע���ڴ����Ϳ� ����:�ڴ�����, �������ͷŷ���ָ��
       ������ָ�����¼�֪ͨ��ͬʱ�����Զ�������ڴ����ͷ��ڴ�ķ���}

    procedure UnregisterMemoryType(MemoryTypeItem: PCnMemoryTypeItem);
    {* ע���ڴ����Ϳ�}

    procedure SetThreshold(MemoryTypeItem: PCnMemoryTypeItem; Threshold: Cardinal);
    {* ���������ڴ�����ֵ��
      ��ֵ�����޵�����
        ��ֵ��ʾ��ϵͳ���е�ʱ���鲻Ҫ������ֵ�����ޱ�ʾ�κ�ʱ�򶼲��ܳ�����ֵ��
    }

    procedure RentMemory(MemoryTypeItem: PCnMemoryTypeItem; var MemoryPtr: Pointer);
    {* ����һ���ڴ�}
    procedure ReturnMemory(MemoryTypeItem: PCnMemoryTypeItem; MemoryPtr: Pointer);
    {* ����һ���ڴ�}
  end;

  TCnCustomSimpleMemPool = class (TComponent)
  private
    FMemorySize: Cardinal;
    FThreshold : Cardinal;
    FOnCreateMemory : TCreateMemoryEvent;
    FOnFreeMemory   : TFreeMemoryEvent;
    FMemTypeItem : PCnMemoryTypeItem;  // ע�᷵�ص��ڴ����Ϳ�ָ��
    FRegistered: Boolean;              // �Ƿ��Ѿ�ע�ᵽ�ڴ�ع�������

    procedure EnsureRegister;
    procedure DoRegister;
    procedure DoUnregister;

    procedure SetThreshold(const Value: Cardinal);
    procedure SetMemorySize(const Value: Cardinal);
    procedure SetOnCreateMemory(const Value: TCreateMemoryEvent);
    procedure SetOnFreeMemory(const Value: TFreeMemoryEvent);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure RentMemory(var MemoryPtr: Pointer);
    {* �����ڴ�}
    procedure ReturnMemory(MemoryPtr: Pointer);
    {* �黹�ڴ�}
  public
    property MemorySize: Cardinal read FMemorySize write SetMemorySize;
    {* �ڴ��Ĵ�С} 
    property Threshold : Cardinal read FThreshold write SetThreshold;
    {* �ڴ���������ֵ���������ֵ�����ɷ��������ڴ�飩}
    property OnCreateMemory : TCreateMemoryEvent read FOnCreateMemory write SetOnCreateMemory;
    {* �Զ�����ϵͳ�з����ڴ�ķ�����Ĭ��ʵ�ֲ��� GetMemory}
    property OnFreeMemory: TFreeMemoryEvent read FOnFreeMemory write SetOnFreeMemory;
    {* �Զ�����ϵͳ���ͷ��ڴ�ķ�����Ĭ��ʵ�ֲ��� FreeMemory}
  end;

  TCnIocpSimpleMemPool = class(TCnCustomSimpleMemPool)
  published
    property MemorySize;
    {* �ڴ��Ĵ�С}
    property Threshold;
    {* �ڴ���������ֵ���������ֵ,���ɷ��������ڴ�飩}
    property OnCreateMemory;
    {* �Զ�����ϵͳ�з����ڴ�ķ�����Ĭ��ʵ�ֲ��� GetMemory}
    property OnFreeMemory;
    {* �Զ�����ϵͳ���ͷ��ڴ�ķ�����Ĭ��ʵ�ֲ��� FreeMemory}
  end;

implementation

var
  CnSimpleMemPoolMgr: TCnSimpleMemPoolMgr;

{ TCnSimpleMemPoolMgr }

constructor TCnSimpleMemPoolMgr.Create;
begin
  FMemoryTypeList := TList.Create;
  FLock := TCriticalSection.Create;
end;

destructor TCnSimpleMemPoolMgr.Destroy;
begin
  Clear;
  FreeAndNil(FMemoryTypeList);
  FreeAndNil(FLock);
  inherited;
end;

procedure TCnSimpleMemPoolMgr.Clear;
var
  TypeItem: PCnMemoryTypeItem;
  I: Integer;
begin
  // ��������ڴ��
  FLock.Enter;
  try
    for I := 0 to FMemoryTypeList.Count - 1 do
    begin
      TypeItem := PCnMemoryTypeItem(FMemoryTypeList[I]);
      UnregisterMemoryTypeItem(TypeItem);
    end;
  finally
    FLock.Release;
  end;
end;

function TCnSimpleMemPoolMgr.RegisterMemoryTypeItem(MemorySize: Cardinal;
  CreateMemoryProc: TCreateMemoryEvent; FreeMemoryProc: TFreeMemoryEvent): PCnMemoryTypeItem;
begin
  Result := New(PCnMemoryTypeItem);
  Result^.Lock := TCriticalSection.Create;
  Result^.RefCount := 1;
  Result^.MemorySize := MemorySize;
  Result^.MemoryBlockList := TList.Create;
  Result^.CreateMemoryProc := CreateMemoryProc;
  Result^.FreeMemoryProc := FreeMemoryProc;
  Result^.Threshold := 20;
  Result^.IdelCount := 0;
end;

procedure TCnSimpleMemPoolMgr.UnregisterMemoryTypeItem(MemoryTypeItem: PCnMemoryTypeItem);
var
  I: Integer;
begin
  for I := 0 to MemoryTypeItem^.MemoryBlockList.Count - 1 do
    FreeMemoryBlockItem(MemoryTypeItem, MemoryTypeItem^.MemoryBlockList[I]);
  FreeAndNil(MemoryTypeItem^.Lock);
  FreeAndNil(MemoryTypeItem^.MemoryBlockList);
  Dispose(MemoryTypeItem);
end;

function TCnSimpleMemPoolMgr.CreateMemoryBlockItem(
  MemoryTypeItem: PCnMemoryTypeItem): PCnMemoryBlockItem;
var
  Size: Integer;
begin
  Size := MemoryTypeItem^.MemorySize;
  // �����ڴ��
  Result := New(PCnMemoryBlockItem);
  // �����ڴ档���û�����ûص�������ʹ�� GetMemory �����ڴ档
  if (Assigned(MemoryTypeItem.CreateMemoryProc)) then
    MemoryTypeItem^.CreateMemoryProc(Self, Result^.MemoryBlockPtr)
  else
    Result^.MemoryBlockPtr := GetMemory(Size);
  Result^.RentTime := 0;
  Result^.IsRent := False;
  Result^.RentCount := 0;
  Result^.Size := Size;
end;

procedure TCnSimpleMemPoolMgr.FreeMemoryBlockItem(MemoryTypeItem: PCnMemoryTypeItem;
  MemoryBlockItem: PCnMemoryBlockItem);
begin
  // �ͷ��ڴ�
  if (Assigned(MemoryTypeItem.FreeMemoryProc)) then
    MemoryTypeItem.FreeMemoryProc(Self, MemoryBlockItem^.MemoryBlockPtr)
  else
    FreeMemory(MemoryBlockItem^.MemoryBlockPtr);
  // �ͷ��ڴ��
  Dispose(MemoryBlockItem);
end;

function TCnSimpleMemPoolMgr.FindMemoryTypeItem(MemorySize: Cardinal;
  CreateMemoryProc: TCreateMemoryEvent; FreeMemoryProc: TFreeMemoryEvent): PCnMemoryTypeItem;
var
  I: Integer;
begin
  FLock.Enter;
  try
    for I := 0 to FMemoryTypeList.Count - 1 do
    begin       
      Result := PCnMemoryTypeItem(FMemoryTypeList[I]);
      if (Result^.MemorySize = MemorySize) and
        (@Result^.CreateMemoryProc = @CreateMemoryProc) and
        (@Result^.FreeMemoryProc = @FreeMemoryProc) then Exit;
    end;
    Result := nil;
  finally
    FLock.Release;
  end;
end;

function TCnSimpleMemPoolMgr.RegisterMemoryType(MemorySize: Cardinal;
  CreateMemoryProc: TCreateMemoryEvent; FreeMemoryProc: TFreeMemoryEvent): PCnMemoryTypeItem;
begin
  Result := FindMemoryTypeItem(MemorySize, CreateMemoryProc, FreeMemoryProc);
  if Result = nil then                      //������,�ʹ���
  begin
    Result := RegisterMemoryTypeItem(MemorySize, CreateMemoryProc, FreeMemoryProc);
    FLock.Enter;
    try
      FMemoryTypeList.Add(Result);            //������List��
    finally
      FLock.Release;  
    end;
  end else
  begin
    Inc(Result^.RefCount);                  //�������������ü���
  end;
end;

procedure TCnSimpleMemPoolMgr.UnregisterMemoryType(MemoryTypeItem: PCnMemoryTypeItem);
begin
  // �������ü���
  Dec(MemoryTypeItem^.RefCount);
  if MemoryTypeItem^.RefCount > 0 then
    Exit;

  // �������������
  FLock.Enter;
  try
    FMemoryTypeList.Remove(MemoryTypeItem);
  finally
    FLock.Release;
  end;
  UnregisterMemoryTypeItem(MemoryTypeItem);
end;

procedure TCnSimpleMemPoolMgr.SetThreshold(MemoryTypeItem: PCnMemoryTypeItem;
  Threshold: Cardinal);
begin
  // ���һ�� MemoryTypeItem �ж�� Pool �������������ʹ��������ֵ
  if MemoryTypeItem <> nil then
  begin
    if MemoryTypeItem^.RefCount = 1 then
      MemoryTypeItem^.Threshold := Threshold
    else
      if MemoryTypeItem^.Threshold < Threshold then
        MemoryTypeItem^.Threshold := Threshold;
  end;
end;

procedure TCnSimpleMemPoolMgr.RentMemory(MemoryTypeItem: PCnMemoryTypeItem; var MemoryPtr: Pointer);
var
  BlockItem: PCnMemoryBlockItem;
begin
  // ����Ҫѭ�����ң�ֻҪ�ҵ���һ���ڴ�飬������������ʾ�����ڴ�鶼�Ѿ��������ˡ�
  MemoryTypeItem^.Lock.Enter;
  try
    if MemoryTypeItem^.MemoryBlockList.Count > 0 then
    begin
      BlockItem := PCnMemoryBlockItem(MemoryTypeItem^.MemoryBlockList[0]);
      if not BlockItem^.IsRent then      // �� 0 ���ڴ���Ƿ�������
      begin
        MemoryTypeItem^.MemoryBlockList.Remove(BlockItem);
        MemoryTypeItem^.MemoryBlockList.Add(BlockItem);  //���ڴ�����·��뵽LIST�����
        MemoryPtr := BlockItem.MemoryBlockPtr;     //ȡ���ڴ���ָ��
        Inc(BlockItem^.RentCount);                 //��������+1
        BlockItem^.RentTime := GetTickCount;       //����ʱ��
        BlockItem^.IsRent := True;                 //�����ñ�־

        //�����ڴ�������һ
        Dec(MemoryTypeItem^.IdelCount);

        Exit;
      end;
    end;

    // �´���һ���ڴ�飬�ҵ���β
    BlockItem := CreateMemoryBlockItem(MemoryTypeItem);
    MemoryPtr := BlockItem^.MemoryBlockPtr;
    Inc(BlockItem^.RentCount);
    BlockItem^.RentTime := GetTickCount;
    BlockItem^.IsRent := True;
    BlockItem^.Size := MemoryTypeItem^.MemorySize;
    MemoryTypeItem^.MemoryBlockList.Add(BlockItem);
  finally
    MemoryTypeItem^.Lock.Release;
  end;
end;

procedure TCnSimpleMemPoolMgr.ReturnMemory(MemoryTypeItem: PCnMemoryTypeItem; MemoryPtr: Pointer);
var
  I: Integer;
  BlockItem: PCnMemoryBlockItem;
  ReleaseCount: Cardinal;
  UsedCount: Cardinal;
  TotalCount: Cardinal;
begin
  //���ڴ��ĵ�������ǿ���Եģ�ԭ�����ڷ����ʱ�����һ���ڿ�ĸ���
  MemoryTypeItem^.Lock.Enter;
  try
    ReleaseCount := 0;
    //�ж��Ƿ�Ҫɾ���ڴ��
    TotalCount := MemoryTypeItem^.MemoryBlockList.Count;
    if TotalCount > MemoryTypeItem^.Threshold then
    begin
      UsedCount := TotalCount - MemoryTypeItem^.IdelCount;
      if UsedCount < MemoryTypeItem^.Threshold then
      begin
        //����Ҫɾ���ڴ��ĸ���
        //����ʾһ��Ҫɾ����ô����ڴ�飬��������»�ɾ����ô���ڴ��
        ReleaseCount := TotalCount - MemoryTypeItem^.Threshold;
      end;
    end;
    
    for I := MemoryTypeItem^.MemoryBlockList.Count - 1 downto 0 do
    begin
      BlockItem := PCnMemoryBlockItem(MemoryTypeItem^.MemoryBlockList[I]);
      if MemoryPtr = BlockItem^.MemoryBlockPtr then         //��ѯ�ڴ��(�Ƚϵ�ַ��ͬ)
      begin
        if BlockItem^.IsRent then
        begin
          //�黹�ڴ��
          BlockItem^.RentTime := 0;
          BlockItem^.IsRent := False;
          MemoryTypeItem^.MemoryBlockList.Remove(BlockItem);
          MemoryTypeItem^.MemoryBlockList.Insert(0, BlockItem);  //���뵽��0��
          //�����ڴ�������һ
          Inc(MemoryTypeItem^.IdelCount);
          Exit;
        end 
        else
          raise Exception.Create(SCnErrorBlockNotRent); //û�б������쳣 
      end;
      //�ͷ��ڴ��
      if (ReleaseCount <> 0) and (not BlockItem^.IsRent) then
      begin
        FreeMemoryBlockItem(MemoryTypeItem, BlockItem);
        MemoryTypeItem^.MemoryBlockList.Remove(BlockItem);
        Dec(ReleaseCount);
      end;
    end;
    raise Exception.Create(SCnErrorBlockUnknow);        //û���ҵ��ڴ���׳��쳣
  finally
    MemoryTypeItem^.Lock.Release;
  end;
end;

{ TCnIocpSimpleMemPool }

constructor TCnCustomSimpleMemPool.Create(AOwner: TComponent);
begin
  inherited;
  FThreshold := 20;
  FMemorySize := 1024;
  FRegistered := False;
  // ʹ���ӳ�ע�᷽ʽ�������ʼ��������ɷ���ע��
  // DoRegister;
end;

destructor TCnCustomSimpleMemPool.Destroy;
begin
  DoUnregister;
  inherited;
end;

procedure TCnCustomSimpleMemPool.EnsureRegister;
begin
  if not FRegistered then
    DoRegister;
end;

procedure TCnCustomSimpleMemPool.DoRegister;
begin
  if (not (csDesigning in ComponentState)) and (not FRegistered) then
  begin
    FMemTypeItem := CnSimpleMemPoolMgr.RegisterMemoryType(
      FMemorySize, FOnCreateMemory, FOnFreeMemory);
    CnSimpleMemPoolMgr.SetThreshold(FMemTypeItem, Threshold);
    FRegistered := True;
  end;
end;

procedure TCnCustomSimpleMemPool.DoUnregister;
begin
  if FRegistered then
  begin
    CnSimpleMemPoolMgr.UnregisterMemoryType(FMemTypeItem);
    FMemTypeItem := nil;
    FRegistered := False;
  end;
end;

procedure TCnCustomSimpleMemPool.RentMemory(var MemoryPtr: Pointer);
begin
  EnsureRegister;
  CnSimpleMemPoolMgr.RentMemory(FMemTypeItem, MemoryPtr);
end;

procedure TCnCustomSimpleMemPool.ReturnMemory(MemoryPtr: Pointer);
begin
  EnsureRegister;
  CnSimpleMemPoolMgr.ReturnMemory(FMemTypeItem, MemoryPtr);
end;

procedure TCnCustomSimpleMemPool.SetMemorySize(const Value: Cardinal);
begin
  if FMemorySize <> Value then
  begin
    DoUnregister;
    FMemorySize := Value;
  end;
end;

procedure TCnCustomSimpleMemPool.SetThreshold(const Value: Cardinal);
begin
  if FThreshold <> Value then
  begin
    FThreshold := Value;
    if FRegistered then
      CnSimpleMemPoolMgr.SetThreshold(FMemTypeItem, FThreshold);
  end;
end;

procedure TCnCustomSimpleMemPool.SetOnCreateMemory(const Value: TCreateMemoryEvent);
begin
  if @FOnCreateMemory <> @Value then
  begin
    DoUnregister;
    FOnCreateMemory := Value;
  end;
end;

procedure TCnCustomSimpleMemPool.SetOnFreeMemory(const Value: TFreeMemoryEvent);
begin
  if @FOnFreeMemory <> @Value then
  begin
    DoUnregister;
    FOnFreeMemory := Value;
  end;
end;

initialization
  CnSimpleMemPoolMgr := TCnSimpleMemPoolMgr.Create;

finalization
  CnSimpleMemPoolMgr.Free;

end.
