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

unit CnContainers;
{* |<PRE>
================================================================================
* ������ƣ�������������
* ��Ԫ���ƣ��������ʵ��
* ��Ԫ���ߣ�С��
* ��    ע���򵥵���������࣬��βPush����ͷPop�����������Ƕ��󣨱�ת����ָ�룩��
*           ����ʱ�ڲ��л�����ƣ��������ⲿͨ���ٽ������⡣�������ӣ�
*           ������
*           var
*             Q: TCnQueue;
*
*           ������
*             Q := TCnQueue.Create;
*            
*           ʹ�ã�
*
*           var
*             TmpObj: TObject;
*           begin
*             TmpObj := TObject.Create;
*             Q.Push(Data); // �������β
*           end;
*            
*           var
*             TmpObj: TObject;
*           begin
*             TmpObj := TObject(Q.Pop); // �Ӷ���ͷ��ȡ��
*             TmpObj.Free;
*           end;
*
*           �ͷţ�
*             Q.Free;
* ����ƽ̨��PWinXP + Delphi 7
* ���ݲ��ԣ�PWin2000/XP + Delphi 5/6/7
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2017.01.17 V1.2
*               ���� TCnObjectRingBuffer ѭ��������ʵ��
*           2016.12.02 V1.1
*               ���� TCnObjectStack ʵ�֣����� Clear �ȷ���
*           2008.04.30 V1.0
*               С���ԭʼ������ֲ������
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, SysUtils, Classes;

type
  TCnQueue = class
  private
    FMultiThread: Boolean;
    FHead: TObject;
    FTail: TObject;
    FSize: Integer;
    FLock: TRTLCriticalSection;
    procedure FreeNode(Value: TObject);
    function GetSize: Integer;
  public
    constructor Create(MultiThread: Boolean = False);
    destructor Destroy; override;
    procedure Push(Data: Pointer);
    function Pop: Pointer;
    property Size: Integer read GetSize;
  end;

  TCnObjectStack = class(TObject)
  private
    FList: TList;
  public
    constructor Create;
    destructor Destroy; override;

    function Count: Integer;
    function IsEmpty: Boolean;
    procedure Clear;

    procedure Push(AObject: TObject);
    function Pop: TObject;
    function Peek: TObject;
  end;

  ECnRingBufferFullException = class(Exception);

  ECnRingBufferEmptyException = class(Exception);

  TCnObjectRingBuffer = class(TObject)
  {* ѭ�����л�����}
  private
    FFullOverwrite: Boolean;
    FMultiThread: Boolean;
    FSize: Integer;
    FList: TList;
    FLock: TRTLCriticalSection;
    // Idx �������Ϊʼ��ָ������λ���м�ķ죬��Ŵӵ� 0 ���� Size - 1 ( �� Size Ҳ�����ڵ� 0 )
    // ��Ԫ�ص�����£�FrontIdx �ߺ�ʼ����Ԫ�أ�ǰ�����ǿգ����ƻ�����β��
    //                 BackIdx �ĵ�ǰʼ����Ԫ�أ�������ǿգ����ƻ�����ͷ
    // ��Ԫ�ص�����£�FrontIdx �� BackIdx ���
    FFrontIdx: Integer;
    FBackIdx: Integer;
    FCount: Integer;
    function GetCount: Integer;
  public
    constructor Create(ASize: Integer; AFullOverwrite: Boolean = False;
      AMultiThread: Boolean = False);
    {* ���캯����ASize �ǻ�����������AFullOverwrite �Ƿ���������������������ʱ
      ������ǰ�����ݣ�AMultiThread �Ƿ���Ҫ���̻߳���}
    destructor Destroy; override;
    {* ��������}

    procedure PushToFront(AObject: TObject);
    {* ��ѭ�����л�����ǰ������һ�� Object��ǰ����ָ�ڲ��洢�����͵�һ�ˣ������Ҳ������������쳣}
    function PopFromBack: TObject;
    {* ��ѭ�����л������󷽵���һ�� Object������ָ�ڲ��洢�����ߵ�һ�ˣ��޿ɵ������쳣}

    procedure PushToBack(AObject: TObject);
    {* ��ѭ�����л�����������һ�� Object������ָ�ڲ��洢�����ߵ�һ�ˣ������Ҳ������������쳣}
    function PopFromFront: TObject;
    {* ��ѭ�����л�����ǰ������һ�� Object��ǰ����ָ�ڲ��洢�����͵�һ�ˣ��޿ɵ������쳣}

    procedure Dump(List: TList; out FrontIdx: Integer; out BackIdx: Integer);
    {* ��ȫ�����ݵ�����һ TList���Լ�ָ��λ��}

    property FullOverwrite: Boolean read FFullOverwrite;
    {* ��ѭ�����л�������ʱ�Ƿ������Ǿ�����}
    property MultiThread: Boolean read FMultiThread;
    {* ��ѭ�����л������Ƿ���Ҫ֧�ֶ��̲߳������ʣ�Ϊ True ʱ�ڲ����ٽ�������}
    property Size: Integer read FSize;
    {* ��ѭ�����л������ĳߴ�}
    property Count: Integer read GetCount;
    {* ��ѭ�����л������ڵ���ЧԪ������}
  end;

implementation

type
  TCnNode = class
  private
    FNext: TCnNode;
    FData: Pointer;
  public
    property Next: TCnNode read FNext write FNext;
    property Data: Pointer read FData write FData;
  end;

{ TCnQueue }

procedure TCnQueue.FreeNode(Value: TObject);
var
  Tmp: TCnNode;
begin
  Tmp := TCnNode(Value).Next;
  TCnNode(Value).Free;
  if Tmp = nil then
    Exit;
  FreeNode(Tmp);
end;

constructor TCnQueue.Create(MultiThread: Boolean);
begin
  FMultiThread := MultiThread;
  FHead := nil;
  FTail := nil;
  FSize := 0;
  if FMultiThread then
    InitializeCriticalSection(FLock);
end;

destructor TCnQueue.Destroy;
begin
  if FHead <> nil then
    FreeNode(FHead);
  if FMultiThread then
    DeleteCriticalSection(FLock);
  inherited;
end;

function TCnQueue.Pop: Pointer;
var
  Tmp: TCnNode;
begin
  if FMultiThread then
    EnterCriticalSection(FLock);
  try
    Result := nil;
    if FHead = nil then
      Exit;

    Result := TCnNode(FHead).Data;
    Tmp := TCnNode(FHead).Next;
    TCnNode(FHead).Free;
    FHead := Tmp;
    
    if Tmp = nil then
      FTail := nil;
    FSize := FSize - 1;
  finally
    if FMultiThread then
      LeaveCriticalSection(FLock);
  end;
end;

procedure TCnQueue.Push(Data: Pointer);
var
  Tmp: TCnNode;
begin
  if FMultiThread then
    EnterCriticalSection(FLock);
  try
    if Data = nil then Exit;
    Tmp := TCnNode.Create;
    Tmp.Data := Data;
    Tmp.Next := nil;
    
    if FTail = nil then
    begin
      FTail := Tmp;
      FHead := Tmp;
    end
    else
    begin
      TCnNode(FTail).Next := Tmp;
      FTail := Tmp
    end;
    
    FSize := FSize + 1;
  finally
    if FMultiThread then
      LeaveCriticalSection(FLock);
  end;
end;

function TCnQueue.GetSize: Integer;
begin
  Result := FSize;
end;

{ TCnObjectStack }

procedure TCnObjectStack.Clear;
begin
  FList.Clear;
end;

function TCnObjectStack.Count: Integer;
begin
  Result := FList.Count;
end;

constructor TCnObjectStack.Create;
begin
  FList := TList.Create;
end;

destructor TCnObjectStack.Destroy;
begin
  FList.Free;
  inherited;
end;

function TCnObjectStack.IsEmpty: Boolean;
begin
  Result := FList.Count = 0;
end;

function TCnObjectStack.Peek: TObject;
begin
  Result := TObject(FList[FList.Count - 1]);
end;

function TCnObjectStack.Pop: TObject;
begin
  Result := TObject(FList[FList.Count - 1]);
  FList.Delete(FList.Count - 1);
end;

procedure TCnObjectStack.Push(AObject: TObject);
begin
  FList.Add(AObject);
end;

{ TCnRingBuffer }

constructor TCnObjectRingBuffer.Create(ASize: Integer; AFullOverwrite,
  AMultiThread: Boolean);
begin
  Assert(ASize > 0);

  FSize := ASize;
  FFullOverwrite := AFullOverwrite;
  FMultiThread := AMultiThread;

  FList := TList.Create;
  FList.Count := FSize;

  if FMultiThread then
    InitializeCriticalSection(FLock);
end;

destructor TCnObjectRingBuffer.Destroy;
begin
  if FMultiThread then
    DeleteCriticalSection(FLock);
  FList.Free;
  inherited;
end;

procedure TCnObjectRingBuffer.Dump(List: TList; out FrontIdx: Integer; out BackIdx: Integer);
var
  I: Integer;
begin
  FrontIdx := FFrontIdx;
  BackIdx := FBackIdx;
  if List <> nil then
  begin
    List.Clear;
    for I := 0 to FList.Count - 1 do
      List.Add(FList[I]);
  end;
end;

function TCnObjectRingBuffer.GetCount: Integer;
begin
  Result := FCount;
end;

function TCnObjectRingBuffer.PopFromBack: TObject;
begin
  Result := nil;
  if FMultiThread then
    EnterCriticalSection(FLock);

  try
    if FCount <= 0 then
      raise ECnRingBufferEmptyException.Create('Ring Buffer Empty. Can NOT Pop From Back.');

    Dec(FBackIdx);
    if FBackIdx < 0 then
      FBackIdx := FSize - 1;
    Result := FList[FBackIdx];
    FList[FBackIdx] := nil;
    Dec(FCount);
  finally
    if FMultiThread then
      LeaveCriticalSection(FLock);
  end;
end;

function TCnObjectRingBuffer.PopFromFront: TObject;
begin
  Result := nil;
  if FMultiThread then
    EnterCriticalSection(FLock);

  try
    if FCount <= 0 then
      raise ECnRingBufferEmptyException.Create('Ring Buffer Empty. Can NOT Pop From Front.');

    Result := FList[FFrontIdx];
    FList[FFrontIdx] := nil;

    Inc(FFrontIdx);
    if FFrontIdx >= FSize then
      FFrontIdx := 0;
    Dec(FCount);
  finally
    if FMultiThread then
      LeaveCriticalSection(FLock);
  end;
end;

procedure TCnObjectRingBuffer.PushToBack(AObject: TObject);
begin
  if FMultiThread then
    EnterCriticalSection(FLock);

  try
    if not FFullOverwrite and (FCount >= FSize) then
      raise ECnRingBufferFullException.Create('Ring Buffer Full. Can NOT Push To Back.');

    FList[FBackIdx] := AObject;
    Inc(FBackIdx);
    if FBackIdx >= FSize then
      FBackIdx := 0;

    if FCount < FSize then
      Inc(FCount);
  finally
    if FMultiThread then
      LeaveCriticalSection(FLock);
  end;
end;

procedure TCnObjectRingBuffer.PushToFront(AObject: TObject);
begin
  if FMultiThread then
    EnterCriticalSection(FLock);

  try
    if not FFullOverwrite and (FCount >= FSize) then
      raise ECnRingBufferFullException.Create('Ring Buffer Full. Can NOT Push To Front.');

    Dec(FFrontIdx);
    if FFrontIdx < 0 then
      FFrontIdx := FSize - 1;
    FList[FFrontIdx] := AObject;

    if FCount < FSize then
      Inc(FCount);
  finally
    if FMultiThread then
      LeaveCriticalSection(FLock);
  end;
end;

end.
