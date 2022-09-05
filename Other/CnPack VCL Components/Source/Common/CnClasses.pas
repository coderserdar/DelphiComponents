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

unit CnClasses;
{* |<PRE>
================================================================================
* ������ƣ�������������
* ��Ԫ���ƣ������ඨ�嵥Ԫ
* ��Ԫ���ߣ��ܾ��� (zjy@cnpack.org)
* ��    ע���õ�Ԫ������������Ļ������
* ����ƽ̨��PWin98SE + Delphi 5.0
* ���ݲ��ԣ�PWin9X/2000/XP + Delphi 5/6
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2018.08.30 V1.4
*               ���� TCnUInt32List/TCnUInt64List ��
*           2003.03.02 V1.3
*               ���� TCnLockObject ��
*           2002.09.10 V1.2
*               �޸� TCnComponent ���ַ���
*           2002.07.09 V1.1
*               ������������
*           2002.04.08 V1.0
*               ���� TCnComponent �������
*           2002.01.11 V0.01Demo
*               ������Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, SysUtils, Classes, TypInfo,
  {$IFDEF COMPILER6_UP} RTLConsts, {$ELSE} Consts, {$ENDIF} CnNativeDecl;

type

//==============================================================================
// �����߳�ͬ���Ķ�����
//==============================================================================

{ TCnLockObject }

  TCnLockObject = class (TObject)
  {* �����߳�ͬ���Ķ�����}
  private
    FLock: TRTLCriticalSection;
    FLockCount: Integer;
    function GetLocking: Boolean;
  protected
    property LockCount: Integer read FLockCount;
    {* ��ǰLock������ֻ������}
  public
    constructor Create;
    {* �����������ڲ���һ�������ʵ��}
    destructor Destroy; override;
    procedure Lock;
    {* �����ٽ�����Ϊ��֤���߳�ͬ����������������Unlock�ɶ�ʹ��}
    function TryLock: Boolean;
    {* �����ǰLock����Ϊ�㣬����������棬���򷵻ؼ١�
       ��������棬�����ڲ�����ɺ����UnLock�ͷ���}
    procedure Unlock;
    {* �˳��ٽ������ͷ�ͬ������������Lock�ɶ�ʹ��}
    property Locking: Boolean read GetLocking;
    {* ȡ��ǰ����״̬}
  end;

//==============================================================================
// ʹ�� RTTI ʵ���� Assign ������ TPersistent ��
//==============================================================================

{ TCnAssignablePersistent }

  TCnAssignablePersistent = class(TPersistent)
  public
    procedure Assign(Source: TPersistent); override;
  end;

//==============================================================================
// ʹ�� RTTI ʵ���� Assign ������ TCollectionItem ��
//==============================================================================

{ TCnAssignableCollectionItem }

  TCnAssignableCollectionItem = class(TCollectionItem)
  public
    procedure Assign(Source: TPersistent); override;
  end;

//==============================================================================
// ʹ�� RTTI ʵ���� Assign ������ TCollection ��
//==============================================================================

{ TCnAssignableCollection }

  TCnAssignableCollection = class(TCollection)
  public
    procedure Assign(Source: TPersistent); override;
  end;

//==============================================================================
// ������֪ͨ���̰߳�ȫ�ĳ־�����
//==============================================================================

{ TCnPersistent }

  TCnPersistent = class(TPersistent)
  {* ������֪ͨ���̰߳�ȫ�ĳ־�����}
  private
    FUpdateCount: Integer;
    FOnChanging: TNotifyEvent;
    FOnChange: TNotifyEvent;
    FOwner: TPersistent;
    FLockObject: TCnLockObject;
    function GetLocking: Boolean;
    function GetLockObject: TCnLockObject;
  protected
    function GetOwner: TPersistent; override;
    procedure Changing; virtual;
    {* �������ݿ�ʼ���£�������¼���Ϊ0������OnChanging�¼���������}
    procedure Changed; virtual;
    {* ���������ѱ����������¼���Ϊ0������OnChange�¼���������}

    procedure SetUpdating(Updating: Boolean); virtual;
    {* ����״̬������̣������ء�
       Ĭ��Ϊ��ʼ����ʱ����Changing������ʱ����Changed}
    function IsUpdating: Boolean;
    {* ��ǰ���¼����Ƿ����0�����ڸ��£�}

    procedure OnChildChanging(Sender: TObject); virtual;
    {* �����Կ�ʼ�����¼�������̣�����Ϊ�������ݸ�TCnPersistent.Create����
       Ĭ��Ϊ����OnChanging�¼���������}
    procedure OnChildChange(Sender: TObject); virtual;
    {* �������ѱ���¼�������̣�����Ϊ�������ݸ�TCnPersistent.Create����
       Ĭ��Ϊ����OnChange�¼���������}

    property Owner: TPersistent read FOwner write FOwner;
    {* ����������� }
    property LockObject: TCnLockObject read GetLockObject;
    {* �߳�ͬ������ }
  public
    constructor Create; overload; virtual;
    {* �����������ڲ���һ�������ʵ����������}
    constructor Create(AOwner: TPersistent); overload;
    {* ������������Ϊʵ���������ߣ�����ֱ�ӻ��Ӱ���TCollection������Ҫ��Ϊ
       published ����ʱʹ��}
    constructor Create(ChangeProc: TNotifyEvent); overload;
    {* ���������������ڸ�OnChange�¼�ָ��һ����ʼֵ}
    constructor Create(ChangingProc, ChangeProc: TNotifyEvent); overload;
    {* ���������������ڸ�OnChanging��OnChange�¼�ָ��һ����ʼֵ}
    destructor Destroy; override;

    procedure BeginUpdate; virtual;
    {* ��ʼ���£������ǰ���¼���Ϊ0���Զ�����Changing�����������ء�
       �ڶԳ������Խ����޸�ʱ����ø÷�����ע�������EndUpdate�ɶ�ʹ��}
    procedure EndUpdate; virtual;
    {* �������£������ǰ���¼���Ϊ0���Զ�����Change�����������ء�
       �ڶԳ��������޸ĺ�����ø÷�����ע�������BeginUpdate�ɶ�ʹ��}

    procedure Lock;
    {* �����ٽ�����Ϊ��֤���߳�ͬ����������������Unlock�ɶ�ʹ��}
    function TryLock: Boolean;
    {* �����ǰLock����Ϊ�㣬����������棬���򷵻ؼ١�
       ��������棬�����ڲ�����ɺ����UnLock�ͷ���}
    procedure Unlock;
    {* �˳��ٽ������ͷ�ͬ������������Lock�ɶ�ʹ��}

    property Locking: Boolean read GetLocking;
    {* ȡ��ǰ����״̬}
  published
    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
    {* ����ʼ�����¼�}
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    {* ���������ѱ���¼�}
  end;

//==============================================================================
// ��Enabled�ĸ���֪ͨ�־�����
//==============================================================================

{ TCnEnabledPersistent }

  TCnEnabledPersistent = class(TCnPersistent)
  {* ��Enabled�ĸ���֪ͨ�־�����}
  private
    FEnabled: Boolean;
  protected
    procedure SetEnabled(const Value: Boolean); virtual;
    procedure SetUpdating(Updating: Boolean); override;
  public
    constructor Create; override;
    {* �����������ڲ���һ�������ʵ��}
    procedure Assign(Source: TPersistent); override;
  published
    property Enabled: Boolean read FEnabled write SetEnabled default False;
    {* Enabled���ԣ����Ϊ�٣���Changing��Changed�����ĵ��ý������������¼�}
  end;

//==============================================================================
// ������֪ͨ�ĳ־�����
//==============================================================================

{ TCnNotifyClass }

  TCnNotifyClass = class(TPersistent)
  {* ������֪ͨ�ĳ־����࣬�ؼ����д󲿷ֳ־���Ļ��࣬һ�㲻��Ҫֱ��ʹ��}
  private
    FOnChanged: TNotifyEvent;
  protected
    FOwner: TPersistent;
    procedure Changed; virtual;
    procedure OnChildChanged(Sender: TObject); virtual;
    function GetOwner: TPersistent; override;
  public
    constructor Create(ChangedProc: TNotifyEvent); virtual;
    {* �๹����������Ϊ֪ͨ�¼�}
    procedure Assign(Source: TPersistent); override;
    {* ����ֵ����}
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
    {* �����ѱ���¼�}
  end;

//==============================================================================
// ���������������
//==============================================================================

{ TCnComponent }

  TCnCopyright = type string;

  TCnComponent = class(TComponent)
  {* CnPack�������������}
  private
    FAbout: TCnCopyright;
    procedure SetAbout(const Value: TCnCopyright);
  protected
    procedure GetComponentInfo(var AName, Author, Email, Comment: string); virtual;
      abstract;
    {* ȡ�����Ϣ�������ṩ�����˵���Ͱ�Ȩ��Ϣ�����󷽷����������ʵ�֡�
     |<PRE>
       var AName: string      - ������ƣ�������֧�ֱ��ػ����ַ���
       var Author: string     - ������ߣ�����ж�����ߣ��÷ֺŷָ�
       var Email: string      - ����������䣬����ж�����ߣ��÷ֺŷָ�
       var Comment:           - ���˵����������֧�ֱ��ػ������з����ַ���
     |</PRE>}
  public
    constructor Create(AOwner: TComponent); override;
  published
    property About: TCnCopyright read FAbout write SetAbout stored False;
    {* ����汾���ԣ����������ʹ��}
  end;

//==============================================================================
// ��ʵ���ӿڶ��������
//==============================================================================

{ TSingletonInterfacedObject }

  TSingletonInterfacedObject = class(TInterfacedObject)
  protected
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  end;

//==============================================================================
// UInt32 �б���
//==============================================================================

const
  CN_MAX_UINT32_SIZE = MaxInt div 16;

type
  PCnUInt32Array = ^TCnUInt32Array;
  TCnUInt32Array = array[0..CN_MAX_UINT32_SIZE - 1] of Cardinal;

  TCnUInt32List = class(TObject)
  {* ���� UInt32 �� List}
  private
    FList: PCnUInt32Array;
    FCount: Integer;
    FCapacity: Integer;
    FIgnoreDuplicated: Boolean;
  protected
    function Get(Index: Integer): Cardinal;
    procedure Grow; virtual;
    procedure Put(Index: Integer; Item: Cardinal);
    procedure SetCapacity(NewCapacity: Integer);
    procedure SetCount(NewCount: Integer);
  public
    destructor Destroy; override;
    function Add(Item: Cardinal): Integer;
    procedure Clear; virtual;
    procedure Delete(Index: Integer);
    class procedure Error(const Msg: string; Data: Integer); overload; virtual;
    class procedure Error(Msg: PResStringRec; Data: Integer); overload;
    procedure Exchange(Index1, Index2: Integer);
    function Expand: TCnUInt32List;
    function Extract(Item: Cardinal): Cardinal;
    function First: Cardinal;
    function IndexOf(Item: Cardinal): Integer;
    procedure Insert(Index: Integer; Item: Cardinal);
    function Last: Cardinal;
    procedure Move(CurIndex, NewIndex: Integer);
    function Remove(Item: Cardinal): Integer;
    property Capacity: Integer read FCapacity write SetCapacity;
    property Count: Integer read FCount write SetCount;
    property Items[Index: Integer]: Cardinal read Get write Put; default;
    property List: PCnUInt32Array read FList;
    property IgnoreDuplicated: Boolean read FIgnoreDuplicated write FIgnoreDuplicated;
  end;

//==============================================================================
// UInt64 �б���
//==============================================================================

const
  CN_MAX_UINT64_SIZE = MaxInt div 16;
  CN_NOT_FOUND_INDEX: TUInt64 = TUInt64(-1);

type
  PCnUInt64Array = ^TCnUInt64Array;
  TCnUInt64Array = array[0..CN_MAX_UINT64_SIZE - 1] of TUInt64;

  TCnUInt64List = class(TObject)
  {* ���� UInt64 �� List����֧�� UInt64 ��ƽ̨���� Int64 ����}
  private
    FList: PCnUInt64Array;
    FCount: TUInt64;
    FCapacity: TUInt64;
    FIgnoreDuplicated: Boolean;
  protected
    function Get(Index: TUInt64): TUInt64;
    procedure Grow; virtual;
    procedure Put(Index: TUInt64; Item: TUInt64);
    procedure SetCapacity(NewCapacity: TUInt64);
    procedure SetCount(NewCount: TUInt64);
  public
    destructor Destroy; override;
    function Add(Item: TUInt64): TUInt64;
    procedure Clear; virtual;
    procedure Delete(Index: TUInt64);
    class procedure Error(const Msg: string; Data: Integer); overload; virtual;
    class procedure Error(Msg: PResStringRec; Data: Integer); overload;
    procedure Exchange(Index1, Index2: TUInt64);
    function Expand: TCnUInt64List;
    function Extract(Item: TUInt64): TUInt64;
    function First: TUInt64;
    function IndexOf(Item: TUInt64): TUInt64;
    // �����±��� TUInt64��֮ǰ���� -1 �� UInt64 �����²����ô��� 0 ������
    // ���ж��Ƿ���� CN_NOT_FOUND_INDEX
    procedure Insert(Index: TUInt64; Item: TUInt64);
    function Last: TUInt64;
    procedure Move(CurIndex, NewIndex: TUInt64);
    function Remove(Item: TUInt64): TUInt64;
    property Capacity: TUInt64 read FCapacity write SetCapacity;
    property Count: TUInt64 read FCount write SetCount;
    property Items[Index: TUInt64]: TUInt64 read Get write Put; default;
    // �ڲ��±ꡢ�ߴ���� TUInt64 ��ʾ���������ڱ���������ʵ���ϴﲻ�� TUInt64
    property List: PCnUInt64Array read FList;
    property IgnoreDuplicated: Boolean read FIgnoreDuplicated write FIgnoreDuplicated;
  end;

procedure AssignPersistent(Source, Dest: TPersistent; UseDefineProperties:
  Boolean = True);

implementation

uses
  CnConsts;

type
  TPersistentHack = class(TPersistent);

procedure AssignPersistent(Source, Dest: TPersistent; UseDefineProperties: 
  Boolean = True);
var
  Stream: TMemoryStream;
  Reader: TReader;
  Writer: TWriter;
  Count: Integer;
  PropIdx: Integer;
  PropList: PPropList;
  PropInfo: PPropInfo;
begin
  if Source is Dest.ClassType then
  begin
    // ʹ�� RTTI ����֤��ֵ���� published ���ԣ������ܴ���ֵΪ Default �����ԣ�
    Count := GetPropList(Dest.ClassInfo, tkProperties - [tkArray, tkRecord,
      tkInterface], nil);
    GetMem(PropList, Count * SizeOf(Pointer));
    try
      GetPropList(Source.ClassInfo, tkProperties - [tkArray, tkRecord,
        tkInterface], @PropList^[0]);
      for PropIdx := 0 to Count - 1 do
      begin
        PropInfo := PropList^[PropIdx];
        case PropInfo^.PropType^^.Kind of
          tkInteger, tkChar, tkWChar, tkClass, tkEnumeration, tkSet:
            SetOrdProp(Dest, PropInfo, GetOrdProp(Source, PropInfo));
          tkFloat:
            SetFloatProp(Dest, PropInfo, GetFloatProp(Source, PropInfo));
          tkString, tkLString, tkWString{$IFDEF UNICODE_STRING}, tkUString{$ENDIF}:
            SetStrProp(Dest, PropInfo, GetStrProp(Source, PropInfo));
          tkVariant:
            SetVariantProp(Dest, PropInfo, GetVariantProp(Source, PropInfo));
          tkInt64:
            SetInt64Prop(Dest, PropInfo, GetInt64Prop(Source, PropInfo));
          tkMethod:
            SetMethodProp(Dest, PropInfo, GetMethodProp(Source, PropInfo));
        end;
      end;
    finally
      FreeMem(PropList);
    end;

    // ʹ�����������Զ��������
    if UseDefineProperties then
    begin
      Stream := nil;
      Reader := nil;
      Writer := nil;
      try
        Stream := TMemoryStream.Create;
        Writer := TWriter.Create(Stream, 4096);
        TPersistentHack(Source).DefineProperties(Writer);
        Writer.FlushBuffer;
        Stream.Position := 0;
        Reader := TReader.Create(Stream, 4096);
        TPersistentHack(Dest).DefineProperties(Reader);
      finally
        FreeAndNil(Reader);
        FreeAndNil(Writer);
        FreeAndNil(Stream);
      end;
    end;
  end;
end;

//==============================================================================
// ֧���̰߳�ȫ�Ļ�����
//==============================================================================

var
  CounterLock: TRTLCriticalSection;

{ TCnLockObject }

// ��ʼ��
constructor TCnLockObject.Create;
begin
  inherited;
  InitializeCriticalSection(FLock); // ��ʼ���ٽ���
end;

// �ͷ�
destructor TCnLockObject.Destroy;
begin
  DeleteCriticalSection(FLock);
  inherited;
end;

// ���Խ����ٽ���������Ѽ������� False��
function TCnLockObject.TryLock: Boolean;
begin
  EnterCriticalSection(CounterLock);
  try
    Result := FLockCount = 0;
    if Result then Lock;
  finally
    LeaveCriticalSection(CounterLock);
  end;
end;

// ����
procedure TCnLockObject.Lock;
begin
  EnterCriticalSection(CounterLock);
  Inc(FLockCount);
  LeaveCriticalSection(CounterLock);
  EnterCriticalSection(FLock);
end;

// �ͷ���
procedure TCnLockObject.Unlock;
begin
  LeaveCriticalSection(FLock);
  EnterCriticalSection(CounterLock);
  Dec(FLockCount);
  LeaveCriticalSection(CounterLock);
end;

function TCnLockObject.GetLocking: Boolean;
begin
  Result := FLockCount > 0;
end;

//==============================================================================
// ʹ�� RTTI ʵ���� Assign ������ TPersistent ��
//==============================================================================

{ TCnAssignablePersistent }

procedure TCnAssignablePersistent.Assign(Source: TPersistent);
begin
  if Source is ClassType then 
  begin
    AssignPersistent(Source, Self);
  end
  else
    inherited Assign(Source);
end;

//==============================================================================
// ʹ�� RTTI ʵ���� Assign ������ TCollectionItem ��
//==============================================================================

{ TCnAssignableCollectionItem }

procedure TCnAssignableCollectionItem.Assign(Source: TPersistent);
begin
  if Source is ClassType then
  begin
    AssignPersistent(Source, Self);
  end
  else
    inherited Assign(Source);
end;

//==============================================================================
// ʹ�� RTTI ʵ���� Assign ������ TCollection ��
//==============================================================================

{ TCnAssignableCollection }

procedure TCnAssignableCollection.Assign(Source: TPersistent);
begin
  if Source is ClassType then
  begin
    AssignPersistent(Source, Self);
  end;
  inherited Assign(Source);
end;

//==============================================================================
// ������֪ͨ���̰߳�ȫ�ĳ־�����
//==============================================================================

{ TCnPersistent }

// ��ʼ���������أ�
constructor TCnPersistent.Create;
begin
  inherited;
  FUpdateCount := 0;
end;

// ��ʼ��������Ϊʵ����������
constructor TCnPersistent.Create(AOwner: TPersistent);
begin
  Create;
  FOwner := AOwner;
end;

// ��ʼ��������Ϊ����֪ͨ�¼�
constructor TCnPersistent.Create(ChangeProc: TNotifyEvent);
begin
  Create;
  FOnChange := ChangeProc;
end;

// ��ʼ��������Ϊ����֪ͨ�¼�
constructor TCnPersistent.Create(ChangingProc, ChangeProc: TNotifyEvent);
begin
  Create;
  FOnChanging := ChangingProc;
  FOnChange := ChangeProc;
end;

destructor TCnPersistent.Destroy;
begin
  if Assigned(FLockObject) then
    FLockObject.Free;
  inherited;
end;

//------------------------------------------------------------------------------
// ����֪ͨ����
//------------------------------------------------------------------------------

// ��ʼ����
procedure TCnPersistent.BeginUpdate;
begin
  if not IsUpdating then SetUpdating(True); // ��ʼ����
  Inc(FUpdateCount);
end;

// ��������
procedure TCnPersistent.EndUpdate;
begin                         // Assert����Ҫ���ػ�
  Assert(FUpdateCount > 0, 'Unpaired TCnPersistent.EndUpdate');
  Dec(FUpdateCount);
  if not IsUpdating then SetUpdating(False);
end;

// ���ڱ��
procedure TCnPersistent.Changing;
begin
  if not IsUpdating and Assigned(FOnChanging) then FOnChanging(Self);
end;

// �������
procedure TCnPersistent.Changed;
begin
  if not IsUpdating and Assigned(FOnChange) then FOnChange(Self);
end;

// ȡ������
function TCnPersistent.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

// ���ڸ���
function TCnPersistent.IsUpdating: Boolean;
begin
  Result := FUpdateCount > 0;
end;

// ����״̬�������
procedure TCnPersistent.SetUpdating(Updating: Boolean);
begin
  if Updating then
    Changing
  else
    Changed;
end;

// �ӵ�λ���
procedure TCnPersistent.OnChildChanging(Sender: TObject);
begin
  if not IsUpdating and Assigned(FOnChanging) then FOnChanging(Sender);
end;

// �ӵ�λ�ѱ��
procedure TCnPersistent.OnChildChange(Sender: TObject);
begin
  if not IsUpdating and Assigned(FOnChange) then FOnChange(Sender);
end;

//------------------------------------------------------------------------------
// �̰߳�ȫ������
//------------------------------------------------------------------------------

// �����ٽ�����Ϊ��֤���߳�ͬ����������������Unlock�ɶ�ʹ��
procedure TCnPersistent.Lock;
begin
  LockObject.Lock;
end;

// �����ǰLock����Ϊ�㣬����������棬���򷵻ؼ�
function TCnPersistent.TryLock: Boolean;
begin
  Result := LockObject.TryLock;
end;

// �˳��ٽ������ͷ�ͬ������������Lock�ɶ�ʹ��
procedure TCnPersistent.Unlock;
begin
  LockObject.Unlock;
end;

// Locking ���Զ�����
function TCnPersistent.GetLocking: Boolean;
begin
  Result := LockObject.GetLocking;
end;

// LockObject ���Զ�������������Ҫʱ�����ڲ�����
function TCnPersistent.GetLockObject: TCnLockObject;
begin
  if not Assigned(FLockObject) then
    FLockObject := TCnLockObject.Create;
  Result := FLockObject;
end;

//==============================================================================
// ��Enabled�ĸ���֪ͨ�־�����
//==============================================================================

{ TCnEnabledPersistent }

// ��ֵ
procedure TCnEnabledPersistent.Assign(Source: TPersistent);
begin
  if Source is TCnEnabledPersistent then
    FEnabled := TCnEnabledPersistent(Source).FEnabled
  else
    inherited Assign(Source);
end;

// ����֪ͨ
procedure TCnEnabledPersistent.SetUpdating(Updating: Boolean);
begin
  if FEnabled then            // ���������֪ͨ
    inherited SetUpdating(Updating); 
end;

// ����
constructor TCnEnabledPersistent.Create;
begin
  inherited Create;
  FEnabled := False;
end;

// ���ò���
procedure TCnEnabledPersistent.SetEnabled(const Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := True;         // ����֪ͨ
    Changed;
    FEnabled := Value;
  end;
end;

{ TCnNotifyClass }

//--------------------------------------------------------//
//������֪ͨ�ĳ־�����                                    //
//--------------------------------------------------------//

//��ֵ
procedure TCnNotifyClass.Assign(Source: TPersistent);
begin
  if not (Source is TCnNotifyClass) then
    inherited Assign(Source);
end;

//����֪ͨ
procedure TCnNotifyClass.Changed;
begin
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

//����
constructor TCnNotifyClass.Create(ChangedProc: TNotifyEvent);
begin
  inherited Create;
  FOnChanged := ChangedProc;
end;

//ȡ������
function TCnNotifyClass.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

//�ӵ�λ����֪ͨ
procedure TCnNotifyClass.OnChildChanged(Sender: TObject);
begin
  Changed;
end;

//==============================================================================
// ���������������
//==============================================================================

{ TCnComponent }

// ��ʼ��
constructor TCnComponent.Create(AOwner: TComponent);
begin
  inherited;
  FAbout := SCnPackAbout;
end;

// ���ù�������
procedure TCnComponent.SetAbout(const Value: TCnCopyright);
begin
  // ������
end;

//==============================================================================
// ��ʵ���ӿڶ��������
//==============================================================================

{ TSingletonInterfacedObject }

function TSingletonInterfacedObject._AddRef: Integer;
begin
  Result := 1;
end;

function TSingletonInterfacedObject._Release: Integer;
begin
  Result := 1;
end;

{ TCnUInt32List }

function TCnUInt32List.Add(Item: Cardinal): Integer;
begin
  if FIgnoreDuplicated and (IndexOf(Item) >= 0) then
  begin
    Result := -1;
    Exit;
  end;

  Result := FCount;
  if Result = FCapacity then
    Grow;
  FList^[Result] := Item;
  Inc(FCount);
end;

procedure TCnUInt32List.Clear;
begin
  SetCount(0);
  SetCapacity(0);
end;

procedure TCnUInt32List.Delete(Index: Integer);
begin
  if (Index < 0) or (Index >= FCount) then
    Error(@SListIndexError, Index);

  Dec(FCount);
  if Index < FCount then
    System.Move(FList^[Index + 1], FList^[Index],
      (FCount - Index) * SizeOf(Cardinal));
end;

destructor TCnUInt32List.Destroy;
begin
  Clear;
  inherited;
end;

class procedure TCnUInt32List.Error(Msg: PResStringRec; Data: Integer);
begin
  TCnUInt32List.Error(LoadResString(Msg), Data);
end;

class procedure TCnUInt32List.Error(const Msg: string; Data: Integer);
begin
  raise EListError.CreateFmt(Msg, [Data])
end;

procedure TCnUInt32List.Exchange(Index1, Index2: Integer);
var
  Item: Cardinal;
begin
  if (Index1 < 0) or (Index1 >= FCount) then
    Error(@SListIndexError, Index1);
  if (Index2 < 0) or (Index2 >= FCount) then
    Error(@SListIndexError, Index2);
  Item := FList^[Index1];
  FList^[Index1] := FList^[Index2];
  FList^[Index2] := Item;
end;

function TCnUInt32List.Expand: TCnUInt32List;
begin
  if FCount = FCapacity then
    Grow;
  Result := Self;
end;

function TCnUInt32List.Extract(Item: Cardinal): Cardinal;
var
  I: Integer;
begin
  Result := 0;
  I := IndexOf(Item);
  if I >= 0 then
  begin
    Result := Item;
    FList^[I] := 0;
    Delete(I);
  end;
end;

function TCnUInt32List.First: Cardinal;
begin
  Result := Get(0);
end;

function TCnUInt32List.Get(Index: Integer): Cardinal;
begin
  if (Index < 0) or (Index >= FCount) then
    Error(@SListIndexError, Index);
  Result := FList^[Index];
end;

procedure TCnUInt32List.Grow;
var
  Delta: Integer;
begin
  if FCapacity > 64 then
    Delta := FCapacity div 4
  else
    if FCapacity > 8 then
      Delta := 16
    else
      Delta := 4;
  SetCapacity(FCapacity + Delta);
end;

function TCnUInt32List.IndexOf(Item: Cardinal): Integer;
begin
  Result := 0;
  while (Result < FCount) and (FList^[Result] <> Item) do
    Inc(Result);
  if Result = FCount then
    Result := -1;
end;

procedure TCnUInt32List.Insert(Index: Integer; Item: Cardinal);
begin
  if (Index < 0) or (Index > FCount) then
    Error(@SListIndexError, Index);
  if FCount = FCapacity then
    Grow;
  if Index < FCount then
    System.Move(FList^[Index], FList^[Index + 1],
      (FCount - Index) * SizeOf(Cardinal));
  FList^[Index] := Item;
  Inc(FCount);
end;

function TCnUInt32List.Last: Cardinal;
begin
  Result := Get(FCount - 1);
end;

procedure TCnUInt32List.Move(CurIndex, NewIndex: Integer);
var
  Item: Cardinal;
begin
  if CurIndex <> NewIndex then
  begin
    if (NewIndex < 0) or (NewIndex >= FCount) then
      Error(@SListIndexError, NewIndex);
    Item := Get(CurIndex);
    FList^[CurIndex] := 0;
    Delete(CurIndex);
    Insert(NewIndex, 0);
    FList^[NewIndex] := Item;
  end;
end;

procedure TCnUInt32List.Put(Index: Integer; Item: Cardinal);
begin
  if (Index < 0) or (Index >= FCount) then
    Error(@SListIndexError, Index);
  if FIgnoreDuplicated and (IndexOf(Item) >= 0) then
    Exit;

  FList^[Index] := Item;
end;

function TCnUInt32List.Remove(Item: Cardinal): Integer;
begin
  Result := IndexOf(Item);
  if Result >= 0 then
    Delete(Result);
end;

procedure TCnUInt32List.SetCapacity(NewCapacity: Integer);
begin
  if (NewCapacity < FCount) or (NewCapacity > MaxListSize) then
    Error(@SListCapacityError, NewCapacity);
  if NewCapacity <> FCapacity then
  begin
    ReallocMem(FList, NewCapacity * SizeOf(Cardinal));
    FCapacity := NewCapacity;
  end;
end;

procedure TCnUInt32List.SetCount(NewCount: Integer);
var
  I: Integer;
begin
  if (NewCount < 0) or (NewCount > MaxListSize) then
    Error(@SListCountError, NewCount);
  if NewCount > FCapacity then
    SetCapacity(NewCount);
  if NewCount > FCount then
    FillChar(FList^[FCount], (NewCount - FCount) * SizeOf(Cardinal), 0)
  else
    for I := FCount - 1 downto NewCount do
      Delete(I);
  FCount := NewCount;
end;

{ TCnUInt64List }

function TCnUInt64List.Add(Item: TUInt64): TUInt64;
begin
  if FIgnoreDuplicated and (IndexOf(Item) <> CN_NOT_FOUND_INDEX) then
  begin
    Result := CN_NOT_FOUND_INDEX;
    Exit;
  end;

  Result := FCount;
  if Result = FCapacity then
    Grow;
  FList^[Result] := Item;
  Inc(FCount);
end;

procedure TCnUInt64List.Clear;
begin
  SetCount(0);
  SetCapacity(0);
end;

procedure TCnUInt64List.Delete(Index: TUInt64);
begin
  if (UInt64Compare(Index, 0) < 0) or (UInt64Compare(Index, FCount) >= 0) then
    Error(@SListIndexError, Index);

  Dec(FCount);
  if UInt64Compare(Index, FCount) < 0 then
    System.Move(FList^[Index + 1], FList^[Index],
      (FCount - Index) * SizeOf(TUInt64));
end;

destructor TCnUInt64List.Destroy;
begin
  Clear;
  inherited;
end;

class procedure TCnUInt64List.Error(Msg: PResStringRec; Data: Integer);
begin
  TCnUInt64List.Error(LoadResString(Msg), Data);
end;

class procedure TCnUInt64List.Error(const Msg: string; Data: Integer);
begin
  raise EListError.CreateFmt(Msg, [Data])
end;

procedure TCnUInt64List.Exchange(Index1, Index2: TUInt64);
var
  Item: TUInt64;
begin

  if (Index1 < 0) or (Index1 >= FCount) then
    Error(@SListIndexError, Index1);
  if (Index2 < 0) or (Index2 >= FCount) then
    Error(@SListIndexError, Index2);
  Item := FList^[Index1];
  FList^[Index1] := FList^[Index2];
  FList^[Index2] := Item;
end;

function TCnUInt64List.Expand: TCnUInt64List;
begin
  if FCount = FCapacity then
    Grow;
  Result := Self;
end;

function TCnUInt64List.Extract(Item: TUInt64): TUInt64;
var
  I: Integer;
begin
  Result := 0;
  I := IndexOf(Item);
  if I <> CN_NOT_FOUND_INDEX then
  begin
    Result := Item;
    FList^[I] := 0;
    Delete(I);
  end;
end;

function TCnUInt64List.First: TUInt64;
begin
  Result := Get(0);
end;

function TCnUInt64List.Get(Index: TUInt64): TUInt64;
begin
  if (UInt64Compare(Index, 0) < 0) or (UInt64Compare(Index, FCount) >= 0) then
    Error(@SListIndexError, Index);
  Result := FList^[Index];
end;

procedure TCnUInt64List.Grow;
var
  Delta: Integer;
begin
  if FCapacity > 64 then
    Delta := FCapacity div 4
  else
    if FCapacity > 8 then
      Delta := 16
    else
      Delta := 4;
  SetCapacity(FCapacity + Delta);
end;

function TCnUInt64List.IndexOf(Item: TUInt64): TUInt64;
begin
  Result := 0;
  while (Result < FCount) and (FList^[Result] <> Item) do
    Inc(Result);
  if Result = FCount then
    Result := CN_NOT_FOUND_INDEX;
end;

procedure TCnUInt64List.Insert(Index: TUInt64; Item: TUInt64);
begin
  if (UInt64Compare(Index, 0) < 0) or (UInt64Compare(Index, FCount) >= 0) then
    Error(@SListIndexError, Index);
  if FCount = FCapacity then
    Grow;
  if Index < FCount then
    System.Move(FList^[Index], FList^[Index + 1],
      (FCount - Index) * SizeOf(TUInt64));
  FList^[Index] := Item;
  Inc(FCount);
end;

function TCnUInt64List.Last: TUInt64;
begin
  Result := Get(FCount - 1);
end;

procedure TCnUInt64List.Move(CurIndex, NewIndex: TUInt64);
var
  Item: TUInt64;
begin
  if CurIndex <> NewIndex then
  begin
    if (UInt64Compare(NewIndex, 0) < 0) or (UInt64Compare(NewIndex, FCount) >= 0) then
      Error(@SListIndexError, NewIndex);
    Item := Get(CurIndex);
    FList^[CurIndex] := 0;
    Delete(CurIndex);
    Insert(NewIndex, 0);
    FList^[NewIndex] := Item;
  end;
end;

procedure TCnUInt64List.Put(Index: TUInt64; Item: TUInt64);
begin
  if (UInt64Compare(Index, 0) < 0) or (UInt64Compare(Index, FCount) >= 0) then
    Error(@SListIndexError, Index);
  if FIgnoreDuplicated and (IndexOf(Item) <> CN_NOT_FOUND_INDEX) then
    Exit;

  FList^[Index] := Item;
end;

function TCnUInt64List.Remove(Item: TUInt64): TUInt64;
begin
  Result := IndexOf(Item);
  if Result <> CN_NOT_FOUND_INDEX then
    Delete(Result);
end;

procedure TCnUInt64List.SetCapacity(NewCapacity: TUInt64);
begin
  if (NewCapacity < FCount) or (NewCapacity > MaxListSize) then
    Error(@SListCapacityError, NewCapacity);
  if NewCapacity <> FCapacity then
  begin
    ReallocMem(FList, NewCapacity * SizeOf(TUInt64));
    FCapacity := NewCapacity;
  end;
end;

procedure TCnUInt64List.SetCount(NewCount: TUInt64);
var
  I: Integer;
begin
  if (NewCount < 0) or (NewCount > MaxListSize) then
    Error(@SListCountError, NewCount);
  if NewCount > FCapacity then
    SetCapacity(NewCount);
  if NewCount > FCount then
    FillChar(FList^[FCount], (NewCount - FCount) * SizeOf(TUInt64), 0)
  else
    for I := FCount - 1 downto NewCount do
      Delete(I);
  FCount := NewCount;
end;

initialization
  InitializeCriticalSection(CounterLock);

finalization
  DeleteCriticalSection(CounterLock);

end.

