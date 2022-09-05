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

unit CnObjectPool;
{* |<PRE>
================================================================================
* ������ƣ������ӹ��������
* ��Ԫ���ƣ�����չ�ĸ����ܵĶ���ص�Ԫ
* ��Ԫ���ߣ�Chinbo��Shenloqi��
* ��    ע��
* ����ƽ̨��PWin2000Pro + Delphi 7.0
* ���ݲ��ԣ�PWin9X/2000/XP + Delphi 5/6
* �� �� �����õ�Ԫ�е��ַ����ݲ����ϱ��ػ�����ʽ
* �޸ļ�¼��2004.03.18 V1.0
*               ������Ԫ
*           2004.09.18 V1.1
*               ��������WaitTimeOut��ʵ�ֵȴ���ʱ
*                 (timeSetEventʵ�֣��������Ƚϴ�Ĭ�ϲ���)
*               ΪWrapper������CanReuse
*                 ����ʵ�ִ��ڻ������Ҫ��ʼ��״̬�Ķ��󲻱�����
*               ������Warpper��Info����
*               �޸���ReleaseOne��ʵ�֣��Ա����಻��Ҫǿ������ת��
*           2004.10.18 V1.2
*               ����û���û���趨ObjectClassҲû��ʵ��OnCreateOne��DoCreateOne�᷵��ʧ��
*               ������ͬʱ����ObjectClass��OnCreateOne
*               ��һ�δ����Ķ���Ĭ������Ϊ��Ҫ��ʼ��
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, Messages, SysUtils, Classes,
  CnConsts, CnClasses, CnCompConsts;

type
  { ��ȡ�ض����ѡ��
    goNone          ���κ����⴦��
    goReInit        ��Ҫ��ʼ����������������
    goMutex         ����Ҫ��ʼ����������������
  }
  TCnObjectPoolGetOption = (goNone, goReInit, goMutex);

  { �ͷųض����ѡ��
    roNone          �����������ü���
    roReInit        Ҫ�����³�ʼ��
    roDelete        �Ӷ������ɾ������
    roFree          �ͷŸö���
  }
  TCnObjectPoolReleaseOption = (roNone, roReInit, roDelete, roFree);

  { ��ȡ�ض���Ľ��
    grSuccess       �ɹ�
    grReuse         ������õĶ���
    grInitFail      ��ʼ��ʧ��
    grGetFail       ��ȡʧ��
    grGetError      ��ȡ�Ľ������
    grWaitFail      �ȴ���ʱ
  }
  TCnObjectPoolGetResult = (grSuccess, grReuse, grReinitFail, grGetFail, grGetError, grWaitFail);

  { �����޿��ж���ʱ�Ĳ���
    (���������Ҫ����ʼ���򻥳������ʹ�ü���Ϊ0�Ķ���)
    bpWait                    �ȴ����еĶ���
    bpGetFail                 ֱ�ӷ���ʧ��
    bpReuse                   ������ö���
  }
  TCnObjectPoolBusyPolicy = (bpWait, bpGetFail, bpReuse);

  { ���ж��󶼴ﵽ�������ֵʱ�Ĳ���
    bpWait                    �ȴ����еĶ���
    bpGetFail                 ֱ�ӷ���ʧ��
  }
  TCnObjectPoolPeakPolicy = bpWait..bpGetFail;

  { ��ȡ�ض���Ĳ���
    gpReuse                   ʹ�����ü�����С�Ķ���
    gpReuseMaxWorEff          ���ü�����С�Ĺ���Ч��(ƽ������ʱ��)��ߵ�
    gpReuseMinWorkCount       ���ü�����С�Ĺ����������ٵ�
    gpReuseMaxWorkCount       ���ü�����С�Ĺ�����������
    gpReuseMinWorkTime        ���ü�����С�Ĺ���ʱ�����ٵ�
    gpReuseMaxWorkTime        ���ü�����С�Ĺ���ʱ������
    gpMaxWorEff               ����Ч����ߵ�(�����ø���������)
    gpMinWorkCount            �����������ٵ�(�����ø���������)
    gpMaxWorkCount            ������������(�����ø���������)
    gpMinWorkTime             ����ʱ�����ٵ�(�����ø���������)
    gpMaxWorkTime             ����ʱ������(�����ø���������)
  }
  TCnObjectPoolGetPolicy = (gpReuse,
    gpReuseMaxWorkEff,
    gpReuseMinWorkCount, gpReuseMaxWorkCount,
    gpReuseMinWorkTime, gpReuseMaxWorkTime,
    gpMaxWorkEff,
    gpMinWorkCount, gpMaxWorkCount,
    gpMinWorkTime, gpMaxWorkTime);

  TCriticalSection = class
  protected
    FSection: TRTLCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Enter;
    procedure Leave;
    function TryEnter: Boolean;
  end;

  TCnCustomObjectPool = class; // forward

  { TCnObjectWrapper }
  TCnObjectWrapper = class(TObject)
  private
    FNeedReInit: Boolean; // �Ƿ���Ҫ���³�ʼ������Ҫ���³�ʼ���Ķ����ڼ���Ϊ0ǰ��������
    FCanReuse: Boolean; //�Ƿ�������
    FRefCount: Integer; // ���ü���
    FWorkCount: Cardinal; // ��������
    FWorkTime: Cardinal; // ����ʱ��
    FObject: TObject; // ��װ�Ķ���
    FList: TList; // ����TickCount
    FOwner: TCnCustomObjectPool;
    cs: TCriticalSection;

    function GetObject: TObject;
    procedure SetNeedReInit(const Value: Boolean);
    procedure SetObject(const Value: TObject);
    function GetNeedReInit: Boolean;
    function GetCanReuse: Boolean;
    procedure SetCanReuse(const Value: Boolean);

    procedure SetOwner(const Value: TCnCustomObjectPool);
  protected
    function GetInfo: string; virtual;
    procedure IncRef; virtual;
    procedure DecRef; virtual;

    property NeedReInit: Boolean read GetNeedReInit write SetNeedReInit;
    property CanReuse: Boolean read GetCanReuse write SetCanReuse; //��ǰ�����Ƿ�������
  public
    constructor Create(AOwner: TCnCustomObjectPool); virtual;
    destructor Destroy; override;

    function RefCount: Integer;
    function WorkCount: Cardinal;
    function WorkTime: Cardinal;
    function WorkEff: Double;

    property ObjectWrapped: TObject read GetObject;
    property Info: string read GetInfo;
  end;

  TCnObjectWrapperClass = class of TCnObjectWrapper;

  { TCnCustomObjectPool }
  TOPEvent = procedure (Pool: TCnCustomObjectPool;
    Wrapper: TCnObjectWrapper;
    var Obj: TObject;
    var bSuccess: Boolean) of object;

  TCnCustomObjectPool = class(TCnComponent)
  private
    FObjectList: TList;
    hRelease, hTerminate: THandle;
    bTerminated: Boolean;

    FMinSize: Integer;
    FMaxSize: Integer;
    FPeakCount: Integer;
    FLowLoadCount: Integer;
    FWaitTimeOut: Integer;
    FPolicyOnBusy: TCnObjectPoolBusyPolicy;
    FPolicyOnGet: TCnObjectPoolGetPolicy;
    FPolicyOnPeak: TCnObjectPoolPeakPolicy;
    FObjectWrapperClass: TCnObjectWrapperClass;

    FOnReleaseOne: TOPEvent;
    FOnCreateOne: TOPEvent;
    FOnFreeOne: TOPEvent;
    FOnGetOne: TOPEvent;
    FOnReInitOne: TOPEvent;
    FObjectClass: TClass;

    function GetCount: Integer;
    procedure SetMinSize(const Value: Integer);
    procedure SetObjectClass(const Value: TClass);
    procedure SetPeakCount(const Value: Integer);
    procedure SetLowLoadCount(const Value: Integer);
    procedure SetOnCreateOne(const Value: TOPEvent);

  protected
    csObjectMgr: TCriticalSection;

    constructor CreateSpecial(AOwner: TComponent;
      AObjectWrapperClass: TCnObjectWrapperClass); virtual;

    procedure CreateBaseCountObjects(const bSetEvent: Boolean);
    function CanIncObject: Boolean; virtual;
    function IncObject(const bSetEvent: Boolean): TCnObjectWrapper; virtual;
    procedure DecObject(ObjWrapper: TCnObjectWrapper); virtual;
    function AddObjWrapper(ObjWrapper: TCnObjectWrapper): Boolean;
    function RemoveObjWrapper(ObjWrapper: TCnObjectWrapper): Boolean;

    function DoCreateOne(Wrapper: TCnObjectWrapper;
      var Obj: TObject): Boolean; virtual;
    function DoFreeOne(Wrapper: TCnObjectWrapper;
      var Obj: TObject): Boolean; virtual;
    function DoGetOne(Wrapper: TCnObjectWrapper;
      var Obj: TObject): Boolean; virtual;
    function DoReleaseOne(Wrapper: TCnObjectWrapper;
      var Obj: TObject): Boolean; virtual;
    function DoReInitOne(Wrapper: TCnObjectWrapper;
      var Obj: TObject): Boolean; virtual;

    function GetOne(var Obj: TObject;
      const go: TCnObjectPoolGetOption): TCnObjectPoolGetResult; virtual;
    procedure ReleaseOne(var Obj;
      const ro: TCnObjectPoolReleaseOption); virtual;

    procedure GetComponentInfo(var AName, Author, Email, Comment: string); override;

    property MinSize: Integer read FMinSize write SetMinSize;
    property MaxSize: Integer read FMaxSize write FMaxSize;
    property LowLoadCount: Integer read FLowLoadCount write SetLowLoadCount;
    property PeakCount: Integer read FPeakCount write SetPeakCount;
    property ObjectClass: TClass read FObjectClass write SetObjectClass;
    property ObjectWrapperClass: TCnObjectWrapperClass read FObjectWrapperClass;
    property PolicyOnBusy: TCnObjectPoolBusyPolicy
      read FPolicyOnBusy
      write FPolicyOnBusy default bpReuse;
    property PolicyOnPeak: TCnObjectPoolPeakPolicy
      read FPolicyOnPeak
      write FPolicyOnPeak default bpWait;
    property PolicyOnGet: TCnObjectPoolGetPolicy
      read FPolicyOnGet
      write FPolicyOnGet default gpReuse;
    property WaitTimeOut: Integer read FWaitTimeOut write FWaitTimeOut;

    property OnCreateOne: TOPEvent read FOnCreateOne write SetOnCreateOne;
    property OnFreeOne: TOPEvent read FOnFreeOne write FOnFreeOne;
    property OnGetOne: TOPEvent read FOnGetOne write FOnGetOne;
    property OnReleaseOne: TOPEvent read FOnReleaseOne write FOnReleaseOne;
    property OnReInitOne: TOPEvent read FOnReInitOne write FOnReInitOne;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function GetInfo: string; virtual;
    procedure ReInitAll;

    property Count: Integer read GetCount;
  end;

  { TCnObjectPool }

  TCnObjectPool = class(TCnCustomObjectPool)
  protected
    procedure GetComponentInfo(var AName, Author, Email, Comment: string); override;
  public
    constructor CreateSpecial(AOwner: TComponent;
      AObjectWrapperClass: TCnObjectWrapperClass); override;

    function GetObject(var Obj: TObject;
      const go: TCnObjectPoolGetOption = goNone): TCnObjectPoolGetResult;
    procedure ReleaseObject(var Obj;
      const ro: TCnObjectPoolReleaseOption = roNone);

    property ObjectClass;
    property ObjectWrapperClass;
  published
    property MinSize;
    property MaxSize;
    property LowLoadCount;
    property PeakCount;
    property PolicyOnBusy;
    property PolicyOnPeak;
    property PolicyOnGet;

    property OnCreateOne;
    property OnFreeOne;
    property OnGetOne;
    property OnReleaseOne;
    property OnReInitOne;
  end;

implementation

uses
  CnCommon,
  MMSystem; //timeSetEvent�ڸõ�Ԫ����

const
  MaxCardinal = High(Cardinal);

{ TCriticalSection }

constructor TCriticalSection.Create;
begin
  inherited;
  InitializeCriticalSection(FSection)
end;

destructor TCriticalSection.Destroy;
begin
  DeleteCriticalSection(FSection);
  inherited;
end;

procedure TCriticalSection.Enter;
begin
  EnterCriticalSection(FSection)
end;

procedure TCriticalSection.Leave;
begin
  LeaveCriticalSection(FSection)
end;

function TCriticalSection.TryEnter: Boolean;
begin
  Result := TryEnterCriticalSection(FSection)
end;

{ TCnObjectWrapper }

constructor TCnObjectWrapper.Create(AOwner: TCnCustomObjectPool);
begin
  FOwner := AOwner;
  FNeedReInit := False;
  FCanReuse := True;
  FRefCount := 0;
  FWorkCount := 0;
  FWorkTime := 0;

  cs := TCriticalSection.Create;
  FList := TList.Create;

  if Assigned(AOwner) then
    AOwner.AddObjWrapper(Self);
end;

procedure TCnObjectWrapper.DecRef;
var
  lcard: Cardinal;
begin
  lcard := GetTickCount;
  cs.Enter;
  try
    if FRefCount > 0 then
      Dec(FRefCount);

    if FWorkCount < MaxCardinal then
      Inc(FWorkCount);

    if FList.Count > 0 then
    begin
      lcard := lcard - Cardinal(FList.Items[0]);
      FList.Delete(0);

      if MaxCardinal - FWorkTime > lcard then
        FWorkTime := FWorkTime + lcard
      else
        FWorkTime := MaxCardinal;
    end;
  finally
    cs.Leave;
  end;
end;

destructor TCnObjectWrapper.Destroy;
begin
  if Assigned(FOwner) then
    FOwner.RemoveObjWrapper(Self);

  FList.Free;
  cs.Free;
  inherited;
end;

function TCnObjectWrapper.GetCanReuse: Boolean;
begin
  cs.Enter;
  try
    Result := FCanReuse and (not FNeedReInit);
  finally
    cs.Leave;
  end;
end;

function TCnObjectWrapper.GetInfo: string;
begin
  Result := 'Too busy to get info.';
  if cs.TryEnter then
    try
      Result := 'RefCount=' + IntToStr(FRefCount) +
        '; WorkCount=' + IntToStr(FWorkCount) +
        '; WorkTime=' + IntToStr(FWorkTime) +
        'ms; WorkEff=' + IntToStr(Trunc(WorkEff)) +
        '; NeedReInit= ' + BoolToStr(FNeedReInit, True) +
        '; CanReUse= ' + BoolToStr(CanReuse, True) +
        '; Obj=' + IntToHex(Cardinal(FObject), 8);
    finally
      cs.Leave;
    end;
end;

function TCnObjectWrapper.GetNeedReInit: Boolean;
begin
  Result := FNeedReInit;
end;

function TCnObjectWrapper.GetObject: TObject;
begin
  Result := FObject;
end;

procedure TCnObjectWrapper.IncRef;
begin
  cs.Enter;
  try
    Inc(FRefCount);

    FList.Add(Pointer(GetTickCount));
  finally
    cs.Leave;
  end;
end;

function TCnObjectWrapper.RefCount: Integer;
begin
  Result := FRefCount;
end;

procedure TCnObjectWrapper.SetCanReuse(const Value: Boolean);
begin
  cs.Enter;
  try
    FCanReuse := Value;
  finally
    cs.Leave;
  end;
end;

procedure TCnObjectWrapper.SetNeedReInit(const Value: Boolean);
begin
  cs.Enter;
  try
    FNeedReInit := Value;
  finally
    cs.Leave;
  end;
end;

procedure TCnObjectWrapper.SetObject(const Value: TObject);
begin
  cs.Enter;
  try
    FObject := Value;
  finally
    cs.Leave;
  end;
end;

procedure TCnObjectWrapper.SetOwner(const Value: TCnCustomObjectPool);
begin
  cs.Enter;
  try
    FOwner := Value;
  finally
    cs.Leave;
  end;
end;

function TCnObjectWrapper.WorkCount: Cardinal;
begin
  Result := FWorkCount;
end;

function TCnObjectWrapper.WorkEff: Double;
begin
  cs.Enter;
  try
    if (FWorkTime = 0) or (FWorkCount = 0) then
      Result := 0
    else
      Result := FWorkTime / FWorkCount;
  finally
    cs.Leave;
  end;
end;

function TCnObjectWrapper.WorkTime: Cardinal;
begin
  Result := FWorkTime;
end;

{ TCnCustomObjectPool }

function TCnCustomObjectPool.AddObjWrapper(
  ObjWrapper: TCnObjectWrapper): Boolean;
begin
  csObjectMgr.Enter;
  try
    Result := FObjectList.IndexOf(ObjWrapper) < 0;

    if Result then
      FObjectList.Add(ObjWrapper);
  finally
    csObjectMgr.Leave;
  end;
end;

constructor TCnCustomObjectPool.Create(AOwner: TComponent);
begin
  inherited;
  hRelease := CreateEvent(nil, True, False, nil);
  hTerminate := CreateEvent(nil, True, False, nil);
  if (hRelease = 0) or (hTerminate = 0) then
    raise Exception.Create('Cannot create events.');

  csObjectMgr := TCriticalSection.Create;
  FObjectList := TList.Create;

  bTerminated := False;
  FMinSize := 0;
  FMaxSize := 0;
  FPeakCount := 0;
  FLowLoadCount := 0;
  FPolicyOnBusy := bpReuse;
  FPolicyOnPeak := bpWait;
  FPolicyOnGet := gpReuse;
  FWaitTimeOut := 0;
  FObjectClass := nil;
  if FObjectWrapperClass = nil then
    FObjectWrapperClass := TCnObjectWrapper;

  //CreateBaseCountObjects(False);
end;

procedure TCnCustomObjectPool.CreateBaseCountObjects(const bSetEvent: Boolean);
var
  i, iCount: Integer;
begin
  if csDesigning in ComponentState then
    Exit;

  csObjectMgr.Enter;
  try
    iCount := FObjectList.Count;

    for i := iCount + 1 to FMinSize do
      IncObject(bSetEvent);
  finally
    csObjectMgr.Leave;
  end;
end;

constructor TCnCustomObjectPool.CreateSpecial(AOwner: TComponent;
  AObjectWrapperClass: TCnObjectWrapperClass);
begin
  FObjectWrapperClass := AObjectWrapperClass;
  Create(AOwner);
end;

procedure TCnCustomObjectPool.DecObject(ObjWrapper: TCnObjectWrapper);
begin
  csObjectMgr.Enter;
  try
    if not DoFreeOne(ObjWrapper, ObjWrapper.FObject) then
    begin
      if Assigned(ObjWrapper.FObject) then
      try
        ObjWrapper.FObject.Free;
      except
      end;
    end;

    try
      ObjWrapper.Free;
    except
    end;
  finally
    csObjectMgr.Leave;
  end;
end;

destructor TCnCustomObjectPool.Destroy;
var
  i, iCount: Integer;
begin
  bTerminated := True;
  
  SetEvent(hTerminate);

  csObjectMgr.Enter;
  try
    iCount := FObjectList.Count;
    for i := iCount - 1 downto 0 do
    begin
      DecObject(TCnObjectWrapper(FObjectList.Items[i]));
    end;

    FObjectList.Free;
  finally
    csObjectMgr.Leave;
  end;
  csObjectMgr.Free;

  CloseHandle(hTerminate);
  CloseHandle(hRelease);

  inherited;
end;

function TCnCustomObjectPool.DoCreateOne(Wrapper: TCnObjectWrapper;
  var Obj: TObject): Boolean;
var
  tmpObj: TObject;
begin
  csObjectMgr.Enter;
  try
    Result := False;

    if (not Assigned(Obj)) and
      Assigned(FObjectClass) then
    //��Ϊ�ܶ����ֱ��ʹ��Create����������������ԭʼ�Ĵ����ǲ��Ƽ���
    //�����е����������ֱ�ӵ���Create�������п��ܷ����쳣������ʹ����try...except...end;
    try
      tmpObj := FObjectClass.Create;
      Obj := tmpObj;
    except
    end;

    //����û���û������ObjectClass��û��ʵ��OnCreateOne����DoCreateOne�᷵��ʧ��
    //��Ȼ�Ѿ���ֹ��OnCreateOne��ObjectClass�Ĺ�ͬ���ã�
    //��������û���ʵ����OnCreateOne��������ObjectClass������ڴ�й¶��
    //Ϊ�˷�ֹ�ڴ�й¶�����ͷ���Ϊ�趨��ObjectClass���Զ��������Ǹ�����
    Result := Assigned(Obj) or Assigned(FOnCreateOne);
    if Assigned(FOnCreateOne) then
    begin
      tmpObj := nil;
      OnCreateOne(Self, Wrapper, tmpObj, Result);
      if Assigned(tmpObj) then
      begin
        //��Ϊ��TXMLDocument֮���������û������Owner����Free���������ʹ����try...except...end;
        try
          if Assigned(Obj) then
            Obj.Free;
        except
        end;
        Obj := tmpObj;
      end;
    end;

    if Result then
    begin
      Wrapper.SetObject(Obj);
      Result := Assigned(Wrapper.ObjectWrapped);

      if Result then
        Wrapper.NeedReInit := True;
    end
    else
    begin
      if Assigned(Obj) then
      try
        FreeAndNil(Obj);
      except
      end;
    end;

  finally
    csObjectMgr.Leave;
  end;
end;

function TCnCustomObjectPool.DoFreeOne(Wrapper: TCnObjectWrapper;
  var Obj: TObject): Boolean;
begin
  csObjectMgr.Enter;
  try
    Result := True;

    if Assigned(OnFreeOne) then
      OnFreeOne(Self, Wrapper, Obj, Result);

    if Assigned(Obj) then
    try
      FreeAndNil(Obj);
    except
    end;
  finally
    csObjectMgr.Leave;
  end;
end;

function TCnCustomObjectPool.DoGetOne(Wrapper: TCnObjectWrapper;
  var Obj: TObject): Boolean;
begin
  csObjectMgr.Enter;
  try
    Result := True;

    if Assigned(OnGetOne) then
      OnGetOne(Self, Wrapper, Obj, Result);
  finally
    csObjectMgr.Leave;
  end;
end;

function TCnCustomObjectPool.DoReInitOne(Wrapper: TCnObjectWrapper;
  var Obj: TObject): Boolean;
begin
  csObjectMgr.Enter;
  try
    Result := True;

    if Assigned(OnReInitOne) then
      OnReInitOne(Self, Wrapper, Obj, Result);

    if Result then
      Wrapper.NeedReInit := False;
  finally
    csObjectMgr.Leave;
  end;
end;

function TCnCustomObjectPool.DoReleaseOne(Wrapper: TCnObjectWrapper;
  var Obj: TObject): Boolean;
begin
  csObjectMgr.Enter;
  try
    Result := True;

    if Assigned(OnReleaseOne) then
      OnReleaseOne(Self, Wrapper, Obj, Result);
  finally
    csObjectMgr.Leave;
  end;
end;

function TCnCustomObjectPool.GetCount: Integer;
begin
  csObjectMgr.Enter;
  try
    Result := FObjectList.Count;
  finally
    csObjectMgr.Leave;
  end;
end;

function TCnCustomObjectPool.GetInfo: string;
var
  i, iCount: Integer;
begin
  Result := 'Too busy to get info.';
  if csObjectMgr.TryEnter then
    try
      iCount := FObjectList.Count;

      Result := 'Object Count = ' + IntToStr(iCount);
      for i := 0 to iCount - 1 do
        Result := Result + #13#10 + TCnObjectWrapper(FObjectList.Items[i]).GetInfo;
    finally
      csObjectMgr.Leave;
    end;
end;

function TCnCustomObjectPool.GetOne(var Obj: TObject;
  const go: TCnObjectPoolGetOption): TCnObjectPoolGetResult;

  function TryGetOne: TCnObjectWrapper;
  var
    i, iFilter, iCount: Integer;
    objwrap: TCnObjectWrapper;
    dResult, dtemp: Double;
    cResult, ctemp: Cardinal;
    iResult, itmp, iMinRef, iValid: Integer;
    bHasRef, btemp: Boolean;
  begin
    Result := nil;

    iCount := FObjectList.Count;
    if iCount = 0 then
      Exit;

    iFilter := -1;
    iResult := -1;
    iMinRef := -1;
    iValid := -1;
    dResult := -1;
    cResult := 0;
    bHasRef := True; // �����õ����ȼ�С�������õ�

    if (go <> goNone) or (PolicyOnBusy <> bpReuse) then
    begin
      case PolicyOnGet of
        gpReuse:
          begin
            for i := 0 to iCount - 1 do
            begin
              objwrap := TCnObjectWrapper(FObjectList.Items[i]);

              if objwrap.RefCount > 0 then
                Continue;

              if objwrap.RefCount = 0 then
              begin
                iFilter := i; 
                Break;
              end;
            end;   
          end;
        gpReuseMaxWorkEff, gpMaxWorkEff:
          begin // �����Ч�ʾ���WorkEff��С
            for i := 0 to iCount - 1 do
            begin
              objwrap := TCnObjectWrapper(FObjectList.Items[i]);

              if objwrap.RefCount > 0 then
                Continue;

              dtemp := objwrap.WorkEff;
              if dtemp = 0 then
              begin
                iFilter := i;
                Break;
              end
              else
              begin
                if (dResult < 0) or (dtemp < dResult) then
                begin
                  dResult := dtemp;
                  iFilter := i;
                end;
              end;
            end;
          end;
        gpReuseMinWorkCount, gpMinWorkCount:
          begin
            for i := 0 to iCount - 1 do
            begin
              objwrap := TCnObjectWrapper(FObjectList.Items[i]);

              if objwrap.RefCount > 0 then
                Continue;

              if objwrap.WorkCount = 0 then
              begin
                iFilter := i;
                Break;
              end
              else
              begin
                ctemp := objwrap.WorkCount;
                if (cResult = 0) or (ctemp < cResult) then
                begin
                  cResult := ctemp;
                  iFilter := i;
                end;
              end;
            end;
          end;
        gpReuseMaxWorkCount, gpMaxWorkCount:
          begin
            for i := 0 to iCount - 1 do
            begin
              objwrap := TCnObjectWrapper(FObjectList.Items[i]);

              if objwrap.RefCount > 0 then
                Continue;

              if iValid < 0 then
                iValid := i;

              ctemp := objwrap.WorkCount;
              if ctemp > cResult then
              begin
                cResult := ctemp;
                iFilter := i;
              end;
            end;

            if iFilter < 0 then
             iFilter := iValid;
          end;
        gpReuseMinWorkTime, gpMinWorkTime:
          begin
            for i := 0 to iCount - 1 do
            begin
              objwrap := TCnObjectWrapper(FObjectList.Items[i]);

              if objwrap.RefCount > 0 then
                Continue;

              if objwrap.WorkTime = 0 then
              begin
                iFilter := i;
                Break;
              end
              else
              begin
                ctemp := objwrap.WorkTime;
                if (cResult = 0) or (ctemp < cResult) then
                begin
                  cResult := ctemp;
                  iFilter := i;
                end;
              end;
            end;
          end;
        gpReuseMaxWorkTime, gpMaxWorkTime:
          begin
            for i := 0 to iCount - 1 do
            begin
              objwrap := TCnObjectWrapper(FObjectList.Items[i]);

              if objwrap.RefCount > 0 then
                Continue;

              if iValid < 0 then
                iValid := i;

              ctemp := objwrap.WorkTime;
              if ctemp > cResult then
              begin
                cResult := ctemp;
                iFilter := i;
              end;
            end;

            if iFilter < 0 then
             iFilter := iValid;
          end;
      end;
    end
    else
    begin
      case PolicyOnGet of
        gpReuse:
          begin
            for i := 0 to iCount - 1 do
            begin
              objwrap := TCnObjectWrapper(FObjectList.Items[i]);

              if ((not objwrap.CanReuse) and (objwrap.RefCount > 0)) or
                ((FPeakCount > 0) and (objwrap.RefCount >= FPeakCount)) then
                Continue;

              if objwrap.RefCount = 0 then
              begin
                iFilter := i;
                Break;
              end
              else
              begin
                itmp := objwrap.RefCount;
                if (iResult < 0) or (itmp < iResult) then
                begin
                  iResult := itmp;
                  iFilter := i;
                end;
              end;
            end;
          end;
        gpReuseMaxWorkEff:
          begin
            for i := 0 to iCount - 1 do
            begin
              objwrap := TCnObjectWrapper(FObjectList.Items[i]);

              if ((not objwrap.CanReuse) and (objwrap.RefCount > 0)) or
                ((FPeakCount > 0) and (objwrap.RefCount >= FPeakCount)) then
                Continue;

              itmp := objwrap.RefCount;
              if (iMinRef < 0) or (itmp <= iMinRef) then
              begin
                btemp := itmp < iMinRef;

                iMinRef := itmp;

                dtemp := objwrap.WorkEff;
                if btemp or (dResult < 0) or (dtemp < dResult) then
                begin
                  dResult := dtemp;
                  iFilter := i;
                end;
              end;
            end;
          end;
        gpReuseMinWorkCount:
          begin
            for i := 0 to iCount - 1 do
            begin
              objwrap := TCnObjectWrapper(FObjectList.Items[i]);

              if ((not objwrap.CanReuse) and (objwrap.RefCount > 0)) or
                ((FPeakCount > 0) and (objwrap.RefCount >= FPeakCount)) then
                Continue;

              itmp := objwrap.RefCount;
              if (iMinRef < 0) or (itmp <= iMinRef) then
              begin
                btemp := itmp < iMinRef;

                iMinRef := itmp;

                ctemp := objwrap.WorkCount;
                if btemp or (cResult = 0) or (ctemp < cResult) then
                begin
                  cResult := ctemp;
                  iFilter := i;
                end;
              end;
            end;
          end;
        gpReuseMaxWorkCount:
          begin
            for i := 0 to iCount - 1 do
            begin
              objwrap := TCnObjectWrapper(FObjectList.Items[i]);

              if ((not objwrap.CanReuse) and (objwrap.RefCount > 0)) or
                ((FPeakCount > 0) and (objwrap.RefCount >= FPeakCount)) then
                Continue;

              itmp := objwrap.RefCount;
              if (iMinRef < 0) or (itmp <= iMinRef) then
              begin
                btemp := itmp < iMinRef;
                if (iValid < 0) or btemp then
                  iValid := i;

                iMinRef := itmp;

                ctemp := objwrap.WorkCount;
                if btemp or (ctemp > cResult) then
                begin
                  cResult := ctemp;
                  iFilter := i;
                end;
              end;
            end;

            if iFilter < 0 then
              iFilter := iValid;
          end;
        gpReuseMinWorkTime:
          begin
            for i := 0 to iCount - 1 do
            begin
              objwrap := TCnObjectWrapper(FObjectList.Items[i]);

              if ((not objwrap.CanReuse) and (objwrap.RefCount > 0)) or
                ((FPeakCount > 0) and (objwrap.RefCount >= FPeakCount)) then
                Continue;

              itmp := objwrap.RefCount;
              if (iMinRef < 0) or (itmp <= iMinRef) then
              begin
                btemp := itmp < iMinRef;

                iMinRef := itmp;

                ctemp := objwrap.WorkTime;
                if btemp or (cResult = 0) or (ctemp < cResult) then
                begin
                  cResult := ctemp;
                  iFilter := i;
                end;
              end;
            end;
          end;
        gpReuseMaxWorkTime:
          begin
            for i := 0 to iCount - 1 do
            begin
              objwrap := TCnObjectWrapper(FObjectList.Items[i]);

              if ((not objwrap.CanReuse) and (objwrap.RefCount > 0)) or
                ((FPeakCount > 0) and (objwrap.RefCount >= FPeakCount)) then
                Continue;

              itmp := objwrap.RefCount;
              if (iMinRef < 0) or (itmp <= iMinRef) then
              begin
                btemp := itmp < iMinRef;
                if (iValid < 0) or btemp then
                  iValid := i;

                iMinRef := itmp;

                ctemp := objwrap.WorkTime;
                if btemp or (ctemp > cResult) then
                begin
                  cResult := ctemp;
                  iFilter := i;
                end;
              end;
            end;

            if iFilter < 0 then
              iFilter := iValid;
          end;
        gpMaxWorkEff:
          begin
            for i := 0 to iCount - 1 do
            begin
              objwrap := TCnObjectWrapper(FObjectList.Items[i]);

              if ((not objwrap.CanReuse) and (objwrap.RefCount > 0)) or
                ((FPeakCount > 0) and (objwrap.RefCount >= FPeakCount)) then
                Continue;

              btemp := objwrap.RefCount > 0;
              if bHasRef or (not btemp) then
              begin
                bHasRef := btemp;

                dtemp := objwrap.WorkEff;
                if (dResult < 0) or (dtemp < dResult) then
                begin
                  dResult := dtemp;
                  iFilter := i;
                end;
              end;
            end;
          end;
        gpMinWorkCount:
          begin
            for i := 0 to iCount - 1 do
            begin
              objwrap := TCnObjectWrapper(FObjectList.Items[i]);

              if ((not objwrap.CanReuse) and (objwrap.RefCount > 0)) or
                ((FPeakCount > 0) and (objwrap.RefCount >= FPeakCount)) then
                Continue;

              btemp := objwrap.RefCount > 0;
              if bHasRef or (not btemp) then
              begin
                bHasRef := btemp;

                ctemp := objwrap.WorkCount;
                if (cResult = 0) or (ctemp < cResult) then
                begin
                  cResult := ctemp;
                  iFilter := i;
                end;
              end;
            end;
          end;
        gpMaxWorkCount:
          begin
            for i := 0 to iCount - 1 do
            begin
              objwrap := TCnObjectWrapper(FObjectList.Items[i]);

              if ((not objwrap.CanReuse) and (objwrap.RefCount > 0)) or
                ((FPeakCount > 0) and (objwrap.RefCount >= FPeakCount)) then
                Continue;

              btemp := objwrap.RefCount > 0;
              if bHasRef or (not btemp) then
              begin
                if (iValid < 0) or (not btemp) then
                  iValid := i;

                bHasRef := btemp;

                ctemp := objwrap.WorkCount;
                if ctemp > cResult then
                begin
                  cResult := ctemp;
                  iFilter := i;
                end;
              end;
            end;

            if iFilter < 0 then
              iFilter := iValid;
          end;
        gpMinWorkTime:
          begin
            for i := 0 to iCount - 1 do
            begin
              objwrap := TCnObjectWrapper(FObjectList.Items[i]);

              if ((not objwrap.CanReuse) and (objwrap.RefCount > 0)) or
                ((FPeakCount > 0) and (objwrap.RefCount >= FPeakCount)) then
                Continue;

              btemp := objwrap.RefCount > 0;
              if bHasRef or (not btemp) then
              begin
                bHasRef := btemp;

                ctemp := objwrap.WorkTime;
                if (cResult = 0) or (ctemp < cResult) then
                begin
                  cResult := ctemp;
                  iFilter := i;
                end;
              end;
            end;
          end;
        gpMaxWorkTime:
          begin
            for i := 0 to iCount - 1 do
            begin
              objwrap := TCnObjectWrapper(FObjectList.Items[i]);

              if ((not objwrap.CanReuse) and (objwrap.RefCount > 0)) or
                ((FPeakCount > 0) and (objwrap.RefCount >= FPeakCount)) then
                Continue;

              btemp := objwrap.RefCount > 0;
              if bHasRef or (not btemp) then
              begin
                if (iValid < 0) or (not btemp) then
                  iValid := i;

                bHasRef := btemp;

                ctemp := objwrap.WorkTime;
                if ctemp > cResult then
                begin
                  cResult := ctemp;
                  iFilter := i;
                end;
              end;
            end;

            if iFilter < 0 then
              iFilter := iValid;
          end;
      end;
    end;

    if iFilter >= 0 then
    begin
      Result := TCnObjectWrapper(FObjectList.Items[iFilter]);
      if (go = goNone) and (PolicyOnBusy = bpReuse) and
        (FLowLoadCount > 0) and (Result.RefCount >= FLowLoadCount) then
      begin
        if CanIncObject then
          Result := IncObject(False);
      end;
    end;
  end;

type
  THandleID = (hidRelease, hidTerminate, hidWaitTimeOut);
var
  Handles: array[THandleID] of THandle;
  hWaitTimeOut, hTimer: THandle;
  objwrap: TCnObjectWrapper;
begin
  hTimer := 0;
  hWaitTimeOut := CreateEvent(nil, True, False, nil);
  Handles[hidWaitTimeOut] := hWaitTimeOut;
  //if hWaitTimeOut = nil then wait timeout cannot work

  try
    Result := grGetFail;
    while True do
    begin
      if bTerminated then
      begin
        Obj := nil;
        Result := grGetFail;
        Break;
      end;

      csObjectMgr.Enter;
      try
        objwrap := nil;

        while True do
        begin
          if bTerminated then
          begin
            Obj := nil;
            Result := grGetFail;
            Break;
          end;

          objwrap := TryGetOne;

          if objwrap = nil then
          begin
            if CanIncObject then
            begin
              IncObject(True); //ע�⣺������Ǵ���ʧ�ܿ��ܻ�������ѭ��
            end
            else
            begin
              Break;
            end;
          end
          else
          begin
            Break;
          end;
        end;

        if objwrap <> nil then
        begin
          Obj := objwrap.ObjectWrapped;
          if not DoGetOne(objwrap, Obj) then
          begin
            Obj := nil;
            Result := grGetFail;

            Break;
          end;

          if objwrap.RefCount > 0 then
          begin
            Result := grReuse;
          end
          else
          begin
            if (objwrap.FNeedReInit) or (goReInit = go) then
            begin
              try
                if DoReInitOne(objwrap, Obj) then
                  Result := grSuccess
                else
                  Result := grReinitFail;
              except
                Result := grReinitFail;
              end;
            end
            else
            begin
              Result := grSuccess;
            end;
          end;

          if Assigned(FObjectClass) and (not (Obj is FObjectClass)) then
          begin
            Obj := nil;
            Result := grGetError;
            Break;
          end;

          objwrap.CanReuse := go = goNone; //goMutex,goReinit�Ķ��󶼲��ܱ�����
          
          objwrap.IncRef;

          Break;
        end
        else
        begin
          case PolicyOnBusy of
            bpReuse:
              begin
                case PolicyOnPeak of
                  bpGetFail:
                    begin
                      Obj := nil;
                      Result := grGetFail;

                      Break;
                    end;
                  bpWait:
                    begin
                      if (hTimer = 0) and (WaitTimeOut > 0) then
                      begin //��Ҫ�����ú����Ŀ����������Ƿ���Ҫ����timebeginperiod�Ⱥ�����
                        hTimer := timeSetEvent(WaitTimeOut, 100, TFNTimeCallBack(hWaitTimeOut), 0, TIME_ONESHOT or TIME_CALLBACK_EVENT_SET);
                      end;
                    end;
                end;
              end;
            bpGetFail:
              begin
                Obj := nil;
                Result := grGetFail;

                Break;
              end;
            bpWait:
              begin
                if (hTimer = 0) and (WaitTimeOut > 0) then
                begin
                  hTimer := timeSetEvent(WaitTimeOut, 100, TFNTimeCallBack(hWaitTimeOut), 0, TIME_ONESHOT or TIME_CALLBACK_EVENT_SET);
                end;
              end;
          end;
        end;
      finally
        csObjectMgr.Leave;
      end;

      if bTerminated then
      begin
        Obj := nil;
        Result := grGetFail;
        Break;
      end;

      try
        case WaitForMultipleObjects(Length(Handles), @Handles, False, INFINITE) of
          WAIT_OBJECT_0 + Ord(hidRelease):
            begin
              ResetEvent(hRelease);
            end;
          WAIT_OBJECT_0 + Ord(hidTerminate):
            begin
              Obj := nil;
              Result := grGetFail;

              Break;
            end;
          WAIT_OBJECT_0 + Ord(hidWaitTimeOut):
            begin
              timeKillEvent(hTimer);
              hTimer := 0;

              Obj := nil;
              Result := grWaitFail;

              Break;
            end;
        else

        end;
      finally
      end;
    end; // end while True

  finally
    if hTimer <> 0 then
      timeKillEvent(hTimer);
    CloseHandle(hWaitTimeOut);
  end;
end;

function TCnCustomObjectPool.CanIncObject: Boolean;
begin
  Result := (FMaxSize <= 0) or (FMaxSize > FObjectList.Count);
end;

function TCnCustomObjectPool.IncObject(const bSetEvent: Boolean): TCnObjectWrapper;
var
  obj: TObject;
  objwrap: TCnObjectWrapper;
begin
  Result := nil;

  csObjectMgr.Enter;
  try
    if (FMaxSize > 0) and (FMaxSize <= FObjectList.Count) then
      Exit;

    objwrap := FObjectWrapperClass.Create(nil);

    try
      obj := nil;
      if not DoCreateOne(objwrap, obj) then
      begin
        if Assigned(obj) then
          if not DoFreeOne(objwrap, obj) then
          begin
            if Assigned(obj) then
            try
              obj.Free;
            except
            end;
          end;
        objwrap.Free;
      end
      else
      begin
        if AddObjWrapper(objwrap) then
        begin
          objwrap.SetOwner(Self);
          
          if bSetEvent then
          begin
            SetEvent(hRelease);
          end;

          Result := objwrap;
        end
        else
        begin
          if Assigned(objwrap) then
          try
            objwrap.Free;
          except
          end;

          if Assigned(obj) then
          try
            obj.Free;
          except
          end;
        end;
      end;
    except
      objwrap.Free;
      raise ;
    end;

  finally
    csObjectMgr.Leave;
  end;
end;

procedure TCnCustomObjectPool.ReInitAll;
var
  i, iCount: Integer;
begin
  csObjectMgr.Enter;
  try
    iCount := FObjectList.Count;

    for i := 0 to iCount - 1 do
      TCnObjectWrapper(FObjectList.Items[i]).NeedReInit := True;
  finally
    csObjectMgr.Leave;
  end;
end;

procedure TCnCustomObjectPool.ReleaseOne(var Obj;
  const ro: TCnObjectPoolReleaseOption);
var
  i, iCount: Integer;
  objwrap: TCnObjectWrapper;
begin
  csObjectMgr.Enter;
  try
    iCount := FObjectList.Count;

    for i := 0 to iCount - 1 do
    begin
      objwrap := TCnObjectWrapper(FObjectList.Items[i]);

      if Assigned(objwrap) then
      begin
        if objwrap.FObject = TObject(Obj) then
        begin
          try
            DoReleaseOne(objwrap, TObject(Obj));
          except
          end;

          TObject(Obj) := nil; //��ָ���Ѿ�û����;��
          objwrap.CanReuse := True; //����ǿ����õĶ����ֵ�Ѿ�����,����ǲ������õ��ͷ�֮��Ϳ�������

          case ro of
            roNone:
              begin
                objwrap.DecRef;

                SetEvent(hRelease);
              end;
            roReInit:
              begin
                objwrap.DecRef;
                objwrap.NeedReInit := True;

                SetEvent(hRelease);
              end;
            roDelete:
              begin
                try
                  objwrap.Free;
                except
                end;
              end;
            roFree:
              begin
                DecObject(objwrap);
              end;
          end;

          CreateBaseCountObjects(True);

          Break;
        end;
      end;
    end;

  finally
    csObjectMgr.Leave;
  end;
end;

function TCnCustomObjectPool.RemoveObjWrapper(
  ObjWrapper: TCnObjectWrapper): Boolean;
var
  i: Integer;
begin
  csObjectMgr.Enter;
  try
    i := FObjectList.IndexOf(ObjWrapper);

    Result := i >= 0;

    if Result then
      FObjectList.Delete(i);
  finally
    csObjectMgr.Leave;
  end;
end;

procedure TCnCustomObjectPool.SetLowLoadCount(const Value: Integer);
begin
  csObjectMgr.Enter;
  try
    FLowLoadCount := Value;
  finally
    csObjectMgr.Leave;
  end;
end;

procedure TCnCustomObjectPool.SetMinSize(const Value: Integer);
begin
  csObjectMgr.Enter;
  try
    FMinSize := Value;

    CreateBaseCountObjects(False);
  finally
    csObjectMgr.Leave;
  end;
end;

procedure TCnCustomObjectPool.SetObjectClass(const Value: TClass);
begin
  csObjectMgr.Enter;
  try
    if Assigned(FOnCreateOne) then
      raise Exception.Create('Cannot set ObjectClass while OnCreateOne not null.');

    if (FObjectList.Count = 0) or
      ((FObjectClass = nil) and Assigned(Value)) then
    begin
      FObjectClass := Value;

      CreateBaseCountObjects(False);
    end
    else
    begin
      raise Exception.Create('Cannot change ObjectClass while ObjectList not null and assigned ObjectClass.');
    end;
  finally
    csObjectMgr.Leave;
  end;
end;

procedure TCnCustomObjectPool.SetPeakCount(const Value: Integer);
begin
  csObjectMgr.Enter;
  try
    if (FPeakCount <> Value) and (Value >= 0) then
      FPeakCount := Value;
  finally
    csObjectMgr.Leave;
  end;
end;

procedure TCnCustomObjectPool.SetOnCreateOne(const Value: TOPEvent);
begin
  //Ϊ��ֹObjectClass��OnCreateOne��ͻ�������趨��OnCreateOne�����ObjectClass
  if Assigned(Value) then
    FObjectClass := nil;

  FOnCreateOne := Value;
end;

procedure TCnCustomObjectPool.GetComponentInfo(var AName, Author, Email,
  Comment: string);
begin

end;

{ TCnObjectPool }

constructor TCnObjectPool.CreateSpecial(AOwner: TComponent;
  AObjectWrapperClass: TCnObjectWrapperClass);
begin
  inherited;

end;

procedure TCnObjectPool.GetComponentInfo(var AName, Author, Email,
  Comment: string);
begin
  AName := SCnObjectPoolName;
  Author := SCnPack_Shenloqi;
  Email := SCnPack_ShenloqiEmail;
  Comment := SCnObjectPoolComment;
end;

function TCnObjectPool.GetObject(var Obj: TObject;
  const go: TCnObjectPoolGetOption): TCnObjectPoolGetResult;
begin
  Result := GetOne(Obj, go);
end;

procedure TCnObjectPool.ReleaseObject(var Obj;
  const ro: TCnObjectPoolReleaseOption);
begin
  ReleaseOne(Obj, ro);
end;

end.
