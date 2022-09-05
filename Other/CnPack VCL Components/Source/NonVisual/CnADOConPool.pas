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

unit CnADOConPool;
{* |<PRE>
================================================================================
* ������ƣ������ӹ��������
* ��Ԫ���ƣ�ADOConnection ����ص�Ԫ
* ��Ԫ���ߣ�Chinbo��Shenloqi��
* ��    ע��
* ����ƽ̨��PWin2000Pro + Delphi 7.0
* ���ݲ��ԣ�PWin9X/2000/XP + Delphi 5/6
* �� �� �����õ�Ԫ�е��ַ����ݲ����ϱ��ػ�����ʽ
* �޸ļ�¼��2004.03.18 V1.0
*               ������Ԫ
*           2004.09.18 V1.1
*               �����Ӹ���̳е�����WaitTimeOut
*               �޸���ReleaseConnection��ʵ�֣�����ǿ������ת��
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

{$IFDEF SUPPORT_ADO}

uses
  Windows, Messages, SysUtils, Classes, CnObjectPool, ADODB,
  CnConsts, CnCompConsts;

type
  TCnADOConWrapper = class(TCnObjectWrapper)
  private
    function GetConnection: TADOConnection;
  public
    property Connection: TADOConnection read GetConnection;
  end;

  TCnADOConPool = class(TCnCustomObjectPool)
  private
    FConnectionString: WideString;
    procedure SetConnectionString(const Value: WideString);
  protected
    function DoCreateOne(Wrapper: TCnObjectWrapper;
      var Obj: TObject): Boolean; override;
    function DoFreeOne(Wrapper: TCnObjectWrapper;
      var Obj: TObject): Boolean; override;
    function DoGetOne(Wrapper: TCnObjectWrapper;
      var Obj: TObject): Boolean; override;
    function DoReleaseOne(Wrapper: TCnObjectWrapper;
      var Obj: TObject): Boolean; override;
    function DoReInitOne(Wrapper: TCnObjectWrapper;
      var Obj: TObject): Boolean; override;

    procedure GetComponentInfo(var AName, Author, Email, Comment: string); override;
  public
    function GetConnection(var con: TADOConnection;
      const go: TCnObjectPoolGetOption = goNone): TCnObjectPoolGetResult;
    procedure ReleaseConnection(var con: TADOConnection;
      const ro: TCnObjectPoolReleaseOption = roNone);
  published
    property ConnectionString: WideString
      read FConnectionString
      write SetConnectionString;
    property MinSize;
    property MaxSize;
    property LowLoadCount;
    property PeakCount;
    property PolicyOnBusy;
    property PolicyOnPeak;
    property PolicyOnGet;
    property WaitTimeOut;

    property OnGetOne;
    property OnReleaseOne;
    property OnReInitOne;
  end;

{$ENDIF SUPPORT_ADO}

implementation

{$IFDEF SUPPORT_ADO}

uses
  ActiveX,
  ComObj;

{ TCnADOConWrapper }

function TCnADOConWrapper.GetConnection: TADOConnection;
begin
  Result := nil;
  if Assigned(ObjectWrapped) then
    Result := TADOConnection(ObjectWrapped);
end;

{ TCnADOConPool }

function TCnADOConPool.DoCreateOne(Wrapper: TCnObjectWrapper;
  var Obj: TObject): Boolean;
begin
  csObjectMgr.Enter;
  try
    CoInitialize(nil);
    try
      Obj := TADOConnection.Create(Self);
      with TADOConnection(Obj) do
      begin
        KeepConnection := True;
        LoginPrompt := False;
        ConnectionString := Self.ConnectionString;
      end;

      Result := inherited DoCreateOne(Wrapper, Obj);

      if Assigned(Wrapper) then
        TCnADOConWrapper(Wrapper).NeedReInit := True;
    finally
      CoUninitialize;
    end;
  finally
    csObjectMgr.Leave;
  end;
end;

function TCnADOConPool.DoFreeOne(Wrapper: TCnObjectWrapper;
  var Obj: TObject): Boolean;
begin
  csObjectMgr.Enter;
  try
    Result := inherited DoFreeOne(Wrapper, Obj);
  finally
    csObjectMgr.Leave;
  end;
end;

function TCnADOConPool.DoGetOne(Wrapper: TCnObjectWrapper;
  var Obj: TObject): Boolean;
begin
  csObjectMgr.Enter;
  try
    Result := inherited DoGetOne(Wrapper, Obj);
  finally
    csObjectMgr.Leave;
  end;
end;

function TCnADOConPool.DoReInitOne(Wrapper: TCnObjectWrapper;
  var Obj: TObject): Boolean;
begin
  Result := True;
  
  csObjectMgr.Enter;
  try
    CoInitialize(nil);
    try
      with TADOConnection(Obj) do
      begin
        Connected := False;
        KeepConnection := True;
        LoginPrompt := False;
        ConnectionString := Self.ConnectionString;
        try
          Connected := True;
        except
          Result := False;
        end;
      end;

      Result := (inherited DoReInitOne(Wrapper, Obj)) and Result;

      if not Result then
        TCnADOConWrapper(Wrapper).NeedReInit := True;

    finally
      CoUninitialize;
    end;
  finally
    csObjectMgr.Leave;
  end;
end;

function TCnADOConPool.DoReleaseOne(Wrapper: TCnObjectWrapper;
  var Obj: TObject): Boolean;
begin
  csObjectMgr.Enter;
  try
    Result := inherited DoReleaseOne(Wrapper, Obj);
  finally
    csObjectMgr.Leave;
  end;
end;

procedure TCnADOConPool.GetComponentInfo(var AName, Author, Email,
  Comment: string);
begin
  AName := SCnADOConPoolName;
  Author := SCnPack_Shenloqi;
  Email := SCnPack_ShenloqiEmail;
  Comment := SCnADOConPoolComment;
end;

function TCnADOConPool.GetConnection(var con: TADOConnection;
  const go: TCnObjectPoolGetOption): TCnObjectPoolGetResult;
var
  Obj: TObject;
begin
  Result := GetOne(Obj, go);
  if Obj is TADOConnection then
    con := TADOConnection(Obj)
  else
  begin
    if Obj <> nil then
      ReleaseOne(Obj, roFree);
    con := nil;
    Result := grGetError;
  end;
end;

procedure TCnADOConPool.ReleaseConnection(var con: TADOConnection;
  const ro: TCnObjectPoolReleaseOption);
begin
  ReleaseOne(con, ro);
end;

procedure TCnADOConPool.SetConnectionString(const Value: WideString);
begin
  csObjectMgr.Enter;
  try
    FConnectionString := Value;
    ReInitAll;
  finally
    csObjectMgr.Leave;
  end;
end;

{$ENDIF SUPPORT_ADO}

end.
