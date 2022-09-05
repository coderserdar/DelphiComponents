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

unit CnSingleton;
{* |<PRE>
================================================================================
* ������ƣ�������������
* ��Ԫ���ƣ���ʵ������ʵ�ֵ�Ԫ
* ��Ԫ���ߣ�Chinbo��Shenloqi��
* ��    ע���õ�Ԫ�����˵�ʵ�������л�����ƣ������ڶ��߳�
* ����ƽ̨��PWin2K SP3 + Delphi 7
* ���ݲ��ԣ�PWin9X/2000/XP + Delphi 6/7 C++Builder 6
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2006.08.12
*                ������Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes;

type
  ECnSingletonException = class(Exception);

  TCnSingleton = class(TObject)
  private
    FForceFree, FAutoFree: Boolean;
    class function Singleton: TObject;
    procedure SetAutoFree(const Value: Boolean);
  protected
    class function RefCount: Cardinal;
    class function GetClassBaseOffsetStr: string; virtual;
  public
    class function NewInstance: TObject; override;
    procedure FreeInstance; override;

    class procedure Lock;
    class procedure Unlock;

    procedure ForceFree;

    property AutoFree: Boolean read FAutoFree write SetAutoFree;
  end;

implementation

uses
  SyncObjs;

type
  PCnSingletonInfo = ^TCnSingletonInfo;
  TCnSingletonInfo = record
    RefCount: Cardinal;
    Instance: TObject;
  end;

  TCnSingletonList = class(TStringList)
  public
    procedure FreeAllSingletons;
  end;

var
  _SingletonList: TCnSingletonList;
  _Finalize: Boolean = False;
  _CriticalSection: TCriticalSection;

{ TCnSingleton }

class function TCnSingleton.NewInstance: TObject;
begin
  Result := Singleton;
end;

procedure TCnSingleton.FreeInstance;
var
  i: Integer;
  tmpSingletonInfo: PCnSingletonInfo;
begin
  Lock;
  try
    i := _SingletonList.IndexOf(GetClassBaseOffsetStr);
    if i < 0 then begin
      inherited FreeInstance;
      Exit;
    end;

    tmpSingletonInfo := PCnSingletonInfo(_SingletonList.Objects[i]);
    if _Finalize or FForceFree then
    begin
      inherited FreeInstance;
      Dispose(PCnSingletonInfo(_SingletonList.Objects[i]));
      _SingletonList.Delete(i);
    end
    else
    begin
      if (tmpSingletonInfo^.RefCount > 1) then
        Dec(tmpSingletonInfo^.RefCount)
      else if FAutoFree then
      begin
        inherited FreeInstance;
        Dispose(PCnSingletonInfo(_SingletonList.Objects[i]));
        _SingletonList.Delete(i);
      end
      else
        tmpSingletonInfo^.RefCount := 0;
    end;
  finally
    Unlock;
  end;
end;

class procedure TCnSingleton.Lock;
begin
  _CriticalSection.Enter;
end;

class procedure TCnSingleton.Unlock;
begin
  _CriticalSection.Leave;
end;

class function TCnSingleton.Singleton: TObject;
var
  i: Integer;
  tmpSingletonInfo: PCnSingletonInfo;
  tmpSingleton: TObject;
begin
  Lock;
  try
    tmpSingleton := nil;
    tmpSingletonInfo := nil;
    if _SingletonList.Find(GetClassBaseOffsetStr, i) then
    begin
      tmpSingletonInfo := PCnSingletonInfo(_SingletonList.Objects[i]);
      tmpSingleton := tmpSingletonInfo.Instance;
    end;
    if tmpSingleton = nil then
    begin
      New(tmpSingletonInfo);
      tmpSingleton := inherited NewInstance;
      tmpSingletonInfo.Instance := tmpSingleton;
      tmpSingletonInfo.RefCount := 0;
      _SingletonList.AddObject(GetClassBaseOffsetStr, TObject(tmpSingletonInfo));
    end;
    if tmpSingletonInfo <> nil then
      Inc(tmpSingletonInfo.RefCount);
    Result := tmpSingleton;
  finally
    Unlock;
  end;
end;

class function TCnSingleton.RefCount: Cardinal;
var
  i: Integer;
  tmpSingletonInfo: PCnSingletonInfo;
begin
  Lock;
  try
    i := _SingletonList.IndexOf(GetClassBaseOffsetStr);
    if i >= 0 then
    begin
      tmpSingletonInfo := PCnSingletonInfo(_SingletonList.Objects[i]);
      Result := tmpSingletonInfo^.RefCount;
    end
    else
      Result := 0;
  finally
    Unlock;
  end;
end;

procedure TCnSingleton.SetAutoFree(const Value: Boolean);
begin
  FAutoFree := Value;
  if Value then
  begin
    if RefCount <= 0 then
      Free;
  end;
end;

procedure TCnSingleton.ForceFree;
begin
  FForceFree := True;
  Free;
end;

class function TCnSingleton.GetClassBaseOffsetStr: string;
begin
  // Self (ClassType) �����ַ��ת�����ַ�����ΪΨһ��ʶ������ַ���
  Result := IntToStr(Integer(Pointer(Self)));
end;

{ TCnSingletonList }

procedure TCnSingletonList.FreeAllSingletons;
var
  i: Integer;
  tmpSingleton: TObject;
begin
  _Finalize := True;
  for i := Count - 1 downto 0 do
  begin
    if Assigned(Objects[i]) then
    begin
      tmpSingleton := PCnSingletonInfo(Objects[i]).Instance;
      if tmpSingleton <> nil then
        FreeAndNil(tmpSingleton);
    end;
  end;
end;

initialization
  _CriticalSection := TCriticalSection.Create;
  _SingletonList := TCnSingletonList.Create;
  _SingletonList.Sorted := True;

finalization
  _SingletonList.Sorted := False;
  _SingletonList.FreeAllSingletons;
  FreeAndNil(_SingletonList);
  _CriticalSection.Free;

end.

