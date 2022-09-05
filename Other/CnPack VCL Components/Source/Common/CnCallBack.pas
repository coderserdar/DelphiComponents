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

unit CnCallBack;
{* |<PRE>
================================================================================
* ������ƣ�CnPack �����
* ��Ԫ���ƣ��ص�ת���Ĺ��ߵ�Ԫ
* ��Ԫ���ߣ�CnPack������ savetime (savetime2k@yahoo.com)
*           ��Х (liuxiao@cnpack.org)
* ��    ע���õ�Ԫ�ǻص�ת���ȵĴ��뵥Ԫ
*           ��װ�Ĵ��벿�������з���Ŀ�ִ�е��ڴ�ռ䣬������ DEP �³���
* ����ƽ̨��PWin2000 + Delphi 5.0
* ���ݲ��ԣ�PWin9X/2000/XP + Delphi 5/6/7
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2006.10.13 V1.0
*               ������Ԫ��ʵ�ֹ���
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Classes, Windows, SysUtils;

type
  ECallBackException = class(Exception)
  end;

function StdcallMethodToCallBack(ASelf: Pointer; AMethodAddr: Pointer): Pointer;
{* �� stdcall �����Ա������ʵ�����԰�װ������һ���µ� stdcall �Ļص�������ַ }

{* ʹ���﷨:
  @AStdCallbackFunc := StdcallMethodToCallBack(AObject, @TAObject.CallbackMethod);
  ���� AStdCallbackFunc �� CallbackMethod ������ʹ�� stdcall ������
}

implementation

type
  TCnCallback = array [1..18] of Byte; // �������������
  PCnCallback = ^TCnCallback;
  
const
  THUNK_SIZE = 4096; // x86 ҳ��С��ĿǰֻŪһ��ҳ��

  StdcallCode: TCnCallback =
    ($8B,$04,$24,$50,$B8,$00,$00,$00,$00,$89,$44,$24,$04,$E9,$00,$00,$00,$00);

  {----------------------------}
  { Stdcall CallbackCode ASM   }
  {----------------------------}
  {    MOV EAX, [ESP];         }
  {    PUSH EAX;               }
  {    MOV EAX, ASelf;         }
  {    MOV [ESP+4], EAX;       }
  {    JMP AMethodAddr;        }
  {----------------------------}

var
  FCallBackPool: Pointer = nil;
  FEmptyPtr: Integer = 0;
  FCS: TRTLCriticalSection;

procedure InitCallBackPool;
begin
  FCallBackPool := VirtualAlloc(nil, THUNK_SIZE, MEM_COMMIT, PAGE_EXECUTE_READWRITE);
  if FCallBackPool = nil then
    raise ECallBackException.Create('Callback Pool Init Error!');
end;

function StdcallMethodToCallBack(ASelf: Pointer; AMethodAddr: Pointer): Pointer;
var
  Instance: PCnCallback;
begin
  Result := nil;
  Instance := nil;
  
  try
    EnterCriticalSection(FCS);

    if FCallBackPool = nil then
    begin
      InitCallBackPool;
      Instance := FCallBackPool;
    end
    else
    begin
      if FEmptyPtr = (THUNK_SIZE div SizeOf(TCnCallback)) then
        raise ECallBackException.Create('Callback Pool Overflow!');

      Inc(FEmptyPtr);
      Instance := PCnCallback(Integer(FCallBackPool) + FEmptyPtr * SizeOf(TCnCallback));
    end;
  finally
    LeaveCriticalSection(FCS);
  end;

  if Instance <> nil then
  begin
    Move(StdcallCode, Instance^, SizeOf(TCnCallback));
    PInteger(@(Instance^[6]))^ := Integer(ASelf);
    PInteger(@(Instance^[15]))^ := Integer(Integer(AMethodAddr) - Integer(Instance) - 18);
    Result := Instance;
  end;
end;

initialization
  InitializeCriticalSection(FCS);

finalization
  DeleteCriticalSection(FCS);
  if FCallBackPool <> nil then
    VirtualFree(FCallBackPool, 0, MEM_RELEASE);

end.
