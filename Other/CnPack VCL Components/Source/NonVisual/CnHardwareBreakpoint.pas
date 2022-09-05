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

unit CnHardwareBreakpoint;
{* |<PRE>
================================================================================
* ������ƣ������ӹ��������
* ��Ԫ���ƣ�Ӳ���ϵ��࣬����Ӳ��HOOK��Ԫ
* ��Ԫ���ߣ�CodeGame
* ��    ע���ṩ���TCGL_VectoredException, TCGL_HardwareBreakpoints
* ����ƽ̨��PWinXP + Delphi 2007
* ���ݲ��ԣ�����
* �޸ļ�¼��2013.08.08 v1.0
*               ��ֲ��Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, SysUtils, Classes;

const
  THREAD_ALL_ACCESS = (STANDARD_RIGHTS_REQUIRED or SYNCHRONIZE or $3FF);

type
  TCallbackInstance = array[1..12] of Byte; //�����Ա�ص�

  PExceptionPointers = ^TExceptionPointers;
  TVEHCallbackProc = function(pException: PExceptionPointers): Integer of object; stdcall;
  THardwareBreakError = procedure(ErrorId: Integer; Error: Exception; pException: PExceptionPointers) of object;

  TCnVectoredException = class(TComponent) //VEH ����
  private
    { Private declarations }
    FHandler: HWND; //VEH���
    FOnCallback: TVEHCallbackProc;
    FOnCallback_Instance: TCallbackInstance;
    procedure MakeCallbackInstance(var Instance: TCallbackInstance; ObjectAddr, FunctionAddr: Pointer);
    procedure SetBreakCallbackProc(FunctionAddr: Pointer); //���ûص�������ַ
  protected
   { protected declarations }
    function DoVEHCallback(pException: PExceptionPointers): Integer; virtual; stdcall;

  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function InstallVEH: boolean; virtual; //��װVEH
    procedure RemoveVEH; virtual; //ж��VEH

    property OnCallback: TVEHCallbackProc read FOnCallback write FOnCallback;
  published
    { published declarations }

  end;
  PBreakpointsProc = ^TBreakpointProc;
  TBreakpointProc = procedure(pException: PExceptionPointers) of object;

  TCnHardwareBreakpoint = class(TCnVectoredException) //Ӳ�ϵ���
  private
    { Private declarations }
    FDr1: DWORD;
    FDr2: DWORD;
    FDr3: DWORD;
    FDr4: DWORD;
    FOnBreakpoint1: TBreakpointProc;
    FOnBreakpoint2: TBreakpointProc;
    FOnBreakpoint3: TBreakpointProc;
    FOnBreakpoint4: TBreakpointProc;
    FOnHardwareBreakError: THardwareBreakError;
  protected
   { protected declarations }
    function DoVEHCallback(pException: PExceptionPointers): Integer; override; stdcall;
    procedure DoBreakpoint1(pException: PExceptionPointers); virtual;
    procedure DoBreakpoint2(pException: PExceptionPointers); virtual;
    procedure DoBreakpoint3(pException: PExceptionPointers); virtual;
    procedure DoBreakpoint4(pException: PExceptionPointers); virtual;
    procedure DoHardwareBreakError(ErrorId: Integer; Error: Exception; pException: PExceptionPointers); virtual;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ClearBreakpoints;
    procedure SetBreakpoints; //����Ӳ���ϵ�ʹ����Ч

  published
    { published declarations }
    property BreakpointsAdderss1: DWORD read FDr1 write FDr1 default 0;
    property BreakpointsAdderss2: DWORD read FDr2 write FDr2 default 0;
    property BreakpointsAdderss3: DWORD read FDr3 write FDr3 default 0;
    property BreakpointsAdderss4: DWORD read FDr4 write FDr4 default 0;
    property OnBreakpoint1: TBreakpointProc read FOnBreakpoint1 write FOnBreakpoint1;
    property OnBreakpoint2: TBreakpointProc read FOnBreakpoint2 write FOnBreakpoint2;
    property OnBreakpoint3: TBreakpointProc read FOnBreakpoint3 write FOnBreakpoint3;
    property OnBreakpoint4: TBreakpointProc read FOnBreakpoint4 write FOnBreakpoint4;
    property OnHardwareBreakError: THardwareBreakError read FOnHardwareBreakError write FOnHardwareBreakError;

  end;

implementation

{ TCnVectoredException }

procedure TCnVectoredException.MakeCallbackInstance(var Instance: TCallbackInstance; ObjectAddr, FunctionAddr: Pointer);
  {----------------------------}
  { CallbackCode DASM          }
  {----------------------------}
  {    POP EAX;                }
  {    PUSH ObjectAddr;        }
  {    PUSH EAX;               }
  {    JMP FunctionAddr;       }
  {----------------------------}
const CallbackCode: TCallbackInstance =
//($8B,$04,$24,$50,$B8,$00,$00,$00,$00,$89,$44,$24,$04,$E9,$00,$00,$00,$00);
  ($58, $68, $00, $00, $00, $00, $50, $E9, $00, $00, $00, $00);
begin
  Move(CallbackCode, Instance, SizeOf(TCallbackInstance));
  PDWORD(@Instance[3])^ := DWORD(ObjectAddr);
  PDWORD(@Instance[9])^ := DWORD(DWORD(FunctionAddr) - DWORD(@Instance) - 12);
end;

procedure TCnVectoredException.SetBreakCallbackProc(FunctionAddr: Pointer);
begin
  MakeCallbackInstance(FOnCallback_Instance, Self, FunctionAddr);
end;

constructor TCnVectoredException.Create(AOwner: TComponent);
begin
  inherited;

  FHandler := 0;
  SetBreakCallbackProc(@TCnVectoredException.DoVEHCallback);
  InstallVEH;
end;

destructor TCnVectoredException.Destroy;
begin
  RemoveVEH;
  inherited;
end;

function TCnVectoredException.InstallVEH: boolean;
type
  TAddVectored = function(FirstHandler: Integer; VectoredHandler: Pointer): HWND; stdcall;
var
  _pAddVectored: TAddVectored;
begin
  Result := False;
  if FHandler <> 0 then Exit;
  _pAddVectored := GetProcAddress(LoadLibrary('Kernel32.dll'), 'AddVectoredExceptionHandler');
  if not Assigned(_pAddVectored) then Exit;
  FHandler := _pAddVectored(1, @Self.FOnCallback_Instance); //��װVEH
  Result := True;
end;

procedure TCnVectoredException.RemoveVEH;
type
  TRemoveVectored = function(VectoredHandler: HWND): Integer; stdcall;
var
  _pRemoveVectored: TRemoveVectored;
begin
  if FHandler = 0 then Exit;
  _pRemoveVectored := GetProcAddress(LoadLibrary('Kernel32.dll'), 'RemoveVectoredExceptionHandler');
  if Assigned(_pRemoveVectored) then _pRemoveVectored(FHandler); //ж��VEH
  FHandler := 0;
end;

function TCnVectoredException.DoVEHCallback(pException: PExceptionPointers): Integer;
begin
  Result := 0;
  if Assigned(Self.FOnCallback) then
  try
    Result := Self.FOnCallback(pException);
  except
  end;
end;

{ TCnHardwareBreakpoint }

procedure TCnHardwareBreakpoint.ClearBreakpoints;
var
  _Regs: CONTEXT;
begin
  {���öϵ�}
  FDr1 := 0;
  FDr2 := 0;
  FDr3 := 0;
  FDr4 := 0;
  _Regs.ContextFlags := CONTEXT_DEBUG_REGISTERS;
  GetThreadContext(GetCurrentThread, _Regs);
  _Regs.Dr0 := FDr1;
  _Regs.Dr1 := FDr2;
  _Regs.Dr2 := FDr3;
  _Regs.Dr3 := FDr4;
  _Regs.Dr7 := $7FF;
  SetThreadContext(GetCurrentThread, _Regs);
end;

constructor TCnHardwareBreakpoint.Create(AOwner: TComponent);
begin
  inherited;
  FDr1 := 0;
  FDr2 := 0;
  FDr3 := 0;
  FDr4 := 0;
  SetBreakCallbackProc(@TCnHardwareBreakpoint.DoVEHCallback);
end;

destructor TCnHardwareBreakpoint.Destroy;
begin
  ClearBreakpoints;
  inherited;
end;

procedure TCnHardwareBreakpoint.DoBreakpoint1(pException: PExceptionPointers);
begin
  if Assigned(Self.FOnBreakpoint1) then
  try
    Self.FOnBreakpoint1(pException);
  except
    on Error: Exception do DoHardwareBreakError(1, Error, pException);
  end;
end;

procedure TCnHardwareBreakpoint.DoBreakpoint2(pException: PExceptionPointers);
begin
  if Assigned(Self.FOnBreakpoint2) then
  try
    Self.FOnBreakpoint2(pException);
  except
    on Error: Exception do DoHardwareBreakError(2, Error, pException);
  end;
end;

procedure TCnHardwareBreakpoint.DoBreakpoint3(pException: PExceptionPointers);
begin
  if Assigned(Self.FOnBreakpoint3) then
  try
    Self.FOnBreakpoint3(pException);
  except
    on Error: Exception do DoHardwareBreakError(3, Error, pException);
  end;
end;

procedure TCnHardwareBreakpoint.DoBreakpoint4(pException: PExceptionPointers);
begin
  if Assigned(Self.FOnBreakpoint4) then
  try
    Self.FOnBreakpoint4(pException);
  except
    on Error: Exception do DoHardwareBreakError(4, Error, pException);
  end;
end;


procedure TCnHardwareBreakpoint.DoHardwareBreakError(ErrorId: Integer; Error: Exception;
  pException: PExceptionPointers); //�쳣����
begin
  if Assigned(Self.FOnHardwareBreakError) then
    Self.FOnHardwareBreakError(ErrorId, Error, pException);
end;

function TCnHardwareBreakpoint.DoVEHCallback(pException: PExceptionPointers): Integer;
begin
  Result := 0;
  case PException^.ExceptionRecord^.ExceptionCode of
    EXCEPTION_SINGLE_STEP:
      begin
        if PException^.ContextRecord^.Eip = FDr1 then Self.DoBreakpoint1(pException) else
          if PException^.ContextRecord^.Eip = FDr2 then Self.DoBreakpoint2(pException) else
            if PException^.ContextRecord^.Eip = FDr3 then Self.DoBreakpoint3(pException) else
              if PException^.ContextRecord^.Eip = FDr4 then Self.DoBreakpoint4(pException);
        Result := -1;
      end;
  end;
end;

procedure TCnHardwareBreakpoint.SetBreakpoints;
var
  _Regs: CONTEXT;
begin
  {���öϵ�}
  _Regs.ContextFlags := CONTEXT_DEBUG_REGISTERS;
  GetThreadContext(GetCurrentThread, _Regs);
  _Regs.Dr0 := FDr1;
  _Regs.Dr1 := FDr2;
  _Regs.Dr2 := FDr3;
  _Regs.Dr3 := FDr4;
  _Regs.Dr7 := $7FF;
  SetThreadContext(GetCurrentThread, _Regs);
end;

end.
