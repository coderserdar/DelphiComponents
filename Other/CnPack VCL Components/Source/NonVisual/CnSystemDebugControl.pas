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

unit CnSystemDebugControl;
{* |<PRE>
================================================================================
* ������ƣ�CnPack ��������������
* ��Ԫ���ƣ���װ�� SystemDebugControl ϵͳ���õ������Ԫ
* ��Ԫ���ߣ���Х��liuxiao@cnpack.org)
* ����ƽ̨��PWinXPPro + Delphi 5.01
* ���ݲ��ԣ�PWin9X/2000/XP + Delphi 5/6/7/2005 + C++Build 5/6
* ������ע���ο��������ڕD�� SystemDebugControl ���������Լ�����������Դ
* �޸ļ�¼��2019.05.19 V1.1
*               �������� GetModuleHandle ���¿����ͷų��������
*           2008.09.18 V1.0
*               LiuXiao ʵ�ֵ�Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Windows, Classes, CnCommon;

type
  _DEBUG_CONTROL_CODE = (
    SysDbgDummyZero,  // 0,
    //����5����Windows NT�����汾�϶���
    SysDbgGetTraceInformation,   // 1,
    SysDbgSetInternalBreakpoint, // 2,
    SysDbgSetSpecialCall,        // 3,
    SysDbgClearSpecialCalls,     // 4,
    SysDbgQuerySpecialCalls,     // 5,

    // ������NT 5.1 ������
    SysDbgDbgBreakPointWithStatus,  // 6,

    //��ȡKdVersionBlock
    SysDbgSysGetVersion,  // 7,

    //���ں˿ռ俽�����û��ռ䣬���ߴ��û��ռ俽�����û��ռ�
    //���ǲ��ܴ��û��ռ俽�����ں˿ռ�
    SysDbgCopyMemoryChunks_0,  // 8,

    //���û��ռ俽�����ں˿ռ䣬���ߴ��û��ռ俽�����û��ռ�
    //���ǲ��ܴ��ں˿ռ俽�����û��ռ�
    SysDbgCopyMemoryChunks_1,  // 9,
  
    //�������ַ�������û��ռ䣬����д���ں˿ռ�
    SysDbgCopyMemoryChunks_2,  // 10,
  
    //���û��ռ俽���������ַ�����ܶ�ȡ�ں˿ռ�
    SysDbgCopyMemoryChunks_3,  // 11,
  
    //��д��������ؿ��ƿ�
    SysDbgSysReadControlSpace,   // 12,
    SysDbgSysWriteControlSpace,  // 13,

    //��д�˿�
    SysDbgSysReadIoSpace,   // 14,
    SysDbgSysWriteIoSpace,  // 15,

    //�ֱ����RDMSR��WRMSR
    SysDbgSysReadMsr,   // 16,
    SysDbgSysWriteMsr,  // 17,

    //��д��������
    SysDbgSysReadBusData,     // 18,
    SysDbgSysWriteBusData,    // 19,

    SysDbgSysCheckLowMemory,  // 20,

    // ������NT 5.2 ������
    //�ֱ����_KdEnableDebugger��_KdDisableDebugger
    SysDbgEnableDebugger,   // 21,
    SysDbgDisableDebugger,  // 22,
    
    //��ȡ������һЩ������صı���
    SysDbgGetAutoEnableOnEvent,  // 23,
    SysDbgSetAutoEnableOnEvent,  // 24,
    SysDbgGetPitchDebugger,      // 25,
    SysDbgSetDbgPrintBufferSize, // 26,
    SysDbgGetIgnoreUmExceptions, // 27,
    SysDbgSetIgnoreUmExceptions  // 28
  );
  DEBUG_CONTROL_CODE = _DEBUG_CONTROL_CODE;

  TIOStruct = record
    IoAddr: DWORD;
    Reserved1: DWORD;
    pBuffer: Pointer;
    NumBYTEs: DWORD;
    Reserved4: DWORD;
    Reserved5: DWORD;
    Reserved6: DWORD;
    Reserved7: DWORD;
  end;

  _MEMORY_CHUNKS = record
    Address: ULONG;
    Data: Pointer;
    Length: ULONG;
  end;
  MEMORY_CHUNKS = _MEMORY_CHUNKS;
  PMEMORY_CHUNKS = ^_MEMORY_CHUNKS;

  _DBGKD_GET_VERSION64 = packed record
    MajorVersion:    Word;
    MinorVersion:    Word;
    ProtocolVersion: Word;
    Flags:           Word;
    MachineType:     Word;
    MaxPacketType:   Byte;
    MaxStateChange:  Byte;
    MaxManipulate:   Byte;
    Simulation:      Byte;
    Unused:          Word;
    KernBase:           Int64;
    PsLoadedModuleList: Int64;
    DebuggerDataList:   Int64;
  end;
  DBGKD_GET_VERSION64 = _DBGKD_GET_VERSION64;
  PDBGKD_GET_VERSION64 = ^_DBGKD_GET_VERSION64;

  TCnSystemDebugControl = class(TComponent)
  private
    FKernelBase: Int64;

    procedure KbcWait4IBE;
    {* �ȴ����̻�����Ϊ��}
    procedure InternalCopyMemory(Code: DEBUG_CONTROL_CODE; Address: Cardinal;
      Memory: Pointer; Length: Cardinal);
    {* ��װ���ڴ渴�Ʋ���}
    procedure SysGetVersion;
    {* ��װ��ȡ�汾�ŵĲ���}
    function GetKernelBase: Cardinal;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function InPortB(Port: DWORD): Byte;
    {* ������˿�}
    procedure OutPortB(Port: DWORD; Value: Byte);
    {* д����˿�}

    // �����Ƕ�д����˿ڵľ���Ӧ�÷�װ
    procedure SimuKeyDown(VKey: Cardinal);
    {* ģ�ⰴ��һ����}
    procedure SimuKeyUp(VKey: Cardinal);
    {* ģ��̧��һ����}
    procedure SimuKey(VKey: Cardinal);
    {* ģ�ⰴ��̧��һ����}
    procedure BeepOn(Freq: Integer);
    {* ��ָ��Ƶ�ʿ�ʼ������������}
    procedure BeepOff;
    {* ��������ֹͣ����}
    function ReadCMOS(Index: Byte): Byte;
    {* �� CMOS ����}
    function ReadFirstHardDiskSerialNumber: string;
    {* ��ȡӲ�����к�}

    procedure ReadKernelMemory(Address: Cardinal; Memory: Pointer;
      Length: Cardinal);
    {* ��ȡ�ں˿ռ�ָ����ַ��ָ�����ȵ����ݣ����ݽ����Ƶ� Memory ָ�Ŀռ���}
    procedure WriteKernelMemory(Address: Cardinal; Memory: Pointer;
      Length: Cardinal);
    {* ��ָ�� Memory ��ַ��ָ�����ȵ�����д���ں˿ռ�ָ����ַ}
    procedure ReadPhysicalMemory(Address: Cardinal; Memory: Pointer;
      Length: Cardinal);
    {* ��ȡ����ָ����ַ��ָ�����ȵ����ݣ����ݽ����Ƶ� Memory ָ�Ŀռ���}
    procedure WritePhysicalMemory(Address: Cardinal; Memory: Pointer;
      Length: Cardinal);
    {* ��ָ�� Memory ��ַ��ָ�����ȵ�����д��ָ�������ַ}

    property KernelBase: Cardinal read GetKernelBase;
    {* �ں�ӳ���ַ���� MZ ���ַ�}
  end;

implementation

const
  KBC_KEY_CMD  = $64; // �������ݶ˿ں�
  KBC_KEY_DATA = $60; // ���̲����˿ں�

type
  TZwSystemDebugControl = function (
    ControlCode: _DEBUG_CONTROL_CODE;
    InputBuffer: Pointer;
    InputBufferLength: ULONG;
    OutputBuffer: Pointer;
    OutputBufferLength: ULONG;
    ReturnLength: PULONG): LongInt; stdcall;

var
  NtDllHandle: THandle = 0;
  ZwSystemDebugControl: TZwSystemDebugControl = nil;

{ TCnSystemDebugControl }

procedure TCnSystemDebugControl.BeepOff;
var
  B: Byte;
begin
  B := InPortB($61) and $FC;
  OutPortB($61, B);
end;

procedure TCnSystemDebugControl.BeepOn(Freq: Integer);
var
  B: Byte;
begin
  if (Freq >= 20) and (Freq <= 20000) then
  begin
    Freq := Trunc(1193181 / Freq);
    B := InPortB($61);
    if (B and 3) = 0 then
    begin
      OutPortB($61, (B or 3));
      OutPortB($43, $B6);
    end;
    OutPortB($42, Byte(Freq and $FF));
    OutPortB($42, Byte(Freq shr 8));
  end;
end;

constructor TCnSystemDebugControl.Create(AOwner: TComponent);
begin
  inherited;
  if NtDllHandle = 0 then
    raise Exception.Create('Only Windows XP/2003 or Later can be Supported.');
end;

destructor TCnSystemDebugControl.Destroy;
begin

  inherited;
end;

function TCnSystemDebugControl.InPortB(Port: DWORD): Byte;
var
  Value: BYTE;
  Io: TIOStruct;
begin
  Value := 0;
  Io.IoAddr := Port;
  Io.Reserved1 := 0;
  Io.pBuffer := Pointer(@Value);
  Io.NumBYTEs := SizeOf(Byte);
  Io.Reserved4 := 1;
  Io.Reserved5 := 0;
  Io.Reserved6 := 1;
  Io.Reserved7 := 0;
  ZwSystemDebugControl(SysDbgSysReadIoSpace, @Io, SizeOf(Io), nil, 0, nil);
  Result := Value;
end;

procedure TCnSystemDebugControl.KbcWait4IBE;
var
  RegVal: DWORD;
begin
  repeat
    RegVal := InPortB(KBC_KEY_CMD);
  until (RegVal and $00000002) = 0;
end;

procedure TCnSystemDebugControl.OutPortB(Port: DWORD; Value: Byte);
var
  Io: TIOStruct;
begin
  Io.IoAddr := Port;
  Io.Reserved1 := 0;
  Io.pBuffer := Pointer(@Value);
  Io.NumBYTEs := SizeOf(Byte);
  Io.Reserved4 := 1;
  Io.Reserved5 := 0;
  Io.Reserved6 := 1;
  Io.Reserved7 := 0;
  ZwSystemDebugControl(SysDbgSysWriteIoSpace, @Io, sizeof(Io), nil, 0, nil);
end;

procedure TCnSystemDebugControl.ReadKernelMemory(Address: Cardinal;
  Memory: Pointer; Length: Cardinal);
begin
  InternalCopyMemory(SysDbgCopyMemoryChunks_0, Address, Memory, Length);
end;

procedure TCnSystemDebugControl.ReadPhysicalMemory(Address: Cardinal;
  Memory: Pointer; Length: Cardinal);
begin
  InternalCopyMemory(SysDbgCopyMemoryChunks_2, Address, Memory, Length);
end;

procedure TCnSystemDebugControl.WriteKernelMemory(Address: Cardinal;
  Memory: Pointer; Length: Cardinal);
begin
  InternalCopyMemory(SysDbgCopyMemoryChunks_1, Address, Memory, Length);
end;

procedure TCnSystemDebugControl.WritePhysicalMemory(Address: Cardinal;
  Memory: Pointer; Length: Cardinal);
begin
  InternalCopyMemory(SysDbgCopyMemoryChunks_3, Address, Memory, Length);
end;

procedure TCnSystemDebugControl.SimuKey(VKey: Cardinal);
begin
  SimuKeyDown(VKey);
  SimuKeyUp(VKey);
end;

procedure TCnSystemDebugControl.SimuKeyDown(VKey: Cardinal);
var
  ScanCode: Cardinal;
begin
  ScanCode := MapVirtualKey(VKey, 0);
  KBCWait4IBE;                               // ��������ǰӦ���ȵȴ����̻�����Ϊ��
  OutPortB(KBC_KEY_CMD, $D2);                // ���ͼ���д������, 0xD2:д���̻�����,0xD3:д��껺����,
  KBCWait4IBE;
  OutPortB(KBC_KEY_DATA, ScanCode);          // д�밴����Ϣ,���¼�
end;

procedure TCnSystemDebugControl.SimuKeyUp(VKey: Cardinal);
var
  ScanCode: Cardinal;
begin
  ScanCode := MapVirtualKey(VKey, 0);
  KBCWait4IBE;                               // �ȴ����̻�����Ϊ��
  OutPortB(KBC_KEY_CMD, $D2);                // ���ͼ���д������
  KBCWait4IBE;
  OutPortB(KBC_KEY_DATA, (ScanCode or $80)); // д�밴����Ϣ���ͷż�
end;

function GetNtNativeAPIs: Boolean;
begin
  if (Win32Platform = VER_PLATFORM_WIN32_NT)
    and (Win32MajorVersion >= 5) and (Win32MinorVersion >= 1) then
  begin
    // ������ GetModuleHandle����Ϊ�����޷��ж���� Handle զ���ģ����ܵ��´���� FreeLibrary
    if NtDllHandle = 0 then
      NtDllHandle := LoadLibrary('NTDLL.DLL');

    if NtDllHandle <> 0 then
    begin
      @ZwSystemDebugControl := GetProcAddress(NtDllHandle, 'ZwSystemDebugControl');
    end;
  end;

  Result := NtDllHandle <> 0;
end;

procedure FreeNtNativeAPIs;
begin
  if NtDllHandle <> 0 then
  begin
    FreeLibrary(NtDllHandle);
    NtDllHandle := 0;
  end;
end;

procedure TCnSystemDebugControl.InternalCopyMemory(
  Code: DEBUG_CONTROL_CODE; Address: Cardinal; Memory: Pointer;
  Length: Cardinal);
var
  M: MEMORY_CHUNKS;
  Len: Integer;
begin
  if Code in [SysDbgCopyMemoryChunks_0..SysDbgCopyMemoryChunks_3] then
  begin
    M.Address := Address;
    M.Data := Memory;
    M.Length := Length;
    ZwSystemDebugControl(Code, @M, SizeOf(MEMORY_CHUNKS), nil, 0, @Len);
  end;
end;

function TCnSystemDebugControl.GetKernelBase: Cardinal;
begin
  if FKernelBase = 0 then
    SysGetVersion;
  Result := Cardinal(FKernelBase);
end;

procedure TCnSystemDebugControl.SysGetVersion;
var
  Block: DBGKD_GET_VERSION64;
begin
  ZwSystemDebugControl(SysDbgSysGetVersion, nil, 0, @Block,
    SizeOf(DBGKD_GET_VERSION64), nil);

  FKernelBase := Block.KernBase;
end;

function TCnSystemDebugControl.ReadCMOS(Index: Byte): Byte;
begin
  OutPortB($70, Index);     // дҪ��������ֵ
  // Sleep(0);
  Result := InPortB($71);   // ��ֵ
end;

function TCnSystemDebugControl.ReadFirstHardDiskSerialNumber: string;
var
  I: Integer;

  function WaitUntilIdle: Byte;
  begin
    Result := InPortB($1F7);
    while Result >= $80 do
      Result := InPortB($1F7);
  end;

begin
  WaitUntilIdle;
  OutPortB($1F6, $A0);

  if (WaitUntilIdle and $50) <> $50 then
    Exit;

  OutPortB($1F6, $A0);
  OutPortB($1F7, $EC);

  if (WaitUntilIdle and $58) <> $58 then
    Exit;

  SetLength(Result, 512);
  for I := 0 to 511 do
    Result[I + 1] := Chr(InPortB($1F0));
end;

initialization
  GetNtNativeAPIs;
  AdjustDebugPrivilege(True);

finalization
  AdjustDebugPrivilege(False);
  FreeNtNativeAPIs;

end.
