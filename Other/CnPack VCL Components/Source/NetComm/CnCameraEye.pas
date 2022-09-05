{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     �й����Լ��Ŀ���Դ�������������                         }
{                   (C)Copyright 2001-2006 CnPack ������                       }
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

unit CnCameraEye;

{* |<PRE>
================================================================================
* ������ƣ�����豸�����
* ��Ԫ���ƣ�ʵ������ͷ���Ƶ�Ԫ
* ��Ԫ���ߣ�rarnu(rarnu@cnpack.org)
* ��    ע����δ����������ͷ�ļ��
* ����ƽ̨��Windows2003 Server + Delphi2007 up2
* ���ݲ��ԣ�Windows2000/XP/2003/Vista + Delphi 7/2006/2007/2009
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2008.08.14 V1.0
*                ������Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, Controls, Windows, Messages;

type
  TCnCameraEye = class(TComponent)
  private
    FDisplay: TWinControl;
    FOnStart: TNotifyEvent;
    FOnStartRecord: TNotifyEvent;
    FOnStop: TNotifyEvent;
    FOnStopRecord: TNotifyEvent;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Start;
    {* ��ʼ���� }
    procedure Stop;
    {* ֹͣ���� }
    procedure SaveToBmp(const FileName: string);
    {* ��ͼ�����浽bmp }
    procedure RecordToAVI(const FileName: string);
    {* ¼��AVI }
    procedure StopRecord;
    {* ֹͣ¼�� }
  published
    property Display: TWinControl read FDisplay write FDisplay;
    {* ͼ����ʾ���� }
    property OnStart: TNotifyEvent read FOnStart write FOnStart;
    {* ��ʼ�����¼� }
    property OnStop: TNotifyEvent read FOnStop write FOnStop;
    {* ֹͣ�����¼� }
    property OnStartRecord: TNotifyEvent read FOnStartRecord write FOnStartRecord;
    {* ��ʼ¼���¼� }
    property OnStopRecord: TNotifyEvent read FOnStopRecord write FOnStopRecord;
    {* ֹͣ¼���¼� }
  end;

implementation

{* ��Ϣ�������� }

const
  WM_CAP_START = WM_USER;
  WM_CAP_STOP = WM_CAP_START + 68;
  WM_CAP_DRIVER_CONNECT = WM_CAP_START + 10;
  WM_CAP_DRIVER_DISCONNECT = WM_CAP_START + 11;
  WM_CAP_SAVEDIB = WM_CAP_START + 25;
  WM_CAP_GRAB_FRAME = WM_CAP_START + 60;
  WM_CAP_SEQUENCE = WM_CAP_START + 62;
  WM_CAP_FILE_SET_CAPTURE_FILEA = WM_CAP_START + 20;
  WM_CAP_SEQUENCE_NOFILE = WM_CAP_START + 63;
  WM_CAP_SET_OVERLAY = WM_CAP_START + 51;
  WM_CAP_SET_PREVIEW = WM_CAP_START + 50;
  WM_CAP_SET_CALLBACK_VIDEOSTREAM = WM_CAP_START + 6;
  WM_CAP_SET_CALLBACK_ERROR = WM_CAP_START + 2;
  WM_CAP_SET_CALLBACK_STATUSA = WM_CAP_START + 3;
  WM_CAP_SET_CALLBACK_FRAME = WM_CAP_START + 5;
  WM_CAP_SET_SCALE = WM_CAP_START + 53;
  WM_CAP_SET_PREVIEWRATE = WM_CAP_START + 52;

{* ������̬�������˺�����DLL�е��룬��̬�ж��Ƿ���� }
type
  TFunCap = function(
    lpszWindowName: PCHAR;
    dwStyle: longint;
    x: integer;
    y: integer;
    nWidth: integer;
    nHeight: integer;
    ParentWin: HWND;
    nId: integer): HWND; stdcall;

var
  hWndC: THandle;
  FunCap: TFunCap;
  DllHandle: THandle;

{ TCnCameraEye }

constructor TCnCameraEye.Create(AOwner: TComponent);
var
  FPointer: Pointer;
begin
  inherited Create(AOwner);
  FDisplay := nil;

  {* ͨ��DLL���룬���DLL�����ڣ���ʾû������ }
  DllHandle := LoadLibrary('AVICAP32.DLL');
  if DllHandle <= 0 then
    raise Exception.Create('Camera driver not installed or invalid.');

  FPointer := GetProcAddress(DllHandle, 'capCreateCaptureWindowA');
  FunCap := TFunCap(FPointer);
end;

destructor TCnCameraEye.Destroy;
begin
  StopRecord;
  Stop;
  FDisplay := nil;
  
  {* ����Ѽ���DLL�����ͷŵ� }
  if DllHandle > 0 then
    FreeLibrary(DllHandle);
  inherited;
end;

procedure TCnCameraEye.RecordToAVI(const FileName: string);
begin
  if hWndC <> 0 then
  begin
    SendMessage(hWndC, WM_CAP_FILE_SET_CAPTURE_FILEA, 0, LongInt(PAnsiChar(AnsiString(FileName))));
    SendMessage(hWndC, WM_CAP_SEQUENCE, 0, 0);
    if Assigned(FOnStartRecord) then
      FOnStartRecord(Self);
  end;
end;

procedure TCnCameraEye.SaveToBmp(const FileName: string);
begin
  if hWndC <> 0 then
    SendMessage(hWndC, WM_CAP_SAVEDIB, 0, LongInt(PAnsiChar(AnsiString(FileName))));
end;

procedure TCnCameraEye.Start;
var
  OHandle: THandle;
begin
  if FDisplay = nil then Exit;
  OHandle := TWinControl(Owner).Handle;
  hWndC := FunCap(
    'My Own Capture Window',
    WS_CHILD or WS_VISIBLE,
    FDisplay.Left, FDisplay.Top, FDisplay.Width, FDisplay.Height,
    OHandle, 0);

  if hWndC <> 0 then
  begin
    {* ����ָ�� }
    SendMessage(hWndC, WM_CAP_SET_CALLBACK_VIDEOSTREAM, 0, 0);
    SendMessage(hWndC, WM_CAP_SET_CALLBACK_ERROR, 0, 0);
    SendMessage(hWndC, WM_CAP_SET_CALLBACK_STATUSA, 0, 0);
    SendMessage(hWndC, WM_CAP_DRIVER_CONNECT, 0, 0);
    SendMessage(hWndC, WM_CAP_SET_SCALE, 1, 0);
    SendMessage(hWndC, WM_CAP_SET_PREVIEWRATE, 66, 0);
    SendMessage(hWndC, WM_CAP_SET_OVERLAY, 1, 0);
    SendMessage(hWndC, WM_CAP_SET_PREVIEW, 1, 0);
  end;

  if Assigned(OnStart) then
    FOnStart(Self);
end;

procedure TCnCameraEye.Stop;
begin
  if hWndC <> 0 then
  begin
    SendMessage(hWndC, WM_CAP_DRIVER_DISCONNECT, 0, 0);
    hWndC := 0;
    if Assigned(FOnStop) then
      FOnStop(Self);
  end;
end;

procedure TCnCameraEye.StopRecord;
begin
  if hWndC <> 0 then
  begin
    SendMessage(hWndC, WM_CAP_STOP, 0, 0);
    if Assigned(FOnStopRecord) then
      FOnStopRecord(Self);
  end;
end;

end.
