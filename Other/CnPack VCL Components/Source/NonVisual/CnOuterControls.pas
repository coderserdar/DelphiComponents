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

unit CnOuterControls;

{* |<PRE>
================================================================================
* ������ƣ�ϵͳ���������
* ��Ԫ���ƣ�ʵ�ֶ��ⲿ������Ƶ�Ԫ
* ��Ԫ���ߣ�rarnu(rarnu@cnpack.org)
* ��    ע��
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
  SysUtils, Classes, Windows, TlHelp32;

type
  TProcessInfo = record
    pHandle: Cardinal;
    pClassName: string;
    pText: string;
  end;

type
  TOnSendMessage = procedure(Sender: TObject; SndMsgResult: Cardinal) of object;
  TOnWindowChange = procedure(Sender: TObject) of object;

type
  TCnOuterControls = class(TComponent)
  private
    fProcessHandle: THandle;
    fTextList: TStringList;
    fHandleList: TStringList;
    fClassList: TStringList;
    fWindowCaption: string;
    fSM: Cardinal;
    fSLP: Cardinal;
    fSWP: Cardinal;
    fSMH: THandle;
    fOnSendMessage: TOnSendMessage;
    fOnWindowChange: TOnWindowChange;
    fPossibleWindow: TStringList;
    procedure SetProcessHandle(const Value: THandle);
    procedure SetWindowCaption(const Value: string);

  public
    constructor Create(AOwner: TComponent); override;
    {* ��ȡ���õĴ��� }
    function GetPossibleWindows: TStringList;
    {* ��ȡָ�� index �������Ϣ }
    function GetProcessControlInfo(index: Integer): TProcessInfo;
    {* ��ָ�����������Ϣ }
    procedure SendMessageToControl; overload;
    procedure SendMessageToControl(hWnd: THandle; Msg: Cardinal; WParam: Cardinal; LParam: Cardinal); overload;
  published
    {* ���õĴ����б� }
    property PossibleWindows: TStringList read fPossibleWindow write fPossibleWindow;
    {* ������Ϣʱ�����¼� }
    property OnSendMessage: TOnSendMessage read fOnSendMessage write fOnSendMessage;
    {* �����иĶ�ʱ�����¼� }
    property OnWindowChange: TOnWindowChange read fOnWindowChange write fOnWindowChange;
    property SndMsgHandle: THandle read fSMH write fSMH;
    property SndMessage: Cardinal read fSM write fSM;
    property SndLParam: Cardinal read fSLP write fSLP;
    property SndWParam: Cardinal read fSWP write fSWP;
    property ProcessHandle: THandle read fProcessHandle write SetProcessHandle;
    {* ���õľ���б� }
    property HandleList: TStringList read fHandleList;
    {* ���õ����б� }
    property ClassList: TStringList read fClassList;
    {* ���õĽ����ı��б� }
    property TextList: TStringList read fTextList;
    {* ���ڱ��� }
    property WindowCaption: string read fWindowCaption write SetWindowCaption;
  end;

var
  IHandleList: TStringList;
  IClassList: TStringList;
  ITextList: TStringList;

{* �˴�ʹ�ûص���������ɴ��ڵı�����ȡֵ }
function EnumChildWndProc(AhWnd: LongInt; AlParam: LParam): boolean; stdcall;
function EnumWindowsFunc(Handle: THandle; List: TStringList): boolean; stdcall;

implementation

function EnumChildWndProc(AhWnd: LongInt; AlParam: LParam): boolean; stdcall;
var
  WndClassName: array[0..511] of Char;
  WndCaption: array[0..511] of Char;
begin
  GetClassName(AhWnd, WndClassName, 512);   //��ȡ�ؼ�����
  GetWindowText(AhWnd, WndCaption, 512);    //��ȡ�ؼ�����
  IHandleList.Add(IntToStr(AhWnd));
  IClassList.Add(string(WndClassName));
  ITextList.Add(string(WndCaption));
  result := true;
end;

function EnumWindowsFunc(Handle: THandle; List: TStringList): boolean; stdcall;
var
  Caption: array[0..511] of Char;
begin
  if GetWindowText(Handle, Caption, 512) <> 0 then
  begin
    if List.IndexOf(Caption) = -1 then
      List.Add(Caption);
  end;
  result := true;
end;

{ TCnOuterControls }

constructor TCnOuterControls.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fTextList := TStringList.Create;
  fTextList.Clear;
  fHandleList := TStringList.Create;
  fHandleList.Clear;
  fClassList := TStringList.Create;
  fClassList.Clear;
  IHandleList := TStringList.Create;
  IHandleList.Clear;
  IClassList := TStringList.Create;
  IClassList.Clear;
  ITextList := TStringList.Create;
  ITextList.Clear;
  fPossibleWindow := TStringList.Create;
  fPossibleWindow.Clear;
  GetPossibleWindows;
end;

function TCnOuterControls.GetPossibleWindows: TStringList;
begin
  fPossibleWindow.Clear;
  EnumWindows(@EnumWindowsFunc, LParam(fPossibleWindow));
  result := fPossibleWindow;
end;

function TCnOuterControls.GetProcessControlInfo(
  index: Integer): TProcessInfo;
var
  piInfo: TProcessInfo;
begin
  piInfo.pHandle := 0;
  piInfo.pClassName := '';
  piInfo.pText := '';
  if fHandleList.Count - 1 < index then
  begin
    result := piInfo;
    Exit;
  end;
  piInfo.pHandle := StrToInt(fHandleList.Strings[index]);
  piInfo.pClassName := fClassList.Strings[index];
  piInfo.pText := fTextList.Strings[index];
  result := piInfo;
end;

procedure TCnOuterControls.SendMessageToControl;
var
  SndResult: Cardinal;
begin
  SndResult := SendMessage(fSMH, fSM, fSWP, fSLP);
  if Assigned(OnSendMessage) then
    OnSendMessage(self, SndResult);
end;

procedure TCnOuterControls.SendMessageToControl(hWnd: THandle; Msg, WParam,
  LParam: Cardinal);
var
  SndResult: Cardinal;
begin
  SndResult := SendMessage(hWnd, Msg, WParam, LParam);
  if Assigned(OnSendMessage) then
    OnSendMessage(self, SndResult);
end;

procedure TCnOuterControls.SetProcessHandle(const Value: THandle);
begin
  fProcessHandle := Value;
  IHandleList.Clear;
  IClassList.Clear;
  ITextList.Clear;
  if fProcessHandle <> 0 then EnumChildWindows(fProcessHandle, @EnumChildWndProc, 0);
  fTextList := ITextList;
  fHandleList := IHandleList;
  fClassList := IClassList;
  if Assigned(OnWindowChange) then
    OnWindowChange(self);
end;

procedure TCnOuterControls.SetWindowCaption(const Value: string);
begin
  fWindowCaption := Value;
  ProcessHandle := FindWindow(nil, PChar(fWindowCaption));
end;

end.

 
