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

unit CnKeyBlocker;
{* |<PRE>
================================================================================
* ������ƣ������ӹ��������
* ��Ԫ���ƣ�ͨ�����̹�������ϵͳ�������
* ��Ԫ���ߣ�����
* ��    ֲ����Х (liuxiao@cnpack.org)
* ��    ע�������ͨ��ʵ�ּ��̹���������ĳЩϵͳ������ Ctrl+Alt+Del ��ϼ�����
*           ��Ϊϵͳ������ԭ����޷����Ρ�
* ����ƽ̨��PWinXP + Delphi 7.0 (Build 8.1)
* ���ݲ��ԣ�PWin2003 + Delphi 7.0 (Build 8.1)
* �� �� �����õ�Ԫ�����ַ�����Դ
* �޸ļ�¼��2008.10.24 v1.1
*               ����һ��ª�¼�
*           2008.05.29 v1.0
*               ��ֲ��Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, windows, ShlObj, Registry, Shellapi, Messages;

type
  TCnBlockKeyEvent = procedure(VirtualKey: Cardinal) of object;

  TCnKeyBlocker = class(TComponent)
  private
    FBCAD: Boolean;
    FBAT: Boolean;
    FBCE: Boolean;
    FEnabled: Boolean;
    FBAE: Boolean;
    FBCR: Boolean;
    FBCK: Boolean;
    FBP: Boolean;
    FBS: Boolean;
    FCKC: Cardinal;
    FBWA: Boolean;
    FBCAE: Boolean;
    FOnBlockKey: TCnBlockKeyEvent;
    procedure SetBCAD(const Value: Boolean);
    procedure SetBAT(const Value: Boolean);
    procedure SetBCE(const Value: Boolean);
    procedure SetEnabled(const Value: Boolean);
    procedure SetBAE(const Value: Boolean);
    procedure SetBCK(const Value: Boolean);
    procedure SetBCR(const Value: Boolean);
    procedure SetBP(const Value: Boolean);
    procedure SetBS(const Value: Boolean);
    procedure SetBWA(const Value: Boolean);
    procedure SetBCAE(const Value: Boolean);
  protected
    procedure UpdateKeyBlock;
    procedure DoBlock(VirtualKey: Cardinal);
    property BlockCtrlAltDelete: Boolean read FBCAD write SetBCAD;
    {* �Ƿ����� Ctrl+Alt+Delete ���������Կ����޷���������ʱ������}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property BlockAltTab: Boolean read FBAT write SetBAT;
    {* �Ƿ����� Alt+Tab ��}
    property BlockCtrlEsc: Boolean read FBCE write SetBCE;
    {* �Ƿ����� Ctrl+Esc ��}
    property BlockAltEsc: Boolean read FBAE write SetBAE;
    {* �Ƿ����� Alt+Esc ��}
    property BlockCtrlEnter: Boolean read FBCR write SetBCR;
    {* �Ƿ����� Ctrl+Enter ��}
    property BlockSleep: Boolean read FBS write SetBS;
    {* �Ƿ����� Sleep ���߼�}
    property BlockPower: Boolean read FBP write SetBP;
    {* �Ƿ����� Power ��Դ��}
    property BlockWinApps: Boolean read FBWA write SetBWA;
    {* �Ƿ����� Windows ��}
    property BlockCtrlAltEnter: Boolean read FBCAE write SetBCAE;
    {* �Ƿ����� Ctrl+Alt+Enter ��}

    property CustomKeyCode: Cardinal read FCKC write FCKC default 0;
    {* �Զ�������μ�}
    property BlockCustomKey: Boolean read FBCK write SetBCK;
    {* �Ƿ������Զ����}

    property Enabled: Boolean read FEnabled write SetEnabled default False;
    {* �Ƿ�ʹ�����ι���}

    property OnBlockKey: TCnBlockKeyEvent read FOnBlockKey write FOnBlockKey;
    {* ���μ�ʱ�������¼������ڸ����ԣ�������ָֻ����������������ڹҽӻ��Ʊ���
       �Ļ��ƣ����¼��� Sender��}
  end;

implementation

const
  LLKHF_ALTDOWN = KF_ALTDOWN shr 8;
  WH_KEYBOARD_LL = 13;

type
  PKBDLLHOOKSTRUCT = ^KBDLLHOOKSTRUCT;
  KBDLLHOOKSTRUCT = packed record
    vkCode: DWORD;
    scanCode: DWORD;
    flags: DWORD;
    Time: DWORD;
    dwExtraInfo: DWORD;
  end;

var
  hhkNTKeyboard: HHOOK = 0;
  aBCAD: Boolean = False;
  aBWA: Boolean = False;
  aBCE: Boolean = False;
  aBAT: Boolean = False;
  aBAE: Boolean = False;
  aBCR: Boolean = False;
  aBCAE: Boolean = False;
  aBP: Boolean = False;
  aaBS: Boolean = False;
  aBCK: Boolean = False;
  aCKC: Cardinal = 0;

  FKeyBlocker: TCnKeyBlocker = nil;

{ TCnKeyBlocker }

constructor TCnKeyBlocker.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FKeyBlocker := Self;
end;

procedure EnableCTRLALTDEL(YesNo: Boolean);
const
  sRegPolicies = '\Software\Microsoft\Windows\CurrentVersion\Policies';
begin
  with TRegistry.Create do
  try
    RootKey := HKEY_CURRENT_USER;
    if OpenKey(sRegPolicies + '\System\', True) then
    begin
      case YesNo of
        False:
          begin
            WriteInteger('DisableTaskMgr', 1); //�������
            WriteInteger('DisableLockWorkstation', 1); //�û����������
            WriteInteger('DisableChangePassword', 1); //�û����Ŀ���
          end;
        True:
          begin
            WriteInteger('DisableTaskMgr', 0);
            WriteInteger('DisableLockWorkstation', 0);
            WriteInteger('DisableChangePassword', 0);
          end;
      end;
    end;
    CloseKey;
    if OpenKey(sRegPolicies + '\Explorer\', True) then
    begin
      case YesNo of
        False:
          begin
            WriteInteger('NoChangeStartMenu', 1); //��ʼ�˵�
            WriteInteger('NoClose', 1); // �ر�ϵͳ�˵�
            WriteInteger('NoLogOff', 1); //ע���˵�
            WriteInteger('NoRun', 1); //���в˵�
            WriteInteger('NoSetFolders', 1); //���ò˵�
          end;
        True:
          begin
            WriteInteger('NoChangeStartMenu', 0);
            WriteInteger('NoClose', 0);
            WriteInteger('NoLogOff', 0);
            WriteInteger('NoRun', 0);
          end;
      end;
    end;
    CloseKey;
  finally
    Free;
  end;
end;

function LowLevelKeyboardFunc(nCode: INTEGER; w_Param: WPARAM;
  l_Param: LPARAM): LRESULT; stdcall;
var
  boolKey: Boolean;
  p: PKBDLLHOOKSTRUCT;
const
  VK_SLEEP = $5F;
  VK_POWER = $5E;
begin
  boolKey := False;
  p := nil;
  if nCode = HC_ACTION then
  begin
    case w_Param of
      WM_KEYDOWN, WM_SYSKEYDOWN, WM_KEYUP, WM_SYSKEYUP:
        begin
          p := PKBDLLHOOKSTRUCT(l_Param);
      //---------!-~------------------------------------------------
      {    if ((GetAsyncKeyState(VK_RBUTTON) and $8000) <> 0) then boolKey := True;
          if (CHAR(p.vkCode) >= '!') and (CHAR(p.vkCode) <= '~') and
            ((GetKeyState(VK_CONTROL) and $8000) <> 0) then boolKey := True;
          if (p.vkCode = VK_SPACE) and
            ((GetKeyState(VK_CONTROL) and $8000) <> 0) then boolKey := True;    }

      //---------F1-F12 ----------------------------------------------
      {    if (p.vkCode = VK_F1) or (p.vkCode = VK_F2) or (p.vkCode = VK_F3) or
            (p.vkCode = VK_F4) or (p.vkCode = VK_F5) or (p.vkCode = VK_F6) or
            (p.vkCode = VK_F7) or (p.vkCode = VK_F8) or (p.vkCode = VK_F9) or
            (p.vkCode = VK_F10) or (p.vkCode = VK_F11) or (p.vkCode = VK_F12) then
            boolKey := True;

          if ((p.vkCode = VK_F1) or (p.vkCode = VK_F2) or (p.vkCode = VK_F3) or
            (p.vkCode = VK_F4) or (p.vkCode = VK_F5) or (p.vkCode = VK_F6) or
            (p.vkCode = VK_F7) or (p.vkCode = VK_F8) or (p.vkCode = VK_F9) or
            (p.vkCode = VK_F10) or (p.vkCode = VK_F11) or (p.vkCode = VK_F12)) and
            (((GetKeyState(VK_MENU) and $8000) <> 0) or ((GetKeyState(VK_CONTROL) and $8000) <> 0)
             or ((GetKeyState(VK_SHIFT)and$8000) <> 0)) then
              boolKey := True; }

      //-------ϵͳ�ȼ�---------------------------------------------
      //WIN(Left or Right)+APPS
          if aBWA then
          begin
            if (p.vkCode = VK_LWIN) or (p.vkCode = VK_RWIN) or (p.vkCode = VK_APPS) then
              boolKey := True;
          end;
      //CTRL+ESC
          if aBCE then
          begin
            if (p.vkCode = VK_ESCAPE) and ((GetKeyState(VK_CONTROL) and $8000) <> 0) then
              boolKey := True;
          end;
      //ALT+TAB
          if aBAT then
          begin
            if (p.vkCode = VK_TAB) and ((GetAsyncKeyState(VK_MENU) and $8000) <> 0) then
              boolKey := True;
          end;
      //ALT+ESC
          if aBAE then
          begin
            if (p.vkCode = VK_ESCAPE) and ((p.flags and LLKHF_ALTDOWN) <> 0) then
              boolKey := True;
          end;
      //CTRL+ENTER
          if aBCR then
          begin
            if (p.vkCode = VK_RETURN) and ((GetKeyState(VK_CONTROL) and $8000) <> 0) then
              boolKey := True;
          end;
      //CTRL+ALT+ENTR
          if aBCAE then
          begin
            if (p.vkCode = VK_RETURN) and ((p.flags and LLKHF_ALTDOWN) <> 0)
              and ((GetKeyState(VK_CONTROL) and $8000) <> 0) then
              boolKey := True;
          end;
      //POWER
          if aBP then
          begin
            if (p.vkCode = VK_POWER) then
              boolKey := True;
          end;
      //SLEEP
          if aaBS then
          begin
            if (p.vkCode = VK_SLEEP) then
              boolKey := True;
          end;
      //Custom
          if aBCK then
          begin
            if (p.vkCode = aCKC) then
              boolKey := True;
          end;

      //���������Ҫ���յļ�������ڴ˴�
        end;
    end;
  end;

  //������Щ��ϼ���������Ϣ���Լ��������뷵�� 1
  if boolKey and (p <> nil) then
  begin
    FKeyBlocker.DoBlock(p.vkCode);
    Result := 1;
    Exit;
  end;

  //�����İ��������ɱ���̴߳������ˣ�
  Result := CallNextHookEx(0, nCode, w_Param, l_Param);
end;

destructor TCnKeyBlocker.Destroy;
begin
  Enabled := False;
  FKeyBlocker := nil;
  inherited Destroy;
end;

procedure TCnKeyBlocker.DoBlock(VirtualKey: Cardinal);
begin
  if Assigned(FOnBlockKey) then
    FOnBlockKey(VirtualKey);
end;

procedure TCnKeyBlocker.SetBAE(const Value: Boolean);
begin
  FBAE := Value;
  aBAE := FBAE;
end;

procedure TCnKeyBlocker.SetBAT(const Value: Boolean);
begin
  FBAT := Value;
  aBAT := FBAT;
end;

procedure TCnKeyBlocker.SetBCAD(const Value: Boolean);
begin
  FBCAD := Value;
  aBCAD := FBCAD;
end;

procedure TCnKeyBlocker.SetBCAE(const Value: Boolean);
begin
  FBCAE := Value;
  aBCAE := FBCAE;
end;

procedure TCnKeyBlocker.SetBCE(const Value: Boolean);
begin
  FBCE := Value;
  aBCE := FBCE;
end;

procedure TCnKeyBlocker.SetBCK(const Value: Boolean);
begin
  FBCK := Value;
  aBCK := FBCK;
end;

procedure TCnKeyBlocker.SetBCR(const Value: Boolean);
begin
  FBCR := Value;
  aBCR := FBCR;
end;

procedure TCnKeyBlocker.SetBP(const Value: Boolean);
begin
  FBP := Value;
  aBP := FBP;
end;

procedure TCnKeyBlocker.SetBS(const Value: Boolean);
begin
  FBS := Value;
  aaBS := FBS;
end;

procedure TCnKeyBlocker.SetBWA(const Value: Boolean);
begin
  FBWA := Value;
  aBWA := FBWA;
end;

procedure TCnKeyBlocker.SetEnabled(const Value: Boolean);
begin
  FEnabled := Value;
  UpdateKeyBlock;
end;

procedure TCnKeyBlocker.UpdateKeyBlock;
begin
  if csDesigning in ComponentState then
    Exit;

  case FEnabled of
    True:
      begin
        if hhkNTKeyboard <> 0 then
          Exit;

        hhkNTKeyboard := SetWindowsHookEx(WH_KEYBOARD_LL, LowLevelKeyboardFunc, HInstance, 0);
        if FBCAD then
        begin
          EnableCTRLALTDEL(False);
          SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_IDLIST, nil, nil);
        end;
      end;
    False:
      begin
        if hhkNTKeyboard = 0 then
          Exit;
        UnhookWindowsHookEx(hhkNTKeyboard); // ж�ع���
        EnableCTRLALTDEL(True);
        SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_IDLIST, nil, nil);
        hhkNTKeyboard := 0;
      end;
  end;
end;

end.
