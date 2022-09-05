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

unit CnRestoreSystemMenu;
{* |<PRE>
================================================================================
* ������ƣ����������ԡ�����༭����
* ��Ԫ���ƣ������ָ��༭���ؼ��Ҽ��˵������
* ��Ԫ���ߣ�Chinbo(Shenloqi@hotmail.com)
* ��    ע��
* ����ƽ̨��PWin2000Pro + Delphi 7.0
* ���ݲ��ԣ�PWin9X/2000/XP + Delphi 5/6
* �� �� �����õ�Ԫ�ʹ����е��ַ����Ѿ����ػ�����ʽ
* �޸ļ�¼��
*           2005.08.05 by shenloqi
*               ��������TCombobox�������©
*           2005.08.03 V1.0
*               ������Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, StdCtrls,
  AppEvnts, CnConsts, CnClasses, CnCompConsts;

type
  TCnContextMenuOption = (ccoVCLMenu, ccoEat, ccoSystemMenu);

{$IFNDEF COMPILER6_UP}
  TCustomCombo = TCustomComboBox;
{$ENDIF}

  TCnAdditionalContextMenuEvent = procedure (Ctrl: TWinControl;
    var cco: TCnContextMenuOption) of object;
  TCnAdditionalWindowContextMenuEvent = procedure (hwnd: THandle;
    var cco: TCnContextMenuOption) of object;
  TCnCustomEditContextMenuEvent = procedure (Edit: TCustomEdit;
    var cco: TCnContextMenuOption) of object;
  TCnCustomComboContextMenuEvent = procedure (Combo: TCustomCombo;
    var cco: TCnContextMenuOption) of object;
  TCnNonDelphiControlContextMenuEvent = procedure (SysControl: THandle;
    var cco: TCnContextMenuOption) of object;

  TCnRestoreSystemMenu = class(TCnComponent)
  private
    FAppEvent: TApplicationEvents;
    FAdditionalClasses: TStringList;
    FAdditionalWindowClasses: TStringList;

    FOnAdditionalContextMenu: TCnAdditionalContextMenuEvent;
    FDefAdditionalCtxMenuOpt: TCnContextMenuOption;
    FOnAdditionalWindowContextMenu: TCnAdditionalWindowContextMenuEvent;
    FDefAdditionalWndCtxMenuOpt: TCnContextMenuOption;
    FOnCustomComboContextMenu: TCnCustomComboContextMenuEvent;
    FDefCstmComboCtxMenuOpt: TCnContextMenuOption;
    FOnCustomEditContextMenu: TCnCustomEditContextMenuEvent;
    FDefCstmEdtCtxMenuOpt: TCnContextMenuOption;
    FOnNonDelphiControlContextMenu: TCnNonDelphiControlContextMenuEvent;
    FDefNonDelphiCtrlCtxMenuOpt: TCnContextMenuOption;

    function GetWindowClass(hwnd: THandle): string;
    function IsAdditionalClass(wc: TWinControl): Boolean;
    function IsAdditionalWindowClass(hwnd: THandle): Boolean;

    procedure DoMessage(var Msg: TMsg; var Handled: Boolean);
  protected
    procedure DoAdditionalContextMenu(Ctrl: TWinControl;
      var cco: TCnContextMenuOption);
    procedure DoAdditionalWindowContextMenu(hwnd: THandle;
      var cco: TCnContextMenuOption);
    procedure DoCustomComboContextMenu(Combo: TCustomCombo;
      var cco: TCnContextMenuOption);
    procedure DoCustomEditContextMenu(Edit: TCustomEdit;
      var cco: TCnContextMenuOption);
    procedure DoNonDelphiControlContextMenu(SysControl: THandle;
      var cco: TCnContextMenuOption);

    procedure GetComponentInfo(var AName, Author, Email, Comment: string); override;      
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property AdditionalClasses: TStringList read FAdditionalClasses write FAdditionalClasses;
    property AdditionalWindowClasses: TStringList read FAdditionalWindowClasses write FAdditionalWindowClasses;

    property DefAdditionalCtxMenuOpt: TCnContextMenuOption
      read FDefAdditionalCtxMenuOpt write FDefAdditionalCtxMenuOpt default ccoSystemMenu;
    property DefAdditionalWndCtxMenuOpt: TCnContextMenuOption
      read FDefAdditionalWndCtxMenuOpt write FDefAdditionalWndCtxMenuOpt default ccoSystemMenu;
    property DefCstmComboCtxMenuOpt: TCnContextMenuOption
      read FDefCstmComboCtxMenuOpt write FDefCstmComboCtxMenuOpt default ccoSystemMenu;
    property DefCstmEdtCtxMenuOpt: TCnContextMenuOption
      read FDefCstmEdtCtxMenuOpt write FDefCstmEdtCtxMenuOpt default ccoSystemMenu;
    property DefNonDelphiCtrlCtxMenuOpt: TCnContextMenuOption
      read FDefNonDelphiCtrlCtxMenuOpt write FDefNonDelphiCtrlCtxMenuOpt default ccoVCLMenu;

    property OnAdditionalContextMenu: TCnAdditionalContextMenuEvent
      read FOnAdditionalContextMenu write FOnAdditionalContextMenu;
    property OnAdditionalWindowContextMenu: TCnAdditionalWindowContextMenuEvent
      read FOnAdditionalWindowContextMenu write FOnAdditionalWindowContextMenu;
    property OnCustomComboContextMenu: TCnCustomComboContextMenuEvent
      read FOnCustomComboContextMenu write FOnCustomComboContextMenu;
    property OnCustomEditContextMenu: TCnCustomEditContextMenuEvent
      read FOnCustomEditContextMenu write FOnCustomEditContextMenu;
    property OnNonDelphiControlContextMenu: TCnNonDelphiControlContextMenuEvent
      read FOnNonDelphiControlContextMenu write FOnNonDelphiControlContextMenu;
  end;

implementation

{ TCnRestoreSystemMenu }

constructor TCnRestoreSystemMenu.Create(AOwner: TComponent);
begin
  inherited;

  FAdditionalClasses := TStringList.Create;
  FAdditionalWindowClasses := TStringList.Create;

  FDefAdditionalCtxMenuOpt := ccoSystemMenu;
  FDefAdditionalWndCtxMenuOpt := ccoSystemMenu;
  FDefCstmComboCtxMenuOpt := ccoSystemMenu;
  FDefCstmEdtCtxMenuOpt := ccoSystemMenu;
  FDefNonDelphiCtrlCtxMenuOpt := ccoVCLMenu;

  FAppEvent := TApplicationEvents.Create(Self);
  FAppEvent.OnMessage := DoMessage;
end;

destructor TCnRestoreSystemMenu.Destroy;
begin
  FAppEvent.Free;
  FAdditionalWindowClasses.Free;
  FAdditionalClasses.Free;

  inherited;
end;

procedure TCnRestoreSystemMenu.DoAdditionalContextMenu(Ctrl: TWinControl;
  var cco: TCnContextMenuOption);
begin
  if Assigned(FOnAdditionalContextMenu) then FOnAdditionalContextMenu(Ctrl, cco);
end;

procedure TCnRestoreSystemMenu.DoAdditionalWindowContextMenu(hwnd: THandle;
  var cco: TCnContextMenuOption);
begin
  if Assigned(FOnAdditionalWindowContextMenu) then FOnAdditionalWindowContextMenu(hwnd, cco);
end;

procedure TCnRestoreSystemMenu.DoCustomComboContextMenu(
  Combo: TCustomCombo; var cco: TCnContextMenuOption);
begin
  if Assigned(FOnCustomComboContextMenu) then FOnCustomComboContextMenu(Combo, cco);
end;

procedure TCnRestoreSystemMenu.DoCustomEditContextMenu(Edit: TCustomEdit;
  var cco: TCnContextMenuOption);
begin
  if Assigned(FOnCustomEditContextMenu) then FOnCustomEditContextMenu(Edit, cco);
end;

procedure TCnRestoreSystemMenu.DoNonDelphiControlContextMenu(
  SysControl: THandle; var cco: TCnContextMenuOption);
begin
  if Assigned(FOnNonDelphiControlContextMenu) then FOnNonDelphiControlContextMenu(SysControl, cco);
end;

function TCnRestoreSystemMenu.GetWindowClass(hwnd: THandle): string;
var
  s: array[0..MAX_PATH - 1] of Char;
begin
  GetClassName(hwnd, s, Length(s));
  Result := UpperCase(s);
end;

function TCnRestoreSystemMenu.IsAdditionalClass(wc: TWinControl): Boolean;
begin
  Result := (wc <> nil) and (FAdditionalClasses.IndexOf(wc.ClassName) >= 0);
end;

function TCnRestoreSystemMenu.IsAdditionalWindowClass(
  hwnd: THandle): Boolean;
begin
  Result := FAdditionalWindowClasses.IndexOf(GetWindowClass(hwnd)) >= 0;
end;

type
  THackCustomEdit = class(TCustomEdit);

procedure TCnRestoreSystemMenu.DoMessage(var Msg: TMsg; var Handled: Boolean);
var
  wc: TWinControl;
  ce: TCustomEdit;
  cmb: TCustomCombo;
  cco: TCnContextMenuOption;
begin
  if Msg.message = WM_RBUTTONUP then
  begin
    wc := FindControl(Msg.hwnd);
    if wc = nil then
      wc := FindControl(GetParent(Msg.hwnd));
    if (wc <> nil) and (csDesigning in wc.ComponentState) then
      Exit;

    if wc is TCustomCombo then
    begin
      cmb := TCustomCombo(wc);
      cco := DefCstmComboCtxMenuOpt;
      DoCustomComboContextMenu(cmb, cco);
      case cco of
        ccoVCLMenu: ;
        ccoEat: Handled := True;
        ccoSystemMenu:
          begin
            with Msg do
            begin
              CallWindowProc(Pointer(GetWindowLong(Msg.hwnd, GWL_WNDPROC)), Msg.hwnd, WM_CONTEXTMENU, WParam, MakeLParam(pt.X, pt.Y));
            end;
            Handled := True;
          end;
      end;
    end
    else if wc is TCustomEdit then
    begin
      ce := TCustomEdit(wc);
      cco := DefCstmEdtCtxMenuOpt;
      DoCustomEditContextMenu(ce, cco);
      case cco of
        ccoVCLMenu: ;
        ccoEat: Handled := True;
        ccoSystemMenu:
          begin
            with THackCustomEdit(ce) do
            begin
              if PopupMenu = nil then
              begin
                with Msg do
                begin
                  CallWindowProc(DefWndProc, Handle, WM_CONTEXTMENU, WParam, MakeLParam(pt.X, pt.Y));
                end;
                Handled := True;
              end;
            end;
          end;
      end;
    end
    else if IsAdditionalClass(wc) then
    begin
      cco := DefAdditionalCtxMenuOpt;
      DoAdditionalContextMenu(wc, cco);
      case cco of
        ccoVCLMenu: ;
        ccoEat: Handled := True;
        ccoSystemMenu:
          begin
            with Msg do
            begin
              CallWindowProc(Pointer(GetWindowLong(Msg.hwnd, GWL_WNDPROC)), Msg.hwnd, WM_CONTEXTMENU, WParam, MakeLParam(pt.X, pt.Y));
            end;
            Handled := True;
          end;
      end;
    end
    else if IsAdditionalWindowClass(Msg.hwnd) then
    begin
      cco := DefAdditionalWndCtxMenuOpt;
      DoAdditionalWindowContextMenu(Msg.hwnd, cco);
      case cco of
        ccoVCLMenu: ;
        ccoEat: Handled := True;
        ccoSystemMenu:
          begin
            with Msg do
            begin
              CallWindowProc(Pointer(GetWindowLong(Msg.hwnd, GWL_WNDPROC)), Msg.hwnd, WM_CONTEXTMENU, WParam, MakeLParam(pt.X, pt.Y));
            end;
            Handled := True;
          end;
      end;
    end
    else if wc = nil then
    begin
      cco := DefNonDelphiCtrlCtxMenuOpt;
      DoNonDelphiControlContextMenu(Msg.hwnd, cco);
      case cco of
        ccoVCLMenu: ;
        ccoEat: Handled := True;
        ccoSystemMenu:
          begin
            with Msg do
            begin
              CallWindowProc(Pointer(GetWindowLong(Msg.hwnd, GWL_WNDPROC)), Msg.hwnd, WM_CONTEXTMENU, WParam, MakeLParam(pt.X, pt.Y));
            end;
            Handled := True;
          end;
      end;
    end
    else
    begin
      Exit;
    end;
  end;
end;

procedure TCnRestoreSystemMenu.GetComponentInfo(var AName, Author, Email,
  Comment: string);
begin
  AName := SCnRestoreSystemMenuName;
  Author := SCnPack_Shenloqi;
  Email := SCnPack_ShenloqiEmail;
  Comment := SCnRestoreSystemMenuComment;
end;

end.
