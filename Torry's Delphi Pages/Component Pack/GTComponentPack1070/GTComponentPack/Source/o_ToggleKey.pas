{*******************************************************}
{                                                       }
{       GT Delphi Components                            }
{       TgTgtToggleKey                                  }
{                                                       }
{       Copyright (c) GT Delphi Components              }
{       http://www.gtdelphicomponents.gr                }
{                                                       }
{                                                       }
{                                                       }
{*******************************************************}
unit o_ToggleKey;

interface

uses
  SysUtils, Classes,Windows;

type
{------------------------------------------------------------------------------}
  TKeyType =(ktCapsLock,ktNumLock,ktScrollLock);
{------------------------------------------------------------------------------}
  TgtToggleKey = class(TComponent)
  private
    { Private declarations }
    FCapsLockOn    : Boolean;
    FNumLockOn     : Boolean;
    FScrollLockOn  : Boolean;
    FKeyBoardState : TKeyBoardState;
    procedure SetCapsState(const Value: Boolean);
    procedure SetNumState(const Value: Boolean);
    procedure SetScrollState(const Value: Boolean);
  protected
    { Protected declarations }
    function  GetKeyState(KeyType:TKeyType):Boolean;
    procedure SetKeyState(KeyType:TKeyType;Enabled:Boolean);
  public
    { Public declarations }
    constructor Create(AOwner:TComponent);override;
  published
    { Published declarations }
    property CapsLockOn   : Boolean read FCapsLockOn   write SetCapsState;
    property NumLockOn    : Boolean read FNumLockOn    write SetNumState;
    property ScrollLockOn : Boolean read FScrollLockOn write SetScrollState;
  end;
{------------------------------------------------------------------------------}


implementation

{ TgtToggleKey }
{------------------------------------------------------------------------------}
constructor TgtToggleKey.Create(AOwner: TComponent);
begin
  inherited;
  FCapsLockOn   := GetKeyState(ktCapsLock);
  FNumLockOn    := GetKeyState(ktNumLock);
  FScrollLockOn := GetKeyState(ktScrollLock);
end;
{------------------------------------------------------------------------------}
function TgtToggleKey.GetKeyState(KeyType: TKeyType): Boolean;
var
  KeyCode : Byte;
begin
  KeyCode := 0;
  case KeyType of
    ktCapsLock:KeyCode   := VK_CAPITAL;
    ktNumLock:KeyCode    := VK_NUMLOCK;
    ktScrollLock:KeyCode := VK_SCROLL;
  end;
  GetKeyBoardState(FKeyBoardState);
  Result := Boolean(FKeyBoardState[KeyCode]);
end;
{------------------------------------------------------------------------------}
procedure TgtToggleKey.SetKeyState(KeyType: TKeyType;Enabled:Boolean);
var
  KeyCode : Byte;
begin
  KeyCode := 0;
  case KeyType of
    ktCapsLock:  KeyCode := VK_CAPITAL;
    ktNumLock:   KeyCode := VK_NUMLOCK;
    ktScrollLock:KeyCode := VK_SCROLL;
  end;
  GetKeyBoardState(FKeyBoardState);
  if Boolean(FKeyBoardState[KeyCode])<>Enabled then
    begin
      Keybd_Event(KeyCode,MapVirtualKey(KeyCode, 0),KEYEVENTF_EXTENDEDKEY,0);
      Keybd_Event(KeyCode,MapVirtualKey(KeyCode, 0),KEYEVENTF_EXTENDEDKEY or KEYEVENTF_KEYUP,0);
    end;
end;
{------------------------------------------------------------------------------}
procedure TgtToggleKey.SetCapsState(const Value: Boolean);
begin
  FCapsLockOn := Value;
  SetKeyState(ktCapsLock,FCapsLockOn);
end;
{------------------------------------------------------------------------------}
procedure TgtToggleKey.SetNumState(const Value: Boolean);
begin
  FNumLockOn := Value;
  SetKeyState(ktNumLock,FNumLockOn);
end;
{------------------------------------------------------------------------------}
procedure TgtToggleKey.SetScrollState(const Value: Boolean);
begin
  FScrollLockOn := Value;
  SetKeyState(ktScrollLock,FScrollLockOn);
end;
{------------------------------------------------------------------------------}
end.
