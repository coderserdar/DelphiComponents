
{*******************************************************}
{                                                       }
{       Borland Delphi Visual Component Library         }
{                                                       }
{  Copyright (c) 1995-2002 Borland Software Corporation }
{                                                       }
{*******************************************************}

unit XPStyleActnCtrlsEx;

interface

uses ActnMan, ActnMenus, XPStyleActnCtrls;

type

{ TXPStyleActnBarsEx - Extends TXPStyleActionBars by adding support for
    XP style popup menus }

  TXPStyleActnBarsEx = class(TXPStyleActionBars)
  public
    function GetPopupClass(ActionBar: TCustomActionBar): TCustomPopupClass; override;
  end;

implementation

uses XPActnCtrlsEx;

{ TXPStyleActnBarsEx }

function TXPStyleActnBarsEx.GetPopupClass(
  ActionBar: TCustomActionBar): TCustomPopupClass;
begin
  // If ActionBar = nil then this must be a popup menu so return the
  // XP specific style of popup menu
  if ActionBar = nil then
    Result := TXPStylePopupMenuEx
  else
    Result := inherited GetPopupClass(ActionBar);
end;

initialization
  // Unregister the old XPStyle and register this new class in its place
  UnregisterActnBarStyle(XPStyle);
  XPStyle.Free;
  XPStyle := TXPStyleActnBarsEx.Create;
  DefaultActnBarStyle := XPStyle.GetStyleName;
  RegisterActnBarStyle(XPStyle);
  // XPStyle will be freed in the finalization section of XPStyleActnCtrls
end.

