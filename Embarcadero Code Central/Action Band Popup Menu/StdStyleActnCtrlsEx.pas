
{*******************************************************}
{                                                       }
{       Borland Delphi Visual Component Library         }
{                                                       }
{  Copyright (c) 1995-2002 Borland Software Corporation }
{                                                       }
{*******************************************************}

unit StdStyleActnCtrlsEx;

interface

uses ActnMan, ActnMenus, StdStyleActnCtrls;

type

{ TStandardStyleActnBarsEx - Extends TStandardStyleActionBars by adding support for
    Standard style popup menus }

  TStandardStyleActnBarsEx = class(TStandardStyleActionBars)
  public
    function GetPopupClass(ActionBar: TCustomActionBar): TCustomPopupClass; override;
  end;

implementation

uses StdActnCtrlsEx;

{ TStandardStyleActnBarsEx }

function TStandardStyleActnBarsEx.GetPopupClass(
  ActionBar: TCustomActionBar): TCustomPopupClass;
begin
  if ActionBar = nil then
    Result := TStandardPopupMenuEx
  else
    Result := inherited GetPopupClass(ActionBar);
end;

initialization
  // Unregister the old Standard and register this new class in its place
  UnregisterActnBarStyle(StandardStyle);
  StandardStyle.Free;
  StandardStyle := TStandardStyleActnBarsEx.Create;
  RegisterActnBarStyle(StandardStyle);
  // StandardStyle will be freed in the finalization section of StdStyleActnCtrls  
end.
 