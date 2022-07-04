unit TBXSwitcher;

// TBX Package
// Copyright 2001-2004 Alex A. Denisov. All Rights Reserved
// See TBX.chm for license and installation instructions
//
// $Id: TBXSwitcher.pas 7 2004-02-21 06:07:53Z  $

interface

{$I TB2Ver.inc}
{$I TBX.inc}

uses
  TBX, TBXThemes, Classes;

type
  TFlatMenuStyle = (fmsAuto, fmsEnable, fmsDisable);

  TTBXSwitcher = class(TComponent)
  private
    FOnThemeChange: TNotifyEvent;
    procedure SetTheme(const Value: string);
    function GetTheme: string;
    function GetThemeCount: Integer;
    function GetThemes(Index: Integer): string;
    function GetEnableXPStyles: Boolean;
    function GetFlatMenuStyle: TFlatMenuStyle;
    procedure SetEnableXPStyles(Value: Boolean);
    procedure SetFlatMenuStyle(Value: TFlatMenuStyle);
    procedure TBMThemeChange(var Message); message TBM_THEMECHANGE;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property ThemeCount: Integer read GetThemeCount;
    property Themes[Index: Integer]: string read GetThemes;
  published
    property Theme: string read GetTheme write SetTheme;
    property EnableXPStyles: Boolean read GetEnableXPStyles write SetEnableXPStyles default True;
    property FlatMenuStyle: TFlatMenuStyle read GetFlatMenuStyle write SetFlatMenuStyle default fmsAuto;
    property OnThemeChange: TNotifyEvent read FOnThemeChange write FOnThemeChange;
  end;

implementation

var
  Counter: Integer = 0;

{ TTBXSwitcher }

constructor TTBXSwitcher.Create(AOwner: TComponent);
begin
  inherited;
  AddThemeNotification(Self);
  Inc(Counter);
end;

destructor TTBXSwitcher.Destroy;
begin
  Dec(Counter);
  RemoveThemeNotification(Self);
  if (csDesigning in ComponentState) and (Counter = 0) then TBXSetTheme('Default');
  inherited;
end;

function TTBXSwitcher.GetEnableXPStyles: Boolean;
begin
  Result := GetTBXSysParam(TSP_XPVISUALSTYLE) = XPVS_AUTOMATIC;
end;

function TTBXSwitcher.GetFlatMenuStyle: TFlatMenuStyle;
begin
  case GetTBXSysParam(TSP_FLATMENUSTYLE) of
    FMS_ENABLED: Result := fmsEnable;
    FMS_DISABLED: Result := fmsDisable;
  else
    Result := fmsAuto;
  end;
end;

function TTBXSwitcher.GetTheme: string;
begin
  Result := TBXCurrentTheme;
end;

function TTBXSwitcher.GetThemeCount: Integer;
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  GetAvailableTBXThemes(SL);
  Result := SL.Count;
  SL.Free;
end;

function TTBXSwitcher.GetThemes(Index: Integer): string;
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  GetAvailableTBXThemes(SL);
  try
    Result := SL[Index];
  finally
    SL.Free;
  end;
end;

procedure TTBXSwitcher.SetEnableXPStyles(Value: Boolean);
const
  XPVStyle: array [Boolean] of Integer = (XPVS_DISABLED, XPVS_AUTOMATIC);
begin
  SetTBXSysParam(TSP_XPVISUALSTYLE, XPVStyle[Value]);
end;

procedure TTBXSwitcher.SetFlatMenuStyle(Value: TFlatMenuStyle);
const
  FMStyle: array [TFlatMenuStyle] of Integer = (FMS_AUTOMATIC, FMS_ENABLED, FMS_DISABLED);
begin
  SetTBXSysParam(TSP_FLATMENUSTYLE, FMStyle[Value]);
end;

procedure TTBXSwitcher.SetTheme(const Value: string);
begin
  TBXSetTheme(Value);
end;

procedure TTBXSwitcher.TBMThemeChange(var Message);
begin
  if Assigned(FOnThemeChange) then FOnThemeChange(Self);
end;

end.
