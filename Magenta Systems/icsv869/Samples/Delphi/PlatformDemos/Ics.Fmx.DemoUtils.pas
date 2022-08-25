unit Ics.Fmx.DemoUtils;

interface

{$I Include\OverbyteIcsDefs.inc}

uses
    FMX.Platform;

function ScreenHeight : Integer;
function ScreenWidth : Integer;

implementation

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function ScreenHeight : Integer;
{$IFDEF COMPILER17_UP}
var
    ScreenService : IFMXScreenService;
begin
    Result := 480;
    if TPlatformServices.Current.SupportsPlatformService(IFMXScreenService,
        IInterface(ScreenService)) then
    Result := Trunc(ScreenService.GetScreenSize.Y);
{$ELSE}
begin
    Result := Trunc(Platform.GetScreenSize.Y);
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function ScreenWidth : Integer;
{$IFDEF COMPILER17_UP}
var
    ScreenService : IFMXScreenService;
begin
    Result := 640;
    if TPlatformServices.Current.SupportsPlatformService(IFMXScreenService,
        IInterface(ScreenService)) then
    Result := Trunc(ScreenService.GetScreenSize.X);
{$ELSE}
begin
    Result := Trunc(Platform.GetScreenSize.X);
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.
