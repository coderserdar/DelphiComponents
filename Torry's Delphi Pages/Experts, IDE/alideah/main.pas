{
This software is provided 'as-is', without any express or implied warranty.
In no event shall the author be held liable for any damages arising from the use
of this software.

License: Freeware, free to use and distribute as long as the original source
stays intact. free to modify as long as all modifications are sent back to me.

IDEAutohide
Copyright  2002 ahmoy law
e-mail:  ahmoy_law@hotmail.com
         ahmoy_law@yahoo.com
Version: 1.00a

Any suggestions, modifications, bugs or anything! kindly please send an email to me :)

p/s: hidup oghe kelate!!!


Bug List
---------
1. Caption button on the Delphi Main Editor form is removed when user selected a different tab (dunno what windows message is triggered)
2. Caption button state is not updated if mouse moved too fast (WM_NCHITTEST problem)
3. Delphi could not correctly position the form during startup if " Initialize engine on Delphi loading" option is enabled
}


Unit main;

interface

uses
    ExptIntf, Windows, Messages, Classes, Forms, Controls, Menus, Graphics,
    Unit_WndProp, Unit_AutoHide, Unit_Misc;

//note: Methods execution sequence at startup (GetName GetIDString GetStyle GetState GetMenuText)

type
  TCriticalFunction = function ( aParam, aParam2: DWORD ): HResult;

  {TWizardAutoHide}
  TWizardAutoHide = class (TIExpert)
  public
    function GetStyle: TExpertStyle; override;
    function GetName: string; override;
    function GetAuthor: string; override;
    function GetComment: string; override;
    function GetPage: string; override;
    function GetGlyph: HICON; override;
    function GetState: TExpertState; override;
    function GetIDString: string; override;
    function GetMenuText: string; override;
    procedure Execute; override;
  end;


procedure ActivateWindow ( aWnd: THandle );
procedure RemoveWindow ( aWnd: THandle );
procedure MenuClick ( aWnd: THandle; aCode: integer );
procedure LockExpandWindow ( aWnd: THandle; aLockExpand: boolean );
function  GetButtonBitmap ( aButtonStatus: TButtonStatus ): TBitmap;
procedure Register;

var
    FMainTriggered: Boolean;
    FMainCriticalSection: TRTLCriticalSection;

    FMainForm: TForm;
    FMainFormWndProc: DWORD;
    FApplication: TApplication;
    FWndProp: TWndProperty;
    FAutoHide: TAutoHideMan;

    FModalRestore: boolean;
    FCaptionButton: boolean;
    FExeSound: boolean;
    //FXPStyle: boolean;

// -----------------------------------------------------------------------------

implementation

uses
    Math, Dialogs, SysUtils, Registry, Unit_FormMainClass, Unit_Constants;


type
    PHookParam = ^THookParam;
    THookParam = record
        nCode: integer;
        wParam: WPARAM;
        lParam: LPARAM
    end;

var
   FMainHook: HHOOK;

   FMainTimer: LongWord;
   FMainTimerInterval: LongWord;
   FDelphiLoadInit: boolean;
   FDelphiCloseKill: boolean;

   FButtonBmpNormal: TBitmap;
   FButtonBmpHighlight: TBitmap;
   FButtonBmpDown: TBitmap;

// -----------------------------------------------------------------------------

procedure DelphiLoadInit; forward;
procedure WindowsInit; forward;
procedure EngineStart; forward;
procedure EngineKill; forward;
function  EngineExecute ( aFunction: TCriticalFunction; aParam, aParam2: DWORD ): HResult; forward;
procedure EngineConfigure; forward;
procedure EngineRegReadDefault; forward;

function  EnumWndProc ( hWnd: THandle; alParam: LPARAM ) : DWORD; stdcall; forward;
Procedure TimerProc ( hWnd:THandle; uMsg:UINT; idEvent:UINT; dwTime:DWORD ); stdcall; forward;
function  HookCBTProc ( nCode: integer; awParam: WPARAM; alParam: LPARAM ) : HResult; stdcall; forward;
function  DelphiWndProc ( hWnd: THandle; uMsg: UINT; aWParam: WPARAM; aLParam: LPARAM ): LRESULT; stdcall; forward;


// -----------------------------------------------------------------------------


procedure DelphiLoadInit;
var
    aRegistry: TRegistry;
begin
aRegistry := TRegistry.Create;
try
    aRegistry.RootKey := HKEY_CURRENT_USER;

    try
        if ( aRegistry.OpenKey ( ROOT_KEYINCLUDE, FALSE ) ) then begin
            FDelphiLoadInit := aRegistry.ReadBool ( REG_DELPHILOADINIT );
            aRegistry.CloseKey;
            end;
    except
        // do nothing!
        end;

finally
    aRegistry.Free;
    end;

if ( FDelphiLoadInit ) then begin
    EngineRegReadDefault;
    EngineStart;
    end;
end;



procedure WindowsInit;
var
    aTimer: THandle;
begin
if ( FMainTimer <> 0 ) then begin
    aTimer := FMainTimer;
    FMainTimer := 0;
    KillTimer ( 0, aTimer );
    end;

//Trace ( '------------------ Windows Init ---------------------' );

FAutoHide.Clear;
FWndProp.Clear;
FWndProp.ClearNewWnd;
FWndProp.UpdatePassport;
FWndProp.DestroyMenu;

// note: EnumThreadWindows must comes first before the SetTimer!
EnumThreadWindows ( GetCurrentThreadId, @EnumWndProc, GetForegroundWindow );
FMainTimer := SetTimer ( 0, TIMER_ID, FMainTimerInterval, @TimerProc );
end;


procedure EngineStart;
begin
if ( FMainTriggered ) then exit;

//Trace ( '------------------ Engine Start ---------------------' );

InitializeCriticalSection ( FMainCriticalSection );

EnterCriticalSection ( FMainCriticalSection );
try
    FMainTriggered := TRUE;

    FButtonBmpNormal := TBitmap.Create;
    FButtonBmpNormal.LoadFromResourceName ( HInstance, 'CAPTION_BMP_NORMAL' );
    FButtonBmpHighlight := TBitmap.Create;
    FButtonBmpHighlight.LoadFromResourceName ( HInstance, 'CAPTION_BMP_HIGHLIGHT' );
    FButtonBmpDown := TBitmap.Create;
    FButtonBmpDown.LoadFromResourceName ( HInstance, 'CAPTION_BMP_DOWN' );


    FApplication := Application;
    FMainForm    := Application.MainForm;

    FMainFormWndProc := GetWindowLong ( FMainForm.Handle, GWL_WNDPROC );
    SetWindowLong ( FMainForm.Handle, GWL_WNDPROC, integer(@DelphiWndProc) );

    FAutoHide := TAutoHideMan.Create;
    FWndProp := TWndProperty.Create;

    WindowsInit;
    FMainHook := SetWindowsHookEx ( WH_CBT, @HookCBTProc, HInstance, GetCurrentThreadId );

finally
    LeaveCriticalSection ( FMainCriticalSection );
    end;
end;


procedure EngineKill;
var
    aWndProp: TWndProperty;
begin
if ( not FMainTriggered ) then exit;

//Trace ( '------------------ Engine Kill ---------------------' );

EnterCriticalSection ( FMainCriticalSection );
try
    FMainTriggered := FALSE;

    FExeSound := FALSE;
    //FCaptionButton := FALSE;
    FModalRestore := FALSE;
    FDelphiCloseKill := FALSE;

    UnhookWindowsHookEx ( FMainHook );
    KillTimer ( 0, FMainTimer );
    FMainTimer := 0;

    FWndProp.ClearPassport;
    FAutoHide.Free;

    aWndProp := FWndProp;
    FWndProp := NIL;
    aWndProp.Free;

    FButtonBmpNormal.Free;
    FButtonBmpHighlight.Free;
    FButtonBmpDown.Free;

    SetWindowLong ( FMainForm.Handle, GWL_WNDPROC, FMainFormWndProc );

finally
    LeaveCriticalSection ( FMainCriticalSection );
    end;

DeleteCriticalSection ( FMainCriticalSection );
end;



function EngineExecute ( aFunction: TCriticalFunction; aParam, aParam2: DWORD ): HResult;
begin
result := 0;
if ( not FMainTriggered ) then exit;

try
    EnterCriticalSection ( FMainCriticalSection );
    try
        result := aFunction ( aParam, aParam2 );
    finally
        LeaveCriticalSection ( FMainCriticalSection );
        end;
except
    // do nothing
    end;
end;


procedure EngineRegReadDefault;
var
    aRegistry: TRegistry;
begin
aRegistry := TRegistry.Create;
try
    aRegistry.RootKey := HKEY_CURRENT_USER;

    aRegistry.OpenKey ( ROOT_KEY, FALSE );

    FMainTimerInterval := Max ( RegReadIntegerDef ( aRegistry,
                                REG_INTERVAL, REG_DEFVAL_INTERVAL ),
                                TIMER_MIN_VALUE );

    FModalRestore := RegReadBoolDef ( aRegistry,
                                REG_MODALRESTORE, REG_DEFVAL_MODALRESTORE );
    FCaptionButton := RegReadBoolDef ( aRegistry,
                                REG_CAPTIONBUTTON, REG_DEFVAL_CAPTIONBUTTON );
    FDelphiCloseKill := RegReadBoolDef ( aRegistry,
                                REG_DELPHICLOSEKILL, REG_DEFVAL_DELPHICLOSEKILL );
    //FXPStyle := RegReadBoolDef ( aRegistry,
    //                            REG_XPSTYLE, REG_DEFVAL_XPSTYLE );
    FExeSound := RegReadBoolDef ( aRegistry,
                                REG_SOUND, REG_DEFVAL_SOUND );

    aRegistry.CloseKey;

finally
    aRegistry.Free;
    end;
end;



procedure EngineConfigure;
var
    aForm: TFormAutoHide;
begin
aForm := TFormAutoHide.Create ( FApplication );
try
    if ( FMainTriggered ) then begin
        KillTimer ( 0, FMainTimer );
        aForm.AutohideLoaded;
        end;

    aForm.ShowModal;

    case ( aForm.FResult ) of
        frCancel: begin
            if ( FMainTriggered ) then
                FMainTimer := SetTimer ( 0, TIMER_ID, FMainTimerInterval, @TimerProc );
            end;

        frLoad: begin
            EngineRegReadDefault;
            EngineStart;
            end;

        frUnload: begin
            EngineKill;
            end;

        frUpdate: begin
            EngineRegReadDefault;

            if ( FMainTriggered ) then
                WindowsInit;
            end;
        end; // case

finally
    aForm.Free;
    end;
end;


// -----------------------------------------------------------------------------


function InternalActivateWindow ( aParam, aParam2: DWORD ): HResult;
var
    aItem: PWndProp;
begin
result := 0;

aItem := FWndProp.Get ( THandle( aParam ) );
if ( aItem <> NIL ) then
    FAutoHide.ActiveWindow ( aItem )
else
    FAutoHide.ActiveWindow ( NIL );
end;


function InternalRemoveWindow ( aParam, aParam2: DWORD ): HResult;
var
    aItem: PWndProp;
    aWnd: THandle;
begin
result := 0;

aWnd := THandle ( aParam );
aItem := FWndProp.Get ( aWnd );
if ( aItem <> NIL ) then begin
    FAutoHide.Remove ( aItem, FALSE );
    FWndProp.Remove ( aWnd );
    end;
end;


function InternalLockExpandWindow ( aParam, aParam2: DWORD ): HResult;
begin
result := 0;
FAutoHide.LockExpandWindow ( aParam, aParam2 <> 0 );
end;


function InternalMenuClick ( aParam, aParam2: DWORD ): HResult;
var
    aItem: PWndProp;
    aWnd: THandle;
begin
result := 0;

aWnd := THandle ( aParam );
aItem := FWndProp.Get ( aWnd );
if ( aItem <> NIL ) then begin
    case ( aParam2 ) of
        MENU_LOCKEXPAND: FAutoHide.UpdateWindowLock ( aItem, ltLockExpand );
        MENU_LOCKSHRINK: FAutoHide.UpdateWindowLock ( aItem, ltLockShrink );
        MENU_UNLOCK:     FAutoHide.UpdateWindowLock ( aItem, ltUnLock );

        MENU_TYPECAPTION:       FAutoHide.UpdateWindowShrink ( aItem, stCaption );
        MENU_TYPESHORTCAPTION:  FAutoHide.UpdateWindowShrink ( aItem, stShortCaption );
        MENU_TYPEULTRATHIN:     FAutoHide.UpdateWindowShrink ( aItem, stUltraThin );

        MENU_UTTOP:   FAutoHide.UpdateWindowUTStyle ( aItem, sdirTop );
        MENU_UTLEFT:  FAutoHide.UpdateWindowUTStyle ( aItem, sdirLeft );
        MENU_UTBOTTOM:FAutoHide.UpdateWindowUTStyle ( aItem, sdirBottom );
        MENU_UTRIGHT: FAutoHide.UpdateWindowUTStyle ( aItem, sdirRight );

        MENU_PROPERTY: EngineConfigure;
        MENU_UNLOAD: EngineKill;
        end; // case
    end;
end;


function InternalUpdateShrunkCaption ( aParam, aParam2: DWORD ): HResult;
begin
result := 0;
FAutoHide.UpdateShrunkCaption;
end;


// -----------------------------------------------------------------------------


function EnumWndProc ( hWnd: THandle; alParam: LPARAM ) : DWORD; stdcall;
var
    aItem: PWndProp;
    aClassname: string;
begin
if ( hWnd <> FApplication.Handle ) then begin
    aClassName := GetWindowClassName ( hWnd );
    if ( aClassName <> '' ) and ( FWndProp.IsIncluded ( aClassName ) ) then begin
        aItem := FWndProp.Add ( hWnd, aClassName );
        FAutoHide.AddWindow ( aItem, hWnd = THandle(alParam) );
        end; // if
    end; // if

result := 1;
end;



Procedure TimerProc ( hWnd:THandle; uMsg:UINT; idEvent:UINT; dwTime:DWORD ); stdcall;
begin
try
    EnterCriticalSection ( FMainCriticalSection );
    try
        FWndProp.Execute;
    finally
        LeaveCriticalSection ( FMainCriticalSection );
        end;
except
    end;
end;



function HookCBTProc ( nCode: integer; awParam: WPARAM; alParam: LPARAM ) : HResult; stdcall;
begin
result := CallNextHookEx ( FMainHook, nCode, awParam, alParam );

if ( nCode = HCBT_CREATEWND ) then
    FWndProp.AddNewWnd ( THandle ( awParam ) );
end;



function DelphiWndProc ( hWnd: THandle; uMsg: UINT; aWParam: WPARAM; aLParam: LPARAM ): LRESULT; stdcall;
begin
if ( uMsg = WM_SETTINGCHANGE ) then
    EngineExecute ( InternalUpdateShrunkCaption, 0, 0 )
else
// cannot checks whether user clicked OK or Cancel
if ( uMsg = WM_CLOSE ) and ( FDelphiCloseKill ) then
    EngineKill;

result := CallWindowProc ( Pointer(FMainFormWndProc), hWnd, uMsg, aWParam, aLParam );
end;


// -----------------------------------------------------------------------------


{TWizardAutoHide}
function TWizardAutoHide.GetStyle: TExpertStyle;
begin
Result := esStandard;
end;

function TWizardAutoHide.GetName: String;
begin
Result  := AH_WIZARD;
end;

function TWizardAutoHide.GetAuthor: string;
begin
Result := AH_AUTHOR;
end;

function TWizardAutoHide.GetComment: String;
begin
Result := AH_DESCRIPTIONS;
end;

function TWizardAutoHide.GetPage: string;
begin
Result := '';
end;

function TWizardAutoHide.GetGlyph: HICON;
begin
Result := 0;
end;

function TWizardAutoHide.GetState: TExpertState;
begin
if ( FMainTriggered ) then
    Result := [esEnabled, esChecked]
else
    Result := [esEnabled];
end;

function TWizardAutoHide.GetIDString: String;
begin
Result := AH_SIGNATURE; // must be unique
end;

function TWizardAutoHide.GetMenuText: String;
begin
Result := AH_NAME;
end;


procedure TWizardAutoHide.Execute;
begin
EngineConfigure;
end;


// -----------------------------------------------------------------------------


procedure ActivateWindow ( aWnd: THandle );
begin
EngineExecute ( InternalActivateWindow, aWnd, 0 );
end;


procedure RemoveWindow ( aWnd: THandle );
begin
EngineExecute ( InternalRemoveWindow, aWnd, 0 );
end;


procedure MenuClick ( aWnd: THandle; aCode: integer );
begin
EngineExecute ( InternalMenuClick, aWnd, aCode );
end;


procedure LockExpandWindow ( aWnd: THandle; aLockExpand: boolean );
begin
EngineExecute ( InternalLockExpandWindow, aWnd, DWORD(aLockExpand) );
end;


function GetButtonBitmap ( aButtonStatus: TButtonStatus ): TBitmap;
begin
case ( aButtonStatus ) of
    bsNormal:
        result := FButtonBmpNormal;
    bsDown:
        result := FButtonBmpDown;
    bsHighlight:
        result := FButtonBmpHighlight;
    else
        result := NIL;
    end;
end;


// -----------------------------------------------------------------------------


procedure Register;
begin
RegisterLibraryExpert (TWizardAutoHide.Create);
end;


// -----------------------------------------------------------------------------


initialization;
    DelphiLoadInit;
finalization;
    EngineKill;

end.




