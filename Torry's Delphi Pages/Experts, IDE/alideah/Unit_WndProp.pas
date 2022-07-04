unit Unit_WndProp;

interface

uses
    SysUtils, Windows, Messages, Classes, Menus, Unit_Misc, Unit_Constants;

type
    {TWndProperty}
    TWndProperty = class
        private
            FList: TList;
            FNewWndList: TList;
            FPassportList: TStringList;

            FMenu: TPopupMenu;
            FNewWndCriticalSection: TRTLCriticalSection;

            procedure InternalAdd ( aWndProp: PWndProp );
            function  CreateWndProp ( aWnd: THandle; const aClassname: string; aAHProp: PWndPropAH ): PWndProp;
            procedure FreeResource ( aItem: PWndProp );
            procedure ReadPassportFromRegistry;
            procedure PrepareMenu ( aItem: PWndProp );
            
        public
            constructor Create;
            destructor Destroy; override;

            procedure Execute;

            function  Add ( aWnd: THandle; const aClassname: string; aAHProp: PWndPropAH=NIL ): PWndProp;
            procedure Remove ( aWnd: THandle );
            function  Get ( aWnd: THandle ): PWndProp;
            procedure Clear;

            procedure UpdatePassport;
            procedure ClearPassport;
            function  IsIncluded ( const aClassName: string ): boolean;

            procedure ClearNewWnd;
            procedure AddNewWnd ( aWnd: THandle );
            procedure ProcessNewWnd;

            procedure ShowMenu ( aItem: PWndProp; const x, y: integer );
            procedure CreateMenu;
            procedure DestroyMenu;
            procedure OnMenuClick ( Sender: TObject );

            class function WndPropSignature ( aItem: PWndProp ): string;
            property Menu: TPopupMenu read FMenu;
        end;



// -----------------------------------------------------------------------------

implementation

uses
    Main, Registry, Controls, Forms, Unit_AutoHide;


// -----------------------------------------------------------------------------



function SubClassWinProc ( hWnd: THandle; Message: UINT; aWParam: WPARAM; aLParam: LPARAM ): LRESULT;  stdcall;
var
    aItem: PWndProp;
    aWPos: PWINDOWPOS;
    aOldWndProc: DWORD;
    aStyle: DWORD;
    aMessage: TagMsg;
    aWindowRect: TRect;
    aPoint: TPoint;
    aMouseIn, aMouseIn2: boolean;
    aActivated: TDrawButton; 
    aButtonStatus: TButtonStatus;
begin
aItem := FWndProp.Get ( hWnd );


case ( Message ) of
    WM_WINDOWPOSCHANGING: begin // will be called by the SetWindowPos
        if ( aItem^.FSetPosCounter > 0 ) then begin
            result := 0;
            exit;
            end;

        if ( aItem^.FShrunk ) then  begin
            aWPos := PWINDOWPOS ( aLParam );
            aWPos^.flags := aWPos^.flags or SWP_NOSIZE;
            result := 0;
            exit;
            end;
        end; // WM_WINDOWPOSCHANGING


    WM_ACTIVATE: begin
        //if ( hWnd = GetForegroundWindow ) then
        if ( LOWORD(aWParam) <> WA_INACTIVE ) and ( HIWORD(aWParam) = 0 ) then begin
            ActivateWindow ( hWnd );
            aActivated := dbActivated;
            end
        else
            aActivated := dbInactivated;

        DrawButton ( aItem, GetButtonBitmap ( aItem^.FButtonStatus ), aActivated );
        end; // WM_ACTIVATE


    WM_NCCALCSIZE: begin
        if ( aWParam <> 0 ) then begin
            result := CallWindowProc ( Pointer(aItem^.FOldWndProc), hWnd, Message, aWParam, aLParam );

            GetCaptionButtonRect ( hWnd, PNCCalcSizeParams ( aLParam )^.lppos.cx, aItem^.FButtonRect );

            aStyle := GetWindowLong ( aItem^.FWnd, GWL_STYLE );
            aItem^.FButtonAvailable := FCaptionButton
                               and ( aStyle and WS_MINIMIZE = 0 )
                               and not ( aItem^.FShrunk and (aItem^.FAHProp.FShrinkType = stUltraThin) )
                               and CanDisplayButton ( aItem );
            exit;
            end; // if
        end; // WM_NCCALCSIZE


    WM_NCACTIVATE, WM_PAINT, WM_NCPAINT: begin
        if ( aItem^.FButtonAvailable ) then begin
            result := CallWindowProc ( Pointer(aItem^.FOldWndProc), hWnd, Message, aWParam, aLParam );

            if ( Message <> WM_NCACTIVATE ) then
                aActivated := dbUnknown
            else
            if (aWParam <> 0) or ((aWParam = 0) and (result = 0)) then
                aActivated := dbActivated
            else
                aActivated := dbInactivated;

            DrawButton ( aItem, GetButtonBitmap ( aItem^.FButtonStatus ), aActivated );
            exit;
            end; // if
        end; // WM_NCACTIVATE, WM_PAINT, WM_NCPAINT


    WM_NCHITTEST: begin
        GetWindowRect ( hWnd, aWindowRect );
        GetCursorPos ( aPoint );
        dec ( aPoint.X, aWindowRect.Left );
        dec ( aPoint.Y, aWindowRect.Top );
        if ( aItem^.FButtonAvailable )
            and ( PtInRect ( aItem^.FButtonRect, aPoint ) ) then begin
            result := HTBORDER;
            exit;
            end;
        end; // WM_NCHITTEST


    WM_NCLBUTTONDOWN: begin
        if ( aWParam = HTBORDER ) then begin
            GetWindowRect ( hWnd, aWindowRect );
            GetCursorPos ( aPoint );
            dec ( aPoint.X, aWindowRect.Left );
            dec ( aPoint.Y, aWindowRect.Top );

            if ( PtInRect ( aItem^.FButtonRect, aPoint ) ) then begin
                SetCapture ( hWnd );
                aItem^.FButtonStatus := bsDown;
                DrawButton ( aItem, GetButtonBitmap ( aItem^.FButtonStatus ) );
                aMouseIn   := TRUE;

                try
                    while ( GetCapture = hWnd ) do begin
                        case ( Integer ( GetMessage ( aMessage, 0, 0, 0 ) ) ) of
                            -1: break; // failed
                             0: begin
                                PostQuitMessage ( aMessage.WParam ); // WM_QUIT
                                break;
                                end;
                            end; // case

                        case ( aMessage.Message ) of
                            WM_KEYDOWN, WM_KEYUP,
                            WM_MBUTTONDOWN, WM_MBUTTONUP, WM_MBUTTONDBLCLK,
                            WM_RBUTTONDOWN, WM_RBUTTONUP, WM_RBUTTONDBLCLK:
                                begin
                                // do nothing
                                end;

                            WM_MOUSEMOVE: begin
                                GetCursorPos ( aPoint );
                                dec ( aPoint.X, aWindowRect.Left );
                                dec ( aPoint.Y, aWindowRect.Top );

                                aMouseIn2 := PtInRect ( aItem^.FButtonRect, aPoint );
                                if ( aMouseIn <> aMouseIn2 ) then begin
                                    aMouseIn := aMouseIn2;
                                    if ( aMouseIn ) then
                                        aItem^.FButtonStatus := bsDown
                                    else
                                        aItem^.FButtonStatus := bsHighlight;
                                    DrawButton ( aItem, GetButtonBitmap(aItem^.FButtonStatus) );
                                    end;
                                end;

                            WM_LBUTTONUP: begin
                                DrawButton ( aItem, GetButtonBitmap(aItem^.FButtonStatus) );

                                GetCursorPos ( aPoint );
                                dec ( aPoint.X, aWindowRect.Left );
                                dec ( aPoint.Y, aWindowRect.Top );

                                aItem^.FButtonStatus := bsNormal;
                                DrawButton ( aItem, GetButtonBitmap(aItem^.FButtonStatus) );

                                if ( PtInRect ( aItem^.FButtonRect, aPoint ) ) then begin
                                   FWndProp.ShowMenu ( aItem,
                                                  aWindowRect.Left + aItem^.FButtonRect.Left,
                                                  aWindowRect.Top + aItem^.FButtonRect.Bottom );
                                   end;

                                Break;
                              end;

                            else begin
                                  TranslateMessage ( aMessage );
                                  DispatchMessage ( aMessage );
                                  end;
                            end; // case
                        end; // while

                  finally
                      DrawButton ( aItem, GetButtonBitmap(aItem^.FButtonStatus) );

                      if ( GetCapture = hWnd ) then
                         ReleaseCapture;
                      end; // finally

                end; // if ( PtInRect ...
            end; // if ( aWParam = HTBORDER )
        end; // WM_NCLBUTTONDOWN


    WM_NCRBUTTONDOWN, WM_NCRBUTTONUP: begin
        if ( aItem^.FButtonAvailable ) and ( aWParam = HTBORDER ) then begin
            GetWindowRect ( hWnd, aWindowRect );
            GetCursorPos ( aPoint );
            dec ( aPoint.X, aWindowRect.Left );
            dec ( aPoint.Y, aWindowRect.Top );
            if ( PtInRect ( aItem^.FButtonRect, aPoint ) ) then begin
                result := 0;
                exit;
                end;
            end; // if
        end; // WM_NCRBUTTONDOWN, WM_NCRBUTTONUP


    WM_NCMOUSEMOVE: begin
        if ( aItem^.FButtonAvailable ) then begin
            aButtonStatus := aItem^.FButtonStatus;

            if ( aWParam = HTBORDER ) then begin
                GetWindowRect ( hWnd, aWindowRect );
                GetCursorPos ( aPoint );
                dec ( aPoint.X, aWindowRect.Left );
                dec ( aPoint.Y, aWindowRect.Top );

                if ( PtInRect ( aItem^.FButtonRect, aPoint ) ) then
                    aItem^.FButtonStatus := bsHighlight
                else
                    aItem^.FButtonStatus := bsNormal;
                end
            else
                aItem^.FButtonStatus := bsNormal;

            if ( aButtonStatus <> aItem^.FButtonStatus ) then
               DrawButton ( aItem, GetButtonBitmap(aItem^.FButtonStatus) );
            end; // if
        end; // WM_NCMOUSEMOVE


    WM_NCLBUTTONDBLCLK: begin
        if ( aItem^.FButtonAvailable ) and ( aWParam = HTBORDER ) then begin
            GetWindowRect ( hWnd, aWindowRect );
            GetCursorPos ( aPoint );
            dec ( aPoint.X, aWindowRect.Left );
            dec ( aPoint.Y, aWindowRect.Top );

            if ( PtInRect ( aItem^.FButtonRect, aPoint ) ) then
                LockExpandWindow ( hWnd, aItem^.FAHProp.FLockType <> ltLockExpand );
            end; // if
        end; // WM_NCLBUTTONDBLCLK


    WM_DESTROY: begin
        aOldWndProc := aItem^.FOldWndProc;
        RemoveWindow ( hWnd );
        result := CallWindowProc ( Pointer(aOldWndProc), hWnd, Message, aWParam, aLParam );
        exit;
        end; // WM_DESTROY

    end; // case

result := CallWindowProc ( Pointer(aItem^.FOldWndProc), hWnd, Message, aWParam, aLParam );
end;



// -----------------------------------------------------------------------------


{TWndProperty}
constructor TWndProperty.Create;
begin
inherited;

InitializeCriticalSection ( FNewWndCriticalSection );

FList := TList.Create;
FNewWndList := TList.Create;
FPassportList := TStringList.Create;
end;


destructor TWndProperty.Destroy;
begin
ClearPassport;
ClearNewWnd;
Clear;

FPassportList.Free;
FNewWndList.Free;
FList.Free;

DestroyMenu;
DeleteCriticalSection ( FNewWndCriticalSection );

inherited;
end;


function TWndProperty.Get ( aWnd: THandle ): PWndProp;
var
    aItem: PWndProp;
    count, counter: integer;
begin
result := NIL;
count := FList.Count - 1;
for counter := 0 to count do begin
    aItem := PWndProp ( FList[counter] );
    if ( aItem^.FWnd = aWnd ) then begin
        result := aItem;
        break;
        end;
    end; // for
end;


procedure TWndProperty.FreeResource ( aItem: PWndProp );
begin
if ( aItem^.FOldWndProc <> 0 ) then
    SetWindowLong ( aItem^.FWnd, GWL_WNDPROC, integer(aItem^.FOldWndProc) );

RedrawWindow ( aItem^.FWnd, NIL, 0, RDW_FRAME or RDW_INVALIDATE	or RDW_UPDATENOW );
end;


function TWndProperty.CreateWndProp ( aWnd: THandle; const aClassname: string; aAHProp: PWndPropAH ): PWndProp;
var
    aIndex: integer;
begin
new ( result );
FillChar ( result^, sizeof(result^), 0 );

result^.FWnd := aWnd;
result^.FClassName := aClassName;
result^.FSetPosCounter := 0;

if ( aAHProp = NIL ) then begin
    aIndex := FPassportList.IndexOf ( aClassName );
    aAHProp := PWndPropAH( FPassportList.Objects[aIndex] );
    end;

Move ( aAHProp^, result^.FAHProp, sizeof(result^.FAHProp) );

result^.FStyle := GetWindowLong ( aWnd, GWL_STYLE );
result^.FExStyle := GetWindowLong ( aWnd, GWL_EXSTYLE );
end;


procedure TWndProperty.InternalAdd ( aWndProp: PWndProp );
var
    aWnd: THandle;
begin
aWnd := aWndProp^.FWnd;

aWndProp^.FOldWndProc := GetWindowLong ( aWnd, GWL_WNDPROC );
SetWindowLong ( aWnd, GWL_WNDPROC, integer ( @SubClassWinProc ) );
FList.Add ( aWndProp );

GetCaptionButtonRect ( aWnd, GetWindowWidth ( aWnd ), aWndProp^.FButtonRect );
aWndProp^.FButtonAvailable := FCaptionButton
                           and not ( aWndProp^.FShrunk and (aWndProp^.FAHProp.FShrinkType = stUltraThin) )
                           and CanDisplayButton ( aWndProp );
if ( aWndProp^.FButtonAvailable ) then
    DrawButton ( aWndProp, GetButtonBitmap(aWndProp^.FButtonStatus) );
end;


function TWndProperty.Add ( aWnd: THandle; const aClassname: string; aAHProp: PWndPropAH ): PWndProp;
begin
result := Get ( aWnd );

if ( result = NIL ) then begin
    result := CreateWndProp ( aWnd, aClassname, aAHProp );
    InternalAdd ( result );
    end;
end;


procedure TWndProperty.Remove ( aWnd: THandle );
var
    aItem: PWndProp;
    count, counter: integer;
begin
count := FList.Count - 1;
for counter := 0 to count do begin
    aItem := PWndProp ( FList[counter] );
    if ( aItem^.FWnd = aWnd ) then begin
        FreeResource ( aItem );
        dispose ( aItem );
        FList.delete ( counter );
        exit;
        end;
    end;
end;


procedure TWndProperty.Clear;
var
   counter: integer;
   aItem: PWndProp;
   count: integer;
begin
count := FList.Count - 1;
for counter := count downto 0 do begin
    aItem := PWndProp ( FList[counter] );
    FreeResource ( aItem );
    dispose ( aItem );
    end;

FList.Clear;
end;


// -----------------------------------------------------------------------------


procedure TWndProperty.ClearPassport;
var
    count, counter: integer;
    aAHProp: PWndPropAH;
begin
count := FPassportList.count - 1;
for counter := 0 to count do begin
    aAHProp := PWndPropAH ( FPassportList.Objects[counter] );
    if ( aAHProp <> NIL ) then
        dispose ( aAHProp );
    end;

FPassportList.Clear;
end;



procedure TWndProperty.ReadPassportFromRegistry;
var
    count, counter: integer;
    aBuffer: string;
    aCor: integer;
    aRegistry: TRegistry;
    aAHProp: PWndPropAH;
begin
ClearPassport;

aRegistry := TRegistry.Create;
try
    aRegistry.RootKey := HKEY_CURRENT_USER;

    if ( aRegistry.OpenKey ( ROOT_KEYINCLUDE, FALSE ) ) then begin
        try
           count := aRegistry.ReadInteger ( REG_WINDOWS_COUNT ) - 1;
           for counter := 0 to count do begin
               aBuffer := aRegistry.ReadString ( inttostr(counter) );
               aCor := Pos ( SIGNATURE_HEAD, aBuffer );
               if ( aCor <= 1 ) then continue;

               new ( aAHProp );
               StrToAHWndProp ( aBuffer, aAHProp^ );
               Delete ( aBuffer, aCor, MAXINT );
               FPassportList.AddObject ( aBuffer, TObject(aAHProp) );
               end;
        except
            end;
        aRegistry.CloseKey;
        end; // if

finally
    aRegistry.Free;
    end;
end;



procedure TWndProperty.UpdatePassport;
begin
ClearPassport;
ReadPassportFromRegistry;
end;



function TWndProperty.IsIncluded ( const aClassName: string ): boolean;
begin
result := ( FPassportList.IndexOf ( aClassName ) <> -1 );
end;


// -----------------------------------------------------------------------------


procedure TWndProperty.ClearNewWnd;
var
    count, counter: integer;
    aAHProp: PWndProp;
begin
count := FNewWndList.Count - 1;
for counter := 0 to count do begin
    aAHProp := PWndProp ( FNewWndList[counter] );
    dispose ( aAHProp );
    end; // for

FNewWndList.Clear;
end;



procedure TWndProperty.AddNewWnd ( aWnd: THandle );
var
    aClassName: string;
    aWndProp: PWndProp;
begin
aClassName := GetWindowClassName ( aWnd );
if ( aClassName = '' ) then exit;

try
    EnterCriticalSection ( FNewWndCriticalSection );
    try
        if ( IsIncluded ( aClassName ) ) then begin
            aWndProp := CreateWndProp ( aWnd, aClassName, NIL );
            FNewWndList.Add ( aWndProp );
            end;
    finally
        LeaveCriticalSection ( FNewWndCriticalSection );
        end;
except
    end;
end;



procedure TWndProperty.ProcessNewWnd;
var
    count, counter: integer;
    aWndProp: PWndProp;
begin
EnterCriticalSection ( FNewWndCriticalSection );
try
    count := FNewWndList.Count - 1;
    for counter := 0 to count do begin
        aWndProp := PWndProp ( FNewWndList[counter] );
        InternalAdd ( aWndProp );
        FAutoHide.AddWindow ( aWndProp, aWndProp^.FAHProp.FLockType <> ltLockShrink );
        end; // for

    FNewWndList.Clear;

finally
    LeaveCriticalSection ( FNewWndCriticalSection );
    end;
end;

// -----------------------------------------------------------------------------


procedure TWndProperty.Execute;
var
    aWnd: THandle;
    aItem: PWndProp;
begin
ProcessNewWnd;

if ( ModalFormVisibled ) then begin
    if ( FModalRestore ) then
        FAutoHide.RestoreShrunkWindow;

    aWnd := GetActiveWindow;
    SetWindowPos ( aWnd, HWND_TOP, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE );
    exit;
    end;

FAutoHide.BringToTopMost;

aWnd := WndFromPoint;
if ( aWnd <> 0 ) then begin
    aItem := FWndProp.Get ( aWnd );
    if ( aItem <> NIL ) then
        FAutoHide.ActiveWindow ( aItem )
    else
        FAutoHide.ActiveWindow ( NIL );
    end;
end;

// -----------------------------------------------------------------------------

procedure TWndProperty.ShowMenu ( aItem: PWndProp; const x, y: integer );
begin
if ( FMenu = NIL ) then
    CreateMenu;

if ( aItem <> NIL ) then begin
    PrepareMenu ( aItem );
    FMenu.Tag := longint ( aItem^.FWnd );
    FMenu.Popup ( x, y );
    end;
end;



procedure TWndProperty.DestroyMenu;
begin
if ( FMenu <> NIL ) then begin
    FMenu.Free;
    FMenu := NIL;
    end;
end;


procedure TWndProperty.PrepareMenu ( aItem: PWndProp );
    function GetMenuItem ( aMenu: TPopupMenu; aID: integer ): TMenuItem;
    var
        count, counter: integer;
    begin
    count := aMenu.Items.Count - 1;
    for counter := 0 to count do begin
        if ( aMenu.Items[counter].Tag = aID ) then begin
            result := aMenu.Items[counter];
            exit;
            end; // if
        end; // for
    result := NIL;
    end;

    procedure CheckMenuItem ( aMenu: TPopupMenu; aID: integer; aChecked: boolean );
    var
        aItem: TMenuItem;
    begin
    aItem := GetMenuItem ( aMenu, aID );
    if ( aItem <> NIL ) then
        aItem.Checked := aChecked;
    end;

    procedure ShowMenuItem ( aMenu: TPopupMenu; aID: integer; aVisibled: boolean );
    var
        aItem: TMenuItem;
    begin
    aItem := GetMenuItem ( aMenu, aID );
    if ( aItem <> NIL ) then
        aItem.Visible := aVisibled;
    end;

var
    aHorzVisibled, aVertVisibled: boolean;
begin
case ( aItem^.FAHProp.FLockType ) of
    ltLockExpand: CheckMenuItem ( FMenu, MENU_LOCKEXPAND, TRUE );
    ltLockShrink: CheckMenuItem ( FMenu, MENU_LOCKSHRINK, TRUE );
    ltUnlock:     CheckMenuItem ( FMenu, MENU_UNLOCK, TRUE );
    end; // case

aHorzVisibled := FALSE;
aVertVisibled := FALSE;

case ( aItem^.FAHProp.FShrinkType ) of
    stCaption: begin
         CheckMenuItem ( FMenu, MENU_TYPECAPTION, TRUE );
         end;
    stShortCaption: begin
        CheckMenuItem ( FMenu, MENU_TYPESHORTCAPTION, TRUE );
        aHorzVisibled := TRUE;
        CheckMenuItem ( FMenu, MENU_UTLEFT,  aItem^.FAHProp.FDirection in [sdirLeft, sdirBottom] );
        CheckMenuItem ( FMenu, MENU_UTRIGHT, aItem^.FAHProp.FDirection in [sdirRight, sdirTop] );
        end;
    stUltraThin: begin
        CheckMenuItem ( FMenu, MENU_TYPEULTRATHIN, TRUE );
        aHorzVisibled := TRUE;
        aVertVisibled := TRUE;
        case ( aItem^.FAHProp.FDirection ) of
            sdirTop:     CheckMenuItem ( FMenu, MENU_UTTOP, TRUE );
            sdirLeft:    CheckMenuItem ( FMenu, MENU_UTLEFT, TRUE );
            sdirBottom:  CheckMenuItem ( FMenu, MENU_UTBOTTOM, TRUE );
            sdirRight:   CheckMenuItem ( FMenu, MENU_UTRIGHT, TRUE );
            end; // case
        end;
    end; // case

ShowMenuItem ( FMenu, MENU_UTTOP, aVertVisibled );
ShowMenuItem ( FMenu, MENU_UTLEFT, aHorzVisibled );
ShowMenuItem ( FMenu, MENU_UTBOTTOM, aVertVisibled );
ShowMenuItem ( FMenu, MENU_UTRIGHT, aHorzVisibled );
ShowMenuItem ( FMenu, MENU_UTSEPARATOR, aVertVisibled or aHorzVisibled );
end;



procedure TWndProperty.CreateMenu;
var
    aItem: TMenuItem;
    aCaption: string;
    aID: string;
    count, counter: integer;
    aType: char;
    aCor: integer;
    aStrings: TStringList;
begin
DestroyMenu;

FMenu := TPopupMenu.Create ( NIL );
FMenu.AutoHotkeys := maManual;

aStrings := TStringList.Create;
try
    SetLength ( aCaption, 512 );
    LoadString ( HInstance, RES_MENUITEMS, PChar(aCaption), 511 );
    SetLength ( aCaption, StrLen( PChar(aCaption) ) );

    aStrings.CommaText := aCaption;
    count := aStrings.Count - 1;
    for counter := 0 to count do begin
        aItem := TMenuItem.Create ( FMenu );
        FMenu.Items.Add ( aItem );

        aCaption := Trim ( aStrings[counter] );

        while ( Length ( aCaption ) >= 4 )
            and ( aCaption[1] = '(' ) do begin

            aCor := Pos ( ')', aCaption );
            if ( aCor = 0 ) then break;
            aType := aCaption[2];

            if ( aType = 'b' ) then begin
                aID := 'MENU_BMP_' + copy ( aCaption, 3, aCor-3 );
                //if ( FXPStyle ) then aID := aID + '_XP';

                Delete ( aCaption, 1, aCor );
                aItem.Bitmap.LoadFromResourceName ( HInstance, aID );
                end
            else
            if ( aType = 't' ) then begin
                aID := copy ( aCaption, 3, aCor-3 );
                Delete ( aCaption, 1, aCor );
                try
                    aItem.Tag := strtoint ( aID );
                    aItem.OnClick := OnMenuClick;
                except
                    raise Exception.Create ( 'Invalid MenuItem Tag: ' + aID );
                    end;
                end
            else
            if ( aType in ['r', 'R'] ) then begin
                aID := copy ( aCaption, 3, aCor-3 );
                Delete ( aCaption, 1, aCor );

                aItem.GroupIndex := strtoint ( aID );
                aItem.RadioItem := TRUE;
                aItem.Checked := aType = 'R';
                end // if
            else
                break;

            end; // while

        aItem.Caption := aCaption;
        end; // for
finally
    aStrings.Free;
    end;
end;


procedure TWndProperty.OnMenuClick ( Sender: TObject );
begin
MenuClick ( THandle(FMenu.Tag), TMenuItem(Sender).Tag );
end;


// -----------------------------------------------------------------------------


class function TWndProperty.WndPropSignature ( aItem: PWndProp ): string;
    function RectToStr ( var aRect: TRect ): string;
    begin
    result := '(' + inttostr( aRect.Left ) + ',' + inttostr( aRect.Top ) + ')'
            + '(' + inttostr( aRect.Right ) + ',' + inttostr( aRect.Bottom ) + ')';
    end;

begin
result := ' Next:0x' + InttoHex ( DWORD(aItem^.FNext), 8 )
        + ' Wnd:0x' + InttoHex ( DWORD(aItem^.FWnd), 8 )
        + ' SetPosCounter:' + Inttostr ( aItem^.FSetPosCounter )
        + ' OldWndProc:0x' + InttoHex ( aItem^.FOldWndProc, 8 )
        + ' ButtonRect:' + RectToStr ( aItem^.FButtonRect )
        + ' Style:' + InttoHex ( aItem^.FStyle, 8 )
        + ' ExStyle:0x' + InttoHex ( aItem^.FExStyle, 8 )
        + ' ClassName: ' + aItem^.FClassName;
end;




end.
