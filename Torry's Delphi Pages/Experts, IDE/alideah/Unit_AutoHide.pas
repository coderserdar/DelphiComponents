unit Unit_AutoHide;

interface

uses
    Windows, Messages, SysUtils, Classes, Forms, Controls, Unit_Misc, Unit_WndProp;

type
    TAutoHideMan = class
        private
            FShrunk: TList;
            FExpanded: TList;

            procedure Shrunk_Clear;
            function  Shrunk_Find ( aItem: PWndProp ): boolean;
            procedure Shrunk_Add ( aItem: PWndProp );
            procedure Shrunk_Del ( aItem: PWndProp );
            procedure Shrunk_Restore ( aItem: PWndProp );
            procedure Shrunk_Shrink ( aItem: PWndProp;
                                      aPrevShrinkType: TShrinkType;
                                      aPrevUTtype: TShrinkDirection );

            procedure Expanded_Clear;
            function  Expanded_Find ( aItem: PWndProp ): boolean;
            procedure Expanded_Add ( aItem: PWndProp );
            procedure Expanded_Del ( aItem: PWndProp );

            procedure Expanded_2_Shrunk;
        public
            constructor Create;
            destructor  Destroy; override;

            procedure Clear;
            procedure ActiveWindow ( aItem: PWndProp );
            procedure AddWindow ( aItem: PWndProp; aExpanded: boolean );
            procedure Remove ( aItem: PWndProp; aRestore: boolean );
            procedure LockExpandWindow ( aWnd: THandle; aLockExpand: boolean );
            procedure RestoreShrunkWindow;
            procedure BringToTopMost;
            procedure UpdateWindowLock ( aItem: PWndProp; aLockType: TLockType );
            procedure UpdateWindowShrink ( aItem: PWndProp; aShrinkType: TShrinkType );
            procedure UpdateWindowUTStyle ( aItem: PWndProp; aUTStyle: TShrinkDirection );
            procedure UpdateShrunkCaption;
        end;


// -----------------------------------------------------------------------------

implementation

uses
    Main, Math, Dialogs, Unit_Constants;

// -----------------------------------------------------------------------------


constructor TAutoHideMan.Create;
begin
inherited;
FShrunk := TList.Create;
FExpanded := TList.Create;
end;


destructor TAutoHideMan.Destroy;
begin
Shrunk_Clear;
FShrunk.Free;
FExpanded.Free;
inherited;
end;


procedure TAutoHideMan.Clear;
begin
Shrunk_Clear;
Expanded_Clear;
end;


function TAutoHideMan.Shrunk_Find ( aItem: PWndProp ): boolean;
begin
result := FShrunk.IndexOf ( aItem ) <> -1;
end;


procedure TAutoHideMan.Shrunk_Shrink ( aItem: PWndProp;
            aPrevShrinkType: TShrinkType; aPrevUTtype: TShrinkDirection );
const
    SWPParam: array [boolean] of DWORD = ( SWP_NOZORDER, 0 );
var
    aWnd: THandle;
    aRect: TRect;
    aWidth: integer;
    aOnShrink: DWORD;
begin
aWnd := aItem^.FWnd;

inc ( aItem.FSetPosCounter );
try
    GetWindowRect ( aItem^.FWnd, aRect );

    if ( not aItem^.FShrunk ) then begin
        aItem^.FWidth := aRect.Right - aRect.Left;
        aItem^.FHeight := aRect.Bottom - aRect.Top;
        aItem^.FShrunk := TRUE;
        end
    else begin
        case ( aPrevShrinkType ) of
            stCaption: begin
                aRect.Bottom := aRect.Top + aItem^.FHeight;
                end;
            stShortCaption: begin
                if ( aItem^.FAHProp.FDirection in [sdirTop, sdirRight] ) then begin
                    aRect.Left := aRect.Right - aItem^.FWidth;
                    aRect.Bottom := aRect.Top + aItem^.FHeight;
                    end
                else begin
                    aRect.Right := aRect.Left + aItem^.FWidth;
                    aRect.Bottom := aRect.Top + aItem^.FHeight;
                    end;
                end;
            stUltraThin: begin
                case ( aPrevUTtype ) of
                    sdirTop:     aRect.Bottom := aRect.Top + aItem^.FHeight;
                    sdirLeft:    aRect.Right := aRect.Left + aItem^.FWidth;
                    sdirBottom:  aRect.Top  := aRect.Bottom - aItem^.FHeight;
                    sdirRight:   aRect.Left := aRect.Right - aItem^.FWidth;
                    end; // case
                end;
            end; // case
        end; // else

    aOnShrink := 0;
    case ( aItem^.FAHProp.FCaptionOnShrink ) of
        saTop:     aOnShrink := HWND_TOP;
        saTopMost: aOnShrink := HWND_TOPMOST;
        end; // case

    case ( aItem^.FAHProp.FShrinkType ) of
        stCaption: begin
            SetWindowPos ( aWnd, aOnShrink, 0, 0,
                    aItem^.FWidth, GetCaptionHeight( aWnd ),
                    SWPParam[aOnShrink<>0] or SWP_NOMOVE or SWP_NOACTIVATE );
            end;
        stShortCaption: begin
            aWidth := min ( GetShortCaptionWindowWidth ( aWnd ), aRect.Right - aRect.Left );

            if ( aItem^.FAHProp.FDirection in [sdirTop, sdirRight] ) then
                SetWindowPos ( aWnd, aOnShrink,
                    aRect.Right - aWidth, aRect.Top,
                    aWidth, GetCaptionHeight( aWnd ),
                    SWPParam[aOnShrink<>0] or SWP_NOACTIVATE )
            else
                SetWindowPos ( aWnd, aOnShrink,
                    0, 0,
                    aWidth, GetCaptionHeight( aWnd ),
                    SWPParam[aOnShrink<>0] or SWP_NOACTIVATE or SWP_NOMOVE );
            end;
        stUltraThin: begin
            case ( aItem^.FAHProp.FUTOnShrink ) of
                saNone:    aOnShrink := 0;
                saTop:     aOnShrink := HWND_TOP;
                saTopMost: aOnShrink := HWND_TOPMOST;
                end; // case

            case ( aItem^.FAHProp.FDirection ) of
                sdirTop: begin
                    SetWindowPos ( aWnd, aOnShrink, 0, 0,
                        aItem^.FWidth, aItem^.FAHProp.FUTWeight,
                        SWPParam[aOnShrink<>0] or SWP_NOMOVE or SWP_NOACTIVATE );
                    end;
                sdirLeft: begin
                    SetWindowPos ( aWnd, aOnShrink, 0, 0,
                        aItem^.FAHProp.FUTWeight, aItem^.FHeight,
                        SWPParam[aOnShrink<>0] or SWP_NOMOVE or SWP_NOACTIVATE );
                    end;
                sdirBottom: begin
                    SetWindowPos ( aWnd, aOnShrink, aRect.Left, aRect.Bottom - aItem^.FAHProp.FUTWeight,
                        aItem^.FWidth, aItem^.FAHProp.FUTWeight,
                        SWPParam[aOnShrink<>0] or SWP_NOACTIVATE );
                    end;
                sdirRight: begin
                    SetWindowPos ( aWnd, aOnShrink, aRect.Right - aItem^.FAHProp.FUTWeight, aRect.Top,
                        aItem^.FAHProp.FUTWeight, aItem^.FHeight,
                        SWPParam[aOnShrink<>0] or SWP_NOACTIVATE );
                    end;
                end; // case
            end;
        end; // case

finally
    dec ( aItem.FSetPosCounter );
    end;
end;



procedure TAutoHideMan.Shrunk_Restore ( aItem: PWndProp );
var
    aWnd: THandle;
    aRect: TRect;
begin
aWnd  := aItem^.FWnd;

inc ( aItem.FSetPosCounter );
try
    aItem^.FShrunk := FALSE;
    GetWindowRect ( aWnd, aRect );

    case ( aItem^.FAHProp.FShrinkType ) of
        stCaption: begin
            SetWindowPos ( aWnd, HWND_NOTOPMOST, 0, 0,
                    aItem^.FWidth, aItem^.FHeight,
                    SWP_NOMOVE or SWP_NOACTIVATE );
            end;
        stShortCaption: begin
            if ( aItem^.FAHProp.FDirection in [sdirTop, sdirRight] ) then
                SetWindowPos ( aWnd, HWND_NOTOPMOST,
                    aRect.Right - aItem^.FWidth, aRect.Top,
                    aItem^.FWidth, aItem^.FHeight,
                    SWP_NOACTIVATE )
            else
                SetWindowPos ( aWnd, HWND_NOTOPMOST, 0, 0,
                    aItem^.FWidth, aItem^.FHeight,
                    SWP_NOMOVE or SWP_NOACTIVATE );
            end;
        stUltraThin: begin
            case ( aItem^.FAHProp.FDirection ) of
                sdirTop,
                sdirLeft: begin
                    SetWindowPos ( aWnd, HWND_NOTOPMOST, 0, 0,
                        aItem^.FWidth, aItem^.FHeight,
                        SWP_NOMOVE or SWP_NOACTIVATE );
                    end;
                sdirBottom: begin
                    SetWindowPos ( aWnd, HWND_NOTOPMOST,
                        aRect.Left, aRect.Bottom - aItem^.FHeight,
                        aItem^.FWidth, aItem^.FHeight,
                        SWP_NOACTIVATE );
                    end;
                sdirRight: begin
                    SetWindowPos ( aWnd, HWND_NOTOPMOST,
                        aRect.Right - aItem^.FWidth, aRect.Top,
                        aItem^.FWidth, aItem^.FHeight,
                        SWP_NOACTIVATE );
                    end;
                end; // case
            end;
        end; // case

finally
    dec ( aItem.FSetPosCounter );
    end; // finally

if ( FExeSound ) then
   MessageBeep ( EXECUTE_SOUND );
end;



procedure TAutoHideMan.Shrunk_Clear;
var
   count, counter: integer;
   aItem: PWndProp;
begin
count := FShrunk.Count - 1;
for counter := 0 to count do begin
    aItem := PWndProp ( FShrunk[counter] );
    Shrunk_Restore ( aItem );
    end; // for

FShrunk.Clear;
end;



procedure TAutoHideMan.Shrunk_Add ( aItem: PWndProp );
begin
if ( not Shrunk_Find ( aItem ) ) then begin
    FShrunk.Add ( aItem );
    Shrunk_Shrink ( aItem, stIgnore, sdirIgnore );

    if ( FExeSound ) then
        MessageBeep ( EXECUTE_SOUND );
    end;
end;



procedure TAutoHideMan.Shrunk_Del ( aItem: PWndProp );
var
    aIndex: integer;
begin
aIndex := FShrunk.IndexOf ( aItem );
if ( aIndex <> -1 ) then begin
    Shrunk_Restore ( aItem );
    FShrunk.Delete ( aIndex );
    end;
end;


procedure TAutoHideMan.Expanded_Clear;
begin
FExpanded.Clear;
end;


function TAutoHideMan.Expanded_Find ( aItem: PWndProp ): boolean;
begin
result := FExpanded.IndexOf ( aItem ) >= 0;
end;


procedure TAutoHideMan.Expanded_Add ( aItem: PWndProp );
begin
if ( not Expanded_Find ( aItem ) ) then begin
    FExpanded.Add ( aItem );
    end;
end;


procedure TAutoHideMan.Expanded_Del ( aItem: PWndProp );
begin
FExpanded.Remove ( aItem );
end;


procedure TAutoHideMan.Expanded_2_Shrunk;
var
    count, counter: integer;
    aWnd, aParent, aAppHandle, aForeground: THandle;
    aItem: PWndProp;
begin
aForeground := GetForegroundWindow;
aAppHandle := FApplication.Handle;

count := FExpanded.Count - 1;
for counter := count downto 0 do begin
    aItem := PWndProp ( FExpanded[counter] );
    if ( aItem^.FAHProp.FLockType = ltLockExpand ) then continue;

    aWnd := aItem^.FWnd;
    if ( aWnd <> aForeground ) then begin
        aParent := GetParent ( aWnd );
        if ( aParent = 0 ) or ( aParent = aAppHandle ) then begin
            Expanded_Del ( aItem );
            Shrunk_Add ( aItem );
            end; // if
        end; // if
    end; // for
end;


procedure TAutoHideMan.ActiveWindow ( aItem: PWndProp );
begin
if ( aItem <> NIL ) and ( aItem^.FAHProp.FLockType <> ltLockShrink ) then begin
    Expanded_Del ( aItem ); // going to be used in the Expanded_2_Shrunk
    Shrunk_Del ( aItem );
    end;

Expanded_2_Shrunk;

if ( aItem <> NIL ) and ( aItem^.FAHProp.FLockType <> ltLockShrink ) then
    Expanded_Add ( aItem );
end;


procedure TAutoHideMan.AddWindow ( aItem: PWndProp; aExpanded: boolean );
begin
if ( aExpanded ) or ( aItem^.FAHProp.FLockType = ltLockExpand ) then begin
    Expanded_Add ( aItem );
    end
else begin
    Shrunk_Add ( aItem );
    end;
end;


procedure TAutoHideMan.Remove ( aItem: PWndProp; aRestore: boolean );
var
    aIndex: integer;
begin
aIndex := FShrunk.IndexOf ( aItem );
if ( aIndex <> -1 ) then begin
    if ( aRestore ) then
        Shrunk_Del ( aItem )
    else
        FShrunk.Delete ( aIndex );
    exit;
    end;

aIndex := FExpanded.IndexOf ( aItem );
if ( aIndex <> -1 ) then begin
    if ( aRestore ) then
        Expanded_Del ( aItem )
    else
        FExpanded.Delete ( aIndex );
    end;
end;


procedure TAutoHideMan.LockExpandWindow ( aWnd: THandle; aLockExpand: boolean );
var
   count, counter: integer;
   aItem: PWndProp;
begin
count := FExpanded.Count - 1;
for counter := 0 to count do begin
    aItem := PWndProp ( FExpanded[counter] );
    if ( aItem^.FWnd = aWnd ) then begin
        if ( aLockExpand ) then
            aItem^.FAHProp.FLockType := ltLockExpand
        else
            aItem^.FAHProp.FLockType := ltUnlock;

        exit;
        end; // if
    end; // for

count := FShrunk.Count - 1;
for counter := 0 to count do begin
    aItem := PWndProp ( FShrunk[counter] );
    if ( aItem^.FWnd = aWnd ) then begin
        Shrunk_Restore ( aItem );
        Expanded_Add ( aItem );
        FShrunk.Delete ( counter );

        if ( aLockExpand ) then
            aItem^.FAHProp.FLockType := ltLockExpand
        else
            aItem^.FAHProp.FLockType := ltUnlock;

        exit;
        end; // if
    end; // for
end;


procedure TAutoHideMan.RestoreShrunkWindow;
var
   count, counter: integer;
   aItem: PWndProp;
begin
count := FShrunk.Count - 1;
for counter := count downto 0 do begin
    aItem := PWndProp ( FShrunk[counter] );
    if ( aItem^.FAHProp.FLockType = ltLockShrink ) then continue;

    Shrunk_Restore ( aItem );
    Expanded_Add ( aItem );
    FShrunk.Delete ( counter );
    end; // for
end;



procedure TAutoHideMan.BringToTopMost;
var
   count, counter: integer;
   aItem: PWndProp;
   aExStyle: DWORD;
begin
count := FShrunk.Count - 1;
for counter := count downto 0 do begin
    aItem := PWndProp ( FShrunk[counter] );

    if ( aItem^.FShrunk ) then
    if ( ((aItem^.FAHProp.FShrinkType = stUltraThin) and (aItem^.FAHProp.FUTOnShrink = saTopMost))
         or ((aItem^.FAHProp.FShrinkType <> stUltraThin) and  (aItem^.FAHProp.FCaptionOnShrink = saTopMost)) ) then begin
        aExStyle := GetWindowLong ( aItem^.FWnd, GWL_EXSTYLE );
        if ( aExStyle and WS_EX_TOPMOST <> WS_EX_TOPMOST ) then begin
            SetWindowPos ( aItem^.FWnd, HWND_TOPMOST, 0, 0, 0, 0,
                    SWP_NOMOVE or SWP_NOSIZE or {SWP_NOREDRAW or} SWP_NOACTIVATE );
            end; // if
        end; // if
    end; // for
end;


procedure TAutoHideMan.UpdateWindowLock ( aItem: PWndProp; aLockType: TLockType );
var
    aWnd: THandle;
begin
if ( aItem^.FAHProp.FLockType = aLockType ) then exit;

case ( aLockType ) of
    ltLockExpand: begin
        aItem^.FAHProp.FLockType := ltLockExpand;
        end;

    ltLockShrink: begin
        if ( aItem^.FAHProp.FShrinkType = stUltraThin ) then begin
            MessageDlg ( ULTRATHINSHRINKLOCK_ERROR, mtError, [mbOK], 0 );
            exit;
            end;

        aItem^.FAHProp.FLockType := ltLockShrink;
        if ( FExpanded.IndexOf ( aItem ) <> -1 ) then begin
            Expanded_Del ( aItem );
            Shrunk_Add ( aItem );
            end;
        end;

    ltUnLock: begin
        aItem^.FAHProp.FLockType := ltUnlock;

        if ( FShrunk.IndexOf ( aItem ) <> -1 ) then begin
            aWnd := GetActiveWindow;
            if ( aWnd <> aItem^.FWnd ) then
                //aWnd := WndFromPoint ( FApplication.Handle );
                aWnd := WndFromPoint;

            if ( aWnd = aItem^.FWnd ) then begin
               Shrunk_Del ( aItem );
               Expanded_Add ( aItem );
               end; // if
            end; // if
        end;
    end; // case

end;


procedure TAutoHideMan.UpdateWindowShrink ( aItem: PWndProp; aShrinkType: TShrinkType );
var
    aPrevShrink: TShrinkType;
begin
if ( aItem^.FAHProp.FShrinkType = aShrinkType ) then exit;
if ( aShrinkType = stUltraThin ) and ( aItem^.FAHProp.FLockType = ltLockShrink ) then begin
   MessageDlg ( ULTRATHINSHRINKLOCK_ERROR, mtError, [mbOK], 0 );
   exit;
   end;

aPrevShrink := aItem^.FAHProp.FShrinkType;
aItem^.FAHProp.FShrinkType := aShrinkType;

if ( not aItem^.FShrunk ) or ( aItem^.FAHProp.FLockType = ltLockExpand ) then exit;
Shrunk_Shrink ( aItem, aPrevShrink, aItem^.FAHProp.FDirection );
end;


procedure TAutoHideMan.UpdateWindowUTStyle ( aItem: PWndProp; aUTStyle: TShrinkDirection );
var
    aPrevUT: TShrinkDirection;
begin
if ( aItem^.FAHProp.FDirection = aUTStyle ) then exit;
aPrevUT := aItem^.FAHProp.FDirection;
aItem^.FAHProp.FDirection := aUTStyle;

if ( not aItem^.FShrunk ) or ( aItem^.FAHProp.FLockType = ltLockExpand ) then exit;
Shrunk_Shrink ( aItem, stUltraThin, aPrevUT );
end;



procedure TAutoHideMan.UpdateShrunkCaption;
const
    SWPParam: array [boolean] of DWORD = ( SWP_NOZORDER, 0 );
var
    count, counter: integer;
    aItem: PWndProp;
    aWnd: THandle;
    aOnShrink: DWORD;
    aRect: TRect;
    aWidth: integer;
begin
count := FShrunk.Count - 1;
for counter := count downto 0 do begin
    aItem := PWndProp ( FShrunk[counter] );

    if ( (aItem^.FShrunk) and (aItem^.FAHProp.FShrinkType <> stUltraThin) ) then begin
        aWnd := aItem^.FWnd;

        inc ( aItem.FSetPosCounter );
        try
            aOnShrink := 0;
            case ( aItem^.FAHProp.FCaptionOnShrink ) of
                saTop:     aOnShrink := HWND_TOP;
                saTopMost: aOnShrink := HWND_TOPMOST;
                end; // case

            if ( aItem^.FAHProp.FShrinkType = stCaption ) then begin
                SetWindowPos ( aWnd, aOnShrink, 0, 0,
                    aItem^.FWidth, GetCaptionHeight( aWnd ),
                    SWPParam[aOnShrink<>0] or SWP_NOMOVE or SWP_NOACTIVATE );
                end
            else begin
                GetWindowRect ( aItem^.FWnd, aRect );

                if ( aItem^.FAHProp.FDirection in [sdirTop, sdirRight] ) then
                    aRect.Left := aRect.Right - aItem^.FWidth
                else
                    aRect.Right := aRect.Left + aItem^.FWidth;

                aWidth := min ( GetShortCaptionWindowWidth ( aWnd ), aRect.Right - aRect.Left );

                if ( aItem^.FAHProp.FDirection in [sdirTop, sdirRight] ) then
                    SetWindowPos ( aWnd, aOnShrink,
                        aRect.Right - aWidth, aRect.Top,
                        aWidth, GetCaptionHeight( aWnd ),
                        SWPParam[aOnShrink<>0] or SWP_NOACTIVATE )
                else
                    SetWindowPos ( aWnd, aOnShrink,
                        0, 0,
                        aWidth, GetCaptionHeight( aWnd ),
                        SWPParam[aOnShrink<>0] or SWP_NOACTIVATE or SWP_NOMOVE );
                end; // else

        finally
            dec ( aItem.FSetPosCounter );
            end;
        end; // if
    end; // for
end;



end.
