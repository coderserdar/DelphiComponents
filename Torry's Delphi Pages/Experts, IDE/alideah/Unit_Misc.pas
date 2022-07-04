unit Unit_Misc;

interface

uses
    SysUtils, Windows, Messages, Classes, Forms, Controls, Menus, Graphics, Registry;


type
    TCaptionButton = ( cbCaption, cbSmallCaption, cbSystem, cbContextHelp, cbSize, cbMinimize, cbMaximize, cbClose );
    TCaptionButtons = set of TCaptionButton;
    TButtonStatus = ( bsNormal, bsDown, bsHighlight );
    TDrawButton = ( dbInactivated, dbUnknown, dbActivated );

    TLockType = ( ltUnlock, ltLockExpand, ltLockShrink );
    TShrinkType = ( stCaption, stShortCaption, stUltraThin, stIgnore );
    TShrinkDirection = ( sdirTop, sdirLeft, sdirBottom, sdirRight, sdirIgnore );
    TShrinkAction = ( saNone, saTop, saTopMost );

    PWndPropAH = ^TWndPropAH;
    TWndPropAH = record
        FLockType: TLockType;
        FShrinkType: TShrinkType;
        FDirection: TShrinkDirection;

        FUTWeight: integer;
        FUTOnShrink: TShrinkAction;
        FCaptionOnShrink: TShrinkAction;
        end;

    PWndProp = ^TWndProp;
    TWndProp = record
        FNext: PWndProp;

        FWnd: THandle;
        FClassName: string;
        FSetPosCounter: integer;
        FOldWndProc: DWORD;
        FStyle: DWORD;
        FExStyle: DWORD;

        FShrunk: longbool;
        FHeight: integer;
        FWidth: integer;

        FButtonRect: TRect;
        FButtonStatus: TButtonStatus;
        FButtonAvailable: boolean;

        FAHProp :TWndPropAH;
        end;


//procedure Trace ( aLog: string );
function GetWindowClassName ( aWnd: THandle ): string;
function GetWindowTextName ( aWnd: THandle ): string;
function GetWindowDescription ( aWnd: THandle ): string;
function GetCaptionHeight ( aWnd: THandle ): integer;
procedure GetWindowSize ( aWnd: THandle; var aWidth, aHeight: integer );
function GetWindowHeight ( aWnd: THandle ): integer;
function GetWindowWidth ( aWnd: THandle ): integer;
function ModalFormVisibled: boolean;
function GetCaptionProperties ( aWnd: THandle ): TCaptionButtons;
function GetBorderThickness ( aWnd: THandle ): integer;
function GetCaptionButtonRect ( aWnd: THandle; aWidth: integer; var aRect: TRect ): boolean;
procedure DrawButton ( aWndProp: PWndProp; aBitmap: TBitmap; aDrawButton: TDrawButton = dbUnknown );
function GetCaptionTextWidth ( aWnd: THandle ): integer;
function GetShortCaptionWindowWidth ( aWnd: THandle ): integer;
function CanDisplayButton ( aWndProp: PWndProp ): boolean;
function WndFromPoint: THandle;
procedure HighlightWindow ( aWnd: THandle; aHighlight: boolean );

procedure AHPropInit ( var aAHProp: TWndPropAH );
procedure StrToAHWndProp ( aString: string; var aAHProp: TWndPropAH );
function  AHWndPropToStr ( aAHProp: PWndPropAH ): string;

procedure Template_GetIDList ( aIDList: TStrings );
procedure Template_FreeIDList ( aIDList: TStrings );
procedure Template_GetItems ( const aID: string; aIDList, aItems: TStrings; aAHProp: PWndPropAH = NIL );
procedure Template_FreeItems ( aItems: TStrings );

function RegReadIntegerDef ( aRegistry: TRegistry; const aKey: string; aDefault: integer ): integer;
function RegReadBoolDef ( aRegistry: TRegistry; const aKey: string; aDefault: boolean ): boolean;

// -----------------------------------------------------------------------------


implementation

uses
    Math, Unit_Constants;


procedure Trace ( aLog: string );
var
    aFile: TextFile;
    aExisted: boolean;
begin
aExisted := FileExists ( 'c:\autohidelog.txt' );

AssignFile ( aFile, 'c:\autohidelog.txt' );
if ( aExisted ) then
    Append ( aFile )
else
    Rewrite ( aFile );

try
    aLog := DateTimeToStr( Now ) + ': ' + aLog;
    writeln ( aFile, aLog );

finally
    CloseFile ( aFile );
    end;
end;


function GetWindowClassName ( aWnd: THandle ): string;
begin
if ( aWnd = 0 ) then
    result := ''
else begin
    SetLength ( result, 512 );
    GetClassName ( aWnd, PChar(result), 511 );
    SetLength ( result, StrLen(PChar(result)) );
    end; // else
end;


function GetWindowTextName ( aWnd: THandle ): string;
begin
if ( aWnd = 0 ) then
    result := ''
else begin
    SetLength ( result, 512 );
    GetWindowText ( aWnd, PChar(result), 511 );
    SetLength ( result, StrLen(PChar(result)) );

    if ( length(result) = 0 ) then
        Trace ( 'GetWindowTextName: GetWindowText failed!'
                + ' (0x' + inttohex( DWORD(aWnd), 8 ) + ')'
                + ' GetLastError=' + inttostr ( GetLastError ) );
    end; // else
end;


function GetWindowDescription ( aWnd: THandle ): string;
var
    aParent: THandle;
begin
result := '0x' + inttohex ( aWnd, 8 )
            + ':' + GetWindowTextName ( aWnd )
            + ':' + GetWindowClassName ( aWnd );

aParent := GetParent ( aWnd );
if ( aParent <> 0 ) then
    result := result + '(0x' + inttohex ( aWnd, 8 )
            + ':' + GetWindowTextName ( aWnd )
            + ':' + GetWindowClassName ( aWnd ) + ')';
end;



function GetCaptionHeight ( aWnd: THandle ): integer;
var
    aExStyle, aStyle: DWORD;
    aFrameThick: integer;
    aCaptionHeight: integer;
begin
aStyle := GetWindowLong ( aWnd, GWL_STYLE );
aExStyle := GetWindowLong ( aWnd, GWL_EXSTYLE );

if ( aStyle and WS_THICKFRAME <> 0  ) then
    aFrameThick := GetSystemMetrics ( SM_CYSIZEFRAME )
else
if ( aStyle and (WS_BORDER or WS_DLGFRAME) <> 0  ) then
    aFrameThick := GetSystemMetrics ( SM_CYFIXEDFRAME )
else
    aFrameThick := 0;

if ( aExStyle and WS_EX_TOOLWINDOW <> 0 ) then
    aCaptionHeight := GetSystemMetrics ( SM_CYSMCAPTION )
else
if ( aStyle and WS_CAPTION <> 0 ) then
    aCaptionHeight := GetSystemMetrics ( SM_CYCAPTION )
else
    aCaptionHeight := 0;

result := Max ( (aFrameThick*2) + aCaptionHeight, MIN_HEIGHT );
end;



procedure GetWindowSize ( aWnd: THandle; var aWidth, aHeight: integer );
var
    aRect: TRect;
begin
GetWindowRect ( aWnd, aRect );
aWidth := aRect.Right - aRect.Left;
aHeight := aRect.Bottom - aRect.Top;
end;


function GetWindowHeight ( aWnd: THandle ): integer;
var
    aWidth: integer;
begin
GetWindowSize ( aWnd, aWidth, Result );
end;


function GetWindowWidth ( aWnd: THandle ): integer;
var
    aHeight: integer;
begin
GetWindowSize ( aWnd, Result, aHeight );
end;


function ModalFormVisibled: boolean;
var
    //count, counter: integer;
    //aWnd: THandle;
    aControl: TWinControl;
    aForm: TForm;
begin
result := FALSE;

aControl := FindControl ( GetActiveWindow );
if ( aControl <> NIL ) and ( aControl is TForm ) then begin
    aForm := TForm ( aControl );
    if ( fsModal in aForm.FormState ) or ( aForm.ClassName = 'TMessageForm' ) then
        result := TRUE;
    end;

{
count := FApplication.ComponentCount - 1;
for counter := count downto 0 do begin
    if ( Application.Components[counter] is TForm ) then begin
        aForm := TForm ( Application.Components[counter] );
        if ( fsModal in aForm.FormState )
            or ( aForm.ClassName = 'TMessageForm' ) then begin
            result := TRUE;
            exit;
            end; // if
        end; // if
    end; // for
}
end;



function GetCaptionButtonRect ( aWnd: THandle; aWidth: integer; var aRect: TRect ): boolean;
var
   aButtons: TCaptionButtons;
   aButtonCount, aBorder: integer;
   aSysMetricsX, aSysMetricsY: integer;
begin
aButtons := GetCaptionProperties ( aWnd );

if ( cbCaption in aButtons ) then begin
    if ( cbSmallCaption in aButtons ) then begin
        aSysMetricsX := GetSystemMetrics ( SM_CXSMSIZE );
        aSysMetricsY := GetSystemMetrics ( SM_CYSMSIZE );
        end
    else begin
        aSysMetricsX := GetSystemMetrics ( SM_CXSIZE );
        aSysMetricsY := GetSystemMetrics ( SM_CYSIZE );
        end;

    aButtonCount := 1;
    if ( cbClose in aButtons ) then inc ( aButtonCount );
    if ( cbSize in aButtons ) then inc ( aButtonCount, 2 );
    if ( cbContextHelp in aButtons ) then inc ( aButtonCount );

    aBorder := GetBorderThickness ( aWnd );

    aRect.Left := aWidth - aBorder - aButtonCount*aSysMetricsX - CAP_BUTTON_SEPARATOR*2;
    aRect.Right := aRect.Left + aSysMetricsX + CAP_BUTTON_SEPARATOR*2; // - 4;
    aRect.Top := aBorder;
    aRect.Bottom := aBorder + aSysMetricsY;

    result := TRUE;
    end
else begin
    SetRect ( aRect, 0, 0, 0, 0 );
    result := FALSE;
    end;
end;


procedure DrawButton ( aWndProp: PWndProp; aBitmap: TBitmap; aDrawButton: TDrawButton );
const
    CCaptionColor: array [boolean] of DWORD = (COLOR_GRADIENTINACTIVECAPTION, COLOR_GRADIENTACTIVECAPTION);
    CCaptionColor2: array [boolean] of DWORD = (COLOR_INACTIVECAPTION, COLOR_ACTIVECAPTION);
var
   aDC: HDC;
   aCanvas: TCanvas;
   aRect: TRect;
   aColor: TColor;
   aActivated: boolean;
   aGradient: longword;
begin
if ( IsRectEmpty ( aWndProp^.FButtonRect ) ) or ( aBitmap = NIL ) then exit;


if ( aDrawButton = dbUnknown ) then
    aActivated := (aWndProp^.FWnd = GetForegroundWindow)
else
    aActivated := (aDrawButton = dbActivated);


aDC := GetWindowDC ( aWndProp^.FWnd );
try
    aCanvas := TCanvas.Create;
    try
        aCanvas.Handle := aDC;

        aGradient := 0;
        SystemParametersInfo ( SPI_GETGRADIENTCAPTIONS, 0, @aGradient, 0 );

        if ( aGradient <> 0 ) then
            aColor := GetSysColor ( CCaptionColor[aActivated] )
        else
            aColor := GetSysColor ( CCaptionColor2[aActivated] );


        move ( aWndProp^.FButtonRect, aRect, sizeof(aRect) );
        aCanvas.Brush.Color := aColor;
        aCanvas.FillRect ( aRect );

        InflateRect ( aRect, -1-CAP_BUTTON_SEPARATOR, -1 );
        aCanvas.BrushCopy ( aRect, aBitmap, Rect(0,0,32,32), clBlue );

        InflateRect ( aRect, 1, 1 );
        aCanvas.Pen.Color := aColor;
        aCanvas.Brush.Style := bsClear;
        aCanvas.Ellipse ( aRect );
        aCanvas.Brush.Style := bsSolid;

    finally
        aCanvas.Handle := 0;
        aCanvas.Free;
        end;

finally
    ReleaseDC ( aWndProp^.FWnd, aDC );
    end;
end;



function GetCaptionProperties ( aWnd: THandle ): TCaptionButtons;
var
    aStyle, aExStyle: DWORD;
begin
aStyle := GetWindowLong ( aWnd, GWL_STYLE );
aExStyle := GetWindowLong ( aWnd, GWL_EXSTYLE );

if ( aExStyle and WS_EX_TOOLWINDOW = WS_EX_TOOLWINDOW ) then begin
    result := [cbCaption, cbSmallCaption];
    if ( aStyle and WS_SYSMENU <> 0 ) then
        include ( result, cbClose );
    end
else
if ( aStyle and WS_POPUPWINDOW = WS_POPUPWINDOW ) then begin
    result := [cbCaption, cbClose];
    if ( aExStyle and WS_EX_CONTEXTHELP <> 0 ) then
        include ( result, cbContextHelp );
    end
else
if ( aStyle and WS_OVERLAPPEDWINDOW = WS_OVERLAPPEDWINDOW )
    or ( aStyle and WS_TILEDWINDOW = WS_TILEDWINDOW ) then begin
    result := [cbCaption, cbSystem, cbSize, cbMinimize, cbMaximize, cbClose];
    end
else
if ( aStyle and WS_CAPTION = WS_CAPTION ) then begin
    result := [cbCaption];
    if ( aStyle and WS_SYSMENU <> 0 ) then begin
        result := result + [cbSystem, cbClose];

        if ( aStyle and (WS_MINIMIZEBOX or WS_MAXIMIZEBOX) <> 0 ) then begin
            include ( result, cbSize );

            if ( aStyle and WS_MINIMIZEBOX <> 0 ) then
                include ( result, cbMinimize );
            if ( aStyle and WS_MAXIMIZEBOX <> 0 ) then
                include ( result, cbMaximize );
            end
        else
        if ( aExStyle and WS_EX_CONTEXTHELP <> 0 ) then
            include ( result, cbContextHelp )

        end; // if
    end // if
else
    result := [];
end;



function GetBorderThickness ( aWnd: THandle ): integer;
var
    aStyle: DWORD;
begin
aStyle := GetWindowLong ( aWnd, GWL_STYLE );

if ( aStyle and WS_THICKFRAME <> 0  ) then
    result := GetSystemMetrics ( SM_CYSIZEFRAME )
else
if ( aStyle and (WS_BORDER or WS_DLGFRAME) <> 0  ) then
    result := GetSystemMetrics ( SM_CYFIXEDFRAME )
else
    result := 0;
end;



function GetCaptionTextWidth ( aWnd: THandle ): integer;
var
    aNCMetrics: NONCLIENTMETRICS;
    aCaption: string;
    aExStyle: DWORD;
    aLogFont: ^LOGFONT;
    aFont, aOldFont: HFONT;
    aDC: HDC;
    aSize: SIZE;
begin
aExStyle := GetWindowLong ( aWnd, GWL_EXSTYLE );
aNCMetrics.cbSize := sizeof ( aNCMetrics );
SystemParametersInfo ( SPI_GETNONCLIENTMETRICS, aNCMetrics.cbSize,
        @aNCMetrics, 0 );

if ( aExStyle and WS_EX_TOOLWINDOW = WS_EX_TOOLWINDOW ) then
    aLogFont := @aNCMetrics.lfSmCaptionFont
else
    aLogFont := @aNCMetrics.lfCaptionFont;

aCaption := GetWindowTextName ( aWnd );

aFont := CreateFontIndirect ( aLogFont^ );
try
    aDC := GetDC ( aWnd );
    try
        aOldFont := SelectObject ( aDC, aFont );
        GetTextExtentPoint32 ( aDC, PChar(aCaption), Length(aCaption), aSize );
        SelectObject ( aDC, aOldFont );
    finally
        ReleaseDC ( aWnd, aDC );
        end;
finally
    DeleteObject ( aFont );
    end;

result := aSize.cx;
end;



function GetShortCaptionWindowWidth ( aWnd: THandle ): integer;
var
    aButtons: TCaptionButtons;
    aBorder: integer;
    aButtonWidth: integer;
begin
aButtons := GetCaptionProperties ( aWnd );
if not ( cbCaption in aButtons ) then begin
    result := 0;
    exit;
    end;

aBorder := GetBorderThickness ( aWnd );
if ( cbSmallCaption in aButtons ) then
    aButtonWidth := GetSystemMetrics ( SM_CXSMSIZE )
else
    aButtonWidth := GetSystemMetrics ( SM_CXSIZE );

result := aButtonWidth + (CAP_BUTTON_SEPARATOR * 2);  // autohide button and extra space

if ( cbSystem in aButtons ) then
    inc ( result, aButtonWidth );

if ( cbContextHelp in aButtons ) then
    inc ( result, aButtonWidth );

if ( cbSize in aButtons ) then
    inc ( result, aButtonWidth*2 );

if ( cbClose in aButtons ) then
    inc ( result, aButtonWidth );

result := result + (aBorder*2) + GetCaptionTextWidth ( aWnd );
end;


function CanDisplayButton ( aWndProp: PWndProp ): boolean;
var
    aBorder: integer;
    aIcon: integer;
    aButtons: TCaptionButtons;
begin
result := FALSE;

aButtons := GetCaptionProperties ( aWndProp^.FWnd );
if ( cbCaption in aButtons ) then begin
    aBorder := GetBorderThickness ( aWndProp^.FWnd );

    if ( cbSystem in aButtons ) then begin
        if ( cbSmallCaption in aButtons ) then
            aIcon := GetSystemMetrics ( SM_CXSMSIZE )
        else
            aIcon := GetSystemMetrics ( SM_CXSIZE );
        end
    else
        aIcon := 0;

    result := aWndProp^.FButtonRect.Left >= (aBorder+aIcon+2);
    end;
end;



function WndFromPoint: THandle;
var
    aParent: THandle;
    aStyle: DWORD;
    aPoint: TPoint;
begin
GetCursorPos ( aPoint );
result := WindowFromPoint ( aPoint );

while ( result <> 0 ) do begin
    aStyle := GetWindowLong ( result, GWL_STYLE );
    if ( aStyle and WS_CHILD <> WS_CHILD ) then
       break;

    aParent := GetParent ( result );
    if ( aParent = 0 ) then break;
    result := aParent;
    end; // while

{
aParent := GetParent ( result );
while ( aParent <> 0 ) do begin
    if ( aParent = aAppHandle ) then
       break;

    result := aParent;
    aParent := GetParent ( result );
    end; // while
}
end;



procedure HighlightWindow ( aWnd: THandle; aHighlight: boolean );
var
    aDC: HDC;
    aCanvas: TCanvas;
    aRect: TRect;
begin
if ( aHighlight ) then begin
    aDC := GetWindowDC ( aWnd );
    try
        aCanvas := TCanvas.Create;
        try
            aCanvas.Handle := aDC;
            aCanvas.Pen.Width := 6;
            aCanvas.Pen.Color := clYellow;
            aCanvas.Pen.Style := psSolid;
            aCanvas.Pen.Mode  := pmXor;

            GetWindowRect ( aWnd, aRect );
            OffsetRect ( aRect, -aRect.Left, -aRect.Top );

            aCanvas.MoveTo ( 0, 0 );
            aCanvas.LineTo ( aRect.Right, 0 );
            aCanvas.LineTo ( aRect.Right, aRect.Bottom );
            aCanvas.LineTo ( 0, aRect.Bottom );
            aCanvas.LineTo ( 0, 0 );

        finally
            aCanvas.Handle := 0;
            aCanvas.Free;
            end;
    finally
        ReleaseDC ( aWnd, aDC );
        end;
    end
else
    RedrawWindow ( aWnd, NIL, 0, RDW_FRAME or RDW_INVALIDATE or RDW_UPDATENOW or RDW_ALLCHILDREN );

end;


procedure AHPropInit ( var aAHProp: TWndPropAH );
begin
aAHProp.FShrinkType := stCaption;
aAHProp.FLockType := ltUnlock;
aAHProp.FCaptionOnShrink := saNone;
aAHProp.FUTWeight := ULTRATHIN_DEFWEIGHT;
aAHProp.FDirection := sdirTop;
aAHProp.FUTOnShrink := saTopMost;
end;


procedure StrToAHWndProp ( aString: string; var aAHProp: TWndPropAH );
var
    aLength: integer;
    aCor: integer;
    aValue: string;
begin
AHPropInit ( aAHProp );

// removes the front braket
aCor := Pos ( SIGNATURE_HEAD, aString );
if ( aCor = 0 ) then exit;
delete ( aString, 1, aCor );

// removes the tail braket
aCor := Pos ( SIGNATURE_TAIL, aString );
if ( aCor = 0 ) then exit;
delete ( aString, aCor, MAXINT );

aLength := Length ( aString );

with aAHProp do begin
    while ( aLength > 0 ) do begin
        case ( aString[1] ) of
           CHAR_LT_UNLOCK:      FLockType := ltUnlock;
           CHAR_LT_EXPAND:      FLockType := ltLockExpand;
           CHAR_LT_SHRINK:      FLockType := ltLockShrink;

           CHAR_ST_CAPTION:     FShrinkType := stCaption;
           CHAR_ST_SHORTCAPTION:FShrinkType := stShortCaption;
           CHAR_ST_ULTRATHIN:   FShrinkType := stUltraThin;

           CHAR_SA_NONE:        FCaptionOnShrink := saNone;
           CHAR_SA_TOP:         FCaptionOnShrink := saTop;
           CHAR_SA_TOPMOST:     FCaptionOnShrink := saTopMost;

           CHAR_UTD_TOP:        FDirection := sdirTop;
           CHAR_UTD_LEFT:       FDirection := sdirLeft;
           CHAR_UTD_BOTTOM:     FDirection := sdirBottom;
           CHAR_UTD_RIGHT:      FDirection := sdirRight;

           CHAR_UTSA_NONE:      FUTOnShrink := saNone;
           CHAR_UTSA_TOP:       FUTOnShrink := saTop;
           CHAR_UTSA_TOPMOST:   FUTOnShrink := saTopMost;

           '0'..'9': begin
              aCor := 1;
              while ( aCor < aLength ) and ( aString[aCor+1] in ['0'..'9'] ) do
                  inc ( aCor );
              try
                  aValue := copy ( aString, 1, aCor );
                  delete ( aString, 1, aCor );
                  dec ( aLength, aCor );
                  FUTWeight := Max ( 1, strtoint ( aValue ) );
              except
                  {do nothing!}
                  end;
              continue;
              end;
            end; // case

        delete ( aString, 1, 1 );
        dec ( aLength );
        end; // while
    end; // with

if ( aAHProp.FShrinkType = stUltraThin ) and ( aAHProp.FLockType = ltLockShrink ) then
    aAHProp.FLockType := ltUnlock;
end;



function AHWndPropToStr ( aAHProp: PWndPropAH ): string;
const
    CLockType: array [TLockType] of char = ( CHAR_LT_UNLOCK, CHAR_LT_EXPAND, CHAR_LT_SHRINK );
    CShrinkTypes: array [TShrinkType] of char = ( CHAR_ST_CAPTION, CHAR_ST_SHORTCAPTION,
                                                  CHAR_ST_ULTRATHIN, CHAR_IGNORE );
    CDirection: array [TShrinkDirection] of char = ( CHAR_UTD_TOP, CHAR_UTD_LEFT,
                                        CHAR_UTD_BOTTOM, CHAR_UTD_RIGHT, CHAR_IGNORE );
    COnShrink: array [TShrinkAction] of char = ( CHAR_SA_NONE, CHAR_SA_TOP, CHAR_SA_TOPMOST );
    COnUTShrink: array [TShrinkAction] of char = ( CHAR_UTSA_NONE, CHAR_UTSA_TOP, CHAR_UTSA_TOPMOST );
begin
if ( aAHProp = NIL ) then begin
    result := '';
    exit;
    end;

SetLength ( result, 6 );
result[1] := SIGNATURE_HEAD;
result[2] := CLockType[aAHProp^.FLockType];
result[3] := CShrinkTypes[aAHProp^.FShrinkType];
result[4] := CDirection[aAHProp^.FDirection];
result[5] := COnShrink[aAHProp^.FCaptionOnShrink];
result[6] := COnUTShrink[aAHProp^.FUTOnShrink];
        
result := result + inttostr ( Max ( ULTRATHIN_MINWEIGHT, aAHProp^.FUTWeight ) ) + SIGNATURE_TAIL;
end;

                             

procedure Template_FreeIDList ( aIDList: TStrings );
var
    count, counter: integer;
    aAHProp: PWndPropAH;
begin
count := aIDList.Count - 1;
for counter := count downto 0 do begin
    aAHProp := PWndPropAH( aIDList.Objects[counter] );
    if ( aAHProp <> NIL ) then
        dispose ( aAHProp );
    end;

aIDList.Clear;
end;


procedure Template_GetIDList ( aIDList: TStrings );
var
    aBuffer: string;
    count, counter: integer;
    aLength, aCor: integer;
    aAHProp: PWndPropAH;
begin
Template_FreeIDList ( aIDList );

SetLength ( aBuffer, 255 );
LoadString ( HInstance, RES_TEMPLATE_COUNT, PChar(aBuffer), 255 );
SetLength ( aBuffer, strlen(Pchar(aBuffer)) );

try
    count := strtoint ( aBuffer ) - 1;
    for counter := 0 to count do begin
        SetLength ( aBuffer, 255 );
        LoadString ( HInstance, RES_TEMPLATE_BASE + counter, PChar(aBuffer), 255 );
        aLength := StrLen( PChar(aBuffer) );

        aCor := pos ( SIGNATURE_HEAD, aBuffer );
        if ( aLength = 0 ) or ( aCor = 1 ) then continue;

        SetLength ( aBuffer, aLength );

        New ( aAHProp );
        StrToAHWndProp ( aBuffer, aAHProp^ );

        if ( aCor <> 0 ) then
            Delete ( aBuffer, aCor, MAXINT );

        aIDList.AddObject ( aBuffer, TObject(aAHProp) );
        end; // for
except
    end;
end;



procedure Template_GetItems ( const aID: string; aIDList, aItems: TStrings; aAHProp: PWndPropAH );
var
    aIndex: integer;
    aBuffer: string;
    aAHPropS: TWndPropAH;
    aAHPropD: PWndPropAH;
    aStrings: TStringList;
    count, counter: integer;
begin
aIndex := aIDList.IndexOf ( aID );
if ( aIndex = -1 ) then exit;

if ( aAHProp = NIL ) then
   aAHProp := PWndPropAH ( aIDList.Objects[aIndex] );

if ( aAHProp = NIL ) then
    AHPropInit ( aAHPropS )
else
    Move ( aAHProp^, aAHPropS, sizeof(aAHPropS) );


SetLength ( aBuffer, 512 );
LoadString ( HInstance, RES_TEMPLATE_ITEM_BASE + aIndex, PChar(aBuffer), 511 );
SetLength ( aBuffer, StrLen( PChar(aBuffer) ) );

aStrings := TStringList.Create;
try
    aStrings.CommaText := aBuffer;
    count := aStrings.count - 1;
    for counter := 0 to count do begin
        aIndex := aItems.IndexOf ( aStrings[counter] );
        if ( aIndex = -1 ) then begin
            new ( aAHPropD );
            Move ( aAHPropS, aAHPropD^, sizeof(aAHPropD^) );
            aItems.AddObject ( aStrings[counter], TObject(aAHPropD) );
            end
        else begin
            aAHPropD := PWndPropAH ( aItems.Objects[aIndex] );
            if ( aAHPropD = NIL ) then
               new ( aAHPropD );
            Move ( aAHPropS, aAHPropD^, sizeof(aAHPropD^) );
            aItems.Objects[aIndex] := TObject(aAHPropD);
            end;
        end; // for
finally
    aStrings.Free;
    end;
end;


procedure Template_FreeItems ( aItems: TStrings );
begin
Template_FreeIDList ( aItems );
end;



function RegReadIntegerDef ( aRegistry: TRegistry; const aKey: string; aDefault: integer ): integer;
begin
try
    result := aRegistry.ReadInteger ( aKey );
except
    result := aDefault;
    end;
end;


function RegReadBoolDef ( aRegistry: TRegistry; const aKey: string; aDefault: boolean ): boolean;
begin
try
    result := aRegistry.ReadBool ( aKey );
except
    result := aDefault;
    end;
end;

end.
