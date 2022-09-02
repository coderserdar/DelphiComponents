(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TurboPower B-Tree Filer
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1986-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Based in part on code written by Ralf Nagel
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

{$C MOVEABLE,DEMANDLOAD,DISCARDABLE}

{--Conditional defines and compiler options that affect this unit}
  {$I BRDefOpt.Inc}
  {$X+,F-,V-,B-,S-,I-,R-}
  {$IFDEF CanSetOvrflowCheck}
    {$Q-}
  {$ENDIF}


{$IFDEF Win32}
  !! Error - this unit cannot be compiled for 32-bit
{$ENDIF}

Unit WBrowser;

Interface

Uses
{$IFDEF Ver80}                                                  {!!.51}
  Messages,                                                     {!!.51}
{$ENDIF}                                                        {!!.51}
  WinTypes,
  WinProcs,
{$IFDEF Ver10}
  WObjects,
{$ELSE}
 {$IFDEF Ver15}
  WObjects,
 {$ELSE}
  Objects,
  OWindows,
  ODialogs,
 {$ENDIF}
{$ENDIF}
  Strings,
  LowBrows,
  MedBrows,
  HiBrows;

Const
{$IFDEF BRUseShell}
  HardError        = OpSBase.HardError;
  ProgrammingError = OpSBase.ProgrammingError;
{$ENDIF}
{$IFDEF BRUseIsam}
  HardError        = 4;
  ProgrammingError = 5;
{$ENDIF}


Type
  PBrowserWindow = ^TBrowserWindow;

  PLowWinBrowser = ^LowWinBrowser;
  LowWinBrowser = Object ( BRHBrowser )
    Owner  : PBrowserWindow;
    OnHeap : Boolean;

    Constructor Init (     ParOnHeap           : Boolean;
                           ADrvOrFileBlockPtr  : Pointer;
                           ANumberOfEltsPerRow : Word;
                           ANumberOfRows       : Word;
                           AKeyNr              : Word;
                           ALKey,
                           AHKey               : GenKeyStr;
                       Var ADatS;
                           AIsVarRec           : Boolean );

    Destructor Done; Virtual;

    Function BuildRow ( Var RR : RowRec ) : Integer; Virtual;
    Function PerformFilter ( Var RR    : RowRec;
                             Var UseIt : Boolean ) : Integer; Virtual;

    Procedure ShowErrorOccured ( EClass : Integer ); Virtual;     {!!.51}
    Function PreCompletePage : Integer; Virtual;
    Function PostCompletePage : Integer; Virtual;
  End;

  TabTable    = Array [1..$7F00] Of Integer;
  TabTablePtr = ^TabTable;

  FontInfo = Record                                               {!!.03}
    Font          : THandle;
    ChHeightExtra,
    ChHeight,
    ChWidth,
    ChRefWidth    : Word;                                         {!!.04}
    FixedPitch    : Boolean;
  End;

  TBrowserWindow = Object (TWindow)
    DoTheFilter,
    FilterInProgress : Boolean;                                   {!!.03}
    BrowserPtr       : PLowWinBrowser;

    FontDescr        : FontInfo;                                  {!!.03b}
    HorizOfs         : Integer;                                   {!!.03b}
    Width,                                                        {!!.03b}
      {-Set correct even before OWL gets wm_Size}
    FullPage,                                                     {!!.03b}
    FirstRow,                                                     {!!.03b}
    MaxHorizOfs      : Word;                                      {!!.03b}

    TextMargin       : TRect;                                     {!!.03b}

    PaintRaster      : Boolean;                                   {!!.05}

  {--Internal use only fields}
    lwBackGrErase : Boolean;                                      {!!.03b}
    lwNoPaintHFCg : Boolean;                                      {!!.05}


    Constructor Init ( AParent : PWindowsObject;
                       ATitle  : PChar );

    Destructor Done; Virtual;

    Function ConnectLowBrowser ( ABrowserPtr : PLowWinBrowser;
                                 AHeader,
                                 AFooter     : BRLRowEltString ) : Boolean;

  {--The following functions must be overwritten in descending objects}
    Function BuildRow ( Var RR : RowRec ) : Integer; Virtual;

  {--The following routines may be overwritten in descending objects}
    Function PerformFilter ( Var RR    : RowRec;
                             Var UseIt : Boolean ) : Integer; Virtual;
    Procedure JustFiltered ( Rejected : Boolean ); Virtual;       {!!.03}
    Procedure EndFilter; Virtual;                                 {!!.03}

    Procedure ShowErrorOccured ( EClass : Integer ); Virtual;     {!!.51}
    Procedure ShowFilterWorking ( CallState : Integer;
                                  Rejected  : Boolean ); Virtual;
    Function PreCompletePage : Integer; Virtual;
    Function PostCompletePage : Integer; Virtual;

    Function HandleChar ( Var Msg : TMessage ) : Boolean; Virtual;

    Procedure SetTheFont; Virtual;
    Procedure DeleteTheFont; Virtual;
    Procedure SetCharValues; Virtual;                             {!!.03b}
    Procedure SetMargins; Virtual;                                {!!.03b}

    Function AdjustHorizOfs ( Delta : Integer) : Integer; Virtual;{!!.03b}
    Function CalcMaxWidth : Integer; Virtual;                     {!!.03b}
    Procedure LineDown; Virtual;                                  {!!.03b}
    Procedure LineUp; Virtual;                                    {!!.03b}
    Procedure PageDown; Virtual;                                  {!!.03b}
    Procedure PageUp; Virtual;                                    {!!.03b}
    Procedure FirstPage; Virtual;                                 {!!.03b}
    Procedure LastPage; Virtual;                                  {!!.03b}
    Procedure MoveToRelPos ( Pos : Word ); Virtual;               {!!.03b}
    Procedure LineRight; Virtual;                                 {!!.03b}
    Procedure LineLeft; Virtual;                                  {!!.03b}
    Procedure PageRight; Virtual;                                 {!!.03b}
    Procedure PageLeft; Virtual;                                  {!!.03b}
    Procedure LeftHome; Virtual;                                  {!!.03b}
    Procedure RightHome; Virtual;                                 {!!.03b}
    Procedure MoveToHorizPos ( Pos : Word ); Virtual;             {!!.03b}

    Procedure GetBrowserTextRect ( Var TextRect : TRect ); Virtual;
                                                                  {!!.03b}
    Procedure GetRowAreaRect ( Var Rect : TRect ); Virtual;       {!!.03b}
    Function GetTextOutPosY ( LineNr : Word ) : Integer; Virtual; {!!.03b}
    Function GetLineNrFromY ( Y : Integer ) : Word; Virtual;      {!!.03b}
    Function XYPosInRect ( X, Y : Integer; Rect : TRect ) : Boolean;
                                                                  {!!.03b}
    Function TotalSpaceForLines ( Rect : TRect ) : Word; Virtual; {!!.03b}

    Procedure FillClientGaps ( DC : HDC ); Virtual;                {!!.05}
    Procedure DisplayRow ( I       : Integer;
                           DC      : HDC;
                           Inverse : Boolean ); Virtual;

    Procedure WriteHeader ( Var HeaderLine : BRLRowEltString;     {!!.03b}
                                DC         : HDC ); Virtual;
    Procedure WriteDataLine ( Var DataLine : BRLRowEltString;     {!!.03b}
                                  LineNr   : Word;
                                  DC       : HDC;
                                  Inverse  : Boolean ); Virtual;
    Procedure WriteFooter ( Var FooterLine : BRLRowEltString;     {!!.03b}
                                DC         : HDC ); Virtual;
    Procedure DrawSeparator ( Pos   : Word;                       {!!.03b}
                              Color : TColorRef;
                              DC    : HDC ); Virtual;

    Function WriteStringOut ( Var S      : String;                {!!.03b}
                                  LineNr : Word;
                                  DC     : HDC;
                                  XOfs   : Integer ) : Word; Virtual;

    Procedure GetHeaderFooterColor                                {!!.03b}
        ( Var Color, BkColor : TColorRef; ForHeader : Boolean ); Virtual;
    Procedure GetHighlightColor                                   {!!.03b}
        ( Var Color, BkColor : TColorRef ); Virtual;
    Procedure GetNormalColor                                      {!!.03b}
        ( Var Color, BkColor : TColorRef ); Virtual;
    Function UseSeparator                                         {!!.03b}
        ( Var Color : TColorRef ) : Boolean; Virtual;

    Procedure Paint (     PaintDC     : HDC;
                      Var PaintStruct : TPaintStruct ); Virtual;

    Function CanCallLowBrowser : Boolean; Virtual;
    Procedure SetupWindow; Virtual;

    Procedure FirstUserInit; Virtual;

  {--The following routines may be called from outside}
    Procedure SetHeaderFooter ( AHeader,
                                AFooter : BRLRowEltString );
    Procedure SetLowHighKey ( ALowKey,
                              AHighKey : GenKeyStr );
    Procedure GetLowHighKey ( Var ALowKey,                        {!!.03}
                                  AHighKey : GenKeyStr );

    Procedure SetAndUpdateBrowserScreen ( NewKeyStr : GenKeyStr;
                                          NewRef    : LongInt );
    Procedure UpdateBrowserScreen;
    Procedure ReInitUpdateBrowserScreen;                          {!!.03b}

    Function GetCurrentRec ( Var Match : Boolean ) : Integer;     {!!.03}
    Function GetCurrentKeyNr : Word;
    Function GetCurrentKeyStr : String;
    Function GetCurrentDatRef : LongInt;
    Function GetCurNrOfLines : Word;                              {!!.03}
    Procedure SetKeyNr ( Value : Word );

    Function SetSuppressTimer ( DoSuppr : Boolean ) : Boolean;
    Function GetSuppressTimer : Boolean;

    Procedure EnableFilter ( FilterOn : Boolean ); Virtual;       {!!.03}{!!.51}
    Function FilterIsOn ( Var InProgress : Boolean ) : Boolean;   {!!.03}

    Procedure SetHeaderNoUpdate ( AHeader : BRLRowEltString ); Virtual;
                                                                  {!!.03b}
    Procedure SetFooterNoUpdate ( AFooter : BRLRowEltString ); Virtual;
                                                                  {!!.03b}
    Function GetHeader : BRLRowEltString;                         {!!.03b}
    Function GetFooter : BRLRowEltString;                         {!!.03b}

    Function TotalCharHeight : Word;                              {!!.03b}

    Procedure PosClientCorruption;                                {!!.04}
    Procedure MarkClientCorruptablePhase;                         {!!.04}
    Function ClientPosCorrupted : Boolean;                        {!!.04}

  {--The following routines may be called from overridden routines}
    Function GetThisRec ( Var RR : RowRec ) : Integer;
    Function BuildBrowScreenRow ( Var RR : RowRec ) : Integer;

  {--The following routines should not be overridden in descending objects}
    Procedure InitFields; Virtual;
    Function GetClassName : PChar; Virtual;
    Procedure Reinit; Virtual;                                    {!!.03b}
    Procedure GetWindowClass ( Var WndClass : TWndClass ); Virtual;{!!.03}
    Procedure NewSize; Virtual;
    {!!.03b wmPaint removed}
    Procedure WMSize ( Var Msg : TMessage ); Virtual
        wm_First + wm_Size;
    Procedure WMLButtonDown ( Var Msg : TMessage); Virtual
        wm_First + wm_LButtonDown;
    Procedure WMKillFocus ( Var Msg : TMessage); Virtual          {!!.03b}
        wm_First + wm_KillFocus;
    {!!.03 wmSetFocus removed}
    Procedure WMNCMouseMove ( Var Msg : TMessage ); Virtual       {!!.03b}
        wm_First + wm_NCMouseMove;
    Procedure WMMouseMove ( Var Msg : TMessage ); Virtual
        wm_First + wm_MouseMove;
    Procedure WMTimer ( Var Msg : TMessage ); Virtual
        wm_First + wm_Timer;
    Procedure WMVScroll ( Var Msg : TMessage ); Virtual
        wm_First + wm_VScroll;
    Procedure WMHScroll ( Var Msg : TMessage ); Virtual
        wm_First + wm_HScroll;
    Procedure WMChar ( Var Msg : TMessage ); Virtual
        wm_First + wm_Char;
    Procedure WMKeyDown ( Var Msg : TMessage ); Virtual
        wm_First + wm_KeyDown;
    Procedure WMKeyUp ( Var Msg : TMessage ); Virtual
        wm_First + wm_KeyUp;

  {--Internal use only methods}
    Procedure InvalidateBrowserScreen;
    Procedure lwUpdateVertScrollBar;
    Procedure lwUpdateHorzScrollBar;

  Private
    lwVertScale,
    lwThumbVTrack,
    lwThumbHTrack         : Word;

    lwHeader,
    lwFooter              : BRLRowEltString;

    lwDoMouseMove,                                                {!!.03b}
    lwCtrlDown,
    lwShiftDown           : Boolean;

    lwSupprTimer          : Boolean;

    lwFInitDone           : Boolean;

    lwPosClientCorruption : Boolean;                              {!!.04}

  {--The following routines are internal use only}
    Procedure lwFirstInit;                                        {!!.03b}
    Procedure lwSetAndUpdateBrowserScreen ( NewKeyStr : GenKeyStr;
                                            NewRef    : LongInt );
    Procedure lwUpdateBrowserScreen;
    Procedure lwNewSize;                                          {!!.03b}
  End;


Implementation

  Const
    VertScrollScale = 63;

(************************************************************************)
  Constructor LowWinBrowser.Init
                           (     ParOnHeap           : Boolean;
                                 ADrvOrFileBlockPtr  : Pointer;
                                 ANumberOfEltsPerRow : Word;
                                 ANumberOfRows       : Word;
                                 AKeyNr              : Word;
                                 ALKey,
                                 AHKey               : GenKeyStr;
                             Var ADatS;
                                 AIsVarRec           : Boolean );

  Begin
    Owner := Nil;
    OnHeap := ParOnHeap;
    If Not BRHBrowser.Init ( ADrvOrFileBlockPtr, ANumberOfEltsPerRow,
        ANumberOfRows, ALKey, AHKey, False, ADatS, AIsVarRec ) Then Fail;
    KeyNr := AKeynr;
  End;


  Destructor LowWinBrowser.Done;

  Begin
    BRHBrowser.Done;
    If Owner <> Nil Then
      Owner^.BrowserPtr := Nil;
    Owner := Nil;
  End;


  Function LowWinBrowser.BuildRow ( Var RR  : RowRec ) : Integer;

  Begin
    BuildRow := Owner^.BuildRow ( RR );
  End;


  Function LowWinBrowser.PerformFilter ( Var RR    : RowRec;
                                         Var UseIt : Boolean ) : Integer;

  Var
    LResult : Integer;                                          {!!.51}
  Begin
    LResult := Owner^.PerformFilter ( RR, UseIt );              {!!.51}
    If ( LResult = NoError ) And Owner^.DoTheFilter Then Begin  {!!.51}
      Owner^.JustFiltered ( Not UseIt );
    End;
    PerformFilter := LResult;                                   {!!.51}
  End;


  Procedure LowWinBrowser.ShowErrorOccured ( EClass : Integer );  {!!.51}

  Begin
    Owner^.ShowErrorOccured ( EClass );                         {!!.51}
    Owner^.EndFilter;
  End;


  Function LowWinBrowser.PreCompletePage : Integer;

  Begin
    PreCompletePage := Owner^.PreCompletePage;
  End;


  Function LowWinBrowser.PostCompletePage : Integer;

  Begin
    PostCompletePage := Owner^.PostCompletePage;
    Owner^.EndFilter;
  End;


(************************************************************************)

  Procedure TBrowserWindow.InitFields;

  Begin
    Attr.Style := Attr.Style Or ws_VScroll Or ws_HScroll;

    lwBackGrErase := False;
    lwNoPaintHFCg := False;                                        {!!.05}
    lwFInitDone := False;
    SetHeaderNoUpdate ( '' );                                      {!!.03b}
    SetFooterNoUpdate ( '' );                                      {!!.03b}

    lwDoMouseMove := False;                                        {!!.03b}
    lwCtrlDown := False;
    lwShiftDown := False;

    HorizOfs := 0;
    lwVertScale := VertScrollScale;
    lwThumbVTrack := $FFFF;                                        {!!.03b}

    lwSupprTimer  := False;

    BrowserPtr := Nil;

    DoTheFilter := False;
    FilterInProgress := False;
    PaintRaster := False;

    lwPosClientCorruption := False;
  End;


  Constructor TBrowserWindow.Init ( AParent   : PWindowsObject;
                                    ATitle    : PChar );

  Begin
    If Not TWindow.Init ( AParent, ATitle ) Then Fail;
    InitFields;
  End;


  Destructor TBrowserWindow.Done;

  Begin
    If BrowserPtr <> Nil Then Begin
      If BrowserPtr^.OnHeap Then Begin
        Dispose ( BrowserPtr, Done )
      End Else Begin
        BrowserPtr^.Done;
      End;
    End;
    BrowserPtr := Nil;
    DeleteTheFont;
    TWindow.Done;
  End;


  Procedure TBrowserWindow.InvalidateBrowserScreen;

  Var
    R : TRect;

  Begin
    lwNoPaintHFCg := False;                                          {!!.05}
    If Not GetUpdateRect ( hWindow, R, False ) Then Begin
      InvalidateRect ( hWindow, Nil, lwBackGrErase );
    End;
  End;


  Procedure TBrowserWindow.DeleteTheFont;

  Begin
    If FontDescr.Font > 0 Then DeleteObject ( FontDescr.Font );
  End;


  Procedure TBrowserWindow.SetTheFont;

  Var
    LogFont : TLogFont;

  Begin
    With FontDescr Do Begin
      Font := GetStockObject ( SYSTEM_Fixed_FONT );
      If Font > 0 Then Begin
        GetObject ( Font, SizeOf (LogFont), @LogFont );
        With LogFont Do Begin
          lfPitchAndFamily := ff_Modern Or Fixed_Pitch;
          lfWeight := FW_Normal;
        End;
        Font := CreateFontIndirect ( LogFont );
      End;
      FixedPitch := True;
    End;
  End;


  Function TBrowserWindow.HandleChar ( Var Msg : TMessage ) : Boolean;

  Begin
    HandleChar := False;
  End;


  Function TBrowserWindow.BuildRow ( Var RR : RowRec ) : Integer;

  Begin
    RunError ( 211 );
  End;


  Function TBrowserWindow.PerformFilter ( Var RR    : RowRec;
                                          Var UseIt : Boolean ) : Integer;

  Begin
    UseIt := True;
    PerformFilter := NoError;
  End;


  Procedure TBrowserWindow.JustFiltered ( Rejected : Boolean );

  Begin
    If FilterInProgress Then Begin
      ShowFilterWorking ( 0, Rejected );
    End Else Begin
      FilterInProgress := True;
      ShowFilterWorking ( -1, Rejected );
    End;
  End;


  Procedure TBrowserWindow.EndFilter;

  Begin
    If FilterInProgress Then Begin
      FilterInProgress := False;
      ShowFilterWorking ( 1, False );
    End;
  End;


  Procedure TBrowserWindow.ShowErrorOccured ( EClass : Integer );    {!!.51}

  Begin
    MessageBeep ( 0 );
  End;


  Procedure TBrowserWindow.ShowFilterWorking ( CallState : Integer;
                                               Rejected  : Boolean );

  Begin
  End;


  Function TBrowserWindow.PreCompletePage : Integer;

  Begin
    PreCompletePage := NoError;
  End;


  Function TBrowserWindow.PostCompletePage : Integer;

  Begin
    PostCompletePage := NoError;
  End;


  Function TBrowserWindow.CanCallLowBrowser : Boolean;            {!!.03}

  Begin
    CanCallLowBrowser := (BrowserPtr <> Nil) And lwFInitDone
        And IsWindow ( HWindow ) And BrowserPtr^.BrowserCallAllowed
        And Not FilterInProgress;
  End;


  Procedure TBrowserWindow.FirstUserInit;

  Begin
  End;


  Procedure TBRowserWindow.SetupWindow;

  Begin
    TWindow.SetupWindow;
    If BrowserPtr <> Nil Then lwFirstInit;
  End;


  Procedure TBrowserWindow.SetHeaderFooter ( AHeader,
                                             AFooter : BRLRowEltString );

  Begin
    If Not CanCallLowBrowser Then Exit;
    SetHeaderNoUpdate ( AHeader );                                 {!!.03b}
    SetFooterNoUpdate ( AFooter );                                 {!!.03b}
    NewSize;                                                       {!!.03b}
    UpdateBrowserScreen;                                           {!!.03b}
  End;


  Procedure TBrowserWindow.SetLowHighKey ( ALowKey,
                                           AHighKey : GenKeyStr );

  Begin
    If Not CanCallLowBrowser Then Exit;
    BrowserPtr^.LowKey := ALowKey;
    BrowserPtr^.HighKey := AHighKey;
  End;


  Procedure TBrowserWindow.GetLowHighKey ( Var ALowKey,
                                               AHighKey : GenKeyStr );

  Begin
    ALowKey := '';
    AHighKey := '';
    If Not CanCallLowBrowser Then Exit;
    ALowKey := BrowserPtr^.LowKey;
    AHighKey := BrowserPtr^.HighKey;
  End;


  Procedure TBrowserWindow.SetAndUpdateBrowserScreen
                                               ( NewKeyStr : GenKeyStr;
                                                 NewRef    : LongInt );

  Begin
    If CanCallLowBrowser Then Begin
      lwSetAndUpdateBrowserScreen ( NewKeyStr, NewRef );
    End;
  End;


  Procedure TBrowserWindow.UpdateBrowserScreen;


  Begin
    If CanCallLowBrowser Then Begin
      lwUpdateBrowserScreen;
    End;
  End;


  Procedure TBrowserWindow.ReInitUpdateBrowserScreen;

  Begin
    If CanCallLowBrowser Then Begin
      InvalidateRect ( hWindow, Nil, True );                       {!!.03b}
      NewSize;                                                     {!!.03b}
      UpdateBrowserScreen;
    End;
  End;


  Function TBrowserWindow.GetThisRec ( Var RR : RowRec ) : Integer;

  Begin
    GetThisRec := ProgrammingError;
    If BrowserPtr = Nil Then Exit;
    GetThisRec := BrowserPtr^.BRGetRec ( RR, False, False );
  End;


  Function TBrowserWindow.BuildBrowScreenRow ( Var RR : RowRec ) : Integer;

  Begin
    BuildBrowScreenRow := ProgrammingError;
    If BrowserPtr = Nil Then Exit;
    BuildBrowScreenRow := BrowserPtr^.BuildBrowScreenRow ( RR );
  End;


  Function TBrowserWindow.GetCurrentRec ( Var Match : Boolean ) : Integer;

  Var
    LResult : Integer;                                          {!!.51}

  Begin
    Match := False;
    GetCurrentRec := ProgrammingError;
    If Not CanCallLowBrowser Then Exit;
    With BrowserPtr^ Do Begin
      GetCurrentRec := GetRowMatchingRec ( BSAPtr^[GetCurRow]^,
          True, True, Match );
    End;
  End;


  Function TBrowserWindow.GetCurrentKeyNr : Word;

  Begin
    GetCurrentKeyNr := $FFFF;                                      {!!.04}
    If BrowserPtr = Nil Then Exit;                                 {!!.03b}
    GetCurrentKeyNr := BrowserPtr^.KeyNr;
  End;


  Function TBrowserWindow.GetCurrentKeyStr : String;

  Begin
    GetCurrentKeyStr := '';
    If Not CanCallLowBrowser Then Exit;
    GetCurrentKeyStr := BrowserPtr^.GetCurrentKeyStr;
  End;


  Function TBrowserWindow.GetCurrentDatRef : LongInt;

  Begin
    GetCurrentDatRef := 0;
    If Not CanCallLowBrowser Then Exit;
    GetCurrentDatRef := BrowserPtr^.GetCurrentDatRef;
  End;


  Function TBrowserWindow.GetCurNrOfLines : Word;

  Begin
    GetCurNrOfLines := FullPage;
  End;


  Procedure TBrowserWindow.SetKeyNr ( Value : Word );

  Begin
    If CanCallLowBrowser Then Begin
      BrowserPtr^.KeyNr := Value;
    End;
  End;


  Function TBrowserWindow.SetSuppressTimer ( DoSuppr : Boolean ) : Boolean;

  Begin
    SetSuppressTimer := lwSupprTimer;
    lwSupprTimer := DoSuppr;
  End;


  Function TBrowserWindow.GetSuppressTimer : Boolean;

  Begin
    GetSuppressTimer := lwSupprTimer;
  End;


  Procedure TBrowserWindow.EnableFilter ( FilterOn : Boolean ); {!!.51}

  Begin
    DoTheFilter := FilterOn;                                    {!!.51}
    If FilterInProgress And Not FilterOn Then Begin             {!!.51}
      EndFilter;                                                   {!!.03b}
    End;
  End;


  Function TBrowserWindow.FilterIsOn ( Var InProgress : Boolean )
                                                             : Boolean;

  Begin
    FilterIsOn := DoTheFilter;
    InProgress := FilterInProgress;
  End;


  Procedure TBrowserWindow.SetHeaderNoUpdate ( AHeader : BRLRowEltString );

  Begin
    lwHeader := AHeader;
  End;


  Procedure TBrowserWindow.SetFooterNoUpdate ( AFooter : BRLRowEltString );

  Begin
    lwFooter := AFooter;
  End;


  Function TBrowserWindow.GetHeader : BRLRowEltString;

  Begin
    GetHeader := lwHeader;
  End;


  Function TBrowserWindow.GetFooter : BRLRowEltString;

  Begin
    GetFooter := lwFooter;
  End;


  Function TBrowserWindow.TotalCharHeight : Word;

  Var
    LResult : Word;                                             {!!.51}

  Begin
    LResult := FontDescr.ChHeight + FontDescr.ChHeightExtra;    {!!.51}
    If LResult = 0 Then LResult := 1;                           {!!.51}
    TotalCharHeight := LResult;                                 {!!.51}
  End;


  Procedure TBrowserWindow.PosClientCorruption;               {!!.04}

  Begin
    lwPosClientCorruption := True;
  End;


  Procedure TBrowserWindow.MarkClientCorruptablePhase;        {!!.04}

  Begin
    lwPosClientCorruption := False;
  End;


  Function TBrowserWindow.ClientPosCorrupted : Boolean;       {!!.04}

  Begin
    ClientPosCorrupted := lwPosClientCorruption;
  End;


  Function TBrowserWindow.AdjustHorizOfs ( Delta : Integer) : Integer;

  Var
    Offset : LongInt;

  Begin
    Offset := LongInt (HorizOfs) + Delta;
    If Offset < 0 Then Begin
      Offset := 0;
    End Else Begin
      If OffSet > MaxHorizOfs Then Begin
        Offset := MaxHorizOfs;
      End;
    End;
    AdjustHorizOfs := Offset;
  End;


  Function TBrowserWindow.CalcMaxWidth : Integer;                 {!!.03b}

  Begin
    CalcMaxWidth := MaxCols * FontDescr.ChWidth;
  End;


  Procedure TBrowserWindow.lwUpdateVertScrollBar;

  Var
    RelPos : Word;

  Begin
    BrowserPtr^.HGetApprRelPos ( RelPos, lwVertScale, GetCurrentKeyStr,
        GetCurrentDatRef );
    If Not BrowserPtr^.StatusOK Then RelPos := 0;
    If Attr.Style And ws_VScroll <> 0 Then Begin                  {!!.03}
      SetScrollPos ( HWindow, SB_Vert, RelPos, True );
    End;
  End;


  Procedure TBrowserWindow.lwUpdateHorzScrollBar;

  Begin
    If MaxHorizOfs > 0 Then Begin
      If Attr.Style And ws_HScroll <> 0 Then Begin                {!!.03}
        SetScrollPos ( HWindow, SB_Horz, HorizOfs, True );
      End;
    End;
  End;


  Procedure TBrowserWindow.SetCharValues;

  Var
    UsedDC : HDC;
    TM     : TTextMetric;

  Begin
    UsedDC := GetDC ( HWindow );

    With FontDescr Do Begin
      ChHeight := 1;
      ChWidth:= 1;
      ChRefWidth := 1;                                             {!!.04}
      ChHeightExtra := 0;                                          {!!.03b}
      If Font > 0 Then SelectObject ( UsedDC, Font);
      If GetTextMetrics ( UsedDC, TM ) Then Begin
        With TM Do Begin
          ChHeight := tmHeight + tmExternalLeading;
          ChWidth := (tmMaxCharWidth + tmAveCharWidth) Shr 1;      {!!.04}
          ChRefWidth := tmAveCharWidth;                            {!!.04}
        End;
      End;
    End;
    ReleaseDC ( HWindow, UsedDC );
  End;


  Procedure TBrowserWindow.SetMargins;                             {!!.03b}

  Begin
    FillChar ( TextMargin, SizeOf (TextMargin), 0 );
  End;


  Procedure TBrowserWindow.Reinit;
    {-Initialize variables that can change if the window is resized}
  Var
    R         : TRect;
    MaxWidth  : Word;
    DC        : HDC;
    RG        : HRgn;

  Begin
    SetCharValues;

  {--Get Rectangle to use}
    GetBrowserTextRect ( R );                                      {!!.03b}

  {--Vertical stuff in rows}
    FullPage := TotalSpaceForLines ( R ) Div TotalCharHeight;      {!!.03b}
    FirstRow := 1;
    If lwHeader <> '' Then Begin
      Dec (FullPage);
      Inc (FirstRow);
    End;
    If lwFooter <> '' Then Dec (FullPage);

    If (FullPage > $FFF0) Or (FullPage = 0) Then FullPage := 1;   {!!.03b}
      {-Holds functionality of this browser when resized below 1} {!!.03b}
    BrowserPtr^.AdjustNrOfRows ( FullPage );                      {!!.03b}

  {--Horizontal stuff in pixels}
    Width := R.Right - R.Left;
    MaxWidth := CalcMaxWidth;
    If Width > MaxWidth Then Begin
      MaxHorizOfs := 0;
    End Else Begin
      MaxHorizOfs := MaxWidth - Width;
    End;
    HorizOfs := AdjustHorizOfs ( 0 );                             {!!.03b}
    If Attr.Style And ws_HScroll <> 0 Then Begin                  {!!.03}
      SetScrollRange ( HWindow, SB_Horz, 0, MaxHorizOfs, False );
      SetScrollPos ( HWindow, SB_Horz, HorizOfs, True );
    End;

  End;


  Procedure TBrowserWindow.LineDown;

  Var
    LRow   : Word;
    Moved  : Word;
    CR     : Word;
    R      : TRect;
    Dummy,
    Update : Boolean;
    TRR    : RowRec;

  Begin
    Update := True;
    lwNoPaintHFCg := True;                                           {!!.05}
    With BrowserPtr^ Do Begin
      LRow := GetLastRow;
      CR := GetCurRow;
      If (CR = LRow) Or (LRow = 0) Then Begin
        MarkClientCorruptablePhase;                                {!!.04}
        HBuildNextPage ( 1, Moved, True, 0, Dummy );
        If StatusOK Then Begin
          If OtherAction Then Begin
            InvalidateBrowserScreen;                               {!!.03b}
          End Else Begin
            If Moved = 1 Then Begin
              If FullPage <> 1 Then Begin
                If LRow = GetLastRow Then Begin
                  If ClientPosCorrupted Then Begin                 {!!.04}
                    InvalidateBrowserScreen;                       {!!.04}
                  End Else Begin                                   {!!.04}
                    GetRowAreaRect ( R );                          {!!.04}
                    Dec (R.Bottom, FontDescr.ChHeightExtra);       {!!.04}
                    CopyRowRec ( BSAPtr^ [LRow]^, TRR );
                    CopyRowRec ( BSAPtr^ [Pred (LRow)]^, BSAPtr^ [LRow]^ );
                    DisplayRow ( LRow, 0, False);
                    CopyRowRec ( TRR, BSAPtr^ [LRow]^ );
                      {-Copy RowRec to avoid scrolling the highlight bar}
                    ScrollWindow ( HWindow, 0, -TotalCharHeight, @R, @R );
                                                                   {!!.03b}
                    ValidateRect ( HWindow, Nil );                 {!!.03b}
                  End;                                             {!!.04}
                End Else Begin
                  SetCurRow ( Succ (CR) );
                  DisplayRow ( LRow, 0, False);
                End;
              End;
              DisplayRow ( GetCurRow, 0, True);
            End Else Begin
              Update := False;
            End;
          End;
        End Else Begin
          Update := False;
        End;
      End Else Begin
        DisplayRow ( CR , 0, False);
        SetCurRow ( Succ (CR) );
        DisplayRow ( GetCurRow, 0, True);
      End;
    End;
    If Update Then lwUpdateVertScrollBar;
  End;


  Procedure TBrowserWindow.LineUp;

  Var
    Moved  : Word;
    CR     : Word;
    R      : TRect;
    Dummy,
    Update : Boolean;
    TRR    : RowRec;

  Begin
    Update := True;
    lwNoPaintHFCg := True;                                           {!!.05}
    With BrowserPtr^Do Begin
      CR := GetCurRow;
      If CR = 1 Then Begin
        MarkClientCorruptablePhase;                                {!!.04}
        HBuildPrevPage ( 1, Moved, True, 0, Dummy );
        If StatusOK Then Begin
          If OtherAction Then Begin
            InvalidateBrowserScreen;                               {!!.03b}
          End Else Begin
            If Moved = 1 Then Begin
              If FullPage <> 1 Then Begin
                If ClientPosCorrupted Then Begin                   {!!.04}
                  InvalidateBrowserScreen;                         {!!.04}
                End Else Begin                                     {!!.04}
                  GetRowAreaRect ( R );                            {!!.04}
                  Dec (R.Bottom, FontDescr.ChHeightExtra);         {!!.04}
                  CopyRowRec ( BSAPtr^ [1]^, TRR );
                  CopyRowRec ( BSAPtr^ [2]^, BSAPtr^ [1]^ );
                  DisplayRow ( 1, 0, False);
                  CopyRowRec ( TRR, BSAPtr^ [1]^ );
                    {-Copy RowRec to avoid scrolling the highlight bar}
                  ScrollWindow ( HWindow, 0, TotalCharHeight, @R, @R );
                                                                   {!!.03b}
                  ValidateRect ( HWindow, Nil );                   {!!.03b}
                End;                                               {!!.04}
              End;
              DisplayRow ( 1, 0, True);
            End Else Begin
              Update := False;
            End;
          End;
        End Else Begin
          Update := False;
        End;
      End Else Begin
        DisplayRow ( CR , 0, False);
        SetCurRow ( Pred (CR) );
        lwNoPaintHFCg := True;                                       {!!.05}
        DisplayRow ( GetCurRow, 0, True);
      End;
    End;
    If Update Then lwUpdateVertScrollBar;
  End;


  Procedure TBrowserWindow.PageDown;

  Var
    BST     : BrowScreenState;
    Moved   : Word;
    Update,
    Changed : Boolean;

  Begin
    Update := False;
    With BrowserPtr^ Do Begin
      GetBrowScreenState ( BST );
      HBuildNextPage ( FullPage, Moved, True, 1, Changed );
      If StatusOK Then Begin
        If OtherAction Or (Moved > 0) Or Changed
            Or BrowScreenStateChanged ( BST ) Then Begin
          InvalidateBrowserScreen;                                 {!!.03b}
          Update := True;
        End;
      End;
    End;
    lwNoPaintHFCg := True;                                           {!!.05}
    If Update Then lwUpdateVertScrollBar;
  End;


  Procedure TBrowserWindow.PageUp;

  Var
    BST     : BrowScreenState;
    Moved   : Word;
    Update,
    Changed : Boolean;

  Begin
    Update := False;
    With BrowserPtr^ Do Begin
      GetBrowScreenState ( BST );
      HBuildPrevPage ( FullPage, Moved, True, 1, Changed );
      If StatusOK Then Begin
        If OtherAction Or (Moved > 0) Or Changed
            Or BrowScreenStateChanged ( BST ) Then Begin
          InvalidateBrowserScreen;                                 {!!.03b}
          Update := True;
        End;
      End;
    End;
    lwNoPaintHFCg := True;                                           {!!.05}
    If Update Then lwUpdateVertScrollBar;
  End;


  Procedure TBrowserWindow.FirstPage;

  Var
    BST     : BrowScreenState;
    Update,
    Changed : Boolean;

  Begin
    Update := False;
    With BrowserPtr^ Do Begin
      GetBrowScreenState ( BST );
      HBuildFirstPage ( Changed );
      If StatusOK Then Begin
        If Changed Or BrowScreenStateChanged ( BST ) Then Begin
          InvalidateBrowserScreen;                                 {!!.03b}
          Update := True;
        End;
      End;
    End;
    If Update Then lwUpdateVertScrollBar;
  End;


  Procedure TBrowserWindow.LastPage;

  Var
    BST     : BrowScreenState;
    Update,
    Changed : Boolean;

  Begin
    Update := False;
    With BrowserPtr^ Do Begin
      GetBrowScreenState ( BST );
      HBuildLastPage ( Changed );
      If StatusOK Then Begin
        If Changed Or BrowScreenStateChanged ( BST ) Then Begin
          InvalidateBrowserScreen;                                 {!!.03b}
          Update := True;
        End;
      End;
    End;
    If Update Then lwUpdateVertScrollBar;
  End;


  Procedure TBrowserWindow.MoveToRelPos ( Pos : Word );

  Var
    Key : GenKeyStr;
    Ref : LongInt;

  Begin
    lwNoPaintHFCg := True;                                           {!!.05}
    BrowserPtr^.HGetApprKeyAndRef ( Pos, lwVertScale, Key, Ref );
    SetAndUpdateBrowserScreen ( Key, Ref );                       {!!.03b}
    lwNoPaintHFCg := True;                                           {!!.05}
  End;


  Procedure TBrowserWindow.LineRight;

  Var
    OldHOfs : Integer;

  Begin
    OldHOfs := HorizOfs;
    HorizOfs := AdjustHorizOfs ( FontDescr.ChWidth );
    If OldHOfs <> HorizOfs Then Begin
      InvalidateBrowserScreen;                                    {!!.03b}
      lwUpdateHorzScrollBar;
    End;
  End;


  Procedure TBrowserWindow.LineLeft;

  Var
    OldHOfs : Integer;

  Begin
    OldHOfs := HorizOfs;
    HorizOfs := AdjustHorizOfs ( -FontDescr.ChWidth );
    If OldHOfs <> HorizOfs Then Begin
      InvalidateBrowserScreen;                                     {!!.03b}
      lwUpdateHorzScrollBar;
    End;
  End;


  Procedure TBrowserWindow.PageRight;

  Var
    OldHOfs : Integer;

  Begin
    OldHOfs := HorizOfs;
    HorizOfs := AdjustHorizOfs ( FontDescr.ChWidth * 10 );
    If OldHOfs <> HorizOfs Then Begin
      InvalidateBrowserScreen;                                     {!!.03b}
      lwUpdateHorzScrollBar;
    End;
  End;


  Procedure TBrowserWindow.PageLeft;

  Var
    OldHOfs : Integer;

  Begin
    OldHOfs := HorizOfs;
    HorizOfs := AdjustHorizOfs ( -FontDescr.ChWidth * 10 );
    If OldHOfs <> HorizOfs Then Begin
      InvalidateBrowserScreen;                                     {!!.03b}
      lwUpdateHorzScrollBar;
    End;
  End;


  Procedure TBrowserWindow.LeftHome;

  Var
    OldHOfs : Integer;

  Begin
    OldHOfs := HorizOfs;
    HorizOfs := AdjustHorizOfs ( -HorizOfs );                      {!!.03b}
    If OldHOfs <> HorizOfs Then Begin
      InvalidateBrowserScreen;                                     {!!.03b}
      lwUpdateHorzScrollBar;
    End;
  End;


  Procedure TBrowserWindow.RightHome;

  Var
    OldHOfs : Integer;

  Begin
    OldHOfs := HorizOfs;
    HorizOfs := AdjustHorizOfs ( MaxHorizOfs - HorizOfs );         {!!.03b}
    If OldHOfs <> HorizOfs Then Begin
      InvalidateBrowserScreen;                                     {!!.03b}
      lwUpdateHorzScrollBar;
    End;
  End;


  Procedure TBrowserWindow.MoveToHorizPos ( Pos : Word );

  Var
    OldHOfs : Integer;

  Begin
    OldHOfs := HorizOfs;
    HorizOfs := Pos;
    HorizOfs := AdjustHorizOfs ( 0 );
    If OldHOfs <> HorizOfs Then Begin
      InvalidateBrowserScreen;                                     {!!.03b}
      lwUpdateHorzScrollBar;
    End;
  End;


  Procedure TBrowserWindow.GetHeaderFooterColor                   {!!.03}
      ( Var Color, BkColor : TColorRef; ForHeader : Boolean );

  Begin
    BkColor := GetSysColor ( Color_HighLight );
    Color := GetSysColor ( Color_HighLightText );
  End;


  Procedure TBrowserWindow.GetHighlightColor                      {!!.03}
      ( Var Color, BkColor : TColorRef );

  Begin
    BkColor := GetSysColor ( Color_Window ) Xor $FFFFFF;
    Color := GetSysColor ( Color_WindowText ) Xor $FFFFFF;
  End;


  Procedure TBrowserWindow.GetNormalColor                         {!!.03}
      ( Var Color, BkColor : TColorRef );

  Begin
    BkColor := GetSysColor ( Color_Window );
    Color := GetSysColor ( Color_WindowText );
  End;


  Function TBrowserWindow.UseSeparator                            {!!.03}
      ( Var Color : TColorRef ) : Boolean;

  Begin
    Color := 0;
    UseSeparator := False;
  End;


  Procedure TBrowserWindow.WriteHeader
                        ( Var HeaderLine : BRLRowEltString;       {!!.03b}
                              DC         : HDC );
  Var
    SepPos   : Word;
    SepColor : TColorRef;

  Begin
    SepPos := WriteStringOut ( HeaderLine, 0, DC, -HorizOfs );
    If UseSeparator ( SepColor ) Then Begin
      DrawSeparator ( SepPos + FontDescr.ChHeight - 1, SepColor, DC );
                                                                  {!!.03b}
    End;
  End;


  Procedure TBrowserWindow.WriteDataLine
                          ( Var DataLine : BRLRowEltString;       {!!.03b}
                                LineNr   : Word;
                                DC       : HDC;
                                Inverse  : Boolean );

  Begin
    WriteStringOut ( DataLine, LineNr, DC, -HorizOfs );
  End;


  Procedure TBrowserWindow.WriteFooter
                        ( Var FooterLine : BRLRowEltString;       {!!.03b}
                              DC         : HDC );

  Var
    SepPos   : Word;
    SepColor : TColorRef;

  Begin
    SepPos := TBrowserWindow.WriteStringOut ( FooterLine,
        Succ (FullPage), DC, 0 );
    If UseSeparator ( SepColor ) Then Begin
      DrawSeparator ( SepPos - 1, SepColor, DC );
    End;
  End;


  Procedure TBrowserWindow.DrawSeparator ( Pos   : Word;          {!!.03b}
                                           Color : TColorRef;
                                           DC    : HDC );

  Var
    OldPen,
    Pen    : HPen;
    TR     : TRect;

  Begin
    Pen := CreatePen ( PS_Solid, 1, Color );
    If Pen <> 0 Then Begin
      OldPen := SelectObject ( DC, Pen );
      GetBrowserTextRect ( TR );                                  {!!.03b}
      MoveTo ( DC, TR.Left, Pos );
      LineTo ( DC, TR.Right, Pos );
      DeleteObject ( SelectObject ( DC, OldPen ));
    End;
  End;


  Function TBrowserWindow.WriteStringOut ( Var S      : String;   {!!.03b}
                                               LineNr : Word;
                                               DC     : HDC;
                                               XOfs   : Integer ) : Word;

  Var
    Buffer : Array [Byte] Of Char;
    Y      : Word;

  Begin
    StrPCopy ( @Buffer, S );

    FillChar ( Buffer [Length ( S )], SizeOf ( Buffer) - Length ( S ),
        32 );                                                     {!!.03b}
    Buffer [255] := #0;

    Y := GetTextOutPosY ( LineNr );                               {!!.03b}
    WriteStringOut := Y;
    TextOut ( DC, XOfs + TextMargin.Left, Y, @Buffer,
        Pred (SizeOf (Buffer)) );
                                                                  {!!.03b}
  End;


  Procedure TBrowserWindow.GetBrowserTextRect ( Var TextRect : TRect );
                                                                  {!!.03b}

  Begin
    GetClientRect ( HWindow, TextRect );
    Inc ( TextRect.Left, TextMargin.Left );
    Inc ( TextRect.Top, TextMargin.Top );
    Dec ( TextRect.Right, TextMargin.Right );
    If TextRect.Right < TextRect.Left Then Begin
      TextRect.Right := TextRect.Left;
    End;
    Dec ( TextRect.Bottom, TextMargin.Bottom );
    If TextRect.Bottom < TextRect.Bottom Then Begin
      TextRect.Bottom := TextRect.Bottom;
    End;
  End;


  Procedure TBrowserWindow.GetRowAreaRect ( Var Rect : TRect );

  Var
    Lines : Word;

  Begin
    Rect.Top := GetTextOutPosY ( 1 );                           {!!.04mod}
    If BrowserPtr <> Nil Then Begin
      Lines := BrowserPtr^.GetLastRow;
    End Else Begin
      Lines := FullPage;
    End;
    Rect.Bottom := Rect.Top + TotalCharHeight * Lines;
    Rect.Left := TextMargin.Left;
    Rect.Right := Rect.Left + Width;
  End;


  Function TBrowserWindow.GetTextOutPosY ( LineNr : Word ) : Integer;

  Var
    Pos : Integer;
    TR  : TRect;

  Begin
    Pos := (LineNr + FirstRow - 2) * TotalCharHeight              {!!.05mod}
        + TextMargin.Top;
    If LineNr > FullPage Then Begin                                  {!!.05}
      GetClientRect ( HWindow, TR );                                 {!!.05}
      Pos := TR.Bottom - TotalCharHeight - FontDescr.ChHeightExtra   {!!.05}
          - TextMargin.Bottom;                                       {!!.05}
    End;                                                             {!!.05}
    GetTextOutPosY := Pos;                                           {!!.05}
  End;


  Function TBrowserWindow.GetLineNrFromY ( Y : Integer ) : Word;  {!!.03b}

  Begin
    GetLineNrFromY := (Y - TextMargin.Top) Div TotalCharHeight
          + 2 - FirstRow;
  End;


  Function TBrowserWindow.XYPosInRect ( X, Y : Integer; Rect : TRect )
                                                                : Boolean;

  Begin
    XYPosInRect := (X >= Rect.Left) And (X < Rect.Right)
        And (Y >= Rect.Top) And (Y < Rect.Bottom);
  End;


  Function TBrowserWindow.TotalSpaceForLines ( Rect : TRect ) : Word;
                                                                  {!!.03b}
  Begin
    TotalSpaceForLines := Rect.Bottom - Rect.Top;
  End;


  Procedure TBrowserWindow.FillClientGaps ( DC : HDC );           {!!.05}

  Var
    HBR       : HBrush;
    R         : TRect;
    BkColor,
    TextColor : TColorRef;

  Begin
    GetBrowserTextRect ( R );
    GetNormalColor ( TextColor, BkColor );
    R.Top := GetTextOutPosY ( FullPage ) + TotalCharHeight
        - FontDescr.ChHeightExtra;
    HBR := CreateSolidBrush ( BkColor );
    If lwFooter <> '' Then Begin
      R.Bottom := GetTextOutPosY ( Succ (FullPage) );
    End;
    If HBR <> 0 Then Begin
      FillRect ( DC, R, HBR );
      DeleteObject ( HBR );
    End;
  End;


  Procedure TBrowserWindow.DisplayRow ( I       : Integer;        {!!.03}
                                        DC      : HDC;
                                        Inverse : Boolean);

  Var
    UsedDC       : HDC;
    BkColor      : TColorRef;
    TextColor    : TColorRef;
    OldBkColor   : TColorRef;
    OldTextColor : TColorRef;
    RG           : HRgn;
    R            : TRect;

  Begin
    UsedDC := DC;
    If DC = 0 Then Begin
      UsedDC := GetDC ( HWindow );
      GetBrowserTextRect ( R );                                    {!!.03b}
      RG := CreateRectRgn ( R.Left, R.Top, R.Right, R.Bottom );    {!!.03b}
      SelectClipRgn ( UsedDC, RG );                                {!!.03b}
      DeleteObject ( RG );                                         {!!.03b}
    End;

    If FontDescr.Font > 0 Then SelectObject ( UsedDC, FontDescr.Font);

    OldBkColor := GetBkColor ( UsedDC );
    OldTextColor := GetTextColor ( UsedDC );

    If Inverse Then Begin                                        {!!.05mov}
      GetHighLightColor ( TextColor, BkColor );                  {!!.05mov}
    End Else Begin                                               {!!.05mov}
      GetNormalColor ( TextColor, BkColor );                     {!!.05mov}
    End;                                                         {!!.05mov}

    SetBkColor ( UsedDC, BkColor );                              {!!.05mov}
    SetTextColor ( UsedDC, TextColor );                          {!!.05mov}

    WriteDataLine ( BrowserPtr^.BSAPtr^ [I]^.Row, I, UsedDC, Inverse );
                                                                 {!!.05mov}
    If I = 1 Then Begin
      If ClientPosCorrupted Or Not lwNoPaintHFCg Then Begin       {!!.05mod}
        MarkClientCorruptablePhase;                               {!!.05}
        FillClientGaps ( UsedDC );
        If (lwHeader <> '') Or (lwFooter <> '') Then Begin
          If lwHeader <> '' Then Begin
            GetHeaderFooterColor ( TextColor, BkColor, True );
            SetBkColor ( UsedDC, BkColor );
            SetTextColor ( UsedDC, TextColor );
            WriteHeader ( lwHeader, UsedDC );
          End;
          If lwFooter <> '' Then Begin
            GetHeaderFooterColor ( TextColor, BkColor, False );
            SetBkColor ( UsedDC, BkColor );
            SetTextColor ( UsedDC, TextColor );
            WriteFooter ( lwFooter, UsedDC );
          End;
        End;
        SetBkColor ( UsedDC, OldBkColor );
        SetTextColor ( UsedDC, OldTextColor );
      End Else Begin                                                 {!!.05}
        If BrowserPtr^.GetCurRow <> 1 Then lwNoPaintHFCg := False;   {!!.05}
      End;
    End;

    SetBkColor ( UsedDC, OldBkColor );
    SetTextColor ( UsedDC, OldTextColor );

    If DC = 0 Then ReleaseDC ( HWindow, UsedDC );
  End;


  Procedure TBrowserWindow.Paint (     PaintDC    : HDC;
                                   Var PaintStruct: TPaintStruct );
  Var
    I,
    CR : Word;
    RG : HRgn;
    R  : TRect;
    DC : HDC;

  Begin
    DC := GetDC ( HWindow );                                       {!!.03b}
    GetBrowserTextRect ( R );                                      {!!.03b}
    RG := CreateRectRgn ( R.Left, R.Top, R.Right, R.Bottom );      {!!.03b}
    SelectClipRgn ( DC, RG );                                      {!!.03b}
    DeleteObject ( RG );                                           {!!.03b}
    CR := BrowserPtr^.GetCurRow;
    For I := 1 To FullPage Do Begin
      DisplayRow ( I, DC, I = CR);                                 {!!.03b}
    End;
    ReleaseDC ( HWindow, DC );                                     {!!.03b}
  End;


  Procedure TBrowserWindow.WMLButtonDown ( Var Msg : TMessage );

  Var
    X, Y      : Word;
    OldCurrow : Integer;
    Rect      : TRect;

  Begin
    If Not CanCallLowBrowser Then Begin
      Exit;
    End;
    If Not lwDoMouseMove Then Exit;                                {!!.03b}
    X := Msg.lParam And $FFFF;                                     {!!.03b}
    Y := Msg.lParam Shr 16;
    OldCurrow := BrowserPtr^.GetCurrow;
    If OldCurRow <> 0 Then Begin
      GetRowAreaRect ( Rect );                                     {!!.03b}
      If Not XYPosInRect ( X, Y, Rect ) Then Exit;                 {!!.03b}
      BrowserPtr^.SetCurRow ( GetLineNrFromY ( Y ) );              {!!.03b}
      If (OldCurRow <> BrowserPtr^.GetCurRow)
          And (BrowserPtr^.GetCurRow <> 0) Then Begin
        DisplayRow ( OldCurRow, 0, False );
        DisplayRow ( BrowserPtr^.GetCurRow, 0, True );
        lwUpDateVertScrollBar;
      End;
    End;
  End;


  Procedure TBrowserWindow.WMKillFocus ( Var Msg : TMessage);      {!!.03b}

  Begin
    lwDoMouseMove := False;
    lwCtrlDown := False;
    lwShiftDown := False;
    lwNoPaintHFCg := False;                                          {!!.50}
    DefWndProc ( Msg );
  End;


  Procedure TBrowserWindow.lwFirstInit;

  Begin
    If lwFInitDone Then Exit;
    If HWindow = 0 Then Exit;
    SetTheFont;
    SetMargins;
    Reinit;
    BrowserPtr^.SetNrOfRows ( FullPage );
    lwThumbHTrack := $FFFF;                                       {!!.03b}
    If Attr.Style And ws_HScroll <> 0 Then Begin                  {!!.03}
      SetScrollRange ( HWindow, SB_Horz, 0, MaxHorizOfs, True );
    End;
    If Attr.Style And ws_VScroll <> 0 Then Begin                  {!!.03}
      SetScrollRange ( HWindow, SB_Vert, 0, lwVertScale, True );
    End;
    lwFInitDone := True;
    FirstUserInit;
  End;


  Procedure TBrowserWindow.lwSetAndUpdateBrowserScreen
                                            ( NewKeyStr : GenKeyStr;
                                              NewRef    : LongInt );

  Begin
    With BrowserPtr^ Do Begin
      HBuildNewPage ( KeyNr, NewKeyStr, NewRef, GetCurRow, NrOfRows );
      InvalidateBrowserScreen;                                     {!!.03b}
      If StatusOK Then lwUpdateVertScrollBar;
    End;
  End;


  Procedure TBrowserWindow.lwUpdateBrowserScreen;

  Var
    Changed : Boolean;

  Begin
    With BrowserPtr^ Do Begin
      HBuildThisPage ( Changed );
      If Changed Then Begin
        InvalidateBrowserScreen;                                   {!!.03b}
        If StatusOK Then lwUpdateVertScrollBar;
      End;
    End;
  End;


  Procedure TBrowserWindow.lwNewSize;

  Var
    OldNrOfRows : Word;

  Begin
    OldNrOfRows := FullPage;
    Reinit;
    With BrowserPtr^ Do Begin
      If OldNrOfRows > FullPage Then Begin
        HShrinkPage ( FullPage );
      End Else Begin
        If OldNrOfRows < FullPage Then Begin
          HExpandPage ( FullPage );
        End;
      End;
    End;
    InvalidateBrowserScreen;                                       {!!.03b}
  End;



  Function TBrowserWindow.ConnectLowBrowser
                              ( ABrowserPtr : PLowWinBrowser;
                                AHeader,
                                AFooter     : BRLRowEltString ) : Boolean;

  Begin
    ConnectLowBrowser := False;
    lwFInitDone := False;
    If ABrowserPtr = Nil Then Exit;
    If BrowserPtr <> Nil Then Begin
      If BrowserPtr^.OnHeap Then Begin
        Dispose ( BrowserPtr, Done );
      End Else Begin
        BrowserPtr^.Done;
      End;
    End;
    BrowserPtr := ABrowserPtr;
    BrowserPtr^.Owner := @Self;
    SetHeaderNoUpdate ( AHeader );                                 {!!.03b}
    SetFooterNoUpdate ( AFooter );                                 {!!.03b}
    lwFirstInit;
    ConnectLowBrowser := True;
  End;


  Function TBrowserWindow.GetClassName : PChar;

  Begin
    GetClassName := 'BTreeBrowser';
  End;


  Procedure TBrowserWindow.GetWindowClass ( Var WndClass : TWndClass );

  Begin
    TWindow.GetWindowClass ( WndClass );
    WndClass.Style := WndClass.Style Or cs_DblClks;
  End;


  Procedure TBrowserWindow.NewSize;


  Begin
    If CanCallLowBrowser Then Begin
      lwNewSize;
    End;
  End;


  Procedure TBrowserWindow.WMSize ( Var Msg: TMessage );


  Begin
    If CanCallLowBrowser Then Begin
      If Msg.wParam <> SizeIconic Then Begin
        NewSize;
      End;
    End;
    TWindow.WMSize ( Msg );
  End;


  Procedure TBrowserWindow.WMTimer ( Var Msg : TMessage );

  Begin
    If Not CanCallLowBrowser Or lwSupprTimer
        Or IsIconic ( HWindow ) Then Exit;
    If BrowserPtr^.BrowserCallAllowed Then Begin
      UpdateBrowserScreen;
    End;
  End;


  Procedure TBrowserWindow.WMNCMouseMove ( Var Msg : TMessage );   {!!.03b}

  Begin
    If Not lwDoMouseMove Then Begin
      If GetFocus = HWindow Then lwDoMouseMove := True;
    End;
    DefWndProc ( Msg );
  End;


  Procedure TBrowserWindow.WMMouseMove ( Var Msg : TMessage );

  Var
    X, Y      : Word;
    OldCurRow : Integer;
    Rect      : TRect;

  Begin
    If Not lwDoMouseMove Then Begin                                {!!.03b}
      If GetFocus = HWindow Then lwDoMouseMove := True;            {!!.03b}
      Exit;                                                        {!!.03b}
    End;                                                           {!!.03b}
    If CanCallLowBrowser Then Begin
      If Msg.wParam = MK_LButton Then Begin
        With BrowserPtr^ Do Begin
          X := Msg.lParam And $FFFF;                               {!!.03b}
          Y := Msg.lParam Shr 16;
          OldCurRow := GetCurRow;
          If OldCurRow <> 0 Then Begin
          GetRowAreaRect ( Rect );                                 {!!.03b}
          If Not XYPosInRect ( X, Y, Rect ) Then Exit;             {!!.03b}
            SetCurRow ( GetLineNrFromY ( Y ) );                    {!!.03b}
            If GetCurRow > GetLastRow Then Begin
              SetCurRow ( GetLastRow );
            End;
            If (OldCurRow <> GetCurRow) And (GetCurRow <> 0) Then Begin
              lwNoPaintHFCg := True;                                 {!!.05}
              DisplayRow ( OldCurRow, 0, False );
              lwNoPaintHFCg := True;                                 {!!.05}
              DisplayRow ( CurRow, 0, True );
              lwUpdateVertScrollBar;
            End;
          End;
        End;
      End;
    End;
  End;


  Procedure TBrowserWindow.WMVScroll ( Var Msg : TMessage );

  Begin
    If Not CanCallLowBrowser Then Begin
      Exit;
    End;
    If Not lwDoMouseMove Then Begin                                {!!.03b}
      If GetFocus = HWindow Then lwDoMouseMove := True;            {!!.03b}
      Exit;                                                        {!!.03b}
    End;                                                           {!!.03b}
    Case Msg.wParam Of
      SB_EndScroll : Begin
        If lwThumbVTrack <> $FFFF Then Begin                      {!!.03b}
          MoveToRelPos ( lwThumbVTrack );
          lwThumbVTrack := $FFFF;                                 {!!.03b}
        End;
      End;
      SB_ThumbTrack : Begin
        lwThumbVTrack := Msg.lParam And $0000FFFF;
      End;
      SB_LineDown : Begin
        LineDown;
      End;
      SB_LineUp : Begin
        LineUp;
      End;
      SB_PageDown : Begin
        PageDown;
      End;
      SB_PageUp : Begin
        PageUp;
      End;
      SB_Top : Begin
        FirstPage;
      End;
      SB_Bottom : Begin
        LastPage;
      End;
    End; {Case}
  End;


  Procedure TBrowserWindow.WMHScroll ( Var Msg : TMessage );

  Begin
    If Not CanCallLowBrowser Then Exit;
    If Not lwDoMouseMove Then Begin                                {!!.03b}
      If GetFocus = HWindow Then lwDoMouseMove := True;            {!!.03b}
      Exit;                                                        {!!.03b}
    End;                                                           {!!.03b}
    Case Msg.wParam Of
      SB_EndScroll : Begin
        If lwThumbHTrack <> $FFFF Then Begin                      {!!.03b}
          MoveToHorizPos ( lwThumbHTrack );
          lwThumbHTrack := $FFFF;                                 {!!.03b}
        End;
      End;
      SB_ThumbTrack : Begin
        lwThumbHTrack := Msg.lParam And $0000FFFF;
      End;
      SB_LineDown : Begin
        LineRight;
      End;
      SB_LineUp : Begin
        LineLeft;
      End;
      SB_PageDown : Begin
        PageRight;
      End;
      SB_PageUp : Begin
        PageLeft;
      End;
      SB_Top : Begin
        LeftHome;
      End;
      SB_Bottom : Begin
        RightHome;
      End;
    End; {Case}
  End;


  Procedure TBrowserWindow.WMKeyDown ( Var Msg : TMessage );

  Begin
    If CanCallLowBrowser Then Begin
      Case Msg.wParam Of
        vk_Down : Begin
          LineDown;
        End;
        vk_Up : Begin
          LineUp;
        End;
        vk_Next : Begin
          If Not lwCtrlDown Then Begin
            PageDown;
          End;
        End;
        vk_Prior : Begin
          If Not lwCtrlDown Then Begin
            PageUp;
          End;
        End;
        vk_Home : Begin
          If lwCtrlDown Then Begin
            FirstPage;
          End Else Begin
            LeftHome;
          End;
        End;
        vk_End : Begin
          If lwCtrlDown Then Begin
            LastPage;
          End Else Begin
            RightHome;
          End;
        End;
        vk_Right : Begin
          If lwCtrlDown Then Begin
            PageRight;
          End Else Begin
            LineRight;
          End;
        End;
        vk_Left : Begin
          If lwCtrlDown Then Begin
            PageLeft;
          End Else Begin
            LineLeft;
          End;
        End;
        vk_Control : Begin
          lwCtrlDown := True;
        End;
        vk_Menu : Begin
          lwCtrlDown := False;
        End;
        vk_Shift : Begin
          lwShiftDown := True;
        End;
      End;
    End;
    DefWndProc( Msg );
  End;


  Procedure TBrowserWindow.WMKeyUp ( Var Msg : TMessage );

  Begin
    Case Msg.wParam Of
      vk_Control : Begin
        lwCtrlDown := False;
      End;
      vk_Shift : Begin
        lwShiftDown := False;
      End;
    End;
    DefWndProc ( Msg );
  End;


  Procedure TBrowserWindow.WMChar ( Var Msg : TMessage );

  Begin
    If Not CanCallLowBrowser Then Begin
      DefWndProc ( Msg );
      Exit;
    End;

    If HandleChar ( Msg ) Then Exit;

    Case UpCase ( Chr (Msg.wParam )) Of
      '0'..'9', 'A'..'Z', 'ä', 'Ä', 'ö', 'Ö', 'ü', 'Ü', 'ß' : Begin
        SetAndUpdateBrowserScreen ( Chr (Msg.wParam ), 0 );      {!!.03b}
      End;
      '+' : Begin
        UpdateBrowserScreen;                                     {!!.03b}
      End Else
        DefWndproc ( Msg );
    End; {Case}
  End;


{$IFDEF InitAllUnits}
Begin
{$ENDIF}
End.
