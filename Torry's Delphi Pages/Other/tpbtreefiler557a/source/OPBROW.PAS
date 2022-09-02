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
 * Portions created by the Initial Developer are Copyright (C) 1996-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Based in part on code written by Ralf Nagel
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

  {-Compiler options}
  {$I BRDefOpt.Inc}
  {$I OPDefine.Inc}
  {$I-,V-,R-,F+,O+}       {-overwrites user defined options
                            and must not be changed}


Unit OPBrow;

Interface

Uses
  Dos,
{$IFDEF OPRO12}       {!! 1.20}
  OPConst,
{$ENDIF}
  OPDos,
  OpString,
  OpInline,
  OpRoot,
  OpCrt,
{$IFDEF UseMouse}
  OpMouse,
{$ENDIF}
  OpCmd,
  OpFrame,
  OpWindow,
{$IFDEF UseDrag}
  OpDrag,
{$ENDIF}
{$IFDEF BrUseShell}
  OPSBase,
  OOPShell,
{$ENDIF}
{$IFDEF BrUseIsam}
  BTBase,
  BTIsBase,
  Filer,     {!!.TP}
{$ENDIF}
  LowBrows,
  MedBrows,
  HiBrows;

{$I OPBrow.ICD}  {configuration data}

Const
  ucOPBrowse = 99;    { Unitcode }

  lwSelectOnClick  = $0001; {implicit Enter for mouse click on browser bar}
  lwSuppressUpdate = $0002; {No background update                         }
  DefOPBrOptions   : Word = lwSelectOnClick;
  BadOPBrOptions   : Word = 0;

Type
  ISBrowserPtr     = ^ISBrowser;
  LowWinBrowserPtr = ^LowWinBrowser;

  LowWinBrowser = Object ( BRHBrowser )
    Owner : ISBrowserPtr;

    Constructor Init (     AOwner               : ISBrowserPtr;
                           ADrvOrFileBlockPtr   : Pointer;
                           ANumberOfEltsPerRow  : Word;
                           ANumberOfRows        : Word;
                           ALKey,
                           AHKey                : GenKeyStr;
                           ASaveStat            : Boolean;
                       Var ADatS;
                           AIsVarRec            : Boolean );

    Destructor Done; Virtual;

    Function PreCompletePage : Integer; Virtual;

    Function PostCompletePage : Integer; Virtual;

    Function BuildRow ( Var RR  : RowRec ) : Integer; Virtual;

    Function PerformFilter ( Var RR    : RowRec;
                             Var UseIt : Boolean ) : Integer; Virtual;

    Procedure ShowErrorOccured ( Class : Integer ); Virtual;
  End;


  ISBrowser = Object ( CommandWindow )
    PBrowser        : LowWinBrowserPtr;
    lwOptions       : Word;           {option flags}
    lwFullPage,
    lwSaveFullPage,
    lwMaxHorizOfs,
    lwFirstRow,
    lwVertScale     : Word;
    lwHorizOfs      : Integer;

    lwDimColor          : Byte;
    lwDimMono           : Byte;
    lwHighlightColor    : Byte;
    lwHighlightMono     : Byte;
    lwHeaderFooterColor : Byte;
    lwHeaderFooterMono  : Byte;

    lwHeader,
    lwFooter            : BRLRowEltString;
    lwUpdateInterval    : Word;

 {$IFDEF BrUseIsam}
    Constructor Init (     X1, Y1, X2, Y2       : Byte;
                           AFileBlockPtr        : IsamFileBlockPtr;
                           ANumberOfEltsPerRow  : Word;
                           ANumberOfRows        : Word;
                           AKeyNr               : Word;
                           ALKey,
                           AHKey                : GenKeyStr;
                           AHeader,
                           AFooter              : BRLRowEltString;
                       Var ADatS;
                           AIsVarRec            : Boolean );
     {-Initialize ISBrowser}
{$ENDIF}
{$IFDEF BrUseShell}
    Constructor Init (     X1, Y1, X2, Y2       : Byte;
                           ADrvPtr              : IFDriverPtr;
                           ANumberOfEltsPerRow  : Word;
                           ANumberOfRows        : Word;
                           AKeyNr               : Word;
                           ALKey,
                           AHKey                : GenKeyStr;
                           AHeader,
                           AFooter              : BRLRowEltString );
     {-Initialize ISBrowser}
{$ENDIF}

{$IFDEF BrUseIsam}
    Constructor InitCustom (     X1, Y1, X2, Y2       : Byte;
                             Var Colors               : ColorSet;
                                 AFileBlockPtr        : IsamFileBlockPtr;
                                 ANumberOfEltsPerRow  : Word;
                                 ANumberOfRows        : Word;
                                 AKeyNr               : Word;
                                 ALKey,
                                 AHKey                : GenKeyStr;
                                 AHeader,
                                 AFooter              : BRLRowEltString;
                             Var ADatS;
                                 AIsVarRec            : Boolean;
                                 WinOptions           : LongInt );

     {-Initialize ISBrowser with custom colors and options}
{$ENDIF}
{$IFDEF BrUseShell}
    Constructor InitCustom (     X1, Y1, X2, Y2       : Byte;
                             Var Colors               : ColorSet;
                                 ADrvPtr              : IFDriverPtr;
                                 ANumberOfEltsPerRow  : Word;
                                 ANumberOfRows        : Word;
                                 AKeyNr               : Word;
                                 ALKey,
                                 AHKey                : GenKeyStr;
                                 AHeader,
                                 AFooter              : BRLRowEltString;
                                 WinOptions           : LongInt );
     {-Initialize ISBrowser with custom colors and options}
{$ENDIF}

    Destructor Done; Virtual;
     {-Dispose of browser}
    Procedure UpdateContents; Virtual;
     {-Redraw the browser}
    Procedure ProcessSelf; Virtual;
     {-Process browse commands}
    Procedure AdjustWindow ( X1, Y1, X2, Y2 : Word ); Virtual;
     {-Set new coordinates and adjust all related structures}
    Procedure DisplayRow ( I       : Integer;
                           Inverse : Boolean);
     {-Display on row of browser}
    Function PreCompletePage : Integer; Virtual;
     {-Hook to be called before a page is build}
    Function PostCompletePage : Integer; Virtual;
     {-Hook to be called after a page is build}
    Procedure ProcessPostCommand; Virtual;
     {-Call user defineable routine after handling of keyboard or
       mouse event }
    Procedure ProcessPreCommand; Virtual;
     {-Call user defineable routine prior to getting keyboard and mouse
       events }
    Function BuildRow ( Var RR  : RowRec ) : Integer; Virtual;
     {-Method to build one row; must be overridden}
    Function PerformFilter ( Var RR    : RowRec;
                             Var UseIt : Boolean ) : Integer; Virtual;
     {-Record filtering function}
    Procedure ShowErrorOccured ( Class : Integer ); Virtual;
     {-Hook to be called if error occurs}
    Procedure CharHandler; Virtual;
     {-Call user defineable routine to position browser}
    Procedure lwOptionsOn ( OptionFlags : Word );
     {-Activate multiple options}
    Procedure lwOptionsOff ( OptionFlags : Word );
     {-Deactivate multiple options}
    Function lwOptionsAreOn ( OptionFlags : Word ) : Boolean;
      {-Return true if all specified options are on}
    Procedure SetDimAttr ( Color, Mono : Byte );
      {-Set attributes for dim characters}
    Procedure SetHighlightAttr ( Color, Mono : Byte );
      {-Set attributes for highlighted characters}
    Procedure SetHeaderFooterAttr ( Color, Mono : Byte );
      {-Set attributes for Header characters}
    Procedure SetHeaderFooter ( AHeader, AFooter : BRLRowEltString );
      {-Change header and footer}
    Procedure SetAndUpdateBrowserScreen ( NewKeyStr : GenKeyStr;
                                          NewRef    : LongInt );
      {-Set new current row and update the browser screen}
    Procedure UpdateBrowserScreen;
      {-Update the browser screen}
    Function GetThisRec ( Var RR : RowRec ) : Integer;
     {-Get the record defined by RR}
    Function GetCurrentRec ( Var Match : Boolean ) : Integer;     {mod !!.03}
     {-Get the current record}
    Function GetCurrentKeyNr : Word;
     {-Get the current key number}
    Function GetCurrentKeyStr : String;
     {-Get the key related to the current record}
    Function GetCurrentDatRef : LongInt;
     {-Get the record number of the current record}
    Procedure SetKeyNr ( Value : Word );
     {-Switch to index Value}
    Function GetBrowseStatus : Boolean; Virtual;
     {-get status of browser}
    Function BuildBrowScreenRow ( Var RR : RowRec ) : Integer;
     {-Build row for record defined by RR}
    Procedure SetLowHighKey ( ALowKey, AHighKey : GenKeyStr );
      {-Set LowKey and HighKey}
    Procedure SetUpdateInterval ( IV : Word );
      {-Update browser screen every IV mseconds}
    {+++++++++ following methods for internal use only +++++++}
    Function lwAdjustHorizOfs ( Delta : Integer) : Integer;
    Procedure lwReinit;
    Procedure lwLineDown;
    Procedure lwLineUp;
    Procedure lwPageDown;
    Procedure lwPageUp;
    Procedure lwFirstPage;
    Procedure lwLastPage;
    Procedure lwMoveToRelPos ( Pos : Word );
    Procedure lwLineRight;
    Procedure lwLineLeft;
    Procedure lwPageRight;
    Procedure lwPageLeft;
    Procedure lwLeftHome;
    Procedure lwRightHome;
    Procedure lwMoveToHorizPos ( Pos : Word );


    Function lwMustUpdateScreen : Boolean; Virtual;
    Procedure lwDrawLinePrim ( S : String; Row : Byte; A : Byte );
  {$IFDEF UseScrollBars}
    Procedure lwSetupForScrollBars;
    Procedure lwUpdateVertScrollBar;
    Procedure lwUpdateHorzScrollBar;
  {$ENDIF}
  {$IFDEF UseMouse}
    Function lwProcessMouseCommand ( Var ibCmd      : Word ) : Boolean;
   {$IFDEF UseDrag}
    Procedure lwClearMouseAutoEvents;
   {$ENDIF}
  {$ENDIF}
  End;

Var
  {$IFDEF UseDrag}
  OpBrCommands : DragProcessor;
  {$ELSE}
  OpBrCommands : CommandProcessor;
  {$ENDIF}

  {===========================================================}

Implementation

  Const
    VertScrollScale = 63;

  Constructor LowWinBrowser.Init
                           (     AOwner               : ISBrowserPtr;
                                 ADrvOrFileBlockPtr   : Pointer;
                                 ANumberOfEltsPerRow  : Word;
                                 ANumberOfRows        : Word;
                                 ALKey,
                                 AHKey                : GenKeyStr;
                                 ASaveStat            : Boolean;
                             Var ADatS;
                                 AIsVarRec            : Boolean );

  Begin
    Owner := AOwner;
    If Not BRHBrowser.Init ( ADrvOrFileBlockPtr, ANumberOfEltsPerRow,
        ANumberOfRows, ALKey, AHKey, ASaveStat, ADatS, AIsVarRec ) Then Fail;
  End;


  Destructor LowWinBrowser.Done;

  Begin
    BRHBrowser.Done;
    Owner := Nil;
  End;


  Function LowWinBrowser.PreCompletePage : Integer;

  Begin
    PreCompletePage := Owner^.PreCompletePage;
  End;


  Function LowWinBrowser.PostCompletePage : Integer;

  Begin
    PostCompletePage := Owner^.PostCompletePage;
  End;


  Function LowWinBrowser.BuildRow ( Var RR  : RowRec ) : Integer;

  Begin
    BuildRow := Owner^.BuildRow ( RR );
  End;


  Function LowWinBrowser.PerformFilter ( Var RR    : RowRec;
                                         Var UseIt : Boolean ) : Integer;

  Begin
    PerFormFilter := Owner^.PerformFilter ( RR, UseIt );
  End;


  Procedure LowWinBrowser.ShowErrorOccured ( Class : Integer );

  Begin
    Owner^.ShowErrorOccured ( Class );
  End;


 {$IFDEF BrUseIsam}
    Constructor ISBrowser.Init (     X1, Y1, X2, Y2       : Byte;
                                     AFileBlockPtr        : IsamFileBlockPtr;
                                     ANumberOfEltsPerRow  : Word;
                                     ANumberOfRows        : Word;
                                     AKeyNr               : Word;
                                     ALKey,
                                     AHKey                : GenKeyStr;
                                     AHeader,
                                     AFooter              : BRLRowEltString;
                                 Var ADatS;
                                     AIsVarRec            : Boolean );
     {-Initialize ISBrowser}
  Begin
    If Not ISBrowser.InitCustom ( X1, Y1, X2, Y2,
                                  DefaultColorSet,
                                  AFileBlockPtr,
                                  ANumberOfEltsPerRow,
                                  ANumberOfRows,
                                  AKeyNr,
                                  ALKey,
                                  AHKey,
                                  AHeader,
                                  AFooter,
                                  ADatS,
                                  AIsVarRec,
                                  DefWindowOptions ) Then Fail;
  End;
{$ENDIF}

{$IFDEF BrUseShell}
    Constructor ISBrowser.Init (     X1, Y1, X2, Y2       : Byte;
                                     ADrvPtr              : IFDriverPtr;
                                     ANumberOfEltsPerRow  : Word;
                                     ANumberOfRows        : Word;
                                     AKeyNr               : Word;
                                     ALKey,
                                     AHKey                : GenKeyStr;
                                     AHeader,
                                     AFooter              : BRLRowEltString );
     {-Initialize ISBrowser}
  Begin
    If Not ISBrowser.InitCustom ( X1, Y1, X2, Y2,
                                  DefaultColorSet,
                                  ADrvPtr,
                                  ANumberOfEltsPerRow,
                                  ANumberOfRows,
                                  AKeyNr,
                                  ALKey,
                                  AHKey,
                                  AHeader,
                                  AFooter,
                                  DefWindowOptions ) Then Fail;
  End;
{$ENDIF}

{$IFDEF BrUseIsam}
    Constructor ISBrowser.InitCustom (     X1, Y1, X2, Y2       : Byte;
                                       Var Colors               : ColorSet;
                                           AFileBlockPtr        : IsamFileBlockPtr;
                                           ANumberOfEltsPerRow  : Word;
                                           ANumberOfRows        : Word;
                                           AKeyNr               : Word;
                                           ALKey,
                                           AHKey                : GenKeyStr;
                                           AHeader,
                                           AFooter              : BRLRowEltString;
                                       Var ADatS;
                                           AIsVarRec            : Boolean;
                                           WinOptions           : LongInt );

     {-Initialize ISBrowser with custom colors and options}
{$ENDIF}

{$IFDEF BrUseShell}
    Constructor ISBrowser.InitCustom (     X1, Y1, X2, Y2       : Byte;
                                       Var Colors               : ColorSet;
                                           ADrvPtr              : IFDriverPtr;
                                           ANumberOfEltsPerRow  : Word;
                                           ANumberOfRows        : Word;
                                           AKeyNr               : Word;
                                           ALKey,
                                           AHKey                : GenKeyStr;
                                           AHeader,
                                           AFooter              : BRLRowEltString;
                                           WinOptions           : LongInt );
     {-Initialize ISBrowser with custom colors and options}

Var
  Dummy : Pointer;
{$ENDIF}

  Var
    PLW : LowWinBrowserPtr;
    Changed : Boolean;

  Begin
  {--Bind the objects}
{$IFDEF BrUseIsam}
    New ( PLW, Init ( @Self, AFileBlockPtr, ANumberOfEltsPerRow,
        ANumberOfRows, ALKey, AHKey, True, ADatS, AIsVarRec ) );
{$ENDIF}

{$IFDEF BrUseShell}
    Dummy := Nil;
    New ( PLW, Init ( @Self, ADrvPtr, ANumberOfEltsPerRow,
        ANumberOfRows, ALKey, AHKey, False, Dummy, False ) );
{$ENDIF}
    If PLW = Nil Then Begin
      InitStatus := epFatal + ecOutOfMemory;
      Fail;
    End;
    If Not CommandWindow.InitCustom ( X1, Y1, X2, Y2, Colors, WinOptions,
        OpBrCommands, ucOPBrowse ) Then Begin
      {-Done already virtually called by RawWindow}
      Fail;
    End;

    lwOptions := DefOpBrOptions;

    PBrowser := PLW;
    PBrowser^.KeyNr := AKeyNr;
    lwHorizOfs := 0;
    lwHeader := AHeader;
    lwFooter := AFooter;

    lwReinit;
    lwVertScale := VertScrollScale;

  {$IFDEF UseScrollBars}
    lwSetupForScrollbars;
  {$ENDIF}

    PBrowser^.SetNrOfRows ( lwFullPage );

    lwUpdateInterval := 0;

    {initialize our own data fields}

    {set video attributes}
    lwDimColor := Colors.TextColor;
    lwDimMono := Colors.TextMono;
    lwHighlightColor := Colors.HighlightColor;
    lwHighlightMono := Colors.HighlightMono;
    lwHeaderFooterColor := Colors.HeaderColor;
    lwHeaderFooterMono := Colors.HeaderMono;
    SetCursor ( cuHidden );
    InitStatus := 0;
  End;


  Destructor ISBrowser.Done;

  Begin
    If PBrowser <> Nil Then Dispose ( PBrowser, Done );
    CommandWindow.Done;
  End;


  Procedure ISBrowser.lwDrawLinePrim ( S : string; Row : Byte; A : Byte );

  Begin
    wFastWrite ( S, Row, 1, A );
  End;


  Procedure ISBrowser.DisplayRow ( I       : Integer;
                                   Inverse : Boolean);
  Var
    S : String;

    Procedure WriteStringOut ( S : String; LineNr : Word; Color : Byte );

    Var
      Y  : Word;
      S1 : String;
      A : Byte;

    Begin
      Case Color Of
       1 : A := ColorMono ( lwHeaderFooterColor, lwHeaderFooterMono );
       2 : A := ColorMono ( lwDimColor, lwDimMono );
       3 : A := ColorMono ( lwHighLightColor, lwHighLightMono );
      End;
      S1 := S;
      FillChar ( S1, 255 , 32 );
      S1 := Copy ( S, Succ ( lwHorizOfs ), 255);
      S1 [0] := #255;
      Y := ( LineNr + lwFirstRow - 1 );
      lwDrawLinePrim ( S1, Y, A );
    End;


  Begin
    If I = 1 Then Begin
      If ( lwHeader <> '' ) Or ( lwFooter <> '' ) Then Begin
        If lwHeader <> '' Then Begin
          WriteStringOut ( lwHeader, 0,  1 );
        End;
        If lwFooter <> '' Then Begin
          WriteStringOut ( lwFooter, Succ ( lwFullPage ),  1 );
        End;
      End;
    End;

    With PBrowser^, BSAPtr^[I]^ Do Begin
      S := Row;
    End;
    If Inverse Then
      WriteStringOut ( S, I,  3 )
    Else
      WriteStringOut ( S, I,  2 );
  End;


  Procedure ISBrowser.UpDateContents;

  Var
    I  : Word;
    CR : Word;

  Begin
    If PBrowser <> Nil Then Begin
      CR := PBrowser^.GetCurRow;
      For I := 1 To lwFullPage Do Begin
        DisplayRow ( I, I = CR);
      End;
    End;
    StackWindow.UpdateContents;
  End;


  Procedure ISBrowser.AdjustWindow ( X1, Y1, X2, Y2 : Word );
  Var
    D : Integer;
  Begin
    D := ( wYH - wYL );
    CommandWindow.AdjustWindow ( X1, Y1, X2, Y2 );
    D := ( wYH - wYL ) - D;
    IF D <> 0 Then Begin
      lwReInit;
      If D > 0 Then PBrowser^.HExpandPage ( lwFullPage ) Else
      If D < 0 Then PBrowser^.HShrinkPage ( lwFullpage );
    End;
    If IsCurrent Then UpdateContents;
  End;




  Procedure ISBrowser.ProcessSelf;
    {-Process browse commands}

  Var
    Finished : Boolean;

  Begin
 {$IFDEF UseScrollBars}
  {--Make sure we're set up for scroll bars}
    lwSetupForScrollBars;
 {$ENDIF}
    {check for pending error}
    cwCmd := ccError;
    If cwGetLastError <> 0 Then
      Exit;

    {Clear any other errors as well}
    ClearErrors;

    {Draw initial screen if not already done}
    Draw;
    If (RawError <> 0) Or (cwGetLastError <> 0) Then Begin
      Exit;
    End;

    Repeat
    {--Get the next command}
      Finished := False;
      ProcessPreCommand;
      If lwMustUpdateScreen Then Begin
        SetLastCommand ( bcUpdate )
      End Else Begin
        GetNextCommand;
      End;
   {$IFDEF UseMouse}
      If cwCmd in [ ccMouseSel,          {Mouse selection - left button up}
                    ccMouseDown,         {Mouse left button down}
                    ccMouseAuto ] Then   {Mouse moved with left down or autorepeat}
      Begin
        Finished := lwProcessMouseCommand ( cwCmd );
      End;
   {$ENDIF}

      Case cwCmd Of
        ccNone : ;
        ccChar : CharHandler;
        ccError : Begin
          Finished := True;
        End;
        ccUp : lwLineUp;
        ccDown : lwLineDown;
        ccPageUp : lwPageUp;
        ccPageDn : lwPageDown;
        ccTopOfFile : lwFirstPage;
        ccEndOfFile : lwLastPage;
        ccLeft : lwLineLeft;
        ccRight : lwLineRight;
        ccWordLeft : lwPageLeft;
        ccWordRight : lwPageRight;
        ccHome : lwLeftHome;
        ccEnd : lwRightHome;
        ccHelp : RequestHelp ( wHelpIndex );
        bcUpdate : UpdateBrowserScreen;
        ccSelect,
        ccQuit,
        ccUser0..ccUser65335 :
          Finished := True;

        Else If ( cwCmd <= 255 ) And ( GetExitCommandPtr <> Nil ) Then Begin
          Finished :=  ( cwCmd In GetExitCommandPtr^ );
        End;
      End; {Case}
      ProcessPostCommand;
    Until Finished;
    rwSaveWindowState;
      {-Save window state}
  End;

  Procedure ISBrowser.CharHandler;

  Begin
  End;


  Procedure ISBrowser.SetUpdateInterval ( IV : Word );
  Begin
    lwUpdateInterval := IV;
  End;


  Function ISBrowser.lwMustUpdateScreen : Boolean;
  var
    T : LongInt;
  Begin
    lwMustUpdateScreen := False;
{$IFDEf BRUseShell}
    If ( IFDNetSupported = Nonet ) Or
{$ENDIF}
{$IFDEf BRUseIsam}
    If ( BTNetSupported = Nonet ) Or
{$ENDIF}
       ( lwUpdateInterval = 0 ) Or
       ( lwOptionsAreOn ( lwSuppressUpdate )) Then Exit;
    T := TimeMS;
    {wait until key pressed}
    While Not cwCmdPtr^.cpKeyPressed Do Begin
      {is it time to check again?}
      If ( TimeMS - T ) >= lwUpdateInterval Then Begin
        lwMustUpdateScreen := True;
        Exit;
      End;
    End;
  End;


  Procedure ISBrowser.SetLowHighKey ( ALowKey, AHighKey : GenKeyStr );

  Begin
    With PBrowser^ Do Begin
      LowKey := ALowKey;
      HighKey := AHighKey;
    End;
  End;


{$IFDEF UseScrollBars}
  Procedure ISBrowser.lwSetupForScrollBars;
    {-Set boundaries for all scroll bars}
  Begin
    ChangeAllScrollBars ( 0, lwMaxHorizOfs, 0, lwVertScale );
  End;
{$ENDIF}

  Procedure ISBrowser.lwReinit;
    {-Initialize variables that can change if window is resized}

  Var
    MaxWidth,
        AWidth : Word;

  Begin
    lwFullPage := Succ ( wYH - wYL );
    lwFirstRow := 1;
    If Trim ( lwHeader ) <> '' Then Begin
      Dec (lwFullPage);
      Inc (lwFirstRow);
    End;
    If Trim ( lwFooter ) <> '' Then Dec (lwFullPage);
    AWidth := Succ ( wXH - wXL );
    MaxWidth := MaxCols;
    If AWidth > MaxWidth Then Begin
      lwMaxHorizOfs := 0;
    End Else Begin
      lwMaxHorizOfs := MaxWidth - AWidth;
    End;

    {$IFDEF UseScrollBars}
      lwSetupForScrollBars;
    {$ENDIF}
  End;



{$IFDEF UseMouse}
  {$IFDEF UseDrag}
    Procedure ISBrowser.lwClearMouseAutoEvents;
    Var
      EventT, QH : Word;
      X,Y        : Byte;
    Begin
      Repeat
        If Not MouseEventsPending Then Exit;
        QH := PeekMouseEvent ( X, Y );
        If QH <> MouseLftAuto Then Exit;
        QH := ReadMouseEvent ( X, Y );
      Until True;
    End;
{$ENDIF}

  Function ISBrowser.lwProcessMouseCommand ( Var ibCmd      : Word )
                                                                   : Boolean;
    {-Process ccMouseSel command. Returns True to return control to user.}
  Const
    LastMouseDown : Integer = 0;
  Var
    L         : LongInt;
    FramePos  : FramePosType;
    HotCode,
    Y         : Byte;
    Dragging  : Boolean;
    AllDone   : Boolean;
    OldCurRow : Integer;

  Begin
    lwProcessMouseCommand := False;
    AllDone := False;
    OldCurrow := PBrowser^.GetCurrow;

    {determine position of mouse}
    L := cwMouseResults ( ibCmd, FramePos, HotCode );

    {Should mouse event be ignored?}
    if cwIgnoreMouseEvent(Dragging, ibCmd, FramePos, HotCode) then Begin
      If ( ibCmd = ccMouseSel ) Then
        lwProcessMouseCommand := True;
      ibCmd := ccNone;
      LastMouseDown := 0;
      Exit;
    End;

    if HotCode = hsNone then begin
      {Not a hot spot}
      if FramePos = frInsideActive then begin
        {Inside active window}
        If ibCmd in [ ccMouseSel, ccMouseDown, ccMouseAuto ] Then Begin
          With PBrowser^ Do Begin
            Y := MouseKeyWordY + MouseYLo - wYL + 1;
            If ibCmd = ccMouseDown Then
              LastMouseDown := GetCurRow;
            If ( ( Y = 1 ) And ( Trim ( lwHeader ) > '') )
                Or ( ( Y = Succ (wYH - wYL) ) And ( Trim ( lwFooter ) > '') )
                Then Begin
              If ibCmd = ccMouseSel Then ibCMD := ccNone;
            End Else Begin
              Y := Y + 1 - lwFirstRow;
              PBrowser^.SetCurRow ( Y );
              If ( OldCurRow <> PBrowser^.GetCurRow ) And
                    ( PBrowser^.GetCurRow <> 0 ) Then Begin
                DisplayRow ( OldCurRow, False );
                DisplayRow ( PBrowser^.GetCurRow, True );
                lwUpDateVertScrollBar;
              End Else Begin
                If ( ibCmd = ccMouseSel ) Then Begin
                  If ( GetCurRow = LastMouseDown ) And
                     ( LastMouseDown <> 0 ) And
                     ( lwOptionsAreOn ( lwSelectOnClick )) Then Begin
                    ibCmd := ccSelect;
                  End;
                End;
              End;
            End;
          End;
          AllDone := ( ibCmd = ccSelect ) Or AllDone;
        End;

      End else if ( not Dragging ) or ( ibCmd = ccMouseDown ) Then Begin
        {Button was pressed outside of active window, not on a hot spot}
        If LongFlagIsSet ( wFlags, wAllMouseEvents ) Then Begin
          AllDone := True;
        End Else Begin
          AllDone := False;
        End;
      End;
    End;

    Case HotCode Of
      hsNone :;          {not a hot spot, do nothing}
      {$IFDEF UseScrollBars}
      hsDecV : Begin     {the decrement fixture of a vertical scroll bar}
        If ibCMD = ccMouseSel Then
          ibCMD := ccNone
        Else
          ibCMD := ccUp;
      End;
      hsDecH : Begin     {the decrement fixture of a horizontal scroll bar}
        If ibCMD = ccMouseSel Then
          ibCMD := ccNone
        Else
          ibCmd := ccLeft;
      End;
      hsIncV : Begin     {the increment fixture of a vertical scroll bar}
       If ibCMD = ccMouseSel Then
          ibCMD := ccNone
        Else
          ibCmd := ccDown;
      End;
      hsIncH : Begin     {the increment fixture of a horizontal scroll bar}
        If ibCMD = ccMouseSel Then
          ibCMD := ccNone
        Else
          ibCmd := ccRight;
      End;
      hsBar : Begin      {the slider portion of a scroll bar}
        Case FramePos Of
          frLL, frRR : Begin  {vertical scroll bar}
            L := TweakSlider ( FramePos, MouseKeyWordY+MouseYLo, L, 1 );
            lwMoveToRelPos ( L );

          End;
          Else Begin     {horizontal scroll bar}
            With PBrowser^ Do Begin
              L := TweakSlider ( FramePos, MouseKeyWordX+MouseXLo, L, 1 );
              lwMoveToHorizPos ( L );
            End;
            AllDone := False;
          End;
        End;
        ibCmd := ccNone;
      End;
      {$ENDIF}
      hsSpot,            {a single character hot spot}
      hsRegion0..255 : Begin  {a user-defined region relative to a frame}
        AllDone := True;
      End;
    End; {Case}
    {$IFDEF UseDrag}
    lwClearMouseAutoEvents;
    {$ENDIF}
    lwProcessMouseCommand := AllDone;
  End;
{$ENDIF}


  Procedure ISBrowser.lwOptionsOn ( OptionFlags : Word );
    {-Activate multiple options}
  Begin
    lwOptions := lwOptions Or ( OptionFlags And Not BadOPBrOptions );
  End;


  Procedure ISBrowser.lwOptionsOff ( OptionFlags : Word );
    {-Deactivate multiple options}
  Begin
    lwOptions := lwOptions And Not ( OptionFlags And Not BadOPBrOptions );
  End;


  Function ISBrowser.lwOptionsAreOn ( OptionFlags : Word ) : Boolean;
    {-Return true if all specified options are on}
  Begin
    lwOptionsAreOn := lwOptions And OptionFlags = OptionFlags;
  End;


  Procedure ISBrowser.SetDimAttr ( Color, Mono : Byte );
    {-Set attributes for dim characters}
  Begin
    lwDimColor := Color;
    lwDimMono := MapMono ( Color, Mono );
  End;


  Procedure ISBrowser.SetHighlightAttr ( Color, Mono : Byte );
    {-Set attributes for highlighted characters}
  Begin
    lwHighlightColor := Color;
    lwHighlightMono := MapMono ( Color, Mono );
  End;


  Procedure ISBrowser.SetHeaderFooterAttr ( Color, Mono : Byte );
    {-Set attributes for Header characters}
  Begin
    lwHeaderFooterColor := Color;
    lwHeaderFooterMono := MapMono ( Color, Mono );
  End;


  Procedure ISBrowser.SetHeaderFooter ( AHeader, AFooter
                                           : BRLRowEltString );

  Var
    OldNrOfRows : Word;

  Begin
    lwHeader := AHeader;
    lwFooter := AFooter;
    If PBrowser = Nil Then Exit;
    OldNrOfRows := lwFullPage;
    lwReinit;
    With PBrowser^ Do Begin
      If OldNrOfRows > lwFullPage Then Begin
        HShrinkPage ( lwFullPage );
      End Else Begin
        If OldNrOfRows < lwFullPage Then Begin
          HExpandPage ( lwFullPage );
       End;
      End;
      If IsCurrent Then UpdateContents;
    End;
  End;

  Function ISBrowser.PreCompletePage : Integer;

  Begin
    PreCompletePage := NoError;
  End;

  Function ISBrowser.PostCompletePage : Integer;

  Begin
    PostCompletePage := NoError;
  End;

  Function ISBrowser.BuildRow ( Var RR  : RowRec ) : Integer;

  Begin
    Abstract;
  End;

  Function ISBrowser.PerformFilter ( Var RR    : RowRec;
                                     Var UseIt : Boolean ) : Integer;

  Begin
    UseIt := True;
    PerformFilter := NoError;
  End;

  Procedure ISBrowser.ShowErrorOccured ( Class : Integer );

  Begin
  End;


  Procedure ISBrowser.ProcessPostCommand;

  Begin
  End;


  Procedure ISBrowser.ProcessPreCommand;

  Begin
  End;


  Procedure ISBrowser.SetAndUpdateBrowserScreen ( NewKeyStr : GenKeyStr;
                                                  NewRef    : LongInt );

  Begin
    With PBrowser^ Do Begin
      HBuildNewPage ( KeyNr, NewKeyStr, NewRef, GetCurRow, NrOfRows );
      If IsCurrent Then UpdateContents;
{$IFDEF UseScrollBars}
      If StatusOK Then Begin
        lwUpdateVertScrollBar;
        lwUpdateHorzScrollBar;
      End;
{$ENDIF}
    End;
  End;

  Procedure ISBrowser.UpdateBrowserScreen;

  Var
    Changed : Boolean;

  Begin
    With PBrowser^ Do Begin
      HBuildThisPage ( Changed );
      If Changed Then Begin
        If IsCurrent Then UpdateContents;
{$IFDEF UseScrollBars}
        If StatusOK Then lwUpdateVertScrollBar;
{$ENDIF}
      End;
    End;
  End;

  Function ISBrowser.GetCurrentRec ( Var Match : Boolean ) : Integer; {mod !!.03}

  Var
    Result : Integer;

  Begin
    With PBrowser^ Do Begin
      Result := GetRowMatchingRec ( BSAPtr^[GetCurRow]^, True, True,
          Match );
    End;
    GetCurrentRec := Result;
  End;


  Function ISBrowser.GetThisRec ( Var RR : RowRec ) : Integer;

  Begin
    GetThisRec := PBrowser^.BRGetRec ( RR, False, False );
  End;


  Function ISBrowser.GetCurrentKeyNr : Word;

  Begin
    GetCurrentKeyNr := PBrowser^.KeyNr;
  End;


  Function ISBrowser.GetCurrentKeyStr : String;

  Begin
    GetCurrentKeyStr := PBrowser^.GetCurrentKeyStr;
  End;


  Function ISBrowser.GetCurrentDatRef : LongInt;

  Begin
    GetCurrentDatRef := PBrowser^.GetCurrentDatRef;
  End;


  Procedure ISBrowser.SetKeyNr ( Value : Word );

  Begin
    PBrowser^.KeyNr := Value;
  End;

  Function ISBrowser.GetBrowseStatus : Boolean;

  Begin
    GetBrowseStatus := PBrowser^.StatusOK;
  End;

  Function ISBrowser.BuildBrowScreenRow ( Var RR : RowRec ) : Integer;

  Begin
    BuildBrowScreenRow := PBrowser^.BuildBrowScreenRow ( RR );
  End;


  Function ISBrowser.lwAdjustHorizOfs ( Delta : Integer ) : Integer;

  Var
    Offset : LongInt;

  Begin
    Offset := LongInt (lwHorizOfs) + Delta;
    If Offset < 0 Then Begin
      Offset := 0;
    End Else Begin
      If OffSet > lwMaxHorizOfs Then Begin
        Offset := lwMaxHorizOfs;
      End;
    End;
    lwAdjustHorizOfs := Offset;
  End;


{$IFDEF UseScrollBars}
  Procedure ISBrowser.lwUpdateVertScrollBar;
    {-Update vertical scroll bar}

  Var
    RelPos : Word;
    Dummy  : Integer;

  Begin
    With PBrowser^ Do Begin
      HGetApprRelPos ( RelPos, lwVertScale, GetCurrentKeyStr,
          GetCurrentDatRef );
      If Not StatusOK Then RelPos := 0;
      DrawSlider ( frRR, RelPos );
    End;
  End;

  Procedure ISBrowser.lwUpdateHorzScrollBar;
    {-Update horizontal scroll bar}

  Var
    Dummy : Integer;

  Begin
    If lwMaxHorizOfs > 0 Then Begin
      DrawSlider ( frBB, lwHorizOfs );
    End;
  End;
{$ENDIF}


  Procedure ISBrowser.lwLineDown;

  Var
    LRow  : Word;
    Moved : Word;
    CR    : Word;
    Update,
    Dummy : Boolean;

  Begin
    Update := True;
    With PBrowser^ Do Begin
      LRow := GetLastRow;                                         {mod !!.03}
      CR := GetCurRow;
      If ( CR = LRow ) Or ( LRow = 0 ) Then Begin
        HBuildNextPage ( 1, Moved, True, 0, Dummy );
        If StatusOK Then Begin
          If OtherAction Then Begin
            UpdateContents;
          End Else Begin
            If Moved = 1 Then Begin
              If lwFullPage <> 1 Then Begin
                If LRow = GetLastRow Then Begin                   {mod !!.03}
                  UpDateContents;
                End Else Begin
                  SetCurRow ( Succ (CR) );
                  DisplayRow ( LRow, False);
                End;
              End;
              DisplayRow ( CR, True);
            End Else Begin
              Update := False;
            End;
          End;
        End Else Begin
          Update := False;
        End;
      End Else Begin
        DisplayRow ( CR , False );
        SetCurRow ( Succ (CR) );
        DisplayRow ( GetCurRow, True );
      End;
    End;
{$IFDEF UseScrollBars}
    If Update Then lwUpdateVertScrollBar;
{$ENDIF}
  End;


  Procedure ISBrowser.lwLineUp;

  Var
    Moved : Word;
    CR    : Word;
    Update,
    Dummy : Boolean;

  Begin
    Update := True;
    With PBrowser^Do Begin
      CR := GetCurRow;
      If CR = 1 Then Begin
        HBuildPrevPage ( 1, Moved, True, 0, Dummy );
        If StatusOK Then Begin
          If OtherAction Then Begin
            UpdateContents;
          End Else Begin
            If Moved = 1 Then Begin
              If lwFullPage <> 1 Then Begin
                UpdateContents;
              End;
              DisplayRow ( 1, True);
            End Else Begin
              Update := False;
            End;
          End;
        End Else Begin
          Update := False;
        End;
      End Else Begin
        DisplayRow ( CR , False );
        SetCurRow ( Pred (CR) );
        DisplayRow ( GetCurRow, True );
      End;
    End;
{$IFDEF UseScrollBars}
    If Update Then lwUpdateVertScrollBar;
{$ENDIF}
  End;


  Procedure ISBrowser.lwPageDown;

  Var
    BST     : BrowScreenState;
    Moved   : Word;
    Update,
    Changed : Boolean;

  Begin
    Update := False;
    With PBrowser^ Do Begin
      GetBrowScreenState ( BST );
      HBuildNextPage ( lwFullPage, Moved, True, 1, Changed );
      If StatusOK Then Begin
        If OtherAction Or (Moved > 0) Or Changed
            Or BrowScreenStateChanged ( BST ) Then Begin
          UpdateContents;
          Update := True;
        End;
      End;
    End;
{$IFDEF UseScrollBars}
    If Update Then lwUpdateVertScrollBar;
{$ENDIF}
  End;


  Procedure ISBrowser.lwPageUp;

  Var
    BST     : BrowScreenState;
    Moved   : Word;
    Update,
    Changed : Boolean;

  Begin
    Update := False;
    With PBrowser^ Do Begin
      GetBrowScreenState ( BST );
      HBuildPrevPage ( lwFullPage, Moved, True, 1, Changed );
      If StatusOK Then Begin
        If OtherAction Or (Moved > 0) Or Changed
            Or BrowScreenStateChanged ( BST ) Then Begin
          UpdateContents;
          Update := True;
        End;
      End;
    End;
{$IFDEF UseScrollBars}
    If Update Then lwUpdateVertScrollBar;
{$ENDIF}
  End;


  Procedure ISBrowser.lwFirstPage;

  Var
    BST     : BrowScreenState;
    Update,
    Changed : Boolean;

  Begin
    Update := False;
    With PBrowser^ Do Begin
      GetBrowScreenState ( BST );
      HBuildFirstPage ( Changed );
      If StatusOK Then Begin
        If Changed Or BrowScreenStateChanged ( BST ) Then Begin
          UpdateContents;
          Update := True;
        End;
      End;
    End;
{$IFDEF UseScrollBars}
    If Update Then lwUpdateVertScrollBar;
{$ENDIF}
  End;


  Procedure ISBrowser.lwLastPage;

  Var
    BST     : BrowScreenState;
    Update,
    Changed : Boolean;

  Begin
    Update := False;
    With PBrowser^ Do Begin
      GetBrowScreenState ( BST );
      HBuildLastPage ( Changed );
      If StatusOK Then Begin
        If Changed Or BrowScreenStateChanged ( BST ) Then Begin
          UpdateContents;
          Update := True;
        End;
      End;
    End;
{$IFDEF UseScrollBars}
    If Update Then lwUpdateVertScrollBar;
{$ENDIF}
  End;


  Procedure ISBrowser.lwMoveToRelPos ( Pos : Word );

  Var
    Key : GenKeyStr;
    Ref : LongInt;

  Begin
    PBrowser^.HGetApprKeyAndRef ( Pos, lwVertScale, Key, Ref );
    SetAndUpdateBrowserScreen ( Key, Ref );
  End;


  Procedure ISBrowser.lwLineRight;

  Var
    OldHOfs : Integer;

  Begin
    OldHOfs := lwHorizOfs;
    lwHorizOfs := lwAdjustHorizOfs ( 1 );
    If OldHOfs <> lwHorizOfs Then Begin
      UpdateContents;
{$IFDEF UseScrollBars}
      lwUpdateHorzScrollBar;
{$ENDIF}
    End;
  End;


  Procedure ISBrowser.lwLineLeft;

  Var
    OldHOfs : Integer;

  Begin
    OldHOfs := lwHorizOfs;
    lwHorizOfs := lwAdjustHorizOfs ( -1 );
    If OldHOfs <> lwHorizOfs Then Begin
      UpdateContents;
{$IFDEF UseScrollBars}
      lwUpdateHorzScrollBar;
{$ENDIF}
    End;
  End;


  Procedure ISBrowser.lwPageRight;

  Var
    OldHOfs : Integer;

  Begin
    OldHOfs := lwHorizOfs;
    lwHorizOfs := lwAdjustHorizOfs ( 10 );
    If OldHOfs <> lwHorizOfs Then Begin
      UpdateContents;
{$IFDEF UseScrollBars}
      lwUpdateHorzScrollBar;
{$ENDIF}
    End;
  End;


  Procedure ISBrowser.lwPageLeft;

  Var
    OldHOfs : Integer;

  Begin
    OldHOfs := lwHorizOfs;
    lwHorizOfs := lwAdjustHorizOfs ( -10 );
    If OldHOfs <> lwHorizOfs Then Begin
      UpdateContents;
{$IFDEF UseScrollBars}
      lwUpdateHorzScrollBar;
{$ENDIF}
    End;
  End;


  Procedure ISBrowser.lwLeftHome;

  Var
    OldHOfs : Integer;

  Begin
    OldHOfs := lwHorizOfs;
    lwHorizOfs := 0;
    If OldHOfs <> lwHorizOfs Then Begin
      UpdateContents;
{$IFDEF UseScrollBars}
      lwUpdateHorzScrollBar;
{$ENDIF}
    End;
  End;


  Procedure ISBrowser.lwRightHome;

  Var
    OldHOfs : Integer;

  Begin
    If PBrowser = Nil Then Exit;
    OldHOfs := lwHorizOfs;
    lwHorizOfs := lwMaxHorizOfs;
    If OldHOfs <> lwHorizOfs Then Begin
      UpdateContents;
{$IFDEF UseScrollBars}
      lwUpdateHorzScrollBar;
{$ENDIF}
    End;
  End;


  Procedure ISBrowser.lwMoveToHorizPos ( Pos : Word );

  Var
    OldHOfs : Integer;

  Begin
    OldHOfs := lwHorizOfs;
    lwHorizOfs := Pos;
    lwHorizOfs := lwAdjustHorizOfs ( 0 );
    If OldHOfs <> lwHorizOfs Then Begin
      UpdateContents;
{$IFDEF UseScrollBars}
      lwUpdateHorzScrollBar;
{$ENDIF}
    End;
  End;



Begin
{--Initialize command processor}
  OpBrCommands.Init ( @OpBrKeySet, OPBRKeyMax );
End.

