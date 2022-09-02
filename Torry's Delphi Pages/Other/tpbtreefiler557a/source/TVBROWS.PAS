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

Unit TVBRows;

{$I BrDefOpt.Inc}

Interface

Uses
  Objects,
  Drivers,
  Views,
{$IFDEF BrUseIsam}
  Filer,     {!!.TP}
{$ENDIF}
{$IFDEF BrUseShell}
  OOPShell,
{$ENDIF}
  LowBrows,
  MedBrows,
  HiBrows;


Const
  CBrowserView = #29#27#28;
  CBInterior = #2#6#7;

Type
  PBrowserScrollbar = ^TBrowserScrollBar;
  LowWinBrowserPtr = ^LowWinBrowser;
  PBrowserWindow = ^TBrowserWindow;
  PBrowserView = ^TBrowserView;

  TBrowserScrollBar = Object ( TScrollBar )
    Function ScrollStep ( Part: Integer ): Integer; Virtual;
  End;

  LowWinBrowser = Object ( BRHBrowser )
    Owner : PBrowserView;

    Constructor Init (     AOwner               : PBrowserView;
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

  TBrowserView = Object ( TView )
    PBrowser      : LowWinBrowserPtr;
    PHScrollBar,
      PVScrollBar : PBrowserScrollBar;

    lwFullPage,
    lwFirstRow,
    lwMaxHorizOfs,
    lwVertScale   : Word;
    lwHorizOfs    : Integer;

    lwHeader,
    lwFooter      : BRLRowEltString;

{$IFDEF BrUseIsam}
    Constructor Init( Var Bounds               : TRect;
                          PHS, PVS             : PBrowserScrollBar;
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
{$ENDIF}
{$IFDEF BrUseShell}
    Constructor Init( Var Bounds               : TRect;
                          PHS, PVS             : PBrowserScrollBar;
                          ADrvPtr              : IFDriverPtr;
                          ANumberOfEltsPerRow  : Word;
                          ANumberOfRows        : Word;
                          AKeyNr               : Word;
                          ALKey,
                          AHKey                : GenKeyStr;
                          AHeader,
                          AFooter              : BRLRowEltString );
{$ENDIF}

    Destructor Done; Virtual;

    Function PreCompletePage : Integer; Virtual;
    Function PostCompletePage : Integer; Virtual;

    Function GetPalette: PPalette; Virtual;

    Procedure HandleEvent(var Event: TEvent); Virtual;

    Procedure Draw; Virtual;

    Procedure ChangeBounds ( Var Bounds : TRect ); Virtual;

    Procedure DisplayRow ( I       : Integer;
                           Inverse : Boolean ); Virtual;

    Procedure SetAndUpdateBrowserScreen ( NewKeyStr : GenKeyStr;
                                          NewRef    : LongInt );

  {--The following functions must be overwritten in descending objects}
    Function BuildRow ( Var RR    : RowRec ) : Integer; Virtual;

  {--The following routines may be overwritten in descending objects}
    Function PerformFilter ( Var RR    : RowRec;
                             Var UseIt : Boolean ) : Integer; Virtual;

    Procedure ShowErrorOccured ( Class : Integer ); Virtual;

  {--The following routines may be called from outside}
    Procedure SetHeaderFooter ( AHeader,
                                AFooter : BRLRowEltString );


    Procedure SetLowHighKey ( ALowKey, AHighKey : GenKeyStr );

    Procedure UpdateBrowserScreen;
    Function GetThisRec ( Var RR : RowRec ) : Integer;
    Function GetCurrentRec ( Var Match : Boolean ) : Integer;     {mod !!.03}
    Function GetCurrentKeyNr : Word;
    Function GetCurrentKeyStr : String;
    Function GetCurrentDatRef : LongInt;
    Procedure SetKeyNr ( Value : Word );
    Function GetBrowseStatus : Boolean; Virtual;
    Function BuildBrowScreenRow ( Var RR : RowRec ) : Integer;

  Private
  {--The following routines are internal use only}
    Function lwAdjustHorizOfs ( Delta : Integer) : Integer;
    Procedure lwUpdateVertScrollBar;
    Procedure lwUpdateHorzScrollBar;
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
    Procedure lwMouseClicked ( Var Event : TEvent );
  End;


  PBInterior = ^TBInterior;
  TBInterior = Object ( TBrowserView )

{$IFDEF BrUseIsam}
    Constructor Init( Var Bounds               : TRect;
                          PHS, PVS             : PBrowserScrollBar;
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
{$ENDIF}
{$IFDEF BrUseShell}
    Constructor Init( Var Bounds               : TRect;
                          PHS, PVS             : PBrowserScrollBar;
                          ADrvPtr              : IFDriverPtr;
                          ANumberOfEltsPerRow  : Word;
                          ANumberOfRows        : Word;
                          AKeyNr               : Word;
                          ALKey,
                          AHKey                : GenKeyStr;
                          AHeader,
                          AFooter              : BRLRowEltString );
{$ENDIF}

    Destructor Done; Virtual;

    Function GetPalette: PPalette; Virtual;

  {--The following functions may be overwritten in descending objects}
    Function PreCompletePage : Integer; Virtual;
    Function PostCompletePage : Integer; Virtual;

  {--The following functions must be overwritten in descending objects}
    Function BuildRow ( Var RR    : RowRec ) : Integer; Virtual;

  {--The following routines may be overwritten in descending objects}
    Function PerformFilter ( Var RR    : RowRec;
                             Var UseIt : Boolean ) : Integer; Virtual;

    Procedure ShowErrorOccured ( Class : Integer ); Virtual;

  End;



  TBrowserWindow = Object ( TWindow )
    PInterior : PBrowserView;
  {$IFDEF BRUseIsam}
    Constructor Init ( var Bounds               : TRect;
                           ATitle               : TTitleStr;
                           ANumber              : Integer;
                           ADrvOrFileBlockPtr   : Pointer;
                           ANumberOfEltsPerRow  : Word;
                           ANumberOfRows        : Word;
                           AKeyNr               : Word;
                           ALKey,
                           AHKey                : GenKeyStr;
                           AHeader,
                           AFooter              : BRLRowEltString;
                       Var ADatS;
                           AIsVarRec            : Boolean );

{$ENDIF}

{$IFDEF BRUseShell}
    Constructor Init ( Var Bounds               : TRect;
                           ATitle               : TTitleStr;
                           ANumber              : Integer;
                           ADrvOrFileBlockPtr   : Pointer;
                           ANumberOfEltsPerRow : Word;
                           ANumberOfRows        : Word;
                           AKeyNr               : Word;
                           ALKey,
                           AHKey                : GenKeyStr;
                           AHeader,
                           AFooter              : BRLRowEltString );
{$ENDIF}

    Destructor Done; Virtual;

    Function MakeBrowserScrollBar ( AOptions: Word ): PBrowserScrollBar;

    Procedure MakeInterior (     Bounds: TRect;
                                 ADrvOrFileBlockPtr   : Pointer;
                                 ANumberOfEltsPerRow : Word;
                                 ANumberOfRows        : Word;
                                 AKeyNr               : Word;
                                 ALKey,
                                 AHKey                : GenKeyStr;
                                 AHeader,
                                 AFooter              : BRLRowEltString;
                             Var ADatS;
                                 AIsVarRec            : Boolean;
                                 AIsIsam              : Boolean );

  {--The following functions may be overwritten in descending objects}
    Function PreCompletePage : Integer; Virtual;
    Function PostCompletePage : Integer; Virtual;

  {--The following functions must be overwritten in descending objects}
    Function BuildRow ( Var RR    : RowRec ) : Integer; Virtual;

  {--The following routines may be overwritten in descending objects}
    Function PerformFilter ( Var RR    : RowRec;
                             Var UseIt : Boolean ) : Integer; Virtual;

    Procedure ShowErrorOccured ( Class : Integer ); Virtual;

  {--The following routines may be called from outside}
    Procedure SetHeaderFooter ( AHeader,
                                AFooter : BRLRowEltString );


    Procedure SetAndUpdateBrowserScreen ( NewKeyStr : GenKeyStr;
                                          NewRef    : LongInt );
    Procedure UpdateBrowserScreen;
    Function GetThisRec ( Var RR : RowRec ) : Integer;
    Function GetCurrentRec ( Var Match : Boolean ) : Integer;     {mod !!.03}
    Function GetCurrentKeyNr : Word;
    Function GetCurrentKeyStr : String;
    Function GetCurrentDatRef : LongInt;
    Procedure SetKeyNr ( Value : Word );
    Function GetBrowseStatus : Boolean; Virtual;
    Function BuildBrowScreenRow ( Var RR : RowRec ) : Integer;
    Procedure SetLowHighKey ( ALowKey, AHighKey : GenKeyStr );
  End;

Implementation

  Const
    VertScrollScale = 63;

(**************************************************************************)

  Function TBrowserScrollBar.ScrollStep ( Part: Integer ): Integer;

  Begin
    ScrollStep := 0;
    Case Part of
      sbLeftArrow  :
        Begin
          If Message ( Owner, evKeyDown, kbLeft, @Self ) = Nil Then;
        End;
      sbRightArrow :
        Begin
          If Message ( Owner, evKeyDown, kbRight, @Self ) = Nil Then;
        End;
      sbPageLeft   :
        Begin
          If Message ( Owner, evKeyDown, kbCtrlLeft, @Self ) = Nil Then;
        End;
      sbPageRight  :
        Begin
          If Message ( Owner, evKeyDown, kbCtrlRight, @Self ) = Nil Then;
        End;
      sbUpArrow    :
        Begin
          If Message ( Owner, evKeyDown, kbUp, @Self ) = Nil Then;
        End;
      sbDownArrow  :
        Begin
          If Message ( Owner, evKeyDown, kbDown, @Self ) = Nil Then;
        End;
      sbPageUp     :
        Begin
          If Message ( Owner, evKeyDown, kbPgUp, @Self ) = Nil Then;
        End;
      sbPageDown   :
        Begin
          If Message ( Owner, evKeyDown, kbPgDn, @Self ) = Nil Then;
        End;
    End;
  End;


  Constructor LowWinBrowser.Init
                           (     AOwner               : PBrowserView;
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

{$IFDEF BRUseIsam}
  Constructor TBrowserView.Init( Var Bounds               : TRect;
                                     PHS, PVS             : PBrowserScrollBar;
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


{$ENDIF}
{$IFDEF BrUseShell}
    Constructor TBrowserView.Init( Var Bounds               : TRect;
                                       PHS, PVS             : PBrowserScrollBar;
                                       ADrvPtr              : IFDriverPtr;
                                       ANumberOfEltsPerRow  : Word;
                                       ANumberOfRows        : Word;
                                       AKeyNr               : Word;
                                       ALKey,
                                       AHKey                : GenKeyStr;
                                       AHeader,
                                       AFooter              : BRLRowEltString );

Var
  Dummy : Pointer;
{$ENDIF}

  Var
    PLW : LowWinBrowserPtr;

  Begin
    TView.Init( Bounds );
    PHScrollBar := PHS;
    PVScrollBar := PVS;
    Eventmask := evMouseDown + evMouseMove +
                 evKeyDown + evBroadcast + evCommand;

    lwHeader := AHeader;
    lwFooter := AFooter;

    lwHorizOfs := 0;

{$IFDEF BrUseIsam}
    New ( PLW, Init ( @Self, AFileBlockPtr, ANumberOfEltsPerRow,
        ANumberOfRows, ALKey, AHKey, True, ADatS, AIsVarRec ) );
{$ENDIF}

{$IFDEF BrUseShell}
    Dummy := Nil;
    New ( PLW, Init ( @Self, ADrvPtr, ANumberOfEltsPerRow,
        ANumberOfRows, ALKey, AHKey, False, Dummy, False ) );
{$ENDIF}
    If PLW = Nil Then Fail;

    PBrowser := PLW;
    PBrowser^.KeyNr := AKeyNr;

    lwReinit;
    lwVertScale := VertScrollScale;
    Options := Options or ofSelectable;

    If PHScrollBar <> Nil Then
      PHScrollBar^.SetParams ( 0, 0, lwMaxHorizOfs, 10, 1 );
    If PVScrollBar <> Nil Then
      PVScrollBar^.SetParams ( 0, 0, lwVertScale, 10, 1 );

    PBrowser^.SetNrOfRows ( lwFullPage );
  End;


  Destructor TBrowserView.Done;

  Begin
    If PBrowser <> Nil Then Dispose ( PBrowser, Done );
    TView.Done;
  End;


  Function TBrowserView.PreCompletePage : Integer;

  Begin
    PreCompletePage := NoError;
  End;

  Function TBrowserView.PostCompletePage : Integer;

  Begin
    PostCompletePage := NoError;
  End;

  Function TBrowserView.GetPalette: PPalette;

  Const
    P: String[Length(CBrowserView)] = CBrowserView;

  Begin
    GetPalette := @P;
  End;


  Procedure TBrowserView.DisplayRow ( I       : Integer;
                                      Inverse : Boolean);
  Var
    S : String;

    Procedure WriteStringOut ( S : String; LineNr : Word; Color : Byte );

    Var
      Y  : Word;
      S1 : String;

    Begin
      S1 := S;
      FillChar ( S1, 255 , 32 );
      S1 := Copy ( S, Succ ( lwHorizOfs ), 255);
      S1 [0] := #255;
      Y := ( LineNr + lwFirstRow - 2 );
      WriteStr ( 0, Y, S1, Color );
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


  Procedure TBrowserView.SetAndUpdateBrowserScreen ( NewKeyStr : GenKeyStr;
                                                     NewRef    : LongInt );

  Begin
    With PBrowser^ Do Begin
      HBuildNewPage ( KeyNr, NewKeyStr, NewRef, GetCurRow, NrOfRows );
      DrawView;
      If StatusOK Then lwUpdateVertScrollBar;
    End;
  End;


  Function TBrowserView.lwAdjustHorizOfs ( Delta : Integer ) : Integer;

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


  Procedure TBrowserView.lwUpdateVertScrollBar;
    {-Update vertical scroll bar}

  Var
    RelPos : Word;
    Dummy  : Integer;

  Begin
    If PVScrollBar = Nil Then Exit;
    With PBrowser^ Do Begin
      HGetApprRelPos ( RelPos, lwVertScale, GetCurrentKeyStr,
          GetCurrentDatRef );
      If Not StatusOK Then RelPos := 0;
      PVScrollBar^.Value := RelPos;
      PVScrollBar^.DrawView;
      If Message ( Self.Owner, evBroadcast, cmScrollBarChanged, @Self ) = Nil Then;
    End;
  End;


  Procedure TBrowserView.lwUpdateHorzScrollBar;
    {-Update horizontal scroll bar}

  Var
    Dummy : Integer;

  Begin
    If PHScrollBar = Nil Then Exit;
    If lwMaxHorizOfs > 0 Then Begin
      PHScrollBar^.Value := lwHorizOfs;
      PHScrollBar^.DrawView;
      If Message ( Self.Owner, evBroadcast, cmScrollBarChanged, @Self ) = Nil Then;
    End;
  End;


  Procedure TBrowserView.lwReinit;
    {-Initialize variables that can change if window is resized}

  Var
    R         : TRect;
    MaxWidth,
    Width     : Word;

  Begin
    GetExtent ( R );

  {--Vertical stuff in rows}
    lwFullPage := ( R.B.Y - R.A.Y );
    lwFirstRow := 1;
    If lwHeader <> '' Then Begin
      Dec ( lwFullPage );
      Inc ( lwFirstRow );
    End;
    If lwFooter <> '' Then Dec (lwFullPage);

  {--Horizontal stuff in pixels}
    Width := R.B.X - R.A.X;
    MaxWidth := MaxCols;
    If Width > MaxWidth Then Begin
      lwMaxHorizOfs := 0;
    End Else Begin
      lwMaxHorizOfs := MaxWidth - Width;
    End;

  {--Final vertical stuff}
    If lwMaxHorizOfs = 0 Then Begin
      Inc ( lwFullPage );
    End;
    If lwFullPage > $FFF0 Then lwFullPage := 1;
      {-Holds functionality of this browser when resized below 1}
    If PHScrollBar <> Nil Then
       PHScrollBar^.SetParams ( 0, 0, lwMaxHorizOfs, 10, 1 );
  End;


  Procedure TBrowserView.lwLineDown;

  Var
    LRow  : Word;
    Moved : Word;
    CR    : Word;
    R     : TRect;
    Update,
    Dummy : Boolean;
    TRR   : RowRec;

  Begin
    Update := True;
    With PBrowser^ Do Begin
      LRow := GetLastRow;                                         {mod !!.03}
      CR := GetCurRow;
      If ( CR = LRow ) Or ( LRow = 0 ) Then Begin
        HBuildNextPage ( 1, Moved, True, 0, Dummy );
        If StatusOK Then Begin
          If OtherAction Then Begin
            DrawView;
          End Else Begin
            If Moved = 1 Then Begin
              If lwFullPage <> 1 Then Begin
                If LRow = GetLastRow Then Begin                   {mod !!.03}
                  DrawView;
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
    If Update Then lwUpdateVertScrollBar;
  End;


  Procedure TBrowserView.lwLineUp;

  Var
    Moved : Word;
    CR    : Word;
    R     : TRect;
    Update,
    Dummy : Boolean;
    TRR   : RowRec;

  Begin
    Update := True;
    With PBrowser^Do Begin
      CR := GetCurRow;
      If CR = 1 Then Begin
        HBuildPrevPage ( 1, Moved, True, 0, Dummy );
        If StatusOK Then Begin
          If OtherAction Then Begin
            DrawView;
          End Else Begin
            If Moved = 1 Then Begin
              If lwFullPage <> 1 Then Begin
                DrawView;
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
    If Update Then lwUpdateVertScrollBar;
  End;


  Procedure TBrowserView.lwPageDown;

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
          DrawView;
          Update := True;
        End;
      End;
    End;
    If Update Then lwUpdateVertScrollBar;
  End;


  Procedure TBrowserView.lwPageUp;

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
          DrawView;
          Update := True;
        End;
      End;
    End;
    If Update Then lwUpdateVertScrollBar;
  End;


  Procedure TBrowserView.lwFirstPage;

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
          DrawView;
          Update := True;
        End;
      End;
    End;
    If Update Then lwUpdateVertScrollBar;
  End;


  Procedure TBrowserView.lwLastPage;

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
          DrawView;
          Update := True;
        End;
      End;
    End;
    If Update Then lwUpdateVertScrollBar;
  End;


  Procedure TBrowserView.lwMoveToRelPos ( Pos : Word );

  Var
    Key : GenKeyStr;
    Ref : LongInt;

  Begin
    PBrowser^.HGetApprKeyAndRef ( Pos, lwVertScale, Key, Ref );
    SetAndUpdateBrowserScreen ( Key, Ref );
  End;


  Procedure TBrowserView.lwLineRight;

  Var
    OldHOfs : Integer;

  Begin
    OldHOfs := lwHorizOfs;
    lwHorizOfs := lwAdjustHorizOfs ( 1 );
    If OldHOfs <> lwHorizOfs Then Begin
      DrawView;
      lwUpdateHorzScrollBar;
    End;
  End;


  Procedure TBrowserView.lwLineLeft;

  Var
    OldHOfs : Integer;

  Begin
    OldHOfs := lwHorizOfs;
    lwHorizOfs := lwAdjustHorizOfs ( -1 );
    If OldHOfs <> lwHorizOfs Then Begin
      DrawView;
      lwUpdateHorzScrollBar;
    End;
  End;


  Procedure TBrowserView.lwPageRight;

  Var
    OldHOfs : Integer;

  Begin
    OldHOfs := lwHorizOfs;
    lwHorizOfs := lwAdjustHorizOfs ( 10 );
    If OldHOfs <> lwHorizOfs Then Begin
      DrawView;
      lwUpdateHorzScrollBar;
    End;
  End;


  Procedure TBrowserView.lwPageLeft;

  Var
    OldHOfs : Integer;

  Begin
    OldHOfs := lwHorizOfs;
    lwHorizOfs := lwAdjustHorizOfs ( -10 );
    If OldHOfs <> lwHorizOfs Then Begin
      DrawView;
      lwUpdateHorzScrollBar;
    End;
  End;


  Procedure TBrowserView.lwLeftHome;

  Var
    OldHOfs : Integer;

  Begin
    OldHOfs := lwHorizOfs;
    lwHorizOfs := 0;
    If OldHOfs <> lwHorizOfs Then Begin
      DrawView;
      lwUpdateHorzScrollBar;
    End;
  End;


  Procedure TBrowserView.lwRightHome;

  Var
    OldHOfs : Integer;

  Begin
    If PBrowser = Nil Then Exit;
    OldHOfs := lwHorizOfs;
    lwHorizOfs := lwMaxHorizOfs;
    If OldHOfs <> lwHorizOfs Then Begin
      DrawView;
      lwUpdateHorzScrollBar;
    End;
  End;


  Procedure TBrowserView.lwMoveToHorizPos ( Pos : Word );

  Var
    OldHOfs : Integer;

  Begin
    OldHOfs := lwHorizOfs;
    lwHorizOfs := Pos;
    lwHorizOfs := lwAdjustHorizOfs ( 0 );
    If OldHOfs <> lwHorizOfs Then Begin
      DrawView;
      lwUpdateHorzScrollBar;
    End;
  End;


  Procedure TBrowserView.lwMouseClicked ( Var Event : TEvent );

  Var
    Y         : Word;
    OldCurrow : Integer;
    Point     : TPoint;

  Begin
    If PBrowser = Nil Then Exit;
    MakeLocal ( Event.Where, Point );
    Y := Point.Y;
    OldCurrow := PBrowser^.GetCurrow;
    If OldCurRow <> 0 Then Begin
      PBrowser^.SetCurRow ( Y + 2 - lwFirstRow );
      If ( OldCurRow <> PBrowser^.GetCurRow ) And
          ( PBrowser^.GetCurRow <> 0 ) Then Begin
        DisplayRow ( OldCurRow, False );
        DisplayRow ( PBrowser^.GetCurRow, True );
        lwUpDateVertScrollBar;
      End;
    End;
  End;


  Procedure TBrowserView.ChangeBounds ( Var Bounds : TRect );

  Var
    R : TRect;
    D : Integer;
    Changed : Boolean;

  Begin
    TView.GetBounds ( R );
    TView.ChangeBounds ( Bounds );
    D :=(Bounds.B.Y - Bounds.A.Y) - (R.B.Y - R.A.Y);
    IF D <> 0 Then Begin
      lwReInit;
      If D > 0 Then PBrowser^.HExpandPage ( lwFullPage ) Else
      If D < 0 Then PBrowser^.HShrinkPage ( lwFullpage );
      DrawView;
    End;
  End;


  Procedure TBrowserView.Draw;

  Var
    I  : Word;
    CR : Word;

  Begin
    If PBrowser = Nil Then Begin
      TView.Draw;
      Exit;
    End;
    CR := PBrowser^.GetCurRow;
    For I := 1 To lwFullPage Do Begin
      DisplayRow ( I, I = CR);
    End;
  End;


  Procedure TBrowserView.HandleEvent ( Var Event: TEvent );

  Begin
    TView.HandleEvent ( Event );
    If State and sfSelected <> 0 Then Begin
      Case Event.What Of
        evBroadCast :
          Begin
            If Event.Command = cmScrollBarChanged Then Begin
              If Event.InfoPtr = PHScrollbar Then
                lwMoveToHorizPos ( PHScrollbar^.Value )
              Else If Event.InfoPtr = PVScrollbar Then
                  lwMoveToRelPos ( PVScrollbar^.Value )
              Else
                Exit;
            End Else
              Exit;
          End;

        evMouseDown, evMouseMove:
          Begin
            If ( State And sfActive <> 0 ) And
               ( Event.Buttons = 1 ) Then Begin
              lwMouseClicked ( Event );
              If Event.Double Then
                If Message(Owner, evBroadcast, cmListItemSelected, @Self) = Nil Then;
            End Else
              Exit;
          End;
        evKeyDown:
          Begin
            Case Event.KeyCode Of
              kbUp:
                Begin
                  lwLineUp;
                End;
              kbDown:
                Begin
                  lwLineDown;
                End;
              kbRight:
                Begin
                  lwLineRight;
                End;
              kbLeft:
                Begin
                  lwLineLeft;
                End;
              kbCtrlRight:
                Begin
                  lwPageRight;
                End;
              kbCtrlLeft:
                Begin
                  lwPageLeft;
                End;
              kbPgUp:
                Begin
                  lwPageUp;
                End;
              kbPgDn:
                Begin
                  lwPageDown;
                End;
              kbCtrlPgDn:
                Begin
                  lwLastPage;
                End;
              kbCtrlPgUp:
                Begin
                  lwFirstPage;
                End;
              kbHome:
                Begin
                  lwLeftHome;
                End;
              kbEnd:
                Begin
                  lwRightHome;
                End;
            Else
              Exit;
          End;
        End
      Else
        Exit
      End;
      ClearEvent ( Event );
    End Else Begin
      If Options And ofSelectable <> 0 Then Begin
        If (Event.Command = cmScrollBarClicked) And
           ((Event.InfoPtr = PHScrollBar) Or
           (Event.InfoPtr = PVScrollBar)) Then Begin
          Select;
        End;
      End;
    End;
  End;

  Function TBrowserView.BuildRow ( Var RR         : RowRec ) : Integer;

  Begin
    RunError ( 211 );
  End;


  Function TBrowserView.PerformFilter ( Var RR    : RowRec;
                                        Var UseIt : Boolean ) : Integer;

  Begin
    UseIt := True;
    PerformFilter := NoError;
  End;


  Procedure TBrowserView.ShowErrorOccured ( Class : Integer );

  Begin
  End;


  Procedure TBrowserView.SetHeaderFooter ( AHeader,
                                             AFooter : BRLRowEltString );

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
      DrawView;
    End;
  End;

  Procedure TBrowserView.SetLowHighKey ( ALowKey, AHighKey : GenKeyStr );

  Begin
    With PBrowser^Do Begin
      LowKey := ALowKey;
      HighKey := AHighKey;
    End;
  End;


  Procedure TBrowserView.UpdateBrowserScreen;

  Var
    Changed : Boolean;

  Begin
    With PBrowser^ Do Begin
      HBuildThisPage ( Changed );
      If Changed Then Begin
        DrawView;
        If StatusOK Then lwUpdateVertScrollBar;
      End;
    End;
  End;


  Function TBrowserView.GetCurrentRec ( Var Match : Boolean ) :
                                                         Integer; {mod !!.03}

  Var
    Result : Integer;

  Begin
    With PBrowser^ Do Begin
      Result := GetRowMatchingRec ( BSAPtr^[GetCurRow]^, True, True,
          Match );
    End;
    GetCurrentRec := Result;
  End;


  Function TBrowserView.GetThisRec ( Var RR : RowRec ) : Integer;

  Begin
    GetThisRec := PBrowser^.BRGetRec ( RR, False, False );
  End;


  Function TBrowserView.GetCurrentKeyNr : Word;

  Begin
    GetCurrentKeyNr := PBrowser^.KeyNr;
  End;


  Function TBrowserView.GetCurrentKeyStr : String;

  Begin
    GetCurrentKeyStr := PBrowser^.GetCurrentKeyStr;
  End;


  Function TBrowserView.GetCurrentDatRef : LongInt;

  Begin
    GetCurrentDatRef := PBrowser^.GetCurrentDatRef;
  End;


  Procedure TBrowserView.SetKeyNr ( Value : Word );

  Begin
    PBrowser^.KeyNr := Value;
  End;

  Function TBrowserView.GetBrowseStatus : Boolean;
  Begin
    GetBrowseStatus := PBrowser^.StatusOK;
  End;

  Function TBrowserView.BuildBrowScreenRow ( Var RR : RowRec ) : Integer;

  Begin
    BuildBrowScreenRow := PBrowser^.BuildBrowScreenRow ( RR );
  End;


(****************************************************************************)
{$IFDEF BrUseIsam}
  Constructor TBInterior.Init( Var Bounds               : TRect;
                                   PHS, PVS             : PBrowserScrollBar;
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
{$ENDIF}
{$IFDEF BrUseShell}
  Constructor TBInterior.Init( Var Bounds               : TRect;
                                   PHS, PVS             : PBrowserScrollBar;
                                   ADrvPtr              : IFDriverPtr;
                                   ANumberOfEltsPerRow  : Word;
                                   ANumberOfRows        : Word;
                                   AKeyNr               : Word;
                                   ALKey,
                                   AHKey                : GenKeyStr;
                                   AHeader,
                                   AFooter              : BRLRowEltString );
{$ENDIF}

  Begin
{$IFDEF BrUseIsam}
    If Not TBrowserView.Init ( Bounds, PHS, PVS,
                               AFileBlockPtr,
                               ANumberOfEltsPerRow,
                               ANumberOfRows,
                               AKeyNr, ALKey, AHKey,
                               AHeader, AFooter,
                               ADatS, AIsVarRec ) Then Begin
{$ENDIF}
{$IFDEF BrUseShell}
    If Not TBrowserView.Init ( Bounds, PHS, PVS,
                               ADrvPtr,
                               ANumberOfEltsPerRow,
                               ANumberOfRows,
                               AKeyNr, ALKey, AHKey,
                               AHeader, AFooter ) Then Begin
{$ENDIF}

       Fail;
     End;
  End;

  Destructor TBInterior.Done;
  Begin
    TBrowserView.Done;
  End;


  Function TBInterior.PreCompletePage : Integer;

  Begin
    PreCompletePage := PBrowserWindow ( Owner )^.PreCompletePage;
  End;


  Function TBInterior.PostCompletePage : Integer;

  Begin
    PostCompletePage := PBrowserWindow ( Owner )^.PostCompletePage;
  End;


  Function TBInterior.GetPalette: PPalette;

  Const
    P: String[Length(CBInterior)] = CBInterior;

  Begin
    GetPalette := @P;
  End;

  Function TBInterior.BuildRow ( Var RR    : RowRec ) : Integer;

  Begin
    BuildRow := PBrowserWindow ( Owner )^.BuildRow ( RR );
  End;

  Function TBInterior.PerformFilter ( Var RR    : RowRec;
                                      Var UseIt : Boolean ) : Integer;

  Begin
    PerformFilter := PBrowserWindow ( Owner )^.PerformFilter ( RR, UseIt );
  End;

  Procedure TBInterior.ShowErrorOccured ( Class : Integer );
  Begin
    PBrowserWindow ( Owner )^.ShowErrorOccured ( Class );
  End;

(****************************************************************************)

  Function TBrowserWindow.MakeBrowserScrollBar ( AOptions: Word ): PBrowserScrollBar;

  Var
    R: TRect;
    S: PBrowserScrollBar;

  Begin
    GetExtent ( R );
    If AOptions And sbVertical = 0 Then
      R.Assign ( R.A.X + 2, R.B.Y-1, R.B.X-2, R.B.Y )
    Else
      R.Assign ( R.B.X-1,R.A.Y+1,R.B.X,R.B.Y-1 );
    S := New ( PBrowserScrollBar, Init ( R ) );
    If S <> Nil Then Begin
      If AOptions And sbHandleKeyboard <> 0 Then
          S^.Options := S^.Options Or ofPostProcess;
    End;
    MakeBrowserScrollBar := S;
  End;


  Procedure TBrowserWindow.MakeInterior (     Bounds: TRect;
                                              ADrvOrFileBlockPtr   : Pointer;
                                              ANumberOfEltsPerRow  : Word;
                                              ANumberOfRows        : Word;
                                              AKeyNr               : Word;
                                              ALKey,
                                              AHKey                : GenKeyStr;
                                              AHeader,
                                              AFooter              : BRLRowEltString;
                                          Var ADatS;
                                              AIsVarRec            : Boolean;
                                              AIsIsam              : Boolean );


  Var
    PVS, PHS : PBrowserScrollBar;

  Begin
    PVS := MakeBrowserScrollBar ( sbVertical );
    If PVS = Nil Then Exit;
    PHS := MakeBrowserScrollBar ( sbHorizontal );
    IF PHS = Nil Then Begin
      Dispose ( PVS, Done );
      Exit;
    End;

{$IFDEF BrUseIsam}
    PInterior := New ( PBInterior, Init ( Bounds, PHS, PVS,
                                          IsamFileBlockPtr ( ADrvOrFileBlockPtr ),
                                          ANumberOfEltsPerRow,
                                          ANumberOfRows,
                                          AKeyNr,
                                          ALKey,
                                          AHKey,
                                          AHeader,
                                          AFooter,
                                          ADatS,
                                          AIsVarRec ));
{$ENDIF}
{$IFDEF BrUseShell}
    PInterior := New ( PBInterior, Init ( Bounds, PHS, PVS,
                                          IFDriverPtr ( ADrvOrFileBlockPtr ),
                                          ANumberOfEltsPerRow,
                                          ANumberOfRows,
                                          AKeyNr,
                                          ALKey,
                                          AHKey,
                                          AHeader,
                                          AFooter ));
{$ENDIF}
    If PInterior <> Nil Then With PInterior^  Do Begin
      GrowMode := gfGrowHiX + gfGrowHiY;
      Options := Options or ofPostProcess;
      Insert ( PInterior );
    End Else Begin
      Dispose ( PVS, Done );
      Dispose ( PHS, Done );
      Exit;
    End;

    Insert ( PVS );
    Insert ( PHS );
  End;

  Function TBrowserWindow.PreCompletePage : Integer;

  Begin
    PreCompletePage := NoError;
  End;

  Function TBrowserWindow.PostCompletePage : Integer;

  Begin
    PostCompletePage := NoError;
  End;

{$IFDEF BRUseIsam}
  Constructor TBrowserWindow.Init ( var Bounds               : TRect;
                                        ATitle               : TTitleStr;
                                        ANumber              : Integer;
                                        ADrvOrFileBlockPtr   : Pointer;
                                        ANumberOfEltsPerRow : Word;
                                        ANumberOfRows        : Word;
                                        AKeyNr               : Word;
                                        ALKey,
                                        AHKey                : GenKeyStr;
                                        AHeader,
                                        AFooter              : BRLRowEltString;
                                    Var ADatS;
                                        AIsVarRec            : Boolean );

  Var
    R : TRect;

  Begin
    If Not TWindow.Init( Bounds, ATitle, ANumber ) Then Fail;
    GetExtent ( R );
    R.Grow ( -1, -1 );

    MakeInterior ( R,
          ADrvOrFileBlockPtr, 1, ANumberOfRows, AKeyNr, ALKey, AHKey, AHeader,
          AFooter, ADatS, AIsVarRec, True );
    If PInterior= Nil Then Begin
      TWindow.Done;
      Fail;
    End;
  End;
{$ENDIF}


{$IFDEF BRUseShell}
  Constructor TBrowserWindow.Init
                     ( Var Bounds               : TRect;
                           ATitle               : TTitleStr;
                           ANumber              : Integer;
                           ADrvOrFileBlockPtr   : Pointer;
                           ANumberOfEltsPerRow : Word;
                           ANumberOfRows        : Word;
                           AKeyNr               : Word;
                           ALKey,
                           AHKey                : GenKeyStr;
                           AHeader,
                           AFooter              : BRLRowEltString );

  Var
    Dummy : Pointer;
    R : TRect;

  Begin
    Dummy := Nil;
    If Not TWindow.Init( Bounds, ATitle, ANumber ) Then Fail;
    GetExtent ( R );
    R.Grow ( -1, -1 );

    MakeInterior ( R,
          ADrvOrFileBlockPtr, 1, ANumberOfRows, AKeyNr, ALKey, AHKey, AHeader,
          AFooter, Dummy, False, False );

    If PInterior = Nil Then Begin
      TWindow.Done;
      Fail;
    End;
  End;
{$ENDIF}


  Destructor TBrowserWindow.Done;

  Begin
    If PInterior <> Nil Then Dispose ( PInterior, Done );
    TWindow.Done;
  End;


  Function TBrowserWindow.BuildRow ( Var RR         : RowRec ) : Integer;

  Begin
    RunError ( 211 );
  End;


  Function TBrowserWindow.PerformFilter ( Var RR    : RowRec;
                                          Var UseIt : Boolean ) : Integer;

  Begin
    UseIt := True;
    PerformFilter := NoError;
  End;


  Procedure TBrowserWindow.ShowErrorOccured ( Class : Integer );

  Begin
  End;


  Procedure TBrowserWindow.SetHeaderFooter ( AHeader,
                                             AFooter : BRLRowEltString );

  Var
    OldNrOfRows : Word;

  Begin
    If PInterior <> Nil Then With PInterior^ Do Begin
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
        DrawView;
      End;
    End;
  End;

  Procedure TBrowserWindow.SetLowHighKey ( ALowKey, AHighKey : GenKeyStr );

  Begin
    If PInterior <> Nil Then
      PInterior^.SetLowHighKey ( ALowKey, AHighKey );
  End;


  Procedure TBrowserWindow.SetAndUpdateBrowserScreen ( NewKeyStr : GenKeyStr;
                                                       NewRef    : LongInt );

  Begin
    If PInterior <> Nil Then
      PInterior^.SetAndUpdateBrowserScreen ( NewKeyStr, NewRef );
  End;


  Procedure TBrowserWindow.UpdateBrowserScreen;

  Var
    Changed : Boolean;

  Begin
    If PInterior <> Nil Then
      PInterior^.UpdateBrowserScreen;
  End;


  Function TBrowserWindow.GetCurrentRec ( Var Match : Boolean ) :
                                                        Integer;  {mod !!.03}

  Var
    Result : Integer;

  Begin
    GetCurrentRec := DialogError;                                 {!!.05}
    If PInterior <> Nil Then Begin
      With PInterior^, PBrowser^ Do Begin
        Result := GetRowMatchingRec ( BSAPtr^[GetCurRow]^, True, True,
            Match );
      End;
      GetCurrentRec := Result;
    End;
  End;


  Function TBrowserWindow.GetThisRec ( Var RR : RowRec ) : Integer;

  Begin
    GetThisRec := DialogError;                                    {!!.05}
    If PInterior <> Nil Then
      GetThisRec := PInterior^.GetThisRec ( RR );
  End;


  Function TBrowserWindow.GetCurrentKeyNr : Word;

  Begin
    GetCurrentKeyNr := DialogError;                               {!!.05}
    If PInterior <> Nil Then
      GetCurrentKeyNr := PInterior^.PBrowser^.KeyNr;
  End;


  Function TBrowserWindow.GetCurrentKeyStr : String;

  Begin
    GetCurrentKeyStr := '';                                       {!!.05}
    If PInterior <> Nil Then
      GetCurrentKeyStr := PInterior^.PBrowser^.GetCurrentKeyStr;
  End;


  Function TBrowserWindow.GetCurrentDatRef : LongInt;

  Begin
    GetCurrentdatRef := 0;                                        {!!.05}
    If PInterior <> Nil Then
      GetCurrentDatRef := PInterior^.PBrowser^.GetCurrentDatRef;
  End;


  Procedure TBrowserWindow.SetKeyNr ( Value : Word );

  Begin
    If PInterior <> Nil Then
      PInterior^.PBrowser^.KeyNr := Value;
  End;

  Function TBrowserWindow.GetBrowseStatus : Boolean;
  Begin
    GetBrowseStatus := False;                                     {!!.05}
    If PInterior <> Nil Then                                      {!!.05}
        GetBrowseStatus := PInterior^.GetBrowseStatus;
  End;


  Function TBrowserWindow.BuildBrowScreenRow ( Var RR : RowRec ) : Integer;

  Begin
    BuildBrowScreenRow := DialogError;                            {!!.05}
    If PInterior <> Nil Then                                      {!!.05}
        BuildBrowScreenRow := PInterior^.BuildBrowScreenRow ( RR );
  End;

End.
