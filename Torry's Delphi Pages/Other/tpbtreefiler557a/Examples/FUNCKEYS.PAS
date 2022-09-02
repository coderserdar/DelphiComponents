{*********************************************************}
(*                B-Tree Filer Ver 5.55                  *)
{*     Copyright (c) TurboPower Software Co 1986-99      *}
{*                 All rights reserved.                  *}
(*      Based in part on code written by Ralf Nagel      *)
{*********************************************************}

  {-Compiler options}
  {$I-,V-,R-,O+,F+}       {-overwrites user defined options
                            and must not be changed}

{$I OPDEFINE.INC}

Unit FuncKeys;

{$IFNDEF UseDrag}
  *****Error : This unit requires OPDrag
{$ENDIF }


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
  OpDrag;

Const
  fkMaxEntries  = 254;         { max. value = 254 }
Var
  DefFuncKeyRow : Byte;
Type
  FuncKeyColorSet = Object
    TxtColor, TxtMono           : Byte;      {normal chars}
    HiColor, HiMono             : Byte;      {highlight chars}
    SelColor, SelMono           : Byte;      {selected entry}
    SelHiColor, SelHiMono       : Byte;      {selected highlight entry}
    ProColor, ProMono           : Byte;      {protected entry}
    ProHiColor, ProHiMono       : Byte;      {protected entry highlight chars}
    SelProColor, SelProMono     : Byte;      {protected and selected entry}
    SelProHiColor, SelProHiMono : Byte;      {protected and selected entry
                                              highlight chars}

    Procedure SetTextAttr ( Color, Mono : Byte );
      {-Set attributes for normal entry}
    Procedure SetHighLightAttr ( Color, Mono : Byte );
      {-Set attributes for highlight chars in normal entry}
    Procedure SetSelectedAttr ( Color, Mono : Byte );
      {-Set attributes for selected entry}
    Procedure SetSelectedHighlightAttr ( Color, Mono : Byte );
      {-Set attributes for highlight chars in selected entry}
    Procedure SetProtectedAttr ( Color, Mono : Byte );
      {-Set attributes for protected entry}
    Procedure SetProtectedHighlightAttr ( Color, Mono : Byte );
      {-Set attributes for highlight chars in protected entry}
    Procedure SetSelProtectedAttr ( Color, Mono : Byte );
      {-Set attributes for protected and selected entry}
    Procedure SetSelProHighLightAttr ( Color, Mono : Byte );
      {-Set attributes for highlight chars in protected and selected entry}
  End;

Const
  MaxHelpLen = 60;
  MaxFKLen = 19;

Type
  FuncKeyEntryPtr = ^FuncKeyEntry;
  FuncKeyLinePtr = ^FuncKeyLine;
  FuncKeyEntryArrayPtr = ^FuncKeyEntryArray;
  HelpStr = String [ MaxHelpLen ];
  FKStr = String [ MaxFKLen ];
  FKStrPtr = ^FKStr;

  FuncKeyEntry = Record
    Command   : Word;
    DispStr   : FKStrPtr;
    Protected : Boolean;
  End;

  FuncKeyEntryArray = Array [0..fkMaxEntries] of FuncKeyEntry;

  FuncKeyLine = Object ( Root )
    fkError       : Word;
    fkColors      : FuncKeyColorSet;
    FuncKeyRow    : Byte;
    FirstDisp,
    LastDisp      : Byte;
    Changed,                                                      {!!.03}
    OneLine       : Boolean;
    KeysPtr       : FuncKeyEntryArrayPtr;
    AllocEntries,
    ActiveEntries : Byte;
    HelpLine      : HelpStr;
    Saveline      : String;
    SaveAttr      : String;

    Constructor Init ( MaxEntries : Byte );
    Constructor InitCustom (     MaxEntries : Byte;
                             Var Colors : FuncKeyColorSet );
    Destructor Done; Virtual;
    Procedure AddCommand  ( ICommand : Word;
                            IDispStr : FKStr );
    Procedure RemoveCommand  ( ICommand : Word );
    Procedure SetHelpLine ( IHelpLine : HelpStr );
    Procedure SetProtection ( Cmd : Word; On : Boolean );
    Function GetMouseFunction ( Var C : CommandWindow ) : Byte;

    Procedure Error ( Code : Word );
    Function GetLastError : Word;
    Function RawError : Word;

    Procedure DisplayFKLine;
    Procedure EraseFKLine;

    Procedure fkDisplayFKEntry (     EntryIndex : Byte;
                                     ScrPos     : Byte;
                                     Selected   : Boolean;
                                 Var NewScrPos  : Byte );

    Function fkGetEntryIndexbyScrPos (     ScrPos      : Byte;
                                       Var StartScrPos : Byte;
                                       Var EndScrPos   : Byte ) : Byte;
    Function fkFindIndexByCommand ( Cmd : Word ) : Byte;
    Procedure fkIncDispPage;
    Procedure fkResetFL ( FD : Byte );                            {mod !!.03}
    Procedure fkCheckOneLine;
    Function fkBuildHelpLine : HelpStr;
  End;

Const
  DefaultFuncKeyColorSet : FuncKeyColorSet = (
    TxtColor      : $70; TxtMono      : $70;
    HiColor       : $7F; HiMono       : $7F;
    SelColor      : $7F; SelMono      : $7F;
    SelHiColor    : $7F; SelHiMono    : $7F;
    ProColor      : $70; ProMono      : $70;
    ProHiColor    : $70; ProHiMono    : $70;
    SelProColor   : $70; SelProMono   : $70;
    SelProHiColor : $70; SelProHiMono : $70 );

Const
  icMoreFK = ccUser55 - 3;
  fkMoreKey = $5E00;
  fkMoreDisplay : FKStr = ^A'^F1'^A' more '#179' '; {!!.TP}

Implementation

  Procedure fkFlexWrite( Var S : string;
                             Row, Col : Word;
                             A1, A2 : Byte);
    {-Converts A1 and A2 to a FlexAttrs array and calls FlexWrite}
  Var
    FA : FlexAttrs;
  Begin
    FA[0] := A1;
    FA[1] := A2;
    FA[2] := A1;
    FA[3] := A1;
    FlexWrite(S, Row, Col, FA);
  End;


  Procedure FuncKeyColorSet.SetTextAttr ( Color, Mono : Byte );

  Begin
    TxtColor := Color;
    TxtMono := Mono;
  End;

  Procedure FuncKeyColorSet.SetHighLightAttr ( Color, Mono : Byte );

  Begin
    HiColor := Color;
    HiMono := Mono;
  End;

  Procedure FuncKeyColorSet.SetSelectedAttr ( Color, Mono : Byte );

  Begin
    SelColor := Color;
    SelMono := Mono;
  End;

  Procedure FuncKeyColorSet.SetSelectedHighlightAttr ( Color, Mono : Byte );

  Begin
    SelHiColor := Color;
    SelHiMono := Mono;
  End;

  Procedure FuncKeyColorSet.SetProtectedAttr ( Color, Mono : Byte );

  Begin
    ProColor := Color;
    ProMono := Mono;
  End;

  Procedure FuncKeyColorSet.SetProtectedHighlightAttr ( Color, Mono : Byte );

  Begin
    ProHiColor := Color;
    ProHiMono := Mono;
  End;

  Procedure FuncKeyColorSet.SetSelProtectedAttr ( Color, Mono : Byte );

  Begin
    SelProColor := Color;
    SelProMono := Mono;
  End;


  Procedure FuncKeyColorSet.SetSelProHighLightAttr ( Color, Mono : Byte );

  Begin
    SelProHiColor := Color;
    SelProHiMono := Mono;
  End;

Function InitFuncKeyEntry ( Var FK : FuncKeyEntry;
                                ICommand : Word;
                                IDispStr : FKStr ) : Boolean;
Begin
  InitFuncKeyEntry := False;
  With FK Do Begin
    Command := ICommand;
    Protected := False;
    IDispStr := TrimSpaces ( IDispStr ) + ' ';
    GetMem ( DispStr, Succ ( Length ( IDispStr )));
    If DispStr <> Nil Then Begin
      DispStr^ := IDispStr;
      InitFuncKeyEntry := True;
    End;
  End;
End;

Procedure RemoveFuncKeyEntry ( Var FK : FuncKeyEntry );

Begin
  FreeMem ( FK.DispStr, Succ ( Length ( FK.DispStr^ )));
End;

Procedure FuncKeyLine.fkCheckOneLine;

Var
  B : Integer;
  SW, I : Byte;

Begin
  OneLine := False;
  SW := ScreenWidth;
  B := 0;
  B := Length ( fkBuildHelpLine );
  I := 0;
  While ( I < ActiveEntries ) Do Begin
    inc ( I );
    inc ( B, FlexLen ( KeysPtr^ [I]. DispStr^ ));
    If B > SW Then Exit;
  End;
  OneLine := True;
End;

Function FuncKeyLine.fkBuildHelpLine : HelpStr;

Var
  SF : HelpStr;

Begin
  If Trim ( HelpLine ) > '' Then Begin
    If ActiveEntries > 0 Then
      SF := #179' '
    Else SF := ' ';
    SF := SF + HelpLine;
  End Else
    SF := '';
  fkBuildHelpLine := SF;
End;

Procedure FuncKeyLine.fkIncDispPage;
Begin
  FirstDisp := Succ ( LastDisp );
  If FirstDisp > ActiveEntries Then FirstDisp := 1;
End;

Procedure FuncKeyLine.fkResetFL ( FD : Byte );                    {mod !!.03}

Begin
  Changed := True;                                                {!!.03}
  FirstDisp := FD;                                                {mod !!.03}
  LastDisp := 0;
  OneLine := True;
End;

Constructor FuncKeyLine.InitCustom (     MaxEntries : Byte;
                                     Var Colors : FuncKeyColorSet );

Begin
  fkError := 0;
  fkColors := Colors;
  fkResetFL ( 0 );                                                {mod !!.03}
  FuncKeyRow := DefFuncKeyRow;
  KeysPtr := Nil;
  AllocEntries := 0;
  ActiveEntries := 0;
  HelpLine := '';
  SaveLine := '';
  SaveAttr := '';
  IF MaxEntries > fkMaxEntries Then Begin
    InitStatus := epFatal + ecBadParam;
    Fail;
  End;
  GetMem ( KeysPtr, Succ ( MaxEntries ) * SizeOf ( FuncKeyEntry));
  IF KeysPtr = Nil Then Begin
    InitStatus := epFatal + ecOutOfMemory;
    Fail;
  End;
  With KeysPtr^ [0] Do Begin
    Command := icMoreFK;
    Protected := False;
    DispStr := @fkMoreDisplay;
  End;
  AllocEntries := Succ ( MaxEntries );
End;

Constructor FuncKeyLine.Init ( MaxEntries : Byte );

Begin
  If Not InitCustom ( MaxEntries, DefaultFuncKeyColorSet ) Then Fail;
End;

Destructor FuncKeyLine.Done;

Var
  I : Integer;

Begin
  EraseFKLine;
  If AllocEntries > 0 Then Begin
    For I:=1 to ActiveEntries Do With KeysPtr^[I] Do
      Freemem ( DispStr, Succ ( Length ( DispStr^ )));
    FreeMem ( KeysPtr, AllocEntries * SizeOf ( FuncKeyEntry) );
    KeysPtr := Nil;
    AllocEntries := 0;
    ActiveEntries := 0;
  End;
End;

Procedure FuncKeyLine.Error ( Code : Word );

Begin
  fkError := Code;
End;

Function FuncKeyLine.GetLastError : Word;

Begin
  GetLastError := fkError;
  fkError := 0;
End;

Function FuncKeyLine.RawError : Word;

Begin
  RawError := fkError;
End;

Procedure FuncKeyLine.RemoveCommand ( ICommand : Word );

Var
  I, J : Byte;

Begin
  fkResetFL ( 0 );                                                {mod !!.03}
  Repeat
    I := fkFindIndexByCommand ( ICommand );
    If I <> 255 Then Begin
      RemoveFuncKeyEntry ( KeysPtr^ [ I ] );
      For J := I To ActiveEntries - 1 Do
        KeysPtr^ [ J ] := KeysPtr ^ [ J + 1 ];
      Dec ( ActiveEntries );
    End;
  Until I = 255;
End;


Procedure FuncKeyLine.AddCommand ( ICommand : Word;
                                   IDispStr : FKStr );

Begin
  If RawError <> 0 Then Exit;
  fkResetFL ( 0 );                                                {mod !!.03}
  If Succ ( ActiveEntries ) >= AllocEntries Then Begin
    Error ( epFatal + ecBufferFull );
    Exit;
  End;
  Inc ( ActiveEntries );
  If Not InitFuncKeyEntry ( KeysPtr^[ ActiveEntries ],
                            ICommand,
                            IDispStr ) Then Begin
    Error ( epFatal + ecOutofMemory );
    Dec ( ActiveEntries );
    Exit;
  End;
End;

Procedure FuncKeyLine.SetHelpLine ( IHelpLine : HelpStr );

Begin
  fkResetFL ( FirstDisp );                                        {mod !!.03}
  HelpLine := IHelpLine;
End;

Procedure FuncKeyLine.fkDisplayFKEntry (     EntryIndex : Byte;
                                             ScrPos     : Byte;
                                             Selected   : Boolean;
                                         Var NewScrPos  : Byte );

Var
  C1, C2 : Byte;
  MouseState : Boolean;

Begin
  HideMousePrim ( MouseState );
  With KeysPtr^ [ EntryIndex ] Do Begin
    With fkColors Do Begin
      If Protected And Not Selected Then Begin
        C1 := ColorMono ( ProColor, ProMono );
        C2 := ColorMono ( ProHiColor, ProHiMono );
      End Else Begin
        If Selected And Not Protected Then Begin
          C1 := ColorMono ( SelColor, SelMono );
          C2 := ColorMono ( SelHiColor, SelHiMono );
        End Else If Selected And Protected Then Begin
         C1 := ColorMono ( SelProColor, SelProMono );
         C2 := ColorMono ( SelProHiColor, SelProHiMono );
       End Else Begin
         C1 := ColorMono ( TxtColor, TxtMono );
         C2 := ColorMono ( HiColor, HiMono );
       End;
     End;
    End;
    fkFlexWrite ( DispStr^, FuncKeyRow, ScrPos, C1, C2 );
    NewScrPos := ScrPos + FlexLen ( DispStr^ );
  End;
  ShowMousePrim ( MouseState );
End;

Procedure FuncKeyLine.DisplayFKLine;

Var
  SF : String;
  I : Byte;
  XPos : Byte;
  MouseState : Boolean;
  MaxLineLen,
    AktLineLen : Byte;
  Finished : Boolean;

Begin
  MaxLineLen := ScreenWidth;
  If Changed Or ( FirstDisp = 0 ) Then Begin                      {mod !!.03}
    fkCheckOneLine;
    Changed := False;
    If FirstDisp = 0 Then FirstDisp := 1;                         {mod !!.03}
  End;
  SF := fkBuildHelpLine;
  HideMousePrim ( MouseState );
  If SaveLine = '' Then Begin
    FastRead ( ScreenWidth, FuncKeyRow, 1, SaveLine );
    ReadAttribute ( ScreenWidth, FuncKeyRow, 1, SaveAttr );
  End;
  ShowMousePrim ( MouseState );
  XPos := 1;
  I := FirstDisp - 1;
  Finished := ActiveEntries = 0;
  If Not OneLine Then fkDisplayFKEntry ( 0, XPos, False, XPos );
  While Not Finished Do Begin
    Inc ( I );
    fkDisplayFKEntry ( I, XPos, False, XPos );
    If I >= ActiveEntries Then
        Finished := True
    Else
    If FlexLen ( TrimTrail ( KeysPtr^ [ I+1 ].DispStr^ )) +
                 XPos + Length ( SF ) > ScreenWidth Then
      Finished := True;
  End;
  LastDisp := I;
  HideMousePrim ( MouseState );
  With fkColors Do Begin
    If ActiveEntries = 0 Then
      FastWrite ( Pad (SF, 255 ), FuncKeyRow, XPos, ColorMono ( TxtColor, TxtMono ))
    Else Begin
      FastWrite ( LeftPad ( SF, ScreenWidth - XPos + 1 ),
                  FuncKeyRow, XPos,
                  ColorMono ( TxtColor, TxtMono ));
    End;
  End;
  ShowMousePrim ( MouseState );
End;

Procedure FuncKeyLine.EraseFKLine;

Var
  MouseState : Boolean;

Begin
  If ( SaveLine > '' ) Then Begin
    HideMousePrim ( MouseState );
    FastWriteAttr ( SaveLine, FuncKeyRow, 1, SaveAttr );
    ShowMousePrim ( MouseState );
  End;
  SaveLine := '';
  SaveAttr := '';
End;

Function FuncKeyLine.fkGetEntryIndexbyScrPos (     ScrPos      : Byte;
                                               Var StartScrPos : Byte;
                                               Var EndScrPos   : Byte ) : Byte;

Var
  I , XSPos, XEPos : Byte;

Begin
  fkGetEntryIndexbyScrPos := 255;
  StartScrPos := 0;
  EndScrPos := 0;
  XSPos := 1;

  If Not OneLine Then Begin
    XEPos := XSPos + FlexLen ( KeysPtr^ [ 0 ].DispStr^) - 1;
    If ( ScrPos >= XSPos ) And ( ScrPos <= XEPos ) Then Begin
      StartScrPos := XSPos;
      EndScrPos := XEPos;
      fkGetEntryIndexbyScrPos := 0;
      Exit;
    End;
    XSPos :=  Succ ( XEPos );
  End;

  For I := FirstDisp To LastDisp Do Begin
    XEPos := XSPos + FlexLen ( KeysPtr^ [ I].DispStr^) - 1;
    If ( ScrPos >= XSPos ) And ( ScrPos <= XEPos ) Then Begin
      StartScrPos := XSPos;
      EndScrPos := XEPos;
      fkGetEntryIndexbyScrPos := I;
      Exit;
    End;
    XSPos :=  Succ ( XEPos );
  End;
End;

Procedure FuncKeyLine.SetProtection ( Cmd : Word; On : Boolean );

Var
  I : Integer;

Begin
  For I := 1 To ActiveEntries Do  With KeysPtr^[I] Do
     If Command = Cmd Then Protected := On;
End;

Function FuncKeyLine.fkFindIndexByCommand ( Cmd : Word ) : Byte;

Var
  I : Integer;

Begin
  fkFindIndexByCommand := 255;
  For I := 0 To ActiveEntries Do
    If KeysPtr^ [ I ].Command = Cmd Then Begin
      fkFindIndexByCommand := I;
      Exit;
  End;
End;

Function FuncKeyLine.GetMouseFunction ( Var C : CommandWindow ) : Byte;

Var
  S : String;
  ActMousePos,
    SPosX, EPosX : Byte;
  EntryIndex,
    LastEntryIndex : Byte;
  LastSPos : Byte;

  Finished : Boolean;
  HotCode : Byte;
  Cmd : Word;

Begin
  { check whether CMD is switchDisplay and process it }
  GetMouseFunction := hsNone;
  Cmd := C.GetLastCommand;
  If Cmd <> ccMouseDown Then Begin
    EntryIndex := fkFindIndexByCommand ( Cmd );
    If EntryIndex = 0 Then
      fkIncDispPage
    Else If EntryIndex <> 255 Then If KeysPtr^[ EntryIndex ].Protected Then
       C.SetLastCommand ( ccNone );
    Exit;
  End;
  HotCode := HandleMousePress ( C );

  GetMouseFunction := HotCode;

  If ( HotCode <> hsNone ) Then Exit;

  If MouseWhereYabs <> FuncKeyRow Then Exit;
  Cmd := ccNone;
  Finished := False;
  LastEntryIndex := 255;
  LastSPos := 0;
  While MousePressed Do Begin
    EntryIndex := 255;
    ActMousePos := MouseWhereXabs;
    If MouseWhereYabs = FuncKeyRow Then
      EntryIndex := fkGetEntryIndexbyScrPos ( ActMousePos, SPosX, EPosX );
    If EntryIndex <> LastEntryIndex Then Begin
      If LastEntryIndex <> 255 Then Begin
        fkDisplayFKEntry ( LastEntryIndex, LastSPos, False, LastSPos );
        LastEntryIndex := 255;
        LastSPos := 0;
      End;
      If EntryIndex <> 255 Then Begin
        fkDisplayFKEntry ( EntryIndex, SPosX, True, EPosX );
        LastEntryIndex := EntryIndex;
        LastSPos := SPosX;
      End;
    End;
  End;
  If LastEntryIndex <> 255 Then Begin
    fkDisplayFKEntry ( LastEntryIndex, LastSPos, False, LastSPos );
    LastEntryIndex := 255;
    LastSPos := 0;
  End;
  EntryIndex := 0;
  ActMousePos := MouseWhereXabs;
  If MouseWhereYabs = FuncKeyRow Then Begin
    EntryIndex := fkGetEntryIndexbyScrPos ( ActMousePos, SPosX, EPosX );
    If EntryIndex <> 255 Then With KeysPtr^[EntryIndex] Do
      If EntryIndex = 0 Then Begin
        fkIncDispPage;
        FirstDisp := Succ ( LastDisp );
        If FirstDisp > ActiveEntries Then FirstDisp := 1;
        C.SetLastCommand ( ccNone );
      End Else
      If Not Protected Then
        C.SetLastCommand ( KeysPtr^[EntryIndex].Command )
      Else
        C.SetLastCommand ( ccNone );
  End Else
    C.SetLastCommand ( ccNone );

  ClearMouseEvents;
End;

Begin
  DefFuncKeyRow := ScreenHeight;
End.
