{*********************************************************}
(*                B-Tree Filer Ver 5.55                  *)
{*     Copyright (c) TurboPower Software Co 1986-99      *}
{*                 All rights reserved.                  *}
(*      Based in part on code written by Ralf Nagel      *)
{*********************************************************}

Unit MKDrag;
{$F+,O+}
Interface
{$I OPDEFINE.INC}

{$UNDEF UseGerman} {!!.TP}

Uses
  Dos,
{$IFDEF OPRO12}       {!! 1.20}
  OPConst,
{$ENDIF}
  OpInline,
  OpRoot,
  OpString,
  OpCrt,
  OpMouse,
  OpCmd,
  OpFrame,
  OpWindow,
  OpDrag,
  FuncKeys;

Const
  icMoveResize = ccUser55 - 1;
  icZoom       = ccUser55 - 2;
  MkMoveKey    = $3200;
  MkZoomKey    = $2C00;

  { Unit MKDrag String constants }
  {$IFDEF UseGerman}                                              {!!.03}
  MKDHelpLineTxt =                                                {!!.03}
       '<Cursort.> schieben, <Shift><Cursort.> Gr”áe,'+           {!!.03}
       ' <Enter> akzept., <ESC> Abbruch';                         {!!.03}
  {$ELSE}                                                         {!!.03}
  MKDHelpLineTxt =                                                {!!.03}
      '<Cursor keys> Move, <Shift><Cursor keys>'+                 {!!.03}
      ' Resize, <Enter> Accept, <ESC> Break';                     {!!.03}
  {$ENDIF}                                                        {!!.03}

  OpMoveKeyMax = 60;   {last available slot in OpMoveKeySet}
  {ID string for installation programs}

  OpMoveKeyID : string[12] = 'opMove keys';
  {default key assignments}

  OpMoveKeySet : array[0..OpMoveKeyMax] of Byte = (
   {length keys         command type      key sequence}

    3,     $00, $00,    ccQuit,          {^Break}
    3,     $00, $3B,    ccHelp,          {F1}
    3,     $00, $47,    ccHome,          {Home}
    3,     $00, $48,    ccUp,            {Up}
    3,     $00, $49,    ccPageUp,        {PgUp}
    3,     $00, $4B,    ccLeft,          {Left}
    3,     $00, $4D,    ccRight,         {Right}
    3,     $00, $4F,    ccEnd,           {End}
    3,     $00, $50,    ccDown,          {Down}
    3,     $00, $51,    ccPageDn,        {PgDn}
    2,     $1B,         ccQuit,          {Esc}
    2,     $0D,         ccSelect,        {Enter}
                   0, 0, 0, 0, 0,        { 50}
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0 );      { 60}


Var
  OpMoveCommands : DragProcessor;

  Function MouseAndKbDrag ( Var M : CommandWindow;
                                FKPtr : FuncKeyLinePtr ) : Byte;


  Function AddMoveZoomCommands ( Var DP : CommandProcessor ) : Word;

  Function HandleMouseOrKBDPress ( Var CW : CommandWindow;
                                       FKPtr : FuncKeyLinePtr ) : Byte;

Implementation

Function MouseAndKbDrag ( Var M : CommandWindow;
                              FKPtr : FuncKeyLinePtr ) : Byte;
  Var
    DP : DragProcessorPtr;
    HotCode : Byte;
    DH : Integer;
    DV : Integer;
    RSFlag : Byte;
    SaveMouse : Boolean;

  Function CheckCoordinates ( DH, DV : Integer; Resize : Boolean ) : Boolean;
  Var
    NX1, NY1, NX2, NY2 : Byte;
  Begin
    M.Coordinates(NX1, NY1, NX2, NY2);
    Inc(NX2, DH);
    Inc(NY2, DV);
    if not Resize then begin
      Inc(NX1, DH);
      Inc(NY1, DV);
    end;
    CheckCoordinates :=
      M.rwValidCoords(NX1, NY1, NX2, NY2, NX1, NY1, NX2, NY2);
  end;

  Procedure GetRelOFFset ( Var DH,DV : Integer;
                           Var CMD : Word;
                           Var Resize : Boolean );
  Var
    Key    : Word;
    ShiftPressed : Boolean;

  Procedure IncHV ( HOffs, VOffs : Integer );
  Begin
    DV := VOffs;
    DH := HOffs;
  End;

  Begin
    Resize := False;
    CMD := OPMoveCommands.GetCommand ( Key );
    DH := 0;
    DV := 0;
    If Not M.wOptionsAreOn ( wResizeable ) Then
      ShiftPressed := False
    Else
      ShiftPressed := ( KbdFlags And 3 ) > 0;
    With M Do
    case Cmd of
      ccUp : Begin
        IncHV ( 0, -1 );
        Resize := ShiftPressed;
      End;

      ccLeft : Begin
        IncHV ( -1, 0 );
        Resize := ShiftPressed;
      End;

      ccRight : begin
        IncHV ( 1, 0 );
        Resize := ShiftPressed;
      End;

      ccDown : Begin
        IncHV ( 0, 1 );
        Resize := ShiftPressed;
      End;

      ccHome :
       If ActiveFramePtr = Nil Then
         IncHV ( wMinXL - wXL, 0 )
       Else with ActiveFramePtr^ Do
         IncHV ( FRMinXL - FRXL, 0 );

      ccPageUp :
       If ActiveFramePtr = Nil Then
         IncHV ( 0, wMinYL - wYL )
       Else with ActiveFramePtr^ Do
         IncHV ( 0, FRMinYL - FRYL );

      ccEnd :
       If ActiveFramePtr = Nil Then
         IncHV ( wMaxXH - wXH , 0 )
       Else with ActiveframePtr^ Do
         IncHV ( FRMaxXH - FRXH, 0 );

      ccPageDn :
       If ActiveFramePtr = Nil Then
         IncHV ( 0, wMaxYH - wYH )
       Else with ActiveFramePtr^ Do
         IncHV ( 0, FRMaxYH - FRYH );
    End;
  End;

  Procedure KBMoveResize ( Var M : CommandWindow );

  Var
    Valid : Boolean;
    CMD : Word;
    Resize : Boolean;
    X1, Y1, X2, Y2 : Byte;

  Begin
    M.Coordinates ( X1, Y1, X2, Y2 );

    Repeat
      GetRelOffSet ( DH, DV, CMD , Resize);

      if (DH <> 0) or (DV <> 0) then begin

        If CheckCoordinates ( DH, DV, Resize ) Then
          If Resize Then
            M.ResizeWindow(DH, DV)
          Else
            M.MoveWindow(DH, DV);
        M.ClearErrors;

      End;
    until CMD in [ccSelect, ccQuit];

    If CMD = ccQuit Then M.AdjustWindow ( X1, Y1, X2, Y2 );

  End;

Begin
  {assure the command processor supports dragging}
  DP := DragProcessorPtr ( M.cwCmdPtr );
  If TypeOf ( DP^ ) <> TypeOf ( DragProcessor ) Then Begin
    MouseAndKBDrag := hsNone;
    Exit;
  End;

  If M.GetLastCommand in [ icMoveResize,
                           icZoom ] Then Begin

    HideMousePrim ( SaveMouse );
    OpMoveCommands.Init ( @OpMoveKeySet, OPMoveKeyMax );
    OPMoveCommands.SetKeyPressedProc ( M.cwCMDPtr^.cpKeyPressedProc );

    Case M.GetLastCommand Of
      icMoveResize : If Not M.IsZoomed Then Begin
                        KBMoveResize ( M );
                        HotCode := MoveHotCode;
                      End;
      icZoom       : Begin
                       If M.IsZoomed Then
                         M.UnZoom
                       Else
                         M.Zoom;
                       HotCode := ZoomHotCode;
                     End;
    End;
    ShowMousePrim ( SaveMouse );
    OpMoveCommands.Done;
  End Else Begin
    If FKPtr <> Nil Then
      HotCode := FKPtr^.GetMouseFunction ( M )
    Else
      HotCode := HandleMousePress ( M );
  End;
  MouseAndKBDrag := HotCode;
End;


Function HandleMouseOrKBDPress ( Var CW : CommandWindow;
                                     FKPtr : FuncKeyLinePtr ) : Byte;
Var
  WPtr       : WindowPtr;
  DP         : DragProcessorPtr;
  Cmd        : Word;
  FKL        : FuncKeyLine;
  FKT        : FuncKeyLinePtr;
  HotCode    : Byte;
  FP         : FramePosType;
  XAbs, YAbs : Byte;

Procedure MousePosition (     WPtr       : WindowPtr;
                        Var FP         : FramePosType;
                        Var XAbs, YAbs : Byte );

Var
  HC : Byte;
  BP : LongInt;

Begin
  With WPtr^ Do Begin
    HC := 0;
    XAbs := MouseKeyWordX+MouseXLo;
    YAbs := MouseKeyWordY+MouseYLo;
    EvaluatePos(XAbs, YAbs);
    BP := PosResults(FP, HC);
  End;
End;

Begin
  HandleMouseOrKBDPress := hsNone;
  WPtr := @CW;
  Cmd := CW.GetLastCommand;
  FKT := FKPtr;
  FKL.Init ( 0 );
  FKL.SetHelpLine ( MKDHelpLineTxt );
  If Cmd in [ icMoveResize, icZoom, ccMouseDown ] Then Begin
    If CMD = icMoveResize Then Begin
      FKL.DisplayFKLine;
    End Else If Cmd = ccMouseDown Then Begin
      MousePosition ( WPtr, FP, XAbs, YAbs );
      If FP = frOutsideFrame Then Begin
        WPtr := wStack.EvaluateStackPos ( XAbs, YAbs );
        If WPtr <> Nil Then Begin
          DP := DragProcessorPtr ( CommandWindowPtr(WPtr)^.cwCmdPtr );
          If TypeOf ( DP^ ) <> TypeOf ( DragProcessor ) Then Begin
            WPTr := Nil;
          End;
        End;
        If Wptr = Nil Then
          WPtr := @CW
        Else
          FKT := Nil;
      End;
    End;
  End;
  If WPtr <> Nil Then Begin
    HotCode := MouseAndKBDrag ( CommandWindowPtr (WPtr)^ , FKT );
  End;
  FKL.EraseFKLine;
  FKL.Done;
  HandleMouseOrKBDPress := HotCode;
End;

Function AddMoveZoomCommands ( Var DP : CommandProcessor ) : Word;

Begin
  If TypeOf ( DP ) <> TypeOf ( DragProcessor ) Then Begin
    AddMoveZoomCommands := ecBadParam;
    Exit;
  End;
  DP.AddCommand ( icMoveResize, 1, MkMoveKey, 0 );
  DP.AddCommand ( icZoom, 1, MkZoomKey, 0 );
  AddMoveZoomCommands := DP.GetLastError;
End;

{$IFDEF InitAllUnits}
Begin
{$ENDIF}

End.

