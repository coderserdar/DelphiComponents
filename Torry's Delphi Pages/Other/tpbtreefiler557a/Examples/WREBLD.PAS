{*********************************************************}
(*                B-Tree Filer Ver 5.55                  *)
{*     Copyright (c) TurboPower Software Co 1986-99      *}
{*                 All rights reserved.                  *}
(*      Based in part on code written by Ralf Nagel      *)
{*********************************************************}

Unit WRebld;

  {$I BrDefOpt.Inc}
  {$X+,F+,V-,B-,S-,I-,R-}
  {$IFDEF CanAllowOverlays}
    {$O+}
  {$ENDIF}
  {$IFDEF CanSetOvrflowCheck}
    {$Q-}
  {$ENDIF}

  {$R WRebld.Res}

Interface

Uses
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
{$IFDEF BrUseIsam}
  BTBase, BTIsBase,
  Filer,     {!!.TP}
  Restruct,
  ReIndex
{$ENDIF}
{$IFDEF BrUseShell}
  OPSBase,
  OOPShell,
  OPSCodes
{$ENDIF}
  ;

Const
  id_Driver = 101;
  id_Keynr  = 102;
  id_RefNrW = 105;
  id_RefNrR = 104;
  id_RecLen = 103;

Type
{$IFDEF BrUseIsam}
  ReXCallType = ( ReXFirst, ReXPerform, ReXFinal );
  GenName     = IsamFileBlockName;
{$ENDIF}
{$IFDEF BrUseShell}
  GenName     = IFDriverName;
{$ENDIF}

  PRebuildInfo = ^TRebuildInfo;
  TRebuildInfo = Object ( TDialog )
    AbortRebuild : Boolean;
    Constructor Init ( AParent : PWindowsObject );
    Procedure Cancel(var Msg: TMessage); virtual id_First + id_Cancel;
  End;

  InformProc = Procedure ( PRInfo   : PRebuildInfo;
                           AName    : GenName;
                           CallType : ReXCallType );

  Procedure DisplayRebuildInfo (     DrName   : GenName;
                                     CallType : ReXCallType;
                                     KeyNr    : Integer;
                                     DatSNrR,
                                     DatSNrW  : LongInt;
                                 Var DatS;
                                     Len      : Word );

{$IFDEF BrUseIsam}
  Procedure StartRebuild ( AName : IsamFileBlockName );
  Procedure StopRebuild;
  Procedure IsamRebuildInfo (     KeyNr    : Integer;
                                  DatSNrR,
                                  DatSNrW  : LongInt;
                              Var DatS;
                                  Len      : Word );
  Procedure RebuildAndInform ( FBlName      : IsamFileBlockName;  {!!.05mod}
                               DatSLen      : LongInt;
                               VarRec       : Boolean;
                               CharConvProc : ProcBTCharConvert;
                               NumberOfKeys : Word;
                               IID          : IsamIndDescr;
                               FuncBuildKey : Pointer;
                               AInformProc  : InformProc );

(*Procedure VRebuildAndInform ( FBlName    : IsamFileBlockName;*) {!!.05del}
{$ENDIF}
  Procedure NoInform ( PRInfo   : PRebuildInfo;
                       DrName   : GenName;
                       CallType : ReXCallType );

  Procedure SetInformProc ( AInformProc : InformProc );


Implementation


  Var
    IP : InformProc;


  Procedure NoInform ( PRInfo   : PRebuildInfo;
                       DrName   : GenName;
                       CallType : ReXCallType );

  Begin
  End;

  Constructor TRebuildInfo.Init ( AParent : PWindowsObject );


  Begin
    If Not TDialog.Init ( Nil, 'Rebuild' ) Then Fail;
    AbortRebuild := False;
  End;


  Procedure TRebuildInfo.Cancel ( Var Msg: TMessage );

  Begin
    AbortRebuild := True;
  End;

{$IFDEF BrUseIsam}
  Procedure IsamRebuildInfo (     KeyNr    : Integer;
                                  DatSNrR,
                                  DatSNrW  : LongInt;
                              Var DatS;
                                  Len      : Word );

  Begin
    DisplayRebuildInfo ( '', ReXPerform, Keynr,
                         DatSNrR, DatSNrW, DatS, Len );
  End;


  Procedure StartRebuild ( AName : IsamFileBlockName );

  Var
    Dummy : LongInt;

  Begin
    DisplayRebuildInfo ( AName, ReXFirst, 0, 0, 0, Dummy, 0 );
  End;


  Procedure StopRebuild;

  Var
    Dummy : LongInt;

  Begin
    DisplayRebuildInfo ( '', ReXFinal, 0, 0, 0, Dummy, 0 );
  End;


  Procedure RebuildAndInform ( FBlName      : IsamFileBlockName;
                               DatSLen      : LongInt;
                               VarRec       : Boolean;
                               CharConvProc : ProcBTCharConvert;
                               NumberOfKeys : Word;
                               IID          : IsamIndDescr;
                               FuncBuildKey : Pointer;
                               AInformProc  : InformProc );

  Var
    Dummy : Boolean;                                                 {!!.05}

  Begin
    IsamReXUserProcPtr := IsamRebuildInfo;
    SetInformProc ( AInformProc );
    StartRebuild ( FBlName );

    RestructFileBlock ( FBlName, DatSLen, DatSLen, VarRec, 0,     {!!.05mod}
        ChangeDatSNoChange, CharConvProc, Nil );                  {!!.05mod}
    If IsamOK Then Begin                                          {!!.05mod}
      ReIndexFileBlock ( FBlName, NumberOfKeys, IID, VarRec,      {!!.05mod}
          Reindex.FuncBuildKey (FuncBuildKey), True, Dummy,       {!!.05mod}
          CharConvProc, Nil );                                    {!!.05mod}
    End;                                                          {!!.05mod}

    StopRebuild;
    SetInformProc ( NoInform );
  End;
{$ENDIF}


  Procedure DisplayRebuildInfo (     DrName   : GenName;
                                     CallType : ReXCallType;
                                     KeyNr    : Integer;
                                     DatSNrR,
                                     DatSNrW  : LongInt;
                                 Var DatS;
                                     Len      : Word );


  Const
    P : PRebuildInfo = Nil;
    ReXInitialized : Boolean = False;

  Var
    C : Char;
    OutP : Array [0..255] of Char;
    J : Integer;
    MsgT : TMsg;
    PW : PWindowsObject;

  Begin
    Case CallType Of
      ReXFirst : Begin
        IP ( P, DRName, CallType );
        New ( P, Init ( Nil ));
        If P = Nil Then Exit;
        P^.Create;
        StrPCopy ( Outp, Drname );
        SetDlgItemText ( P^.HWindow, id_Driver, Outp );
        ReXInitialized := P <> Nil;
      End;
      ReXPerform : Begin
        If Not ReXInitialized Then Exit;
        If P^.AbortRebuild Then
      {$IFDEF BRUseShell}
          IFDIsamOK := False;
      {$ENDIF}
      {$IFDEF BRUseIsam}
          IsamOK := False;
      {$ENDIF}
        IP ( P, DRName, CallType );
        If DatSNrR mod 50 = 0 Then Begin
          wvsPrintf ( OutP, '%i', Keynr );
          SetDlgItemText ( P^.HWindow, id_KeyNr, Outp );
          wvsPrintf ( OutP, '%i', Len );
          SetDlgItemText ( P^.HWindow, id_RecLen, Outp );
          wvsPrintf ( OutP, '%ld', DatsNrR );
          SetDlgItemText ( P^.HWindow, id_RefnrR, Outp );
          wvsPrintf ( OutP, '%ld', DatsNrW );
          SetDlgItemText ( P^.HWindow, id_RefnrW, Outp );
        End;
        UpdateWindow ( P^.Hwindow );
      End;
      ReXFinal : Begin
        If ReXInitialized Then Begin
          IP ( P, DRName, CallType );
          Dispose ( P, Done );
          P := Nil;
          ReXInitialized := False;
        End;
      End;
    End; {Case}
  End;


  Procedure SetInformProc ( AInformProc : Informproc );

  Begin
    IP := AInformProc;
  End;


Begin
  IP := NoInform;
End.
