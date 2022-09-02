{********************************************************************}
{* RESTRUCT.PAS - restructure fileblock                             *}
{********************************************************************}

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

{--Conditional defines and compiler options}
{$I BTDefine.Inc}
{$IFDEF CanAllowOverlays}
  {$O+,F+}
{$ENDIF}

Unit ReStruct;                                                       {!!.50}

Interface

Uses
  BTBase,
  BTIsBase,
  Filer,     {!!.TP}
  VRec,      {!!.TP}
  BufRecIO;

Type
  FuncChangeDatS = Function  ( Var DatSOld;
                               Var DatSNew;
                               Var Len     : Word ) : Boolean;


  Procedure RestructFileBlock ( FBlName        : IsamFileBlockName;
                                DatSLen        : LongInt;
                                DatSLenOld     : LongInt;
                                VarRec         : Boolean;
                                MaxDiffBytes   : LongInt;
                                ChangeDatSFunc : FuncChangeDatS;
                                CharConvProc   : ProcBTCharConvert;
                                CCHookPtr      : Pointer );
    {-Restructures a fileblock's data file with variable or fixed
      record lengths; deletes the old index file}

  Function ChangeDatSNoChange ( Var DatSOld;
                                Var DatSNew;
                                Var Len     : Word ) : Boolean;
     {-Simply copies DatSOld to DatSNew; returns true, if not deleted}


Implementation

{$UNDEF NeedAssigned}
{$IFDEF VER60}
{$DEFINE NeedAssigned}
{$ENDIF}
{$IFDEF VER15}
{$DEFINE NeedAssigned}
{$ENDIF}

{$IFDEF NeedAssigned}
function Assigned(var P) : boolean;
  var
    Pt : pointer absolute P;
  begin
    Assigned := Pt <> nil;
  end;
{$ENDIF}

  Procedure RestructFileBlock ( FBlName        : IsamFileBlockName;
                                DatSLen        : LongInt;
                                DatSLenOld     : LongInt;
                                VarRec         : Boolean;
                                MaxDiffBytes   : LongInt;
                                ChangeDatSFunc : FuncChangeDatS;
                                CharConvProc   : ProcBTCharConvert;
                                CCHookPtr      : Pointer );


  Var
    FNameTemp,
    FNameD,
    FNameI,
    FNameS           : IsamFileBlockName;
    IFBPtr           : IsamFileBlockPtr;
    BufPtrOld,
    BufPtrNew        : Pointer;
    BufLenOld,
    BufLenNew        : Word;
    F                : IsamFile;
    SavFileExisted,
    ExtraSaveDirUsed : Boolean;


    Procedure CreateSaveFile;

    Var
      F : IsamFile;

    Begin
      IsamAssign ( F, IsamForceExtension ( FNameD, DatExtension ) );
      IsamRename ( F, IsamForceExtension ( FNameS, SavExtension ) );
      If Not IsamOK Then Begin
        If IsamError = 10140 Then Begin
          IsamError := 10399;
          Exit;
        End;
        If IsamExists ( IsamForceExtension ( FNameS, SavExtension ) )
            Then Begin
          IsamClearOK;
          SavFileExisted := True;
        End Else Begin
          If Not ExtraSaveDirUsed Then Begin
            IsamOK := False;
            IsamError := 10410;
            Exit;
          End;
          IsamClearOK;
          IsamAssign ( F, IsamForceExtension ( FNameI, IxExtension ) );
          IsamDelete ( F );
          IsamClearOK;
          IsamCopyFile ( IsamForceExtension ( FNameD, DatExtension ),
              IsamForceExtension ( FNameS, SavExtension ), True );
        End;
      End;
    End;


    Procedure RecreateDataFile;

    Var
      F : IsamFile;

    Begin
      If SavFileExisted Then Exit;
      IsamClearOK;
      IsamAssign ( F, IsamForceExtension ( FNameS, SavExtension ) );
      IsamRename ( F, IsamForceExtension ( FNameD, DatExtension ) );
      If Not IsamOK Then Begin
        IsamClearOK;
        IsamCopyFile ( IsamForceExtension ( FNameS, SavExtension ),
            IsamForceExtension ( FNameD, DatExtension ), True );
      End;
    End;


    Procedure UnDo ( Level : Word; Error : Integer );

    Begin
      If BufPtrOld <> Nil Then Begin
        FreeMem ( BufPtrOld, BufLenOld );
      End;

      If BufPtrNew <> Nil Then Begin
        FreeMem ( BufPtrNew, BufLenNew );
      End;

      If Level >= 4 Then Begin
        IsamClose ( F );
      End;

      If Level >= 3 Then Begin
        DoneRecBuffers;
      End;

      If Level >= 2 Then Begin
        BTCloseFileBlock ( IFBPtr );
      End;

      If Level >= 1 Then Begin
        BTDeleteFileBlock ( FblName );
        RecreateDataFile;
      End;

      IsamOK := False;
      IsamError := Error;
    End;


    Function AdjustBuffers ( ToSize1, ToSize2 : Word ) : Boolean;
      {-Called only when variable recs are used}


      Function Alloc ( Var P : Pointer; Size : Word ) : Boolean;
                                                         {!!.52 rewritten}
      Begin
        Alloc := False;
        while not IsamGetMem(P, Size) do
          if not DecreaseBufferMem then
            Exit;
        Alloc := True;
      End;


    Begin
      AdjustBuffers := False;

      If BufPtrOld <> Nil Then Begin
        FreeMem ( BufPtrOld, BufLenOld );
        BufPtrOld := Nil;
      End;
      If BufLenNew <> ToSize2 Then Begin
        If BufPtrNew <> Nil Then Begin
          FreeMem ( BufPtrNew, BufLenNew );
          BufPtrNew := Nil;
        End;
      End;

      If Not Alloc ( BufPtrOld, ToSize1 ) Then Exit;
      BufLenOld := ToSize1;

      If BufPtrNew = Nil Then Begin
        If Not Alloc ( BufPtrNew, ToSize2 ) Then Begin
          FreeMem ( BufPtrOld, BufLenOld );
          BufPtrOld := Nil;
          Exit;
        End;
        BufLenNew := ToSize2;
      End;

      AdjustBuffers := True;
    End;


  Type
    PIsamIndDescr = ^IsamIndDescr;

  Var
    MaxSize,
    CurRecLen,
    OldRecLen,
    InitBufSize,
    TempSize    : Word;
    Ok,
    Stop,
    RetryRead,
    WasRead     : Boolean;
    CurrentRef,
    DatSRead,
    DatSWritten : LongInt;

  Begin
    IsamClearOK;
    SavFileExisted := False;

  {--Set buffer pointers to Nil}
    BufPtrOld := Nil;
    BufPtrNew := Nil;
    BufLenOld := 0;
    BufLenNew := 0;

  {--Get file names for data, index, and save file}
    IsamExtractFileNames ( FBlName, FNameD, FNameTemp );
    IsamExtractFileNames ( FNameTemp, FNameI, FNameS );
    ExtraSaveDirUsed := FNameS <> FNameTemp;
    If Not ExtraSaveDirUsed Then Begin
      FNameS := FNameD;
    End;

  {--Check record lengths}
    If VarRec Then Begin
      MaxSize := MaxVariableRecLength;
    End Else Begin
      MaxSize := $FFFF;
    End;
    If (DatSLenOld > MaxSize) Or (DatSLen > MaxSize) Then Begin
      UnDo ( 0, 10412 );
      Exit;
    End;

  {--Do data and save file exist simultaniously?}
    If IsamExists ( IsamForceExtension ( FNameS, SavExtension ) ) And
        IsamExists ( IsamForceExtension ( FNameD, DatExtension ) )
        Then Begin
      UnDo ( 0, 10465 );
      Exit;
    End;

  {--Adjust buffer for variable recs}
    If VarRec Then Begin
      If DatSLenOld > DatSLen Then Begin
        InitBufSize := DatSLenOld;
      End Else Begin
        InitBufSize := DatSLen;
      End;
      If Not BTAdjustVariableRecBuffer ( InitBufSize ) Then Begin
        UnDo ( 0, 10411 );
        Exit;
      End;
    End Else Begin
      MaxDiffBytes := 0;
    End;

  {--Move data to save file}
    CreateSaveFile;
    If Not IsamOK Then Begin
      UnDo ( 0, IsamError );
      Exit;
    End;

  {--Create the new fileblock without keys}
    BTCreateFileBlock ( FNameD + ';' + FNameI, DatSLen, 0,
        PIsamIndDescr (@MaxSize)^ {dummy} );
    If Not IsamOK Then Begin
      UnDo ( 0, IsamError );
      Exit;
    End;

  {--Open the new fileblock}
    BTOpenFileBlock ( IFBPtr, FNameD + ';' + FNameI, False, False, False,
        False );
    If Not IsamOK Then Begin
      UnDo ( 1, IsamError );
      Exit;
    End;

  {--Get buffer for data read and write}
    CreateRecBuffers ( DatSLenOld, IFBPtr );

  {--Get buffers for records}
    If VarRec Then Begin
      If MaxDiffBytes < 0 Then Begin
        InitBufSize := - MaxDiffBytes;
      End Else Begin
        InitBufSize := DatSLenOld + MaxDiffBytes;
      End;
    End Else Begin
      InitBufSize := DatSLen;
    End;
    If Not AdjustBuffers ( DatSLenOld, InitBufSize ) Then Begin
      UnDo ( 3, 10411 );
      Exit;
    End;

  {--Open save file for reading}
    IsamAssign ( F, IsamForceExtension ( FNameS, SavExtension ) );
    IsamReset ( F, False, False );
    If Not IsamOK Then Begin
      UnDo ( 3, IsamError );
      Exit;
    End;

  {--Initialize status and control data}
    CurRecLen := 0;
    CurrentRef := 0;
    DatSRead := 0;
    DatSWritten := 0;
    Stop := False;

  {--Restructure loop}
    Repeat
    {--Read record}
      If VarRec Then Begin
        Repeat
          RetryRead := False;
          BufGetVariableRecRebuilt ( F, DatSLenOld, CurrentRef, BufPtrOld,
              BufLenOld, WasRead, CurRecLen );
          If Not IsamOK Then Begin
            If (IsamError = 10415) Or (IsamError = 10070) Then Begin
              {-A fractal record was read, skip it and retry}
              IsamClearOK;
              RetryRead := True;
            End Else Begin
              {-A serious error occured}
              UnDo ( 4, IsamError );
              Exit;
            End;
          End Else Begin
            If Not WasRead Then Begin
              If CurRecLen = 0 Then Begin
                {-No more data}
                Stop := True;
              End Else Begin
                {-Buffer was too small}
                If MaxDiffBytes < 0 Then Begin
                  TempSize := - MaxDiffBytes;
                End Else Begin
                  TempSize := CurRecLen + MaxDiffBytes;
                End;
                If AdjustBuffers ( CurRecLen, TempSize ) Then Begin
                  {-Buffer adjusted, so try it again}
                  RetryRead := True;
                End Else Begin
                  {-No more memory for buffer}
                  UnDo ( 4, 10411 );
                  Exit;
                End;
              End;
            End;
          End;
        Until Not RetryRead;
      End Else Begin
        Inc (CurrentRef);
        BufGetBlock ( F, CurrentRef * DatSLenOld, DatSLenOld, BufPtrOld^ );
        If Not IsamOk Then Begin
          If IsamError = 10070 Then Begin
            IsamClearOK;
            Stop := True;
          End Else Begin
            UnDo ( 4, IsamError );
            Exit;
          End;
        End;
        CurRecLen := DatSLenOld;
      End;

      If Not Stop Then Begin
        Inc (DatSRead);
        CharConvProc ( BufPtrOld, CurRecLen, True, CCHookPtr );
        OldRecLen := CurRecLen;
        Ok := ChangeDatSFunc ( BufPtrOld^, BufPtrNew^, CurRecLen );
        If Not VarRec Then CurRecLen := DatSLen;
          {-Reset in case CurRecLen was changed}
        If Not IsamOK Then Begin
          UnDo ( 4, 10475 );
          Exit;
        End;
        If Ok Then Begin
          CharConvProc ( BufPtrNew, CurRecLen, False, CCHookPtr );
          If VarRec Then Begin
            BufAddVariableRecRebuilt ( IFBPtr, BufPtrNew, CurRecLen );
          End Else Begin
            BufAddRec ( IFBPtr, BufPtrNew^ );
          End;
          If Not IsamOK Then Begin
            UnDo ( 4, IsamError );
            Exit;
          End;
          Inc (DatSWritten);
        End;

        If Assigned(IsamReXUserProcPtr) Then Begin
          IsamReXUserProcPtr(0, DatSRead, DatSWritten,
              BufPtrOld^, OldRecLen );
          If Not IsamOK Then Begin
            UnDo ( 4, 10460 );
            Exit;
          End;
        End;
      End;
    Until Stop;

    FreeMem ( BufPtrOld, BufLenOld );
    FreeMem ( BufPtrNew, BufLenNew );
    DoneRecBuffers;
    Ok := IsamOK;
    BTCloseFileBlock ( IFBPtr );
    Ok := Ok And IsamOK;
    IsamClearOK;
    IsamClose ( F );
    If IsamOK And Ok Then Begin
      IsamDelete ( F );
    End;
  End;


  Function ChangeDatSNoChange ( Var DatSOld;
                                Var DatSNew;
                                Var Len     : Word ) : Boolean;

  Begin
    If LongInt (DatSOld) = 0 Then Begin
      Move ( DatSOld, DatSNew, Len );
      ChangeDatSNoChange := True;
    End Else Begin
      ChangeDatSNoChange := False;
    End;
  End;


{$IFDEF InitAllUnits}
Begin
{$ENDIF}
End.
