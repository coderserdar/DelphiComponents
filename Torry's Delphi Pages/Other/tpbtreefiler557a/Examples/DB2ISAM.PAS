{*********************************************************}
(*                B-Tree Filer Ver 5.55                  *)
{*     Copyright (c) TurboPower Software Co 1986-99      *}
{*                 All rights reserved.                  *}
(*      Based in part on code written by Ralf Nagel      *)
{*********************************************************}

{Note: the translations of German strings to English have not
       been marked with !!.TP}

{$IFDEF Win32}
  !! Error - this unit cannot be compiled for 32-bit
{$ENDIF}

Uses
  Crt,
  BTBase, BTIsBase,
  Filer,     {!!.TP}
  IsamTool,
  DbImpExp;

Var
  LHPtr : PListHeader;
  DBFName : DBaseFileName;
  IFBName : IsamFileBlockName;
  MemoSize : Word;
  AZStrings,
  AutoRel,
  WriteTDef : Boolean;
  Key : Char;
  Error : Integer;
  PSS : Longint;
  FuncWriteTypeDef : IntFct_WriteTDef;


  Function ParseArgs ( Var DBFName          : DBaseFileName;
                       Var IFBName          : IsamFileBlockName;
                       Var MemoSize         : Word;
                       Var AZStrings        : Boolean;
                       Var FuncWriteTypeDef : IntFct_WriteTDef;
                       Var AutoRel          : Boolean ) : Boolean;

  Var
    i,
    Error : Integer;
    DBase,
    Isam,
    Memo,
    Str,
    WType,
    ARel,
    DBFNameExpected,
    IFBNameExpected : Boolean;
    ArgStr : String;

  Begin
    ParseArgs := False;
    If ParamCount < 1 Then Exit;

    DBase := False;
    Isam := False;
    Memo := False;
    Str := False;
    WType := False;
    ARel := False;
    DBFNameExpected := False;
    IFBNameExpected := False;

    For i := 1 To ParamCount Do Begin
      ArgStr := ParamStr ( i );
      If ( ArgStr [1] <> '/' ) And ( ArgStr <> '-') Then Begin
        If DBFNameExpected Or IFBNameExpected Then Begin
          If DBFNameExpected Then Begin
            DBFName := ArgStr;
            DBase := True;
            DBFNameExpected := False;
          End;
          If IFBNameExpected Then Begin
            IFBName := ArgStr;
            Isam := True;
            IFBNameExpected := False;
          End;
        End Else Begin
          Exit;
        End;
      End Else Begin
        If DBFNameExpected Or IFBNameExpected Then Exit;
        Delete ( ArgStr, 1, 1 );
        Case UpCase ( ArgStr [1] ) Of
          'D' : Begin
            If DBase Then Exit;
            Delete ( ArgStr, 1, 1 );
            If ArgStr = '' Then Begin
              DBFNameExpected := True;
            End Else Begin
              DBFName := ArgStr;
              DBase := True;
            End;
          End;

          'I' : Begin
            If Isam Then Exit;
            Delete ( ArgStr, 1, 1 );
            If ArgStr = '' Then Begin
              IFBNameExpected := True;
            End Else Begin
              IFBName := ArgStr;
              Isam := True;
            End;
          End;

          'M' : Begin
            If Memo Then Exit;
            Delete ( ArgStr, 1, 1 );
            If ArgStr = '' Then Exit;
            Val ( ArgStr, MemoSize, Error );
            If Error <> 0 Then Exit;
            Memo := True;
          End;

          'S' : Begin
            If Str Then Exit;
            Delete ( ArgStr, 1, 1 );
            Case ArgStr [1] Of
              '+' : AZStrings := False;                        {!!.52}
              '-' : AZStrings := True;                         {!!.52}
              Else Begin
                If ArgStr = '' Then Begin
                  AZStrings := False;                          {!!.52}
                End Else Begin
                  Exit;
                End;
              End;
            End;
            Str := True;
          End;

          'T' : Begin
            If WType Then Exit;
            Delete ( ArgStr, 1, 1 );
            Case UpCase ( ArgStr [1] ) Of
              '+' : FuncWriteTypeDef := WritePascalTypeDef;
              '-' : FuncWriteTypeDef := WriteNoTypeDef;
              Else Begin
                If ArgStr = '' Then Begin
                  FuncWriteTypeDef := WritePascalTypeDef;
                End Else Begin
                  Exit;
                End;
              End;
            End;
            WType := True;
          End;

          'A' : Begin
            If ARel Then Exit;
            Delete ( ArgStr, 1, 1 );
            Case ArgStr [1] Of
              '+' : AutoRel := True;
              '-' : AutoRel := False;
              Else Begin
                If ArgStr = '' Then Begin
                  AutoRel := True;
                End Else Begin
                  Exit;
                End;
              End;
            End;
            ARel := True;
          End;

          Else Begin
            Exit;
          End;
        End;
      End;
    End;

    If Not DBase Then Exit;
  {--Standards}
    If Not Isam Then IFBName := DBFName;
    If Not Memo Then MemoSize := 512;
    If Not Str Then AZStrings := False;
    If Not WType Then FuncWriteTypeDef := WritePascalTypeDef;
    If Not ARel Then AutoRel := False;
    ParseArgs := True;
  End;


  Procedure ShowHelp;

  Begin
    {!!.TP rewritten for our style}
  (*Writeln ( 'Syntax: db2isam /D <dBASE_Filename> [/I <Filer_Filename>]'
        + ' {/<Option>(+|-|n)}' );
    Writeln;
    Writeln ( '  <dBASE_Filename>    Name for *.', DBDataExtension, ', *.',
        DBMemoExtension, ' and *.', DumpExtension, ' files' );
    Writeln ( '  <Filer_Filename>    Name for *.', DatExtension, ' and *.',
        PasIncExtension, ' files' );
    Writeln ( '  M512                max. size for memo fields '
        + '(0 <= n <= 65000)' );
    Writeln ( '  S+                  create Pascal-style strings' );
    Writeln ( '  T+                  write the record format in an include'
        + ' file' );
    Writeln ( '  A-                  add an auto-incremented field to the'
        + ' Filer records' );*)
    writeln('Syntax: DB2ISAM /D dBaseFile [options]');
    writeln;
    writeln('where dBaseFile is the name for the data file (.', DBDataExtension,
             '), memo file (.', DBMemoExtension, ')');
    writeln('      and conversion error log file (.', DumpExtension, ')');
    writeln;
    writeln('The following options can be specified:');
    writeln('   /I FilerFile   Name for the .', DatExtension, ' and .',
            PasIncExtension, ' files' );
    writeln('   /Mn            Maximum size of a memo field (0 <= n <= 65000)');
    writeln('   /S+            Convert dBase character fields to Pascal strings');
    writeln('   /T+            Create a Pascal include file');
    writeln('   /A-            Insert an auto-incrementing field into the record');
  End;


  {$F+}
  Function UserInfo (     Status : WorkStatus;
                          LHPtr  : PListHeader;
                          ReadRecs,
                          WriteRecs,
                          ErrorRecs : Longint;
                      Var DatSBuf ) : Integer;

  Const
    Percent : Integer = 0;
    X : Integer = 0;
    Y : Integer = 0;
    Records : Longint = 0;

  Var
    NewPercent : Integer;
    Key,
    Dummy : Char;

  Begin
    UserInfo := 0;

    Case Status Of
      WSInit : Begin
        Percent := 0;
        Records := ReadRecs;
        Writeln;
        Writeln;
        Writeln;
        Writeln;
        Writeln;
        Writeln;
        X := Wherex;
        Y := Wherey - 5;
        GotoXY ( X, Y - 1 );
        Write ( 'The dBASE-file contains ', Records, ' record');
        If Records = 1 Then Begin
          Writeln ( '.' );
        End Else Begin
          Writeln ( 's.' );
        End;
        Writeln ( ' Abort the conversion by pressing any key.' );
        Exit;
      End;

      WSWork : Begin
        NewPercent := Integer ( ReadRecs * Longint ( 100 ) Div Records );
        If ( NewPercent = 0 ) Or ( NewPercent <> Percent ) Then Begin
          GotoXY ( X, Y );
          Writeln ( 'Converted: ', NewPercent, '%' );
          Percent := NewPercent;
        End;

        If KeyPressed Then Begin
          While KeyPressed Do Dummy := ReadKey;
          Repeat
            GotoXY ( X, Y + 1 );
            Write ( 'Terminate conversion process? (Y/N):' );
            Key := UpCase ( ReadKey );
          Until ( Key = 'Y' ) Or ( Key = 'N' );
          If Key = 'Y' Then Begin
            UserInfo := Integer ( Key );
            Exit;
          End;
          DelLine;
        End;
        Exit;
      End;

      WSExit : Begin
        GotoXY (X, Y + 1);
        Writeln ( 'Results: ', ReadRecs, ' records read; ',
            WriteRecs, ' records written' );
        If ErrorRecs <> 0 Then Begin
          Write ( 'Data conversion errors were found in ', ErrorRecs,
              ' record' );
          If ErrorRecs = 1 Then Begin
            Writeln ( '.' );
          End Else Begin
            Writeln ( 's.' );
          End;
          Writeln ( 'The conversion error log file has the details.' );
        End;
        Exit;
      End;
    End;
  End;
  {$F-}


  {$F+}
  Procedure ErrorHandler ( Reaction   : ErrorReaction;
                           IsamError  : Integer;
                           DBaseError : DBaseErrorNr );

  Begin
    Case Reaction Of
      ERAbort : Begin
        Writeln;
        Writeln ( 'Program aborted!' );
      End;

      ERIgnore : Exit;
    End;
    If IsamError <> 0 Then Begin
      Writeln ( 'IsamError: ', IsamErrorMessage ( IsamError ) );
    End;
    If DBaseError <> DEZERO Then Begin
      Writeln ( 'DBaseError: ', DBaseErrorMessage ( DBaseError ) );
    End;
  End;
  {$F-}


Begin
  Writeln ( 'dB2Isam, Copyright (C) ENZ EDV-Beratung GmbH, 1993' );
  Writeln ( 'dBase to B-Tree Filer Conversion Utility' );
  Writeln;

  If Not ParseArgs ( DBFName, IFBName, MemoSize, AZStrings,
      FuncWriteTypeDef, AutoRel ) Then Begin
    ShowHelp;
    Halt;
  End;

  UseErrorMessage := German;
  DBUseErrorMessage := DBGerman;
  ProcErrorHandler := ErrorHandler;

  PSS := BTInitIsam ( NoNet, MinimizeUseOfNormalHeap, 0 );
  If Not IsamOK Then Begin
    ErrorHandler ( ERAbort, IsamError, DEZERO );
    Halt;
  End;

  IFBName := IsamForceExtension ( IFBName, DatExtension );
  If IsamExists ( IFBName ) Then Begin
    While KeyPressed Do Key := ReadKey;
    Repeat
      Write ( IFBName, ' already exists! Overwrite it? (Y/N) :' );
      Key := UpCase ( ReadKey );
      Writeln ( Key );
    Until ( Key = 'Y' ) Or ( Key = 'N');
    If Key = 'N' Then Begin
      BTExitIsam;
      Halt;
    End;
  End;

  If CreateListHeaderUseDBaseFiles ( LHPtr, DBFName, MemoSize ) <> 0
      Then Begin
    BTExitIsam;
    Halt;
  End;

  If CompleteDBaseList ( LHPtr, AZStrings, AutoRel ) <> 0 Then Begin
    Error := CloseDBaseFiles ( LHPtr );
    FreeListHeader ( LHPtr );
    BTExitIsam;
    Halt;
  End;

  Error := DBaseImport ( LHPtr, IFBName, FuncWriteTypeDef, UserInfo,
      NoCArrConv, StdDecideWrite );

  Error := CloseDBaseFiles ( LHPtr );

  FreeListHeader ( LHPtr );

  BTExitIsam;
  If Not IsamOK Then Begin
    ErrorHandler ( ERAbort, IsamError, DEZERO );
  End;

  Writeln;
End.
