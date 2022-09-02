{*********************************************************}
(*                B-Tree Filer Ver 5.55                  *)
{*     Copyright (c) TurboPower Software Co 1986-99      *}
{*                 All rights reserved.                  *}
(*      Based in part on code written by Ralf Nagel      *)
{*********************************************************}

Program Isam2dB;

{$IFDEF Win32}
  !! Error - this unit cannot be compiled for 32-bit
{$ENDIF}

{--Conditional defines and compiler options}
  {$I BTDefine.Inc}

  {$IFDEF UseSymFlex}
    {$I DDDefine.Inc}
  {$ENDIF}

Uses
  Crt,
  Dos,
  BTBase, BTIsBase,
  Filer,     {!!.TP}
{$IFDEF UseSymFlex}
  FLibSupp,
  DataDict,
{$ENDIF}
  IsamTool,
  DBImpExp;


Const
{--FileDescrArr can be incremented. FArr must contain FileDescrArr
   file names. Function CreateListHeaderOpenAFileBlock must treat
   FileDescrArr files.}
  FileDescrArr = 1;
  FArr : Array [1 .. FileDescrArr] Of String = ( 'ADDRESS.DAT' );

(***Definition of the ADDRESS.DAT record layout

-Please note that this is a variable-length record fileblock. We ensure that
 only the first section of a record is converted by rejecting records that
 have an $01 as their first byte (these are 'continuation records' - the
 additional sections). StdDecideWrite will do this for us, these records
 'appear' to be deleted.

    record
      Dele           : LongInt;      {  0}
      FirstName      : String[15];   {  4}
      Name           : String[15];   { 20}
      Company        : String[25];   { 36}
      Address        : String[25];   { 62}
      City           : String[15];   { 88}
      State          : String[2];    {104}
      Zip            : String[10];   {107}
      Telephone      : String[12];   {118}
      NotesLen       : Word;      {<-- 133 bytes to here}
      Notes          : MemoField; {memo field: 1..MaxMemoSize bytes}
    end;                          {1065 bytes maximum, 134 minimum}
 ***)

Var
  LHPtr   : PListHeader;
  DBFName : DBaseFileName;
  IFBName : IsamFileBlockName;
  DBVer   : DBaseVersion;
  KeyNr   : Word;
  Key     : Char;
  Error   : Integer;
  PSS     : Longint;


  Function ParseArgs ( Var DBFName : DBaseFileName;
                       Var IFBName : IsamFileBlockName;
                       Var DBVer   : DBaseVersion;
                       Var KeyNr   : Word ) : Boolean;

  Var
    i,
    Error : Integer;
    DBase,
    Isam,
    DBV,
    Key,
    DBFNameExpected,
    IFBNameExpected : Boolean;
    ArgStr : String;

  Begin
    ParseArgs := False;
    If ParamCount < 1 Then Exit;

    DBase := False;
    Isam := False;
    DBV := False;
    Key := False;
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

          'V' : Begin
            If DBV Then Exit;
            Delete ( ArgStr, 1, 1 );
            If ArgStr = '' Then Exit;
            Val ( ArgStr, DBVer, Error );
            If Error <> 0 Then Exit;
            Case DBVer Of
              3 : DBVer := DBVersion3X;
              4 : DBVer := DBVersion4X;
              Else Exit;
            End;
            DBV := True;
          End;

          'K' : Begin
            If Key Then Exit;
            Delete ( ArgStr, 1, 1 );
            If ArgStr = '' Then Exit;
            Val ( ArgStr, KeyNr, Error );
            If Error <> 0 Then Exit;
            Key := True;
          End;

          Else Begin
            Exit;
          End;
        End;
      End;
    End;

    If Not Isam Then Exit;
  {--Standards}
    If Not DBase Then DBFName := IFBName;
    If Not DBV Then DBVer := DBVersion3X;
    If Not Key Then KeyNr := 0;
    ParseArgs := True;
  End;


  Procedure ShowHelp;

  Var
    I : Integer;

  Begin
    {!!.TP rewritten for our style}
  (*Writeln ( 'Syntax: Isam2dB /I <Filer_filename> [/D <dBASE_filename>]'
        + ' {/<Option>n}' );
    Writeln;
    Writeln ( '  <Filer_filename>    Name for *.', DatExtension, ' and *.',
        DumpExtension, ' files' );
    Writeln ( '  <dBASE_filename>    Name for *.', DBDataExtension,
        ' and *.', DBMemoExtension, ' files' );
    Writeln ( '  V3                  type of dBase compatible file to'
        + ' create' );
    Writeln ( '                      (3 = dBASE III) or (4 = dBASE IV)' );
    Writeln ( '  K0                  index number of the B-Tree Filer'
        + ' fileblock to use');
    Writeln ( '                      (0 (no index) <= n <= 254)' );*)
    writeln('Syntax: ISAM2DB /I FilerFile [options]');
    writeln;
    writeln('where FilerFile is the name for the B-Tree Filer data file (.', DatExtension, ') and');
    writeln('      index file (.', IxExtension, ') and also the conversion error log file (.', DumpExtension, ')');
    writeln;
    writeln('The following options can be specified:');
    writeln('   /D dBaseFile   Name  for the .', DBDataExtension,
             ' and .', DBMemoExtension, ' files');
    writeln('   /Vn            Type of the dBase file to create (3 for dBase III or');
    writeln('                  4 for dBase IV)');
    writeln('   /Kn            Key number for reading the B-Tree Filer fileblock (0 to read');
    writeln('                  in sequential order, 1 to use the first index, etc)');
    Writeln;
    Writeln ( 'This program will (can) only convert the following files:');
    For I := 0 To FileDescrArr - 1 Do Begin
      If I Mod 5 = 0 Then Begin
        Writeln;
        Write ( '   ' );
      End;
      Write ( FArr [I+1], ' ' : 15 - Length ( FArr [I+1] ) );
    End;
    Writeln;
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
        Write ( 'The B-Tree Filer file contains ', Records, ' record' );
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
            Write ( 'Terminate conversion? (Y/N):' );
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
        Writeln ( 'Results: ', ReadRecs, ' records read; ', WriteRecs,
            ' records written' );
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
        If IsamError <> 0 Then Begin
          Writeln ( 'IsamError: ', IsamErrorMessage ( IsamError ) );
        End;
        If DBaseError <> DEZERO Then Begin
          Writeln ( 'DBaseError: ', DBaseErrorMessage ( DBaseError ) );
        End;
        Writeln ( 'Program aborted!' );
      End;

      ERIgnore : Exit;
    End;
  End;
  {$F-}


  Function FileNamesCmp ( FName1,
                          FName2 : PathStr ) : Boolean;

  Var
    D1, D2 : DirStr;
    N1, N2 : NameStr;
    E1, E2 : ExtStr;

    Function StrUpr ( S : String ) : String;

    Var
      Idx : Integer;

    Begin
      For Idx := 1 To Length ( S ) Do
        S [Idx] := UpCase ( S [Idx] );
      StrUpr := S;
    End;

  Begin
    FSplit ( FName1, D1, N1, E1 );
    FSplit ( FName2, D2, N2, E2 );
    FileNamesCmp := StrUpr ( N1 ) = StrUpr ( N2 );
  End;


  Function CreateListHeaderOpenAFileBlock (
      Var LHPtr       : PListHeader;
          BTIFileName : IsamFileBlockName ) : Integer;

  Var
    Error,
    DescrInd : Integer;
    Found,
    OK       : Boolean;

  Begin
    CreateListHeaderOpenAFileBlock := -1;

    Found := False;
    DescrInd := 0;
    Repeat
      Inc ( DescrInd );
      Found := FileNamesCmp ( BTIFileName, FArr [DescrInd] );
    Until Found Or ( DescrInd > FileDescrArr );
    If Not Found Then Begin
      ShowHelp;
      Exit;
    End;

    Case DescrInd Of
      1 : Begin {'Address.Dat'} {!!.TP}
        LHPtr := CreateListHeaderOpenFileBlock ( BTIFileName );
        If LHPtr = Nil Then Exit;
        If Not AddFieldNode ( LHPtr, DelMarkName, ReservedCType,
             4,   0, -1, -1 ) Then Exit;
        If Not AddFieldNode ( LHPtr, 'NAME', StringCType,
            16,  20, -1, -1 ) Then Exit;
        If Not AddFieldNode ( LHPtr, 'FIRSTNAME', StringCType,
            16,   4, -1, -1 ) Then Exit;
        If Not AddFieldNode ( LHPtr, 'CITY', StringCType,
            16,  88, -1, -1 ) Then Exit;
        If Not AddFieldNode ( LHPtr, 'STATE', StringCType,
             3, 104, -1, -1 ) Then Exit;
        If Not AddFieldNode ( LHPtr, 'ZIP', StringCType,
            11, 107, -1, -1 ) Then Exit;
        If Not AddFieldNode ( LHPtr, 'PHONE', StringCType,
            13, 118, -1, -1 ) Then Exit;
      End;

      Else Exit;
    End;

    CreateListHeaderOpenAFileBlock := 0;
  End;


Begin
  Writeln ( 'Isam2dB, Copyright (C) ENZ EDV-Beratung GmbH, 1993' );
  Writeln ( 'B-Tree Filer to dBase Conversion Utility' );
  Writeln;

  If Not ParseArgs ( DBFName, IFBName, DBVer, KeyNr ) Then Begin
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

  DBFName := IsamForceExtension ( DBFName, DBDataExtension );
  If IsamExists ( DBFName ) Then Begin
    While KeyPressed Do Key := ReadKey;
    Repeat
      Write ( DBFName, ' already exists! Overwrite it? (Y/N) :' );
      Key := UpCase ( ReadKey );
      Writeln ( Key );
    Until ( Key = 'Y' ) Or ( Key = 'N');
    If Key = 'N' Then Begin
      BTExitIsam;
      Halt;
    End;
  End;

  If CreateListHeaderOpenAFileBlock ( LHPtr, IFBName ) <> 0 Then Begin
    BTExitIsam;
    Halt;
  End;

  If CompleteIsamList ( LHPtr, DBVer ) <> 0 Then Begin
    Error := CloseIsamFiles ( LHPtr );
    FreeListHeader ( LHPtr );
    BTExitIsam;
    Halt;
  End;

  Error := DBaseExport ( LHPtr, DBFName, KeyNr, UserInfo,
      NoCArrConv, StdDecideWrite );

  Error := CloseIsamFiles ( LHPtr );

  FreeListHeader ( LHPtr );

  BTExitIsam;
  If Not IsamOK Then Begin
    ErrorHandler ( ERAbort, IsamError, DEZERO );
  End;

  Writeln;
End.
