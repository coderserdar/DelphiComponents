{********************************************************************}
{* DBIMPEXP.PAS - dBASE import/export                               *}
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
{$IFDEF UseSymFlex}
  {$I DDDefine.Inc}
{$ENDIF}
{$IFDEF CanAllowOverlays}
  {$O+,F+}
{$ENDIF}

{$IFDEF Win32}
  !! Error - this unit cannot be compiled for 32-bit
{$ENDIF}

Unit
  DBImpExp;

Interface

Uses
{$IFDEF Windows}
 {$IFDEF VER80}                                                 {!!.51}
  DosSupp,                                                      {!!.51}
  SysUtils,                                                     {!!.51}
 {$ELSE}                                                        {!!.51}
  Strings,
  WinDos,
 {$ENDIF}                                                       {!!.51}
{$ELSE}
  Dos,
{$ENDIF}
  BTBase,
  BTIsBase,
  Filer,     {!!.TP}
{$IFDEF UseSymFlex}
  FLibSupp,
  DataDict,
{$ENDIF}
  CArrConv;


{--Definition of possible languages for error messages of DBaseErrorMessage}
  (**************************************************************************)
  (*******************)  {$DEFINE DBaseGermanMessage}   (********************)
  (*******************)  { $DEFINE DBaseEnglishMessage}  (********************)
  (**************************************************************************)
  {-Adding a space before the $ sign of the DEFINE deactivates the error
    messages of this language}


Const
{--Interfaced constants}
  DBDataExtension  : String [3] = 'DBF';
  DBMemoExtension  : String [3] = 'DBT';
  DumpExtension    : String [3] = 'DMP';
  PasIncExtension  : String [3] = 'INC';

  StartAutoRel : LongInt = 1;                                    {!!.42mod}

  DBFieldNameLen         = 10;
  AutoRelName            = '_AutoRel';
  DelMarkName            = '_DelMark';

{--FType}
  DelMarkFType           = #0;
    {-Not a dBASE field type, but describes the 1st byte of a dBASE record}
  DateFType              = 'D';
  CharFType              = 'C';
  LogicFType             = 'L';
  NumerFType             = 'N';
  FloatFType             = 'F';
  MemoFType              = 'M';

{$IFNDEF UseSymFlex}
{--CType}
  ReservedCType   = 0;
  BooleanCType    = 1;
  CharCType       = 2;
  ByteCType       = 3;
  ShortIntCType   = 4;
  IntegerCType    = 5;
  WordCType       = 6;
  LongIntCType    = 7;
  CompCType       = 8;
  RealCType       = 9;
  SingleCType     = 10;
  DoubleCType     = 11;
  ExtendedCType   = 12;
  StringCType     = 13;
  ArrayCType      = 14;
{$ENDIF}                                                        {!!.42}
{$IFNDEF SymFlexGE20}                                           {!!.42}
  AZStringCType   = 15;
  DateCType       = 16;
  TimeCType       = 17;
{$ENDIF}

{--DecideCase}
  DCWrite = 0;
  DCSkip  = 1;
  DCAbort = 2;

{--WorkStatus}
  WSInit = 0;
  WSWork = 1;
  WSExit = 2;

{--ErrorReaction}
  ERAbort  = 0;
  ERIgnore = 1;

{--DBaseErrorNr}
  DEZERO    =    0; {No error}
  DEEOF     = 9011; {End of file}
  DEOOM     = 9012; {Out of memory}
  DEBV      = 9013; {Bad version or not a dBASE III/IV file}
  DECMF     = 9014; {Corrupted memo file}
  DERSTL    = 9015; {Record size to large}
  DEWCT     = 9016; {Wrong CType}
  DEEWTD    = 9017; {Error writing type definition file}
  DEECF     = 9018; {Error converting field}
  DELHNI    = 9019; {List header not initialized or bad part}
  DETMF     = 9020; {Too many fields}
  DEWFT     = 9021; {Wrong field type}
  DEFWTL    = 9022; {Field width too large}
  DETMD     = 9023; {Too many decimals}
  DEFTVC    = 9024; {Field type version conflict}
  DEARFNA   = 9025; {Auto relation field is not allowed here}
  DEFCNMF   = 9026; {File contains no memo fields}
  DEEODF    = 9027; {Error opening dump file}
  DEEWDF    = 9028; {Error writing to dump file}
  DEECDF    = 9029; {Error closing dump file}
  DEPE      = 9030; {Programming error}
  DEFNAE    = 9031; {Field name already exists}
  DENFD     = 9032; {No field defined}
  DELAST    = 9033; {Last error const, not an error}

{--DBaseVersion}
  DBVersion3X = $0300;
  DBVersion4X = $0400;

{--Private constants}
  DB4MaxFields = 255;


{--Interfaced types}
Type
  DBaseUsedErrorMessages = ( DBNoMsg, DBGerman, DBEnglish );

  DBaseFieldName = Array [0 .. DBFieldNameLen] Of Char;
    {-dBASE field name}

  DBaseFieldNameStr = String [DBFieldNameLen];

  DBaseFileName = IsamFileName;
    {-DOS file name}

  DBaseVersion = Integer;


{--Private types}
  DBaseFileField = Record
    Name         : DBaseFieldName;
    FType        : Char;
    Address      : ^Char;
    Width,
    Decimals     : Byte;
    Reserved1    : Array [0 .. 1] of Byte;
    IDWorkReg    : Byte;
    Reserved2    : Array [0 .. 1] of Byte;
    SetFieldFlag : Byte;
    Reserved3    : Array [0 .. 7] of Byte;
  End;
  PDBaseFileField = ^DBaseFileField;
    {-dBASE file descriptor of a field}

  DBaseFileDate = Record
    Year,
    Month,
    Day    : Byte;
  End;
  PDBaseFileDate = ^DBaseFileDate;
    {-dBASE date descriptor}

  DBaseFileFieldArray = Array [0 .. DB4MaxFields-1] of DBaseFileField;
  PDBaseFileFieldArray = ^DBaseFileFieldArray;
    {-An array of dBASE descriptors}

  DBaseFileHeader = Record
    DBaseVer   : Byte;
    LastChange : DBaseFileDate;
    NrOfRecs   : Longint;
    HeaderSize,
    RecordSize : Word;
  End;
  PDBaseFileHeader = ^DBaseFileHeader;

  DBaseFileFullHeader = Record
    Part           : DBaseFileHeader;
    Reserved1      : Array [0 .. 1] Of Char;
    TransActionFlag,
    EncryptionFlag : Byte;
    Reserved2      : Array [0 .. 11] Of Char;
    MDXFlag        : Byte;
    Reserved3      : Array [0 .. 2] Of Char;
  End;
  PDBaseFileFullHeader = ^DBaseFileFullHeader;
    {-dBASE file descriptor of '.DBF' file}

  DBaseMemoHeader = Record
    NextFree,
    Dummy1   : Longint;
    FileName : Array [0 .. 7] Of Char;
    Valid    : Longint;
    BlockSize,
    Dummy2   : Word;
  End;
  PDBaseMemoHeader = ^DBaseMemoHeader;
    {-dBASE descriptor of '.DBT' file}

  DBaseMemoRecord = Record
    Rec : Array [0 .. 511] Of Char;
  End;
  PDBaseMemoRecord = ^DBaseMemoRecord;

  DBase4FirstMemoRec = Record
    Valid,
    Width : Longint;
  End;
  PDBase4FirstMemoRec = ^DBase4FirstMemoRec;

  DBaseMemoRec = Record
    Case Byte Of
      1 : ( Header : DBaseMemoHeader; );
      2 : ( Rec    : DBaseMemoRecord; );
      3 : ( RecIV  : DBase4FirstMemoRec; );
  End;
  PDBaseMemoRec = ^DBaseMemoRec;

  IsamField = Record
    CType,
    BufSize,
    Offset   : Word;
    Width,
    Decimals : Integer;
  End;
  PIsamField = ^IsamField;

  DBaseNormalField = Record
    FType    : Char;
    Width,
    Decimals : Byte;
    Offset   : Word;
  End;
  PDBaseNormalField = ^DBaseNormalField;

  DBaseAutoRelField = Record
    Relation : Longint;
  End;
  PDBaseAutoRelField = ^DBaseAutoRelField;

  DBaseField = Record
    Case NormalContents : Boolean Of
      True  : ( Normal : DBaseNormalField; );
      False : ( Auto   : DBaseAutoRelField; );
  End;
  PDBaseField = ^DBaseField;

  PFieldNode = ^FieldNode;
  FieldNode = Record
    FieldName   : DBaseFieldNameStr;
    ConvStatus  : Integer;
    DBFieldPtr  : PDBaseField;
    BTFieldPtr  : PIsamField;
    NextPtr     : PFieldNode;
  End;

  IsamHeader = Record
    DatSLen : Word;
    FBPtr   : IsamFileBlockPtr;
  End;
  PIsamHeader = ^IsamHeader;

  DBaseMemo = Record
    IFile    : IsamFile;
    Modified : Boolean;
    Header   : DBaseMemoHeader;
    MaxSize  : Word;
  End;
  PDBaseMemo = ^DBaseMemo;

  DBaseHeader = Record
    Header   : DBaseFileHeader;
    DBVer    : DBaseVersion;
    RefNr    : Longint;
    Fields   : Byte;
    IFile    : IsamFile;
    Modified : Boolean;
    MemoPtr  : PDBaseMemo;
  End;
  PDBaseHeader = ^DBaseHeader;

  ListHeader = Record
    DBSource    : Boolean;
    ListPtr     : PFieldNode;
    DBHeaderPtr : PDBaseHeader;
    BTHeaderPtr : PIsamHeader;
  End;

{--Interfaced types}
  PListHeader = ^ListHeader;

  DecideCase = Integer;
  EnumFct_DecideWrite = Function (     LHPtr  : PListHeader;
                                       Errors : Integer;
                                   Var BTBuf,
                                       DBBuf ) : DecideCase;

  WorkStatus = Integer;
  IntFct_ReXUser = Function (     Status    : WorkStatus;
                                  LHPtr     : PListHeader;
                                  ReadRecs,
                                  WriteRecs,
                                  ErrorRecs : Longint;
                              Var DatSBuf ) : Integer;

  ErrorReaction = Integer;
  DBaseErrorNr = Integer;
  VoidFct_ErrorHandler = Procedure ( Reaction   : ErrorReaction;
                                     IsamError  : Integer;
                                     DBaseError : DBaseErrorNr );
    {-Either DBaseError or IsamError is set}

  IntFct_WriteTDef = Function ( LHPtr  : PListHeader;
                                IFName : IsamFileName ) : Integer;

  VoidFct_CharArrConvert = Procedure ( CArrPtr : Pointer;
                                       Len     : Word );

{--Interfaced routines}
  Function DBaseErrorMessage ( ErrorNr : DBaseErrorNr ) : String;
    {-Returns an error string}

  Function CreateListHeaderUseDBaseFiles
      ( Var LHPtr       : PListHeader;
            DBFileName  : DBaseFileName;
            MaxMemoSize : Word ) : Integer;
    {-Opens an existing dBASE III/IV file and builds the source (dBASE)
      part of the list}

  Function CompleteDBaseList ( LHPtr : PListHeader;
                               AZStrs,
                               AutoRel : Boolean ) : Integer;
    {-Builds the destination (B-Tree Isam) part of the list}

  Function WriteNoTypeDef ( LHPtr  : PListHeader;
                            IFName : IsamFileName ) : Integer;
    {-Passed to DBaseImport to write no type definition}

  Function WritePascalTypeDef ( LHPtr  : PListHeader;
                                IFName : IsamFileName ) : Integer;
    {-Passed to DBaseImport to write a Pascal type definition}

  Function DBaseImport ( LHPtr            : PListHeader;
                         IFBName          : IsamFileBlockName;
                         FuncWriteTypeDef : IntFct_WriteTDef;
                         FuncReXUser      : IntFct_ReXUser;
                         ProcCArrConv     : VoidFct_CharArrConvert;
                         FuncDecideWrite  : EnumFct_DecideWrite )
                                                             : Integer;
    {-Converts the dBASE file to a B-Tree Isam file}

  Function CloseDBaseFiles ( LHPtr : PListHeader ) : Integer;
    {-Closes dBASE file(s)}

  Function CreateListHeaderOpenFileBlock ( BTFileName : IsamFileBlockName )
                                                            : PListHeader;
    {-Allocates a ListHeader and returns a pointer to it and opens the
      B-Tree Isam Fileblock BTFileName; if an error occurs the error
      handler is called and Nil is returned}

  Function AddFieldNode ( Var LHPtr     : PListHeader;
                              Name      : DBaseFieldNameStr;
                              CType,
                              BufSize,
                              Offset    : Word;
                              Width,
                              Decimals  : Integer ) : Boolean;
    {-Allocates a FieldNode and inserts it at the end of the list LHPtr^;
      the B-Tree Isam part is allocated and filled with arguments;
      if an error occurs the error handler is called, files are closed,
      LHPtr is freed, and False is returned}

  Function CompleteIsamList ( LHPtr : PListHeader;
                              DBVer : DBaseVersion ) : Integer;
    {-Builds the destination (dBASE) part of list}

  Function DBaseExport ( LHPtr           : PListHeader;
                         DBFName         : DBaseFileName;
                         KeyNr           : Word;
                         FuncReXUser     : IntFct_ReXUser;
                         ProcCArrConv    : VoidFct_CharArrConvert;
                         FuncDecideWrite : EnumFct_DecideWrite )
                                                              : Integer;
    {-Converts the B-Tree Isam Fileblock to a dBASE file}

  Function CloseIsamFiles ( LHPtr : PListHeader ) : Integer;
    {-Closes B-Tree Isam file(s)}

  Procedure FreeListHeader ( Var LHPtr : PListHeader );
    {-Deallocates the LHPtr and its structures}

  Procedure NoErrorHandler ( Reaction   : ErrorReaction;
                             IsamError  : Integer;
                             DBaseError : DBaseErrorNr);
    {-Assignable to ProcErrorHandler to install no error handler}

  Function NoReXUser (     Status    : WorkStatus;
                           LHPtr     : PListHeader;
                           ReadRecs,
                           WriteRecs,
                           ErrorRecs : Longint;
                       Var DatSBuf ) : Integer;
    {-Should be passed to DBaseImport and DBaseExport in FuncReXUser
      if no status infomations of import/export are needed.}

  Procedure NoCArrConv ( CArrPtr : Pointer;
                         Len     : Word );

  Function StdDecideWrite (     LHPtr   : PListHeader;
                                Errors  : Integer;
                            Var BTBuf,
                                DBBuf ) : DecideCase;
    {-Decides whether the converted record is written or not}


{--Interfaced typed constants and variables}
Const
  DBUseErrorMessage : DBaseUsedErrorMessages =
  {$IFDEF DBaseEnglishMessage}
    DBEnglish;
  {$ELSE}
    {$IFDEF DBaseGermanMessage}
      DBGerman;
    {$ELSE}
      DBNoMsg;
    {$ENDIF}
  {$ENDIF}

Var
  ProcErrorHandler : VoidFct_ErrorHandler;                          {!!.42}

Implementation {=========================================================}


Const
  DBMaxFields            = 128;
  DBDateFieldWidth       = 8;
  DBLogicFieldWidth      = 1;
  DBMemoFieldWidth       = 10;
  DBMaxCharFieldWidth    = 254;
  DBMaxNumFieldWidth     = 19;
  DBMaxNumFieldDecimals  = 15;
  DB4MaxNumFieldWidth    = 20;
  DB4MaxNumFieldDecimals = 18;
  DBMaxRecSize           = 4001;
    {-Maximum record size + 1 byte (dBASE delete mark)}
  DBMinMemoRecSize       = 512;
  DBMaxMemoSize          = $FFF7; {64 K Bytes - 8 Bytes}
  DBDataOnly             = $03;
  DBDataAndMemo          = $83;
  DB4DataAndMemo         = $8B;
  DB4ValidMemoField      = $0008FFFF;
  DB4ValidMemoFile       = $01020000;
  DBEndOfHeader          = #$0D;
  DBEndOfFile            = #$1A;
  DBEndOfMemoRec         = #$1A;


Type
  CharArr = Array [0 .. $FFFE] Of Char;
  PText = ^Text;
  PCharArr = ^CharArr;
{$IFNDEF UseSymFlex}                                                {!!.42}
  {$IFDEF Ver55}                                                    {!!.42}
    PChar = ^Char;                                                  {!!.42}
  {$ENDIF}                                                          {!!.42}
  {$IFDEF Ver60}                                                    {!!.42}
    PChar = ^Char;                                                  {!!.42}
  {$ENDIF}                                                          {!!.42}
{$ELSE}                                                             {!!.42}
 {$IFNDEF SymFlexGE20}                                              {!!.42}
  {$IFDEF Ver55}                                                    {!!.42}
    PChar = ^Char;                                                  {!!.42}
  {$ENDIF}                                                          {!!.42}
  {$IFDEF Ver60}                                                    {!!.42}
    PChar = ^Char;                                                  {!!.42}
  {$ENDIF}                                                          {!!.42}
 {$ENDIF}                                                           {!!.42}
{$ENDIF}                                                            {!!.42}


{$IFNDEF SymFlexGE20}
  Function GetAZSLength ( AZSPtr : PChar ) : Word;

  Var
    L : Word;

  Begin
  {$IFDEF Windows}
    GetAZSLength := StrLen (AZSPtr);
  {$ELSE}
    Inline (
               $FC /                          { Cld }
               $C4 / $BE / AZSPtr /           { Les DI, AZSPtr [BP] }
               $B9 / $FF / $FF /              { Mov CX, $FFFF }
               $31 / $C0 /                    { Xor AX, AX }
               $F2 /                          { Repne }
               $AE /                          { Scasb }
               $B8 / $FE / $FF /              { Mov AX, $FFFE }
               $29 / $C8 /                    { Sub AX, CX }
               $89 / $86 / L                  { Mov [BP+Ofs(L)], AX }
                                                                       );
    GetAZSLength := L;
  {$ENDIF}
  End;


  Function BytePosInMem ( ToFindByte : Byte;
                          MemPtr     : Pointer;
                          MaxBytes   : Word ) : Word;

  Var
    LResult : Word;                                             {!!.51}

  Begin
    If MaxBytes = 0 Then Begin
      BytePosInMem := 0;
    End Else Begin
      Inline (
               $FC /                          { Cld }
               $C4 / $BE / MemPtr /           { Les DI, MemPtr [BP] }
               $8B / $8E / MaxBytes /         { Mov CX, [BP+Ofs(MaxBytes)] }
               $8A / $86 / ToFindByte /       { Mov AL, [BP+Ofs(ToFindByte)] }
               $F2 /                          { Repne }
               $AE /                          { Scasb }
               $8B / $86 / MaxBytes /         { Mov AX, [BP+Ofs(MaxBytes)] }
               $74 / $01 /                    { Jz Found }
               $40 /                          { Inc AX }
       {Found:}
               $29 / $C8 /                    { Sub AX, CX }
               $89 / $86 / LResult            { Mov [BP+Ofs(LResult)], AX }{!!.51}
                                                                       );
      BytePosInMem := Pred (LResult);                           {!!.51}
    End;
  End;
{$ENDIF}


  Function GetFNameOnly ( FName : IsamFileName ) : IsamFileName;

  Var
  {$IFDEF Windows}                                                   {!!.42}
    Idx  : Integer;
  {$ELSE}                                                            {!!.42}
    Dir  : DirStr;                                                   {!!.42}
    Name : NameStr;                                                  {!!.42}
    Ext  : ExtStr;                                                   {!!.42}
  {$ENDIF}                                                           {!!.42}

  Begin
  {$IFDEF Windows}                                                   {!!.42}
    FName := IsamForceExtension ( FName, '' );
    Delete ( FName, Length ( FName ), 1 );
    Idx := Length ( FName );
    While (Idx > 0) And (FName [Idx] <> '\')                      {!!.42mod}
        And (FName [Idx] <> ':') Do Begin                         {!!.42mod}
      Dec ( Idx );
    End;

    GetFNameOnly := Copy ( FName, Succ (Idx), Length ( FName ) - Idx );

  {$ELSE}                                                            {!!.42}
    FSplit ( FName, Dir, Name, Ext );                                {!!.42}
    GetFNameOnly := Name;                                            {!!.42}
  {$ENDIF}                                                           {!!.42}
  End;


  Function StrUpCase ( Src : String ) : String;                   {!!.42mod}

  Var
    Idx : Byte;

  Begin
    For Idx := 1 To Length ( Src ) Do                             {!!.42mod}
      Src [Idx] := UpCase ( Src [Idx] );
    StrUpCase := Src;                                             {!!.42mod}
  End;


  Procedure NoErrorHandler ( Reaction   : ErrorReaction;
                             IsamError  : Integer;
                             DBaseError : DBaseErrorNr );

  Begin
  End;


  Procedure CallProcErrorHandler ( Reaction   : ErrorReaction;
                                   IsamError  : Integer;
                                   DBaseError : DBaseErrorNr );

  Begin
    If (@ProcErrorHandler <> @NoErrorHandler) And
        (@ProcErrorHandler <> Nil) Then Begin
      ProcErrorHandler ( Reaction, IsamError, DBaseError );
    End;
  End;


  Function CallFuncReXUser (     FuncReXUser : IntFct_ReXUser;
                                 Status      : WorkStatus;
                                 LHPtr       : PListHeader;
                                 ReadRecs,
                                 WriteRecs,
                                 ErrorRecs   : Longint;
                             Var DatSBuf )   : Integer;

  Begin
    If (@FuncReXUser <> @NoReXUser) And
        (@FuncReXUser <> Nil) Then Begin
      CallFuncReXUser := FuncReXUser ( Status, LHPtr, ReadRecs,
          WriteRecs, ErrorRecs, DatSBuf );
    End Else Begin
      CallFuncReXUser := 0;
    End;
  End;


  Procedure CallProcCArrConv ( ProcCArrConv : VoidFct_CharArrConvert;
                               CArrPtr      : Pointer;
                               Len          : Word );

  Begin
    If (@ProcCArrConv <> @NoCArrConv) And
        (@ProcCArrConv <> Nil) Then Begin
      ProcCArrConv ( CArrPtr, Len );
    End;
  End;


  Function CallFuncDecideWrite (     FuncDecideWrite : EnumFct_DecideWrite;
                                     LHPtr           : PListHeader;
                                     Errors          : Integer;
                                 Var BTBuf,
                                     DBBuf )         : DecideCase;

  Begin
    If @FuncDecideWrite <> Nil Then Begin
      CallFuncDecideWrite := FuncDecideWrite ( LHPtr, Errors, BTBuf,
          DBBuf);
    End Else Begin
      CallFuncDecideWrite := DCWrite;
    End;
  End;


  Function StdDecideWrite (     LHPtr   : PListHeader;
                                Errors  : Integer;
                            Var BTBuf,
                                DBBuf ) : DecideCase;

  Begin
    If LHPtr^.DBSource Then Begin
      If Char ( DBBuf ) = ' ' Then Begin
        StdDecideWrite := DCWrite;
      End Else Begin
        StdDecideWrite := DCSkip;
      End;
    End Else Begin
      If Longint ( BTBuf ) = 0 Then Begin
        StdDecideWrite := DCWrite;
      End Else Begin
        StdDecideWrite := DCSkip;
      End;
    End;
  End;


  Procedure NoCArrConv ( CArrPtr : Pointer;
                         Len     : Word );

  Begin
  End;


  Function NoReXUser (     Status    : WorkStatus;
                           LHPtr     : PListHeader;
                           ReadRecs,
                           WriteRecs,
                           ErrorRecs : Longint;
                       Var DatSBuf ) : Integer;

  Begin
  End;


  Function DBaseErrorMessage ( ErrorNr : DBaseErrorNr ) : String;

  Begin
    If (DEEOF < ErrorNr) And (ErrorNr < DELAST) Then Begin
      Case DBUseErrorMessage Of
        DBNoMsg : Begin
          DBaseErrorMessage := '';
        End;

        DBGerman : Begin
        {$IFDEF DBaseGermanMessage}
          Case ErrorNr Of
            DEEOF   : DBaseErrorMessage := 'Dateiende erreicht';
            DEOOM   : DBaseErrorMessage := 'Nicht gengend Speicher frei';
            DEBV    : DBaseErrorMessage := 'Falsche Version oder es ist '
                + 'keine dBASE III/VI Datei';
            DECMF   : DBaseErrorMessage := 'Format der Memo-Datei stimmt nicht '
                + 'oder die Datei ist korrumpiert';
            DERSTL  : DBaseErrorMessage := 'Datensatz ist zu groá';
            DEWCT   : DBaseErrorMessage := 'Falscher CType';
            DEEWTD  : DBaseErrorMessage := 'Fehler beim Schreiben der '
                + 'Typdefinitionsdatei';
            DEECF  : DBaseErrorMessage := 'Feld konnte nicht konvertiert '
                + 'werden';
            DELHNI  : DBaseErrorMessage := 'ListHeader ist nicht '
                + 'oder falsch initialisiert';
            DETMF   : DBaseErrorMessage := 'Zu viele Felder';
            DEWFT   : DBaseErrorMessage := 'Ungltiger Feldtyp';
            DEFWTL  : DBaseErrorMessage := 'Feldl„nge zu groá';
            DETMD  : DBaseErrorMessage := 'Zuviele Dezimalstellen';
            DEFTVC  : DBaseErrorMessage := 'Feldtyp paát nicht zur dBASE-'
                + 'Version';
            DEARFNA : DBaseErrorMessage := 'Autorelationen sind an dieser '
                + 'Stelle nicht erlaubt';
            DEFCNMF : DBaseErrorMessage := 'Datei enth„lt keine Memofelder';
            DEEODF  : DBaseErrorMessage := 'Fehler beim ™ffnen der Dumpdatei';
            DEEWDF  : DBaseErrorMessage := 'Fehler beim Schreiben der '
                + 'Dumpdatei';
            DEECDF  : DBaseErrorMessage := 'Fehler beim Schlieáen der '
                + 'Dumpdatei';
            DEPE    : DBaseErrorMessage := 'Programmierfehler';
            DEFNAE  : DBaseErrorMessage := 'Feldname existiert schon';
            DENFD   : DBaseErrorMessage := 'Kein Feld definiert';
          End;
        {$ENDIF}
        End;

        DBEnglish : Begin
        {$IFDEF DBaseEnglishMessage}
          Case ErrorNr Of
            DEEOF   : DBaseErrorMessage := 'End of file';
            DEOOM   : DBaseErrorMessage := 'Out of memory';
            DEBV    : DBaseErrorMessage := 'Bad version or is not an '
                + 'dBASE III/IV file';
            DECMF   : DBaseErrorMessage := 'Corrupted memo file';
            DERSTL  : DBaseErrorMessage := 'Record size to large';
            DEWCT   : DBaseErrorMessage := 'Wrong CType';
            DEEWTD  : DBaseErrorMessage := 'Error writing type definition'
                + ' file';
            DEECF   : DBaseErrorMessage := 'Error converting field';
            DELHNI  : DBaseErrorMessage := 'List header not initialized '
                + 'or bad part';
            DETMF   : DBaseErrorMessage := 'Too many fields';
            DEWFT   : DBaseErrorMessage := 'Wrong field type';
            DEFWTL  : DBaseErrorMessage := 'Field width too large';
            DETMD   : DBaseErrorMessage := 'Too many decimals';
            DEFTVC  : DBaseErrorMessage := 'Field type version conflict';
            DEARFNA : DBaseErrorMessage := 'Auto relation field is not '
                + 'allowed here';
            DEFCNMF : DBaseErrorMessage := 'File contains no memo fields';
            DEEODF  : DBaseErrorMessage := 'Error opening dump file';
            DEEWDF  : DBaseErrorMessage := 'Error writing dump file';
            DEECDF  : DBaseErrorMessage := 'Error closing dump file';
            DEPE    : DBaseErrorMessage := 'Programming error';
            DEFNAE  : DBaseErrorMessage := 'Field name already exists';
            DENFD   : DBaseErrorMessage := 'No field defined';
          End;
        {$ENDIF}
        End;
      End;
    END ELSE BEGIN
      DBaseErrorMessage := '';
    End;
  End;


  Function DBaseEOF ( LHPtr : PListHeader ) : Boolean;

  Begin
    DBaseEOF := LHPtr^.DBHeaderPtr^.RefNr >
        LHPtr^.DBHeaderPtr^.Header.NrOfRecs;
  End;


  Function DBaseGo ( LHPtr : PListHeader;
                     Ref   : Longint ) : Integer;

  Var
    Position : Longint;

  Begin
    IsamClearOK;                                               {!!.52}
    If (Ref <= 0 ) And ( LHPtr^.DBHeaderPtr^.Header.NrOfRecs < Ref )
        Then Begin
      CallProcErrorHandler ( ERIgnore, 0, DEEOF );
    End;
    LHPtr^.DBHeaderPtr^.RefNr := Ref;
    Position := LHPtr^.DBHeaderPtr^.Header.HeaderSize +
        ( LHPtr^.DBHeaderPtr^.RefNr - 1 ) *
        Longint ( LHPtr^.DBHeaderPtr^.Header.RecordSize );
    IsamLongSeek ( LHPtr^.DBHeaderPtr^.IFile, Position );
    If Not IsamOK  Then Begin
      DBaseGo := BTIsamErrorClass;
      CallProcErrorHandler ( ERAbort, IsamError, DEZERO );
      Exit;
    End;
    DBaseGo := 0;
  End;


  Function DBaseSkip ( LHPtr : PListHeader ) : Integer;

  Begin
    DBaseSkip := DBaseGo ( LHPtr, LHPtr^.DBHeaderPtr^.RefNr + 1 );
  End;


  Procedure UndoDBaseImpExp ( LHPtr        : PListHeader;
                              IsamBufPtr ,
                              DBaseBufPtr  : Pointer;
                              IsamBufSize,
                              DBaseBufSize : Word );

  Var
    Dummy : Integer;

  Begin
    If LHPtr^.DBSource Then Begin
      Dummy := CloseIsamFiles ( LHPtr )
    End Else Begin
      Dummy := CloseDBaseFiles ( LHPtr );
    End;

    If IsamBufPtr <> Nil Then FreeMem ( IsamBufPtr, IsamBufSize );
    If DBaseBufPtr <> Nil Then FreeMem ( DBaseBufPtr, DBaseBufSize );
  End;


  Function OpenDumpFile ( Var DumpFilePtr : PText;
                              DumpFName   : IsamFileName ) : Integer;

  Begin
    OpenDumpFile := -1;

    If MaxAvail < SizeOf ( Text ) Then Begin
      CallProcErrorHandler ( ERIgnore, 0, DEEODF );
      Exit;
    End;
    GetMem ( DumpFilePtr, SizeOf ( Text ) );

    Assign ( DumpFilePtr^, DumpFName );
    Rewrite ( DumpFilePtr^ );
    If IOResult <> 0 Then Begin
      FreeMem ( DumpFilePtr, SizeOf ( Text ) );
      DumpFilePtr := Nil;
      CallProcErrorHandler ( ERIgnore, 0, DEEODF );
      Exit;
    End;

    OpenDumpFile := 0;
  End;


  Function CloseDumpFile ( Var DumpFilePtr : PText ) : Integer;

  Var                                                           {!!.42}
    LResult : Word;                                             {!!.42}{!!.51}

  Begin
    CloseDumpFile := 0;
    If DumpFilePtr = Nil Then Exit;

    Close ( DumpFilePtr^ );
    FreeMem ( DumpFilePtr, SizeOf ( Text ) );
    DumpFilePtr := Nil;
    LResult := IOResult;                                        {!!.42}{!!.51}
    If LResult <> 0 Then Begin                                  {!!.42}{!!.51}
      CloseDumpFile := LResult;                                 {!!.42}{!!.51}
      CallProcErrorHandler ( ERIgnore, 0, DEECDF );
    End;
  End;


  Function WriteDump ( Var DumpFilePtr : PText;
                           LHPtr       : PListHeader;
                           Errors      : Integer ) : Integer;

  Var
    LResult: Integer;                                           {!!.51}
    CurFNPtr  : PFieldNode;
    i,
    Width     : Integer;
    DumpFName : IsamFileName;

  Begin
    If LHPtr = Nil Then Exit;

    If DumpFilePtr = Nil Then Begin
      If LHPtr^.DBSource Then Begin
        DumpFName := LHPtr^.DBHeaderPtr^.IFile.Name;
      End Else Begin
        DumpFName := BTDataFileName ( LHPtr^.BTHeaderPtr^.FBPtr );
      End;
      DumpFName := IsamForceExtension ( DumpFName, DumpExtension ); {!!.42mod}

      LResult := OpenDumpFile ( DumpFilePtr, DumpFName );       {!!.51}
      If LResult <> 0 Then Begin                                {!!.51}
        WriteDump := LResult;                                   {!!.51}
        Exit;
      End;
    End;

    Writeln ( DumpFilePtr^,
        'Record number: ', LHPtr^.DBHeaderPtr^.RefNr : 8,
        ' Number of errors: ', Errors );
    LResult := IOResult;                                        {!!.42}{!!.51}
    If LResult <> 0 Then Begin                                  {!!.42}{!!.51}
      WriteDump := LResult;                                     {!!.42}{!!.51}
      CallProcErrorHandler ( ERIgnore, 0, DEEWDF );
      LResult := CloseDumpFile ( DumpFilePtr );                 {!!.51}
      Exit;
    End;

    CurFNPtr := LHPtr^.ListPtr;
    While CurFNPtr <> Nil Do Begin
      If CurFNPtr^.ConvStatus <> 0 Then Begin
        Writeln ( DumpFilePtr^, 'Field name: ', CurFNPtr^.FieldName );
        LResult := IOResult;                                    {!!.42}{!!.51}
        If LResult <> 0 Then Begin                              {!!.42}{!!.51}
          WriteDump := LResult;                                 {!!.42}{!!.51}
          CallProcErrorHandler ( ERIgnore, 0, DEEWDF );
          LResult := CloseDumpFile ( DumpFilePtr );             {!!.51}
          Exit;
        End;
      End;
      CurFNPtr := CurFNPtr^.NextPtr;
    End;

    WriteDump := 0;
  End;


  Procedure FreeFieldList ( Var ListPtr : PFieldNode );

  Var
    CurFNPtr : PFieldNode;

  Begin
    While ListPtr <> Nil Do Begin
      CurFNPtr := ListPtr;
      ListPtr := ListPtr^.NextPtr;
      If CurFNPtr^.DBFieldPtr <> Nil Then
          FreeMem ( CurFNPtr^.DBFieldPtr, SizeOf ( DBaseField ) );
      If CurFNPtr^.BTFieldPtr <> Nil Then
          FreeMem ( CurFNPtr^.BTFieldPtr, SizeOf ( IsamField ) );
      FreeMem ( CurFNPtr, SizeOf ( FieldNode ) );
    End;
  End;


  Procedure FreeListHeader ( Var LHPtr : PListHeader );

  Begin
    If LHPtr = Nil Then Exit;

    If LHPtr^.DBHeaderPtr <> Nil Then Begin
      If LHPtr^.DBHeaderPtr^.MemoPtr <> Nil Then Begin
        FreeMem ( LHPtr^.DBHeaderPtr^.MemoPtr, SizeOf ( DBaseMemo ) );
      End;
      FreeMem ( LHPtr^.DBHeaderPtr, SizeOf ( DBaseHeader ) );
    End;

    If LHPtr^.BTHeaderPtr <> Nil Then Begin
      FreeMem ( LHPtr^.BTHeaderPtr, SizeOf ( IsamHeader ) );
    End;

    If LHPtr^.ListPtr <> Nil Then Begin
      FreeFieldList ( LHPtr^.ListPtr );
    End;

    FreeMem ( LHPtr, SizeOf ( ListHeader ) );
    LHPtr := Nil;
  End;


  Procedure SetDateOfToDay ( Var LastChange : DBaseFileDate );

  Var
    Year,
    Month,
    Day,
    DayOfWeek : Word;

  Begin
    GetDate ( Year, Month, Day, DayOfWeek );
    If ( Year > 100) And ( Year < 2000 ) Then
      Dec ( Year, 1900 );
    LastChange.Year := Byte ( Year );
    LastChange.Month := Byte ( Month );
    LastChange.Day := Byte ( Day );
  End;


  Function CloseDBaseFiles ( LHPtr : PListHeader ) : Integer;

  Begin
    CloseDBaseFiles := -1;
    IsamClearOK;

    If LHPtr = Nil Then Exit;
    If LHPtr^.DBHeaderPtr = Nil Then Exit;
    If LHPtr^.DBHeaderPtr^.Modified Then Begin
      SetDateOfToDay ( LHPtr^.DBHeaderPtr^.Header.LastChange );
      IsamLongSeek ( LHPtr^.DBHeaderPtr^.IFile, 0 );
      If Not IsamOK Then Begin
        CloseDBaseFiles := BTIsamErrorClass;
        CallProcErrorHandler ( ERAbort, IsamError, DEZERO );
        Exit;
      End;
      IsamBlockWrite ( LHPtr^.DBHeaderPtr^.IFile,
          LHPtr^.DBHeaderPtr^.Header, SizeOf ( DBaseFileHeader ) );
      If Not IsamOK Then Begin
        CloseDBaseFiles := BTIsamErrorClass;
        CallProcErrorHandler ( ERAbort, IsamError, DEZERO );
        Exit;
      End;
      LHPtr^.DBHeaderPtr^.Modified := False;
    End;
    If LHPtr^.DBHeaderPtr^.IFile.Handle <> 65535 Then Begin
      IsamClose (LHPtr^.DBHeaderPtr^.IFile);
      If Not IsamOK Then Begin
        CloseDBaseFiles := BTIsamErrorClass;
        CallProcErrorHandler ( ERAbort, IsamError, DEZERO );
        Exit;
      End;
    End;

    If LHPtr^.DBHeaderPtr^.MemoPtr = Nil Then Begin
      CloseDBaseFiles := 0;
      Exit;
    End;
    If LHPtr^.DBHeaderPtr^.MemoPtr^.Modified Then Begin
      IsamLongSeek ( LHPtr^.DBHeaderPtr^.MemoPtr^.IFile, 0 );
      If Not IsamOK Then Begin
        CloseDBaseFiles := BTIsamErrorClass;
        CallProcErrorHandler ( ERAbort, IsamError, DEZERO );
        Exit;
      End;
      IsamBlockWrite ( LHPtr^.DBHeaderPtr^.MemoPtr^.IFile,
          LHPtr^.DBHeaderPtr^.MemoPtr^.Header.NextFree,
          SizeOf ( LHPtr^.DBHeaderPtr^.MemoPtr^.Header.NextFree ) );
      If Not IsamOK Then Begin
        CloseDBaseFiles := BTIsamErrorClass;
        CallProcErrorHandler ( ERAbort, IsamError, DEZERO );
        Exit;
      End;
      LHPtr^.DBHeaderPtr^.MemoPtr^.Modified := False;
    End;
    If LHPtr^.DBHeaderPtr^.MemoPtr^.IFile.Handle <> 65535 Then Begin
      IsamClose ( LHPtr^.DBHeaderPtr^.MemoPtr^.IFile );
      If Not IsamOK Then Begin
        CloseDBaseFiles := BTIsamErrorClass;
        CallProcErrorHandler ( ERAbort, IsamError, DEZERO );
        Exit;
      End;
    End;

    CloseDBaseFiles := 0;
  End;


  Function CloseIsamFiles ( LHPtr : PListHeader ) : Integer;

  Begin
    CloseIsamFiles := -1;
    IsamClearOK;

    If LHPtr = Nil Then Exit;
    If LHPtr^.BTHeaderPtr = Nil Then Exit;
    If LHPtr^.BTHeaderPtr^.FBPtr = Nil Then Exit;

    BTCloseFileBlock ( LHPtr^.BTHeaderPtr^.FBPtr );
    If Not IsamOK Then Begin
      CloseIsamFiles := BTIsamErrorClass;
      CallProcErrorHandler ( ERAbort, IsamError, DEZERO );
      Exit;
    End;
    CloseIsamFiles := 0;
  End;


  Function CheckListHeaderPtr ( LHPtr    : PListHeader;
                                DBSource : Boolean ) : Boolean;

  Begin
    CheckListHeaderPtr := False;

    If LHPtr = Nil Then Exit;
    If LHPtr^.ListPtr = Nil Then Exit;
    If LHPtr^.BTHeaderPtr = Nil Then Exit;
    If LHPtr^.DBHeaderPtr = Nil Then Exit;
    If LHPtr^.DBSource <> DBSource Then Exit;

    CheckListHeaderPtr := True;
  End;


{$I DBImpExp.In1}
{$I DBImpExp.In2}


Begin
  ProcErrorHandler := NoErrorHandler;                          {!!.42}
End.
