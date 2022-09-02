{$I fsdefine.inc}

Unit ubase;

Interface

Uses
  Windows,
  Classes,
  Controls,
  ExtCtrls,
  Stdctrls,
  fsdb,
  fsserverremoteclass,
  fsllbase,
  fslldict,
  uentity,
  uconfig,
  fsfunInterp;

Type
  TMenuAction = (maServerAttach,
    maServerDetach,
    maDatabaseOpen,
    maDatabaseClose);

Var
  ClosingApp: Boolean;
  ServerList: TffeServerList;
  fFieldTypes: Array[TfsFieldType] Of String[20];

Function FFEBlockSizeIndex(Const aBlockSize: Longint): Integer;

Function FFEBoolToStr(B: Boolean): TffShStr;

Procedure FFEEnableContainer(Container: TWinControl; Switch: Boolean);

Function FFEFieldAllowedDefault(aFieldType: TfsFieldType): Boolean;
{ Returns true if the field type is allowed to have a default value.
  AutoInc, ByteArrays, and Boolean fields are not allowed to have a
  default value}

Function FFEFieldTypeHasDecPl(aFieldType: TfsFieldType): Boolean;
{ Returns true if the given field type has a "decimal places" factor
  associated with it.  For example, currency and float fields. }
Function FFEFieldTypeHasRound(aFieldType: TfsFieldType): Boolean;
Function FFEFieldTypeHasBlob(aFieldType: TfsFieldType): Boolean;
Function FFEFieldTypeHasUnits(aFieldType: TfsFieldType): Boolean;
{ Returns true if the given field type has a "number of units" factor
  associated with it.  For example, string and character fields. }

Function FFEFieldTypeRequiresUnits(aFieldType: TfsFieldType): Boolean;
{ Returns true if the given field type requires a units factor. }

Function FFEFieldTypeToIndex(aFieldType: TfsFieldType): Integer;
{ Converts a given FF fieldtype value to an integer index, skipping
  the reserved positions }
//Function FFEFieldBlobToIndex(aFieldBlob: TDataCompLevel): Integer;
Function FFEIndexToFieldType(aIndex: String): TfsFieldType;
//Function FFEIndexToFieldBlob(aIndex: Integer): TDataCompLevel;
Function FFEStrToFieldBlob(aIndex: String): TDataCompLevel;
{ Converts an integer index to a FF field type, skipping the
  reserved positions }
Function FFEStrToFieldRound(aIndex: String): TRound;
Function FFEStrToFieldDefault(aIndex: String): TDefaultUpdate;
Function FFEVersionStr: TffShStr;

Implementation

Uses
  fsnetmsg,
  fsllprot,
  Db,
  uconsts,
  SysUtils,
  TypInfo;

Var
  FFEFirstReservedFieldType,
    FFELastReservedFieldType: TfsFieldType;
  {--------}

Function FFEBlockSizeIndex(Const aBlockSize: Longint): Integer;
Begin
  Case aBlockSize Of
    4 * 1024: Result := 0;
    8 * 1024: Result := 1;
    16 * 1024: Result := 2;
    32 * 1024: Result := 3;
    64 * 1024: Result := 4;
    Else
      Result := -1;
  End;
End;
{--------}

Function FFEBoolToStr(B: Boolean): TffShStr;
Begin
  If B Then
    Result := 'Y'
  Else
    Result := 'N';
End;
{--------}

Procedure FFEEnableContainer(Container: TWinControl; Switch: Boolean);
Var
  I: Integer;
Begin
  With Container Do
    Begin
      Enabled := Switch;
      For I := 0 To ControlCount - 1 Do
        Begin
          Controls[I].Enabled := Switch;
          If (Controls[I] Is TGroupBox) Or (Controls[I] Is TPanel) Then
            FFEEnableContainer(Controls[I] As TWinControl, Switch);
        End;
    End;
End;
{--------}

Function FFEFieldAllowedDefault(aFieldType: TfsFieldType): Boolean;
Begin
  Result := aFieldType In [fstBoolean,
    fstSingleChar,
    fstSingleWideChar,
    fstUInt8,
    fstInt8,
    fstInt16,
    fstInt32,
    fstUInt16,
    fstUInt32,
    fstInt64,
    fstSingle,
    fstDouble,
    fstExtended,
    fstCurrency,
    fstBcd,
    fstDate,
    fstTime,
    fstDateTime,
    fstArrayUInt8,
    fstArrayUInt16,
    fstArrayInt32,
    fstArrayDouble,
    fstShortString,
    fstNullString,
    fstVarNullString,
    fstWideString,
    fstWideString];
End;
{--------}

Function FFEFieldTypeHasDecPl(aFieldType: TfsFieldType): Boolean;
Begin
  Result := aFieldType In [fstSingle,
    fstDouble,
    fstExtended,
    fstBcd,
    fstRecVersion,
    fstArrayDouble,
    fstCurrency];
End;

Function FFEFieldTypeHasRound(aFieldType: TfsFieldType): Boolean;
Begin
  Result := aFieldType In [fstSingle,
    fstDouble,
    fstExtended,
    fstBcd,
    fstArrayDouble,
    fstCurrency];
End;

Function FFEFieldTypeHasBlob(aFieldType: TfsFieldType): Boolean;
Begin
  Result := aFieldType In [fstblob..fstBLOBGraphic];
End;
{--------}

Function FFEFieldTypeHasUnits(aFieldType: TfsFieldType): Boolean;
Begin
  Result := aFieldType In [fstUInt8,
    fstUInt16,
    fstUInt32,
    fstInt8,
    fstInt16,
    fstInt32,
    fstSingle,
    fstDouble,
    fstExtended,
    fstInt64,
    fstCurrency,
    fstBcd,
    fstRecVersion,
    fstArrayUInt8,
    fstArrayUInt16,
    fstArrayInt32, fstArrayDouble,
    fstShortString..High(TfsFieldType)];
End;
{--------}

Function FFEFieldTypeRequiresUnits(aFieldType: TfsFieldType): Boolean;
Begin
  Result := aFieldType In [fstRecVersion, fstArrayUInt8, fstArrayUInt16, fstArrayInt32, fstArrayDouble,
    fstShortString..High(TfsFieldType)];
End;
{--------}

Function FFEFieldTypeToIndex(aFieldType: TfsFieldType): Integer;
Begin
  If aFieldType < FFEFirstReservedFieldType Then
    Result := Ord(aFieldType)
  Else If aFieldType > FFELastReservedFieldType Then
    Result := Ord(aFieldType) -
      (Ord(FFELastReservedFieldType) -
      Ord(FFEFirstReservedFieldType) + 1)
  Else
    Result := -1;
End;

Function FFEFieldBlobToIndex(aFieldBlob: TDataCompLevel): Integer;
Begin
  Result := Ord(aFieldBlob);
End;
{--------}

Function FFEIndexToFieldType(aIndex: String): TfsFieldType;
Var
  s: String;
Begin
  s := UpperCase(aIndex);
  If UpperCase('Boolean') = S Then Result := fstBoolean;
  If UpperCase('SingleChar') = S Then Result := fstSingleChar;
  If UpperCase('SingleWideChar') = S Then Result := fstSingleWideChar;
  If UpperCase('UInt8') = S Then Result := fstUInt8;
  If UpperCase('UInt16') = S Then Result := fstUInt16;
  If UpperCase('UInt32') = S Then Result := fstUInt32;
  If UpperCase('Int8') = S Then Result := fstInt8;
  If UpperCase('Int16') = S Then Result := fstInt16;
  If UpperCase('Int32') = S Then Result := fstInt32;
  If UpperCase('Int64') = S Then Result := fstInt64;
  If UpperCase('AutoInc32') = S Then Result := fstAutoInc32;
  If UpperCase('AutoInc64') = S Then Result := fstAutoInc64;
  If UpperCase('Single') = S Then Result := fstSingle;
  If UpperCase('Double') = S Then Result := fstDouble;
  If UpperCase('Extended') = S Then Result := fstExtended;
  If UpperCase('Currency') = S Then Result := fstCurrency;
  If UpperCase('Date') = S Then Result := fstDate;
  If UpperCase('Time') = S Then Result := fstTime;
  If UpperCase('DateTime') = S Then Result := fstDateTime;
  If UpperCase('Blob') = S Then Result := fstBlob;
  If UpperCase('BlobMemo') = S Then Result := fstBlobMemo;
  If UpperCase('BlobGraphic') = S Then Result := fstBlobGraphic;
  If UpperCase('Clob') = S Then Result := fstClob;
  If UpperCase('WideClob') = S Then Result := fstWideClob;
  If UpperCase('BlobFile') = S Then Result := fstBlobFile;
  If UpperCase('BCD') = S Then Result := fstBCD;
  If UpperCase('ArrayUInt16') = S Then Result := fstArrayUInt16;
  If UpperCase('ArrayInt32') = S Then Result := fstArrayInt32;
  If UpperCase('ArrayDouble') = S Then Result := fstArrayDouble;
  If UpperCase('RecVersion') = S Then Result := fstRecVersion;
  If UpperCase('ArrayUInt8') = S Then Result := fstArrayUInt8;
  If UpperCase('ShortString') = S Then Result := fstShortString;
  If UpperCase('VarNullString') = S Then Result := fstVarNullString;
  If UpperCase('NullString') = S Then Result := fstNullString;
  If UpperCase('VarWideString') = S Then Result := fstVarWideString;
  If UpperCase('WideString') = S Then Result := fstWideString;
End;

Function FFEStrToFieldBlob(aIndex: String): TDataCompLevel;
Begin //blNone, blFastest, blDefault, blMax
  If AnsiUpperCase(Trim(aIndex)) = 'BLNONE' Then
    Result := blNone
  Else If AnsiUpperCase(Trim(aIndex)) = 'BLFASTEST' Then
    Result := blFastest
  Else If AnsiUpperCase(Trim(aIndex)) = 'BLDEFAULT' Then
    Result := blDefault
  Else If AnsiUpperCase(Trim(aIndex)) = 'BLMAX' Then
    Result := blMax;
End;

Function FFEStrToFieldDefault(aIndex: String): TDefaultUpdate;
Begin
  If AnsiUpperCase(Trim(aIndex)) = 'DUNORMAL' Then
    Result := duNormal
  Else If AnsiUpperCase(Trim(aIndex)) = 'DUIFNULL' Then
    Result := duIFNULL
  Else If AnsiUpperCase(Trim(aIndex)) = 'DUALWAYS' Then
    Result := duALWAYS;
End;

Function FFEStrToFieldRound(aIndex: String): TRound;
Begin
  If AnsiUpperCase(Trim(aIndex)) = 'NONE' Then
    Result := rNone
  Else If AnsiUpperCase(Trim(aIndex)) = 'MATHEMATICAL' Then
    Result := rMATHEMATICAL
  Else If AnsiUpperCase(Trim(aIndex)) = 'MATAFTER1' Then
    Result := rMATAfter1
  Else If AnsiUpperCase(Trim(aIndex)) = 'MATAFTER2' Then
    Result := rMATAfter2
  Else If AnsiUpperCase(Trim(aIndex)) = 'MATAFTER3' Then
    Result := rMATAfter3
  Else If AnsiUpperCase(Trim(aIndex)) = 'MATAFTER4' Then
    Result := rMATAfter4
  Else If AnsiUpperCase(Trim(aIndex)) = 'MATAFTER5' Then
    Result := rMATAfter5
  Else If AnsiUpperCase(Trim(aIndex)) = 'MATAFTER6' Then
    Result := rMATAfter6
  Else If AnsiUpperCase(Trim(aIndex)) = 'MATAFTER7' Then
    Result := rMATAfter7
  Else If AnsiUpperCase(Trim(aIndex)) = 'MATAFTER8' Then
    Result := rMATAfter8
  Else If AnsiUpperCase(Trim(aIndex)) = 'MATAFTER9' Then
    Result := rMATAfter9;
End;

Function FFEIndexToFieldBlob(aIndex: Integer): TDataCompLevel;
Begin
  Result := TDataCompLevel(Ord(aIndex));
End;
{--------}

Procedure PopulateFieldTypes;
Var
  I: TfsFieldType;
Begin
  FFEFirstReservedFieldType := fstClob; //fstInterval;
  FFELastReservedFieldType := fstBcd; //fstRecVersion;
  For I := Low(I) To High(I) Do
    fFieldTypes[I] := GetEnumName(TypeInfo(TfsFieldType), Ord(I));
End;
{--------}

Function FFEVersionStr: TffShStr;
Begin
  Result := floattostrf(FSVersionNumber / 1000, fffixed, 5, 3);
End;
{--------}

Initialization
  ClosingApp := False;
  PopulateFieldTypes;
End.

