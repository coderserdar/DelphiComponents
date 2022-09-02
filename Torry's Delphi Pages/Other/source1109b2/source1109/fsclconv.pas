{$I fsdefine.inc}

Unit fsclconv;

Interface

Uses
  Windows,
  SysUtils,
  Db,
  fsclbde,
  fssrbde,
  fsllbase,
  fsllexcp,
  fsconst,
  fsclbase;

Procedure GetBDEFieldDescriptor(Const FFFieldDesc: TFFFieldDescriptor;
  Var BDEFldDesc: FLDDesc);
{-converts a FlashFiler field descriptor into a physical BDE one}

Procedure GetBDEIndexDescriptor(Const FFIndexDesc: TFFIndexDescriptor;
  Var BDEIdxDesc: IDXDesc);
{-converts a FlashFiler index descriptor into a BDE one}

Procedure GetBDELogicalFieldDescriptor(Const FFFieldDesc: FLDDesc;
  Var BDEFieldDesc: FLDDesc);
{-converts a FlashFiler based BDE field description to a logical one
  NOTE: the field iOffset is not set - to be calculated later}

Procedure GetBDEVChkDescriptor(FFVChkDesc: TffVCheckDescriptor;
  Var BDEVChkDesc: VCHKDesc;
  FieldNumber: Longint;
  Required: boolean);
{-converts a FlashFiler validity check descriptor into a BDE one}

Function GetFFSearchKeyAction(
  Const aDBISearchCond: DBISearchCond): TffSearchKeyAction;
{-convert a BDE search action to the FF one}

Function MapDataToFS(FFType: TfsFieldType;
  PhySize: Integer;
  aSource: pointer;
  aDest: pointer): boolean;
{-maps a BDE logical field value in aSource to the equivalent FF
  value in aDest. Note that type conversion is assumed to be the
  reverse of MapFFTypeToBDE}

Function MapFFDataToBDE(FFType: TfsFieldType;
  PhySize: Integer;
  aSource: pointer;
  aDest: pointer): boolean;
{-maps a FlashFiler field value in aSource to the equivalent BDE
  value in aDest. Note that type conversion is assumed to be the
  same as MapFFTypeToBDE}

Procedure MapFFTypeToBDE(FFType: TfsFieldType;
  PhySize: Integer;
  Var BDEType: Word;
  Var LogSize: Longint);
{-maps a FlashFiler field type to the nearest BDE logical type/
  subtype and returns the logical size}

Procedure MapVCLTypeToFF(Const VCLType: TFieldType;
  Const VCLSize: Integer;
  Var FFType: TfsFieldType;
  Var FFSize: Longint);
{-maps a VCL field type to the nearest FF equivalent.  If the specified
  VCLType is not supported, FFType is set to fftInterval. }

Function FFBDEDateEncode(aDay: Word;
  aMonth: Word;
  aYear: Word): DBIDATE;
{-converts day, month, year to a raw date for a field}

Procedure FFBDEDateDecode(aDate: DBIDATE;
  Var aDay: Word;
  Var aMonth: Word;
  Var aYear: Word);
{-converts a raw date from a field to day, month, year}

Function FFBDETimeEncode(aHour: Word;
  aMin: Word;
  aMilSec: Word): DBITIME;
{-converts hour, min, milsec to a raw time for a field}

Procedure FFBDETimeDecode(aTime: DBITIME;
  Var aHour: Word;
  Var aMin: Word;
  Var aMilSec: Word);
{-converts a raw time from a field to hour, min, milsec}

Implementation

Uses
  fsstdate;

{Notes on date and time formats: There are four different date formats
       in play here in FlashFiler Client:
       TffDBIDate : a longint being the number of days since 1/1/0100;
                    this is the BDE logical type
       TDateTime  : a double whose integral part is
                    (Delphi 1) the number of days since 1/1/0100
                    (Delphi 2+) the number of days since 1/1/1800
       TStDate    : a longint being the number of days since 1/1/1600.
The big problem is the different definitions of TDateTime. In Delphi
1, to convert from TffDBIDate to TDateTime is an assignment (they use
the same base date; in later compilers, you have to add 693594 days
(and vice versa for the inverse operation). To convert TStDates to
TDateTimes, use the StDate routine StDateToDateTime and
DateTimeToStDate.

Times are less confusing. The BDE logical type (TffDbiTime) is the
number of milliseconds since midnight in a longint and the conversion
from TStTime (the seconds since midnight) is simple.

Unions of dates and times are also relatively simple. The BDE logical
type TffTimeStamp is a number of milliseconds since the standard BDE
base date.}

Const
  IGNORE_OEMANSI: Boolean = True;
  IGNORE_ANSIOEM: Boolean = True;
  dsfsMaxStringSize = 32767;

  {===Interfaced routines==============================================}

Procedure GetBDEFieldDescriptor(Const FFFieldDesc: TFFFieldDescriptor;
  Var BDEFldDesc: FLDDesc);
Begin
  {clear the result structure to binary zeros}
  FillChar(BDEFldDesc, sizeof(FLDDesc), 0);
  {fill the relevant parts of the result structure}
  With BDEFldDesc, FFFieldDesc Do
    Begin
      iFldNum := succ(fdNumber);
      FFStrPCopy(szName, fdName);
      iFldType := ord(fdType);
      iSubType := 0;
      iUnits1 := fdUnits;
      iUnits2 := fdDecPl;
      iOffset := fdOffset;
      iLen := fdLength;
      {iNullOffset := 0;}
      If Assigned(fdVCheck) Or fdRequired Then
        efldvVchk := fldvHASCHECKS
      Else
        efldvVchk := fldvNOCHECKS;
      efldrRights := fldrREADWRITE;
      {bCalcField := False;}
    End;
End;
{--------}

Procedure GetBDEIndexDescriptor(Const FFIndexDesc: TFFIndexDescriptor;
  Var BDEIdxDesc: IDXDesc);
Var
  i: Integer;
Begin
  {clear the result structure to binary zeros}
  FillChar(BDEIdxDesc, sizeof(IDXDesc), 0);
  {fill the relevant parts of the result structure}
  With FFIndexDesc, BDEidxDesc Do
    Begin
      StrPLCopy(szName, idName, sizeof(szName) - 1);
      iIndexId := idNumber;
      {StrCopy(szTagName, '');}
      StrCopy(szFormat, 'BTREE');
      {bPrimary := false;}
      bUnique := Not idDups;

      bMaintained := True; {all FF keys are maintained}
      {bSubSet := false;}
      {iCost := 0}
      If (idCount = -1) Then
        Begin
          {this is a User-defined or Seq.Access Index: we'll treat it as
           an expression Index - see dBASE info...}
          bExpIdx := True;
          iFldsInKey := 0;
        End
      Else {it's a composite index}
        Begin
          bExpIdx := False;
          iFldsInKey := idCount;
          For i := 0 To pred(iFldsInKey) Do
            aiKeyFld[i] := succ(idFields[i]); {FF fields are 0-based, BDE's are 1-based}
        End;
      iKeyLen := idKeyLen;
      If idCount <> -1 Then
        Begin
          bCaseInsensitive := boolean(FFIndexDesc.idFieldsCase[0]);
          bDescending := Not boolean(FFIndexDesc.idFieldsAscDesc[0]);
        End
      Else
        Begin
          bCaseInsensitive := True;
          bDescending := False;
        End;
    End;
End;
{--------}

Procedure GetBDELogicalFieldDescriptor(Const FFFieldDesc: FLDDesc;
  Var BDEFieldDesc: FLDDesc);
Begin
  {clear the result structure to binary zeros}
  FillChar(BDEFieldDesc, sizeof(BDEFieldDesc), 0);
  {fill the relevant parts of the result structure}
  With BDEFieldDesc Do
    Begin
      iFldNum := FFFieldDesc.iFldNum;
      StrCopy(szName, FFFieldDesc.szName);
      MapFFTypeToBDE(TfsFieldType(FFFieldDesc.iFldType),
        FFFieldDesc.iLen,
        iFldType,
        iLen);
      If (iFldType In [fldIntArray, fldDoubleArray, fldByteArray..fldUnicode]) Then
        iUnits1 := iLen;
      {iUnits2 := 0; - unused}
      {iOffset := 0; - this is set later}
      {iNullOffset := 0; - there is none in a converted desc}
      efldvVchk := fldvNOCHECKS;
      efldrRights := fldrREADWRITE;
      {bCalcField := 0;}
    End;
End;
{--------}

Procedure GetBDEVChkDescriptor(FFVChkDesc: TffVCheckDescriptor;
  Var BDEVChkDesc: VCHKDesc;
  FieldNumber: Longint;
  Required: boolean);
Begin
  {clear the result structure to binary zeros}
  FillChar(BDEVchkDesc, sizeof(VCHKDesc), 0);
  {fill the relevant parts of the result structure}
  With BDEVChkDesc, FFVChkDesc Do
    Begin
      iFldNum := FieldNumber;
      bRequired := Required;
      bHasMinVal := vdHasMinVal;
      bHasMaxVal := vdHasMaxVal;
      bHasDefVal := vdHasDefVal;
      If vdHasMinVal Then
        Move(vdMinVal, aMinVal, 254);
      If vdHasMaxVal Then
        Move(vdMaxVal, aMaxVal, 254);
      If vdHasDefVal Then
        Move(vdDefVal, aDefVal, 254);
      StrPCopy(szPict, vdPicture);
      {elkupType := lkupNONE;}
      {szLkupTblName[0] := #0;}
    End;
End;
{--------}

Function GetFFSearchKeyAction(
  Const aDBISearchCond: DBISearchCond): TffSearchKeyAction;
Begin
  Case aDBISearchCond Of
    keySEARCHEQ: Result := skaEqual;
    keySEARCHGT: Result := skaGreater;
    keySEARCHGEQ: Result := skaGreaterEqual;
    Else
      Result := skaEqual;
  End;
End;
{--------}

Function MapDataToFS(FFType: TfsFieldType;
  PhySize: Integer;
  aSource: pointer;
  aDest: pointer): boolean;
Var
  WorkWideChar: Array[0..1] Of WideChar;
  DateValue: TStDate;
  Dt: TDatetime;
Begin
  // SetData
  Result := True;
  Case FFType Of
    fstBoolean:
      Begin
        Boolean(aDest^) := WordBool(aSource^);
      End;
    fstSingleChar:
      Begin
        Char(aDest^) := Char(aSource^); {copy one character}
      End;
    fstSingleWideChar:
      Begin
        If Not IGNORE_ANSIOEM Then
          AnsiToOEM(aSource, aSource);
        StringToWideChar(StrPas(aSource), WorkWideChar, PhySize);
        Move(WorkWideChar[0], aDest^, sizeof(WideChar));
      End;
    fstUInt8:
      Begin
        Byte(aDest^) := Word(aSource^);
      End;
    fstUInt16:
      Begin
        Word(aDest^) := Word(aSource^);
      End;
    fstUInt32:
      Begin
        Longword(aDest^) := Longword(aSource^);
      End;
    fstInt8:
      Begin
        Shortint(aDest^) := Shortint(aSource^); {!!.07}
      End;
    fstInt16:
      Begin
        Smallint(aDest^) := Smallint(aSource^);
      End;
    fstInt32:
      Begin
        Longint(aDest^) := Longint(aSource^);
      End;
    fstAutoInc32:
      Begin
        Longint(aDest^) := Longint(aSource^);
      End;
    fstSingle:
      Begin
        Single(aDest^) := Double(aSource^);
      End;
    fstDouble:
      Begin
        Double(aDest^) := Double(aSource^);
      End;
    fstExtended:
      Begin
        Extended(aDest^) := Extended(aSource^);
      End;
    fstInt64, fstAutoInc64, fstRecVersion:
      Begin
        Int64(aDest^) := Int64(aSource^);
      End;
    fstCurrency:
      Begin
        Int64(aDest^) := Round(Currency(aSource^) * 10000.0);
      End;
    fstBcd:
      Begin
        //TBcd(aDest^) := TBcd(aSource^);
      End;

    fstDate:
      Begin
        DateValue := DateTimeToStDate(
          DbiDate(aSource^) - 693594.0);
        If DateValue = BadDate Then
          Begin
            TStDate(aDest^) := 0;
            Result := False;
          End
        Else
          TStDate(aDest^) := DateValue;
      End;
    fstTime:
      Begin
        {StTimes are stored as # seconds since midnight; BDE logical
         times as # milliseconds; to convert, divide by 1000}
        TStTime(aDest^) := DBITime(aSource^) Div 1000;
      End;
    fstDateTime:
      Begin
        {FS datetimes are compatible with Delphi's TDateTime, viz:
         <days>.<timefraction>; BDE stores as # millisecs since
         base date; to convert, divide by # millisecs/day}
        dt := TimeStamp(aSource^) / 86400000.0;
        fsSetMillisecond(dt, 0);
        TDateTime(aDest^) := dt;
      End;
    fstBLOB,
      fstBLOBMemo,
      fstBLOBGraphic:
      Begin
        {just copy over the BLOB number}
        Longint(aDest^) := Longint(aSource^);
      End;
    fstArrayUInt8, fstArrayUInt16, fstArrayInt32, fstArrayDouble:
      Begin
        Move(aSource^, aDest^, PhySize);
      End;
    fstShortString:
      Begin
        If Not IGNORE_ANSIOEM Then
          AnsiToOEM(aSource, aSource);
        TffShStr(aDest^) := StrPas(aSource);
      End;
    fstNullString,
      fstVarNullString:
      Begin
        If Not IGNORE_ANSIOEM Then
          AnsiToOEM(aSource, aSource);
        StrLCopy(aDest, aSource, pred(PhySize));
      End;
    fstWideString, fstVarWideString:
      Begin
        {convert this to a Pascal String}
        If Not IGNORE_ANSIOEM Then
          AnsiToOEM(aSource, aSource);
        StringToWideChar(StrPas(aSource), PWideChar(aDest), pred(PhySize));
      End;
    {fstUnicode:
      Begin
        Move(aSource^, aDest^, pred(PhySize));
      End; }
    Else
      Result := False;
  End;
End;
{--------}

Function MapFFDataToBDE(FFType: TfsFieldType;
  PhySize: Integer;
  aSource: Pointer;
  aDest: Pointer)
  : Boolean;
Var
  WorkString: String;
Begin
  {WARNING: the case statement below is in ascending order of switch
            value}
  // GetData
  Result := True;
  Case FFType Of
    fstBoolean:
      Begin
        WordBool(aDest^) := Boolean(aSource^);
      End;
    fstSingleChar:
      Begin
        Word(aDest^) := 0;
        char(aDest^) := char(aSource^);
      End;
    fstSingleWideChar:
      Begin
        {convert this to a Pascal String}
        WorkString := WideCharLenToString(PWideChar(aSource), 1);
        StrPLCopy(aDest, WorkString, pred(PhySize));
        If Not IGNORE_OEMANSI Then
          OEMToAnsi(aDest, aDest);
      End;
    fstUInt8:
      Begin
        Word(aDest^) := Byte(aSource^);
      End;
    fstUInt16:
      Begin
        Word(aDest^) := Word(aSource^);
      End;
    fstUInt32:
      Begin
        Longword(aDest^) := Longword(aSource^);
      End;
    fstInt8:
      Begin
        {NOTE: This maps to a SmallInt because the VCL does not have
               an 8-bit integer field type. }
        Smallint(aDest^) := Shortint(aSource^);
      End;
    fstInt16:
      Begin
        Smallint(aDest^) := Smallint(aSource^);
      End;
    fstInt32:
      Begin
        Longint(aDest^) := Longint(aSource^);
      End;
    fstAutoInc32:
      Begin
        Longint(aDest^) := Longint(aSource^);
      End;
    fstSingle:
      Begin
        Double(aDest^) := Single(aSource^);
      End;
    fstDouble:
      Begin
        Double(aDest^) := Double(aSource^);
      End;
    fstExtended:
      Begin
        Extended(aDest^) := Extended(aSource^);
      End;
    fstInt64, fstAutoInc64, fstRecVersion:
      Begin
        Int64(aDest^) := Int64(aSource^);
      End;
    fstCurrency:
      Begin
        Currency(aDest^) := Int64(aSource^) / 10000.0;
      End;
    fstBcd:
      Begin
        // TBcd(aDest^) := Tbcd(aSource^);
      End;

    fstDate:
      Begin
        If TStDate(aSource^) = BadDate Then
          Result := False
        Else {it's a valid StDate value}
          Begin
            DbiDate(aDest^) :=
              Trunc(StDateToDateTime(TStDate(aSource^)))
              + 693594;
          End;
      End;
    fstTime:
      Begin
        {StTimes are stored as # seconds since midnight; BDE logical
         times as # milliseconds; to convert, multiply by 1000}
        If TStTime(aSource^) = BadTime Then
          Result := False
        Else {it's a valid StDate value}
          Begin
            DBITime(aDest^) := TStTime(aSource^) * 1000;
          End;
      End;
    fstDateTime:
      Begin
        {FS datetimes are compatible with Delphi's TDateTime, viz:
         <days>.<timefraction>; BDE stores as # millisecs since
         base date; to convert, multiply by # millisecs/day}
        TimeStamp(aDest^) := TDateTime(aSource^) * 86400000.0;
      End;
    fstBLOB,
      fstBLOBMemo,
      fstBLOBGRAPHIC:
      Begin
        {just copy the BLOB number over}
        Longint(aDest^) := Longint(aSource^);
      End;
    fstArrayUInt8, fstArrayUInt16, fstArrayInt32, fstArrayDouble:
      Begin
        Move(aSource^, aDest^, PhySize);
      End;
    fstShortString:
      Begin
        StrPLCopy(aDest, TffShStr(aSource^), pred(PhySize));
        If Not IGNORE_OEMANSI Then
          OEMToAnsi(aDest, aDest);
      End;
    fstNullString,
      fstVarNullString:
      Begin
        StrLCopy(aDest, aSource, pred(PhySize));
        If Not IGNORE_ANSIOEM Then
          AnsiToOEM(aDest, aDest);
      End;
    fstWideString, fstVarWideString:
      Begin
        {convert this to a Pascal String}
        WorkString := WideCharLenToString(PWideChar(aSource),
          lstrlenw(PWideChar(aSource)));
        StrPLCopy(aDest, WorkString, pred(PhySize));
        If Not IGNORE_OEMANSI Then
          OEMToAnsi(aDest, aDest);
      End;
    { fstUnicode:
     Begin
       Move(aSource^, aDest^, pred(PhySize));
     End;}
    Else
      Result := False;
  End;
End;
{--------}

Procedure MapFFTypeToBDE(FFType: TfsFieldType;
  PhySize: Integer;
  Var BDEType: Word;
  Var LogSize: Longint);
Begin
  Case FFType Of
    fstBoolean: {..8-bit boolean flag}
      Begin
        BDEType := fldBoolean;
        LogSize := sizeof(WordBool);
      End;
    fstSingleChar: {..8-bit character}
      Begin
        BDEType := fldSingleChar;
        LogSize := 1;
      End;
    fstSingleWideChar: {..16-bit character (UNICODE)}
      Begin
        BDEType := fldSingleWideChar;
        LogSize := 1;
      End;
    fstUInt8: {..byte (8-bit unsigned integer)}
      Begin
        BDEType := fldByte;
        LogSize := sizeof(Word);
      End;
    fstUInt16: {..16-bit unsigned integer (aka word)}
      Begin
        BDEType := fldWord16;
        LogSize := sizeof(Word);
      End;
    fstUInt32: {..32-bit unsigned integer}
      Begin
        BDEType := fldWord32;
        LogSize := sizeof(Longint);
      End;
    fstInt8: {..8-bit signed integer}
      Begin
        BDEType := fldInt8;
        LogSize := sizeof(Smallint);
      End;
    fstInt16: {..16-bit signed integer}
      Begin
        BDEType := fldInt16;
        LogSize := sizeof(Smallint);
      End;
    fstInt32: {..32-bit signed integer}
      Begin
        BDEType := fldINT32;
        LogSize := sizeof(Longint);
      End;
    fstAutoInc32: {..32-bit unsigned auto-increment }
      Begin
        BDEType := fldAutoInc32;
        LogSize := sizeof(Longint);
      End;
    fstSingle: {..IEEE single (4 bytes)}
      Begin
        BDEType := fldSingle;
        LogSize := 8;
      End;
    fstDouble: {..IEEE double (8 bytes)}
      Begin
        BDEType := fldDouble;
        LogSize := 8;
      End;
    fstExtended: {..IEEE extended (10 bytes)}
      Begin
        BDEType := fldExtended;
        LogSize := 10;
      End;
    fstInt64:
      Begin
        BDEType := fldInt64;
        LogSize := sizeof(Int64);
      End;
    fstAutoInc64:
      Begin
        BDEType := fldAutoInc64;
        LogSize := sizeof(Int64);
      End;
    fstRecVersion:
      Begin
        BDEType := fldRecVersion;
        LogSize := sizeof(Int64);
      End;
    fstCurrency: {..Delphi currency type (8 bytes: scaled integer)
      range -922337203685477.5808..922337203685477.5807}
      Begin
        BDEType := fldCurrency;
        LogSize := 8;
      End;
    fstBcd:
      Begin
        BDEType := fldBcd;
        LogSize := 1; //sizeof(TBcd);
      End;

    fstDate: {..SysTools date type (4 bytes)}
      Begin
        BDEType := fldDATE;
        LogSize := sizeof(DBIDATE);
      End;
    fstTime: {..SysTools time type (4 bytes)}
      Begin
        BDEType := fldTIME;
        LogSize := sizeof(DbiTIME);
      End;
    fstDateTime: {..Delphi date/time type (8 bytes)}
      Begin
        BDEType := fldDateTime;
        LogSize := sizeof(TimeStamp);
      End;
    fstBLOB {, fstExBLOBFile}:
      Begin
        BDEType := fldBLOB;
        LogSize := sizeof(Longint);
      End;
    fstBLOBMemo:
      Begin
        BDEType := fldBLOBMEMO;
        LogSize := sizeof(Longint);
      End;
    fstBLOBGRAPHIC:
      Begin
        BDEType := fldBLOBGRAPHIC;
        LogSize := sizeof(Longint);
      End;
    fstArrayUInt8: {..array of bytes}
      Begin
        BDEType := fldByteArray;
        LogSize := PhySize;
      End;
    fstArrayUInt16:
      Begin
        BDEType := fldWordArray;
        LogSize := PhySize Div 2;
      End;
    fstArrayInt32:
      Begin
        BDEType := fldIntArray;
        LogSize := PhySize Div 4;
      End;
    fstArrayDouble:
      Begin
        BDEType := fldDoubleArray;
        LogSize := PhySize Div 8;
      End;
    fstShortString:
      Begin
        BDEType := fldShortString;
        LogSize := pred(PhySize);
      End;
    fstNullString:
      Begin
        BDEType := fldNullString;
        If (PhySize > dsfsMaxStringSize) Then
          PhySize := dsfsMaxStringSize;
        LogSize := pred(PhySize);
      End;
    fstVarNullString:
      Begin
        BDEType := fldVarNullString;
        If (PhySize > dsfsMaxStringSize) Then
          PhySize := dsfsMaxStringSize;
        LogSize := pred(PhySize);
      End;
    fstWideString:
      Begin
        If (PhySize > dsfsMaxStringSize) Then
          PhySize := dsfsMaxStringSize;
        BDEType := fldWideString;
        LogSize := pred(PhySize Div 2);
      End;
    fstVarWideString:
      Begin
        If (PhySize > dsfsMaxStringSize) Then
          PhySize := dsfsMaxStringSize;
        BDEType := fldVarWideString;
        LogSize := pred(PhySize Div 2);
      End;
    {fstUnicode:
      Begin
        If (PhySize > dsfsMaxStringSize) Then
          PhySize := dsfsMaxStringSize;
        BDEType := fldUnicode;
        LogSize := pred(PhySize Div 2);
      End;}
    Else
      FSRaiseExceptionNoData(EfsClientException, fsStrResClient, fsccInvalidParameter);
  End;
End;
{--------}

Procedure MapVCLTypeToFF(Const VCLType: TFieldType;
  Const VCLSize: Integer;
  Var FFType: TfsFieldType;
  Var FFSize: Longint);
Begin
  Case VCLType Of
    ftString:
      Begin
        FFType := fstNullString;
        FFSize := VCLSize;
      End;
    ftSmallint:
      Begin
        FFType := fstInt16;
        FFSize := SizeOf(Smallint);
      End;
    ftInteger:
      Begin
        FFType := fstInt32;
        FFSize := SizeOf(Longint);
      End;
    ftWord:
      Begin
        FFType := fstUInt16;
        FFSize := SizeOf(Word);
      End;
    ftBoolean:
      Begin
        FFType := fstBoolean;
        FFSize := SizeOf(Boolean);
      End;
    ftFloat:
      Begin
        FFType := fstDouble;
        FFSize := SizeOf(Double);
      End;
    ftCurrency:
      Begin
        FFType := fstCurrency;
        FFSize := 8;
      End;
    ftLargeInt:
      Begin
        FFType := fstInt64;
        FFSize := SizeOf(Int64);
      End;
    ftBCD:
      Begin
        FFType := fstBcd;
        //FFSize := SizeOf(TBcd);
      End;
    ftDate:
      Begin
        FFType := fstDate;
        FFSize := sizeof(TStDate);
      End;
    ftTime:
      Begin
        FFType := fstTime;
        FFSize := SizeOf(TStTime);
      End;
    ftDateTime:
      Begin
        FFType := fstDateTime;
        FFSize := SizeOf(TDateTime);
      End;
    ftBytes:
      Begin
        FFType := fstArrayUInt8;
        FFSize := VCLSize;
      End;
    ftVarBytes:
      Begin
        If (VCLSize <= 256) Then
          Begin
            FFType := fstShortString;
            FFSize := VCLSize;
          End
        Else
          Begin
            FFType := fstInterval;
            FFSize := 0;
          End;
      End;
    ftAutoInc:
      Begin
        FFType := fstAutoInc32;
        FFSize := SizeOf(Longint);
      End;
    ftBlob:
      Begin
        FFType := fstBLOB;
        FFSize := SizeOf(TffInt64);
      End;
    ftMemo:
      Begin
        FFType := fstBLOBMemo;
        FFSize := SizeOf(TffInt64);
      End;
    ftGraphic:
      Begin
        FFType := fstBLOBGraphic;
        FFSize := SizeOf(TffInt64);
      End;
    ftFixedChar:
      Begin
        FFType := fstNullString;
        FFSize := VCLSize;
      End;
    ftWideString:
      Begin
        FFType := fstWideString;
        FFSize := VCLSize;
      End;
    ftGuid:
      Begin
        FFType := fstNullString;
        FFSize := VCLSize;
      End;
    Else
      Begin
        { Use this to represent an unsupported field type. }
        FFType := fstInterval;
        FFSize := 0;
      End;
  End;
End;
{--------}

Function FFBDEDateEncode(aDay: Word;
  aMonth: Word;
  aYear: Word): DBIDATE;
Begin
  Result := trunc(SysUtils.EncodeDate(aYear, aMonth, aDay))
    + 693594;
End;
{--------}

Procedure FFBDEDateDecode(aDate: DBIDATE;
  Var aDay: Word;
  Var aMonth: Word;
  Var aYear: Word);
Var
  DT: TDateTime;
Begin
  DT := aDate - 693594.0;
  SysUtils.DecodeDate(DT, aYear, aMonth, aDay);
End;
{--------}

Function FFBDETimeEncode(aHour: Word;
  aMin: Word;
  aMilSec: Word): DBITIME;
{-converts hour, min, milsec to a raw time for a field}
Begin
  Result := trunc(SysUtils.EncodeTime(aHour, aMin, aMilSec Mod 1000,
    aMilSec Div 1000) * 86400000.0);
End;
{--------}

Procedure FFBDETimeDecode(aTime: DBITIME;
  Var aHour: Word;
  Var aMin: Word;
  Var aMilSec: Word);
{-converts a raw time from a field to hour, min, milsec}
Var
  DT: TDateTime;
  aSec: Word;
Begin
  DT := aTime / 86400000.0;
  SysUtils.DecodeTime(DT, aHour, aMin, aSec, aMilSec);
  aMilSec := aMilSec + aSec * 1000;
End;

{====================================================================}

End.

