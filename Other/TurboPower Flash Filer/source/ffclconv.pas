{*********************************************************}
{* FlashFiler: Field and Record Conversion Routines      *}
{*********************************************************}

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
 * The Original Code is TurboPower FlashFiler
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1996-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

{$I ffdefine.inc}

unit ffclconv;

interface

uses
  Windows,
  SysUtils,
  DB,
  ffclbde,
  ffsrbde,
  ffllbase,
  ffllexcp,
  ffconst,
  ffclbase;

procedure GetBDEFieldDescriptor(const FFFieldDesc : TFFFieldDescriptor;
                                  var BDEFldDesc  : FLDDesc);
  {-converts a FlashFiler field descriptor into a physical BDE one}

procedure GetBDEIndexDescriptor(const FFIndexDesc : TFFIndexDescriptor;
                                  var BDEIdxDesc   : IDXDesc);
  {-converts a FlashFiler index descriptor into a BDE one}

procedure GetBDELogicalFieldDescriptor(const FFFieldDesc  : FLDDesc;
                                         var BDEFieldDesc : FLDDesc);
  {-converts a FlashFiler based BDE field description to a logical one
    NOTE: the field iOffset is not set - to be calculated later}

procedure GetBDEVChkDescriptor(FFVChkDesc  : TffVCheckDescriptor;
                           var BDEVChkDesc : VCHKDesc;
                               FieldNumber : longint;
                               Required    : boolean);
  {-converts a FlashFiler validity check descriptor into a BDE one}


function GetFFSearchKeyAction(
         const aDBISearchCond : DBISearchCond) : TffSearchKeyAction;
  {-convert a BDE search action to the FF one}

function MapBDEDataToFF(FFType  : TffFieldType;
                        PhySize : integer;
                        aSource : pointer;
                        aDest   : pointer) : boolean;
  {-maps a BDE logical field value in aSource to the equivalent FF
    value in aDest. Note that type conversion is assumed to be the
    reverse of MapFFTypeToBDE}

procedure MapBDETypeToFF(BDEType    : word;
                         BDESubType : word;
                         LogSize    : integer;
                     var FFType     : TffFieldType;
                     var PhySize    : word);
  {-maps a BDE field type/subtype to the nearest FF type and returns
    the physical size}

function MapFFDataToBDE(FFType  : TffFieldType;
                        PhySize : integer;
                        aSource : pointer;
                        aDest   : pointer) : boolean;
  {-maps a FlashFiler field value in aSource to the equivalent BDE
    value in aDest. Note that type conversion is assumed to be the
    same as MapFFTypeToBDE}

procedure MapFFTypeToBDE(FFType     : TffFieldType;
                         PhySize    : integer;
                     var BDEType    : word;
                     var BDESubType : word;
                     var LogSize    : word);
  {-maps a FlashFiler field type to the nearest BDE logical type/
    subtype and returns the logical size}

procedure MapVCLTypeToFF(const VCLType  : TFieldType;
                         const VCLSize  : integer;
                           var FFType   : TffFieldType;
                           var FFSize   : word);
  {-maps a VCL field type to the nearest FF equivalent.  If the specified
    VCLType is not supported, FFType is set to fftReserved20. }

function FFBDEDateEncode(aDay   : word;
                         aMonth : word;
                         aYear  : word) : DBIDATE;
  {-converts day, month, year to a raw date for a field}

procedure FFBDEDateDecode(aDate  : DBIDATE;
                   var aDay   : word;
                   var aMonth : word;
                   var aYear  : word);
  {-converts a raw date from a field to day, month, year}

function FFBDETimeEncode(aHour : Word;
                         aMin  : Word;
                         aMilSec : Word) : DBITIME;
  {-converts hour, min, milsec to a raw time for a field}

procedure FFBDETimeDecode(aTime   : DBITIME;
                      var aHour   : Word;
                      var aMin    : Word;
                      var aMilSec : Word);
  {-converts a raw time from a field to hour, min, milsec}

implementation

uses
  ffstdate;

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
base date.

Note that FlashFiler stores its datetimes as the Delphi 1
TDateTime type, so the conversion between the FlashFiler physical
value and the BDE logical value is a matter of multiplying/dividing by
the number of milliseconds in a day.}

const
  IGNORE_OEMANSI : Boolean = True;
  IGNORE_ANSIOEM : Boolean = True;
  dsMaxStringSize = 8192; {copied from DB.PAS}


{===Interfaced routines==============================================}
procedure GetBDEFieldDescriptor(const FFFieldDesc : TFFFieldDescriptor;
                                  var BDEFldDesc  : FLDDesc);
begin
  {clear the result structure to binary zeros}
  FillChar(BDEFldDesc, sizeof(FLDDesc), 0);
  {fill the relevant parts of the result structure}
  with BDEFldDesc, FFFieldDesc do begin
    iFldNum := succ(fdNumber);
    FFStrPCopy(szName, fdName);
    iFldType := ord(fdType);
    iSubType := 0;
    iUnits1 := fdUnits;
    iUnits2 := fdDecPl;
    iOffset := fdOffset;
    iLen := fdLength;
    {iNullOffset := 0;}
    if Assigned(fdVCheck) or fdRequired then
      efldvVchk := fldvHASCHECKS
    else
      efldvVchk := fldvNOCHECKS;
    efldrRights := fldrREADWRITE;
    {bCalcField := False;}
  end;
end;
{--------}
procedure GetBDEIndexDescriptor(const FFIndexDesc : TFFIndexDescriptor;
                                  var BDEIdxDesc  : IDXDesc);
var
  i : Integer;
begin
  {clear the result structure to binary zeros}
  FillChar(BDEIdxDesc, sizeof(IDXDesc), 0);
  {fill the relevant parts of the result structure}
  with FFIndexDesc, BDEidxDesc do begin
    StrPLCopy(szName, idName, sizeof(szName) - 1);
    iIndexId := idNumber;
    {StrCopy(szTagName, '');}
    StrCopy(szFormat, 'BTREE');
    {bPrimary := false;}
    bUnique := not idDups;
    bDescending := not idAscend;
    bMaintained := True;              {all FF keys are maintained}
    {bSubSet := false;}
    {iCost := 0}
    if (idCount = -1) then begin
      {this is a User-defined or Seq.Access Index: we'll treat it as
       an expression Index - see dBASE info...}
      bExpIdx := True;
      iFldsInKey := 0;
    end
    else {it's a composite index} begin
      bExpIdx := False;
      iFldsInKey := idCount;
      for i := 0 to pred(iFldsInKey) do
        aiKeyFld[i] := succ(idFields[i]); {FF fields are 0-based, BDE's are 1-based}
    end;
    iKeyLen := idKeyLen;
    {bOutOfDate := false;}
    {iKeyExpType := 0;}
    {StrCopy(szKeyExp, '');}
    {StrCopy(szKeyCond, '');}
    bCaseInsensitive := idNoCase;
    {iBlockSize := 0;}
    {iRestrNum := 0}
  end;
end;
{--------}
procedure GetBDELogicalFieldDescriptor(const FFFieldDesc  : FLDDesc;
                                         var BDEFieldDesc : FLDDesc);
begin
  {clear the result structure to binary zeros}
  FillChar(BDEFieldDesc, sizeof(BDEFieldDesc), 0);
  {fill the relevant parts of the result structure}
  with BDEFieldDesc do begin
    iFldNum := FFFieldDesc.iFldNum;
    StrCopy(szName, FFFieldDesc.szName);
    MapFFTypeToBDE(TFFFieldType(FFFieldDesc.iFldType),
                   FFFieldDesc.iLen,
                   iFldType,
                   iSubType,
                   iLen);
    if (iFldType = fldZSTRING) then
      iUnits1 := iLen
    else if (iFldType = fldBYTES) then
      iUnits1 := iLen;
    {iUnits2 := 0; - unused}
    {iOffset := 0; - this is set later}
    {iNullOffset := 0; - there is none in a converted desc}
    efldvVchk := fldvNOCHECKS;
    efldrRights := fldrREADWRITE;
    {bCalcField := 0;}
  end;
end;
{--------}
procedure GetBDEVChkDescriptor(FFVChkDesc  : TffVCheckDescriptor;
                           var BDEVChkDesc : VCHKDesc;
                               FieldNumber : longint;
                               Required    : boolean);
begin
  {clear the result structure to binary zeros}
  FillChar(BDEVchkDesc, sizeof(VCHKDesc), 0);
  {fill the relevant parts of the result structure}
  with BDEVChkDesc, FFVChkDesc do begin
    iFldNum := FieldNumber;
    bRequired := Required;
    bHasMinVal := vdHasMinVal;
    bHasMaxVal := vdHasMaxVal;
    bHasDefVal := vdHasDefVal;
    if vdHasMinVal then
      Move(vdMinVal, aMinVal, 254);
    if vdHasMaxVal then
      Move(vdMaxVal, aMaxVal, 254);
    if vdHasDefVal then
      Move(vdDefVal, aDefVal, 254);
    StrPCopy(szPict, vdPicture);
    {elkupType := lkupNONE;}
    {szLkupTblName[0] := #0;}
  end;
end;
{--------}
function GetFFSearchKeyAction(
         const aDBISearchCond : DBISearchCond) : TffSearchKeyAction;
begin
  case aDBISearchCond of
    keySEARCHEQ  : Result := skaEqual;
    keySEARCHGT  : Result := skaGreater;
    keySEARCHGEQ : Result := skaGreaterEqual;
  else
    Result := skaEqual;
  end;
end;
{--------}
function MapBDEDataToFF(FFType  : TffFieldType;
                        PhySize : integer;
                        aSource : pointer;
                        aDest   : pointer) : boolean;
var
  WorkWideChar : array [0..1] of WideChar;
  DateValue    : TStDate;
begin
  {WARNING: the case statement below is in ascending order of switch
            value}
  Result := true;
  case FFType of
    fftBoolean:
      begin
        Boolean(aDest^) := WordBool(aSource^);
      end;
    fftChar:
      begin
        Char(aDest^) := Char(aSource^); {copy one character}
      end;
    fftWideChar:
      begin
        if not IGNORE_ANSIOEM then
          AnsiToOEM(aSource, aSource);
        StringToWideChar(StrPas(aSource), WorkWideChar, PhySize);
        Move(WorkWideChar[0], aDest^, sizeof(WideChar));
      end;
    fftByte:
      begin
        Byte(aDest^) := Word(aSource^);
      end;
    fftWord16:
      begin
        Word(aDest^) := Word(aSource^);
      end;
    fftWord32:
      begin
        LongInt(aDest^) := LongInt(aSource^);
      end;
    fftInt8:
      begin
        ShortInt(aDest^) := ShortInt(aSource^);                        {!!.07}
      end;
    fftInt16:
      begin
        SmallInt(aDest^) := SmallInt(aSource^);
      end;
    fftInt32:
      begin
        LongInt(aDest^) := LongInt(aSource^);
      end;
    fftAutoInc:
      begin
        LongInt(aDest^) := LongInt(aSource^);
      end;
    fftSingle:
      begin
        Single(aDest^) := Double(aSource^);
      end;
    fftDouble:
      begin
        Double(aDest^) := Double(aSource^);
      end;
    fftExtended:
      begin
        Extended(aDest^) := Double(aSource^);
      end;
    fftComp:
      begin
        Comp(aDest^) := Double(aSource^);
      end;
    fftCurrency:
      begin
        Currency(aDest^) := Double(aSource^);
      end;
    fftStDate:
      begin
        DateValue := DateTimeToStDate(
            DbiDate(aSource^)  - 693594.0);
        if DateValue = BadDate then begin
          TStDate(aDest^) := 0;
          Result := false;
        end
        else
          TStDate(aDest^) := DateValue;
      end;
    fftStTime:
      begin
        {StTimes are stored as # seconds since midnight; BDE logical
         times as # milliseconds; to convert, divide by 1000}
        TStTime(aDest^) := DBITime(aSource^) div 1000;
      end;
    fftDateTime:
      begin
        {FF datetimes are compatible with Delphi's TDateTime, viz:
         <days>.<timefraction>; BDE stores as # millisecs since
         base date; to convert, divide by # millisecs/day}
        TDateTime(aDest^) := TimeStamp(aSource^) / 86400000.0;
      end;
    fftBLOB,
    fftBLOBMemo,
    fftBLOBFmtMemo,
    fftBLOBOLEObj,
    fftBLOBGraphic,
    fftBLOBDBSOLEObj,
    fftBLOBTypedBin,
    fftBLOBFile:
      begin
        {just copy over the BLOB number}
        LongInt(aDest^) := LongInt(aSource^);
      end;
    fftByteArray:
      begin
        Move(aSource^, aDest^, PhySize);
      end;
    fftShortString,
    fftShortAnsiStr:
      begin
        if not IGNORE_ANSIOEM then
          AnsiToOEM(aSource, aSource);
        TffShStr(aDest^) := StrPas(aSource);
      end;
    fftNullString,
    fftNullAnsiStr:
      begin
        if not IGNORE_ANSIOEM then
          AnsiToOEM(aSource, aSource);
        StrLCopy(aDest, aSource, pred(PhySize));
      end;
    fftWideString:
      begin
        {convert this to a Pascal String}
        if not IGNORE_ANSIOEM then
          AnsiToOEM(aSource, aSource);
        StringToWideChar(StrPas(aSource), PWideChar(aDest), pred(PhySize));
       end;
  else
    Result := false;
  end;
end;
{--------}
procedure MapBDETypeToFF(BDEType    : word;
                         BDESubType : word;
                         LogSize    : integer;
                     var FFType     : TffFieldType;
                     var PhySize    : word);
begin
  {WARNING: the case statement below is in ascending order of switch
            value}
  case BDEType of
    fldZSTRING:
      begin
        FFType := fftNullString;
        PhySize := LogSize;
      end;
    fldDATE:
      begin
        FFType := fftStDate;
        PhySize := sizeof(TStDate);
      end;
    fldBLOB:
      begin
        case BDESubType of
          fldstMEMO        : FFType := fftBLOBMemo;
          fldstBINARY      : FFType := fftBLOB;
          fldstFMTMEMO     : FFType := fftBLOBFmtMemo;
          fldstOLEOBJ      : FFType := fftBLOBOLEObj;
          fldstGRAPHIC     : FFType := fftBLOBGraphic;
          fldstDBSOLEOBJ   : FFType := fftBLOBDBSOLEObj;
          fldstTYPEDBINARY : FFType := fftBLOBTypedBin;
          fldstBFILE       : FFType := fftBLOBFile;
        else
          FFRaiseExceptionNoData(EffClientException, ffStrResClient, ffccInvalidParameter);
        end;
        PhySize := sizeof(longint);
      end;
    fldBOOL:
      begin
        FFType := fftBoolean;
        PhySize := sizeof(boolean);
      end;
    fldINT16:
      begin
        FFType := fftInt16;
        PhySize := sizeof(smallint);
      end;
    fldINT32:
      begin
        if (BDESubType = fldstAUTOINC) then
          FFType := fftAutoInc
        else
          FFType := fftInt32;
        PhySize := sizeof(longint);
      end;
    fldFLOAT:
      begin
        if BDESubType = fldstMoney then begin
          FFType := fftCurrency;
          PhySize := sizeof(Currency);
        end
        else begin
          FFType := fftDouble;
          PhySize := sizeof(double);
        end;
      end;
    fldBCD:
      begin
        FFType := fftCurrency;
        PhySize := sizeof(Currency);
      end;
    fldBYTES:
      begin
        FFType := fftByteArray;
        PhySize := LogSize;
      end;
    fldTIME:
      begin
        FFType := fftstTime;
        PhySize := sizeof(TStTime);
      end;
    fldTIMESTAMP:
      begin
        FFType := fftDateTime;
        PhySize := sizeof(TDateTime);
      end;
    fldUINT16:
      begin
        FFType := fftWord16;
        PhySize := sizeof(word);
      end;
    fldUINT32:
      begin
        FFType := fftWord32;
        PhySize := sizeof(TffWord32);
      end;
    fldFLOATIEEE:
      begin
        FFType := fftExtended;
        PhySize := sizeof(Extended);
      end;
    fldVARBYTES:
      begin
        if (LogSize > 256) then
          FFRaiseExceptionNoData(EffClientException, ffStrResClient, ffccInvalidParameter);
        FFType := fftShortString;
        PhySize := LogSize;
      end;
  else
    FFRaiseExceptionNoData(EffClientException, ffStrResClient, ffccInvalidParameter);
  end;
end;
{--------}
function MapFFDataToBDE(FFType  : TffFieldType;
                        PhySize : Integer;
                        aSource : Pointer;
                        aDest   : Pointer)
                                : Boolean;
var
  WorkString : string;
begin
  {WARNING: the case statement below is in ascending order of switch
            value}
  Result := True;
  case FFType of
    fftBoolean:
      begin
        WordBool(aDest^) := Boolean(aSource^);
      end;
    fftChar:
      begin
        word(aDest^) := 0;
        char(aDest^) := char(aSource^);
      end;
    fftWideChar:
      begin
        {convert this to a Pascal String}
        WorkString := WideCharLenToString(PWideChar(aSource), 1);
        StrPLCopy(aDest, WorkString, pred(PhySize));
        if not IGNORE_OEMANSI then
          OEMToAnsi(aDest, aDest);
      end;
    fftByte:
      begin
        Word(aDest^) := Byte(aSource^);
      end;
    fftWord16:
      begin
        Word(aDest^) := Word(aSource^);
      end;
    fftWord32:
      begin
        LongInt(aDest^) := LongInt(aSource^);
      end;
    fftInt8:
      begin
        {NOTE: This maps to a SmallInt because the VCL does not have
               an 8-bit integer field type. }
        SmallInt(aDest^) := ShortInt(aSource^);
      end;
    fftInt16:
      begin
        SmallInt(aDest^) := SmallInt(aSource^);
      end;
    fftInt32:
      begin
        LongInt(aDest^) := LongInt(aSource^);
      end;
    fftAutoInc:
      begin
        LongInt(aDest^) := LongInt(aSource^);
      end;
    fftSingle:
      begin
        Double(aDest^) := Single(aSource^);
      end;
    fftDouble:
      begin
        Double(aDest^) := Double(aSource^);
      end;
    fftExtended:
      begin
        Double(aDest^) := Extended(aSource^);
      end;
    fftComp:
      begin
        Double(aDest^) := Comp(aSource^);
      end;
    fftCurrency:
      begin
        Double(aDest^) := Currency(aSource^);
      end;
    fftStDate:
      begin
        if TStDate(aSource^) = BadDate then
          Result := false
        else {it's a valid StDate value} begin
          DbiDate(aDest^) :=
             Trunc(StDateToDateTime(TStDate(aSource^)))
             + 693594;
        end;
      end;
    fftStTime:
      begin
        {StTimes are stored as # seconds since midnight; BDE logical
         times as # milliseconds; to convert, multiply by 1000}
        if TStTime(aSource^) = BadTime then
          Result := false
        else {it's a valid StDate value} begin
          DBITime(aDest^) := TStTime(aSource^) * 1000;
        end;
      end;
    fftDateTime:
      begin
        {FF datetimes are compatible with Delphi's TDateTime, viz:
         <days>.<timefraction>; BDE stores as # millisecs since
         base date; to convert, multiply by # millisecs/day}
        TimeStamp(aDest^) := TDateTime(aSource^) * 86400000.0;
      end;
    fftBLOB,
    fftBLOBMemo,
    fftBLOBFmtMemo,
    fftBLOBOLEObj,
    fftBLOBGRAPHIC,
    fftBLOBDBSOLEOBJ,
    fftBLOBTypedBin,
    fftBLOBFile:
      begin
        {just copy the BLOB number over}
        longint(aDest^) := longint(aSource^);
      end;
    fftByteArray:
      begin
        Move(aSource^, aDest^, PhySize);
      end;
    fftShortString,
    fftShortAnsiStr:
      begin
        StrPLCopy(aDest, TffShStr(aSource^), pred(PhySize));
        if not IGNORE_OEMANSI then
          OEMToAnsi(aDest, aDest);
      end;
    fftNullString,
    fftNullAnsiStr:
      begin
        StrLCopy(aDest, aSource, pred(PhySize));
        if not IGNORE_ANSIOEM then
          AnsiToOEM(aDest, aDest);
      end;
    fftWideString:
      begin
        {convert this to a Pascal String}
        WorkString := WideCharLenToString(PWideChar(aSource),
                                          lstrlenw(PWideChar(aSource)));
        StrPLCopy(aDest, WorkString, pred(PhySize));
        if not IGNORE_OEMANSI then
          OEMToAnsi(aDest, aDest);
       end;
  else
    Result := false;
  end;
end;
{--------}
procedure MapFFTypeToBDE(FFType     : TFFFieldType;
                         PhySize    : integer;
                     var BDEType    : word;
                     var BDESubType : word;
                     var LogSize    : word);
begin
  BDESubType := 0;
  case FFType of
    fftBoolean:      {..8-bit boolean flag}
      begin
        BDEType := fldBOOL;
        LogSize := sizeof(WordBool);
      end;
    fftChar:         {..8-bit character}
      begin
        BDEType := fldZString;
        LogSize := 1; 
      end;
    fftWideChar:     {..16-bit character (UNICODE)}
      begin
        BDEType := fldZString;
        LogSize := 1; 
      end;
    fftByte:         {..byte (8-bit unsigned integer)}
      begin
        BDEType := fldUINT16;
        LogSize := sizeof(word);
      end;
    fftWord16:       {..16-bit unsigned integer (aka word)}
      begin
        BDEType := fldUINT16;
        LogSize := sizeof(word);
      end;
    fftWord32:       {..32-bit unsigned integer}
      begin
        BDEType := fldINT32;                                    
        LogSize := sizeof(longint);
      end;
    fftInt8:         {..8-bit signed integer}
      begin
        BDEType := fldINT16;
        LogSize := sizeof(smallint);
      end;
    fftInt16:        {..16-bit signed integer}
      begin
        BDEType := fldINT16;
        LogSize := sizeof(smallint);
      end;
    fftInt32:        {..32-bit signed integer}
      begin
        BDEType := fldINT32;
        LogSize := sizeof(longint);
      end;
    fftAutoInc:      {..32-bit unsigned auto-increment }
      begin
        BDEType := fldINT32;
        BDESubType := fldstAUTOINC;
        LogSize := sizeof(longint);
      end;
    fftSingle:       {..IEEE single (4 bytes)}
      begin
        BDEType := fldFLOAT;
        LogSize := sizeof(double);
      end;
    fftDouble:       {..IEEE double (8 bytes)}
      begin
        BDEType := fldFLOAT;
        LogSize := sizeof(double);
      end;
    fftExtended:     {..IEEE extended (10 bytes)}
      begin
        BDEType := fldFLOAT;   {BDE doesn't REALLY support 80-bit float}
        LogSize := sizeof(double);
      end;
    fftComp:         {..IEEE comp type (8 bytes signed integer)
                      range -2E63+1..2E63-1}
      begin
        BDEType := fldFLOAT;
        LogSize := sizeof(double);
      end;
    fftCurrency:     {..Delphi currency type (8 bytes: scaled integer)
                      range -922337203685477.5808..922337203685477.5807}
      begin
        BDEType := fldFloat;
        BDESubType := fldstMONEY;
        LogSize := sizeof(double);
      end;
    fftStDate:       {..SysTools date type (4 bytes)}
      begin
        BDEType := fldDATE;
        LogSize := sizeof(DBIDATE);
      end;
    fftStTime:       {..SysTools time type (4 bytes)}
      begin
        BDEType := fldTIME;
        LogSize := sizeof(DbiTIME);
      end;
    fftDateTime:     {..Delphi date/time type (8 bytes)}
      begin
        BDEType := fldTIMESTAMP;
        LogSize := sizeof(TIMESTAMP);
      end;
    fftBLOB, fftBLOBFile:
      begin
        BDEType := fldBLOB;
        BDESubType := fldstBINARY;
        LogSize := sizeof(longint);
      end;
    fftBLOBMemo:
      begin
        BDEType := fldBLOB;
        BDESubType := fldstMEMO;
        LogSize := sizeof(longint);
      end;
    fftBLOBFmtMemo:
      begin
        BDEType := fldBLOB;
        BDESubType := fldstFMTMEMO;
        LogSize := sizeof(longint);
      end;
    fftBLOBOLEObj:
      begin
        BDEType := fldBLOB;
        BDESubType := fldstOLEOBJ;
        LogSize := sizeof(longint);
      end;
    fftBLOBGRAPHIC:
      begin
        BDEType := fldBLOB;
        BDESubType := fldstGRAPHIC;
        LogSize := sizeof(longint);
      end;
    fftBLOBDBSOLEOBJ:
      begin
        BDEType := fldBLOB;
        BDESubType := fldstDBSOLEOBJ;
        LogSize := sizeof(longint);
      end;
    fftBLOBTypedBin:
      begin
        BDEType := fldBLOB;
        BDESubType := fldstTYPEDBINARY;
        LogSize := sizeof(longint);
      end;
    fftByteArray:    {..array of bytes}
      begin
        BDEType := fldBYTES;
        LogSize := PhySize;
      end;
    fftShortString,  {..length byte string}
    fftShortAnsiStr:
      begin
        BDEType := fldZSTRING;
        LogSize := pred(PhySize);
      end;
    fftNullString,   {..null-terminated string}
    fftNullAnsiStr,
    fftWideString:
      begin
        if (PhySize <= dsMaxStringSize) then begin
          BDEType := fldZSTRING;
{Begin !!.11}
          if FFType = fftWideString then
            LogSize := pred(PhySize div 2)
          else
            LogSize := pred(PhySize);
{End !!.11}
        end
        else begin
          BDEType    := fldBLOB;
          BDESubType := fldstMEMO;
          LogSize := sizeof(longint);
        end;
      end;
  else
    FFRaiseExceptionNoData(EffClientException, ffStrResClient, ffccInvalidParameter);
  end;
end;
{--------}
procedure MapVCLTypeToFF(const VCLType  : TFieldType;
                         const VCLSize  : integer;
                           var FFType   : TffFieldType;
                           var FFSize   : word);
begin
  case VCLType of
    ftString :
      begin
        FFType := fftNullString;
        FFSize := VCLSize;
      end;
    ftSmallInt :
      begin
        FFType := fftInt16;
        FFSize := SizeOf(smallInt);
      end;
    ftInteger :
      begin
        FFType := fftInt32;
        FFSize := SizeOf(longInt);
      end;
    ftWord :
      begin
        FFType := fftWord16;
        FFSize := SizeOf(Word);
      end;
    ftBoolean :
      begin
        FFType := fftBoolean;
        FFSize := SizeOf(Boolean);
      end;
    ftFloat :
      begin
        FFType := fftDouble;
        FFSize := SizeOf(Double);
      end;
    ftCurrency :
      begin
        FFType := fftCurrency;
        FFSize := SizeOf(Currency);
      end;
    ftBCD :
      begin
        FFType := fftCurrency;
        FFSize := SizeOf(Currency);
      end;
    ftDate :
      begin
        FFType := fftStDate;
        FFSize := sizeof(TStDate);
      end;
    ftTime :
      begin
        FFType := fftstTime;
        FFSize := SizeOf(TStTime);
      end;
    ftDateTime :
      begin
        FFType := fftDateTime;
        FFSize := SizeOf(TDateTime);
      end;
    ftBytes :
      begin
        FFType := fftByteArray;
        FFSize := VCLSize;
      end;
    ftVarBytes :
      begin
        if (VCLSize <= 256) then begin
          FFType := fftShortString;
          FFSize := VCLSize;
        end else begin
          FFType := fftReserved20;
          FFSize := 0;
        end;
      end;
    ftAutoInc :
      begin
        FFType := fftAutoInc;
        FFSize := SizeOf(LongInt);
      end;
    ftBlob :
      begin
        FFType := fftBLOB;
        FFSize := SizeOf(TffInt64);                                    {!!.13}
      end;
    ftMemo :
      begin
        FFType := fftBLOBMemo;
        FFSize := SizeOf(TffInt64);                                    {!!.13}
      end;
    ftGraphic :
      begin
        FFType := fftBLOBGraphic;
        FFSize := SizeOf(TffInt64);                                    {!!.13}
      end;
    ftFmtMemo :
      begin
        FFType := fftBLOBFmtMemo;
        FFSize := SizeOf(TffInt64);                                    {!!.13}
      end;
    ftParadoxOLE :
      begin
        FFType := fftBLOBOleObj;
        FFSize := SizeOf(TffInt64);                                    {!!.13}
      end;
    ftDBaseOLE :
      begin
        FFType := fftBLOBDBSOleObj;
        FFSize := SizeOf(TffInt64);                                    {!!.13}
      end;
    ftTypedBinary :
      begin
        FFType := fftBLOBTypedBin;
        FFSize := SizeOf(TffInt64);                                    {!!.13}
      end;
    {$IFDEF DCC4OrLater}
    ftFixedChar :
      begin
        FFType := fftNullString;
        FFSize := VCLSize;
      end;
    ftWideString :
      begin
        FFType := fftWideString;
        FFSize := VCLSize;
      end;
    {$ENDIF}
    {$IFDEF DCC5OrLater}
    ftGUID :
      begin
        FFType := fftNullString;
        FFSize := VCLSize;
      end;
    {$ENDIF}
  else
    { Use this to represent an unsupported field type. }
    FFType := fftReserved20;
    FFSize := 0;
  end;
end;
{--------}
function FFBDEDateEncode(aDay   : word;
                         aMonth : word;
                         aYear  : word) : DBIDATE;
begin
  Result := trunc(SysUtils.EncodeDate(aYear, aMonth, aDay))
            + 693594;
end;
{--------}
procedure FFBDEDateDecode(aDate  : DBIDATE;
                      var aDay   : word;
                      var aMonth : word;
                      var aYear  : word);
var
  DT : TDateTime;
begin
  DT := aDate - 693594.0;
  SysUtils.DecodeDate(DT, aYear, aMonth, aDay);
end;
{--------}
function FFBDETimeEncode(aHour : Word;
                         aMin  : Word;
                         aMilSec : Word) : DBITIME;
  {-converts hour, min, milsec to a raw time for a field}
begin
  Result := trunc(SysUtils.EncodeTime(aHour, aMin, aMilSec mod 1000,
                                      aMilSec div 1000) * 86400000.0);
end;
{--------}
procedure FFBDETimeDecode(aTime   : DBITIME;
                      var aHour   : Word;
                      var aMin    : Word;
                      var aMilSec : Word);
  {-converts a raw time from a field to hour, min, milsec}
var
  DT   : TDateTime;
  aSec : Word;
begin
  DT := aTime / 86400000.0;
  SysUtils.DecodeTime(DT, aHour, aMin, aSec, aMilSec);
  aMilSec := aMilSec + aSec * 1000;
end;

{====================================================================}

end.

