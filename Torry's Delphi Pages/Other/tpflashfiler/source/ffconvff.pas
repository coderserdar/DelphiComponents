{*********************************************************}
{* FlashFiler: Field conversion for server               *}
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

unit ffconvff;

interface

uses
  ffllbase,
  fflldict,
  ffsrbde,
  ffstdate,
  SysUtils;

function FFConvertSingleField(aSourceValue,
                              aTargetValue: Pointer;
                              aSourceType,
                              aTargetType: TffFieldType;
                              aSourceLength,
                              aTargetLength: Integer): TffResult;

{ This is the low-level data conversion routine for converting one FlashFiler
  datatype to another.  This is primarily used by the table restructure
  operation.  This routine accepts an input and output field specification
  and determines if the input field can be converted to the output field.
  If so, it copies and translates the input field data into the output
  field.

  This routine serves two purposes for table restructuring:

  1)  when the records are read, it does the data conversion between
  the input fields and the output fields;

  2) when the field map is initially validated (before the data is read/
  written), it is used to determine if each field map entry is legal
  (without actually moving any data around).

  By serving double-duty like this, we centralize this fairly detailed
  logic and reduce the likelihood of mistakes in updating it. Specifically,
  when used for situation #2, nil is passed in for the field pointers and
  the logic checks for this in the case statement.  This lets the
  logic flow through the case statement to find the correct datatype
  matches, but stops short of actually copying any data.

  Note on BLOB Conversions:  BLOB-to-BLOB and ByteArray-to-BLOB conversions
  are legal and this routine validates that fact (when called with nil value
  pointers), but does not actually copy to/from BLOB fields.  The caller
  is responsible for detecting a BLOB target field and handling the data
  conversion.  All this routine does it tell you that such a conversion is
  legal.

  Note on null field values: This routine assumes it will not see a null
  input value (that is, it assumes null input values are captured by the
  caller and handled at that level).  After all, if the input value is null,
  the output value will always be null regardless of the datatypes involved.

  It is intended that this routine could be compiled into both a server app
  and a client app.  Specifically, this routine is used by FF Explorer to
  perform real time validation of table restructure field assignments
  without waiting for the whole restructure package to be sent to the
  server and subsequently fail if the user selected incompatible datatypes.


  Parameters:

  aSourceValue and aTargetValue point to the input and output field values
  (that is, the start position within the record buffers where these values
  can be found).  If both are nil, then only an assignment compatabiliy
  check is performed, no data is actually moved.

  aSourceType and aTargetType indicate the FlashFiler datatype of the
  fields.

  aSourceLength and aTargetLength are the maximum lengths, in bytes, of
  each data field (ignored if only doing assignment compatability check).
}

implementation

uses
  typinfo,
  ffconst,
  ffllexcp;

function FFRemoveThousandSeparator(const str : string): string;
begin
  Result := str;
  while pos(ThousandSeparator, Result)>0 do
    Delete(Result, pos(ThousandSeparator, Result), 1);
end;

function FFConvertSingleField(aSourceValue,
                              aTargetValue: Pointer;
                              aSourceType,
                              aTargetType: TffFieldType;
                              aSourceLength,
                              aTargetLength: Integer): TffResult;
var
  MinLength: Integer;
  srcBoolean: ^Boolean absolute aSourceValue;
  WorkString: String[11];                                                  {!!.10}
  { workspacelength equals Length(IntToStr(Low(Integer))),
    used for converting various int-types to string }
  {Begin !!.13}
  aCode,
  intRes: Integer;
  wordRes: TffWord32;
  {End !!.13}
begin
  Result := DBIERR_NONE;

  MinLength := FFMinI(aSourceLength, aTargetLength);

  case aSourceType of
    fftBoolean: begin
      { Booleans can be translated into char or string fields (Y or N), or
        integer numeric fields (ordinal value, 0 - false, 1 - true) }

      case aTargetType of
        fftBoolean:
          if Assigned(aTargetValue) then
            Boolean(aTargetValue^) := srcBoolean^;
        fftChar:
          if Assigned(aTargetValue) then
            if srcBoolean^ then Char(aTargetValue^) := 'Y'
            else Char(aTargetValue^) := 'N';
        fftByte, fftInt8:
          if Assigned(aTargetValue) then
            Byte(aTargetValue^) := Ord(srcBoolean^);
        fftWord16, fftInt16:
          if Assigned(aTargetValue) then
            Word(aTargetValue^) := Ord(srcBoolean^);
        fftWord32, fftInt32:
          if Assigned(aTargetValue) then
            LongInt(aTargetValue^) := Ord(srcBoolean^);
        fftShortString, fftShortAnsiStr:
          if Assigned(aTargetValue) then
            if srcBoolean^ then TffShStr(aTargetValue^) := 'Y'
            else TffShStr(aTargetValue^) := 'N';
        fftNullString, fftNullAnsiStr:
          if Assigned(aTargetValue) then
            if srcBoolean^ then FFStrPCopy(aTargetValue, 'Y')
            else FFStrPCopy(aTargetValue, 'N');
        fftWideChar:
          if Assigned(aTargetValue) then
            if srcBoolean^ then WideChar(aTargetValue^) := FFCharToWideChar('Y')
            else WideChar(aTargetValue^) := FFCharToWideChar('N');
        fftWideString:
          if Assigned(aTargetValue) then
            if srcBoolean^ then FFShStrLToWideStr('Y', aTargetValue, 1)
            else FFShStrLToWideStr('N', aTargetValue, 1);
        else Result := DBIERR_INVALIDFLDXFORM;
      end;
    end;

    fftChar: begin
      case aTargetType of
        fftChar:
          if Assigned(aTargetValue) then
            Char(aTargetValue^) := Char(aSourceValue^);
        fftShortString, fftShortAnsiStr:
          if Assigned(aTargetValue) then
            TffShStr(aTargetValue^) := Char(aSourceValue^);
        fftNullString, fftNullAnsiStr:
          if Assigned(aTargetValue) then
            FFStrPCopy(aTargetValue, Char(aSourceValue^));
        fftWideChar:
          if Assigned(aTargetValue) then
            WideChar(aTargetValue^) := FFCharToWideChar(Char(aSourceValue^));
        fftWideString:
          if Assigned(aTargetValue) then
            FFShStrLToWideStr(Char(aSourceValue^), aTargetValue, 1);
        fftBLOB..ffcLastBLOBType: ;
        { Validate only; do not actually move BLOB data around. }
        else Result := DBIERR_INVALIDFLDXFORM;
      end;
    end;

    fftWideChar: begin
      case aTargetType of
        fftChar:
          if Assigned(aTargetValue) then
            Char(aTargetValue^) := FFWideCharToChar(WideChar(aSourceValue^));
        fftShortString, fftShortAnsiStr:
          if Assigned(aTargetValue) then
            TffShStr(aTargetValue^) := FFWideCharToChar(WideChar(aSourceValue^));
        fftNullString, fftNullAnsiStr:
          if Assigned(aTargetValue) then
            FFStrPCopy(aTargetValue, FFWideCharToChar(WideChar(aSourceValue^)));
        fftWideChar:
          if Assigned(aTargetValue) then
            WideChar(aTargetValue^) := WideChar(aSourceValue^);
        fftWideString:
          if Assigned(aTargetValue) then begin
            PWideChar(aTargetValue)^ := WideChar(aSourceValue^);
            PWideChar(LongInt(aTargetValue) + SizeOf(WideChar))^ := WideChar(#0);
          end;
        fftBLOB..ffcLastBLOBType: ;
        { Validate only; do not actually move BLOB data around. }
        else Result := DBIERR_INVALIDFLDXFORM;
      end;
    end;

    fftByte: begin
      case aTargetType of
        fftByte:
          if Assigned(aTargetValue) then
            Byte(aTargetValue^) := Byte(aSourceValue^);
        fftWord16, fftInt16:
          if Assigned(aTargetValue) then
            TffWord16(aTargetValue^) := Byte(aSourceValue^);
        fftWord32, fftInt32, fftAutoInc:
          if Assigned(aTargetValue) then
            TffWord32(aTargetValue^) := Byte(aSourceValue^);
        fftSingle:
          if Assigned(aTargetValue) then
            Single(aTargetValue^) := Byte(aSourceValue^);
        fftDouble:
          if Assigned(aTargetValue) then
            Double(aTargetValue^) := Byte(aSourceValue^);
        fftExtended:
          if Assigned(aTargetValue) then
            Extended(aTargetValue^) := Byte(aSourceValue^);
        fftComp:
          if Assigned(aTargetValue) then
            Comp(aTargetValue^) := Byte(aSourceValue^);
        fftCurrency:
          if Assigned(aTargetValue) then begin
            Comp(aTargetValue^) := Byte(aSourceValue^);
            Comp(aTargetValue^) := Comp(aTargetValue^) * 10000.0;
          end;
        {Begin !!.10}
        fftShortString, fftShortAnsiStr:
          if Assigned(aTargetValue) then begin
            WorkString := IntToStr(Byte(aSourceValue^));
            if Length(WorkString)>aTargetLength-1 then
              Result := DBIERR_INVALIDFLDXFORM
            else
              TffShStr(aTargetValue^) := WorkString;
          end;
        fftNullString, fftNullAnsiStr:
          if Assigned(aTargetValue) then begin
            WorkString := IntToStr(Byte(aSourceValue^));
            if Length(WorkString)>aTargetLength-1 then
              Result := DBIERR_INVALIDFLDXFORM
            else
              FFStrPCopy(aTargetValue, WorkString);
          end;
        fftWideString:
          if Assigned(aTargetValue) then begin
            { Note: the length of a "wide" field is the number of bytes
              it occupies, not the number of wide chars it will hold. }
            WorkString := IntToStr(Byte(aSourceValue^));
            if Length(WorkString)>(aTargetLength div SizeOf(WideChar))-1 then
              Result := DBIERR_INVALIDFLDXFORM
            else
              FFShStrLToWideStr(WorkString, aTargetValue, Length(WorkString));
          end;
        {End !!.10}
        else Result := DBIERR_INVALIDFLDXFORM;
      end;
    end;

    fftWord16: begin
      case aTargetType of
        fftWord16:
          if Assigned(aTargetValue) then
            TffWord16(aTargetValue^) := TffWord16(aSourceValue^);
        fftWord32, fftInt32, fftAutoInc:
          if Assigned(aTargetValue) then
            TffWord32(aTargetValue^) := TffWord16(aSourceValue^);
        fftSingle:
          if Assigned(aTargetValue) then
            Single(aTargetValue^) := TffWord16(aSourceValue^);
        fftDouble:
          if Assigned(aTargetValue) then
            Double(aTargetValue^) := TffWord16(aSourceValue^);
        fftExtended:
          if Assigned(aTargetValue) then
            Extended(aTargetValue^) := TffWord16(aSourceValue^);
        fftComp:
          if Assigned(aTargetValue) then
            Comp(aTargetValue^) := TffWord16(aSourceValue^);
        fftCurrency:
          if Assigned(aTargetValue) then begin
            Comp(aTargetValue^) := TffWord16(aSourceValue^);
            Comp(aTargetValue^) := Comp(aTargetValue^) * 10000.0;
          end;
        {Begin !!.10}
        fftShortString, fftShortAnsiStr:
          if Assigned(aTargetValue) then begin
            WorkString := IntToStr(TffWord16(aSourceValue^));
            if Length(WorkString)>aTargetLength-1 then
              Result := DBIERR_INVALIDFLDXFORM
            else
              TffShStr(aTargetValue^) := WorkString;
          end;
        fftNullString, fftNullAnsiStr:
          if Assigned(aTargetValue) then begin
            WorkString := IntToStr(TffWord16(aSourceValue^));
            if Length(WorkString)>aTargetLength-1 then
              Result := DBIERR_INVALIDFLDXFORM
            else
              FFStrPCopy(aTargetValue, WorkString);
          end;
        fftWideString:
          if Assigned(aTargetValue) then begin
            { Note: the length of a "wide" field is the number of bytes
              it occupies, not the number of wide chars it will hold. }
            WorkString := IntToStr(TffWord16(aSourceValue^));
            if Length(WorkString)>(aTargetLength div SizeOf(WideChar))-1 then
              Result := DBIERR_INVALIDFLDXFORM
            else
              FFShStrLToWideStr(WorkString, aTargetValue, Length(WorkString));
          end;
        {End !!.10}
        else Result := DBIERR_INVALIDFLDXFORM;
      end;
    end;

    fftWord32,
    fftAutoInc: begin
      case aTargetType of
        fftWord32,
        fftAutoInc:
          if Assigned(aTargetValue) then
            TffWord32(aTargetValue^) := TffWord32(aSourceValue^);
        fftSingle:
          if Assigned(aTargetValue) then
            Single(aTargetValue^) := TffWord32(aSourceValue^);
        fftDouble:
          if Assigned(aTargetValue) then
            Double(aTargetValue^) := TffWord32(aSourceValue^);
        fftExtended:
          if Assigned(aTargetValue) then
            Extended(aTargetValue^) := TffWord32(aSourceValue^);
        fftComp:
          if Assigned(aTargetValue) then
            Comp(aTargetValue^) := TffWord32(aSourceValue^);
        fftCurrency:
          if Assigned(aTargetValue) then begin
            Comp(aTargetValue^) := TffWord32(aSourceValue^);
            Comp(aTargetValue^) := Comp(aTargetValue^) * 10000.0;
          end;
        {Begin !!.10}
        fftShortString, fftShortAnsiStr:
          if Assigned(aTargetValue) then begin
            WorkString := IntToStr(TffWord32(aSourceValue^));
            if Length(WorkString)>aTargetLength-1 then
              Result := DBIERR_INVALIDFLDXFORM
            else
              TffShStr(aTargetValue^) := WorkString;
          end;
        fftNullString, fftNullAnsiStr:
          if Assigned(aTargetValue) then begin
            WorkString := IntToStr(TffWord32(aSourceValue^));
            if Length(WorkString)>aTargetLength-1 then
              Result := DBIERR_INVALIDFLDXFORM
            else
              FFStrPCopy(aTargetValue, WorkString);
          end;
        fftWideString:
          if Assigned(aTargetValue) then begin
            { Note: the length of a "wide" field is the number of bytes
              it occupies, not the number of wide chars it will hold. }
            WorkString := IntToStr(TffWord32(aSourceValue^));
            if Length(WorkString)>(aTargetLength div SizeOf(WideChar))-1 then
              Result := DBIERR_INVALIDFLDXFORM
            else
              FFShStrLToWideStr(WorkString, aTargetValue, Length(WorkString));
          end;
        {End !!.10}
        else Result := DBIERR_INVALIDFLDXFORM;
      end;
    end;

    fftInt8: begin
      case aTargetType of
        fftInt8:
          if Assigned(aTargetValue) then
            ShortInt(aTargetValue^) := ShortInt(aSourceValue^);
        fftInt16:
          if Assigned(aTargetValue) then
            SmallInt(aTargetValue^) := ShortInt(aSourceValue^);
        fftInt32:
          if Assigned(aTargetValue) then
            LongInt(aTargetValue^) := ShortInt(aSourceValue^);
        {Begin !!.10}
        fftWord32, fftAutoInc:
          if Assigned(aTargetValue) then begin
            if ShortInt(aSourceValue^)<0 then
              Result := DBIERR_INVALIDFLDXFORM
            else
              TffWord32(aTargetValue^) := ShortInt(aSourceValue^);
          end;
        {End !!.10}
        fftSingle:
          if Assigned(aTargetValue) then
            Single(aTargetValue^) := ShortInt(aSourceValue^);
        fftDouble:
          if Assigned(aTargetValue) then
            Double(aTargetValue^) := ShortInt(aSourceValue^);
        fftExtended:
          if Assigned(aTargetValue) then
            Extended(aTargetValue^) := ShortInt(aSourceValue^);
        fftComp:
          if Assigned(aTargetValue) then
            Comp(aTargetValue^) := ShortInt(aSourceValue^);
        fftCurrency:
          if Assigned(aTargetValue) then begin
            Comp(aTargetValue^) := ShortInt(aSourceValue^);
            Comp(aTargetValue^) := Comp(aTargetValue^) * 10000.0;
          end;
        {Begin !!.10}
        fftShortString, fftShortAnsiStr:
          if Assigned(aTargetValue) then begin
            WorkString := IntToStr(ShortInt(aSourceValue^));
            if Length(WorkString)>aTargetLength-1 then
              Result := DBIERR_INVALIDFLDXFORM
            else
              TffShStr(aTargetValue^) := WorkString;
          end;
        fftNullString, fftNullAnsiStr:
          if Assigned(aTargetValue) then begin
            WorkString := IntToStr(ShortInt(aSourceValue^));
            if Length(WorkString)>aTargetLength-1 then
              Result := DBIERR_INVALIDFLDXFORM
            else
              FFStrPCopy(aTargetValue, WorkString);
          end;
        fftWideString:
          if Assigned(aTargetValue) then begin
            { Note: the length of a "wide" field is the number of bytes
              it occupies, not the number of wide chars it will hold. }
            WorkString := IntToStr(ShortInt(aSourceValue^));
            if Length(WorkString)>(aTargetLength div SizeOf(WideChar))-1 then
              Result := DBIERR_INVALIDFLDXFORM
            else
              FFShStrLToWideStr(WorkString, aTargetValue, Length(WorkString));
          end;
        {End !!.10}
        else Result := DBIERR_INVALIDFLDXFORM;
      end;
    end;

    fftInt16: begin
      case aTargetType of
        fftInt16:
          if Assigned(aTargetValue) then
            SmallInt(aTargetValue^) := SmallInt(aSourceValue^);
        fftInt32:
          if Assigned(aTargetValue) then
            LongInt(aTargetValue^) := SmallInt(aSourceValue^);
        {Begin !!.10}
        fftWord32, fftAutoInc:
          if Assigned(aTargetValue) then begin
            if SmallInt(aSourceValue^)<0 then
              Result := DBIERR_INVALIDFLDXFORM
            else
              TffWord32(aTargetValue^) := SmallInt(aSourceValue^);
          end;
        {End !!.10}
        fftSingle:
          if Assigned(aTargetValue) then
            Single(aTargetValue^) := SmallInt(aSourceValue^);
        fftDouble:
          if Assigned(aTargetValue) then
            Double(aTargetValue^) := SmallInt(aSourceValue^);
        fftExtended:
          if Assigned(aTargetValue) then
            Extended(aTargetValue^) := SmallInt(aSourceValue^);
        fftComp:
          if Assigned(aTargetValue) then
            Comp(aTargetValue^) := SmallInt(aSourceValue^);
        fftCurrency:
          if Assigned(aTargetValue) then begin
            Comp(aTargetValue^) := SmallInt(aSourceValue^);
            Comp(aTargetValue^) := Comp(aTargetValue^) * 10000.0;
          end;
        {Begin !!.10}
        fftShortString, fftShortAnsiStr:
          if Assigned(aTargetValue) then begin
            WorkString := IntToStr(SmallInt(aSourceValue^));
            if Length(WorkString)>aTargetLength-1 then
              Result := DBIERR_INVALIDFLDXFORM
            else
              TffShStr(aTargetValue^) := WorkString;
          end;
        fftNullString, fftNullAnsiStr:
          if Assigned(aTargetValue) then begin
            WorkString := IntToStr(SmallInt(aSourceValue^));
            if Length(WorkString)>aTargetLength-1 then
              Result := DBIERR_INVALIDFLDXFORM
            else
              FFStrPCopy(aTargetValue, WorkString);
          end;
        fftWideString:
          if Assigned(aTargetValue) then begin
            { Note: the length of a "wide" field is the number of bytes
              it occupies, not the number of wide chars it will hold. }
            WorkString := IntToStr(SmallInt(aSourceValue^));
            if Length(WorkString)>(aTargetLength div SizeOf(WideChar))-1 then
              Result := DBIERR_INVALIDFLDXFORM
            else
              FFShStrLToWideStr(WorkString, aTargetValue, Length(WorkString));
          end;
        {End !!.10}
        else Result := DBIERR_INVALIDFLDXFORM;
      end;
    end;

    fftInt32: begin
      case aTargetType of
        fftInt32:
          if Assigned(aTargetValue) then
            LongInt(aTargetValue^) := LongInt(aSourceValue^);
        {Begin !!.10}
        fftWord32, fftAutoInc:
          if Assigned(aTargetValue) then begin
            if LongInt(aSourceValue^)<0 then
              Result := DBIERR_INVALIDFLDXFORM
            else
              TffWord32(aTargetValue^) := LongInt(aSourceValue^);
          end;
        {End !!.10}
        fftSingle:
          if Assigned(aTargetValue) then
            Single(aTargetValue^) := LongInt(aSourceValue^);
        fftDouble:
          if Assigned(aTargetValue) then
            Double(aTargetValue^) := LongInt(aSourceValue^);
        fftExtended:
          if Assigned(aTargetValue) then
            Extended(aTargetValue^) := LongInt(aSourceValue^);
        fftComp:
          if Assigned(aTargetValue) then
            Comp(aTargetValue^) := LongInt(aSourceValue^);
        fftCurrency:
          if Assigned(aTargetValue) then begin
            Comp(aTargetValue^) := LongInt(aSourceValue^);
            Comp(aTargetValue^) := Comp(aTargetValue^) * 10000.0;
          end;
        {Begin !!.10}
        fftShortString, fftShortAnsiStr:
          if Assigned(aTargetValue) then begin
            WorkString := IntToStr(LongInt(aSourceValue^));
            if Length(WorkString)>aTargetLength-1 then
              Result := DBIERR_INVALIDFLDXFORM
            else
              TffShStr(aTargetValue^) := WorkString;
          end;
        fftNullString, fftNullAnsiStr:
          if Assigned(aTargetValue) then begin
            WorkString := IntToStr(LongInt(aSourceValue^));
            if Length(WorkString)>aTargetLength-1 then
              Result := DBIERR_INVALIDFLDXFORM
            else
              FFStrPCopy(aTargetValue, WorkString);
          end;
        fftWideString:
          if Assigned(aTargetValue) then begin
            { Note: the length of a "wide" field is the number of bytes
              it occupies, not the number of wide chars it will hold. }
            WorkString := IntToStr(LongInt(aSourceValue^));
            if Length(WorkString)>(aTargetLength div SizeOf(WideChar))-1 then
              Result := DBIERR_INVALIDFLDXFORM
            else
              FFShStrLToWideStr(WorkString, aTargetValue, Length(WorkString));
          end;
        {End !!.10}
        else Result := DBIERR_INVALIDFLDXFORM;
      end;
    end;

    fftSingle: begin
      case aTargetType of
        fftSingle:
          if Assigned(aTargetValue) then
            Single(aTargetValue^) := Single(aSourceValue^);
        fftDouble:
          if Assigned(aTargetValue) then
            Double(aTargetValue^) := Single(aSourceValue^);
        fftExtended:
          if Assigned(aTargetValue) then
            Extended(aTargetValue^) := Single(aSourceValue^);
        fftCurrency:
          if Assigned(aTargetValue) then begin
            Comp(aTargetValue^) := Single(aSourceValue^) * 10000.0;   {!!.10}
//            Comp(aTargetValue^) := Comp(aTargetValue^) * 10000.0;
          end;
        else Result := DBIERR_INVALIDFLDXFORM;
      end;
    end;

    fftDouble: begin
      case aTargetType of
        fftDouble:
          if Assigned(aTargetValue) then
            Double(aTargetValue^) := Double(aSourceValue^);
        fftExtended:
          if Assigned(aTargetValue) then
            Extended(aTargetValue^) := Double(aSourceValue^);
        fftCurrency:
          if Assigned(aTargetValue) then begin
            Comp(aTargetValue^) := Double(aSourceValue^) * 10000.0;   {!!.10}
//            Comp(aTargetValue^) := Comp(aTargetValue^) * 10000.0;
          end;
        else Result := DBIERR_INVALIDFLDXFORM;
      end;
    end;

    fftExtended: begin
      case aTargetType of
        fftExtended:
          if Assigned(aTargetValue) then
            Extended(aTargetValue^) := Extended(aSourceValue^);
        fftCurrency:
          if Assigned(aTargetValue) then begin
            Comp(aTargetValue^) := Extended(aSourceValue^);
            Comp(aTargetValue^) := Comp(aTargetValue^) * 10000.0;
          end;
        else Result := DBIERR_INVALIDFLDXFORM;
      end;
    end;

    fftComp:
      case aTargetType of
        fftComp:
          if Assigned(aTargetValue) then
            Comp(aTargetValue^) := Comp(aSourceValue^);
        else Result := DBIERR_INVALIDFLDXFORM;
      end;

    fftCurrency: begin
      case aTargetType of
        fftCurrency:
          if Assigned(aTargetValue) then
            Comp(aTargetValue^) := Comp(aSourceValue^);
        {Begin !!.10}
        fftSingle:
          if Assigned(aTargetValue) then begin
            Single(aTargetValue^) := Comp(aSourceValue^);
            Single(aTargetValue^) := Single(aTargetValue^) / 10000.0;
          end;
        fftDouble:
          if Assigned(aTargetValue) then begin
            Double(aTargetValue^) := Comp(aSourceValue^);
            Double(aTargetValue^) := Double(aTargetValue^) / 10000.0;
          end;
        {End !!.10}
        fftExtended:
          if Assigned(aTargetValue) then begin
            Extended(aTargetValue^) := Comp(aSourceValue^);
            Extended(aTargetValue^) := Extended(aTargetValue^) / 10000.0;
          end;
        else Result := DBIERR_INVALIDFLDXFORM;
      end;
    end;

    fftStDate: begin
      case aTargetType of
        fftStDate:
          if Assigned(aTargetValue) then
            LongInt(aTargetValue^) := LongInt(aSourceValue^);
        fftDateTime:
          if Assigned(aTargetValue) then

            TDateTime(aTargetValue^) :=
              StDateToDateTime(LongInt(aSourceValue^))
              + 693594.0;  {TDateTime's are stored as Delphi 1 values}
        else Result := DBIERR_INVALIDFLDXFORM;
      end;
    end;

    fftStTime: begin
      case aTargetType of
        fftStTime:
          if Assigned(aTargetValue) then
            LongInt(aTargetValue^) := LongInt(aSourceValue^);
        fftDateTime:
          if Assigned(aTargetValue) then
            TDateTime(aTargetValue^) := StTimeToDateTime(LongInt(aSourceValue^));
        else Result := DBIERR_INVALIDFLDXFORM;
      end;
    end;

    fftDateTime: begin
      case aTargetType of
        fftDateTime:
          if Assigned(aTargetValue) then
            TDateTime(aTargetValue^) := TDateTime(aSourceValue^);
        fftStDate:
          if Assigned(aTargetValue) then
            LongInt(aTargetValue^) := DateTimeToStDate(TDateTime(aSourceValue^)
            - 693594.0);  { TDateTime's are stored as Delphi 1 values }
        fftStTime:
          if Assigned(aTargetValue) then
            LongInt(aTargetValue^) := DateTimeToStTime(TDateTime(aSourceValue^));
        else Result := DBIERR_INVALIDFLDXFORM;
      end;
    end;

    fftBLOB..ffcLastBLOBType:
      if not (aTargetType in [fftBLOB..ffcLastBLOBType]) then
        Result := DBIERR_INVALIDFLDXFORM;
      { Validate only; do not actually move BLOB data around. }

    fftByteArray: begin
      case aTargetType of
        fftByteArray:
          if Assigned(aTargetValue) then
            Move(aSourceValue^, aTargetValue^, MinLength);
        fftBLOB..ffcLastBLOBType: ;
          { Validate only; do not move BLOB data around. }
        else Result := DBIERR_INVALIDFLDXFORM;
      end;
    end;

    fftShortString, fftShortAnsiStr: begin
      case aTargetType of
        fftChar:
          if Assigned(aTargetValue) then
            Char(aTargetValue^) := TffShStr(aSourceValue^)[1];
        fftShortString, fftShortAnsiStr:
          if Assigned(aTargetValue) then
            TffShStr(aTargetValue^) := Copy(TffShStr(aSourceValue^), 1, MinLength - 1);
        fftNullString, fftNullAnsiStr:
          if Assigned(aTargetValue) then
            FFStrPCopy(aTargetValue, Copy(TffShStr(aSourceValue^), 1, MinLength - 1));
        fftWideChar:
          if Assigned(aTargetValue) then
            WideChar(aTargetValue^) := FFCharToWideChar(TffShStr(aSourceValue^)[1]);
        fftWideString:
          if Assigned(aTargetValue) then begin
            { Note: the length of a "wide" field is the number of bytes
              it occupies, not the number of wide chars it will hold. }
            MinLength := FFMinI(aSourceLength - 1, (aTargetLength div SizeOf(WideChar)) - 1);
            FFShStrLToWideStr(TffShStr(aSourceValue^), aTargetValue, MinLength);
          end;
        fftBLOB..ffcLastBLOBType: ;
        { Validate only; do not actually move BLOB data around. }

        {Begin !!.13}
        fftByte:
          if Assigned(aTargetValue) then begin
            Val(FFRemoveThousandSeparator(TffShStr(aSourceValue^)), intRes, aCode);
            if (aCode=0) and (intRes>=Low(Byte)) and (intRes<=High(Byte)) then
              Byte(aTargetValue^) := intRes
            else
              Result := DBIERR_INVALIDFLDXFORM;
          end;
        fftWord16:
          if Assigned(aTargetValue) then begin
            Val(FFRemoveThousandSeparator(TffShStr(aSourceValue^)), wordRes, aCode);
            if (aCode=0) then
              TffWord16(aTargetValue^) := wordRes
            else
              Result := DBIERR_INVALIDFLDXFORM;
          end;
        fftInt16:
          if Assigned(aTargetValue) then begin
            Val(FFRemoveThousandSeparator(TffShStr(aSourceValue^)), intRes, aCode);
            if (aCode=0) and (intRes>=Low(SmallInt)) and (intRes<=High(SmallInt)) then
              Smallint(aTargetValue^) := intRes
            else
              Result := DBIERR_INVALIDFLDXFORM;
          end;
        fftWord32, fftAutoInc:
          if Assigned(aTargetValue) then begin
            Val(FFRemoveThousandSeparator(TffShStr(aSourceValue^)), wordRes, aCode);
            if (aCode=0) then
              TffWord32(aTargetValue^) := wordRes
            else
              Result := DBIERR_INVALIDFLDXFORM;
          end;
        fftInt32:
          if Assigned(aTargetValue) then begin
            Val(FFRemoveThousandSeparator(TffShStr(aSourceValue^)), intRes, aCode);
            if (aCode=0) then
              Integer(aTargetValue^) := intRes
            else
              Result := DBIERR_INVALIDFLDXFORM;
          end;
        fftSingle:
          if Assigned(aTargetValue) then
            Single(aTargetValue^) := StrToFloat(FFRemoveThousandSeparator(TffShStr(aSourceValue^)));
        fftDouble:
          if Assigned(aTargetValue) then
            Double(aTargetValue^) := StrToFloat(FFRemoveThousandSeparator(TffShStr(aSourceValue^)));
        fftExtended:
          if Assigned(aTargetValue) then
            Extended(aTargetValue^) := StrToFloat(FFRemoveThousandSeparator(TffShStr(aSourceValue^)));
        fftComp:
          if Assigned(aTargetValue) then
            Comp(aTargetValue^) := StrToFloat(FFRemoveThousandSeparator(TffShStr(aSourceValue^)));
        fftCurrency:
          if Assigned(aTargetValue) then begin
            Comp(aTargetValue^) := StrToFloat(FFRemoveThousandSeparator(TffShStr(aSourceValue^)));
            Comp(aTargetValue^) := Comp(aTargetValue^) * 10000.0;
          end;
        {End !!.13}

        else Result := DBIERR_INVALIDFLDXFORM;
      end;
    end;

    fftNullString, fftNullAnsiStr: begin
      case aTargetType of
        fftChar:
          if Assigned(aTargetValue) then
            Char(aTargetValue^) := FFStrPas(aSourceValue)[1];
        fftShortString, fftShortAnsiStr:
          if Assigned(aTargetValue) then
            TffShStr(aTargetValue^) := Copy(FFStrPas(aSourceValue), 1, MinLength - 1);
        fftNullString, fftNullAnsiStr:
          if Assigned(aTargetValue) then
            StrLCopy(aTargetValue, aSourceValue, MinLength - 1);
        fftWideChar:
          if Assigned(aTargetValue) then
            WideChar(aTargetValue^) := FFCharToWideChar(Char(aSourceValue^));
        fftWideString:
          if Assigned(aTargetValue) then begin
            { Note: the length of a "wide" field is the number of bytes
              it occupies, not the number of wide chars it will hold. }
            MinLength := FFMinI(aSourceLength - 1, (aTargetLength div SizeOf(WideChar)) - 1);
            FFNullStrLToWideStr(aSourceValue, aTargetValue, MinLength);
          end;
        fftBLOB..ffcLastBLOBType: ;
        { Validate only; do not actually move BLOB data around. }

        {Begin !!.13}
        fftByte:
          if Assigned(aTargetValue) then begin
            Val(FFRemoveThousandSeparator(PChar(aSourceValue)), intRes, aCode);
            if (aCode=0) and (intRes>=Low(Byte)) and (intRes<=High(Byte)) then
              Byte(aTargetValue^) := intRes
            else
              Result := DBIERR_INVALIDFLDXFORM;
          end;
        fftWord16:
          if Assigned(aTargetValue) then begin
            Val(FFRemoveThousandSeparator(PChar(aSourceValue)), wordRes, aCode);
            if (aCode=0) then
              TffWord16(aTargetValue^) := wordRes
            else
              Result := DBIERR_INVALIDFLDXFORM;
          end;
        fftInt16:
          if Assigned(aTargetValue) then begin
            Val(FFRemoveThousandSeparator(PChar(aSourceValue)), intRes, aCode);
            if (aCode=0) and (intRes>=Low(SmallInt)) and (intRes<=High(SmallInt)) then
              Smallint(aTargetValue^) := intRes
            else
              Result := DBIERR_INVALIDFLDXFORM;
          end;
        fftWord32, fftAutoInc:
          if Assigned(aTargetValue) then begin
            Val(FFRemoveThousandSeparator(PChar(aSourceValue)), wordRes, aCode);
            if (aCode=0) then
              TffWord32(aTargetValue^) := wordRes
            else
              Result := DBIERR_INVALIDFLDXFORM;
          end;
        fftInt32:
          if Assigned(aTargetValue) then begin
            Val(FFRemoveThousandSeparator(PChar(aSourceValue)), intRes, aCode);
            if (aCode=0) then
              Integer(aTargetValue^) := intRes
            else
              Result := DBIERR_INVALIDFLDXFORM;
          end;
        fftSingle:
          if Assigned(aTargetValue) then
            Single(aTargetValue^) := StrToFloat(FFRemoveThousandSeparator(PChar(aSourceValue)));
        fftDouble:
          if Assigned(aTargetValue) then
            Double(aTargetValue^) := StrToFloat(FFRemoveThousandSeparator(PChar(aSourceValue)));
        fftExtended:
          if Assigned(aTargetValue) then
            Extended(aTargetValue^) := StrToFloat(FFRemoveThousandSeparator(PChar(aSourceValue)));
        fftComp:
          if Assigned(aTargetValue) then
            Comp(aTargetValue^) := StrToFloat(FFRemoveThousandSeparator(PChar(aSourceValue)));
        fftCurrency:
          if Assigned(aTargetValue) then begin
            Comp(aTargetValue^) := StrToFloat(FFRemoveThousandSeparator(PChar(aSourceValue)));
            Comp(aTargetValue^) := Comp(aTargetValue^) * 10000.0;
          end;
        {End !!.13}

        else Result := DBIERR_INVALIDFLDXFORM;
      end;
    end;

    fftWideString: begin
      case aTargetType of
        fftChar:
          if Assigned(aTargetValue) then
            Char(aTargetValue^) := FFWideCharToChar(WideChar(aSourceValue^));
        fftShortString, fftShortAnsiStr:
          if Assigned(aTargetValue) then begin
            { Note: the length of a "wide" field is the number of bytes
              it occupies, not the number of wide chars it will hold. }
            MinLength := FFMinI(aTargetLength - 1, (aSourceLength div SizeOf(WideChar)) - 1);
            TffShStr(aTargetValue^) := FFWideStrLToShStr(aSourceValue, MinLength);
          end;
        fftNullString, fftNullAnsiStr:
          if Assigned(aTargetValue) then begin
            { Note: the length of a "wide" field is the number of bytes
              it occupies, not the number of wide chars it will hold. }
            MinLength := FFMinI(aTargetLength - 1, (aSourceLength div SizeOf(WideChar)) - 1);
            FFWideStrLToNullStr(aSourceValue, aTargetValue, MinLength);
          end;
        fftWideChar:
          if Assigned(aTargetValue) then
            WideChar(aTargetValue^) := WideChar(aSourceValue^);
        fftWideString:
          if Assigned(aTargetValue) then
            FFWideStrLToWideStr(aSourceValue, aTargetValue, FFMinI(aSourceLength, aTargetLength) - 1);
        fftBLOB..ffcLastBLOBType: ;
        { Validate only; do not actually move BLOB data around. }
        else Result := DBIERR_INVALIDFLDXFORM;
      end;
    end;
    else Result := DBIERR_INVALIDFLDXFORM;
  end;
end;


end.

