unit cyDERDBUtils;

{ cyDERDBUtils
  Description: Document elements recognition with DB handling utilities

    $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
    $  €€€ Accept any PAYPAL DONATION $$$  €
    $      to: mauricio_box@yahoo.com      €
    €€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€

    * ***** BEGIN LICENSE BLOCK *****
    *
    * Version: MPL 1.1
    *
    * The contents of this file are subject to the Mozilla Public License Version
    * 1.1 (the "License"); you may not use this file except in compliance with the
    * License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
    *
    * Software distributed under the License is distributed on an "AS IS" basis,
    * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
    * the specific language governing rights and limitations under the License.
    *
    * The Initial Developer of the Original Code is Mauricio
    * (https://sourceforge.net/projects/tcycomponents/).
    *
    * Donations: see Donation section on Description.txt
    *
    * Alternatively, the contents of this file may be used under the terms of
    * either the GNU General Public License Version 2 or later (the "GPL"), or the
    * GNU Lesser General Public License Version 2.1 or later (the "LGPL"), in which
    * case the provisions of the GPL or the LGPL are applicable instead of those
    * above. If you wish to allow use of your version of this file only under the
    * terms of either the GPL or the LGPL, and not to allow others to use your
    * version of this file under the terms of the MPL, indicate your decision by
    * deleting the provisions above and replace them with the notice and other
    * provisions required by the LGPL or the GPL. If you do not delete the
    * provisions above, a recipient may use your version of this file under the
    * terms of any one of the MPL, the GPL or the LGPL.
    *
    * ***** END LICENSE BLOCK *****}

interface

uses Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, DB, DBClient, Grids, DBGrids, cyBaseDBGrid, cyDBAdvGrid, StdCtrls, ExtCtrls,
  cyFieldLink, cyStrUtils, cyDERUtils;

function GetValueFromField(SourceField: TField; ResultValueType: TFieldType; const SourceFromCharPos: Integer = 0; const SourceCharCount: Integer = 0; const ApplyMask: String = ''; const ApplyDER: Boolean = false): Variant;

function GetValueFromVariant(Source: Variant; ResultValueType: TFieldType; const SourceFromCharPos: Integer = 0; const SourceCharCount: Integer = 0; const ApplyMask: String = ''; const ApplyDER: Boolean = false): Variant;

implementation

function GetValueFromField(SourceField: TField; ResultValueType: TFieldType; const SourceFromCharPos: Integer = 0; const SourceCharCount: Integer = 0; const ApplyMask: String = ''; const ApplyDER: Boolean = false): Variant;
var
  Source_FromPos, Source_Count: Integer;
  TryInt: Integer;
  TryDate: TDateTime;
  TryFloat: Extended;

  StrValue: String;

  // DER :
  DERStr: DERString;
  Rslt: TElementsType;
  RsltNumbers, RsltInteger, RsltFloat, RsltPercentage, RsltwebSite, RsltWebMail, RsltMoney, RsltDate: String;
begin
  if SourceField.Value = Null then
  begin
    Result := Null;
    Exit;
  end;

  Source_FromPos := SourceFromCharPos;
  Source_Count := SourceCharCount;

  case ResultValueType of
    ftSmallint, ftInteger, ftWord, ftLargeint, ftLongWord, ftShortint, ftByte, ftAutoInc:
      if (Source_FromPos <> 0) or (ApplyMask <> '') or ApplyDER then
      begin
        if Source_FromPos = 0 then Source_FromPos := 1;
        if Source_Count = 0 then Source_Count := Length(SourceField.AsString);

        StrValue := Copy(SourceField.AsString, Source_FromPos, Source_Count);

        if ApplyMask <> '' then
        begin
          if IsMatchMask2(ApplyMask, StrValue)
          then StrValue := MergeMaskToMatchedString2(ApplyMask, StrValue)
          else StrValue := '';
        end;

        if ApplyDER then
        begin
          DERStr := StringToDERCharSet(StrValue, true);
          Rslt := DERExecute(DERStr, RsltNumbers, RsltInteger, RsltFloat, RsltPercentage, RsltwebSite, RsltWebMail, RsltMoney, RsltDate, false, false);

          case Rslt of
            etNumbers: StrValue := RsltNumbers;
            etInteger: StrValue := RsltInteger;
          end;
        end;

        if TryStrToInt(StrValue, TryInt)
        then Result := TryInt
        else Result := 0;
      end
      else
        Result := SourceField.AsInteger;

    ftFloat, ftCurrency, ftExtended, ftFMTBcd:
      begin
        if (Source_FromPos <> 0) or (ApplyMask <> '') or ApplyDER then
        begin
          if Source_FromPos = 0 then Source_FromPos := 1;
          if Source_Count = 0 then Source_Count := Length(SourceField.AsString);

          StrValue := Copy(SourceField.AsString, Source_FromPos, Source_Count);

          if ApplyMask <> '' then
          begin
            if IsMatchMask2(ApplyMask, StrValue)
            then StrValue := MergeMaskToMatchedString2(ApplyMask, StrValue)
            else StrValue := '';
          end;

          if ApplyDER then
          begin
            DERStr := StringToDERCharSet(StrValue, true);
            Rslt := DERExecute(DERStr, RsltNumbers, RsltInteger, RsltFloat, RsltPercentage, RsltwebSite, RsltWebMail, RsltMoney, RsltDate, false, false);

            case Rslt of
              etMoney, etFloat: StrValue := RsltNumbers;
              etInteger: StrValue := RsltInteger;
              etNumbers: StrValue := RsltNumbers;
            end;
          end;

          if TryStrToFloat(StrValue, TryFloat)
          then Result := TryFloat
          else Result := 0;
        end
        else
          Result := SourceField.AsFloat;


        if pos('E', Result) <> 0 then
          Result := 0;
      end;

    ftDate, ftTime, ftDateTime:
      if (Source_FromPos <> 0) or (ApplyMask <> '') or ApplyDER then
      begin
        if Source_FromPos = 0 then Source_FromPos := 1;
        if Source_Count = 0 then Source_Count := Length(SourceField.AsString);

        StrValue := Copy(SourceField.AsString, Source_FromPos, Source_Count);

        if ApplyMask <> '' then
        begin
          if IsMatchMask2(ApplyMask, StrValue)
          then StrValue := MergeMaskToMatchedString2(ApplyMask, StrValue)
          else StrValue := '';
        end;

        if ApplyDER and (StrValue <> '') then
        begin
          DERStr := StringToDERCharSet(StrValue, true);

          // 2017-07-05 Improve date detection :
          if cyStrUtils.String_IsNumbers(DERStr) then
            if Copy(DERStr, 5, 1) = '2' then           // First year char is '2' ?
            begin
              // Its seems to be day + month + year !
              Insert('-', DERStr, 5);
              Insert('-', DERStr, 3);
            end
            else begin
              // Its seems to be Year + Month + day !
              Insert('-', DERStr, 7);
              Insert('-', DERStr, 5);
            end;

          Rslt := DERExecute(DERStr, RsltNumbers, RsltInteger, RsltFloat, RsltPercentage, RsltwebSite, RsltWebMail, RsltMoney, RsltDate, false, false);

          if Rslt = etDate then
            StrValue := DateToStr( StrToInt(RsltDate) );
        end;

        if TryStrToDate(StrValue, TryDate)
        then Result := TryDate
        else Result := Null;            // 2017-10-19      Result := 0;
      end
      else
        Result := SourceField.AsDateTime;

    ftBoolean:
      Result := SourceField.Value;  // Passed 'Yes' (english), 'Sim' (Portuguese) !

    ftBlob:     // ftBlob value type is a variant array of byte that cannot be compared with same type (null or not null), so we export as string
    begin
      if SourceField.AsString = ''
      then Result := ''                    // Null
      else Result := SourceField.AsString; //  SourceField.Value;
    end;

    else   // * Handle as String *
      if (Source_FromPos <> 0) or (ApplyMask <> '') or ApplyDER then
      begin
        if Source_FromPos = 0 then Source_FromPos := 1;
        if Source_Count = 0 then Source_Count := Length(SourceField.AsString);

        StrValue := Copy(SourceField.AsString, Source_FromPos, Source_Count);

        if ApplyMask <> '' then
        begin
          if IsMatchMask2(ApplyMask, StrValue)
          then StrValue := MergeMaskToMatchedString2(ApplyMask, StrValue)
          else StrValue := '';
        end;

        if ApplyDER then
        begin
          DERStr := StringToDERCharSet(StrValue, true);
          Rslt := DERExecute(DERStr, RsltNumbers, RsltInteger, RsltFloat, RsltPercentage, RsltwebSite, RsltWebMail, RsltMoney, RsltDate, false, false);

          case Rslt of
            etText:       Result := StrValue;
            etNumbers:    Result := RsltNumbers;
            etInteger:    Result := RsltInteger;
            etFloat:      Result := RsltFloat;
            etPercentage: Result := RsltPercentage;
            etwebSite:    Result := RsltwebSite;
            etWebMail:    Result := RsltWebMail;
            etMoney:      Result := RsltMoney;
            etDate:       Result := DateToStr( StrToInt(RsltDate) );
            else
               Result := StrValue;
          end;
        end
        else
          Result := StrValue;

      end
      else
        Result := SourceField.AsString;
  end;
end;

function GetValueFromVariant(Source: Variant; ResultValueType: TFieldType; const SourceFromCharPos: Integer = 0; const SourceCharCount: Integer = 0; const ApplyMask: String = ''; const ApplyDER: Boolean = false): Variant;
var
  Source_FromPos, Source_Count: Integer;
  TryInt: Integer;
  TryDate: TDateTime;
  TryFloat: Extended;

  StrValue: String;

  // DER :
  DERStr: DERString;
  Rslt: TElementsType;
  RsltNumbers, RsltInteger, RsltFloat, RsltPercentage, RsltwebSite, RsltWebMail, RsltMoney, RsltDate: String;
begin
  if Source = Null then
  begin
    Result := Null;
    Exit;
  end;

  Source_FromPos := SourceFromCharPos;
  Source_Count := SourceCharCount;

  case ResultValueType of
    ftSmallint, ftInteger, ftWord, ftLargeint, ftLongWord, ftShortint, ftByte, ftAutoInc:
      if (Source_FromPos <> 0) or (ApplyMask <> '') or ApplyDER then
      begin
        if Source_FromPos = 0 then Source_FromPos := 1;
        if Source_Count = 0 then Source_Count := Length(Source);

        StrValue := Copy(Source, Source_FromPos, Source_Count);

        if ApplyMask <> '' then
        begin
          if IsMatchMask2(ApplyMask, StrValue)
          then StrValue := MergeMaskToMatchedString2(ApplyMask, StrValue)
          else StrValue := '';
        end;

        if ApplyDER then
        begin
          DERStr := StringToDERCharSet(StrValue, true);
          Rslt := DERExecute(DERStr, RsltNumbers, RsltInteger, RsltFloat, RsltPercentage, RsltwebSite, RsltWebMail, RsltMoney, RsltDate, false, false);

          case Rslt of
            etNumbers: StrValue := RsltNumbers;
            etInteger: StrValue := RsltInteger;
          end;
        end;

        // 2017-08-07
        if StrValue = '' then
          Result := Null
        else
          if TryStrToInt(StrValue, TryInt)
          then Result := TryInt
          else Result := 0;
      end
      else
        Result := Source;

    ftFloat, ftCurrency, ftExtended, ftFMTBcd:
      begin
        if (Source_FromPos <> 0) or (ApplyMask <> '') or ApplyDER then
        begin
          if Source_FromPos = 0 then Source_FromPos := 1;
          if Source_Count = 0 then Source_Count := Length(Source);

          StrValue := Copy(Source, Source_FromPos, Source_Count);

          if ApplyMask <> '' then
          begin
            if IsMatchMask2(ApplyMask, StrValue)
            then StrValue := MergeMaskToMatchedString2(ApplyMask, StrValue)
            else StrValue := '';
          end;

          if ApplyDER then
          begin
            DERStr := StringToDERCharSet(StrValue, true);
            Rslt := DERExecute(DERStr, RsltNumbers, RsltInteger, RsltFloat, RsltPercentage, RsltwebSite, RsltWebMail, RsltMoney, RsltDate, false, false);

            case Rslt of
              etMoney, etFloat: StrValue := RsltNumbers;
              etInteger: StrValue := RsltInteger;
              etNumbers: StrValue := RsltNumbers;
            end;
          end;

          // 2017-08-07
          if StrValue = '' then
            Result := Null
          else
            if TryStrToFloat(StrValue, TryFloat)
            then Result := TryFloat
            else Result := 0;
        end
        else
          Result := Source;

        if Result <> Null then
          if pos('E', Result) <> 0 then
            Result := 0;
      end;

    ftDate, ftTime, ftDateTime:
      if (Source_FromPos <> 0) or (ApplyMask <> '') or ApplyDER then
      begin
        if Source_FromPos = 0 then Source_FromPos := 1;
        if Source_Count = 0 then Source_Count := Length(Source);

        StrValue := Copy(Source, Source_FromPos, Source_Count);

        if ApplyMask <> '' then
        begin
          if IsMatchMask2(ApplyMask, StrValue)
          then StrValue := MergeMaskToMatchedString2(ApplyMask, StrValue)
          else StrValue := '';
        end;

        if ApplyDER and (StrValue <> '') then
        begin
          DERStr := StringToDERCharSet(StrValue, true);

          // 2017-07-05 Improve date detection :
          if cyStrUtils.String_IsNumbers(DERStr) then
            if Copy(DERStr, 5, 1) = '2' then           // First year char is '2' ?
            begin
              // Its seems to be day + month + year !
              Insert('-', DERStr, 5);
              Insert('-', DERStr, 3);
            end
            else begin
              // Its seems to be Year + Month + day !
              Insert('-', DERStr, 7);
              Insert('-', DERStr, 5);
            end;

          Rslt := DERExecute(DERStr, RsltNumbers, RsltInteger, RsltFloat, RsltPercentage, RsltwebSite, RsltWebMail, RsltMoney, RsltDate, false, false);

          if Rslt = etDate then
            StrValue := DateToStr( StrToInt(RsltDate) );
        end;

        // 2017-08-07
        if StrValue = '' then
          Result := Null
        else
          if TryStrToDate(StrValue, TryDate)
          then Result := TryDate
          else Result := Null;    // 2017-10-19 ... 0;
      end
      else
        Result := Source;

    ftBoolean:
      Result := Source; // 2016-06-30 Passou 'Sim' !

    ftBlob:     // ftBlob value type is a variant array of byte that cannot be compared with same type (null or not null), so we export as string
      Result := Source;  // RHR TODO: convert into string type

    else   // * Handle as String *
      if (Source_FromPos <> 0) or (ApplyMask <> '') or ApplyDER then
      begin
        if Source_FromPos = 0 then Source_FromPos := 1;
        if Source_Count = 0 then Source_Count := Length(Source);

        StrValue := Copy(Source, Source_FromPos, Source_Count);

        if ApplyMask <> '' then
        begin
          if IsMatchMask2(ApplyMask, StrValue)
          then StrValue := MergeMaskToMatchedString2(ApplyMask, StrValue)
          else StrValue := '';
        end;

        if ApplyDER then
        begin
          DERStr := StringToDERCharSet(StrValue, true);
          Rslt := DERExecute(DERStr, RsltNumbers, RsltInteger, RsltFloat, RsltPercentage, RsltwebSite, RsltWebMail, RsltMoney, RsltDate, false, false);

          case Rslt of
            etText:       Result := StrValue;
            etNumbers:    Result := RsltNumbers;
            etInteger:    Result := RsltInteger;
            etFloat:      Result := RsltFloat;
            etPercentage: Result := RsltPercentage;
            etwebSite:    Result := RsltwebSite;
            etWebMail:    Result := RsltWebMail;
            etMoney:      Result := RsltMoney;
            etDate:       Result := DateToStr( StrToInt(RsltDate) );
            else
               Result := StrValue;
          end;
        end;
      end
      else
        Result := Source;
  end;
end;

end.
