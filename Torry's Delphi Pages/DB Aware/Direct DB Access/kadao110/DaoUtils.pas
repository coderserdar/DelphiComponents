unit DaoUtils;
//******************************************************************************
//                    Delphi Dao Project Version 2.40
//                 Copyright (c) 2000 by Kiril Antonov
//******************************************************************************
// 05.07 2000 - Fixed a very rediculous bug in RemoveNonDigitChars
//              Now it works properly. RemoveNonDigitChars_II removed!
{$I KADaoCommonDirectives.pas}
interface
Uses SysUtils, Db, ComObj, ActiveX {$IFDEF D6UP}, Variants{$ENDIF};


  Function ComposeDateTimeRecord(S:String):TTimeStamp;
  Function ComposeDateTimeVariant(S:String):OleVariant;
  Function RemoveNonDigitChars(DT:String):String;
  Function DaoSizeToBDESize(DaoType:Integer;DaoSize:Integer):Integer;
  Function DaoToBDE(DaoType:Integer):TFieldType;
  Function BDEToDao(BDEType:TFieldType):Integer;
  Function GetBDEFieldTypeNames(BDEType:TFieldType):String;
  Function GetDaoFieldTypeNames(DaoType:Integer):String;
  Function PSafeArrayToOleVariant(PSA: PSafeArray): OleVariant;
  Function OleVariantToPSafeArray(OV: OleVariant):PSafeArray;
  Function BracketField (const FieldName:String) : String;


implementation
Uses
 DAOApi,Windows,Dialogs,Registry;
Const
 LangGarbage='ã.';


Function PSafeArrayToOleVariant(PSA: PSafeArray): OleVariant;
begin
  TVarData(Result).VType  := varArray;
  TVarData(Result).VArray := PVarArray(PSA);
end;


Function OleVariantToPSafeArray(OV: OleVariant):PSafeArray;
begin
  Result := PSafeArray (TVarData(OV).VArray);
end;

Function ComposeDateTimeRecord(S:String):TTimeStamp;
Var
  P      : Integer;
Begin
  Result.Date := 0;
  Result.Time := 0;
  P:=Pos(' ',S);
  if P = 0 Then Exit;
  Result.Date := StrToInt(Copy(S,1,P-1));
  System.Delete(S,1,P);
  Result.Time := StrToInt(S);
End;

Function ComposeDateTimeVariant(S:String):OleVariant;
Var
 DTS    : TTimeStamp;
 P      : Integer;
Begin
 Result := NULL;
 P:=Pos(' ',S);
 if P = 0 Then Exit;
 Try
   DTS.Date := StrToInt(Copy(S,1,P-1));
   System.Delete(S,1,P);
   DTS.Time := StrToInt(S);
   VarClear(Result);
   TVarData(Result).VType := varDate;
   TVarData(Result).vDate:= TimeStampToDateTime(DTS);
 Except
  Result := NULL;
 End;
End;

Function RemoveNonDigitChars(DT:String):String;
Var
  Allow  : String;
  X,L,P  : Integer;
Begin
  Result:=DT;
  L:=Length(Result);
  if L=0 Then Exit;                            
  Allow := '1234567890/';
  Allow:=Allow+DateSeparator;
  Allow:=Allow+TimeSeparator;
  //****************************************************** Language Specific
  P:=Pos(LangGarbage,Result);
  if P > 0 Then Delete(Result,P,2);
  //****************************************************** Language Specific
  L:=Length(Result);
  For X:=1 to L do
      Begin
       if Pos(Result[X],Allow)=0 Then Result[X]:=' ';
      End;
  Repeat
    P:= Pos('  ',Result);
    if P > 0 Then Delete(Result,P,1);
  Until P=0;
  Result:=Trim(Result);
  //***************************** Remove any spaces exept between date and time
  P:=Pos(TimeSeparator,Result);
  While (P > 0) And (Result[P] <> ' ') Do Dec(P);
  if P > 0 Then Result[P]:=#0;
  Repeat
    P:= Pos(' ',Result);
    if P > 0 Then Delete(Result,P,1);
  Until P=0;
  P:= Pos(#0,Result);
  if P > 0 Then Result[P]:=' ';
End;

Function DaoSizeToBDESize(DaoType:Integer;DaoSize:Integer):Integer;
Begin
 Result:=0;
 Case DaoType of
        dbBoolean        : Result := 0;
        dbByte           : Result := 0;
        dbInteger        : Result := 0;
        dbLong           : Result := 0;
        dbCurrency       : Result := 0;
        dbSingle         : Result := 0;
        dbDouble         : Result := 0;
        dbDate           : Result := 0;
        dbBinary         : Result := DaoSize;
        dbText           : Result := DaoSize;
        dbLongBinary     : Result := DaoSize;
        dbMemo           : Result := DaoSize;
        dbGUID           : Result := DaoSize;
        dbBigInt         : Result := 0;
        dbVarBinary      : Result := DaoSize;
        dbChar           : Result := DaoSize;
        dbNumeric        : Result := 0;
        dbDecimal        : Result := 0;
        dbFloat          : Result := 0;
        dbTime           : Result := 0;
        dbTimeStamp      : Result := 0;
        dbAutoIncInteger : Result := 0;

        dbAttachment     : Result := 0;
        dbComplexByte    : Result := 0;
        dbComplexInteger : Result := 0;
        dbComplexLong    : Result := 0;
        dbComplexSingle  : Result := 0;
        dbComplexDouble  : Result := 0;
        dbComplexDecimal : Result := 0;
        dbComplexGUID    : Result := DaoSize;
        dbComplexText    : Result := DaoSize;
 End;
End;

Function DaoToBDE(DaoType:Integer):TFieldType;
Begin
 Result:=ftUnknown;
 Case DaoType of
        dbBoolean        : Result := ftBoolean;
        dbByte           : Result := ftSmallint;
        dbInteger        : Result := ftSmallint;
        dbLong           : Result := ftInteger;
        dbCurrency       : Result := ftCurrency;
        dbSingle         : Result := ftFloat;
        dbDouble         : Result := ftFloat;
        dbDate           : Result := ftDate;
        dbText           : Result := ftString;
        dbLongBinary     : Result := ftBlob;                                
        dbMemo           : Result := ftMemo;
        //********************************************
        dbBinary         : Result := ftBytes; 
        dbGUID           : Result := ftBytes;
        //********************************************
        dbBigInt         : Result := ftInteger;
        dbVarBinary      : Result := ftBlob;
        dbChar           : Result := ftString;
        dbNumeric        : Result := ftFloat;
        dbDecimal        : Result := ftFloat;
        dbFloat          : Result := ftFloat;
        dbTime           : Result := ftTime;
        dbTimeStamp      : Result := ftDateTime;
        dbAutoIncInteger : Result := ftAutoInc;
        //********************************************
        dbAttachment     : Result := ftIDispatch;
        dbComplexByte    : Result := ftIDispatch;
        dbComplexInteger : Result := ftIDispatch;
        dbComplexLong    : Result := ftIDispatch;
        dbComplexSingle  : Result := ftIDispatch;
        dbComplexDouble  : Result := ftIDispatch;
        dbComplexDecimal : Result := ftIDispatch;
        dbComplexGUID    : Result := ftIDispatch;
        dbComplexText    : Result := ftIDispatch;
 End;
End;

Function BDEToDao(BDEType:TFieldType):Integer;
Begin
 Result:=dbUnspecifyed;
 Case BDEType of
        ftString       : Result := dbText;
        ftSmallint     : Result := dbInteger;
        ftInteger      : Result := dbLong;
        ftWord         : Result := dbLong;
        ftBoolean      : Result := dbBoolean;
        ftFloat        : Result := dbSingle;
        ftCurrency     : Result := dbCurrency;
        ftBCD          : Result := dbCurrency;
        ftDate         : Result := dbDate;
        ftTime         : Result := dbDate;
        ftDateTime     : Result := dbDate;
        ftBytes	       : Result := dbGUID;
        ftVarBytes     : Result := dbMemo;
        ftAutoInc      : Result := dbAutoIncInteger;
        ftBlob         : Result := dbLongBinary;
        ftMemo         : Result := dbMemo;
        ftGraphic      : Result := dbLongBinary;
        ftFmtMemo      : Result := dbMemo;
        ftParadoxOle   : Result := dbLongBinary	;
        ftDBaseOle     : Result := dbLongBinary	;
        ftTypedBinary  : Result := dbLongBinary	;
      End;
End;

Function GetBDEFieldTypeNames(BDEType:TFieldType):String;
Begin
 Result:='ftUnknown';
 Case BDEType of
        ftBoolean    : Result := 'ftBoolean';
        ftInteger    : Result := 'ftInteger';
        ftSmallInt   : Result := 'ftSmallInt';
        ftWord       : Result := 'ftWord';
        ftBCD        : Result := 'ftBCD';
        ftCurrency   : Result := 'ftCurrency';
        ftFloat      : Result := 'ftFloat';
        ftDate       : Result := 'ftDate';
        ftBlob       : Result := 'ftBlob';
        ftString     : Result := 'ftString';
        ftMemo       : Result := 'ftMemo';
        ftAutoInc    : Result := 'ftAutoInc';
        ftTime       : Result := 'ftTime';
        ftDateTime   : Result := 'ftDateTime';
 End;
End;

Function GetDaoFieldTypeNames(DaoType:Integer):String;
Begin
 Result:='dbUnknown';
 Case DaoType of
        dbBoolean    : Result := 'dbBoolean';
        dbByte       : Result := 'dbByte';
        dbInteger    : Result := 'dbInteger';
        dbLong       : Result := 'dbLong';
        dbCurrency   : Result := 'dbCurrency';
        dbSingle     : Result := 'dbSingle';
        dbDouble     : Result := 'dbDouble';
        dbDate       : Result := 'dbDate';
        dbBinary     : Result := 'dbBinary';
        dbText       : Result := 'dbText';
        dbLongBinary : Result := 'dbLongBinary';
        dbMemo       : Result := 'dbMemo';
        dbGUID       : Result := 'dbGUID';
        dbBigInt     : Result := 'dbBigInt';
        dbVarBinary  : Result := 'dbVarBinary';
        dbChar       : Result := 'dbChar';
        dbNumeric    : Result := 'dbNumeric';
        dbDecimal    : Result := 'dbDecimal';
        dbFloat      : Result := 'dbFloat';
        dbTime       : Result := 'dbTime';
        dbTimeStamp  : Result := 'dbTimeStamp';
 End;
End;

Function BracketField (const FieldName:String) : String;
Var
  P : Integer;
  S : String;
Begin
  Result := '';
  S := FieldName;
  P := Pos('.',S);
  While P > 0 Do
    Begin
      Result := Result+System.Copy(S,1,P-1);
      Result := Result+'].[';
      System.Delete(S,1,P);
      P := Pos('.',S);
    End;
  Result := '['+Result+S+']';
End;

end.
