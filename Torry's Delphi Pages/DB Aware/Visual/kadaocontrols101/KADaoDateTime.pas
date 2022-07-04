unit KADaoDateTime;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Dialogs;

type
  TKADaoDateTime = class(TComponent)
  private
    { Private declarations }
    F_DateSeparator   : Char;
    F_ShortDateFormat : String;
    F_LongDateFormat  : String;
    F_TimeSeparator   : Char;
    F_TimeAMString    : String;
    F_TimePMString    : String;
    F_ShortTimeFormat : String;
    F_LongTimeFormat  : String;
    F_TimeFormat      : String;
    F_ShortMonthNames : Array[1..12] of String;
    F_LongMonthNames  : Array[1..12] of String;
    F_ShortDayNames   : Array[1..7]  of String;
    F_LongDayNames    : Array[1..7]  of String;
    F_HasLongMonth    : Boolean;
    F_HasShortMonth   : Boolean;
    F_HasDate         : Boolean;
    F_HasTime         : Boolean;
    F_Den             : Integer;
    F_DenOtSedmicata  : Integer;
    F_Mesec           : Integer;
    F_Godina          : Integer;
    F_Chasove         : Integer;
    F_Minuti          : Integer;
    F_Secundi         : Integer;
    F_MiliSecundi     : Integer;
    F_SDOrder         : Integer;
    F_LDOrder         : Integer;
    F_DateOrder       : Integer;
    F_TimeOrder       : Integer;
    F_CodePage        : Integer;
    F_Date            : TDateTime;
    F_Time            : TDateTime;
    F_DateTime        : TDateTime;
    F_Digits          : TStringList;
    F_DateTimeString  : String;
    F_SQLString       : String;
    F_SQLField        : String;
    F_Over2000        : Integer;

    Procedure         F_Set_CodePage(Value : Integer);
    Procedure         F_Set_Date(Value : TDateTime);
    Procedure         F_Set_Date_Time(Value : TDateTime);
    Procedure         F_Set_Den(Value : Integer);
    Procedure         F_Set_Mesec(Value : Integer);
    Procedure         F_Set_Godina(Value : Integer);
    Procedure         F_Set_Chasove(Value : Integer);
    Procedure         F_Set_Minuti(Value : Integer);
    Procedure         F_Set_MiliSecundi(Value : Integer);
    Procedure         F_Set_Secundi(Value : Integer);
    Procedure         F_Set_DenOtSedmicata(Value : Integer);
    Procedure         F_Set_SQLString(Value : String);
    Procedure         F_Set_SQLField(Value : String);
    Procedure         F_Set_DateTimeString(Value : String);

    Procedure         DTError(Orig:String);
    Function          GetLongMonth(Var DTS : String): Integer;
    Function          GetShortMonth(Var DTS : String): Integer;
    Function          RemoveLongDayName(Var DTS : String):Integer;
    Function          RemoveShortDayName(Var DTS : String):Integer;
    Procedure         RemoveNonNumericChars(Var DTS : String);
    Procedure         RemoveExtraSpaces(Var DTS:String);
    Procedure         RemoveFirstSpaces(Var DTS:String);
    Procedure         RemoveLastSpaces(Var DTS:String);
    Procedure         RemoveQuotedData(Var FMTS : String);
    Function          GetLocaleString(Locale, LocaleType: Integer; const Default: String): String;
    Procedure         GetDateAndTime(Const DTS : String;Var Data : String; Var Vreme  : String);
    Procedure         RefreshDataTimeInfo;
    Function          ProcessDateTimeString(Var DTS:String):TDateTime;
  protected
    { Protected declarations }
    procedure         Loaded; override;
  public
    { Public declarations }
    F_AccessShorting  : Boolean;
    F_BulShort        : Boolean;
    Constructor       Create(AOwner: TComponent);override;
    Destructor        Destroy;override;
  published
    { Published declarations }
    Property AccessShorting : Boolean       Read F_AccessShorting Write F_AccessShorting;
    Property CodePage       : Integer       Read F_CodePage       Write F_Set_CodePage;
    Property Date           : TDateTime     Read F_Date           Write F_Set_Date;
    Property DateTime       : TDateTime     Read F_DateTime       Write F_Set_Date_Time;
    Property Day            : Integer       Read F_Den            Write F_Set_Den;
    Property Month          : Integer       Read F_Mesec          Write F_Set_Mesec;
    Property Year           : Integer       Read F_Godina         Write F_Set_Godina;
    Property Hours          : Integer       Read F_Chasove        Write F_Set_Chasove;
    Property Minutes        : Integer       Read F_Minuti         Write F_Set_Minuti;
    Property MilliSeconds   : Integer       Read F_MiliSecundi    Write F_Set_MiliSecundi;
    Property Seconds        : Integer       Read F_Secundi        Write F_Set_Secundi;
    Property WeekDay        : Integer       Read F_DenOtSedmicata Write F_Set_DenOtSedmicata;
    Property SQLString      : String        Read F_SQLString      Write F_Set_SQLString;
    Property SQLField       : String        Read F_SQLField       Write F_Set_SQLField;
    Property Over2000       : Integer       Read F_Over2000       Write F_Over2000;
    Property DateTimeString : String        Read F_DateTimeString Write F_Set_DateTimeString;
  end;

procedure Register;

implementation

Constructor TKADaoDateTime.Create(AOwner: TComponent);
Begin
  inherited Create(AOwner);
  F_Digits           := TStringList.Create;
  F_AccessShorting   := False;
  F_BulShort         := False;
  F_Den              := 0;
  F_Mesec            := 0;
  F_Godina           := 0;
  F_Chasove          := 0;
  F_Minuti           := 0;
  F_Secundi          := 0;
  F_MiliSecundi      := 0;
  F_DenOtSedmicata   := 0;
  F_Over2000         := 10;
  F_HasLongMonth     := False;
  F_HasShortMonth    := False;
  F_HasDate          := False;
  F_HasTime          := False;
  F_BulShort         := False;
  F_SDOrder          := 0;
  F_LDOrder          := 0;
  F_DateOrder        := 0;
  F_TimeOrder        := 0;
  F_CodePage         := 0;
  F_Date             := 0;
  F_Time             := 0;
  F_DateTime         := 0;
  F_DateTimeString   := '';
  F_SQLString        := '';
  F_SQLField         := '';
  RefreshDataTimeInfo;
End;

Destructor TKADaoDateTime.Destroy;
Begin
  F_Digits.Free;
  Inherited Destroy;
End;

procedure TKADaoDateTime.Loaded;
Begin
  Inherited Loaded;
  if F_DateTimeString <> '' Then DateTimeString := F_DateTimeString;
End;

Procedure TKADaoDateTime.F_Set_CodePage(Value : Integer);
Begin
 //***************************** Read Only
End;

Procedure TKADaoDateTime.F_Set_Date(Value : TDateTime);
Begin
  F_Date := Value;
  F_SQLString := FormatDateTime('mm"/"dd"/"yyyy', F_Date);
  F_SQLField := '#'+F_SQLString+'#';
  F_DateTimeString := DateToStr(F_Date);
  F_DenOtSedmicata:=DayOfWeek(F_Date);
End;

Procedure TKADaoDateTime.F_Set_Date_Time(Value : TDateTime);
Begin
 F_DateTime := Value;
 F_SQLString := FormatDateTime('mm"/"dd"/"yyyy hh":"nn":"ss', F_DateTime);
 F_SQLField := '#'+F_SQLString+'#';
 F_DateTimeString := DateTimeToStr(F_DateTime);
 F_DenOtSedmicata:=DayOfWeek(F_DateTime);
End;

Procedure TKADaoDateTime.F_Set_Den(Value : Integer);
Begin
 //***************************** Read Only
End;

Procedure TKADaoDateTime.F_Set_Mesec(Value : Integer);
Begin
 //***************************** Read Only
End;

Procedure TKADaoDateTime.F_Set_Godina(Value : Integer);
Begin
 //***************************** Read Only
End;

Procedure TKADaoDateTime.F_Set_Chasove(Value : Integer);
Begin
 //***************************** Read Only
End;

Procedure TKADaoDateTime.F_Set_Minuti(Value : Integer);
Begin
 //***************************** Read Only
End;

Procedure TKADaoDateTime.F_Set_MiliSecundi(Value : Integer);
Begin
 //***************************** Read Only
End;

Procedure TKADaoDateTime.F_Set_Secundi(Value : Integer);
Begin
 //***************************** Read Only
End;

Procedure TKADaoDateTime.F_Set_DenOtSedmicata(Value : Integer);
Begin
 //***************************** Read Only
End;

Procedure TKADaoDateTime.F_Set_SQLString(Value : String);
Begin
End;

Procedure TKADaoDateTime.F_Set_SQLField(Value : String);
Begin
End;


Procedure TKADaoDateTime.F_Set_DateTimeString(Value : String);
Var
  S : String;
Begin
  F_DateTimeString := Value;
  if csLoading in ComponentState Then Exit;
  S := F_DateTimeString;
  ProcessDateTimeString(S);
  if F_HasDate And  F_HasTime Then F_SQLString := FormatDateTime('mm"/"dd"/"yyyy hh":"nn":"ss', F_DateTime)
  Else
  if F_HasDate Then F_SQLString := FormatDateTime('mm"/"dd"/"yyyy', F_Date)
  Else
  if F_HasTime Then F_SQLString := FormatDateTime('hh":"nn":"ss', F_Time);
  F_SQLField := '#'+F_SQLString+'#';
End;

Procedure TKADaoDateTime.DTError(Orig:String);
Var
 S : String;
Begin
 S := 'Invalid format!';
 if F_HasDate And F_HasTime Then S := Orig+' is not a valid date and time.'
 Else
 if F_HasDate               Then S := Orig+' is not a valid date.'
 Else
 if F_HasTime               Then S := Orig+' is not a valid time.';
 Raise EConvertError.Create(S);
End;

Function TKADaoDateTime.GetLongMonth(Var DTS : String): Integer;          
Var
   X : Integer;
   P : Integer;
Begin
   Result := 0;
   For X := 1 to 12 do
      Begin
        P := Pos(AnsiLowerCase(F_LongMonthNames[X]),AnsiLowerCase(DTS));
        if P > 0 Then
           Begin
             Result := X;
             System.Delete(DTS,P,Length(F_LongMonthNames[X]));
             System.Insert(' ',DTS,P);
             Exit;
           End;
      End;
End;

Function TKADaoDateTime.GetShortMonth(Var DTS : String): Integer;
Var
   X : Integer;
   P : Integer;
Begin
   Result := 0;
   For X := 1 to 12 do
      Begin
        P := Pos(AnsiLowerCase(F_ShortMonthNames[X]),AnsiLowerCase(DTS));
        if P > 0 Then
           Begin
             Result := X;
             System.Delete(DTS,P,Length(F_ShortMonthNames[X]));
             System.Insert(' ',DTS,P);
             Exit;
           End;
      End;
End;

Function TKADaoDateTime.RemoveLongDayName(Var DTS : String):Integer;
Var
   X : Integer;
   P : Integer;
Begin
  Result := 0;
  For X := 1 to 7 do
      Begin
        P := Pos(AnsiLowerCase(F_LongDayNames[X]),AnsiLowerCase(DTS));
        if P > 0 Then
           Begin
             Result := X;
             System.Delete(DTS,P,Length(F_LongDayNames[X]));
             System.Insert(' ',DTS,P);
             Exit;
           End;
      End;
End;

Function TKADaoDateTime.RemoveShortDayName(Var DTS : String):Integer;
Var
   X : Integer;
   P : Integer;
Begin
  Result := 0;
  For X := 1 to 7 do
      Begin
        P := Pos(AnsiLowerCase(F_ShortDayNames[X]),AnsiLowerCase(DTS));
        if P > 0 Then
           Begin
             Result := X;
             System.Delete(DTS,P,Length(F_ShortDayNames[X]));
             System.Insert(' ',DTS,P);
             Exit;
           End;
      End;
End;

Procedure TKADaoDateTime.RemoveNonNumericChars(Var DTS : String);
Var
 CH : Char;
 X  : Integer;
 L  : Integer;
Begin
 L  := Length(DTS);
 if L=0 Then Exit;
 For X := 1 to L do
   Begin
     CH := DTS[X];
     if (NOT IsCharAlpha(CH)) And (IsCharAlphaNumeric(CH)) Then
        Begin
        End
     Else
        Begin
         DTS[X]:=' ';
        End;
   End;
End;

Procedure TKADaoDateTime.RemoveExtraSpaces(Var DTS:String);
Var
 P : Integer;
Begin
 P := Pos('  ',DTS);
 While P > 0 do
   Begin
    System.Delete(DTS,P,1);
    P := Pos('  ',DTS);
   End;
End;

Procedure TKADaoDateTime.RemoveFirstSpaces(Var DTS:String);
Begin
 While (Length(DTS) > 0) And (DTS[1]=' ') do
   Begin
    System.Delete(DTS,1,1);
   End;
End;

Procedure TKADaoDateTime.RemoveLastSpaces(Var DTS:String);
Var
 L : Integer;
Begin
 L := Length(DTS);
 While (L > 0) And (DTS[L]=' ') do
   Begin
    System.Delete(DTS,L,1);
    L := Length(DTS);
   End;
End;

Procedure TKADaoDateTime.RemoveQuotedData(Var FMTS : String);
Var
  X    : Integer;
  L    : Integer;
  FIQ  : Boolean;
Begin
  L := Length(FMTS);
  if L=0 Then Exit;
  FIQ := False;
  For X := 1 to L do
    Begin
     if FMTS[X]='''' Then
        Begin
          FMTS[X] := ' ';
          FIQ := NOT FIQ;
        End;
     if FIQ THEN FMTS[X] := ' ';
    End;
  RemoveExtraSpaces(FMTS);
  RemoveFirstSpaces(FMTS);
  RemoveLastSpaces (FMTS);
End;

Function TKADaoDateTime.GetLocaleString(Locale, LocaleType: Integer; const Default: String): String;
var
  L: Integer;
  Buffer: Array[0..255] of Char;
begin
  L := GetLocaleInfo(Locale, LocaleType, Buffer, SizeOf(Buffer));
  if L > 0 then SetString(Result, Buffer, L - 1) else Result := Default;
end;

Procedure TKADaoDateTime.RefreshDataTimeInfo;
Var
  X      : Integer;
Begin
  F_DateSeparator   := DateSeparator;
  F_ShortDateFormat := ShortDateFormat;
  F_LongDateFormat  := LongDateFormat;
  F_TimeSeparator   := TimeSeparator;
  F_TimeAMString    := TimeAMString;
  F_TimePMString    := TimePMString;
  F_ShortTimeFormat := ShortTimeFormat;
  F_LongTimeFormat  := LongTimeFormat;
  F_TimeFormat      := GetLocaleString(GetThreadLocale, LOCALE_STIMEFORMAT, 'hh:mm:ss');
  F_SDOrder         := StrToInt(GetLocaleString(GetThreadLocale, LOCALE_IDATE,  '0'));
  F_LDOrder         := StrToInt(GetLocaleString(GetThreadLocale, LOCALE_ILDATE, '0'));
  F_TimeOrder       := StrToInt(GetLocaleString(GetThreadLocale, LOCALE_ITIME, '0'));
  For X := 1 to 12 do
      Begin
       F_ShortMonthNames[X] := ShortMonthNames[X];
       F_LongMonthNames[X]  := LongMonthNames[X];
      End;
  For X := 1 to 7 do
      Begin
       F_ShortDayNames[X]   := ShortDayNames[X];
       F_LongDayNames[X]    := LongDayNames[X];
      End;
  F_CodePage := GetACP;
  if F_CodePage=1251 Then
     Begin
       if AnsiLowerCase(F_ShortDayNames[5])='четвъвтък' then ShortDayNames[5][6]:='р';
       if AnsiLowerCase(F_LongDayNames[5]) ='четвъвтък' then LongDayNames[5][6]:='р';
       F_ShortDayNames[5]   := ShortDayNames[5];
       F_LongDayNames[5]    := LongDayNames[5];
       If F_BulShort Then
          Begin
           F_ShortDayNames[1]    := 'нед.';
           F_ShortDayNames[2]    := 'пон.';
           F_ShortDayNames[3]    := 'вто.';
           F_ShortDayNames[4]    := 'сря.';
           F_ShortDayNames[5]    := 'чет.';
           F_ShortDayNames[6]    := 'пет.';
           F_ShortDayNames[7]    := 'съб.';
           //*******************************************************************
          End;
     End;
  if F_AccessShorting Then
     Begin
       //*******************************************************************
       F_ShortMonthNames[1]  := Copy(F_ShortMonthNames[1],1,7);
       F_ShortMonthNames[2]  := Copy(F_ShortMonthNames[2],1,7);
       F_ShortMonthNames[3]  := Copy(F_ShortMonthNames[3],1,7);
       F_ShortMonthNames[4]  := Copy(F_ShortMonthNames[4],1,7);
       F_ShortMonthNames[5]  := Copy(F_ShortMonthNames[5],1,7);
       F_ShortMonthNames[6]  := Copy(F_ShortMonthNames[6],1,7);
       F_ShortMonthNames[7]  := Copy(F_ShortMonthNames[7],1,7);
       F_ShortMonthNames[8]  := Copy(F_ShortMonthNames[8],1,7);
       F_ShortMonthNames[9]  := Copy(F_ShortMonthNames[9],1,7);
       F_ShortMonthNames[10] := Copy(F_ShortMonthNames[10],1,7);
       F_ShortMonthNames[11] := Copy(F_ShortMonthNames[11],1,7);
       F_ShortMonthNames[12] := Copy(F_ShortMonthNames[12],1,7);
       //*******************************************************************
     End;
End;

Procedure TKADaoDateTime.GetDateAndTime(Const DTS : String;Var Data : String; Var Vreme  : String);
Var
 PDD : Integer;
 PTD : Integer;
 X   : Integer;
 CH  : Char;
 S   : String;
Begin
 PDD   := Pos(F_DateSeparator,DTS);
 PTD   := Pos(F_TimeSeparator,DTS);
 Data  := DTS;
 Vreme := '';
 S     := DTS;
 if PTD = 0 Then Exit;
 If PTD > PDD Then
    Begin
     For X := 1 to PTD-1 do
         Begin
          CH := S[X];
          if (NOT IsCharAlpha(CH)) And (IsCharAlphaNumeric(CH)) Then
             Begin
             End
          Else
             Begin
              S[X]:=' ';
             End;
         End;
     Repeat
       Dec(PTD);
     Until (PTD=0) or (DTS[PTD]=' ');
     Data  := Copy(S,1,PTD);
     System.Delete(S,1,PTD);
     Vreme := S;
    End;
End;

Function  TKADaoDateTime.ProcessDateTimeString(Var DTS:String):TDateTime;
Var
  Orig      : String;
  Data      : String;
  Vreme     : String;
  X         : Integer;
  L         : Integer;
  DD,MM,YY  : Word;
Begin
  Result          := 0;
  if DTS           = '' Then Exit;
  //****************************************************************************
  Orig            := DTS;
  F_Den           := 0;
  F_Mesec         := 0;
  F_Godina        := 0;
  F_Chasove       := 0;
  F_Minuti        := 0;
  F_Secundi       := 0;
  F_MiliSecundi   := 0;
  F_HasLongMonth  := False;
  F_HasShortMonth := False;
  F_HasDate       := False;
  F_HasTime       := False;
  //****************************************************************************
  RefreshDataTimeInfo;
  //****************************************************************************
  F_Mesec := GetLongMonth(DTS);
  if F_Mesec > 0 Then F_HasLongMonth := True;
  if F_Mesec = 0 Then
     Begin
       F_Mesec := GetShortMonth(DTS);
       if F_Mesec > 0 Then F_HasShortMonth := True;
     End;
  F_DenOtSedmicata := RemoveLongDayName(DTS);
  if F_DenOtSedmicata=0 Then F_DenOtSedmicata := RemoveShortDayName(DTS);
  //****************************************************************************
  F_HasDate := (Pos(F_DateSeparator,DTS) > 0);
  if F_Mesec > 0 Then F_HasDate := True;
  F_HasTime := (Pos(F_TimeSeparator,DTS) > 0);
  Data   := '';
  Vreme  := '';
  GetDateAndTime(DTS,Data,Vreme);
  RemoveNonNumericChars(Data);
  RemoveExtraSpaces(Data);
  RemoveFirstSpaces(Data);
  RemoveLastSpaces(Data);
  If Length(Data) > 0 Then F_HasDate := True;
  //****************************************************************************
  Data   := '';
  Vreme  := '';
  if F_HasDate And F_HasTime Then
     Begin
      Data   := '';
      Vreme  := '';
      GetDateAndTime(DTS,Data,Vreme);
     End
  Else
  if F_HasDate Then
     Begin
      Data   := DTS;
      Vreme  := '';
     End
  Else
  if F_HasTime Then
     Begin
      Data   := '';
      Vreme  := DTS;
     End
  Else
  DTError(Orig);

  RemoveNonNumericChars(Data);
  RemoveExtraSpaces(Data);
  RemoveFirstSpaces(Data);
  RemoveLastSpaces(Data);

  RemoveNonNumericChars(Vreme);
  RemoveExtraSpaces(Vreme);
  RemoveFirstSpaces(Vreme);
  RemoveLastSpaces(Vreme);

  if F_HasDate Then
     Begin
       F_Digits.Clear;
       if F_HasLongMonth Then F_DateOrder := F_LDOrder Else F_DateOrder := F_SDOrder;
       L := Length(Data);
       if L = 0 Then DTError(Orig);
       For X := 1 to L do if Data[X] = ' ' Then Data[X]:=#13;
       F_Digits.Text := Data;
       if F_Digits.Count < 2 Then DTError(Orig);
       Case F_DateOrder of
            0 : Begin {mdy}
                  if F_Mesec=0 Then
                     Begin
                       if F_Digits.Count < 3 Then DTError(Orig);
                       F_Mesec  := StrToInt(F_Digits.Strings[0]);
                       F_Den    := StrToInt(F_Digits.Strings[1]);
                     End
                  Else
                     Begin
                       F_Den    := StrToInt(F_Digits.Strings[1]);
                     End;
                  F_Godina      := StrToInt(F_Digits.Strings[2]);
                End;
            1 : Begin {dmy}
                  F_Den := StrToInt(F_Digits.Strings[0]);
                  if F_Mesec=0 Then
                     Begin
                       if F_Digits.Count < 3 Then DTError(Orig);
                       F_Mesec  := StrToInt(F_Digits.Strings[1]);
                       F_Godina := StrToInt(F_Digits.Strings[2]);
                     End
                  Else
                     Begin
                       F_Godina := StrToInt(F_Digits.Strings[1]);
                     End;
                End;
            2 : Begin {ymd}
                  F_Godina := StrToInt(F_Digits.Strings[0]);
                  if F_Mesec=0 Then
                     Begin
                       if F_Digits.Count < 3 Then DTError(Orig);
                       F_Mesec  := StrToInt(F_Digits.Strings[1]);
                       F_Den    := StrToInt(F_Digits.Strings[2]);
                     End
                  Else
                     Begin
                       F_Den    := StrToInt(F_Digits.Strings[1]);
                     End;
                End;
       End;
     End;
  //****************************************************************************
  if F_HasTime Then
     Begin
      F_Digits.Clear;
      L := Length(Vreme);
      if L = 0 Then DTError(Orig);
      For X := 1 to L do if Vreme[X] = ' ' Then Vreme[X]:=#13;
      F_Digits.Text := Vreme;
      if F_Digits.Count < 2 Then DTError(Orig);
      Case F_TimeOrder of
          0 : Begin
                F_Chasove     := StrToInt(F_Digits.Strings[0]);
                F_Minuti      := StrToInt(F_Digits.Strings[1]);
                if F_Digits.Count > 2 Then F_Secundi     := StrToInt(F_Digits.Strings[2]);
                if F_Digits.Count > 3 Then F_MiliSecundi := StrToInt(F_Digits.Strings[3]);
              End;
          1 : Begin
                F_Chasove     := StrToInt(F_Digits.Strings[0]);
                F_Minuti      := StrToInt(F_Digits.Strings[1]);
                if F_Digits.Count > 2 Then F_Secundi     := StrToInt(F_Digits.Strings[2]);
                if F_Digits.Count > 3 Then F_MiliSecundi := StrToInt(F_Digits.Strings[3]);
              End;
      End;
     End;
  //****************************************************************************
  F_Date      := 0;
  F_Time      := 0;
  F_DateTime  := 0;
  if F_HasDate And F_HasTime Then
     Begin
      if F_Godina < F_Over2000+1 Then F_Godina := F_Godina+2000;
      if (F_Godina > F_Over2000) And (F_Godina < 99) Then F_Godina := F_Godina+1900;
      F_Date      := EncodeDate(F_Godina, F_Mesec, F_Den);
      F_Time      := EncodeTime(F_Chasove, F_Minuti, F_Secundi, F_MiliSecundi);
      F_DateTime  := F_Date+F_Time;
      F_Time      := F_DateTime;
      Result      := F_DateTime;
      if F_DenOtSedmicata=0 Then F_DenOtSedmicata:=DayOfWeek(Result);
     End
  Else
  if F_HasDate Then
     Begin
      if F_Godina < F_Over2000+1 Then F_Godina := F_Godina+2000;
      if (F_Godina > F_Over2000) And (F_Godina < 99) Then F_Godina := F_Godina+1900;
      F_Date      := EncodeDate(F_Godina, F_Mesec, F_Den);
      F_DateTime  := F_Date;
      Result      := F_Date;
      if F_DenOtSedmicata=0 Then F_DenOtSedmicata:=DayOfWeek(Result);
     End
  Else
  if F_HasTime Then
     Begin
       DecodeDate(Now, YY, MM, DD);
       F_Den      := DD;
       F_Mesec    := MM;
       F_Godina   := YY;
       F_Time     := EncodeDate(F_Godina, F_Mesec, F_Den)+EncodeTime(F_Chasove, F_Minuti, F_Secundi, F_MiliSecundi);
       F_DateTime := F_Time;
       Result     := F_Time;
     End;
  if F_DenOtSedmicata > 0 Then
     Begin
       if DayOfWeek(Result) <> F_DenOtSedmicata Then DTError(Orig);
     End;
  //****************************************************************************
End;

procedure Register;
begin
  RegisterComponents('KADao Controls', [TKADaoDateTime]);
end;

end.
