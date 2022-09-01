unit bvStringUtils;

interface

uses sysutils,{IniUnit,}math,bvlocalization,classes;

const SEPARATORSTRING='<ENTERSEPARATOR>';


function IsValidDate(DateStr:string;WithRaise:boolean=false):boolean;
function IsValidInteger(NumberStr:string;WithRaise:boolean=false):boolean;
function IsValidFloat(NumberStr:string;WithRaise:boolean=false):boolean;
function IsValidMoney(NumberStr:string;WithRaise:boolean=false):boolean;

function MyStrToInt(NumberStr:String;WithRaise:boolean=false):integer;

function GetMonth(DT:TDateTime) :string;


function IsDigit(Value:char):boolean;
function IsAlphaDigit(Value:char;OnlyAlpha:boolean=false;OnlyUpper:boolean=true):boolean;
function IsStringKey(key:word):boolean;

function bvCurrToStrF(Value:Currency;ParDecimalSeparator:char='?';ParCurrencyString:string='?';parthousandSeparator:string='?';ParCurrencyDecimals:integer=2):string;
function bvCurrToStr(Value:Currency):string;

function bvFloatToStr(Value:Extended):string;

function GetWord(const Str:string;var First:integer;SpaceChar:char=' ';LeftTrim:boolean=true):string;
function GetTErm(const Str:string;var First:integer):string;

function SetFixedString(const str:string;Len:integer):string;

function GetString(const Value:string):string;
function SetStr(const Value:string):string;

function DeleteSpaces(str:string):string;

function CurrMod(Value :Currency):Currency;

function InString(List:TStrings;Str:string;CaseSensitive:boolean=false;Symb:char='%'):boolean;
function StrInTemplate(tempStr:string;pStr:string;CaseSensitive:boolean=false;Symb:char='%';Delimiter:char=#0):boolean;

function StrToDateProtected(str:string):TDateTime;
function StrToFloatProtected(Str:string):double;

type
  MyDateConvError=class(Exception);
  MyNumberConvError=class(Exception);

function IsQuoted(Str:string):boolean;
function DeleteQuoted(Str:string):String;

function CorrectBarCode(BarCode:string):string;


function GetPos(Substr,Str:string;IgnoreCase:boolean=true;MatchWord:boolean=true;FirstI:integer=1;lastI:integer=-1):integer;

function WordToEnd(Str :string; SubStr :TStrings):string;


implementation


function GetPos(Substr,Str:string;IgnoreCase:boolean;MatchWord:boolean;FirstI:integer;lastI:integer):integer;
var i:integer;
    Len,Len1:integer;
    k:integer;

begin
  Result:=0;
  Len:=length(str);
  if (FirstI>Len) or (Len=0) then exit;

  if LastI=-1 then LastI:=LEN;

  Len1:=length(Substr);
  if (Len1=0) then exit;

  i:=FirstI;
  while (i<=Len) and (i<=lastI) do begin
      if IgnoreCase and (ansiuppercase(str[i])=ansiuppercase(Substr[1]))
         or not IgnoreCase and (str[i]=Substr[1])
      then begin
          k:=i;
          while (k-i<len1) and (k<=LastI) do begin
             if IgnoreCase and (ansiuppercase(str[k])<>ansiuppercase(substr[k-i+1]))
                or not IgnoreCase and (str[k]<>substr[k-i+1])
             then begin
                i:=k+1;
                break;
             end
             else inc(k);
          end;
          if not ((k-i<len1) and (k<=LastI))
          then begin
             if MatchWord
                and
                ( (i+Len1<LastI) and IsAlphaDigit(Str[i+Len1],false,false)
                  or
                  (i>FirstI) and isalphadigit( Str[i-1],false,false)
                )
             then begin
                i:=i+Len1;
             end
             else begin
               REsult:=i;
               exit;
             end;
          end;
      end
      else inc(i);
  end;
end;

function CorrectBarCode(BarCode:string):string;
var i:integer;
begin
  REsult:=trim(Barcode);
  while (length(Result)>0)
        and  (Result[1]='0')
  do delete(result,1,1);

  i:=1;
  while i<=length(Result) do begin
    if not (Result[i] in ['0'..'9'])
    then delete(Result,i,1)
    else inc(i);
  end;
end;

function SetFixedString(const str:string;Len:integer):string;
var strlen:integer;
begin
   strlen:=length(str);
   if strlen>=len then Result:=copy(str,1,len)
   else result:=str+stringofchar(' ',len-strlen);
end;


function GetMonth(DT:TDateTime) :string;
var Year,Month,Day:word;
begin
  DecodeDate(DT,Year,Month,Day);
  if (month>=1) and (month<=12) then Result:=ArrMonth[Month]
  else Result:='?????????';
end;


function IsValidDate(DateStr:string;WithRaise:boolean):boolean;
//var ThisDate:TDateTime;
begin
  Result:=true;
  if (trim(DateStr)='') or (trim(DateStr)='.  .') then exit
  else begin
    try
      {Thisdate:=} StrtoDate(DateStr);
    except
      Result:=false;
      if WithRaise then raise MyDateConvError.Create(StrBadDataTimeFormat);
    end;
  END;
end;

function IsValidInteger(NumberStr:string;WithRaise:boolean):boolean;
//var ThisNumber:longint;
begin
  Result:=true;
  if trim(NumberStr)='' then exit
  else begin
    try
      {ThisNumber:= } strtoint(NumberStr);
    except
      Result:=false;
      if WithRaise then raise MyNumberConvError.Create(StrBadIntegerFormat);
    end;
  end;
end;

function IsValidFloat(NumberStr:string;WithRaise:boolean):boolean;
begin
  Result:=true;
  if trim(NumberStr)='' then exit
  else begin
    try
      StrToFloat(NumberStr);
    except
      Result:=false;
      if WithRaise then raise MyNumberConvError.Create(strBadFormatofNumber);
    end;
  end;
end;

function IsValidMoney(NumberStr:string;WithRaise:boolean):boolean;
begin
  Result:=true;
  if trim(NumberStr)='' then exit
  else begin
    try
      StrToCurr(NumberStr);
    except
      Result:=false;
      if WithRaise then raise MyNumberConvError.Create(StrBadFormatOfCurrency);
    end;
  end;
end;

function MyStrToInt(NumberStr:String;WithRaise:boolean):integer;
begin
  if IsValidInteger(NumberStr,withRaise) then begin
    If Trim(NumberStr)='' then Result:=0
    else Result:=strtoint(NumberStr);
  end
  else Result:=0
end;



function CurrMod(Value :Currency):Currency;
begin
  if Value>100 then Value:=Value-(int(Value/100))*100;
  if Value>20 then Value:=Value-(int(Value/10))*10;
  if Value<10 then Result:=Value
  else Result:=0;
//  else Result:=CurrMod(int(Value/10));
end;

//function GetSumm(Value :currency) :string;


function IsQuoted(Str:string):boolean;
begin
//  Result:=false;
  if Length(Str)<1 then Result:=false
  else   Result:=(str[1]=#39) or (str[1]='"');
  if (Result=false) and (Length(Str)>=2) then  Result:=(str[Length(Str)]=#39) or (str[Length(Str)]='"')
end;


function DeleteQuoted(Str:string):String;
begin
  if (Length(Str)>=1)
     and ((str[1]=#39) or (str[1]='"'))
  then Str:=copy(Str,2,Length(Str)-1);

  if (Length(Str)>=1)
     and ((str[length(Str)]=#39) or (str[length(Str)]='"'))
  then Str:=copy(Str,1,Length(Str)-1);

  Result:=Str;
end;




function IsDigit(value:char):boolean;
begin
  Result:=Value in ['0'..'9'];
end;

function IsAlphaDigit(Value:char;OnlyAlpha:boolean=false;OnlyUpper:boolean=true):boolean;
begin
  Value:=ansiuppercase(value)[1];
  Result:=not OnlyAlpha and (IsDigit(Value))
          or (value in ['A'..'Z'])
          or (Value in AltChars)
          or (Not OnlyUpper
              and ((value in ['a'..'z'])
                   or
                   (value in AltSmallChars))
             )
end;


function GetWord(const Str:string;var First:integer;SpaceChar:char;LeftTrim:boolean):string;
var i:integer;
    LEN:integer;
begin
  first:=1;
  Len:=Length(str);
  Result:='';

  if lefttrim then begin
    while First<=len do begin
       if str[first]<>SpaceChar then break
       else inc(First);
    end;
  end;
  if First>Len then begin
     First:=-1;
  end
  else begin
     i:=First;

     while (I<=Len) do begin
        if Str[i]=SpaceChar then break
        else Result:=Result+Str[i];
        inc(i);
     end
  end
end;

function GetTerm(const Str:string;var First:integer):string;
var i:integer;
    LEN:integer;
begin
  first:=1;
  Len:=Length(str);
  while First<=len do begin
     if IsAlphaDigit(str[first],true) then break
     else inc(First);
  end;
  if First>Len then begin
     First:=-1;
     Result:=''
  end
  else begin
     i:=First+1;
     REsult:=Str[first];
     while (I<=Len) do begin
        if not IsAlphaDigit(Str[i]) then break
        else Result:=Result+Str[i];
        inc(i);
     end
  end

end;

function IsStringKey(key:word):boolean;
begin
  Result:= ((char(Key)=' ') OR IsAlphaDigit(char(Key))
           or (key in [96,186,188,190])
           {96,188,190 - это необъ€снимый глюк!}
           )
         and not (Key in [16,17,18])
end;

function bvFloatTostr(Value:Extended):string;
begin
   REsult:=FloatToStr(Value);   //FormatFloat('#.#',Value); - ан-но floattostr
   Result:=StringReplace( Result,ThousandSeparator,'',[rfReplaceAll]);
   Result:=StringReplace( Result,DecimalSeparator,'.',[rfReplaceAll]);
end;

function bvCurrToStr(Value:Currency):string;
begin
   result := bvCurrToStrf(value,'.','',' ',2);
end;

function bvCurrToStrF(Value:Currency;ParDecimalSeparator:char='?';ParCurrencyString:string='?';parthousandSeparator:string='?';ParCurrencyDecimals:integer=2):string;
begin
  Result:=currtostrF(Value,ffCurrency,ParCurrencyDecimals);
  if PardecimalSeparator<>'?'
  then begin
     Result:=StringReplace( Result,DecimalSeparator,ParDecimalSeparator,[rfReplaceAll]);
  end;

  if parthousandSeparator<>'?'
  then begin
     Result:=StringReplace( Result,ThousandSeparator,parthousandSeparator,[rfReplaceAll]);
  end;

  if parcurrencystring<>'?' then begin
      if trim(CurrencyString)<>''
      then begin
         Result:=StringReplace( Result,Currencystring,ParCurrencyString,[rfReplaceAll]);
      end
      else if ParCurrencyString<>'?'
      then begin
         Result:=result+ParCurrencystring;
      end;
  end;

end;

function GetString(const Value:string):string;
begin
  REsult:=StringReplace( value,SEPARATORSTRING,#13#10,[rfReplaceall,rfIgnorecase]);
end;

function SetStr(const Value:string):string;
begin
  REsult:=StringReplace( value,#13#10,SEPARATORSTRING,[rfReplaceall,rfIgnorecase]);
end;




function DeleteSpaces(str:string):string;
var i:integer;
    //len:integer;
begin
   REsult:='';
   //len:=0;
   for i:=1 to length(str) do begin
     if not
        (
         (
           (str[i]=' ') or (str[i]= #09)
         )
         and
         (
           (i=1) or (str[i-1]=' ') or (str[i-1]= #09)
         )
        )
     then
       REsult:=Result+str[i];
   end;

   REsult:=trim(Result);
//   repeat
//     REsult:=StringReplace( str,'  ',' ',[rfREplaceAll]);
//   until REsult=str;
end;

function StrInTemplate(TempStr:string;pStr:string;CaseSensitive:boolean=false;Symb:char='%';Delimiter:char=#0):boolean;
var First,Last,Mid1,Mid2,Mid3,Mid4,Mid5:string;
    i,k:integer;
    Len:integer;
    Ok:boolean;
    Str,Str1:string;

    defaultID:integer;
    lenp:integer;
begin
  Result:=false;

  pStr:=trim(pStr);
  if not CaseSensitive then  pStr:=Ansiuppercase(pStr);


  lenp:=length(pStr);

  if lenp=0 then begin
     exit
  end;


  if not CaseSensitive then str1:=AnsiupperCase(TempSTr)
  else Str1:=TempStr;

  while not Result and (Str1>'') do
  begin
      if (Delimiter<>#0) and (pos(Delimiter,Str1)>0) then  Str:=copy(Str1,1,pos(Delimiter,Str1)-1)
      else Str:=Str1;

      delete( Str1,1,length(Str)+1);

      str:=trim(Str);
      //if not CaseSensitive then Str:=ansiuppercase(Str);
      len:=length(str);

      //if Str=Symb then defaultid:=dmKassa.tasertif.fieldbyname('ID').asinteger;

      last:='';
      First:='';
      Mid1:='';
      Mid2:='';
      Mid3:='';
      Mid4:='';
      Mid5:='';


      if (len>0) then begin

        i:=1;

        while i<=len do begin
           if str[i]=Symb then begin
             break
           end
           else begin
              AppendStr(First,str[i]);
              inc(i);
           end
        end;

        if i<=len then begin
          inc(i);
          Last:='';
          while i<=len do begin
             if str[i]=Symb then begin
               break
             end
             else begin
                AppendStr(last,str[i]);
                inc(i);
             end
          end;

          if i<=len then begin
            inc(i);
            Mid1:=Last;
            Last:='';

            while i<=len do begin
               if str[i]=Symb then begin
                 break
               end
               else begin
                  AppendStr(last,str[i]);
                  inc(i);
               end
            end;

            if i<=len then begin
              inc(i);
              Mid2:=Last;
              Last:='';

              while i<=len do begin
                 if str[i]=Symb then begin
                   break
                 end
                 else begin
                    AppendStr(last,str[i]);
                    inc(i);
                 end
              end;

              if i<=len then begin
                inc(i);
                Mid3:=Last;
                Last:='';

                while i<=len do begin
                   if str[i]=Symb then begin
                     break
                   end
                   else begin
                      AppendStr(last,str[i]);
                      inc(i);
                   end
                end;

                if i<=len then begin
                  inc(i);
                  Mid4:=Last;
                  Last:='';

                  while i<=len do begin
                     if str[i]=Symb then begin
                       break
                     end
                     else begin
                        AppendStr(last,str[i]);
                        inc(i);
                     end
                  end;

                  if i<=len then begin
                    inc(i);
                    Mid5:=Last;
                    Last:='';

                    while i<=len do begin
                       if str[i]=Symb then begin
                         break
                       end
                       else begin
                          AppendStr(last,str[i]);
                          inc(i);
                       end
                    end;

                  end
                end
              end

            end

          end
        end;
      end;

      if (First<>'')
         or (Mid1<>'')
         or (Mid2<>'')
         or (Mid3<>'')
         or (Mid4<>'')
         or (Mid5<>'')
         or (last<>'')
      then begin
         if (First='') or  (pos(First, pStr)=1)
         then begin
            Ok:=true;
            if Last<>'' then begin
              k:=length(Last);
              for i:=length(pStr) downto 1 do begin
                if pStr[i]<>last[k] then begin
                  Ok:=false;
                  break
                end
                else begin
                  dec(k);
                  if k<=0 then begin
                    //Ok:=false;
                    break
                  end;
                end;
              end;
            end;

            i:=1;
            if Ok and (mid1<>'')
            then begin
                 k:=pos(mid1,pStr);
                 if (k>i) and (k<lenp)
                 then begin
                    i:=k
                 end
                 else ok:=false;
            end;

            if Ok and (mid2<>'')
            then begin
                 k:=pos(mid2,pStr);
                 if (k>i) and (k<lenp)
                 then begin
                    i:=k
                 end
                 else ok:=false;
            end;

            if Ok and (mid3<>'')
            then begin
               k:=pos(mid3,pstr);
               if (k>i) and (k<lenp)
               then begin
                  i:=k
               end
               else ok:=false;
            end;

            if Ok and (mid4<>'')
            then begin
               k:=pos(mid4,pStr);
               if (k>i) and (k<lenp)
               then begin
                  i:=k
               end
               else ok:=false;
            end;

            if Ok and (mid5<>'')
            then begin
               k:=pos(mid5,pStr);
               if (k>i) and (k<lenp)
               then begin
                  //i:=k  последний раз не надо
               end
               else ok:=false;
            end;

            if ok then begin
               Result:=true;
               break
            end;
         end;
      end;
  end;

end;


function InString(List:TStrings;Str:string;CaseSensitive:boolean=false;Symb:char='%'):boolean;
var i:integer;
begin
  //Str1:=ansiuppercase(Str);
  Result:=false;
  for i:=0 to List.count-1 do begin
     Result:= StrInTemplate(List[i],Str,Casesensitive,Symb);
     if Result then break;
     {if CaseSensitive then Str2:=List[i]
     else Str2:=ansiuppercase(List[i]);
     if pos(str2,Str1)>0 then begin
        Result:=true;
        exit;
     end;:
     }
  end;
  //Result:=false;
end;

function StrToDateProtected(str:string):TDateTime;
var i:integer;
begin
    Str:=stringreplace(trim(Str),' ','',[rfReplaceAll]);
    for i:=1 to length(Str) do begin
      if not isDigit(Str[i])
         and not (Str[i]=dateSeparator)
      then Str[i]:=DateSeparator;
    end;


    try
      if Str=''
      then Result:=0
      else Result:=strtodate(str);
    except
      Result:=0;
    end;

end;

function StrToFloatProtected(Str:string):double;
var i:integer;
begin
    Str:=stringreplace(trim(Str),' ','',[rfReplaceAll]);
    for i:=1 to length(Str) do begin
      if not isDigit(Str[i])
         and not (Str[i]=decimalSeparator)
      then Str[i]:=decimalSeparator;
    end;

    if str='' then Result:=0
    else
    try
      REsult:=StrToFloat(Str);
    except
      Result:=0;
    end;
end;

function WordToEnd(Str :string; SubStr :TStrings):string;
var i:integer;
    ipos :integer;
    len :integer;
    istr :string;
begin
  Result := Str;
  Str := AnsiUpperCase(Str);

  for i:=0 to Substr.Count-1 do
  begin
     iStr := substr[i];
     len := length(iStr);

     ipos := pos(ansiuppercase(iStr),Str);
     if (ipos >0)
        and ((ipos=1)
             or
             (str[ipos-1] in [#9,' '])
            )
        and ((length(str) <= (ipos +len))
             or
             (str[ipos+len] in [#9,' '])
            )
     then begin
        if ipos>1 then begin // пробел захватим, если это не первое слово.
           dec(iPos);
           inc(len);
        end;

        delete(Result,ipos,len);
        delete(Str,ipos,len);

        result := Result +' '+iStr;
        Str := Str +' '+iStr;
     end;
  end;

end;


end.
