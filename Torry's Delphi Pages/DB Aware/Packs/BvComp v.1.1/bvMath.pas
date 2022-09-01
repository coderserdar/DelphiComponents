unit bvMath;

interface

uses math;

function GetOkrugl(Cena:Currency;Okrugl:Integer;Trim:boolean=false):Currency;
function bvRound(Value:double;Decimals:Integer=0;trim:boolean=false):double;

type TFPoint=record
       X:double;
       Y:double;
     end;

function FPOINT(X,Y:double):TFPoint;

type ARRPoint=array of TFPoint;

function bvInterpolation(X:double;ARR:ARRPOINT):double;


type  SPLINESCOEFF=record
           C1:double;
           C2:double;
      end;

    ARRSPLINESCOEFF=array of SPLINESCOEFF;

function GetSplinesCoeff(ARR:ARRPOINT):ARRSPLINESCOEFF;
function bvInterpolationSplines(X:double;ARR:ARRPOINT;COEFF:ARRSPLINESCOEFF):double;


procedure QuickSortARRFPOINT(SortList:ARRPOINT; L, R: Integer);


implementation

{
function CompareFPointFunction(Item1, Item2: Pointer): Integer;
begin
   if TFPoint(Item1^).X<TFPoint(Item2^).X then Result:=-1
   else if TFPoint(Item1^).X>TFPoint(Item2^).X then Result:=2
   else Result:=0;
end;
}
procedure QuickSortARRFPOINT(SortList:ARRPOINT; L, R: Integer);
var
  I, J: Integer;
  P, T: TFPOINT;
begin
  repeat
    I := L;
    J := R;
    P := SortList[(L + R) shr 1];
    repeat
      while SortList[I].X<P.x  do Inc(I);
      while SortList[J].x> P.x do Dec(J);
      if I <= J then
      begin
        T := SortList[I];
        SortList[I] := SortList[J];
        SortList[J] := T;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSortARRFPOINT(SortList, L, J);
    L := I;
  until I >= R;
end;


function GetOkrugl(Cena:Currency;Okrugl:Integer;Trim:boolean=false):Currency;
var OkrVal:Currency;
    p2,p5:integer;
begin
  // 0-Точно,1-До 5 коп, 2 - до 10 коп., 3 - до 50 коп.
  // 4 - До 1руб., 5 - До 5 руб, 6- до 10 руб.,
  // 7 - До 50 руб., 8- До 100 руб....
  if Okrugl=0 then Result:=Cena
  else begin
    p2:=Okrugl div 2;
    p5:=Okrugl-p2-1;
    OkrVal:=0.05*intpower(2,p2)*intpower(5,p5);
    Result:=bvround( cena/OkrVal,0,trim )*OkrVal;
  end;
{
  case Okrugl of
    0: Result:=Cena;
    1: Result:=round(cena/0.05)*0.05;
    2: Result:=round(Cena/0.1)*0.1;
    3: Result:=round(cena/0.5)*0.5;
    4: Result:=round(Cena);
    5: Result:=round(Cena/5)*5;
    6: Result:=round(Cena/10)*10;
    7: Result:=round(Cena/50)*50;
    8: Result:=round(Cena/100)*100;
    else Result:=Cena;
  end;
}
end;

function bvRound(Value:double;Decimals:Integer=0;trim:boolean=false):double;
var Value1,VAlue2:double;
    DecValue:Integer;
begin                  
   DecValue:=round(IntPower(10,Decimals));

   Value1:=frac(Value);
   VAlue1:=Value1*DECVALUE;
   if not trim then  begin
      VAlue2:=frac(Value1);
      VAlue2:=round(VAlue2);
   end
   else begin
      Value2:=0;
   end;
   VAlue1:=int(Value1);
   VAlue1:=(Value1+VAlue2)/decVALUE;

   REsult:=int(VAlue)+Value1;

{
   VAlue:=VAlue*DECVALUE;

   Value1:=frac(value)-0.00000000001;

   //Value1:=Value1/DECVALUE;

   //Value1:=real(Value1);
   //Value1:=real(frac(Value)); //frac(Value);
   Value:=int(Value);
   if not Trim
   then begin
     if Value1>0.5 then begin        // все-таки нельзя >=0.5, т.к. идут ошибки округления
        if Value>=0 then Value:=Value+1
        else Value:=Value-1
     end;
   end;

   Result:=Value/DecValue;
}
end;



function bvInterpolation(X:double;ARR:ARRPoint):double;
  function COEFF(X:double;i:integer):double;
  var V1,V2:double;
      k:integer;
  begin
    Result:=1;


    for k:=low(ARR) to HIGH(ARR) do begin
       if k=i then begin
          V1:=1;
          V2:=1;
       end
       else begin
          V1:=X-ARR[k].X;
          V2:=ARR[i].X-ARR[k].X;
       end;

       Result:=Result* (V1) / (V2);
    end
  end;

var i:integer;
begin
  Result:=0;
  if (X<ARR[low(ARR)].X) then result:=ARR[low(ARR)].X
  else if  (X>ARR[HIGH(ARR)].X) then result:=ARR[high(ARR)].Y
  else
  for i:=low(ARR) to HIGH(ARR) do begin
     Result:=Result+ ARR[i].Y *coeff(X,i)
  end;
end;

function GetSplinesCoeff(ARR:ARRPoint):ARRSPLINESCOEFF;
var i:integer;
begin
  setlength(Result,HIGH(ARR));

  REsult[0].C1:=(ARR[1].y-ARR[0].y)/(ARR[1].x-ARR[0].x);

  for i:=0 to High(Arr)-1 do begin
     Result[i+1].C1:= 2*(ARR[i+1].y-arr[i].y) /
                      (Arr[i+1].x-arr[i].x) - Result[i].c1;
  end;

  for i:=0 to high(arr)-2 do begin
     Result[i].C2:=(REsult[i+1].C1-REsult[i].C1) /
                    (2 * (Arr[i+1].x-Arr[i].X)  );
  end;

  Result[High(Arr)-1].C2:=(Arr[high(Arr)].Y-Arr[high(Arr)-1].Y) /
                            sqr(Arr[high(Arr)].x-Arr[high(Arr)-1].x)
                          -Result[high(Arr)-1].C1 / (Arr[high(Arr)].x-Arr[high(Arr)-1].x);

end;


function bvInterpolationSplines(X:double;ARR:ARRPOINT;COEFF:ARRSPLINESCOEFF):double;
var i:integer;
begin

  Result:=0;

  if (X<ARR[0].X) then result:=ARR[0].X
  else if  (X>ARR[HIGH(ARR)].X) then result:=ARR[high(ARR)].Y
  else
  for i:=1 to HIGH(ARR) do begin
     if Arr[i].X>X then begin
         REsult:=Arr[i-1].Y+coeff[i-1].c1*(X-Arr[i-1].X)+coeff[i-1].c2*sqr(X-Arr[i-1].X);
         break;
     end;
  end;

end;


function FPOINT(X,Y:double):TFPoint;
begin
  REsult.X:=X;
  Result.Y:=Y;
end;

end.
