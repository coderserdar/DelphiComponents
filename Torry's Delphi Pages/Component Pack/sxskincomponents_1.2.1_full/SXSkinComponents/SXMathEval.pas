unit SXMathEval;

////////////////////////////////////////////////////////////////////////////////
// SXSkinComponents: Skinnable Visual Controls for Delphi and C++Builder      //
//----------------------------------------------------------------------------//
// Version: 1.2.1                                                             //
// Author: Alexey Sadovnikov                                                  //
// Web Site: http://www.saarixx.info/sxskincomponents/                        //
// E-Mail: sxskincomponents@saarixx.info                                      //
//----------------------------------------------------------------------------//
// LICENSE:                                                                   //
// 1. You may freely distribute this file.                                    //
// 2. You may not make any changes to this file.                              //
// 3. The only person who may change this file is Alexey Sadovnikov.          //
// 4. You may use this file in your freeware projects.                        //
// 5. If you want to use this file in your shareware or commercial project,   //
//    you should purchase a project license or a personal license of          //
//    SXSkinComponents: http://saarixx.info/sxskincomponents/en/purchase.htm  //
// 6. You may freely use, distribute and modify skins for SXSkinComponents.   //
// 7. You may create skins for SXSkinComponents.                              //
//----------------------------------------------------------------------------//
// Copyright (C) 2006-2007, Alexey Sadovnikov. All Rights Reserved.           //
////////////////////////////////////////////////////////////////////////////////

interface

{$I Compilers.inc}

uses Windows, SysUtils, Math;

type

 TSXOnGetVariable=function(const VarName:String;var Error:Boolean):Single of object;

function SXEvalMathString(const S:String;OnGetVar:TSXOnGetVariable;var Error:Boolean):Single;

implementation

function EvalNum(S:String;OnGetVar:TSXOnGetVariable;var Error:Boolean):Single;
begin
 if (S<>'') and (S[1]='!') then
  begin
   Result:=EvalNum(Copy(S,2,MaxInt),OnGetVar,Error);
   if Error then exit;
   if Result=0 then Result:=1 else Result:=0;
   exit;
  end;
 if (S<>'') and (S[1]='~') then S[1]:='-';
 Error:=False;
 if TryStrToFloat(S,Result) then exit;
 Result:=OnGetVar(S,Error);
end;

function EvalSqrt(const S:String;OnGetVar:TSXOnGetVariable;var Error:Boolean):Single;
begin
 if (S<>'') and (S[1]='$') then
  begin
   Result:=EvalSqrt(Copy(S,2,MaxInt),OnGetVar,Error);
   if Error then
    begin
     Result:=0;
     exit;
    end;
   Result:=Sqrt(Result);
  end else Result:=EvalNum(S,OnGetVar,Error);
end;

function EvalPower(const S:String;OnGetVar:TSXOnGetVariable;var Error:Boolean):Single;
var  A:Integer;
 R1,R2:Single;
begin
 A:=length(S);
 while (A>=1) and not (S[A]='^') do Dec(A);
 if A=0 then
  begin
   Result:=EvalSqrt(S,OnGetVar,Error);
   exit;
  end;
 R1:=EvalPower(Copy(S,1,A-1),OnGetVar,Error);
 if Error then
  begin
   Result:=0;
   exit;
  end;
 R2:=EvalSqrt(Copy(S,A+1,MaxInt),OnGetVar,Error);
 if Error then
  begin
   Result:=0;
   exit;
  end;
 Result:=Power(R1,R2);
end;

function EvalMod(const S:String;OnGetVar:TSXOnGetVariable;var Error:Boolean):Single;
var  A:Integer;
 R1,R2:Single;
begin
 A:=length(S);
 while (A>=1) and not (S[A]='%') do Dec(A);
 if A=0 then
  begin
   Result:=EvalPower(S,OnGetVar,Error);
   exit;
  end;
 R1:=EvalMod(Copy(S,1,A-1),OnGetVar,Error);
 if Error then
  begin
   Result:=0;
   exit;
  end;
 R2:=EvalPower(Copy(S,A+1,MaxInt),OnGetVar,Error);
 if Error then
  begin
   Result:=0;
   exit;
  end;
 if round(R2)=0 then
  begin
   Error:=True;
   Result:=0;
   exit;
  end;
 try
  Result:=round(R1) mod round(R2);
 except
  Result:=0;
  Error:=True;
 end;
end;

function EvalMulDiv(const S:String;OnGetVar:TSXOnGetVariable;var Error:Boolean):Single;
var  A:Integer;
   Mul:Boolean;
  RDiv:Boolean;
 R1,R2:Single;
begin
 A:=length(S);
 while (A>=1) and not (S[A] in ['*','/','|']) do Dec(A);
 if A=0 then
  begin
   Result:=EvalMod(S,OnGetVar,Error);
   exit;
  end;
 Mul:=S[A]='*';
 RDiv:=S[A]='|';
 R1:=EvalMulDiv(Copy(S,1,A-1),OnGetVar,Error);
 if Error then
  begin
   Result:=0;
   exit;
  end;
 R2:=EvalMod(Copy(S,A+1,MaxInt),OnGetVar,Error);
 if Error then
  begin
   Result:=0;
   exit;
  end;
 if Mul then Result:=R1*R2 else
  if RDiv then
   begin
    if round(R2)=0 then
     begin
      Error:=True;
      Result:=0;
      exit;
     end;
    try
     Result:=round(R1) div round(R2);
    except
     Result:=0;
     Error:=True;
    end;
   end else
    begin
     if R2=0 then
      begin
       Error:=True;
       Result:=0;
       exit;
      end;
     try
      Result:=R1/R2;
     except
      Result:=0;
      Error:=True;
     end;
    end;
end;

function EvalPlusMinus(const S:String;OnGetVar:TSXOnGetVariable;var Error:Boolean):Single;
var  A:Integer;
  Diff:Boolean;
 R1,R2:Single;
begin
 if (S<>'') and (S[1]='-') then
  begin
   Result:=EvalPlusMinus('0'+S,OnGetVar,Error);
   exit;
  end;
 A:=length(S);
 while (A>=1) and not (S[A] in ['+','-']) do Dec(A);
 if A=0 then
  begin
   Result:=EvalMulDiv(S,OnGetVar,Error);
   exit;
  end;
 Diff:=S[A]='-';
 R1:=EvalPlusMinus(Copy(S,1,A-1),OnGetVar,Error);
 if Error then
  begin
   Result:=0;
   exit;
  end;
 R2:=EvalMulDiv(Copy(S,A+1,MaxInt),OnGetVar,Error);
 if Error then
  begin
   Result:=0;
   exit;
  end;
 try
  if Diff then Result:=R1-R2 else
   Result:=R1+R2;
 except
  Result:=0;
  Error:=True;
 end;
end;

function EvalComparison(const S:String;OnGetVar:TSXOnGetVariable;var Error:Boolean):Single;
var  A:Integer;
    Op:(opEqual,opLarger,opLess,opLargerEqual,opLessEqual,opNotEqual);
 SetOp:Boolean;
 S1,S2:String;
 V1,V2:Single;
begin
 A:=length(S);
 while (A>=1) and not (S[A] in ['=','>','<']) do Dec(A);
 if A=0 then
  begin
   Result:=EvalPlusMinus(S,OnGetVar,Error);
   exit;
  end;
 Op:=opEqual; 
 SetOp:=False;
 if (A>1) and (S[A]='=') then
  begin
   SetOp:=True;
   case S[A-1] of
    '!': Op:=opNotEqual;
    '>': Op:=opLargerEqual;
    '<': Op:=opLessEqual;
    else SetOp:=False;
   end;
   if SetOp then
    begin
     S1:=Copy(S,1,A-2);
     S2:=Copy(S,A+1,MaxInt);
    end;
  end;
 if not SetOp then
  begin
   case S[A] of
    '=': Op:=opEqual;
    '>': Op:=opLarger;
    '<': Op:=opLess;
   end;
   S1:=Copy(S,1,A-1);
   S2:=Copy(S,A+1,MaxInt);
  end;
 V1:=EvalPlusMinus(S1,OnGetVar,Error);
 if Error then
  begin
   Result:=0;
   exit;
  end;
 V2:=EvalPlusMinus(S2,OnGetVar,Error);
 if Error then
  begin
   Result:=0;
   exit;
  end;
 case Op of
  opEqual:       if SameValue(V1,V2) then Result:=1 else Result:=0;
  opLarger:      if V1>V2 then Result:=1 else Result:=0;
  opLess:        if V1<V2 then Result:=1 else Result:=0;
  opLargerEqual: if (V1>V2) or SameValue(V1,V2) then Result:=1 else Result:=0;
  opLessEqual:   if (V1<V2) or SameValue(V1,V2) then Result:=1 else Result:=0;
  opNotEqual:    if SameValue(V1,V2) then Result:=0 else Result:=1;
  else           Result:=0;
 end;
end;

function EvalLogical(const S:String;OnGetVar:TSXOnGetVariable;var Error:Boolean):Single;
var A,B:Integer;
  IsAnd:Boolean;
  V1,V2:Single;
begin
 A:=0;
 IsAnd:=False;
 for B:=1 to length(S)-1 do
  begin
   if (S[B]='&') and (S[B+1]='&') then
    begin
     A:=B;
     IsAnd:=True;
     break;
    end;
   if (S[B]='|') and (S[B+1]='|') then
    begin
     A:=B;
     break;
    end;
  end;
 if A=0 then
  begin
   Result:=EvalComparison(S,OnGetVar,Error);
   exit;
  end;
 V1:=EvalComparison(Copy(S,1,A-1),OnGetVar,Error);
 if Error then
  begin
   Result:=0;
   exit;
  end;
 if IsAnd and (V1=0) then
  begin
   Result:=0;
   exit;
  end;
 if not IsAnd and (V1=1) then
  begin
   Result:=1;
   exit;
  end;
 V2:=EvalComparison(Copy(S,A+2,MaxInt),OnGetVar,Error);
 if Error then
  begin
   Result:=0;
   exit;
  end;
 if V2<>0 then Result:=1 else Result:=0;
end;

function EvalAllParenth(S:String;OnGetVar:TSXOnGetVariable;var Error:Boolean):Single;
var A,B,C:Integer;
begin
 repeat
  A:=Pos('(',S);
  if A>0 then
   begin
    C:=1;
    B:=A;
    repeat
     Inc(B);
     if B<=length(S) then
      begin
       case S[B] of
        '(': Inc(C);
        ')': Dec(C);
       end;
      end;
    until (B>length(S)) or (C=0);
    if C>0 then
     begin
      Result:=0;
      Error:=True;
      exit;
     end;
    S:=Copy(S,1,A-1)+FloatToStr(EvalAllParenth(Copy(S,A+1,B-A-1),OnGetVar,Error))+Copy(S,B+1,MaxInt);
    if (length(S)>=A) and (S[A]='-') then S[A]:='~';
    if Error then
     begin
      Result:=0;
      exit;
     end;
   end;
 until A=0;
 Result:=EvalLogical(S,OnGetVar,Error);
end;

function SXEvalMathString(const S:String;OnGetVar:TSXOnGetVariable;var Error:Boolean):Single;
var A:Integer;
begin
 for A:=1 to length(S) do
  if S[A] in ['$','-','+','*','/','|','%','(',')','^','=','!','>','<'] then
   begin
    Result:=EvalAllParenth(S,OnGetVar,Error);
    exit;
   end;
 Result:=EvalNum(S,OnGetVar,Error);
end;

end.
