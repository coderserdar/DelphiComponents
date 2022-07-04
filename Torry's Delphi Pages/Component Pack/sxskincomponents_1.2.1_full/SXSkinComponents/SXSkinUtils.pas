unit SXSkinUtils;

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

uses Windows, Classes, Controls;

function WithoutAllSpaces(const S:String):String;
function GetFilePath(const S:String):String;
function IsRoot(S:String):Boolean;
function GetServerRoot(S:String):String;
function GetUpDir(S:String):String;
function GetFullPath(const Relative,Current:String):String;
function WithoutLastSlash(const S:String):String;
function WithLastSlash(const S:String):String;
function SXStrToFloatDef(S:String;Default:Real):Real;
procedure SXLStrCmp(const S1,S2:AnsiString);
function ValueFromIndex(SL:TStrings;Index:Integer):String;
procedure SetValueFromIndex(SL:TStrings;Index:Integer;const NewValue:String);
function PathIsRelative(const S:String):Boolean;
function GetRelativePath(Main,Changeable:String):String;
procedure SetComponentEnabled(A:TWinControl;C:Boolean);

procedure Save8Flags(Stream:TStream;F1,F2,F3,F4,F5,F6,F7,F8:Boolean);
procedure Load8Flags(Stream:TStream;out F1,F2,F3,F4,F5,F6,F7,F8:Boolean);
procedure Load8MaskedFlags(Stream:TStream;out F1,F2,F3,F4,F5,F6,F7,F8:Boolean;
           MF1,MF2,MF3,MF4,MF5,MF6,MF7,MF8:Boolean);
procedure SaveString(Stream:TStream;const S:String);
procedure LoadString(Stream:TStream;out S:String);
procedure SavePackedInteger(S:TStream;A:Integer);
procedure LoadPackedInteger(S:TStream;out A:Integer);
procedure SaveListToStream(Stream:TStream;T:TStringList);
procedure LoadListFromStream(Stream:TStream;T:TStringList);

function PtInRoundRect(PX,PY,X1,Y1,X2,Y2,R:Single):Boolean; overload;
function PtInRoundRect(PX,PY,X1,Y1,X2,Y2,R:Integer):Boolean; overload;
function PtInEllipse(PX,PY,X1,Y1,X2,Y2:Single):Boolean; overload;
function PtInEllipse(PX,PY,X1,Y1,X2,Y2:Integer):Boolean; overload;

procedure NormalizeWinPoint(var P:TPoint);

implementation

uses SysUtils, Math;

function WithoutAllSpaces(const S:String):String;
var A,B:Integer;
begin
 if S='' then
  begin
   Result:='';
   exit;
  end;
 SetLength(Result,length(S));
 A:=1; B:=0;
 while A<=length(S) do
  begin
   if not (S[A] in [' ',#10,#13,#9]) then
    begin
     Inc(B);
     Result[B]:=S[A];
    end;
   Inc(A);
  end;
 SetLength(Result,B);
end;

function GetFilePath(const S:String):String;
var A,B:Integer;
begin
 B:=0;
 for A:=1 to length(S) do
  begin
   if S[A]='?' then break;
   if (S[A]='/') or (S[A]='\') then B:=A;
  end;
 if B=0 then Result:='' else
  Result:=Copy(S,1,B);
end;

function GetServerRoot(S:String):String;
var A:Integer;
begin
 A:=Pos(':\',S);
 if A>0 then
  begin
   Result:=Copy(S,1,A+1);
   Delete(S,1,A+1);
  end else
   begin
    A:=Pos('://',S);
    if A>0 then
     begin
      Result:=Copy(S,1,A+2);
      Delete(S,1,A+2);
     end;
   end;
 while (S<>'') and (S[1]<>'/') and (S[1]<>'\') do
  begin
   Result:=Result+S[1];
   Delete(S,1,1);
  end;
end;

function IsRoot(S:String):Boolean;
var A:Integer;
begin
 A:=Pos(':\',S);
 if A>0 then Delete(S,1,A+1) else
  begin
   A:=Pos('://',S);
   if A>0 then Delete(S,1,A+2);
  end;
 Result:=(Pos('/',S)=0) and (Pos('\',S)=0);
end;

function GetUpDir(S:String):String;
begin
 S:=WithoutLastSlash(S);
 if IsRoot(S) then
  begin
   Result:=S;
   exit;
  end;
 while (S<>'') and (S[length(S)]<>'/') and (S[length(S)]<>'\') do
  Delete(S,length(S),1);
 S:=WithoutLastSlash(S);
 if S='' then S:='/';
 Result:=S;
end;

function GetFullPath(const Relative,Current:String):String; {Relative File Name; Full File Name}
var S1,S2,SS:String;
           A:Integer;
begin
 S1:=Current;
 S2:=Relative;
 A:=Pos('?',S2);
 if A>0 then
  begin
   SS:=Copy(S2,A,MaxInt);
   Delete(S2,A,MaxInt);
  end else SS:='';
 try
  if S2='' then S2:='/';
  if (Pos('//',S2)<>0) or
     (Pos('\\',S2)<>0) or
     (Pos(':\',S2)<>0) then
      begin
       Result:=S2;
       exit;
      end;
  S1:=GetFilePath(S1);
  if (S2[1]='/') or (S2[1]='\') or (Copy(S2,1,2)='./') or (Copy(S2,1,2)='.\') then
   begin
    if S2[1]='.' then Delete(S2,1,1);
    S1:=GetServerRoot(S1);
    Result:=S1+S2;
   end else
    begin
     while (Pos('../',S2)=1) or (Pos('..\',S2)=1) do
      begin
       S1:=GetUpDir(S1);
       Delete(S2,1,3);
      end;
     Result:=WithLastSlash(S1)+S2;
    end;
 finally
  Result:=Result+SS
 end;
end;

function WithoutLastSlash(const S:String):String;
begin
 Result:=S;
 if (Result<>'') and (Result[length(Result)] in ['/','\']) then
  Delete(Result,length(S),1);
end;

function WithLastSlash(const S:String):String;
begin
 if S='' then
  begin
   Result:='/';
   exit;
  end;
 Result:=S;
 if (Result[length(Result)]<>'/') and (Result[length(Result)]<>'\') then
  begin
   if Pos('\',S)<>0 then Result:=Result+'\' else
    Result:=Result+'/';
  end;
end;

function SXStrToFloatDef(S:String;Default:Real):Real;
var A:Integer;
begin
 A:=Pos(',',S);
 if A<>0 then S[A]:='.';
 {$R-}Val(S,Result,A);{$R+}
 if A<>0 then
  Result:=Default;
end;

procedure SXLStrCmp(const S1,S2:AnsiString);
type
  PStrRec = ^StrRec;
  StrRec = packed record
    refCnt: Longint;
    length: Longint;
  end;
const
  skew = SizeOf(StrRec);
asm
        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
        MOV     ESI,EAX
        MOV     EDI,EDX
        CMP     EAX,EDX
        JE      @@exit
        TEST    ESI,ESI
        JE      @@str1null
        TEST    EDI,EDI
        JE      @@str2null
        MOV     EAX,[ESI-skew].StrRec.length
        MOV     EDX,[EDI-skew].StrRec.length
        SUB     EAX,EDX { eax = len1 - len2 }
        JA      @@skip1
        ADD     EDX,EAX { edx = len2 + (len1 - len2) = len1     }
@@skip1:
        PUSH    EDX
        SHR     EDX,2
        JE      @@cmpRest
@@longLoop:
        MOV     ECX,[ESI]
        MOV     EBX,[EDI]
        CMP     ECX,EBX
        JNE     @@misMatch
        DEC     EDX
        JE      @@cmpRestP4
        MOV     ECX,[ESI+4]
        MOV     EBX,[EDI+4]
        CMP     ECX,EBX
        JNE     @@misMatch
        ADD     ESI,8
        ADD     EDI,8
        DEC     EDX
        JNE     @@longLoop
        JMP     @@cmpRest
@@cmpRestP4:
        ADD     ESI,4
        ADD     EDI,4
@@cmpRest:
        POP     EDX
        AND     EDX,3
        JE      @@equal
        MOV     ECX,[ESI]
        MOV     EBX,[EDI]
        CMP     CL,BL
        JNE     @@exit
        DEC     EDX
        JE      @@equal
        CMP     CH,BH
        JNE     @@exit
        DEC     EDX
        JE      @@equal
        AND     EBX,$00FF0000
        AND     ECX,$00FF0000
        CMP     ECX,EBX
        JNE     @@exit
@@equal:
        ADD     EAX,EAX
        JMP     @@exit
@@str1null:
        MOV     EDX,[EDI-skew].StrRec.length
        SUB     EAX,EDX
        JMP     @@exit
@@str2null:
        MOV     EAX,[ESI-skew].StrRec.length
        SUB     EAX,EDX
        JMP     @@exit
@@misMatch:
        POP     EDX
        CMP     CL,BL
        JNE     @@exit
        CMP     CH,BH
        JNE     @@exit
        SHR     ECX,16
        SHR     EBX,16
        CMP     CL,BL
        JNE     @@exit
        CMP     CH,BH

@@exit:
        POP     EDI
        POP     ESI
        POP     EBX
end;

function ValueFromIndex(SL:TStrings;Index:Integer):String;
{$IFNDEF COMPILER_9_UP}
var A:Integer;
{$ENDIF}
begin
 {$IFNDEF COMPILER_9_UP}
 Result:='';
 if (Index<0) or (Index>=SL.Count) then exit;
 A:=Pos('=',SL[Index]);
 if A=0 then exit;
 Result:=Copy(SL[Index],A+1,MaxInt);
 {$ELSE}
 Result:=SL.ValueFromIndex[Index];
 {$ENDIF}
end;

procedure SetValueFromIndex(SL:TStrings;Index:Integer;const NewValue:String);
{$IFNDEF COMPILER_9_UP}
var A:Integer;
{$ENDIF}
begin
 {$IFNDEF COMPILER_9_UP}
 if (Index<0) or (Index>=SL.Count) then exit;
 A:=Pos('=',SL[Index]);
 if A=0 then
  begin
   SL[Index]:=SL[Index]+'='+NewValue;
   exit;
  end;
 SL[Index]:=Copy(SL[Index],1,A)+NewValue;
 {$ELSE}
 SL.ValueFromIndex[Index]:=NewValue;
 {$ENDIF}
end;

function PathIsRelative(const S:String):Boolean;
begin
 Result:=True;
 if (Pos(':\',S)<>0) or (Pos(':/',S)<>0) then
  Result:=False;
end;

function GetRelativePath(Main,Changeable:String):String;  //Creates Link from Main to Changeable
var SS,S1,S2:String;
         A,B:Integer;
begin
 SS:=Changeable;
 try
  if not PathIsRelative(SS) then
   begin
    A:=Pos('://',Main);
    B:=Pos('://',Changeable);
    if (A<>0) and (A=B) then
     begin
      Delete(Main,1,A+2);
      Delete(Changeable,1,A+2);
      //Comparison of Servers
      A:=Pos('/',Main);
      if A=0 then
       begin
        S1:=Main;
        Main:='';
       end else
        begin
         S1:=Copy(Main,1,A-1);
         Delete(Main,1,A);
        end;
      A:=Pos('/',Changeable);
      if A=0 then
       begin
        S2:=Changeable;
        Changeable:='/';
       end else
        begin
         S2:=Copy(Changeable,1,A-1);
         Delete(Changeable,1,A);
        end;
      if S1<>S2 then exit; //Servers are Different
      if Main='' then  //Main is Server Root
       begin
        SS:=Changeable;
        exit;
       end;
      repeat
       A:=Pos('/',Main);
       B:=Pos('/',Changeable);
       if (A<>0) and (B<>0) then
        begin
         S1:=Copy(Main,1,A-1);
         S2:=Copy(Changeable,1,B-1);
         if S1<>S2 then break else
          begin
           Delete(Main,1,A);
           Delete(Changeable,1,A);
          end;
        end;
      until (A=0) or (B=0);
      if A=0 then
       begin
        SS:=Changeable;
        exit;
       end;
      //B=0 or S1<>S2
      SS:='';
      repeat
       A:=Pos('/',Main);
       if A<>0 then
        begin
         SS:=SS+'../';
         Delete(Main,1,A);
        end;
      until A=0;
      SS:=SS+Changeable;
      exit;
     end else
      if A<>B then exit;  //Different Protos
    A:=Pos(':\',Main);
    B:=Pos(':\',Changeable);
    if (A<>0) and (A=B) then
     begin
      if SameText(Copy(Main,1,A-1),Copy(Changeable,1,A-1)) then
       begin
        Delete(Main,1,A+1);
        Delete(Changeable,1,A+1);
        if Main='' then  //Main is Drive Root
         begin
          SS:=Changeable;
          exit;
         end;
        repeat
         A:=Pos('\',Main);
         B:=Pos('\',Changeable);
         if (A<>0) and (B<>0) then
          begin
           S1:=Copy(Main,1,A-1);
           S2:=Copy(Changeable,1,B-1);
           if S1<>S2 then break else
            begin
             Delete(Main,1,A);
             Delete(Changeable,1,A);
            end;
          end;
        until (A=0) or (B=0);
        if A=0 then
         begin
          SS:=Changeable;
          exit;
         end;
        //B=0 or S1<>S2
        SS:='';
        repeat
         A:=Pos('\',Main);
         if A<>0 then
          begin
           SS:=SS+'..\';
           Delete(Main,1,A);
          end;
        until A=0;
        SS:=SS+Changeable;
        exit;
       end else Result:=Changeable;
     end else
      if A<>B then exit;  //Different Drives
   end;
 finally
  Result:=SS;
 end;
end;

procedure SetComponentEnabled(A:TWinControl;C:Boolean);
var B:Integer;
begin
 A.Enabled:=C;
 for B:=0 to A.ControlCount-1 do
  if A.Controls[B] is TWinControl then
   SetComponentEnabled(A.Controls[B] as TWinControl,C) else
    A.Controls[B].Enabled:=C;
end;

procedure Save8Flags(Stream:TStream;F1,F2,F3,F4,F5,F6,F7,F8:Boolean);
var B:Byte;
begin
 B:=0;
 if F1 then B:=B or $01;
 if F2 then B:=B or $02;
 if F3 then B:=B or $04;
 if F4 then B:=B or $08;
 if F5 then B:=B or $10;
 if F6 then B:=B or $20;
 if F7 then B:=B or $40;
 if F8 then B:=B or $80;
 Stream.Write(B,1);
end;

procedure Load8Flags(Stream:TStream;out F1,F2,F3,F4,F5,F6,F7,F8:Boolean);
var B:Byte;
begin
 Stream.Read(B,1);
 F1:=B and $01<>0;
 F2:=B and $02<>0;
 F3:=B and $04<>0;
 F4:=B and $08<>0;
 F5:=B and $10<>0;
 F6:=B and $20<>0;
 F7:=B and $40<>0;
 F8:=B and $80<>0;
end;

procedure Load8MaskedFlags(Stream:TStream;out F1,F2,F3,F4,F5,F6,F7,F8:Boolean;
           MF1,MF2,MF3,MF4,MF5,MF6,MF7,MF8:Boolean);
var B:Byte;
begin
 Stream.Read(B,1);
 if MF1 then F1:=B and $01<>0;
 if MF2 then F2:=B and $02<>0;
 if MF3 then F3:=B and $04<>0;
 if MF4 then F4:=B and $08<>0;
 if MF5 then F5:=B and $10<>0;
 if MF6 then F6:=B and $20<>0;
 if MF7 then F7:=B and $40<>0;
 if MF8 then F8:=B and $80<>0;
end;

procedure SaveString(Stream:TStream;const S:String);
var B:Byte;
    W:Word;
begin
 if length(S)<255 then
  begin
   B:=length(S);
   Stream.Write(B,sizeof(B));
   if B<>0 then Stream.Write(S[1],B);
  end else
   begin
    B:=255;
    Stream.Write(B,sizeof(B));
    W:=length(S);
    Stream.Write(W,sizeof(W));
    if W<>0 then Stream.Write(S[1],W);
   end;
end;

procedure LoadString(Stream:TStream;out S:String);
var B:Byte;
    W:Word;
begin
 if Stream.Read(B,sizeof(B))<sizeof(B) then raise EInOutError.Create('');
 if B<>255 then
  begin
   SetLength(S,B);
   if B>0 then Stream.Read(S[1],B);
  end else
   begin
    Stream.Read(W,sizeof(W));
    SetLength(S,W);
    Stream.Read(S[1],W);
   end;
end;

procedure SavePackedInteger(S:TStream;A:Integer);
var B:Byte;
begin
 if (A>=0) and (A<255) then
  begin
   B:=A;
   S.Write(B,sizeof(B));
   exit;
  end;
 B:=255;
 S.Write(B,sizeof(B));
 S.Write(A,sizeof(A));
end;

procedure LoadPackedInteger(S:TStream;out A:Integer);
var B:Byte;
begin
 if S.Read(B,sizeof(B))<>1 then raise EInOutError.Create('');
 if B<255 then A:=B else
  S.Read(A,sizeof(A));
end;

procedure SaveListToStream(Stream:TStream;T:TStringList);
var A:Integer;
begin
 SavePackedInteger(Stream,T.Count);
 for A:=0 to T.Count-1 do
  SaveString(Stream,T[A]);
end;

procedure LoadListFromStream(Stream:TStream;T:TStringList);
var A,C:Integer;
      S:String;
begin
 T.Clear;
 LoadPackedInteger(Stream,C);
 for A:=0 to C-1 do
  begin
   LoadString(Stream,S);
   T.Add(S);
  end;
end;

function PtInRoundRect(PX,PY,X1,Y1,X2,Y2,R:Single):Boolean;
var C,L:Single;
begin
 if (PY<Y1) or (PY>=Y2) or (PX<X1) or (PX>=X2) then
  begin
   Result:=False;
   exit;
  end;
 if PY<Y1+R then
  begin
   L:=PY-Y1+0.5;
   C:=R-Sqrt(L*(2*R-L));
   Result:=(PX>=round(X1+C)) and (PX<=round(X2-C));
   exit;
  end;
 if PY<=Y2-R then
  begin
   Result:=(PX>=X1) and (PX<X2);
   exit;
  end;
 L:=Y2-PY-0.5;
 C:=R-Sqrt(L*(2*R-L));
 Result:=(PX>=round(X1+C)) and (PX<round(X2-C));
end;

function PtInRoundRect(PX,PY,X1,Y1,X2,Y2,R:Integer):Boolean;
var C,L:Single;
begin
 if (PY<Y1) or (PY>=Y2) or (PX<X1) or (PX>=X2) then
  begin
   Result:=False;
   exit;
  end;
 if PY<Y1+R then
  begin
   L:=PY-Y1+0.5;
   C:=R-Sqrt(L*(2*R-L));
   Result:=(PX>=round(X1+C)) and (PX<=round(X2-C));
   exit;
  end;
 if PY<=Y2-R then
  begin
   Result:=(PX>=X1) and (PX<X2);
   exit;
  end;
 L:=Y2-PY-0.5;
 C:=R-Sqrt(L*(2*R-L));
 Result:=(PX>=round(X1+C)) and (PX<round(X2-C));
end;

function PtInEllipse(PX,PY,X1,Y1,X2,Y2:Single):Boolean;
var R1,R2,L:Single;
begin
 if (PY<Y1) or (PY>=Y2) or (PX<X1) or (PX>=X2) then
  begin
   Result:=False;
   exit;
  end;
 R1:=(X2-X1-1)/2; R2:=(Y2-Y1-1)/2;
 if PY-Y1<R2 then
  begin
   L:=PY-Y1+0.5;
   L:=(R2-Sqrt(L*(2*R2-L)))*R1/R2;
  end else
 if PY>Y2-R2 then
  begin
   L:=Y2-PY-0.5;
   L:=(R2-Sqrt(L*(2*R2-L)))*R1/R2;
  end else L:=0;
 Result:=(PX>=round(L+X1)) and (PX<round(X2-L));
end;

function PtInEllipse(PX,PY,X1,Y1,X2,Y2:Integer):Boolean;
var R1,R2,L:Single;
begin
 if (PY<Y1) or (PY>=Y2) or (PX<X1) or (PX>=X2) then
  begin
   Result:=False;
   exit;
  end;
 R1:=(X2-X1-1)/2; R2:=(Y2-Y1-1)/2;
 if PY-Y1<R2 then
  begin
   L:=PY-Y1+0.5;
   L:=(R2-Sqrt(L*(2*R2-L)))*R1/R2;
  end else
 if PY>Y2-R2 then
  begin
   L:=Y2-PY-0.5;
   L:=(R2-Sqrt(L*(2*R2-L)))*R1/R2;
  end else L:=0;
 Result:=(PX>=round(L)+X1) and (PX<X2-round(L));
end;

procedure NormalizeWinPoint(var P:TPoint);
begin
 if P.X>32000 then P.X:=P.X-65536;
 if P.Y>32000 then P.Y:=P.Y-65536;
end;

end.
