unit Md5unit;
{*****************************************************************************
 UNIT: MD5Unit
 Description:  This unit contains an Object Pascal Object which can be used to
               perform an MD5 Hashing of byte array, file, or Pascal String.  An
               MD5 Hashing or Message Digest is a 'finger print' of the
               input. This is 100% PASCAL!!!

               "It is conjectured that it is computationally infeasible
               to produce two messages having the same message digest"....
               "The MD5 algorithm is intended for digital signature
               applications, where a large file must be "compressed" in a
               secure manner before being encrypted with a private (secret) key
               under a public-key cryptosystem such as RSA." R. Rivest
               RfC: 1321, RSA Data Security, Inc. April 1992

 The MD5 Algorithm was produced by RSA Data Security Inc.(See LEGAL)
 -----------------------------------------------------------------------------
 Code Author:  Greg Carter, gregc@cryptocard.com
 Organization: CRYPTOCard Corporation, info@cryptocard.com
               R&D Division, Carleton Place, ON, CANADA, K7C 3T2
               1-613-253-3152 Voice, 1-613-253-4685 Fax.
 Date of V.1:  Jan. 3 1996.

 Compatibility & Testing with BP7.0: Anne Marcel Roorda, garfield@xs4all.nl
 -----------------------------------------------------------------------------}
 {Useage:  Below is typical usage(for File)of the MD5 Object, Follow these steps:
	Step 1: Declare and Create a New TMD5 object.  This can be done by
                'Drag N Drop' a TMD5 off the Delphi Tool Pallet,
		or explicitly in code.
	Step 2: Set the InputType.
	Step 3: Point to the input(InputString, InputFilePath, pInputArray).
	Step 4: Point to the output Array(pOutputArray).
	Step 5: Call the MD5_Hash procedure.
		Your Done!

Example
procedure Tcryptfrm.Button1Click(Sender: TObject);
var
 md5hash: TMD5;                  (* Step 1a *)
 outarray: array[0..15] of char;
 InputFile: File;
 startTime: LongInt;
begin
 md5hash := TMD5.Create(Self);   (* Step 1b *)
 try
  If OpenDialog1.Execute then
  begin
    md5hash.InputType := SourceFile;  (* Step 2 *)
    md5hash.InputFilePath := OpenDialog1.FileName; (* Step 3 *)
    md5hash.pOutputArray := @outarray;             (* Step 4 *)
    startTime := timeGetTime;
    md5hash.MD5_Hash;                              (* Step 5 *)
    LEDLabel1.Caption := IntToStr(timeGetTime - startTime);
    Label2.Caption := StrPas(outarray);     (* Do something with output *)
  end;(* if *)
 finally
  md5hash.free;
 end;
end;
{-----------------------------------------------------------------------------}
{LEGAL:        The algorithm was placed into the public domain, hence requires
               no license or runtime fees.  However this code is copyright by
               CRYPTOCard.  CRYPTOCard grants anyone who may wish to use, modify
               or redistribute this code privileges to do so, provided the user
               agrees to the following three(3) rules:

               1)Any Applications, (ie exes which make use of this
               Object...), for-profit or non-profit,
               must acknowledge the author of this Object(ie.
               MD5 Implementation provided by Greg Carter, CRYPTOCard
               Corporation) somewhere in the accompanying Application
               documentation(ie AboutBox, HelpFile, readme...).  NO runtime
               or licensing fees are required!

               2)Any Developer Component(ie Delphi Component, Visual Basic VBX,
               DLL) derived from this software must acknowledge that it is
               derived from "MD5 Object Pascal Implementation Originated by
               Greg Carter, CRYPTOCard Corporation 1996". Also all efforts should
               be made to point out any changes from the original.
               !!!!!Further, any Developer Components based on this code
               *MAY NOT* be sold for profit.  This Object was placed into the
               public domain, and therefore any derived components should
               also.!!!!!

               3)CRYPTOCard Corporation makes no representations concerning this
               software or the suitability of this software for any particular
               purpose. It is provided "as is" without express or implied
               warranty of any kind. CRYPTOCard accepts no liability from any
               loss or damage as a result of using this software.

CRYPTOCard Corporation is in no way affiliated with RSA Data Security Inc.
The MD5 Algorithm was produced by RSA Data Security Inc.
-----------------------------------------------------------------------------
Why Use this instead of a freely available C DLL?

The goal was to provide a number of Encryption/Hash implementations in Object
Pascal, so that the Pascal Developer has considerably more freedom.  These
Implementations are geared toward the PC(Intel) Microsoft Windows developer,
who will be using Borland's New 32bit developement environment(Delphi32).  The
code generated by this new compiler is considerablely faster then 16bit versions.
And should provide the Developer with faster implementations then those using
C DLLs.
-----------------------------------------------------------------------------
NOTES: Version 1 does not contain any cross-platform considerations.  If trying
       to use this code on a Big Endian style processor you will need to write
       additional code to reorder the bytes.
------------------------------------------------------------------------------
Revised:  00/00/00 BY: ******* Reason: ******
------------------------------------------------------------------------------
}
{Declare the compiler defines}
{$I CRYPTDEF.INC}
{------Changeable compiler switches-----------------------------------}
{$A+   Word align variables }
{$F+   Force Far calls }
{$K+   Use smart callbacks
{$N+   Allow coprocessor instructions }
{$P+   Open parameters enabled }
{$S+   Stack checking }
{$T-   @ operator is NOT typed }
{$U-   Non Pentium safe FDIV }
{$Z-   No automatic word-sized enumerations}
{---------------------------------------------------------------------}

interface
uses Cryptcon, SysUtils{$IFDEF DELPHI}, Classes, Controls{$ENDIF}
     {$IFDEF BP7},objects{$ENDIF};

Type
ULONG32 = record
 LoWord16: WORD;
 HiWord16: WORD;
end;

PULONG32 = ^ULONG32;
PLong = ^LongInt;

hashDigest = record
  A: Longint;
  B: Longint;
  C: Longint;
  D: Longint;
end;{hashArray}

PTR_Hash = ^hashDigest;

{$IFDEF DELPHI}
 TMD5 = class(TComponent)
 Private
 { Private declarations }
{$ENDIF}

{$IFDEF BP7}
 PTMD5 = ^TMD5; {For BP7 Objects}
 TMD5 = object(TObject)
 Public             {Since BP7 doesn't support Properties, we make these Public}
{$ENDIF}

  FType : TSourceType;                     {Source type, whether its a file or ByteArray, or
                                            a Pascal String}
  FInputFilePath: String;                  {Full Path to Input File}
  FInputArray: PByte;                      {Point to input array}
  FInputString: String;                    {Input String}
  FOutputDigest: PTR_Hash;                 {output MD5 Digest}
  FSourceLength: LongInt;                  {input length in BYTES}
  FActiveBlock: Array[0..15] of LongInt;   {the 64Byte block being transformed}
  FA, FB, FC, FD, FAA, FBB, FCC, FDD: LongInt;
  {FA..FDD are used during Step 4, the transform.  I made them part of the
   Object to cut down on time used to pass variables.}
  FpA, FpB, FpC, FpD: PLong;
  {FIXME! do we need these, or just use the '@' operator?}
  {Put in for readability}
  {FF, GG, HH, II are used in Step 4, the transform}
  Procedure FF(a, b, c, d, x: Pointer; s: BYTE; ac: Longint);
  Procedure GG(a, b, c, d, x: Pointer; s: BYTE; ac: Longint);
  Procedure HH(a, b, c, d, x: Pointer; s: BYTE; ac: Longint);
  Procedure II(a, b, c, d, x: Pointer; s: BYTE; ac: Longint);

{$IFDEF DELPHI}
 protected
    { Protected declarations }
{$ENDIF}
 public
    { Public declarations }
  {Initialize is used in Step 3, this fills FA..FD with init. values
   and points FpA..FpD to FA..FD}
  Procedure MD5_Initialize;
  {this is where all the magic happens}
  Procedure MD5_Transform;
  Procedure MD5_Finish;
  Procedure MD5_Hash_Bytes;
{  Procedure MD5_Hash_String;(Pascal Style strings???)}
  Procedure MD5_Hash_File;
  {This procedure sends the data 64Bytes at a time to MD5_Transform}
  Procedure MD5_Hash;
{$IFDEF DELPHI}
  Property pInputArray: PByte read FInputArray write FInputArray;
  Property pOutputArray: PTR_Hash read FOutputDigest write FOutputDigest;{!!See FOutputArray}
 Published
  Property InputType: TSourceType read FType write FType;
  Property InputFilePath: String read FInputFilePath write FInputFilePath;
  Property InputString: String read FInputString write FInputString;
  Property InputLength: LongInt read FSourceLength write FSourceLength;
{$ENDIF}
end;{TMD5}

{$IFDEF DELPHI}
 procedure Register;{register the component to the Delphi toolbar}
{$ENDIF}

Const
{Constants for MD5Transform routine.}
 S11 = 7;
 S12 = 12;
 S13 = 17;
 S14 = 22;
 S21 = 5;
 S22 = 9;
 S23 = 14;
 S24 = 20;
 S31 = 4;
 S32 = 11;
 S33 = 16;
 S34 = 23;
 S41 = 6;
 S42 = 10;
 S43 = 15;
 S44 = 21;

implementation

{This will only work on an intel}
{$IFDEF i286}
Function ROL(A: Longint; Amount: BYTE): Longint;NEAR;
begin
  inline(
  $8A/$4E/$04/        { mov cl,[bp+4]  }
  $66/$8B/$46/$06/    { mov eax,[bp+6] }
  $66/$D3/$C0/        { rol eax,cl     }
  $66/$89/$46/$FC     { mov [bp-4],eax }
  );
end;
{$ENDIF}

{$IFDEF i386}
Function ROL(A: Longint; Amount: BYTE): Longint; Assembler;
asm
 mov cl, Amount
 rol eax, cl
end;
{$ENDIF}

{$IFDEF DELPHI}
procedure Register;
  {Registers the Component to the toobar, on the tab named 'Crypto'}
  {Now all a Delphi programmer needs to do is drag n drop to have
   Blowfish encryption}
begin
  RegisterComponents('Crypto', [TMD5]);
end;
{$ENDIF}
Procedure TMD5.MD5_Initialize;
var
 a, b, c, d: LongInt;
begin
 a := $67452301; b:=$efcdab89; c:=$98badcfe; d:=$10325476;
 Move(a, FA, 4); FpA := @FA;
 Move(b, FB, 4); FpB := @FB;
 Move(c, FC, 4); FpC := @FC;
 Move(d, FD, 4); FpD := @FD;
end;{MD5_Initialize}

Procedure TMD5.FF(a, b, c, d, x: Pointer; s: BYTE; ac: Longint);
{Purpose:  Round 1 of the Transform.
           Equivalent to a = b + ((a + F(b,c,d) + x + ac) <<< s)
           Where F(b,c,d) = b And c Or Not(b) And d
}
var
 Fret: LongInt;
begin
 Fret := ((PLong(b)^) And (PLong(c)^)) Or ((Not(PLong(b)^)) And (PLong(d)^));
 PLong(a)^ := PLong(a)^ + Fret + PLong(x)^ + ac;
 {NOW DO THE ROTATE LEFT}
 LongInt(a^):= ROL(LongInt(a^), s);
 {LongInt(a^):= ( LongInt(a^) SHL s) Or (LongInt(a^) SHR (32-(s)) );}
 Inc(PLong(a)^, PLong(b)^);
end;{FF}

Procedure TMD5.GG(a, b, c, d, x: Pointer; s: BYTE; ac: Longint);
{Purpose:  Round 2 of the Transform.
           Equivalent to a = b + ((a + G(b,c,d) + x + ac) <<< s)
           Where G(b,c,d) = b And d Or c Not d
}
var
 Gret: LongInt;
begin
 Gret := (PLong(b)^ And PLong(d)^) Or ( PLong(c)^ And (Not PLong(d)^));
 PLong(a)^ := PLong(a)^ + Gret + PLong(x)^ + ac;
 LongInt(a^):= ROL(LongInt(a^), s);
 {LongInt(a^):= ( LongInt(a^) SHL s) Or (LongInt(a^) SHR (32-(s)) );}
 Inc(PLong(a)^, PLong(b)^);
end;{GG}

Procedure TMD5.HH(a, b, c, d, x: Pointer; s: BYTE; ac: Longint);
{Purpose:  Round 3 of the Transform.
           Equivalent to a = b + ((a + H(b,c,d) + x + ac) <<< s)
           Where H(b,c,d) = b Xor c Xor d
}
var
 Hret: LongInt;
begin
 Hret := PLong(b)^ Xor PLong(c)^ Xor PLong(d)^;
 PLong(a)^ := PLong(a)^ + Hret + PLong(x)^ + ac;
 LongInt(a^):= ROL(LongInt(a^), s);
 {LongInt(a^):= ( LongInt(a^) SHL s) Or (LongInt(a^) SHR (32-(s)) );}
 PLong(a)^ := PLong(b)^ + PLong(a)^;
end;{HH}

Procedure TMD5.II(a, b, c, d, x: Pointer; s: BYTE; ac: Longint);
{Purpose:  Round 4 of the Transform.
           Equivalent to a = b + ((a + I(b,c,d) + x + ac) <<< s)
           Where I(b,c,d) = C Xor (b Or Not(d))
}
var
 Iret: LongInt;
begin
 Iret := (PLong(c)^ Xor (PLong(b)^ Or (Not PLong(d)^)));
 PLong(a)^ := PLong(a)^ + Iret + PLong(x)^ + ac;
 LongInt(a^):= ROL(PLong(a)^, s );
{ LongInt(a^):= ( LongInt(a^) SHL s) Or (LongInt(a^) SHR (32-(s)) );}
 PLong(a)^ := PLong(b)^ + PLong(a)^;
end;{II}

Procedure TMD5.MD5_Transform;
{Purpose:  Perform Step 4 of the algorithm.  This is where all the important
           stuff happens.  This performs the rounds on a 64Byte Block.  This
           procedure should be called in a loop until all input data has been
           transformed.
}

begin
  FAA := FA;
  FBB := FB;
  FCC := FC;
  FDD := FD;

  { Round 1 }
  FF (FpA, FpB, FpC, FpD, @FActiveBlock[ 0], S11, $d76aa478); { 1 }
  FF (FpD, FpA, FpB, FpC, @FActiveBlock[ 1], S12, $e8c7b756); { 2 }
  FF (FpC, FpD, FpA, FpB, @FActiveBlock[ 2], S13, $242070db); { 3 }
  FF (FpB, FpC, FpD, FpA, @FActiveBlock[ 3], S14, $c1bdceee); { 4 }
  FF (FpA, FpB, FpC, FpD, @FActiveBlock[ 4], S11, $f57c0faf); { 5 }
  FF (FpD, FpA, FpB, FpC, @FActiveBlock[ 5], S12, $4787c62a); { 6 }
  FF (FpC, FpD, FpA, FpB, @FActiveBlock[ 6], S13, $a8304613); { 7 }
  FF (FpB, FpC, FpD, FpA, @FActiveBlock[ 7], S14, $fd469501); { 8 }
  FF (FpA, FpB, FpC, FpD, @FActiveBlock[ 8], S11, $698098d8); { 9 }
  FF (FpD, FpA, FpB, FpC, @FActiveBlock[ 9], S12, $8b44f7af); { 10 }
  FF (FpC, FpD, FpA, FpB, @FActiveBlock[10], S13, $ffff5bb1); { 11 }
  FF (FpB, FpC, FpD, FpA, @FActiveBlock[11], S14, $895cd7be); { 12 }
  FF (FpA, FpB, FpC, FpD, @FActiveBlock[12], S11, $6b901122); { 13 }
  FF (FpD, FpA, FpB, FpC, @FActiveBlock[13], S12, $fd987193); { 14 }
  FF (FpC, FpD, FpA, FpB, @FActiveBlock[14], S13, $a679438e); { 15 }
  FF (FpB, FpC, FpD, FpA, @FActiveBlock[15], S14, $49b40821); { 16 }

 { Round 2 }
  GG (FpA, FpB, FpC, FpD, @FActiveBlock[ 1], S21, $f61e2562); { 17 }
  GG (FpD, FpA, FpB, FpC, @FActiveBlock[ 6], S22, $c040b340); { 18 }
  GG (FpC, FpD, FpA, FpB, @FActiveBlock[11], S23, $265e5a51); { 19 }
  GG (FpB, FpC, FpD, FpA, @FActiveBlock[ 0], S24, $e9b6c7aa); { 20 }
  GG (FpA, FpB, FpC, FpD, @FActiveBlock[ 5], S21, $d62f105d); { 21 }
  GG (FpD, FpA, FpB, FpC, @FActiveBlock[10], S22,  $2441453); { 22 }
  GG (FpC, FpD, FpA, FpB, @FActiveBlock[15], S23, $d8a1e681); { 23 }
  GG (FpB, FpC, FpD, FpA, @FActiveBlock[ 4], S24, $e7d3fbc8); { 24 }
  GG (FpA, FpB, FpC, FpD, @FActiveBlock[ 9], S21, $21e1cde6); { 25 }
  GG (FpD, FpA, FpB, FpC, @FActiveBlock[14], S22, $c33707d6); { 26 }
  GG (FpC, FpD, FpA, FpB, @FActiveBlock[ 3], S23, $f4d50d87); { 27 }
  GG (FpB, FpC, FpD, FpA, @FActiveBlock[ 8], S24, $455a14ed); { 28 }
  GG (FpA, FpB, FpC, FpD, @FActiveBlock[13], S21, $a9e3e905); { 29 }
  GG (FpD, FpA, FpB, FpC, @FActiveBlock[ 2], S22, $fcefa3f8); { 30 }
  GG (FpC, FpD, FpA, FpB, @FActiveBlock[ 7], S23, $676f02d9); { 31 }
  GG (FpB, FpC, FpD, FpA, @FActiveBlock[12], S24, $8d2a4c8a); { 32 }

  { Round 3 }
  HH (FpA, FpB, FpC, FpD, @FActiveBlock[ 5], S31, $fffa3942); { 33 }
  HH (FpD, FpA, FpB, FpC, @FActiveBlock[ 8], S32, $8771f681); { 34 }
  HH (FpC, FpD, FpA, FpB, @FActiveBlock[11], S33, $6d9d6122); { 35 }
  HH (FpB, FpC, FpD, FpA, @FActiveBlock[14], S34, $fde5380c); { 36 }
  HH (FpA, FpB, FpC, FpD, @FActiveBlock[ 1], S31, $a4beea44); { 37 }
  HH (FpD, FpA, FpB, FpC, @FActiveBlock[ 4], S32, $4bdecfa9); { 38 }
  HH (FpC, FpD, FpA, FpB, @FActiveBlock[ 7], S33, $f6bb4b60); { 39 }
  HH (FpB, FpC, FpD, FpA, @FActiveBlock[10], S34, $bebfbc70); { 40 }
  HH (FpA, FpB, FpC, FpD, @FActiveBlock[13], S31, $289b7ec6); { 41 }
  HH (FpD, FpA, FpB, FpC, @FActiveBlock[ 0], S32, $eaa127fa); { 42 }
  HH (FpC, FpD, FpA, FpB, @FActiveBlock[ 3], S33, $d4ef3085); { 43 }
  HH (FpB, FpC, FpD, FpA, @FActiveBlock[ 6], S34,  $4881d05); { 44 }
  HH (FpA, FpB, FpC, FpD, @FActiveBlock[ 9], S31, $d9d4d039); { 45 }
  HH (FpD, FpA, FpB, FpC, @FActiveBlock[12], S32, $e6db99e5); { 46 }
  HH (FpC, FpD, FpA, FpB, @FActiveBlock[15], S33, $1fa27cf8); { 47 }
  HH (FpB, FpC, FpD, FpA, @FActiveBlock[ 2], S34, $c4ac5665); { 48 }

  { Round 4 }
  II (FpA, FpB, FpC, FpD, @FActiveBlock[ 0], S41, $f4292244); { 49 }
  II (FpD, FpA, FpB, FpC, @FActiveBlock[ 7], S42, $432aff97); { 50 }
  II (FpC, FpD, FpA, FpB, @FActiveBlock[14], S43, $ab9423a7); { 51 }
  II (FpB, FpC, FpD, FpA, @FActiveBlock[ 5], S44, $fc93a039); { 52 }
  II (FpA, FpB, FpC, FpD, @FActiveBlock[12], S41, $655b59c3); { 53 }
  II (FpD, FpA, FpB, FpC, @FActiveBlock[ 3], S42, $8f0ccc92); { 54 }
  II (FpC, FpD, FpA, FpB, @FActiveBlock[10], S43, $ffeff47d); { 55 }
  II (FpB, FpC, FpD, FpA, @FActiveBlock[ 1], S44, $85845dd1); { 56 }
  II (FpA, FpB, FpC, FpD, @FActiveBlock[ 8], S41, $6fa87e4f); { 57 }
  II (FpD, FpA, FpB, FpC, @FActiveBlock[15], S42, $fe2ce6e0); { 58 }
  II (FpC, FpD, FpA, FpB, @FActiveBlock[ 6], S43, $a3014314); { 59 }
  II (FpB, FpC, FpD, FpA, @FActiveBlock[13], S44, $4e0811a1); { 60 }
  II (FpA, FpB, FpC, FpD, @FActiveBlock[ 4], S41, $f7537e82); { 61 }
  II (FpD, FpA, FpB, FpC, @FActiveBlock[11], S42, $bd3af235); { 62 }
  II (FpC, FpD, FpA, FpB, @FActiveBlock[ 2], S43, $2ad7d2bb); { 63 }
  II (FpB, FpC, FpD, FpA, @FActiveBlock[ 9], S44, $eb86d391); { 64 }

  Inc(FA, FAA);
  Inc(FB, FBB);
  Inc(FC, FCC);
  Inc(FD, FDD);
  { Zeroize sensitive information}
  FillChar(FActiveBlock, SizeOf(FActiveBlock), #0);
end;{TMD5.MD5_Transform}

Procedure TMD5.MD5_Hash;
var
 pStr: PChar;
begin
  MD5_Initialize;
  case FType of
   SourceFile:
   begin
    MD5_Hash_File;
   end;{SourceFile}
   SourceByteArray:
   begin
    MD5_Hash_Bytes;
   end;{SourceByteArray}
   SourceString:
   begin
    {Convert Pascal String to Byte Array}
 {$IFDEF DELPHI}
    pStr := StrAlloc(Length(FInputString) + 1);
    try {protect dyanmic memory allocation}
    StrPCopy(pStr, FInputString);
 {$ENDIF}
 {$IFDEF BP7}
    GetMem(pStr, Length(FInputString));
    Move(FInputString[1],pStr^, Length(FInputString));
 {$ENDIF}
    FSourceLength := Length(FInputString);
    FInputArray := Pointer(pStr);
    MD5_Hash_Bytes;
 {$IFDEF DELPHI}
    finally
     StrDispose(pStr);
    end;
 {$ENDIF}
 {$IFDEF BP7}
    FreeMem(pStr,Length(FInputString));
 {$ENDIF}
   end;{SourceString}
  end;{case}
  MD5_Finish;
end;{TMD5.MD5_Hash}

Procedure TMD5.MD5_Hash_Bytes;
var
  Buffer: array[0..4159] of Byte;
  Count64: Comp;
  index: longInt;
begin
  Move(FInputArray^, Buffer, FSourceLength);
  Count64 := FSourceLength * 8;     {Save the Length(in bits) before padding}
  Buffer[FSourceLength] := $80;     {Must always pad with at least a '1'}
  inc(FSourceLength);

  while (FSourceLength mod 64)<>56 do begin
   Buffer[FSourceLength] := 0;
   Inc(FSourceLength);
  end;
  Move(Count64,Buffer[FSourceLength],SizeOf(Count64){This better be 64bits});
  index := 0;
  Inc(FSourceLength, 8);
  repeat
    Move(Buffer[Index], FActiveBlock, 64);
    {Flip bytes here on Mac??}
    MD5_Transform;
    Inc(Index,64);
  until Index = FSourceLength;
end;{TMD5.Hash_Bytes}

Procedure TMD5.MD5_Hash_File;
var
  Buffer:array[0..4159] of BYTE;
  InputFile: File;
  Count64: Comp;
  DoneFile : Boolean;
  Index: LongInt;
  NumRead: {$IFDEF DELPHI32}integer {$ELSE}WORD{$ENDIF};
begin
DoneFile := False;
{$IFDEF DELPHI}
 AssignFile(InputFile, FInputFilePath);
{$ENDIF}
{$IFDEF BP7}
 Assign(InputFile, FInputFilePath);
{$ENDIF}

Reset(InputFile, 1);
Count64 := 0;
repeat
    BlockRead(InputFile,Buffer,4096,NumRead);
    Count64 := Count64 + NumRead;
    if NumRead<>4096 {reached end of file}
      then begin
          Buffer[NumRead]:= $80;
          Inc(NumRead);
          while (NumRead mod 64)<>56
            do begin
               Buffer[ NumRead ] := 0;
               Inc(NumRead);
              end;
          Count64 := Count64 * 8;
          Move(Count64,Buffer[NumRead],8);
          Inc(NumRead,8);
          DoneFile := True;
        end;
    Index := 0;
    repeat
     Move(Buffer[Index], FActiveBlock, 64);
     {Flip bytes here on a Mac(I think)}

     MD5_Transform;
     Inc(Index,64);
    until Index = NumRead;
  until DoneFile;
{$IFDEF DELPHI}
  CloseFile(InputFile);
{$ENDIF}
{$IFDEF BP7}
  Close(InputFile);
{$ENDIF}
end;{TMD5.MD5_Hash_File}


Procedure TMD5.MD5_Finish;
begin
 FOutputDigest^.A := LongInt(FpA^);
 FOutputDigest^.B := LongInt(FpB^);
 FOutputDigest^.C := LongInt(FpC^);
 FOutputDigest^.D := LongInt(FpD^);
end;
end.
