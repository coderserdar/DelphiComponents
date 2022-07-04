unit Rc5unit;
{*****************************************************************************
 UNIT: TRC5Unit
 Description:  This unit contains an Object Pascal Object which can be used to
               perform TRC5 block ciphers. 'RC5' is a trademark of RSA security
               corporation, this code is an implementation of the algorithm which
               RSA referers to as 'RC5'.  TRC5 can be used with different
               parameters, w (word size in bits), r (encryption rounds), and b
               (key length in bytes).  This implementation always uses TRC5-32,r,b.
               Note the word size if fixed.  You should always use at least 12
               rounds(FRounds), and a key length of greater then 8bytes.

               For more information on the algorithm see 'Applied Cryptography',
               Bruce Snheier or visit RSA's www page where you can d/l a
               postscript file describing the algorithm.  http://www.rsa.com

 The RC5 Algorithm was produced by Ronald Rivest.(See LEGAL)
 -----------------------------------------------------------------------------
 Code Author:  Greg Carter, gregc@cryptocard.com
 Organization: CRYPTOCard Corporation, info@cryptocard.com
               R&D Division, Carleton Place, ON, CANADA, K7C 3T2
               1-613-253-3152 Voice, 1-613-253-4685 Fax.
 Date of V.1:  Jan. 3 1996.

 Compatibility & Testing with BP7.0: Anne Marcel Roorda, garfield@xs4all.nl
 -----------------------------------------------------------------------------}
 {Useage: Below is typical usage(for File)of the TRC5 Object,
          Follow these steps:
           1) Declare and Create Variable of type TRC5.
           2) Set InputSource Type, either SourceFile, SourceByteArray, or
              SourceString(Pascal style string).
           3) Set Cipher Mode, optionally IVector.
           4) Point to Input Source and set Input Length(If needed)
           5) Point to Output Structure(array, file).
           6) Set Key;
           7) Call BF_EncipherData Method.
           8) Reference the Output. Thats it.
 **** Note **** Steps 2..6 can occure in any order.
 Here is a procedure in Delphi used to encrypt a file:
procedure Tcryptfrm.OpenCiphButtonClick(Sender: TObject);
var
 RC5: TRC5; (*Step 1*)
begin
RC5 := TRC5.Create;(*Step 1b*)
 try
  If OpenDialog1.Execute then
  begin
   RC5.InputType := SourceFile; (*Step 2*)
   RC5.CipherMode := ECBMode;   (*Step 3*)
   RC5.InputFilePath := OpenDialog1.FileName; (*Step 4*)
   RC5.OutputFilePath := ChangeFileExt(OpenDialog1.FileName, '.ccc'); (*Step 5*)
   RC5.Key := 'abcdefghijklmnopqrstuvwxyz'; (*Step 6*)
   RC5.BF_EncipherData(False);  (*Step 7*)
  end;
 finally
  RC5.free;
 end;
end;

{-----------------------------------------------------------------------------}
{LEGAL:        The algorithm is in the process of being patented, and its name
               'RC5' is trademarked.  Please oontact RSA Data Security for
               licensing arrangements. This code is copyright by
               CRYPTOCard.  CRYPTOCard grants anyone who may wish to use, modify
               or redistribute this code privileges to do so, provided the user
               agrees to the following three(3) rules:

               1)Any Applications, (ie exes which make use of this
               Object...), for-profit or non-profit,
               must acknowledge the author of this Object(ie.
               TRC5 Implementation provided by Greg Carter, CRYPTOCard
               Corporation) somewhere in the accompanying Application
               documentation(ie AboutBox, HelpFile, readme...).  NO runtime
               or licensing fees are required!

               2)Any Developer Component(ie Delphi Component, Visual Basic VBX,
               DLL) derived from this software must acknowledge that it is
               derived from "TRC5 Object Pascal Implementation Originated by
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
The RC5 Algorithm was produced by Ronald Rivest.
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
NOTES: Make sure to read the LEGAL notes.
------------------------------------------------------------------------------
Revised:  00/00/00 BY: ******* Reason: ******
------------------------------------------------------------------------------
}
interface
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
{$IFDEF DELPHI}
{$U-   Non Pentium safe FDIV }
{$Z-   No automatic word-sized enumerations}
{$ENDIF}
{---------------------------------------------------------------------}
{.$DEFINE TEST}
uses SysUtils, Cryptcon{$IFDEF DELPHI}, Classes, Controls{$ENDIF}
     {$IFDEF BP7},objects{$ENDIF};

const
 P = $B7E15163;
 Q = $9E3779B9;

type
 AB = record
  A: UWORD_32bits;
  B: UWORD_32bits;
 end; {AB}

 pAB = ^AB;

{$IFDEF DELPHI}
 TRC5 = class(TCrypto)
 Private
 { Private declarations }
{$ENDIF}
{$IFDEF BP7}
 PRC5 = ^TRC5;   {For BP7 Objects}
 TRC5 = object(TCrypto)
 Public          {Since BP7 doesn't support Properties, we make these Public}
{$ENDIF}
  FRounds: BYTE; {Number of Rounds}
 Private
  FpActiveBlock: pAB; {64bit Cipher BLOCK}
  FpA: PLong;   {Lower 32 bits of Active Encipher Data, Little Endian}
  FpB: PLong;   {Upper 32 bits of Active Encipher Data, Little Endian}
  FpKey: PLArray;  {Pointer to SubKeys Array}
  FSubKeyLen: WORD;{Previous Subkey Length}
{$IFDEF DELPHI}
  Procedure EncipherBLOCK;override; {Enciphers BLOCK}
  Procedure DecipherBLOCK;override; {Deciphers BLOCK}
  Procedure SetKeys;      override; {Sets up En\DecipherKey SubKeys}
{$ENDIF}
{$IFDEF BP7}
  Procedure EncipherBLOCK; virtual; {Enciphers BLOCK}
  Procedure DecipherBLOCK; virtual; {Deciphers BLOCK}
  Procedure SetKeys;       virtual; {Sets up En\DecipherKey SubKeys}
{$ENDIF}
 public
    { Public declarations }
{$IFDEF DELPHI}
  constructor Create(Owner: TComponent);override;
  destructor  Destroy;override;
 Published
  property Rounds: BYTE read FRounds write FRounds;
{$ENDIF}
{$IFDEF BP7}
  constructor Init;
  destructor  Done;
{$ENDIF}
 end;{TRC5}

{$IFDEF DELPHI}
 procedure Register;{register the component to the Delphi toolbar}
{$ENDIF}

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
Function  ROR(A: UWORD_32bits; Amount: BYTE): UWORD_32bits;NEAR;
 begin
  inline(
  $8A/$4E/$04/        { mov cl,[bp+4]  }
  $66/$8B/$46/$06/    { mov eax,[bp+6] }
  $66/$D3/$C8/        { ror eax,cl     }
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

Function ROR(A: Longint; Amount: BYTE): Longint; Assembler;
asm
 mov cl, Amount
 ror eax, cl
end;
{$ENDIF}

{$IFDEF DELPHI}
procedure Register;
  {Registers the Component to the toobar, on the tab named 'Crypto'}
  {Now all a Delphi programmer needs to do is drag n drop to have
   Blowfish encryption}
begin
  RegisterComponents('Crypto', [TRC5]);
end;
{$ENDIF}

{==================================TRC5========================================}

{$IFDEF DELPHI}
constructor TRC5.Create(Owner: TComponent);
{$ENDIF}
{$IFDEF BP7}
constructor TRC5.Init;
{$ENDIF}
begin
 {GetMem(FIVTemp, FBLOCKSIZE);}
 {$IFDEF DELPHI}
  inherited Create(Owner);
 {$ENDIF}
  FBLOCKSIZE := SizeOf(AB);
  FpActiveBlock := @FSmallBuffer;
  FpA := @FpActiveBlock^.A;
  FpB := @FpActiveBlock^.B;
  FRounds := 8;
  FSubKeyLen := (FRounds * 2) + 2;  {calculate new subkeylen}
  GetMem(FpKey, FSubKeyLen * 4);    {get mem for new subkeys}
  FIVTemp := nil;
end;{Create}

{$IFDEF DELPHI}
destructor TRC5.Destroy;
{$ENDIF}
{$IFDEF BP7}
destructor TRC5.Done;
{$ENDIF}
 begin
  If FpKey <> nil then FreeMem(FpKey, (FSubKeyLen * 4));
  {$IFDEF DELPHI}
  inherited Destroy;
  {$ENDIF}
end;{TRC5.Destroy;}

Procedure TRC5.SetKeys;
{------------------------------------------------------------------------------
Generating the subkeys:

 Copy user key in an array L, of c 32bit words, padding final word with
 zeros if necessary. Then
  S0 = P
  for i = 1 to 2(r +1) - 1
     Si = (S(i-1) + Q) Mod 2tothe32
 where P=$b7e15163 Q=$9e3779b9

 then
  i=j=0
  A=B=0
  do 3*n times(where n is the maximum of 2(r + 1) and c)
    A = Si =(Si + A + B) <<< 3
    B = Lj =(Lj + A + B) <<< (A + B)
  i=(i+1) mod 2(r +1)
  j=(j+1) mod c
-------------------------------------------------------------------------------}
var
 userKeyLen,SubkeyLen, paddedLen, i, j, n, maxtimes : WORD;
 L: PLArray; {array of bytes}
 A, B: LongInt;
 {$IFDEF ORDER_ABCD}
 plittleL: Paword;
 bigL: aword;
 {$ENDIF}
begin
{$IFDEF TEST}
 FKey := #$91 + #$5F + #$46 + #$19 + #$BE + #$41 + #$B2 + #$51 + #$63 + #$55
         + #$A5 + #$01 + #$10 + #$A9 + #$CE + #$91;
 FRounds := 12;
{$ENDIF}
 userKeyLen := Length(FKey);
 {If UserKeyLen <= 0 then Signal Error..}
 SubKeyLen := (FRounds * 2) + 2;  {calculate new subkeylen}
 FreeMem(FpKey, FSubKeyLen * 4);  {free old subkey mem}
 GetMem(FpKey, SubKeyLen * 4);    {get mem for new subkeys}
 FSubKeyLen := SubKeyLen;         {save old length}
 paddedLen := userKeyLen DIV 4;
 if (userKeyLen MOD 4) <> 0 then Inc(paddedLen);
 {get some memory for temp array L}
 GetMem(L, paddedLen * 4);
 {if L = nil then error}
 FillChar(L^, paddedLen * 4, #0);{initialize with zeros}
 {copy users key into L, L is array of UWORD32, so on BigEndian we need
  to make sure that the bytes get in the right places, RC5 assumes LittleEnd}

 Move(FKey[1], L^, userKeyLen);
 {$IFDEF ORDER_ABCD}
 pbigL := L;
 For i := 1 to (paddedLen) do begin
   bigL := plittleL^;
   plittleL^.w.Byte3 :=  bigL.w.Byte0;
   plittleL^.w.Byte2 :=  bigL.w.Byte1;
   plittleL^.w.Byte1 :=  bigL.w.Byte2;
   plittleL^.w.Byte0 :=  bigL.w.Byte3;
   Inc(plittleL);
 end;{for}
 {$ENDIF}
 {Initialize SubKeys}
 FpKey^[0] := P;
 For i:= 1 to  (SubKeyLen - 1) do begin
   FpKey^[i] := FpKey^[i - 1] + Q;
 end;{for}

 i := 0; j := 0; n := 0; A := 0; B := 0;
 if paddedLen > SubKeyLen then
   maxtimes := 3*paddedLen
 else
   maxtimes := 3*SubKeyLen;
 {calculate SubKeys}
 repeat
  inc(n);
  FpKey^[i] := FpKey^[i] + A + B;
  {$IFDEF ORDER_DCBA} {Intel, use inline asm functions}
   FpKey^[i] := ROL(FpKey^[i], 3);
  {$ELSE}
   FpKey^[i] := (FpKey^[i] SHL 3) Or (FpKey^[i] SHR (32 - 3));
  {$ENDIF}
  A := FpKey^[i];
  L^[j] := L^[j] + A + B;
  {$IFDEF ORDER_DCBA} {Intel, use inline asm functions}
   L^[j] := ROL(L^[j], ((A + B) AND 31));
  {$ELSE}
   L^[j] := (L^[j] SHL ((A + B) AND 31)) Or (L^[j] SHR (32 - ((A + B) AND 31)));
  {$ENDIF}
  B := L^[j];
  i := (i + 1) MOD SubKeyLen;
  j := (j + 1) MOD paddedLen;
 until n = maxtimes;
 FillChar(L^, paddedLen * 4, #0);
 FreeMem(L,paddedLen * 4);
end;{TRC5.SetKeys}

Procedure TRC5.EncipherBLOCK;
{------------------------------------------------------------------------------
Enciphers a 64bit block, two 32bit halfs, A & B
Encryption uses 2r + r(r = rounds) key dependent 32bit words(S0..S31).
To encrypt first divide the plaintext block into two 32 bit words: A & B.
Then:
 A = A + S0
 B = B + S1

 For i = 1 to r
  A = ((A Xor B) <<< B) + S2i
  B = ((B Xor A) <<< A) + S(2i + 1)

The output is A & B
-------------------------------------------------------------------------------}
var
 i, j : WORD;
{$IFDEF ORDER_ABCD}
 bigL: aword;
{$ENDIF}
begin
 {$IFDEF TEST}
  FpA^ := $EEDBA521;
  FpB^ := $6D8F4B15;
 {$ENDIF}
  {Flip bytes here on Mac}
 {$IFDEF ORDER_ABCD}
   pbigL := Paword(FpA)^;
   Paword(FpA)^.w.Byte3 :=  bigL.w.Byte0;
   Paword(FpA)^.w.Byte2 :=  bigL.w.Byte1;
   Paword(FpA)^.w.Byte1 :=  bigL.w.Byte2;
   Paword(FpA)^.w.Byte0 :=  bigL.w.Byte3;

   pbigL := Paword(FpB)^;
   Paword(FpB)^.w.Byte3 :=  bigL.w.Byte0;
   Paword(FpB)^.w.Byte2 :=  bigL.w.Byte1;
   Paword(FpB)^.w.Byte1 :=  bigL.w.Byte2;
   Paword(FpB)^.w.Byte0 :=  bigL.w.Byte3;
 {$ENDIF}

  Inc(FpA^, FpKey^[0]);
  Inc(FpB^, FpKey^[1]);

  For i:= 1 to FRounds do begin
   j := 2 * i;
 {$IFDEF ORDER_DCBA} {Intel, use asm functions}
   FpA^ := ROL((FpA^ Xor FpB^), (FpB^ AND 31)) + FpKey^[j];
   FpB^ := ROL((FpB^ Xor FpA^), (FpA^ AND 31)) + FpKey^[j + 1];
 {$ELSE}
   FpA^ := FpA^ Xor FpB^;
   FpA^ := ((FpA^ SHL (FpB^ AND 31)) Or (FpA^ SHR (32 - (FpB^ AND 31)))) + FpKey^[j];
   FpB^ := FpB^ Xor FpA^;
   FpB^ := ((FpB^ SHL (FpA^ AND 31)) Or (FpB^ SHR (32 - (FpA^ AND 31)))) + FpKey^[j + 1];
 {$ENDIF}
  end;{for}
 {Flip Bytes here on Mac}
 {$IFDEF ORDER_ABCD}
   pbigL := Paword(FpA)^;
   Paword(FpA)^.w.Byte3 :=  bigL.w.Byte0;
   Paword(FpA)^.w.Byte2 :=  bigL.w.Byte1;
   Paword(FpA)^.w.Byte1 :=  bigL.w.Byte2;
   Paword(FpA)^.w.Byte0 :=  bigL.w.Byte3;

   pbigL := Paword(FpB)^;
   Paword(FpB)^.w.Byte3 :=  bigL.w.Byte0;
   Paword(FpB)^.w.Byte2 :=  bigL.w.Byte1;
   Paword(FpB)^.w.Byte1 :=  bigL.w.Byte2;
   Paword(FpB)^.w.Byte0 :=  bigL.w.Byte3;
 {$ENDIF}
{If you were testing, then FpA^ should = $AC13C0F7 and FpB^ should = $52892B5B}
end;{TRC5.EncipherBLOCK}

Procedure TRC5.DecipherBLOCK;
{------------------------------------------------------------------------------
Decryption
 For i = r down to 1
   B = ((B - S(2i +1)) >>> A) Xor A
   B = ((A - S2i)>>>B) Xor B

 B = B - S1
 A = A - S0
 ------------------------------------------------------------------------------}
var
 i, j: WORD;
{$IFDEF ORDER_ABCD}
 bigL: aword;
{$ENDIF}
begin
  {Flip bytes here on Mac}
 {$IFDEF ORDER_ABCD}
   pbigL := Paword(FpA)^;
   Paword(FpA)^.w.Byte3 :=  bigL.w.Byte0;
   Paword(FpA)^.w.Byte2 :=  bigL.w.Byte1;
   Paword(FpA)^.w.Byte1 :=  bigL.w.Byte2;
   Paword(FpA)^.w.Byte0 :=  bigL.w.Byte3;

   pbigL := Paword(FpB)^;
   Paword(FpB)^.w.Byte3 :=  bigL.w.Byte0;
   Paword(FpB)^.w.Byte2 :=  bigL.w.Byte1;
   Paword(FpB)^.w.Byte1 :=  bigL.w.Byte2;
   Paword(FpB)^.w.Byte0 :=  bigL.w.Byte3;
 {$ENDIF}

  For i:= FRounds downto 1 do begin
   j := i * 2;
{$IFDEF ORDER_DCBA}
   FpB^ := ROR((FpB^ - FpKey^[j + 1]), (FpA^ AND 31)) Xor FpA^;
   FpA^ := ROR((FpA^ - FpKey^[j]), (FpB^ AND 31)) Xor FpB^;
{$ELSE}
   FpB^ := FpB^ - FpKey^[j + 1];
   FpB^ := ((FpB^ SHR (FpA^ AND 31)) Or (FpB^ SHL (32 - (FpA^ AND 31)))) Xor FpA^;
   FpA^ := FpA^ - FpKey^[j];
   FpA^ := ((FpA^ SHR (FpB^ AND 31)) Or (FpA^ SHL (32 - (FpB^ AND 31)))) Xor FpB^;
{$ENDIF}
  end;{for}
  Dec(FpB^, FpKey^[1]);
  Dec(FpA^, FpKey^[0]);
 {$IFDEF ORDER_ABCD}
   pbigL := Paword(FpA)^;
   Paword(FpA)^.w.Byte3 :=  bigL.w.Byte0;
   Paword(FpA)^.w.Byte2 :=  bigL.w.Byte1;
   Paword(FpA)^.w.Byte1 :=  bigL.w.Byte2;
   Paword(FpA)^.w.Byte0 :=  bigL.w.Byte3;

   pbigL := Paword(FpB)^;
   Paword(FpB)^.w.Byte3 :=  bigL.w.Byte0;
   Paword(FpB)^.w.Byte2 :=  bigL.w.Byte1;
   Paword(FpB)^.w.Byte1 :=  bigL.w.Byte2;
   Paword(FpB)^.w.Byte0 :=  bigL.w.Byte3;
 {$ENDIF}
 end;{TRC5.DecipherBLOCK}

end.
