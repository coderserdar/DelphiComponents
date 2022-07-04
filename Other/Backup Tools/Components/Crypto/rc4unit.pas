unit Rc4unit;
{*****************************************************************************
 UNIT: TRC5Unit
 Description:  This unit contains an Object Pascal Object which can be used to
               perform TRC4 ciphers. 'RC4' is a trademark of RSA security
               corporation, this code is an implementation of the algorithm which
               RSA referers to as 'RC4'. TRC4 is a stream cipher, it is slightly
               different from the other ciphers included in this library.
               In 16bit code it is the fastest of the encryption algorithms.
               RSA has NOT made RC4 public, this is an implementation of the
               'rumored' algorithm.  It is only included here incase RSA
               decides to make RC4 public.  DO NOT use it, unless you have
               authorization from RSA.

               For more information on the algorithm see 'Applied Cryptography',
               Bruce Snheier or visit RSA's www page.  http://www.rsa.com

 The RC4 Algorithm was produced by Ronald Rivest.(See LEGAL)
 -----------------------------------------------------------------------------
 Code Author:  Greg Carter, gregc@cryptocard.com
 Organization: CRYPTOCard Corporation, info@cryptocard.com
               R&D Division, Carleton Place, ON, CANADA, K7C 3T2
               1-613-253-3152 Voice, 1-613-253-4685 Fax.
 Date of V.1:  Jan. 3 1996.

 Compatibility & Testing with BP7.0: Anne Marcel Roorda, garfield@xs4all.nl
 -----------------------------------------------------------------------------}
 {Useage:  Below is typical usage(for File)of the TRC4 Object,
           Follow these steps:
           1) Declare and Create Variable of type TRC4.
           2) Set InputSource Type, either SourceFile, SourceByteArray, or
              SourceString(Pascal style string).
           3) Point to Input Source and set Input Length(If needed)
           4) Point to Output Structure(array, file).
           5) Set Key;
           6) Call BF_EncipherData Method.
           7) Reference the Output. Thats it.
 **** Note **** Steps 2..6 can occure in any order.
 Here is a procedure in Delphi used to encrypt a file:
procedure Tcryptfrm.OpenCiphButtonClick(Sender: TObject);
var
 RC4: TRC4; (*Step 1*)
begin
RC4 := TRC4.Create;(*Step 1b*)
 try
  If OpenDialog1.Execute then
  begin
   RC4.InputType := SourceFile; (*Step 2*)
   RC4.InputFilePath := OpenDialog1.FileName; (*Step 3*)
   RC4.OutputFilePath := ChangeFileExt(OpenDialog1.FileName, '.ccc'); (*Step 4*)
   RC4.Key := 'abcdefghijklmnopqrstuvwxyz'; (*Step 5*)
   RC4.BF_EncipherData(False);  (*Step 6*)
  end;
 finally
  RC4.free;
 end;
end;

{-----------------------------------------------------------------------------}
{LEGAL:        The algorithm is in the process of being patented, and its name
               'RC4' is trademarked.  Please oontact RSA Data Security for
               licensing arrangements. This code is copyright by
               CRYPTOCard.  CRYPTOCard grants anyone who may wish to use, modify
               or redistribute this code privileges to do so, provided the user
               agrees to the following three(3) rules:

               1)Any Applications, (ie exes which make use of this
               Object...), for-profit or non-profit,
               must acknowledge the author of this Object(ie.
               TRC4 Implementation provided by Greg Carter, CRYPTOCard
               Corporation) somewhere in the accompanying Application
               documentation(ie AboutBox, HelpFile, readme...).  NO runtime
               or licensing fees are required!

               2)Any Developer Component(ie Delphi Component, Visual Basic VBX,
               DLL) derived from this software must acknowledge that it is
               derived from "TRC4 Object Pascal Implementation Originated by
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
The RC4 Algorithm was produced by Ronald Rivest.
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

type
{$IFDEF DELPHI}
 TRC4 = class(TCrypto)
{$ENDIF}
{$IFDEF BP7}
 PRC4 = ^TRC4;   {For BP7 Objects}
 TRC4 = object(TCrypto)
{$ENDIF}
 Private
  {RC4 Key Elements}
  FState: Array[0..255] of BYTE;
  FI: BYTE;
  FJ: BYTE;
 {$IFDEF DELPHI}
  Procedure SetKeys;       override; {Sets up En\DecipherKey SubKeys}
  Procedure Encipher_Bytes;override;
  Procedure Decipher_Bytes;override;
{$ENDIF}
{$IFDEF BP7}
  Procedure Encipher_Bytes; virtual;
  Procedure Decipher_Bytes; virtual;
  Procedure SetKeys;        virtual; {Sets up En\DecipherKey SubKeys}
{$ENDIF}
 public
    { Public declarations }
{$IFDEF DELPHI}
  constructor Create(Owner: TComponent);override;
{$ENDIF}
{$IFDEF BP7}
  constructor Init;
{$ENDIF}
end;{TRC4}

{$IFDEF DELPHI}
 procedure Register;{register the component to the Delphi toolbar}
{$ENDIF}

implementation

{$IFDEF DELPHI}
procedure Register;
  {Registers the Component to the toobar, on the tab named 'Crypto'}
  {Now all a Delphi programmer needs to do is drag n drop to have
   Blowfish encryption}
begin
  RegisterComponents('Crypto', [TRC4]);
end;
{$ENDIF}

{==================================TRC4========================================}

{$IFDEF DELPHI}
constructor TRC4.Create(Owner: TComponent);
{$ENDIF}
{$IFDEF BP7}
constructor TRC4.Init;
{$ENDIF}
begin
{ Decipher_Bytes := @Encipher_Bytes;}
{$IFDEF DELPHI}
  inherited Create(Owner);
{$ENDIF}
end;

Procedure TRC4.SetKeys;
{------------------------------------------------------------------------------
 Initializing the S-Box.  First fill it linearly: So=0, S1=1...S255=255.
 Then fill another 256byte array with the key, repeating the key as necessary
 to fill the entire array: K0, K1..K255.
 Then
 j=0
 for i=0 to 255
  j = (j + Si + Ki) mod 256
  swap Si and Sj
-------------------------------------------------------------------------------}
var
 KeyLen, j: WORD; i, swapbyte: BYTE;
 K: Array[0..255] of BYTE;

begin
 KeyLen := Length(FKey);
 FI := 0; FJ := 0;j := 0;
 for i:= 0 to 255 do begin
  FState[i] := i; K[i] := BYTE(FKey[(i MOD KeyLen) + 1]);
 end;
 for i := 0 to 255 do begin
  j := (j + FState[i] + K[i]) MOD 256;
  swapbyte := FState[i]; FState[i] := FState[j]; FState[j] := swapbyte;
 end;
end;{SetKeys}

Procedure TRC4.Encipher_Bytes;
{------------------------------------------------------------------------------
 i=j=0

 i=(i + 1) mod 256
 j=(j + Si) mod 256
 swap Si and Sj
 t = (Si + Sj) mod 256
 K = St

 the BYTE K is XOR withe plaintext to produce ciphertext or XORED with
 the ciphertext to produce plaintext

 We assume that the data to encipher is in FBuffer, and FInputLength holds the
 length of FBuffer.
 ------------------------------------------------------------------------------}
 var
  i, j, t, swapbyte: BYTE;
  x: WORD;
 begin
  i:= FI; j:= FJ;

  For x := 0 to (FInputLength - 1) do begin
   i := (i + 1) MOD 256;
   j := (j + FState[i]) MOD 256;
   swapbyte := FState[i]; FState[i] := FState[j]; FState[j] := swapbyte;
   t := (FState[i] + FState[j]) MOD 256;
   FOutputArray^[x] := FState[t] Xor FBuffer[x];
  end;

  FI := i;
  FJ := j;
 end;

Procedure TRC4.Decipher_Bytes;
begin
 Encipher_Bytes;
end;
end.
