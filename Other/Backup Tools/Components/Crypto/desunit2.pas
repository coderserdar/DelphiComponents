unit Desunit2;

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
{$U-   Non Pentium safe FDIV }
{$Z-   No automatic word-sized enumerations}
{---------------------------------------------------------------------}
uses SysUtils, Cryptcon{$IFDEF DELPHI}, Classes, Controls{$ENDIF}
     {$IFDEF BP7},objects{$ENDIF};

type
{$IFDEF DELPHI}
 TDES = class(TCrypto)
{$ENDIF}
{$IFDEF BP7}
 PDES = ^TDES;   {For BP7 Objects}
 TDES = object(TCrypto)
{$ENDIF}
 Private
  {DES Key Elements}
  FRoundKeys : Array [1..16,1..48] Of Byte;
  FC: Array [1..28] Of Byte;
  FD: Array [1..28] Of Byte;
  FInputValue  : Array [1..64] Of Byte;
  FOutputValue : Array [1..64] Of Byte;
  FL, FR, FFunctionResult : Array [1..32] Of Byte;
  Procedure F (var FK);

  Procedure SetBit (Var Data; Index, Value : Byte);
  Function  GetBit (Var Data; Index : Byte) : Byte;
  Procedure Shift (Var SubKeyPart);
  Procedure SubKey (Round : Byte; Var SubKey);
 {$IFDEF DELPHI}
  Procedure SetKeys;      override; {Sets up En\DecipherKey SubKeys}
  Procedure EncipherBLOCK;override;
  Procedure DecipherBLOCK;override;
{$ENDIF}
{$IFDEF BP7}
  Procedure EncipherBLOCK;virtual;
  Procedure DecipherBLOCK;virtual;
  Procedure SetKeys;virtual;       {Sets up En\DecipherKey SubKeys}
{$ENDIF}
 public
    { Public declarations }
{$IFDEF DELPHI}
  constructor Create(Owner: TComponent);override;
  destructor  Destroy;override;
{$ENDIF}
{$IFDEF BP7}
  constructor Init;
  destructor  Done;virtual;
{$ENDIF}
end;{TRC4}

{$IFDEF DELPHI}
 procedure Register;{register the component to the Delphi toolbar}
{$ENDIF}

Const
  IP : Array [1..64] Of Byte = (58,50,42,34,26,18,10,2,
                                60,52,44,36,28,20,12,4,
                                62,54,46,38,30,22,14,6,
                                64,56,48,40,32,24,16,8,
                                57,49,41,33,25,17, 9,1,
                                59,51,43,35,27,19,11,3,
                                61,53,45,37,29,21,13,5,
                                63,55,47,39,31,23,15,7);
  InvIP : Array [1..64] Of Byte = (40, 8,48,16,56,24,64,32,
                                   39, 7,47,15,55,23,63,31,
                                   38, 6,46,14,54,22,62,30,
                                   37, 5,45,13,53,21,61,29,
                                   36, 4,44,12,52,20,60,28,
                                   35, 3,43,11,51,19,59,27,
                                   34, 2,42,10,50,18,58,26,
                                   33, 1,41, 9,49,17,57,25);
  E : Array [1..48] Of Byte = (32, 1, 2, 3, 4, 5,
                                4, 5, 6, 7, 8, 9,
                                8, 9,10,11,12,13,
                               12,13,14,15,16,17,
                               16,17,18,19,20,21,
                               20,21,22,23,24,25,
                               24,25,26,27,28,29,
                               28,29,30,31,32, 1);
  P : Array [1..32] Of Byte = (16, 7,20,21,
                               29,12,28,17,
                                1,15,23,26,
                                5,18,31,10,
                                2, 8,24,14,
                               32,27, 3, 9,
                               19,13,30, 6,
                               22,11, 4,25);
  SBoxes : Array [1..8,0..3,0..15] Of Byte =
           (((14, 4,13, 1, 2,15,11, 8, 3,10, 6,12, 5, 9, 0, 7),
             ( 0,15, 7, 4,14, 2,13, 1,10, 6,12,11, 9, 5, 3, 8),
             ( 4, 1,14, 8,13, 6, 2,11,15,12, 9, 7, 3,10, 5, 0),
             (15,12, 8, 2, 4, 9, 1, 7, 5,11, 3,14,10, 0, 6,13)),

            ((15, 1, 8,14, 6,11, 3, 4, 9, 7, 2,13,12, 0, 5,10),
             ( 3,13, 4, 7,15, 2, 8,14,12, 0, 1,10, 6, 9,11, 5),
             ( 0,14, 7,11,10, 4,13, 1, 5, 8,12, 6, 9, 3, 2,15),
             (13, 8,10, 1, 3,15, 4, 2,11, 6, 7,12, 0, 5,14, 9)),

            ((10, 0, 9,14, 6, 3,15, 5, 1,13,12, 7,11, 4, 2, 8),
             (13, 7, 0, 9, 3, 4, 6,10, 2, 8, 5,14,12,11,15, 1),
             (13, 6, 4, 9, 8,15, 3, 0,11, 1, 2,12, 5,10,14, 7),
             ( 1,10,13, 0, 6, 9, 8, 7, 4,15,14, 3,11, 5, 2,12)),

            (( 7,13,14, 3, 0, 6, 9,10, 1, 2, 8, 5,11,12, 4,15),
             (13, 8,11, 5, 6,15, 0, 3, 4, 7, 2,12, 1,10,14, 9),
             (10, 6, 9, 0,12,11, 7,13,15, 1, 3,14, 5, 2, 8, 4),
             ( 3,15, 0, 6,10, 1,13, 8, 9, 4, 5,11,12, 7, 2,14)),

            (( 2,12, 4, 1, 7,10,11, 6, 8, 5, 3,15,13, 0,14, 9),
             (14,11, 2,12, 4, 7,13, 1, 5, 0,15,10, 3, 9, 8, 6),
             ( 4, 2, 1,11,10,13, 7, 8,15, 9,12, 5, 6, 3, 0,14),
             (11, 8,12, 7, 1,14, 2,13, 6,15, 0, 9,10, 4, 5, 3)),

            ((12, 1,10,15, 9, 2, 6, 8, 0,13, 3, 4,14, 7, 5,11),
             (10,15, 4, 2, 7,12, 9, 5, 6, 1,13,14, 0,11, 3, 8),
             ( 9,14,15, 5, 2, 8,12, 3, 7, 0, 4,10, 1,13,11, 6),
             ( 4, 3, 2,12, 9, 5,15,10,11,14, 1, 7, 6, 0, 8,13)),

            (( 4,11, 2,14,15, 0, 8,13, 3,12, 9, 7, 5,10, 6, 1),
             (13, 0,11, 7, 4, 9, 1,10,14, 3, 5,12, 2,15, 8, 6),
             ( 1, 4,11,13,12, 3, 7,14,10,15, 6, 8, 0, 5, 9, 2),
             ( 6,11,13, 8, 1, 4,10, 7, 9, 5, 0,15,14, 2, 3,12)),

            ((13, 2, 8, 4, 6,15,11, 1,10, 9, 3,14, 5, 0,12, 7),
             ( 1,15,13, 8,10, 3, 7, 4,12, 5, 6,11, 0,14, 9, 2),
             ( 7,11, 4, 1, 9,12,14, 2, 0, 6,10,13,15, 3, 5, 8),
             ( 2, 1,14, 7, 4,10, 8,13,15,12, 9, 0, 3, 5, 6,11)));

  PC_1 : Array [1..56] Of Byte = (57,49,41,33,25,17, 9,
                                   1,58,50,42,34,26,18,
                                  10, 2,59,51,43,35,27,
                                  19,11, 3,60,52,44,36,
                                  63,55,47,39,31,23,15,
                                   7,62,54,46,38,30,22,
                                  14, 6,61,53,45,37,29,
                                  21,13, 5,28,20,12, 4);

  PC_2 : Array [1..48] Of Byte = (14,17,11,24, 1, 5,
                                   3,28,15, 6,21,10,
                                  23,19,12, 4,26, 8,
                                  16, 7,27,20,13, 2,
                                  41,52,31,37,47,55,
                                  30,40,51,45,33,48,
                                  44,49,39,56,34,53,
                                  46,42,50,36,29,32);

  ShiftTable : Array [1..16] Of Byte = (1,1,2,2,2,2,2,2,1,2,2,2,2,2,2,1);

  DESKEYLENGTH = 8;

implementation

{$IFDEF DELPHI}
procedure Register;
  {Registers the Component to the toobar, on the tab named 'Crypto'}
  {Now all a Delphi programmer needs to do is drag n drop to have
   Blowfish encryption}
begin
  RegisterComponents('Crypto', [TDES]);
end;
{$ENDIF}

{==============================TDES============================================}

{$IFDEF DELPHI}
constructor TDES.Create(Owner: TComponent);
{$ENDIF}
{$IFDEF BP7}
constructor TDES.Init;
{$ENDIF}
 begin
  FBLOCKSIZE := 8;
  FIVTemp := nil;
{  GetMem(FIVTemp, FBLOCKSIZE);}
 {$IFDEF DELPHI}
  inherited Create(Owner);
 {$ENDIF}
 {$IFDEF BP7}
  inherited Init;
 {$ENDIF}
end;{TDES.Create}

{$IFDEF DELPHI}
destructor TDES.Destroy;
{$ENDIF}
{$IFDEF BP7}
destructor TDES.Done;
{$ENDIF}
 begin
  {$IFDEF DELPHI}
  inherited Destroy;
  {$ENDIF}
  {$IFDEF BP7}
  inherited Done;
  {$ENDIF}
end;{TDES.Destroy;}

Function TDES.GetBit (Var Data; Index : Byte) : Byte;

Var
  Bits : Array [0..7] Of Byte ABSOLUTE Data;

Begin
  Dec (Index);
  If Bits[Index DIV 8] And (128 SHR (Index MOD 8))>0 then GetBit:=1 Else GetBit:=0;
End;{GetBit}

Procedure TDES.SetBit (Var Data; Index, Value : Byte);

Var
  Bits : Array [0..7] Of Byte ABSOLUTE Data;
  Bit : Byte;

Begin
  Dec (Index);
  Bit:=128 SHR (Index MOD 8);
  Case Value Of
    0 : Bits[Index DIV 8]:=Bits[Index DIV 8] And (Not Bit);
    1 : Bits[Index DIV 8]:=Bits[Index DIV 8] Or Bit;
  End;
End;{SetBit}

Procedure TDES.F (Var FK);

Var
  {R : Array [1..48] Of Byte ABSOLUTE FR;}
  K : Array [1..48] Of Byte ABSOLUTE FK;
  Temp1 : Array [1..48] Of Byte;
  Temp2 : Array [1..32] Of Byte;
  n, h, i, j, Row, Column : Integer;
  {TotalOut : Array [1..32] Of Byte ABSOLUTE Output;}

Begin
  For n:=1 to 48 Do Temp1[n]:=FR[E[n]] Xor K[n];
  For n:=1 to 8 Do Begin
    i:=(n-1)*6;
    j:=(n-1)*4;
    Row:=Temp1[i+1]*2+Temp1[i+6];
    Column:=Temp1[i+2]*8 + Temp1[i+3]*4 + Temp1[i+4]*2 + Temp1[i+5];
    For h:=1 to 4 Do Begin
      Case h Of
        1 : Temp2[j+h]:=(SBoxes[n,Row,Column] And 8) DIV 8;
        2 : Temp2[j+h]:=(SBoxes[n,Row,Column] And 4) DIV 4;
        3 : Temp2[j+h]:=(SBoxes[n,Row,Column] And 2) DIV 2;
        4 : Temp2[j+h]:=(SBoxes[n,Row,Column] And 1);
      End;
    End;
  End;
  For n:=1 to 32 Do FFunctionResult[n]:=Temp2[P[n]];
End;{F}

Procedure TDES.Shift (Var SubKeyPart);

Var
  SKP : Array [1..28] Of Byte ABSOLUTE SubKeyPart;
  n, b : Byte;

Begin
  b:=SKP[1];
  For n:=1 to 27 Do SKP[n]:=SKP[n+1];
  SKP[28]:=b;
End;{Shift}

Procedure TDES.SubKey (Round : Byte; Var SubKey);

Var
  SK : Array [1..48] Of Byte ABSOLUTE SubKey;
  n, b : Byte;

Begin
  For n:=1 to ShiftTable[Round] Do Begin
    Shift (FC);
    Shift (FD);
  End;
  For n:=1 to 48 Do Begin
    b:=PC_2[n];
    If b<=28 then SK[n]:=FC[b] Else SK[n]:= FD[b-28];
  End;
End;{SubKey}

Procedure TDES.SetKeys;
var
 n: BYTE;
 Key: Array [0..7] of BYTE;
begin
  move(FKey[1], Key, DESKEYLENGTH);
  For n:=1 to 28 Do Begin
    FC[n]:=GetBit(Key,PC_1[n]);
    FD[n]:=GetBit(Key,PC_1[n+28]);
  End;
  For n:=1 to 16 Do SubKey (n,FRoundKeys[n]);
end;

Procedure TDES.EncipherBLOCK;
var
  n, i, b, Round : Byte;
begin
  For n:=1 to 64 Do FInputValue[n]:=GetBit (FSmallBuffer,n);
  For n:=1 to 64 Do If n<=32 then FL[n]:=FInputValue[IP[n]] Else FR[n-32]:=FInputValue[IP[n]];
  For Round:=1 to 16 Do Begin
    F (FRoundKeys[Round]);
    For n:=1 to 32 Do FFunctionResult[n]:=FFunctionResult[n] Xor FL[n];
    FL := FR;
    FR := FFunctionResult;
  End;
  For n:=1 to 64 Do Begin
    b:=InvIP[n];
    If b<=32 then FOutputValue[n]:=FR[b] Else FOutputValue[n]:=FL[b-32];
  End;
  For n:=1 to 64 Do SetBit (FSmallBuffer,n,FOutputValue[n]);
end;{TDES.EncipherBLOCK}

Procedure TDES.DecipherBLOCK;
var
  n, i, b, Round : Byte;
begin
  For n:=1 to 64 Do FInputValue[n]:=GetBit (FSmallBuffer,n);
  For n:=1 to 64 Do If n<=32 then FL[n]:=FInputValue[IP[n]] Else FR[n-32]:=FInputValue[IP[n]];
  For Round:=1 to 16 Do Begin
    F (FRoundKeys[17-Round]);
    For n:=1 to 32 Do FFunctionResult[n]:=FFunctionResult[n] Xor FL[n];
    FL := FR;
    FR := FFunctionResult;
  End;
  For n:=1 to 64 Do Begin
    b:=InvIP[n];
    If b<=32 then FOutputValue[n]:=FR[b] Else FOutputValue[n]:=FL[b-32];
  End;
  For n:=1 to 64 Do SetBit (FSmallBuffer,n,FOutputValue[n]);
end;{TDES.DecipherBLOCK}

end.
