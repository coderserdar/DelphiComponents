(**************************************************************)
(*   B-Tree Isam Char Conversion, Strings to Keys Ver 5.55    *)
(*       Copyright (C) 1994 ENZ EDV-Beratung GmbH             *)
(*                All rights reserved                         *)
(*   Modified and used under license by TurboPower Software   *)
(**************************************************************)

{--Conditional defines and compiler options}
  {$I BTDefine.Inc}
  {$F-,V-,B-,S-,I-,R-}
  {$IFDEF CanAllowOverlays}
    {$O+,F+}
  {$ENDIF}
  {$IFDEF CanSetOvrflowCheck}
    {$Q-}
  {$ENDIF}

Unit CCSKeys;

Interface

{$IFNDEF Windows}
Uses
  BaseSupp;
{$ENDIF}

Const
  BadCharValue = 126;
  CPDos437 = 0;
  CPDos850 = 1;
  CPWin1252 = 2;
  CPMaxConst = 2;
  CPNotSupported = 255;

Procedure ConvertChars ( BufPtr       : Pointer;
                         BufLen       : Word;
                         SourceDestCP : Word;
                         OtherCP      : LongInt );
  {-Converts BufLen characters pointed to by BufPtr; the low byte of
    SourceDestCP contains the code page constant for the source character
    set, the high byte for the destination character set; each bit set in
    OtherCP specifies a code page constant that may also be used as source
    or destination}

Function CheckChars ( BufPtr       : Pointer;
                      BufLen       : Word;
                      SourceDestCP : Word;
                      OtherCP      : LongInt ) : LongInt;
  {-Checks BufLen characters pointed to by BufPtr, whether they can be
    converted; the low byte of SourceDestCP contains the code page constant
    for the source character set, the high byte for the destination character
    set; each bit set in OtherCP specifies a code page constant that may also
    be used as source or destination}

Function NLStrToKey ( Var Source       : String;
                          SourceDestCP : Word;
                          OtherCP      : LongInt ) : String;
  {-Builds a key string out of Source and returns it; the low byte of
    SourceDestCP contains the code page constant for the source character
    set, the high byte for the destination character set}

Function GetCodePageConst : Byte;
  {-Returns one of the code page constants or CPNotSupported}

Function IntStrToKey ( S : String ) : String;
  {-Returns a key string assuming the current code page and no others}

Function BuildCodePageMask ( CodePageConst : Byte ) : LongInt;
  {-Returns 1 shifted left by CodePageConst}


Implementation

{--Local define to decide whether Asm instructions are allowed}
  {$UNDEF XXNoVerAsmXX}
  {$IFDEF Ver50}
    {$DEFINE XXNoVerAsmXX}
  {$ENDIF}
  {$IFDEF Ver55}
    {$DEFINE XXNoVerAsmXX}
  {$ENDIF}

{$L CC437WIN.OBJ}
  Procedure CC437WIN; External;
{$L CCWIN437.OBJ}
  Procedure CCWIN437; External;
{$L CC437850.OBJ}
  Procedure CC437850; External;
{$L CC850437.OBJ}
  Procedure CC850437; External;
{$L CC850WIN.OBJ}
  Procedure CC850WIN; External;
{$L CCWIN850.OBJ}
  Procedure CCWIN850; External;

{$L KEYS437.OBJ}
  Procedure KEYS437; External;
{$L KEYS850.OBJ}
  Procedure KEYS850; External;
{$L KEYSWIN.OBJ}
  Procedure KEYSWIN; External;

Type
  ArrConvertTableOfs = Array [0..CPMaxConst, 0..CPMaxConst] Of Word;

{$IFDEF XXNoVerAsmXX}
Type
  ConvertTable = Array [-2..255] Of Byte;
  PConvertTable = ^ConvertTable;

Var
  ArrCvtTblOfs : ArrConvertTableOfs;
{$ELSE}
Const
  ArrCvtTblOfs : ArrConvertTableOfs =
      ( (Ofs (KEYS437), Ofs (CC437850), Ofs (CC437WIN)),
        (Ofs (CC850437), Ofs (KEYS850), Ofs (CC850WIN)),
        (Ofs (CCWIN437), Ofs (CCWIN850), Ofs (KEYSWIN)) );
{$ENDIF}

  Procedure MarkBadChars ( BufPtr       : Pointer;
                           BufLen       : Word;
                           SourceDestCP : Word );

  Var
    TblOfs : Word;

  {$IFDEF XXNoVerAsmXX}
    TblPtr : PConvertTable;
    CCPtr  : ^Byte;
    I      : Word;
  {$ENDIF}

  Begin
    If BufLen = 0 Then Exit;
    If (Lo (SourceDestCP) > CPMaxConst) Or (Hi (SourceDestCP) > CPMaxConst)
        Or (SourceDestCP = Swap (SourceDestCP)) Then Exit;
    TblOfs := ArrCvtTblOfs [Lo (SourceDestCP), Hi (SourceDestCP)];

  {$IFDEF XXNoVerAsmXX}
    TblPtr := Ptr ( CSeg, TblOfs );
    For I := 0 To Pred (BufLen) Do Begin
      CCPtr := Ptr ( Seg (BufPtr^), Ofs (BufPtr^) + I );
      If (CCPtr^ < TblPtr^ [-2]) Or (CCPtr^ >= TblPtr^ [-1]) Then Begin
        If TblPtr^ [CCPtr^] = BadCharValue Then Begin
          CCPtr^ := BadCharValue;
        End;
      End;
    End;
  {$ELSE}
    Asm
                        Push DS
                        Lds SI, BufPtr
                        Les DI, BufPtr
                        Mov BX, TblOfs
                        Mov CX, BufLen
                        SegCS Mov DX, [BX]
                        Add BX, 2
                        Cld

      @@DoLoop:
                        LodSB
                        Cmp AL, DL
                        Jc @@DoTable
                        Cmp AL, DH
                        Jc @@Skip

      @@DoTable:
                        SegCS XLat
                        Cmp AL, BadCharValue
                        Jnz @@Skip

                        Mov DI, SI
                        Dec DI
                        StoSB

      @@Skip:
                        Loop @@DoLoop
                        Pop DS
    End;
  {$ENDIF}
  End;


  Procedure ConvertChars ( BufPtr       : Pointer;
                           BufLen       : Word;
                           SourceDestCP : Word;
                           OtherCP      : LongInt );


  Var
    I,
    TblOfs : Word;

  {$IFDEF XXNoVerAsmXX}
    TblPtr : PConvertTable;
    CCPtr  : ^Byte;
  {$ENDIF}

  Begin
    If BufLen = 0 Then Exit;
    If (Lo (SourceDestCP) > CPMaxConst) Or (Hi (SourceDestCP) > CPMaxConst)
        Then Exit;

  {$IFDEF XXNoVerAsmXX}
    If SourceDestCP <> Swap (SourceDestCP) Then Begin
      TblOfs := ArrCvtTblOfs [Lo (SourceDestCP), Hi (SourceDestCP)];
      TblPtr := Ptr ( CSeg, TblOfs );
      For I := 0 To Pred (BufLen) Do Begin
        CCPtr := Ptr ( Seg (BufPtr^), Ofs (BufPtr^) + I );
        If (CCPtr^ < TblPtr^ [-2]) Or (CCPtr^ >= TblPtr^ [-1]) Then Begin
          CCPtr^ := TblPtr^ [CCPtr^];
        End;
      End;
    End;
  {$ELSE}
    If SourceDestCP <> Swap (SourceDestCP) Then Begin
      TblOfs := ArrCvtTblOfs [Lo (SourceDestCP), Hi (SourceDestCP)];
      Asm
                          Push DS
                          Lds SI, BufPtr
                          Les DI, BufPtr
                          Mov BX, TblOfs
                          Mov CX, BufLen
                          SegCS Mov DX, [BX]
                          Add BX, 2
                          Cld
  
        @@DoLoop:
                          LodSB
                          Cmp AL, DL
                          Jc @@DoTable
                          Cmp AL, DH
                          Jc @@Skip
  
        @@DoTable:
                          SegCS XLat
                          Mov DI, SI
                          Dec DI
                          StoSB
  
        @@Skip:
                          Loop @@DoLoop
                          Pop DS
      End;
    End;
  {$ENDIF}

    If OtherCP <> 0 Then Begin
      OtherCP := OtherCP And (Not BuildCodePageMask ( Lo (SourceDestCP) ))
          And (Not BuildCodePageMask ( Hi (SourceDestCP) ));
        {-Mask out already processed code pages}
      I := 0;
      While (OtherCP <> 0) And (I <= CPMaxConst) Do Begin
        If Odd (OtherCP) And ((SourceDestCP And $FF) <> I)
            And ((SourceDestCP Shr 8) <> I) Then Begin
          MarkBadChars ( BufPtr, BufLen,
              Swap (SourceDestCP) And $FF Or Swap (I) );
        End;
        OtherCP := OtherCP Shr 1;
        Inc (I);
      End;
    End;
  End;


  Function CheckChars ( BufPtr       : Pointer;
                        BufLen       : Word;
                        SourceDestCP : Word;
                        OtherCP      : LongInt ) : LongInt;


  Var
    I,
    TblOfs  : Word;
    FuncRes : LongInt;

  {$IFDEF XXNoVerAsmXX}
    TblPtr : PConvertTable;
    CCPtr  : ^Byte;
  {$ENDIF}

  Begin
    FuncRes := 0;
    CheckChars := 0;
    If BufLen = 0 Then Exit;
    If (Lo (SourceDestCP) > CPMaxConst) Or (Hi (SourceDestCP) > CPMaxConst)
        Then Exit;

  {$IFDEF XXNoVerAsmXX}
    If SourceDestCP <> Swap (SourceDestCP) Then Begin
      TblOfs := ArrCvtTblOfs [Lo (SourceDestCP), Hi (SourceDestCP)];
      TblPtr := Ptr ( CSeg, TblOfs );
      For I := 0 To Pred (BufLen) Do Begin
        CCPtr := Ptr ( Seg (BufPtr^), Ofs (BufPtr^) + I );
        If (CCPtr^ < TblPtr^ [-2]) Or (CCPtr^ >= TblPtr^ [-1]) Then Begin
          If TblPtr^ [CCPtr^] = BadCharValue Then Begin
            CheckChars := $10000 + I;
            Exit;
          End;
        End;
      End;
    End;
  {$ELSE}
    If SourceDestCP <> Swap (SourceDestCP) Then Begin
      TblOfs := ArrCvtTblOfs [Lo (SourceDestCP), Hi (SourceDestCP)];
      Asm
                          Push DS
                          Lds SI, BufPtr
                          Mov BX, TblOfs
                          Mov CX, BufLen
                          SegCS Mov DX, [BX]
                          Add BX, 2
                          Cld
  
        @@DoLoop:
                          LodSB
                          Cmp AL, DL
                          Jc @@DoTable
                          Cmp AL, DH
                          Jc @@Skip

        @@DoTable:
                          SegCS XLat
                          Cmp AL, BadCharValue
                          Jnz @@Skip

                          Lea DI, FuncRes
                          Mov AX, BufLen
                          Sub AX, CX
                          Mov SS:[DI], AX
                          Mov AX, 1
                          Mov SS:[DI+2], AX
                          Jmp @@ExitPoint

        @@Skip:
                          Loop @@DoLoop
  
        @@ExitPoint:
                          Pop DS
      End;
    End;
  {$ENDIF}

    CheckChars := FuncRes;
    If FuncRes <> 0 Then Exit;

    If OtherCP <> 0 Then Begin
      OtherCP := OtherCP And (Not BuildCodePageMask ( Lo (SourceDestCP) ))
          And (Not BuildCodePageMask ( Hi (SourceDestCP) ));
        {-Mask out already processed code pages}
      I := 0;
      While (OtherCP <> 0) And (I <= CPMaxConst) Do Begin
        If Odd (OtherCP) And ((SourceDestCP And $FF) <> I)
            And ((SourceDestCP Shr 8) <> I) Then Begin
          FuncRes := CheckChars ( BufPtr, BufLen,
              SourceDestCP And $FF Or Swap (I), 0 );
          If FuncRes <> 0 Then Begin
            CheckChars := FuncRes;
            Exit;
          End;
        End;
        OtherCP := OtherCP Shr 1;
        Inc (I);
      End;
    End;
  End;


  Function NLStrToKey ( Var Source       : String;
                            SourceDestCP : Word;
                            OtherCP      : LongInt ) : String;

  Var
    I,
    TblOfs : Word;

  {$IFDEF XXNoVerAsmXX}
    TblPtr : PConvertTable;
    CCPtr  : ^Byte;
    S      : String;
  {$ENDIF}

  Begin
    NLStrToKey := '';
    If Length (Source) = 0 Then Exit;
    If (Lo (SourceDestCP) > CPMaxConst) Or (Hi (SourceDestCP) > CPMaxConst)
        Then Exit;
    TblOfs := ArrCvtTblOfs [Hi (SourceDestCP), Hi (SourceDestCP)];

  {$IFDEF XXNoVerAsmXX}
    S := Source;
    If (SourceDestCP <> Swap (SourceDestCP)) Or (OtherCP <> 0) Then Begin
      ConvertChars ( @S [1], Length (S), SourceDestCP, OtherCP );
    End;
    TblPtr := Ptr ( CSeg, TblOfs );
    For I := 0 To Pred (Length (S)) Do Begin
      CCPtr := Ptr ( Seg (S), Ofs (S [1]) + I );
      If (CCPtr^ < TblPtr^ [-2]) Or (CCPtr^ >= TblPtr^ [-1]) Then Begin
        CCPtr^ := TblPtr^ [CCPtr^];
      End;
    End;
    NLStrToKey := S;
  {$ELSE}
    NLStrToKey := Source;
    Asm
                        Mov DX, SourceDestCP
                        Cmp DH, DL
                        Jnz @@DoConvert
                        Lea DI, OtherCP
                        Mov AX, SS:[DI]
                        Or AX, SS:[DI+2]
                        Jz @@NoConvert

      @@DoConvert:
                        Les DI, @Result
                        Mov AL, ES:[DI]
                        Xor AH, AH
                        Inc DI
                        Push ES
                        Push DI
                        Push AX
                        Push DX
                        Lea DI, OtherCP
                        Mov AX, SS:[DI+2]
                        Push AX
                        Mov AX, SS:[DI]
                        Push AX
                        Call ConvertChars

      @@NoConvert:
                        Push DS
                        Lds SI, @Result
                        Les DI, @Result
                        Mov CL, ES:[DI]
                        Xor CH, CH
                        Inc SI
                        Inc DI
                        Mov BX, TblOfs
                        SegCS Mov DX, [BX]
                        Add BX, 2
                        Cld

      @@DoLoop:
                        LodSB
                        Cmp AL, DL
                        Jc @@DoTable
                        Cmp AL, DH
                        Jc @@Skip

      @@DoTable:
                        SegCS XLat
                        Mov DI, SI
                        Dec DI
                        StoSB

      @@Skip:
                        Loop @@DoLoop
                        Pop DS
    End;
  {$ENDIF}
  End;


  Function GetCodePageConst : Byte;

{$IFNDEF Windows}
  Var
    Regs : GenRegisters;
{$ENDIF}


  Begin
  {$IFDEF Windows}
    GetCodePageConst := CPWin1252;
  {$ELSE}
    GetCodePageConst := CPDos437;
    If Swap (CallDosVersion) < $031E Then Exit;
      {-Dos versions < 3.3 don't have code page support}
    GetCodePageConst := CPNotSupported;
    DefaultRegisters ( Regs );
    With Regs Do Begin
      AX := $6601;           {get code page}
      CallMsDos ( Regs );
      If Odd (Flags) Then Exit;
      Case BX Of
        437 : GetCodePageConst := CPDos437;
        850 : GetCodePageConst := CPDos850;
      End; {Case}
    End;
  {$ENDIF}
  End;


  Function IntStrToKey ( S : String ) : String;

  Var
    CP : Byte;

  Begin
    CP := GetCodePageConst;
    IntStrToKey := NLStrToKey ( S, Word (CP) Shl 8 Or CP, 0 );
  End;


  Function BuildCodePageMask ( CodePageConst : Byte ) : LongInt;

  Begin
    BuildCodePageMask := LongInt (1) Shl CodePageConst;
  End;


{$IFDEF XXNoVerAsmXX}
Begin
  ArrCvtTblOfs [0, 0] := Ofs (KEYS437);
  ArrCvtTblOfs [0, 1] := Ofs (CC437850);
  ArrCvtTblOfs [0, 2] := Ofs (CC437WIN);
  ArrCvtTblOfs [1, 0] := Ofs (CC850437);
  ArrCvtTblOfs [1, 1] := Ofs (KEYS850);
  ArrCvtTblOfs [1, 2] := Ofs (CC850WIN);
  ArrCvtTblOfs [2, 0] := Ofs (CCWIN437);
  ArrCvtTblOfs [2, 1] := Ofs (CCWIN850);
  ArrCvtTblOfs [2, 2] := Ofs (KEYSWIN);

 {$UNDEF XXNoVerAsmXX}
{$ELSE}
 {$IFDEF InitAllUnits}
Begin
 {$ENDIF}
{$ENDIF}


End.
