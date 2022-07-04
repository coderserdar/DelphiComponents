{$A+} { word align }
{$O+} { ?? ?? }

unit lzh;
(*
 * LZHUF.C English version 1.0
 * Based on Japanese version 29-NOV-1988
 * LZSS coded by Haruhiko OKUMURA
 * Adaptive Huffman Coding coded by Haruyasu YOSHIZAKI
 * Edited and translated to English by Kenji RIKITAKE
 * Translated from C to Turbo Pascal by Douglas Webb   2/18/91
 *    Update and bug correction of TP version 4/29/91 (Sorry!!)
 *    Added Delphi exception handling may 09 1996 Danny Heijl
 *                                                Danny.Heijl@cevi.be
 * Added support for Delphi streams Aug. 05 1999
 *    Bruno Depero (bdepero@usa.net) and
 *    Kim Madsen (kbm@optical.dk)
 *)

{
     This unit allows the user to compress data using a combination of
   LZSS compression and adaptive Huffman coding, or conversely to decompress
   data that was previously compressed by this unit.

     There are a number of options as to where the data being compressed/
   decompressed is coming from/going to.

   In fact it requires that you pass the "LZHPack" procedure 2 procedural
  parameter of type 'GetProcType' and 'PutProcType' (declared below) which
  will accept 3 parameters and act in every way like a 'BlockRead'/
  'BlockWrite' procedure call. Your 'GetBytesProc' procedure should return
  the data to be compressed, and Your 'PutBytesProc' procedure should do
  something with the compressed data (ie., put it in a file).  In case you
  need to know (and you do if you want to decompress this data again) the
  number of bytes in the compressed data (original, not compressed size)
  is returned in 'Bytes_Written'.

  GetBytesProc = PROCEDURE(VAR DTA; NBytes:WORD; VAR Bytes_Got : WORD);

  DTA is the start of a memory location where the information returned
  should be.  NBytes is the number of bytes requested.  The actual number
  of bytes returned must be passed in Bytes_Got (if there is no more data
  then 0 should be returned).

  PutBytesProc = PROCEDURE(VAR DTA; NBytes:WORD; VAR Bytes_Got : WORD);

  As above except instead of asking for data the procedure is dumping out
  compressed data, do somthing with it.


   "LZHUnPack" is basically the same thing in reverse.  It requires
  procedural parameters of type 'PutProcType'/'GetProcType' which
  will act as above.  'GetProcType' must retrieve data compressed using
  "LZHPack" (above) and feed it to the unpacking routine as requested.
  'PutProcType' must accept the decompressed data and do something
  withit.  You must also pass in the original size of the decompressed data,
  failure to do so will have adverse results.


   Don't forget that as procedural parameters the 'GetProcType'/'PutProcType'
  procedures must be compiled in the 'F+' state to avoid a catastrophe.

}

{ Note: All the large data structures for these routines are allocated when
  needed from the heap, and deallocated when finished.  So when not in use
  memory requirements are minimal.  However, this unit uses about 34K of
  heap space, and 400 bytes of stack when in use. }

{$R-} { NO range checking !! }

interface


uses Sysutils,Classes;

{$IFDEF WIN32}
type Int16 = SmallInt;
{$ELSE}
type Int16 = Integer;
{$ENDIF}

{.$DEFINE DEBUG}
{$IFDEF DEBUG}
  {$D+}
{$ENDIF}

TYPE

  ElzhException = Class(Exception);

  TWriteProc = procedure(VAR DTA; NBytes:WORD; VAR Bytes_Put : WORD) of object;

  PutBytesProc = TwriteProc;
  {
   Your 'PutBytesProc' procedure should do something with the compressed
   data (ie., put it in a file).

   DTA is the start of a memory location where the information returned
   should be.  NBytes is the number of bytes requested.  The actual number
   of bytes put should be returned in Bytes_Got.

   Don't forget that as procedural parameters the 'GetProcType'/'PutProcType'
  procedures must be compiled in the 'F+' state to avoid a catastrophe.
  }

  TReadProc = procedure(VAR DTA; NBytes:WORD; VAR Bytes_Got : WORD) of object;
  GetBytesProc = TReadProc;
  {
   Your 'GetBytesProc' procedure should return the data to be compressed.
   In case you need to know (and you do if you want to decompress this
   data again) the number of bytes in the compressed data (original, not
   compressed size) is returned in 'Bytes_Written'.

   DTA is the start of a memory location where the information returned
   should be.  NBytes is the number of bytes requested.  The actual number
   of bytes returned must be passed in Bytes_Got (if there is no more data
   then 0 should be returned).

   Don't forget that as procedural parameters the 'GetProcType'/'PutProcType'
  procedures must be compiled in the 'F+' state to avoid a catastrophe.
  }

CONST
  EXIT_OK = 0;
  EXIT_FAILED = 1;
{ LZSS Parameters }
  N                 = 4096;   { Size of string buffer }
  F                 = 60;        { Size of look-ahead buffer }
  THRESHOLD   = 2;
  NUL           = N;    { End of tree's node  }


{ Huffman coding parameters }
  N_CHAR   =      (256 - THRESHOLD + F);
                                     { character code (:= 0..N_CHAR-1) }
  T      =  (N_CHAR * 2 - 1);  { Size of table }
  R      =  (T - 1);                 { root position }
  MAX_FREQ =      $8000;
                                             { update when cumulative frequency }
                                             { reaches to this value }
{
 * Tables FOR encoding/decoding upper 6 bits of
 * sliding dictionary pointer
 }
{ encoder table }
  p_len : Array[0..63] of BYTE =
       ($03, $04, $04, $04, $05, $05, $05, $05,
      $05, $05, $05, $05, $06, $06, $06, $06,
      $06, $06, $06, $06, $06, $06, $06, $06,
      $07, $07, $07, $07, $07, $07, $07, $07,
      $07, $07, $07, $07, $07, $07, $07, $07,
      $07, $07, $07, $07, $07, $07, $07, $07,
      $08, $08, $08, $08, $08, $08, $08, $08,
      $08, $08, $08, $08, $08, $08, $08, $08);

  p_code : Array [0..63] OF BYTE =
       ($00, $20, $30, $40, $50, $58, $60, $68,
      $70, $78, $80, $88, $90, $94, $98, $9C,
      $A0, $A4, $A8, $AC, $B0, $B4, $B8, $BC,
      $C0, $C2, $C4, $C6, $C8, $CA, $CC, $CE,
      $D0, $D2, $D4, $D6, $D8, $DA, $DC, $DE,
      $E0, $E2, $E4, $E6, $E8, $EA, $EC, $EE,
      $F0, $F1, $F2, $F3, $F4, $F5, $F6, $F7,
      $F8, $F9, $FA, $FB, $FC, $FD, $FE, $FF);

{ decoder table }
  d_code: Array [0..255] OF BYTE =
       ($00, $00, $00, $00, $00, $00, $00, $00,
      $00, $00, $00, $00, $00, $00, $00, $00,
      $00, $00, $00, $00, $00, $00, $00, $00,
      $00, $00, $00, $00, $00, $00, $00, $00,
      $01, $01, $01, $01, $01, $01, $01, $01,
      $01, $01, $01, $01, $01, $01, $01, $01,
      $02, $02, $02, $02, $02, $02, $02, $02,
      $02, $02, $02, $02, $02, $02, $02, $02,
      $03, $03, $03, $03, $03, $03, $03, $03,
      $03, $03, $03, $03, $03, $03, $03, $03,
      $04, $04, $04, $04, $04, $04, $04, $04,
      $05, $05, $05, $05, $05, $05, $05, $05,
      $06, $06, $06, $06, $06, $06, $06, $06,
      $07, $07, $07, $07, $07, $07, $07, $07,
      $08, $08, $08, $08, $08, $08, $08, $08,
      $09, $09, $09, $09, $09, $09, $09, $09,
      $0A, $0A, $0A, $0A, $0A, $0A, $0A, $0A,
      $0B, $0B, $0B, $0B, $0B, $0B, $0B, $0B,
      $0C, $0C, $0C, $0C, $0D, $0D, $0D, $0D,
      $0E, $0E, $0E, $0E, $0F, $0F, $0F, $0F,
      $10, $10, $10, $10, $11, $11, $11, $11,
      $12, $12, $12, $12, $13, $13, $13, $13,
      $14, $14, $14, $14, $15, $15, $15, $15,
      $16, $16, $16, $16, $17, $17, $17, $17,
      $18, $18, $19, $19, $1A, $1A, $1B, $1B,
      $1C, $1C, $1D, $1D, $1E, $1E, $1F, $1F,
      $20, $20, $21, $21, $22, $22, $23, $23,
      $24, $24, $25, $25, $26, $26, $27, $27,
      $28, $28, $29, $29, $2A, $2A, $2B, $2B,
      $2C, $2C, $2D, $2D, $2E, $2E, $2F, $2F,
      $30, $31, $32, $33, $34, $35, $36, $37,
      $38, $39, $3A, $3B, $3C, $3D, $3E, $3F);

 d_len: Array[0..255] of BYTE =
       ($03, $03, $03, $03, $03, $03, $03, $03,
      $03, $03, $03, $03, $03, $03, $03, $03,
      $03, $03, $03, $03, $03, $03, $03, $03,
      $03, $03, $03, $03, $03, $03, $03, $03,
      $04, $04, $04, $04, $04, $04, $04, $04,
      $04, $04, $04, $04, $04, $04, $04, $04,
      $04, $04, $04, $04, $04, $04, $04, $04,
      $04, $04, $04, $04, $04, $04, $04, $04,
      $04, $04, $04, $04, $04, $04, $04, $04,
      $04, $04, $04, $04, $04, $04, $04, $04,
      $05, $05, $05, $05, $05, $05, $05, $05,
      $05, $05, $05, $05, $05, $05, $05, $05,
      $05, $05, $05, $05, $05, $05, $05, $05,
      $05, $05, $05, $05, $05, $05, $05, $05,
      $05, $05, $05, $05, $05, $05, $05, $05,
      $05, $05, $05, $05, $05, $05, $05, $05,
      $05, $05, $05, $05, $05, $05, $05, $05,
      $05, $05, $05, $05, $05, $05, $05, $05,
      $06, $06, $06, $06, $06, $06, $06, $06,
      $06, $06, $06, $06, $06, $06, $06, $06,
      $06, $06, $06, $06, $06, $06, $06, $06,
      $06, $06, $06, $06, $06, $06, $06, $06,
      $06, $06, $06, $06, $06, $06, $06, $06,
      $06, $06, $06, $06, $06, $06, $06, $06,
      $07, $07, $07, $07, $07, $07, $07, $07,
      $07, $07, $07, $07, $07, $07, $07, $07,
      $07, $07, $07, $07, $07, $07, $07, $07,
      $07, $07, $07, $07, $07, $07, $07, $07,
      $07, $07, $07, $07, $07, $07, $07, $07,
      $07, $07, $07, $07, $07, $07, $07, $07,
      $08, $08, $08, $08, $08, $08, $08, $08,
      $08, $08, $08, $08, $08, $08, $08, $08);

TYPE
  Freqtype = Array[0..T] OF WORD;
  FreqPtr = ^freqtype;
  PntrType = Array[0..PRED(T+N_Char)] OF Int16;
  pntrPtr = ^pntrType;
  SonType = Array[0..PRED(T)] OF Int16;
  SonPtr = ^SonType;


  TextBufType = Array[0..N+F-2] OF BYTE;
  TBufPtr = ^TextBufType;
  WordRay = Array[0..N] OF Int16;
  WordRayPtr = ^WordRay;
  BWordRay = Array[0..N+256] OF Int16;
  BWordRayPtr = ^BWordRay;


  {PG 17/09/98}
  TLZH = class
  Private
    code, len : WORD;
    Procedure InitTree;  { Initializing tree }
    Procedure InsertNode(r : Int16);  { Inserting node to the tree }
    Procedure DeleteNode(p: Int16);  { Deleting node from the tree }
    Function GetBit(GetBytes:GetBytesProc): Int16;    { get one bit }
    Function GetByte(GetBytes:GetBytesProc): Int16;   { get a byte }
    Procedure update(c : Int16);
    Procedure StartHuff;
    PROCEDURE Putcode(l : Int16; c: WORD;PutBytes:PutBytesProc);        { output c bits }
    PROCEDURE reconst;
    PROCEDURE EncodeChar(c: WORD;PutBytes:PutBytesProc);
    Procedure EncodePosition(c : WORD;PutBytes:PutBytesProc);
    Procedure EncodeEnd(PutBytes:PutBytesProc);
    FUNCTION DecodeChar(GetBytes:GetBytesProc): Int16;
    Function DecodePosition(GetBytes:GetBytesProc) : WORD;
    Procedure InitLZH;
    Procedure EndLZH;

  Public
    StreamIn,StreamOut:TStream;

    getbuf : WORD;
    getlen : BYTE;
    putlen : BYTE;
    putbuf : WORD;
    textsize : longint;
    codesize : longINT;
    printcount : longint ;
    match_position : Int16 ;
    match_length : Int16;

    text_buf : TBufPtr;
    lson,dad : WordRayPtr;
    rson : BWordRayPtr;
    freq : FreqPtr;     { cumulative freq table }

  {
   * pointing parent nodes.
   * area [T..(T + N_CHAR - 1)] are pointers FOR leaves
   }
    prnt : PntrPtr;

  { pointing children nodes (son[], son[] + 1)}
    son : SonPtr;

    Procedure LZHPack(VAR Bytes_Written:LongInt; GetBytes:GetBytesProc;
                                                 PutBytes:PutBytesProc);
      {#XLZHUnPack}
      {
         This procedure allows the user to compress data using a combination of
       LZSS compression and adaptive Huffman coding.

         There are a number of options as to where the data being compressed
      is coming from.

       In fact it requires that you pass the "LZHPack" procedure 2 procedural
      parameter of type 'GetProcType' and 'PutProcType' (declared below) which
      will accept 3 parameters and act in every way like a 'BlockRead'/
      'BlockWrite' procedure call. Your 'GetBytesProc' procedure should return
      the data to be compressed, and Your 'PutBytesProc' procedure should do
      something with the compressed data (ie., put it in a file).  In case you
      need to know (and you do if you want to decompress this data again) the
      number of bytes in the compressed data (original, not compressed size)
      is returned in 'Bytes_Written'.

      DTA is the start of a memory location where the information returned
      should be.  NBytes is the number of bytes requested.  The actual number
      of bytes returned must be passed in Bytes_Got (if there is no more data
      then 0 should be returned).

      As above except instead of asking for data the procedure is dumping out
      compressed data, do somthing with it.

      }
    Procedure LZHUnpack(TextSize : Longint; GetBytes:GetBytesProc;
                                            PutBytes: PutBytesProc);
      {#X LZHPack}
      {
        "LZHUnPack" is basically the same as LZHPack in reverse.  It requires
      procedural parameters of type 'PutProcType'/'GetProcType' which
      will act as above.  'GetProcType' must retrieve data compressed using
      "LZHPack" (above) and feed it to the unpacking routine as requested.
      'PutProcType' must accept the decompressed data and do something
      withit.  You must also pass in the original size of the decompressed data,
      failure to do so will have adverse results.
      }

    procedure GetBlockStream(var DTA; NBytes: Word; var Bytes_Got: Word);
    procedure PutBlockStream(var DTA; NBytes: Word; var Bytes_Got: Word);
  End;

implementation

Procedure TLZH.InitTree;  { Initializing tree }
VAR
  i : Int16;
BEGIN
  FOR i := N + 1 TO N + 256  DO rson^[i] := NUL;      { root }
  FOR i := 0 TO N DO dad^[i] := NUL;                  { node }
END;

Procedure TLZH.InsertNode(r : Int16);  { Inserting node to the tree }
VAR
  tmp,i, p, cmp : Int16;
  key : TBufPtr;
  c : WORD;
BEGIN
  cmp := 1;
  key := @text_buf^[r];
  p := SUCC(N) + key^[0];
  rson^[r] := NUL;
  lson^[r] := NUL;
  match_length := 0;
  WHILE match_length < F DO BEGIN
    IF (cmp >= 0) THEN BEGIN
          IF (rson^[p] <> NUL) THEN begin
        p := rson^[p]
      end
          ELSE BEGIN
            rson^[p] := r;
                dad^[r] := p;
                exit;
      END;
    END
    ELSE BEGIN
      IF (lson^[p] <> NUL) THEN  begin
             p := lson^[p]
      end
      ELSE BEGIN
        lson^[p] := r;
                dad^[r] := p;
                exit;
      END;
    END;
    i := 0;
    cmp := 0;
        While (i < F) AND (cmp = 0) DO BEGIN
      inc(i);
      cmp := key^[i] - text_buf^[p + i];
    END;
    IF (i > THRESHOLD) THEN BEGIN
      tmp := PRED((r - p) AND PRED(N));
          IF (i > match_length) THEN BEGIN
        match_position := tmp;
        match_length := i;
      END;
          IF (match_length < F) AND (i = match_length) THEN BEGIN
        c := tmp;
                IF (c < match_position) THEN begin
          match_position := c;
        end;
      END;
    END; { if i > threshold }
  END; { WHILE match_length < F }
  dad^[r] := dad^[p];
  lson^[r] := lson^[p];
  rson^[r] := rson^[p];
  dad^[lson^[p]] := r;
  dad^[rson^[p]] := r;
  IF (rson^[dad^[p]] = p) THEN begin
       rson^[dad^[p]] := r;
  end
  ELSE begin
    lson^[dad^[p]] := r;
  end;
  dad^[p] := NUL;  { remove p }
END;

Procedure TLZH.DeleteNode(p: Int16);  { Deleting node from the tree }
VAR
  q : Int16;
BEGIN
  IF (dad^[p] = NUL) THEN exit;                 { unregistered }

  IF (rson^[p] = NUL) THEN begin
   q := lson^[p];
  end
  ELSE begin
    IF (lson^[p] = NUL) THEN begin
      q := rson^[p];
    end
    ELSE BEGIN
      q := lson^[p];
      IF (rson^[q] <> NUL) THEN BEGIN
        REPEAT
          q := rson^[q];
        UNTIL (rson^[q] = NUL);
        rson^[dad^[q]] := lson^[q];
        dad^[lson^[q]] := dad^[q];
        lson^[q] := lson^[p];
        dad^[lson^[p]] := q;
      END;
      rson^[q] := rson^[p];
      dad^[rson^[p]] := q;
    END;
  end;
  dad^[q] := dad^[p];

  IF (rson^[dad^[p]] = p) THEN
    rson^[dad^[p]] := q
  ELSE
    lson^[dad^[p]] := q;

  dad^[p] := NUL;
END;

{ Huffman coding parameters }

Function TLZH.GetBit(GetBytes:GetBytesProc): Int16;   { get one bit }
VAR
  i: BYTE;
  i2 : Int16;
  Wresult : Word;
BEGIN
  WHILE (getlen <= 8) DO BEGIN
    GetBytes(i,1,Wresult);
    If Wresult = 1 THEN
      i2 := i
    ELSE
      i2 := 0;

    getbuf := getbuf OR (i2 SHL (8 - getlen));
    INC(getlen,8);
  END;
  i2 := getbuf;
  getbuf := getbuf SHL 1;
  DEC(getlen);
  getbit := Int16((i2 < 0));

END;

Function TLZH.GetByte(GetBytes:GetBytesProc): Int16;  { get a byte }
VAR
  j : BYTE;
  i,Wresult : WORD;
BEGIN
  WHILE (getlen <= 8) DO BEGIN
    GetBytes(j,1,Wresult);
    If Wresult = 1 THEN
      i := j
    ELSE
      i := 0;

    getbuf := getbuf OR (i SHL (8 - getlen));
    INC(getlen,8);
  END;
  i := getbuf;
  getbuf := getbuf SHL 8;
  DEC(getlen,8);
  getbyte := Int16(i SHR 8);
END;

PROCEDURE TLZH.Putcode(l : Int16; c: WORD;PutBytes:PutBytesProc);       { output c bits }
VAR
  Temp : BYTE;
  Got : WORD;
BEGIN
  putbuf := putbuf OR (c SHR putlen);
  inc(putlen,l);
  IF (putlen >= 8) THEN BEGIN
    Temp := putbuf SHR 8;
    PutBytes(Temp,1,Got);
    DEC(putlen,8);
    IF (putlen  >= 8) THEN BEGIN
      Temp := Lo(PutBuf);
      PutBytes(Temp,1,Got);
      INC(codesize,2);
      DEC(putlen,8);
      putbuf := c SHL (l - putlen);
    END
    ELSE BEGIN
          putbuf := putbuf SHL 8;
          INC(codesize);
    END;
  END;
END;

{ initialize freq tree }

Procedure TLZH.StartHuff;
VAR
  i, j : Int16;
BEGIN
  FOR i := 0 to PRED(N_CHAR) DO BEGIN
    freq^[i] := 1;
    son^[i] := i + T;
    prnt^[i + T] := i;
  END;
  i := 0;
  j := N_CHAR;
  WHILE (j <= R) DO BEGIN
    freq^[j] := freq^[i] + freq^[i + 1];
    son^[j] := i;
    prnt^[i] := j;
    prnt^[i + 1] := j;
    INC(i,2);
    INC(j);
  END;
  freq^[T] := $ffff;
  prnt^[R] := 0;
END;

{ reconstruct freq tree }

PROCEDURE TLZH.reconst;
VAR
 i, j, k, tmp : Int16;
 f, l : WORD;
BEGIN
 { halven cumulative freq FOR leaf nodes }
  j := 0;
  FOR i := 0 to PRED(T) DO BEGIN
    IF (son^[i] >= T) THEN BEGIN
      freq^[j] := SUCC(freq^[i]) DIV 2;    {@@ Bug Fix MOD -> DIV @@}
      son^[j] := son^[i];
      INC(j);
    END;
  END;
  { make a tree : first, connect children nodes }
  i := 0;
  j := N_CHAR;
  WHILE (j < T) DO BEGIN
    k := SUCC(i);
    f := freq^[i] + freq^[k];
    freq^[j] := f;
    k := PRED(j);
    WHILE f < freq^[k] DO DEC(K);
    INC(k);
    l := (j - k) SHL 1;
    tmp := SUCC(k);
    move(freq^[k], freq^[tmp], l);
    freq^[k] := f;
    move(son^[k], son^[tmp], l);
    son^[k] := i;
    INC(i,2);
    INC(j);
  END;
      { connect parent nodes }
  FOR i := 0 to PRED(T) DO BEGIN
    k := son^[i];
    IF (k >= T) THEN BEGIN
          prnt^[k] := i;
    END
    ELSE BEGIN
          prnt^[k] := i;
      prnt^[SUCC(k)] := i;
        END;
  END;

END;

{ update freq tree }

Procedure TLZH.update(c : Int16);
VAR
  i, j, k, l : Int16;
BEGIN
  IF (freq^[R] = MAX_FREQ) THEN BEGIN
    reconst;
  END;
  c := prnt^[c + T];
  REPEAT
   INC(freq^[c]);
   k := freq^[c];

      { swap nodes to keep the tree freq-ordered }
   l := SUCC(C);
   IF (k > freq^[l]) THEN BEGIN
     WHILE (k > freq^[l]) DO INC(l);
     DEC(l);
     freq^[c] := freq^[l];
     freq^[l] := k;

     i := son^[c];
     prnt^[i] := l;
     IF (i < T) THEN prnt^[SUCC(i)] := l;

     j := son^[l];
     son^[l] := i;

     prnt^[j] := c;
     IF (j < T) THEN prnt^[SUCC(j)] := c;
     son^[c] := j;

     c := l;
   END;
   c := prnt^[c];
 UNTIL (c = 0);   { REPEAT it until reaching the root }
END;

PROCEDURE TLZH.EncodeChar(c: WORD;PutBytes:PutBytesProc);
VAR
  i : WORD;
  j, k : Int16;
BEGIN
  i := 0;
  j := 0;
  k := prnt^[c + T];

      { search connections from leaf node to the root }
  REPEAT
    i := i SHR 1;

      {
      IF node's address is odd, output 1
      ELSE output 0
      }
    IF BOOLEAN(k AND 1) THEN INC(i,$8000);
    INC(j);
    k := prnt^[k];
  UNTIL (k = R);
  Putcode(j, i,PutBytes);
  code := i;
  len := j;
  update(c);
END;

Procedure TLZH.EncodePosition(c : WORD;PutBytes:PutBytesProc);
VAR
  i,j : WORD;
BEGIN
      { output upper 6 bits with encoding }
  i := c SHR 6;
  j := p_code[i];
  Putcode(p_len[i],j SHL 8,PutBytes);

      { output lower 6 bits directly }
  Putcode(6, (c AND $3f) SHL 10,PutBytes);
END;

Procedure TLZH.EncodeEnd(PutBytes:PutBytesProc);
VAR
  Temp : BYTE;
  Got : WORD;
BEGIN
  IF BOOLEAN(putlen) THEN BEGIN
    Temp := Lo(putbuf SHR 8);
    PutBytes(Temp,1,Got);
    INC(codesize);
  END;
END;

FUNCTION TLZH.DecodeChar(GetBytes:GetBytesProc): Int16;
VAR
  c : WORD;
BEGIN
  c := son^[R];

    {
     * start searching tree from the root to leaves.
     * choose node #(son[]) IF input bit = 0
     * ELSE choose #(son[]+1) (input bit = 1)
    }
  WHILE (c < T) DO BEGIN
    c := c + GetBit(GetBytes);
    c := son^[c];
  END;
  c := c - T;
  update(c);
  Decodechar := Int16(c);
END;

Function TLZH.DecodePosition(GetBytes:GetBytesProc) : WORD;
VAR
  i, j, c : WORD;
BEGIN
     { decode upper 6 bits from given table }
  i := GetByte(GetBytes);
  c := WORD(d_code[i] SHL 6);
  j := d_len[i];

      { input lower 6 bits directly }
  DEC(j,2);
  While j <> 0 DO BEGIN
    i := (i SHL 1) + GetBit(GetBytes);
    DEC(J);
  END;
  DecodePosition := c OR i AND $3f;
END;

{ Compression }

Procedure TLZH.InitLZH;
BEGIN
  getbuf := 0;
  getlen := 0;
  putlen := 0;
  putbuf := 0;
  textsize := 0;
  codesize := 0;
  printcount := 0;
  match_position := 0;
  match_length := 0;
  try
    New(lson);
    New(dad);
    New(rson);
    New(text_buf);
    New(freq);
    New(prnt);
    New(son);
  except
    Raise ElzhException.Create('LZH : Cannot get memory for dictionary tables');
  end;
END;

Procedure TLZH.EndLZH;

BEGIN
  try
    Dispose(son);
    Dispose(prnt);
    Dispose(freq);
    Dispose(text_buf);
    Dispose(rson);
    Dispose(dad);
    Dispose(lson);
  except
    Raise ElzhException.Create('LZH : Error freeing memory for dictionary tables');
  end;
END;

Procedure TLZH.LZHPack(VAR Bytes_Written:LongInt; GetBytes:GetBytesProc; PutBytes:PutBytesProc);
VAR
   ct : BYTE;
   i, len, r, s, last_match_length : Int16;
   Got : WORD;
BEGIN

   InitLZH;

   try
     textsize := 0;                 { rewind and rescan }
     StartHuff;
     InitTree;
     s := 0;
     r := N - F;
     FillChar(Text_buf^[0],r,' ');
     len := 0;
     Got := 1;
     While (len < F) AND (Got <> 0) DO BEGIN
       GetBytes(ct,1,Got);
       IF Got <> 0 THEN BEGIN
         text_buf^[r + len] := ct;
         INC(len);
       END;
     END;
     textsize := len;
     FOR i := 1 to F DO begin
       InsertNode(r - i);
     end;
     InsertNode(r);
     REPEAT
       IF (match_length > len) THEN begin
         match_length := len;
       end;
       IF (match_length <= THRESHOLD) THEN BEGIN
         match_length := 1;
               EncodeChar(text_buf^[r],PutBytes);
       END
       ELSE BEGIN
         EncodeChar(255 - THRESHOLD + match_length,PutBytes);
               EncodePosition(match_position,PutBytes);
       END;
       last_match_length := match_length;
       i := 0;
       Got := 1;
       While (i < last_match_length) AND (Got <> 0) DO BEGIN
         GetBytes(ct,1,Got);
         IF Got <> 0 THEN BEGIN
           DeleteNode(s);
           text_buf^[s] := ct;
           IF (s < PRED(F)) THEN begin
             text_buf^[s + N] := ct;
           end;
           s := SUCC(s) AND PRED(N);
           r := SUCC(r) AND PRED(N);
           InsertNode(r);
           inc(i);
         END;
       END; { endwhile }
       INC(textsize,i);
       While (i < last_match_length) DO BEGIN
         INC(i);
         DeleteNode(s);
         s := SUCC(s) AND PRED(N);
         r := SUCC(r) AND PRED(N);
         DEC(len);
         IF BOOLEAN(len) THEN InsertNode(r);
       END; { endwhile }
     UNTIL (len <= 0);  { end repeat }
     EncodeEnd(PutBytes);

   finally
     EndLZH;
   end;

   Bytes_Written := TextSize;

END;

Procedure TLZH.LZHUnpack(TextSize : Longint; GetBytes:GetBytesProc; PutBytes: PutBytesProc);
VAR
  c, i, j, k, r : Int16;
  c2            : Byte;
  count         : Longint;
  Put           : Word;

BEGIN

   InitLZH;

   try
     StartHuff;
     r := N - F;
     FillChar(text_buf^[0],r,' ');
     Count := 0;
     While count < textsize DO BEGIN
       c := DecodeChar(GetBytes);
       IF (c < 256) THEN BEGIN
         c2 := Lo(c);
               PutBytes(c2,1,Put);
               text_buf^[r] := c;
         INC(r);
               r := r AND PRED(N);
               INC(count);
       END
       ELSE BEGIN                {c >= 256 }
               i := (r - SUCC(DecodePosition(GetBytes))) AND PRED(N);
               j := c - 255 + THRESHOLD;
               FOR k := 0 TO PRED(j) DO BEGIN
                 c := text_buf^[(i + k) AND PRED(N)];
           c2 := Lo(c);
                 PutBytes(c2,1,Put);
                 text_buf^[r] := c;
           INC(r);
                 r := r AND PRED(N);
                 INC(count);
         END;  { for }
       END;  { if c < 256 }
     END; {endwhile count < textsize }

   finally
     ENDLZH;
   end;
end;

// Return as many bytes to the LZH compression buffer as requested.
procedure TLZH.GetBlockStream(var DTA; NBytes: Word; var Bytes_Got: Word);
begin
     //copy from stream into lzh compression buffer
     Bytes_Got := NBytes;
     if (StreamIn.Size - StreamIn.Position) < NBytes then
        Bytes_Got := StreamIn.Size - StreamIn.Position;
     StreamIn.ReadBuffer(DTA, Bytes_Got);
end;

procedure TLZH.PutBlockStream(var DTA; NBytes: Word; var Bytes_Got: Word);
begin
     //write from lzh decompression buffer to stream
     Bytes_Got := NBytes;
     StreamOut.WriteBuffer(DTA, Bytes_Got);
end;

END.
