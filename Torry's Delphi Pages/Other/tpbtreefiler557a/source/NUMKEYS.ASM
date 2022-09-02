;* ***** BEGIN LICENSE BLOCK *****
;* Version: MPL 1.1
;*
;* The contents of this file are subject to the Mozilla Public License Version
;* 1.1 (the "License"); you may not use this file except in compliance with
;* the License. You may obtain a copy of the License at
;* http://www.mozilla.org/MPL/
;*
;* Software distributed under the License is distributed on an "AS IS" basis,
;* WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
;* for the specific language governing rights and limitations under the
;* License.
;*
;* The Original Code is TurboPower B-Tree Filer
;*
;* The Initial Developer of the Original Code is
;* TurboPower Software
;*
;* Portions created by the Initial Developer are Copyright (C) 1996-2002
;* the Initial Developer. All Rights Reserved.
;*
;* Based in part on code written by Ralf Nagel
;*
;* Contributor(s):
;*
;* ***** END LICENSE BLOCK *****

;****************************************************** Macros and Equates

SetZero         MACRO Reg
                XOR     Reg,Reg         ;Reg = 0
                ENDM

ChkZero         MACRO Reg
                OR      Reg,Reg         ;Reg = 0?
                ENDM

BitsPerChar     EQU     DH
BPCdelta        EQU     DL
BitCount        EQU     BH
BitsLeft        EQU     BH
CharsLeft       EQU     BL

DATA    SEGMENT BYTE PUBLIC             ;!!.22
        EXTRN   ProcPtr : WORD          ;!!.22
DATA    ENDS                            ;!!.22

;****************************************************** Code

CODE    SEGMENT BYTE PUBLIC

        ASSUME  CS:CODE,DS:DATA

        PUBLIC  Pack4BitKey, Pack5BitKeyUC, Pack6BitKeyUC, Pack6BitKey
        PUBLIC  Unpack4BitKey, Unpack5BitKeyUC, Unpack6BitKeyUC, Unpack6BitKey
        PUBLIC  DescendingKey

;****************************************************** Pack4

;characters         (  )  *  +  ,  -  .  /  0  1  2   3  4   5   6   7   8   9
Pack4Table      DB  1, 2, 0, 3, 0, 4, 5, 0, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15

Pack4   PROC NEAR

        CMP     AL,'('                  ;less than '('?
        JB      P6zeroIt                ;map to 0
        CMP     AL,'9'                  ;greater than '9'?
        JA      P6zeroIt                ;map to 0
        PUSH    BX                      ;save BX
        MOV     BX,Offset Pack4Table    ;CS:BX => Pack4Table
        SUB     AL,'('
        XLAT    BYTE PTR CS:[0]         ;map character to 0-15
        POP     BX                      ;restore BX
        RET

Pack4   ENDP

;****************************************************** Pack5

Pack5   PROC NEAR

        CMP     AL,'z'                  ;greater than 'z'?
        JA      P6zeroIt                ;map to 0
        CMP     AL,'a'                  ;less than 'a'?
        JB      P5checkAtoZ             ;check for 'A'..'Z'
        SUB     AL,96                   ;in range 'a'-'z' -- map to 1-26
        RET

P5checkAtoZ:
        CMP     AL,'A'                  ;less than 'A'?
        JB      P6zeroIt                ;map to 0
        CMP     AL,'Z'                  ;greater than 'Z'?
        JA      P6zeroIt                ;map to 0
        SUB     AL,64                   ;in range 'A'-'Z' -- map to 1-26
        RET

Pack5   ENDP

;****************************************************** Pack6UC

Pack6UC PROC NEAR

        CMP     AL,'_'                  ;greater than '_'?
        JA      P6UCcheckaz             ;check for 'a'-'z'
        CMP     AL,'!'                  ;less than '!'?
        JB      P6zeroIt                ;map to 0
        SUB     AL,32                   ;in range -- map to 1-63
        RET

P6UCcheckaz:
        CMP     AL,'z'                  ;greater than 'z'?
        JA      P6zeroIt                ;map to 0
        CMP     AL,'a'                  ;less than 'a'?
        JB      P6zeroIt                ;map to 0
        SUB     AL,64                   ;in range 'a'-'z' -- map to 33-58
        RET

Pack6UC  ENDP

;****************************************************** Pack6

Pack6   PROC NEAR

        CMP     AL,'z'                  ;greater than 'z'?
        JA      P6zeroIt                ;map to 0
        CMP     AL,'a'                  ;less than 'a'?
        JB      P6checkAtoZ             ;check for 'A'-'Z'
        SUB     AL,60                   ;in range 'a'-'z' -- map to 37-62
        RET

P6checkAtoZ:
        CMP     AL,'A'                  ;less than 'A'?
        JB      P6check0to9             ;check for '0'-'9'
        CMP     AL,'Z'                  ;greater than 'Z'?
        JA      P6zeroIt                ;map to 0
        SUB     AL,54                   ;in range 'A'-'Z' -- map to 11-36
        RET

P6check0to9:
        CMP     AL,'0'                  ;less than '0'?
        JB      P6zeroIt                ;map to 0
        CMP     AL,'9'                  ;greater than '9'?
        JA      P6zeroIt                ;map to 0
        SUB     AL,47                   ;in range '0'-'9' -- map to 1-10
        RET

P6zeroIt:
        SetZero AL
        RET

Pack6   ENDP

;****************************************************** Pack4BitKey

;function Pack4BitKey(Src : string; Len : Byte) : string;
;Pack the Source string into sequences of 4 bits (max length = Len)

Pack4BitKey:

        MOV     BitsPerChar,4           ;number of bits per character = 4
        MOV     BPCdelta,4              ;BPCdelta = 8-4
        MOV     ProcPtr,Offset Pack4    ;Load procedure offset
        JMP     SHORT PackKey

;****************************************************** Pack5BitKeyUC

;function Pack5BitKeyUC(Src : string; Len : Byte) : string;
;Pack the Source string into sequences of 5 bits (max length = Len)

Pack5BitKeyUC:

        MOV     BitsPerChar,5           ;number of bits per character = 5
        MOV     BPCdelta,3              ;BPCdelta = 8-5
        MOV     ProcPtr,Offset Pack5    ;Load procedure offset
        JMP     SHORT PackKey

;****************************************************** Pack6BitKeyUC

;function Pack6BitKey(Src : string; Len : Byte) : string;
;Pack the Source string into sequences of 6 bits (max length = Len)

Pack6BitKeyUC:

        MOV     BitsPerChar,6           ;number of bits per character = 6
        MOV     BPCdelta,2              ;BPCdelta = 8-6
        MOV     ProcPtr,Offset Pack6UC  ;Load procedure offset
        JMP     SHORT PackKey

;****************************************************** Pack6BitKey

;function Pack6BitKey(Src : string; Len : Byte) : string;
;Pack the Source string into sequences of 6 bits (max length = Len)

Pack6BitKey:

        MOV     BitsPerChar,6           ;number of bits per character = 6
        MOV     BPCdelta,2              ;BPCdelta = 8-6
        MOV     ProcPtr,Offset Pack6    ;Load procedure offset

        ;falls through into PackKey

;****************************************************** PackKey

;Primitive routine to make a key

PLen    EQU     BYTE  PTR [BP+6]        ;!!.22
PSrc    EQU     DWORD PTR [BP+8]        ;!!.22
PDest   EQU     DWORD PTR [BP+12]       ;!!.22
LProcP  EQU     WORD  PTR [BP-2]        ;!!.22
L2      EQU     WORD  PTR [BP-4]        ;!!.22

PackKey PROC FAR
        PUSH    BP                      ;!!.22
        MOV     BP,SP                   ;Set up stack frame !!.22
        SUB     SP,4                    ;!!.22
        MOV     AX,ProcPtr              ;!!.22
        MOV     LProcP,AX               ;!!.22
        PUSH    DS                      ;Save DS
        CLD                             ;Go forward
        LDS     SI,PSrc                 ;DS:SI -> Src[0]
        LES     DI,PDest                ;ES:DI -> Result[0]
        SetZero AH                      ;AX = Len
        MOV     AL,PLen
        STOSB                           ;Set the result's length byte
        MOV     L2,DI                   ;L2 = Ofs(Result[Len+1]) !!.22
        ADD     L2,AX                   ;!!.22
        SetZero AH                      ;AH = Next byte to plug in
        LODSB                           ;AL = Length(Src)
        MOV     CharsLeft,AL            ;BL = # of chars left in Src
        SetZero BitCount                ;BH = # of bits in AH

PMain:
        CMP     DI,L2                   ;string full?  !!.22
        JAE     PExit                   ;if so, we're done
        ChkZero CharsLeft               ;any characters left in Src?
        JZ      PFinish
        LODSB                           ;load next character
        DEC     CharsLeft               ;decrement loop counter
        CALL    LProcP                  ;pack character in AL !!.22

        MOV     CL,BPCdelta             ;shift bottom of AL into top
        SHL     AL,CL
        SetZero CH                      ;set loop count
        MOV     CL,BitsPerChar
PLoop:
        CMP     BitCount,8              ;do we have 8 bits yet?
        JB      PNext
        MOV     ES:[DI],AH              ;store character in AH at ES:DI
        INC     DI                      ;advance string index
        SetZero BitCount                ;Reset BH to 0
PNext:
        ROL     AL,1                    ;rotate high bit of AL into CF
        RCL     AH,1                    ;rotate it from CF into low bit of AH
        INC     BitCount                ;increment counter
        LOOP    PLoop                   ;next bit

        JMP     SHORT PMain             ;do it again

PFinish:
        ChkZero BitCount                ;anything left in AH?
        JZ      PZeroPad                ;if not, pad with 0's
        MOV     CL,8                    ;move the bits in AH up into place
        SUB     CL,BitCount
        SHL     AH,CL
        MOV     ES:[DI],AH              ;store character in AH at ES:DI
        INC     DI                      ;advance string index

PZeroPad:                               ;pad to end of string with zeros !!.22
        MOV     CX,L2                   ;CX = # of zeros to write
        SUB     CX,DI
        SetZero AL                      ;AL = 0
        REP     STOSB                   ;fill with zeros if CX > 0

PExit:
        POP     DS
        MOV     SP,BP                   ;!!.22
        POP     BP                      ;!!.22
        RET     6                       ;remove parameters and return

PackKey ENDP

;****************************************************** Unpack4

;                0   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15
Unpack4Table DB ' ','(',')','+','-','.','0','1','2','3','4','5','6','7','8','9'

Unpack4 PROC NEAR

        PUSH    BX                      ;save BX
        XCHG    AH,AL                   ;switch AH and AL
        MOV     BX,Offset Unpack4Table  ;CS:BX => Unpack4Table
        XLAT    BYTE PTR CS:[0]         ;map character to 0-15
        XCHG    AH,AL                   ;switch AH and AL
        POP     BX                      ;restore BX
        RET

Unpack4 ENDP

;****************************************************** Unpack5

Unpack5 PROC NEAR

        ChkZero AH                      ;AH = 0?
        JZ      U6space                 ;if so, return a space
        ADD     AH,64                   ;map to 'A'..'Z'
        RET

Unpack5 ENDP

;****************************************************** Unpack6UC

Unpack6UC        PROC NEAR

        ChkZero AH,AH                   ;AH = 0?
        JZ      U6space                 ;if so, return a space
        ADD     AH,32                   ;else, map to '!'-'_'
        RET

Unpack6UC        ENDP

;****************************************************** Unpack6

Unpack6 PROC NEAR

        ChkZero AH                      ;AH = 0?
        JZ      U6space                 ;if so, return a space
        CMP     AH,37                   ;less than 37?
        JB      U6checkAtoZ             ;check for 'A'-'Z'
        ADD     AH,60                   ;in range 37-62 -- map to 'a'-'z'
        RET

U6checkAtoZ:
        CMP     AH,11                   ;less than 11?
        JB      U60to9
        ADD     AH,54                   ;in range 37-62 -- map to 'a'-'z'
        RET

U60to9:
        ADD     AH,47                   ;in range 1-10 -- map to '0'-'9'
        RET

U6space:
        MOV     AH,' '
        RET

Unpack6 ENDP

;****************************************************** Unpack4BitKey

;function Unpack4BitKey(Src : string) : string;
;Unpack a key created by Pack4BitKey

Unpack4BitKey:

        MOV     BitsPerChar,4           ;number of bits per character = 4
        MOV     ProcPtr,Offset Unpack4  ;Load procedure offset
        JMP     SHORT UnpackKey

;****************************************************** Unpack5BitKeyUC

;function Unpack5BitKeyUC(Src : string) : string;
;Unpack a key created by Pack5BitKeyUC

Unpack5BitKeyUC:

        MOV     BitsPerChar,5           ;number of bits per character = 5
        MOV     ProcPtr,Offset Unpack5  ;Load procedure offset
        JMP     SHORT UnpackKey

;****************************************************** Unpack6BitKeyUC

;function Unpack6BitKeyUC(Src : string) : string;
;Unpack a key created by Pack6BitKeyUC

Unpack6BitKeyUC:

        MOV     BitsPerChar,6           ;number of bits per character = 6
        MOV     ProcPtr,Offset Unpack6UC ;Load procedure offset
        JMP     SHORT UnpackKey

;****************************************************** Unpack6BitKey

;function Unpack6BitKey(Src : string) : string;
;Unpack a key created by Pack6BitKey

Unpack6BitKey:

        MOV     BitsPerChar,6           ;number of bits per character = 6
        MOV     ProcPtr,Offset Unpack6  ;Load procedure offset

        ;falls through into UnpackKey

;****************************************************** UnpackKey

;Primitive routine to unpack a packed key

USrc    EQU     DWORD PTR [BP+6]        ;!!.22
UDest   EQU     DWORD PTR [BP+10]       ;!!.22
LProcP  EQU     WORD  PTR [BP-2]        ;!!.22

UnpackKey       PROC FAR
        PUSH    BP                      ;!!.22
        MOV     BP,SP                   ;Set up stack frame !!.22
        SUB     SP,2                    ;!!.22
        MOV     AX,ProcPtr              ;!!.22
        MOV     LProcP,AX               ;!!.22
        PUSH    DS                      ;Save DS
        CLD                             ;Go forward
        LDS     SI,USrc                 ;DS:SI -> Src[0]
        LES     DI,UDest                ;ES:DI -> Result[0]
        LODSB                           ;AL = Length(Src)
        MOV     CharsLeft,AL            ;BL = # of chars left in Src
        SetZero BitsLeft                ;BH = # of bits in AL
        PUSH    DI                      ;save offset of Result[0]

UMain:
        ChkZero CharsLeft               ;any characters left in Src?
        JZ      UFinish

        ;rotate next packed char into AH

        SetZero CH                      ;set loop count
        MOV     CL,BitsPerChar
        SetZero AH                      ;AH is empty
ULoop:
        ChkZero BitsLeft                ;any bits left in AL?
        JNZ     UNext
        LODSB                           ;reload AL
        DEC     CharsLeft               ;decrement counter
        MOV     BitsLeft,8              ;Reset BH to 8
UNext:
        ROL     AL,1                    ;rotate high bit of AL into CF
        RCL     AH,1                    ;rotate it from CF into low bit of AH
        DEC     BitsLeft                ;decrement bit counter
        LOOP    ULoop                   ;next bit

        CALL    LProcP                  ;unpack char in AH  !!.22
        INC     DI                      ;advance string index
        MOV     ES:[DI],AH              ;store the unpacked character
        JMP     SHORT UMain             ;do it again

UFinish:
        CMP     BitsLeft,BitsPerChar    ;full character left in AL?
        JB      UExit                   ;if not, we're done
        MOV     CL,8                    ;move the bits in AL down into place
        SUB     CL,BitsPerChar                                  ;!!.55
        SHR     AL,CL
        MOV     AH,AL                   ;move it into AH
        CALL    LProcP                  ;unpack char in AH !!.22
        INC     DI                      ;advance string index
        MOV     ES:[DI],AH              ;store character in AH at ES:DI

UExit:
        MOV     AX,DI                   ;AX = DI
        POP     DI                      ;Restore pointer to length byte
        SUB     AX,DI                   ;Get length of our string
        STOSB                           ;Set length byte
        POP     DS                      ;restore DS
        MOV     SP,BP                   ;!!.22
        POP     BP                      ;!!.22
        RET     4                       ;remove parameter and return

UnpackKey       ENDP

;****************************************************** MakeDescendingKey

;function DescendingKey(S : string; MaxLen : Byte) : string;
;Invert values in S to allow descending sorts, pad to MaxLen with #$FF

dkLen   EQU     BYTE  PTR SS:[BX+4]
dkSrc   EQU     DWORD PTR SS:[BX+6]
dkDest  EQU     DWORD PTR SS:[BX+10]

DescendingKey   PROC FAR

        MOV     BX,SP                   ;Set up stack frame
        PUSH    DS                      ;Save DS
        CLD                             ;go forward
        XOR     DH,DH                   ;DX = MaxLen
        MOV     DL,dkLen
        LDS     SI,dkSrc                ;DS:SI => S
        LES     DI,dkDest               ;ES:DI => result
        XOR     AH,AH                   ;AX = CX = length(S)
        LODSB
        MOV     CX,AX
        MOV     AL,DL                   ;set length byte of result
        STOSB
        SUB     DX,CX                   ;calculate amount of padding
        JCXZ    dkPad                   ;pad to maximum width if string empty
dkAgain:
        LODSB                           ;get next byte
        NOT     AL                      ;invert
        STOSB                           ;store it
        LOOP    dkAgain
dkPad:
        CMP     DX,0                    ;DX > 0?    !!.41
        JLE     dkDone                  ;           !!.41
        MOV     AL,0FFh                 ;pad with $FF
        MOV     CX,DX                   ;loop count into CX
        REP     STOSB                   ;pad the string
dkDone:
        POP     DS                      ;restore DS
        RET     6                       ;remove parameters and return

DescendingKey   ENDP

CODE    ENDS

        END
