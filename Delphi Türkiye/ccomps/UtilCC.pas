unit UtilCC;

interface

uses grids;

function PointerToLongint(P:Pointer):Longint;
function LongintToPointer(L:Longint):Pointer;

function DelSpace(S:String) : String;
function DelChar (S:String;C:Char): String;
function ReverseString(S:String) : String;
function UpperCaseTr(S:String) : String;
function LowerCaseTr(S:String) : String;
function FirstLettersUp(S:String) : String;
function Replicate(C:Char;S:Word) : String;
function Search(Strs:TDrawGrid;S:String):Integer;

implementation

uses SysUtils;

// SetLength procedure' ýný Assembler'dan caðýramadýðým için yazýldý;
procedure XSetLength(var S: String;N :Integer);
begin
  SetLength(S,N);
end;

function DelSpace(S:String) : String;assembler;
asm
     PUSH        EBX
     PUSH        ESI
     PUSH        EDI
     PUSH        ECX

     MOV         EDI,EDX
     MOV         ESI,EAX
     MOV         EAX,ESI
     CALL        StrLen

     MOV         EBX,EAX
     MOV         EAX,EDI
     MOV         EDX,EBX
     CALL        XSetLength

     MOV         ECX,0

     MOV         EDX,ESI
     MOV         ESI,[EDI]
     TEST        EBX,EBX
     JZ          @@1
@@10:
     MOV         AL,[EDX]
     CMP         AL,$20
     JNE         @@0
     JMP         @@20
@@0:
     MOV         [ESI],AL
     INC         ECX
     INC         ESI
@@20:
     INC         EDX
     DEC         EBX
     TEST        EBX,EBX
     JNZ         @@10
@@1:

     MOV         EAX,EDI
     MOV         EDX,ECX
     CALL        XSetLength


     POP         ECX
     POP         EDI
     POP         ESI
     POP         EBX
     RET
     MOV         EAX,EAX

end;

function DelChar (S:String;C:Char): String;assembler;
asm
//  EAX  ----> S
//  EDX  ----> C
//  ECX  ----> @result

     PUSH        EAX
     PUSH        EBX
     PUSH        ECX
     PUSH        EDX
     PUSH        ESI
     PUSH        EDI

     MOV         ESI,EAX   // ESI ' ya S
     MOV         EDI,ECX   // EDI ' ya Result
     MOV         EBX,EDX   // EBX ' e  C

     MOV         EAX,ESI  // Uzunluðu Bulusnacak String (S) EAX'e
     CALL        StrLen   // Uzunluðu Al

     MOV         EDX,EAX   // EDX' e String'in uzunluðu

// EDX' i Yeniden Hesapla (Silinecek Olanlarýn Sayýsýný Düþ)
     PUSH        EDX
     PUSH        ESI

     MOV         ECX,EDX
     MOV         AL,BL
@@0:
     CMP         [ESI],AL
     JNE         @@1
     DEC         EDX
@@1:
     DEC         ECX
     INC         ESI
     TEST        ECX,ECX
     JNZ         @@0

     POP         ESI

     PUSH        ESI
     PUSH        EDI
     PUSH        EBX

     MOV         EAX,EDI
     MOV         EDX,EDX
     CALL        XSetLength

     POP         EBX
     POP         EDI
     POP         ESI
     POP         EDX

     MOV         ECX,[EDI]
     MOV         EDI,ECX

@@001:
     CMP         [ESI],BL
     JNE         @@2
     JMP         @@3
@@2:
     MOV         AL,[ESI]
     MOV         [EDI],AL
     INC         EDI
@@3:
     INC         ESI
     DEC         EDX
     TEST        EDX,EDX
     JNZ         @@001

     POP         EDI
     POP         ESI
     POP         EDX
     POP         ECX
     POP         EBX
     POP         EAX

     RET
     MOV         EAX,EAX
end;

function ReverseString(S:String) : String;assembler;
asm
     PUSH        EBX
     PUSH        ESI
     PUSH        EDI
     PUSH        ECX

     MOV         EDI,EDX
     MOV         ESI,EAX
     MOV         EAX,ESI
     CALL        StrLen

     MOV         EBX,EAX
     MOV         EAX,EDI
     MOV         EDX,EBX
     CALL        XSetLength

     MOV         EDX,ESI
     MOV         ESI,[EDI]
     TEST        EBX,EBX
     JZ          @@1

     MOV         ECX,EBX
     DEC         ECX
     ADD         EDX,ECX
@@10:
     MOV         AL,[EDX]
     MOV         [ESI],AL
     INC         ESI
     DEC         EDX
     DEC         EBX
     TEST        EBX,EBX
     JNZ         @@10
@@1:

     POP         ECX
     POP         EDI
     POP         ESI
     POP         EBX
     RET
     MOV         EAX,EAX
end;

function UpperCaseTr(S:String) : String;
asm
     PUSH        EBX
     PUSH        ESI
     PUSH        EDI
     PUSH        ECX

     MOV         EDI,EDX
     MOV         ESI,EAX
     MOV         EAX,ESI
     CALL        StrLen

     MOV         EBX,EAX
     MOV         EAX,EDI
     MOV         EDX,EBX
     CALL        XSetLength

     MOV         EDX,ESI
     MOV         ESI,[EDI]
     TEST        EBX,EBX
     JZ          @@50
@@2:
     MOV         AL,[EDX]

     CMP         AL,'ð'
     JNE         @@3
     MOV         AL,'Ð'
     JMP         @@30
@@3:
     CMP         AL,'ü'
     JNE         @@4
     MOV         AL,'Ü'
     JMP         @@30
@@4:
     CMP         AL,'ý'
     JNE         @@5
     MOV         AL,'I'
     JMP         @@30
@@5:
     CMP         AL,'i'
     JNE         @@6
     MOV         AL,'Ý'
     JMP         @@30
@@6:
     CMP         AL,'þ'
     JNE         @@7
     MOV         AL,'Þ'
     JMP         @@30
@@7:
     CMP         AL,'ö'
     JNE         @@8
     MOV         AL,'Ö'
     JMP         @@30
@@8:
     CMP         AL,'ç'
     JNE         @@9
     MOV         AL,'Ç'
     JMP         @@30
@@9:
     CMP         AL,'a'
     JL          @@30
     CMP         AL,'z'
     JG          @@30
     SUB         AL,32
@@30:
     MOV         [ESI],AL
     INC         ESI
@@40:
     INC         EDX
     DEC         EBX
     TEST        EBX,EBX
     JNZ         @@2
@@50:

//     MOV         EAX,EDI
//     MOV         EDX,ECX
//     CALL        XSetLength


     POP         ECX
     POP         EDI
     POP         ESI
     POP         EBX
     RET
     MOV         EAX,EAX
end;

function LowerCaseTr(S:String) : String;
asm
     PUSH        EBX
     PUSH        ESI
     PUSH        EDI
     PUSH        ECX

     MOV         EDI,EDX
     MOV         ESI,EAX
     MOV         EAX,ESI
     CALL        StrLen

     MOV         EBX,EAX
     MOV         EAX,EDI
     MOV         EDX,EBX
     CALL        XSetLength

     MOV         EDX,ESI
     MOV         ESI,[EDI]
     TEST        EBX,EBX
     JZ          @@50
@@2:
     MOV         AL,[EDX]

     CMP         AL,'Ð'
     JNE         @@3
     MOV         AL,'ð'
     JMP         @@30
@@3:
     CMP         AL,'Ü'
     JNE         @@4
     MOV         AL,'ü'
     JMP         @@30
@@4:
     CMP         AL,'I'
     JNE         @@5
     MOV         AL,'ý'
     JMP         @@30
@@5:
     CMP         AL,'Ý'
     JNE         @@6
     MOV         AL,'i'
     JMP         @@30
@@6:
     CMP         AL,'Þ'
     JNE         @@7
     MOV         AL,'þ'
     JMP         @@30
@@7:
     CMP         AL,'Ö'
     JNE         @@8
     MOV         AL,'ö'
     JMP         @@30
@@8:
     CMP         AL,'Ç'
     JNE         @@9
     MOV         AL,'ç'
     JMP         @@30
@@9:
     CMP         AL,'A'
     JL          @@30
     CMP         AL,'Z'
     JG          @@30
     ADD         AL,32
@@30:
     MOV         [ESI],AL
     INC         ESI
@@40:
     INC         EDX
     DEC         EBX
     TEST        EBX,EBX
     JNZ         @@2
@@50:

//     MOV         EAX,EDI
//     MOV         EDX,ECX
//     CALL        XSetLength


     POP         ECX
     POP         EDI
     POP         ESI
     POP         EBX
     RET
     MOV         EAX,EAX
end;

function FirstLettersUp(S:String) : String;
asm
     PUSH        EBX
     PUSH        ESI
     PUSH        EDI
     PUSH        ECX

     MOV         EDI,EDX
     MOV         ESI,EAX
     MOV         EAX,ESI
     CALL        StrLen

     MOV         EBX,EAX
     MOV         EAX,EDI
     MOV         EDX,EBX
     CALL        XSetLength

     MOV         EDX,ESI
     MOV         ESI,[EDI]
     TEST        EBX,EBX
     JZ          @@50
     MOV         ECX,1
@@2:
     MOV         AL,[EDX]
     CMP         ECX,1
     JNE         @@30

     XOR         ECX,ECX

     CMP         AL,'ð'
     JNE         @@3
     MOV         AL,'Ð'
     JMP         @@30
@@3:
     CMP         AL,'ü'
     JNE         @@4
     MOV         AL,'Ü'
     JMP         @@30
@@4:
     CMP         AL,'ý'
     JNE         @@5
     MOV         AL,'I'
     JMP         @@30
@@5:
     CMP         AL,'i'
     JNE         @@6
     MOV         AL,'Ý'
     JMP         @@30
@@6:
     CMP         AL,'þ'
     JNE         @@7
     MOV         AL,'Þ'
     JMP         @@30
@@7:
     CMP         AL,'ö'
     JNE         @@8
     MOV         AL,'Ö'
     JMP         @@30
@@8:
     CMP         AL,'ç'
     JNE         @@9
     MOV         AL,'Ç'
     JMP         @@30
@@9:
     CMP         AL,'a'
     JL          @@30
     CMP         AL,'z'
     JG          @@30
     SUB         AL,32
@@30:
     CMP         AL,020h
     JNE         @@35
     MOV         ECX,1
@@35:
     MOV         [ESI],AL
     INC         ESI
@@40:
     INC         EDX
     DEC         EBX
     TEST        EBX,EBX
     JNZ         @@2
@@50:

//     MOV         EAX,EDI
//     MOV         EDX,ECX
//     CALL        XSetLength


     POP         ECX
     POP         EDI
     POP         ESI
     POP         EBX
     RET
     MOV         EAX,EAX
end;

function Replicate(C:Char;S:Word) : String;assembler;
asm
//  AL   ----> C
//  EDX  ----> S
//  ECX  ----> @result
     PUSH        EAX
     PUSH        EBX
     PUSH        ECX
     PUSH        EDX
     PUSH        ESI
     PUSH        EDI

     MOV         EBX,EDX  //  EBX 'e   S'yi ATA
     MOV         EDI,ECX  //  EDI ' ya @result'u ata
     XOR         ECX,ECX
     MOV         CL,AL    //  ECX' e   C ' yi ata

     PUSH        ECX
     PUSH        EDX

     MOV         EAX,EDI
     MOV         EDX,EBX
     CALL        XSetLength

     POP         EDX
     POP         ECX

     MOV         EAX,EDI
     MOV         EDI,[EAX]
@@2:
     MOV         AL,CL
     MOV         [EDI],AL
     INC         EDI
@@40:
     DEC         EBX
     TEST        EBX,EBX
     JNZ         @@2
@@50:

     POP         EDI
     POP         ESI
     POP         EDX
     POP         ECX
     POP         EBX
     POP         EAX
     RET
     MOV         EAX,EAX
end;

function LongintToPointer(L:Longint):Pointer;assembler;
asm
  MOV            EAX,L
end;

function PointerToLongint(P:Pointer):Longint;assembler;
asm
  MOV             EAX,P
end;

function Search(Strs:TDrawGrid;S:String):Integer;assembler;
asm
// EAX -->  @Strs
// EDX -->  @S
// EAX -->  @Result
  PUSH   ECX
  PUSH   EDX


//  CALL   Strs.GetText
  call   Strs.GetEditText  

  POP    EDX
  POP    ECX

  RET
  MOV         EAX,EAX
end;

end.
