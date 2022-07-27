{: Contains the Low Level and System Routines used by ESBPCS for CLX.

 This is designed to work in Borland Delphi 6 CLX and above, Borland
 C++ Builder 6 CLX and above, and Borland Kylix 2 and above.
 Most if not all features will work in Kylix 1 but it is not currently supported.<p>

 Supplies low level & System routines such as 16-bit & 32-bit BitLists
 and Block & Data Manipulations.<p>

 Copyright © 1999-2002 ESB Consultancy.<p>

 v2.3 - 14 September 2002
}
unit QESBPCSSystem;

{$I esbpcs.inc}

interface

uses
     {$IFDEF LINUX}
     Types,
     {$ENDIF}
     {$IFDEF MSWINDOWS}
     Windows,
     {$ENDIF}
     QESBPCSGlobals;

{--- Some Low level Operations ---}

{: Swaps the data for the specified number of bytes between the
 two structures.  Uses fast dword moves.	BASM.
 @param Obj1 First Object.
 @param Obj2 Second Object.
 @param Size Number of bytes to swap starting from the beginning of
  each object.
 @cat MemOps
}
procedure ESBExchange (var Obj1, Obj2; const Size: LongWord);

{: Returns True if two structures have the same bytes for the
 first Size Words.  Uses fast dword compares.	BASM.
 @param Obj1 First Object.
 @param Obj2 Second Object.
 @param Size Number of bytes to compare starting from the beginning of
  each object.
 @cat MemOps
}
function ESBSame (const Obj1, Obj2; const Size: LongWord): Boolean;

{: Fills given structure with specified number of 0 values,
 effectively clearing it.	Uses fast dword moves.
 @param Dest Object to clear.
 @param Size Number of bytes to clear starting from the beginning of
  the object.
 @cat MemOps
}
procedure ESBClear (var Dest; const Size: LongWord);

{--- Swapping ---}

{: Swap 2 Values using a Temp Value. This Pascal code is faster than our
 previous BASM code WHEN Delphi 4 (and above) has Optimization Turned On.
 @param X First Value to Process.
 @param Y Second Value to Process.
 @cat ExtraStrings
 @cat IntMath
 @cat FloatMath
 @cat MemOps
 @cat ComplexMath
 @cat FracMath
 @cat FinMath
 @cat DTMath
 @cat ImpMath
}
procedure SwapXY (var X, Y: Boolean); overload;
procedure SwapXY (var X, Y: Char); overload;
procedure SwapXY (var X, Y: Byte); overload;
procedure SwapXY (var X, Y: ShortInt); overload;
procedure SwapXY (var X, Y: Word); overload;
procedure SwapXY (var X, Y: SmallInt); overload;
procedure SwapXY (var X, Y: LongWord); overload;
procedure SwapXY (var X, Y: LongInt); overload;
procedure SwapXY (var X, Y: Int64); overload;
procedure SwapXY (var X, Y: Extended); overload;
procedure SwapXY (var X, Y: Double); overload;
procedure SwapXY (var X, Y: Single); overload;
procedure SwapXY (var X, Y: TESBCurrency); overload;
procedure SwapXY (var X, Y: TESBLongCurrency); overload;
procedure SwapXY (var X, Y: Currency); overload;
procedure SwapXY (var X, Y: TESBComplex); overload;
procedure SwapXY (var X, Y: TESBFraction); overload;
procedure SwapXY (var X, Y: TESBMixedFraction); overload;
procedure SwapXY (var X, Y: TDateTime); overload;
procedure SwapXY (var X, Y: TPoint); overload;
procedure SwapXY (var X, Y: TRect); overload;
procedure SwapXY (var X, Y: TESBImperial); overload;

{--- IF Function ---}

{: Performs an if that can be used in Expressions. If Condition is True
 then TrueResult is returned Else FalseResult is returned.
 @param Condition Controls which result is returned.
 @param TrueResult Returned if Condition is True.
 @param FalseResult Returned if Condition is True.
 @cat BooleanConv
 @cat ExtraStrings
 @cat IntMath
 @cat FloatMath
 @cat ComplexMath
 @cat DTMath
 @cat FinMath
 @cat FracMath
 @cat ImpMath
}
function iff (const Condition: Boolean;
     const TrueResult, FalseResult: string): string; overload;
function iff (const Condition: Boolean;
     const TrueResult, FalseResult: Integer): Integer; overload;
function iff (const Condition: Boolean;
     const TrueResult, FalseResult: Int64): Int64; overload;
function iff (const Condition: Boolean;
     const TrueResult, FalseResult: Extended): Extended; overload;
function iff (const Condition: Boolean;
     const TrueResult, FalseResult: Boolean): Boolean; overload;
function iff (const Condition: Boolean;
     const TrueResult, FalseResult: Char): Char; overload;
function iff (const Condition: Boolean;
     const TrueResult, FalseResult: TObject): TObject; overload;
function iff (const Condition: Boolean;
     const TrueResult, FalseResult: TPoint): TPoint; overload;
function iff (const Condition: Boolean;
     const TrueResult, FalseResult: TRect): TRect; overload;
function iff (const Condition: Boolean;
     const TrueResult, FalseResult: TESBComplex): TESBComplex; overload;
function iff (const Condition: Boolean;
     const TrueResult, FalseResult: TDateTime): TDateTime; overload;
function iff (const Condition: Boolean;
     const TrueResult, FalseResult: TESBCurrency): TESBCurrency; overload;
function iff (const Condition: Boolean;
     const TrueResult, FalseResult: TESBLongCurrency): TESBLongCurrency; overload;
function iff (const Condition: Boolean;
     const TrueResult, FalseResult: Currency): Currency; overload;
function iff (const Condition: Boolean;
     const TrueResult, FalseResult: TESBFraction): TESBFraction; overload;
function iff (const Condition: Boolean;
     const TrueResult, FalseResult: TESBMixedFraction): TESBMixedFraction; overload;
function iff (const Condition: Boolean;
     const TrueResult, FalseResult: TESBImperial): TESBImperial; overload;

{--- Bit Manipulation ---}

{: Sets all Bits in a BitList to 0.
 @param BitList BitList to process.
 @cat MemOps
}
procedure ClearAllBits (var BitList: TESBBitList); overload;
procedure ClearAllBits (var BitList: TESBLongBitList); overload;

{: Sets all Bits in a BitList to 1.
 @param BitList BitList to process.
 @cat MemOps
}
procedure SetAllBits (var BitList: TESBBitList); overload;
procedure SetAllBits (var BitList: TESBLongBitList); overload;

{: Flips all Bits in a BitList, i.e 1 becomes 0 and 0 becomes 1.
 @param BitList BitList to process.
 @cat MemOps
}
procedure FlipAllBits (var BitList: TESBBitList); overload;
procedure FlipAllBits (var BitList: TESBLongBitList); overload;

{: Sets specified Bit of a BitList to 0.
 @param BitList BitList to process.
 @param I Bit to clear, starts at 0.
 @cat MemOps
}
procedure ClearBit (var BitList: TESBBitList; const I: Byte); overload;
procedure ClearBit (var BitList: TESBLongBitList; const I: Byte); overload;

{: Sets specified Bit of a BitList to 1.
 @param BitList BitList to process.
 @param I Bit to clear, starts at 0.
 @cat MemOps
}
procedure SetBit (var BitList: TESBBitList; const I: Byte); overload;
procedure SetBit (var BitList: TESBLongBitList; const I: Byte); overload;

{: Flips specified Bit of a BitList, ie 0 becomes 1 and 1 becomes 0.
 @param BitList BitList to process.
 @param I Bit to clear, starts at 0.
 @cat MemOps
}
procedure FlipBit (var BitList: TESBBitList; const I: Byte); overload;
procedure FlipBit (var BitList: TESBLongBitList; const I: Byte); overload;

{: Returns True if Specified Bit of BitList is 1. Can pass both types of
 BitList to this.
 @param BitList BitList to process.
 @param I Bit to clear, starts at 0.
 @cat MemOps
}
function BitIsSet (const BitList: TESBLongBitList; const I: Byte): Boolean;

{: Reverses the Bit List, ie Bit 15 swap Bit 0, Bit 14 swap Bit1, etc.
 @param BitList BitList to process.
 @cat MemOps
}
procedure ReverseBits (var BitList: TESBBitList); overload;
procedure ReverseBits (var BitList: TESBLongBitList); overload;

{: Converts a Bit list to a string of '1' and '0'.
 @param BitList BitList to process.
 @cat MemOps
}
function Bits2Str (const BitList: TESBBitList): String16;
{: Converts a Long Bit list to a string of '1' and '0'.
 @param BitList BitList to process.
 @cat MemOps
}
function Bits2LStr (const BitList: TESBLongBitList): String32;

{:Converts a string of '1' and '0' into a BitList.
 @param S String to process.
 @cat MemOps
}
function Str2Bits (var S: String16): TESBBitList; overload;
function Str2Bits (var S: String32): TESBLongBitList; overload;

{: Returns a number from 0 -> 32 indicating the number of Bits Set.
 This is also known as the Hamming Weight. Can pass both types of
 BitList to this.
 @param BitList BitList to process.
 @cat MemOps
}
function BitsSet (const BitList: TESBLongBitList): Byte; overload;

{--- Int64 manipulation ---}

{: Split a Int64 into High DWord and Low DWord.
 @param L Int64 to process.
 @param HiDWord Returns High DWord (4 bytes).
 @param LoDWord Returns Low DWord (4 bytes).
 @cat MemOps
 @cat IntMath
}
procedure SplitInt64 (const L: Int64; var HiDWord, LoDWord: LongWord);

{: Combine High and Low DWord into Int64.
 @param HiDWord High DWord (4 bytes).
 @param LoDWord Low DWord (4 bytes).
 @cat MemOps
 @cat IntMath
}
function MakeInt64 (const HiDWord, LoDWord: LongWord): Int64;

{$IFDEF MSWINDOWS}
{: Returns the Color Depth that Windows is currently in. }
function GetColorDepth: Integer;
{$ENDIF}

implementation

uses
     SysUtils;

{--- Some Low level Operations ---}

procedure ESBExchange (var Obj1, Obj2; const Size: LongWord); register;
asm
	push 	edi			// Preserve EDI
	push 	ebx			// Preserve EBX
	push 	ecx            // Preserve ECX (which currently has Size)
	shr  	ecx, 2         // Integer divide by 4 to see if less than 4 Byte
						// ECX now contains the number of DWords to Swap
	jz   	@@LessThanFour	// If so then jump

@@AgainDWords:
						// Swap DWords
	mov  	ebx, [eax]	// Copy a DWord from Obj1 into EBX
	mov  	edi, [edx]	// Copy a DWord from Obj2 into EDI
	mov  	[edx], ebx	// Copy a Dword from EBX into Obj2
	mov  	[eax], edi	// Copy a Dword from EDI into Obj1

	add  	eax, 4		// Move to the next DWord in Obj1
	add  	edx, 4		// Move to the next DWord in Obj2
	dec  	ecx			// Decrement count
	jnz  	@@AgainDWords	// Repeat until ECX is Zero

@@LessThanFour:
	pop  	ecx  		// Restore original ECX (i.e Size)
	and  	ecx, $3		// Get just the remainder after division by 4
	jz   	@@Done		// If Zero nothing to do
	mov  	bl, [eax]      // Otherwose swap a byte
	mov  	bh, [edx]
	mov  	[edx], bl
	mov  	[eax], bh
	inc  	eax      		// And move one byte
	inc  	edx
	dec  	ecx
	jz   	@@Done

	mov  	bl, [eax]		// Repeat for remainding bytes...
	mov  	bh, [edx]
	mov  	[edx], bl
	mov  	[eax], bh
	inc  	eax
	inc  	edx
	dec  	ecx
	jz   	@@Done

	mov  	bl, [eax]
	mov  	bh, [edx]
	mov  	[edx], bl
	mov  	[eax], bh

@@Done:
	pop  	ebx            // Restore EBX
	pop  	edi			// Restore EDI
end;

function ESBSame (const Obj1, Obj2; const Size: LongWord): Boolean;
asm
	push 	esi			// Preserve ESI
	push 	edi			// Preserve EDI

	mov		esi, eax		// Move DWord from Obj1 into ESI
	mov		edi, edx		// Move DWord from Obj2 into EDI

	mov		eax, ecx		// Store Initial Size in EAX

	sar		ecx, 2		// Count DIV 4 gives numer of Dwords
	js   	@@False
	repe 	cmpsd		// compare as dwords
	jnz  	@@False 		// if Not Zero, then False

	mov 		ecx, eax		// Compare mod 3 bytes
	and  	ecx, 03
	repe 	cmpsb		// compare as bytes
	jnz  	@@False		// if Not Zero, then False

@@True:
	mov  	al, True		// else True
	jmp  	@@Done

@@False:
	mov  	al, False

@@Done:
	pop		edi			// Restore EDI
	pop		esi			// Restore ESI
end;

procedure ESBClear (var Dest; const Size: LongWord);
begin
     FillChar (Dest, Size, 0);
end;

procedure SwapXY (var X, Y: Char);
var
     Temp: Char;
begin
     Temp := X;
     X := Y;
     Y := Temp;
end;

procedure SwapXY (var X, Y: Boolean);
var
     Temp: Boolean;
begin
     Temp := X;
     X := Y;
     Y := Temp;
end;

procedure SwapXY (var X, Y: Byte);
var
     Temp: Byte;
begin
     Temp := X;
     X := Y;
     Y := Temp;
end;

procedure SwapXY (var X, Y: ShortInt);
var
     Temp: ShortInt;
begin
     Temp := X;
     X := Y;
     Y := Temp;
end;

procedure SwapXY (var X, Y: Word);
var
     Temp: Word;
begin
     Temp := X;
     X := Y;
     Y := Temp;
end;

procedure SwapXY (var X, Y: SmallInt);
var
     Temp: SmallInt;
begin
     Temp := X;
     X := Y;
     Y := Temp;
end;

procedure SwapXY (var X, Y: LongInt);
var
     Temp: LongInt;
begin
     Temp := X;
     X := Y;
     Y := Temp;
end;

procedure SwapXY (var X, Y: LongWord);
var
     Temp: LongWord;
begin
     Temp := X;
     X := Y;
     Y := Temp;
end;

procedure SwapXY (var X, Y: TESBCurrency);
var
     Temp: TESBCurrency;
begin
     Temp := X;
     X := Y;
     Y := Temp;
end;

procedure SwapXY (var X, Y: Int64);
var
     Temp: Int64;
begin
     Temp := X;
     X := Y;
     Y := Temp;
end;

procedure SwapXY (var X, Y: Extended);
var
     Temp: Extended;
begin
     Temp := X;
     X := Y;
     Y := Temp;
end;

procedure SwapXY (var X, Y: Double);
var
     Temp: Double;
begin
     Temp := X;
     X := Y;
     Y := Temp;
end;

procedure SwapXY (var X, Y: Single);
var
     Temp: Single;
begin
     Temp := X;
     X := Y;
     Y := Temp;
end;

procedure SwapXY (var X, Y: TESBLongCurrency);
var
     Temp: TESBLongCurrency;
begin
     Temp := X;
     X := Y;
     Y := Temp;
end;

procedure SwapXY (var X, Y: Currency);
var
     Temp: Currency;
begin
     Temp := X;
     X := Y;
     Y := Temp;
end;

procedure SwapXY (var X, Y: TESBComplex);
var
     Temp: TESBComplex;
begin
     Temp := X;
     X := Y;
     Y := Temp;
end;

procedure SwapXY (var X, Y: TESBFraction);
var
     Temp: TESBFraction;
begin
     Temp := X;
     X := Y;
     Y := Temp;
end;

procedure SwapXY (var X, Y: TESBMixedFraction);
var
     Temp: TESBMixedFraction;
begin
     Temp := X;
     X := Y;
     Y := Temp;
end;

procedure SwapXY (var X, Y: TDateTime);
var
     Temp: TDateTime;
begin
     Temp := X;
     X := Y;
     Y := Temp;
end;

procedure SwapXY (var X, Y: TPoint);
var
     Temp: TPoint;
begin
     Temp := X;
     X := Y;
     Y := Temp;
end;

procedure SwapXY (var X, Y: TRect);
var
     Temp: TRect;
begin
     Temp := X;
     X := Y;
     Y := Temp;
end;

procedure SwapXY (var X, Y: TESBImperial);
var
     Temp: TESBImperial;
begin
     Temp := X;
     X := Y;
     Y := Temp;
end;

procedure SplitInt64 (const L: Int64; var HiDWord, LoDWord: LongWord);
begin
     with Int64Rec (L) do
     begin
          HiDWord := Hi;
          LoDWord := Lo;
     end;
end;

function MakeInt64 (const HiDWord, LoDWord: LongWord): Int64;
asm
	xchg eax, edx  //Swap eax and edx
end;

{--- Bit Manipulation ---}

procedure ClearAllBits (var BitList: TESBBitList);
begin
     BitList := $0000
end;

procedure SetAllBits (var BitList: TESBBitList);
begin
     BitList := $FFFF
end;

procedure FlipAllBits (var BitList: TESBBitList);
asm
	not [eax].TESBBitList
end;

procedure ClearBit (var BitList: TESBBitList; const I: Byte);
asm
	and edx, $0f    //Should be faster than "movzx edx, dl"
  	btr [eax], edx  //Clear the bit #I
end;

procedure SetBit (var BitList: TESBBitList; const I: Byte);
asm
	and edx, $0f
  	bts [eax], edx  //Set to 1 the bit #I
end;

procedure FlipBit (var BitList: TESBBitList; const I: Byte);
asm
	and edx, $0f
  	btc [eax], edx  //Complement the bit #I
end;

function Bits2Str (const BitList: TESBBitList): String16;
var
     I: Integer;
begin
     SetLength (Result, 16);
     for I := 0 to 15 do
          if BitIsSet (BitList, I) then
               Result [I + 1] := '1'
          else
               Result [I + 1] := '0';
end;

procedure ReverseBits (var BitList: TESBBitList);
asm
	push	esi		// Preserve ESI
	mov 	dx, [eax] // DX <- BitList
	mov 	esi, eax	// Move addr BitList into ESI
	mov	cx, 16	// 16 iterations needed for a word
@@Loop:
	shr	dx, 1	// move least significant bit into CF
	adc	ax, ax	// ax := 2 * ax + carry bit
	dec	cx
	jnz 	@@Loop
	mov	[esi], ax	// AX -> BitList
	pop	esi		// Restore ESI
end;

function Str2Bits (var S: String16): TESBBitList;
asm
	push		esi			// Preserve ESI
	push 	ebx			// Preserve EBX

	mov		esi, eax		// Move Pointer to S ESI

	lodsb				// Read Length Byte into AL
	sub		ah, ah		// Clear AH
	mov		cx, ax		// CX <- AX, CX now has the length
	sub		bx, bx		// Clear BX for BitList construction
	mov		dl, '0'		// For comparisons

@@Loop:
	lodsb				// Load Next Character in AL
	shl		bx, 1		// BX <- BX * 2
	cmp		al, dl		// Is AL = '0'
	je		@@NoOne		// If Yes then it is Not a One and Jump
	add		bx, 1		// Otherwise add 1
@@NoOne:
	Loop		@@Loop		// Loop till string all processed

	mov		ax, bx		// AX <- BX for function result

	pop		ebx			// Restore EBX
	pop		esi			// Restore ESI
end;

{--- Long Bit Manipulation ---}

procedure ClearAllBits (var BitList: TESBLongBitList);
begin
     BitList := $00000000
end;

procedure SetAllBits (var BitList: TESBLongBitList);
begin
     BitList := $FFFFFFFF
end;

procedure FlipAllBits (var BitList: TESBLongBitList);
asm
	not [eax].TESBLongBitList
end;

procedure ClearBit (var BitList: TESBLongBitList; const I: Byte);
asm
	and edx, $1f    //Should be faster than "movzx edx, dl"
  	btr [eax], edx  //Clear the bit #I
end;

procedure SetBit (var BitList: TESBLongBitList; const I: Byte);
asm
	and edx, $1f
  	bts [eax], edx  //Set to 1 the bit #I
end;

procedure FlipBit (var BitList: TESBLongBitList; const I: Byte);
asm
  	and edx, $1f
  	btc [eax], edx  //Complemente the bit #I
end;

function BitIsSet (const BitList: TESBLongBitList; const I: Byte): Boolean;
asm
  	and  edx, $1f
  	bt   eax, edx  //Test the bit #I
  	setc al          //Result := Bit value (was in CF)
end;

function Bits2LStr (const BitList: TESBLongBitList): String32;
var
     I: Integer;
begin
     SetLength (Result, 32);
     for I := 0 to 31 do
          if BitIsSet (BitList, I) then
               Result [I + 1] := '1'
          else
               Result [I + 1] := '0';
end;

procedure ReverseBits (var BitList: TESBLongBitList);
asm
	push	esi		// Preserve ESI
	mov 	dx, [eax] // DX <- BitList
	mov 	esi, eax	// Move addr BitList into ESI
	mov	cx, 16	// 16 iterations needed for a word
@@Loop:
	shr	dx, 1	// move least significant bit into CF
	adc	ax, ax	// ax := 2 * ax + carry bit
	dec	cx
	jnz 	@@Loop
	mov	[esi], ax	// AX -> BitList
	pop	esi		// Restore ESI
end;

function Str2Bits (var S: String32): TESBLongBitList;
asm
	push		esi			// Preserve ESI
	push 	ebx			// Preserve EBX

	mov		esi, eax		// Move Pointer to S to ESI

	lodsb				// Read Length Byte into AL
	sub		ah, ah		// Clear AH
	mov		cx, ax		// CX <- AX, CX now has the length
	sub		ebx, ebx		// Clear EBX for BitList construction
	mov		dl, '0'		// For comparisons

@@Loop:
	lodsb				// Load Next Character in AL
	shl		ebx, 1		// EBX <- EBX * 2
	cmp		al, dl		// Is AL = '0'
	je		@@NoOne		// If Yes then it is Not a One and Jump
	add		ebx, 1		// Otherwise add 1
@@NoOne:
	Loop		@@Loop		// Loop till string all processed

	mov		eax, ebx		// EAX <- EBX for function result

	pop		ebx			// Restore EBX
	pop		esi			// Restore ESI
end;

function BitsSet (const BitList: TESBLongBitList): Byte;
asm
        mov edx, eax	// EDX <- BitList
        xor eax, eax	// Clear EAX
@@Loop: add edx, edx	// "Shift Left"
	                    // if no carry then no increment
        adc eax, 0   	// Otherwise Inc EAX (which is the function Result)
        and edx, edx
	jnz @@Loop  		// Loop for all Bits in BitList
end;

{--- If Function ---}

function iff (const Condition: Boolean; const TrueResult, FalseResult: string): string;
begin
     if Condition then
          Result := TrueResult
     else
          Result := FalseResult
end;

function iff (const Condition: Boolean; const TrueResult, FalseResult: Integer): Integer;
begin
     if Condition then
          Result := TrueResult
     else
          Result := FalseResult
end;

function iff (const Condition: Boolean; const TrueResult, FalseResult: Int64): Int64;
begin
     if Condition then
          Result := TrueResult
     else
          Result := FalseResult
end;

function iff (const Condition: Boolean; const TrueResult, FalseResult: Extended): Extended;
begin
     if Condition then
          Result := TrueResult
     else
          Result := FalseResult
end;

function iff (const Condition: Boolean; const TrueResult, FalseResult: Boolean): Boolean;
begin
     if Condition then
          Result := TrueResult
     else
          Result := FalseResult
end;

function iff (const Condition: Boolean; const TrueResult, FalseResult: Char): Char;
begin
     if Condition then
          Result := TrueResult
     else
          Result := FalseResult
end;

function iff (const Condition: Boolean;
     const TrueResult, FalseResult: TObject): TObject;
begin
     if Condition then
          Result := TrueResult
     else
          Result := FalseResult
end;

function iff (const Condition: Boolean;
     const TrueResult, FalseResult: TPoint): TPoint;
begin
     if Condition then
          Result := TrueResult
     else
          Result := FalseResult
end;

function iff (const Condition: Boolean;
     const TrueResult, FalseResult: TRect): TRect;
begin
     if Condition then
          Result := TrueResult
     else
          Result := FalseResult
end;

function iff (const Condition: Boolean; const TrueResult, FalseResult: TESBComplex): TESBComplex;
begin
     if Condition then
          Result := TrueResult
     else
          Result := FalseResult
end;

function iff (const Condition: Boolean;
     const TrueResult, FalseResult: TDateTime): TDateTime;
begin
     if Condition then
          Result := TrueResult
     else
          Result := FalseResult
end;

function iff (const Condition: Boolean;
     const TrueResult, FalseResult: Currency): Currency;
begin
     if Condition then
          Result := TrueResult
     else
          Result := FalseResult
end;

function iff (const Condition: Boolean;
     const TrueResult, FalseResult: TESBCurrency): TESBCurrency;
begin
     if Condition then
          Result := TrueResult
     else
          Result := FalseResult
end;

function iff (const Condition: Boolean;
     const TrueResult, FalseResult: TESBLongCurrency): TESBLongCurrency;
begin
     if Condition then
          Result := TrueResult
     else
          Result := FalseResult
end;

function iff (const Condition: Boolean;
     const TrueResult, FalseResult: TESBFraction): TESBFraction;
begin
     if Condition then
          Result := TrueResult
     else
          Result := FalseResult
end;

function iff (const Condition: Boolean;
     const TrueResult, FalseResult: TESBMixedFraction): TESBMixedFraction;
begin
     if Condition then
          Result := TrueResult
     else
          Result := FalseResult
end;

function iff (const Condition: Boolean;
     const TrueResult, FalseResult: TESBImperial): TESBImperial;
begin
     if Condition then
          Result := TrueResult
     else
          Result := FalseResult
end;

{$IFDEF MSWINDOWS}

function GetColorDepth: Integer;
var
     DC: hDC;
begin
     DC := GetDC (0);
     try
          Result := (GetDeviceCaps (DC, PLANES) * GetDeviceCaps (DC, BITSPIXEL));
     finally
          ReleaseDC (0, DC);
     end;
end;
{$ENDIF}

end.
