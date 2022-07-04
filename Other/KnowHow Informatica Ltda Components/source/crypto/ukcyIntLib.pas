{
=======================================================================

    KLIB v100
    Serious Software Made in Brazil


    home-page: www.knowhow-online.com.br (sorry, just portuguese)
    authors: Demian Lessa (demian@knowhow-online.com.br) and Leonardo Freitas

    Released under the Netscape Public License Version 1.0 
   (see license.txt)

    Unless otherwise noted, all materials provided in this release
    are copyright © 2001 by KnowHow Informatica Ltda.

=======================================================================
}

unit ukcyIntLib;

interface

uses
	Classes, uksyTypes;

{
---------------------------------------------------------------------------------
------------------ Multiple Precision Integer Memory Management -----------------
---------------------------------------------------------------------------------
}

type

	PInt = type PByteArray;

{ Multiple Precision Integer Management }

function IntAlloc( Size: Cardinal ): PInt;
procedure IntDispose( Source: PInt );

{ Memory management support routines }

function IntSize( Source: PInt ): Cardinal;
function IntCopy( Source, Target: PInt ): PInt;
function IntSetSize( var Source: PInt; Size: Cardinal ): PInt;
function IntCompare( P1, P2: PInt ): Integer;
function IntNormalize( Source: PInt ): PInt;

{ Alternative PInt allocation mechanisms }

function IntFromPInt( Source: PInt ): PInt;
function IntFromByte( Source: Byte ): PInt;
function IntFromWord( Source: Word ): PInt;
function IntFromStream( Source: TStream ): PInt;
function IntFromCardinal( Source: Cardinal ): PInt;
function IntFromString( const Source: string ): PInt;
function IntFromData( const Source; Size: Cardinal ): PInt;
function IntFromDecimalString( const Source: string ): PInt;

{
---------------------------------------------------------------------------------
----------------- Multiple Precision Integer Generic Operations -----------------
---------------------------------------------------------------------------------
}

{ Multiple Precision String Operations }

function AddString( S1, S2: string ): string;
function MulString( const S1, S2: string ): string;

{ Multiple Precision Integer Generic Operations }

function IntEqual( P1, P2: PInt ): Boolean;
function IntIsOne( Source: PInt ): Boolean;
function IntIsZero( Source: PInt ): Boolean;

{ String conversion routines }

function IntHexStr( Source: PInt ): string;
function IntOctStr( Source: PInt ): string;
function IntBinStr( Source: PInt ): string;
function IntDecStr( Source: PInt ): string;

{
---------------------------------------------------------------------------------
----------------- Multiple Precision Integer Bitwise Operations -----------------
---------------------------------------------------------------------------------
}

function IntNot( P1: PInt ): PInt;
function IntOr( P1, P2: PInt ): PInt;
function IntAnd( P1, P2: PInt ): PInt;
function IntXor( P1, P2: PInt ): PInt;

{
---------------------------------------------------------------------------------
---------------- Multiple Precision Integer Arithmetic Operations ---------------
---------------------------------------------------------------------------------
}

function IntAdd( P1, P2: PInt ): PInt;
function IntSub( P1, P2: PInt ): PInt;
function IntMul( P1, P2: PInt ): PInt;
function IntDiv( P1, P2: PInt; var R: PInt ): PInt;
function IntSquare( P1: PInt ): PInt;
function IntPower( P1, P2: PInt ): PInt;
function IntFactorial( P1: PInt ): PInt;

function IntAddModN( P1, P2, N: PInt ): PInt;
function IntSubModN( P1, P2, N: PInt ): PInt;
function IntMulModN( P1, P2, N: PInt ): PInt;
function IntDivModN( P1, P2, N: PInt; var R: PInt ): PInt;
function IntSquareModN( P1, N: PInt ): PInt;
function IntPowerModN( P1, P2, N: PInt ): PInt;

function IntGCD( P1, P2: PInt ): PInt;
function RelativePrimes( P1, P2: PInt ): Boolean;
function IntInverseModN( P1, P2: PInt ): PInt;

implementation

uses
	SysUtils, uksyUtils, uksyClasses, ukcyConsts, ukcyResStr;

type

	EKCYIntLib = class( EKCrypto );

{
	To do list:
		.Shl, Shr, RotL, RotR
		.LCM
		.Chinese remainder
		.RSA
		.More...
}
	
{
---------------------------------------------------------------------------------
------------------ Multiple Precision Integer Memory Management -----------------
---------------------------------------------------------------------------------
}

{--------------------------- Internal Implementation ---------------------------}

type

	TKIntGarbageCollector = class( TKGarbageCollector )
	private
		FPInts: TList;

		procedure _IntFree( Source: PInt );
		function _IntNew( Size: Cardinal ): PInt;

		function GetPInts: TList;
		procedure FreePInts; 

		property PInts: TList
						 read GetPInts;

	protected
		procedure FreeLists; override;

		function IntAlloc( Size: Cardinal ): PInt;
		procedure IntDispose( Source: PInt );

	end;

	TGetCollectorProc = function: TKIntGarbageCollector;


var
	_GetCollector: TGetCollectorProc = nil;

procedure StartTempCollector; forward;
procedure EndTempCollector( var Keep: PInt ); forward;

procedure TKIntGarbageCollector._IntFree( Source: PInt );
begin
	if ( not CheckPointer( Source ) ) then
		Exit;
	Source := IncPtr( Source, -SizeOf( Cardinal ) );
	System.FreeMem( Source, Cardinal( Pointer( Source )^ ) );
end;

function TKIntGarbageCollector._IntNew( Size: Cardinal ): PInt;
begin
	Inc( Size, SizeOf( Cardinal ) );
	Result := SysUtils.AllocMem( Size );
	Cardinal( Pointer( Result )^ ) := Size;
	Result := IncPtr( Result, SizeOf( Cardinal ) );
end;

function TKIntGarbageCollector.GetPInts: TList;
begin
	if ( not CheckObject( FPInts ) ) then
		FPInts := TList.Create;
	Result := FPInts;
end;

procedure TKIntGarbageCollector.FreePInts;
var
	i: Integer;
begin
{ PInt list has not been instantiated }
	if ( not CheckObject( FPInts ) ) then
		Exit;
{ Dispose each PInt in the list }
	for i := FPInts.Count - 1 downto 0 do
	begin
		_IntFree( FPInts[i] );
		FPInts.Delete( i );
	end;
{ Dispose the PInt list }
	FreeClean( FPInts );
end;

procedure TKIntGarbageCollector.FreeLists;
begin
	FreePInts;
	inherited FreeLists;
end;

function TKIntGarbageCollector.IntAlloc( Size: Cardinal ): PInt;
begin
	Result := _IntNew( Size );
	PInts.Add( Result );
end;

procedure TKIntGarbageCollector.IntDispose( Source: PInt );
var
	pi: PInt;
begin
	pi := Source;
	_IntFree( Source );
	PInts.Remove( pi );
end;

function _HexValue( Source: Char ): Byte;
begin
// DOUBT!
	Result := 0;
	Source := UpCase( Source );
	if ( Source in CHARSETS[ceDigit] ) then
		Result := Ord( Source ) - Ord( '0' )
	else if ( Source in ['A'..'F'] ) then
		Result := 10 + Ord( Source ) - Ord( 'A' );
end;

{---------------------------- Public Implementation ----------------------------}

function IntAlloc( Size: Cardinal ): PInt;
begin
	Result := _GetCollector.IntAlloc( Size );
end;

procedure IntDispose( Source: PInt );
begin
	_GetCollector.IntDispose( Source );
end;

function IntSize( Source: PInt ): Cardinal;
{ Returns the size in bytes of Source }
asm
	test EAX, EAX
	je   @@bye
	sub  EAX, 4
	mov  EAX, [EAX]
	sub  EAX, 4

@@bye:
	ret
end;

function IntCopy( Source, Target: PInt ): PInt;
{ Copies the value in Target to Source;
		.if Source is bigger than Target, extra bytes in Source will be zero;
		.if Target is bigger than Source, extra bytes in Target will be discarded; }
var
	i,
	iszSource,
	iszTarget: Cardinal;
begin
	ForcePointers( [Source, Target] );
	iszSource := IntSize( Source );
	iszTarget := IntSize( Target );
	Result := Source;
	for i := iszSource - 1 downto 0 do
		if ( i <= iszTarget - 1 ) then
			Source^[i] := Target^[i]
		else
			Source^[i] := 0;
end;

function IntSetSize( var Source: PInt; Size: Cardinal ): PInt;
{ Reallocates Source to guarantee its new size is Size }
begin
	Result := IntAlloc( Size );
	IntCopy( Result, Source );
	IntDispose( Source );
	Source := Result;
end;

function IntCompare( P1, P2: PInt ): Integer;
{ Returns 0 if P1 = P2; 1 if P1 > P2; -1 if P2 > P1 }

	function Sign( Value: Integer ): Integer;
	begin
		Result := 0;
		if ( Value > 0 ) then
			Result := 1
		else if ( Value < 0 ) then
			Result := -1;
	end;

var
	i,
	iszP1,
	iszP2,
	iszMax: Cardinal;
begin
	ForcePointers( [P1, P2] );
	Result := 0;
	iszP1 := IntSize( P1 );
	iszP2 := IntSize( P2 );
	iszMax := Max( iszP1, iszP2 );
	for i := iszMax - 1 downto 0 do
	begin
		if ( i < iszP1 ) and ( i < iszP2 ) then
			Result := Sign( P1^[i] - P2^[i] )
		else if ( i < iszP1 ) then
			Result := Sign( P1^[i] )
		else if ( i < iszP2 ) then
			Result := -Sign( P2^[i] );
		if ( Result <> 0 ) then
			Exit;
	end;
end;

function IntNormalize( Source: PInt ): PInt;
var
	iPos,
	iszSource: Cardinal;
begin
	ForcePointer( Source );
	Result := Source;
	iszSource := IntSize( Source );
	if ( iszSource <= 1 ) then
		Exit;
	iPos := iszSource;
	while ( iPos > 0 ) and ( Source^[iPos - 1] = 0 ) do
		Dec( iPos );
	if ( iPos > 0 ) then
	begin
		Result := IntAlloc( iPos );
		IntCopy( Result, Source );
		IntDispose( Source );
	end
	else if ( iPos = 0 ) then
	begin
		Result := IntFromByte( 0 );
		IntDispose( Source );
	end;
end;

{ Alternative PInt allocation mechanisms }

function IntFromPInt( Source: PInt ): PInt;
begin
	ForcePointer( Source );
	Result := IntAlloc( IntSize( Source ) );
	IntCopy( Result, Source );
end;

function IntFromByte( Source: Byte ): PInt;
begin
	Result := IntAlloc( SizeOf( Byte ) );
	Result^[0] := Source;
end;

function IntFromWord( Source: Word ): PInt;
var
	i: Integer;
begin
	Result := IntAlloc( SizeOf( Word ) );
	for i := 1 to SizeOf( Word ) do
	begin
		Result^[i] := Byte( Source and $FF );
		Source := ( Source shl 8 );
	end;
end;

function IntFromStream( Source: TStream ): PInt;
var
	p: Pointer;
	iPos,
	iszStream: Integer;
begin
	ForceStream( Source );
	iszStream := Source.Size;
	Result := IntAlloc( iszStream );
	GetMem( p, iszStream );
	try
		iPos := Source.Position;
		try
			Source.Position := 0;
			Source.ReadBuffer( p^, iszStream );
			Move( p^, Result^, iszStream );
		finally
			Source.Position := iPos;
		end;	
	finally
		FreeMem( p, Source.Size );
	end;
end;

function IntFromCardinal( Source: Cardinal ): PInt;
var
	i: Integer;
begin
	Result := IntAlloc( SizeOf( Cardinal ) );
	for i := 1 to SizeOf( Cardinal ) do
	begin
		Result^[i] := Byte( Source and $FF );
		Source := ( Source shl 8 );
	end;
end;

function IntFromString( const Source: string ): PInt;
var
	iszString: Integer;
begin
	ForceStr( Source );
	iszString := Length( Source );
	Result := IntAlloc( iszString );
	Move( Pointer( Source )^, Result^, iszString );
end;

function IntFromData( const Source; Size: Cardinal ): PInt;
begin
	ForceReference( Source );
	Result := IntAlloc( Size );
	Move( Source, Result^, Size );
end;

function IntFromDecimalString( const Source: string ): PInt;
var
	i,
	iPos: Integer;
	wDividend: Word;
	sHex,
	sResult,
	sDividend: string;
begin
	ForceTrimStr( Source );
	sHex := '';
	sDividend := Source;
	while CheckStr( sDividend ) do
	begin
		iPos := 1;
		sResult := '';
		if ( Length( sDividend ) > 2 ) or ( StrToInt( sDividend ) >= 16 ) then
			while ( StrToInt( Copy( sDividend, 1, iPos ) ) < 16 ) do
				Inc( iPos );
		wDividend := StrToInt( Copy( sDividend, 1, iPos ) );
		for i := iPos to Length( sDividend ) do
		begin
			sResult := sResult + IntToStr( wDividend div 16 );
			if ( i < Length( sDividend ) ) then
				wDividend := ( wDividend mod 16 ) * 10 + StrToInt( sDividend[i + 1] )
			else
			begin
				sDividend := sResult;
				wDividend := ( wDividend mod 16 );
				sHex := IntToHex( wDividend mod 16, 1 ) + sHex;
			end;
		end;
		if ( Length( sDividend ) <= 2 ) and ( StrToInt( sDividend ) < 16 ) then
		begin
			sHex := IntToHex( StrToInt( sDividend ) mod 16, 1 ) + sHex;
			sDividend := '';
			if ( ( Length( sHex ) mod 2 ) = 1 ) then
				sHex := '0' + sHex;
		end;
	end;
	Result := IntAlloc( Length( sHex ) div 2 );
	for i := 0 to ( Length( sHex ) div 2 - 1 ) do
		Result^[( Length( sHex ) div 2 - 1 ) - i] :=
			_HexValue( sHex[2 * i + 1] ) * 16 +	_HexValue( sHex[2 * i + 2] );
end;

{
---------------------------------------------------------------------------------
----------------- Multiple Precision Integer Generic Operations -----------------
---------------------------------------------------------------------------------
}

{ Multiple Precision String Operations }

function AddString( S1, S2: string ): string;
var
	b1,
	b2,
	cf: byte;
	i: Cardinal;
begin
	ForceStrCharSetEnum( S1, ceDigit );
	ForceStrCharSetEnum( S2, ceDigit );
// DOUBT!
	while ( Length( S1 ) > Length( S2 ) ) do { StrLeftPad is better ? }
		S2 := '0' + S2;
	while ( Length( S2 ) > Length( S1 ) ) do
		S1 := '0' + S1;

	cf := 0;
	Result := '';
	for i := Length( S1 ) downto 1 do
	begin
		b1 := _HexValue( S1[i] );
		b2 := _HexValue( S2[i] );
		Result := Char( 48 + ( ( b1 + b2 + cf ) mod 10 ) ) + Result;
		cf := ( ( b1 + b2 + cf ) div 10 );
	end;
	if ( cf <> 0 ) then
		Result := Char( 48 + ( cf mod 10 ) ) + Result;
end;

function MulString( const S1, S2: string ): string;
var
	p1,
	p2,
	sPartial: string;
	i,
	j: Integer;
	b1,
	b2,
	cf: Byte;
begin
  ForceStrCharSetEnum( S1, ceDigit );
	ForceStrCharSetEnum( S2, ceDigit );
	
	if ( Length( S1 ) >= Length( S2 ) ) then
	begin
		p1 := S1;
		p2 := S2;
	end
	else
	begin
		p1 := S2;
		p2 := S1;
	end;

	Result := '0';

	for i := Length( p2 ) downto 1 do
	begin
		cf := 0;
		sPartial := '';
		for j := Length( p1 ) downto 1 do
		begin
			b1 := _HexValue( p2[i] );
			b2 := _HexValue( p1[j] );
			sPartial := Char( 48 + ( ( b1 * b2 + cf ) mod 10 ) ) + sPartial;
			cf := ( ( b1 * b2 + cf ) div 10 );
		end;
		if ( cf <> 0 ) then
			sPartial := Char( 48 + ( cf mod 10 ) ) + sPartial;
		Result := AddString( Result, StrRightPad( sPartial,
			Length( sPartial ) + Length( p2 ) - i, '0' ) );
	end;

end;

{ Multiple Precision Integer Generic Operations }

function IntEqual( P1, P2: PInt ): Boolean;
begin
	Result := ( IntCompare( P1, P2 ) = 0 );
end;

function IntIsOne( Source: PInt ): Boolean;
var
	i: Cardinal;
begin
	ForcePointer( Source );
	Result := false;
	for i := IntSize( Source ) - 1 downto 1 do
		if ( Source^[i] <> 0 ) then
			Exit;
	Result := ( Source^[0] = 1 );
end;

function IntIsZero( Source: PInt ): Boolean;
var
	i: Cardinal;
begin
	ForcePointer( Source );
	Result := false;
	for i := IntSize( Source ) - 1 downto 0 do
		if ( Source^[i] <> 0 ) then
			Exit;
	Result := true;
end;

{ String conversion routines }

function IntHexStr( Source: PInt ): string;
var
	i,
	iszSource: Cardinal;
begin
	Result := '';
	if ( not CheckPointer( Source ) ) then
		Exit;
	iszSource := IntSize( Source );
	for i := iszSource - 1 downto 0 do
		Result := Result + IntToHex( Source^[i], 2 );
	iszSource := Length( Result );
	if ( ( iszSource mod 2 ) = 1 ) then
		Result := '0' + Result
	else if ( iszSource = 0 ) then
		Result := '00';
end;

function IntOctStr( Source: PInt ): string;
var
	s: string;
	i: Integer;
begin
  Result := '';
	s := IntHexStr( Source );
	for i := 1 to Length( s ) do
		Result := Result + HEX_TO_OCT[_HexValue( s[i] )];
end;

function IntBinStr( Source: PInt ): string;
var
	s: string;
	i: Integer;
begin
	Result := '';
	s := IntHexStr( Source );
	for i := 1 to Length( s ) do
		Result := Result + HEX_TO_BIN[_HexValue( s[i] )];
end;

function IntDecStr( Source: PInt ): string;
var
  base: string;
	i,
	iszSource: Cardinal;
begin
	Result := '';
	if ( not CheckPointer( Source ) ) then
		Exit;
	base := '1';
	Result := '0';
	iszSource := IntSize( Source );
	for i := 0 to iszSource - 1 do
	begin
		Result := AddString( Result, MulString( IntToStr( Source^[i] ), base ) );
		base := MulString( base, '256' );
	end;
end;

{
---------------------------------------------------------------------------------
----------------- Multiple Precision Integer Bitwise Operations -----------------
---------------------------------------------------------------------------------
}

function IntNot( P1: PInt ): PInt;
var
	i,
	isz: Cardinal;
begin
	ForcePointers( [P1] );
	Result := IntFromPInt( P1 );
	isz := IntSize( Result );
	i := 0;
	while ( i < isz ) do
	begin
		Result^[i] := ( not Result^[i] );
		Inc( i );
	end;
	Result := IntNormalize( Result );
end;

function IntOr( P1, P2: PInt ): PInt;
var
	pTemp: PInt;
	i,
	iszLo,
	iszHi: Cardinal;
begin
	ForcePointers( [P1, P2] );
	if ( IntCompare( P1, P2 ) >= 0 ) then
	begin
		iszLo := IntSize( P2 );
		pTemp := IntFromPInt( P2 );
		Result := IntFromPInt( P1 );
	end
	else
	begin
		iszLo := IntSize( P1 );
		pTemp := IntFromPInt( P1 );
		Result := IntFromPInt( P2 );
	end;
	try
		iszHi := IntSize( Result );
		i := 0;
		while ( i < iszLo ) do
		begin
			Result^[i] := Result^[i] or pTemp^[i];
			Inc( i );
		end;
		while ( i < iszHi ) do
		begin
			Result^[i] := Result^[i] or 0;
			Inc( i );
		end;
	finally
		IntDispose( pTemp );
	end;
	Result := IntNormalize( Result );
end;

function IntAnd( P1, P2: PInt ): PInt;
var
  pTemp: PInt;
	i,
	iszLo,
	iszHi: Cardinal;
begin
	ForcePointers( [P1, P2] );
	if ( IntCompare( P1, P2 ) >= 0 ) then
	begin
		iszLo := IntSize( P2 );
		pTemp := IntFromPInt( P2 );
		Result := IntFromPInt( P1 );
	end
	else
	begin
		iszLo := IntSize( P1 );
		pTemp := IntFromPInt( P1 );
		Result := IntFromPInt( P2 );
	end;
	try
		iszHi := IntSize( Result );
		i := 0;
		while ( i < iszLo ) do
		begin
			Result^[i] := Result^[i] and pTemp^[i];
			Inc( i );
		end;
		while ( i < iszHi ) do
		begin
			Result^[i] := Result^[i] and 0;
			Inc( i );
		end;
	finally
		IntDispose( pTemp );
	end;
	Result := IntNormalize( Result );
end;

function IntXor( P1, P2: PInt ): PInt;
var
  pTemp: PInt;
	i,
	iszLo,
	iszHi: Cardinal;
begin
	ForcePointers( [P1, P2] );
	if ( IntCompare( P1, P2 ) >= 0 ) then
	begin
		iszLo := IntSize( P2 );
		pTemp := IntFromPInt( P2 );
		Result := IntFromPInt( P1 );
	end
	else
	begin
		iszLo := IntSize( P1 );
		pTemp := IntFromPInt( P1 );
		Result := IntFromPInt( P2 );
	end;
	try
		iszHi := IntSize( Result );
		i := 0;
		while ( i < iszLo ) do
		begin
			Result^[i] := Result^[i] xor pTemp^[i];
			Inc( i );
		end;
		while ( i < iszHi ) do
		begin
			Result^[i] := Result^[i] xor 0;
			Inc( i );
		end;
	finally
		IntDispose( pTemp );
	end;
	Result := IntNormalize( Result );
end;

{
---------------------------------------------------------------------------------
---------------- Multiple Precision Integer Arithmetic Operations ---------------
---------------------------------------------------------------------------------
}

{--------------------------- Internal Implementation ---------------------------}

{
	Multiple precision assembler support for addition, subtraction,
	multiplication, and division. Adapted from Ray Duncan, "Microsoft Macro
	Assembler". Chapter 12.
}

procedure _IntAdd( P1, Sum: PInt ); assembler;
asm
	push  ESI
	push  EDI
	push  EBP
	push  EBX               { preserve registers }

	mov   ESI, P1
	mov   EDI, Sum

	push  EAX
	call  IntSize
	mov   ECX, EAX          { ECX = IntSize( P1 ) }
	pop   EAX               { EAX = P1 }
	cld
	clc

@@continue:
	lodsb
	adc   byte ptr [EDI], AL
	inc   EDI
	loop  @@continue

@@bye:
	pop   EBX
	pop   EBP
	pop   EDI
	pop   ESI               { restore registers }
	cld
end;

procedure _IntSub( P1, Sub: PInt ); assembler;
asm
	push  ESI
	push  EDI
	push  EBP
	push  EBX               { preserve registers }

	mov   ESI, P1
	mov   EDI, Sub

	push  EAX
	call  IntSize
	mov   ECX, EAX          { ECX = IntSize( P1 ) }
	pop   EAX               { EAX = P1 }
	cld
	clc

@@continue:
	lodsb
	sbb   byte ptr [EDI], AL
	inc   EDI
	loop  @@continue

@@bye:
	pop   EBX
	pop   EBP
	pop   EDI
	pop   ESI               { restore registers }
	cld
end;

procedure _IntMul( P1, Product: PInt );
{ K = IntSize( P1 ); 2 * K = IntSize( Product ) }
asm
	push  ESI
	push  EDI
	push  EBP
	push  EBX               { preserve registers }

	mov   ESI, P1
	mov   EDI, Product

	call  IntSize
	mov   ECX, EAX          { ECX = IntSize( P1 ) = size of operands }

	push  EDI               { save EDI for future use }

	mov   EDX, ECX          { EDX = operand size = Max( IntSize( P1 ), IntSize( P2 ) ) }
	add   EDI, ECX          { EDI = hi order address of product }
	mov   EBP, EDI

	xor   EAX, EAX
	rep   STOSB             { zero all high order bytes of product }

	pop   EDI
	mov   ECX, EDX
	shl   ECX, 1
	shl   ECX, 1
	shl   ECX, 1
	inc   ECX							  { ECX = #bits per argument + 1 }
	cld
	clc

@@mul_1:
	pushf
	mov   EBX, EDX
	shl   EBX, 1
	dec   EBX
	popf

@@mul_2:
	rcr   byte ptr [EDI + EBX], 1
	dec   EBX
	jns   @@mul_2
	jnc   @@mul_4
	xchg  EBP, EDI
	push  ECX
	mov   ECX, EDX
	xor   EBX, EBX

@@mul_3:
	mov   AL, [ESI + EBX]
	adc   [EDI + EBX], AL
	inc   EBX
	loop  @@mul_3
	pop   ECX
	xchg  EBP, EDI

@@mul_4:
	loop  @@mul_1

@@bye:
	pop   EBX
	pop   EBP
	pop   EDI
	pop   ESI               { restore registers }
	cld
end;

procedure _IntDiv( Divisor, Dividend: PInt );
{ K = IntSize( Divisor ); 2 * K = IntSize( Dividend )
	Divisor will output the remainder; Dividend will output the quotient }
asm
	push  ESI
	push  EDI
	push  EBP
	push  EBX               { preserve registers }

	mov   ESI, Divisor
	mov   EDI, Dividend

	call  IntSize
	mov   ECX, EAX          { ECX = IntSize( Divisor ) }

	mov   EDX, ECX
	mov   EBP, ECX
	shl   EBP, 1
	shl   EBP, 1
	shl   EBP, 1
	clc

@@div_1:
	push  EDI
	mov   ECX, EDX

@@div_2:
	rcl   word ptr [EDI], 1
	inc   EDI
	inc   EDI
	loop  @@div_2
	pop   EDI
	jnc   @@div_5

@@div_3:
	push  ESI
	push  EDI
	add   EDI, EDX
	mov   ECX, EDX
	clc

@@div_4:
	mov   AL, [ESI]
	sbb   [EDI], AL
	inc   ESI
	inc   EDI
	loop  @@div_4
	pop   EDI
	pop   ESI
	stc
	dec   EBP
	jnz   @@div_1
	jmp   @@div_7

@@div_5:
	push  ESI
	push  EDI
	add   EDI, EDX
	mov   ECX, EDX
	clc

@@div_6:
	mov   AL, [EDI]
	sbb   AL, [ESI]
	inc   ESI
	inc   EDI
	loop  @@div_6
	pop   EDI
	pop   ESI
	jnc   @@div_3
	clc
	dec   EBP
	jnz   @@div_1

@@div_7:
	mov   ECX, EDX

@@div_8:
	rcl   byte ptr [EDI], 1
	inc   EDI
	loop  @@div_8
	xchg  ESI, EDI
	mov   ECX, EDX
	rep   movsb

@@bye:
	pop   EBX
	pop   EBP
	pop   EDI
	pop   ESI               { restore registers }
	cld
end;

{---------------------------- Public Implementation ----------------------------}

function IntAdd( P1, P2: PInt ): PInt;
var
	op: PInt;
	iszMax: Cardinal;
begin
	ForcePointers( [P1, P2] );
	iszMax := Max( IntSize( P1 ), IntSize( P2 ) ) + 1;
	op := IntAlloc( iszMax );
	Result := IntAlloc( iszMax );
	IntCopy( op, P1 );
	IntCopy( Result, P2 );
	_IntAdd( op, Result );
	IntDispose( op );
	Result := IntNormalize( Result );
end;

function IntSub( P1, P2: PInt ): PInt;
var
	op: PInt;
	iszMax: Cardinal;
begin
	ForcePointers( [P1, P2] );
	iszMax := Max( IntSize( P1 ), IntSize( P2 ) );
	op := IntAlloc( iszMax );
	Result := IntAlloc( iszMax );
	IntCopy( op, P2 );
	IntCopy( Result, P1 );
	_IntSub( op, Result );
	IntDispose( op );
	Result := IntNormalize( Result );
end;

function IntMul( P1, P2: PInt ): PInt;
var
	op: PInt;
	iszP1,
	iszP2,
	iszMax: Cardinal;
begin
	ForcePointers( [P1, P2] );
	iszP1 := IntSize( P1 );
	iszP2 := IntSize( P2 );
	iszMax := Max( iszP1, iszP2 );
	Result := IntAlloc( iszMax * 2 );
	if ( iszP1 >= iszP2 ) then
	begin
		op := P1;
		IntCopy( Result, P2 );
	end
	else
	begin
		op := P2;
		IntCopy( Result, P1 );
	end;
	if CheckPointers( [op, Result] ) then
	begin
		_IntMul( op, Result );
		Result := IntNormalize( Result );
	end;
end;

function IntDiv( P1, P2: PInt; var R: PInt ): PInt;
var
	iszP1,
	iszP2,
	iComp: Integer;
begin
	ForcePointers( [P1, P2] );
	if ( IntIsZero( P1 ) and IntIsZero( P2 ) ) then
		RaiseException( EKCYIntLib, sErrIntNotDefined );
	if IntIsZero( P2 ) then
		RaiseException( EKCYIntLib, sErrIntDivByZero );
	if IntIsZero( P1 ) then
	begin
		Result := IntFromByte( 0 );
		Exit;
	end;
	iComp := IntCompare( P1, P2 );
{ P1 = P2 => Q = 1; R = 0 }
	if ( iComp = 0 ) then
	begin
		Result := IntFromByte( 1 );
		R := IntFromByte( 0 );
		Exit;
	end
{ P1 < P2 => Q = 0; R = P1 }
	else if ( iComp = -1 ) then
	begin
		Result := IntFromByte( 0 );
		R := IntFromPInt( P1 );
		Exit;
	end;
{ P1 > P2 }
	iszP1 := IntSize( P1 );
	iszP2 := IntSize( P2 );
	while ( 2 * iszP2 < iszP1 ) do
		inc( iszP2 );
	R := IntFromPInt( P2 ); { R = divisor }
	Result := IntAlloc( 2 * iszP2 );
	IntCopy( Result, P1 ); { Result = dividend }
	_IntDiv( R, Result );  { R = remainder; Result = quotient }
	R := IntNormalize( R );
	IntSetSize( Result, iszP2 );
	Result := IntNormalize( Result );
end;

function IntSquare( P1: PInt ): PInt;
begin
	Result := IntMul( P1, P1 );
end;

function IntPower( P1, P2: PInt ): PInt;
var
	p: PInt;
begin
	ForcePointers( [P1, P2] );
	if IntIsZero( P1 ) then
		RaiseException( EKCYIntLib, sErrIntPowerBaseZero );
	if ( IntIsOne( P1 ) or IntIsZero( P2 ) ) then
	begin
		Result := IntFromByte( 1 );
		Exit;
	end
	else if IntIsOne( P2 ) then
	begin
		Result := IntFromPInt( P1 );
		Exit;
	end;
{ redirect garbage collection for a temporary collector; by the end of the
	algorithm, the original garbage collector will be restored for good }
	StartTempCollector;
	try
		p := IntFromByte( 1 );
		Result := IntMul( P1, p );
		while ( not IntIsOne( P2 ) ) do
		begin
			Result := IntMul( Result, P1 );
			_IntSub( P2, p );
		end;
	finally
		EndTempCollector( Result );
	end;
end;

function IntFactorial( P1: PInt ): PInt;
var
	pOne,
	pHigh: PInt;
begin
	ForcePointers( [P1] );
	if ( IntIsZero( P1 ) or IntIsOne( P1 ) ) then
	begin
		Result := IntFromByte( 1 );
		Exit;
	end;
{ redirect garbage collection for a temporary collector; by the end of the
	algorithm, the original garbage collector will be restored for good }
	StartTempCollector;
	try
		pOne := IntFromByte( 1 );
		Result := IntFromByte( 1 );
		pHigh := IntFromPInt( P1 );
		while ( not IntIsOne( pHigh ) ) do
		begin
			Result := IntMul( Result, pHigh );
			pHigh := IntSub( pHigh, pOne );
		end;
	finally
		EndTempCollector( Result );
	end;
end;

function IntAddModN( P1, P2, N: PInt ): PInt;
var
	pSum,
	pQuo: PInt;
begin
	pQuo := nil;
	ForcePointers( [P1, P2, N] );
	if IntIsZero( N ) then
		RaiseException( EKCYIntLib, sErrIntModZero );
	pSum := IntAdd( P1, P2 );
	try
		pQuo := IntDiv( pSum, N, Result );
	finally
		IntDispose( pQuo );
		IntDispose( pSum );
	end;
end;

function IntSubModN( P1, P2, N: PInt ): PInt;
var
	pSub,
	pQuo: PInt;
begin
	pQuo := nil;
	ForcePointers( [P1, P2, N] );
	if IntIsZero( N ) then
		RaiseException( EKCYIntLib, sErrIntModZero );
	pSub := IntSub( P1, P2 );
	try
		pQuo := IntDiv( pSub, N, Result );
	finally
		IntDispose( pQuo );
		IntDispose( pSub );
	end;
end;

function IntMulModN( P1, P2, N: PInt ): PInt;
var
	pMul,
	pQuo: PInt;
begin
	pQuo := nil;
	ForcePointers( [P1, P2, N] );
	if IntIsZero( N ) then
		RaiseException( EKCYIntLib, sErrIntModZero );
	pMul := IntMul( P1, P2 );
	try
		pQuo := IntDiv( pMul, N, Result );
	finally
		IntDispose( pQuo );
		IntDispose( pMul );
	end;
end;

function IntDivModN( P1, P2, N: PInt; var R: PInt ): PInt;
var
	pDiv,
	pRem,
	pQuo: PInt;
begin
	pQuo := nil;
	ForcePointers( [P1, P2, N] );
	if IntIsZero( N ) then
		RaiseException( EKCYIntLib, sErrIntModZero );
	pDiv := IntDiv( P1, P2, pRem );
	try
		pQuo := IntDiv( pDiv, N, Result );
	finally
		IntDispose( pQuo );
		IntDispose( pDiv );
		IntDispose( pRem );
	end;
end;

function IntSquareModN( P1, N: PInt ): PInt;
var
	pSqu,
	pQuo: PInt;
begin
	pQuo := nil;
	ForcePointers( [P1, N] );
	if IntIsZero( N ) then
		RaiseException( EKCYIntLib, sErrIntModZero );
	pSqu := IntSquare( P1 );
	try
		pQuo := IntDiv( pSqu, N, Result );
	finally
		IntDispose( pQuo );
		IntDispose( pSqu );
	end;
end;

function IntPowerModN( P1, P2, N: PInt ): PInt;
var
	pPow,
	pQuo: PInt;
begin
	pQuo := nil;
	ForcePointers( [P1, P2, N] );
	if IntIsZero( N ) then
		RaiseException( EKCYIntLib, sErrIntModZero );
	pPow := IntPower( P1, P2 );
	try
		pQuo := IntDiv( pPow, N, Result );
	finally
		IntDispose( pQuo );
		IntDispose( pPow );
	end;
end;

function IntGCD( P1, P2: PInt ): PInt;
var
	rn,
	pBig,
	pSmall: PInt;
	bFirst: Boolean;
	iCompare: Integer;
begin
	ForcePointers( [P1, P2] );
	iCompare := IntCompare( P1, P2 );
	if ( iCompare = 0 ) then
	begin
		Result := IntFromPInt( P1 );
		Exit;
	end
	else if ( iCompare = 1 ) then
	begin
		pBig := P1;
		pSmall := P2;
	end
	else
	begin
		pBig := P2;
		pSmall := P1;
	end;
{ redirect garbage collection for a temporary collector; by the end of the
	algorithm, the original garbage collector will be restored for good }
	Result := nil;
	StartTempCollector;
	try
		bFirst := true;
		while ( bFirst or ( not IntIsZero( rn ) ) ) do
		begin
			if ( not bFirst ) then
			begin
				pBig := pSmall;
				pSmall := rn;
			end;
			IntDiv( pBig, pSmall, rn );
			if IntIsZero( rn ) then
				Result := pSmall;
			bFirst := false;
		end;
	finally
		EndTempCollector( Result );
	end;
	if ( not CheckPointer( Result ) ) then
		Result := IntFromByte( 1 );
end;

function RelativePrimes( P1, P2: PInt ): Boolean;
begin
	Result := IntIsOne( IntGCD( P1, P2 ) );
end;

function IntInverseModN( P1, P2: PInt ): PInt;
{ P1 = b; P2 = n }
var
	n,
	pBig,
	pSmall,
	qn,
	rn,
	t0,
	tn,
	temp: PInt;
	bRaise,
	bFirst: Boolean;
	iCompare: Integer;
begin
	ForcePointers( [P1, P2] );
	iCompare := IntCompare( P1, P2 );
	if ( iCompare = 0 ) then
		RaiseException( EKCYIntLib, sErrInvInverseModNEqualParams );
	if ( iCompare = 1 ) then
		RaiseExceptionFmt( EKCYIntLib, sErrInvInverseModNParams, [IntHexStr( P2 ),
		  IntHexStr( P1 )] );
	Result := nil;
	bRaise := false;
{ redirect garbage collection for a temporary collector; by the end of the
	algorithm, the original garbage collector will be restored for good }
	StartTempCollector;
	try
		pBig := P2;
		pSmall := P1;
		bFirst := true;
		t0 := IntFromByte( 0 );
		tn := IntFromByte( 1 );
		n := IntFromPInt( P2 );
		while ( bFirst or ( not IntIsZero( rn ) ) ) do
		begin
			if ( not bFirst ) then
			begin
				pBig := pSmall;
				pSmall := rn;
			end;
			qn := IntDiv( pBig, pSmall, rn );
			temp := IntMul( qn, tn );
			if ( not IntIsZero( rn ) ) then
			begin
				if ( IntCompare( t0, temp ) = -1 ) then
				begin
					temp := IntSub( temp, t0 );
					IntDiv( temp, n, temp );
					temp := IntSub( n, temp );
				end
				else
				begin
					temp := IntSub( t0, temp );
					IntDiv( temp, n, temp );
				end;
				t0 := tn;
				tn := temp;
			end
			else
				Result := pSmall;
			bFirst := false;
		end;
		if ( not IntIsOne( Result ) ) then
			bRaise := true
		else
			IntDiv( tn, n, Result );
	finally
		EndTempCollector( Result );
	end;
	if bRaise then
		RaiseExceptionFmt( EKCYIntLib, sErrInvInverseModN , [IntHexStr( P1 ), IntHexStr( P2 )] );
	if ( not CheckPointer( Result ) ) then
		RaiseException( EKCYIntLib, sErrInvInverseModnState );
end;

{
---------------------------------------------------------------------------------
----------- Multiple Precision Integer Memory Management (Forward) --------------
---------------------------------------------------------------------------------
}

var
	TempID: TDateTime = 0;
	DefaultID: TDateTime = 0;
	TempCollector: Boolean = false;
	CollectorList: TKGarbageCollectorList = nil;

procedure StartTempCollector;
begin
{ force initialization, if it hasn't been done yet }
	if ( DefaultID = 0 ) then
		_GetCollector;
	TempCollector := true;
	CollectorList.NewCollector( TKIntGarbageCollector, TempID );
end;

procedure EndTempCollector( var Keep: PInt );
var
	p: Pointer;
	iszKeep: Cardinal;
begin
	p := nil;
	iszKeep := 0;
	TempCollector := false;
	if CheckPointer( Keep ) then
	begin
		iszKeep := IntSize( Keep ) + SizeOf( Cardinal );
		p := AllocMem( iszKeep );
		Move( IncPtr( Keep, -SizeOf( Cardinal ) )^, p^, iszKeep );
		p := IncPtr( p, SizeOf( Cardinal ) );
	end;
	CollectorList.DisposeCollectorByID( TempID );
	TempID := 0;
	if CheckPointer( p ) then
	begin
		Keep := IntFromPInt( p );
		FreeMem( IncPtr( p, -SizeOf( Cardinal ) ), iszKeep );
	end;
end;
 
function InternalGetCollector: TKIntGarbageCollector;
begin
	if ( not CheckObject( CollectorList ) ) then
	begin
		CollectorList := TKGarbageCollectorList.Create;
		Result := TKIntGarbageCollector( CollectorList.NewCollector( TKIntGarbageCollector,
			DefaultID ) );
		Exit;
	end;
	if TempCollector then
		Result := TKIntGarbageCollector( CollectorList.Collectors[TempID] )
	else
		Result := TKIntGarbageCollector( CollectorList.Collectors[DefaultID] );
end;

{
--------------------------------------------------------------------------------
------------------- Initialization and Finalization Routines -------------------
--------------------------------------------------------------------------------
}

procedure Init;
begin
	_GetCollector := InternalGetCollector;
end;

procedure Done;
begin
	if CheckObject( CollectorList ) then
		CollectorList.Free;
end;

initialization
	Init;

finalization
	Done;

end.
