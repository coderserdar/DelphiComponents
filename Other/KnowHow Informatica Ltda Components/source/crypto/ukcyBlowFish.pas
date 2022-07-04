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

unit ukcyBlowFish;

{$I s:\v100\include\iKLIB100.inc}

{$R-,Q-}

interface

uses
	uksyUtils, ukcyConsts, ukcyTypes;


type

	EKCYBlowFish = class( EKCrypto );

{
--------------------------------------------------------------------------------
-------------------- BlowFish Implementation Architecture ----------------------
--------------------------------------------------------------------------------
}

{ TKCustomBlowFish }

	TKCustomBlowFish = class( TObject )
	private
		fbfPad: Byte;
		fpbfKey: Pointer;
		fbfInitialized: Boolean;
		fbfszKey: TKBlowFishKeySize;
		fpbfControl: PKBlowFishControl;
		fbfInterations: TKBlowFishIterations;

		procedure ForceInitialization;

	protected

		function PaddingToCypher( const bIn; szIn: Integer; var bOut: Pointer ):
			Integer; virtual; abstract;
		function UnpaddingDecipher( const bIn; szIn: Integer; var bOut: Pointer ):
			Integer; virtual; abstract;
		procedure AllocateOutPut( pIn: Pointer; szIn: Integer;
			var bOut: Pointer ); virtual;
		function DeAllocateOutPut( var pOut: Pointer ): Integer;
		procedure Initialize;
		procedure ForceBufferInBlockSize( szIn: Integer ); dynamic;
		procedure ForceSizeIn( szIn: Integer ); dynamic;
		procedure bfEncipher( const bIn; szIn: Integer; var bOut: Pointer;
			var szOut: Integer ); virtual;
		procedure bfDecipher( const bIn; szIn: Integer; var bOut: Pointer;
			var szOut: Integer ); virtual;
		function GetPadAsChar: Char; virtual;
		procedure SetPadAsChar( Value: Char ); virtual;
		constructor InternalCreate( pbfKey: Pointer; szKey: TKBlowFishKeySize;
			Iterations: TKBlowFishIterations; Pad: Byte ); virtual;

		property bfKey: Pointer
						 read fpbfKey;
		property bfKeySize: TKBlowFishKeySize
						 read fbfszKey;
		property bfIterations: TKBlowFishIterations
						 read fbfInterations;
		property bfPad: Byte
						 read fbfPad write fbfPad;
		property bfInitiazlied: Boolean
						 read fbfInitialized;

	public
		destructor Destroy; override;

	end;

{ TKPointerBlowFish }

	TKPointerBlowFish = class;

{$M+}
	TKBlockPadType = ( bptNone, bptNormal, bptBinary, bptCustom );
{$M-}	

	TKCustomPadEvent = procedure( Sender: TKPointerBlowFish; Data: Pointer;
		szIn: Integer; var PadCount: Byte; Padding: Boolean ) of object;

	TKPointerBlowFish = class( TKCustomBlowFish )
	private
		fUnPadCount: Integer;
		fBlockPadType: TKBlockPadType;
		FOnCustomPad: TKCustomPadEvent;

	protected

		function PaddingToCypher( const bIn; szIn: Integer; var bOut: Pointer ):
			Integer; override;
		function UnpaddingDecipher( const bIn; szIn: Integer; var bOut: Pointer ):
			Integer; override;

		procedure CheckValidBlockPadType; dynamic;	

		property BlockPadType: TKBlockPadType
						 read fBlockPadType;

	public
		constructor Create( Key: Pointer; KeySize: TKBlowFishKeySize;
			ABlockPadType: TKBlockPadType );

		function ptEncipher( pIn: Pointer; szIn: Integer;
			var szOut: Integer ): Pointer; virtual;
		function ptDecipher( pIn: Pointer; szIn: Integer;
			var szOut: Integer ): Pointer; virtual;

		property UnPadCount: Integer
						 read fUnPadCount;
		property OnCustomPad: TKCustomPadEvent
						 read FOnCustomPad write FOnCustomPad;

	end;

{ TKStringBlowFish }

	TKStringBlowFish = class( TKPointerBlowFish )
	private
		function GetStrKey: string;

	public
		constructor Create( const sKey: string;
			ABlockPadType: TKBlockPadType ); virtual;

  	class procedure ForceStringKey( const sKey: string ); dynamic;
		class procedure ForceStringData( const S: string ); dynamic;

		function Encipher( const S: string ): string; virtual;
		function Decipher( const S: string ): string; virtual;

		property Key: string
						 read GetStrKey;
		property PadChar: Char
						 read GetPadAsChar write SetPadAsChar;

	end;

{ TKPCharBlowFish }

	TKPCharBlowFish = class( TKPointerBlowFish )
	private
		function GetPCharKey: PChar;

	protected
		procedure AllocateOutPut( pIn: Pointer; szIn: Integer;
			var bOut: Pointer ); override;

	public
		constructor Create( sKey: PChar; szIn: Integer;
			ABlockPadType: TKBlockPadType ); virtual;

		class procedure ForcePCharKey( sKey: PChar ); dynamic;
		class procedure ForcePCharData( pc: PChar ); dynamic;

		function Encipher( pc: PChar; szIn: Integer; var szOut: Integer ): PChar; virtual;
		function Decipher( pc: PChar; szIn: Integer; var szOut: Integer ): PChar; virtual;

		property Key: PChar
						 read GetPCharKey;
		property PadChar: Char
						 read GetPadAsChar write SetPadAsChar;

	end;

implementation

uses
	Windows, SysUtils, ukcyResStr, ukcyUtils;

{

# An Integer library with USEFUL and WELL-NAMED routines;
# A Custom Block cypher base class:

	Pointer-mode only abstract block cypher class;
		initialization,
		finalization,
		selftest,
		cypher, and
		decypher routines;
	Deal with padding;
	Deal with data streams;
	Variable key size (let the constructor deal with this);
	Variable input size (known just to descendant classes, so make it static);
	Variable output size (known just to descendant classes, so make it static);
	Cypher/decypher modes implementations (explain more):
		.ECB (operates on each block individually );
		.CBC (operates on a block, using CBC chaining mode );
		.OFB (operates on a block, using OFB chaining mode );

		OK as of 10/12/1998

		To do List:

		. Review these @¥¡#@$¤! method names;
		. Translate some of the stuff here to bare-bone assembly language;
		. Generate a high-level entry-point for the algorithm, ie,
				an interface on which the user just gives the buffer (const
				or stream), the key, and the block size, and the algorithm
				will encrypt/decrypt the buffer;
		. The algorithm must take care of padding and size control
				as necessary.
}
	
{
--------------------------------------------------------------------------------
-------------------- BlowFish Implementation Architecture ----------------------
--------------------------------------------------------------------------------
}

	{

	--------------------------------- TKCustomBlowFish ---------------------------

  PRIVATE
	-------

	»	Used to garantee that at the end of construction process the Initialize
		method will be called (test the Initialized property and raise an exception
		if it is false ).

		procedure ForceInitialization;

	PROTECTED
	---------

	» This function will be called into bfEncipher and will normalize the input
		buffer ( PlainText ) with any necessary padding to it. The return value is
		the number of blocks to encipher. ( The new size if Result * BF_BLOCK_SIZE ).
		The BOUT POINTER, passed through bfEncipher, must be allocated by this function.
		This newly allocated buffer will be used into bfEnchipher to process the BF
		algorithm and return back the cyphered block to THE USER which IS RESPONSIBLE
		TO FREE THIS BLOCK.

    function PaddingToCypher( const bIn; szIn: Integer; var bOut: Pointer ):
			Integer; virtual; abstract;

		Ex:
				procedure bfEncipher( const bIn; szIn: Integer; var bOut: Pointer; var szOut: Integer );
				begin
					(...)
					iBlocks := PaddingToCypher( bIn, szIn, bOut );

					(... Cypher the adjusted plaintext returned into bOut ...)

					szOut := iBlocks * BF_BLOCK_SIZE;
					(...)
				end;

	» This function will be called into bfDecipher and will normalize the output
		buffer ( PlainText ) of the deciphering algorithm. In ohter words, it will
		unpad the padding doing into PaddingToCypher before the Cypher mechanism.
		The return value is the number of bytes of the newly adjusted output buffer
		( PlainText ).
		The BOUT POINTER, passed through bfDecipher, must be allocated by this function.
		This newly allocated buffer will be used as the return value of bOut parameter
		of bfDecipher. THE USER IS RESPONSIBLE TO FREE THIS BLOCK.

		function UnpaddingDecipher( const bIn; szIn: Integer; var bOut: Pointer ):
			Integer; virtual; abstract;

    Ex:
				procedure bfDecipher( const bIn; szIn: Integer; var bOut: Pointer;
					var szOut: Integer );
				var
					pDeciphered: Pointer;
				begin
					(...)

					pDeciphered := (... DeCipher the cypheredtext passed into bIn ...);

					(szIn will have the size of the pDeciphered text that is the plaintext
					 plus the pad text. The return value is the original size of PlainText
					 allocated and filled into bOut.)

					szOut := UnpaddingDecipher( pDeciphered , szIn, bOut );

					(...)
				end;

	» Allocates the OutPut Buffer passed back to the user. This function can be
		called into PaddingToCypher and UnpaddingDecipher to allocate the output
		adjusted buffer.

    procedure AllocateOutPut( pIn: Pointer; szIn: Integer;
			var bOut: Pointer ); virtual;
			
		Ex:
				function PaddingToCypher( const bIn; szIn: Integer; var bOut: Pointer ):
					Integer;
				var
					pAdjust: Pointer;
				begin
					(...)

					(... Adjustt the input buffer bIn with the padding mechanism ... )

					AllocateOutPut( pAdjust, szIn, bOut );

					(... Return the appropriate result value ... )
				end;



	»	DeAllocates a previously allocated block by AllocateOutPut. Make sure that
		this pointer is allocated by AllocateOutput, other wise it will raise an
		exception. The return value is the size deallocated.

		function DeAllocateOutPut( var pOut: Pointer ): Integer;

	» Initialize the algorithm structures. Static to garantee this functionality.

		procedure Initialize;

	» This method will be called into the start of the bfDecipher method to
		ensure the size of the input cyphertext is correct ( that is a multiple of
		BF_BLOCK_SIZE - adjusted into PaddingToCypher ).
		If this check fails this function will raise an exception. It can be overrided
		into derived classes to perform any custom size checking.

		procedure ForceBufferInBlockSize( szIn: Integer ); dynamic;

	»	This method will be called into the start of bfDecipher and bfEncipher to
		ensure that the Input size was valid. The default check is to check if szIn
		is less or equal that BF_INVALID_BUFFER_SIZE (actually = 0).

		procedure ForceSizeIn( szIn: Integer ); dynamic;

	»	This method will encipher the plaintext passed into bIn (with size szIn) into
		the bOut pointer with the szOut size. The caller must call this function with
		a valid reference of bOut, but the allocation of bOut is made into PaddingToCypher.
		Checks for the parameters will be made at the start of method for bIn and
		after calling PaddingToCypher method to garantee the Output buffer was correctly
		allocated for enciphering.
		THE USER IS RESPONSIBLE TO FREE THIS BLOCK, otherwise we'll have a memory leak.

		procedure bfEncipher( const bIn; szIn: Integer; var bOut: Pointer;
			var szOut: Integer ); virtual;

	» This method will decipher the cyphertext passed into bIn (with size szIn) into
		the bOut pointer with the szOut size. The caller must call this function with
		a valid reference of bOut, but the allocation of bOut is made into UnpaddingDecipher.
		Checks for the parameters will be made at the start of method for bIn and
		szIn (multiple of BF_BLOCK_SIZE) and after calling UnpaddingDecipher method to garantee
		the Output buffer was correctly allocated for return the original input buffer unpadded.
		THE USER IS RESPONSIBLE TO FREE THIS BLOCK, otherwise we'll have a memory leak.

		procedure bfDecipher( const bIn; szIn: Integer; var bOut: Pointer;
			var szOut: Integer ); virtual;

	»	These functions are the read/write prototypes for the Char suite of the PadByte

		function GetPadAsChar: Char; virtual;
		procedure SetPadAsChar( Value: Char ); virtual;

	»	Allocate any necessary structures and initialize the algorithm.
		The pbfKey and szKey is the key and keysize, respectively of the algorithm.
		This key MUST HAVE a valid pointer reference with a key of size from 1 to
		56 bytes ( 448 bits - algorithm restriction ). The pad byte is just a reapid
		property setting parameter.
		Iterations parameter determine the number of iterations that the algorithm
		will perform, it can be from 8 to 24. Actualy the Iterations MUST BE 16,
		diferent values will discarted and 16 will be setted.

		constructor InternalCreate( pbfKey: Pointer; szKey: TKBlowFishKeySize;
			Iterations: TKBlowFishIterations; Pad: Byte ); virtual;

	»	PadByte can be freely changed. But remember that if you cypher with one
		padbyte YOU MUST decipher with the same padbyte (of course).
		
		property bfPad: Byte
						 read fbfPad write fbfPad;


	-------------------------------- TKPointerBlowFish ---------------------------

	TKBlockPadType = ( bptNone, bptNormal, bptBinary, bptCustom );

	bptNone   -> No pad/unpad used
	bptNormal -> Normal pad/unpad logic used (described below)
	bptBinary -> Binary pad/unpad logic used (described below)
	bptCustom -> Used pad/unpad defined logic (Event dispatcher)

	PRIVATE
	-------

	» The implementation of the pad mechanism is generic for every data (binary
		or not) that use the blowfish algorithm. The padding is made by placing the
		pad byte at the end of the block until the data size reaches the first
		block size multiple value. The unpad process do the same thing into a inverse
		order.

		The binary padding mechanism consists on a padding counting mechanism.
		The last byte of the data is the number of characters padded.

		PS: The worst situation is when the szIn is a block size multiple, in this
				case we'll put 7 foo bytes just to compute the 0 sized pad! :(

		PS: Note that we allways will put from 0 to 7 pad characters.

		function PaddingToCypher( const bIn; szIn: Integer; var bOut: Pointer ):
			Integer; override;
		function UnpaddingDecipher( const bIn; szIn: Integer; var bOut: Pointer ):
			Integer; override;

	--------------------------------- TKPCharBlowFish ----------------------------

	PUBLIC
	------

	»	Here i need the size because StrLen cannot compute the realy size if the
		cyphertext contains #0. So is the caller responsability to give the correct
		size. This size doesn't include the null terminator. The szOut return the
		the size of the result PChar without the null terminator for the same reason.

		function Encipher( pc: PChar; szIn: Integer; var szOut: Integer ): PChar; virtual;
		function Decipher( pc: PChar; szIn: Integer; var szOut: Integer ): PChar; virtual;

}

{--------------------------- Internal Implementation ---------------------------}

function F( pbfc: PKBlowFishControl; x: LongInt ): {$IFDEF DELPHI4}Cardinal{$ELSE}LongInt{$ENDIF};
var
	a, b, c, d: Word;
begin
	d := ( x and $00FF );
	x := ( x shr 8 );
	c := ( x and $00FF );
	x := ( x shr 8 );
	b := ( x and $00FF );
	x := ( x shr 8 );
	a := ( x and $00FF );
	Result := pbfc^.SBox[0][a] + pbfc^.SBox[1][b];
	Result := Result xor pbfc^.SBox[2][c];
	Result := Result + pbfc^.SBox[3][d];
end;

procedure Encipher( pbfc: PKBlowFishControl; xl, xr: Pointer;
	Iterations: TKBlowFishIterations );
var
	i: Byte;
	iXL, iXR, iTMP: {$IFDEF DELPHI4}Cardinal{$ELSE}LongInt{$ENDIF};
begin
	iXL := {$IFDEF DELPHI4}Cardinal( xl^ ){$ELSE}LongInt( xl^ ){$ENDIF};
	iXR := {$IFDEF DELPHI4}Cardinal( xr^ ){$ELSE}LongInt( xr^ ){$ENDIF};
	for i := 0 to Iterations - 1 do
	begin
		iXL := ( iXL xor pbfc^.PArray[i] );
		iXR := ( F( pbfc, iXL ) xor iXR );
		iTMP := iXL;
		iXL := iXR;
		iXR := iTMP;
	end;
	iTMP := iXL;
	iXL := iXR;
	iXR := iTMP;
	iXR := ( iXR xor pbfc^.PArray[Iterations] );
	iXL := ( iXL xor pbfc^.PArray[Iterations+1] );
	{$IFDEF DELPHI4}Cardinal( xl^ ){$ELSE}LongInt( xl^ ){$ENDIF} := iXL;
	{$IFDEF DELPHI4}Cardinal( xr^ ){$ELSE}LongInt( xr^ ){$ENDIF} := iXR;
end;

procedure Decipher( pbfc: PKBlowFishControl; xl, xr: Pointer;
	Iterations: TKBlowFishIterations );
var
	i: Byte;
	iXL, iXR, iTMP: {$IFDEF DELPHI4}Cardinal{$ELSE}LongInt{$ENDIF};
begin
	iXL := {$IFDEF DELPHI4}Cardinal( xl^ ){$ELSE}LongInt( xl^ ){$ENDIF};
	iXR := {$IFDEF DELPHI4}Cardinal( xr^ ){$ELSE}LongInt( xr^ ){$ENDIF};
	for i := Iterations + 1 downto 2 do
	begin
		iXL := ( iXL xor pbfc^.PArray[i] );
		iXR := ( F( pbfc, iXL ) xor iXR );
		iTMP := iXL;
		iXL := iXR;
		iXR := iTMP;
	end;
	iTMP := iXL;
	iXL := iXR;
	iXR := iTMP;
	iXR := ( iXR xor pbfc^.PArray[1] );
	iXL := ( iXL xor pbfc^.PArray[0] );
	{$IFDEF DELPHI4}Cardinal( xl^ ){$ELSE}LongInt( xl^ ){$ENDIF} := iXL;
	{$IFDEF DELPHI4}Cardinal( xr^ ){$ELSE}LongInt( xr^ ){$ENDIF} := iXR;
end;

procedure BlowFishInitialize( pbfc: PKBlowFishControl; pKey: Pointer;
	szKeySize: Integer; Iterations: TKBlowFishIterations );
var
	i, j, k: Byte;
	data, dataL, dataR: {$IFDEF DELPHI4}Cardinal{$ELSE}LongInt{$ENDIF};
begin
{ Fill pbfc S-Boxes with the precomputed S-Boxes }
	for i := 0 to 255 do
		with pbfc^ do
		begin
			SBox[0][i] := SBox_0[i];
			SBox[1][i] := SBox_1[i];
			SBox[2][i] := SBox_2[i];
			SBox[3][i] := SBox_3[i];
		end;
{ Fill pbfc P-Boxes }
	j := 0;
	for i := 0 to Iterations + 1 do
	begin
		data := $00000000;
		for k := 0 to 3 do {* for ( k = 0; k < 4; ++k ) }
		begin
			data := ( ( data shl 8 ) or PByte( Integer( pKey ) + j )^ );
			inc( j );
			if ( j >= szKeySize ) then
				j := 0;
		end;
		pbfc^.PArray[i] := ( pbfc^.PArray[i] xor data );
	end;
{ Rearrange pbfc P-Boxes }
	dataL := $00000000;
	dataR := $00000000;
	for i := 0 to ( ( Iterations + 1 ) div 2 ) do
	begin
		Encipher( pbfc, @dataL, @dataR, Iterations );
		pbfc^.PArray[(2 * i)] := dataL;
		pbfc^.PArray[(2 * i + 1)] := dataR;
	end;
{ Rearrange pbfc S-Boxes }
	for i := 0 to 3 do {* for ( i = 0; i < 4; ++i ) }
	begin
		for j := 0 to ( 255 div 2 ) do
		begin
			Encipher( pbfc, @dataL, @dataR, Iterations );
			pbfc^.SBox[i][( 2 * j )] := dataL;
			pbfc^.SBox[i][( 2 * j ) + 1] := dataR;
		end;
	end;
end;

procedure BlowFishEncipher( pbfc: PKBlowFishControl; data: Pointer; Blocks: LongInt;
	Iterations: TKBlowFishIterations );
var
	d: Pointer;
	i: LongInt;
begin
	d := data;
	for i := 0 to Blocks - 1 do
	begin
		Encipher( pbfc, d, Ptr( LongInt( d ) + SizeOf( LongInt ) ), Iterations );
		d := Ptr( LongInt( d ) + 2 * SizeOf( LongInt ) );
	end;
end;

procedure BlowFishDecipher( pbfc: PKBlowFishControl; data: Pointer; Blocks: LongInt;
 Iterations: TKBlowFishIterations );
var
	d: Pointer;
	i: LongInt;
begin
	d := data;
	for i := 0 to Blocks - 1 do
	begin
		Decipher( pbfc, d, Ptr( LongInt( d ) + SizeOf( LongInt ) ), Iterations );
		d := Ptr( LongInt( d ) + 2 * SizeOf( LongInt ) );
	end;
end;

{---------------------------- Public Implementation ----------------------------}

{ TKCustomBlowFish }

constructor TKCustomBlowFish.InternalCreate( pbfKey: Pointer; szKey: TKBlowFishKeySize;
	Iterations: TKBlowFishIterations; Pad: Byte );
begin
	ForcePointer( pbfKey );
	inherited Create;
	fbfszKey := szKey;
	fbfPad := Pad;
{ Fixed until now... }
	fbfInterations := BF_DEF_ITERATIONS;
	fpbfKey := AllocMem( fbfszKey );
	Move( pbfKey^, fpbfKey^, fbfszKey );
	fpbfControl := nil;
	fbfInitialized := False;
	Initialize;
end;

destructor TKCustomBlowFish.Destroy;
begin
	FreeMem( fpbfKey, fbfszKey );
	fpbfKey := nil;
	if CheckPointer( fpbfControl ) then
		Dispose( fpbfControl );
	fpbfControl := nil;
	fbfInitialized := False;
	inherited Destroy;
end;

procedure TKCustomBlowFish.Initialize;
begin
	fpbfControl := New( PKBlowFishControl );
	uksyUtils.ZeroMemory( fpbfControl, SizeOf( TKBlowFishControl ) );
	BlowFishInitialize( fpbfControl, fpbfKey, fbfszKey, fbfInterations );
	fbfInitialized := True;
end;

procedure TKCustomBlowFish.ForceInitialization;
begin
	if ( not ( ( fbfInitialized ) and ( CheckPointer( fpbfControl ) ) ) ) then
		uksyUtils.RaiseException( EKCYBlowFish, sErrBFNotInitialized );
end;

procedure TKCustomBlowFish.ForceSizeIn( szIn: Integer );
begin
	if ( szIn <= BF_INVALID_BUFFER_SIZE ) then
		uksyUtils.RaiseExceptionFmt( EKCYBlowFish, sErrBFInvInputSize, [szIn] );
end;

procedure TKCustomBlowFish.bfEncipher( const bIn; szIn: Integer; var bOut: Pointer;
	var szOut: Integer );
var
	iBlocks: Integer;
begin
	szOut := BF_INVALID_BUFFER_SIZE;
	ForceSizeIn( szIn );
	ForceInitialization;
	ForceReference( bIn );
	iBlocks := PaddingToCypher( bIn, szIn, bOut );
	ForcePointer( bOut );
	BlowFishEncipher( fpbfControl, bOut, iBlocks, fbfInterations );
	szOut := ( iBlocks * BF_BLOCK_SIZE );
end;

procedure TKCustomBlowFish.ForceBufferInBlockSize( szIn: Integer );
begin
	if ( ( szIn mod BF_BLOCK_SIZE ) <> 0 ) then
		uksyUtils.RaiseExceptionFmt( EKCYBlowFish, sErrSBFInvDecipher_szIn, [szIn, BF_BLOCK_SIZE] );
end;

procedure TKCustomBlowFish.bfDecipher( const bIn; szIn: Integer; var bOut: Pointer;
	var szOut: Integer );
var
{
	The iSize variable will return in szOut the size in bytes of the Normalized
 ( unpadded ) plaintext buffer. But will be used as number of blocks to decipher
	based on the size of the cyphertext
}
	iSize: Integer;
	pIn: Pointer;
begin
	szOut := BF_INVALID_BUFFER_SIZE;
	ForceSizeIn( szIn );
	ForceInitialization;
	ForceReference( bIn );
	ForceBufferInBlockSize( szIn );
	iSize := ( szIn div BF_BLOCK_SIZE );
	pIn := AllocMem( szIn );
	try
		Move( bIn, pIn^, szIn );
		BlowFishDecipher( fpbfControl, pIn, iSize, fbfInterations );
		iSize := UnpaddingDecipher( pIn^, szIn, bOut );
	finally
		FreeMem( pIn, szIn );
	end;
	ForcePointer( bOut );
	szOut := iSize;
end;

procedure TKCustomBlowFish.AllocateOutPut( pIn: Pointer; szIn: Integer;
	var bOut: Pointer );
begin
	ForceReference( pIn );
	ForceSizeIn( szIn );
	bOut := AllocMem( szIn + SizeOf( Cardinal ) );
	try
		Cardinal( Pointer( bOut )^ ) := szIn + SizeOf( Cardinal );
		Inc( PByte( bOut ), SizeOf( Cardinal ) );
		Move( pIn^, bOut^, szIn );
	except
		FreeMem( bOut, szIn + SizeOf( Cardinal ) );
		raise;
	end;
end;

function TKCustomBlowFish.DeAllocateOutPut( var pOut: Pointer ): Integer;
begin
	ForceReference( pOut );
	Dec( PByte( pOut ), SizeOf( Cardinal ) );
	Result := Cardinal(Pointer( pOut )^) - SizeOf( Cardinal );
	if ( Result > BF_INVALID_BUFFER_SIZE ) then
	begin
		FreeMem( pOut, Result );
		pOut := nil;
	end
	else
		uksyUtils.RaiseExceptionFmt( EKCYBlowFish, sErrBFInvDeAlloc, [Result] );
	if CheckPointer( pOut ) then
		uksyUtils.RaiseExceptionFmt( EKCYBlowFish, sErrBFDeAllocError, [0] );
end;

function TKCustomBlowFish.GetPadAsChar: Char;
begin
	Result := Char( fbfPad );
end;

procedure TKCustomBlowFish.SetPadAsChar( Value: Char );
begin
	fbfPad := Byte( Value );
end;

{ TKPointerBlowFish }

constructor TKPointerBlowFish.Create( Key: Pointer; KeySize: TKBlowFishKeySize;
	ABlockPadType: TKBlockPadType );
begin
	InternalCreate( Key, KeySize, BF_DEF_ITERATIONS, BF_DEF_PAD_BYTE );
	fUnPadCount := 0;
	fBlockPadType := ABlockPadType;
end;

procedure TKPointerBlowFish.CheckValidBlockPadType;
begin
	if ( ( fBlockPadType = bptNone ) or ( ( fBlockPadType = bptCustom ) and
		( not Assigned( FOnCustomPad ) ) ) ) then
		uksyUtils.RaiseExceptionFmt( EKCYBlowFish, sErrPBFInvPadType,
			[EnumName( Byte( fBlockPadType ), TypeInfo( TKBlockPadType ) )] );
end;

function TKPointerBlowFish.PaddingToCypher( const bIn; szIn: Integer;
	var bOut: Pointer ): Integer;
var
	i,
	j,
	k: Byte;
	P: PByte;
	bFree: Boolean;
begin
{ Check the valid pad/unpad block types }
	CheckValidBlockPadType;

{ j determines if there any pad to perform. }
	j := ( BF_BLOCK_SIZE - ( szIn mod BF_BLOCK_SIZE ) );

	bFree := False; { Should I free the P pointer ? }

	P := nil;
	try
	{ There is any pad to do if, and only if, ( szIn mod BF_BLOCK_SIZE ) <> 0 }
		if ( j <> BF_BLOCK_SIZE ) then
		begin
			k := 0;
			Inc ( szIn, j );
			P := AllocMem( szIn );
			bFree := True;              { We MUST Free the p pointer }
			Move( bIn, P^, szIn - j );  { Move the contents of the input buffer with
																		the original size. }
			case fBlockPadType of
				bptNormal : k := j;       { Depend of the block type, leave room for size }
				bptBinary : k := ( j - 1 );
				bptCustom : FOnCustomPad( Self, P, szIn, j, True );
			end;
			if ( fBlockPadType <> bptCustom ) then
			begin
				Inc( P, szIn - j );         { Manualy put P to point to the last+1 byte of
																			the input buffer. }

				for i := 0 to k - 1 do      { Iterates the number of times to pad }
				begin
					PByte( P )^ := bfPad;     { Put the pad character at the end }
					Inc( P, SizeOf( Byte ) ); { Manualy put P to point to the end }
				end;

				case fBlockPadType of
					bptNormal : ;               { For Normal padding until here the work is done }
					bptBinary :
					begin
						PByte( P )^ := Byte( k ); { For binary padding compute the size in the
																				last byte }
						Inc( P, SizeOf( Byte ) ); { Manualy put P to point to the end }
					end;
				end;

				Dec( P, szIn );             { Manualy restore the original location of P.
																			Now at the start of the original buffer but
																			with necessary pad. }
			end;
		end
		else
	{ Otherwise [ ( szIn mod BF_BLOCK_SIZE ) = 0 ], so, if BlockPadType is bptNormal,
		no pad is needed, so reference the input buffer into P and set the flag to not free
		the memory pointed to P (already set, ok!).
		Otherwise, the worst (memory) pad is done. }
			case fBlockPadType of
				bptNormal: P := PByte( @bIn );
				bptBinary:
				begin
					Inc ( szIn, BF_BLOCK_SIZE );
					P := AllocMem( szIn );
					bFree := True;
					Move( bIn, P^, szIn-BF_BLOCK_SIZE ); { Move the contents of the input buffer with
																								 the original size. }
					Inc( P, szIn-BF_BLOCK_SIZE );        { Manualy put P to point to the last+1 byte of
																								the input buffer. }
					k := ( BF_BLOCK_SIZE - 1 ); { Times to pad }

					for i := 0 to k - 1 do      { Iterates the number of times to pad }
					begin
						PByte( P )^ := bfPad;     { Put the pad character at the end }
						Inc( P, SizeOf( Byte ) ); { Manualy put P to point to the end }
					end;

					PByte( P )^ := k ;          { For binary padding compute the size in the
																				last byte }

					Dec( P, szIn - SizeOf( k ) ); { Manualy restore the original location of P.
																					Now at the start of the original buffer but
														   				  	with necessary pad less the pad size space. }
				end;

				bptCustom: FOnCustomPad( Self, P, szIn, j, True );

			end;

	{ Delegates the Allocation mechanism }
		AllocateOutPut( P, szIn, bOut );

	finally
		if CheckPointer( P ) and ( bFree ) then { Just free P if it's previos allocated and }
			FreeMem( P, szIn );										{	any pad operation occours into it }
	end;

{ Return the number of blocks to encipher. }
	Result := ( szIn div BF_BLOCK_SIZE );
end;

function TKPointerBlowFish.UnpaddingDecipher( const bIn; szIn: Integer;
	var bOut: Pointer ): Integer;
const
	BF_NORMAL_PAD = High( Byte );
var
	j,
	k: Byte;
	P: Pointer;
begin
{ Check the valid pad/unpad block types }
	CheckValidBlockPadType;

	P := nil;
	try
		P := AllocMem( szIn );
		Move( bIn, P^, szIn );    { Move the contents of the input buffer with
																the original size. }

		if ( fBlockPadType <> bptCustom ) then
		begin
			Inc( PByte( P ), szIn - 1 ); { Manualy put P to point to the last (less one)
																	   byte of the input buffer. }

			case fBlockPadType of
				bptBinary :							{ Depend of the block type, get pad size and adjust P }
				begin
					k := PByte( P )^;                  { Get PadCount }
					Dec( PByte( P ), SizeOf( Byte ) ); { Move to the Last pad char }
				end
				else
					k := BF_NORMAL_PAD;   { Otherwise is Normal pad }
			end;
			if ( k = BF_NORMAL_PAD ) then { Compute the unpad manualy. }
			begin
				{ The unpadding logic is simple. The max number of pad bytes that we have
					putted is ( BF_BLOCK_SIZE - 1 ), so loop the end of the buffer from beggining
					until reach a different byte or reach ( BF_BLOCK_SIZE - 1 ). }
				j := 0;
				while ( j < BF_BLOCK_SIZE ) and ( PByte( P )^ = bfPad ) do
				begin
					Dec( PByte( P ), SizeOf( Byte ) );   {Manualy put P to point to the end-1
																								and remove the pad character at the end}
					Inc( j );               						 {compute new size to adjust}
				end;
				Dec( PByte( P ), szIn-(j+1) ); {Ajust p to point to the begining of the original buffer }
				Dec( szIn, j );                  {Adjust the size to the original size }
			end
			else
			begin
				j := ( k + 1 );
				{ Ajust p to point to the begining of the original buffer. The SizOf(K) is
					already computed above and the -1 is because the zero based pointer indexing }
				Dec( PByte( P ), szIn - SizeOf( k ) - 1 );
				Dec( szIn, j );            { Adjust the size to the original size remember
																		 to leave one to the pad size! }
				Dec( j, SizeOf( k ) );     { ReAdjust the real number of padding chars.}
			end;
			if ( j <> 0 ) then
			begin
				ReAllocMem( P, szIn );    {Adjust the memory used if appliable}
				fUnPadCount := j;
			end
			else
				fUnPadCount := 0;
		end
		else
		begin
		  FOnCustomPad( Self, P, szIn, j, False );
			fUnPadCount := j;
		end;

	{ Delegates the Allocation mechanism }
		AllocateOutPut( P, szIn, bOut );

	finally
		if CheckPointer( P ) then
  		FreeMem( P, szIn );
	end;
{ Return the number of bytes of the output buffer. }
	Result := szIn;
end;

function TKPointerBlowFish.ptEncipher( pIn: Pointer; szIn: Integer;
	var szOut: Integer ): Pointer;
begin
	Result := nil;
	szOut := 0;
	bfEncipher( pIn^, szIn, Result, szOut );
end;

function TKPointerBlowFish.ptDecipher( pIn: Pointer; szIn: Integer;
	var szOut: Integer ): Pointer;
begin
	Result := nil;
	szOut := 0;
	bfDecipher( pIn^, szIn, Result, szOut );
end;

{ TKStringBlowFish }

constructor TKStringBlowFish.Create( const sKey: string;
	ABlockPadType: TKBlockPadType );
var
	b: TKBlowFishKeySize;
begin
	ForceStringKey( sKey );
	b := TKBlowFishKeySize( Length( sKey ) );
	inherited Create( Pointer( sKey ), b, ABlockPadType );
end;

function TKStringBlowFish.GetStrKey: string;
begin
	Result := PChar( fpbfKey );
end;

class procedure TKStringBlowFish.ForceStringKey( const sKey: string );
begin
	if ( not CheckTrimStr( sKey ) ) then
		uksyUtils.RaiseException( EKCYBlowFish, sErrSBFInvNullKey );
	if ( ( Length( sKey ){+1} ) > BF_MAX_KEY_BLOCK_BYTES ) then
		uksyUtils.RaiseExceptionFmt( EKCYBlowFish, sErrSBFKeyTooLong, [BF_MAX_KEY_BLOCK_BYTES] );
	if ( ( Length( sKey ){+1} ) < BF_MIN_KEY_STRING_LEN ) then
		uksyUtils.RaiseExceptionFmt( EKCYBlowFish, sErrSBFKeyTooSmall, [BF_MIN_KEY_STRING_LEN] );
end;

class procedure TKStringBlowFish.ForceStringData( const S: string );
begin
	if ( not CheckTrimStr( s ) ) then
		uksyUtils.RaiseException( EKCYBlowFish, sErrSBFInvNullData );
end;

function TKStringBlowFish.Encipher( const S: string ): string;
var
	pOut: Pointer;
	iRes: Integer;
begin
	ForceStringData( S );
	pOut := nil;
	try
		pOut := ptEncipher( Pointer( S ), Length( S ), iRes );
		SetString( Result, PChar( pOut ), iRes );
	finally
		if CheckPointer( pOut ) then
			DeAllocateOutPut( pOut );
	end;
end;

function TKStringBlowFish.Decipher( const S: string ): string;
var
	pOut: Pointer;
	iRes: Integer;
begin
	ForceStringData( S );
	pOut := nil;
	try
		pOut := ptDecipher( Pointer( S ), Length( S ), iRes );
		SetString( Result, PChar( pOut ), iRes );
	finally
		if CheckPointer( pOut ) then
			DeAllocateOutPut( pOut );
	end;
end;

{ TKPCharBlowFish }

constructor TKPCharBlowFish.Create( sKey: PChar; szIn: Integer;
	ABlockPadType: TKBlockPadType );
var
	b: TKBlowFishKeySize;
begin
	ForcePCharKey( sKey );
	b := TKBlowFishKeySize( StrLen( sKey ) );
	inherited Create( sKey, b, ABlockPadType );
end;

procedure TKPCharBlowFish.AllocateOutPut( pIn: Pointer; szIn: Integer;
	var bOut: Pointer );
begin
	inherited AllocateOutPut( pIn, szIn, bOut );
	Exit;
	bOut := nil;
	PChar( bOut ) := StrAlloc( szIn + 1 );
	try
		uksyUtils.ZeroMemory( PChar( bOut ), szIn + 1 );
		Move( pIn^, PChar( bOut )^, szIn );
	except
		StrDispose( bOut );
		raise;
	end;
end;

function TKPCharBlowFish.GetPCharKey: PChar;
begin
	Result := PChar( fpbfKey );
end;

class procedure TKPCharBlowFish.ForcePCharKey( sKey: PChar );
begin
	if ( not CheckPChar( sKey ) ) then
		uksyUtils.RaiseException( EKCYBlowFish, sErrSBFInvNullKey );
	if ( ( StrLen( sKey ){+1} ) > BF_MAX_KEY_BLOCK_BYTES ) then
		uksyUtils.RaiseExceptionFmt( EKCYBlowFish, sErrSBFKeyTooLong, [BF_MAX_KEY_BLOCK_BYTES] );
	if ( ( StrLen( sKey ){+1} ) < BF_MIN_KEY_PChar_LEN ) then
		uksyUtils.RaiseExceptionFmt( EKCYBlowFish, sErrSBFKeyTooSmall, [BF_MIN_KEY_PChar_LEN] );
end;

class procedure TKPCharBlowFish.ForcePCharData( pc: PChar );
begin
	if ( not CheckPChar( pc ) ) then
		uksyUtils.RaiseException( EKCYBlowFish, sErrSBFInvNullData );
end;

function TKPCharBlowFish.Encipher( pc: PChar; szIn: Integer;
  var szOut: Integer ): PChar;
var
	pOut: Pointer;
begin
  ForcePCharData( pc );
	pOut := nil;
	try
		pOut := ptEncipher( pc, szIn, szOut );
		Result := StrAlloc( szOut + 1 );
		try
			uksyUtils.ZeroMemory( Result, szOut + 1 );
			Move( pOut^, Result^, szOut );
		except
			StrDispose( Result );
			raise;
		end;
	finally
		if CheckPointer( pOut ) then
			szIn := DeAllocateOutPut( pOut );
	end;
	if ( szIn <> szOut ) then
		uksyUtils.RaiseExceptionFmt( EKCYBlowFish, sErrPCBFInvDeAlloc, [szOut, szIn] );
end;

function TKPCharBlowFish.Decipher( pc: PChar; szIn: Integer;
	var szOut: Integer ): PChar;
var
	pOut: Pointer;
begin
  ForcePCharData( pc );
	pOut := nil;
	try
		pOut := ptDecipher( pc, szIn, szOut );
		Result := StrAlloc( szOut + 1 );
		try
			uksyUtils.ZeroMemory( Result, szOut + 1 );
			Move( pOut^, Result^, szOut );
		except
			StrDispose( Result );
			raise;
		end;
	finally
		if CheckPointer( pOut ) then
			szIn := DeAllocateOutPut( pOut );
	end;
	if ( szIn <> szOut ) then
		uksyUtils.RaiseExceptionFmt( EKCYBlowFish, sErrPCBFInvDeAlloc, [szOut, szIn] );
end;

end.