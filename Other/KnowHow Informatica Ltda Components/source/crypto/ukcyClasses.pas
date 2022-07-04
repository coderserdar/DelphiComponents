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

unit ukcyClasses;

{$I s:\v100\include\iKLIB100.inc}

interface

uses
	Classes, uksyConsts, uksyUtils, ukcyBlowFish;

type

  EKCYClasses = class( EKCrypto );

	EKStringBlowFish = class( EKCYClasses );

  { redeclared to avoid unit usage - ukcyBlowFish! }
	TKBlockPadType = ( bptNone, bptNormal, bptBinary, bptCustom );

{ TKStringBlowFishCypher }

  TKStringBlowFishCypher = class;

	TKStringPadEvent = procedure( Sender: TKStringBlowFishCypher; Data: Pointer;
		DataSize: Integer; var PadCount: Byte; Padding: Boolean ) of object;

	TKStringBlowFishCypher = class( TComponent )
	private
		FPadChar: Char;
		FKey: string;
		FPlainText: string;
		FCypherText: string;
		FBlockPadType: TKBlockPadType;
		FStringBlowFish: TKStringBlowFish;
		FOnEncipher: TNotifyEvent;
		FOnDecipher: TNotifyEvent;
		FOnCustomPad: TKStringPadEvent;

		procedure SetKey( const Value: string );
		procedure SetCypherText( const Value: string );
		procedure SetPlainText( const Value: string );
		procedure SetBlockPadType( Value: TKBlockPadType );

		procedure ForceValues( Cyphering: Boolean );

	protected
		procedure DoEncipher; dynamic;
		procedure DoDecipher; dynamic;
		procedure CustomPad( Sender: TKPointerBlowFish; Data: Pointer;
			DataSize: Integer; var PadCount: Byte; Padding: Boolean ); dynamic;

	public
		destructor Destroy; override;
		constructor Create( AOwner: TComponent ); override;

  	procedure ClearValues;
		procedure Encipher; virtual;
		procedure Decipher; virtual;
    class function PadLength( PlainTextLength: Integer ): Integer; virtual;
    class function PadStrLength( const PlainText: string ): Integer; virtual;

		property PadChar: Char
						 read FPadChar write FPadChar default CH_SPACE;

	published
		property Key: string
						 read FKey write SetKey;
		property PlainText: string
						 read FPlainText write SetPlainText;
		property CypherText: string
						 read FCypherText write SetCypherText;
		property BlockPadType: TKBlockPadType
						 read FBlockPadType write SetBlockPadType default bptNormal;
		property OnCustomPad: TKStringPadEvent
						 read FOnCustomPad write FOnCustomPad;
		property OnEncipher: TNotifyEvent
						 read FOnEncipher write FOnEncipher;
		property OnDecipher: TNotifyEvent
						 read FOnDecipher write FOnDecipher;

	end;

implementation

uses
	ukcyConsts, ukcyResStr, ukcyUtils;

{ TKStringBlowFishCypher }

{
	Custom Padding
	--------------

	The user is responsable to pad the Data pointer with the PadCount value.
	The data pointer was correctly allocated with szIn size.
	The Padding parameter indicates if a Padding/UnPadding was taking place.

	Custom UnPadding
	----------------

	The user is responsable to unppad the Data pointer with their custom algorithm.
	The data pointer was correctly allocated with szIn size.
	IS THE USER RESPONSABILITY to REALLOCATE any necessary memory for Data pointer
	( that is the remain bytes unppaded ). The number of bytes unpadded by the custom
	algorithm MUST be passed within the PadCount var parameter. This will be the
	UpPadCount property and will be used after for deallocating memory.

	THE USER MUST GIVE A CORRECT PADCOUNT VALUE AND MUST REALLOCATE THE MEMORY FOR
	DATA POINTER. SO USE THIS EVENT VERY CAREFULLY

	procedure CustomPad( Sender: TKPointerBlowFish; Data: Pointer;
		szIn: Integer; var PadCount: Byte ); dynamic;
}

constructor TKStringBlowFishCypher.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	FBlockPadType := bptNormal;
	FPadChar := CH_SPACE;
	FStringBlowFish := nil;
end;

destructor TKStringBlowFishCypher.Destroy;
begin
	FreeClean( FStringBlowFish );
	inherited Destroy;
end;

procedure TKStringBlowFishCypher.CustomPad( Sender: TKPointerBlowFish; Data: Pointer;
	DataSize: Integer; var PadCount: Byte; Padding: Boolean );
begin
	ForceReference( FOnCustomPad );
	FOnCustomPad( Self, Data, DataSize, PadCount, Padding );
end;

procedure TKStringBlowFishCypher.DoEncipher;
begin
	if Assigned( FOnEncipher ) then
		FOnEncipher( Self );
end;

procedure TKStringBlowFishCypher.DoDecipher;
begin
	if Assigned( FOnDecipher ) then
		FOnDecipher( Self );
end;

procedure TKStringBlowFishCypher.SetKey( const Value: string );
begin
	if ( not CheckStrEqual( Value, FKey ) ) then
	begin
		if CheckObject( FStringBlowFish ) then
			FreeClean( FStringBlowFish );
		FKey := Value;
{ If the key isn't null, adjust its size to the minimum or maximum size. Dependent
	of the key length. Create the FStringBlowFish object and resync the CustomPad event }
		if CheckStr( FKey ) then
		begin
			if ( Length( FKey ) < BF_MIN_KEY_STRING_LEN ) then
				FKey := StrLeftPad( FKey, BF_MIN_KEY_STRING_LEN, FPadChar )
			else if ( Length( FKey ) > BF_MAX_KEY_BLOCK_BYTES ) then
				Delete( FKey, BF_MAX_KEY_BLOCK_BYTES + 1, Length( FKey ) - BF_MAX_KEY_BLOCK_BYTES );
			FStringBlowFish := TKStringBlowFish.Create( FKey, ukcyBlowFish.TKBlockPadType( BlockPadType ) );
			if ( BlockPadType = bptCustom ) then
			begin
				if Assigned( FOnCustomPad ) then
					FStringBlowFish.OnCustomPad := CustomPad
				else
					RaiseException( EKStringBlowFish, sErrInvStrBFBlockPadType );
			end;
		end;
	end;
end;

procedure TKStringBlowFishCypher.SetCypherText( const Value: string );
begin
	if ( not CheckStrEqual( Value, FCypherText ) ) then
		FCypherText := Value;
end;

procedure TKStringBlowFishCypher.SetPlainText( const Value: string );
begin
	if ( not CheckStrEqual( Value, FPlainText ) ) then
		FPlainText := Value;
end;

procedure TKStringBlowFishCypher.SetBlockPadType( Value: TKBlockPadType );
begin
	if ( Value <> FBlockPadType ) and ( Value <> bptNone ) then
	begin
		FBlockPadType := Value;
{ if there is any StringBlowFish object created and the BlockPadType isn't
	custom, set the event to nil. Ohterwise, if it's custom pad, an event handler
	must be set. }
		if CheckObject( FStringBlowFish ) then
			if ( FBlockPadType <> bptCustom ) then
				FStringBlowFish.OnCustomPad := nil
			else if ( not Assigned( FOnCustomPad ) ) then
				RaiseException( EKStringBlowFish, sErrInvStrBFBlockPadType );
	end;
end;

procedure TKStringBlowFishCypher.ForceValues( Cyphering: Boolean );
begin
	if ( ( not ( CheckStr( FKey ) and CheckObject( FStringBlowFish ) ) ) or
			 ( Cyphering and ( not CheckTrimStr( FPlainText ) ) ) or
			 ( not ( Cyphering or CheckTrimStr( FCypherText ) ) ) ) then
		RaiseException( EKStringBlowFish, sErrInvStrBFValues );
end;

procedure TKStringBlowFishCypher.ClearValues;
begin
  Key := ''; { destroy the undeling object }
	FPlainText := '';
	FCypherText := '';
end;

procedure TKStringBlowFishCypher.Encipher;
begin
	ForceValues( True );
	FCypherText := FStringBlowFish.Encipher( FPlainText );
	DoEncipher;
end;

procedure TKStringBlowFishCypher.Decipher;
begin
	ForceValues( False );
	FPlainText := FStringBlowFish.Decipher( FCypherText );
	DoDecipher;
end;

class function TKStringBlowFishCypher.PadStrLength( const PlainText: string ): Integer;
begin
  Result := PadLength( Length( PlainText ) );
end;

class function TKStringBlowFishCypher.PadLength( PlainTextLength: Integer ): Integer;
begin
  Result := ( BF_BLOCK_SIZE - ( PlainTextLength mod BF_BLOCK_SIZE ) );
  if ( Result = BF_BLOCK_SIZE ) then
    Result := 0;
end;

end.
