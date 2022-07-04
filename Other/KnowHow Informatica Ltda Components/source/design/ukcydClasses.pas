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

unit ukcydClasses;

{$I s:\v100\include\iKLIB100.inc}

interface

uses
  DsgnIntf, uksydClasses;

type

{ TKStringBlowFishEditor }

	TKStringBlowFishEditor = class( TDefaultEditor )
	public
		procedure ExecuteVerb( Index: Integer ); override;
		function GetVerb( Index: Integer ): string; override;
		function GetVerbCount: Integer; override;

	end;

{ TKStringBlowFishKeyProperty }

	TKStringBlowFishKeyProperty = class( TStringProperty )
	public
		function GetEditLimit: Integer; override;

	end;

{ TKStringBlowFishBlockPadTypeProperty }

	TKStringBlowFishBlockPadTypeProperty = class( TEnumProperty )
	public
		procedure SetValue( const Value: string ); override;

	end;

{ TKStringBlowFishCustomPadProperty }

	TKStringBlowFishCustomPadProperty = class( TKMethodProperty )
	public
		procedure SetValue( const Value: string ); override;
		
	end;

implementation

uses
	SysUtils, TypInfo, uksyUtils, ukcyConsts, ukcyUtils, ukcyClasses, ukcydConsts;

{ TKStringBlowFishEditor }

procedure TKStringBlowFishEditor.ExecuteVerb( Index: Integer );
begin
	case Index of
		0: ( Component as TKStringBlowFishCypher ).Encipher;
		1: ( Component as TKStringBlowFishCypher ).Decipher;
	end;
	Designer.Modified;
end;

function TKStringBlowFishEditor.GetVerb( Index: Integer ): string;
begin
	case Index of
		0: Result := sSBFEncipher;
		1: Result := sSBFDecipher;
	else
	  Result := '';	
	end;
end;

function TKStringBlowFishEditor.GetVerbCount: Integer;
const
	STRBF_COUNT: array[Boolean] of Byte = ( 0, STRING_BLOWFISH_VERBCOUNT );
begin
	Result := STRBF_COUNT[( ( Component as TKStringBlowFishCypher ).BlockPadType <> bptCustom )];
end;

{ TKStringBlowFishKeyProperty }

function TKStringBlowFishKeyProperty.GetEditLimit: Integer;
begin
  Result := BF_MAX_KEY_BLOCK_BYTES;
end;

{ TKStringBlowFishBlockPadTypeProperty }

procedure TKStringBlowFishBlockPadTypeProperty.SetValue( const Value: string );
var
	bpt: TKBlockPadType;
	ppi: PPropInfo;
	mt: TMethod;
begin
	bpt := TKBlockPadType( GetEnumValue( GetPropType, Value ) );
	if ( bpt = bptCustom ) and ( not Assigned( ( GetComponent( 0 ) as TKStringBlowFishCypher ).OnCustomPad ) ) then
	begin
		ppi := TypInfo.GetPropInfo( TKStringBlowFishCypher.ClassInfo, 'OnCustomPad' );
		if CheckPointer( ppi ) then
		begin
			mt := Designer.CreateMethod( ( GetComponent( 0 ) as TKStringBlowFishCypher ).Name +
				'CustomPad', GetTypeData( TypeInfo( TKStringPadEvent ) ) );
			TypInfo.SetMethodProp( ( GetComponent( 0 ) as TKStringBlowFishCypher ), ppi, mt );
			Designer.Modified;
		end;
	end;
	inherited SetValue( Value );
end;

{ TKStringBlowFishCustomPadProperty }

procedure TKStringBlowFishCustomPadProperty.SetValue( const Value: string );
begin
	if ( ( ( GetComponent( 0 ) as TKStringBlowFishCypher ).BlockPadType = bptCustom ) and
			 ( not CheckTrimStr( Value ) ) ) then
		( GetComponent( 0 ) as TKStringBlowFishCypher ).BlockPadType := bptNormal;			 
	inherited SetValue( Value );
end;

end.
