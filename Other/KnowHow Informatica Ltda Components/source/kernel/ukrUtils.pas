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

unit ukrUtils;

{$I s:\v100\include\iKLIB100.inc}

{$BOOLEVAL OFF}

interface

uses
	Windows, Messages, Classes, Graphics, Forms, Menus, ComCtrls, StdCtrls,
	uksyUtils, uksyTypes;

{
--------------------------------------------------------------------------------
------------------------------ Generic Globals ---------------------------------
--------------------------------------------------------------------------------
}

type

	EKRUtils = class( EKKernel );

{
--------------------------------------------------------------------------------
------------------------------- Menu Routines ----------------------------------
--------------------------------------------------------------------------------
}

procedure AssignMenu( MenuFrom, MenuTo: TPopupMenu );
procedure AssignMenuItems( ItemsFrom, ItemsTo: TMenuItem );

{
--------------------------------------------------------------------------------
------------------------------ Generic Routines --------------------------------
--------------------------------------------------------------------------------
}

{------------------------------ Generic Routines -------------------------------}

function Cript( const Value: string ): string;
function Uncript( const Value: string ): string;
function Nearest( RoundTo: Integer; Value: Double ): Integer;
function GetSize( const Source: string): Integer;
function GetRefCount( const Source: string ): Integer;

type

	TKCreditCard = ( ccNone, ccMaster, ccVisa, ccAmex, ccDinners, ccDiscover, ccRoute, ccJCB );

function Lunh( const Card: string ): Boolean;	
function VerifyCardExpiration( AYear, AMonth: Word ): Boolean;	
function VerifyCardNumber( ccType: TKCreditCard; const Card: string ): Boolean;

function ExcludeDataBetweenTokens( Stream: TStringStream; const Data, Token: string ): string;
procedure ExcludeDataBetweenTokensEx( Stream: TStringStream; const Token: string );

{$IFNDEF DELPHI4}

function WrapText( const Line, BreakStr: string; BreakChars: TKCharSet;
	MaxCol: Integer ): string;
function FindCmdLineSwitch( const Switch: string; SwitchChars: TKCharSet;
	IgnoreCase: Boolean ): Boolean;

{$ENDIF}

type

  TKSearchClassesCallBack = function( ACIass: TClass ): Boolean of object;

procedure POSearchClasses( Aninstance: Cardinal; ACallBack: TKSearchClassesCallBack );

{----------------------------- Brazilian Routines ------------------------------}

function NameInteger( Value: Integer ): String;
function VerifyCPF( const CPF: String ): Boolean;
function VerifyCGC( const CGC: String ): Boolean;

{------------------------------ Graphic Routines -------------------------------}

function BmpToIco( const BMPName: string ): Boolean;
function IcoToBmp( const IcoName: string ): Boolean;
function CreateBitmap( ResName: PChar ): TBitmap;
function MakeModuleBitmap( Module: THandle; ResID: PChar ): TBitmap;

{------------------------------- Sound Routines --------------------------------}

procedure NoSound;
procedure Sound( Freq: Word );
procedure Play( Freq: Word; MiliSecs: Integer );

{------------------------------- WinAPI Routines -------------------------------}

function ExecuteDOS( const cmd: String; ss: TStrings ): Boolean;
function WinPassword: string;

function GetFormParentHandle( Child: TCustomForm ): HWnd;
function GetFormParent( Child: TCustomForm ): TComponent;
procedure SetFormParentHandle( ChildHwnd, ParentHwnd: HWnd );
procedure SetFormParent( Child: TCustomForm; ParentHwnd: HWnd );
function SendMsgTimeOut( Handle: HWND; var Message: TMessage;
	uTimeout: UINT ): Boolean;
function GetLtpPortAddress( PortNo: Integer ): Word; assembler; stdcall;

procedure SetControlTabStops( CustomEdit: TCustomEdit; TabStops: TStrings );

{
--------------------------------------------------------------------------------
------------------------- Generic Dialog Architecture --------------------------
--------------------------------------------------------------------------------
}

procedure RegisterCheckListDialog;
procedure UnregisterCheckListDialog;

{
--------------------------------------------------------------------------------
-------------------------------- TreeView Releted ------------------------------
--------------------------------------------------------------------------------
}

procedure SaveTreeViewToTextFile( const FileName: string; TreeView: TCustomTreeView );
procedure LoadTreeViewFromTextFile( const FileName: string; TreeView: TCustomTreeView );

{
--------------------------------------------------------------------------------
-------------------------------- ListView Releted ------------------------------
--------------------------------------------------------------------------------
}

{ not yet finished }
procedure SaveListViewToTextFile( const FileName: string; ListView: TCustomListView );
procedure LoadListViewFromTextFile( const FileName: string; ListView: TCustomListView );

{##NI##}

{
--------------------------------------------------------------------------------
------------------------- Registration Information -----------------------------
--------------------------------------------------------------------------------
}

function IsKernel_Shareware: Boolean;

{##NI##}

{
--------------------------------------------------------------------------------
--------------------- Validating Registration Information ----------------------
--------------------------------------------------------------------------------
}

function PackageUserName: string;
function PackageCompanyName: string;
function PackageVersion: TKLibVersion;

implementation

uses
	Consts, SysUtils, Controls, CheckLst, uksyConsts, uksyResStr, uksyPackReg,
	uksyClasses, ukrConsts, ukrResStr, ukrTypes, ukrClasses, ukrPackReg;

{
--------------------------------------------------------------------------------
------------------------------- Menu Routines ----------------------------------
--------------------------------------------------------------------------------
}

procedure AssignMenu( MenuFrom, MenuTo: TPopupMenu );
begin
	if ( not CheckObjects( [MenuFrom, MenuTo] ) ) then
		Exit;
//	AssignPubProps( MenuFrom, MenuTo ); Do not assign (attrib) the Items property...
	with MenuFrom do
	begin
		MenuTo.Alignment := Alignment;
		MenuTo.AutoPopup := AutoPopup;
		MenuTo.HelpContext := HelpContext;
	end;
	AssignMenuItems( MenuFrom.Items, MenuTo.Items );
end;

procedure AssignMenuItems( ItemsFrom, ItemsTo: TMenuItem );
var
	i: Integer;
	mi: TMenuItem;
begin
	for i := ItemsTo.Count - 1 downto 0 do
		ItemsTo.Items[i].Free;
	for i := 0 to ItemsFrom.Count - 1 do
	begin
		mi := TMenuItem.Create( ItemsTo );
		AssignPubProps( ItemsFrom.Items[i], mi );
{
		with ItemsFrom.Items[i] do
		begin
			mi.MenuIndex := MenuIndex;
			mi.Break := Break;
			mi.Caption := Caption;
			mi.Checked := Checked;
			mi.Default := Default;
			mi.Enabled := Enabled;
			mi.GroupIndex := GroupIndex;
			mi.HelpContext := HelpContext;
			mi.Hint := Hint;
			mi.RadioItem := RadioItem;
			mi.ShortCut := ShortCut;
			mi.Visible := Visible;
			mi.OnClick := OnClick;
		end;
}		
		ItemsTo.Insert( i, mi );
		AssignMenuItems( ItemsFrom.Items[i], ItemsTo.Items[i] );
	end;
end;

{
--------------------------------------------------------------------------------
------------------------------ Generic Routines --------------------------------
--------------------------------------------------------------------------------
}

{------------------------------ Generic Routines -------------------------------}

const
	pace_c: array[1..5] of Integer = ( 51, -25, 16, 89, -41 );

function ValidateRange( Value: Integer ): Integer;
begin
	Result := Value;
	while ( Result < 128 ) do
		Inc( Result, 128 );
	while ( Result > 127 ) do
		Dec( Result, 128 );
end;

function Cript( const Value: string ): string;
var
	s: string;
	i,
	j: Integer;
begin
	if ( Value = K_DEF_PASSWORD ) then
		Result := Value
	else
	begin
		j := 1;
		s := '';
		for i := 1 to Length( Value ) do
		begin
			s := s + Chr( ValidateRange( Ord( Value[i] ) + pace_c[j] ) );
			if ( j = 5 ) then
				j := 1
			else
				Inc( j );
		end;
		Result := s;
	end;
end;

function Uncript( const Value: string ): string;
var
	s: string;
	i,
	j: Integer;
begin
	if ( Value = K_DEF_PASSWORD ) then
		Result := Value
	else
	begin
		j := 1;
		s := '';
		for i := 1 to Length( Value ) do
		begin
			s := s + Chr( ValidateRange( Ord( Value[i] ) - pace_c[j] ) );
			if ( j = 5 ) then
				j := 1
			else
				Inc( j );
		end;
		Result := s;
	end;
end;

function Nearest( RoundTo: Integer; Value: Double ): Integer;
var
	iConst,
	iInteger: Integer;
	xDelta,
	xFraction: Extended;
begin
	iConst := 1;
	Result := 0;
	xDelta := 0.5;
{ Tratar tudo como positivo. }
	if ( RoundTo * Value < 0 ) then
		iConst := -1;
	Value := abs( Value );
	RoundTo := abs( RoundTo );
	xFraction := abs( Frac( Value ) );
	if ( xFraction >= 0.5 ) then
		xDelta := 0.9;
{ Arredondar para o próximo inteiro. }
	iInteger := Round( Int( Value + xDelta ) );
{ Calcular a parcela "Nearest" para a parte inteira. }
	while ( iInteger >= RoundTo ) do
	begin
		Value := Value - RoundTo;
		Dec( iInteger, RoundTo );
		inc( Result, RoundTo );
	end;
{ Calcular a parcela "Nearest" para a parte fracionária. }
	if ( iInteger > 0 ) and
		 ( 2 * ( iInteger - ( Int( Value + xDelta ) - Int( Value ) ) + xFraction ) >= RoundTo ) then
		inc( Result, RoundTo );
{ Resolver a questão do sinal. }
	Result := iConst * Result;
end;

function GetSize( const Source: string ): Integer;
var
	szPointer: Pointer;
begin
	if CheckPointer( Pointer( Source ) ) then
	begin
		szPointer := IncPtr( Pointer( Source ), -4 );
		Result := Integer( szPointer^ );
	end
	else
		Result := 0;
end;

function GetRefCount( const Source: string ): Integer;
var
	rcPointer: Pointer;
begin
	if CheckPointer( Pointer( Source ) ) then
	begin
		rcPointer := IncPtr( Pointer( Source ), -8 );
		Result := Integer( rcPointer^ );
	end
	else
		Result := 0;
end;

function VerifyCardExpiration( AYear, AMonth: Word ): Boolean;
begin
	Result := ( CurrentYear < AYear ) or ( ( CurrentYear = AYear ) and ( CurrentMonth <= AMonth ) );
end;

function Lunh( const Card: string ): Boolean;
var
	i,
	iOdd,
	iEven,
	iNum: Integer;
	sInv: string;
begin
	Result := CheckTrimStr( Card );
	if Result then
	begin
		i := 0;
		iOdd := 0;
		iEven := 0;
		sInv := InvertString( Card );
		while ( i < Length( sInv ) ) do
		begin
			iOdd := iOdd + Ord( sInv[i] );
			if ( ( i + 1 ) < Length( sInv ) ) then
			begin
				iNum := 2 * Ord( sInv[i + 1] );
				if ( iNum > 9 ) then
					iEven := iEven + 1 + ( iNum - 10 )
				else
					iEven := iEven + iNum;
			end;
			i := ( i + 2 ); 
		end;
		Result := ( i > 0 ) and ( ( iOdd + iEven ) mod 10 = 0 );
	end;
end;

function VerifyCardNumber( ccType: TKCreditCard; const Card: string ): Boolean;

	function IsMaster( const Source: string ): Boolean;
	begin
		Result := ( ( Length( Source ) = 16 ) and ValueBetween( StrToIntDef(
			Copy( Source, 1, 2 ), 0 ), 51, 55, True ) );
	end;

	function IsVisa( const Source: string ): Boolean;
	begin
		Result := ( ( Length( Source ) in [13, 16] ) and ( Source[1] = '4' ) );
	end;

	function IsAmex( const Source: string ): Boolean;
	begin
		Result := ( ( Length( Source ) = 15 ) and ( StrToIntDef( Copy( Source, 1, 2 ),
			0 ) in [34, 37] ) );
	end;

	function IsDinners( const Source: string ): Boolean;
	begin
		Result := ( ( Length( Source ) = 14 ) and ( ( StrToIntDef( Copy( Source, 1, 2 ),
			0 ) in [36, 38] ) or ValueBetween( StrToIntDef( Copy( Source, 1, 3 ), 0 ),
			300, 305, True ) ) );
	end;

	function IsDiscover( const Source: string ): Boolean;
	begin
		Result := ( ( Length( Source ) = 16 ) and ( StrToIntDef( Copy( Source, 1, 4 ),
			0 ) = 6011 ) );
	end;

	function IsRoute( const Source: string ): Boolean;
	begin
		Result := ( ( Length( Source ) = 15 ) and ( ( StrToIntDef( Copy( Source, 1, 4 ),
			0 ) = 2114 ) or ( StrToIntDef( Copy( Source, 1, 4 ), 0 ) = 2149 ) ) );
	end;

	function IsJCB( const Source: string ): Boolean;
	begin
		Result := ( ( ( Length( Source ) = 15 ) and ( ( StrToIntDef( Copy( Source, 1, 4 ),
			0 ) = 2131 ) or ( StrToIntDef( Copy( Source, 1, 4 ), 0 ) = 1800 ) ) ) or
			( ( Length( Source ) = 16 ) and ( StrToIntDef( Copy( Source, 1, 1 ), 0 ) = 3 ) ) );
	end;

begin
	Result := ( ValueBetween( Byte( ccType ), Byte( Low( TKCreditCard ) ), Byte( High(
		TKCreditCard ) ), True ) and ValueBetween( Length( Trim( Card ) ), 13, 16, True ) );
	if Result then
	begin
		case ccType of
			ccNone    : { do nothing };
			ccMaster  : Result := IsMaster( Card );
			ccVisa    : Result := IsVisa( Card );
			ccAmex    : Result := IsAmex( Card );
			ccDinners : Result := IsDinners( Card );
			ccDiscover: Result := IsDiscover( Card );
			ccRoute   : Result := IsRoute( Card );
			ccJCB     : Result := IsJCB( Card );
		end;
		if Result then
			Result := Lunh( Card );
	end;
end;

procedure ExcludeDataBetweenTokensEx( Stream: TStringStream; const Token: string );
var
	iPos1,
	iPos2: Integer;
	Data: string;
begin
	Force( [Stream, Token] );
	Data := Stream.DataString;
	iPos2 := -1;
	iPos1 := Pos( Token, Data ) - 1;
	if ( iPos1 > 0 ) then
		iPos2 := Pos( Token, PChar( @Data[iPos1 + Length( Token )] ) ) + iPos1 + ( 2 * Length( Token ) );
	if ( iPos2 <> -1 ) and ( iPos2 <= iPos1 + ( 2 * Length( Token ) ) ) then
		RaiseExceptionFmt( EKRUtils, sErrInvTokenPairs, [Token] );
	if ( iPos2 > 0 ) then
	begin
{
		Count := ( iPos2 - iPos1 );
		Stream.CopyBufferEx( iPos1 + Count, iPos1, Stream.Size - ( Count + iPos1 ) );}
		CopyBufferEx( Stream, iPos2, iPos1, Stream.Size - iPos2 );
	end;
end;

function ExcludeDataBetweenTokens( Stream: TStringStream; const Data, Token: string ): string;
var
	iPos1,
	iPos2: Integer;
begin
	Force( [Stream, Data, Token] );
	Result := Data;
	iPos2 := -1;
	iPos1 := Pos( Token, Data );
	if ( iPos1 > 0 ) then
		iPos2 := Pos( Token, PChar( @Data[iPos1 + Length( Token )] ) ) + iPos1 + ( 2 * Length( Token ) );
	if ( iPos2 <> -1 ) and ( iPos2 <= iPos1 + ( 2 * Length( Token ) ) ) then
		RaiseExceptionFmt( EKRUtils, sErrInvTokenPairs, [Token] );
	if ( iPos2 > 0 ) then
		Delete( Result, iPos1, iPos2 - iPos1 );
end;

{$IFNDEF DELPHI4}

function WrapText( const Line, BreakStr: string; BreakChars: TKCharSet;
	MaxCol: Integer ): string;
const
	QuoteChars = ['''', '"'];
var
	Col, Pos: Integer;
	LinePos, LineLen: Integer;
	BreakLen, BreakPos: Integer;
	QuoteChar, CurChar: Char;
	ExistingBreak: Boolean;
begin
	Col := 1;
	Pos := 1;
	LinePos := 1;
	BreakPos := 0;
	QuoteChar := ' ';
	ExistingBreak := false;
	LineLen := Length( Line );
	BreakLen := Length( BreakStr );
	Result := '';
	while Pos <= LineLen do
	begin
		CurChar := Line[Pos];
		if CurChar in LeadBytes then
		begin
			Inc( Pos );
			Inc( Col );
		end else
			if CurChar = BreakStr[1] then
			begin
				if QuoteChar = ' ' then
				begin
					ExistingBreak := CheckStrEqual( BreakStr, Copy( Line, Pos, BreakLen ) );
					if ExistingBreak then
					begin
						Inc( Pos, BreakLen-1 );
						BreakPos := Pos;
					end;
				end
			end
			else if CurChar in BreakChars then
			begin
				if QuoteChar = ' ' then BreakPos := Pos
			end
			else if CurChar in QuoteChars then
				if CurChar = QuoteChar then
					QuoteChar := ' '
				else if QuoteChar = ' ' then
					QuoteChar := CurChar;
		Inc( Pos );
		Inc( Col );
		if not ( QuoteChar in QuoteChars ) and ( ExistingBreak or
			( ( Col > MaxCol ) and ( BreakPos > LinePos ) ) ) then
		begin
			Col := Pos - BreakPos;
			Result := Result + Copy( Line, LinePos, BreakPos - LinePos + 1 );
      if not ( CurChar in QuoteChars ) then
				while ( Pos <= LineLen ) and ( Line[Pos] in BreakChars + [#13, #10] ) do Inc( Pos );
			if not ExistingBreak and ( Pos < LineLen ) then
        Result := Result + BreakStr;
      Inc( BreakPos );
			LinePos := BreakPos;
      ExistingBreak := false;
		end;
  end;
  Result := Result + Copy( Line, LinePos, MaxInt );
end;

function FindCmdLineSwitch( const Switch: string; SwitchChars: TKCharSet;
  IgnoreCase: Boolean ): Boolean;
var
	I: Integer;
	S: string;
begin
  for I := 1 to ParamCount do
	begin
		S := ParamStr( I );
		if ( SwitchChars = [] ) or ( S[1] in SwitchChars ) then
			if IgnoreCase then
      begin
				if ( AnsiCompareText( Copy( S, 2, Maxint ), Switch ) = 0 ) then
        begin
          Result := true;
          Exit;
        end;
      end
			else begin
				if ( AnsiCompareStr( Copy( S, 2, Maxint ), Switch ) = 0 ) then
				begin
					Result := true;
					Exit;
				end;
			end;
	end;
	Result := false;
end;

{$ENDIF}

procedure POSearchClasses( Aninstance: Cardinal; ACallBack: TKSearchClassesCallBack );
var
	DosHeader: PIMAGE_DOS_HEADER;
	NTHeader: PIMAGE_NT_HEADERS;
	SectionHeader: PIMAGE_SECTION_HEADER;
	pCodeBegin,
	pCodeEnd,
	pcode,
	p: PChar;

	function GetSectionHeader( const ASectionName: string ): Boolean;
	var
	 i: Integer;
	begin
		SectionHeader:= PIMAGE_SECTION_HEADER( NTHeader );
		Inc( PIMAGE_NT_HEADERS( SectionHeader ) );
		Result := True;
		for i := 0 to NTHeader^.FileHeader.NumberOfSections - 1 do
		begin
			if ( StrLIComp( SectionHeader^.Name, PChar( ASectionName ),
			     IMAGE_SIZEOF_SHORT_NAME ) = 0 ) then
				Exit;
			Inc( SectionHeader );  // It will work fine ?
		end;
		Result := False;
	end;

	{ Result := ( APointer = nil ) or ( ( Integer( pMin ) <= lnteger( APointer ) ) and
		 ( Integer( APointer ) <= lnteger( pMax ) ) ); }
	function InRangeOrNil( APointer, pmin, pmax: Pointer ): Boolean; assembler;
	asm
		TEST APointer,APointer
		JZ @_EndTrue
		CMP APointer,pMin
		JL @_EndFalse
		CMP APointer,pMax
		JLE @_EndTrue
		@_EndFalse:
		XOR EAX,EAX
		RET
		@_EndTrue:
		MOV AL,1
		RET
	end;

	function IsIdent( p: PChar ): Boolean;
	var
		ig,
		i: Integer;
	begin
		ig := Ord( p^ );
		Inc( p );
		Result := ( ig > 0 ) and ( p^ in ( CHARSET_ALPHA + ['_'] ) );
		if ( not Result ) then
			Exit;
		for i := 2 to ig do
		begin
			Inc( p );
			if ( not ( p^ in CHARSET_IDENTIFIER ) ) then
			begin
				Result := False;
				Exit;
			end;
		end;
	end;

begin
	ForceReference( @ACallBack );
	{ Read the DOS header }
	DosHeader := PIMAGE_DOS_HEADER( AnInstance );
	ForcePointer( DosHeader );
	if ( not DosHeader^.e_magic = IMAGE_DOS_SIGNATURE ) then // POUnrecognizedFileFormat
		Exit;
	{ Read the NT header ( PE format ) }
	NTHeader := PIMAGE_NT_HEADERS( Longint( DosHeader ) + DosHeader^.e_lfanew );
	if IsBadReadPtr( NTHeader, SizeOf( IMAGE_NT_HEADERS ) ) or
		 ( NTHeader^.Signature <> IMAGE_NT_SIGNATURE ) then // PONotAPEFile
		Exit;
	{ Find the code section }
	if ( not GetSectionHeader( 'CODE' ) ) then // PONoinitializedData;
		Exit;
	{ Computes beginning & end of the code section }
	pCodeBegin := PChar( AnInstance + SectionHeader^.VirtualAddress );
	pCodeEnd := pCodeBegin + ( SectionHeader^.SizeOfRawData - 3 );
	pcode:= pCodeBegin;
	while ( pcode < pCodeEnd ) do
	begin
		p := PPointer( pCode )^;
		{ Search for a class }
		if ( ( p = ( pcode - vmtSelfPtr ) ) and // Is it SelfPtr pointer?
			InRangeOrNil( PPointer( p + vmtClassName )^, p, pCodeEnd) and
			InRangeOrNil( PPointer( p + vmtDynamicTable )^, p, pCodeEnd ) and
			InRangeOrNil( PPointer( p + vmtMethodTable )^, p, pCodeEnd ) and
			InRangeOrNil( PPointer( p + vmtFieldTable )^, p, pCodeEnd ) and
			InRangeOrNil( PPointer( p + vmtTypeInfo )^, pCodeBegin, pCodeEnd ) and
			InRangeOrNil( PPointer( p + vmtInitTable )^, pCodeBegin, pCodeEnd ) and
			InRangeOrNil( PPointer( p + vmtAutoTable )^, pCodeBegin, pCodeEnd ) and
			InRangeOrNil( PPointer( p + vmtIntfTable )^, pCodeBegin, pCodeEnd ) and
			IsIdent( PPointer( p + vmtClassName )^ ) ) then
		begin
			if ( not ACallBack( TClass( p ) ) ) then
				Exit;
			Inc( pCode, 4 );
		end
		else
			Inc( pCode );
	end;
end;

{----------------------------- Brazilian Routines ------------------------------}

type
	TKNumbers = array[0..9] of String;
	TKMagnitudes = array[0..4] of String;

const
	UNITS: TKNumbers =
		( ( '' ),
			( 'um' ),
			( 'dois' ),
			( 'três' ),
			( 'quatro' ),
			( 'cinco' ),
			( 'seis' ),
			( 'sete' ),
			( 'oito' ),
			( 'nove' ) );

	TEENS: TKNumbers =
		( ( 'dez' ),
			( 'onze' ),
			( 'doze' ),
			( 'treze' ),
			( 'catorze' ),
			( 'quinze' ),
			( 'dezesseis' ),
			( 'dezessete' ),
			( 'dezoito' ),
			( 'dezenove' ) );

	TENS: TKNumbers =
		( ( '' ),
			( 'dez' ),
			( 'vinte' ),
			( 'trinta' ),
			( 'quarenta' ),
			( 'cinquenta' ),
			( 'sessenta' ),
			( 'setenta' ),
			( 'oitenta' ),
			( 'noventa' ) );

	HUNDREDS: TKNumbers =
		( ( '' ),
			( 'cento' ),
			( 'duzentos' ),
			( 'trezentos' ),
			( 'quatrocentos' ),
			( 'quinhentos' ),
			( 'seiscentos' ),
			( 'setecentos' ),
			( 'oitocentos' ),
			( 'novecentos' ) );

	MAGNITUDE_SG: TKMagnitudes =
		( ( ' ' ),
			( ' mil' ),
			( ' milhão' ),
			( ' bilhão' ),
			( ' trilhão' ) );

	MAGNITUDE_PL: TKMagnitudes =
		( ( ' ' ),
			( ' mil' ),
			( ' milhões' ),
			( ' bilhões' ),
			( ' trilhões' ) );

function HundredToString( Value: Integer ): String;
begin
	Result := '';
	if ( Value < 0 ) or ( Value > 999 ) then Exit;
{ 100 }
	if ( Value = 100 ) then
	begin
		Result := 'cem';
		Exit;
	end;
	Result := HUNDREDS[Value div 100];
	Value := Value mod 100;
	if ( Value > 0 ) and
		 ( Result <> '' ) then
		Result := Result + ' e ';

{ X10..X19 }
	if ( Value >= 10 ) and ( Value < 20 ) then
	begin
		Result := Result +
							TEENS[Value mod 10];
		Exit;
	end
	else
{ X20..X99 e X00..X09 }
	begin
		Result := Result +
							TENS[Value div 10];
		if ( ( Value div 10 ) > 0 ) and
			 ( ( Value mod 10 ) > 0 ) then
			Result := Result + ' e ';
		Result := Result + UNITS[Value mod 10];
	end;
end;

function NameInteger( Value: Integer ): String;

	function GetSuffix( Order, Number: Integer ): String;
	begin
		if ( Number = 1 ) then
			Result := MAGNITUDE_SG[Order]
		else
			Result := MAGNITUDE_PL[Order];
	end;

var
	iGroup,
	iValue,
	iNumber,
	iLastGroup: Integer;
begin
	iGroup := 0;
	Result := '';
	iValue := Value;
	iLastGroup := 0;
	while ( iValue > 0 ) do
		if ( ( iValue mod 1000 ) = 0 ) then
		begin
			inc( iLastGroup );
			iValue := iValue div 1000;
		end
		else
			Break;
	while ( Value > 0 ) do
	begin
		iNumber := Value mod 1000;
		Value := Value div 1000;
		Result := HundredToString( iNumber ) +
							GetSuffix( iGroup, iNumber ) +
							Result;
		if ( Value > 0 ) and
			 ( iGroup = iLastGroup ) and
			 ( ( iNumber mod 100 = 0 ) or
				 ( ( iNumber > 0 ) and ( iNumber <= 99 ) ) ) then
			Result := ' e ' + Result
		else
			Result := ' ' + Result;
		inc( iGroup );
	end;
	if ( Result = '' ) then
		Result := 'zero'
	else
		Result := Trim( Result );
end;

const
	ORD_0 = 48;

function VerifyCPF( const CPF: String ): Boolean;
var
	sCPF: string;
	i,
	iSum,
	iDigit01,
	iDigit02: Integer;
begin
  ForceTrimStr( CPF );
	sCPF := CPF;
	Result := false;
	if ( not ( Length( sCPF ) in [11, 14] ) ) then
		Exit;
	if ( Length( sCPF ) = 14 ) then
		sCPF := Copy( sCPF, 1, 3 ) + Copy( sCPF, 5, 3 ) +
						Copy( sCPF, 9, 3 ) + Copy( sCPF, 13, 2 );
	Result := IsValidString( sCPF, ceDigit );
	if ( not Result ) then
		Exit;
{ first check digit }
	iSum := 0;
	for i := 9 downto 1 do
		inc( iSum, ( Ord( sCPF[i] ) - ORD_0 ) * ( 11 - i ) );
	iDigit01 := 11 - ( iSum mod 11 );
	if ( iDigit01 > 9 ) then
		iDigit01 := 0;
	Result := ( iDigit01 = ( Ord( sCPF[10] ) - ORD_0 ) );
	if ( not Result ) then
		Exit;
{ second check digit }
	iSum := 0;
	for i := 10 downto 1 do
		inc( iSum, ( Ord( sCPF[i] ) - ORD_0 ) * ( 12 - i ) );
	iDigit02 := 11 - ( iSum mod 11 );
	if ( iDigit02 > 9 ) then
		iDigit02 := 0;
	Result := ( iDigit02 = ( Ord( sCPF[11] ) - ORD_0 ) );
end;

function VerifyCGC( const CGC: String ): Boolean;
var
	sCGC: String;
	i,
	iSum,
	iDigit01,
	iDigit02: Integer;
begin
  ForceTrimStr( CGC );
	sCGC := CGC;
	Result := false;
	if ( not ( Length( sCGC ) in [14, 18] ) ) then
		Exit;
	if ( Length( sCGC ) = 18 ) then
		sCGC := Copy( sCGC, 1, 2 ) + Copy( sCGC, 4, 3 ) +
						Copy( sCGC, 8, 3 ) + Copy( sCGC, 12, 4 ) +
						Copy( sCGC, 17, 2 );
	Result := IsValidString( sCGC, ceDigit );
	if ( not Result ) then
		Exit;
{ first check digit }
	iSum := 0;
	for i := 1 to 12 do
		if ( i < 5 ) then
			inc( iSum, ( Ord( sCGC[i] ) - ORD_0 ) *( 6 - i ) )
		else
			inc( iSum, ( Ord( sCGC[i] ) - ORD_0 ) * ( 14 - i ) );
	iDigit01 := 11 - ( iSum mod 11 );
	if ( iDigit01 > 9 ) then
		iDigit01 := 0;
	Result := ( iDigit01 = ( Ord( sCGC[13] ) - ORD_0 ) );
	if ( not Result ) then
		Exit;
{ second check digit }
	iSum := 0;
	for i := 1 to 13 do
		if ( i < 6 ) then
			inc( iSum, ( Ord( sCGC[i] ) - ORD_0 ) * ( 7 - i ) )
		else
			inc( iSum, ( Ord( sCGC[i] ) - ORD_0 ) * ( 15 - i ) );
	iDigit02 := 11 - ( iSum mod 11 );
	if ( iDigit02 > 9 ) then
		iDigit02 := 0;
	Result := ( iDigit02 = ( Ord( sCGC[14] ) - ORD_0 ) );
end;

{------------------------------ Graphic Routines -------------------------------}

function BmpToIco( const BMPName: string ): Boolean;
var
	Icon: TIcon;
	Bitmap: TBitmap;
begin
	ForceFile( BMPName );
	Icon := TIcon.Create;
	try
		Bitmap := TBitmap.Create;
		try
			try
				BitMap.LoadFromFile( BMPName );
				Icon.Width := BitMap.Width;
				Icon.Height := BitMap.Height;
				Icon.Palette := BitMap.Palette;
				Icon.SaveToFile( ChangeFileExt( BMPName, '.ico' ) );
			except
				on E: Exception do
					RaiseExceptionFmt( EKRUtils, sErrBmpConv, [BMPName] );
			end;
		finally
			Bitmap.Free;
		end;
	finally
		Icon.Free;
	end;
	Result := CheckFile( ChangeFileExt( BMPName, '.ico' ) );
end;

function IcoToBmp( const IcoName: string ): Boolean;
var
	Icon: TIcon;
	Bitmap: TBitmap;
begin
	ForceFile( IcoName );
	Icon := TIcon.Create;
	try
		Bitmap := TBitmap.Create;
		try
			try
				Icon.LoadFromFile( IcoName );
				Bitmap.Width := Icon.Width;
				Bitmap.Height := Icon.Height;
				Bitmap.Canvas.Draw( 0, 0, Icon );
				Bitmap.SaveToFile( ChangeFileExt( IcoName,'.bmp' ) );
			except
				on E: Exception do
					RaiseExceptionFmt( EKRUtils, sErrIcoConv, [IcoName] );
			end;
		finally
			Bitmap.Free;
		end;
	finally
		Icon.Free;
	end;
	Result := CheckFile( ChangeFileExt( IcoName, '.bmp' ) );
end;

function CreateBitmap( ResName: PChar ): TBitmap;
begin
	Result := MakeModuleBitmap( HInstance, ResName );
	if ( not CheckObject( Result ) ) then
		RaiseExceptionFmt( EKRUtils, SResNotFound, [ResName] );
end;

function MakeModuleBitmap( Module: THandle; ResID: PChar ): TBitmap;
begin
	Result := TBitmap.Create;
	try
		Result.Handle := LoadBitmap( Module, ResID );
		if ( not CheckHandle( Result.Handle ) ) then
			FreeClean( Result );
	except
		FreeClean( Result );
		raise;
	end;
end;

{------------------------------- Sound Routines --------------------------------}

const
	DEF_PORT = $61;

procedure SetPort( Address, Value: Word );
var
	bValue: Byte;
begin
	bValue := Trunc( Value and 255 );
  asm
    MOV DX, Address
		MOV AL, bValue
		OUT DX, AL
	end;
end;

function GetPort( Address: Word ): Word;
var
	bValue: Byte;
begin
	asm
		MOV DX, Address
		IN  AL, DX
		MOV bValue, AL
	end;
	Result := bValue;
end;

procedure NoSound;
var
	wValue: Word;
begin
	if CheckWinNT then
		RaiseException( EKRUtils, sErrInvWinNTCall );
	wValue := GetPort( DEF_PORT );
	wValue := wValue and $FC;
	SetPort( DEF_PORT, wValue );
end;

procedure Sound( Freq: Word );
const
	DEF_SOUND = $1234DD;
var
	B: Word;
begin
	if CheckWinNT then
		RaiseException( EKRUtils, sErrInvWinNTCall );
	if ( Freq > 18 ) then
	begin
		Freq := Word( DEF_SOUND div LongInt( Freq ) );
		B := GetPort( DEF_PORT );
		if ( ( B and 3 ) = 0 ) then
		begin
			SetPort( DEF_PORT, B or 3 );
			SetPort( $43, $B6 );
		end;
		SetPort( $42, Freq );
		SetPort( $42, ( Freq shr 8 ) );
	end;
end;

procedure Play( Freq: Word; MiliSecs: Integer );
begin
	Sound( Freq );
	Delay( MiliSecs );
	NoSound;
end;

{------------------------------- WinAPI Routines -------------------------------}

const
{
	Executable file image that allows a win32 app to call a win16 exe
	to retrieve current user's Windows password.
}
	PWD_COM: array[0..62] of Byte =
	(
		$B3, $0E, $BF, $2B, $01, $B8, $84, $11,
		$B9, $0F, $00, $CD, $2F, $89, $FA, $FC,
		$B9, $16, $00, $30, $C0, $F2, $AE, $26,
		$C7, $45, $FF, $0D, $0A, $C7, $45, $01,
		$24, $00, $B4, $09, $CD, $21, $B8, $00,
		$4C, $CD, $21, $00, $00, $00, $00, $00,
		$00, $00, $00, $00, $00, $00,	$00, $00,
		$00, $00, $00, $0D, $0A, $24, $00
	);

{

( Code Segment )

		MOV     BL,0EH    ; Interruption SubServices for int 2F (Multiplex)

											; 0EH => User PassWord !!!
                      ; 03H => User LogIn
											; 05H => User WorkGroup

		MOV     DI,012B   ; Data Segment address of the return value
		MOV     AX,1184H  ; Interuption Service
		MOV     CX,000F   ; Only need for User PassWord (MaxLen=0F)
		INT     2F        ; Interruption Call
		MOV     DX,DI
		CLD
		MOV     CX,0016
		XOR     AL,AL
		REPNZ
		SCASB
		ES:
		MOV     WORD PTR [DI-01],0A0D
		MOV     WORD PTR [DI+01],0024
		MOV     AH,09
		INT     21
		MOV     AX,4C00
		INT     21

( Data Segment )

		00 00 00 00 00 00 00 00
		00 00 00 00 00 00 00 00
		0D 0A 24 00

}

function ExecuteDOS( const cmd: string; ss: TStrings ): Boolean;
var
	bOK: Boolean;
	sFile: string;
	hFile: Integer;
	psi: PStartupInfo;
	ppi: PProcessInformation;
begin
	ForceObject( ss );
	Result := false;
	psi := New( PStartupInfo );
	try
		ZeroMemory( psi, SizeOf( TStartupInfo ) );
		ppi := New( PProcessInformation );
		try
			ZeroMemory( ppi, SizeOf( TProcessInformation ) );
			sFile := GetTempPathFile( 'tmp', false );
			sFile := ChangeFileExt( sFile, '.~mp' );
			hFile := FileCreate( sFile );
			bOK := CheckHandle( hFile );
			if bOK then
				try
					with psi^ do
					begin
						cb := SizeOf( TStartupInfo );
						dwFlags := STARTF_USESHOWWINDOW or STARTF_FORCEONFEEDBACK or
											 STARTF_USESTDHANDLES;
						wShowWindow := SW_HIDE;
						hStdInput := INVALID_HANDLE_VALUE;
						hStdOutput := hFile;
						hStdError := INVALID_HANDLE_VALUE;
					end;
					Result := CreateProcess( nil,
						PChar( 'command.com /c ' + cmd ), nil, nil, false,
						NORMAL_PRIORITY_CLASS, nil, nil, psi^, ppi^ );
					if Result then
						WaitForSingleObject( ppi^.hProcess, INFINITE );
				finally
					FileClose( hFile );
					ss.LoadFromFile( sFile );
					SysUtils.DeleteFile( sFile );
				end;
		finally
			Dispose( ppi );
		end;
	finally
		Dispose( psi );
	end;
end;

function WinPassword: string;
var
	sFile: string;
	fs: TFileStream;
	sl: TStrings;
begin
	Result := '';
	sFile := ChangeFileExt( GetTempPathFile( 'pwd', false ), '.com' );
	try
		fs := TFileStream.Create( sFile, fmCreate or fmShareExclusive );
		try
			fs.WriteBuffer( PWD_COM, SizeOf( PWD_COM ) );
		finally
			fs.Free;
		end;
    sl := TStringList.Create;
    try
			ExecuteDos( sFile, sl );
      Result := sl.Text;
		finally
			sl.Free;
		end;
	finally
		ForceDeleteFile( sFile );
	end;
end;

function GetFormParentHandle( Child: TCustomForm ): HWnd;
begin
	ForceObject( Child );
	Result := GetWindowLong( Child.Handle, GWL_HWNDPARENT );
end;

function GetFormParent( Child: TCustomForm ): TComponent;
var
	Handle: HWnd;
begin
	ForceObject( Child );
	Handle := GetFormParentHandle( Child );
	if CheckObject( Application ) and
		 ( Handle = Application.Handle ) then
		Result := Application
	else
		Result := FindControl( Handle );
end;

procedure SetFormParentHandle( ChildHwnd, ParentHwnd: Hwnd );
begin
	ForceHandle( ChildHwnd );
	ForceHandle( ParentHwnd );
	SetWindowLong( ChildHwnd, GWL_HWNDPARENT, ParentHwnd );
end;

procedure SetFormParent( Child: TCustomForm; ParentHwnd: Hwnd );
begin
	ForceObject( Child );
	ForceHandle( ParentHwnd );
	SetFormParentHandle( Child.Handle, ParentHwnd );
end;

function SendMsgTimeOut( Handle: HWND; var Message: TMessage;
	uTimeout: UINT ): Boolean;
begin
	Result := false;
	if CheckHandle( Handle ) then
		with Message do
			SendMsgTimeOut := Boolean( SendMessageTimeOut( Handle, Msg, WParam,
				LParam, SMTO_NORMAL	or SMTO_ABORTIFHUNG, uTimeOut, {$IFDEF DELPHI4}Cardinal( Result )
				  {$ELSE}Result{$ENDIF} ) );
end;

function GetLtpPortAddress( PortNo: Integer ): Word; assembler; stdcall;
begin
	ForceWinNT( False );
	Result := 0; { Will be the AX }
	asm
		PUSH ES
		PUSH EBX
		MOV  EBX, PortNo
		SHL  EBX, 1
		MOV  AX, 40h
		MOV  ES, ax
		MOV  AX, ES:[EBX + 6]
		POP  EBX
		POP  ES
	end;
end;

{
--------------------------------------------------------------------------------
------------------------- Generic Dialog Architecture --------------------------
--------------------------------------------------------------------------------
}

const
	FM_CBX_WIDTH = 252;
	FM_CBX_HEIGHT = 302;

	CBX_GUTTER = 8;
	CBX_TOP = 5;
	CBX_LEFT = 5;
	CBX_HEIGHT = 230;
	CBX_WIDTH = 240;

	CBX_ITEM_HEIGHT = 20;

type

	TKInternalCheckListForm = class( TKCustomInternalForm )
	private
		FBits: TBits;
		FSorted: Boolean;
		FStrings: TStrings;
		FBackColor: TColor;
		FCbLst: TCheckListBox;
    FPopUpMenu: TPopUpMenu;

		procedure SetItems;
		procedure GetSelection;
		procedure SetSelection;

	protected
		procedure FixButtons; override;
		procedure PrepareForm; override;
		procedure UnprepareForm; override;

{ 	procedure CbLstDblClick( Sender: TObject ); dynamic; }
  	procedure CheckUnCheckAllClick( Sender: TObject ); dynamic; 

	public
		destructor Destroy; override;
		constructor Create( AOwner: TComponent ); override;

		procedure AssignBits( ABits: TBits );
		procedure AssignList( AList: TStrings );

		property Bits: TBits
						 read FBits;
		property List: TStrings
						 read FStrings;
		property Sorted: Boolean
						 read FSorted write FSorted;				 
		property BackColor: TColor
						 read FBackColor write FBackColor;

	end;

constructor TKInternalCheckListForm.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	FBits := TBits.Create;
	FStrings := TStringList.Create;
	DialogStyle := dsOkCancel;
end;

destructor TKInternalCheckListForm.Destroy;
begin
	FreeClean( FStrings );
	FreeClean( FBits );
	FreeClean( FCbLst );
	inherited Destroy;
end;

procedure TKInternalCheckListForm.SetItems;
var
	i,
	iMax: Integer;
begin
	FCbLst.Items.Assign( FStrings );
	iMax := 0;
	for i := 0 to FCbLst.Items.Count - 1 do
		if ( ScaleX( Canvas.TextWidth( FCbLst.Items[i] ) ) > iMax ) then
			iMax := ScaleX( Canvas.TextWidth( FCbLst.Items[i] ) );
	if ( iMax > FCbLst.Width ) then
		FCbLst.Width := iMax;
	if ( ( FCbLst.Width + ( 2 * FCbLst.Left ) ) > ClientWidth ) then
		ClientWidth := FCbLst.Width + ( 2 * FCbLst.Left );
{
	Leave this fixed for simplicity... After all can add ItemViewCount for max height

	FCbLst.Height := ( FCbLst.Items.Count + 1 ) * FCbLst.ItemHeight;
}		   
end;

procedure TKInternalCheckListForm.GetSelection;
var
	i: Integer;
begin
	for i := 0 to FCbLst.Items.Count - 1 do
		FBits[i] := FCbLst.Checked[i];
end;

procedure TKInternalCheckListForm.SetSelection;
var
	i: Integer;
begin
	for i := 0 to FCbLst.Items.Count - 1 do
		FCbLst.Checked[i] := FBits[i];
end;

procedure TKInternalCheckListForm.AssignList( AList: TStrings );
begin
	ForceObject( AList );
	FStrings.Assign( AList );
end;

procedure TKInternalCheckListForm.AssignBits( ABits: TBits );
var
	i: Integer;
begin
	ForceObject( ABits );
	FBits.Size := ABits.Size;
	for i := 0 to FBits.Size - 1 do
		FBits.Bits[i] := ABits.Bits[i];
end;

procedure TKInternalCheckListForm.PrepareForm;
var
  mi: TMenuItem;
begin
	inherited PrepareForm;
	lbMessage.Visible := False;
	Height := ScaleY( FM_CBX_HEIGHT );
	Width := ScaleX( FM_CBX_WIDTH );
	FCbLst := TCheckListBox.Create( nil );
  FPopUpMenu := TPopUpMenu.Create( nil );
  with FPopUpMenu do
  begin
    AutoPopup := True;
    mi := TMenuItem.Create( FPopUpMenu );
    mi.Caption := sCheckListUnCheckAll;
    mi.Tag := -2;
    mi.OnClick := CheckUnCheckAllClick;
    Items.Add( mi );
    mi := TMenuItem.Create( FPopUpMenu );
    mi.Caption := sCheckListCheckAll; 
    mi.Tag := -1;
    mi.OnClick := CheckUnCheckAllClick;
    Items.Add( mi );
  end;
	with FCbLst do
	begin
		Parent := Self;
		Top := ScaleY( CBX_TOP );
		Left := ScaleX( CBX_LEFT );
		Height := ScaleY( CBX_HEIGHT );
		Width := ( Self.ClientWidth - ( 2 * Left ) );
		Color := BackColor;
		Sorted := Self.Sorted;
		Style := lbOwnerDrawFixed;
		ItemHeight := ScaleY( CBX_ITEM_HEIGHT );
    PopupMenu := FPopUpMenu;
		TabOrder := 0;
		SetItems;
		SetSelection;
	end;
	ActiveControl := FCbLst;
end;

procedure TKInternalCheckListForm.FixButtons;
var
	i: Integer;
begin
	inherited FixButtons;
	for i := 0 to Buttons.Count - 1 do
		TControl( Buttons[i] ).Top := FCbLst.Top + FCbLst.Height + ScaleY( CBX_GUTTER );
	ClientHeight := TControl( Buttons[0] ).Top + TControl( Buttons[0] ).Height +
		ScaleY( CBX_GUTTER );
end;

procedure TKInternalCheckListForm.UnprepareForm;
begin
	AssignList( FCbLst.Items );
	GetSelection;
	FreeClean( FCbLst );
  FreeClean( FPopUpMenu );
	inherited UnprepareForm;
end;

procedure TKInternalCheckListForm.CheckUnCheckAllClick( Sender: TObject );
var
	i: Integer;
begin
  with ( Sender as TMenuItem ) do
  	for i := 0 to FCbLst.Items.Count - 1 do
      case Tag of
        -2: FCbLst.Checked[i] := False;
        -1: FCbLst.Checked[i] := True;
      end;
end;

{
procedure TKInternalCheckListForm.CbLstDblClick( Sender: TObject );
var
	i: Integer;
begin
	for i := 0 to FCbLst.Items.Count - 1 do
		FCbLst.Checked[i] := ( not FCbLst.Checked[i] );
end;
}

function InternalInputCheckListDialog( const Caption: string; BackColor: TColor; Sorted: Boolean;
	Source: TStrings; ResultList: TBits ): Boolean;
var
	i: Integer;
	fm: TKInternalCheckListForm;
begin
	ForceStrings( Source );
	fm := TKInternalCheckListForm.Create( nil );
	try
		fm.Caption := Caption;
		fm.Sorted := Sorted;
		fm.BackColor := BackColor;
		fm.AssignList( Source );
		if CheckObject( ResultList ) then
		begin
			ResultList.Size := Source.Count;
			fm.AssignBits( ResultList );
		end;	
		Result := ( fm.ShowModal = mrOk );
		if ( Result and CheckObject( ResultList ) ) then
			for i := 0 to ResultList.Size - 1 do
				ResultList.Bits[i] := fm.Bits[i];
	finally
		fm.Free;
	end;
end;

procedure SetControlTabStops( CustomEdit: TCustomEdit; TabStops: TStrings );
var
	i: Integer;
	pia: PIntegerArray;
begin
  ForceObjects( [CustomEdit, TabStops] );
	for i := TabStops.Count - 1 downto 0 do
		if ( StrToIntDef( TabStops[i], MaxInt ) = MaxInt ) then
			TabStops.Delete( i );
	if ( not CheckStrings( TabStops ) ) then
		SendMessage( CustomEdit.Handle, EM_SETTABSTOPS, 0, 0 )
	else
	begin
		pia := AllocMem( TabStops.Count * SizeOf( Integer ) );
		try
			for i := 0 to TabStops.Count - 1 do
				pia^[i] := StrToIntDef( TabStops[i], 0 );
			SendMessage( CustomEdit.Handle, EM_SETTABSTOPS, TabStops.Count, Integer( pia ) );
		finally
			FreeMem( pia, TabStops.Count * SizeOf( Integer ) );
		end;
	end;
	CustomEdit.Invalidate;
end;

procedure RegisterCheckListDialog;
begin
	InputCheckListDialogFunc := InternalInputCheckListDialog;
end;

procedure UnregisterCheckListDialog;
begin
  InputCheckListDialogFunc := nil;
end;

{
--------------------------------------------------------------------------------
-------------------------------- TreeView Releted ------------------------------
--------------------------------------------------------------------------------
}

type
	TCustomTreeViewHack = class( TCustomTreeView );

procedure SaveTreeViewToTextFile( const FileName: string; TreeView: TCustomTreeView );

	procedure InternalSaveTreeView( fs: TFileStream; tn: TTreeNode );

		function GetTabs( Level: Integer ): string;
		var
			i: Integer;
		begin
			Result := '';
			for i := 0 to Level - 1 do
				Result := Result + CH_TAB;
		end;

	var
		s: string;
		t: TTreeNode;
	begin
		s := ( GetTabs( tn.Level ) + GetFirstString( [tn.Text, sEmptyNodeText] ) + CH_CRLF );
		fs.WriteBuffer( Pointer( s )^, Length( s ) );
		t := tn.GetFirstChild;
		while CheckObject( t ) do
		begin
			InternalSaveTreeView( fs, t );
			t := t.GetNextSibling;
		end;
	end;

var
	tn: TTreeNode;
	fs: TFileStream;
begin
	ForceObject( TreeView );
	ForceDeleteFile( FileName );
	fs := TFileStream.Create( FileName, fmCreate or fmShareExclusive );
	try
		tn := TreeView.TopItem;
		while CheckObject( tn ) do
		begin
			InternalSaveTreeView( fs, tn );
			tn := tn.GetNextSibling;
		end;
	finally
		fs.Free;
	end;
end;

procedure LoadTreeViewFromTextFile( const FileName: string; TreeView: TCustomTreeView );

	function GetLevel( tn: TTreeNode ): Integer;
	begin
		if CheckObject( tn ) then
			Result := tn.Level
		else
			Result := -1;
	end;

	function FindLastNodeAtLevel( Level: Integer ): TTreeNode;
	begin
		Result := TreeView.TopItem;
		{ Found the node at the correct level and position }
		while CheckObject( Result ) and ( Result.Level <> Level ) do
			Result := Result.GetLastChild;
		{ in the case of the first level, go to the correct (last) position }
		if ( Level = 0 ) then
			while CheckObject( Result.GetNextSibling ) do
				Result := Result.GetNextSibling;
	end;

var
	i,
	j,
	k,
	l: Integer;
	st: TStrings;
	tl: TTreeNode;
begin
	ForceObject( TreeView );
	ForceFile( FileName );
	st := TStringList.Create;
	try
		st.LoadFromFile( FileName );
		TCustomTreeViewHack( TreeView ).Items.BeginUpdate;
		try
			TCustomTreeViewHack( TreeView ).Items.Clear;
			for i := 0 to st.Count - 1 do
			begin
				j := CountTokens( CharRTrim( st[i], CH_TAB ), CH_TAB ); { To avoid right counting Tabs... }
				{ If j is 0, insert a new root node }
				if ( j = 0 ) then
				{ tl := }TCustomTreeViewHack( TreeView ).Items.Add( nil, st[i] )
				{ Otherwise find the parent node for the j level node }
				else
				begin
					k := 0;     { Root nodes level }
					{
						If the file is ok, the node at the level j-1 will be found, otherwise
						the file has more than one tab distance between lines
						(Eg: Lin0;Tab0, Lin1;Tab3)
					}
					tl := FindLastNodeAtLevel( j - 1 );
					if ( not CheckObject( tl ) ) then
						while ( k < j ) do
						begin
							tl := FindLastNodeAtLevel( k );
							l := GetLevel( tl );
							if ( l = ( j - 1 ) ) then { node founded }
								Break
							else if ( l = -1 ) then   { node does not exists }
							begin
								tl := FindLastNodeAtLevel( k - 1 );             { Get the last founded (incorect level) node }
								tl := tl.Owner.AddChild( tl, sEmptyNodeText );  { Add a child within it }
								Inc( k );																				{ Adjust border }
							end
							else                      { node not yet at the correct level }
								Inc( k );
						end;
					ForceObject( tl );
					TCustomTreeViewHack( TreeView ).Items.AddChild( tl, CharTrim( st[i], CH_TAB ) );
				end;
			end;
		finally
			TCustomTreeViewHack( TreeView ).Items.EndUpdate;
		end;
	finally
		st.Free;
	end;
end;

{
--------------------------------------------------------------------------------
-------------------------------- ListView Releted ------------------------------
--------------------------------------------------------------------------------
}

type
  TCustomListViewHack = class( TCustomListView );

procedure SaveListViewToTextFile( const FileName: string; ListView: TCustomListView );
var
	s: string;
	i,
	j: Integer;
	fs: TFileStream;
begin
	ForceObject( ListView );
	ForceDeleteFile( FileName );
	fs := TFileStream.Create( FileName, fmCreate or fmShareExclusive );
	try
		with TCustomListViewHack( ListView ) do
		begin
		  s := '';
			for i := 0 to Columns.Count - 1 do
				s := s + Columns.Items[i].Caption + CH_LIST_TOKEN + IntToStr(
					Columns.Items[i].Width ) + CH_TAB;
			if CheckStr( s ) then
			  System.Delete( s, Length( s ), 1 );		
			fs.WriteBuffer( Pointer( s )^, Length( s ) );
			for i := 0 to Items.Count - 1 do
			begin
				s := StrAdjustRight( Items[i].Caption, Columns.Items[0].Width, CH_SPACE );
				for j := 0 to Min( Items[i].SubItems.Count, Columns.Count - 1 ) - 1 do
					s := s + CH_TAB + StrAdjustRight( Items[i].SubItems[j], Columns.Items[j + 1].Width, CH_SPACE );
				s := s + StrRightPad( '', ( ( Columns.Count - 1 ) - Items[i].SubItems.Count ), CH_TAB );
				fs.WriteBuffer( Pointer( s )^, Length( s ) );
			end;
		end;
	finally
		fs.Free;
	end;
end;	

procedure LoadListViewFromTextFile( const FileName: string; ListView: TCustomListView );
begin
  NotYetImplemented;
end;

{
--------------------------------------------------------------------------------
------------------------- Registration Information -----------------------------
--------------------------------------------------------------------------------
}

{--------------------------- Internal Implementation ---------------------------}

type

	TSignature	 = TUserName;
	TKey				 = TUserName;

{$IFNDEF INTERNAL_VERSION}

	PKInstallInfo = ^TKInstallInfo;
	TKInstallInfo = record
		Signature: TSignature;
		Key: TKey;
	end;

const

(* KLIB100_REGISTRY_SIGNATURE = '{09536FA0-BF69-11D2-B212-00C0DFE081C4}' *)

	KnowHowInstallInfo: TKInstallInfo =
	(
{$IFDEF KLIB100}
		Signature: '{09536FA0-BF69-11D2-B212-00C0DFE081C4}'; { do not resource/const }
{$ELSE}
		Signature: *MUST GENERATE AN ERROR!*;
{$ENDIF}
		Key:
			#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32 +
			#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32 +
			#32#32#32#32#32#32;
	);

{$ENDIF}

function IsKernel_Shareware: Boolean;
begin
{$IFDEF INTERNAL_VERSION}
	Result := false;
{$ELSE}
	Result := ( not CheckRegistryInfo( GetKernelRegistryInfo,
		LongInt( @KnowHowInstallInfo ) - SizeOf( TKInstallInfo ) ) );
{$ENDIF}		
end;

procedure RegisterKernelUnits;
begin
	RegisterRunningPackage( perKernel, $28C0B6C6 ); { do not const... }
	RegisterRunningPackage( pedKernel, $64374B66 );
end;

procedure UnregisterKernelUnits;
begin
	UnregisterRunningPackage( perKernel, $28C0B6C6 ); { do not const... }
	UnregisterRunningPackage( pedKernel, $64374B66 );
end;

{
--------------------------------------------------------------------------------
--------------------- Validating Registration Information ----------------------
--------------------------------------------------------------------------------
}

type

	PKRegistryInfo = ^TKRegistryInfo;
	TKRegistryInfo = record
		Signature: TSignature;
		Key: TKey;
		UserName: TUserName;
		Company: TCompanyName;
	end;

function PackageUserName: string;
begin
	Result := Trim( PKRegistryInfo( GetKernelRegistryInfo + SizeOf( TKRegistryInfo ) ).UserName );
end;

function PackageCompanyName: string;
begin
	Result := Trim( PKRegistryInfo( GetKernelRegistryInfo + SizeOf( TKRegistryInfo ) ).Company );
end;

function PackageVersion: TKLibVersion;
begin
	ZeroMemory( @Result, SizeOf( TKLIBVersion ) );
	Result.Release := StrToDateTime( KERNEL_VER_RELEASE_DATE );
	Result.Version := KERNEL_VER;
	Result.Reserved := KERNEL_VER_INT;
end;

{
--------------------------------------------------------------------------------
------------------- Initialization and Finalization Routines -------------------
--------------------------------------------------------------------------------
}

procedure Init;
begin
	RegisterKernelUnits;
	TestKernelShareWareVersion;
	CreateRegCheckerThread( perKernel );
	RegisterCheckListDialog;
end;

procedure Done;
begin
	UnregisterCheckListDialog;
	UnregisterKernelUnits;
end;

initialization
	Init;

finalization
	Done;

end.
