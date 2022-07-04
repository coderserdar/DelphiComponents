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

unit ukeUtils;

{$I s:\v100\include\iKLIB100.inc}

interface

uses
	Classes, uksyUtils, ukrMessages;

type
	
	EKEUtils = class( EKExt );
	EKMessages = class( EKEUtils );

{
--------------------------------------------------------------------------------
------------------------------- Message Related --------------------------------
--------------------------------------------------------------------------------
}

function MsgEnumToIdent( MsgEnum: Integer; var Ident: string ): Boolean;
function IdentToMsgEnum( const Ident: string; var MsgEnum: Integer ): Boolean;
function MsgEnumToString( MsgEnum: TIdentMsgMapEntryEnum ): string;
function StringToMsgEnum( const Source: string ): TIdentMsgMapEntryEnum;

procedure ForceMsgEnumByValue( Value: TIdentMsgMapEntryEnum );
procedure ForceMsgEnumByName( const Value: string );
procedure GetMsgEnumValues( Proc: TGetStrProc );

{
--------------------------------------------------------------------------------
------------------------------ File/Mask Related -------------------------------
--------------------------------------------------------------------------------
}

function MatchesMask( const Filename, Mask: string ): Boolean;

{
--------------------------------------------------------------------------------
-------------------------------- Lexer Related ---------------------------------
--------------------------------------------------------------------------------
}

procedure AnalyseCmdLine( sl: TStrings );
function BuildCmdLineList( sl: TStrings ): string;

procedure RelativeExpressions( AnchorOwner, Anchor: TComponent;
  FilterClass: TComponentClass; const PropNames: array of PChar; Expressions: TStrings;
  Controls: Boolean );

{##NI##}

{
--------------------------------------------------------------------------------
------------------------- Registration Information -----------------------------
--------------------------------------------------------------------------------
}

function IsExt_Shareware: Boolean;

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
	SysUtils, Controls, TypInfo, uksyConsts, uksyTypes, uksyPackReg, ukeResStr,
  ukeConsts, ukePackReg, ukeClasses;

{
--------------------------------------------------------------------------------
------------------------------- Message Related --------------------------------
--------------------------------------------------------------------------------
}

function MsgEnumToIdent( MsgEnum: Integer; var Ident: string ): Boolean;
begin
	Result := ValueBetween( MsgEnum, Low( TIdentMsgMapEntryEnum ),
		High( TIdentMsgMapEntryEnum ), True );
	Result := Result and IntToIdent( MsgEnum, Ident, MsgEnumsMap );
end;

function IdentToMsgEnum( const Ident: string; var MsgEnum: Integer ): Boolean;
begin
	Result := IdentToInt( Ident, MsgEnum, MsgEnumsMap );
	Result := Result and ValueBetween( MsgEnum, Low( TIdentMsgMapEntryEnum ),
		High( TIdentMsgMapEntryEnum ), True );
end;

function MsgEnumToString( MsgEnum: TIdentMsgMapEntryEnum ): string;
begin
	if ( not MsgEnumToIdent( MsgEnum, Result ) ) then
		RaiseExceptionFmt( EKMessages, sMessagesInvValue, [MsgEnum] );
end;

function StringToMsgEnum( const Source: string ): TIdentMsgMapEntryEnum;
var
	iMsgEnum: Integer;
begin
	if ( not IdentToMsgEnum( Source, iMsgEnum ) ) then
		RaiseExceptionFmt( EKMessages, sMessagesInvName, [Source] );
	Result := TIdentMsgMapEntryEnum( iMsgEnum );
end;

procedure ForceMsgEnumByValue( Value: TIdentMsgMapEntryEnum );
begin
	MsgEnumToString( Value );
end;

procedure ForceMsgEnumByName( const Value: string );
begin
	StringToMsgEnum( Value );
end;

procedure GetMsgEnumValues( Proc: TGetStrProc );
var
	i: Integer;
begin
	for i := Low( MsgEnumsMap ) to High( MsgEnumsMap ) - 1 do
		 Proc( MsgEnumsMap[i].Name );
end;

{
--------------------------------------------------------------------------------
------------------------------ File/Mask Related -------------------------------
--------------------------------------------------------------------------------
}

function MatchesMask( const Filename, Mask: string ): Boolean;
var
	CMask: TKMask;
begin
	CMask := TKMask.Create( Mask );
	try
		Result := CMask.Matches( Filename );
	finally
		CMask.Free;
	end;
end;

{
--------------------------------------------------------------------------------
-------------------------------- Lexer Related ---------------------------------
--------------------------------------------------------------------------------
}

procedure AnalyseCmdLine( sl: TStrings );
var
  cl: TKCmdLineLexer;
begin
	ForceObject( sl );
	sl.BeginUpdate;
	try
	  sl.Clear;
		cl := TKCmdLineLexer.CreateDefault;
		try
			cl.Lexer;
			sl.Add( CMDLINE_APPNAME + CH_EQUAL_TOKEN + cl.ApplicationName );
			sl.AddStrings( cl.Commands );
		finally
			cl.Free;
		end;
	finally
		sl.EndUpdate;
	end;
end;

function BuildCmdLineList( sl: TStrings ): string;
begin
	if CheckStrings( sl ) then
	begin
		TrimStrings( sl );
		Result := sl.Text;
		if ( ( Length( Result ) >= Length( CH_CRLF ) ) and ( AnsiLastChar( Result ) = CH_LF ) and
				 ( Result[Length( Result ) - 1] = CH_CR ) ) then
			Delete( Result, Length( Result ) - 1, 2 );
		if CheckStr( Result ) then
			Result := CH_BRACKET_OPEN + Result + CH_BRACKET_CLOSE;
	end
	else
	  Result := '';
end;

{
  We will set the n properties for all components in the anchor owner, relative
  to the anchor component. The properties MUST exists in anchor. All components
  that have those properties will be evaluated. The final result is the relative
  expressions.

  Strategy:
  --------

  C0 = Anchor;
  CN = AnchorOwner.Components[j];

  Expr[k]           > CN.PropNameN=C0.PropNameN+Eval_DeltaN0_PropNameN
  Eval_DeltaN0_PropNameN=CN.PropNameN-C0.PropNameN

}

type

  TKRelativeMathSolverHelper = class( TObject )
  private
    FOwner: TComponent;

  public
    constructor Create( AOwner: TComponent );

    procedure SolveIdentEvent( Sender: TKCustomMathSolver; Expr: TKMathExpression;
      const IdentName: string; var IdentValue: Extended );

    property Owner: TComponent
             read FOwner;
               
  end;

constructor TKRelativeMathSolverHelper.Create( AOwner: TComponent );
begin
  ForceObject( AOwner );
  inherited Create;
  FOwner := AOwner;
end;

procedure TKRelativeMathSolverHelper.SolveIdentEvent( Sender: TKCustomMathSolver;
  Expr: TKMathExpression; const IdentName: string; var IdentValue: Extended );
begin
  IdentValue := GetCompPathOrdValue( Owner, IdentName );
end;

procedure RelativeExpressions( AnchorOwner, Anchor: TComponent;
  FilterClass: TComponentClass; const PropNames: array of PChar;
  Expressions: TStrings; Controls: Boolean );
var
  c: TComponent;
  i, j, iCount: Integer;
  piA, piC: PPropInfo;
  ms: TKMathSolver;
  rmse: TKRelativeMathSolverHelper;
  sAnchor, sName, sDelta, sValue: string;
begin
  if ( not CheckClass( FilterClass ) ) then
    if Controls then
      FilterClass := TControl
    else
      FilterClass := TComponent;
  if ( ( not ( CheckObjects( [AnchorOwner, Anchor, Expressions] ) and
         CheckObjectClass( Anchor, FilterClass ) ) ) or
     ( not ( Controls or ( Anchor.Owner = AnchorOwner ) ) ) or
     ( Controls and ( not ( CheckObjectClass( Anchor, TControl ) and
       CheckObjectClass( AnchorOwner, TWinControl ) and
       ( ( Anchor as TControl ).Parent = ( AnchorOwner as TWinControl ) ) ) ) ) ) then
    RaiseException( EKMathSolver, sErrInvRelativeAnchorParams );
  Expressions.BeginUpdate;
  try
    Expressions.Clear;
    rmse := TKRelativeMathSolverHelper.Create( AnchorOwner );
    try
      ms := TKMathSolver.Create( nil );
      ms.OnSolveIdent := rmse.SolveIdentEvent;
      try
        if Controls then
          iCount := ( AnchorOwner as TWinControl ).ControlCount
        else
          iCount := AnchorOwner.ComponentCount;
        for i := Low( PropNames ) to High( PropNames ) do
        begin
          if ( not IsValidIdent( PropNames[i] ) ) then
            RaiseExceptionFmt( EKMathSolver, sErrInvRelativeAnchorInvProp, [PropNames[i]] );
          piA := GetPropInfo( Anchor.ClassInfo, PropNames[i] );
          sAnchor := Anchor.Name + CH_DOTMARK + PropNames[i];
          if ( not ( CheckPointer( piA ) and ( piA^.PropType^^.Kind in [tkInteger, tkFloat] ) ) ) then
            RaiseExceptionFmt( EKMathSolver, sErrInvRelativeAnchorInvProp, [sAnchor] );
          for j := 0 to iCount - 1 do
          begin
            if Controls then
              c := ( AnchorOwner as TWinControl ).Controls[j]
            else
              c := AnchorOwner.Components[j];
            if ( ( c <> Anchor ) and CheckObjectClass( c, FilterClass ) ) then
            begin
              piC := GetPropInfo( c.ClassInfo, PropNames[i] );
              if ( CheckPointer( piC ) and ( piC^.PropType^^.Kind in [tkInteger, tkFloat] ) ) then
              begin
                if ( not IsValidIdent( c.Name ) ) then
                  RaiseExceptionFmt( EKMathSolver, sErrInvRelativeAnchorOwnerComps, [j, PropNames[i]] );
                sName := c.Name + CH_DOTMARK + PropNames[i];
                sDelta := sName + CH_MINUS_SIGN + sAnchor;
                sValue := sAnchor + CH_PLUS_SIGN + FormatFloat( '0', ms.SolveStrExpr( sDelta ) );
                { ? See TKPropertyExpression.ExecuteVerb(4) ? }
                if ( Expressions.IndexOf( sName + CH_EQUAL_TOKEN + sValue ) = -1 ) then
                  Expressions.Add( sName + CH_EQUAL_TOKEN + sValue );
              end;
            end;
          end;  
        end;
      finally
        ms.Free;
      end;
    finally
      rmse.Free;
    end;
  finally
    Expressions.EndUpdate;
  end;
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

function IsExt_Shareware: Boolean;
begin
{$IFDEF INTERNAL_VERSION}
	Result := false;
{$ELSE}
	Result := ( not CheckRegistryInfo( GetExtRegistryInfo,
		LongInt( @KnowHowInstallInfo ) - SizeOf( TKInstallInfo ) ) );
{$ENDIF}
end;

procedure RegisterExtUnits;
begin
	RegisterRunningPackage( perExt, $9897D72E ); { do not const... }
	RegisterRunningPackage( pedExt, $1B142453 );
end;

procedure UnregisterExtUnits;
begin
	UnregisterRunningPackage( perExt, $9897D72E ); { do not const... }
	UnregisterRunningPackage( pedExt, $1B142453 );
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
	Result := Trim( PKRegistryInfo( GetExtRegistryInfo + SizeOf( TKRegistryInfo ) ).UserName );
end;

function PackageCompanyName: string;
begin
	Result := Trim( PKRegistryInfo( GetExtRegistryInfo + SizeOf( TKRegistryInfo ) ).Company );
end;

function PackageVersion: TKLibVersion;
begin
	ZeroMemory( @Result, SizeOf( TKLIBVersion ) );
	Result.Release := StrToDateTime( EXT_VER_RELEASE_DATE );
	Result.Version := EXT_VER;
	Result.Reserved := EXT_VER_INT;
end;

{
--------------------------------------------------------------------------------
------------------- Initialization and Finalization Routines -------------------
--------------------------------------------------------------------------------
}

procedure Init;
begin
  RegisterExtUnits;
	TestExtShareWareVersion;
	CreateRegCheckerThread( perExt );

{ Integer Constants }
	RegisterIntegerConsts( TypeInfo( TIdentMsgMapEntryEnum ), IdentToMsgEnum, MsgEnumToIdent );
end;

procedure Done;
begin
	UnregisterExtUnits;
end;

initialization
	Init;

finalization
	Done;

end.
