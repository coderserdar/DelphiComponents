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

unit ukedClasses;

{$I s:\v100\include\iKLIB100.inc}

{$R+}

interface

uses
	Classes, TypInfo, DsgnIntf, uksydClasses, uksydUtils, ukrMessages, ukrClasses,
  ukeClasses, ukedfMsgEnums;

type

{
--------------------------------------------------------------------------------
------------------------- Component Editor Support -----------------------------
--------------------------------------------------------------------------------
}

{ TKStringsArrayComponentEditor }

	TKStringsArrayComponentEditor = class( TKDefaultEditor )
	protected
		function GetComparePropName: string; override;

	end;

{ TKDFMResourceComponentEditor }

	TKDFMResourceComponentEditor = class( TKDefaultEditor )
	public
		procedure ExecuteVerb( Index: Integer ); override;
		function GetVerb( Index: Integer ): string; override;
		function GetVerbCount: Integer; override;

	end;

{ TKDCC32Editor }

	TKDCC32Editor = class( TComponentEditor )
	public
		procedure ExecuteVerb( Index: Integer ); override;
		function GetVerb( Index: Integer ): string; override;
		function GetVerbCount: Integer; override;

	end;

{ TKPropertyExpressionEditor }

	TKPropertyExpressionEditor = class( TComponentEditor )
  private
    FRelIndex: Integer;
    FRegIntMathProps: string;
    FRegFltMathProps: string;

    function GetRegMathProps: string;
    function EnumMathProperty( Index: Integer; EditorClass: TPropertyEditorClass;
      PropInfo: PTypeInfo; PropClass: TClass; const PropName: string;
      Data: Pointer ): Boolean;
    function EnumRelativeProperty( Index: Integer; EditorClass: TPropertyEditorClass;
      PropInfo: PTypeInfo; PropClass: TClass; const PropName: string;
      Data: Pointer ): Boolean;

	public
		constructor Create( AComponent: TComponent; ADesigner:
      {$IFDEF DELPHI4}IFormDesigner{$ELSE}TFormDesigner{$ENDIF} ); override;

		function GetVerbCount: Integer; override;
		procedure ExecuteVerb( Index: Integer ); override;
		function GetVerb( Index: Integer ): string; override;

    property RegMathProps: string
             read GetRegMathProps;

	end;
  
{
--------------------------------------------------------------------------------
-------------------------- Property Editor Support -----------------------------
--------------------------------------------------------------------------------
}

{ TKMsgEnumProperty }

	TKMsgEnumProperty = class( TIntegerProperty )
	public
		function GetAttributes: TPropertyAttributes; override;
		function GetValue: string; override;
		procedure SetValue( const Value: String ); override;
		procedure GetValues( Proc: TGetStrProc ); override;

	end;

{ TKMessagesGroupProperty }

	TKMessagesGroupProperty = class( TKDialogClassProperty )
	private
		procedure SetMsgEnums( const Values: TIdentMsgMapEntryEnums );
		function GetMsgEnums: TIdentMsgMapEntryEnums;

	protected
		function GetAddCaption: string; virtual;
		function EditMsgEnumsSet( var iSetValue: TIdentMsgMapEntryEnums ): Boolean; virtual;

	public
		function GetValue: string; override;
		procedure Edit; override;
		property MsgEnums: TIdentMsgMapEntryEnums
						 read GetMsgEnums write SetMsgEnums;

	end;

{ TKVersionProperty }

	TKVersionProperty = class( TStringProperty )
	public
		function GetAttributes: TPropertyAttributes; override;

	end;

{ TKCompileNumberProperty }

	TKCompileNumberProperty = class( TIntegerProperty )
	public
		function GetAttributes: TPropertyAttributes; override;

	end;

{ TKVersionControlEditor }

	TKVersionControlEditor = class( TComponentEditor )
	public
		constructor Create( AComponent: TComponent; ADesigner: TKFormDesigner ); override;

	end;

{ TKCompileFileNameProperty }

	TKCompileFileNameProperty = class( TKFileNameProperty )
	protected
		function GetFilter: string; override;
		function GetDefExt: string; override;

	end;

{ TKCompileItemOptProperty }

	TKCompileItemOptProperty = class( TPropertyEditor )
	public
		procedure Edit; override;
		function GetValue: string; override;
		function GetAttributes: TPropertyAttributes; override;

	end;

{ TKDFMResourceStreamProperty }

	TKDFMResourceStreamProperty = class( TClassProperty )
	public
		procedure Edit; override;
		function GetValue: string; override;
		function GetAttributes: TPropertyAttributes; override;

	end;

{ TKMathTermProperty }

  TKMathTermProperty = class( TKDialogClassProperty )
  public
    function GetValue: string; override;
    procedure Edit; override;

  end;  

{ TKMathIntegerProperty }

	EKMathIntegerProperty = class( EKSYDClasses );

	TKMathIntegerProperty = class( TKIntegerProperty )
	private
		FMathExpr: string;
    FCompProps: TStrings;
		FMathLexer: TKMathLexer;
		FPropExpr: TKPropertyExpression;
		FOnSolveFunc: TKMathExprSolveFuncEvent;
		FOnSolveIdent: TKMathExprSolveIdentEvent;

		function CheckPropertyExpression: Boolean;
    function EnumMathPropertyEdt( Index: Integer; EditorClass: TPropertyEditorClass;
      PropInfo: PTypeInfo; PropClass: TClass; const PropName: string; Data: Pointer ): Boolean;

	protected
		procedure SolveIdentEvent( Sender: TKMathExpression; const IdentName: string;
			var IdentValue: Extended ); dynamic;
		procedure SolveFuncEvent( Sender: TKMathExpression; const FuncName: string;
			FuncParams: TKExtendedStack; var FuncReturn: Extended ); dynamic;
		procedure SetSolverEvents( OnSolveIdent: TKMathExprSolveIdentEvent;
			OnSolveFunc: TKMathExprSolveFuncEvent );

	public
		destructor Destroy; override;

    procedure Initialize; override;
		function GetValue: string; override;
    function GetAttributes: TPropertyAttributes; override;
    procedure SetValue( const Value: string ); override;
    procedure Edit; override;

	end;

{ TKMathFloatProperty }

	EKMathFloatProperty = class( EKSYDClasses );

	TKMathFloatProperty = class( TKFloatProperty )
	private
		FMathExpr: string;
    FCompProps: TStrings;
		FMathLexer: TKMathLexer;
		FPropExpr: TKPropertyExpression;
		FOnSolveFunc: TKMathExprSolveFuncEvent;
		FOnSolveIdent: TKMathExprSolveIdentEvent;

		function CheckPropertyExpression: Boolean;
    function EnumMathPropertyEdt( Index: Integer; EditorClass: TPropertyEditorClass;
      PropInfo: PTypeInfo; PropClass: TClass; const PropName: string; Data: Pointer ): Boolean;

	protected
		procedure SolveIdentEvent( Sender: TKMathExpression; const IdentName: string;
			var IdentValue: Extended ); dynamic;
		procedure SolveFuncEvent( Sender: TKMathExpression; const FuncName: string;
			FuncParams: TKExtendedStack; var FuncReturn: Extended ); dynamic;
		procedure SetSolverEvents( OnSolveIdent: TKMathExprSolveIdentEvent;
			OnSolveFunc: TKMathExprSolveFuncEvent );

	public
	  destructor Destroy; override;

		procedure Initialize; override;
 		function GetValue: string; override;
    function GetAttributes: TPropertyAttributes; override;
    procedure SetValue( const Value: string ); override;
    procedure Edit; override;

	end;

  TKEnumMathPropertyEditor = function( Index: Integer; EditorClass: TPropertyEditorClass;
    PropInfo: PTypeInfo; PropClass: TClass; const PropName: string;
    Data: Pointer ): Boolean of object;

procedure RegisterMathPropertyEditor( Info: PTypeInfo; AClass: TClass;
  const APropName: string );
function EnumRegMathPropEditors( Data: Pointer; CallBack: TKEnumMathPropertyEditor ): Integer;

implementation

uses
	Windows, SysUtils, Controls, Forms, Dialogs, ToolIntf, EditIntf, ExptIntf,
  uksyConsts, uksyUtils, uksyTypes, ukrResStr, ukeResStr, ukeUtils, ukeConsts,
  ukedConsts, ukedfDCC32Opt, ukedfMathExpr, ukefAddMathExpr;

type

	EKEDClasses = class( EKDExt );

{
--------------------------------------------------------------------------------
------------------------- Component Editor Support -----------------------------
--------------------------------------------------------------------------------
}

{------------------------ TKStringsArrayComponentEditor ------------------------}

function TKStringsArrayComponentEditor.GetComparePropName: string;
begin
	Result := sDefaultStringsArrayPropertyName;
end;

{------------------------- TKDFMResourceComponentEditor ------------------------}

procedure TKDFMResourceComponentEditor.ExecuteVerb( Index: Integer );
var
	s: string;
	fs: TFileStream;
	rs: TKDFMResourceStream;
begin
	case Index of
		0:
		begin
			s := SelectLoadFile( DFM_RES_FILTER, sDFMResComp, ApplicationPath, DFM_RES_DEFEXT );
			if CheckFile( s ) then
			begin
				fs := TFileStream.Create( s, fmOpenRead or fmShareExclusive	);
				try
					rs := TKDFMResourceStream.Create( TKDFMResource( Component ) );
					try
						ForceStreamCopy( fs, rs.Stream );
            rs.Stream.Position := 0;
						TKDFMResource( Component ).Resource := rs;
					finally
						rs.Free;
					end;
				finally
					fs.Free;
				end;
				Designer.Modified;
			end;
		end;
	end;
end;

function TKDFMResourceComponentEditor.GetVerb( Index: Integer ): string;
begin
	case Index of
		0: Result := sDFMResComp;
	else
		Result := '';
	end;
end;

function TKDFMResourceComponentEditor.GetVerbCount: Integer;
begin
	Result := DFM_RES_VERBCOUNT;
end;

{ TKVersionControlEditor }

{--------------------------- Internal Implementation ---------------------------}

type

{ TKIVersionControlAddInNotifier }

	TKIVersionControlAddInNotifier = class( TIAddInNotifier )
	private
		FOwner: TKVersionControl;

	public
		destructor Destroy; override;
		constructor Create( AOwner: TKVersionControl );

		procedure FileNotification( NotifyCode: TFileNotification;
			const FileName: string; var Cancel: Boolean ); override;
		procedure EventNotification( NotifyCode: TEventNotification;
			var Cancel: Boolean ); override;

	end;

	TKVersionControlHack = class( TKVersionControl );

	TKVersionControlSHack = class( TKCustomLinkable )
	private
		FAuxObject: TObject; { Cool!!! }
		FCompileTime: Boolean;

	end;

constructor TKIVersionControlAddInNotifier.Create( AOwner: TKVersionControl );
begin
	ForceObject( AOwner );
	inherited Create;
	FOwner := AOwner;
	if ( not ToolServices.AddNotifierEx( Self ) ) then
		RaiseExceptionFmt( EKEDClasses, sErrVerCtrlInvAddInReg, [FOwner.Name] );
end;

destructor TKIVersionControlAddInNotifier.Destroy;
begin
	if ( not ToolServices.RemoveNotifier( Self ) ) then
		RaiseExceptionFmt( EKEDClasses, sErrVerCtrlInvAddInUnReg, [FOwner.Name] );
	FOwner := nil;
	inherited Destroy;
end;

procedure TKIVersionControlAddInNotifier.FileNotification( NotifyCode: TFileNotification;
	const FileName: string; var Cancel: Boolean );
begin
	{ Do nothing }
end;

procedure TKIVersionControlAddInNotifier.EventNotification( NotifyCode: TEventNotification;
	var Cancel: Boolean );
begin
	if ( NotifyCode = enAfterCompile ) and ( FOwner.Active ) then
		TKVersionControlHack( FOwner ).DoCompileNumberChange;
end;

{---------------------------- Public Implementation ----------------------------}

constructor TKVersionControlEditor.Create( AComponent: TComponent; ADesigner: TKFormDesigner );
begin
	ForceObjectClass( AComponent, TKVersionControl );
	inherited Create( AComponent, ADesigner );
	if ( not CheckObject( TKVersionControlSHack( AComponent ).FAuxObject ) ) then
	begin
		TKVersionControlSHack( Component ).FAuxObject := TKIVersionControlAddInNotifier.Create( ( Component as TKVersionControl ) );
		TKVersionControlSHack( Component ).FCompileTime := True;
	end;
end;

{ TKDCC32Editor }

procedure TKDCC32Editor.ExecuteVerb( Index: Integer );
var
  sFileName: string;
begin
  sFileName := '';
	case Index of
		0: { MergeFromFile }
		begin
			sFileName := SelectLoadFiles( sDCC32Filter, sDCC32MergeTitle,
				( Component as TKDCC32 ).InitialDirectory, sDCC32DefExt, [ofHideReadOnly,
				ofPathMustExist, ofFileMustExist], nil );
			if CheckTrimStr( sFileName ) then
				( Component as TKDCC32 ).MergeFromFile( sFileName );
		end;
		1: { LoadFromFile }
		begin
			sFileName := SelectLoadFiles( sDCC32Filter, sDCC32LoadTitle,
				( Component as TKDCC32 ).InitialDirectory, sDCC32DefExt, [ofHideReadOnly,
				ofPathMustExist, ofFileMustExist], nil );
			if CheckTrimStr( sFileName ) then
				( Component as TKDCC32 ).LoadFromFile( sFileName );
		end;
		2: { SaveToFile }
		begin
			sFileName := SelectSaveFiles( sDCC32Filter, sDCC32SaveTitle,
				( Component as TKDCC32 ).InitialDirectory, sDCC32DefExt, [ofHideReadOnly,
				ofPathMustExist], nil );
			if CheckTrimStr( sFileName ) then
			begin
				if ( CheckFile( sFileName ) and ( not ConfirmFmt( sDCC32ConfirmDel, [sFileName] ) ) ) then
					Exit;
				( Component as TKDCC32 ).SaveToFile( sFileName );
			end;
		end;
	end;
	if CheckTrimStr( sFileName ) then
		Designer.Modified;
end;

function TKDCC32Editor.GetVerb( Index: Integer ): string;
begin
	Result := DCC32_VERB[Index];
end;

function TKDCC32Editor.GetVerbCount: Integer;
begin
	Result := DCC32_VERBCOUNT;
end;

{ TKPropertyExpressionEditor }

{--------------------------- Internal Implementation ---------------------------}

{
	Situtation:

	We have a TKPropertyExpression, that manage and solve (only at run-time)
	math expressions for component property set.
	At Design-Time its function is just a expression manager/references manager.
	The expression evaluation is made by the property editor, that retrieve the
	expression from the manager.

	There is two possible expression management:

	A) Save component reference, property name and expression as strings.
	B) Save component name, property name and expression, all as strings.

	A) Is better because the work of find and catch component reference (difficult)
		 is not needed. But there is a serious problem: COMPONENT DESTRUCTION!
		 (invalid references, no..........)

	B) There is a lot of work to do (need a notifier, etc...) and the component name
		 property name pairs must be parsed and founded, but we can safelly know when
		 components are destructed and/or renamed (another problem) whitin the notifier.
		 The first work (finding) is already done by the property editor, cool!
		 Just create the add in!

	On the fly Property Editor Registration!

	The Component editor can register (but not unregister), property editor for the
	selected component/property name)!

  ----------------

}

type

{ TKIPropertyExpressionModuleNotifier }

	TKIPropertyExpressionModuleNotifier = class( TIModuleNotifier )
	private
		FOwnerName: string;
		FModIntf: TIModuleInterface;
		FOwner: TKPropertyExpression;
		procedure ModuleNotifier( IsRegister: Boolean );
    function GetActiveComponentNames: string;

	public
		destructor Destroy; override;
		constructor Create( AOwner: TKPropertyExpression );

		procedure Notify( NotifyCode: TNotifyCode ); override;
		procedure ComponentRenamed( ComponentHandle: Pointer; const OldName, NewName: string ); override;

	end;

	TKPropertyExpressionHack = class( TKPropertyExpression );

	TKPropertyExpressionSHack = class( TKCustomMathSolver )
	private
		FAuxObject: TObject; { Cool!!! }

	end;

constructor TKIPropertyExpressionModuleNotifier.Create( AOwner: TKPropertyExpression );
begin
//  DebugLogMessageEx( 'c:\teste.not', 'TKIPEMN.Create!' );
	ForceObject( AOwner );
	inherited Create;
	FOwner := AOwner;
	FOwnerName := AOwner.Name;
  FModIntf := ToolServices.GetModuleInterface( ChangeFileExt( ToolServices.GetCurrentFile,
    DELPHI_UNIT_EXT ) );
	ModuleNotifier( True );
end;

destructor TKIPropertyExpressionModuleNotifier.Destroy;
begin
//  DebugLogMessageEx( 'c:\teste.not', 'TKIPEMN.Destroy' );
	ModuleNotifier( False );
  TKPropertyExpressionSHack( FOwner ).FAuxObject := nil;
	FOwner := nil;
  FModIntf.Free;
	inherited Destroy;
end;

procedure TKIPropertyExpressionModuleNotifier.ModuleNotifier( IsRegister: Boolean );

	procedure InternalRaise;
	begin
		case IsRegister of
			True : RaiseExceptionFmt( EKPropertyExpression, sErrDPEInvModReg, [FOwner.Name] );
			False: RaiseExceptionFmt( EKPropertyExpression, sErrDPEInvModUnReg, [FOwner.Name] );
		end;
	end;

begin
	if CheckObject( FModIntf ) then
		case IsRegister of
			True : FModIntf.AddNotifier( Self );
			False: FModIntf.RemoveNotifier( Self );
		end
	else
		InternalRaise;
end;

procedure TKIPropertyExpressionModuleNotifier.Notify( NotifyCode: TNotifyCode );
begin
//  DebugLogMessageEx( 'c:\teste.not', 'TKIPEMN.NotifyCode(' + ExtractFileName(ToolServices.GetCurrentFile)+') = ' + EnumName( Cardinal( NotifyCode ), TypeInfo( TNotifyCode ) ) );
	case NotifyCode of
    ncModuleDeleted:
    begin
      if CheckStrings( FOwner.Expressions )then
        FOwner.RemoveAllExprs( '' ); { to avoid loose synchronization on loading (Alt+F12) }
{
  We need this free here in the case of a Alt+F12 or Ctrl+F4 in the current module
  In the case of a component deletion, the PropertyExpression will delete the
  module itself
}
      Free;
    end;
    ncBeforeSave,
    ncAfterSave,
    ncFormSelected,
    ncFormSaving   : ;
		ncFormModified :
			if CheckStrings( FOwner.Expressions ) then
        if ( FOwner.Locked and ( not FOwner.SolvingAll ) ) then
          FOwner.SolveAll
        else if ( not FOwner.Locked ) then
          FOwner.AdjustRelativeExpr( GetActiveComponentNames );
	end;
end;

procedure TKIPropertyExpressionModuleNotifier.ComponentRenamed( ComponentHandle: Pointer;
	const OldName, NewName: string );
const
{
	 CheckTrimStr( NewName ), CheckTrimStr( OldName )

				False									 False  -> ? Cannot Happen!
				False									 True   -> Comp OldName Deleted
				True									 False  -> Comp NewName Added
				True									 True   -> Comp OldName Renamed to NewName
}

	COMP_ERROR   = 0;
	COMP_DELETED = 1;
	COMP_ADDED   = 2;
	COMP_RENAMED = 3;

	PROPEXPR_COMPFILTER: array[Boolean, Boolean] of Byte = (
		( COMP_ERROR, COMP_DELETED ), ( COMP_ADDED, COMP_RENAMED ) );

begin
//  DebugLogMessageEx( 'c:\teste.not', 'TKIPEMN.CompRenamed (O;N;F) = ' + OldName + ';' + NewName + ';' + IntToStr( PROPEXPR_COMPFILTER[CheckTrimStr( NewName ), CheckTrimStr( OldName )] ) );
	case PROPEXPR_COMPFILTER[CheckTrimStr( NewName ), CheckTrimStr( OldName )] of
    COMP_ERROR   : { do nothing ... Error condition };
    COMP_DELETED :
    { When we have a component deletion, we MUST ensure that there is not the Owner
      (PropertyExpression)! }
      if CheckStrEqual( OldName, FOwnerName ) then
        Notify( ncModuleDeleted )
      else if ( CheckObject( FOwner ) and CheckObject( FOwner.Expressions ) and ( not FOwner.SolvingAll ) ) then
        FOwner.RemoveAllExprs( OldName );
    COMP_ADDED   : { do nothing ... the property editor will add expressions };
    COMP_RENAMED :
    begin
      FOwnerName := NewName;
      FOwner.RenameAllExprs( OldName, NewName )
    end;
  end;
end;

function TKIPropertyExpressionModuleNotifier.GetActiveComponentNames: string;
var
  s: string;
  i, j: Integer;
  fi: TIFormInterface;
  ci: TIComponentInterface;
begin
  Result := '';
  fi := FModIntf.GetFormInterface;
  if CheckObject( fi ) then
    try
      for i := 0 to fi.GetSelCount - 1 do
      begin
        ci := fi.GetSelComponent( i );
        if CheckObject( ci ) then
          try
            for j := 0 to ci.GetPropCount - 1 do
            begin
              s := '';
              if ( CheckStrEqual( ci.GetPropName( j ), 'Name' ) and
                ( ci.GetPropType( j ) = ptLString ) and ci.GetPropValue( j, s ) and
                CheckStr( s ) ) then
              begin
//                DebugLogMessageExFmt( 'c:\teste.not', 'TKIPEMN.GetActiveComp - s = %s; Len = %d', [s, Length( s )] );
                Result := Result + s + CH_LIST_TOKEN;
//                DebugLogMessageExFmt( 'c:\teste.not', 'TKIPEMN.GetActiveComp - Result = %s; Len= %d', [Result, Length( Result )] );
                Break;
              end;
            end;  
          finally
            ci.Free;
          end;
      end;
    finally
      fi.Free;
    end;
end;

{---------------------------- Public Implementation ----------------------------}

var
  FRegPropList: TStringList = nil;

type

{ TKMathPropEdtRegInfo }

  TKMathPropEdtRegInfo = class( TObject )
  private
    FPropName: string;
    FPropType: PTypeInfo;
    FPropClass: TClass;

    function GetPropEdtClass: TPropertyEditorClass;
    constructor Create( Info: PTypeInfo; AClass: TClass; const APropName: string );

  public
    property PropType: PTypeInfo
             read FPropType;
    property PropClass: TClass
             read FPropClass;
    property PropName: string
             read FPropName;
    property PropEdtClass: TPropertyEditorClass
             read GetPropEdtClass;         

  end;
  
{ TKMathPropEdtRegInfo }

constructor TKMathPropEdtRegInfo.Create( Info: PTypeInfo; AClass: TClass;
  const APropName: string );
begin
  ForcePointer( Info );
  inherited Create;
  FPropName := APropName;
  FPropClass := GetFirstClass( [AClass, TObject] );
  FPropType := Info;
end;

function TKMathPropEdtRegInfo.GetPropEdtClass: TPropertyEditorClass;
const
  PROPEDT_CLASS: array[Boolean] of TPropertyEditorClass =
    ( TKMathFloatProperty, TKMathIntegerProperty );
begin
  Result := PROPEDT_CLASS[( FPropType^.Kind = tkInteger )];
end;

{ TKPropertyExpressionEditor }

constructor TKPropertyExpressionEditor.Create( AComponent: TComponent; ADesigner:
  {$IFDEF DELPHI4}IFormDesigner{$ELSE}TFormDesigner{$ENDIF} );
begin
//  DebugLogMessageEx( 'c:\teste.not', 'TKPEEdt.Create' );
	ForceObjectClass( AComponent, TKPropertyExpression );
	inherited Create( AComponent, ADesigner );
  FRelIndex := 0;
  FRegIntMathProps := '';
  FRegFltMathProps := '';
	if ( not CheckObject( TKPropertyExpressionSHack( Component ).FAuxObject ) ) then
		TKPropertyExpressionSHack( Component ).FAuxObject := TKIPropertyExpressionModuleNotifier.Create( ( Component as TKPropertyExpression ) );
end;

function TKPropertyExpressionEditor.GetRegMathProps: string;
begin
  if CheckStr( FRegIntMathProps ) then
    Delete( FRegIntMathProps, Length( FRegIntMathProps ), 2 );
  if CheckStr( FRegFltMathProps ) then
    Delete( FRegFltMathProps, Length( FRegFltMathProps ), 2 );
  Result := sPropExprRegInt + FRegIntMathProps + SPropExprRegFloat + FRegFltMathProps;
end;

function TKPropertyExpressionEditor.EnumMathProperty( Index: Integer;
  EditorClass: TPropertyEditorClass; PropInfo: PTypeInfo; PropClass: TClass;
  const PropName: string; Data: Pointer ): Boolean;
begin
  Result := True;
  if CheckClassReference( EditorClass, TKMathIntegerProperty ) then
    FRegIntMathProps := FRegIntMathProps + PropName + CH_COMMA + CH_SPACE
  else if CheckClassReference( EditorClass, TKMathFloatProperty ) then
    FRegFltMathProps := FRegFltMathProps + PropName + CH_COMMA + CH_SPACE
  else
    Result := False;
end;

function TKPropertyExpressionEditor.EnumRelativeProperty( Index: Integer;
  EditorClass: TPropertyEditorClass; PropInfo: PTypeInfo; PropClass: TClass;
  const PropName: string; Data: Pointer ): Boolean;
begin
  Result := True;
  if CheckTrimStr( PropName ) then
  begin
    PPCharArray( Data )^[FRelIndex] := StrNew( PChar( PropName ) );
    Inc( FRelIndex );
  end
  else if CheckClass( PropClass ) then
  begin
    { Get All Integer or Float published property for PropClass }
  end;
end;

procedure TKPropertyExpressionEditor.ExecuteVerb( Index: Integer );
var
  sl: TStrings;
  bCtrl: Boolean;
  ppn: PPCharArray;
  ctrl: TControl;
	sPropName: string;
begin
	{ Register Property Editor Support }
	case Index of
    0:
      if Confirm( sPropExprRelativeExprs ) then
      begin
        sl := TStringList.Create;
        try
          Index := EnumRegMathPropEditors( Self, nil );
          if ( Index > 0 ) then
          begin
            ppn := AllocMem( SizeOf( PChar ) * Index );
            try
              FRelIndex := 0;
              EnumRegMathPropEditors( ppn, EnumRelativeProperty );
              { Here we should have a very undesired effect: If we have two
                properties with same name but different types, we will have
                a problem! Will base in the Anchor prop name!
                We need RelativeExpressions to have PropNames as an array of
                PChar (instead of string), to use the Slice standard function }
              ctrl := SelectControl( ( Component.Owner as TControl ), nil, False, True, False );
              ForceObject( ctrl );
              bCtrl := Confirm( sPropExprUseCtrlParent );
              RelativeExpressions( Component.Owner, ctrl, TControl, Slice( ppn^, Index ), sl, bCtrl );
              ( Component as TKPropertyExpression ).Expressions := nil; { Close and Clear }
              ( Component as TKPropertyExpression ).Expressions := sl;
              while ( FRelIndex > 0 ) do
              begin
                Dec( FRelIndex );
                StrDispose( ppn^[FRelIndex] );
              end;
              Designer.Modified;
            finally
              FreeMem( ppn, SizeOf( PChar ) * Index );
            end;
          end
          else
            Warn( sPropExprWarnNoRegMathProps );
        finally
          sl.Free;
        end;
      end;
		1:
			if Confirm( sPropExprClearExprs ) then
      begin
				( Component as TKPropertyExpression ).Expressions := nil; { close and clear }
        Designer.Modified;
      end;
    2:
    begin
      ( Component as TKPropertyExpression ).Locked := ( not ( Component as TKPropertyExpression ).Locked );
      Designer.Modified;
    end;
		4:
			if Confirm( sPropExprRegPropEdt ) then
			begin
				sPropName := InputDialogBox( sPropertyExprRegPropEdtVerb, sPropExprRegPropPrompt, '', '' );
				ForceValidIdent( sPropName );
				if Confirm( sPropExprRegPropTypeInfoInt ) then
          RegisterMathPropertyEditor( TypeInfo( Integer ), TControl, sPropName  )
				else
          RegisterMathPropertyEditor( TypeInfo( Extended ), TControl, sPropName  );
				Designer.Modified;
			end;
		5:
		begin
      EnumRegMathPropEditors( Self, EnumMathProperty );
      Inform( RegMathProps );
		end;
	end;
end;

function TKPropertyExpressionEditor.GetVerb( Index: Integer ): string;
begin
	case Index of
    0: Result := sPropertyExprRelativeExprsVerb;
		1: Result := sPropertyExprClearExprsVerb;
    2: Result := LoadResString( PROPEXPR_LOCK_VERB[( not ( Component as TKPropertyExpression ).Locked )] );
    3: Result := '-';
    4: Result := sPropertyExprRegPropEdtVerb;
		5: Result := sPropertyExprRegPropEdtEnumVerb;
	else
		Result := '';
	end;
end;

function TKPropertyExpressionEditor.GetVerbCount: Integer;
begin
	Result := 6;
end;

{
--------------------------------------------------------------------------------
-------------------------- Property Editor Support -----------------------------
--------------------------------------------------------------------------------
}

{------------------------------ TKMsgEnumProperty ------------------------------}

procedure TKMsgEnumProperty.GetValues( Proc: TGetStrProc );
begin
	GetMsgEnumValues( Proc );
end;

procedure TKMsgEnumProperty.SetValue( const Value: String );
begin
	SetOrdValue( Integer( StringToMsgEnum( Value ) ) );
end;

function TKMsgEnumProperty.GetValue: string;
begin
	Result := MsgEnumToString( TIdentMsgMapEntryEnum( GetOrdValue ) );
end;

function TKMsgEnumProperty.GetAttributes: TPropertyAttributes;
begin
	Result := inherited GetAttributes + [paValueList {paMultiSelect, paRevertable!!!}];
end;

{--------------------------- TKMessagesGroupProperty ---------------------------}

function TKMessagesGroupProperty.GetValue: string;
begin
	Result := ( GetComponent( 0 ) as TKMessageSpy ).Messages.MsgEnumsAsString;
end;

procedure TKMessagesGroupProperty.Edit;
var
	iSetValue: TIdentMsgMapEntryEnums;
begin
	iSetValue := MsgEnums;
	if EditMsgEnumsSet( iSetValue ) then
	begin
		MsgEnums := iSetValue;
		Designer.Modified;
	end;
end;

function TKMessagesGroupProperty.GetAddCaption: string;
begin
{ Derived Property Editor classes can use this method for customization pruposes }
	Result := ( GetComponent( 0 ) as TKMessageSpy ).Name;
end;

function TKMessagesGroupProperty.EditMsgEnumsSet( var iSetValue: TIdentMsgMapEntryEnums ): Boolean;
begin
	Result := MsgEnumsEdit( iSetValue, GetAddCaption, imeWM, imeCN );
end;

procedure TKMessagesGroupProperty.SetMsgEnums( const Values: TIdentMsgMapEntryEnums );
begin
	( GetComponent( 0 ) as TKMessageSpy ).Messages.MsgEnums := Values;
end;

function TKMessagesGroupProperty.GetMsgEnums: TIdentMsgMapEntryEnums;
begin
	Result := ( GetComponent( 0 ) as TKMessageSpy ).Messages.MsgEnums;
end;

{ TKVersionProperty }

function TKVersionProperty.GetAttributes: TPropertyAttributes;
begin
	Result := inherited GetAttributes + [paReadOnly];
end;

{ TKCompileNumberProperty }

function TKCompileNumberProperty.GetAttributes: TPropertyAttributes;
begin
	Result := inherited GetAttributes + [paReadOnly];
end;

{ TKCompileFileNameProperty }

function TKCompileFileNameProperty.GetFilter: string;
begin
	Result := sCompFNFilter;
end;

function TKCompileFileNameProperty.GetDefExt: string;
begin
	Result := DELPHI_PROJECT_EXT;
end;

{ TKCompileItemOptProperty }

type

	TKCompileItemsHack = class( TKCompileItems );

function TKCompileItemOptProperty.GetAttributes: TPropertyAttributes;
begin
	Result := inherited GetAttributes - [paMultiSelect] + [paDialog, paReadOnly];
end;

function TKCompileItemOptProperty.GetValue: string;
begin
	Result := '(' + GetName + ')';
end;

procedure TKCompileItemOptProperty.Edit;
var
	ci: TKCompileItem;
begin
{ The ItemOptions is a place holder for the CompileItem options editor.
	The algorithm will consider this item as a new appended item if the name
	containse the DefaultItemName, set by the collection at construction or with a
	explicity confirmation, otherwise the editor will be opened witout assign the
	default values previously saved in to the standard file 'kcidef.dcc'. }
	ci := ( GetComponent( 0 ) as TKCompileItem );
	if EditDCC32ItemOptions( ci, ( CheckStrContains( TKCompileItemsHack(
		ci.Owner ).GetDefaultItemName( ci ), ci.Name ) or ConfirmFmt( sCIConfirmAppend,
		[ci.Name] ) ) ) then
		Modified;
end;

{ TKDFMResourceStreamProperty }

function TKDFMResourceStreamProperty.GetAttributes: TPropertyAttributes;
begin
	Result := inherited GetAttributes - [paSubProperties, paMultiSelect] + [paDialog, paReadOnly];
end;

function TKDFMResourceStreamProperty.GetValue: string;
var
	rs: TKDFMResource;
begin
	rs := ( GetComponent( 0 ) as TKDFMResource );
	if ( rs.Resource.Stream.Size = 0 ) then
		Result := '(Empty)'
	else
	  Result := '(Resource)';
end;

procedure TKDFMResourceStreamProperty.Edit;
var
	s: string;
	fs: TFileStream;
	rs: TKDFMResource;
	rds: TKDFMResourceStream;
begin
	s := SelectLoadFile( DFM_RES_FILTER, sDFMResComp, ApplicationPath, DFM_RES_DEFEXT );
	if CheckFile( s ) then
	begin
		rs := ( GetComponent( 0 ) as TKDFMResource );
		fs := TFileStream.Create( s, fmOpenRead or fmShareExclusive	);
		try
			rds := TKDFMResourceStream.Create( rs );
			try
				ForceStreamCopy( fs, rds.Stream );
				rds.Stream.Position := 0;
				SetOrdValue( LongInt( rds ) );
			finally
				rds.Free;
			end;
		finally
			fs.Free;
		end;
	end;
end;

{ TKMathTermProperty }

type

	TKMathExpressionsHack = class( TKMathExpressions );

function TKMathTermProperty.GetValue: string;
begin
  Result := ( CH_PARENTHESIS_OPEN + GetName + CH_PARENTHESIS_CLOSE );
end;

type
  TKCustomMathSolverHack = class( TKCustomMathSolver );

procedure TKMathTermProperty.Edit;
var
  sl: TStrings;
  isExprs: Boolean;
begin
  sl := TStringList.Create;
  try
    sl.Assign( TStrings( GetOrdValue ) );
    isExprs := CheckStrEqual( GetName, 'Expressions' );
    EditPropExprs( ( GetComponent( 0 ) as TComponent ).Owner,
      ( GetComponent( 0 ) as TKCustomMathSolver ), isExprs, True, True, True, sl );
    with TKCustomMathSolverHack( ( GetComponent( 0 ) as TKCustomMathSolver ) ) do
      if isExprs then
        SetExpressions( sl )
      else
        SetIdentifiers( sl );  
    Modified;
  finally
    sl.Free;
  end;
end;

{ TKMathIntegerProperty }

destructor TKMathIntegerProperty.Destroy;
begin
	FMathLexer.Free;
  FCompProps.Free;
	inherited Destroy;
end;

procedure TKMathIntegerProperty.Initialize;
begin
	FMathExpr := '';
	FPropExpr := nil;
	inherited Initialize;
	FMathLexer := TKMathLexer.CreateDefault;
  FCompProps := TStringList.Create;
  EnumRegMathPropEditors( FCompProps, EnumMathPropertyEdt );
	if ( not CheckPropertyExpression ) then
		SetSolverEvents( SolveIdentEvent, SolveFuncEvent );
end;

function TKMathIntegerProperty.EnumMathPropertyEdt( Index: Integer;
  EditorClass: TPropertyEditorClass; PropInfo: PTypeInfo; PropClass: TClass;
  const PropName: string; Data: Pointer ): Boolean;
begin
  if ( CheckTrimStr( PropName ) and ( TStrings( Data ).IndexOf( PropName ) = -1 ) ) then
    TStrings( Data ).AddObject( PropName, TObject( PropInfo ) );
  Result := True;
end;

function TKMathIntegerProperty.CheckPropertyExpression: Boolean;
var
	i,
	j: Integer;
	c: TComponent;
begin
	Result := False;
	{ The Base Designer (eg Form, QuickRep, does not have owners at design time! }
	c := ( GetComponent( 0 ) as TComponent ).Owner;
	if ( not CheckObject( c ) ) then
		c := ( GetComponent( 0 ) as TComponent ); { so get the base designer itself! }
	if CheckObject( c ) then
		for i := 0 to c.ComponentCount - 1 do
			if CheckObjectClass( c.Components[i], TKPropertyExpression ) then
			begin
				FPropExpr := ( c.Components[i] as TKPropertyExpression );
				SetSolverEvents( TKPropertyExpressionHack( FPropExpr ).SolveIdentEvent,
					TKPropertyExpressionHack( FPropExpr ).SolveFuncEvent );
        FMathLexer.ExprList.DefFuncResult := FPropExpr.DefFuncResult;
        FMathLexer.ExprList.DefIdentResult := FPropExpr.DefIdentResult;
				for j := 0 to FPropExpr.ExpressionCount - 1 do
					if CheckAbsStrContains( ( ( GetComponent( 0 ) as TComponent ).Name + CH_DOTMARK + GetName ),
						TKPropertyExpressionHack( FPropExpr ).Expressions.Names[j] ) then
					begin
						FMathExpr := TKPropertyExpressionHack( FPropExpr ).Expressions.Names[j];
						FMathExpr := TKPropertyExpressionHack( FPropExpr ).Expressions.Values[FMathExpr];
						Break;
					end;
				Result := True;
				Exit;
			end;
end;

procedure TKMathIntegerProperty.SetSolverEvents( OnSolveIdent: TKMathExprSolveIdentEvent;
	OnSolveFunc: TKMathExprSolveFuncEvent );
begin
	FOnSolveIdent := OnSolveIdent;
	FOnSolveFunc := OnSolveFunc;
	TKMathExpressionsHack( FMathLexer.ExprList ).OnSolveIdent := FOnSolveIdent;
	TKMathExpressionsHack( FMathLexer.ExprList ).OnSolveFunc := FOnSolveFunc;
end;

function TKMathIntegerProperty.GetValue: string;
begin
	if CheckTrimStr( FMathExpr ) then
		Result := FMathExpr
	else
		Result := inherited GetValue;
end;

function TKMathIntegerProperty.GetAttributes: TPropertyAttributes;
begin
  Result := ( inherited GetAttributes + [paDialog] );
end;

procedure TKMathIntegerProperty.Edit;
var
  p: TPoint;
  s: string;
begin
  FillChar( p, SizeOf( TPoint ), -1 );
  s := FMathExpr;
  if EditExpression( ( GetComponent( 0 ) as TComponent ).Name + CH_DOTMARK + GetName,
    ( GetComponent( 0 ) as TComponent ).Owner, FCompProps, True, s, @p ) then
    SetValue( s );
end;

procedure TKMathIntegerProperty.SetValue( const Value: string );
var
	i: Integer;
begin
	i := -1;
//  DebugLogMessageEx( 'c:\teste.not', 'TKMIP.SetValue( ' + Value + ')' );
	if ( IsValidString( Value, ceDigit ) or IsValidString( Value, ceHexDigits ) ) then
	begin
		FMathExpr := '';
    if CheckObject( FPropExpr ) then
      FPropExpr.RemoveExpr( ( GetComponent( 0 ) as TComponent ).Name, GetName );
		inherited SetValue( Value );
	end
	else if ( not CheckStrEqual( FMathExpr, Value ) ) then
	begin
		FMathExpr := Value;
		if CheckObject( FPropExpr ) then
			i := FPropExpr.AddExpr( ( GetComponent( 0 ) as TComponent ).Name, GetName, FMathExpr );
	end;
	if CheckTrimStr( FMathExpr ) then
	begin
//		DebugLogMessageEx( 'c:\teste.not', 'TKMIP.SetValue: FMathExpr = ' + FMathExpr );
		try
			if ( i = -1 ) then
			begin
				FMathLexer.SetExpression( FMathExpr );
				FMathLexer.Lexer;
				inherited SetValue( IntToStr( Trunc( FMathLexer.ExprList.NormalExpressions[0].Solve ) ) );
			end
			else if CheckObject( FPropExpr ) then
				FPropExpr.SolveAll; { will set the property via SetXXXProp! }
			Modified;
		except
			FMathExpr := '';
			if ( ( i <> -1 ) and CheckObject( FPropExpr ) ) then
        FPropExpr.RemoveExpr( ( GetComponent( 0 ) as TComponent ).Name, GetName );
      raise;
		end;
	end;
end;

procedure TKMathIntegerProperty.SolveIdentEvent( Sender: TKMathExpression;
	const IdentName: string; var IdentValue: Extended );
var
  pi: PPropInfo;
begin
  pi := GetPropInfo;
  if ( pi^.PropType^^.Kind in PROPEXPR_TYPEKIND_ORDSET ) then
  begin
    { At Design time the Form does not have an owner, so we can use GetFirstObject, but... }
    if CheckObjectClass( GetComponent( 0 ), TForm ) then
      IdentValue := GetCompPathOrdValue( ( GetComponent( 0 ) as TComponent ), IdentName )
    else
      IdentValue := GetCompPathOrdValue( ( GetComponent( 0 ) as TComponent ).Owner, IdentName );
  end
  else
    RaiseExceptionFmt( EKMathIntegerProperty, sErrInvMathIdentType, [IdentName, ( Sender.Index + 1 ),
      Byte( pi^.PropType^^.Kind )] );
end;

procedure TKMathIntegerProperty.SolveFuncEvent( Sender: TKMathExpression;
	const FuncName: string; FuncParams: TKExtendedStack; var FuncReturn: Extended );
begin
	RaiseExceptionFmt( EKMathIntegerProperty, sErrInvMathFunc, [FuncName, ( Sender.Index + 1 )] );
end;

{ TKMathFloatProperty }

destructor TKMathFloatProperty.Destroy;
begin
	FMathLexer.Free;
  FCompProps.Free;
	inherited Destroy;
end;

procedure TKMathFloatProperty.Initialize;
begin
	FMathExpr := '';
	FPropExpr := nil;
	inherited Initialize;
	FMathLexer := TKMathLexer.CreateDefault;
  FCompProps := TStringList.Create;
  EnumRegMathPropEditors( FCompProps, EnumMathPropertyEdt );
	if ( not CheckPropertyExpression ) then
		SetSolverEvents( SolveIdentEvent, SolveFuncEvent );
end;

function TKMathFloatProperty.EnumMathPropertyEdt( Index: Integer;
  EditorClass: TPropertyEditorClass; PropInfo: PTypeInfo; PropClass: TClass;
  const PropName: string; Data: Pointer ): Boolean;
begin
  if ( CheckTrimStr( PropName ) and ( TStrings( Data ).IndexOf( PropName ) = -1 ) ) then
    TStrings( Data ).AddObject( PropName, TObject( PropInfo ) );
  Result := True;
end;

function TKMathFloatProperty.CheckPropertyExpression: Boolean;
var
	i,
	j: Integer;
	c: TComponent;
begin
	Result := False;
	{ The Base Designer (eg Form, QuickRep, does not have owners at design time! }
	c := ( GetComponent( 0 ) as TComponent ).Owner;
	if ( not CheckObject( c ) ) then
		c := ( GetComponent( 0 ) as TComponent ); { so get the base designer itself! }
	if CheckObject( c ) then
		for i := 0 to c.ComponentCount - 1 do
			if CheckObjectClass( c.Components[i], TKPropertyExpression ) then
			begin
				FPropExpr := ( c.Components[i] as TKPropertyExpression );
				SetSolverEvents( TKPropertyExpressionHack( FPropExpr ).SolveIdentEvent,
					TKPropertyExpressionHack( FPropExpr ).SolveFuncEvent );
        FMathLexer.ExprList.DefFuncResult := FPropExpr.DefFuncResult;
        FMathLexer.ExprList.DefIdentResult := FPropExpr.DefIdentResult;
				for j := 0 to FPropExpr.ExpressionCount - 1 do
					if CheckAbsStrContains( ( ( GetComponent( 0 ) as TComponent ).Name + CH_DOTMARK + GetName ),
						TKPropertyExpressionHack( FPropExpr ).Expressions.Names[j] ) then
					begin
						FMathExpr := TKPropertyExpressionHack( FPropExpr ).Expressions.Names[j];
						FMathExpr := TKPropertyExpressionHack( FPropExpr ).Expressions.Values[FMathExpr];
						Break;
					end;
				Result := True;
				Exit;
			end;
end;

procedure TKMathFloatProperty.SetSolverEvents( OnSolveIdent: TKMathExprSolveIdentEvent;
	OnSolveFunc: TKMathExprSolveFuncEvent );
begin
	FOnSolveIdent := OnSolveIdent;
	FOnSolveFunc := OnSolveFunc;
	TKMathExpressionsHack( FMathLexer.ExprList ).OnSolveIdent := FOnSolveIdent;
	TKMathExpressionsHack( FMathLexer.ExprList ).OnSolveFunc := FOnSolveFunc;
end;

function TKMathFloatProperty.GetValue: string;
begin
	if CheckTrimStr( FMathExpr ) then
		Result := FMathExpr
	else
		Result := inherited GetValue;
end;

procedure TKMathFloatProperty.SetValue( const Value: string );
var
	i: Integer;
begin
	i := -1;
//  DebugLogMessageEx( 'c:\teste.not', 'TKMFP.SetValue( ' + Value + ')' );
	if ( IsValidString( Value, ceDigit ) or IsValidString( Value, ceHexDigits ) ) then
	begin
		FMathExpr := '';
    if CheckObject( FPropExpr ) then
      FPropExpr.RemoveExpr( ( GetComponent( 0 ) as TComponent ).Name, GetName );
		inherited SetValue( Value );
	end
	else if ( not CheckStrEqual( FMathExpr, Value ) ) then
	begin
		FMathExpr := Value;
		if CheckObject( FPropExpr ) then
			i := FPropExpr.AddExpr( ( GetComponent( 0 ) as TComponent ).Name, GetName, FMathExpr );
	end;
	if CheckTrimStr( FMathExpr ) then
	begin
//		DebugLogMessageEx( 'c:\teste.not', 'TKMFP.SetValue: FMathExpr = ' + FMathExpr );
		try
			if ( i = -1 ) then
			begin
				FMathLexer.SetExpression( FMathExpr );
				FMathLexer.Lexer;
				inherited SetValue( IntToStr( Trunc( FMathLexer.ExprList.NormalExpressions[0].Solve ) ) );
			end
			else if CheckObject( FPropExpr ) then
				FPropExpr.SolveAll; { will set the property via SetXXXProp! }
			Modified;
		except
			FMathExpr := '';
			if ( ( i <> -1 ) and CheckObject( FPropExpr ) ) then
        FPropExpr.RemoveExpr( ( GetComponent( 0 ) as TComponent ).Name, GetName );
      raise;
		end;
	end;
end;

function TKMathFloatProperty.GetAttributes: TPropertyAttributes;
begin
  Result := ( inherited GetAttributes + [paDialog] );
end;

procedure TKMathFloatProperty.Edit;
var
  p: TPoint;
  s: string;
begin
  FillChar( p, SizeOf( TPoint ), -1 );
  s := FMathExpr;
  if EditExpression( ( GetComponent( 0 ) as TComponent ).Name + CH_DOTMARK + GetName,
    ( GetComponent( 0 ) as TComponent ).Owner, FCompProps, True, s, @p ) then
    SetValue( s );
end;

procedure TKMathFloatProperty.SolveIdentEvent( Sender: TKMathExpression;
	const IdentName: string; var IdentValue: Extended );
var
  pi: PPropInfo;
begin
  pi := GetPropInfo;
  if ( pi^.PropType^^.Kind in PROPEXPR_TYPEKIND_FLTSET ) then
  begin
    { At Design time the Form does not have an owner, so we can use GetFirstObject, but... }
    if CheckObjectClass( GetComponent( 0 ), TForm ) then
      IdentValue := GetCompPathFloatValue( ( GetComponent( 0 ) as TComponent ), IdentName )
    else
      IdentValue := GetCompPathFloatValue( ( GetComponent( 0 ) as TComponent ).Owner, IdentName );
  end
  else
    RaiseExceptionFmt( EKMathFloatProperty, sErrInvMathIdentType, [IdentName, ( Sender.Index + 1 ),
      Byte( pi^.PropType^^.Kind )] );
end;

procedure TKMathFloatProperty.SolveFuncEvent( Sender: TKMathExpression;
	const FuncName: string; FuncParams: TKExtendedStack; var FuncReturn: Extended );
begin
	RaiseExceptionFmt( EKMathFloatProperty, sErrInvMathFunc, [FuncName, ( Sender.Index + 1 )] );
end;

{ Utility Functions }

procedure RegisterMathPropertyEditor( Info: PTypeInfo; AClass: TClass; const APropName: string );
var
  mperi: TKMathPropEdtRegInfo;
begin
  if ( not CheckObject( FRegPropList ) ) then
    FRegPropList := TKMathExprTermStrings.Create;
  if ( not ( CheckPointer( Info ) and ( Info^.Kind in [tkInteger, tkFloat] ) ) ) then
    RaiseExceptionFmt( EKMathSolver, sErrInvMathPropTypeInfo, [APropName] )
  else if ( FRegPropList.IndexOf( IntToStr( Integer( Info^.Kind ) ) +
    CH_LIST_TOKEN + APropName ) = -1 ) then
  begin
    mperi := TKMathPropEdtRegInfo.Create( Info, AClass, APropName );
    FRegPropList.AddObject( IntToStr( Integer( Info^.Kind ) ) + CH_LIST_TOKEN + APropName, mperi );
    RegisterPropertyEditor( Info, AClass, APropName, mperi.PropEdtClass );
  end
  else
    RaiseExceptionFmt( EKMathSolver, sErrInvMathPropAlreadyInst, [APropName,
      EnumName( Cardinal( Info^.Kind ), TypeInfo( TTypeKind ) )] );
end;

function EnumRegMathPropEditors( Data: Pointer; CallBack: TKEnumMathPropertyEditor ): Integer;
var
  i: Integer;          
begin
  if ( not CheckObject( FRegPropList ) ) then
    uksyUtils.RaiseException( EKMathSolver, sErrInvMathPropEnumList );
  if ( not CheckReference( CallBack ) ) then
    Result := FRegPropList.Count
  else
  begin
    Result := 0;
    for i := 0 to FRegPropList.Count - 1 do
      with TKMathPropEdtRegInfo( FRegPropList.Objects[i] ) do
      begin
        if ( not CallBack( i, PropEdtClass, PropType, PropClass, PropName, Data ) ) then
          Exit;
        Inc( Result );
      end;
  end;
end;

{
--------------------------------------------------------------------------------
------------------- Initialization and Finalization Routines -------------------
--------------------------------------------------------------------------------
}

procedure Init;
begin
end;

procedure Done;
begin
	FRegPropList.Free;
end;

initialization
	Init;

finalization
	Done;

end.
