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

unit ukexClasses;

{$I s:\v100\include\iKLIB100.inc}

interface

uses
	Windows, SysUtils, Classes, TypInfo, ExptIntf, ToolIntf, EditIntf, DsgnIntf,
	uksyClasses, uksydUtils, uksydClasses;

type

	EKOTAExperts = class( EKOTA );

{
--------------------------------------------------------------------------------
-------------------------- Generic Expert Classes ------------------------------
--------------------------------------------------------------------------------
}

{ TKCustomFormExpert }

	TKCustomFormExpert = class( TKIAddInExpert )
	private
		fRaise: Boolean;

  {$IFDEF DELPHI4}
  protected
  {$ENDIF}
    procedure MenuClick( Sender: TIMenuItemIntf ); override;

	protected
		procedure ClickExecute( Module: TIModuleInterface;
			Form: TIFormInterface; Data: Pointer ); virtual; abstract;

		property RaiseExceptions: Boolean
						 read fRaise write fRaise;

	end;

{ TKAdjustComponentExpert }

  TKAdjustComponentExpert = class( TKCustomFormExpert )
  private

  {$IFDEF DELPHI4}
  protected
  {$ENDIF}
		procedure ClickExecute( Module: TIModuleInterface; Form: TIFormInterface;
      Data: Pointer ); override;
      
  public
    procedure Initialize; override;    
    
  end;

{
--------------------------------------------------------------------------------
---------------------------- Code Expert Classes -------------------------------
--------------------------------------------------------------------------------
}

{ TKCustomCodeExpert }

	TKCustomCodeExpert = class( TKIAddInExpert )
	private
		fRaise: Boolean;

  {$IFDEF DELPHI4}
  protected
  {$ENDIF}
    procedure MenuClick( Sender: TIMenuItemIntf ); override;

	protected
		procedure ClickExecute( Module: TIModuleInterface;
			Editor: TIEditorInterface; Data: Pointer ); virtual; abstract;

		property RaiseExceptions: Boolean
						 read fRaise write fRaise;

	end;

{ TKCommentExpert }

	TKCommentExpert = class( TKCustomCodeExpert )
	protected
		function CreateReplaceBuffer( const OriginalBuffer: string ): string;
			virtual; stdcall;
		procedure ClickExecute( Module: TIModuleInterface;
			Editor: TIEditorInterface; Data: Pointer );  override;

	public
		procedure Initialize; override;
		
	end;

{
--------------------------------------------------------------------------------
------------------------- Collection Expert Classes ----------------------------
--------------------------------------------------------------------------------
}

	TKICollectionModCreator = class( TKIModuleCreator )
	private
		FCollectionName: string;
		FCollectionItemName: string;
		FCollectionCompName: string;
		FCollectionPropName: string;

	{$IFDEF DELPHI4}
	protected
	{$ENDIF}
		procedure FillModuleSource( StringStream: TKCustomStringStream;
			UnitName, Form, Ancestor: string ); override;

	public
		property CollectionName: string
						 read FCollectionName write FCollectionName;
		property CollectionItemName: string
						 read FCollectionItemName write FCollectionItemName;
		property CollectionCompName: string
						 read FCollectionCompName write FCollectionCompName;
		property CollectionPropName: string
						 read FCollectionPropName write FCollectionPropName;

	end;

{ TKCollectionExpert }

	TKCollectionExpert = class( TKIFormExpert )
	public
    procedure Initialize; override;

  end;	

implementation

uses
	DB, Menus, uksyConsts, uksyUtils, ukexConsts, ukexUtils;

{
--------------------------------------------------------------------------------
-------------------------- Generic Expert Classes ------------------------------
--------------------------------------------------------------------------------
}

{ TKCustomFormExpert }

procedure TKCustomFormExpert.MenuClick( Sender: TIMenuItemIntf );
begin
	ExecuteCurrentFormInterfaceByMethod( ClickExecute, RaiseExceptions, nil );
end;

{ TKAdjustComponentExpert }

procedure TKAdjustComponentExpert.Initialize;
begin
  inherited Initialize;
	Name := sAdjustCompName;
  RaiseExceptions := True;
end;

procedure TKAdjustComponentExpert.ClickExecute( Module: TIModuleInterface;
  Form: TIFormInterface; Data: Pointer );
var
  ci: TIComponentInterface;
begin
  ci := Form.GetFormComponent;
  if CheckObject( ci ) then
    try
      AdjustComponents( ( TObject( ci.GetComponentHandle ) as TComponent ),
        Point( 1, 1 ), 75, [TField, TMenuItem] );
      SaveModuleInterface( Form.FileName, True, True );
    finally
      ci.Free;
    end
  else if RaiseExceptions then
    RaiseException( EKOTAExperts, sErrInvFormComp );   
end;

{
--------------------------------------------------------------------------------
---------------------------- Code Expert Classes -------------------------------
--------------------------------------------------------------------------------
}

{ TKCustomCodeExpert }

procedure TKCustomCodeExpert.MenuClick( Sender: TIMenuItemIntf );
begin
	ExecuteCurrentEditorInterfaceByMethod( ClickExecute, RaiseExceptions, nil );
end;

{ TKCommentExpert }

const
	NO_COMMENTED           = 0;
	SPECIAL_COMMENTED      = 1;
	SQUAREBRAKET_COMMENTED = 2;
	ALREADY_COMMENTED      = 3;

procedure TKCommentExpert.Initialize;
begin
  inherited Initialize;
	Name := sCommentName;
  RaiseExceptions := True;
end;

function TKCommentExpert.CreateReplaceBuffer( const OriginalBuffer: string ): string;

  (* function ValidatePos( Position: Integer ): Integer;
  var
		s1,
    s2: string;
		iPos: Integer;
  begin
		Result := Position;
    Exit;
    s1 := Copy( OriginalBuffer, 1, Position ); {Get First Part from Begin to CommentPos }
    s1 := StrRScan( PChar( s1 ), '''' );       {Get First string part if existst }
    s2 := Copy( OriginalBuffer, Position+1,
      Length( OriginalBuffer )-Position+1 );     {Get Last Part from CommentPos to End }
    iPos := Pos( '''', s2 );
    if ( iPos > 0 ) then
      s2 := Copy( s2, 1, iPos )                {Get Last string part if existst }
    else
      s2 := '';
    if ( Trim( s1 ) <> '' ) or ( Trim( s2 ) <> '' ) then
      Result := -1;
	end; *)

const
	{
		iPos1 > 0 ?   iPos2 > 0?
			False         False   » NO_COMMENTED           (0)
			False         True    » SPECIAL_COMMENTED      (1)
      True          False   » SQUAREBRAKET_COMMENTED (2)
      True          True    » ALREADY_COMMENTED      (3)
  }
  Filter: array[Boolean, Boolean] of ShortInt = ( ( NO_COMMENTED,
    SPECIAL_COMMENTED ), ( SQUAREBRAKET_COMMENTED, ALREADY_COMMENTED ) );
var
  iPos1,
	iPos2: Integer;
begin
	Result := OriginalBuffer;
	iPos1 := Pos( '{', OriginalBuffer );
	iPos2 := Pos( '(*', OriginalBuffer );
	case Filter[( iPos1 > 0 ), ( iPos2 > 0 )] of
		NO_COMMENTED,
		SPECIAL_COMMENTED      : Result := '{'#13#10' '+Result+#13#10'}'; //'{ '#9+sCECopyRight+#13#10'  '+Result+#13#10' }';
		SQUAREBRAKET_COMMENTED : Result := '(*'#13#10' '+Result+#13#10'*)'; //'(* '#9+sCECopyRight+#13#10'  '+Result+#13#10' *)';
		ALREADY_COMMENTED      :
		begin
			Result := ' // ' + Result;
			Result := StringReplace( Result, #13#10, #13#10' // ', krfAll );
		{ Insert( '// ', Result, 1 ); }
		end;
		else
			RaiseException( EKOTAExperts, sErrCannotComment );
	end;
end;

procedure TKCommentExpert.ClickExecute( Module: TIModuleInterface;
  Editor: TIEditorInterface; Data: Pointer ); 
var
  View: TIEditView;
  StartPos, EndPos: LongInt;
  CharPos: TCharPos;
  EditPos: TEditPos;
  Reader: TKIEditReaderStream;
	OriginalBuffer,
	ReplacedBuffer: string;
begin
	if ( CompareBlocks( Editor.BlockStart, Editor.BlockAfter ) > 0 ) then
    if ( Editor.GetViewCount = 1 ) then
    begin
       View := Editor.GetView(0);
       if ( View <> nil ) then
        try
          { Get the cursor position, which is where the expert will insert text.
            (Start position of the edition...)}
          StartPos := View.CharPosToPos( Editor.BlockStart );
          EndPos := View.CharPosToPos( Editor.BlockAfter );
          Reader := TKIEditReaderStream.Create( Editor );
          try
            SetLength( OriginalBuffer, EndPos-StartPos );
            Reader.Seek( StartPos, soFromBeginning );
            Reader.ReadBuffer( Pointer( OriginalBuffer )^, EndPos-StartPos );
            ReplacedBuffer := CreateReplaceBuffer( OriginalBuffer );
					finally
            Reader.Free;
					end;
          ReplaceSelection( Editor, 0, ReplacedBuffer, False );
					{Editor.BlockVisible := False;}
          CharPos := Editor.BlockAfter;
          View.ConvertPos( False, EditPos, CharPos );
          View.CursorPos := EditPos;
        finally
          View.Free;
        end
      else if RaiseExceptions then
				RaiseExceptionFmt( EKOTAExperts, sErrInvViewIntf, [ToolServices.GetCurrentFile] );
    end
    else if RaiseExceptions then
			RaiseExceptionFmt( EKOTAExperts, sErrInvEditorIntFViewCount, [ToolServices.GetCurrentFile,
			  Editor.GetViewCount] )
  else if RaiseExceptions then
    RaiseException( EKOTAExperts, sErrInvEditorBlock );
end;

{
--------------------------------------------------------------------------------
------------------------- Collection Expert Classes ----------------------------
--------------------------------------------------------------------------------
}

{--------------------------- Internal Implementation ---------------------------}

const
	COLLECTION_TEMPLATE_WITH_COMMENTS =
	'unit %0:s;'#13#10+
	#13#10+
	'interface'#13#10+
	#13#10+
	'uses'#13#10+
	'  Classes, uksyClasses, uksyUtils;'#13#10+
	#13#10+
	'type '#13#10+
	#13#10+
	'  %1:s = class( %7:s );'#13#10+
	#13#10+
	'  %2:s = class;'#13#10+
	'  %3:s = class;'#13#10+
	'  %4:s = class; '#13#10+
	#13#10+
	'{ %2:s }'#13#10+
	#13#10+
	'  %2:s = class( TKCustomCollectionItem )'#13#10+
	'  private'#13#10+
	'    { User defined private fields/methods }'#13#10+
	#13#10+
	'    { Default overrideen protected methods as private }'#13#10+
	'    function GetOwnerCollection: %3:s;'#13#10+
	#13#10+
	'  protected '#13#10+
	'    { User defined protected methods } '#13#10#13#10+
	'  public '#13#10+
	'    { Default overrideen public methods/properties }'#13#10+
	'    constructor Create( ACollection: TCollection ); override;'#13#10+
	#13#10+
	'    procedure Assign( Source: TPersistent ); override;'#13#10+
	'    function Equals( Item: TKCustomCollectionItem ): Boolean; override; '#13#10+
	#13#10+
	'    property Owner: %3:s '#13#10+
	'             read GetOwnerCollection;'#13#10+
	#13#10+
	'    { User defined public methods/properties }'#13#10+
	#13#10+
	'  published '#13#10+
	'    { Default published properties }'#13#10+
	'    property Name;'#13#10+
	#13#10+
	'    { User defined published properties } '#13#10+
	#13#10+
	'  end;'#13#10+
	#13#10+
	'{ %3:s }'#13#10+
	#13#10+
	'  %3:s = class( TKCustomCollection ) '#13#10+
	'  private'#13#10+
	'    { User defined private fields/methods }'#13#10+
	#13#10+
	'    { Default overrideen protected methods as private }'#13#10+
	'    function GetOwnerComp: %4:s;'#13#10+
	#13#10+
	'    procedure SetItem( Index: Integer; AItem: %2:s ); '#13#10+
	'    function GetItem( Index: Integer ): %2:s;'#13#10+
	#13#10+
	'    function GetItemByName( const AName: string ): %2:s; '#13#10+
	#13#10+
	'  protected '#13#10+
	'    procedure Update( Item: TCollectionItem ); override;'#13#10+
	#13#10+
	'    property Component: %4:s '#13#10+
	'             read GetOwnerComp;'#13#10+
	#13#10+
	'  public '#13#10+
	'    { Default overrideen public methods/properties }'#13#10+
	#13#10+
	'    constructor Create( AComp: %4:s ); virtual;'#13#10+
	#13#10+
	'    function Add: %2:s; virtual; '#13#10+
	'    property Items[Index: Integer]: %2:s'#13#10+
	'             read GetItem write SetItem; default;'#13#10+
	'    property ItemByName[const AName: string]: %2:s '#13#10+
	'             read GetItemByName; '#13#10+
	'    property Names;'#13#10+
	#13#10+
	'    { User defined public methods/properties }'#13#10+
	#13#10+
	'  end;'#13#10+
	#13#10+
	'{ %4:s }'#13#10+
	#13#10+
	'  %4:s = class( %5:s ) '#13#10+
	'  private'#13#10+
	'    { User defined private fields/methods }'#13#10+
	#13#10+
	'    { Default private fields/methods } '#13#10+
	'    F%6:s: %3:s;'#13#10+
	'    procedure Set%6:s( Value: %3:s );'#13#10+
	#13#10+
	'  protected'#13#10+
	'    procedure UpdateItem( Item: %2:s ); virtual;'#13#10+
	#13#10+
	'  public '#13#10+
	'    { Default implemented public methods }'#13#10+
	'    constructor Create( AOwner: TComponent ); override;'#13#10+
	'    destructor Destroy; override;'#13#10+
	#13#10+
	'  published '#13#10+
	'    { Default implemented published properties }'#13#10+
	'    property %6:s: %3:s'#13#10+
	'             read F%6:s write Set%6:s; '#13#10+
	#13#10+
	'  end;'#13#10+
	#13#10+
	'implementation'#13#10+
	#13#10+
	'uses '#13#10+
	'  SysUtils;'#13#10+
	#13#10+
	'{ %2:s }'#13#10+
	#13#10+
	'constructor %2:s.Create( ACollection: TCollection ); '#13#10+
	'begin'#13#10+
	'	 ForceObjectClass( ACollection, %3:s );'#13#10+
	'	 inherited Create( ACollection );'#13#10+
	'	 ForceObject( Owner.Component );'#13#10+
	'end; '#13#10+
	#13#10+
	'procedure %2:s.Assign( Source: TPersistent );'#13#10+
	'begin'#13#10+
	'  inherited Assign( Source );'#13#10+
	'  if CheckObjectClass( Source, %2:s ) then'#13#10+
	'  begin'#13#10+
	'{ '#13#10+
	' Put here specific assignment support'#13#10+
	'} '#13#10+
	'  end;'#13#10+
	'end; '#13#10+
	#13#10+
	'function %2:s.Equals( Item: TKCustomCollectionItem ): Boolean;'#13#10+
	'begin'#13#10+
	'{ '#13#10+
	'  Put here specific Equal testing dependly of the properties you add and'#13#10+
	'  want to compare with.'#13#10+
	'} '#13#10+
	'  Result := ( inherited Equals( Item ) ) { and ... };'#13#10+
	'end; '#13#10+
	#13#10+
	'function %2:s.GetOwnerCollection: %3:s;'#13#10+
	'begin'#13#10+
	'  Result := %3:s( inherited GetOwnerCollection ); '#13#10+
	'end; '#13#10+
	#13#10+
	'{ '#13#10+
	#13#10+
	'Put here specific method implementations....'#13#10+
	#13#10+
	'} '#13#10+
	#13#10+
	'{ %3:s } '#13#10+
	#13#10+
	'constructor %3:s.Create( AComp: %4:s );'#13#10+
	'begin'#13#10+
	'  ForceObject( AComp );'#13#10+
	'	 inherited Create( AComp, %2:s, False ); '#13#10+
	'end; '#13#10+
	#13#10+
	'function %3:s.Add: %2:s;'#13#10+
	'begin'#13#10+
	'  Result := %2:s( inherited Add ); '#13#10+
	'end; '#13#10+
	#13#10+
	'function %3:s.GetItem( Index: Integer ): %2:s;'#13#10+
	'begin'#13#10+
	'  Result := %2:s( inherited GetItem( Index ) );'#13#10+
	'end; '#13#10+
	#13#10+
	'function %3:s.GetItemByName( const AName: string ): %2:s;'#13#10+
	'begin'#13#10+
	'	 Result := %2:s( inherited GetItemByName( AName ) ); '#13#10+
	'end; '#13#10+
	#13#10+
	'function %3:s.GetOwnerComp: %4:s;'#13#10+
	'begin'#13#10+
	'  Result := %4:s( inherited GetOwnerComp ); '#13#10+
	'end; '#13#10+
	#13#10+
	'procedure %3:s.SetItem( Index: Integer; AItem: %2:s );'#13#10+
	'begin'#13#10+
	'  inherited SetItem( Index, AItem );'#13#10+
	'end; '#13#10+
	#13#10+
	'procedure %3:s.Update( Item: TCollectionItem ); '#13#10+
	'var'#13#10+
	'  i: Integer;'#13#10+
	'begin'#13#10+
	'  if CheckObject( Item ) then'#13#10+
	'    Component.UpdateItem( %2:s( Item ) ) '#13#10+
	'  else'#13#10+
	'    for i := 0 to Count - 1 do'#13#10+
	'      Component.UpdateItem( Items[i] );'#13#10+
	'end; '#13#10+
	#13#10+
	'{ '#13#10+
	#13#10+
	'Put here specific method implementations....'#13#10+
	#13#10+
	'} '#13#10+
	#13#10+
	'{ %4:s }'#13#10+
	#13#10+
	'constructor %4:s.Create( AOwner: TComponent );'#13#10+
	'begin'#13#10+
	'  inherited Create( AOwner );'#13#10+
	'  F%6:s := %3:s.Create( Self );'#13#10+
	'end; '#13#10+
	#13#10+
	'destructor %4:s.Destroy; '#13#10+
	'begin'#13#10+
	'  FreeClean( F%6:s ); '#13#10+
	'  inherited Destroy;'#13#10+
	'end; '#13#10+
	#13#10+
	'procedure %4:s.Set%6:s( Value: %3:s );'#13#10+
	'begin'#13#10+
	'  F%6:s.Assign( Value );'#13#10+
	'end;'#13#10+
	#13#10+
	'procedure %4:s.UpdateItem( Item: %2:s );'#13#10+
	'begin'#13#10+
	'  { Default update item code }'#13#10+
	'end; '#13#10+
	#13#10+
	'{ '#13#10+
	#13#10+
	'Put here specific method implementations....'#13#10+
	#13#10+
	'} '#13#10+
	#13#10+
	'end.'#0;

	COLLECTION_TEMPLATE =
	'unit %0:s;'#13#10+
	#13#10+
	'interface'#13#10+
	#13#10+
	'uses'#13#10+
	'  Classes, uksyClasses, uksyUtils;'#13#10+
	#13#10+
	'type '#13#10+
	#13#10+
	'  %1:s = class( %7:s );'#13#10+
	#13#10+
	'  %2:s = class;'#13#10+
	'  %3:s = class;'#13#10+
	'  %4:s = class;'#13#10+
	#13#10+
	'{ %2:s }'#13#10+
	#13#10+
	'  %2:s = class( TKCustomCollectionItem )'#13#10+
	'  private'#13#10+
	'    function GetOwnerCollection: %3:s;'#13#10+
	#13#10+
	'  protected '#13#10+
	#13#10+
	'  public '#13#10+
	'    constructor Create( ACollection: TCollection ); override;'#13#10+
	#13#10+
	'    procedure Assign( Source: TPersistent ); override;'#13#10+
	'    function Equals( Item: TKCustomCollectionItem ): Boolean; override; '#13#10+
	#13#10+
	'    property Owner: %3:s '#13#10+
	'             read GetOwnerCollection;'#13#10+
	#13#10+
	'  published'#13#10+
	'    property Name;'#13#10+
	#13#10+
	'  end;'#13#10+
	#13#10+
	'{ %3:s }'#13#10+
	#13#10+
	'  %3:s = class( TKCustomCollection ) '#13#10+
	'  private'#13#10+
	'    function GetOwnerComp: %4:s;'#13#10+
	#13#10+
	'    procedure SetItem( Index: Integer; AItem: %2:s ); '#13#10+
	'    function GetItem( Index: Integer ): %2:s;'#13#10+
	#13#10+
	'    function GetItemByName( const AName: string ): %2:s; '#13#10+
	#13#10+
	'  protected'#13#10+
	'    procedure Update( Item: TCollectionItem ); override;'#13#10+
	#13#10+
	'    property Component: %4:s '#13#10+
	'             read GetOwnerComp;'#13#10+
	#13#10+
	'  public'#13#10+
	'    constructor Create( AComp: %4:s ); virtual;'#13#10+
	#13#10+
	'    function Add: %2:s; virtual; '#13#10+
	'    property Items[Index: Integer]: %2:s'#13#10+
	'             read GetItem write SetItem; default;'#13#10+
	'    property ItemByName[const AName: string]: %2:s '#13#10+
	'             read GetItemByName; '#13#10+
	'    property Names;'#13#10+
	#13#10+
	'  end;'#13#10+
	#13#10+
	'{ %4:s }'#13#10+
	#13#10+
	'  %4:s = class( %5:s )'#13#10+
	'  private'#13#10+
	'    F%6:s: %3:s;'#13#10+
	'    procedure Set%6:s( Value: %3:s );'#13#10+
	#13#10+
	'  protected'#13#10+
	'    procedure UpdateItem( Item: %2:s ); virtual;'#13#10+
	#13#10+
	'  public '#13#10+
	'    constructor Create( AOwner: TComponent ); override;'#13#10+
	'    destructor Destroy; override;'#13#10+
	#13#10+
	'  published'#13#10+
	'    property %6:s: %3:s'#13#10+
	'             read F%6:s write Set%6:s; '#13#10+
	#13#10+
	'  end;'#13#10+
	#13#10+
	'implementation'#13#10+
	#13#10+
	'uses '#13#10+
	'  SysUtils;'#13#10+
	#13#10+
	'{ %2:s }'#13#10+
	#13#10+
	'constructor %2:s.Create( ACollection: TCollection ); '#13#10+
	'begin'#13#10+
	'  ForceObjectClass( ACollection, %3:s );'#13#10+
	'  inherited Create( ACollection );'#13#10+
	'  ForceObject( Owner.Component );'#13#10+
	'end; '#13#10+
	#13#10+
	'procedure %2:s.Assign( Source: TPersistent );'#13#10+
	'begin'#13#10+
	'  inherited Assign( Source );'#13#10+
	'  if CheckObjectClass( Source, %2:s ) then'#13#10+
	'  begin'#13#10+
	'  end;'#13#10+
	'end; '#13#10+
	#13#10+
	'function %2:s.Equals( Item: TKCustomCollectionItem ): Boolean;'#13#10+
	'begin'#13#10+
	'  Result := ( inherited Equals( Item ) ) { and ... };'#13#10+
	'end; '#13#10+
	#13#10+
	'function %2:s.GetOwnerCollection: %3:s;'#13#10+
	'begin'#13#10+
	'  Result := %3:s( inherited GetOwnerCollection ); '#13#10+
	'end; '#13#10+
	#13#10+
	'{ %3:s } '#13#10+
	#13#10+
	'constructor %3:s.Create( AComp: %4:s );'#13#10+
	'begin'#13#10+
	'  ForceObject( AComp );'#13#10+
	'  inherited Create( AComp, %2:s, False ); '#13#10+
	'end; '#13#10+
	#13#10+
	'function %3:s.Add: %2:s;'#13#10+
	'begin'#13#10+
	'  Result := %2:s( inherited Add ); '#13#10+
	'end; '#13#10+
	#13#10+
	'function %3:s.GetItem( Index: Integer ): %2:s;'#13#10+
	'begin'#13#10+
	'  Result := %2:s( inherited GetItem( Index ) );'#13#10+
	'end; '#13#10+
	#13#10+
	'function %3:s.GetItemByName( const AName: string ): %2:s;'#13#10+
	'begin'#13#10+
	'  Result := %2:s( inherited GetItemByName( AName ) ); '#13#10+
	'end; '#13#10+
	#13#10+
	'function %3:s.GetOwnerComp: %4:s;'#13#10+
	'begin'#13#10+
	'  Result := %4:s( inherited GetOwnerComp ); '#13#10+
	'end; '#13#10+
	#13#10+
	'procedure %3:s.SetItem( Index: Integer; AItem: %2:s );'#13#10+
	'begin'#13#10+
	'  inherited SetItem( Index, AItem );'#13#10+
	'end; '#13#10+
	#13#10+
	'procedure %3:s.Update( Item: TCollectionItem ); '#13#10+
	'var'#13#10+
	'  i: Integer;'#13#10+
	'begin'#13#10+
	'  if CheckObject( Item ) then'#13#10+
	'    Component.UpdateItem( %2:s( Item ) ) '#13#10+
	'  else'#13#10+
	'    for i := 0 to Count - 1 do'#13#10+
	'      Component.UpdateItem( Items[i] );'#13#10+
	'end; '#13#10+
	#13#10+
	'{ %4:s }'#13#10+
	#13#10+
	'constructor %4:s.Create( AOwner: TComponent );'#13#10+
	'begin'#13#10+
	'  inherited Create( AOwner );'#13#10+
	'  F%6:s := %3:s.Create( Self );'#13#10+
	'end; '#13#10+
	#13#10+
	'destructor %4:s.Destroy; '#13#10+
	'begin'#13#10+
	'  FreeClean( %6:s );'#13#10+
	'  inherited Destroy;'#13#10+
	'end; '#13#10+
	#13#10+
	'procedure %4:s.Set%6:s( Value: %3:s );'#13#10+
	'begin'#13#10+
	'  F%6:s.Assign( Value );'#13#10+
	'end;'#13#10+
	#13#10+
	'procedure %4:s.UpdateItem( Item: %2:s );'#13#10+
	'begin'#13#10#13#10+
	'end; '#13#10+
	#13#10+
	'end.'#0;

	COLLECTION_UNIT_NAME   = 'uksyCollection';
	COLLECTION_EXCEPT_NAME = 'EKCollection';
  COLLECTION_EXCEPT_CLASS = 'EKSsytem';
	COLLECTION_ITEM_NAME   = 'TKCollectionItem';
	COLLECTION_NAME        = 'TKCollection';
	COLLECTION_COMP_NAME   = 'TKComponent';
	COLLECTION_PROP_NAME   = 'Collection';
	COLLECTION_COMP_ANCESTOR = 'TComponent';

{---------------------------- Public Implementation ----------------------------}

{ TKICollectionModCreator }

procedure TKICollectionModCreator.FillModuleSource( StringStream: TKCustomStringStream;
	UnitName, Form, Ancestor: string );
const
	OVERHEAD = ( 4 * KB );
	WICH_TEMPLATE: array[Boolean] of string =
		( COLLECTION_TEMPLATE, COLLECTION_TEMPLATE_WITH_COMMENTS );
var
	s,
	sExptName,
  sExptCls,
	sItemName,
	sColName,
	sCompName,
	sPropName,
	sCompAncestor,
	sTemplate: string;
	ss: TStrings;
begin
{ Do not call the inherited method - avoid default module source... }
	ss := TStringList.Create;
	try
		GetAllRegisteredComponents( ss );
		sExptName := COLLECTION_EXCEPT_NAME;
    sExptCls := COLLECTION_EXCEPT_CLASS;
		sItemName := COLLECTION_ITEM_NAME;
		sColName := COLLECTION_NAME;
		sCompName := COLLECTION_COMP_NAME;
		sPropName := COLLECTION_PROP_NAME;
		sTemplate := WICH_TEMPLATE[Confirm( sColTempWichTemplate )];
		InputDialog( sCollectionTemplateTitle, sColTempSelExptName, '', sExptName );
    InputDialog( sCollectionTemplateTitle, sColTempSelExptCls, '', sExptCls );
		InputDialog( sCollectionTemplateTitle, sColTempSelItemName, '', sItemName );
		InputDialog( sCollectionTemplateTitle, sColTempSelColName, '', sColName );
		InputDialog( sCollectionTemplateTitle, sColTempSelCompName, '', sCompName );
		InputDialog( sCollectionTemplateTitle, sColTempSelPropName, '', sPropName );
		sExptName := GetFirstString( [sExptName, COLLECTION_EXCEPT_NAME] );
    sExptCls  := GetFirstString( [sExptCls, COLLECTION_EXCEPT_CLASS] );
		sItemName := GetFirstString( [sItemName, COLLECTION_ITEM_NAME] );
		sColName := GetFirstString( [sColName, COLLECTION_NAME] );
		sCompName := GetFirstString( [sCompName, COLLECTION_COMP_NAME] );
		sPropName := GetFirstString( [sPropName, COLLECTION_PROP_NAME] );
		sCompAncestor := GetFirstString( [InputListDialogEx( sCollectionTemplateTitle,
			sColTempSelAncestorClassName, ss.IndexOf( COLLECTION_COMP_ANCESTOR ), True, ss ),
			COLLECTION_COMP_ANCESTOR] );
	finally
		ss.Free;
	end;
	SetLength( s, Length( sTemplate ) + OVERHEAD );
	UnitName := GetFirstString( [UnitName, COLLECTION_UNIT_NAME] );
	s := StrFmt( PChar( s ), PChar( sTemplate ), [UnitName, sExptName, sItemName,
		sColName, sCompName, sCompAncestor, sPropName, sExptCls] );
	StringStream.WriteString( s );
end;

{ TKCollectionExpert }

procedure TKCollectionExpert.Initialize;
begin
	inherited Initialize;
	Name := sColExpertName;
	Comment := sColExpertComment;
end;

end.

