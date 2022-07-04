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

unit ukdbdInternal;

{$I s:\v100\include\iKLIB100.inc}

interface

{##NI##}

uses
	Messages, Forms, DsgnIntf, CnColEdt, uksydClasses, uksydInternal, ukdbdClasses;

{
--------------------------------------------------------------------------------
--------------------------- Generic Hack Classes -------------------------------
--------------------------------------------------------------------------------
}

type

	TKCheckConstraintsProperty = class( TCheckConstraintsProperty )
	private
		FChanged: Boolean;
		FPropHack: TKPropertyEditor;

	public
		destructor Destroy; override;

		procedure Edit; override;
		procedure Initialize; override;

	end;

{ TKFieldsProperty }

	TKFieldsProperty = class( TKDialogClassProperty )
	public
		procedure Edit; override;

	end;

{ TKQueryItemNameProperty }

	TKQueryItemNameProperty = class( TComponentNameProperty )
	public
		procedure SetValue( const Value: string ); override;

	end;

{ TKQueryParamsProperty }

	TKQueryParamsProperty = class( TKQueryItemParamsProperty )
	public
		procedure Edit; override;

	end;

{ TKUpdateObjectProperty }

	TKPUpdateObjectProperty = class( TClassProperty )
	public
		function GetAttributes: TPropertyAttributes; override;
		procedure Edit; override;

	end;

{
	This register procedure must be here because we have in this unit hacked
	property editors that can only be compiled into a package but not used by
	any other unit.
}

{##NI##}

procedure Register;

{ Registering internal and hacked property editors... }

implementation

uses
	Windows, SysUtils, Classes, Controls, StdCtrls, ComCtrls, TypInfo, DB, DBTables,
	ExptIntf, EditIntf, DsgnWnds, {$IFDEF DELPHI4}ParamDlg{$ELSE}Qbinddlg{$ENDIF},
	ColnEdit, UpdSqlEd, DSDesign, uksyUtils, uksydUtils, ukrClasses, ukdbUtils,
	ukdbTables, ukdbScript;

{
--------------------------------------------------------------------------------
--------------------------- Generic Hack Classes -------------------------------
--------------------------------------------------------------------------------
}

{-------------------------- Internal Implementation ----------------------------}

type

	PDsgnWndConstraintHack = ^TDsgnWndConstraintHack;
	TDsgnWndConstraintHack = record
		Owner: TKQueryItem;
		Designer: TKFormDesigner;
		ListItemIndex: Integer;
		DsgnColForm: TCollectionEditor;
		DsgnWndForm: TDesignWindow;
		DsgnWndOldOnClose: TCloseEvent;
	end;

	TConstraintDsgnWndHack = class( TObject )
	private
		FList: TList;

		function GetDsgnWndConstraintHack( Index: Integer ): TDsgnWndConstraintHack;

	public
		destructor Destroy; override;

		procedure FormClose( Sender: TObject; var Action: TCloseAction );
		function IndexOfDsgnWnd( AForm: TDesignWindow ): Integer;
		function Add( Item: PDsgnWndConstraintHack ): Integer;
		procedure Delete( Index: Integer );
		property Items[Index: Integer]: TDsgnWndConstraintHack
						 read GetDsgnWndConstraintHack;

	end;

const
	CONSTRAINTS_PROPERTY = 'Constraints';

function dwh: TConstraintDsgnWndHack; forward;

destructor TConstraintDsgnWndHack.Destroy;
begin
	while CheckList( FList ) do
		Self.Delete( FList.Count - 1 );
	FList.Free;
	inherited Destroy;
end;

function TConstraintDsgnWndHack.GetDsgnWndConstraintHack( Index: Integer ): TDsgnWndConstraintHack;
begin
	ZeroMemory( @Result, SizeOf( TDsgnWndConstraintHack ) );
	if CheckObject( FList ) then
		Result := PDsgnWndConstraintHack( FList.Items[Index] )^;
end;

function TConstraintDsgnWndHack.IndexOfDsgnWnd( AForm: TDesignWindow ): Integer;
begin
	if ( CheckList( FList ) and CheckObject( AForm ) ) then
		for Result := 0 to FList.Count - 1 do
			if ( Items[Result].DsgnWndForm = AForm ) then
				Exit;
	Result := -1;
end;

procedure TConstraintDsgnWndHack.Delete( Index: Integer );
begin
	if CheckObject( FList ) then
	begin
		Dispose( PDsgnWndConstraintHack( FList.Items[Index] ) );
		FList.Delete( Index );
	end;
end;

function TConstraintDsgnWndHack.Add( Item: PDsgnWndConstraintHack ): Integer;
begin
	if ( not CheckObject( FList ) ) then
		FList := TList.Create;
	ForcePointer( Item );
	Result := FList.Add( Item );
end;

procedure TConstraintDsgnWndHack.FormClose( Sender: TObject; var Action: TCloseAction );
var
	i,
	k: Integer;
begin
	i := IndexOfDsgnWnd( TDesignWindow( Sender ) );
	if ( i <> -1 ) then
	begin
		with Items[i] do
		begin
			if Assigned( DsgnWndOldOnClose ) then
				DsgnWndOldOnClose( Sender, Action );
			DsgnWndForm.OnClose := DsgnWndOldOnClose;
			if ( CheckObject( DsgnColForm ) and DsgnColForm.Visible ) then
				with DsgnColForm do
				begin
					SetSelection;
					Show;
					for k := 0 to ListView1.Items.Count - 1 do
						ListView1.Items.Item[k].Selected := ( k = ListItemIndex );
				end
			else
				Designer.SelectComponent( Owner.Owner.Component ); { SQLScript }
		end;
		Delete( i );
	end;
end;

{---------------------------- Public Implementation ----------------------------}

{------------------------- TKCheckConstraintsProperty --------------------------}

destructor TKCheckConstraintsProperty.Destroy;
begin
	FPropHack.Free;
	inherited Destroy;
end;

procedure TKCheckConstraintsProperty.Initialize;
begin
	FChanged := false;
	inherited Initialize;
	FPropHack := TKPropertyEditor.CreateFrom( Self );
end;

procedure TKCheckConstraintsProperty.Edit;
var
	i,
	j: Integer;
	pi: PPropInfo;
	ip: TInstProp;
	pwch: PDsgnWndConstraintHack;
begin
	if ( not FChanged ) then
		with FPropHack do
		begin
			ip := PropList^[0];
			PropList^[0].Instance := TKQueryItem( PropList^[0].Instance ).Query;
			pi := TypInfo.GetPropInfo( TQuery.ClassInfo, CONSTRAINTS_PROPERTY );
			PropList^[0].PropInfo := pi;
			FChanged := true;
		end;
	inherited Edit;
	j := 0;
	pwch := New( PDsgnWndConstraintHack );
	try
		ZeroMemory( pwch, SizeOf( TDsgnWndConstraintHack ) );
		pwch^.Owner := ( ip.Instance as TKQueryItem );
{ This is the Proxy Form Designer! }
		pwch^.Designer := ( ( pwch^.Owner.Owner.Component.Owner as TCustomForm ).Designer as TKFormDesigner );
		for i := 0 to Screen.FormCount - 1 do
			if CheckObjectClass( Screen.Forms[i], TConstraintEditor ) and
				 CheckStrContains( pwch^.Owner.QueryName + '.', Screen.Forms[i].Caption ) then
			begin
				pwch^.DsgnWndForm := ( Screen.Forms[i] as TDesignWindow );
				pwch^.DsgnWndOldOnClose := pwch^.DsgnWndForm.OnClose;
				pwch^.DsgnWndForm.OnClose := dwh.FormClose;
			end
			else if CheckObjectClass( Screen.Forms[i], TCollectionEditor ) and
				 CheckStrContains( pwch^.Owner.Owner.GetNamePath, Screen.Forms[i].Caption ) then
			begin
				pwch^.DsgnColForm := ( Screen.Forms[i] as TCollectionEditor );
				pwch^.ListItemIndex := pwch^.DsgnColForm.ListView1.Selected.Index;
			end;
		j := dwh.IndexOfDsgnWnd( pwch^.DsgnWndForm );
		if ( j = -1 ) then
			dwh.Add( pwch );
	finally
		if ( j <> -1 ) then
			Dispose( pwch );
	end;
end;

{-------------------------------- TKFieldsProperty -------------------------------}

{-------------------------- Internal Implementation ----------------------------}

type

	TDSEHack = class( TDataSetEditor );

	TQuerySHack = class( TPersistent )
	private
		FOwner: TComponent;

	end;

	PDsgnWndFieldsEditorHack = ^TDsgnWndFieldsEditorHack;
	TDsgnWndFieldsEditorHack = record
		Owner: TKQueryItem;
		OldQueryOwner: TComponent;
		Designer: TKFormDesigner;
		ListItemIndex: Integer;
		DsgnColForm: TCollectionEditor;
		DsgnWndForm: TDesignWindow;
		DsgnListBox: TListBox;
		DsgnWndOldOnClose: TCloseEvent;
		DsgnWndOldOnDestroy: TNotifyEvent;
	end;

	TFieldsEditorDsgnWndHack = class( TObject )
	private
		FList: TList;

		function GetDsgnWndFieldsEditorHack( Index: Integer ): TDsgnWndFieldsEditorHack;

	protected
		procedure ResyncFieldsData( Index: Integer );
		procedure SelectColEditor( Index: Integer );

		procedure FormDestroy( Sender: TObject );
		procedure FormClose( Sender: TObject; var Action: TCloseAction );
		procedure HookProc( var Message: TMessage; var HookInfo: TKHookInfo );
		procedure LB_HookProc( var Message: TMessage; var HookInfo: TKHookInfo );

	public
		destructor Destroy; override;

		procedure Delete( Index: Integer );
		function Add( Item: PDsgnWndFieldsEditorHack ): Integer;
		function IndexOfDsgnWnd( AForm: TDesignWindow ): Integer;
		function IndexOfQueryItem( AQItem: TKQueryItem ): Integer;

		property Items[Index: Integer]: TDsgnWndFieldsEditorHack
						 read GetDsgnWndFieldsEditorHack;

	end;

	TFieldsEditorNotifier = class( TIModuleNotifier )
	private
		FDesigner: TKFormDesigner;
		FModIntf: TIModuleInterface;

	public
		constructor Create( Designer: TKFormDesigner; ModIntf: TIModuleInterface );
		destructor Destroy; override;

		procedure Notify( NotifyCode: TNotifyCode ); override;
		procedure ComponentRenamed( ComponentHandle: Pointer;
			const OldName, NewName: string ); override;

	end;

const
	FIELDSDATA_PROPERTY = 'Fields';

function snl: TStringList; forward;
function fwh: TFieldsEditorDsgnWndHack; forward;

destructor TFieldsEditorDsgnWndHack.Destroy;
begin
	while CheckList( FList ) do
		Self.Delete( FList.Count - 1 );
	FList.Free;
	inherited Destroy;
end;

function TFieldsEditorDsgnWndHack.GetDsgnWndFieldsEditorHack( Index: Integer ): TDsgnWndFieldsEditorHack;
begin
	ZeroMemory( @Result, SizeOf( TDsgnWndFieldsEditorHack ) );
	if CheckObject( FList ) then
		Result := PDsgnWndFieldsEditorHack( FList.Items[Index] )^;
end;

function TFieldsEditorDsgnWndHack.IndexOfDsgnWnd( AForm: TDesignWindow ): Integer;
begin
	if ( CheckList( FList ) and CheckObject( AForm ) ) then
		for Result := 0 to FList.Count - 1 do
			if ( Items[Result].DsgnWndForm = AForm ) then
				Exit;
	Result := -1;
end;

function TFieldsEditorDsgnWndHack.IndexOfQueryItem( AQItem: TKQueryItem ): Integer;
begin
	if ( CheckList( FList ) and CheckObject( AQItem ) ) then
		for Result := 0 to FList.Count - 1 do
			if Items[Result].Owner.Equals( AQItem ) then
				Exit;
	Result := -1;
end;

procedure TFieldsEditorDsgnWndHack.Delete( Index: Integer );
begin
	if CheckObject( FList ) then
	begin
		UnhookWindow( MakeHookData( Items[Index].DsgnListBox.Handle, LB_HookProc, LB_HookProc ) );
		UnhookWindow( MakeHookData( Items[Index].DsgnWndForm.Handle, HookProc, HookProc ) );
		Dispose( PDsgnWndFieldsEditorHack( FList.Items[Index] ) );
		FList.Delete( Index );
	end;
end;

function TFieldsEditorDsgnWndHack.Add( Item: PDsgnWndFieldsEditorHack ): Integer;
begin
	if ( not CheckObject( FList ) ) then
		FList := TList.Create;
	ForcePointer( Item );
	Result := FList.Add( Item );
end;

procedure TFieldsEditorDsgnWndHack.ResyncFieldsData( Index: Integer );
var
	pi: PPropInfo;
begin
	with Items[Index] do
	begin
		TQuerySHack( Owner.Query ).FOwner := OldQueryOwner;
		pi := GetPropInfo( Owner.ClassInfo, FIELDSDATA_PROPERTY );
		if CheckPointer( pi ) then
			SetOrdProp( Owner, pi, LongInt( Owner.FieldsData ) );
		Designer.Modified;
	end;
end;

procedure TFieldsEditorDsgnWndHack.HookProc( var Message: TMessage; var HookInfo: TKHookInfo );
var
	i,
	j: Integer;
begin
	if ( not HookInfo.IsHookBefore ) then
	begin
		j := -1;
		for i := 0 to FList.Count - 1 do
			if ( Items[i].DsgnWndForm.Handle = HookInfo.Handle ) then
			begin
				j := i;
				Break;
			end;
		if ( j <> -1 ) then
			with Message, Items[j] do
				case Msg of
					WM_SETCURSOR:
						DsgnWndForm.Caption := Owner.Owner.Component.Name + '.' + Owner.Name;
					WM_ACTIVATE:
						TDSEHack( DsgnWndForm ).SetSelection( nil );
				end;
	end;
end;

procedure TFieldsEditorDsgnWndHack.LB_HookProc( var Message: TMessage; var HookInfo: TKHookInfo );
var
	i,
	j: Integer;
begin
	if ( not HookInfo.IsHookBefore ) then
	begin
		j := -1;
		for i := 0 to FList.Count - 1 do
			if ( Items[i].DsgnListBox.Handle = HookInfo.Handle ) then
			begin
				j := i;
				Break;
			end;
		if ( j <> -1 ) then
			with Message, Items[j] do
				case Msg of
					WM_PAINT:
						if ( DsgnListBox.Items.Count = 0 ) then
						begin
							TDSEHack( DsgnWndForm ).SetSelection( nil );
							DsgnWndForm.Caption := Owner.Owner.Component.Name + '.' + Owner.Name;
						end;
				end;
	end;
end;

procedure TFieldsEditorDsgnWndHack.FormDestroy( Sender: TObject );
var
	i: Integer;
begin
	i := IndexOfDsgnWnd( TDesignWindow( Sender ) );
	if ( i <> -1 ) then
		with Items[i] do
		begin
			Delete( i );
			if Assigned( DsgnWndOldOnDestroy ) then
				DsgnWndOldOnDestroy( Sender );
		end;
end;

procedure TFieldsEditorDsgnWndHack.SelectColEditor( Index: Integer );
var
	k: Integer;
begin
	with Items[Index] do
	begin
		if ( CheckObject( DsgnColForm ) and DsgnColForm.Visible ) then
			with DsgnColForm do
			begin
				SetSelection;
				Show;
				for k := 0 to ListView1.Items.Count - 1 do
					ListView1.Items.Item[k].Selected := ( k = ListItemIndex );
			end
		else
			Designer.SelectComponent( Owner.Owner.Component ); { SQLScript }
	end;
end;

procedure TFieldsEditorDsgnWndHack.FormClose( Sender: TObject; var Action: TCloseAction );
var
	i: Integer;
begin
	i := IndexOfDsgnWnd( TDesignWindow( Sender ) );
	if ( i <> -1 ) then
		with Items[i] do
			if ( Action <> caFree ) then
			begin
				if Assigned( DsgnWndOldOnClose ) then
					DsgnWndOldOnClose( Sender, Action );
				DsgnWndForm.OnClose := DsgnWndOldOnClose;
				ResyncFieldsData( i );
				SelectColEditor( i );
//				Delete( i );
			end
			else
			begin
				ResyncFieldsData( i );
				Delete( i );
				if Assigned( DsgnWndOldOnClose ) then
					DsgnWndOldOnClose( Sender, Action );
			end;
end;

{ TFieldsEditorNotifier }

constructor TFieldsEditorNotifier.Create( Designer: TKFormDesigner;
	ModIntf: TIModuleInterface );
begin
	inherited Create;
	ForceObject( ModIntf );
	ForceDesigner( Designer );
	FModIntf := ModIntf;
	FModIntf.AddRef;
	FModIntf.AddNotifier( Self );
	FDesigner := Designer;
end;

destructor TFieldsEditorNotifier.Destroy;
var
	i: Integer;
begin
	FDesigner := nil;
	if CheckObject( FModIntf ) then
		FModIntf.RemoveNotifier( Self );
	FModIntf.Release;
	i := snl.IndexOfObject( Self );
	if ( i <> -1 ) then
		snl.Delete( i );
	inherited Destroy;
end;

procedure TFieldsEditorNotifier.Notify( NotifyCode: TNotifyCode );

	procedure ResyncFieldsEditorData;
	var
		i: Integer;
	begin
		if CheckObject( fwh ) then
			for i := fwh.FList.Count - 1 downto 0 do { Uh! Hard Duet! }
				if ( fwh.Items[i].Designer = FDesigner ) then
					if ( NotifyCode = ncFormSaving ) then
						fwh.ResyncFieldsData( i )
					else
						fwh.Delete( i );
	end;

begin
	case NotifyCode of
		ncModuleDeleted:
		begin
			ResyncFieldsEditorData;
			Free;
		end;
		ncFormSaving:
		  ResyncFieldsEditorData;
	end;
end;

procedure TFieldsEditorNotifier.ComponentRenamed( ComponentHandle: Pointer;
	const OldName, NewName: string );
begin
end;

{---------------------------- Public Implementation ----------------------------}

procedure TKFieldsProperty.Edit;
var
	i,
	j: Integer;
	sModName: string;
	mi: TIModuleInterface;
	pwch: PDsgnWndFieldsEditorHack;
begin
	pwch := nil;
	j := fwh.IndexOfQueryItem( ( GetComponent( 0 ) as TKQueryItem ) );
	if ( j = -1 ) then
	begin
		pwch := New( PDsgnWndFieldsEditorHack );
		ZeroMemory( pwch, SizeOf( TDsgnWndFieldsEditorHack ) );
		pwch^.Owner := ( GetComponent( 0 ) as TKQueryItem );
		ForceObject( pwch^.Owner.Query.Owner );
		pwch^.OldQueryOwner := pwch^.Owner.Query.Owner;
		TQuerySHack( pwch^.Owner.Query ).FOwner := pwch^.Owner.Owner.Component.Owner;
	end;
	ShowDataSetDesigner( Designer, ( GetComponent( 0 ) as TKQueryItem ).Query );
	if ( j <> -1 ) then
		with fwh.Items[j] do
		begin
			DsgnWndForm.Caption := Owner.Owner.Component.Name + '.' + Owner.Name;
			TDSEHack( DsgnWndForm ).SetSelection( nil );
		end
	else if ( j = -1 ) then
	begin
		ForcePointer( pwch );
{ This is the Proxy Form Designer! Cool!!! Phweeeee!! }
{ pwch^.QueryItem.QueryCollection.SQLScript.Owner }
		pwch^.Designer := ( ( pwch^.Owner.Owner.Component.Owner as TCustomForm ).Designer as TKFormDesigner );
		for i := 0 to Screen.FormCount - 1 do
			if CheckObjectClass( Screen.Forms[i], TDataSetEditor ) and
				 CheckStrEqual( pwch^.Owner.Query.Owner.Name + '.' +
					 pwch^.Owner.QueryName, Screen.Forms[i].Caption ) then
			begin
				pwch^.DsgnWndForm := ( Screen.Forms[i] as TDesignWindow );

				pwch^.DsgnWndOldOnClose := pwch^.DsgnWndForm.OnClose;
				pwch^.DsgnWndOldOnDestroy := pwch^.DsgnWndForm.OnDestroy;
				pwch^.DsgnWndForm.OnClose := fwh.FormClose;
				pwch^.DsgnWndForm.OnDestroy := fwh.FormDestroy;
				pwch^.DsgnListBox := TDataSetEditor( pwch^.DsgnWndForm ).FieldListBox;
				pwch^.DsgnWndForm.Caption := pwch^.Owner.Owner.Component.Name + '.' + pwch^.Owner.Name;
				TDSEHack( pwch^.DsgnWndForm ).SetSelection( nil );
				HookWindow( MakeHookData( pwch^.DsgnWndForm.Handle, fwh.HookProc, fwh.HookProc ) );
				HookWindow( MakeHookData( pwch^.DsgnListBox.Handle, fwh.LB_HookProc, fwh.LB_HookProc ) );
			end
			else if CheckObjectClass( Screen.Forms[i], TCollectionEditor ) and
				 CheckStrContains( pwch^.Owner.Owner.GetNamePath, Screen.Forms[i].Caption ) then
			begin
				pwch^.DsgnColForm := ( Screen.Forms[i] as TCollectionEditor );
				pwch^.ListItemIndex := pwch^.DsgnColForm.ListView1.Selected.Index;
			end;
		fwh.Add( pwch );
		with ToolServices do
		begin
			sModName := ChangeFileExt( GetCurrentFile, '.pas' );
			mi := GetModuleInterface( sModName );
		end;
		if CheckObject( mi ) then
			try
				if ( snl.IndexOf( sModName ) = -1 ) then
					snl.AddObject( sModName, TFieldsEditorNotifier.Create( pwch^.Designer, mi ) );
			finally
				mi.Free;
			end;
	end;
end;

{---------------------------- TKQueryParamsProperty ----------------------------}

procedure TKQueryParamsProperty.Edit;
{$IFDEF DELPHI4}
begin
	inherited Edit;
end;
{$ELSE}
var
	List: TParams;
	Query: TQuery;
begin
	Query := ( GetComponent( 0 ) as TKQueryItem ).Query;
	ForceObject( Query.Owner );
	List := TParams.Create;
	try
		List.Assign( Query.Params );
		if ( EditQueryParams( Query, List ) and ( not List.IsEqual( Query.Params ) ) ) then
		begin
			Modified;
			Query.Close;
			Query.Params := List;
		end;
	finally
		List.Free;
	end;
end;
{$ENDIF}

{--------------------------- TKQueryItemNameProperty ---------------------------}

procedure TKQueryItemNameProperty.SetValue( const Value: string );
var
	i: Integer;
begin
	inherited SetValue( Value );
	if CheckObject( fwh.FList ) then
		for i := 0 to fwh.FList.Count - 1 do
			with fwh.Items[i] do
				DsgnWndForm.Caption := Owner.Owner.Component.Name + '.' + Owner.Name;
end;

{--------------------------- TKPUpdateObjectProperty ---------------------------}

function TKPUpdateObjectProperty.GetAttributes: TPropertyAttributes;
begin
	Result := ( inherited GetAttributes ) + [paDialog];
end;

procedure TKPUpdateObjectProperty.Edit;
var
	us: TUpdateSQL;
begin
	us := TKQueryItem( GetComponent( 0 ) ).UpdateObject.UpdateSQL;
	if EditUpdateSQL( us ) then
		Designer.Modified;
end;

{ Register Procedure }

procedure Register;
begin

{ SQLScript Property Editors }
	RegisterPropertyEditor( TypeInfo( TParams ), TKQueryItem, 'Params', TKQueryParamsProperty );
	RegisterPropertyEditor( TypeInfo( TKPUpdateObject ), TKQueryItem, 'UpdateObject', TKPUpdateObjectProperty );
	RegisterPropertyEditor( TypeInfo( TCheckConstraints ), TKQueryItem, 'Constraints', TKCheckConstraintsProperty );
	RegisterPropertyEditor( TypeInfo( TKFieldsData ), TKQueryItem, 'FieldsData', TKFieldsProperty );
	RegisterPropertyEditor( TypeInfo( string ), TKQueryItem, 'Name', TKQueryItemNameProperty );

end;

{
--------------------------------------------------------------------------------
------------------- Initialization and Finalization Routines -------------------
--------------------------------------------------------------------------------
}

var
	_snl: TStringList = nil;
	_dwh: TConstraintDsgnWndHack = nil;
	_fwh: TFieldsEditorDsgnWndHack = nil;

function snl: TStringList;
begin
	if ( not CheckObject( _snl ) ) then
		_snl := TStringList.Create;
	Result := _snl;
end;

function dwh: TConstraintDsgnWndHack;
begin
	if ( not CheckObject( _dwh ) ) then
		_dwh := TConstraintDsgnWndHack.Create;
	Result := _dwh;
end;

function fwh: TFieldsEditorDsgnWndHack;
begin
	if ( not CheckObject( _fwh ) ) then
		_fwh := TFieldsEditorDsgnWndHack.Create;
	Result := _fwh;
end;

procedure Init;
begin
	_snl := TStringList.Create;
	_dwh := TConstraintDsgnWndHack.Create;
	_fwh := TFieldsEditorDsgnWndHack.Create;
end;

procedure Done;
begin
	_dwh.Free;
	_fwh.Free;
{ do not delete the item in sl- the destructor of the object will do it for you }
	while CheckStrings( _snl ) do
		_snl.Objects[_snl.Count - 1].Free;
	_snl.Free;
end;

initialization
	Init;

finalization
	Done;

end.
