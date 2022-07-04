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

unit ukfCtrls;

{$I s:\v100\include\iKLIB100.inc}

interface

uses
	Windows, Messages, Classes, Controls, Forms, DB, uksyClasses, ukrClasses,
	ukrDBCtrls, ukfClasses,	ukfConsts;

type

{ TKFormDBNavigator }

	TKFormDBNavigator = class( TKCustomDBNavigator )
	public
		property AvailableButtons;

	published
		// property Align;
		property DragCursor;
		property DragMode;
		property Enabled;
		property Flat;
		property Ctl3D;
		property Hints;
		property ParentCtl3D;
		property ParentShowHint;
		property PopupMenu;
		property ShowHint;
		property TabOrder;
		property TabStop;
		property Visible;

	{$IFDEF DELPHI4}
		property Anchors;
		property Constraints;
		property DragKind;
	{$ENDIF}

		property OnDblClick;
		property OnDragDrop;
		property OnDragOver;
		property OnEndDrag;
		property OnEnter;
		property OnExit;
		property OnResize;
		property OnStartDrag;

	{$IFDEF DELPHI4}
		property OnStartDock;
		property OnEndDock;
	{$ENDIF}

		property DataSource;
		property NavButtons;
		property NavEvents;
		property NavMode;
		property Synchronization;

		property AfterAction;
		property BeforeAction;
		property OnPrint;
		property OnSearch;

	end;

	TKForm = class;
	TKChild = class;
	TKFormClass = class of TKForm;
	TKChildClass = class of TKChild;

{

	TKForm

	This is a general purpose form substitution. Use TKForm anywhere
	you would normally use a TForm.

	Properties:

		About: String
			. The text to display in the system menu for a custom about
				menu item. Will work when the application is minimized too.
				This text will display only if AboutCmd is true;

		AboutCmd: Boolean
			. If true, the string in the About property will be used to
				display an extra menu item in the system menu.

		CloseAction: TCloseAction
			. This property allows then programmer to set the default
				close behaviour of the form. Notice that the value choosen
				for the CloseAction property will be the value of the
				Action parameter of the FormClose event handler.

		ConfirmClose: Boolean
			. Set ConfirmClose to true to force confirmation when closing
				the form. This confirmation query will be executed right
				before the OnCloseQuery event is fired, and the result of
				the query will be used as the initial value for the CloseQuery
				CanClose parameter.

		DataSource: TDataSource
			. Use this property to associate one datasource with the form.
				This might be useful for sinchronization with data-aware
				controls outside the form.

		EnabledProps: Boolean
			. This property allows for a central mechanism for setting
				the Enabled property of EVERY control on the form in a
				single step.

		ForceOnTaskbar: Boolean
			. Set ForceOnTaskbar to true to make a form display in the
				taskbar. This property will not work for MDIChild forms.

		MaxTrack: TKFormTrack
			. MaxTrack will determine the maximum width and height of
				a form. If MaxTrack.Ignore is false then the values in
				MaxTrack.Width and MaxTrack.Height will be used to enforce
				maximum dimensions of the form.

		MinTrack: TKFormTrack
			. MinTrack will determine the minimum width and height of
				a form. If MinTrack.Ignore is false then the values in
				MinTrack.Width and MinTrack.Height will be used to enforce
				minimum dimensions of the form.

		Moveable: Boolean
			. If false, the form will not be movable. Notice that as
				a side-effect to setting Moveable to false, the form's
				maximize button will be disabled.

		Persistence: TKPersistence
			. Use Persistence to allow for automatic streaming of any
				properties to the registry. Create PersistenceItems to
				determine which properties of which components should be
				streamed.

				. LoadOption: select the event for automatic loading of
						the streamed properties; possible values are loCreate,
						loActivate and loShow.

				. RegistryKey: select the key under HKEY_CURRENT_USER to
						use as the base key for all streamed properties of the
						form.

				. SaveOption: select the event for automatic saving of
						the streamed properties; possible values are soDestroy,
						soDeactivate and soHide.

				. PersistenceItem.ComponentName: select the component you
						want a property value to be streamed.

				. PersistenceItem.DefaultValue: select the default value of
						the property to be used when no values are found in the
						registry.

				. PersistenceItem.PropertyName: select the property you
						want the value to be streamed.

				. PersistenceItem.RegistrySubKey: select the key under
						Persistence.RegistryKey you want the properties to be
						streamed.

		ReadOnlyProps: Boolean
			. This property allows for a central mechanism for setting
				the ReadOnly property of EVERY control on the form in a
				single step.

		Sizeable: Boolean
			. If false, the form will not be sizeable. Notice that as
				a side-effect to setting Sizeable to false, the form's
				maximize button will be disabled.

	Events:

		AfterLoad: TNotifyEvent
			. Called after the properties defined by Persistence have
				been loaded from the registry and updated. Will occur
				before OnCreate, OnActivate, or OnShow depending on the
				value of Persistence.LoadOption.

		BeforeSave: TNotifyEvent
			. Called before the properties defined by Persistence are
				updated in the registry. Will occur	after OnDeactivate,
				or OnHide, but before OnDestroy depending on the value
				of Persistence.SaveOption.

		DataSourceChanged: TNotifyEvent
			. Called whenever the datasource associated to the form has
				changed. Notice that the event is called when the reference
				to the datasource changes, not when the data itself changes.

		OnAbout: TNotifyEvent
			. Called every time the user selects the system menu item with
				the string defined in the About property.

		OnClose: TCloseEvent
			. Called every time the user closes the form; notice that the
				value of Action will be the value defined by the CloseAction
				property.

		OnCreate: TNotifyEvent
			. Called right after the form has been created. For KForms, the
				OnCreate will happen after the entire process of creation has
				finished, including any calls to Loaded.

		OnDestroy: TNotifyEvent
			. Called right before the Form is actually destroyed.

		OnMaximize: TNotifyEvent
			. Called right after the Form is maximized. This event handler
				is synchronized with the application window.

		OnMinimize: TNotifyEvent
			. Called right after the Form is minimized. This event handler
				is synchronized with the application window.

		OnRestore: TNotifyEvent
			. Called right after the Form is restored. This event handler
				is synchronized with the application window.

}

	TKForm = class( TForm )
	private
		FAbout: String;
		FResized: Boolean;
		FMoveable: Boolean;
		FSizeable: Boolean;
		FAboutCmd: Boolean;
		FIsMainForm: Boolean;
//		FDesignWidth: Integer;
//		FDesignHeight: Integer;
		FConfirmClose: Boolean;
		FEnabledProps: Boolean;
		FReadOnlyProps: Boolean;
		FForceOnTaskbar: Boolean;

		FMinTrack: TKFormTrack;
		FMaxTrack: TKFormTrack;
		FDataSource: TDataSource;
		FPersistence: TKPersistence;

		FOnAbout: TNotifyEvent;
		FNewClose: TCloseEvent;
		FNewCreate: TNotifyEvent;
		FOnRestore: TNotifyEvent;
		FAfterLoad: TNotifyEvent;
		FNewDestroy: TNotifyEvent;
		FBeforeSave: TNotifyEvent;
		FOnMaximize: TNotifyEvent;
		FOnMinimize: TNotifyEvent;
		FCloseAction: TCloseAction;
		FDataSourceChanged: TNotifyEvent;
		FNewCloseQuery: TCloseQueryEvent;

		function  GetAbout: String;
		procedure SetAbout( Value: String );
		procedure SetAboutCmd( Value: Boolean );
		procedure SetSizeable( Value: Boolean );
		procedure SetMoveable( Value: Boolean );
		procedure SetEnabledProps( Value: Boolean );
		procedure SetReadOnlyProps( Value: Boolean );
		procedure SetForceOnTaskbar( Value: Boolean );
		procedure SetDataSource( Value: TDataSource );

		procedure WMNCHitTest( var Msg: TMessage );
							message WM_NCHITTEST;
		procedure WMSysCommand( var Msg: TWMSysCommand );
							message WM_SYSCOMMAND;
		procedure WMInitMenuPopup( var Msg: TWMInitMenuPopup );
							message WM_INITMENUPOPUP;
		procedure WMGetMinMaxInfo( var Msg: TWMGetMinMaxInfo );
							message WM_GETMINMAXINFO;
		procedure WMNCLButtonDblClk( var Msg: TWMNCLButtonDblCLk );
							message WM_NCLBUTTONDBLCLK;

	protected
		procedure Loaded; override;
		procedure DoShow; override;
		procedure DoHide; override;
		procedure DoCreate; dynamic;
		procedure Activate; override;
		procedure DoRestore; dynamic;
		procedure DoMaximize; dynamic;
		procedure DoMinimize; dynamic;
		procedure DoAboutCmd; dynamic;
		procedure Deactivate; override;
		procedure SaveToRegistry; dynamic;
		procedure LoadFromRegistry; dynamic;
		procedure DoDataSourceChanged; dynamic;
		procedure DoDestroy( Sender: TObject ); dynamic;
		procedure ReadRegistryKey( Reader: TReader ); virtual;
		procedure ReadLoadOption( Reader: TReader ); virtual;
		procedure ReadSaveOption( Reader: TReader ); virtual;
		procedure WriteRegistryKey( Writer: TWriter ); virtual;
		procedure WriteLoadOption( Writer: TWriter ); virtual;
		procedure WriteSaveOption( Writer: TWriter ); virtual;
		procedure DefineProperties( Filer: TFiler ); override;
		procedure CreateParams( var Params: TCreateParams ); override;
		procedure DoClose( {$IFNDEF DELPHI4}Sender: TObject; {$ENDIF}
			var Action: TCloseAction ); {$IFDEF DELPHI4}override{$ELSE}dynamic{$ENDIF};
		procedure DoCloseQuery( Sender: TObject; var CanClose: Boolean ); dynamic;
		procedure DoAppMessages( var Message: TMessage; var HookInfo: TKHookInfo ); dynamic;
		procedure Notification( AComponent: TComponent; Operation: TOperation ); override;

	public
		destructor Destroy; override;
		constructor Create( AOwner: TComponent ); override;

	published
		property About: String
						 read GetAbout write SetAbout;
		property AboutCmd: Boolean
						 read FAboutCmd write SetAboutCmd default false;
		property EnabledProps: Boolean
						 read FEnabledProps write SetEnabledProps default true;
		property CloseAction: TCloseAction
						 read FCloseAction write FCloseAction default caFree;
		property ConfirmClose: Boolean
						 read FConfirmClose write FConfirmClose default false;
		property DataSource: TDataSource
						 read FDataSource write SetDataSource;
//		property DesignHeight: Integer
//						 read FDesignHeight write FDesignHeight default 600;
//		property DesignWidth: Integer
//						 read FDesignWidth write FDesignWidth default 800;
		property ForceOnTaskbar: Boolean
						 read FForceOnTaskbar write SetForceOnTaskbar default false;
		property MinTrack: TKFormTrack
						 read FMinTrack write FMinTrack;
		property MaxTrack: TKFormTrack
						 read FMaxTrack write FMaxTrack;
		property Moveable: Boolean
						 read FMoveable write SetMoveable default true;
		property Persistence: TKPersistence
						 read FPersistence write FPersistence;
		property ReadOnlyProps: Boolean
						 read FReadOnlyProps write SetReadOnlyProps default false;
		property Sizeable: Boolean
						 read FSizeable write SetSizeable default true;

		property AfterLoad: TNotifyEvent
						 read FAfterLoad write FAfterLoad;
		property BeforeSave: TNotifyEvent
						 read FBeforeSave write FBeforeSave;
		property DataSourceChanged: TNotifyEvent
						 read FDataSourceChanged write FDataSourceChanged;
		property OnAbout: TNotifyEvent
						 read FOnAbout write FOnAbout;
		property OnClose: TCloseEvent
						 read FNewClose write FNewClose;
		property OnCloseQuery: TCloseQueryEvent
						 read FNewCloseQuery write FNewCloseQuery;
		property OnCreate: TNotifyEvent
						 read FNewCreate write FNewCreate;
		property OnDestroy: TNotifyEvent
						 read FNewDestroy write FNewDestroy;
		property OnMaximize: TNotifyEvent
						 read FOnMaximize write FOnMaximize;
		property OnMinimize: TNotifyEvent
						 read FOnMinimize write FOnMinimize;
		property OnRestore: TNotifyEvent
						 read FOnRestore write FOnRestore;

	end;

{

	TKSimpleMDI

	This is a general MDI Parent form. Use TKSimpleForm anywhere you
	would normally use a TForm with fsMDIForm style. Use TKChild forms
	as children of this form.

	Properties:

		Gradient: TKGradient
			. The style to be painted in the form's background canvas. There
				are a few predefined styles; bitmaps (tile or stretched ) are
				also allowed.

		MultiInstance: Boolean
			. If false, avoids multiple instances of a given form to be
				created in the client area; this would be the case of a
				DB application with heterogeneous child forms. If true,
				many instances of a given class may be created within the
				client area; suitable for applications such as text editors
				or spreadsheets.

	Methods:

		CreateChild( ChildClass: TKChildClass )
			. Use this method to create a new instance of a ChildClass.

		FindChild( ChildClass: TKChildClass ): Integer
			. Use this method to find the index of an instance of
				ChildClass in the MDIChildren array of its MDI parent.
				The value -1 indicates that no instance was found.

}

	TKSimpleMDI = class( TKForm )
	private
		FGradient: TKGradient;
		FMultiInstance: Boolean;
		FClientInstance: TFarProc;
		FPrevClientProc: TFarProc;

		procedure KMUpdateMenu( var Msg: TMessage );
							message KM_UPDATEMENU;
		procedure ClientWndProc( var Message: TMessage );

	protected
		procedure DoCascade; dynamic;
		procedure CreateWnd; override;
		procedure DoCloseAll; dynamic;
		procedure DoMinimizeAll; dynamic;
		procedure DoTileVertical; dynamic;
		procedure DoTileHorizontal; dynamic;
		procedure DoClose( {$IFNDEF DELPHI4}Sender: TObject; {$ENDIF}
			var Action: TCloseAction ); override;

	public
		destructor Destroy; override;
		constructor Create( AOwner: TComponent ); override;

		function CreateChild( ChildClass: TKChildClass ): TForm;
		function  FindChild( ChildClass: TKChildClass ): Integer;

	published
		property Gradient: TKGradient
						 read FGradient write FGradient;
		property MultiInstance: Boolean
						 read FMultiInstance write FMultiInstance default false;

	end;

{

	TKMDIForm

	This is a general MDI Parent form. Use TKMDIForm anywhere you
	would normally use a TForm with fsMDIForm style. This MDI Form
	has a default main menu for fast prototyping and development.
	Use TKChild forms as children of this form.

}

	TKMDIForm = class( TKSimpleMDI )
	protected

		procedure KMUpdateMenu( var Msg: TMessage );
							message KM_UPDATEMENU;

	end;

{

	TKNavMDIForm

	This is a general MDI Parent form. Use TKNavMDIForm anywhere you
	would normally use a TForm with fsMDIForm style. This MDI Form has
	a default main menu and navigation toolbar for fast prototyping and
	development. This form fully integrates its toolbar states with the
	states of its children. Use TKChild forms as children of this form.
	Use TKDBChild forms if you need automatic integration	with the
	child's datasource.

}

	TKNavMDIForm = class( TKMDIForm )
	private
		function GetNavigator: TKFormDBNavigator;

	protected
		procedure Notification( AComponent: TComponent; Operation: TOperation ); override;

	public
		property Navigator: TKFormDBNavigator
						 read GetNavigator;

	end;

{	TKChild }

	TKChild = class( TKForm )
	protected
		procedure DoShow; override;
		procedure DoCreate; override;

	public
		destructor Destroy; override;
		constructor Create( AOwner: TComponent ); override;

	end;

{	TKDBChild }

	TKDBChild = class( TKChild )
	private

		function GetNavigator: TKFormDBNavigator;
		procedure SetTablesOpen( Value: Boolean );
		procedure KMLinkChild( var Msg: TMessage );
							message KM_LINKCHILD;

	protected
		procedure DoCreate; override;
		procedure Activate; override;
		procedure DoRestore; override;
		procedure Deactivate; override;
		procedure DoMinimize; override;
		procedure DoMaximize; override;
		procedure DoDataSourceChanged; override;
		procedure DoClose( {$IFNDEF DELPHI4}Sender: TObject; {$ENDIF}
			var Action: TCloseAction ); override;

	public
		procedure ActivateMainLinks; 
		procedure DeactivateMainLinks;

		property Navigator: TKFormDBNavigator
						 read GetNavigator;

	end;

function  ChildByHandle( Hnd: HWnd ): TKChild;
function  ChildByClass( ChildClass: TKChildClass ): TKChild;
function  KFormByHandle( Hnd: HWnd ): TKForm;
function  KFormByClass( KFormClass: TKFormClass ): TKForm;

implementation

uses
	SysUtils, Registry, TypInfo, Menus, Graphics, DBTables, uksyUtils, ukrUtils,
	ukfResStr;

{************************************************************************}
{ Stand-Alone Procedures/Functions }
{************************************************************************}

{
	Looks for a form with the given handle and returns a TKChild
	reference if found; if no form is found, returns nil.
}
function ChildByHandle( Hnd: HWnd ): TKChild;
var
	frm: TForm;
begin
	frm := FormByHandle( Hnd );
	if ( frm <> nil ) and ( frm is TKChild ) then
		Result := TKChild( frm )
	else
		Result := nil;
end;

{
	Looks for a form of ChildClass and returns a TKChild
	reference if found; if no form is found, returns nil.
}
function ChildByClass( ChildClass: TKChildClass ): TKChild;
var
	frm: TForm;
begin
	frm := FindFirstForm( ChildClass );
	try
		if ( frm <> nil ) then
			Result := TKChild( frm )
		else
			Result := nil;
	finally
		CloseFindForm;
	end;
end;

{
	Looks for a form with the given handle and returns a TKForm
	reference if found; if no form is found, returns nil.
}
function KFormByHandle( Hnd: HWnd ): TKForm;
var
	frm: TForm;
begin
	frm := FormByHandle( Hnd );
	if ( frm <> nil ) and ( frm is TKForm ) then
		Result := TKForm( frm )
	else
		Result := nil;
end;

{
	Looks for a form of KFormClass and returns a TKForm
	reference if found; if no form is found, returns nil.
}
function KFormByClass( KFormClass: TKFormClass ): TKForm;
var
	frm: TForm;
begin
	frm := FindFirstForm( KFormClass );
	try
		if ( frm <> nil ) then
			Result := TKForm( frm )
		else
			Result := nil;
	finally
		CloseFindForm;
	end;
end;

{************************************************************************}
{ TKForm }
{************************************************************************}

constructor TKForm.Create( AOwner: TComponent );
begin
	FAbout := '';
	FResized := false;
	FMoveable := true;
	FSizeable := true;
	FAboutCmd := false;
//FDesignWidth := 800;
//FDesignHeight := 600;
	FIsMainForm := false;
	FEnabledProps := true;
	FConfirmClose := false;
	FCloseAction := caFree;
	FReadOnlyProps := false;
	FForceOnTaskbar := false;
	FPersistence := TKPersistence.Create( Self );
	FMinTrack := TKFormTrack.Create( tsMinTrack );
	FMaxTrack := TKFormTrack.Create( tsMaxTrack );
	inherited Create( AOwner );
	{$IFNDEF DELPHI4}
	inherited OnClose := DoClose;
	{$ENDIF}
	inherited OnDestroy := DoDestroy;
	inherited OnCloseQuery := DoCloseQuery;
	Scaled := false;
	AutoScroll := false;
//	Font.Name := 'Arial';
	FMinTrack.SetOwner( Self );
	FMaxTrack.SetOwner( Self );
	if ( Application.MainForm = nil ) and
		 ( not ( csDesigning in ComponentState ) ) then
	begin
		FIsMainForm := true;
		HookWindow( MakeHookData( Application.Handle, DoAppMessages, nil ) );
	end;
{	if ( Screen.Width <> FDesignWidth ) then
	begin
		Width := MulDiv( Width, Screen.Width, FDesignWidth );
		Height := MulDiv( Height, Screen.Height, FDesignHeight );
		bOldMin := MinTrack.Ignore;
		bOldMax := MaxTrack.Ignore;
		MinTrack.Ignore := true;
		MaxTrack.Ignore := true;
		ScaleBy( Screen.Width, FDesignWidth );
		MinTrack.Ignore := bOldMin;
		MaxTrack.Ignore := bOldMax;
	end;}
	DoCreate;
end;

destructor TKForm.Destroy;
var
	i: Integer;
begin
	if ( not ( csDesigning in ComponentState ) ) then
		UnHookWindow( MakeHookData( Application.Handle, DoAppMessages, nil ) );
	for i := 0 to ComponentCount - 1 do
		if ( Components[i] is TDataset ) then
			TDataset( Components[i] ).Close;
	inherited Destroy;
	FMinTrack.Free;
	FMaxTrack.Free;
	FPersistence.Free;
	FMinTrack := nil;
	FMaxTrack := nil;
	FPersistence := nil;
end;

procedure TKForm.Loaded;
begin
	inherited Loaded;
end;

procedure TKForm.CreateParams( var Params: TCreateParams );
begin
	inherited CreateParams( Params );
	if ( FForceOnTaskbar and ( FormStyle <> fsMDIChild ) ) then
		with Params do
			ExStyle := ExStyle or WS_EX_APPWINDOW;
end;

procedure TKForm.Notification( AComponent: TComponent; Operation: TOperation );
begin
	inherited Notification( AComponent, Operation );
	if ( csLoading in ComponentState ) then Exit;
	if ( Operation = opRemove ) and
		 ( AComponent is TDataSource ) and
		 ( DataSource = TDataSource( AComponent ) ) then
		DataSource := nil;
end;

procedure TKForm.ReadRegistryKey( Reader: TReader );
begin
	if ( FPersistence <> nil ) then
		FPersistence.RegistryKey := Reader.ReadString;
end;

procedure TKForm.ReadLoadOption( Reader: TReader );
begin
	if ( FPersistence <> nil ) then
		FPersistence.LoadOption := TLoadOptions( GetEnumValue( TypeInfo( TLoadOptions ), Reader.ReadString ) );
end;

procedure TKForm.ReadSaveOption( Reader: TReader );
begin
	if ( FPersistence <> nil ) then
		FPersistence.SaveOption := TSaveOptions( GetEnumValue( TypeInfo( TSaveOptions ), Reader.ReadString ) );
end;

procedure TKForm.WriteRegistryKey( Writer: TWriter );
begin
	if ( FPersistence <> nil ) then
		Writer.WriteString( FPersistence.RegistryKey );
end;

procedure TKForm.WriteLoadOption( Writer: TWriter );
begin
	if ( FPersistence <> nil ) then
		Writer.WriteString( GetEnumName( TypeInfo( TLoadOptions ), Ord( FPersistence.LoadOption ) ) );
end;

procedure TKForm.WriteSaveOption( Writer: TWriter );
begin
	if ( FPersistence <> nil ) then
		Writer.WriteString( GetEnumName( TypeInfo( TSaveOptions ), Ord( FPersistence.SaveOption ) ) );
end;

procedure TKForm.DefineProperties( Filer: TFiler );

	function DoLoad: Boolean;
	begin
		if Assigned( Filer.Ancestor ) then
			Result := ( FPersistence.LoadOption <> TKForm( Filer.Ancestor ).Persistence.LoadOption )
		else
			Result := ( FPersistence <> nil ) and
								( FPersistence.LoadOption <> loCreate );
	end;

	function DoSave: Boolean;
	begin
		if Assigned( Filer.Ancestor ) then
			Result := ( FPersistence.SaveOption <> TKForm( Filer.Ancestor ).Persistence.SaveOption )
		else
			Result := ( FPersistence <> nil ) and
								( FPersistence.SaveOption <> soDestroy );
	end;

begin
	inherited DefineProperties( Filer );
	Filer.DefineProperty( 'LoadOption', ReadLoadOption, WriteLoadOption, DoLoad );
	Filer.DefineProperty( 'RegistryKey', ReadRegistryKey, WriteRegistryKey, true );
	Filer.DefineProperty( 'SaveOption', ReadSaveOption, WriteSaveOption, DoSave );
end;

procedure TKForm.SaveToRegistry;
var
	i: Integer;
	c: TComponent;
	pi: PPropInfo;
	rg: TRegIniFile;
	sKey: String;
begin
	if ( csDesigning in ComponentState ) then Exit;
	if Assigned( FBeforeSave ) then
		FBeforeSave( Self );
	with Persistence do
		for i := 0 to Pred( Count ) do
		begin
			if ( Items[i].ComponentName = Self.Name ) then
				c := Self
			else
				c := FindComponent( Items[i].ComponentName );
			if ( c <> nil ) then
				with Items[i] do
				begin
					rg := TRegIniFile.Create( RegistryKey );
					try
						sKey := ComponentName + '.' + PropertyName;
						pi := GetPropInfo( c.ClassInfo, PropertyName );
						if ( pi <> nil ) then
							case pi^.PropType^.Kind of
								tkChar,
								tkInteger:
									rg.WriteInteger( RegistrySubKey, sKey, GetOrdProp( c, pi ) );
								tkEnumeration:
									rg.WriteString( RegistrySubKey, sKey, GetEnumName( pi^.PropType^, GetOrdProp( c, pi ) ) );
								tkString,
								tkLString:
									rg.WriteString( RegistrySubKey, sKey, GetStrProp( c, pi ) );
							end;
					finally
						rg.Free;
					end;
				end;
		end;
end;

procedure TKForm.LoadFromRegistry;
var
	i: Integer;
	c: TComponent;
	pi: PPropInfo;
	rg: TRegIniFile;
	sKey: String;
begin
	if ( csDesigning in ComponentState ) then Exit;
	with Persistence do
		for i := 0 to Pred( Count ) do
		begin
			if ( CompareText( Items[i].ComponentName, Self.Name ) = 0 ) then
				c := Self
			else
				c := FindComponent( Items[i].ComponentName );
			if ( c <> nil ) then
				with Items[i] do
				begin
					rg := TRegIniFile.Create( RegistryKey );
					try
						sKey := ComponentName + '.' + PropertyName;
						pi := GetPropInfo( c.ClassInfo, PropertyName );
						if ( pi <> nil ) then
							case pi^.PropType^.Kind of
								tkChar,
								tkInteger:
									SetOrdProp( c, pi, rg.ReadInteger( RegistrySubKey, sKey, StrToInt( DefaultValue ) ) );
								tkEnumeration:
									SetOrdProp( c, pi, GetEnumValue( pi^.PropType^, rg.ReadString( RegistrySubKey, sKey, DefaultValue ) ) );
								tkString,
								tkLString:
									SetStrProp( c, pi, rg.ReadString( RegistrySubKey, sKey, DefaultValue ) );
							end;
					finally
						rg.Free;
					end;
				end;
		end;
	if Assigned( FAfterLoad ) then
		FAfterLoad( Self );
end;

procedure TKForm.WMNCHitTest( var Msg: TMessage );
begin
	if ( csDesigning in ComponentState ) then
	begin
		inherited;
		Exit;
	end;
	Msg.Result := DefWindowProc( Handle, WM_NCHitTest, Msg.WParam, Msg.LParam );
	if ( Msg.Result in HITTEST_SET ) then
	begin
		if ( not FSizeable ) then
			Msg.Result := HTERROR;
	end
	else if ( Msg.Result = HTCAPTION ) then
	begin
		if ( not FMoveable ) then
			Msg.Result := HTERROR;
	end;
	if ( Msg.Result <> HTERROR ) then
		inherited;
end;

procedure TKForm.WMSysCommand( var Msg: TWMSysCommand );
begin
	if ( csDesigning in ComponentState ) then
	begin
		inherited;
		Exit;
	end;
	if ( Msg.CmdType <> SC_MAXIMIZE ) or
		 ( ( Msg.CmdType = SC_MAXIMIZE ) and FMoveable and FSizeable ) then
		inherited;
	case Msg.CmdType of
		CM_ABOUT: 	 DoAboutCmd;
		SC_RESTORE:  DoRestore;
		SC_MAXIMIZE: if ( FMoveable and FSizeable ) then DoMaximize;
		SC_MINIMIZE: DoMinimize;
	end;
end;

procedure TKForm.WMInitMenuPopup( var Msg: TWMInitMenuPopup );
begin
	inherited;
	if ( csDesigning in ComponentState ) then Exit;
	if ( not Msg.SystemMenu ) then Exit;
	SetMenuDefaultItem( Msg.MenuPopup, CM_ABOUT, Ord( false ) );
{ enable/disable System Menu's Size command }
	if ( FSizeable and FMoveable ) then
		EnableMenuItem( Msg.MenuPopup, SC_SIZE, MF_BYCOMMAND or MF_ENABLED )
	else
		EnableMenuItem( Msg.MenuPopup, SC_SIZE, MF_BYCOMMAND or MF_GRAYED );
{ enable/disable System Menu's Move command }
	if FMoveable then
		EnableMenuItem( Msg.MenuPopup, SC_MOVE, MF_BYCOMMAND or MF_ENABLED )
	else
		EnableMenuItem( Msg.MenuPopup, SC_MOVE, MF_BYCOMMAND or MF_GRAYED );
{ enable/disable System Menu's Maximize command }
	if ( FMoveable and FSizeable and ( WindowState <> wsMaximized ) ) then
		EnableMenuItem( Msg.MenuPopup, SC_MAXIMIZE, MF_BYCOMMAND or MF_ENABLED )
	else
		EnableMenuItem( Msg.MenuPopup, SC_MAXIMIZE, MF_BYCOMMAND or MF_GRAYED );
{ enable/disable System Menu's Restore command }
	if ( not FMoveable ) then
	begin
		if ( WindowState <> wsMinimized ) then
			EnableMenuItem( Msg.MenuPopup, SC_RESTORE, MF_BYCOMMAND or MF_GRAYED )
		else
			EnableMenuItem( Msg.MenuPopup, SC_RESTORE, MF_BYCOMMAND or MF_ENABLED );
	end
	else
	begin
		if ( WindowState = wsNormal ) then
			EnableMenuItem( Msg.MenuPopup, SC_RESTORE, MF_BYCOMMAND or MF_GRAYED )
		else if ( WindowState = wsMaximized ) then
			EnableMenuItem( Msg.MenuPopup, SC_RESTORE, MF_BYCOMMAND or MF_ENABLED );
	end;
end;

procedure TKForm.WMGetMinMaxInfo( var Msg: TWMGetMinMaxInfo );
begin
	inherited;
	if ( Self = nil ) or
		 ( csDesigning in ComponentState ) then Exit;
	with Msg.MinMaxInfo^ do
		if ( not FSizeable ) then
		begin
{ sets max window to form's size }
			with ptMaxTrackSize do
			begin
				X := Width;
				Y := Height;
			end;
{ sets min window to form's size }
			with ptMinTrackSize do
			begin
				X := Width;
				Y := Height;
			end;
		end
		else
		begin
{ sets min window }
			if ( not FMinTrack.Ignore ) then
				with ptMinTrackSize do
				begin
					X := FMinTrack.Width;
					Y := FMinTrack.Height;
				end;
{ sets max window }
			if ( not FMaxTrack.Ignore ) then
				with ptMaxTrackSize do
				begin
					X := FMaxTrack.Width;
					Y := FMaxTrack.Height;
				end;
		end;
end;

procedure TKForm.WMNCLButtonDblClk( var Msg: TWMNCLButtonDblCLk );
begin
	if ( csDesigning in ComponentState ) then
	begin
		inherited;
		Exit;
	end;
	if ( FMoveable and FSizeable ) then
	begin
		inherited;
		if ( WindowState = wsMaximized ) then
			DoMaximize
		else
			DoRestore;
	end;
end;

procedure TKForm.DoAppMessages( var Message: TMessage; var HookInfo: TKHookInfo );
begin
{ synchronize the form and the application window states }
	case Message.Msg of
		WM_SYSCOMMAND:
		begin
			HookInfo.Process := False;
			CallWndProc( Application.Handle, Message );
{ calls the event only after the message handling }
			case Message.wParam of
				CM_ABOUT: DoAboutCmd;
				SC_RESTORE: DoRestore;
				SC_MINIMIZE: DoMinimize;
			end;
		end;
		WM_INITMENUPOPUP:
			if Boolean( HIWORD( Message.lParam ) ) then
				SetMenuDefaultItem( Message.wParam, CM_ABOUT, Ord( false ) );
	end;
end;

procedure TKForm.SetForceOnTaskbar( Value: Boolean );
begin
	if ( FForceOnTaskbar = Value ) then Exit;
	FForceOnTaskbar := Value;
	RecreateWnd;
end;

procedure TKForm.SetAboutCmd( Value: Boolean );
var
	pcAbout: PChar;
	hSysMenu: HMenu;
begin
	if ( FAboutCmd = Value ) then Exit;
	FAboutCmd := Value;
	if ( csDesigning in ComponentState ) then Exit;
	if FAboutCmd then
	begin
		pcAbout := StrAlloc( Length( About ) + 1 );
		try
			StrPCopy( pcAbout, About );
			hSysMenu := GetSystemMenu( Handle, false );
			AppendMenu( hSysMenu, MF_SEPARATOR, CM_SEPARATOR, '' );
			AppendMenu( hSysMenu, MF_STRING, CM_ABOUT, pcAbout );
{ changes the application menu only if is the main form }
			if FIsMainForm then
			begin
				hSysMenu := GetSystemMenu( Application.Handle, false );
				AppendMenu( hSysMenu, MF_SEPARATOR, CM_SEPARATOR, '' );
				AppendMenu( hSysMenu, MF_STRING, CM_ABOUT, pcAbout );
			end;
		finally
			StrDispose( pcAbout );
		end;
	end
	else
	begin
		hSysMenu := GetSystemMenu( Handle, false );
		DeleteMenu( hSysMenu, CM_SEPARATOR, MF_BYCOMMAND );
		DeleteMenu( hSysMenu, CM_ABOUT, MF_BYCOMMAND );
{ changes the application menu only if is the main form }
		if FIsMainForm then
		begin
			hSysMenu := GetSystemMenu( Application.Handle, false );
			DeleteMenu( hSysMenu, CM_SEPARATOR, MF_BYCOMMAND );
			DeleteMenu( hSysMenu, CM_ABOUT, MF_BYCOMMAND );
		end;
	end;
end;

function TKForm.GetAbout: String;
begin
	if ( FAbout = '' ) then
		Result := sFAbout
	else
		Result := FAbout;
end;

procedure TKForm.SetDataSource( Value: TDataSource );
begin
	if ( Value <> FDataSource ) then
	begin
		FDataSource := Value;
		if ( Value <> nil ) then
			Value.FreeNotification( Self );
		DoDataSourceChanged;
	end;
end;

procedure TKForm.SetAbout( Value: String );
begin
	if ( FAbout = Value ) then Exit;
	FAbout := Value;
	if AboutCmd then
	begin
		AboutCmd := false;
		AboutCmd := true;
	end;
end;

procedure TKForm.SetEnabledProps( Value: Boolean );
var
	i: Integer;
	pi: PPropInfo;
	wc: TWinControl;
begin
	if ( FEnabledProps = Value ) then Exit;
	FEnabledProps := Value;
	for i := 0 to Pred( ComponentCount ) do
		if ( Components[i] is TWinControl ) then
		begin
			wc := TWinControl( Components[i] );
			pi := GetPropInfo( wc.ClassInfo, 'Enabled' );
			if ( pi <> nil ) then
				SetOrdProp( wc, pi, LongInt( Value ) );
		end;
end;

procedure TKForm.SetReadOnlyProps( Value: Boolean );
var
	i: Integer;
	pi: PPropInfo;
	wc: TWinControl;
begin
	if ( FReadOnlyProps = Value ) then Exit;
	FReadOnlyProps := Value;
	for i := 0 to Pred( ComponentCount ) do
		if ( Components[i] is TWinControl ) then
		begin
			wc := TWinControl( Components[i] );
			pi := GetPropInfo( wc.ClassInfo, 'ReadOnly' );
			if ( pi <> nil ) then
				SetOrdProp( wc, pi, LongInt( Value ) );
		end;
end;

procedure TKForm.SetSizeable( Value: Boolean );
begin
	FSizeable := Value;
	if ( FSizeable and FMoveable ) then
		BorderIcons := BorderIcons + [biMaximize]
	else
		BorderIcons := BorderIcons - [biMaximize];
end;

procedure TKForm.SetMoveable( Value: Boolean );
begin
	FMoveable := Value;
	if ( FSizeable and FMoveable ) then
		BorderIcons := BorderIcons + [biMaximize]
	else
		BorderIcons := BorderIcons - [biMaximize];
end;

procedure TKForm.DoRestore;
begin
	if Assigned( FOnRestore ) then
		FOnRestore( Self );
end;

procedure TKForm.DoMaximize;
begin
	if Assigned( FOnMaximize ) then
		FOnMaximize( Self );
end;

procedure TKForm.DoMinimize;
begin
	if FIsMainForm then
		SetForeGroundWindow( Application.Handle );
	if Assigned( FOnMinimize ) then
		FOnMinimize( Self );
end;

procedure TKForm.DoAboutCmd;
begin
	if Assigned( FOnAbout ) then
		FOnAbout( Self );
end;

type
	THackControl = class( TControl );

procedure TKForm.DoShow;
begin
	if ( FPersistence.LoadOption = loShow ) then
		LoadFromRegistry;
{	if ( not FResized ) then
	begin
		PixelsPerInch := Screen.PixelsPerInch;
		FResized := true;
		for i := Pred( ControlCount ) to 0 do
			THackControl( Controls[i] ).Font.Size :=
				MulDiv( THackControl( Controls[i] ).Font.Size, Width, FDesignWidth );
	end;}
	inherited DoShow;
end;

procedure TKForm.DoHide;
begin
	inherited DoHide;
	if ( FPersistence.SaveOption = soHide ) then
		SaveToRegistry;
end;

procedure TKForm.Activate;
begin
	if ( FPersistence.LoadOption = loActivate ) then
		LoadFromRegistry;
	inherited Activate;
end;

procedure TKForm.Deactivate;
begin
	inherited Deactivate;
	if ( FPersistence.SaveOption = soDeactivate ) then
		SaveToRegistry;
end;

procedure TKForm.DoDataSourceChanged;
begin
	if Assigned( FDataSourceChanged ) then
		FDataSourceChanged( Self );
end;

procedure TKForm.DoCreate;
begin
	if ( FPersistence.LoadOption = loCreate ) then
		LoadFromRegistry;
	if Assigned( FNewCreate ) then
		FNewCreate( Self );
end;

procedure TKForm.DoDestroy( Sender: TObject );
begin
	if ( FPersistence.SaveOption = soDestroy ) then
		SaveToRegistry;
	if Assigned( FNewDestroy ) then
		FNewDestroy( Sender );
end;

procedure TKForm.DoClose( {$IFNDEF DELPHI4}Sender: TObject; {$ENDIF}
	var Action: TCloseAction );
begin
{$IFDEF DELPHI4}
//	inherited DoClose( Action );
{$ELSE}
{
	Action := FCloseAction;
	if Assigned( FNewClose ) then
		FNewClose( Sender, Action );
}
{$ENDIF}
	Action := FCloseAction;
	if Assigned( FNewClose ) then
		FNewClose( {$IFDEF DELPHI4}Self, {$ELSE}Sender, {$ENDIF} Action );
end;

procedure TKForm.DoCloseQuery( Sender: TObject; var CanClose: Boolean );
begin
	if FConfirmClose then
		CanClose := ( ShowDialog( sFWarning, sFClosing, nil, dsYesNo, boQuestion02 ) = mrYes );
	if Assigned( FNewCloseQuery ) then
		FNewCloseQuery( Sender, CanClose );
end;

{************************************************************************}
{ TKSimpleMDI }
{************************************************************************}

constructor TKSimpleMDI.Create( AOwner: TComponent );
begin
	FMultiInstance := false;
	FGradient := TKGradient.Create( Self, nil );
	inherited Create( AOwner );
	inherited FormStyle := fsMDIForm;
end;

destructor TKSimpleMDI.Destroy;
begin
	FGradient.Free;
	inherited Destroy;
end;

procedure TKSimpleMDI.CreateWnd;
begin
	inherited CreateWnd;
	FClientInstance := MakeObjectInstance( ClientWndProc );
	FPrevClientProc := Pointer( GetWindowLong( ClientHandle, GWL_WNDPROC ) );
	Windows.SetWindowLong( ClientHandle, GWL_WNDPROC, LongInt( FClientInstance ) );
	PostMessage( Handle, KM_UPDATEMENU, 0, 0 );
	PostMessage( Handle, WM_SIZE, Width, Height );
end;

procedure TKSimpleMDI.ClientWndProc( var Message: TMessage );
var
	aDC: hDC;
	bmp: TBitmap;
begin
	with Message do
		if ( FGradient.GradientStyle = gsNone ) then
			Result := CallWindowProc( FPrevClientProc, ClientHandle, Msg, wParam, lParam )
		else if ( Msg = WM_ERASEBKGND ) then
		begin
			aDC := TWMEraseBkGnd( Message ).DC;
			bmp := TBitmap.Create;
			try
				ControlPaintGradient( Self, bmp, FGradient ); {Review...}
				BitBlt( aDC, 0, 0, bmp.Width, bmp.Height,
								bmp.Canvas.Handle, 0, 0, SRCCOPY );
			finally
				bmp.Free;
			end;
			Result := 1;
		end
		else if ( Msg = WM_VSCROLL ) or ( Msg = WM_HSCROLL ) then
		begin
			Result := CallWindowProc( FPrevClientProc, ClientHandle, Msg, wParam, lParam );
			InvalidateRect( ClientHandle, nil, true );
		end
		else
			Result := CallWindowProc( FPrevClientProc, ClientHandle, Msg, wParam, lParam )
end;

procedure TKSimpleMDI.DoCascade;
var
	iCoord,
	iDelta,
	iChildIndex: Integer;
begin
	iDelta := MDIChildCount - 1;
	for iChildIndex := iDelta downto 0 do
		with MDIChildren[iChildIndex] do
		begin
			iCoord := ( iDelta - iChildIndex ) * FormOffSet;
			SetBounds( iCoord, iCoord, Width, Height );
		end;
end;

procedure TKSimpleMDI.DoCloseAll;
begin
	while ( MDIChildCount > 0 ) do
	begin
		MDIChildren[0].Close;
		Application.ProcessMessages;
	end;
end;

procedure TKSimpleMDI.DoTileVertical;
var
	i: Integer;
begin
	for i := 0 to Pred( MDIChildCount ) do
		MDIChildren[i].TileMode := tbVertical;
	Tile;
end;

procedure TKSimpleMDI.DoMinimizeAll;
var
	i: Integer;
begin
	for i := Pred( MDIChildCount ) downto 0 do
	begin
		PostMessage( MDIChildren[i].Handle, WM_SYSCOMMAND, SC_MINIMIZE, 0 );
		Application.ProcessMessages;
	end;
end;

procedure TKSimpleMDI.DoTileHorizontal;
var
	i: Integer;
begin
	for i := 0 to Pred( MDIChildCount ) do
		MDIChildren[i].TileMode := tbHorizontal;
	Tile;
end;

procedure TKSimpleMDI.DoClose( {$IFNDEF DELPHI4}Sender: TObject; {$ENDIF}
	var Action: TCloseAction );
begin
	inherited DoClose( {$IFNDEF DELPHI4}Self, {$ENDIF} Action );
	if ( Action = caFree ) then
		DoCloseAll;
end;

procedure TKSimpleMDI.KMUpdateMenu( var Msg: TMessage );
begin
end;

function TKSimpleMDI.CreateChild( ChildClass: TKChildClass ): TForm;
var
	iInstance: Integer;
begin
	iInstance := FindChild( ChildClass );
	if ( FMultiInstance or ( iInstance = -1 ) ) then
		Result := ChildClass.Create( Self )
	else
	begin
		MDIChildren[iInstance].Show;
		Result := MDIChildren[iInstance];
	end;
end;

function TKSimpleMDI.FindChild( ChildClass: TKChildClass ): Integer;
var
	iIndex,
	iInstance: Integer;
begin
	iIndex := 0;
	iInstance := -1;
	while ( iIndex < MDIChildCount ) and ( iInstance = -1 ) do
	begin
		if ( MDIChildren[iIndex] is ChildClass ) then
			iInstance := iIndex;
		inc( iIndex );
	end;
	Result := iInstance;
end;

{************************************************************************}
{ TKMDIForm }
{************************************************************************}

procedure TKMDIForm.KMUpdateMenu( var Msg: TMessage );
const
	MAX_MNIJANELA = 6;
	ITEM_JANELA: array[1..MAX_MNIJANELA] of String[25] =
	( 'mniCascata',
		'mniOrganizarIcones',
		'mniLadoLadoVertical',
		'mniLadoLadoHorizontal',
		'mniFecharTodas',
		'mniMinimizarTodas' );
var
	i: Integer;
	cp: TComponent;
	bZero: Boolean;
begin
	bZero := ( MDIChildCount = 0 );
	for i := 1 to MAX_MNIJANELA do
	begin
		cp := FindComponent( ITEM_JANELA[i] );
		if ( cp <> nil ) then
			( cp as TMenuItem ).Enabled := not bZero;
	end;
end;


{************************************************************************}
{ TKNavMDIForm }
{************************************************************************}

function TKNavMDIForm.GetNavigator: TKFormDBNavigator;
var
	i: Integer;
begin
	Result := nil;
	for i := 0 to Pred( ComponentCount ) do
		if ( Components[i] is TKFormDBNavigator ) then
		begin
			Result := TKFormDBNavigator( Components[i] );
			Exit;
		end;
end;

procedure TKNavMDIForm.Notification( AComponent: TComponent; Operation: TOperation );
begin
	inherited Notification( AComponent, Operation );
	if ( csLoading in ComponentState ) then Exit;
	if ( Operation = opInsert ) and
		 ( AComponent is TKFormDBNavigator ) and
		 ( Navigator <> nil ) then
		raise EKForm.Create( sDBFormOnlyOne );
end;

{************************************************************************}
{ TKChild }
{************************************************************************}

constructor TKChild.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	inherited FormStyle := fsMDIChild;
end;

destructor TKChild.Destroy;
begin
	if ( Owner <> nil ) and ( not ( csDesigning in ComponentState ) ) then
		PostMessage( ( Owner as TForm ).Handle, KM_UPDATEMENU, 0, 0 );
	inherited Destroy;
end;

procedure TKChild.DoShow;
var
	iCoord: Integer;
begin
	with ( Owner as TForm ) do
		iCoord := ( MDIChildCount - 1 ) * FormOffSet;
	SetBounds( iCoord, iCoord, Width, Height );
	inherited DoShow;
end;

procedure TKChild.DoCreate;
begin
	if ( not ( csDesigning in ComponentState ) ) and ( Owner <> nil ) then
		PostMessage( ( Owner as TForm ).Handle, KM_UPDATEMENU, 0, 0 );
	inherited DoCreate;
end;


{************************************************************************}
{ TKDBChild }
{************************************************************************}

function TKDBChild.GetNavigator: TKFormDBNavigator;
begin
	if ( Owner = nil ) or ( csDesigning in ComponentState ) then
	begin
		Result := nil;
		Exit;
	end;
	if ( Owner is TKNavMDIForm ) then
		Result := TKNavMDIForm( Owner ).Navigator
	else
		Result := nil;
end;

procedure TKDBChild.SetTablesOpen( Value: Boolean );
var
	i: Integer;
begin
	if ( csDesigning in ComponentState ) then	Exit;
	for i := 0 to ComponentCount - 1 do
		if ( Components[i] is TTable ) then
			with TTable( Components[i] ) do
				if ( TableName <> '' ) then
					Active := Value;
end;

procedure TKDBChild.KMLinkChild( var  Msg: TMessage );
begin
	if ( csDesigning in ComponentState ) then	Exit;
	if ( WindowState <> wsMinimized ) then
		ActivateMainLinks;
end;

procedure TKDBChild.DoClose( {$IFNDEF DELPHI4}Sender: TObject; {$ENDIF}
	var Action: TCloseAction );
begin
	if not ( csDesigning in ComponentState ) then
		SetTablesOpen( false );
	inherited DoClose( {$IFNDEF DELPHI4}Self, {$ENDIF} Action );
end;

procedure TKDBChild.DoCreate;
begin
	if ( csDesigning in ComponentState ) then	Exit;
	SetTablesOpen( true );
	inherited DoCreate;
end;

procedure TKDBChild.Activate;
begin
	if ( WindowState <> wsMinimized ) then
		ActivateMainLinks
	else
		DeactivateMainLinks;
	inherited Activate;
end;

procedure TKDBChild.DoRestore;
begin
	ActivateMainLinks;
	inherited DoRestore;
end;

procedure TKDBChild.Deactivate;
begin
	DeactivateMainLinks;
	inherited Deactivate;
end;

procedure TKDBChild.DoMinimize;
var
	frm: TKChild;
begin
	DeactivateMainLinks;
	if ( Owner <> nil ) and ( not ( csDesigning in ComponentState ) ) then
	begin
		frm := ChildByHandle( ( Owner as TForm ).ActiveMDIChild.Handle );
		if ( frm <> nil ) then
			PostMessage( frm.Handle, KM_LINKCHILD, 0, 0 );
	end;
	inherited DoMinimize;
end;

procedure TKDBChild.DoMaximize;
begin
	ActivateMainLinks;
	inherited DoMaximize;
end;

procedure TKDBChild.DoDataSourceChanged;
begin
	ActivateMainLinks;
	inherited DoDataSourceChanged;
end;

procedure TKDBChild.ActivateMainLinks;
begin
	if ( csDesigning in ComponentState ) or ( Navigator = nil ) then	Exit;
	with Navigator do
	begin
		Enabled := false;
		DataSource := Self.DataSource;
		Enabled := true;
	end;
end;

procedure TKDBChild.DeactivateMainLinks;
begin
	if ( csDesigning in ComponentState ) or ( Navigator = nil ) then	Exit;
	with Navigator do
	begin
		Enabled := false;
		DataSource := nil;
		Enabled := true;
	end;
end;

end.
