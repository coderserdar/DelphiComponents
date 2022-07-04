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

unit ukrDBCtrls;

{$I s:\v100\include\iKLIB100.inc}

{$BOOLEVAL OFF}

interface

uses
	Windows, Messages, Classes, Controls, Forms, DB, Buttons, ExtCtrls, uksyUtils,
	uksyClasses;

{##FNS##}

type

	EKRDBCtrls = class( EKKernel );

{
--------------------------------------------------------------------------------
----------------------------- Generic DB Navigator -----------------------------
--------------------------------------------------------------------------------
}

type

	TNavIndex = ( niFirst, niPrior, niNext, niLast, niSave, niCancel,
		niInsert, niDelete, niRefresh, niPrint, niSearch );

const

	DEF_NAV_BUTTONS = [niFirst, niPrior, niNext, niLast, niSave, niCancel,
		niInsert, niDelete, niRefresh, niPrint, niSearch];

type

  EKDBNavigator = class( EKRDBCtrls );

	TKNavCollection = class;
	TKCustomDBNavigator = class;

	TKFormNavEvent = procedure( Sender: TObject; Form: TCustomForm ) of object;

{------------------------------ TKNavCollectionItem ----------------------------}

	TKNavCollectionItem = class( TKCustomCollectionItem )
	private
		FFormName: string;
		FFormExists: Boolean;
		FOnPrint: TKFormNavEvent;
		FOnSearch: TKFormNavEvent;

		procedure SetFormName( const Value: string );
		function GetOwnerCollection: TKNavCollection;

		function GetForm: TCustomForm;

	protected
		function DoPrintEvent: Boolean; dynamic;
		function DoSearchEvent: Boolean; dynamic;

		property Form: TCustomForm
						 read GetForm;

	public
		constructor Create( ACollection: TCollection ); override;

		procedure Assign( Source: TPersistent ); override;
		function Equals( Item: TKCustomCollectionItem ): Boolean; override;

		property FormExists: Boolean
						 read FFormExists;
		property Owner: TKNavCollection
						 read GetOwnerCollection;

	published
		property FormName: string
						 read FFormName write SetFormName;
		property Name;

		property OnPrint: TKFormNavEvent
						 read FOnPrint write FOnPrint;
		property OnSearch: TKFormNavEvent
						 read FOnSearch write FOnSearch;

	end;

{-------------------------------- TKNavCollection ------------------------------}

	TKNavCollection = class( TKCustomCollection )
	private
		function GetOwnerComp: TKCustomDBNavigator;

		procedure SetItem( Index: Integer; AItem: TKNavCollectionItem );
		function GetItem( Index: Integer ): TKNavCollectionItem;

		function GetItemByName( const AName: string ): TKNavCollectionItem;

	protected
		property Navigator: TKCustomDBNavigator
						 read GetOwnerComp;

	public
		constructor Create( ANav: TKCustomDBNavigator ); virtual;

		function Add: TKNavCollectionItem; virtual;
		property Items[Index: Integer]: TKNavCollectionItem
						 read GetItem write SetItem; default;
		property ItemByName[const AName: string]: TKNavCollectionItem
						 read GetItemByName;
		property Names;

	end;

	TKNavButton = class;
	TKNavDataLink = class;

	TNavMode = ( nmChange, nmRead );
	TSynchronizationMode = ( smHide, smDisable );

	TButtonSet = set of TNavIndex;
	TButtonArray = array[TNavIndex] of Boolean;
	TKNavButtonStyle = set of ( nsAllowTimer, nsFocusRect );

	TKAfterActionEvent = procedure( Sender: TObject; Button: TNavIndex ) of object;
	TKBeforeActionEvent = procedure( Sender: TObject; Button: TNavIndex; var Confirm: Boolean ) of object;

{----------------------------------- TKNavButton --------------------------------}

	TKNavButton = class( TSpeedButton )
	private
		FTimer: TTimer;
		FIndex: TNavIndex;
		FStyle: TKNavButtonStyle;

		procedure TimerExpired( Sender: TObject );

	protected
		procedure Paint; override;
		procedure MouseUp( Button: TMouseButton; Shift: TShiftState;
			X, Y: Integer ); override;
		procedure MouseDown( Button: TMouseButton; Shift: TShiftState;
			X, Y: Integer ); override;

	public
		destructor Destroy; override;

		property Index : TNavIndex
						 read FIndex write FIndex;
		property Style: TKNavButtonStyle
						 read FStyle write FStyle;

	end;

{---------------------------------- TKNavDataLink -------------------------------}

	TKNavDataLink = class( TDataLink )
	private
		FNavigator: TKCustomDBNavigator;

	protected
		procedure ActiveChanged; override;
		procedure EditingChanged; override;
		procedure DataSetChanged; override;

	public
		destructor  Destroy; override;
		constructor Create( ANav: TKCustomDBNavigator );

	end;

{------------------------------ TKCustomDBNavigator ----------------------------}

	TKCustomDBNavigator = class ( TCustomPanel )
	private
		FFlat: Boolean;
		FHints: TStrings;
		FNavMode: TNavMode;
		FDataLink: TKNavDataLink;
		FNavButtons: TButtonSet;
		FNavCollection: TKNavCollection;
		FAvailableButtons: TButtonArray;
		FSynchronization: TSynchronizationMode;

		FOnPrint: TNotifyEvent;
		FOnSearch: TNotifyEvent;
		FAfterAction: TKAfterActionEvent;
		FBeforeAction: TKBeforeActionEvent;

		FMinSize: TPoint;
		FFocusedButton: TNavIndex;

		function GetDataSource: TDataSource;
		function GetAvailableButtons( Index: TNavIndex ): Boolean;

		procedure SetFlat( Value: Boolean );
		procedure SetHints( Value: TStrings );
		procedure SetNavMode( Value: TNavMode );
		procedure SetNavButtons( Value: TButtonSet );
		procedure SetDataSource( Value: TDataSource );
		procedure SetNavCollection( Value: TKNavCollection );
		procedure SetSynchronization( Value: TSynchronizationMode );
		procedure SetAvailableButtons( Index: TNavIndex; Value: Boolean );

		function ScaleX( Value: Integer ): LongInt;
		function ScaleY( Value: Integer ): LongInt;

		procedure InitHints;
		procedure InitButtons;
		procedure UpdateNavDisplay;
		procedure NavClick( Sender: TObject );
		procedure AdjustNavSize( var W: Integer; var H: Integer );
		procedure BtnMouseDown( Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer );

		procedure WMSize( var Message: TWMSize );
							message WM_SIZE;
		procedure WMSetFocus( var Message: TWMSetFocus );
							message WM_SETFOCUS;
		procedure WMKillFocus( var Message: TWMKillFocus );
							message WM_KILLFOCUS;
		procedure WMGetDlgCode( var Message: TWMGetDlgCode );
							message WM_GETDLGCODE;
		procedure CMEnabledChanged( var Message: TMessage );
							message CM_ENABLEDCHANGED;

		property Align; { ??? }

	protected
		Buttons: array[TNavIndex] of TKNavButton;

		procedure DataChanged;
		procedure ActiveChanged;
		procedure Loaded; override;
		function AnyButton: Boolean;
		procedure KeyDown( var Key: Word; Shift: TShiftState ); override;
		procedure GetChildren( Proc: TGetChildProc; Root: TComponent ); override;
		procedure Notification( AComponent: TComponent;	Operation: TOperation ); override;

		procedure DoPrint; dynamic;
		procedure DoSearch; dynamic;

		procedure DoAction( Index: TNavIndex ); dynamic;
		function CanDoAction( Index: TNavIndex ): Boolean; dynamic;

		procedure SyncNavButtons; dynamic;
		procedure SyncNavAvailable; dynamic;

		property AvailableButtons[Index: TNavIndex]: Boolean
						 read GetAvailableButtons write SetAvailableButtons;
		property DataSource: TDataSource
						 read GetDataSource write SetDataSource;
		property Flat: Boolean
						 read FFlat write SetFlat default false;
		property Hints: TStrings
						 read FHints write SetHints;
		property NavEvents: TKNavCollection
						 read FNavCollection write SetNavCollection;
		property NavButtons: TButtonSet
						 read FNavButtons write SetNavButtons default DEF_NAV_BUTTONS;
		property NavMode: TNavMode
						 read FNavMode write SetNavMode default nmChange;
		property ShowHint default true;
		property Synchronization: TSynchronizationMode
						 read FSynchronization write SetSynchronization default smDisable;

		property AfterAction: TKAfterActionEvent
						 read FAfterAction write FAfterAction;
		property BeforeAction: TKBeforeActionEvent
						 read FBeforeAction write FBeforeAction;

		property OnPrint: TNotifyEvent
						 read FOnPrint write FOnPrint;
		property OnSearch: TNotifyEvent
						 read FOnSearch write FOnSearch;

	public
		destructor  Destroy; override;
		constructor Create( AOwner: TComponent ); override;

		procedure BtnClick( Index: TNavIndex ); dynamic;
		procedure SetBounds( ALeft, ATop, AWidth, AHeight: Integer ); override;

	end;

{##FNS##}

implementation

{$R brrDBNav.res}

{.$R u:\delphi\klib100\source\kernel100\lib\brrDBNav.res

 We try to use this form to D4 complaint but this generates a incomatible
 directory list for other delphis.... sorry...
}

uses
	SysUtils, Graphics, uksyConsts, uksyPackReg, ukrResStr, ukrUtils;

{
--------------------------------------------------------------------------------
----------------------------- Generic DB Navigator -----------------------------
--------------------------------------------------------------------------------
}

{----------------------------- TKNavCollectionItem -----------------------------}

constructor TKNavCollectionItem.Create( ACollection: TCollection );
begin
	ForceObjectClass( ACollection, TKNavCollection );
	inherited Create( ACollection );
	ForceObject( Owner.Navigator );
end;

procedure TKNavCollectionItem.Assign( Source: TPersistent );
begin
	if ( CheckObjectClass( Source, TKNavCollectionItem ) and
			 Designing( Owner.Navigator ) ) then
		FormName := ( Source as TKNavCollectionItem ).FormName
	else
		inherited Assign( Source );
end;

function TKNavCollectionItem.Equals( Item: TKCustomCollectionItem ): Boolean;
begin
	Result := ( inherited Equals( Item ) ) and
		CheckStrEqual( FormName, TKNavCollectionItem( Item ).FormName );
end;

function TKNavCollectionItem.GetOwnerCollection: TKNavCollection;
begin
	Result := TKNavCollection( inherited GetOwnerCollection );
end;

procedure TKNavCollectionItem.SetFormName( const Value: string );
begin
	if ( Designing( Owner.Navigator ) or Loading( Owner.Navigator ) ) and
		 ( not CheckStrEqual( Value, FFormName ) ) then
		FFormName := Value;
end;

function TKNavCollectionItem.GetForm: TCustomForm;
var
	comp: TComponent;
begin
	Result := nil;
	if CheckTrimStr( FormName ) then
	begin
		comp := FindGlobalComponent( FormName );
		if CheckObject( comp ) then
		begin
			if CheckObjectClass( comp, TCustomForm ) then
				Result := ( comp as TCustomForm )
			else
				RaiseExceptionFmt( EKDBNavigator, sErrDBNavInvFrmClass, [comp.ClassName, Name] );
		end;
	end;
	FFormExists := CheckObject( Result );	
end;

function TKNavCollectionItem.DoPrintEvent: Boolean;
var
	f: TCustomForm;
begin
	f := Form;
	Result := ( Assigned( FOnPrint ) and CheckObject( f ) and
		( Screen.ActiveCustomForm = f ) );
	if ( Result ) then
		FOnPrint( Owner.Navigator, f );
end;

function TKNavCollectionItem.DoSearchEvent: Boolean;
var
	f: TCustomForm;
begin
	f := Form;
	Result := ( Assigned( FOnSearch ) and CheckObject( f ) and
		( Screen.ActiveCustomForm = f ) );
	if ( Result ) then
		FOnSearch( Owner.Navigator, f );
end;

{------------------------------- TKNavCollection -------------------------------}

constructor TKNavCollection.Create( ANav: TKCustomDBNavigator );
begin
	ForceObject( ANav );
	inherited Create( ANav, TKNavCollectionItem, False );
end;

function TKNavCollection.GetOwnerComp: TKCustomDBNavigator;
begin
	Result := TKCustomDBNavigator( inherited GetOwnerComp );
end;

procedure TKNavCollection.SetItem( Index: Integer; AItem: TKNavCollectionItem );
begin
	inherited SetItem( Index, AItem );
end;

function TKNavCollection.GetItem( Index: Integer ): TKNavCollectionItem;
begin
	Result := TKNavCollectionItem( inherited GetItem( Index ) );
end;

function TKNavCollection.Add: TKNavCollectionItem;
begin
	Result := TKNavCollectionItem( inherited Add );
end;

function TKNavCollection.GetItemByName( const AName: string ): TKNavCollectionItem;
begin
	Result := TKNavCollectionItem( inherited GetItemByName( AName ) );
end;

{---------------------------------- TKNavButton --------------------------------}

{--------------------------- Internal Implementation ---------------------------}

const
	REPEAT_PAUSE = 100;
	INIT_REPEAT_PAUSE = 400;

{---------------------------- Public Implementation ----------------------------}

destructor TKNavButton.Destroy;
begin
	FTimer.Free;
	inherited Destroy;
end;

procedure TKNavButton.MouseDown( Button: TMouseButton; Shift: TShiftState; X, Y: Integer );
begin
	inherited MouseDown( Button, Shift, X, Y );
	if ( nsAllowTimer in FStyle ) then
	begin
		if ( not CheckObject( FTimer ) ) then
			FTimer := TTimer.Create( Self );
		FTimer.OnTimer := TimerExpired;
		FTimer.Interval := INIT_REPEAT_PAUSE;
		FTimer.Enabled  := True;
	end;
end;

procedure TKNavButton.MouseUp( Button: TMouseButton; Shift: TShiftState;	X, Y: Integer );
begin
	inherited MouseUp( Button, Shift, X, Y );
	if CheckObject( FTimer ) then
		FTimer.Enabled  := False;
end;

procedure TKNavButton.TimerExpired( Sender: TObject );
begin
	FTimer.Interval := REPEAT_PAUSE;
	if ( ( FState = bsDown ) and MouseCapture ) then
		try
			Click;
		except
			FTimer.Enabled := False;
			raise;
		end;
end;

procedure TKNavButton.Paint;
var
	R: TRect;
begin
	inherited Paint;
	if ( ( GetFocus = Parent.Handle ) and
			 ( FIndex = TKCustomDBNavigator( Parent ).FFocusedButton ) ) then
	begin
		R := Bounds( 0, 0, Width, Height );
		InflateRect( R, -3, -3 );
		if ( FState = bsDown ) then
			OffsetRect( R, 1, 1 );
		DrawFocusRect( Canvas.Handle, R );
	end;
end;

{---------------------------------- TKNavDataLink ------------------------------}

constructor TKNavDataLink.Create( ANav: TKCustomDBNavigator );
begin
	inherited Create;
	FNavigator := ANav;
end;

destructor TKNavDataLink.Destroy;
begin
	FNavigator := nil;
	inherited Destroy;
end;

procedure TKNavDataLink.EditingChanged;
begin
	if CheckObject( FNavigator ) then
		FNavigator.DataChanged;
end;

procedure TKNavDataLink.DataSetChanged;
begin
	if CheckObject( FNavigator ) then
		FNavigator.DataChanged;
end;

procedure TKNavDataLink.ActiveChanged;
begin
	if CheckObject( FNavigator ) then
		FNavigator.ActiveChanged;
end;

{----------------------------- TKCustomDBNavigator -----------------------------}

{--------------------------- Internal Implementation ---------------------------}

const
	NAV_BUTTONS_PATTERN = 'NB_%s';

var
{ Do not resource }
	NavBmpIdxName: array[TNavIndex] of PChar = ( 'FIRST', 'PRIOR', 'NEXT', 'LAST',
		'SAVE', 'CANCEL', 'INSERT', 'DELETE', 'REFRESH', 'PRINT', 'SEARCH' );

	BtnHintId: array[TNavIndex] of Pointer = ( @sDNFirst, @sDNPrior, @sDNNext,
		@sDNLast, @sDNSave, @sDNCancel, @sDNInsert, @sDNDelete, @sDNRefresh,
		@sDNPrint, @sDNSearch );

{---------------------------- Public Implementation ----------------------------}

constructor TKCustomDBNavigator.Create( AOwner: TComponent );
begin
	ForceAnyPackagesRunning( ClassName, [perDBDialogs, perDBCtrls, perForms] );
	inherited Create( AOwner );
	ControlStyle := ControlStyle + [csOpaque] - [csAcceptsControls, csSetCaption];
	if ( not NewStyleControls ) then
		ControlStyle := ControlStyle + [csFramed];

	FMinSize := Point( ScaleX( 34 ), ScaleY( 32 ) );

	Align := alNone;
	ShowHint := true;
	Width := ScaleX( 394 );
	Height := FMinSize.y;
	BevelOuter := bvNone;
	BevelInner := bvNone;
	FFocusedButton := niFirst;

	FFlat := false;
	FSynchronization := smDisable;
	FNavButtons := DEF_NAV_BUTTONS;
	SyncNavAvailable;
	FHints := TStringList.Create;
	FNavCollection := TKNavCollection.Create( Self );
	FDataLink := TKNavDataLink.Create( Self );

	InitButtons;
end;

destructor TKCustomDBNavigator.Destroy;
begin
	FHints.Free;
	FDataLink.Free;
	FNavCollection.Free;
	inherited Destroy;
end;

procedure TKCustomDBNavigator.Loaded;
var
	W, H: Integer;
begin
	inherited Loaded;
	W := Width;
	H := Height;
	AdjustNavSize( W, H );
	if ( ( W <> Width ) or ( H <> Height ) ) then
		inherited SetBounds( Left, Top, W, H );
	InitHints;
	ActiveChanged;
end;

procedure TKCustomDBNavigator.Notification( AComponent: TComponent; Operation: TOperation );
begin
	inherited Notification( AComponent, Operation );
	if CheckObject( FDataLink ) and ( Operation = opRemove ) and
		( AComponent = DataSource ) then
		DataSource := nil;
end;

function TKCustomDBNavigator.GetDataSource: TDataSource;
begin
	Result := FDataLink.DataSource;
end;

function TKCustomDBNavigator.GetAvailableButtons( Index: TNavIndex ): Boolean;
begin
	Result := FAvailableButtons[Index];
end;

procedure TKCustomDBNavigator.SetFlat( Value: Boolean );
var
	i: TNavIndex;
begin
	if ( FFlat <> Value ) then
	begin
		FFlat := Value;
		for i := Low( Buttons ) to High( Buttons ) do
			Buttons[i].Flat := FFlat;
	end;
end;

procedure TKCustomDBNavigator.SetHints( Value: TStrings );
begin
	FHints.Assign( Value );
	InitHints;
end;

procedure TKCustomDBNavigator.SetNavMode( Value: TNavMode );
begin
	if ( FNavMode <> Value ) then
	begin
		FNavMode := Value;
		DataChanged;
		UpdateNavDisplay;
	end;
end;

procedure TKCustomDBNavigator.SetNavButtons( Value: TButtonSet );
var
	i: TNavIndex;
begin
	if ( FNavButtons <> Value ) then
	begin
		FNavButtons := Value;
		for i := Low( TNavIndex ) to High( TNavIndex ) do
			Buttons[i].Visible := ( i in FNavButtons );
		UpdateNavDisplay;
	end;
end;

procedure TKCustomDBNavigator.SetDataSource( Value: TDataSource );
begin
	FDataLink.DataSource := Value;
	if ( not Loading( Self ) ) then
		ActiveChanged;
	if CheckObject( Value ) then
		Value.FreeNotification( Self );
end;

procedure TKCustomDBNavigator.SetNavCollection( Value: TKNavCollection );
begin
	FNavCollection.Assign( Value );
end;

procedure TKCustomDBNavigator.SetSynchronization( Value: TSynchronizationMode );
var
	iWidth: Integer;
	Index: TNavIndex;
begin
	if ( FSynchronization <> Value ) then
	begin
		FSynchronization := Value;
		if ( not Loading( Self ) ) then
		begin
      iWidth := Width;
			if ( FSynchronization = smHide ) then
				for Index := Succ( niLast ) to High( TNavIndex ) do
					if ( not Buttons[Index].Enabled ) then
						iWidth := iWidth - Buttons[Index].Width;
			DataChanged;
			Width := iWidth;
			UpdateNavDisplay;
		end;
	end;
end;

procedure TKCustomDBNavigator.SetAvailableButtons( Index: TNavIndex; Value: Boolean );
var
	iWidth: Integer;
begin
	if ( Value <> FAvailableButtons[Index] ) then
	begin
		FAvailableButtons[Index] := Value;
		if ( not Loading( Self ) ) then
		begin
			iWidth := -1;
			if ( FSynchronization = smHide ) and
				 ( ( Index > niLast ) and Buttons[Index].Enabled ) then
			begin
				iWidth := Width;
				if Value then
					iWidth := iWidth + Buttons[Index].Width
				else
					iWidth := iWidth - Buttons[Index].Width;
			end;
			DataChanged;
			if ( iWidth <> -1 ) then
				Width := iWidth;
			UpdateNavDisplay;
		end;
	end;
end;

function TKCustomDBNavigator.ScaleX( Value: Integer ): LongInt;
begin
	Result := MulDiv( Value, Screen.Width, 800 );
end;

function TKCustomDBNavigator.ScaleY( Value: Integer ): LongInt;
begin
	Result := MulDiv( Value, Screen.Height, 600 );
end;

procedure TKCustomDBNavigator.InitHints;
var
	i: Integer;
	j: TNavIndex;
begin
	for j := Low( Buttons ) to High( Buttons ) do
		Buttons[j].Hint := LoadResString( BtnHintId[J] );
	j := Low( Buttons );
	for i := 0 to FHints.Count - 1 do
	begin
		if CheckStr( FHints.Strings[i] ) then
			Buttons[j].Hint := FHints.Strings[i];
		if ( j = High( Buttons ) ) then
			Exit;
		Inc( j );
	end;
end;

procedure TKCustomDBNavigator.InitButtons;
var
	x: Integer;
	i: TNavIndex;
	bFirstSpace,
	bSecondSpace: Boolean;
	btn: TKNavButton;
	bmp,
	bmps: TBitmap;
	rt: TRect;
	imSize: TPoint;
begin
	x := 0;
	bFirstSpace := false;
	bSecondSpace := false;
	ImSize := Point( ScaleX( 48 ), ScaleY( 24 ) );
	bmp := TBitmap.Create;
	try
		bmps := TBitmap.Create;
		try
			bmps.Width := ImSize.x;
			bmps.Height := ImSize.y;
			rt := Bounds( 0, 0, bmps.Width, bmps.Height );
			for i := Low( TNavIndex ) to High( TNavIndex ) do
			begin
				btn := TKNavButton.Create( Self );
				with btn do
				begin
					Parent := Self;
					Index := i;
{ first space between last and save }
					if ( i > niLast ) and ( not bFirstSpace ) then
					begin
						x := x + ScaleX( 10 );
						bFirstSpace := true;
					end;
{ second space between refresh and print }
					if ( i > niRefresh ) and ( not bSecondSpace ) then
					begin
						x := x + ScaleX( 10 );
						bSecondSpace := true;
					end;
					SetBounds( x, 0, FMinSize.X, FMinSize.Y );
					bmp.LoadFromResourceName( HInstance, Format( NAV_BUTTONS_PATTERN,
						[NavBmpIdxName[I]] ) );
					bmps.Canvas.StretchDraw( rt, bmp );
					Glyph.Assign( bmps );
					NumGlyphs := 2;
					Visible := true;
					Enabled := true;
					OnClick := NavClick;
					OnMouseDown := BtnMouseDown;
					x := x + FMinSize.X;
				end;
				Buttons[i] := btn;
			end;
		finally
			bmps.Free;
		end;
	finally
		bmp.Free;
	end;
	InitHints;
	with Buttons[niPrior] do
		Style := Style + [nsAllowTimer];
	with Buttons[niNext] do
		Style := Style + [nsAllowTimer];
	DataChanged;
end;

procedure TKCustomDBNavigator.UpdateNavDisplay;
var
	W, H: Integer;
begin
	W := Width;
	H := Height;
	AdjustNavSize( W, H );
{ AdjustNavSize returns better width/height values for SetBounds }
	if ( W <> Width ) or ( H <> Height ) then
		inherited SetBounds( Left, Top, W, H );
	Invalidate;
end;

procedure TKCustomDBNavigator.NavClick( Sender: TObject );
begin
	BtnClick( TKNavButton( Sender ).Index );
end;

procedure TKCustomDBNavigator.AdjustNavSize( var W: Integer; var H: Integer );
var
	i: TNavIndex;
	bGroup1,
	bGroup2,
	bGroup3,
	bFirstSpace,
	bSecondSpace: Boolean;
	iCount,
	idWidth,
	iMinWidth,
	ButtonWidth: Integer;
	ButtonHeight: Integer;
begin
{
	in load time no drawing, please; if the first button is not created yet
	no drawing, please
}
	if Loading( Self ) or ( not AnyButton ) then
		Exit;

{ no spaces, please }
	bGroup1 := false;
	bGroup2 := false;
	bGroup3 := false;

{ we've got no buttons to paint yet }
	iCount := 0;

{
	loop through all the buttons and check which ones will be painted;
	keep a record of groups to be painted: these groups will define the
	separations among themselves
}
	for i := Low( Buttons ) to High( Buttons ) do
		if Buttons[i].Visible then
		begin
			Inc( iCount );
			bGroup1 := bGroup1 or ( i <= niLast );
			bGroup2 := bGroup2 or ( ( i > niLast ) and ( i <= niRefresh ) );
			bGroup3 := bGroup3 or ( i > niRefresh );
		end;
	bFirstSpace := ( bGroup1 and bGroup2 ) or ( bGroup1 and bGroup3 and ( not bGroup2 ) );
	bSecondSpace := ( bFirstSpace and bGroup2 and bGroup3 );

{
	the min width is the number of buttons * minwidth plus the separation spaces-
	if applicable
}
	if ( iCount > 0 ) then
		iMinWidth := ( iCount * FMinSize.x ) +
								 ( Ord( bFirstSpace ) * ScaleX( 10 ) ) +
								 ( Ord( bSecondSpace ) * ScaleX( 10 ) )
	else
		iMinWidth := ( Ord( High( TNavIndex ) ) * FMinSize.x ) +
								 ( Ord( bFirstSpace ) * ScaleX( 10 ) ) +
								 ( Ord( bSecondSpace ) * ScaleX( 10 ) );

{ normalize width, if appropriate... }
	idWidth := 0;
	if ( W < iMinWidth ) then
		W := iMinWidth
	else if ( W > iMinWidth ) and ( iCount > 0 ) then
	begin
		idWidth := ( ( W - iMinWidth ) div iCount );
		W := iMinWidth + ( idWidth * iCount );
	end;

{ normalize height, if appropriate... }
	if ( H < FMinSize.Y ) then
		H := FMinSize.Y;

{ calculate button dimensions }
	ButtonHeight := H;
	ButtonWidth := FMinSize.x + idWidth;

{ call setbounds for each and every button }
	iCount := 0;
	for i := Low( Buttons ) to High( Buttons ) do
		if Buttons[i].Visible then
		begin
			Buttons[i].SetBounds(
				iCount * ButtonWidth +
				Ord( bFirstSpace and ( i > niLast ) ) * ScaleX( 10 ) +
				Ord( bSecondSpace and ( i > niRefresh ) ) * ScaleX( 10 ),
				0, ButtonWidth, ButtonHeight );
			Inc( iCount );
			W := Buttons[i].Left + Buttons[i].Width;
		end
		else
			Buttons[i].SetBounds( 0, Height + 1, ButtonWidth, Height );
end;

procedure TKCustomDBNavigator.BtnMouseDown( Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer );
var
	OldFocus: TNavIndex;
begin
	OldFocus := FFocusedButton;
	FFocusedButton := TKNavButton( Sender ).Index;
	if ( TabStop and ( GetFocus <> Handle ) and CanFocus ) then
	begin
		SetFocus;
		if ( GetFocus <> Handle ) then
			Exit;
	end
	else if ( TabStop and ( GetFocus = Handle ) and ( OldFocus <> FFocusedButton ) ) then
	begin
		Buttons[OldFocus].Invalidate;
		Buttons[FFocusedButton].Invalidate;
	end;
end;

procedure TKCustomDBNavigator.WMSize( var Message: TWMSize );
begin
	inherited;
	UpdateNavDisplay;
	Message.Result := 0;
end;

procedure TKCustomDBNavigator.WMSetFocus( var Message: TWMSetFocus );
begin
	Buttons[FFocusedButton].Invalidate;
end;

procedure TKCustomDBNavigator.WMKillFocus( var Message: TWMKillFocus );
begin
	Buttons[FFocusedButton].Invalidate;
end;

procedure TKCustomDBNavigator.WMGetDlgCode( var Message: TWMGetDlgCode );
begin
	Message.Result := DLGC_WANTARROWS;
end;

procedure TKCustomDBNavigator.CMEnabledChanged( var Message: TMessage );
begin
	inherited;
	if ( not Loading( Self ) ) then
		ActiveChanged;
end;

procedure TKCustomDBNavigator.DataChanged;
begin
	SyncNavButtons;
	if ( FSynchronization = smHide ) then
		UpdateNavDisplay;
end;

procedure TKCustomDBNavigator.ActiveChanged;
begin
	DataChanged;
end;

function TKCustomDBNavigator.AnyButton: Boolean;
var
	i: TNavIndex;
begin
	Result := false;
	for i := Low( Buttons ) to High( Buttons ) do
		Result := Result or CheckObject( Buttons[i] );
end;

procedure TKCustomDBNavigator.KeyDown( var Key: Word; Shift: TShiftState );
var
	NewFocus,
	OldFocus: TNavIndex;
begin
	OldFocus := FFocusedButton;
	case Key of
		VK_RIGHT:
		begin
			NewFocus := FFocusedButton;
			repeat
				if ( NewFocus < High( Buttons ) ) then
					NewFocus := Succ( NewFocus );
			until ( ( NewFocus = High( Buttons ) ) or Buttons[NewFocus].Visible );
			if ( NewFocus <> FFocusedButton ) then
			begin
				FFocusedButton := NewFocus;
				Buttons[OldFocus].Invalidate;
				Buttons[FFocusedButton].Invalidate;
			end;
		end;
		VK_LEFT:
		begin
			NewFocus := FFocusedButton;
			repeat
				if ( NewFocus > Low( Buttons ) ) then
					NewFocus := Pred( NewFocus );
			until ( ( NewFocus = Low( Buttons ) ) or Buttons[NewFocus].Visible );
			if ( NewFocus <> FFocusedButton ) then
			begin
				FFocusedButton := NewFocus;
				Buttons[OldFocus].Invalidate;
				Buttons[FFocusedButton].Invalidate;
			end;
		end;
		VK_SPACE:
		begin
			if Buttons[FFocusedButton].Enabled then
				Buttons[FFocusedButton].Click;
		end;
	end;
end;

procedure TKCustomDBNavigator.GetChildren( Proc: TGetChildProc; Root: TComponent );
begin
  { Do not stream the NavButtons... }
end;

procedure TKCustomDBNavigator.DoPrint;
var
	i: Integer;
	bCalled: Boolean;
begin
	bCalled := False;
	with NavEvents do
		for i := 0 to Count - 1 do
{$BOOLEVAL ON}
			bCalled := ( bCalled or Items[i].DoPrintEvent );
{$BOOLEVAL OFF}
	if ( ( not bCalled ) and Assigned( FOnPrint ) ) then
	  FOnPrint( Self );
end;

procedure TKCustomDBNavigator.DoSearch;
var
	i: Integer;
	bCalled: Boolean;
begin
	bCalled := False;
	with NavEvents do
		for i := 0 to Count - 1 do
{$BOOLEVAL ON}
			bCalled := ( bCalled or Items[i].DoSearchEvent );
{$BOOLEVAL OFF}
	if ( ( not bCalled ) and Assigned( FOnSearch ) ) then
		FOnSearch( Self );
end;

procedure TKCustomDBNavigator.SyncNavButtons;
var
	i: TNavIndex;
	bDesign,
	bUpEnable,
	bDnEnable,
	bCanModify,
	bActiveEditing,
	bActiveNotEditing: Boolean;
	Available: TButtonArray;
begin

{ ***** DataSource is NIL ***** }
	if ( not ( CheckObject( DataSource ) and CheckObject( FDataLink.DataSet ) ) ) then
	begin
		for i := Low( TNavIndex ) to High( TNavIndex ) do
			if ( i in FNavButtons ) then
				Buttons[i].Enabled := false;
		Exit;
	end;

	bDesign := Designing( Self );

{ ***** Navigator enabled;
				DataLink OK and active;
				Current record being edited; ***** }

	bActiveEditing := ( Enabled ) and
										( FDataLink.Active ) and
										( FDataLink.Editing );

{ ***** Navigator enabled;
				DataLink OK and active;
				Current record not being edited; ***** }

	bActiveNotEditing := ( Enabled ) and
											 ( FDataLink.Active ) and
											 ( not FDataLink.Editing );

{ ***** Active;
				Not at BOF; ***** }

	bUpEnable := ( bActiveNotEditing ) and
							 ( not FDataLink.DataSet.BOF );

{ ***** Active;
				Not at EOF; ***** }

	bDnEnable := ( bActiveNotEditing ) and
							 ( not FDataLink.DataSet.EOF );

{ ***** User is allowed to change data;
				Dataset can be modified; ***** }

	bCanModify := ( FNavMode = nmChange ) and
								( FDataLink.DataSet.CanModify );

{ ***** Set common availability for the buttons ***** }

	Available[niLast]  := bDnEnable and ( niLast in FNavButtons );
	Available[niNext]  := bDnEnable and ( niNext in FNavButtons );
	Available[niPrior] := bUpEnable and ( niPrior in FNavButtons );
	Available[niFirst] := bUpEnable and ( niFirst in FNavButtons );

	Available[niPrint]  := ( bDnEnable or bUpEnable ) and
												 ( niPrint in FNavButtons );
	Available[niSearch] := ( bDnEnable or bUpEnable ) and
												 ( niSearch in FNavButtons );

	Available[niSave]    := ( bCanModify ) and
													( bActiveEditing ) and
													( niSave in FNavButtons );
	Available[niCancel]  := ( bCanModify ) and
													( bActiveEditing ) and
													( niCancel in FNavButtons );
	Available[niInsert]  := ( bCanModify ) and
													( bActiveNotEditing ) and
													( niInsert in FNavButtons );
	Available[niRefresh] := ( bActiveNotEditing ) and
													( niRefresh in FNavButtons );
	Available[niDelete]  := ( bCanModify ) and
													( bActiveNotEditing ) and
													( niDelete in FNavButtons ) and
													( not ( FDataLink.DataSet.BOF and FDataLink.DataSet.EOF ) );

{ ***** Enabling navigation buttons ***** }

	Buttons[niLast].Enabled  := Available[niLast] and FAvailableButtons[niLast];
	Buttons[niNext].Enabled  := Available[niNext] and FAvailableButtons[niNext];
	Buttons[niFirst].Enabled := Available[niFirst] and FAvailableButtons[niFirst];
	Buttons[niPrior].Enabled := Available[niPrior] and FAvailableButtons[niPrior];

{ ***** Synchronizing control buttons ***** }

	Buttons[niSave].Enabled    := Available[niSave] and FAvailableButtons[niSave];
	Buttons[niCancel].Enabled  := Available[niCancel] and FAvailableButtons[niCancel];
	Buttons[niInsert].Enabled  := Available[niInsert] and FAvailableButtons[niInsert];
	Buttons[niRefresh].Enabled := Available[niRefresh] and FAvailableButtons[niRefresh];
	Buttons[niDelete].Enabled  := Available[niDelete] and FAvailableButtons[niDelete];

	Buttons[niPrint].Enabled   := Available[niPrint] and FAvailableButtons[niPrint];
	Buttons[niSearch].Enabled  := Available[niSearch] and FAvailableButtons[niSearch];

{ ***** Synchronizing visibility of the buttons ***** }

	if ( ( not bDesign ) and ( FSynchronization = smHide ) ) then
	begin

{ ***** Synchronizing control buttons ***** }

		Buttons[niSave].Visible    := Available[niSave] and FAvailableButtons[niSave];
		Buttons[niCancel].Visible  := Available[niCancel] and FAvailableButtons[niCancel];
		Buttons[niInsert].Visible  := Available[niInsert] and FAvailableButtons[niInsert];
		Buttons[niRefresh].Visible := Available[niRefresh] and FAvailableButtons[niRefresh];
		Buttons[niDelete].Visible  := Available[niDelete] and FAvailableButtons[niDelete];

		Buttons[niPrint].Visible   := Available[niPrint] and FAvailableButtons[niPrint];
		Buttons[niSearch].Visible  := Available[niSearch] and FAvailableButtons[niSearch];
	end
	else
	begin
{ ***** Synchronizing control buttons ***** }

		Buttons[niSave].Visible    := ( niSave in FNavButtons ); 
		Buttons[niCancel].Visible  := ( niCancel in FNavButtons );
		Buttons[niInsert].Visible  := ( niInsert in FNavButtons );
		Buttons[niRefresh].Visible := ( niRefresh in FNavButtons );
		Buttons[niDelete].Visible  := ( niDelete in FNavButtons );

		Buttons[niPrint].Visible   := ( niPrint in FNavButtons );
		Buttons[niSearch].Visible  := ( niSearch in FNavButtons );
	end;
end;

procedure TKCustomDBNavigator.SyncNavAvailable;
var
	i: TNavIndex;
begin
	for i := Low( TNavIndex ) to High( TNavIndex ) do
		FAvailableButtons[i] := ( i in FNavButtons );
end;

function TKCustomDBNavigator.CanDoAction( Index: TNavIndex ): Boolean;
begin
	Result := True;
	if Assigned( FBeforeAction ) then
		FBeforeAction( Self, Index, Result );
end;

procedure TKCustomDBNavigator.DoAction( Index: TNavIndex );
const
	SPECIAL_ACTION_BUTTONS = [niPrint, niSearch];
begin
	if ( ( not ( Index in SPECIAL_ACTION_BUTTONS ) ) and Assigned( FAfterAction ) ) then
		FAfterAction( Self, Index );
end;

procedure TKCustomDBNavigator.BtnClick( Index: TNavIndex );
begin
	if Designing( Self ) then
		Exit;
	if ( CanDoAction( Index ) and CheckObject( DataSource ) ) then
		with DataSource.DataSet do
			case Index of
				niFirst: First;
				niPrior: Prior;
				niNext: Next;
				niLast: Last;
				niSave: Post;
				niCancel: Cancel;
				niInsert: Insert;
				niDelete: Delete;
				niRefresh: Refresh;
				niPrint: DoPrint;
				niSearch: DoSearch;
			end;
	DoAction( Index );
end;

procedure TKCustomDBNavigator.SetBounds( ALeft, ATop, AWidth, AHeight: Integer );
var
	W, H: Integer;
begin
	W := AWidth;
	H := AHeight;
	AdjustNavSize( W, H );
{ AdjustNavSize returns better width/height values for SetBounds }
	inherited SetBounds( ALeft, ATop, W, H );
end;

end.
