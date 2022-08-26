// ------------------------------------------------------------------------------
// DPF.Android.JTabHost Component
//
// Dadeh Pardazane Faragir ( DPF ) Co.
//
// Web: http://www.dpfaragir.com
//
// Developed By: Babak Yaghoobi
//
// Email #1: yaghoobi@dpfaragir.com
// Email #2: b_yaghobi@yahoo.com
//
// ------------------------------------------------------------------------------
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
// ------------------------------------------------------------------------------
unit DPF.Android.JTabHost;

interface

{$I DPF.Android.Defs.inc}

uses
  System.SysUtils,
  System.Classes,
  System.Types,
  System.UITypes,
  System.Variants,

  System.TypInfo,
  System.Generics.Collections,
  DPF.Android.BaseControl,
  DPF.Android.JView,
  DPF.Android.JRelativeLayout,
  DPF.Android.Widget,
  DPF.Android.Common,
{$IFDEF ANDROID}
  Androidapi.JNI.Os,
  Androidapi.JNI.Widget,
  Androidapi.JNI.App,
  Androidapi.Jni,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNIBridge,
  Androidapi.JNI.GraphicsContentViewText,
  FMX.Helpers.Android,
  FMX.Platform.Android,
{$IFDEF DELPHIXE6}
  Androidapi.Helpers,
{$ENDIF}
{$ELSE}
  DPF.Android.DesignTime,
  System.Math,
  FMX.Menus,
{$ENDIF}
{$IFDEF DELPHIXE5} FMX.Graphics, {$ENDIF}FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls;

const
  FILL_PARENT  = -1;
  MATCH_PARENT = -1;
  WRAP_CONTENT = -2;

type

  TDPFJTabHost          = class;
  TDPFAndroidTabBarItem = class;

{$IFDEF ANDROID}

  TDPFJTabHostOnTabChangeListener = class( TJavaLocal, JTabHost_OnTabChangeListener )
  private
    FDPFJTabHost: TDPFJTabHost;
  public
    constructor create( ADPFJTabHost: TDPFJTabHost );
    procedure onTabChanged( tabId: JString ); cdecl;
  end;

{$ENDIF}

  TDPFTabBarSystemItem = ( tbsiNone, tbsiMore, tbsiFavorites, tbsiFeatured, tbsiTopRated, tbsiRecents, tbsiContacts, tbsiHistory, tbsiBookmarks, tbsiSearch, tbsiDownloads, tbsiMostRecent, tbsiMostViewed );

  TDPFOnTabChanged = procedure( Sender: TObject; SelectedIndex: Integer ) of object;

  // ----------------------------------------------------------------------------
  TDPFAndroidTabBar = class( TDPFAndBaseControl )
  private
{$IFNDEF ANDROID}
    TabPositions: array of TRectF;
    TabWidth    : single;
    Tabs        : integer;
    LastIsMore  : boolean;
    Popup       : TPopupMenu;
    procedure GetTabPositions;
    procedure Clicked( Button: TMouseButton; Shift: TShiftState; X, Y: Single );
    procedure PopupClicked( Sender: TObject );
{$ENDIF}
  public
{$IFNDEF ANDROID}
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;
    procedure Paint; override;
    procedure MouseDown( Button: TMouseButton; Shift: TShiftState; X, Y: Single ); override;
{$ENDIF}
  end;

  // ----------------------------------------------------------------------------
  TDPFTabBarProperty = class( TPersistent )
  private
    FDPFTabBarController: TDPFJTabHost;
    FBackgroundImage    : string;
    FAlpha              : Single;

    FVisible               : Boolean;
    FUserInteractionEnabled: Boolean;
    FTintColor             : TAlphaColor;
    FHeight                : Integer;
    procedure SetAlpha( const Value: Single );
    procedure SetVisible( const Value: Boolean );
    procedure SetUserInteractionEnabled( const Value: Boolean );
{$IFDEF ANDROID}
    procedure RealignTabBar( Visible: Boolean );
{$ENDIF}
  public
    constructor Create( ADPFTabBarController: TDPFJTabHost );
    destructor Destroy; override;

  published
    property Alpha          : Single read FAlpha write SetAlpha;
    property BackgroundImage: string read FBackgroundImage write FBackgroundImage;
    property Visible        : Boolean read FVisible write SetVisible default true;

    property UserInteractionEnabled: Boolean read FUserInteractionEnabled write SetUserInteractionEnabled default true;
  end;

  // ----------------------------------------------------------------------------
  // TDPFAndroidTabBarItem
  TDPFAndroidTabBarItem = class( TDPFJRelativeLayout )
  private
{$IFDEF ANDROID}
    // FJTabHost_TabSpec: JTabHost_TabSpec;

{$ENDIF}
    FItemTitle         : string;
    FItemImage         : string;
    FTabBarSystemItem  : TDPFTabBarSystemItem;
    FBadgeValue        : string;
    FPageControl       : TDPFJTabHost;
    FPageIndexInitValue: Integer;

    procedure SetPageControl( const APageControl: TDPFJTabHost );
    function GetPageIndex: Integer;
    procedure SetPageIndex( const Value: Integer );
    procedure SetTabBarSystemItem( const Value: TDPFTabBarSystemItem );
    procedure SetItemTitle( const Value: string );
    procedure SetBadgeValue( const Value: string );

  protected
  public
    procedure Loaded; override;
{$IFNDEF ANDROID}
    procedure Paint; override;
{$ENDIF}
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;
    property PageControl: TDPFJTabHost read FPageControl write SetPageControl;
  published
    property PageIndex       : Integer read GetPageIndex write SetPageIndex stored true;
    property ItemTitle       : string read FItemTitle write SetItemTitle;
    property ItemImage       : string read FItemImage write FItemImage;
    property BadgeValue      : string read FBadgeValue write SetBadgeValue;
    property TabBarSystemItem: TDPFTabBarSystemItem read FTabBarSystemItem write SetTabBarSystemItem default tbsiNone;

    // property Visible stored false;
    property Top stored false;
    property Height stored false;
    property Left stored false;
    property Width stored false;
  end;

  // ------------------------------------------------------------------------------
  // TDPFJTabHost Component
  // ------------------------------------------------------------------------------
  [ComponentPlatformsAttribute( PidWin32 or pidAndroid )]
  TDPFJTabHost = class( TDPFANDBaseControl )
  private
{$IFDEF ANDROID}
    FJTabHost                      : JTabHost;
    FJTabWidget                    : JTabWidget;
    FTabContentLayout              : JFrameLayout;
    FLinearLayout                  : JLinearLayout;
    FDPFJTabHostOnTabChangeListener: TDPFJTabHostOnTabChangeListener;
{$ENDIF}
    FBar            : TDPFAndroidTabBar;
    FPages          : TList<TDPFAndroidTabBarItem>;
    FActivePage     : TDPFAndroidTabBarItem;
    FTabBarPanel    : TDPFTabBarProperty;
    FActivePageIndex: Integer;
    FOnTabChanged   : TDPFOnTabChanged;
    function GetPage( index: Integer ): TDPFAndroidTabBarItem;
    function GetPageCount: Integer;
    procedure SetActivePage( const Page: TDPFAndroidTabBarItem );
    procedure ShowPage( Page: TDPFAndroidTabBarItem );

    procedure SetActivePageIndex( const Value: Integer );

    procedure SetTabBarPanel( const Value: TDPFTabBarProperty );
    function GetActivePage: TDPFAndroidTabBarItem;
  protected

{$IFNDEF ANDROID}
    procedure MouseDown( Button: TMouseButton; Shift: TShiftState; X, Y: Single ); override;
{$ENDIF}
    procedure Resize; override;
    procedure Move; override;
  public
{$IFDEF ANDROID}
    procedure Loaded; override;
{$ELSE}
{$ENDIF}
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;

{$IFNDEF ANDROID}
    procedure Change;
{$ENDIF}
    procedure RemovePage( Page: TDPFAndroidTabBarItem );
    procedure InsertPage( Page: TDPFAndroidTabBarItem );

    property Pages[index: Integer]: TDPFAndroidTabBarItem read GetPage;
    property PageCount: Integer read GetPageCount;
  published
    property ActivePage     : TDPFAndroidTabBarItem read GetActivePage write SetActivePage stored false;
    property ActivePageIndex: Integer read FActivePageIndex write SetActivePageIndex default -1;
    property TabBarPanel    : TDPFTabBarProperty read FTabBarPanel write SetTabBarPanel;
    property OnTabChanged   : TDPFOnTabChanged read FOnTabChanged write FOnTabChanged;

    property Alpha;
    property Align;
    property Position;
    property Width;
    property Height;
    property Enabled;
    property Visible;
  end;

  // ------------------------------------------------------------------------------

implementation

// ------------------------------------------------------------------------------
function GetRealParentForm( Control: TComponent; TopForm: Boolean = True ): TCustomForm;
begin
  while ( TopForm or not( Control is TCustomForm ) ) and ( TControl( Control ).Parent <> nil ) do
    Control := TControl( Control ).Parent;
  if Control is TCustomForm then
    Result := TCustomForm( Control )
  else
    Result := nil;
end;

// ------------------------------------------------------------------------------
[UIPermission( SecurityAction.LinkDemand, Window = UIPermissionWindow.AllWindows )]
function GetParentForm( Control: TControl; TopForm: Boolean = True ): TCustomForm;
begin
  // Override the "TopForm" parameter if the control passed in is in design mode
  // This makes controls calling this function operate correctly when the designer
  // is embedded
  if csDesigning in Control.ComponentState then
    TopForm := False;
  Result    := GetRealParentForm( Control, TopForm );
end;

// ------------------------------------------------------------------------------
{ TDPFAndroidTabBarItem }

constructor TDPFAndroidTabBarItem.Create( AOwner: TComponent );
begin
  inherited Create( AOwner );
  FPageIndexInitValue := -1;
  ControlCaption      := name;
  FBadgeValue         := '';
  FTabBarSystemItem   := tbsiNone;
  Align               := TAlignLayout.alClient;
  SetSubComponent( true );
  if AOwner is TDPFJTabHost then
    PageControl    := TDPFJTabHost( AOwner );
  AddThisToSubView := False;
  // IsAutoResize     := true;
{$IFDEF ANDROID}
  FJRelativeLayout.setId( GetUniqueViewID );
  // FJContentLayout.removeAllViews;
  // FJView.setId( GetUniqueViewID );
{$ENDIF}
end;

// ------------------------------------------------------------------------------
destructor TDPFAndroidTabBarItem.Destroy;
begin
  inherited;
end;

// ------------------------------------------------------------------------------
function TDPFAndroidTabBarItem.GetPageIndex: Integer;
begin
  if FPageControl <> nil then
    Result := FPageControl.FPages.IndexOf( Self )
  else
    Result := -1;
end;

// ------------------------------------------------------------------------------
procedure TDPFAndroidTabBarItem.Loaded;
begin
  inherited;
  if ( name <> '' ) and ( ParentControl is TDPFJTabHost ) then
    PageControl := TDPFJTabHost( ParentControl );
end;

{$IFNDEF ANDROID}

// ------------------------------------------------------------------------------
procedure TDPFAndroidTabBarItem.Paint;
var
  C: TAlphaColor;
begin
  C := TAlphaColors.White; // BackgroundColor;
  if C = TAlphaColors.Null then
    C := TAlphaColors.White;
  InternalPaint( '', TAlphaColors.Black, TDPFTextAlignment.taCenter, C );
end;
{$ENDIF}

// ------------------------------------------------------------------------------
procedure TDPFAndroidTabBarItem.SetBadgeValue( const Value: string );
begin
  FBadgeValue := Value;
end;

// ------------------------------------------------------------------------------
procedure TDPFAndroidTabBarItem.SetItemTitle( const Value: string );
begin
  FItemTitle := Value;
end;

// ------------------------------------------------------------------------------
procedure TDPFAndroidTabBarItem.SetPageControl( const APageControl: TDPFJTabHost );
begin
  if FPageControl <> APageControl then
  begin
    if FPageControl <> nil then
    begin
{$IFNDEF ANDROID}
      ( Parent as TDPFJTabHost ).Invalidate;
{$ENDIF}
      FPageControl.RemovePage( Self );
    end;
    Parent := APageControl;
    if APageControl <> nil then
    begin
{$IFNDEF ANDROID}
      ( Parent as TDPFJTabHost ).Invalidate;
{$ENDIF}
      APageControl.InsertPage( Self );
    end;
  end;
end;

// ------------------------------------------------------------------------------
procedure TDPFAndroidTabBarItem.SetPageIndex( const Value: Integer );
var
  I, MaxPageIndex: Integer;
begin
  if FPageControl = nil then
    FPageIndexInitValue := Value;

  if ( FPageControl <> nil ) { and ( csDesigning in ComponentState ) } then
  begin
    MaxPageIndex := FPageControl.FPages.Count - 1;
    if Value > MaxPageIndex then
      raise EListError.Create( 'TDPFAndroidTabBarItem.SetPageIndex: ' + IntToStr( Value ) );

    FPageControl.FPages.Move( PageIndex, Value );

    for I := 0 to FPageControl.FPages.Count - 1 do
    begin
      TDPFAndroidTabBarItem( FPageControl.FPages[I] ).index := I;
    end;

  end;
{$IFNDEF ANDROID}
  ( Parent as TDPFJTabHost ).Invalidate;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFAndroidTabBarItem.SetTabBarSystemItem( const Value: TDPFTabBarSystemItem );
begin
  FTabBarSystemItem := Value;
{$IFNDEF ANDROID}
  ( Parent as TDPFJTabHost ).Invalidate;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
{ TDPFJTabHost }
// ------------------------------------------------------------------------------
{$IFNDEF ANDROID}

procedure TDPFJTabHost.Change;
var
  Form: TCustomForm;
begin
  if csDesigning in ComponentState then
  begin
    Form := GetParentForm( Self );
    if ( Form <> nil ) and ( Form.Designer <> nil ) then
    begin
      Form.Designer.Modified;
      ShowMessage( 'Changed!' );
    end;
  end;
end;
{$ENDIF}

// ------------------------------------------------------------------------------
constructor TDPFJTabHost.Create( AOwner: TComponent );
{$IFDEF ANDROID}
// var GU: TGUID;
{$ENDIF}
begin
  inherited Create( AOwner );
{$IFNDEF ANDROID}
  FDesignInteractive := True;
{$ENDIF}
  ControlCaption := 'TabHost';

  FPages       := TList<TDPFAndroidTabBarItem>.Create;
  FTabBarPanel := TDPFTabBarProperty.Create( Self );
  FBar         := TDPFAndroidTabBar.Create( Self );
  FBar.Parent  := Self;
{$IFDEF WIN32}
  FBar.Height         := 45;
  FBar.Align          := TAlignLayout.alTop;
  FBar.Locked         := true;
  FBar.Stored         := false;
  FBar.ControlCaption := 'Tab Bar';
  FBar.BringToFront;
  Self.AddObject( FBar );
{$ENDIF}
  FActivePageIndex := -1;

{$IFDEF ANDROID}
  FJTabHost := TJTabHost.JavaClass.init( SharedActivityContext, nil ); // nil very important for not raise exception !!!
  FJTabHost.setId( 16908306 ); // TabHost
  JControl := FJTabHost;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
destructor TDPFJTabHost.Destroy;
var
  I: Integer;
begin
  for I := 0 to FPages.Count - 1 do
  begin
    FPages[I].FPageControl := nil;
  end;
  FPages.DisposeOf;
  FTabBarPanel.DisposeOf;
  FBar.DisposeOf;

{$IFDEF ANDROID}
{$ENDIF}
  inherited;
end;

// ------------------------------------------------------------------------------
{$IFDEF ANDROID}

procedure TDPFJTabHost.Loaded;
var
  I                   : Integer;
  s                   : string;
  FTabBarItem         : TDPFAndroidTabBarItem;
  LocalActivityManager: JLocalActivityManager;
  V1, V2              : JView;
  FJTabHost_TabSpec   : JTabHost_TabSpec;
begin
  try
    FLinearLayout := TJLinearLayout.JavaClass.init( SharedActivityContext, nil );
    FLinearLayout.setOrientation( TJLinearLayout.JavaClass.VERTICAL );
    FLinearLayout.setBackgroundColor( TAlphaColors.Brown );
    FLinearLayout.setLayoutParams( TJLinearLayout_LayoutParams.JavaClass.init( FILL_PARENT, WRAP_CONTENT ) );

    FJTabWidget := TJTabWidget.JavaClass.init( SharedActivityContext, nil );
    FJTabWidget.setId( 16908307 ); // 16908307  = tabs
    FJTabWidget.setBackgroundColor( TAlphaColors.Green );
    FJTabWidget.setLayoutParams( TJLinearLayout_LayoutParams.JavaClass.init( FILL_PARENT, FILL_PARENT ) );

    FTabContentLayout := TJFrameLayout.JavaClass.init( SharedActivityContext, nil );
    FTabContentLayout.setId( 16908305 ); // 16908305 = tabcontent
    FTabContentLayout.setBackgroundColor( TAlphaColors.Yellow );
    FTabContentLayout.setLayoutParams( TJLinearLayout_LayoutParams.JavaClass.init( FILL_PARENT, FILL_PARENT ) );
    FTabContentLayout.setPadding( 0, 0, 0, 0 );

    FLinearLayout.addView( FJTabWidget );
    FLinearLayout.addView( FTabContentLayout );

    FJTabHost.addView( FLinearLayout );

    // LocalActivityManager := TJLocalActivityManager.JavaClass.init( SharedActivity, true );
    // LocalActivityManager.dispatchCreate( LocalActivityManager.saveInstanceState );
    FJTabHost.setup( { LocalActivityManager } ); // important

    FDPFJTabHostOnTabChangeListener := TDPFJTabHostOnTabChangeListener.create( self );
    FJTabHost.setOnTabChangedListener( FDPFJTabHostOnTabChangeListener );

    // ---------------------------------------------------------------------------
    for I := PageCount - 1 downto 0 do
    begin
      FTabBarItem := Pages[I];
      // FTabBarItem.FJView.setBackgroundColor( TAlphaColors.Yellow );
      // FTabBarItem.FJView.setLayoutParams( TJLinearLayout_LayoutParams.JavaClass.init( FILL_PARENT, FILL_PARENT ) );
      FTabBarItem.FJRelativeLayout.setX( 0 );
      FTabBarItem.FJRelativeLayout.setY( 0 );
      FTabBarItem.FJRelativeLayout.setLayoutParams( TJLinearLayout_LayoutParams.JavaClass.init( FILL_PARENT, FILL_PARENT ) );
      try
        FTabContentLayout.addView( FTabBarItem.FJRelativeLayout );
      except
        on e: exception do
          DPFNSLog( e.Message );
      end;

      FJTabHost_TabSpec := FJTabHost.newTabSpec( StringToJString( 'Tab ' + i.toString ) );
      FJTabHost_TabSpec.setIndicator( StrToJCharSequence( FTabBarItem.ItemTitle ) );
      FJTabHost_TabSpec.setContent( FTabBarItem.FJRelativeLayout.getId );
      FJTabHost.addTab( FJTabHost_TabSpec );
    end;
  except
    on e: exception do
    begin
      DPFNSLog( e.Message );
    end;
  end;
  AddSubView( Self, ParentControl );
  // ----------------------------
  // Important
  inherited;
end;
{$ENDIF}

// ------------------------------------------------------------------------------
function TDPFJTabHost.GetActivePage: TDPFAndroidTabBarItem;
begin
  Result := nil;
  if ( FActivePageIndex > -1 ) and ( FActivePageIndex < FPages.Count ) then
    Result := FPages[ActivePageIndex];
end;

// ------------------------------------------------------------------------------
function TDPFJTabHost.GetPage( index: Integer ): TDPFAndroidTabBarItem;
begin
  Result := nil;
  if ( index > 0 ) or ( index < FPages.Count ) then
    Result := FPages[index];
end;

// ------------------------------------------------------------------------------
function TDPFJTabHost.GetPageCount: Integer;
begin
  Result := FPages.Count;
end;

// ------------------------------------------------------------------------------
procedure TDPFJTabHost.InsertPage( Page: TDPFAndroidTabBarItem );
begin
  if ( csLoading in ComponentState ) then
    FPages.Insert( 0, Page )
  else
    FPages.Add( Page );

  Page.FPageControl := Self;

  if FBar.Parent <> Self then
    FBar.Parent := Self;

  // Page.UpdateTabShowing;
end;

// ------------------------------------------------------------------------------
procedure TDPFJTabHost.RemovePage( Page: TDPFAndroidTabBarItem );
begin
  Page.FPageControl := nil;
  FPages.Remove( Page );
end;

// ------------------------------------------------------------------------------
procedure TDPFJTabHost.Resize;
begin
  inherited;
{$IFDEF ANDROID}
  if FJTabHost <> nil then
  begin
    // FJTabHost.view.SetFrame( CGRectMake( Position.X, Position.Y, Width * Scale.X, Height * Scale.Y ) );
    TabBarPanel.RealignTabBar( TabBarPanel.FVisible );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFJTabHost.SetActivePage( const Page: TDPFAndroidTabBarItem );
begin
  if Page = nil then
  begin
    if ActivePageIndex <> -1 then
      ActivePageIndex := -1;
    Exit;
  end;
  if Page = FActivePage then
    Exit;

  if Page.PageControl <> Self then
    Exit;
  ShowPage( Page );
  // Page.BringToFront;

  if Assigned( Application ) and Assigned( Application.MainForm ) then
    TCustomForm( Application.MainForm ).ActiveControl := Page;

  ActivePageIndex := Page.PageIndex;
  FActivePage     := Page;
{$IFNDEF ANDROID}
  // Change;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFJTabHost.SetActivePageIndex( const Value: Integer );
begin
  if FActivePageIndex = Value then
    exit;

  FActivePageIndex := Value;
  if ( Value > -1 ) and ( Value < PageCount ) then
  begin
    ActivePage := FPages[Value];
{$IFDEF ANDROID}
    if not TabBarPanel.Visible then
    begin
      // SetAnimationTransition( ActivePage.AnimationTransition );
      // if Assigned( FOnSelectPage ) then
      // OnSelectPage( Self, Value );
    end;
    FJTabHost.setCurrentTab( Value );
{$ELSE}
    Invalidate;
    ShowPage( FActivePage );
    // FActivePage.BringToFront;
{$ENDIF}
  end
  else
    ActivePage := nil;
end;

// ------------------------------------------------------------------------------
procedure TDPFJTabHost.SetTabBarPanel( const Value: TDPFTabBarProperty );
begin
  FTabBarPanel.Assign( Value );
end;

// ------------------------------------------------------------------------------
procedure TDPFJTabHost.ShowPage( Page: TDPFAndroidTabBarItem );
var
  I: Integer;
begin
  for I := 0 to FPages.Count - 1 do
{$IFDEF DELPHIXE7}
    FPages[I].Visible := false;
{$ELSE}
    FPages[I].DesignVisible := false;
{$ENDIF}
{$IFDEF DELPHIXE7}
  Page.Visible := true;
{$ELSE}
  Page.DesignVisible := true;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
{$IFNDEF ANDROID}

procedure TDPFJTabHost.MouseDown( Button: TMouseButton; Shift: TShiftState; X, Y: Single );
begin
  if Y >= FBar.Top then
    FBar.Clicked( Button, Shift, X, Y - FBar.Top );
end;
{$ENDIF}

// ------------------------------------------------------------------------------
procedure TDPFJTabHost.Move;
begin
  Resize;
end;

// ------------------------------------------------------------------------------
{ TDPFTabBarProperty }
constructor TDPFTabBarProperty.Create( ADPFTabBarController: TDPFJTabHost );
begin
  inherited Create;
  FDPFTabBarController    := ADPFTabBarController;
  FTintColor              := TAlphaColors.Null;
  FVisible                := true;
  FUserInteractionEnabled := true;
  FAlpha                  := 1;
  FHeight                 := 49;
end;

// ------------------------------------------------------------------------------
destructor TDPFTabBarProperty.Destroy;
begin

  inherited;
end;

// ------------------------------------------------------------------------------
procedure TDPFTabBarProperty.SetAlpha( const Value: Single );
begin
  FAlpha := Value;
{$IFDEF ANDROID}
  if ( FDPFTabBarController <> nil ) and ( FDPFTabBarController.FJTabHost <> nil ) then
    FDPFTabBarController.FJTabHost.setAlpha( FAlpha );
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFTabBarProperty.SetUserInteractionEnabled( const Value: Boolean );
begin
  FUserInteractionEnabled := Value;
{$IFDEF ANDROID}
  if ( FDPFTabBarController <> nil ) and ( FDPFTabBarController.FJTabHost <> nil ) then
    FDPFTabBarController.FJTabHost.setClickable( FUserInteractionEnabled );
{$ENDIF}
end;

// ------------------------------------------------------------------------------
{$IFDEF ANDROID}

procedure TDPFTabBarProperty.RealignTabBar( Visible: Boolean );
begin
end;
{$ENDIF}

// ------------------------------------------------------------------------------
procedure TDPFTabBarProperty.SetVisible( const Value: Boolean );
begin
  if FVisible = Value then
    Exit;

  FDPFTabBarController.FBar.Visible := Value;

{$IFDEF ANDROID}
{$ELSE}
  if Value then
    FDPFTabBarController.FBar.Height := 49
  else
    FDPFTabBarController.FBar.Height := 0;
{$ENDIF}
  FVisible := Value;
  FDPFTabBarController.Realign;
end;

// ------------------------------------------------------------------------------

{ TDPFTabBar }
{$IFNDEF ANDROID}

constructor TDPFAndroidTabBar.Create( AOwner: TComponent );
begin
  inherited;
  Popup := TPopupMenu.Create( nil );
end;

// ------------------------------------------------------------------------------
destructor TDPFAndroidTabBar.Destroy;
begin
  Popup.Clear;
  Popup.Free;
  inherited;
end;

// ------------------------------------------------------------------------------
procedure TDPFAndroidTabBar.GetTabPositions;
var
  i         : integer;
  x         : single;
  TabControl: TDPFJTabHost;
  MaxTabs   : integer;

begin
  TabControl := Parent as TDPFJTabHost;
  MaxTabs    := Round( TabControl.Width ) div 64;
  if TabControl.PageCount > MaxTabs then
  begin
    Tabs       := MaxTabs;
    LastIsMore := true;
  end
  else
  begin
    Tabs       := TabControl.PageCount;
    LastIsMore := false;
  end;
  SetLength( TabPositions, Tabs );
  if Tabs = 0 then
  begin
    TabWidth := Self.Width;
    Exit;
  end;
  TabWidth := Width / Tabs;
  x        := 0;
  for i    := 0 to Tabs - 1 do
  begin
    TabPositions[i].Left   := x;
    TabPositions[i].Top    := 0;
    TabPositions[i].Bottom := Height;
    TabPositions[i].Right  := x + TabWidth;
    x                      := x + TabWidth;
  end;
end;

// ------------------------------------------------------------------------------
procedure TDPFAndroidTabBar.MouseDown( Button: TMouseButton; Shift: TShiftState; X, Y: Single );
begin
  if csDesigning in ComponentState then
    inherited
  else
    Clicked( Button, Shift, X, Y );
end;

// ------------------------------------------------------------------------------
procedure TDPFAndroidTabBar.Clicked( Button: TMouseButton; Shift: TShiftState; X, Y: Single );
var
  i, c      : integer;
  mi        : TMenuItem;
  TabControl: TDPFJTabHost;

begin
  GetTabPositions;
  c     := -1;
  for i := 0 to Tabs - 1 do
  begin
    if ( X >= TabPositions[i].Left ) and ( X <= TabPositions[i].Right ) then
    begin
      c := i;
      break;
    end;
  end;
  if c = -1 then
    Exit;
  if ( c = Tabs - 1 ) and LastIsMore then
  begin
    Popup.Clear;
    TabControl := Parent as TDPFJTabHost;
    for i      := c to TabControl.PageCount - 1 do
    begin
      mi := TMenuItem.Create( nil );
      if TabControl.Pages[i].ItemTitle <> '' then
        mi.Text := TabControl.Pages[i].ItemTitle
      else
        mi.Text  := TabControl.Pages[i].Name;
      mi.Tag     := i;
      mi.OnClick := PopupClicked;
      Popup.AddObject( mi );
    end;
    Popup.Popup( Screen.MousePos.X, Screen.MousePos.Y );
  end
  else
    ( Parent as TDPFJTabHost ).SetActivePageIndex( c );
end;

// ------------------------------------------------------------------------------
procedure TDPFAndroidTabBar.Paint;
var
  TabControl: TDPFJTabHost;
  Caption, s: string;
  Bmp       : TBitmap;
  SystemItem: TDPFTabBarSystemItem;
  i         : integer;
  x, w      : single;
  R         : TRectF;

begin
  Canvas.BeginScene;
  TabControl := Parent as TDPFJTabHost;
  // Background
  BitmapAsBackground( Self, iOS_GUI_Bitmaps.TabBar.BG );
  // Tabs
  GetTabPositions;
  if Tabs = 0 then
  begin
    Canvas.EndScene;
    Exit;
  end;
  for i := 0 to Tabs - 1 do
  begin
    Bmp     := nil;
    Caption := '';
    if ( i = Tabs - 1 ) and LastIsMore then
      SystemItem := tbsiMore
    else
      SystemItem := TabControl.Pages[i].TabBarSystemItem;
    case SystemItem of
      tbsiNone:
        Caption := TabControl.Pages[i].ItemTitle;
      tbsiBookmarks:
        begin
          if TabControl.ActivePageIndex = i then
            Bmp := iOS_GUI_Bitmaps.TabBar.Bookmark_On
          else
            Bmp   := iOS_GUI_Bitmaps.TabBar.Bookmark_Off;
          Caption := 'Bookmarks';
        end;
      tbsiContacts:
        begin
          if TabControl.ActivePageIndex = i then
            Bmp := iOS_GUI_Bitmaps.TabBar.Contacts_On
          else
            Bmp   := iOS_GUI_Bitmaps.TabBar.Contacts_Off;
          Caption := 'Contacts';
        end;
      tbsiDownloads:
        begin
          if TabControl.ActivePageIndex = i then
            Bmp := iOS_GUI_Bitmaps.TabBar.Downloads_On
          else
            Bmp   := iOS_GUI_Bitmaps.TabBar.Downloads_Off;
          Caption := 'Downloads';
        end;
      tbsiFavorites:
        begin
          if TabControl.ActivePageIndex = i then
            Bmp := iOS_GUI_Bitmaps.TabBar.Favorites_On
          else
            Bmp   := iOS_GUI_Bitmaps.TabBar.Favorites_Off;
          Caption := 'Favorites';
        end;
      tbsiFeatured:
        begin
          if TabControl.ActivePageIndex = i then
            Bmp := iOS_GUI_Bitmaps.TabBar.Featured_On
          else
            Bmp   := iOS_GUI_Bitmaps.TabBar.Featured_Off;
          Caption := 'Featured';
        end;
      tbsiHistory:
        begin
          if TabControl.ActivePageIndex = i then
            Bmp := iOS_GUI_Bitmaps.TabBar.History_On
          else
            Bmp   := iOS_GUI_Bitmaps.TabBar.History_Off;
          Caption := 'History';
        end;
      tbsiMore:
        begin
          Bmp     := iOS_GUI_Bitmaps.TabBar.More_Off;
          Caption := 'More';
        end;
      tbsiMostRecent:
        begin
          if TabControl.ActivePageIndex = i then
            Bmp := iOS_GUI_Bitmaps.TabBar.MostRecent_On
          else
            Bmp   := iOS_GUI_Bitmaps.TabBar.MostRecent_Off;
          Caption := 'Most Recent';
        end;
      tbsiMostViewed:
        begin
          if TabControl.ActivePageIndex = i then
            Bmp := iOS_GUI_Bitmaps.TabBar.MostViewed_On
          else
            Bmp   := iOS_GUI_Bitmaps.TabBar.MostViewed_Off;
          Caption := 'Most Viewed';
        end;
      tbsiRecents:
        begin
          if TabControl.ActivePageIndex = i then
            Bmp := iOS_GUI_Bitmaps.TabBar.History_On
          else
            Bmp   := iOS_GUI_Bitmaps.TabBar.History_Off;
          Caption := 'Recents';
        end;
      tbsiSearch:
        begin
          if TabControl.ActivePageIndex = i then
            Bmp := iOS_GUI_Bitmaps.TabBar.Search_On
          else
            Bmp   := iOS_GUI_Bitmaps.TabBar.Search_Off;
          Caption := 'Search';
        end;
      tbsiTopRated:
        begin
          if TabControl.ActivePageIndex = i then
            Bmp := iOS_GUI_Bitmaps.TabBar.Favorites_On
          else
            Bmp   := iOS_GUI_Bitmaps.TabBar.Favorites_Off;
          Caption := 'Top Rated';
        end;
    end;
    if Bmp <> nil then
      BitmapToPosition( Self, Bmp, TabPositions[i].Left + TabWidth / 2 - Bmp.Width / 2, 3 );
    Canvas.Font.Size  := 9;
    Canvas.Font.Style := [TFontStyle.fsBold];
    if SystemItem = tbsiMore then
      Canvas.Fill.Color := $FF999999
    else
    begin
      if TabControl.ActivePageIndex = i then
        Canvas.Fill.Color := TAlphaColors.White
      else
        Canvas.Fill.Color := $FF999999;
    end;
    Canvas.Font.Family := 'Helvetica';
    PaintCaption( Self, Caption, RectF( TabPositions[i].Left, 35, TabPositions[i].Right, Height ), lbMiddleTruncation, 1, TTextAlign.taCenter );

    // Badge
    if SystemItem <> tbsiMore then
    begin
      s := TabControl.Pages[i].BadgeValue;
      if s <> '' then
      begin
        Canvas.Font.Size   := 9;
        Canvas.Font.Style  := [TFontStyle.fsBold];
        Canvas.Font.Family := 'Helvetica';
        Canvas.Fill.Color  := $FFFFFFFF;
        if Length( s ) = 1 then
          w := 22
        else
          w := Canvas.TextWidth( s ) + 16;
        x   := Round( TabPositions[i].Right - 14 - ( w / 2 ) );
        BitmapToPosition( Self, iOS_GUI_Bitmaps.TabBar.BadgeLeft, x, 2 );
        R.Left   := x + iOS_GUI_Bitmaps.TabBar.BadgeLeft.Width;
        R.Top    := 2;
        R.Bottom := 2 + iOS_GUI_Bitmaps.TabBar.BadgeBG.Height;
        R.Right  := x + w - iOS_GUI_Bitmaps.TabBar.BadgeRight.Width;
        BitmapToRect( Self, iOS_GUI_Bitmaps.TabBar.BadgeBG, R );
        BitmapToPosition( Self, iOS_GUI_Bitmaps.TabBar.BadgeRight, x + w - iOS_GUI_Bitmaps.TabBar.BadgeRight.Width, 2 );
        R.Left  := x;
        R.Right := x + w;
        R.Top   := 7;
        Canvas.FillText( R, s, false, 1, [], TTextAlign.taCenter, TTextAlign.taLeading );
      end;
    end;
  end;
  Canvas.EndScene;
end;

// ------------------------------------------------------------------------------
procedure TDPFAndroidTabBar.PopupClicked( Sender: TObject );
begin
  ( Parent as TDPFJTabHost ).SetActivePageIndex( ( Sender as TMenuItem ).Tag );
end;
{$ENDIF}
{$IFDEF ANDROID}

// ------------------------------------------------------------------------------
constructor TDPFJTabHostOnTabChangeListener.create( ADPFJTabHost: TDPFJTabHost );
begin
  inherited create;
  FDPFJTabHost := ADPFJTabHost;
end;

// ------------------------------------------------------------------------------
procedure TDPFJTabHostOnTabChangeListener.onTabChanged( tabId: JString ); cdecl;
var
  idx: Integer;
begin
  idx := FDPFJTabHost.FJTabHost.getCurrentTab;
  if assigned( FDPFJTabHost.FOnTabChanged ) then
    FDPFJTabHost.FOnTabChanged( FDPFJTabHost, Idx );
end;

// ------------------------------------------------------------------------------
{$ENDIF}

end.
