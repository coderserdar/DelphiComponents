// ------------------------------------------------------------------------------
// DPF.iOS.UIToolbar Component
//
// Dadeh Pardazane Faragir ( DPF ) Co.
//
// Web: http://www.dpfaragir.com
//
// Developed By: Babak Yaghoobi
//
// Email #1: yaghoobi@dpfaragir.com
// Email #2: b_yaghobi@yahoo.com
// Email #3: bayaghoobi@gmail.com
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
unit DPF.iOS.UIToolbar;

interface

{$I DPF.iOS.Defs.inc}

uses
  System.SysUtils,
  System.Classes,
  System.Types,
  System.UITypes,
  System.Variants,

  System.TypInfo,
  DPF.iOS.BaseControl,
  DPF.iOS.UIView,
  DPF.iOS.UIFont,
{$IFDEF IOS}
  Macapi.ObjectiveC,
  Macapi.ObjCRuntime,
  IOSapi.CocoaTypes,
  IOSapi.Foundation,
  IOSapi.Uikit,
  IOSapi.CoreGraphics,
  IOSapi.CoreImage,
  FMX.Platform.iOS,
  DPF.iOS.Common,
  DPF.iOS.Classes,
{$ELSE}
  // Added by Fenistil
  DPF.iOS.DesignTime,
  System.Math,
{$ENDIF}
  FMX.Layouts, FMX.Memo,
{$IFDEF DELPHIXE5} FMX.Graphics, {$ENDIF}FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls;

type
{$M+}
  TDPFToolbar     = class;
  TDPFToolbarItem = class;
{$IFDEF IOS}
  TDPFToolBarButtonDelegate = class;

  UIToolbar = interface( UIView )
    ['{90062BBC-88E7-4427-B3A5-8BB14FA23677}']
    function backgroundImageForToolbarPosition( topOrBottom: UIToolbarPosition; barMetrics: UIBarMetrics ): UIImage; cdecl;
    function barStyle: UIBarStyle; cdecl;
    function isTranslucent: Boolean; cdecl;
    function items: NSArray; cdecl;
    procedure setBackgroundImage( backgroundImage: UIImage; forToolbarPosition: UIToolbarPosition; barMetrics: UIBarMetrics ); cdecl;
    procedure setBarStyle( barStyle: UIBarStyle ); cdecl;
    procedure setItems( items: NSArray ); cdecl; overload;
    procedure setItems( items: NSArray; animated: Boolean ); cdecl; overload;
    procedure setTintColor( tintColor: UIColor ); cdecl;
    procedure setTranslucent( translucent: Boolean ); cdecl;
    function tintColor: UIColor; cdecl;
    function barTintColor: UIColor; cdecl; // Available in iOS 7.0 and later.
    procedure setBarTintColor( barTintColor: UIColor ); cdecl; // Available in iOS 7.0 and later.
  end;

  TUIToolbar = class( TOCGenericImport<UIToolbarClass, UIToolbar> )
  end;

{$ENDIF}

  TDPFToolbarPosition = ( tpAny = 0, tpBottom = 1, tpTop = 2 );

  // ----------------------------------------------------------------------------
  // Toolbar Sections
  TDPFToolbarItem = class( TCollectionItem )
  private
{$IFDEF IOS}
    FButtonDelegate: TDPFToolBarButtonDelegate;
    FDPFBarItem    : UIBarButtonItem;
{$ENDIF}
    FOwner           : TCollection;
    FTitle           : string;
    FEnabled         : Boolean;
    FImage           : string;
    FWidth           : Single;
    FButtonItemStyle : TDPFBarButtonItemStyle;
    FButtonSystemItem: TDPFBarButtonSystemItem;
    FCustomView      : TDPFiOSBaseControl;
    FBitmap          : TBitmap;
    FButtonItemKind  : TDPFBarButtonKind;
    FHeight          : Single;
    FOnClick         : TNotifyEvent;
    FFont            : TDPFFont;
    FTintColor       : TAlphaColor;
    FModalResult     : TModalResult;
    {$IFDEF MSWINDOWS}
    FButtonRect : TRectF;
    {$ENDIF}
    procedure SetEnabled( const Value: Boolean );
    procedure SetBitmap( const Value: TBitmap );
    procedure SetTitle( const Value: string );
    procedure SetButtonSystemItem( const Value: TDPFBarButtonSystemItem );
    procedure SetButtonItemStyle( const Value: TDPFBarButtonItemStyle );
    procedure SetFont( const Value: TDPFFont );
    procedure SetTintColor( const Value: TAlphaColor );
    procedure SetButtonItemKind( const Value: TDPFBarButtonKind );
    procedure SetModalResult(const Value: TModalResult);
    procedure SetOnClick(const Value: TNotifyEvent);
  protected
    function GetDisplayName: string; override;
    procedure DoClick; virtual;
  public
{$IFDEF IOS}
    property DPFBarItem: UIBarButtonItem read FDPFBarItem;
{$ENDIF}
    procedure AddToToolBar;
    constructor Create( AOwner: TCollection ); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override; //SZ
  published
    property Font            : TDPFFont read FFont write SetFont;
    property Title           : string read FTitle write SetTitle;
    property Image           : string read FImage write FImage;
    property Bitmap          : TBitmap read FBitmap write SetBitmap;
    property CustomView      : TDPFiOSBaseControl read FCustomView write FCustomView;
    property ButtonItemKind  : TDPFBarButtonKind read FButtonItemKind write SetButtonItemKind default bkSystem;
    property ButtonItemStyle : TDPFBarButtonItemStyle read FButtonItemStyle write SetButtonItemStyle default bbisPlain;
    property ButtonSystemItem: TDPFBarButtonSystemItem read FButtonSystemItem write SetButtonSystemItem default bbsiDone;
    property OnClick         : TNotifyEvent read FOnClick write SetOnClick;       // SZ
    property ModalResult     : TModalResult read FModalResult write SetModalResult; // SZ

    property Enabled  : Boolean read FEnabled write SetEnabled default True;
    property TintColor: TAlphaColor read FTintColor write SetTintColor default TAlphaColors.Null;
    property Width    : Single read FWidth write FWidth;
    property Height   : Single read FHeight write FHeight;

  end;

  // ----------------------------------------------------------------------------
  TDPFToolbarCollection = class( TCollection )
  private
    FOwner: TComponent;
  protected
    function GetOwner: TPersistent; override;
    function GetItem( Index: Integer ): TDPFToolbarItem;
    procedure SetItem( Index: Integer; Value: TDPFToolbarItem );

  public
    constructor Create( AOwner: TComponent );
    destructor Destroy; override;

    function Add: TDPFToolbarItem;
    function Insert( Index: Integer ): TDPFToolbarItem;

    property Items[index: Integer]: TDPFToolbarItem read GetItem write SetItem; default;
  end;

  // ------------------------------------------------------------------------------
  [ComponentPlatformsAttribute( PidWin32 or pidiOSDevice32 or pidiOSDevice64 or PidiOSSimulator )]
  TDPFToolbar = class( TDPFiOSBaseControl )
  private
{$IFDEF IOS}
    FUIToolbar: UIToolbar;
{$ENDIF}
    FBackgroundColor: TAlphaColor;
    FBarItems       : TDPFToolbarCollection;
    FBackgroundImage: string;
    FTranslucent    : Boolean;
    procedure SetBackgroundColor( const Value: TAlphaColor );
    procedure SetBarItems( const Value: TDPFToolbarCollection );

  protected
{$IFDEF IOS}
    procedure ReCreateToolBar;
    function CreateBarButtonItem( ToolbarItem: TDPFToolbarItem ): UIBarButtonItem;
{$ENDIF}
    procedure Resize; override;
    procedure Move; override;
    {$IFDEF MSWINDOWS}
    procedure MouseClick(Button: TMouseButton; Shift: TShiftState; X: Single;
      Y: Single); override;
    {$ENDIF}
  public
{$IFDEF IOS}
    procedure Loaded; override;
{$ELSE}
    procedure Paint; override;
{$ENDIF}
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;
  published
    property Translucent    : Boolean read FTranslucent write FTranslucent default false;
    property BackgroundImage: string read FBackgroundImage write FBackgroundImage;
    property BackgroundColor: TAlphaColor read FBackgroundColor write SetBackgroundColor default TAlphaColors.Null;
    property BarItems       : TDPFToolbarCollection read FBarItems write SetBarItems;

    property UserInteraction;
    property ContentMode;
    property Alpha;
    property Align;
    property Position;
    property Width;
    property Height;
    property Visible;
  end;

{$IFDEF IOS}

  // ------------------------------------------------------------------------------
  IDPFButtonDelegate = interface( IObjectiveC )
    ['{16256CB2-C356-4291-A246-F6A21C7D1C56}']
    procedure clickedButton( Sender: UIBarButtonItem ); cdecl;
  end;

  // ------------------------------------------------------------------------------
  TDPFToolBarButtonDelegate = class( TOCLocal, IDPFButtonDelegate )
  private
    FDPFToolbarItem: TDPFToolbarItem;
  public
    constructor Create( ADPFToolbarItem: TDPFToolbarItem );
    procedure clickedButton( Sender: UIBarButtonItem ); cdecl;
  end;
{$ENDIF}

  // ------------------------------------------------------------------------------

implementation

// ------------------------------------------------------------------------------
{ TDPFToolbar }
// ------------------------------------------------------------------------------
constructor TDPFToolbar.Create( AOwner: TComponent );
begin
  inherited Create( AOwner );
  ControlCaption   := 'Toolbar';
  FBarItems        := TDPFToolbarCollection.Create( Self );
  FTranslucent     := false;
  FBackgroundColor := TAlphaColors.Null;
  Align            := TAlignLayout.Top;
{$IFDEF IOS}
  FUIToolbar := TUIToolbar.Wrap( TUIToolbar.Alloc.initWithFrame( CGRectMake( Position.X, Position.Y, Width, Height ) ) );
  FUIControl := FUIToolbar;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
destructor TDPFToolbar.Destroy;
begin
  {$IFDEF IOS}
  FUIToolbar.Release; // SZ: cocoa memory rules: was created using Alloc -> must be freed using .Release
  FUIToolbar := nil; //SZ
  FUIControl := nil; //SZ
  {$ENDIF}
  FBarItems.Free; // SZ
  inherited;
end;

{$IFDEF IOS}

// ------------------------------------------------------------------------------
function TDPFToolbar.CreateBarButtonItem( ToolbarItem: TDPFToolbarItem ): UIBarButtonItem;
var
  Image  : UIImage;
  CButton: UIButton;
  ND     : NSDictionary;
begin
  // Titled Button
  if ToolbarItem.ButtonItemKind = bkTitle then
  begin
    Result := TUIBarButtonItem.Wrap( TUIBarButtonItem.Alloc.initWithTitle( NSStr( ToolbarItem.Title ), LongInt( ToolbarItem.ButtonItemStyle ), ToolbarItem.FButtonDelegate.GetObjectID, Sel_getUid( 'clickedButton:' ) ) );
  end
  // System Button
  else if ToolbarItem.ButtonItemKind = bkSystem then
  begin
    Result := TUIBarButtonItem.Wrap( TUIBarButtonItem.Alloc.initWithBarButtonSystemItem( LongInt( ToolbarItem.ButtonSystemItem ), ToolbarItem.FButtonDelegate.GetObjectID, Sel_getUid( 'clickedButton:' ) ) )
  end
  // Imaged Button
  else if ToolbarItem.ButtonItemKind = bkImage then
  begin
    Image := nil;
    if ToolbarItem.Image <> '' then
      Image := TUIImage.Wrap( TUIImage.OCClass.ImageNamed( NSStr( ToolbarItem.Image ) ) )
    else if not ToolbarItem.Bitmap.IsEmpty then
    begin
      Image := BitmapToUIImage( ToolbarItem.Bitmap );
    end;
    CButton := TUIButton.Wrap( TUIButton.OCClass.buttonWithType( UIButtonTypeCustom ) );

    if Image <> nil then
    begin
      CButton.setBounds( CGRectMake( 0, 0, Image.size.Width, Image.size.Height ) );
      CButton.setImage( Image, UIControlStateNormal );
      CButton.addTarget( ToolbarItem.FButtonDelegate.GetObjectID, Sel_getUid( 'clickedButton:' ), UIControlEventTouchUpInside );
      Result := TUIBarButtonItem.Wrap( TUIBarButtonItem.Alloc.initWithCustomView( CButton ) );
      Image.release;
      Image := nil;
    end;
    // CButton.release;
    CButton := nil;
  end
  // CustomView
  else if ToolbarItem.ButtonItemKind = bkCustomView then
  begin
    if ToolbarItem.CustomView <> nil then
    begin
      if ToolbarItem.CustomView.UIControl = nil then
        ToolbarItem.CustomView.Loaded;

      ToolbarItem.Width  := ToolbarItem.CustomView.Width;
      ToolbarItem.Height := ToolbarItem.CustomView.Height;

      UIView( ToolbarItem.CustomView.UIControl ).SetFrame( CGRectMake( 0, 0, ToolbarItem.CustomView.Width, ToolbarItem.CustomView.Height ) );
      Result := TUIBarButtonItem.Wrap( TUIBarButtonItem.Alloc.initWithCustomView( UIView( ToolbarItem.CustomView.UIControl ) ) );
    end;
  end
  else
    Result                := TUIBarButtonItem.Wrap( TUIBarButtonItem.Alloc.init );
  ToolbarItem.FDPFBarItem := Result;

  Result.setWidth( ToolbarItem.Width );
  Result.SetEnabled( ToolbarItem.Enabled );

  if ToolbarItem.TintColor = TAlphaColors.Null then
    Result.setTintColor( nil )
  else
    Result.setTintColor( TColorToUIColor( ToolbarItem.TintColor ) );

  Result.setStyle( LongInt( ToolbarItem.ButtonItemStyle ) );

  Result.SetTitle( NSStr( ToolbarItem.Title ) );
  ND := TNSDictionary.Wrap( TNSDictionary.OCClass.dictionaryWithObject( { TUIFont.OCClass.boldSystemFontOfSize( 11 ) } ( ToolbarItem.Font._UIFont as ILocalObject ).GetObjectID, ( UITextAttributeFont as ILocalObject ).GetObjectID ) );
  Result.setTitleTextAttributes( ND, UIControlStateNormal );
  // ND.release;
end;

// ------------------------------------------------------------------------------
procedure TDPFToolbar.ReCreateToolBar;
var
  I       : Integer;
  ItemsArr: NSMutableArray;
  BB      : UIBarButtonItem;
  Image   : UIImage;
begin
  ItemsArr := TNSMutableArray.Wrap( TNSMutableArray.OCClass.arrayWithArray( FUIToolbar.items ) );
  ItemsArr.retain;
  while ItemsArr.count > 0 do
  begin
    TUIBarButtonItem.Wrap( ItemsArr.objectAtIndex( 0 ) ).release;
    ItemsArr.removeObjectAtIndex( 0 );
  end;
  ItemsArr.release;

  FUIToolbar.setHidden( not Visible );

  SetBackgroundColor( FBackgroundColor );

  if FBarItems.Count > 0 then
    ItemsArr := TNSMutableArray.Create;

  for I := 0 to FBarItems.Count - 1 do
  begin
    BB := CreateBarButtonItem( FBarItems.Items[I] );
    ItemsArr.addObject( ( BB as ILocalObject ).GetObjectID );
  end;
  if FBarItems.Count > 0 then
  begin
    FUIToolbar.SetItems( ItemsArr, True );
    ItemsArr.release;
  end
  else
    FUIToolbar.SetItems( nil, True );

  if FBackgroundImage <> '' then
  begin
    Image := TUIImage.Wrap( TUIImage.OCClass.ImageNamed( NSStr( FBackgroundImage ) ) );
    FUIToolbar.setBackgroundImage( Image, UIToolbarPositionBottom, UIBarMetricsDefault );
    Image.release;
    Image := nil;
  end;

  FUIToolbar.setTranslucent( FTranslucent );
end;

// ------------------------------------------------------------------------------
procedure TDPFToolbar.Loaded;
begin
  ReCreateToolBar;

  Resize;
  AddSubView( Self, ParentControl );
  // ----------------------------
  // Important
  inherited;
end;
{$ENDIF}

// ------------------------------------------------------------------------------
procedure TDPFToolbar.Resize;
begin
  inherited;

{$IFDEF IOS}
  {if FUIToolbar <> nil then
    FUIToolbar.setFrame( CGRectMake( Position.X, Position.Y, Width, Height ) );}
{$ENDIF}
end;

// ------------------------------------------------------------------------------
{$IFDEF MSWINDOWS}
procedure TDPFToolbar.MouseClick(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
var
  I: Integer;
begin
  for I := 0 to FBarItems.Count - 1 do
  begin
    if FBarItems[I].FButtonRect.Contains(PointF(X, Y)) then
      FBarItems[I].DoClick;
  end;
end;
{$ENDIF}

procedure TDPFToolbar.Move;
begin
  Resize;
end;

// ------------------------------------------------------------------------------
{$IFNDEF IOS}

procedure TDPFToolbar.Paint;
var
  i, NoS         : integer;
  x, SumWidth, fs: single;
  Bmp            : TBitmap;
  R              : TRectF;
  CaptionRect    : TRectF;
  LineBreak      : TDPFLineBreak;

  function GetWidth( SegmentIndex: integer; const IncludeMargin: boolean = true ): single;
  begin
    with BarItems[i] do
    begin
      Result := 0;
      case ButtonItemKind of
        bkCustomView, bkImage, bkTitle:
          begin
            Result := Width;
            if IncludeMargin then
              Result := Result + 10;
          end;
        bkSystem:
          begin
            case ButtonItemStyle of
              bbisPlain:
                begin
                  case ButtonSystemItem of
                    bbsiDone:
                      Result := iOS_GUI_Bitmaps.ToolBar.LargeIcons.Done.Width;
                    bbsiCancel:
                      Result := iOS_GUI_Bitmaps.ToolBar.LargeIcons.Cancel.Width;
                    bbsiEdit:
                      Result := iOS_GUI_Bitmaps.ToolBar.LargeIcons.Edit.Width;
                    bbsiSave:
                      Result := iOS_GUI_Bitmaps.ToolBar.LargeIcons.Save.Width;
                    bbsiAdd:
                      Result := iOS_GUI_Bitmaps.ToolBar.LargeIcons.Add.Width;
                    bbsiFixedSpace:
                      Result := Width;
                    bbsiCompose:
                      Result := iOS_GUI_Bitmaps.ToolBar.LargeIcons.Compose.Width;
                    bbsiReply:
                      Result := iOS_GUI_Bitmaps.ToolBar.LargeIcons.Reply.Width;
                    bbsiAction:
                      Result := iOS_GUI_Bitmaps.ToolBar.LargeIcons.Action.Width;
                    bbsiOrganize:
                      Result := iOS_GUI_Bitmaps.ToolBar.LargeIcons.Organize.Width;
                    bbsiBookmarks:
                      Result := iOS_GUI_Bitmaps.ToolBar.LargeIcons.Bookmarks.Width;
                    bbsiSearch:
                      Result := iOS_GUI_Bitmaps.ToolBar.LargeIcons.Search.Width;
                    bbsiRefresh:
                      Result := iOS_GUI_Bitmaps.ToolBar.LargeIcons.Refresh.Width;
                    bbsiStop:
                      Result := iOS_GUI_Bitmaps.ToolBar.LargeIcons.Stop.Width;
                    bbsiCamera:
                      Result := iOS_GUI_Bitmaps.ToolBar.LargeIcons.Camera.Width;
                    bbsiTrash:
                      Result := iOS_GUI_Bitmaps.ToolBar.LargeIcons.Trash.Width;
                    bbsiPlay:
                      Result := iOS_GUI_Bitmaps.ToolBar.LargeIcons.Play.Width;
                    bbsiPause:
                      Result := iOS_GUI_Bitmaps.ToolBar.LargeIcons.Pause.Width;
                    bbsiRewind:
                      Result := iOS_GUI_Bitmaps.ToolBar.LargeIcons.Rewind.Width;
                    bbsiFastForward:
                      Result := iOS_GUI_Bitmaps.ToolBar.LargeIcons.FastForward.Width;
                  end;
                  if IncludeMargin and ( ButtonSystemItem <> bbsiFixedSpace ) and ( ButtonSystemItem <> bbsiFlexibleSpace ) then
                    Result := Result + 12;
                end;
              bbisBordered, bbisDone:
                begin
                  case ButtonSystemItem of
                    bbsiDone:
                      Result := iOS_GUI_Bitmaps.ToolBar.SmallIcons.Done.Width + 10;
                    bbsiCancel:
                      Result := iOS_GUI_Bitmaps.ToolBar.SmallIcons.Cancel.Width + 10;
                    bbsiEdit:
                      Result := iOS_GUI_Bitmaps.ToolBar.SmallIcons.Edit.Width + 10;
                    bbsiSave:
                      Result := iOS_GUI_Bitmaps.ToolBar.SmallIcons.Save.Width + 10;
                    bbsiAdd:
                      Result := iOS_GUI_Bitmaps.ToolBar.SmallIcons.Add.Width + 10;
                    bbsiFixedSpace:
                      Result := Width;
                    bbsiCompose:
                      Result := iOS_GUI_Bitmaps.ToolBar.SmallIcons.Compose.Width + 10;
                    bbsiReply:
                      Result := iOS_GUI_Bitmaps.ToolBar.SmallIcons.Reply.Width + 10;
                    bbsiAction:
                      Result := iOS_GUI_Bitmaps.ToolBar.SmallIcons.Action.Width + 10;
                    bbsiOrganize:
                      Result := iOS_GUI_Bitmaps.ToolBar.SmallIcons.Organize.Width + 10;
                    bbsiBookmarks:
                      Result := iOS_GUI_Bitmaps.ToolBar.SmallIcons.Bookmarks.Width + 10;
                    bbsiSearch:
                      Result := iOS_GUI_Bitmaps.ToolBar.SmallIcons.Search.Width + 10;
                    bbsiRefresh:
                      Result := iOS_GUI_Bitmaps.ToolBar.SmallIcons.Refresh.Width + 10;
                    bbsiStop:
                      Result := iOS_GUI_Bitmaps.ToolBar.SmallIcons.Stop.Width + 10;
                    bbsiCamera:
                      Result := iOS_GUI_Bitmaps.ToolBar.SmallIcons.Camera.Width + 10;
                    bbsiTrash:
                      Result := iOS_GUI_Bitmaps.ToolBar.SmallIcons.Trash.Width + 10;
                    bbsiPlay:
                      Result := iOS_GUI_Bitmaps.ToolBar.SmallIcons.Play.Width + 10;
                    bbsiPause:
                      Result := iOS_GUI_Bitmaps.ToolBar.SmallIcons.Pause.Width + 10;
                    bbsiRewind:
                      Result := iOS_GUI_Bitmaps.ToolBar.SmallIcons.Rewind.Width + 10;
                    bbsiFastForward:
                      Result := iOS_GUI_Bitmaps.ToolBar.SmallIcons.FastForward.Width + 10;
                  end;
                  if IncludeMargin and ( ButtonSystemItem <> bbsiFixedSpace ) and ( ButtonSystemItem <> bbsiFlexibleSpace ) then
                    Result := Result + 10;
                end;
            end;
          end;
      end;
    end;
  end;

begin
  // Added by Fenistil
  Canvas.BeginScene;
  // Background
  BitmapAsBackground( Self, iOS_GUI_Bitmaps.ToolBar.BG );

  // Calculate Spaces
  NoS      := 0;
  SumWidth := 0;
  for i    := 0 to BarItems.Count - 1 do
    if BarItems[i].ButtonSystemItem = bbsiFlexibleSpace then
      inc( NoS )
    else
      SumWidth := SumWidth + GetWidth( i );
  if NoS > 0 then
    fs := Max( ( Self.Width - SumWidth ) / NoS, 0 )
  else
    fs := 0;
  // Drawing Buttons
  x     := 0;
  for i := 0 to BarItems.Count - 1 do
  begin
    // Space
    if ( BarItems[i].ButtonItemKind = bkSystem ) and ( BarItems[i].ButtonSystemItem in [bbsiFlexibleSpace, bbsiFixedSpace] ) then
    begin
      if BarItems[i].ButtonSystemItem = bbsiFlexibleSpace then
        x := x + fs;
      if BarItems[i].ButtonSystemItem = bbsiFixedSpace then
        x := x + BarItems[i].Width;
      Continue;
    end;

    // Background
    if BarItems[i].ButtonItemStyle in [bbisBordered, bbisDone] then
    begin
      x := x + 5; // Left margin
      // Left
      R := RectF( x, 8, x + 5, 38 );
      if BarItems[i].ButtonItemStyle = bbisBordered then
        BitmapToRect( Self, iOS_GUI_Bitmaps.ToolBar.ButtonLeft, R )
      else
        BitmapToRect( Self, iOS_GUI_Bitmaps.ToolBar.DoneLeft, R );
      // Center
      R.Left  := R.Right;
      R.Right := R.Left + GetWidth( i, false ) - 10;
      if BarItems[i].ButtonItemStyle = bbisBordered then
        BitmapToRect( Self, iOS_GUI_Bitmaps.ToolBar.ButtonBG, R )
      else
        BitmapToRect( Self, iOS_GUI_Bitmaps.ToolBar.DoneBG, R );
      // Right
      R.Left  := R.Right;
      R.Right := R.Left + 5;
      if BarItems[i].ButtonItemStyle = bbisBordered then
        BitmapToRect( Self, iOS_GUI_Bitmaps.ToolBar.ButtonRight, R )
      else
        BitmapToRect( Self, iOS_GUI_Bitmaps.ToolBar.DoneRight, R );
    end;

    // Custom / Image
    if BarItems[i].ButtonItemKind in [bkCustomView, bkImage] then
    begin
      if BarItems[i].ButtonItemStyle = bbisPlain then
      begin
        x                   := x + 5; // Left margin
        R                   := RectF( x, 8, x + BarItems[i].Width, 38 );
        {$IFDEF MSWINDOWS}
        BarItems[I].FButtonRect := R; // SZ added (not very clean to do this in OnPaint, but it is only for testing on Windows)
        {$ENDIF}
        Canvas.Stroke.Color := TAlphaColors.White;
        Canvas.Stroke.Kind  := TBrushKind.Solid;
        Canvas.Fill.Color   := TAlphaColors.Gray;
        Canvas.Fill.Kind    := TBrushKind.Solid;
        Canvas.FillRect( R, 0, 0, AllCorners, 1 );
      end;
      x := x + BarItems[i].Width;
    end;

    // System
    if BarItems[i].ButtonItemKind = bkSystem then
    begin
      if BarItems[i].ButtonItemStyle = bbisPlain then
      begin
        Bmp := nil;
        case BarItems[i].ButtonSystemItem of
          bbsiAdd:
            Bmp := iOS_GUI_Bitmaps.ToolBar.LargeIcons.Add;
          bbsiCancel:
            Bmp := iOS_GUI_Bitmaps.ToolBar.LargeIcons.Cancel;
          bbsiDone:
            Bmp := iOS_GUI_Bitmaps.ToolBar.LargeIcons.Done;
          bbsiEdit:
            Bmp := iOS_GUI_Bitmaps.ToolBar.LargeIcons.Edit;
          bbsiSave:
            Bmp := iOS_GUI_Bitmaps.ToolBar.LargeIcons.Save;
          bbsiCompose:
            Bmp := iOS_GUI_Bitmaps.ToolBar.LargeIcons.Compose;
          bbsiReply:
            Bmp := iOS_GUI_Bitmaps.ToolBar.LargeIcons.Reply;
          bbsiAction:
            Bmp := iOS_GUI_Bitmaps.ToolBar.LargeIcons.Action;
          bbsiOrganize:
            Bmp := iOS_GUI_Bitmaps.ToolBar.LargeIcons.Organize;
          bbsiBookmarks:
            Bmp := iOS_GUI_Bitmaps.ToolBar.LargeIcons.Bookmarks;
          bbsiSearch:
            Bmp := iOS_GUI_Bitmaps.ToolBar.LargeIcons.Search;
          bbsiRefresh:
            Bmp := iOS_GUI_Bitmaps.ToolBar.LargeIcons.Refresh;
          bbsiStop:
            Bmp := iOS_GUI_Bitmaps.ToolBar.LargeIcons.Stop;
          bbsiCamera:
            Bmp := iOS_GUI_Bitmaps.ToolBar.LargeIcons.Camera;
          bbsiTrash:
            Bmp := iOS_GUI_Bitmaps.ToolBar.LargeIcons.Trash;
          bbsiPlay:
            Bmp := iOS_GUI_Bitmaps.ToolBar.LargeIcons.Play;
          bbsiPause:
            Bmp := iOS_GUI_Bitmaps.ToolBar.LargeIcons.Pause;
          bbsiRewind:
            Bmp := iOS_GUI_Bitmaps.ToolBar.LargeIcons.Rewind;
          bbsiFastForward:
            Bmp := iOS_GUI_Bitmaps.ToolBar.LargeIcons.FastForward;
        end;
        if Bmp <> nil then
        begin
          x := x + 6; // Left margin
          {$IFDEF MSWINDOWS}
          BarItems[I].FButtonRect := RectF(X,8,X+Bmp.Width,8+Bmp.Height); // SZ added (not very clean to do this in OnPaint, but it is only for testing on Windows)
          {$ENDIF}
          BitmapToPosition( Self, Bmp, x, 8 ); // Paint Bitmap
          x := x + Bmp.Width;
          x := x + 6; // Right margin
        end;
      end
      else
      begin // Bordered / Done
        Bmp := nil;
        case BarItems[i].ButtonSystemItem of
          bbsiAdd:
            Bmp := iOS_GUI_Bitmaps.ToolBar.SmallIcons.Add;
          bbsiCancel:
            Bmp := iOS_GUI_Bitmaps.ToolBar.SmallIcons.Cancel;
          bbsiDone:
            Bmp := iOS_GUI_Bitmaps.ToolBar.SmallIcons.Done;
          bbsiEdit:
            Bmp := iOS_GUI_Bitmaps.ToolBar.SmallIcons.Edit;
          bbsiSave:
            Bmp := iOS_GUI_Bitmaps.ToolBar.SmallIcons.Save;
          bbsiCompose:
            Bmp := iOS_GUI_Bitmaps.ToolBar.SmallIcons.Compose;
          bbsiReply:
            Bmp := iOS_GUI_Bitmaps.ToolBar.SmallIcons.Reply;
          bbsiAction:
            Bmp := iOS_GUI_Bitmaps.ToolBar.SmallIcons.Action;
          bbsiOrganize:
            Bmp := iOS_GUI_Bitmaps.ToolBar.SmallIcons.Organize;
          bbsiBookmarks:
            Bmp := iOS_GUI_Bitmaps.ToolBar.SmallIcons.Bookmarks;
          bbsiSearch:
            Bmp := iOS_GUI_Bitmaps.ToolBar.SmallIcons.Search;
          bbsiRefresh:
            Bmp := iOS_GUI_Bitmaps.ToolBar.SmallIcons.Refresh;
          bbsiStop:
            Bmp := iOS_GUI_Bitmaps.ToolBar.SmallIcons.Stop;
          bbsiCamera:
            Bmp := iOS_GUI_Bitmaps.ToolBar.SmallIcons.Camera;
          bbsiTrash:
            Bmp := iOS_GUI_Bitmaps.ToolBar.SmallIcons.Trash;
          bbsiPlay:
            Bmp := iOS_GUI_Bitmaps.ToolBar.SmallIcons.Play;
          bbsiPause:
            Bmp := iOS_GUI_Bitmaps.ToolBar.SmallIcons.Pause;
          bbsiRewind:
            Bmp := iOS_GUI_Bitmaps.ToolBar.SmallIcons.Rewind;
          bbsiFastForward:
            Bmp := iOS_GUI_Bitmaps.ToolBar.SmallIcons.FastForward;
        end;
        if Bmp <> nil then
        begin
          x := x + 5; // Left padding
          {$IFDEF MSWINDOWS}
          BarItems[I].FButtonRect := RectF(X,8,X+Bmp.Width,8+Bmp.Height); // SZ added (not very clean to do this in OnPaint, but it is only for testing on Windows)
          {$ENDIF}
          BitmapToPosition( Self, Bmp, x, 8 ); // Paint Bitmap
          x := x + Bmp.Width;
          x := x + 5; // Right padding
        end;
        x := x + 5; // Right margin
      end;
    end;

    // Title
    if BarItems[i].ButtonItemKind = bkTitle then
    begin
      if BarItems[i].ButtonItemStyle = bbisPlain then
      begin
        Canvas.Font.Size   := 15;
        Canvas.Font.Style  := [TFontStyle.fsBold];
        Canvas.Font.Family := 'Helvetica';
        LineBreak          := lbTailTruncation;
      end
      else
      begin
        Canvas.Font.Size   := 12;
        Canvas.Font.Style  := [TFontStyle.fsBold];
        Canvas.Fill.Color  := TAlphaColors.White;
        Canvas.Font.Family := 'Helvetica';
        LineBreak          := lbMiddleTruncation;
      end;
      // Shadow
      Canvas.Fill.Color := TAlphaColors.Gray;
      CaptionRect       := RectF( x, 7, x + GetWidth( i, false ), 37 );
      PaintCaption( Self, BarItems[i].Title, CaptionRect, LineBreak, 1, TTextAlign.Center );
      // Text
      Canvas.Fill.Color := TAlphaColors.White;
      CaptionRect       := RectF( x, 8, x + GetWidth( i, false ), 38 );
      PaintCaption( Self, BarItems[i].Title, CaptionRect, LineBreak, 1, TTextAlign.Center );
      x := x + CaptionRect.Width + 5;

      {$IFDEF MSWINDOWS}
      BarItems[I].FButtonRect := CaptionRect; // SZ added (not very clean to do this in OnPaint, but it is only for testing on Windows)
      {$ENDIF}
    end;
  end;
  Canvas.EndScene;
end;
{$ENDIF}

// ------------------------------------------------------------------------------
procedure TDPFToolbar.SetBackgroundColor( const Value: TAlphaColor );
begin
  FBackgroundColor := Value;
{$IFDEF IOS}
  if FUIToolbar <> nil then
  begin
    if FBackgroundColor <> TAlphaColors.Null then
    begin
      FUIToolbar.setTintColor( TColorToUIColor( FBackgroundColor ) );
      if TOSVersion.Major > 6 then
        FUIToolbar.setBarTintColor( TColorToUIColor( FBackgroundColor ) );
    end;
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFToolbar.SetBarItems( const Value: TDPFToolbarCollection );
begin
  FBarItems.Assign( Value );
end;

// ------------------------------------------------------------------------------
{ TDPFToolbarCollection }
function TDPFToolbarCollection.Add: TDPFToolbarItem;
begin
  Result := inherited Add as TDPFToolbarItem;
end;

// ------------------------------------------------------------------------------
constructor TDPFToolbarCollection.Create( AOwner: TComponent );
begin
  inherited Create( TDPFToolbarItem );
  FOwner := AOwner;
end;

// ------------------------------------------------------------------------------
destructor TDPFToolbarCollection.Destroy;
begin

  inherited;
end;

// ------------------------------------------------------------------------------
function TDPFToolbarCollection.GetItem( Index: Integer ): TDPFToolbarItem;
begin
  Result := inherited Items[index] as TDPFToolbarItem;
end;

// ------------------------------------------------------------------------------
function TDPFToolbarCollection.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

// ------------------------------------------------------------------------------
function TDPFToolbarCollection.Insert( Index: Integer ): TDPFToolbarItem;
begin
  Result := inherited insert( index ) as TDPFToolbarItem;
end;

// ------------------------------------------------------------------------------
procedure TDPFToolbarCollection.SetItem( Index: Integer; Value: TDPFToolbarItem );
begin
  inherited SetItem( index, Value );
end;

// ------------------------------------------------------------------------------
{ TDPFToolbarItem }

procedure TDPFToolbarItem.Assign(Source: TPersistent);    // SZ
  procedure AssignFromToolbarItem(Src: TDPFToolbarItem);
  begin
    Title := Src.Title;
    Enabled := Src.Enabled;
    Image := Src.Image;
    Width := Src.Width;
    ButtonItemStyle := Src.ButtonItemStyle;
    ButtonSystemItem := Src.ButtonSystemItem;
    CustomView := Src.CustomView;
    Bitmap := Src.Bitmap;
    ButtonItemKind := Src.ButtonItemKind;
    Height := Src.Height;
    OnClick := Src.OnClick;
    Font := Src.Font;
    TintColor := Src.TintColor;
    ModalResult := Src.ModalResult;
  end;

begin
  if Source is TDPFToolbarItem then
    AssignFromToolbarItem(TDPFToolbarItem(Source))
  else
    inherited;
end;

constructor TDPFToolbarItem.Create( AOwner: TCollection );
begin
  inherited Create( AOwner );
  FFont             := TDPFFont.Create;
  FFont.FontName    := ios_Helvetica_Bold;
  FFont.FontSize    := 14;
  FTitle            := '';
  FOwner            := AOwner;
  FBitmap           := TBitmap.Create( 0, 0 );
  FEnabled          := True;
  FWidth            := 100;
  FWidth            := 50;
  FButtonItemStyle  := bbisPlain;
  FButtonSystemItem := bbsiDone;
  FButtonItemKind   := bkSystem;
{$IFDEF IOS}
  FButtonDelegate := TDPFToolBarButtonDelegate.Create( Self );
{$ENDIF}
end;

// ------------------------------------------------------------------------------
destructor TDPFToolbarItem.Destroy;
{$IFDEF IOS }
var
  Toolbar: TDPFToolbar;
  NS     : NSMutableArray;
{$ENDIF}
begin
  FFont.DisposeOf;  // SZ moved here
  FBitmap.Free;  // SZ added
{$IFDEF IOS }
  if Assigned( FDPFBarItem ) then
  begin
    FDPFBarItem.setAction( nil );
    FDPFBarItem.setTarget( nil );
    FDPFBarItem.release;
  end;
  FButtonDelegate.DisposeOf;
  ToolBar := TDPFToolbar( Self.FOwner.Owner );
  NS      := TNSMutableArray.Wrap( TNSMutableArray.OCClass.arrayWithArray( Toolbar.FUIToolbar.items ) );
  NS.retain;
  if index < NS.count then
    NS.removeObjectAtIndex( index );
  ToolBar.FUIToolbar.setItems( NS );
  NS.release;

{$ENDIF}
  inherited;
end;

procedure TDPFToolbarItem.DoClick; // SZ added
var
  O: TComponent;
begin
  inherited;

  if (Self <> nil) and (ModalResult <> mrNone) then
  begin
    O := ((FOwner as TDPFToolbarCollection).Owner as TDPFToolbar).Scene.GetObject;
    while O <> nil do
    begin
      if (O is TCommonCustomForm) then
      begin
        TCommonCustomForm(O).ModalResult := FModalResult;
        Break;
      end;
      O := O.Owner;
    end;
  end;

  if Assigned(FOnClick) then
    FOnClick(Self);
end;

// ------------------------------------------------------------------------------
function TDPFToolbarItem.GetDisplayName: string;
begin
  Result := Format( 'Item %d %s', [index, Title]);
end;

// ------------------------------------------------------------------------------
procedure TDPFToolbarItem.AddToToolBar;
{$IFDEF IOS}
var
  Toolbar: TDPFToolbar;
  BB     : UIBarButtonItem;
  NS     : NSMutableArray;
{$ENDIF}
begin
{$IFDEF IOS}
  ToolBar := TDPFToolbar( Self.FOwner.Owner );
  NS      := TNSMutableArray.Wrap( TNSMutableArray.OCClass.arrayWithArray( Toolbar.FUIToolbar.items ) );
  NS.retain;
  BB := ToolBar.CreateBarButtonItem( Self );
  NS.addObject( ( BB as ILocalObject ).GetObjectID );
  ToolBar.FUIToolbar.setItems( NS );
  NS.release;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFToolbarItem.SetBitmap( const Value: TBitmap );
begin
  FBitmap.Assign( Value );
end;

// ------------------------------------------------------------------------------
procedure TDPFToolbarItem.SetButtonItemKind( const Value: TDPFBarButtonKind );
begin
  FButtonItemKind := Value;
{$IFDEF IOS}
{$ELSE}
  TDPFToolbar( FOwner.Owner ).Invalidate;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFToolbarItem.SetButtonItemStyle( const Value: TDPFBarButtonItemStyle );
begin
  FButtonItemStyle := Value;
{$IFDEF IOS}
  if FDPFBarItem <> nil then
    FDPFBarItem.setStyle( LongInt( Value ) );
{$ELSE}
  TDPFToolbar( FOwner.Owner ).Invalidate;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFToolbarItem.SetButtonSystemItem( const Value: TDPFBarButtonSystemItem );
{$IFDEF IOS}
var
  Toolbar: TDPFToolbar;
  BB     : UIBarButtonItem;
  NS     : NSMutableArray;
{$ENDIF}
begin
  FButtonSystemItem := Value;
{$IFDEF IOS}
  if FDPFBarItem <> nil then
  begin
    ToolBar := TDPFToolbar( Self.FOwner.Owner );
    NS      := TNSMutableArray.Wrap( TNSMutableArray.OCClass.arrayWithArray( Toolbar.FUIToolbar.items ) );
    NS.retain;
    BB := TUIBarButtonItem.Wrap( NS.objectAtIndex( Self.Index ) );
    BB.release;
    BB := TUIBarButtonItem.Wrap( TUIBarButtonItem.Alloc.initWithBarButtonSystemItem( UIBarButtonSystemItem( Value ), FButtonDelegate.GetObjectID, Sel_getUid( 'clickedButton:' ) ) );

    BB.setWidth( Width );
    BB.SetEnabled( Enabled );
    BB.setStyle( LongInt( ButtonItemStyle ) );
    BB.SetTitle( NSStr( Title ) );

    NS.replaceObjectAtIndex( Self.Index, ( BB as ILocalObject ).GetObjectID );

    FDPFBarItem := BB;
    Toolbar.FUIToolbar.setItems( NS );
    NS.release;

  end;
{$ELSE}
  TDPFToolbar( FOwner.Owner ).Invalidate;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFToolbarItem.SetEnabled( const Value: Boolean );
begin
  FEnabled := Value;
{$IFDEF IOS}
  if FDPFBarItem <> nil then
    FDPFBarItem.SetEnabled( FEnabled );
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFToolbarItem.SetFont( const Value: TDPFFont );
begin
  FFont.Assign( Value );
end;

procedure TDPFToolbarItem.SetModalResult(const Value: TModalResult);
begin
  FModalResult := Value;
end;

procedure TDPFToolbarItem.SetOnClick(const Value: TNotifyEvent);
begin
  FOnClick := Value;
end;

procedure TDPFToolbarItem.SetTintColor( const Value: TAlphaColor );
begin
  FTintColor := Value;
{$IFDEF IOS}
  if FDPFBarItem <> nil then
  begin
    if FTintColor = TAlphaColors.Null then
      FDPFBarItem.setTintColor( nil )
    else
      FDPFBarItem.setTintColor( TColorToUIColor( FTintColor ) );
  end;
{$ENDIF}
end;

procedure TDPFToolbarItem.SetTitle( const Value: string );
begin
  FTitle := Value;
{$IFDEF IOS}
  if FDPFBarItem <> nil then
  begin
    FDPFBarItem.SetTitle( NSStr( FTitle ) );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
{ TDPFToolBarButtonDelegate }
{$IFDEF IOS}

procedure TDPFToolBarButtonDelegate.clickedButton( Sender: UIBarButtonItem ); //SZ
begin
  try
    FDPFToolbarItem.DoClick;
  except
    if Assigned(Application) then
      Application.HandleException(nil);
  end;
end;

// ------------------------------------------------------------------------------
constructor TDPFToolBarButtonDelegate.Create( ADPFToolbarItem: TDPFToolbarItem );
begin
  inherited Create;
  FDPFToolbarItem := ADPFToolbarItem;
end;

{$ENDIF}

// ------------------------------------------------------------------------------
end.
