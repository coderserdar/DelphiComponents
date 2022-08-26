// ------------------------------------------------------------------------------
// DPF.iOS.UISegmentedControl Component
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
unit DPF.iOS.UISegmentedControl;

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
{$ENDIF}
  FMX.Layouts, FMX.Memo,
{$IFDEF DELPHIXE5} FMX.Graphics, {$ENDIF}FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls;

type
  TDPFSegmentedControl      = class;
  TDPFSegmentedControlStyle = ( scsPlain, scsBordered, scsBar, scsBezeled );

  TDPFSegmentedControlValueChanged = procedure( Sender: TObject; SegmentIndex: Integer ) of object;

{$IFDEF IOS}

  // ------------------------------------------------------------------------------
  DPFSegmentedControlDelegate = interface( NSObject )
    ['{30331A2F-DCD3-44BA-ABCD-89BB56430CEB}']
    procedure valueChanged; cdecl;
  end;

  // ------------------------------------------------------------------------------
  TDPFSegmentedControlDelegate = class( TOCLocal )
  private
    FDPFSegmentedControl: TDPFSegmentedControl;
  public
    constructor Create( ADPFSegmentedControl: TDPFSegmentedControl );
    function GetObjectiveCClass: PTypeInfo; override;
    procedure valueChanged; cdecl;
  end;
{$ENDIF}

  // ----------------------------------------------------------------------------
  // SegmentedControl Sections
  TDPFSegmentedControlItem = class( TCollectionItem )
  private
    FOwner   : TCollection;
    FTitle   : string;
    FEnabled : Boolean;
    FImage   : string;
    FTagStr  : string;
    FVisible : Boolean;
    FOldWidth: Single;
    procedure SetEnabled( const Value: Boolean );
    procedure SetTitle( const Value: string );
    procedure SetVisible( const Value: Boolean );
  protected
    function GetDisplayName: string; override;
  public
    constructor Create( AOwner: TCollection ); override;
    destructor Destroy; override;
    procedure Apply( const isUpdate: Boolean; const Animated: Boolean = True );
    procedure Assign(Source: TPersistent); override;
  published
    property Title  : string read FTitle write SetTitle;
    property Image  : string read FImage write FImage;
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property Visible: Boolean read FVisible write SetVisible default True;
    property TagStr : string read FTagStr write FTagStr;
  end;

  // ----------------------------------------------------------------------------
  TDPFSegmentedControlCollection = class( TCollection )
  private
    FOwner: TComponent;
  protected
    function GetOwner: TPersistent; override;
    function GetItem( Index: Integer ): TDPFSegmentedControlItem;
    procedure SetItem( Index: Integer; Value: TDPFSegmentedControlItem );
    procedure Update( Item: TCollectionItem ); override; // Added by Fenistil

  public
    constructor Create( AOwner: TComponent );
    destructor Destroy; override;

    function Add: TDPFSegmentedControlItem;
    function Insert( Index: Integer ): TDPFSegmentedControlItem;

    property Items[index: Integer]: TDPFSegmentedControlItem read GetItem write SetItem; default; // default - Added by Fenistil
  end;

  // ------------------------------------------------------------------------------
  [ComponentPlatformsAttribute( PidWin32 or pidiOSDevice32 or pidiOSDevice64 or PidiOSSimulator )]
  TDPFSegmentedControl = class( TDPFiOSBaseControl )
  private
{$IFDEF IOS}
    FUISegmentedControl      : UISegmentedControl;
    FSegmentedControlDelegate: TDPFSegmentedControlDelegate;
{$ENDIF}
    FBackgroundColor           : TAlphaColor;
    FSegments                  : TDPFSegmentedControlCollection;
    FBackgroundImageDisable    : string;
    FBackgroundImageNormal     : string;
    FBackgroundImageHighlighted: string;
    FStyle                     : TDPFSegmentedControlStyle;
    FOnChanged                 : TDPFSegmentedControlValueChanged;
    FSelectedIndex             : Integer;
    FTintColor                 : TAlphaColor;
    procedure SetBackgroundColor( const Value: TAlphaColor );
    procedure SetSegments( const Value: TDPFSegmentedControlCollection );
    procedure SetSelectedIndex( const Value: Integer );
    procedure SetStyle( const Value: TDPFSegmentedControlStyle );
    procedure SetTintColor( const Value: TAlphaColor );
    function GetSelectedSegment: TDPFSegmentedControlItem;
    function GetSelectedSegmentText: string; // Added by Fenistil

  protected
    procedure Resize; override;
    procedure Move; override;
  public
{$IFDEF IOS}
    procedure OnFontChanged( Sender: TObject );
    procedure Loaded; override;
{$ELSE}
    procedure Paint; override;
{$ENDIF}
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;
    procedure ClearAll;
  published
    property TintColor                 : TAlphaColor read FTintColor write SetTintColor default TAlphaColors.Null;
    property BackgroundColor           : TAlphaColor read FBackgroundColor write SetBackgroundColor default TAlphaColors.Null;
    property BackgroundImageNormal     : string read FBackgroundImageNormal write FBackgroundImageNormal;
    property BackgroundImageDisable    : string read FBackgroundImageDisable write FBackgroundImageDisable;
    property BackgroundImageHighlighted: string read FBackgroundImageHighlighted write FBackgroundImageHighlighted;
    property Style                     : TDPFSegmentedControlStyle read FStyle write SetStyle default scsPlain;
    property Segments                  : TDPFSegmentedControlCollection read FSegments write SetSegments;
    property SelectedIndex             : Integer read FSelectedIndex write SetSelectedIndex default -1;
    property OnChanged                 : TDPFSegmentedControlValueChanged read FOnChanged write FOnChanged;

    property SelectedSegment    : TDPFSegmentedControlItem read GetSelectedSegment;
    property SelectedSegmentText: string read GetSelectedSegmentText;

    property UserInteraction;
    property ContentMode;
    property Alpha;
    property Align;
    property Position;
    property Width;
    property Height;
    property Visible;
  end;

implementation

// ------------------------------------------------------------------------------
{ TDPFSegmentedControl }
// ------------------------------------------------------------------------------
constructor TDPFSegmentedControl.Create( AOwner: TComponent );
begin
  inherited Create( AOwner );
  ControlCaption   := 'SegmentedControl';
  FBackgroundColor := TAlphaColors.Null;
  FTintColor       := TAlphaColors.Null;
  FStyle           := scsPlain;
  FSegments        := TDPFSegmentedControlCollection.Create( Self );
  FSelectedIndex   := -1;
{$IFDEF IOS}
  Font.OnChanged            := OnFontChanged;
  FSegmentedControlDelegate := TDPFSegmentedControlDelegate.Create( Self );

  FUISegmentedControl := TUISegmentedControl.Create;
  FUIControl          := FUISegmentedControl;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
destructor TDPFSegmentedControl.Destroy;
begin
  FSegments.DisposeOf;
{$IFDEF IOS}
  FUISegmentedControl.removeTarget( FSegmentedControlDelegate.GetObjectID, // target
    Sel_getUid( 'valueChanged' ), // action
    UIControlEventValueChanged ); // event
  FSegmentedControlDelegate.DisposeOf;
{$ENDIF}
  inherited;
end;

// ------------------------------------------------------------------------------
{$IFDEF IOS}

procedure TDPFSegmentedControl.OnFontChanged( Sender: TObject );
var
  NSD: NSDictionary;
begin
  NSD := TNSDictionary.Wrap( TNSDictionary.OCClass.dictionaryWithObject( ( Font._UIFont as ILocalObject ).GetObjectID, ( UITextAttributeFont as ILocalObject ).GetObjectID ) );
  FUISegmentedControl.setTitleTextAttributes( NSD, UIControlStateNormal );
end;
{$ENDIF}

// ------------------------------------------------------------------------------
function TDPFSegmentedControl.GetSelectedSegment: TDPFSegmentedControlItem;
begin
  Result := nil;
  if SelectedIndex > -1 then
    Result := Segments[SelectedIndex];
end;

// ------------------------------------------------------------------------------
function TDPFSegmentedControl.GetSelectedSegmentText: string;
begin
  Result := '';
  if SelectedSegment <> nil then
    Result := SelectedSegment.Title;

end;

// ------------------------------------------------------------------------------
{$IFDEF IOS}

procedure TDPFSegmentedControl.Loaded;
var
  I    : Integer;
  Image: UIImage;
begin
  for I := 0 to FSegments.Count - 1 do
  begin
    if FSegments.Items[I].Image <> '' then
    begin
      Image := TUIImage.Wrap( TUIImage.OCClass.ImageNamed( NSStr( FSegments.Items[I].Image ) ) );
      FUISegmentedControl.insertSegmentWithImage( Image, I, True );
      Image := nil;
    end
    else
      FUISegmentedControl.insertSegmentWithTitle( NSStr( FSegments.Items[I].Title ), I, True );

    FUISegmentedControl.setEnabled( FSegments.Items[I].Enabled, I );
  end;

  if FBackgroundImageNormal <> '' then
  begin
    Image := TUIImage.Wrap( TUIImage.OCClass.ImageNamed( NSStr( FBackgroundImageNormal ) ) );
    FUISegmentedControl.setBackgroundImage( Image, UIControlStateNormal, UIBarMetricsDefault );
    Image := nil;
  end;

  if FBackgroundImageDisable <> '' then
  begin
    Image := TUIImage.Wrap( TUIImage.OCClass.ImageNamed( NSStr( FBackgroundImageDisable ) ) );
    FUISegmentedControl.setBackgroundImage( Image, UIControlStateDisabled, UIBarMetricsDefault );
    Image := nil;
  end;

  if FBackgroundImageHighlighted <> '' then
  begin
    Image := TUIImage.Wrap( TUIImage.OCClass.ImageNamed( NSStr( FBackgroundImageHighlighted ) ) );
    FUISegmentedControl.setBackgroundImage( Image, UIControlStateHighlighted, 0 );
    Image := nil;
  end;

  FUISegmentedControl.setHidden( not Visible );
  FUISegmentedControl.setSegmentedControlStyle( Integer( FStyle ) );
  // FUISegmentedControl.setTintColor(TUIColor.Wrap( TUIColor.OCClass.clearColor ) );

  SetBackgroundColor( FBackgroundColor );

  if FSelectedIndex <> -1 then
  begin
    FUISegmentedControl.setSelectedSegmentIndex( FSelectedIndex );
    FUISegmentedControl.setHighlighted( True );
  end;

  FUISegmentedControl.AddTarget( FSegmentedControlDelegate.GetObjectID, // target
    Sel_getUid( 'valueChanged' ), // action
    UIControlEventValueChanged ); // event

  OnFontChanged( self );
  Resize;
  AddSubView( Self, ParentControl );
  // ----------------------------
  // Important

  for I := 0 to FSegments.Count - 1 do
  begin
    FSegments.Items[I].FOldWidth := FUISegmentedControl.widthForSegmentAtIndex( i );
  end;

  inherited;
end;
{$ENDIF}

// ------------------------------------------------------------------------------
procedure TDPFSegmentedControl.ClearAll;
begin
  while Segments.Count > 0 do
  begin
    Segments.Delete( 0 );
  end;
{$IFDEF IOS}
  FUISegmentedControl.removeAllSegments;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFSegmentedControl.Resize;
begin
  inherited;
{$IFDEF IOS}
  // if FUISegmentedControl <> nil then FUISegmentedControl.SetFrame( CGRectMake( Position.X, Position.Y, Width, Height ) );
{$ELSE}
  // Added by Fenistil
  Invalidate;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFSegmentedControl.Move;
begin
  Resize;
end;

{$IFNDEF IOS}

// ------------------------------------------------------------------------------
procedure TDPFSegmentedControl.Paint;
var
  Caption        : string;
  R, Seg         : TRectF;
  SegmentWidth, x: single;

  procedure PaintButtons_30px( Bitmaps: TSegmentedControl_Bitmaps_30px; Shadow_Offset: integer; TextColor_Up, TextColor_Down: TAlphaColor );
  var
    i          : integer;
    CaptionRect: TRectF;

  begin
    with Bitmaps do
    begin
      for i := 0 to Segments.Count - 1 do
      begin
        // Actual segment's rectangle without the next separator
        Seg.Left    := Round( x );
        Seg.Right   := Round( x + SegmentWidth );
        CaptionRect := Seg;
        // Left border - if needed
        if i = 0 then
        begin
          R.Left  := Seg.Left;
          R.Right := Seg.Left + Left_Down.Width;
          if SelectedIndex = i then
            BitmapToRect( Self, Left_Down, R )
          else
            BitmapToRect( Self, Left_Up, R );
          Seg.Left := Seg.Left + Left_Down.Width;
        end;
        // Right border - if needed
        if i = Segments.Count - 1 then
        begin
          R.Left  := Seg.Right - Right_Down.Width;
          R.Right := Seg.Right;
          if SelectedIndex = i then
            BitmapToRect( Self, Right_Down, R )
          else
            BitmapToRect( Self, Right_Up, R );
          Seg.Right := Seg.Right - Right_Down.Width;
        end;
        // ButtonBG
        R.Left  := Seg.Left;
        R.Right := Seg.Right;
        if SelectedIndex = i then
          BitmapToRect( Self, ButtonBG_Down, R )
        else
          BitmapToRect( Self, ButtonBG_Up, R );
        x := Seg.Right;
        // Separator - if not last button
        if i < Segments.Count - 1 then
        begin
          R.Left  := Seg.Right;
          R.Right := Seg.Right + 1;
          if ( SelectedIndex = i ) or ( SelectedIndex = i + 1 ) then
            BitmapToRect( Self, Separator_Down, R )
          else
            BitmapToRect( Self, Separator_Up, R );
          x := x + 1;
        end;
        Canvas.Font.Size   := 13;
        Canvas.Font.Style  := [TFontStyle.fsBold];
        Canvas.Font.Family := 'Helvetica';
        if Shadow_Offset <> 0 then
        begin
          CaptionRect.Top    := CaptionRect.Top + Shadow_Offset;
          CaptionRect.Bottom := CaptionRect.Bottom + Shadow_Offset;
          Canvas.Fill.Color  := TAlphaColors.Black;
          Canvas.FillText( CaptionRect, Segments[i].Title, false, 1, [], TTextAlign.Center, TTextAlign.Center );
          CaptionRect.Top    := CaptionRect.Top - Shadow_Offset;
          CaptionRect.Bottom := CaptionRect.Bottom - Shadow_Offset;
        end;
        if SelectedIndex = i then
          Canvas.Fill.Color := TextColor_Down
        else
          Canvas.Fill.Color := TextColor_Up;
        Canvas.FillText( CaptionRect, Segments[i].Title, false, 1, [], TTextAlign.Center, TTextAlign.Center );
      end;
    end;
  end;

  procedure PaintButtons_44px( Bitmaps: TSegmentedControl_Bitmaps_44px; Shadow_Offset: integer; TextColor_Up, TextColor_Down: TAlphaColor );
  var
    i          : integer;
    CaptionRect: TRectF;

  begin
    with Bitmaps do
    begin
      for i := 0 to Segments.Count - 1 do
      begin
        // Actual segment's rectangle without the next separator
        Seg.Left    := Round( x );
        Seg.Right   := Round( x + SegmentWidth );
        CaptionRect := Seg;
        // Left border - if needed
        if i = 0 then
        begin
          R.Left  := Seg.Left;
          R.Right := Seg.Left + Left_Down.Width;
          if SelectedIndex = i then
            BitmapToRect( Self, Left_Down, R )
          else
            BitmapToRect( Self, Left_Up, R );
          Seg.Left := Seg.Left + Left_Down.Width;
        end;
        // Right border - if needed
        if i = Segments.Count - 1 then
        begin
          R.Left  := Seg.Right - Right_Down.Width;
          R.Right := Seg.Right;
          if SelectedIndex = i then
            BitmapToRect( Self, Right_Down, R )
          else
            BitmapToRect( Self, Right_Up, R );
          Seg.Right := Seg.Right - Right_Down.Width;
        end;
        // ButtonBG
        R.Left  := Seg.Left;
        R.Right := Seg.Right;
        if SelectedIndex = i then
          BitmapToRect( Self, ButtonBG_Down, R )
        else
          BitmapToRect( Self, ButtonBG_Up, R );
        x := Seg.Right;
        // Separator's right shadow
        if ( i > 0 ) and ( SelectedIndex = i ) then
        begin
          R.Left  := Seg.Left;
          R.Right := Seg.Left + Separator_DownRight.Width;
          BitmapToRect( Self, Separator_DownRight, R )
        end;
        // Separator - if not last button
        if i < Segments.Count - 1 then
        begin
          // Separator's left shadow
          if ( SelectedIndex = i ) then
          begin
            R.Left  := Seg.Right - Separator_DownLeft.Width;
            R.Right := Seg.Right;
            BitmapToRect( Self, Separator_DownLeft, R )
          end;
          // Separator
          R.Left  := Seg.Right;
          R.Right := Seg.Right + 1;
          if ( SelectedIndex = i ) or ( SelectedIndex = i + 1 ) then
            BitmapToRect( Self, Separator_Down, R )
          else
            BitmapToRect( Self, Separator_Up, R );
          x := x + 1;
        end;

        Canvas.Font.Size   := 15;
        Canvas.Font.Style  := [TFontStyle.fsBold];
        Canvas.Font.Family := 'Helvetica';
        if ( Shadow_Offset <> 0 ) and ( SelectedIndex <> i ) then
        begin
          CaptionRect.Top    := CaptionRect.Top + Shadow_Offset;
          CaptionRect.Bottom := CaptionRect.Bottom + Shadow_Offset;
          Canvas.Fill.Color  := TAlphaColors.White;
          Canvas.FillText( CaptionRect, Segments[i].Title, false, 1, [], TTextAlign.Center, TTextAlign.Center );
          CaptionRect.Top    := CaptionRect.Top - Shadow_Offset;
          CaptionRect.Bottom := CaptionRect.Bottom - Shadow_Offset;
        end;
        if SelectedIndex = i then
          Canvas.Fill.Color := TextColor_Down
        else
          Canvas.Fill.Color := TextColor_Up;
        Canvas.FillText( CaptionRect, Segments[i].Title, false, 1, [], TTextAlign.Center, TTextAlign.Center );
      end;
    end;
  end;

begin
  // Added by Fenistil
  Canvas.BeginScene;
  if Segments.Count <= 0 then
  begin
    Canvas.Fill.Color       := TAlphaColors.Gray;
    Canvas.Stroke.Kind      := TBrushKind.Solid;
    Canvas.Stroke.Color     := TAlphaColors.Black;
    Canvas.Stroke.Cap       := TStrokeCap.Round;
    Canvas.Stroke.Join      := TStrokeJoin.Round;
    Canvas.Stroke.Dash      := TStrokeDash.Solid;
    Canvas.Stroke.Thickness := 1;
    Canvas.DrawRect( ClipRect, 0, 0, AllCorners, 1, TCornerType.Round );
    Caption := name + #13#10 + '(Add segments!)';
    Canvas.FillText( ClipRect, Caption, True, 1, [], TTextAlign.Center, TTextAlign.Center );
  end
  else
  begin
    R            := ClipRect;
    Seg          := ClipRect;
    Seg.Left     := 0;
    Seg.Right    := 0;
    x            := 0;
    SegmentWidth := ( ClipRect.Width - ( Segments.Count - 1 ) ) / Segments.Count; // Width-(Segments * Separators[1px])
    case Style of
      scsBar:
        PaintButtons_30px( iOS_GUI_Bitmaps.SegmentedControl.Bar, -1, TAlphaColors.White, TAlphaColors.White );
      scsBezeled:
        PaintButtons_30px( iOS_GUI_Bitmaps.SegmentedControl.Bezeled, -1, $FFA8ADB4, TAlphaColors.White );
      scsBordered:
        PaintButtons_44px( iOS_GUI_Bitmaps.SegmentedControl.Bordered, 1, $FF7F7F7F, TAlphaColors.White );
      scsPlain:
        PaintButtons_44px( iOS_GUI_Bitmaps.SegmentedControl.Plain, 1, $FF7F7F7F, TAlphaColors.White );
    end;
  end;
  Canvas.EndScene;
end;
{$ENDIF}

// ------------------------------------------------------------------------------
procedure TDPFSegmentedControl.SetBackgroundColor( const Value: TAlphaColor );
begin
  FBackgroundColor := Value;
{$IFDEF IOS}
  if FUISegmentedControl <> nil then
  begin
    if FBackgroundColor <> TAlphaColors.Null then
      FUISegmentedControl.SetBackgroundColor( TColorToUIColor( FBackgroundColor ) )
    else
      FUISegmentedControl.SetBackgroundColor( TUIColor.Wrap( TUIColor.OCClass.clearColor ) );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFSegmentedControl.SetSegments( const Value: TDPFSegmentedControlCollection );
begin
  FSegments.Assign( Value );
end;

// ------------------------------------------------------------------------------
procedure TDPFSegmentedControl.SetSelectedIndex( const Value: Integer );
begin
  FSelectedIndex := Value;
{$IFDEF IOS}
  if FUISegmentedControl <> nil then
  begin
    FUISegmentedControl.setSelectedSegmentIndex( FSelectedIndex );
  end;
{$ELSE}
  // Added by Fenistil
  Invalidate;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFSegmentedControl.SetStyle( const Value: TDPFSegmentedControlStyle );
begin
  FStyle := Value;
{$IFNDEF IOS}
  (*
    case Style of
    scsBar, scsBezeled:
    Height := iOS_GUI_Bitmaps.SegmentedControl.Bar.ButtonBG_Down.Height;
    scsBordered, scsPlain:
    Height := iOS_GUI_Bitmaps.SegmentedControl.Bordered.ButtonBG_Down.Height;
    end;
  *)
  Invalidate;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFSegmentedControl.SetTintColor( const Value: TAlphaColor );
begin
  FTintColor := Value;
{$IFDEF IOS}
  if FUISegmentedControl <> nil then
  begin
    if Value <> TAlphaColors.Null then
      FUISegmentedControl.setTintColor( TColorToUIColor( Value ) )
    else
      FUISegmentedControl.setTintColor( nil );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
{ TDPFSegmentedControlItem }
procedure TDPFSegmentedControlItem.Apply( const isUpdate: Boolean; const Animated: Boolean = True );
{$IFDEF IOS}
var
  Img                 : UIImage;
  FDPFSegmentedControl: TDPFSegmentedControl;
{$ENDIF}
begin
{$IFDEF IOS}
  FDPFSegmentedControl := TDPFSegmentedControl( FOwner.Owner );
  if Image <> '' then
  begin
    Img := TUIImage.Wrap( TUIImage.OCClass.ImageNamed( NSStr( Image ) ) );
    if not isUpdate then
      FDPFSegmentedControl.FUISegmentedControl.insertSegmentWithImage( Img, index, Animated )
    else
      FDPFSegmentedControl.FUISegmentedControl.setImage( Img, index );
    Img := nil;
  end
  else
  begin
    if not isUpdate then
      FDPFSegmentedControl.FUISegmentedControl.insertSegmentWithTitle( NSStr( Title ), index, Animated )
    else
      FDPFSegmentedControl.FUISegmentedControl.setTitle( NSStr( Title ), index );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFSegmentedControlItem.Assign(Source: TPersistent);
  procedure AssignFromSegControlItem(Src: TDPFSegmentedControlItem);
  begin
    Title := Src.Title;
    Image := Src.Image;
    Enabled := Src.Enabled;
    Visible := Src.Visible;
    TagStr := Src.TagStr;
  end;
begin
  if Source is TDPFSegmentedControlItem then
    AssignFromSegControlItem(TDPFSegmentedControlItem(Source))
  else
    inherited;
end;

constructor TDPFSegmentedControlItem.Create( AOwner: TCollection );
begin
  inherited Create( AOwner );
  FOwner   := AOwner;
  FEnabled := True;
  FVisible := true;
end;

// ------------------------------------------------------------------------------
destructor TDPFSegmentedControlItem.Destroy;
begin

  inherited;
end;

// ------------------------------------------------------------------------------
function TDPFSegmentedControlItem.GetDisplayName: string;
begin
  Result := Format( 'Segment %d', [index] );
end;

// ------------------------------------------------------------------------------
procedure TDPFSegmentedControlItem.SetEnabled( const Value: Boolean );
begin
  FEnabled := Value;
{$IFDEF IOS}
  TDPFSegmentedControl( TDPFSegmentedControlCollection( FOwner ).FOwner ).FUISegmentedControl.setEnabled( FEnabled, index );
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFSegmentedControlItem.SetVisible( const Value: Boolean );
begin
{$IFDEF IOS}
  if Value then
    TDPFSegmentedControl( TDPFSegmentedControlCollection( FOwner ).FOwner ).FUISegmentedControl.setWidth( FOldWidth, index )
  else
  begin
    if FVisible <> Value then
      FOldWidth := TDPFSegmentedControl( TDPFSegmentedControlCollection( FOwner ).FOwner ).FUISegmentedControl.widthForSegmentAtIndex( index );
    TDPFSegmentedControl( TDPFSegmentedControlCollection( FOwner ).FOwner ).FUISegmentedControl.setWidth( 0.1, index );
  end;
{$ENDIF}
  FVisible := Value;
end;

// ------------------------------------------------------------------------------
procedure TDPFSegmentedControlItem.SetTitle( const Value: string );
begin
  FTitle := Value;
{$IFNDEF IOS}
  ( FOwner.Owner as TDPFSegmentedControl ).Invalidate;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
{ TDPFSegmentedControlCollection }
function TDPFSegmentedControlCollection.Add: TDPFSegmentedControlItem;
begin
  Result := inherited Add as TDPFSegmentedControlItem;
end;

// ------------------------------------------------------------------------------
constructor TDPFSegmentedControlCollection.Create( AOwner: TComponent );
begin
  inherited Create( TDPFSegmentedControlItem );
  FOwner := AOwner;
end;

// ------------------------------------------------------------------------------
destructor TDPFSegmentedControlCollection.Destroy;
begin

  inherited;
end;

// ------------------------------------------------------------------------------
function TDPFSegmentedControlCollection.GetItem( index: Integer ): TDPFSegmentedControlItem;
begin
  Result := inherited Items[index] as TDPFSegmentedControlItem;
end;

// ------------------------------------------------------------------------------
function TDPFSegmentedControlCollection.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

// ------------------------------------------------------------------------------
function TDPFSegmentedControlCollection.Insert( index: Integer ): TDPFSegmentedControlItem;
begin
  Result := inherited insert( index ) as TDPFSegmentedControlItem;
end;

// ------------------------------------------------------------------------------
procedure TDPFSegmentedControlCollection.SetItem( index: Integer; Value: TDPFSegmentedControlItem );
begin
  inherited SetItem( index, Value );
end;

procedure TDPFSegmentedControlCollection.Update( Item: TCollectionItem );
begin
  inherited;
{$IFNDEF IOS}
  ( Owner as TDPFSegmentedControl ).Invalidate;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
{$IFDEF IOS}

// ------------------------------------------------------------------------------
{ TDPFSegmentedControlDelegate }
constructor TDPFSegmentedControlDelegate.Create( ADPFSegmentedControl: TDPFSegmentedControl );
begin
  inherited Create;
  FDPFSegmentedControl := ADPFSegmentedControl;
end;

// ------------------------------------------------------------------------------
function TDPFSegmentedControlDelegate.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo( DPFSegmentedControlDelegate );
end;

// ------------------------------------------------------------------------------
procedure TDPFSegmentedControlDelegate.valueChanged;
begin
  FDPFSegmentedControl.FSelectedIndex := FDPFSegmentedControl.FUISegmentedControl.selectedSegmentIndex;
  if Assigned( FDPFSegmentedControl.OnChanged ) then
  begin
    FDPFSegmentedControl.OnChanged( FDPFSegmentedControl, FDPFSegmentedControl.FSelectedIndex );
  end;
end;
{$ENDIF}

// ------------------------------------------------------------------------------
end.
