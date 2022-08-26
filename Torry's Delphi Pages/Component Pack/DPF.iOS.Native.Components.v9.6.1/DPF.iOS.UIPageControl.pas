// ------------------------------------------------------------------------------
// DPF.iOS.UIPageControl Component
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
unit DPF.iOS.UIPageControl;

interface

{$I DPF.iOS.Defs.inc}

uses
  System.SysUtils,
  System.Classes,
  System.Types,
  System.UITypes,

  System.TypInfo,
  DPF.iOS.BaseControl,
{$IFDEF IOS}
  Macapi.ObjectiveC,
  Macapi.ObjCRuntime,
  IOSapi.CocoaTypes,
  IOSapi.Foundation,
  IOSapi.Uikit,
  IOSapi.CoreGraphics,
  DPF.iOS.Common,
{$ENDIF}
{$IFDEF DELPHIXE5} FMX.Graphics, {$ENDIF}
  FMX.Types;

type
  TDPFUIPageControl           = class;
  TDPFPageControlOnPageChange = procedure( sender: TObject; FromPage: Integer; ToPage: Integer ) of object;

{$IFDEF IOS}

  UIPageControlClass = interface( UIControlClass )
    ['{02BA0834-E1CB-4167-9567-F15A89B9BD62}']
  end;

  UIPageControl = interface( UIControl )
    ['{5BDDAB85-5146-44FB-8B76-58B3A630D356}']
    function pageIndicatorTintColor: UIColor; cdecl;
    function currentPageIndicatorTintColor: UIColor; cdecl;
    function currentPage: NSInteger; cdecl;
    function defersCurrentPageDisplay: Boolean; cdecl;
    function hidesForSinglePage: Boolean; cdecl;
    function numberOfPages: NSInteger; cdecl;

    procedure setPageIndicatorTintColor( color: UIColor ); cdecl;
    procedure setCurrentPageIndicatorTintColor( color: UIColor ); cdecl;
    procedure setCurrentPage( currentPage: NSInteger ); cdecl;
    procedure setDefersCurrentPageDisplay( defersCurrentPageDisplay: Boolean ); cdecl;
    procedure setHidesForSinglePage( hidesForSinglePage: Boolean ); cdecl;
    procedure setNumberOfPages( numberOfPages: NSInteger ); cdecl;
    function sizeForNumberOfPages( pageCount: NSInteger ): CGSize; cdecl;
    procedure updateCurrentPageDisplay; cdecl;
  end;

  TUIPageControl = class( TOCGenericImport<UIPageControlClass, UIPageControl> )
  end;

  // ------------------------------------------------------------------------------
  DPFPageControlDelegate = interface( NSObject )
    ['{A2408AE4-731B-48C5-A9ED-8DF176A38B55}']
    procedure onPageChanged; cdecl;
  end;

  // ------------------------------------------------------------------------------

  TDPFPageControlDelegate = class( TOCLocal )
  private
    FDPFUIPageControl: TDPFUIPageControl;
  public
    constructor Create( ADPFUIPageControl: TDPFUIPageControl );
    function GetObjectiveCClass: PTypeInfo; override;
    procedure onPageChanged; cdecl;
  end;

{$ENDIF}

  [ComponentPlatformsAttribute( PidWin32 or pidiOSDevice32 or pidiOSDevice64 or PidiOSSimulator )]
  TDPFUIPageControl = class( TDPFiOSBaseControl )
  private
{$IFDEF IOS}
    FUIPageControl         : UIPageControl;
    FDPFPageControlDelegate: TDPFPageControlDelegate;
{$ENDIF}
    FBackgroundColor              : TAlphaColor;
    FNumberOfPages                : Integer;
    FCurrentPage                  : Integer;
    FHidesForSinglePage           : Boolean;
    FDefersCurrentPageDisplay     : Boolean;
    FCurrentPageIndicatorTintColor: TAlphaColor;
    FPageIndicatorTintColor       : TAlphaColor;
    FOnPageChange                 : TDPFPageControlOnPageChange;
    FEnabled                      : Boolean;
    procedure SetBackgroundColor( const Value: TAlphaColor );
    procedure SetCurrentPage( const Value: Integer );
    procedure SetNumberOfPages( const Value: Integer );
    procedure SetHidesForSinglePage( const Value: Boolean );
    procedure SetDefersCurrentPageDisplay( const Value: Boolean );
    procedure SetCurrentPageIndicatorTintColor( const Value: TAlphaColor );
    procedure SetPageIndicatorTintColor( const Value: TAlphaColor );
    procedure SetEnabled( const Value: Boolean );

  protected
    FLastPage: Integer;
    procedure Resize; override;
    procedure Move; override;
  public
{$IFDEF IOS}
    procedure Loaded; override;
{$ELSE}
    procedure Paint; override;
{$ENDIF}
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;
    procedure UpdateCurrentPageDisplay;
{$IFDEF IOS}
    function GetSizeForNumberOfPages( pageCount: NSInteger ): CGSize;
{$ENDIF}
  published
    property BackgroundColor              : TAlphaColor read FBackgroundColor write SetBackgroundColor default TAlphaColors.White;
    property CurrentPageIndicatorTintColor: TAlphaColor read FCurrentPageIndicatorTintColor write SetCurrentPageIndicatorTintColor default TAlphaColors.White;
    property PageIndicatorTintColor       : TAlphaColor read FPageIndicatorTintColor write SetPageIndicatorTintColor default $4CFFFFFF;
    property NumberOfPages                : Integer read FNumberOfPages write SetNumberOfPages default 1;
    property CurrentPage                  : Integer read FCurrentPage write SetCurrentPage default 0;
    property HidesForSinglePage           : Boolean read FHidesForSinglePage write SetHidesForSinglePage default false;
    property DefersCurrentPageDisplay     : Boolean read FDefersCurrentPageDisplay write SetDefersCurrentPageDisplay default false;
    property OnPageChange                 : TDPFPageControlOnPageChange read FOnPageChange write FOnPageChange;
    property Enabled                      : Boolean read FEnabled write SetEnabled default True;

    property Visible;
    property Align;
    property Position;
    property Width;
    property Height;
  end;

implementation

// ------------------------------------------------------------------------------
{ TDPFUIPageControl }
constructor TDPFUIPageControl.Create( AOwner: TComponent );
begin
  inherited Create( AOwner );
  ControlCaption            := 'Page Control';
  FBackgroundColor          := TAlphaColors.White;
  FNumberOfPages            := 1;
  FPageIndicatorTintColor   := TAlphaColors.White;
  FHidesForSinglePage       := false;
  FDefersCurrentPageDisplay := false;
  FCurrentPage              := 0;
  FLastPage                 := 0;
  FEnabled                  := true;

{$IFDEF IOS}
  FUIPageControl := TUIPageControl.Create;
  FUIControl     := FUIPageControl;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
destructor TDPFUIPageControl.Destroy;
begin
{$IFDEF IOS}
  FUIPageControl.removeTarget( FDPFPageControlDelegate.GetObjectID, // target
    Sel_getUid( 'onPageChanged' ), // action
    UIControlEventValueChanged ); // event
  FDPFPageControlDelegate.DisposeOf;
{$ENDIF}
  inherited;
end;

// ------------------------------------------------------------------------------
{$IFDEF IOS}

procedure TDPFUIPageControl.Loaded;
begin
  FUIPageControl.setFrame( CGRectMake( Position.X, Position.Y, Width, Height ) );

  FUIPageControl.setBackgroundColor( TColorToUIColor( FBackgroundColor ) );
  SetNumberOfPages( FNumberOfPages );
  SetCurrentPage( FCurrentPage );
  SetHidesForSinglePage( FHidesForSinglePage );
  SetDefersCurrentPageDisplay( FDefersCurrentPageDisplay );
  SetEnabled( Enabled );
  FDPFPageControlDelegate := TDPFPageControlDelegate.Create( Self );
  FUIPageControl.AddTarget( FDPFPageControlDelegate.GetObjectID, // target
    Sel_getUid( 'onPageChanged' ), // action
    UIControlEventValueChanged ); // event

  AddSubView( Self, ParentControl );
  Resize;
end;
{$ENDIF}

// ------------------------------------------------------------------------------
procedure TDPFUIPageControl.Resize;
begin
  inherited;
  exit;
{$IFDEF IOS}
  if FUIPageControl <> nil then
  begin
    FUIPageControl.setFrame( CGRectMake( Position.X, Position.Y, Width, Height ) );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFUIPageControl.Move;
begin
  Resize;
end;

{$IFNDEF IOS}

// ------------------------------------------------------------------------------
procedure TDPFUIPageControl.Paint;
var
  x, y, i: integer;
begin
  // Added by Fenistil
  Canvas.BeginScene;
  Canvas.Fill.Kind := TBrushKind.Solid;
  if BackgroundColor <> TAlphaColors.Null then
  begin
    Canvas.Fill.Color := BackgroundColor;
    Canvas.FillRect( ClipRect, 0.5, 0.5, AllCorners, Alpha, TCornerType.InnerRound );
  end;
  x                  := Round( ClipRect.CenterPoint.X - ( NumberOfPages * 6 + ( NumberOfPages - 1 ) * 10 ) / 2 );
  y                  := Round( ClipRect.CenterPoint.Y - 3 );
  Canvas.Stroke.Kind := TBrushKind.None;
  for i              := 1 to NumberOfPages do
  begin
    if i = 1 then
      Canvas.Fill.Color := CurrentPageIndicatorTintColor
    else
      Canvas.Fill.Color := PageIndicatorTintColor;
    Canvas.FillEllipse( RectF( x, y, x + 6, y + 6 ), 1 );
    x := x + 16;
  end;
  Canvas.EndScene;
end;
{$ENDIF}

// ------------------------------------------------------------------------------
procedure TDPFUIPageControl.SetBackgroundColor( const Value: TAlphaColor );
begin
  FBackgroundColor := Value;
{$IFDEF IOS}
  if FUIPageControl <> nil then
  begin
    if FBackgroundColor <> TAlphaColors.Null then
      FUIPageControl.setBackgroundColor( TColorToUIColor( FBackgroundColor ) )
    else
      FUIPageControl.setBackgroundColor( TUIColor.Wrap( TUIColor.OCClass.clearColor ) );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFUIPageControl.SetCurrentPage( const Value: Integer );
begin
  FCurrentPage := Value;
  FLastPage    := Value;
{$IFDEF IOS}
  if FUIPageControl <> nil then
  begin
    FUIPageControl.setCurrentPage( FCurrentPage );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFUIPageControl.SetCurrentPageIndicatorTintColor( const Value: TAlphaColor );
begin
  FCurrentPageIndicatorTintColor := Value;
{$IFDEF IOS}
  if FUIPageControl <> nil then
  begin
    if FCurrentPageIndicatorTintColor <> TAlphaColors.Null then
      FUIPageControl.setCurrentPageIndicatorTintColor( TColorToUIColor( FCurrentPageIndicatorTintColor ) )
    else
      FUIPageControl.setCurrentPageIndicatorTintColor( TUIColor.Wrap( TUIColor.OCClass.clearColor ) );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFUIPageControl.SetDefersCurrentPageDisplay( const Value: Boolean );
begin
  FDefersCurrentPageDisplay := Value;
{$IFDEF IOS}
  if FUIPageControl <> nil then
  begin
    FUIPageControl.setDefersCurrentPageDisplay( Value );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFUIPageControl.SetEnabled( const Value: Boolean );
begin
  FEnabled := Value;
{$IFDEF IOS}
  if Assigned( FUIPageControl ) then
    FUIPageControl.setEnabled( FEnabled );
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFUIPageControl.SetHidesForSinglePage( const Value: Boolean );
begin
  FHidesForSinglePage := Value;
{$IFDEF IOS}
  if FUIPageControl <> nil then
  begin
    FUIPageControl.setHidesForSinglePage( Value );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFUIPageControl.SetNumberOfPages( const Value: Integer );
begin
  FNumberOfPages := Value;
{$IFDEF IOS}
  if FUIPageControl <> nil then
  begin
    FUIPageControl.SetNumberOfPages( FNumberOfPages );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFUIPageControl.SetPageIndicatorTintColor( const Value: TAlphaColor );
begin
  FPageIndicatorTintColor := Value;
{$IFDEF IOS}
  if FUIPageControl <> nil then
  begin
    if FPageIndicatorTintColor <> TAlphaColors.Null then
      FUIPageControl.setPageIndicatorTintColor( TColorToUIColor( FPageIndicatorTintColor ) )
    else
      FUIPageControl.setPageIndicatorTintColor( TUIColor.Wrap( TUIColor.OCClass.clearColor ) );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFUIPageControl.UpdateCurrentPageDisplay;
begin
{$IFDEF IOS}
  if FUIPageControl <> nil then
    FUIPageControl.updateCurrentPageDisplay;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
{$IFDEF IOS}

function TDPFUIPageControl.GetSizeForNumberOfPages( pageCount: NSInteger ): CGSize;
begin
  if FUIPageControl <> nil then
    result := FUIPageControl.sizeForNumberOfPages( pageCount );
end;
// ------------------------------------------------------------------------------
{ TDPFPageControlDelegate }

constructor TDPFPageControlDelegate.Create( ADPFUIPageControl: TDPFUIPageControl );
begin
  inherited Create;
  FDPFUIPageControl := ADPFUIPageControl;
end;

// ------------------------------------------------------------------------------
function TDPFPageControlDelegate.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo( DPFPageControlDelegate );
end;

// ------------------------------------------------------------------------------
procedure TDPFPageControlDelegate.onPageChanged;
begin
  if Assigned( FDPFUIPageControl.OnPageChange ) then
  begin
    FDPFUIPageControl.OnPageChange( FDPFUIPageControl, FDPFUIPageControl.FLastPage, FDPFUIPageControl.FUIPageControl.currentPage );
  end;
  FDPFUIPageControl.FLastPage := FDPFUIPageControl.FUIPageControl.currentPage;
end;
{$ENDIF}

// ------------------------------------------------------------------------------
end.
