// ------------------------------------------------------------------------------
// DPF.Android.JView Component
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
unit DPF.Android.JView;

interface

{$I DPF.Android.Defs.inc}

uses
  System.SysUtils,
  System.Classes,
  System.Types,
  System.UITypes,
  System.Variants,
  System.Math,

  System.TypInfo,
  DPF.Android.BaseControl,
{$IFDEF ANDROID}
  Androidapi.JNI.Widget,
  Androidapi.Jni,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNIBridge,
  Androidapi.JNI.GraphicsContentViewText,
  FMX.Helpers.Android,
  Androidapi.Helpers,
{$ELSE}
  DPF.Android.DesignTime,
{$ENDIF}
  FMX.Forms,
{$IFDEF DELPHIXE5} FMX.Graphics, {$ENDIF}FMX.Types;

type

  TDPFJView = class;

{$IFDEF ANDROID}
{$ENDIF}
  TDPFViewOnDrawRect = procedure( Sender: TObject; Rect: DPFNSRect ) of object;
  TDPFTouchesBegan   = procedure( Sender: TObject; const Location: DPFNSPoint; const PreviousLocation: DPFNSPoint ) of object;
  TDPFTouchesMoved   = procedure( Sender: TObject; const Location: DPFNSPoint; const PreviousLocation: DPFNSPoint ) of object;
  TDPFTouchesEnded   = procedure( Sender: TObject; const TapCount: Integer; const Location: DPFNSPoint; const PreviousLocation: DPFNSPoint ) of object;

  [ComponentPlatformsAttribute( PidWin32 or pidAndroid )]
  TDPFJView = class( TDPFANDBaseControl )
  private
    FEmbFrame       : TFrame;
    FForm           : TForm;
    FOnFormChanging : TDPFFormChanging;
    FOnFormChanged  : TDPFFormChanged;
    FFrame          : TDPFFrame;
    FOnFrameChanged : TDPFFrameChanged;
    FOnFrameChanging: TDPFFrameChanging;
    procedure SetForm( const Value: TForm );
    procedure SetFrame( const Value: TDPFFrame );

  protected
{$IFDEF ANDROID}
    FJView: JView;
{$ENDIF}
    procedure Resize; override;
    procedure Move; override;
{$IFNDEF ANDROID}
    procedure Paint; override;
{$ENDIF}
  public
{$IFDEF ANDROID}
    property GetJView: JView read FJView;
    procedure Loaded; override;
{$ENDIF}
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;
    // procedure clearAllSubviews( isRelease: Boolean = false );
    function GetFrame: TFrame;

  published
    property Form : TForm read FForm write SetForm;
    property Frame: TDPFFrame read FFrame write SetFrame;

    property OnFormChanging: TDPFFormChanging read FOnFormChanging write FOnFormChanging;
    property OnFormChanged : TDPFFormChanged read FOnFormChanged write FOnFormChanged;

    property OnFrameChanging: TDPFFrameChanging read FOnFrameChanging write FOnFrameChanging;
    property OnFrameChanged : TDPFFrameChanged read FOnFrameChanged write FOnFrameChanged;

    property Clickable;
    property Focusable;
    property FocusableInTouchMode;
    property BackgroundColor1;
    property BackgroundColor2;
    property BackgroundColor3;
    property BackgroundImage;
    property BorderWidth;
    property BorderColor;
    property BorderCornerRadius;
    property GradientOrientation;
    property Alpha;
    property Align;
    property Position;
    property Width;
    property Height;
    property Visible;
    property OnClick;
  end;

implementation

// ------------------------------------------------------------------------------
{ TDPFJView }
constructor TDPFJView.Create( AOwner: TComponent );
begin
  inherited Create( AOwner );
  ControlCaption   := 'View Control';
  BackgroundColor1 := TAlphaColors.Lightsteelblue;
  Visible          := True;
  FForm            := nil;

{$IFDEF ANDROID}
  CallInUIThreadAndWaitFinishing(
    procedure( )
    begin
      FJView := TJView.JavaClass.init( SharedActivity );
    end );
  JControl := FJView;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
destructor TDPFJView.Destroy;
begin
{$IFDEF ANDROID}
{$ENDIF}
  inherited;
end;

// ------------------------------------------------------------------------------
function TDPFJView.GetFrame: TFrame;
begin
  result := FEmbFrame;
end;

// ------------------------------------------------------------------------------
{$IFDEF ANDROID}

procedure TDPFJView.Loaded;
begin
  Resize; // Very Important for Embbeded Forms, Frames

  addSubview( Self, ParentControl );
  // ----------------------------
  // Important
  inherited;
end;
{$ENDIF}

// ------------------------------------------------------------------------------
procedure TDPFJView.Resize;
begin
{$IFDEF ANDROID}
{$ENDIF}
  inherited;
end;

// ------------------------------------------------------------------------------
procedure TDPFJView.Move;
begin
  Resize;
end;

{$IFNDEF ANDROID}

// ------------------------------------------------------------------------------
procedure TDPFJView.Paint;
var
  C: TAlphaColor;
begin
  C := BackgroundColor1;
  if C = TAlphaColors.Null then
    C := TAlphaColors.White;
  InternalPaint( '', TAlphaColors.Black, TDPFTextAlignment.taCenter, C );
end;
{$ENDIF}

// ------------------------------------------------------------------------------
procedure TDPFJView.SetForm( const Value: TForm );
{$IFDEF ANDROID}
var
  C        : TDPFANDBaseControl;
  CanChange: Boolean;
{$ENDIF}
begin
{$IFDEF ANDROID}
  if ( FJView <> nil ) and ( ( FForm <> Value ) or ( Assigned( Value ) and not Assigned( Value.TagObject ) ) ) then
  begin
    CanChange := True;
    if Assigned( FOnFormChanging ) then
      FOnFormChanging( Self, FForm, Value, CanChange );
    if not CanChange then
      exit;

    if Assigned( FForm ) then
    begin
      MoveChildsToOriginParent( Self );
      // clearAllSubviews( );
      FForm.TagObject := nil;
    end;

    if Assigned( Value ) then
    begin

      if Value.TagObject <> nil then
        MoveChildsToOriginParent( Value.TagObject as TDPFANDBaseControl );

      while Value.ChildrenCount <> 0 do
        if Value.Children[0] is TDPFANDBaseControl then
        begin
          C        := Value.Children[0] as TDPFANDBaseControl;
          C.Parent := Self;
          C.Loaded;
          Value.TagObject := Self;
        end;
    end;
    if Assigned( FOnFormChanged ) then
      FOnFormChanged( Self, FForm, Value );
  end;
{$ENDIF}
  FForm := Value;
end;

// ------------------------------------------------------------------------------
procedure TDPFJView.SetFrame( const Value: TDPFFrame );
{$IFDEF ANDROID}
var
  C        : TDPFANDBaseControl;
  CanChange: Boolean;
{$ENDIF}
begin
{$IFDEF ANDROID}
  if Assigned( FEmbFrame ) and ( FEmbFrame is Value ) then
    exit;

  if ( FJView <> nil ) then
  begin
    CanChange := True;
    if Assigned( FOnFrameChanging ) then
      FOnFrameChanging( Self, FFrame, Value, CanChange );
    if not CanChange then
      exit;

    if Assigned( FEmbFrame ) then
    begin
      MoveChildsToOriginParent( Self );
      // clearAllSubviews(  );
      FEmbFrame.TagObject := nil;
      FEmbFrame.DisposeOf;
      FEmbFrame := nil;
    end;
    if Assigned( Value ) then
    begin
      FEmbFrame := Value.Create( nil );

      while FEmbFrame.ChildrenCount <> 0 do
        if FEmbFrame.Children[0] is TDPFANDBaseControl then
        begin
          C        := FEmbFrame.Children[0] as TDPFANDBaseControl;
          C.Parent := Self;
          C.Loaded;
        end;
    end;
    if Assigned( FOnFrameChanged ) then
      FOnFrameChanged( Self, Frame, Value );
  end;
{$ENDIF}
  FFrame := Value;
end;

// ------------------------------------------------------------------------------

end.
