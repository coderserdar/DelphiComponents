// ------------------------------------------------------------------------------
// DPF.Android.JSpinner Component
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
unit DPF.Android.JSpinner;

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
  DPF.Android.Widget,
  Androidapi.JNI.Widget,
  Androidapi.Jni,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNIBridge,
  Androidapi.JNI.GraphicsContentViewText,
  FMX.Helpers.Android,
{$IFDEF DELPHIXE6}
  Androidapi.Helpers,
{$ENDIF}
{$ELSE}
  DPF.Android.DesignTime,
{$ENDIF}
  FMX.Forms,
{$IFDEF DELPHIXE5} FMX.Graphics, {$ENDIF}FMX.Types;

type

  TDPFJSpinner = class;

{$IFDEF ANDROID}

  TDPFSpinnerOnItemSelectedListener = class( TJavaLocal, JAdapterView_OnItemSelectedListener )
  private
    FDPFJSpinner: TDPFJSpinner;
  public
    constructor create( ADPFJSpinner: TDPFJSpinner );

    procedure onItemSelected( parent: JAdapterView; view: JView; position: Integer; id: Int64 ); cdecl;
    procedure onNothingSelected( parent: JAdapterView ); cdecl;
  end;
{$ENDIF}

  TDPFSpinnerStyle = ( ssDropdown, { ssSelectableListItem, ssSingleChoice, ssMultipleChoice, ssItemChecked, ssItemActivated2, ssItemActivated1, } ssSpinnerItem );

  TDPFSpinnerOnItemSelected    = procedure( sender: TObject; position: Integer; id: Int64 ) of object;
  TDPFSpinneronNothingSelected = procedure( sender: TObject ) of object;

  [ComponentPlatformsAttribute( PidWin32 or pidAndroid )]
  TDPFJSpinner = class( TDPFANDBaseControl )
  private
    FItems            : TStrings;
    FSpinnerStyle     : TDPFSpinnerStyle;
    FOnItemSelected   : TDPFSpinnerOnItemSelected;
    FOnNothingSelected: TDPFSpinnerOnNothingSelected;
    FPrompt           : string;
    procedure SetItems( const Value: TStrings );
    procedure TextChanged( Sender: TObject );
    procedure SetPrompt( const Value: string );
  protected
{$IFDEF ANDROID}
    FJSpinner                        : JSpinner;
    FJSpinnerDataAdapter             : JArrayAdapter;
    FJSpinnerList                    : JArrayList;
    FDPFSpinnerOnItemSelectedListener: TDPFSpinnerOnItemSelectedListener;
{$ENDIF}
    procedure Resize; override;
    procedure Move; override;
{$IFNDEF ANDROID}
    procedure Paint; override;
{$ENDIF}
  public
{$IFDEF ANDROID}
    property GetJSpinner: JSpinner read FJSpinner;
    procedure Loaded; override;
{$ENDIF}
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;

  published
    property Items       : TStrings read FItems write SetItems;
    property SpinnerStyle: TDPFSpinnerStyle read FSpinnerStyle write FSpinnerStyle default TDPFSpinnerStyle.ssDropdown;
    property Prompt      : string read FPrompt write SetPrompt;

    property OnItemSelected   : TDPFSpinnerOnItemSelected read FOnItemSelected write FOnItemSelected;
    property OnNothingSelected: TDPFSpinnerOnNothingSelected read FOnNothingSelected write FOnNothingSelected;

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

const
  TDPFSpinnerStyleToInt: array [TDPFSpinnerStyle] of integer = ( 17367049, { 17367061, 17367055, 17367056, 17367045, 17367063, 17367062, } 17367048 );

  // ------------------------------------------------------------------------------
  { TDPFJSpinner }
constructor TDPFJSpinner.Create( AOwner: TComponent );
begin
  inherited Create( AOwner );
  ControlCaption   := 'Spinner';
  BackgroundColor1 := TAlphaColors.Lightsteelblue;
  FSpinnerStyle    := TDPFSpinnerStyle.ssDropdown;
  FItems           := TStringList.Create;

{$IFDEF ANDROID}
  CallInUIThreadAndWaitFinishing(
    procedure( )
    begin
      FJSpinner := TJSpinner.JavaClass.init( SharedActivity );
      FDPFSpinnerOnItemSelectedListener := TDPFSpinnerOnItemSelectedListener.create( self );
      FJSpinner.setOnItemSelectedListener( FDPFSpinnerOnItemSelectedListener );
    end );
  JControl := FJSpinner;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
destructor TDPFJSpinner.Destroy;
begin
{$IFDEF ANDROID}
  FDPFSpinnerOnItemSelectedListener := nil;
  FJSpinnerDataAdapter              := nil;
  FJSpinnerList                     := nil;
{$ENDIF}
  FItems.Free;
  inherited;
end;

// ------------------------------------------------------------------------------
{$IFDEF ANDROID}

procedure TDPFJSpinner.Loaded;
begin
  TextChanged( nil );
  SetPrompt( FPrompt );
  TStringList( FItems ).OnChange := TextChanged;
  addSubview( Self, ParentControl );
  // ----------------------------
  // Important
  inherited;
end;
{$ENDIF}

// ------------------------------------------------------------------------------
procedure TDPFJSpinner.Resize;
begin
{$IFDEF ANDROID}
{$ENDIF}
  inherited;
end;

// ------------------------------------------------------------------------------
procedure TDPFJSpinner.SetItems( const Value: TStrings );
begin
  FItems.Assign( Value );
end;

// ------------------------------------------------------------------------------
procedure TDPFJSpinner.SetPrompt( const Value: string );
begin
  FPrompt := Value;
{$IFDEF ANDROID}
  if assigned( FJSpinner ) then
    CallInUIThreadAndWaitFinishing(
      procedure
      begin
        if Value <> '' then
          FJSpinner.setPrompt( StrToJCharSequence( Value ) )
        else
          FJSpinner.setPrompt( nil );
      end );
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFJSpinner.TextChanged( Sender: TObject );
begin
{$IFDEF ANDROID}
  CallInUIThreadAndWaitFinishing(
    procedure
    var
      I: Integer;
    begin
      if assigned( FJSpinnerDataAdapter ) and ( FItems.Count = 0 ) then
        FJSpinnerDataAdapter.clear;

      // --------------------------------------------------------------------------
      // simple_spinner_dropdown_item = 17367049
      // simple_selectable_list_item = 17367061
      // simple_list_item_single_choice = 17367055
      // simple_list_item_multiple_choice = 17367056
      // simple_list_item_checked = 17367045
      // simple_list_item_activated_2 = 17367063
      // simple_list_item_activated_1 = 17367062
      // simple_spinner_item = 17367048
      //
      FJSpinnerList := TJArrayList.JavaClass.init( FItems.Count );
      for I := 0 to FItems.Count - 1 do
      begin
        FJSpinnerList.add( StringToJString( FItems[I] ) );
      end;

      FJSpinnerDataAdapter := TJArrayAdapter.JavaClass.init( SharedActivity, TDPFSpinnerStyleToInt[FSpinnerStyle], TJList.Wrap( ( FJSpinnerList as ILocalObject ).GetObjectID ) );
      FJSpinnerDataAdapter.setDropDownViewResource( TDPFSpinnerStyleToInt[FSpinnerStyle] );
      FJSpinner.setAdapter( TJSpinnerAdapter.Wrap( ( FJSpinnerDataAdapter as ILocalObject ).GetObjectID ) );
    end );
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFJSpinner.Move;
begin
  inherited;
end;

{$IFNDEF ANDROID}

// ------------------------------------------------------------------------------
procedure TDPFJSpinner.Paint;
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

{$IFDEF ANDROID}

constructor TDPFSpinnerOnItemSelectedListener.create( ADPFJSpinner: TDPFJSpinner );
begin
  inherited create;
  FDPFJSpinner := ADPFJSpinner;
end;

// ------------------------------------------------------------------------------
procedure TDPFSpinnerOnItemSelectedListener.onItemSelected( parent: JAdapterView; view: JView; position: Integer; id: Int64 ); cdecl;
begin
  if assigned( FDPFJSpinner.OnItemSelected ) then
    FDPFJSpinner.OnItemSelected( FDPFJSpinner, position, id );
end;

// ------------------------------------------------------------------------------
procedure TDPFSpinnerOnItemSelectedListener.onNothingSelected( parent: JAdapterView ); cdecl;
begin
  if assigned( FDPFJSpinner.OnNothingSelected ) then
    FDPFJSpinner.OnNothingSelected( FDPFJSpinner );
end;

{$ENDIF}
// ------------------------------------------------------------------------------

end.
