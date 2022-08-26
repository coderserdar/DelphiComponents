// ------------------------------------------------------------------------------
// DPF.iOS.UIFont Class
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
unit DPF.iOS.UIFont;

interface

{$I DPF.iOS.Defs.inc}

uses
  System.SysUtils,
  System.Classes,
  System.Types,
  System.UITypes,
  System.Variants,

  System.TypInfo,

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
{$ENDIF}
  FMX.Layouts, FMX.Memo,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls;

const
  // http://iphonedevwiki.net/index.php/UIFont

  ios_smallSystemFontSize           = 12;
  ios_systemFontSize                = 14;
  ios_boldSystemFontSize            = 20;
  ios_labelFontSize                 = 17;
  ios_buttonFontSize                = 18;
  ios_DefaultFontSizeForUIButton    = 15;
  ios_DefaultFontSizeForUITextField = 12;

type

  TDPFIOSFontList = (

    ios_AmericanTypewriter, ios_AmericanTypewriter_Bold, ios_AppleGothic,

    ios_ArialMT, ios_Arial_BoldMT, ios_Arial_BoldItalicMT, ios_Arial_ItalicMT,

    ios_ArialRoundedMTBold,

    ios_ArialUnicodeMS,

    ios_Courier, ios_Courier_BoldOblique, ios_Courier_Oblique, ios_Courier_Bold,

    ios_CourierNewPS_BoldMT, ios_CourierNewPS_ItalicMT, ios_CourierNewPS_BoldItalicMT, ios_CourierNewPSMT,

    ios_DBLCDTempBlack,

    ios_Georgia_Bold, ios_Georgia, ios_Georgia_BoldItalic, ios_Georgia_Italic,

    ios_Helvetica_Oblique, ios_Helvetica_BoldOblique, ios_Helvetica, ios_Helvetica_Bold,

    ios_HelveticaNeue, ios_HelveticaNeue_Light, ios_HelveticaNeue_UltraLight, ios_HelveticaNeue_Bold,

    ios_HiraKakuProN_W3, ios_HiraKakuProN_W6,

    ios_MarkerFelt_Thin,

    ios_STHeitiJ_Medium, ios_STHeitiJ_Light,

    ios_GeezaPro, ios_GeezaPro_Bold,

    ios_STHeitiK_Medium, ios_STHeitiK_Light,

    ios_STHeitiSC_Medium, ios_STHeitiSC_Light,

    ios_STHeitiTC_Light, ios_STHeitiTC_Medium,

    ios_TimesNewRomanPSMT, ios_TimesNewRomanPS_BoldMT, ios_TimesNewRomanPS_BoldItalicMT, ios_TimesNewRomanPS_ItalicMT,

    ios_TrebuchetMS_Italic, ios_TrebuchetMS, ios_Trebuchet_BoldItalic, ios_TrebuchetMS_Bold,

    ios_Verdana_Bold, ios_Verdana_BoldItalic, ios_Verdana, ios_Verdana_Italic,

    ios_Zapfino );

  // ------------------------------------------------------------------------------

  TDPFFont = class( TPersistent )
  private
    FFontSize          : integer;
    FFontName          : TDPFIOSFontList;
    FOnChanged         : TNotifyEvent;
    FBoldSystemFontSize: Boolean;
{$IFDEF IOS}
    FUIFont: UIFont;
    function GetFUIFont: UIFont;
{$ENDIF}
    procedure SetFontName( const Value: TDPFIOSFontList );
    procedure SetFontSize( const Value: integer );
    procedure SetBoldSystemFontSize( const Value: Boolean );
  protected
  public
    constructor create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
{$IFDEF IOS}
    property _UIFont: UIFont read GetFUIFont;
{$ENDIF}
  published
    property FontName          : TDPFIOSFontList read FFontName write SetFontName default ios_Verdana;
    property FontSize          : integer read FFontSize write SetFontSize default ios_systemFontSize;
    property BoldSystemFontSize: Boolean read FBoldSystemFontSize write SetBoldSystemFontSize default false;
    property OnChanged         : TNotifyEvent read FOnChanged write FOnChanged;
  end;

function GetDeviceFontName( FontName: TDPFIOSFontList ): string;
{$IFDEF IOS}
function GetFontsList: TStringList;
{$ENDIF}

// ------------------------------------------------------------------------------
implementation

const
  TDPFIOSFontStrList: array [TDPFIOSFontList] of string = (

    'AmericanTypewriter', 'AmericanTypewriter-Bold', 'AppleGothic',

    'ArialMT', 'Arial-BoldMT', 'Arial-BoldItalicMT', 'Arial-ItalicMT',

    'ArialRoundedMTBold',

    'ArialUnicodeMS',

    'Courier', 'Courier-BoldOblique', 'Courier-Oblique', 'Courier-Bold,',

    'CourierNewPS-BoldMT', 'CourierNewPS-ItalicMT', 'CourierNewPS-BoldItalicMT', 'CourierNewPSMT',

    'DBLCDTempBlack',

    'Georgia-Bold', 'Georgia', 'Georgia-BoldItalic', 'Georgia-Italic',

    'Helvetica-Oblique', 'Helvetica-BoldOblique', 'Helvetica', 'Helvetica-Bold',

    'HelveticaNeue', 'HelveticaNeue-Light', 'HelveticaNeue-UltraLight', 'HelveticaNeue-Bold',

    'HiraKakuProN-W3', 'HiraKakuProN-W6',

    'HiraKakuProN-W3', 'HiraKakuProN-W6',

    'MarkerFelt-Thin',

    'STHeitiJ-Medium', 'STHeitiJ-Light',

    'GeezaPro', 'GeezaPro-Bold',

    'STHeitiSC-Medium', 'STHeitiSC-Light',

    'STHeitiTC-Light', 'STHeitiTC-Medium',

    'TimesNewRomanPSMT', 'TimesNewRomanPS-BoldMT', 'TimesNewRomanPS-BoldItalicMT', 'TimesNewRomanPS-ItalicMT',

    'TrebuchetMS-Italic', 'TrebuchetMS', 'Trebuchet-BoldItalic', 'TrebuchetMS-Bold',

    'Verdana-Bold', 'Verdana-BoldItalic', 'Verdana', 'Verdana-Italic',

    'Zapfino' );

  // Equivalent to [UIFont fontWithName:@"Arial-BoldMT" size:24]
  // UIFont * Font = [UIFont fontWithFamilyName: @" Arial " traits: GSBoldFontMask size: 24];

  // ------------------------------------------------------------------------------
function GetDeviceFontName( FontName: TDPFIOSFontList ): string;
begin
  result := TDPFIOSFontStrList[FontName];
end;

// ------------------------------------------------------------------------------
{ TDPFFont }
// ------------------------------------------------------------------------------
procedure TDPFFont.Assign(Source: TPersistent);
  procedure AssignFromDPFFont(Src: TDPFFont);
  begin
    FontName := Src.FontName;
    FontSize := Src.FontSize;
    BoldSystemFontSize := Src.BoldSystemFontSize;
    OnChanged := Src.OnChanged;
  end;
begin
  if Source is TDPFFont then
    AssignFromDPFFont(TDPFFont(Source))
  else
    inherited;
end;

constructor TDPFFont.create;
begin
  inherited create;

  FFontName           := ios_Verdana;
  FFontSize           := ios_systemFontSize;
  FBoldSystemFontSize := false;
end;

// ------------------------------------------------------------------------------
destructor TDPFFont.Destroy;
begin
  inherited;
end;

// ------------------------------------------------------------------------------
{$IFDEF IOS}

function GetFontsList: TStringList;
var
  NS: NSArray;
  I : Integer;
begin
  NS     := TUIFont.OCClass.familyNames;
  Result := TStringList.Create;

  for I := 0 to NS.count - 1 do
    Result.Add( UTF8ToString( TNSString.Wrap( NS.objectAtIndex( i ) ).UTF8String ) );
  Result.Sort;
end;

// ------------------------------------------------------------------------------
function TDPFFont.GetFUIFont: UIFont;
begin
{$IFDEF IOS}
  if not Assigned( FUIFont ) then
  begin
    FUIFont := TUIFont.Wrap( TUIFont.OCClass.fontWithName( NSSTR( TDPFIOSFontStrList[FFontName] ), FFontSize ) );
  end;
{$ENDIF}
  Result := FUIFont;
end;
{$ENDIF}

// ------------------------------------------------------------------------------
procedure TDPFFont.SetBoldSystemFontSize( const Value: Boolean );
begin
{$IFDEF IOS}
  if FBoldSystemFontSize <> Value then
  begin
    FUIFont := TUIFont.Wrap( TUIFont.OCClass.boldSystemFontOfSize( FFontSize ) );
  end;
{$ENDIF}
  FBoldSystemFontSize := Value;
  if Assigned( FOnChanged ) then
    FOnChanged( Self );
end;

// ------------------------------------------------------------------------------
procedure TDPFFont.SetFontName( const Value: TDPFIOSFontList );
begin
{$IFDEF IOS}
  if FFontName <> Value then
  begin
    FUIFont := TUIFont.Wrap( TUIFont.OCClass.fontWithName( NSSTR( TDPFIOSFontStrList[Value] ), FFontSize ) );
  end;
{$ENDIF}
  if Assigned( FOnChanged ) then
    FOnChanged( Self );
  FFontName := Value;
end;

// ------------------------------------------------------------------------------
procedure TDPFFont.SetFontSize( const Value: integer );
begin
{$IFDEF IOS}
  if FFontSize <> Value then
  begin
    { if FUIFont <> nil then
      FUIFont.release; }
    if FBoldSystemFontSize then
      FUIFont := TUIFont.Wrap( TUIFont.OCClass.boldSystemFontOfSize( Value ) )
    else
      FUIFont := TUIFont.Wrap( TUIFont.OCClass.fontWithName( NSSTR( TDPFIOSFontStrList[FFontName] ), Value ) );
  end;
{$ENDIF}
  if Assigned( FOnChanged ) then
    FOnChanged( Self );
  FFontSize := Value;
end;

// ------------------------------------------------------------------------------
end.
