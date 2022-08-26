// ------------------------------------------------------------------------------
// DPF.iOS.UIDateTimeComboBox Component
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
unit DPF.iOS.UIDateTimeComboBox;

interface

{$I DPF.iOS.Defs.inc}

uses
  System.SysUtils,
  System.Classes,
  System.Types,
  System.UITypes,
  System.MaskUtils,

  DPF.iOS.BaseControl,
  DPF.iOS.UITextField,
  DPF.iOS.UIDatePicker,
{$IFDEF IOS}
  Macapi.ObjectiveC,
  Macapi.ObjCRuntime,
  IOSapi.CocoaTypes,
  IOSapi.Foundation,
  iOSapi.QuartzCore,
  IOSapi.Uikit,
  IOSapi.CoreGraphics,
  IOSapi.CoreImage,
  FMX.Platform.iOS,
  DPF.iOS.Common,
{$ENDIF}
  System.TypInfo;

type

  TDPFDateTimeComboBox = class;

{$IFDEF IOS}

  // ------------------------------------------------------------------------------
  IDPFButtonDelegate = interface( IObjectiveC )
    ['{F3F963F0-5BE1-4250-AFDA-220D0EAE58DB}']
    procedure clickedButton( Sender: UIBarButtonItem ); cdecl;
  end;

  // ------------------------------------------------------------------------------
  TDPFDateTimeCloseButtonDelegate = class( TOCLocal, IDPFButtonDelegate )
  private
    FDPFComboBox: TDPFDateTimeComboBox;
  public
    constructor Create( ADPFComboBox: TDPFDateTimeComboBox );
    procedure clickedButton( Sender: UIBarButtonItem ); cdecl;
  end;
{$ENDIF}

  TDPFOnCloseComboBox = procedure( sender: TObject ) of object;

  [ComponentPlatformsAttribute( PidWin32 or pidiOSDevice32 or pidiOSDevice64 or PidiOSSimulator )]
  TDPFDateTimeComboBox = class( TDPFTextField )
  private
    FPickerCloseTitle    : string;
    FPickerToolbarVisible: boolean;
    FPickerMode          : TDPFDatePickerMode;
    FLocaleType          : TDPFLocaleType;
    FCalendarType        : TDPFCalendarType;
    FTimeZoneType        : TDPFTimeZoneType;
    FMaximumDate         : TDateTime;
    FMinimumDate         : TDateTime;
    FOnCloseComboBox     : TDPFOnCloseComboBox;
    procedure SetPickerMode( const Value: TDPFDatePickerMode );
    procedure SetCalendarType( const Value: TDPFCalendarType );
    procedure SetLocaleType( const Value: TDPFLocaleType );
    procedure SetTimeZoneType( const Value: TDPFTimeZoneType );
    procedure SetMaximumDate( const Value: TDateTime );
    procedure SetMinimumDate( const Value: TDateTime );

  protected
    FDPFDatePicker: TDPFDatePicker;
{$IFDEF IOS}
    FButtonDelegate: TDPFDateTimeCloseButtonDelegate;
    toolbar        : UIToolbar;
{$ENDIF}
    procedure Resize; override;
    procedure Move; override;
    procedure OnPickerChanged( Sender: TObject );
  public
    property Picker: TDPFDatePicker read FDPFDatePicker;
{$IFDEF IOS}
    procedure Loaded; override;
{$ELSE}
    procedure Paint; override;
{$ENDIF}
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;
    procedure Clear( ResetDateTime: boolean = true );
  published
    property PickerCloseTitle    : string read FPickerCloseTitle write FPickerCloseTitle;
    property PickerToolbarVisible: boolean read FPickerToolbarVisible write FPickerToolbarVisible default false;
    property PickerMode          : TDPFDatePickerMode read FPickerMode write SetPickerMode default TDPFDatePickerMode.pmDateAndTime;
    property TimeZoneType        : TDPFTimeZoneType read FTimeZoneType write SetTimeZoneType default TDPFTimeZoneType.tztDefault;
    property LocaleType          : TDPFLocaleType read FLocaleType write SetLocaleType default TDPFLocaleType.en_;
    property CalendarType        : TDPFCalendarType read FCalendarType write SetCalendarType default TDPFCalendarType.Gregorian;
    property MinimumDate         : TDateTime read FMinimumDate write SetMinimumDate;
    property MaximumDate         : TDateTime read FMaximumDate write SetMaximumDate;
    property OnCloseComboBox     : TDPFOnCloseComboBox read FOnCloseComboBox write FOnCloseComboBox;

    property UserInteraction;
    property ContentMode;
    property Alpha;
    property Visible;
    property Align;
    property Position;
    property Width;
    property Height;
  end;

implementation

// ------------------------------------------------------------------------------
{ TDPFDateTimeComboBox }

constructor TDPFDateTimeComboBox.Create( AOwner: TComponent );
begin
  inherited Create( AOwner );
  ControlCaption                  := 'DateTime ComboBox';
  FDPFDatePicker                  := TDPFDatePicker.Create( nil );
  FDPFDatePicker.AddThisToSubView := false;
  FDPFDatePicker.OnChange         := OnPickerChanged;
  FPickerCloseTitle               := 'Done';
  FPickerToolbarVisible           := false;
  FMinimumDate                    := 0;
  FMaximumDate                    := 0;

  FTimeZoneType := TDPFTimeZoneType.tztDefault;
  FLocaleType   := TDPFLocaleType.en_;
  FPickerMode   := TDPFDatePickerMode.pmDateAndTime;
  FCalendarType := TDPFCalendarType.Gregorian;

{$IFDEF IOS}
  FButtonDelegate := TDPFDateTimeCloseButtonDelegate.Create( Self );
{$ENDIF}
end;

// ------------------------------------------------------------------------------
destructor TDPFDateTimeComboBox.Destroy;
begin
  FDPFDatePicker.DisposeOf;
{$IFDEF IOS}
  toolbar.release;
  FButtonDelegate.DisposeOf;
{$ENDIF}
  inherited;
end;

// ------------------------------------------------------------------------------
{$IFDEF IOS}

procedure TDPFDateTimeComboBox.Loaded;
var
  doneButton, flexibleSpaceLeft: UIBarButtonItem;
  ItemsArr                     : NSMutableArray;
begin

  FDPFDatePicker.Loaded;

  if FPickerToolbarVisible then
  begin
    ItemsArr := TNSMutableArray.Create;

    toolbar := TUIToolbar.Wrap( TUIToolbar.Alloc.init );
    toolbar.setBarStyle( UIBarStyleBlackTranslucent );
    toolbar.sizeToFit;

    flexibleSpaceLeft := TUIBarButtonItem.Wrap( TUIBarButtonItem.Alloc.initWithBarButtonSystemItem( UIBarButtonSystemItemFlexibleSpace, nil, nil ) );
    doneButton        := TUIBarButtonItem.Wrap( TUIBarButtonItem.Alloc.initWithTitle( NSStr( FPickerCloseTitle ), UIBarButtonItemStyleDone, FButtonDelegate.GetObjectID, Sel_getUid( 'clickedButton:' ) ) );

    ItemsArr := TNSMutableArray.Create;
    ItemsArr.addObject( ( flexibleSpaceLeft as ILocalObject ).GetObjectID );
    ItemsArr.addObject( ( doneButton as ILocalObject ).GetObjectID );

    toolbar.setItems( ItemsArr );
    ItemsArr.release;
    UITextField( FUIControl ).setInputAccessoryView( toolbar );
  end;

  DPF.iOS.UITextField.UITextField( FUIControl ).setInputView( UIView( FDPFDatePicker.UIControl ) );

  AddSubView( Self, ParentControl );

  // ----------------------------
  // Important
  inherited;
end;
{$ENDIF}

// ------------------------------------------------------------------------------
procedure TDPFDateTimeComboBox.Resize;
begin
  inherited;
end;

// ------------------------------------------------------------------------------
procedure TDPFDateTimeComboBox.Clear( ResetDateTime: boolean = true );
begin
  if ResetDateTime then
    Picker.PickerDate := 0;
  Text                := '';
end;

// ------------------------------------------------------------------------------
procedure TDPFDateTimeComboBox.SetCalendarType( const Value: TDPFCalendarType );
begin
  FCalendarType               := Value;
  FDPFDatePicker.CalendarType := FCalendarType;
end;

// ------------------------------------------------------------------------------
procedure TDPFDateTimeComboBox.SetLocaleType( const Value: TDPFLocaleType );
begin
  FLocaleType               := Value;
  FDPFDatePicker.LocaleType := LocaleType;
end;

// ------------------------------------------------------------------------------
procedure TDPFDateTimeComboBox.SetMaximumDate( const Value: TDateTime );
begin
  FMaximumDate               := Value;
  FDPFDatePicker.MaximumDate := value;
end;

// ------------------------------------------------------------------------------
procedure TDPFDateTimeComboBox.SetMinimumDate( const Value: TDateTime );
begin
  FMinimumDate               := Value;
  FDPFDatePicker.MinimumDate := value;
end;

// ------------------------------------------------------------------------------
procedure TDPFDateTimeComboBox.SetPickerMode( const Value: TDPFDatePickerMode );
begin
  FPickerMode               := Value;
  FDPFDatePicker.PickerMode := value;
end;

// ------------------------------------------------------------------------------
procedure TDPFDateTimeComboBox.SetTimeZoneType( const Value: TDPFTimeZoneType );
begin
  FTimeZoneType               := Value;
  FDPFDatePicker.TimeZoneType := value;
end;

// ------------------------------------------------------------------------------
procedure TDPFDateTimeComboBox.Move;
begin
  Resize;
end;

// ------------------------------------------------------------------------------
procedure TDPFDateTimeComboBox.OnPickerChanged( Sender: TObject );
begin
  case FDPFDatePicker.PickerMode of
    pmTime:
      Text := TimeToStr( TDPFDatePicker( Sender ).PickerDate );
    pmDate:
      Text := DateToStr( TDPFDatePicker( Sender ).PickerDate );
    pmDateAndTime:
      Text := DateTimeToStr( TDPFDatePicker( Sender ).PickerDate );
    pmCountDownTimer:
      Text := FormatDateTime( 'hh:nn', TDPFDatePicker( Sender ).PickerDate );
  end;
end;

// ------------------------------------------------------------------------------
{$IFNDEF IOS}

procedure TDPFDateTimeComboBox.Paint;
begin
  inherited;
end;
{$ENDIF}
// ------------------------------------------------------------------------------
{ TDPFDateTimeCloseButtonDelegate }
{$IFDEF IOS}

procedure TDPFDateTimeCloseButtonDelegate.clickedButton( Sender: UIBarButtonItem );
begin
  UITextField( FDPFComboBox.UIControl ).resignFirstResponder;
  if Assigned( FDPFComboBox.FOnCloseComboBox ) then
    FDPFComboBox.FOnCloseComboBox( FDPFComboBox );
end;

// ------------------------------------------------------------------------------
constructor TDPFDateTimeCloseButtonDelegate.Create( ADPFComboBox: TDPFDateTimeComboBox );
begin
  inherited Create;
  FDPFComboBox := ADPFComboBox;
end;

{$ENDIF}

end.
