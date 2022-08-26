// ------------------------------------------------------------------------------
// DPF.Android.Widget Java Classes
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
unit DPF.Android.Widget;

interface

{$I DPF.ANDROID.Defs.inc}

uses
  System.SysUtils,
  System.Classes,
  System.Types,
  System.UITypes,

  System.TypInfo,
{$IFDEF ANDROID}
  Androidapi.JNI.Webkit,
  Androidapi.JNI.VideoView,
  Androidapi.JNI.App,
  Androidapi.JNI.Widget,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Os,
  Androidapi.Jni,
  Androidapi.JNIBridge,
  Androidapi.JNI.Embarcadero,
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.Util,
  Androidapi.JNI.Net,
  Androidapi.Log,
  FMX.Helpers.Android,
  FMX.Platform.Android,
{$ELSE}
  DPF.ANDROID.DesignTime,
{$ENDIF}
{$IFDEF DELPHIXE5} FMX.Graphics, {$ENDIF}FMX.Types;

{$IFDEF ANDROID}

type
  JDPFTextView                            = interface;
  JRelativeLayout                         = interface;
  JRelativeLayout_LayoutParams            = interface;
  JAnalogClock                            = interface;
  JCompoundButton                         = interface;
  JCompoundButton_OnCheckedChangeListener = interface;
  JCheckBox                               = interface;
  JCheckedTextView                        = interface;
  JChronometer                            = interface;
  JDatePicker                             = interface;
  JDatePickerDialog                       = interface;
  JDatePicker_OnDateChangedListener       = interface;
  JBaseAdapter                            = interface;
  JSimpleAdapter                          = interface;
  JArrayAdapter                           = interface;
  JToast                                  = interface;
  JProgressBar                            = interface;
  JProgressDialog                         = interface;
  JTimePicker                             = interface;
  JTimePickerDialog                       = interface;
  JTimePickerDialog_OnTimeSetListener     = interface;
  JNumberPicker                           = interface;
  JRadioButton                            = interface;
  JRadioGroup                             = interface;
  JRadioGroup_OnCheckedChangeListener     = interface;
  JTableLayout                            = interface;
  JTableLayout_LayoutParams               = interface;
  JTableRow                               = interface;
  JTableRow_LayoutParams                  = interface;
  JAbsSeekBar                             = interface;
  JSeekBar                                = interface;
  JSeekBar_OnSeekBarChangeListener        = interface;
  JAbsSpinner                             = interface;
  JSpinner                                = interface;
  JTabHost                                = interface;
  JTabHost_TabSpec                        = interface;
  JTabWidget                              = interface;
  JTabHost_OnTabChangeListener            = interface;
  JLocalActivityManager                   = interface;
  JActivityGroup                          = interface;
  JAnimation                              = interface;
  JViewAnimator                           = interface;
  JInterpolator                           = interface;
  JAnimation_AnimationListener            = interface;
  JTransformation                         = interface;
  JViewSwitcher                           = interface;
  JViewSwitcher_ViewFactory               = interface;
  JTextSwitcher                           = interface;
  JAnimationUtils                         = interface;
  JLayoutAnimationController              = interface;
  JBaseMovementMethod                     = interface;
  JScrollingMovementMethod                = interface;
  JMovementMethod                         = interface;
  JTextClock                              = interface;
  JDPFNativeLayout                        = interface;

  JDPFOnWebViewListener  = interface;
  JDPFWebView            = interface;
  JDPFWebClient          = interface;
  JDPFOnListViewListener = interface;
  JDPFListView           = interface;
  JDPFHTTP               = interface;
  JDPFVideoView          = interface;
  JDPFAnimation = interface;

  // ----------------------------------------------------------------------------
  // JDPFTextView Class
  // ----------------------------------------------------------------------------
  JDPFTextViewClass = interface( JScrollViewClass )
    ['{8C77321C-7382-4B24-AC8F-F7E773F232DE}']

    function init( context: JContext ): JDPFTextView; cdecl;
  end;

  [JavaSignature( 'com/DPFaragir/DPFTextView' )]
  JDPFTextView = interface( JScrollView )
    ['{C3965C82-6041-4C29-8E28-E77F30372AA9}']

    procedure setTextColor( color: Integer ); cdecl;
    procedure setText( text: JCharSequence ); cdecl;
    procedure setAllCaps( allCaps: Boolean ); cdecl;
    procedure setLines( lines: Integer ); cdecl;
    procedure setGravity( gravity: Integer ); cdecl;
    procedure setTextSize( size: Single ); cdecl; overload;
    procedure setTextSize( unit_: Integer; size: Single ); cdecl; overload;
  end;

  TJDPFTextView = class( TJavaGenericImport<JDPFTextViewClass, JDPFTextView> )
  end;

  // ----------------------------------------------------------------------------
  // JRelativeLayout_LayoutParams Class
  // ----------------------------------------------------------------------------
  JRelativeLayout_LayoutParamsClass = interface( JViewGroup_MarginLayoutParamsClass )
    ['{1CEB8E0E-FA54-4E84-8E8A-DF80BFB80D4A}']
    { Methods }
    function init( c: JContext; attrs: JAttributeSet ): JRelativeLayout_LayoutParams; cdecl; overload;
    function init( width: Integer; height: Integer ): JRelativeLayout_LayoutParams; cdecl; overload;
    function init( width: Integer; height: Integer; weight: Single ): JRelativeLayout_LayoutParams; cdecl; overload;
    function init( p: JViewGroup_LayoutParams ): JRelativeLayout_LayoutParams; cdecl; overload;
    function init( source: JViewGroup_MarginLayoutParams ): JRelativeLayout_LayoutParams; cdecl; overload;
  end;

  [JavaSignature( 'android/widget/RelativeLayout$LayoutParams' )]
  JRelativeLayout_LayoutParams = interface( JViewGroup_MarginLayoutParams )
    ['{5E258D8D-E8D6-4627-AA29-AAB278F14B6A}']
    { Property Methods }
    // function _Getgravity: Integer;
    // procedure _Setgravity( Value: Integer );
    // function _Getweight: Single;
    // procedure _Setweight( Value: Single );
    { Methods }
    function debug( output: JString ): JString; cdecl;
    // function getRules: TJavaArray<Char>; cdecl;
    procedure addRule( verb: integer ); cdecl; overload;
    procedure addRule( verb: integer; anchor: integer ); cdecl; overload;
    procedure removeRule( verb: integer ); cdecl;
    { Properties }
    // property gravity: Integer read _Getgravity write _Setgravity;
    // property weight: Single read _Getweight write _Setweight;
  end;

  TJRelativeLayout_LayoutParams = class( TJavaGenericImport<JRelativeLayout_LayoutParamsClass, JRelativeLayout_LayoutParams> )
  end;

  // ----------------------------------------------------------------------------
  // JRelativeLayout Class
  // ----------------------------------------------------------------------------
  JRelativeLayoutClass = interface( JViewGroupClass )
    ['{12E2A045-862F-4490-A562-43F3F8E4969D}']

    function init( context: JContext ): JRelativeLayout; cdecl; overload;
    function init( context: JContext; attrs: JAttributeSet ): JRelativeLayout; cdecl; overload;
    function init( context: JContext; attrs: JAttributeSet; defStyle: Integer ): JRelativeLayout; cdecl; overload;
  end;

  [JavaSignature( 'android/widget/RelativeLayout' )]
  JRelativeLayout = interface( JViewGroup )
    ['{114F80EA-C637-4D61-B751-1B7A10BAD74B}']

    function shouldDelayChildPressedState: boolean; cdecl;
    function getBaseline: integer; cdecl;
    function getGravity: integer; cdecl;
    procedure setGravity( gravity: integer ); cdecl;
    procedure requestLayout; cdecl;
    procedure setHorizontalGravity( horizontalGravity: integer ); cdecl;
    procedure setIgnoreGravity( viewId: integer ); cdecl;
    procedure setVerticalGravity( verticalGravity: integer ); cdecl;
  end;

  TJRelativeLayout = class( TJavaGenericImport<JRelativeLayoutClass, JRelativeLayout> )
  end;

  // ----------------------------------------------------------------------------
  // JTableLayout_LayoutParams Class
  // ----------------------------------------------------------------------------
  JTableLayout_LayoutParamsClass = interface( JLinearLayout_LayoutParamsClass )
    ['{AE2DB468-6CA3-4517-BDB2-6DDD70E00A0A}']

    function init( context: JContext; attrs: JAttributeSet ): JTableLayout_LayoutParams; cdecl; overload;
    function init( width: Integer; height: integer ): JTableLayout_LayoutParams; cdecl; overload;
    function init( w: integer; h: Integer; initWeight: single ): JTableLayout_LayoutParams; cdecl; overload;
    function init: JTableLayout_LayoutParams; cdecl; overload;
    function init( p: JViewGroup_LayoutParams ): JTableLayout_LayoutParams; cdecl; overload;
    function init( source: JViewGroup_MarginLayoutParams ): JTableLayout_LayoutParams; cdecl; overload;
  end;

  [JavaSignature( 'android/widget/TableLayout$LayoutParams' )]
  JTableLayout_LayoutParams = interface( JLinearLayout_LayoutParams )
    ['{72217F72-82FC-490A-9CB3-46AE9C7EBE60}']
  end;

  TJTableLayout_LayoutParams = class( TJavaGenericImport<JTableLayout_LayoutParamsClass, JTableLayout_LayoutParams> )
  end;

  // ----------------------------------------------------------------------------
  // JTableLayout Class
  // ----------------------------------------------------------------------------
  JTableLayoutClass = interface( JLinearLayoutClass )
    ['{42559408-FE09-420E-97F4-BD47DDA3D938}']

    function init( context: JContext ): JTableLayout; cdecl; overload;
    function init( context: JContext; attrs: JAttributeSet ): JTableLayout; cdecl; overload;
  end;

  [JavaSignature( 'android/widget/TableLayout' )]
  JTableLayout = interface( JLinearLayout )
    ['{CBBC2135-E096-4C73-BF8C-7BD99B1768BE}']
  end;

  TJTableLayout = class( TJavaGenericImport<JTableLayoutClass, JTableLayout> )
  end;

  // ----------------------------------------------------------------------------
  // JTableRow Class
  // ----------------------------------------------------------------------------
  JTableRowClass = interface( JLinearLayoutClass )
    ['{4886C1C1-FBED-4877-8D3B-8EC713FF6F5E}']

    function init( context: JContext ): JTableLayout; cdecl; overload;
    function init( context: JContext; attrs: JAttributeSet ): JTableLayout; cdecl; overload;
  end;

  [JavaSignature( 'android/widget/TableRow' )]
  JTableRow = interface( JLinearLayout )
    ['{D288A5CB-3171-4E65-BEC4-FDA44DFE3624}']

    function getVirtualChildCount: Integer; cdecl;
    function getVirtualChildAt( i: integer ): JView; cdecl;
  end;

  TJTableRow = class( TJavaGenericImport<JTableRowClass, JTableRow> )
  end;

  // ----------------------------------------------------------------------------
  // JTableRow_LayoutParams Class
  // ----------------------------------------------------------------------------
  JTableRow_LayoutParamsClass = interface( JLinearLayout_LayoutParamsClass )
    ['{ABB169B4-9228-413D-88A0-2256F8119A0F}']

    function init( context: JContext; attrs: JAttributeSet ): JTableLayout_LayoutParams; cdecl; overload;
    function init( width: Integer; height: integer ): JTableLayout_LayoutParams; cdecl; overload;
    function init( w: integer; h: Integer; initWeight: single ): JTableLayout_LayoutParams; cdecl; overload;
    function init: JTableLayout_LayoutParams; cdecl; overload;
    function init( p: JViewGroup_LayoutParams ): JTableLayout_LayoutParams; cdecl; overload;
    function init( source: JViewGroup_MarginLayoutParams ): JTableLayout_LayoutParams; cdecl; overload;
  end;

  [JavaSignature( 'android/widget/TableRow$LayoutParams' )]
  JTableRow_LayoutParams = interface( JLinearLayout_LayoutParams )
    ['{029A54A8-0D34-4E24-BD28-FAB809A33B58}']
  end;

  TJTableRow_LayoutParams = class( TJavaGenericImport<JTableRow_LayoutParamsClass, JTableRow_LayoutParams> )
  end;

  // ----------------------------------------------------------------------------
  // JAnalogClock Class
  // ----------------------------------------------------------------------------
  JAnalogClockClass = interface( JViewClass )
    ['{F42F3A02-0749-4178-A81F-07C46ABBC470}']
    function init( context: JContext ): JAnalogClock; cdecl; overload;
    function init( context: JContext; attrs: JAttributeSet ): JAnalogClock; cdecl; overload;
    function init( context: JContext; attrs: JAttributeSet; defStyle: Integer ): JAnalogClock; cdecl; overload;
  end;

  [JavaSignature( 'android/widget/AnalogClock' )]
  JAnalogClock = interface( JView )
    ['{32EBA2F4-29CE-4EA6-8A22-8BE78220C359}']
  end;

  TJAnalogClock = class( TJavaGenericImport<JAnalogClockClass, JAnalogClock> )
  end;

  // ----------------------------------------------------------------------------
  // JCompoundButton Class
  // ----------------------------------------------------------------------------
  JCompoundButtonClass = interface( JButtonClass )
    ['{E0D6B6C3-56FF-41BE-A951-E6D6203A5D6A}']
    function init( context: JContext ): JCompoundButton; cdecl; overload;
    function init( context: JContext; attrs: JAttributeSet ): JCompoundButton; cdecl; overload;
    function init( context: JContext; attrs: JAttributeSet; defStyle: Integer ): JCompoundButton; cdecl; overload;
  end;

  [JavaSignature( 'android/widget/CompoundButton' )]
  JCompoundButton = interface( JButton )
    ['{D0ED6102-5C3A-460A-A096-BBD597F92805}']

    function isChecked: Boolean; cdecl;
    procedure performClick; cdecl;
    procedure setButtonDrawable( d: JDrawable ); cdecl; overload;
    procedure setButtonDrawable( resid: Integer ); cdecl; overload;
    procedure setChecked( checked: boolean ); cdecl;
    procedure setOnCheckedChangeListener( listener: JCompoundButton_OnCheckedChangeListener );
    procedure toggle; cdecl;
    procedure drawableStateChanged; cdecl;
    function verifyDrawable( who: JDrawable ): Boolean;
    procedure onDraw( canvas: JCanvas ); cdecl;
    function getCompoundPaddingRight: integer; cdecl;
    function getCompoundPaddingLeft: integer; cdecl;
  end;

  TJCompoundButton = class( TJavaGenericImport<JCompoundButtonClass, JCompoundButton> )
  end;

  // ----------------------------------------------------------------------------
  // JCompoundButton_OnCheckedChangeListener Class
  // ----------------------------------------------------------------------------
  JCompoundButton_OnCheckedChangeListenerClass = interface( IJavaClass )
    ['{92CBF77F-D189-4F39-ADDB-1CB248741CDC}']
  end;

  [JavaSignature( 'android/widget/CompoundButton$OnCheckedChangeListener' )]
  JCompoundButton_OnCheckedChangeListener = interface( IJavaInstance )
    ['{0F89C02F-4452-4C93-A248-3FAAF868700A}']
    procedure onCheckedChanged( buttonView: JCompoundButton; isChecked: boolean ); cdecl;
  end;

  TJCompoundButton_OnCheckedChangeListener = class( TJavaGenericImport<JCompoundButton_OnCheckedChangeListenerClass, JCompoundButton_OnCheckedChangeListener> )
  end;

  // ----------------------------------------------------------------------------
  // JCheckBox Class
  // ----------------------------------------------------------------------------
  JCheckBoxClass = interface( JCompoundButtonClass )
    ['{5BBA49A5-35EE-40DD-93A7-71E3FE707DC4}']
    function init( context: JContext ): JCheckBox; cdecl; overload;
    function init( context: JContext; attrs: JAttributeSet ): JCheckBox; cdecl; overload;
    function init( context: JContext; attrs: JAttributeSet; defStyle: Integer ): JCheckBox; cdecl; overload;
  end;

  [JavaSignature( 'android/widget/CheckBox' )]
  JCheckBox = interface( JCompoundButton )
    ['{87A3CD88-98DF-4FC3-B5CE-B85F0E728B69}']

    function isChecked: boolean; cdecl;
    procedure setChecked( value: boolean ); cdecl;
    procedure toggle; cdecl;
    // procedure setOnCheckedChangeListener( listener: JCompoundButton_OnCheckedChangeListener ); cdecl;

  end;

  TJCheckBox = class( TJavaGenericImport<JCheckBoxClass, JCheckBox> )
  end;

  // ----------------------------------------------------------------------------
  // JRadioButton Class
  // ----------------------------------------------------------------------------
  JRadioButtonClass = interface( JCompoundButtonClass )
    ['{D1173416-881E-4679-B10B-D16EA18E36FA}']
    function init( context: JContext ): JRadioButton; cdecl; overload;
    function init( context: JContext; attrs: JAttributeSet ): JRadioButton; cdecl; overload;
    function init( context: JContext; attrs: JAttributeSet; defStyle: Integer ): JRadioButton; cdecl; overload;
  end;

  [JavaSignature( 'android/widget/RadioButton' )]
  JRadioButton = interface( JCompoundButton )
    ['{148A48DB-659F-48B2-ACC3-DE91C7C387C8}']

    procedure toggle; cdecl;
  end;

  TJRadioButton = class( TJavaGenericImport<JRadioButtonClass, JRadioButton> )
  end;

  // ----------------------------------------------------------------------------
  // JRadioGroup_OnCheckedChangeListener Class
  // ----------------------------------------------------------------------------
  JRadioGroup_OnCheckedChangeListenerClass = interface( IJavaClass )
    ['{238E659C-CC57-4E01-9FEB-B917D4EB6CEA}']
  end;

  [JavaSignature( 'android/widget/RadioGroup$OnCheckedChangeListener' )]
  JRadioGroup_OnCheckedChangeListener = interface( IJavaInstance )
    ['{B52180A2-DED1-4C0F-8E35-818967BA3A65}']

    procedure onCheckedChanged( group: JRadioGroup; checkedId: integer ); cdecl;
  end;

  TJRadioGroup_OnCheckedChangeListener = class( TJavaGenericImport<JRadioGroup_OnCheckedChangeListenerClass, JRadioGroup_OnCheckedChangeListener> )
  end;

  // ----------------------------------------------------------------------------
  // JRadioGroup Class
  // ----------------------------------------------------------------------------
  JRadioGroupClass = interface( JLinearLayoutClass )
    ['{3FC7A12B-B6B3-4FC1-84BD-CB8D8295ABC9}']
    function init( context: JContext ): JRadioGroup; cdecl; overload;
    function init( context: JContext; attrs: JAttributeSet ): JRadioGroup; cdecl; overload;
  end;

  [JavaSignature( 'android/widget/RadioGroup' )]
  JRadioGroup = interface( JLinearLayout )
    ['{BE8E78A7-C781-4A17-99BE-943598E1C86C}']

    // procedure addView( child: JView; index: Integer; params: JViewGroup_LayoutParams ); cdecl;
    procedure check( id: integer ); cdecl;
    procedure clearCheck; cdecl;
    function getCheckedRadioButtonId: Integer; cdecl;
    procedure setOnCheckedChangeListener( listener: JRadioGroup_OnCheckedChangeListener ); cdecl;
  end;

  TJRadioGroup = class( TJavaGenericImport<JRadioGroupClass, JRadioGroup> )
  end;

  // ----------------------------------------------------------------------------
  // JCheckedTextView Class
  // ----------------------------------------------------------------------------
  JCheckedTextViewClass = interface( JTextViewClass )
    ['{62CC5340-D3E1-4ACF-8027-C8504A198403}']
    function init( context: JContext ): JCheckedTextView; cdecl; overload;
    function init( context: JContext; attrs: JAttributeSet ): JCheckedTextView; cdecl; overload;
    function init( context: JContext; attrs: JAttributeSet; defStyle: Integer ): JCheckedTextView; cdecl; overload;
  end;

  [JavaSignature( 'android/widget/CheckedTextView' )]
  JCheckedTextView = interface( JTextView )
    ['{AD046037-00B3-4DCF-941F-45782DBA5395}']

    function isChecked: boolean; cdecl;
    procedure setChecked( value: boolean ); cdecl;
    procedure toggle; cdecl;
    procedure setCheckMarkDrawable( resid: integer ); cdecl; overload;
    procedure setCheckMarkDrawable( d: JDrawable ); cdecl; overload;
    function getCheckMarkDrawable: JDrawable; cdecl;
    procedure setOnCheckedChangeListener( listener: JCompoundButton_OnCheckedChangeListener ); cdecl;

  end;

  TJCheckedTextView = class( TJavaGenericImport<JCheckedTextViewClass, JCheckedTextView> )
  end;

  // ----------------------------------------------------------------------------
  // JChronometer_OnChronometerTickListener Class
  // ----------------------------------------------------------------------------
  JChronometer_OnChronometerTickListenerClass = interface( IJavaClass )
    ['{B7A870F1-34F9-4C17-9F7D-F975EA4F486E}']
  end;

  [JavaSignature( 'android/widget/Chronometer$OnChronometerTickListener' )]
  JChronometer_OnChronometerTickListener = interface( IJavaInstance )
    ['{1D4A45B7-FE65-4111-A098-32CAE94A0CD6}']
    procedure onChronometerTick( chronometer: JChronometer ); cdecl;
  end;

  TJChronometer_OnChronometerTickListener = class( TJavaGenericImport<JChronometer_OnChronometerTickListenerClass, JChronometer_OnChronometerTickListener> )
  end;

  // ----------------------------------------------------------------------------
  // JChronometer Class
  // ----------------------------------------------------------------------------
  JChronometerClass = interface( JTextViewClass )
    ['{47A12FF0-773C-42BC-A6B8-F112D0350405}']
    function init( context: JContext ): JChronometer; cdecl; overload;
    function init( context: JContext; attrs: JAttributeSet ): JChronometer; cdecl; overload;
    function init( context: JContext; attrs: JAttributeSet; defStyle: Integer ): JChronometer; cdecl; overload;
  end;

  [JavaSignature( 'android/widget/Chronometer' )]
  JChronometer = interface( JTextView )
    ['{417B5EFF-C099-40E1-B5D8-33AA607F6DD0}']

    procedure start; cdecl;
    procedure stop; cdecl;
    procedure setFormat( format: JString ); cdecl;
    procedure setBase( base: longword ); cdecl;
    function getFormat: JString; cdecl;
    function getBase: Longword; cdecl;
    procedure setOnChronometerTickListener( listener: JChronometer_OnChronometerTickListener ); cdecl;
  end;

  TJChronometer = class( TJavaGenericImport<JChronometerClass, JChronometer> )
  end;

  // ----------------------------------------------------------------------------
  // JChronometer_OnChronometerTickListener Class
  // ----------------------------------------------------------------------------
  JNumberPicker_OnValueChangeListenerClass = interface( IJavaClass )
    ['{88EBABB3-FA88-499C-A572-F5DC8547E8C9}']
  end;

  [JavaSignature( 'android/widget/NumberPicker$OnValueChangeListener' )]
  JNumberPicker_OnValueChangeListener = interface( IJavaInstance )
    ['{AB611699-1A94-479C-87B9-04531278F4D0}']
    procedure onValueChange( picker: JNumberPicker; oldVal: Integer; newVal: Integer ); cdecl;
  end;

  TJNumberPicker_OnValueChangeListener = class( TJavaGenericImport<JNumberPicker_OnValueChangeListenerClass, JNumberPicker_OnValueChangeListener> )
  end;

  // ----------------------------------------------------------------------------
  // JNumberPicker Class
  // ----------------------------------------------------------------------------
  JNumberPickerClass = interface( JLinearLayoutClass )
    ['{173BA38B-BA1A-4DA3-8252-3E65B5892BD6}']
    function init( context: JContext ): JNumberPicker; cdecl; overload;
    function init( context: JContext; attrs: JAttributeSet ): JNumberPicker; cdecl; overload;
    function init( context: JContext; attrs: JAttributeSet; defStyle: Integer ): JNumberPicker; cdecl; overload;
  end;

  [JavaSignature( 'android/widget/NumberPicker' )]
  JNumberPicker = interface( JLinearLayout )
    ['{58831FB4-F9E6-4950-89C5-52B5098B2CC5}']

    procedure computeScroll; cdecl;
    function getDisplayedValues: TJavaArray<JString>; cdecl;

    function getMaxValue: Integer; cdecl;
    function getMinValue: Integer; cdecl;
    function getSolidColor: Integer; cdecl;
    function getValue: Integer; cdecl;
    function getWrapSelectorWheel: boolean; cdecl;
    // procedure onInitializeAccessibilityEvent(event: JAccessibilityEvent ); cdecl ;
    function onInterceptTouchEvent( event: JMotionEvent ): boolean; cdecl;
    function onTouchEvent( event: JMotionEvent ): boolean; cdecl;
    procedure scrollBy( x: integer; y: integer ); cdecl;
    procedure setDisplayedValues( displayedValues: TJavaObjectArray<JString> ); cdecl;
    procedure setEnabled( enabled: boolean ); cdecl;
    // procedure setFormatter(formatter: JNumberPicker_Formatter );cdecl;
    procedure setMaxValue( maxValue: integer ); cdecl;
    procedure setMinValue( minValue: integer ); cdecl;
    procedure setOnLongPressUpdateInterval( intervalMillis: int64 ); cdecl;
    // procedure setOnScrollListener(onScrollListener: JNumberPicker_OnScrollListener );cdecl;
    procedure setOnValueChangedListener( onValueChangedListener: JNumberPicker_OnValueChangeListener ); cdecl;
    procedure setValue( value: integer ); cdecl;
    procedure setWrapSelectorWheel( wrapSelectorWheel: boolean ); cdecl;

  end;

  TJNumberPicker = class( TJavaGenericImport<JNumberPickerClass, JNumberPicker> )
  end;

  // ----------------------------------------------------------------------------
  // JDatePicker Class
  // ----------------------------------------------------------------------------
  JDatePickerClass = interface( JFrameLayoutClass )
    ['{0E9429B3-1E55-4495-A930-83BC9613BCF1}']
    function init( context: JContext ): JDatePicker; cdecl; overload;
    function init( context: JContext; attrs: JAttributeSet ): JDatePicker; cdecl; overload;
    function init( context: JContext; attrs: JAttributeSet; defStyle: Integer ): JDatePicker; cdecl; overload;
  end;

  [JavaSignature( 'android/widget/DatePicker' )]
  JDatePicker = interface( JFrameLayout )
    ['{35C45AB3-7D4B-4BE9-A1A7-E07CD87511E2}']

    // procedure init( year: Integer; monthOfYear: Integer; dayOfMonth: Integer; onDateChangedListener: JDatePicker_OnDateChangedListener ); cdecl; overload;
    function getDatePicker: JDatePicker; cdecl;
    function getCalendarViewShown: boolean; cdecl;
    function isEnabled: boolean; cdecl;
    function getSpinnersShown: boolean; cdecl;
    function getDayOfMonth: integer; cdecl;
    function getMaxDate: int64; cdecl;
    function getMinDate: int64; cdecl;
    function getMonth: integer; cdecl;
    function getYear: integer; cdecl;
    procedure setMaxDate( maxDate: int64 ); cdecl;
    procedure setMinDate( maxDate: int64 ); cdecl;
    procedure setSpinnersShown( shown: Boolean ); cdecl;
    procedure updateDate( year: Integer; month: Integer; dayOfMonth: Integer ); cdecl;
  end;

  TJDatePicker = class( TJavaGenericImport<JDatePickerClass, JDatePicker> )
  end;

  // ----------------------------------------------------------------------------
  // JDatePickerDialog_OnDateSetListener Class
  // ----------------------------------------------------------------------------
  JDatePickerDialog_OnDateSetListenerClass = interface( IJavaClass )
    ['{32FBDCEF-084D-4EE1-8FEE-87B0F58004EB}']
  end;

  [JavaSignature( 'android/app/DatePickerDialog$OnDateSetListener' )]
  JDatePickerDialog_OnDateSetListener = interface( IJavaInstance )
    ['{95C6B011-58D6-4A6E-831C-9FA490F09CDC}']
    procedure onDateSet( view: JDatePicker; year: integer; monthOfYear: Integer; dayOfMonth: integer ); cdecl;
  end;

  TJDatePickerDialog_OnDateSetListener = class( TJavaGenericImport<JDatePickerDialog_OnDateSetListenerClass, JDatePickerDialog_OnDateSetListener> )
  end;

  // ----------------------------------------------------------------------------
  // JDatePicker_OnDateChangedListener Class
  // ----------------------------------------------------------------------------
  JDatePicker_OnDateChangedListenerClass = interface( IJavaClass )
    ['{4501BE5F-462A-4AE2-B900-E2398133B1B4}']
  end;

  [JavaSignature( 'android/widget/DatePicker$OnDateChangedListener' )]
  JDatePicker_OnDateChangedListener = interface( IJavaInstance )
    ['{01F085CF-0338-4A99-8841-B3D1FDBC6350}']
    procedure onDateChanged( view: JDatePicker; year: Integer; monthOfYear: Integer; dayOfMonth: Integer ); cdecl;
  end;

  TJDatePicker_OnDateChangedListener = class( TJavaGenericImport<JDatePicker_OnDateChangedListenerClass, JDatePicker_OnDateChangedListener> )
  end;

  // ----------------------------------------------------------------------------
  // JDatePickerDialog Class
  // ----------------------------------------------------------------------------
  JDatePickerDialogClass = interface( JAlertDialogClass )
    ['{A190FD19-36E1-4E77-8FDC-F946073B510B}']
    function init( context: JContext; callBack: JDatePickerDialog_OnDateSetListener; year: Integer; monthOfYear: Integer; dayOfMonth: Integer ): JDatePickerDialog; cdecl; overload;
    function init( context: JContext; theme: integer; callBack: JDatePickerDialog_OnDateSetListener; year: Integer; monthOfYear: Integer; dayOfMonth: Integer ): JDatePickerDialog; cdecl; overload;
  end;

  [JavaSignature( 'android/app/DatePickerDialog' )]
  JDatePickerDialog = interface( JAlertDialog )
    ['{0ABF386C-3F97-4C5B-A861-FEFEC9CA38A7}']

    function getDatePicker: JDatePicker; cdecl;
    procedure updateDate( year: Integer; monthOfYear: Integer; dayOfMonth: Integer ); cdecl;
  end;

  TJDatePickerDialog = class( TJavaGenericImport<JDatePickerDialogClass, JDatePickerDialog> )
  end;

  // ----------------------------------------------------------------------------
  // JBaseAdapter Class
  // ----------------------------------------------------------------------------
  JBaseAdapterClass = interface( JObjectClass )
    ['{54DC05BB-0621-404D-AB59-0B7C0D33ED1B}']

    function init: JBaseAdapter; cdecl;
  end;

  [JavaSignature( 'android/widget/BaseAdapter' )]
  JBaseAdapter = interface( JObject )
    ['{545A8754-18F9-44F9-A0BD-145D32F9B4E3}']

    function areAllItemsEnabled: boolean; cdecl;
    function getDropDownView( position: integer; convertView: JView; parent: JViewGroup ): JView; cdecl;
    function getItemViewType( position: integer ): integer; cdecl;
    function getViewTypeCount: integer; cdecl;
    function hasStableIds: boolean; cdecl;
    function isEmpty: boolean; cdecl;
    function isEnabled( position: integer ): boolean; cdecl;
    procedure notifyDataSetChanged; cdecl;
    procedure notifyDataSetInvalidated; cdecl;
    procedure registerDataSetObserver( observer: JDataSetObserver ); cdecl;
    procedure unregisterDataSetObserver( observer: JDataSetObserver ); cdecl;

  end;

  TJBaseAdapter = class( TJavaGenericImport<JBaseAdapterClass, JBaseAdapter> )
  end;

  // ----------------------------------------------------------------------------
  // JSimpleAdapter Class
  // ----------------------------------------------------------------------------
  JSimpleAdapterClass = interface( JBaseAdapterClass )
    ['{1145DF94-02B3-42A5-B83C-400F2B208AAE}']

    function init( context: JContext; data: JArrayList; resource: integer; from: TJavaArray<JString>; &to: TJavaArray<integer> ): JSimpleAdapter; cdecl;
  end;

  [JavaSignature( 'android/widget/SimpleAdapter' )]
  JSimpleAdapter = interface( JBaseAdapter )
    ['{C3FE238B-9594-40F1-ABF6-47A80273D413}']

  end;

  TJSimpleAdapter = class( TJavaGenericImport<JSimpleAdapterClass, JSimpleAdapter> )
  end;

  // ----------------------------------------------------------------------------
  // JArrayAdapter Class
  // ----------------------------------------------------------------------------
  JArrayAdapterClass = interface( JBaseAdapterClass )
    ['{5AA2A691-E536-434B-8059-D601450123C6}']

    function init( context: JContext; resource: integer ): JArrayAdapter; cdecl; overload;
    function init( context: JContext; resource: integer; objects: JList ): JArrayAdapter; cdecl; overload;

    function init( context: JContext; resource: integer; textViewResourceId: Integer; objects: JList ): JArrayAdapter; cdecl; overload;
    function init( context: JContext; resource: integer; textViewResourceId: Integer; objects: TJavaObjectArray<JObject> ): JArrayAdapter; cdecl; overload;
    function init( context: JContext; resource: integer; textViewResourceId: Integer ): JArrayAdapter; cdecl; overload;

    function createFromResource( context: JContext; textArrayResId: integer; textViewResId: Integer ): JArrayAdapter; cdecl;
  end;

  [JavaSignature( 'android/widget/ArrayAdapter' )]
  JArrayAdapter = interface( JBaseAdapter )
    ['{0A661039-1ED3-4417-A7A8-39B2F115D9D5}']

    procedure add( obj: JObject ); cdecl;
    procedure clear; cdecl;
    procedure remove( obj: JObject ); cdecl;
    procedure setDropDownViewResource( resource: integer ); cdecl;
    procedure setNotifyOnChange( notifyOnChange: boolean ); cdecl;
    function getCount: integer; cdecl;
    function getDropDownView( position: integer; convertView: JView; parent: JViewGroup ): JView; cdecl;
    function getItemId( position: integer ): int64; cdecl;

  end;

  TJArrayAdapter = class( TJavaGenericImport<JArrayAdapterClass, JArrayAdapter> )
  end;

  // ----------------------------------------------------------------------------
  // JToast Class
  // ----------------------------------------------------------------------------
  JToastClass = interface( JObjectClass )
    ['{465EF77D-2645-4AEF-8446-5B590C7674C7}']

    function init( context: JContext ): JToast; cdecl; overload;
    function makeText( context: JContext; text: JCharSequence; duration: Integer ): JToast; cdecl;
  end;

  [JavaSignature( 'android/widget/Toast' )]
  JToast = interface( JObject )
    ['{1C46FD3E-0500-4B21-8D3E-91DBE32235F2}']

    procedure show; cdecl;
    procedure cancel; cdecl;
    procedure setDuration( duration: integer ); cdecl; overload;
    procedure setText( s: JCharSequence ); cdecl; overload;
    procedure setText( resId: Integer ); cdecl; overload;
    procedure setGravity( gravity: integer; xOffset: Integer; yOffset: Integer ); cdecl;
  end;

  TJToast = class( TJavaGenericImport<JToastClass, JToast> )
  end;

  // ----------------------------------------------------------------------------
  // JProgressBar Class
  // ----------------------------------------------------------------------------
  JProgressBarClass = interface( JViewClass )
    ['{D9017C54-C502-4986-8EF6-CE8AC5F060CD}']

    function init( context: JContext ): JProgressBar; cdecl; overload;
    function init( context: JContext; attrs: JAttributeSet ): JProgressBar; cdecl; overload;
    function init( context: JContext; attrs: JAttributeSet; defStyle: Integer ): JProgressBar; cdecl; overload;
  end;

  [JavaSignature( 'android/widget/ProgressBar' )]
  JProgressBar = interface( JView )
    ['{B6C0CC1F-470B-40EE-818D-35EA8ECA629C}']

    function getMax: integer; cdecl;
    function getProgress: integer; cdecl;
    function getProgressDrawable: JDrawable; cdecl;
    function getSecondaryProgress: integer; cdecl;

    procedure setMax( max: integer ); cdecl;
    procedure setProgress( progress: integer ); cdecl;
    procedure setProgressDrawable( d: JDrawable ); cdecl;
    procedure setSecondaryProgress( secondaryProgress: integer ); cdecl;
    procedure setVisibility( v: integer ); cdecl;
    procedure setIndeterminate( indeterminate: boolean ); cdecl;
    procedure postInvalidate; cdecl;
  end;

  TJProgressBar = class( TJavaGenericImport<JProgressBarClass, JProgressBar> )
  end;

  // ----------------------------------------------------------------------------
  // JProgressDialog Class
  // ----------------------------------------------------------------------------
  JProgressDialogClass = interface( JAlertDialogClass )
    ['{B68A1618-1B21-4E65-B9DF-7670A742423C}']

    function init( context: JContext ): JProgressDialog; cdecl; overload;
    function init( context: JContext; theme: Integer ): JProgressDialog; cdecl; overload;

    procedure show( context: JContext; title: JCharSequence; message: JCharSequence ); cdecl; overload;
    procedure show( context: JContext; title: JCharSequence; message: JCharSequence; indeterminate: boolean; cancelable: boolean ); cdecl; overload;
    procedure show( context: JContext; title: JCharSequence; message: JCharSequence; indeterminate: boolean; cancelable: boolean; cancelListener: JDialogInterface_OnCancelListener ); cdecl; overload;
    procedure show( context: JContext; title: JCharSequence; message: JCharSequence; indeterminate: boolean ); cdecl; overload;
  end;

  [JavaSignature( 'android/app/ProgressDialog' )]
  JProgressDialog = interface( JAlertDialog )
    ['{9B5EEF73-2573-46DA-9B09-DB3BAE7C642F}']

    function getMax: integer; cdecl;
    function getProgress: integer; cdecl;
    function getProgressDrawable: JDrawable; cdecl;
    function getSecondaryProgress: integer; cdecl;

    procedure setMax( max: integer ); cdecl;
    procedure setProgress( progress: integer ); cdecl;
    procedure setProgressDrawable( d: JDrawable ); cdecl;
    procedure setSecondaryProgress( secondaryProgress: integer ); cdecl;
    procedure setVisibility( v: integer ); cdecl;
    procedure setIndeterminate( indeterminate: boolean ); cdecl;
    procedure setMessage( &message: JCharSequence ); cdecl;
    procedure setProgressNumberFormat( format: JString ); cdecl;
    // procedure setProgressPercentFormat( format: JNumberFormat ); cdecl;
    procedure setProgressStyle( style: integer ); cdecl;
  end;

  TJProgressDialog = class( TJavaGenericImport<JProgressDialogClass, JProgressDialog> )
  end;

  // ----------------------------------------------------------------------------
  // JTimePicker_OnTimeChangedListener Class
  // ----------------------------------------------------------------------------
  JTimePicker_OnTimeChangedListenerClass = interface( IJavaClass )
    ['{7A859F2D-DF7A-4D59-9278-2CC65CE0F3F8}']
  end;

  [JavaSignature( 'android/widget/TimePicker$OnTimeChangedListener' )]
  JTimePicker_OnTimeChangedListener = interface( IJavaInstance )
    ['{7BCCDFC3-C074-417B-B5C9-95A1900FDE11}']
    procedure onTimeChanged( view: JTimePicker; hourOfDay: integer; minute: Integer ); cdecl;
  end;

  TJTimePicker_OnTimeChangedListener = class( TJavaGenericImport<JTimePicker_OnTimeChangedListenerClass, JTimePicker_OnTimeChangedListener> )
  end;

  // ----------------------------------------------------------------------------
  // JTimePicker Class
  // ----------------------------------------------------------------------------
  JTimePickerClass = interface( JFrameLayoutClass )
    ['{024BFD0C-6BEC-48B1-A00B-99EAEB80B650}']

    function init( context: JContext ): JTimePicker; cdecl; overload;
    function init( context: JContext; attrs: JAttributeSet ): JTimePicker; cdecl; overload;
    function init( context: JContext; attrs: JAttributeSet; defStyle: Integer ): JTimePicker; cdecl; overload;
  end;

  [JavaSignature( 'android/widget/TimePicker' )]
  JTimePicker = interface( JFrameLayout )
    ['{5CD6B9A9-151B-4F2D-942E-010345ACAB34}']

    function getBaseline: integer; cdecl;
    function getCurrentHour: integer; cdecl;
    function getCurrentMinute: integer; cdecl;
    function is24HourView: boolean; cdecl;
    function isEnabled: boolean; cdecl;

    procedure setCurrentHour( currentHour: Integer ); cdecl;
    procedure setCurrentMinute( currentMinute: Integer ); cdecl;
    procedure setEnabled( enabled: boolean ); cdecl;
    procedure setIs24HourView( is24HourView: Boolean ); cdecl;
    procedure setOnTimeChangedListener( onTimeChangedListener: JTimePicker_OnTimeChangedListener ); cdecl;
  end;

  TJTimePicker = class( TJavaGenericImport<JTimePickerClass, JTimePicker> )
  end;

  // ----------------------------------------------------------------------------
  // JTimePickerDialog Class
  // ----------------------------------------------------------------------------
  JTimePickerDialogClass = interface( JAlertDialogClass )
    ['{90ED326A-12F4-4639-82F8-30562B35CEDB}']

    function init( context: JContext; callBack: JTimePickerDialog_OnTimeSetListener; hourOfDay: integer; minute: integer; is24HourView: boolean ): JTimePickerDialog; cdecl; overload;
    function init( context: JContext; theme: integer; callBack: JTimePickerDialog_OnTimeSetListener; hourOfDay: integer; minute: integer; is24HourView: boolean ): JTimePickerDialog; cdecl; overload;

  end;

  [JavaSignature( 'android/app/TimePickerDialog' )]
  JTimePickerDialog = interface( JAlertDialog )
    ['{89FD2DF6-CA6A-4AF0-98C6-67CE74FB9C11}']

  end;

  TJTimePickerDialog = class( TJavaGenericImport<JTimePickerDialogClass, JTimePickerDialog> )
  end;

  // ----------------------------------------------------------------------------
  // JTimePickerDialog_OnTimeSetListener Class
  // ----------------------------------------------------------------------------
  JTimePickerDialog_OnTimeSetListenerClass = interface( IJavaClass )
    ['{2BC4DECA-64BF-463D-ADC5-E58A2E18456D}']

  end;

  [JavaSignature( 'android/app/TimePickerDialog$OnTimeSetListener' )]
  JTimePickerDialog_OnTimeSetListener = interface( IJavaInstance )
    ['{2A8342AD-2633-42A2-9C30-796489AA6C9C}']

    procedure onTimeSet( view: JTimePicker; hourOfDay: integer; minute: Integer ); cdecl;
  end;

  TJTimePickerDialog_OnTimeSetListener = class( TJavaGenericImport<JTimePickerDialog_OnTimeSetListenerClass, JTimePickerDialog_OnTimeSetListener> )
  end;

  // ----------------------------------------------------------------------------
  // JAbsSeekBar Class
  // ----------------------------------------------------------------------------
  JAbsSeekBarClass = interface( JProgressBarClass )
    ['{9D9BA5BB-4D13-4958-B6C5-D3A333268BAF}']

    function init( context: JContext ): JAbsSeekBar; cdecl; overload;
    function init( context: JContext; attrs: JAttributeSet ): JAbsSeekBar; cdecl; overload;
    function init( context: JContext; attrs: JAttributeSet; defStyle: Integer ): JAbsSeekBar; cdecl; overload;
  end;

  [JavaSignature( 'android/widget/AbsSeekBar' )]
  JAbsSeekBar = interface( JProgressBar )
    ['{3A629628-E851-4DA5-B6C9-4CDDEB86A35D}']

    function getKeyProgressIncrement: Integer; cdecl;
    function getThumb: JDrawable; cdecl;
    function getThumbOffset: Integer; cdecl;
    procedure jumpDrawablesToCurrentState; cdecl;
    procedure setMax( max: integer ); cdecl; // synchronized
    procedure setKeyProgressIncrement( increment: integer ); cdecl;
    procedure setThumb( thumb: JDrawable ); cdecl;
    procedure setThumbOffset( thumbOffset: integer ); cdecl;
  end;

  TAbsSeekBar = class( TJavaGenericImport<JAbsSeekBarClass, JAbsSeekBar> )
  end;

  // ----------------------------------------------------------------------------
  // JSeekBar_OnSeekBarChangeListener Class
  // ----------------------------------------------------------------------------
  JSeekBar_OnSeekBarChangeListenerClass = interface( IJavaClass )
    ['{830F5B7F-6F21-4B50-93E4-6CE0958F9745}']

  end;

  [JavaSignature( 'android/widget/SeekBar$OnSeekBarChangeListener' )]
  JSeekBar_OnSeekBarChangeListener = interface( IJavaInstance )
    ['{EC64CC51-AF84-497B-B6AC-1CF651D4D006}']

    procedure onProgressChanged( seekBar: JSeekBar; progress: integer; fromUser: boolean ); cdecl;
    procedure onStartTrackingTouch( seekBar: JSeekBar ); cdecl;
    procedure onStopTrackingTouch( seekBar: JSeekBar ); cdecl;
  end;

  TJSeekBar_OnSeekBarChangeListener = class( TJavaGenericImport<JSeekBar_OnSeekBarChangeListenerClass, JSeekBar_OnSeekBarChangeListener> )
  end;

  // ----------------------------------------------------------------------------
  // JSeekBar Class
  // ----------------------------------------------------------------------------
  JSeekBarClass = interface( JAbsSeekBarClass )
    ['{56CC2515-D4C9-4EF4-9480-DC1B54986A23}']

    function init( context: JContext ): JSeekBar; cdecl; overload;
    function init( context: JContext; attrs: JAttributeSet ): JSeekBar; cdecl; overload;
    function init( context: JContext; attrs: JAttributeSet; defStyle: Integer ): JSeekBar; cdecl; overload;
  end;

  [JavaSignature( 'android/widget/SeekBar' )]
  JSeekBar = interface( JAbsSeekBar )
    ['{3319BF2B-1296-410B-A9D8-645E26CE31A3}']

    procedure setOnSeekBarChangeListener( listener: JSeekBar_OnSeekBarChangeListener ); cdecl;
  end;

  TJSeekBar = class( TJavaGenericImport<JSeekBarClass, JSeekBar> )
  end;

  // ----------------------------------------------------------------------------
  // JAbsSpinner Class
  // ----------------------------------------------------------------------------
  JAbsSpinnerClass = interface( JAdapterViewClass )
    ['{C0F99D82-872A-4F00-A00E-1764FB16FF81}']

    function init( context: JContext ): JAbsSpinner; cdecl; overload;
    function init( context: JContext; attrs: JAttributeSet ): JAbsSpinner; cdecl; overload;
    function init( context: JContext; attrs: JAttributeSet; defStyle: Integer ): JAbsSpinner; cdecl; overload;
  end;

  [JavaSignature( 'android/widget/AbsSpinner' )]
  JAbsSpinner = interface( JAdapterView )
    ['{11D1EF31-A88D-42E4-8D93-1C0307524F92}']

    function getAdapter: JSpinnerAdapter; cdecl;
    function getCount: Integer; cdecl;
    function getSelectedView: JView; cdecl;
    function pointToPosition( x: Integer; y: Integer ): Integer; cdecl;
    procedure requestLayout; cdecl;
    procedure setAdapter( adapter: JSpinnerAdapter ); cdecl;
    procedure setSelection( position: Integer; animate: boolean ); cdecl; overload;
    procedure setSelection( position: integer ); cdecl; overload;
  end;

  TJAbsSpinner = class( TJavaGenericImport<JAbsSpinnerClass, JAbsSpinner> )
  end;

  // ----------------------------------------------------------------------------
  // JSpinner Class
  // ----------------------------------------------------------------------------
  JSpinnerClass = interface( JAbsSpinnerClass )
    ['{ABCACE07-C484-4E31-8889-1D53C19F91F7}']

    function init( context: JContext ): JSpinner; cdecl; overload;
    function init( context: JContext; mode: integer ): JSpinner; cdecl; overload;
    function init( context: JContext; attrs: JAttributeSet ): JSpinner; cdecl; overload;
    function init( context: JContext; attrs: JAttributeSet; defStyle: Integer ): JSpinner; cdecl; overload;
    function init( context: JContext; attrs: JAttributeSet; defStyle: Integer; mode: integer ): JSpinner; cdecl; overload;
  end;

  [JavaSignature( 'android/widget/Spinner' )]
  JSpinner = interface( JAbsSpinner )
    ['{F5D96491-2062-45D6-9B89-02F0C0A49AB3}']

    function getBaseline: Integer; cdecl;
    function getDropDownHorizontalOffset: Integer; cdecl;
    function getDropDownVerticalOffset: Integer; cdecl;
    function getDropDownWidth: Integer; cdecl;
    function getGravity: Integer; cdecl;
    function getPopupBackground: JDrawable; cdecl;
    function etPrompt: JCharSequence; cdecl;
    function performClick: Boolean; cdecl;
    procedure setAdapter( adapter: JSpinnerAdapter ); cdecl;
    procedure setDropDownHorizontalOffset( pixels: integer ); cdecl;
    procedure setDropDownVerticalOffset( pixels: integer ); cdecl;
    procedure setDropDownWidth( pixels: integer ); cdecl;
    procedure setEnabled( enabled: boolean ); cdecl;
    procedure setGravity( gravity: integer ); cdecl;
    procedure setOnItemClickListener( Listener: JAdapterView_OnItemClickListener ); cdecl;
    procedure setPopupBackgroundDrawable( background: JDrawable ); cdecl;
    procedure setPopupBackgroundResource( resId: integer ); cdecl;
    procedure setPrompt( prompt: JCharSequence ); cdecl;
    procedure setPromptId( promptId: integer ); cdecl;
  end;

  TJSpinner = class( TJavaGenericImport<JSpinnerClass, JSpinner> )
  end;

  // ----------------------------------------------------------------------------
  // JTabHost_TabContentFactory Class
  // ----------------------------------------------------------------------------
  JTabHost_TabContentFactoryClass = interface( IJavaClass )
    ['{E1E7DC6F-FB93-4316-90EF-C493E812C7CB}']

  end;

  [JavaSignature( 'android/widget/TabHost$TabContentFactory' )]
  JTabHost_TabContentFactory = interface( IJavaInstance )
    ['{72C7D36D-112A-4007-AD87-51874E9F8217}']

    function createTabContent( tag: JString ): JView; cdecl;
  end;

  TJTabHost_TabContentFactory = class( TJavaGenericImport<JTabHost_TabContentFactoryClass, JTabHost_TabContentFactory> )
  end;

  // ----------------------------------------------------------------------------
  // JTabHost_OnTabChangeListener Class
  // ----------------------------------------------------------------------------
  JTabHost_OnTabChangeListenerClass = interface( IJavaClass )
    ['{82C68A8D-A904-48F5-9132-DC3CE73461A4}']

  end;

  [JavaSignature( 'android/widget/TabHost$OnTabChangeListener' )]
  JTabHost_OnTabChangeListener = interface( IJavaInstance )
    ['{82B58551-D33B-4436-A429-2F30968DFF1E}']

    procedure onTabChanged( tabId: JString ); cdecl;
  end;

  TJTabHost_OnTabChangeListener = class( TJavaGenericImport<JTabHost_OnTabChangeListenerClass, JTabHost_OnTabChangeListener> )
  end;

  // ----------------------------------------------------------------------------
  // JTabHost_TabSpec Class
  // ----------------------------------------------------------------------------
  JTabHost_TabSpecClass = interface( JObjectClass )
    ['{8BC3DEB2-9E03-421D-832F-6A4FE27912D6}']

  end;

  [JavaSignature( 'android/widget/TabHost$TabSpec' )]
  JTabHost_TabSpec = interface( JObject )
    ['{00028166-18FD-4C70-849A-5EB8E95C9AB8}']

    function getTag: JString; cdecl;
    function setContent( viewId: integer ): JTabHost_TabSpec; cdecl; overload; // Specify the id of the view that should be used as the content of the tab.
    function setContent( intent: JIntent ): JTabHost_TabSpec; cdecl; overload; // Specify an intent to use to launch an activity as the tab content.
    function setContent( contentFactory: JTabHost_TabContentFactory ): JTabHost_TabSpec; cdecl; overload; // Specify a TabHost.TabContentFactory to use to create the content of the tab.
    function setIndicator( &label: JCharSequence ): JTabHost_TabSpec; cdecl; overload; // Specify a label as the tab indicator.
    function setIndicator( view: JView ): JTabHost_TabSpec; cdecl; overload; // Specify a view as the tab indicator.
    function setIndicator( &label: JCharSequence; icon: JDrawable ): JTabHost_TabSpec; cdecl; overload; // Specify a label and icon as the tab indicator.

  end;

  TJTabHost_TabSpec = class( TJavaGenericImport<JTabHost_TabSpecClass, JTabHost_TabSpec> )
  end;

  // ----------------------------------------------------------------------------
  // JTabWidget Class
  // ----------------------------------------------------------------------------
  JTabWidgetClass = interface( JLinearLayoutClass )
    ['{0A55455B-5B35-421B-8E51-5945607D101E}']

    function init( context: JContext ): JTabWidget; cdecl; overload;
    function init( context: JContext; attrs: JAttributeSet ): JTabWidget; cdecl; overload;
    function init( context: JContext; attrs: JAttributeSet; defStyle: Integer ): JTabWidget; cdecl; overload;
  end;

  [JavaSignature( 'android/widget/TabWidget' )]
  JTabWidget = interface( JLinearLayout )
    ['{5C7544F2-720E-44BA-A790-391AE0756352}']

    procedure removeAllViews; cdecl;
    procedure addView( child: JView ); cdecl;
    procedure setDividerDrawable( drawable: JDrawable ); cdecl; overload;
    procedure setDividerDrawable( resId: integer ); cdecl; overload;
    procedure setEnabled( enabled: boolean ); cdecl;
    procedure setLeftStripDrawable( drawable: JDrawable ); cdecl; overload;
    procedure setLeftStripDrawable( resId: integer ); cdecl; overload;
    procedure setRightStripDrawable( drawable: JDrawable ); cdecl; overload;
    procedure setRightStripDrawable( resId: integer ); cdecl; overload;
    procedure setCurrentTab( index: integer ); cdecl;
    procedure setStripEnabled( stripEnabled: boolean ); cdecl;
    procedure focusCurrentTab( index: integer ); cdecl;
    function getChildTabViewAt( index: integer ): JView; cdecl;
    function getTabCount: Integer; cdecl;
    function sStripEnabled: Boolean; cdecl;
  end;

  TJTabWidget = class( TJavaGenericImport<JTabWidgetClass, JTabWidget> )
  end;

  // ----------------------------------------------------------------------------
  // JTabHost Class
  // ----------------------------------------------------------------------------
  JTabHostClass = interface( JFrameLayoutClass )
    ['{2EA9C734-AE15-43BA-A569-C9901BBE9748}']

    function init( context: JContext ): JTabHost; cdecl; overload;
    function init( context: JContext; attrs: JAttributeSet ): JTabHost; cdecl; overload;
  end;

  [JavaSignature( 'android/widget/TabHost' )]
  JTabHost = interface( JFrameLayout )
    ['{CAA979A7-69C8-4928-8016-0E2585569C43}']

    procedure addTab( tabSpec: JTabHost_TabSpec ); cdecl; // Add a tab.
    procedure clearAllTabs; cdecl;                        // Removes all tabs from the tab widget associated with this tab host.
    function dispatchKeyEvent( event: JKeyEvent ): boolean; cdecl; // Dispatch a key event to the next view on the focus path.
    procedure dispatchWindowFocusChanged( hasFocus: boolean ); cdecl; // Called when the window containing this view gains or loses window focus.
    function getCurrentTab: Integer; cdecl;
    function getCurrentTabTag: JString; cdecl;
    function getCurrentTabView: JView; cdecl;
    function getCurrentView: JView; cdecl;
    function getTabContentView: JFrameLayout; cdecl; // Get the FrameLayout which holds tab content
    function getTabWidget: JTabWidget; cdecl;
    function newTabSpec( tag: JString ): JTabHost_TabSpec; cdecl; // Get a new TabHost.TabSpec associated with this tab host.
    // procedure onInitializeAccessibilityEvent(event: JAccessibilityEvent ); cdecl ;
    // procedure onInitializeAccessibilityNodeInfo( info: JAccessibilityNodeInfo ); cdecl;
    procedure onTouchModeChanged( isInTouchMode: boolean ); cdecl;
    procedure sendAccessibilityEvent( eventType: integer ); cdecl;
    procedure setCurrentTab( index: integer ); cdecl;
    procedure setCurrentTabByTag( tag: JString ); cdecl;
    procedure setOnTabChangedListener( l: JTabHost_OnTabChangeListener ); cdecl;
    procedure setup; cdecl; overload;
    procedure setup( activityGroup: JLocalActivityManager ); cdecl; overload; // If you are using setContent(android.content.Intent), this must be called since the activityGroup is needed to launch the local activity.

  end;

  TJTabHost = class( TJavaGenericImport<JTabHostClass, JTabHost> )
  end;

  // ----------------------------------------------------------------------------
  // JLocalActivityManager Class
  // ----------------------------------------------------------------------------
  JLocalActivityManagerClass = interface( JFrameLayoutClass )
    ['{80C74439-407D-412D-BA2A-8BE8A2C643E8}']

    function init( parent: JActivity; singleMode: boolean ): JLocalActivityManager; cdecl;
  end;

  [JavaSignature( 'android/app/LocalActivityManager' )]
  JLocalActivityManager = interface( JFrameLayout )
    ['{88AEBAB7-36F8-48B8-AA33-4EB263CAF70C}']

    procedure dispatchCreate( state: JBundle ); cdecl;
    procedure dispatchDestroy( finishing: boolean ); cdecl;
    procedure dispatchPause( finishing: boolean ); cdecl;
    procedure dispatchResume; cdecl;
    procedure dispatchStop; cdecl;
    procedure removeAllActivities; cdecl;
    function getActivity( id: JString ): JActivity; cdecl;
    function getCurrentActivity: JActivity; cdecl;
    function getCurrentId: JString; cdecl;
    function saveInstanceState: JBundle; cdecl;
    function startActivity( id: JString; intent: JIntent ): JWindow; cdecl;
    function destroyActivity( id: JString; finish: boolean ): JWindow; cdecl;
  end;

  TJLocalActivityManager = class( TJavaGenericImport<JLocalActivityManagerClass, JLocalActivityManager> )
  end;

  // ----------------------------------------------------------------------------
  // JActivityGroup Class
  // ----------------------------------------------------------------------------
  JActivityGroupClass = interface( JActivityClass )
    ['{9EF87A3B-452A-4611-A355-95E05A842D9F}']

    function init: JActivityGroup; cdecl; overload;
    function init( singleActivityMode: Boolean ): JActivityGroup; cdecl; overload;
  end;

  [JavaSignature( 'android/app/ActivityGroup' )]
  JActivityGroup = interface( JActivity )
    ['{A007D28E-B3DE-4BC2-ABBB-B061D8F2F93B}']

    function getCurrentActivity: JActivity; cdecl;
  end;

  TJActivityGroup = class( TJavaGenericImport<JActivityGroupClass, JActivityGroup> )
  end;

  // ----------------------------------------------------------------------------
  // JInterpolator Class
  // ----------------------------------------------------------------------------
  JInterpolatorClass = interface( IJavaClass )
    ['{3F4306DD-36CD-4DCF-8128-85B472096814}']

  end;

  [JavaSignature( 'android/view/animation/Interpolator' )]
  JInterpolator = interface( IJavaInstance )
    ['{661AD159-D197-45F2-AB6B-8ED08B8A9A8D}']

  end;

  TJInterpolator = class( TJavaGenericImport<JInterpolatorClass, JInterpolator> )
  end;

  // ----------------------------------------------------------------------------
  // JAnimation_AnimationListener Class
  // ----------------------------------------------------------------------------
  JAnimation_AnimationListenerClass = interface( IJavaClass )
    ['{6F1E812F-6CDC-48C2-B864-A26A47BF572E}']

  end;

  [JavaSignature( 'android/view/animation/Animation$AnimationListener' )]
  JAnimation_AnimationListener = interface( IJavaInstance )
    ['{01D103D5-A16D-4566-9C88-F24165F36DE9}']

    procedure onAnimationStart( animation: JAnimation ); cdecl;
    procedure onAnimationRepeat( animation: JAnimation ); cdecl;
    procedure onAnimationEnd( animation: JAnimation ); cdecl;
  end;

  TJAnimation_AnimationListener = class( TJavaGenericImport<JAnimation_AnimationListenerClass, JAnimation_AnimationListener> )
  end;

  // ----------------------------------------------------------------------------
  // JTransformation Class
  // ----------------------------------------------------------------------------
  JTransformationClass = interface( JObjectClass )
    ['{3B1CA80F-4E63-4FFE-9BCA-1533D1383679}']

    function init: JTransformation; cdecl; overload;
  end;

  [JavaSignature( 'android/view/animation/Transformation' )]
  JTransformation = interface( JObject )
    ['{A19AC842-E750-4261-8B2E-B221EA3AEB6F}']

    function _GetTYPE_ALPHA: integer;
    function _GetTYPE_BOTH: integer;
    function _GetTYPE_IDENTITY: integer;
    function _GetTYPE_MATRIX: integer;

    property TYPE_ALPHA: Integer read _GetTYPE_ALPHA;
    property TYPE_BOTH: Integer read _GetTYPE_BOTH;
    property TYPE_IDENTITY: Integer read _GetTYPE_IDENTITY;
    property TYPE_MATRIX: Integer read _GetTYPE_MATRIX;
    // ------------------------------------------------------------------

    procedure clear; cdecl;
    procedure setTransformationType( transformationType: integer ); cdecl;
    procedure compose( t: JTransformation ); cdecl;
    procedure setAlpha( alpha: single ); cdecl;
    procedure &set( t: JTransformation ); cdecl;
    function getAlpha: single; cdecl;
    function getMatrix: JMatrix; cdecl;
    function getTransformationType: integer; cdecl;
    function toShortString: JString; cdecl;
    function toString: JString; cdecl;

  end;

  TJTransformation = class( TJavaGenericImport<JTransformationClass, JTransformation> )
  end;

  // ----------------------------------------------------------------------------
  // JAnimationUtils Class
  // ----------------------------------------------------------------------------
  JAnimationUtilsClass = interface( JObjectClass )
    ['{BD224B71-F9F6-453B-91D7-46B7139403BD}']

    function init: JAnimationUtils; cdecl;
    function currentAnimationTimeMillis: int64; cdecl;
    function loadLayoutAnimation( context: JContext; id: integer ): JLayoutAnimationController; cdecl;
    function loadInterpolator( context: JContext; id: integer ): JInterpolator; cdecl;
    function loadAnimation( context: JContext; id: integer ): JAnimation; cdecl;
    function makeInAnimation( c: JContext; fromLeft: boolean ): JAnimation; cdecl;
    function makeInChildBottomAnimation( c: JContext ): JAnimation; cdecl;
    function makeOutAnimation( c: JContext; toRight: boolean ): JAnimation; cdecl;
  end;

  [JavaSignature( 'android/view/animation/AnimationUtils' )]
  JAnimationUtils = interface( JObject )
    ['{748F0E58-DD81-4C99-84FB-87D56AEE12CE}']

  end;

  TJAnimationUtils = class( TJavaGenericImport<JAnimationUtilsClass, JAnimationUtils> )
  end;

  // ----------------------------------------------------------------------------
  // JLayoutAnimationController Class
  // ----------------------------------------------------------------------------
  JLayoutAnimationControllerClass = interface( JObjectClass )
    ['{93FB9606-ECB5-4FBE-B7B3-373D38E8C44B}']

    function init( context: JContext; attrs: JAttributeSet ): JLayoutAnimationController; cdecl; overload;
    function init( animation: JAnimation ): JLayoutAnimationController; cdecl; overload;
    function init( animation: JAnimation; delay: single ): JLayoutAnimationController; cdecl; overload;
  end;

  [JavaSignature( 'android/view/animation/LayoutAnimationController' )]
  JLayoutAnimationController = interface( JObject )
    ['{99C9B88D-DFA8-48E0-9677-C5652C475248}']

    function _GetORDER_NORMAL: integer;
    function _GetORDER_RANDOM: integer;
    function _GetORDER_REVERSE: integer;

    property ORDER_NORMAL: integer read _GetORDER_NORMAL;
    property ORDER_RANDOM: integer read _GetORDER_RANDOM;
    property ORDER_REVERSE: integer read _GetORDER_REVERSE;
    // -------------------------------------------------------------------

    function getDelay: single; cdecl;
    function isDone: boolean; cdecl;
    function getOrder: integer; cdecl;
    function getInterpolator: JInterpolator; cdecl;
    function getAnimation: JAnimation; cdecl;
    function getAnimationForView( view: JView ): JAnimation; cdecl;
    function willOverlap: boolean; cdecl;
    function getDelayForView( view: JView ): boolean; cdecl;

    procedure start; cdecl;
    procedure setAnimation( context: JContext; resourceID: integer ); cdecl; overload;
    procedure setAnimation( animation: JAnimation ); cdecl; overload;
    procedure setDelay( delay: single ); cdecl;
    procedure setInterpolator( interpolator: JInterpolator ); cdecl; overload;
    procedure setInterpolator( context: JContext; resourceID: integer ); cdecl; overload;
    procedure setOrder( order: integer ); cdecl;

  end;

  TJLayoutAnimationController = class( TJavaGenericImport<JLayoutAnimationControllerClass, JLayoutAnimationController> )
  end;

  // ----------------------------------------------------------------------------
  // JViewAnimator Class
  // ----------------------------------------------------------------------------
  JAnimationClass = interface( JObjectClass )
    ['{FEFB07D4-9F3B-40A7-A000-19E1069BD374}']

    function init: JAnimation; cdecl; overload;
    function init( context: JContext; attrs: JAttributeSet ): JAnimation; cdecl; overload;
  end;

  [JavaSignature( 'android/view/animation/Animation' )]
  JAnimation = interface( JObject )
    ['{C26DCBF0-DBA6-40FF-9942-99A77A1DFEDF}']

    function _GetABSOLUTE: integer;
    function _GetINFINITE: integer;
    function _GetRELATIVE_TO_PARENT: integer;
    function _GetRELATIVE_TO_SELF: integer;
    function _GetRESTART: integer;
    function _GetREVERSE: integer;
    function _GetSTART_ON_FIRST_FRAME: integer;
    function _GetZORDER_BOTTOM: integer;
    function _GetZORDER_NORMAL: integer;
    function _GetZORDER_TOP: integer;

    property &ABSOLUTE: Integer read _GetABSOLUTE;
    property INFINITE: Integer read _GetINFINITE;
    property RELATIVE_TO_PARENT: Integer read _GetRELATIVE_TO_PARENT;
    property RELATIVE_TO_SELF: Integer read _GetRELATIVE_TO_SELF;
    property RESTART: Integer read _GetRESTART;
    property REVERSE: Integer read _GetREVERSE;
    property START_ON_FIRST_FRAME: Integer read _GetSTART_ON_FIRST_FRAME;
    property ZORDER_BOTTOM: Integer read _GetZORDER_BOTTOM;
    property ZORDER_NORMAL: Integer read _GetZORDER_NORMAL;
    property ZORDER_TOP: Integer read _GetZORDER_TOP;
    // ------------------------------------------------------------------

    procedure start; cdecl;
    procedure startNow; cdecl;
    procedure cancel; cdecl;
    procedure reset; cdecl;
    procedure restrictDuration( durationMillis: int64 ); cdecl;
    procedure scaleCurrentDuration( scale: single ); cdecl;
    procedure setBackgroundColor( bg: integer ); cdecl;
    procedure setDuration( durationMillis: int64 ); cdecl;
    procedure setFillAfter( fillAfter: boolean ); cdecl;
    procedure setFillBefore( fillBefore: boolean ); cdecl;
    procedure setDetachWallpaper( detachWallpaper: boolean ); cdecl;
    procedure setFillEnabled( fillEnabled: boolean ); cdecl;
    procedure setRepeatMode( repeatMode: integer ); cdecl;
    procedure setZAdjustment( zAdjustment: integer ); cdecl;
    procedure setStartTime( startTimeMillis: int64 ); cdecl;
    procedure setStartOffset( startOffset: int64 ); cdecl;
    procedure setRepeatCount( repeatCount: integer ); cdecl;
    procedure setInterpolator( i: JInterpolator ); cdecl; overload;
    procedure setInterpolator( context: JContext; resID: integer ); cdecl; overload;
    procedure setAnimationListener( listener: JAnimation_AnimationListener ); cdecl;
    procedure initialize( width: integer; height: integer; parentWidth: integer; parentHeight: integer ); cdecl;
    function computeDurationHint: int64; cdecl;
    function getStartOffset: int64; cdecl;
    function getStartTime: int64; cdecl;
    function getDuration: int64; cdecl;
    function getBackgroundColor: integer; cdecl;
    function getRepeatMode: integer; cdecl;
    function getZAdjustment: integer; cdecl;
    function getRepeatCount: integer; cdecl;
    function getDetachWallpaper: boolean; cdecl;
    function hasEnded: boolean; cdecl;
    function willChangeTransformationMatrix: boolean; cdecl;
    function willChangeBounds: boolean; cdecl;
    function hasStarted: boolean; cdecl;
    function isFillEnabled: boolean; cdecl;
    function isInitialized: boolean; cdecl;
    function getTransformation( currentTime: int64; outTransformation: JTransformation; scale: single ): boolean; cdecl; overload;
    function getTransformation( currentTime: Int64; outTransformation: JTransformation ): boolean; cdecl; overload;
    function getFillAfter: boolean; cdecl;
    function getFillBefore: boolean; cdecl;
    // function getInterpolator: JInterpolator; cdecl;
  end;

  TJAnimation = class( TJavaGenericImport<JAnimationClass, JAnimation> )
  end;

  // ----------------------------------------------------------------------------
  // JViewAnimator Class
  // ----------------------------------------------------------------------------
  JViewAnimatorClass = interface( JFrameLayoutClass )
    ['{0F9D8F1E-E843-400B-A486-59F3AC59398E}']

    function init( context: JContext ): JViewAnimator; cdecl; overload;
    function init( context: JContext; attrs: JAttributeSet ): JViewAnimator; cdecl; overload;
  end;

  [JavaSignature( 'android/widget/ViewAnimator' )]
  JViewAnimator = interface( JFrameLayout )
    ['{A737BBED-9A16-47EE-AB3F-183E83AA97F4}']

    procedure addView( child: JView; index: integer; params: JViewGroup_LayoutParams ); cdecl;
    procedure removeAllViews; cdecl;
    procedure removeViewAt( index: integer ); cdecl;
    procedure removeViewInLayout( view: JView ); cdecl;
    procedure removeView( view: JView ); cdecl;
    procedure showNext; cdecl;
    procedure showPrevious; cdecl;
    procedure setOutAnimation( context: JContext; resourceID: integer ); cdecl; overload;
    procedure setOutAnimation( outAnimation: JAnimation ); cdecl; overload;
    procedure setDisplayedChild( whichChild: integer ); cdecl;
    procedure setAnimateFirstView( animate: boolean ); cdecl;
    procedure setInAnimation( inAnimation: JAnimation ); cdecl; overload;
    procedure setInAnimation( context: JContext; resourceID: integer ); cdecl; overload;
    procedure removeViewsInLayout( start: integer; count: integer ); cdecl;
    procedure removeViews( start: integer; count: integer ); cdecl;
    function getAnimateFirstView: boolean; cdecl;
    function getBaseline: integer; cdecl;
    function getDisplayedChild: integer; cdecl;
    function getCurrentView: JView; cdecl;
    function getInAnimation: JAnimation; cdecl;
    function getOutAnimation: JAnimation; cdecl;
  end;

  TJViewAnimator = class( TJavaGenericImport<JViewAnimatorClass, JViewAnimator> )
  end;

  // ----------------------------------------------------------------------------
  // JViewSwitcher_ViewFactory Class
  // ----------------------------------------------------------------------------
  JViewSwitcher_ViewFactoryClass = interface( IJavaClass )
    ['{37B3817A-F44F-456F-8511-F3A4A4DE630B}']

  end;

  [JavaSignature( 'android/widget/ViewSwitcher$ViewFactory' )]
  JViewSwitcher_ViewFactory = interface( IJavaInstance )
    ['{6D1E2143-A1B6-41CC-9834-F62C7991CD33}']

    function makeView: JView; cdecl;
  end;

  TJViewSwitcher_ViewFactory = class( TJavaGenericImport<JViewSwitcher_ViewFactoryClass, JViewSwitcher_ViewFactory> )
  end;

  // ----------------------------------------------------------------------------
  // JViewSwitcher Class
  // ----------------------------------------------------------------------------
  JViewSwitcherClass = interface( JViewAnimatorClass )
    ['{D8C85212-0C2B-471B-A1D1-F67145A4FE88}']

    function init( context: JContext ): JViewSwitcher; cdecl; overload;
    function init( context: JContext; attrs: JAttributeSet ): JViewSwitcher; cdecl; overload;
  end;

  [JavaSignature( 'android/widget/ViewSwitcher' )]
  JViewSwitcher = interface( JViewAnimator )
    ['{9B1BB54F-5456-4226-B3F5-C14728DCA40E}']

    procedure reset; cdecl;
    procedure setFactory( factory: JViewSwitcher_ViewFactory ); cdecl;
    procedure addView( child: JView; index: integer; params: JViewGroup_LayoutParams ); cdecl;
    function getNextView: JView; cdecl;
  end;

  TJViewSwitcher = class( TJavaGenericImport<JViewSwitcherClass, JViewSwitcher> )
  end;

  // ----------------------------------------------------------------------------
  // JTextSwitcher Class
  // ----------------------------------------------------------------------------
  JTextSwitcherClass = interface( JViewSwitcherClass )
    ['{8741BC97-BA7D-48EF-955A-6FC687CCB556}']

    function init( context: JContext ): JTextSwitcher; cdecl; overload;
    function init( context: JContext; attrs: JAttributeSet ): JTextSwitcher; cdecl; overload;
  end;

  [JavaSignature( 'android/widget/TextSwitcher' )]
  JTextSwitcher = interface( JViewSwitcher )
    ['{BFADB0AA-7624-47FA-AFA2-2FDDFB5F977D}']

    procedure addView( child: JView; index: integer; params: JViewGroup_LayoutParams ); cdecl;
    procedure setCurrentText( text: JCharSequence ); cdecl;
    procedure setText( text: JCharSequence ); cdecl;
  end;

  TJTextSwitcher = class( TJavaGenericImport<JTextSwitcherClass, JTextSwitcher> )
  end;

  // ----------------------------------------------------------------------------
  // JBaseMovementMethod Class
  // ----------------------------------------------------------------------------
  JBaseMovementMethodClass = interface( JObjectClass )
    ['{F033FBBA-9C57-499E-801F-7097ADC92614}']

    function init: JBaseMovementMethod; cdecl; overload;
  end;

  [JavaSignature( 'android/text/method/BaseMovementMethod' )]
  JBaseMovementMethod = interface( JObject )
    ['{1C77C960-7343-47F4-B672-CC0C66652239}']

    function canSelectArbitrarily: boolean; cdecl;
    function onGenericMotionEvent( widget: JTextView; text: JSpannable; event: JMotionEvent ): Boolean; cdecl;
    function onKeyDown( widget: JTextView; text: JSpannable; keyCode: integer; event: JKeyEvent ): Boolean; cdecl;
    function onKeyOther( widget: JTextView; text: JSpannable; event: JKeyEvent ): Boolean; cdecl;
    function onKeyUp( widget: JTextView; text: JSpannable; keyCode: integer; event: JKeyEvent ): Boolean; cdecl;
    function onTouchEvent( widget: JTextView; text: JSpannable; event: JMotionEvent ): Boolean; cdecl;
    function onTrackballEvent( widget: JTextView; text: JSpannable; event: JMotionEvent ): Boolean; cdecl;

    procedure initialize( widget: JTextView; text: JSpannable ); cdecl;
    procedure onTakeFocus( widget: JTextView; text: JSpannable; direction: integer ); cdecl;
  end;

  TJBaseMovementMethod = class( TJavaGenericImport<JBaseMovementMethodClass, JBaseMovementMethod> )
  end;

  // ----------------------------------------------------------------------------
  // JScrollingMovementMethod Class
  // ----------------------------------------------------------------------------
  JScrollingMovementMethodClass = interface( JBaseMovementMethodClass )
    ['{9C2A63F0-A0E3-499F-8BF6-13388FFC95FC}']

    function init: JScrollingMovementMethod; cdecl; overload;
    // function getInstance: JMovementMethod; cdecl; overload;
  end;

  [JavaSignature( 'android/text/method/ScrollingMovementMethod' )]
  JScrollingMovementMethod = interface( JBaseMovementMethod )
    ['{640F4CD7-57C5-4E6B-B09F-336E4A49B095}']

  end;

  TJScrollingMovementMethod = class( TJavaGenericImport<JScrollingMovementMethodClass, JScrollingMovementMethod> )
  end;

  // ----------------------------------------------------------------------------
  // JMovementMethod Class
  // ----------------------------------------------------------------------------
  JMovementMethodClass = interface( IJavaClass )
    ['{C6C2FC2B-6A05-4E1F-B6E7-CFED0CF76098}']

    function init: JScrollingMovementMethod; cdecl; overload;
  end;

  [JavaSignature( 'android/text/method/MovementMethod' )]
  JMovementMethod = interface( IJavaInstance )
    ['{8356F2CC-C779-4D20-9BF7-BAD645D4BDAA}']

    function canSelectArbitrarily: boolean; cdecl;
    function onGenericMotionEvent( widget: JTextView; text: JSpannable; event: JMotionEvent ): Boolean; cdecl;
    function onKeyDown( widget: JTextView; text: JSpannable; keyCode: integer; event: JKeyEvent ): Boolean; cdecl;
    function onKeyOther( widget: JTextView; text: JSpannable; event: JKeyEvent ): Boolean; cdecl;
    function onKeyUp( widget: JTextView; text: JSpannable; keyCode: integer; event: JKeyEvent ): Boolean; cdecl;
    function onTouchEvent( widget: JTextView; text: JSpannable; event: JMotionEvent ): Boolean; cdecl;
    function onTrackballEvent( widget: JTextView; text: JSpannable; event: JMotionEvent ): Boolean; cdecl;
  end;

  TJMovementMethod = class( TJavaGenericImport<JMovementMethodClass, JMovementMethod> )
  end;

  // ----------------------------------------------------------------------------
  // JTextClock Class
  // ----------------------------------------------------------------------------
  JTextClockClass = interface( JTextViewClass )
    ['{558FE5FB-B125-4C3D-976F-338B824F0CC7}']

    function init( context: JContext ): JTextClock; cdecl; overload;
    function init( context: JContext; attrs: JAttributeSet ): JTextClock; cdecl; overload;
    function init( context: JContext; attrs: JAttributeSet; defStyle: Integer ): JTextClock; cdecl; overload;
  end;

  [JavaSignature( 'android/widget/TextClock' )]
  JTextClock = interface( JTextView )
    ['{6A601689-2D2A-4692-BCCE-A37A3EB7950C}']

    function getFormat12Hour: JCharSequence; cdecl;
    function getFormat24Hour: JCharSequence; cdecl;
    function getTimeZone: JString; cdecl;
    function is24HourModeEnabled: boolean; cdecl;

    procedure setFormat12Hour( format: JCharSequence ); cdecl;
    procedure setFormat24Hour( format: JCharSequence ); cdecl;
    procedure setTimeZone( timeZone: JString ); cdecl;
  end;

  TJTextClock = class( TJavaGenericImport<JTextClockClass, JTextClock> )
  end;

  (*
    // ----------------------------------------------------------------------------
    // JWebView
    // ----------------------------------------------------------------------------
    JWebViewClass = interface( JAbsoluteLayoutClass )
    ['{F8B65BCE-6E98-44FE-A0E2-DDF9C8F0955E}']
    //    function _GetSCHEME_GEO: JString;
    //    function _GetSCHEME_MAILTO: JString;
    //    function _GetSCHEME_TEL: JString;

    function init( context: JContext ): JWebView; cdecl; overload;
    //    function init( context: JContext; attrs: JAttributeSet ): JWebView; cdecl; overload;
    //    function init( context: JContext; attrs: JAttributeSet; defStyle: Integer ): JWebView; cdecl; overload;
    //    function findAddress( addr: JString ): JString; cdecl;

    //    property SCHEME_GEO: JString read _GetSCHEME_GEO;
    //    property SCHEME_MAILTO: JString read _GetSCHEME_MAILTO;
    //    property SCHEME_TEL: JString read _GetSCHEME_TEL;
    end;

    [JavaSignature( 'android/webkit/WebView' )]
    JWebView = interface( JAbsoluteLayout )
    ['{A036376C-A7CB-400A-AE0D-95C0660B3D86}']
    { Methods }

    procedure addJavascriptInterface(object_: JObject; name: JString); cdecl;
    function canGoBack: Boolean; cdecl;
    function canGoBackOrForward(steps: Integer): Boolean; cdecl;
    function canGoForward: Boolean; cdecl;
    function canZoomIn: Boolean; cdecl;//Deprecated
    function canZoomOut: Boolean; cdecl;//Deprecated
    function capturePicture: JPicture; cdecl;
    procedure clearCache(includeDiskFiles: Boolean); cdecl;
    procedure clearFormData; cdecl;
    procedure clearHistory; cdecl;
    procedure clearMatches; cdecl;
    procedure clearSslPreferences; cdecl;
    procedure clearView; cdecl;
    procedure computeScroll; cdecl;
    function copyBackForwardList: JWebBackForwardList; cdecl;
    procedure destroy; cdecl;
    function dispatchKeyEvent(event: JKeyEvent): Boolean; cdecl;
    procedure documentHasImages(response: JMessage); cdecl;
    function findAll(find: JString): Integer; cdecl;//Deprecated
    procedure findAllAsync(find: JString); cdecl;
    procedure findNext(forward: Boolean); cdecl;
    procedure flingScroll(vx: Integer; vy: Integer); cdecl;
    procedure freeMemory; cdecl;
    function getCertificate: JSslCertificate; cdecl;
    function getContentHeight: Integer; cdecl;
    function getFavicon: JBitmap; cdecl;
    function getHitTestResult: JWebView_HitTestResult; cdecl;
    function getHttpAuthUsernamePassword(host: JString; realm: JString): TJavaObjectArray<JString>; cdecl;
    function getOriginalUrl: JString; cdecl;
    function getProgress: Integer; cdecl;
    function getScale: Single; cdecl;//Deprecated
    function getSettings: JWebSettings; cdecl;
    function getTitle: JString; cdecl;
    function getUrl: JString; cdecl;
    procedure goBack; cdecl;
    procedure goBackOrForward(steps: Integer); cdecl;
    procedure goForward; cdecl;
    procedure invokeZoomPicker; cdecl;
    function isPrivateBrowsingEnabled: Boolean; cdecl;
    procedure loadData(data: JString; mimeType: JString; encoding: JString); cdecl;
    procedure loadDataWithBaseURL(baseUrl: JString; data: JString; mimeType: JString; encoding: JString; historyUrl: JString); cdecl;
    procedure onChildViewAdded(parent: JView; child: JView); cdecl;//Deprecated
    procedure onChildViewRemoved(p: JView; child: JView); cdecl;//Deprecated
    function onCreateInputConnection(outAttrs: JEditorInfo): JInputConnection; cdecl;
    function onGenericMotionEvent(event: JMotionEvent): Boolean; cdecl;
    procedure onGlobalFocusChanged(oldFocus: JView; newFocus: JView); cdecl;//Deprecated
    function onHoverEvent(event: JMotionEvent): Boolean; cdecl;
    function onKeyDown(keyCode: Integer; event: JKeyEvent): Boolean; cdecl;
    function onKeyMultiple(keyCode: Integer; repeatCount: Integer; event: JKeyEvent): Boolean; cdecl;
    function onKeyUp(keyCode: Integer; event: JKeyEvent): Boolean; cdecl;
    procedure onPause; cdecl;
    procedure onResume; cdecl;
    function onTouchEvent(event: JMotionEvent): Boolean; cdecl;
    function onTrackballEvent(event: JMotionEvent): Boolean; cdecl;
    procedure onWindowFocusChanged(hasWindowFocus: Boolean); cdecl;
    function overlayHorizontalScrollbar: Boolean; cdecl;
    function overlayVerticalScrollbar: Boolean; cdecl;
    function pageDown(bottom: Boolean): Boolean; cdecl;
    function pageUp(top: Boolean): Boolean; cdecl;
    procedure pauseTimers; cdecl;
    function performAccessibilityAction(action: Integer; arguments: JBundle): Boolean; cdecl;
    function performLongClick: Boolean; cdecl;
    procedure postUrl(url: JString; postData: TJavaArray<Byte>); cdecl;
    procedure reload; cdecl;
    procedure removeJavascriptInterface(name: JString); cdecl;
    function requestChildRectangleOnScreen(child: JView; rect: JRect; immediate: Boolean): Boolean; cdecl;
    function requestFocus(direction: Integer; previouslyFocusedRect: JRect): Boolean; cdecl;
    procedure requestFocusNodeHref(hrefMsg: JMessage); cdecl;
    procedure requestImageRef(msg: JMessage); cdecl;
    function restoreState(inState: JBundle): JWebBackForwardList; cdecl;
    procedure resumeTimers; cdecl;
    procedure savePassword(host: JString; username: JString; password: JString); cdecl;
    function saveState(outState: JBundle): JWebBackForwardList; cdecl;
    procedure saveWebArchive(filename: JString); cdecl; overload;
    procedure saveWebArchive(basename: JString; autoname: Boolean; callback: JValueCallback); cdecl; overload;
    procedure setBackgroundColor(color: Integer); cdecl;
    procedure setCertificate(certificate: JSslCertificate); cdecl;//Deprecated
    procedure setDownloadListener(listener: JDownloadListener); cdecl;
    procedure setFindListener(listener: JWebView_FindListener); cdecl;
    procedure setHorizontalScrollbarOverlay(overlay: Boolean); cdecl;
    procedure setHttpAuthUsernamePassword(host: JString; realm: JString; username: JString; password: JString); cdecl;
    procedure setInitialScale(scaleInPercent: Integer); cdecl;
    procedure setLayerType(layerType: Integer; paint: JPaint); cdecl;
    procedure setLayoutParams(params: JViewGroup_LayoutParams); cdecl;
    procedure setMapTrackballToArrowKeys(setMap: Boolean); cdecl;//Deprecated
    procedure setNetworkAvailable(networkUp: Boolean); cdecl;
    procedure setOverScrollMode(mode: Integer); cdecl;
    procedure setPictureListener(listener: JWebView_PictureListener); cdecl;//Deprecated
    procedure setScrollBarStyle(style: Integer); cdecl;
    procedure setVerticalScrollbarOverlay(overlay: Boolean); cdecl;
    procedure setWebChromeClient(client: JWebChromeClient); cdecl;
    procedure setWebViewClient(client: JWebViewClient); cdecl;
    function shouldDelayChildPressedState: Boolean; cdecl;//Deprecated
    function showFindDialog(text: JString; showIme: Boolean): Boolean; cdecl;
    function zoomIn: Boolean; cdecl;
    function zoomOut: Boolean; cdecl;

    procedure stopLoading; cdecl;
    procedure loadUrl(url: JString; additionalHttpHeaders: JMap); cdecl; overload;
    procedure loadUrl(url: JString); cdecl; overload;
    end;

    TJWebView = class( TJavaGenericImport<JWebViewClass, JWebView> )
    end;
  *)

  // ----------------------------------------------------------------------------
  // JDPFOnWebViewListenerClass
  // ----------------------------------------------------------------------------
  JDPFOnWebViewListenerClass = interface( IJavaClass )
    ['{9244FD77-89CB-46F4-9160-D5B2C4F1412C}']
  end;

  [JavaSignature( 'com/DPFaragir/webview/DPFOnWebViewListener' )]
  JDPFOnWebViewListener = interface( IJavaInstance )
    ['{8A06E016-9C95-4EF5-B133-CED8E3A9A864}']

    procedure doUpdateVisitedHistory( view: JWebView; url: JString; isReload: Boolean ); cdecl;
    procedure onFormResubmission( view: JWebView; dontResend: JMessage; resend: JMessage ); cdecl;
    procedure onLoadResource( view: JWebView; url: JString ); cdecl;
    procedure onPageFinished( view: JWebView; url: JString ); cdecl;
    procedure onPageStarted( view: JWebView; url: JString; favicon: JBitmap ); cdecl;
    procedure onReceivedError( view: JWebView; errorCode: Integer; description: JString; failingUrl: JString ); cdecl;
    procedure onReceivedHttpAuthRequest( view: JWebView; handler: JHttpAuthHandler; host: JString; realm: JString ); cdecl;
    procedure onReceivedSslError( view: JWebView; handler: JSslErrorHandler; error: JSslError ); cdecl;
    procedure onScaleChanged( view: JWebView; oldScale: Single; newScale: Single ); cdecl;
    procedure onUnhandledKeyEvent( view: JWebView; event: JKeyEvent ); cdecl;
    function shouldOverrideKeyEvent( view: JWebView; event: JKeyEvent ): Boolean; cdecl;
    function shouldOverrideUrlLoading( view: JWebView; url: JString ): Boolean; cdecl;
  end;

  TJDPFOnWebViewListener = class( TJavaGenericImport<JDPFOnWebViewListenerClass, JDPFOnWebViewListener> )
  end;

  // ----------------------------------------------------------------------------
  // JDPFWebView
  // ----------------------------------------------------------------------------
  JDPFWebViewClass = interface( JWebViewClass )
    ['{78BE2711-5230-48A7-BE12-898FAEE3C79C}']

    function init( context: JContext ): JDPFWebView; cdecl;
  end;

  [JavaSignature( 'com/DPFaragir/webview/DPFWebView' )]
  JDPFWebView = interface( JWebView )
    ['{8EDCF4F3-BBF8-4155-A8FB-E4B5A5D5AB7E}']

    procedure SetWebViewListener( listener: JDPFOnWebViewListener ); cdecl;
  end;

  TJDPFWebView = class( TJavaGenericImport<JDPFWebViewClass, JDPFWebView> )
  end;

  // ----------------------------------------------------------------------------
  // JDPFOnListViewListenerClass
  // ----------------------------------------------------------------------------
  JDPFOnListViewListenerClass = interface( IJavaClass )
    ['{71EDE4E6-D62D-45AA-AFA1-898F6D3FA154}']
  end;

  [JavaSignature( 'com/DPFaragir/listview/DPFOnListViewListener' )]
  JDPFOnListViewListener = interface( IJavaInstance )
    ['{3204EEF1-36AA-4400-B7E8-C8BCF2EC4982}']

    function onGetRowCount( view: JListView ): Integer; cdecl;
    function onGetCustomView( view: JListView; customView: JView; position: Integer ): JView; cdecl;
    procedure onItemSelected( listview: JListView; childView: JView; position: Integer; id: Int64 ); cdecl;
    procedure onNothingSelected( listview: JListView ); cdecl;
    procedure onItemClick( listview: JListView; view: JView; position: integer; id: Int64 ); cdecl;
  end;

  TJDPFOnListViewListener = class( TJavaGenericImport<JDPFOnListViewListenerClass, JDPFOnListViewListener> )
  end;

  // ----------------------------------------------------------------------------
  // JDPFListView
  // ----------------------------------------------------------------------------
  JDPFListViewClass = interface( JListViewClass )
    ['{174DF75E-9513-40E9-BA72-CC9532EFFBD3}']

    function init( context: JContext ): JDPFListView; cdecl;
  end;

  [JavaSignature( 'com/DPFaragir/listview/DPFListView' )]
  JDPFListView = interface( JListView )
    ['{B5EA87A9-C70B-4515-9B41-BC794BA10A5E}']

    procedure setListViewListener( listener: JDPFOnListViewListener ); cdecl;
    procedure reLoad( ); cdecl;
    procedure setCustomView( cView: JView ); cdecl;
    procedure setItemSelectedColor( color: Integer ); cdecl;
    procedure setListViewChoiceMode( choiceMode: Integer ); cdecl;
  end;

  TJDPFListView = class( TJavaGenericImport<JDPFListViewClass, JDPFListView> )
  end;

  // ----------------------------------------------------------------------------
  // JDPFWebClient
  // ----------------------------------------------------------------------------
  JDPFWebClientClass = interface( JWebViewClientClass )
    ['{5821A8AD-1012-4989-AD22-B5F09A512A11}']

    function init: JDPFWebClient; cdecl;
  end;

  [JavaSignature( 'com/DPFaragir/webview/DPFWebClient' )]
  JDPFWebClient = interface( JWebViewClient )
    ['{C4BC2E75-130F-4EB3-BF16-FB24C52C22A9}']

    procedure SetWebViewListener( listener: JDPFOnWebViewListener ); cdecl;
    procedure doUpdateVisitedHistory( view: JWebView; url: JString; isReload: Boolean ); cdecl;
    procedure onFormResubmission( view: JWebView; dontResend: JMessage; resend: JMessage ); cdecl;
    procedure onLoadResource( view: JWebView; url: JString ); cdecl;
    procedure onPageFinished( view: JWebView; url: JString ); cdecl;
    procedure onPageStarted( view: JWebView; url: JString; favicon: JBitmap ); cdecl;
    procedure onReceivedError( view: JWebView; errorCode: Integer; description: JString; failingUrl: JString ); cdecl;
    procedure onReceivedHttpAuthRequest( view: JWebView; handler: JHttpAuthHandler; host: JString; realm: JString ); cdecl;
    procedure onReceivedSslError( view: JWebView; handler: JSslErrorHandler; error: JSslError ); cdecl;
    procedure onScaleChanged( view: JWebView; oldScale: Single; newScale: Single ); cdecl;
    procedure onUnhandledKeyEvent( view: JWebView; event: JKeyEvent ); cdecl;
    function shouldOverrideKeyEvent( view: JWebView; event: JKeyEvent ): Boolean; cdecl;
    function shouldOverrideUrlLoading( view: JWebView; url: JString ): Boolean; cdecl;
  end;

  TPFJWebClient = class( TJavaGenericImport<JDPFWebClientClass, JDPFWebClient> )
  end;

  // ----------------------------------------------------------------------------
  // JDPFNativeLayout
  // ----------------------------------------------------------------------------
  JDPFNativeLayoutClass = interface( JWindowManager_LayoutParamsClass )
    ['{78763435-FB88-4E9B-A5EC-2F88A415F4BD}']

    function init( Con: JContext; Token: JIBinder ): JDPFNativeLayout; cdecl;
  end;

  [JavaSignature( 'com/DPFaragir/nativelayout/DPFNativeLayout' )]
  JDPFNativeLayout = interface( JWindowManager_LayoutParams )
    ['{023379A0-BCE4-419B-9BA3-019195B46951}']

    procedure SetControl( view: JView ); cdecl;
    procedure SetFocus( newFocusState: Boolean ); cdecl;
    procedure SetPosition( absoluteX: Integer; absoluteY: Integer ); cdecl;
    procedure SetSize( absoluteWidth: Integer; absoluteHeight: Integer ); cdecl;
  end;

  TJDPFNativeLayout = class( TJavaGenericImport<JDPFNativeLayoutClass, JDPFNativeLayout> )
  end;

  // ----------------------------------------------------------------------------
  // DPFOnHTTPListenerClass
  // ----------------------------------------------------------------------------
  JDPFOnHTTPListenerClass = interface( IJavaClass )
    ['{7EDF8AEF-52A8-4617-9907-78B78B0231D7}']
  end;

  [JavaSignature( 'com/DPFaragir/http/DPFOnHTTPListener' )]
  JDPFOnHTTPListener = interface( IJavaInstance )
    ['{02ED35DD-4BEA-4FA5-8E8F-9CFAD6E4581B}']

    procedure onStarted( DPFHttp: JDPFHTTP ); cdecl;
    procedure onCancelled( DPFHttp: JDPFHTTP ); cdecl;
    procedure onProgressUpdate( DPFHttp: JDPFHTTP; progress: Integer; downloadSize: Integer; downloaded: Integer ); cdecl;
    procedure onFinished( DPFHttp: JDPFHTTP; returnCode: Integer; httpResponseCode: Integer; httpResponseMessage: JString; error: JString ); cdecl;
  end;

  TJDPFOnHTTPListener = class( TJavaGenericImport<JDPFOnHTTPListenerClass, JDPFOnHTTPListener> )
  end;

  // ----------------------------------------------------------------------------
  // JDPFHTTP
  // ----------------------------------------------------------------------------
  JDPFHTTPClass = interface( JObjectClass )
    ['{B1034CED-F0D8-4F63-B259-A8D4C3D7E595}']

    function init: JDPFHTTP; cdecl;
  end;

  [JavaSignature( 'com/DPFaragir/http/DPFHTTP' )]
  JDPFHTTP = interface( JObject )
    ['{E8F5B21C-BCE1-43C3-80C4-55068207626D}']

    procedure startDownload( context: JContext; url: JString; saveFileName: JString; USerName: JString; Password: JString; OnHTTPListener: JDPFOnHTTPListener ); cdecl;
    procedure startUpload( context: JContext; fileName: JString; url: JString; Headers: TJavaObjectArray<JString>; Fields: TJavaObjectArray<JString>; OnHTTPListener: JDPFOnHTTPListener ); cdecl;
    procedure cancelTask( ); cdecl;
  end;

  TJDPFHTTP = class( TJavaGenericImport<JDPFHTTPClass, JDPFHTTP> )
  end;

  // ----------------------------------------------------------------------------
  // JDPFOnAnimationListenerClass
  // ----------------------------------------------------------------------------
  JDPFOnAnimationListenerClass = interface( IJavaClass )
    ['{72DEDAF4-DDE7-434B-B88D-86308E33DCCD}']
  end;

  [JavaSignature( 'com/DPFaragir/Animation/DPFOnAnimationListener' )]
  JDPFOnAnimationListener = interface( IJavaInstance )
    ['{C2B9BDA6-F4FB-47A4-831F-A0CAEB1F3269}']

    procedure onAnimationStart( animation: JAnimation ); cdecl;
    procedure onAnimationRepeat( animation: JAnimation ); cdecl;
    procedure onAnimationEnd( animation: JAnimation ); cdecl;
  end;

  TJDPFOnAnimationListener = class( TJavaGenericImport<JDPFOnAnimationListenerClass, JDPFOnAnimationListener> )
  end;

  // ----------------------------------------------------------------------------
  // JDPFAnimation
  // ----------------------------------------------------------------------------
  JDPFAnimationClass = interface( JObjectClass )
    ['{F645A7A7-3737-4F4E-8848-9C54FBA00284}']

    function init: JDPFAnimation; cdecl;
  end;

  [JavaSignature( 'com/DPFaragir/Animation/DPFAnimation' )]
  JDPFAnimation = interface( JObject )
    ['{B67EEC49-72E3-4794-A4FD-C0F3F2BE956E}']

    procedure startTranslateAnimation( context: JContext; view: JView; screenScale: single; fromXDelta: single; toXDelta: single; fromYDelta: single; toYDelta: single; FromAlpha: Single; ToAlpha: Single; durationMillis: Longword; OnAnimation: JDPFOnAnimationListener ); cdecl;
  end;

  TJDPFAnimation = class( TJavaGenericImport<JDPFAnimationClass, JDPFAnimation> )
  end;

  // ----------------------------------------------------------------------------
  // JDPFVideoView
  // ----------------------------------------------------------------------------
  JDPFVideoViewClass = interface( JSurfaceViewClass )
    ['{AD433D36-2635-4819-8F80-E8323D990DAE}']

    function init( context: JContext ): JDPFVideoView; cdecl;
  end;

  [JavaSignature( 'com/DPFaragir/DPFVideoView' )]
  JDPFVideoView = interface( JSurfaceView )
    ['{308AF0D5-9FEE-4FB2-BFE4-E07FBD6F5D43}']

    procedure playVideo; cdecl;
  end;

  TJDPFVideoView = class( TJavaGenericImport<JDPFVideoViewClass, JDPFVideoView> )
  end;

{$ENDIF}

  // ------------------------------------------------------------------------------
implementation

// ------------------------------------------------------------------------------
end.
