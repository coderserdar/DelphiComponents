{: Contains the General Edit Components for ESBPCS for CLX.

 This is designed to work in Borland Delphi 6 CLX and above, Borland
 C++ Builder 6 CLX and above, and Borland Kylix 2 and above.
 Most if not all features will work in Kylix 1 but it is not currently supported.<p>

 This suite of Edit Components is aimed at making Data Entry easier
 by supporting Read Only Colouring, On Focus Colouring,
 Handling of Enter as Tab, Handling of Arrow Keys as Tabs, only numeric
 characters in numeric fields, formatting options dependent upon Data
 Type, Methods to aid in manipulation of Data Types.<p>

 Plus many of the Edit Components are used in various Compound
 Components within ESBPCS.<p>

 For Numeric Edit Components see <See Unit=ESBPCSNumEdit>.<p>

 Copyright © 1999-2002 ESB Consultancy<p>

 v2.3 - 14 September 2002
}

unit QESBPCSEdit;

{$I esbpcs.inc}

interface

uses
     Classes, QControls, QGraphics, QStdCtrls,
     QESBPCSGlobals, QESBPCSGlobals2;

type
     {: Enhanced Custom Edit Control with Enhanced Color Changing and other features.
      ColorRW replaces the normal Color Property of Standard Edit Controls.<p>
  By default Read Only fields will be shown in a different Color,
 to disable this set ColorRW and ColorRO to the same, eg clWindow.
  clBtnFace is often a better choice for ColorRO on older Video Cards
  and older Notebooks<p>
  ColorFocus can be used so that the Color of the edit field changes
  when it receives focus (provided it is not ReadOnly). To use this feature
  ColorFocus must be set to a different value than ColorRW but beware
  the various Color combinations that result.<p>
  ColorDisabled controls the Color of the Control when Disabled, ie
  Enabled := False.<p>
    Null allows an edit field to be marked as having no proper value,
  and it will then display whatever NullStr is set to. Ctrl-N is
  the Keyboard entry for Null if AllowKbdNull is true. OnNullEvent
  is called when the Keyboard entry of Null is permitted.<p>
  If <See Var=ESBEnterAsTab> is true then the Enter Key will be treated
  as though it were the Tab Key.<p>
  If <See Var=ESBEscAsUndo> is true then the Esc Key will be cause an Undo
  to occur in the field - require Delphi 7 or Kylix 3.<p>
  If Arrows is False and <See Var=ESBArrowsAsTab> is true then Up Arrow will move to previous
  field like Shift-Tab, and Down Arrow will move to next field like
  Tab. }
     TESBCustomEdit = class (TCustomEdit)
     private
          { Private declarations }
          fAllowKbdNull: Boolean; // When True Ctrl-N gives Keyboard Null and OnNull Event called
          fBlankWhenNull: Boolean; // Should Nulls be displayed as Blanks
          fColorFocus: TColor; // Color to use when the field is Focused
          fColorRO: TColor; // Color to use when the field is ReadOnly
          fColorRW: TColor; // Color to use when the field is Read/Write
          fColorDisabled: TColor; // Color to display Control in when disabled
          fFocused: Boolean; // Set to True when control has the focus
          fNull: Boolean; // Set to True when the cell contains a Null Value
          fNullStr: string; // Value to display when fNull is True
          fReadOnly: Boolean; // New ReadOnly Property
          fMouseOver: Boolean; // Set to True when Mouse is over the control
          fColor_Defaults: Boolean;
          fStrValue: string; // String Value - replacement for Text
          fInUpdate: Boolean; // In Updating Mode
          fStoreValue: Boolean; // Controls whether the current value is stored
          fHotTrack: Boolean;
          fEnterValue: string;

          fOnEnter: TNotifyEvent; // Replacement Enter Routine
          fOnExit: TNotifyEvent; // Replacement Exit Routine
          fOnKeyPress: TKeyPressEvent; // Replacement OnKeyPressRoutine
          fOnNull: TNotifyEvent; // Event Called when Null is Set
          fOnCNKeyDown: TKeyEvent; // Event called when Control Notification of key Down
          fOnMouseEnter: TNotifyEvent; // Called when Mouse enters the Controls's Area
          fOnMouseExit: TNotifyEvent; // Called when Mouse leaves the Controls's Area
     protected
          fValidChar: TESBCharSet; // Set of Valid Characters for Keyboard Input
          fValidCharHold: TESBCharSet; // Held Valid Char
          fNoDisplayUpdate: Boolean;
          fBlankIsNull: Boolean;

          procedure KeyPress (var Key: Char); override;
          procedure KeyProcess (var Key: Char);
          procedure KeyDown (var Key: Word; Shift: TShiftState); override;
          procedure DoEnter; override;
          procedure DoExit; override;
          procedure MouseUp (Button: TMouseButton; Shift: TShiftState;
               X, Y: Integer); override;
          procedure MouseEnter (AControl: TControl); override;
          procedure MouseLeave (AControl: TControl); override;

          procedure SetBlankWhenNull (Value: Boolean);
          procedure SetColorFocus (Value: TColor);
          procedure SetColorDisabled (Value: TColor);
          procedure SetColorRO (Value: TColor);
          procedure SetColorRW (Value: TColor);
          procedure SetNull (Value: Boolean);
          procedure SetNullStr (const Value: string);
          procedure SetReadOnly (Value: Boolean);
          function GetVersion: string;
          procedure SetVersion (const Value: string);
          function GetStrValue: string; virtual;
          procedure SetStrValue (const Value: string); virtual;
          procedure Convert2Value; virtual;
          procedure SetColor_Defaults (Value: Boolean);
          procedure SetEnabled (const Value: Boolean); override;

          procedure DisplayText; virtual;
          procedure UpdateColors;

          function StoreNullStr: Boolean;
          procedure SetColors2Defaults; virtual;

     public
          { Public declarations }
          //: Creates the Edit Component.
          constructor Create (AOwner: TComponent); override;

          //: Causes all Colour Updates to Wait until the EndUpdate is called
          procedure BeginUpdate;

          //: Causes all Colour Updates to wait from when BeginUpdate is called
          procedure EndUpdate;
          //: Causes the Selection to be Removed
          procedure SelectNone;
          //: Returns True if the field is Clear
          function IsClear: Boolean;

          //: function to indicate whether value is stored in form
          function ValueStored: Boolean;

          procedure CutToClipboard; override;
          procedure PasteFromClipboard; override;

     published
          { Published declarations }

          {: Boolean Flag to signify if the control should be store the design
           time value, or simply use the value it defaults to when created. }
          property StoreValue: Boolean
               read fStoreValue
               write fStoreValue
               default True;
          {: When Set to True, Ctrl-N will set a field to Null
                   and the OnNull event will be called. }
          property AllowKbdNull: Boolean
               read fAllowKbdNull
               write fAllowKbdNull
               default True;
          {: Boolean Flag to signify if the Value is Null - that is that
           no proper value is contained. Cell will display whateve
           NullStr is set to. }
          property Null: Boolean
               read fNull
               write SetNull
               default False;
          {: When Null is true, signifying that there is no proper value
           then this string is displayed. Can be Empty. }
          property NullStr: string
               read fNullStr
               write SetNullStr
               stored StoreNullStr;
          {: When enabled, the Edit Box will display "Empty" when the
   Value is Null. }
          property BlankWhenNull: Boolean
               read fBlankWhenNull
               write SetBlankWhenNull
               default False;

          {: When Set to True all the Color Properties will get their current
           Default Values. Works similar to "ParentColor" }
          property Color_Defaults: Boolean
               read fColor_Defaults
               write SetColor_Defaults
               stored False
               default True;
          {: Color that the Control is displayed in if the Control is
           Disabled, ie Enabled := False.
           If set to clNone then ColorRW will be used
   unless that is also clNone, then ParentColor will be used. }
          property ColorDisabled: TColor
               read fColorDisabled
               write SetColorDisabled
               default DefDisabledColor;
          {: Color that the Control is displayed in the Control is focused.
                       Also used when <See Property=HotTrack> is true.
           If set to clNone then ColorRW will be used
           unless that is also clNone, then ParentColor will be used. }
          property ColorFocus: TColor
               read fColorFocus
               write SetColorFocus
               default DefFocusColor2;
          {: Color that the Control is displayed in if the Control is
           ReadOnly.
           If set to clNone then ColorRW will be used
           unless that is also clNone, then ParentColor will be used. }
          property ColorRO: TColor
               read fColorRO
               write SetColorRO
               default DefROColor;
          {: Color that the Control is displayed in if the Control is not
           ReadOnly, ie ReadWrite.
           If set to clNone then ParentColor will be used. }
          property ColorRW: TColor
               read fColorRW
               write SetColorRW
               default DefRWColor;
          {: Boolean Flag to signify if the Control is ReadOnly.
           <See Var=ESBNoTabStopOnReadOnly> controls if TabStop is also Toggled. }
          property ReadOnly: Boolean
               read fReadOnly
               write SetReadOnly
               default False;
          {: Overriden Text that still works the same way that
           the Standard TEdit property does, except it isn't stored. }
          property Text: string
               read GetStrValue
               write SetStrValue
               stored false;
          {: Alternate property to Text for completion . }
          property AsString: string
               read GetStrValue
               write SetStrValue
               stored false;
          {: Displays the Current Version of the Component. }
          property Version: string
               read GetVersion
               write SetVersion
               stored false;
          {: When True and the Mouse is Over the control, then it will display
           using <See Property=ColorFocus>. }
          property HotTrack: Boolean
               read fHotTrack
               write fHotTrack
               default False;

          {: Overriden OnEnter that still works much the same way that
           the Standard TEdit Event does. }
          property OnEnter: TNotifyEvent
               read fOnEnter
               write fOnEnter;
          {: Overriden OnExit that still works much the same way that
   the Standard TEdit Event does. If Bounds Validation is Enabled,
   then it will be done before calling the inherited OnEnter. }
          property OnExit: TNotifyEvent
               read fOnExit
               write fOnExit;
          {: Overriden OnKeyPress that still works much the same way that
           the Standard TEdit Event does. }
          property OnKeyPress: TKeyPressEvent
               read fOnKeyPress
               write fOnKeyPress;
          {: This event is called if AllowKbdNull is true, and Ctrl-N is
           entered from the keyboard. }
          property OnNull: TNotifyEvent
               read fOnNull
               write fOnNull;
          {: Event called prior at the start of the Control Notification
           of key Down. This allows Tabs to be trapped. }
          property OnCNKeyDown: TKeyEvent
               read fOnCNKeyDown
               write fOnCNKeyDown;
          {: Event is called when the Mouse enters the Control's Area. }
          property OnMouseEnter: TNotifyEvent
               read fOnMouseEnter
               write fOnMouseEnter;
          {: Event is called when the Mouse leaves the Control's Area. }
          property OnMouseExit: TNotifyEvent
               read fOnMouseExit
               write fOnMouseExit;

          //: Published property from TCustomEdit
          property AutoSize;
          //: Published property from TCustomEdit
          property Font;
          //: Published property from TCustomEdit
          property ParentFont;
     end;

type
     {: Enhanced Edit Control with Enhanced Color Changing.
      Includes Methods to return the Text already Trimmed and in
      different cases.<p>
      ColorRW replaces the normal Color Property of Standard Edit Controls.<p>
      By default Read Only fields will be shown in a different Color,
      to disable this set ColorRW and ColorRO to the same, eg clWindow.
      clBtnFace is often a better choice for ColorRO on older Video Cards
      and older Notebooks<p>
  ColorFocus can be used so that the Color of the edit field changes
      when it receives focus (provided it is not ReadOnly). To use this feature
      ColorFocus must be set to a different value than ColorRW but beware
      the various Color combinations that result.<p>
      ColorDisabled controls the Color of the Control when Disabled, ie
      Enabled := False.<p>
      OnMouseEnter & OnMouseExit - allow you to set up your own "hot" controls
  if the Flat look'n'feel is not what you are after.<p>
      Null allows an edit field to be marked as having no proper value,
      and it will then display whatever NullStr is set to. Ctrl-N is
      the Keyboard entry for Null if AllowKbdNull is true. OnNullEvent
      is called when the Keyboard entry of Null is permitted.<p>
      If <See Var=ESBEnterAsTab> is true then the Enter Key will be treated
      as though it were the Tab Key.<p>
  If <See Var=ESBEscAsUndo> is true then the Esc Key will be cause an Undo
      to occur in the field - requires Delphi 7 or Kylix 3.<p>
      If Arrows is False and <See Var=ESBArrowsAsTab> is true then Up Arrow will move to previous
      field like Shift-Tab, and Down Arrow will move to next field like
      Tab. }
     TESBPCSEdit = class (TESBCustomEdit)
     private
          fProperAdjust: Boolean;
          fTrimTrailing: Boolean;
          fTrimLeading: Boolean;
          fAutoAdvance: Boolean;
     protected
          procedure DoExit; override;
          procedure KeyUp (var Key: Word; Shift: TShiftState); override;
     public
          //: Creates the Edit Component.
          constructor Create (AOwner: TComponent); override;

          //: Returns the Text in Lower Case using AnsiLowerCase. Result is also Trimmed.
          function LowerText: string;
          {: Returns the Text in Proper Case using <See Routine=ESBProperStr>.
           Result is also Trimmed.}
          function ProperText: string;
          //: Returns the Text trimmed of leading and trailing spaces.
          function TrimmedText: string;
          //: Returns the Text in Upper Case using AnsiUpperCase. Result is also Trimmed
          function UpperText: string;
     published
          {: When True and MaxLength > 0 then when the last character is
   entered the focus moves to the next control. }
          property AutoAdvance: Boolean
               read fAutoAdvance
               write fAutoAdvance
               default False;
          {: At Runtime, if on Exiting the Field and the Field is not ReadOnly
           and has Value of '' then it will be set to Null if this property
           is true. Similarly if the field is set to ''. }
          property BlankIsNull: Boolean
               read fBlankIsNull
               write fBlankIsNull
               default False;
          {: When True Trims trailing spaces when the control is exited. }
          property TrimTrailing: Boolean
               read fTrimTrailing
               write fTrimTrailing
               default True;
          {: When True Trims leading spaces when the control is exited. }
          property TrimLeading: Boolean
               read fTrimLeading
               write fTrimLeading
               default False;
          {: When True converts text into Proper Case when the control
           is exited. Uses <See Routine=ESBProperStr>. }
          property ProperAdjust: Boolean
               read fProperAdjust
               write fProperAdjust
               default False;
          {: Overriden Text that still works the same way that
           the Standard TEdit property does. }
          property Text
               stored ValueStored;

          //: Published property from TCustomEdit
          property Align;
          //: Published property from TCustomEdit
          property Alignment;
          //: Published property from TCustomEdit
          property Anchors;
          //: Published property from TCustomEdit
          property AutoSelect;
          //: Published property from TCustomEdit
          property AutoSize;
          //: Published property from TCustomEdit
          property BorderStyle;
          //: Published property from TCustomEdit
          property CharCase;
          //property Color;
          //: Published property from TCustomEdit
          property Constraints;
          //: Published property from TCustomEdit
          property DragMode;
          //: Published property from TCustomEdit
          property EchoMode;
          //: Published property from TCustomEdit
          property Enabled;
          //: Published property from TCustomEdit
          property HideSelection;
          //: Published property from TCustomEdit
          property MaxLength;
          //: Published property from TCustomEdit
          property ParentShowHint;
          //: Published property from TCustomEdit
          property PopupMenu;
          //: Published property from TCustomEdit
          property ShowHint;
          //: Published property from TCustomEdit
          property TabOrder;
          //: Published property from TCustomEdit
          property TabStop;
          //: Published property from TCustomEdit
          property Visible;
          //: Published property from TCustomEdit
          property OnClick;
          {$IFNDEF BelowD5}
          //: Published property from TCustomEdit
          property OnContextPopup;
          {$ENDIF}
          //: Published property from TCustomEdit
          property OnChange;
          //: Published property from TCustomEdit
          property OnDblClick;
          //: Published property from TCustomEdit
          property OnDragDrop;
          //: Published property from TCustomEdit
          property OnDragOver;
          //: Published property from TCustomEdit
          property OnEndDrag;
          //: Published property from TCustomEdit
          property OnKeyDown;
          //: Published property from TCustomEdit
          property OnKeyString;
          //: Published property from TCustomEdit
          property OnKeyUp;
          //: Published property from TCustomEdit
          property OnMouseDown;
          //: Published property from TCustomEdit
          property OnMouseMove;
          //: Published property from TCustomEdit
          property OnMouseUp;
          //: Published property from TCustomEdit
          property OnMouseWheel;
          //: Published property from TCustomEdit
          property OnMouseWheelDown;
          //: Published property from TCustomEdit
          property OnMouseWheelUp;
          //: Published property from TCustomEdit
          property OnReturnPressed;
          //: Published property from TCustomEdit
          property OnStartDrag;
     end;

     {: Enhanced Base Edit Control that forms the basis for the various
      Type Specific Edit Controls. This Component is not actually
          used itself but as the Parent of the other controls. <p>
          This adds Replacement Methods and framework for Bounds Validation.<p>
          Edit Control has ReadOnly Color Changing.
          Includes Methods to return the Text already Trimmed and in
          different cases.<p>
          OnMouseEnter & OnMouseExit - allow you to set up your own "hot" controls
          if the Flat look'n'feel is not what you are after.<p>
          Null allows an edit field to be marked as having no proper value,
          and it will then display whatever NullStr is set to. Ctrl-N is
          the Keyboard entry for Null if AllowKbdNull is true. OnNullEvent
          is called when the Keyboard entry of Null is permitted.<p>
      ColorRW replaces the normal Color Property of Standard Edit Controls.<p>
          By default Read Only fields will be shown in a different Color,
          to disable this set ColorRW and ColorRO to the same, eg clWindow.
          clBtnFace is often a better choice for ColorRO on older Video Cards
      and older Notebooks<p>
          ColorFocus can be used so that the Color of the edit field changes
          when it receives focus (provided it is not ReadOnly). To use this feature
          ColorFocus must be set to a different value than ColorRW but beware
          the various Color combinations that result.<p>
          ColorDisabled controls the Color of the Control when Disabled, ie
          Enabled := False.<p>
          If <See Var=ESBEnterAsTab> is true then the Enter Key will be treated
          as though it were the Tab Key.<p>
  If <See Var=ESBEscAsUndo> is true then the Esc Key will be cause an Undo
          to occur in the field - requires Delphi 7 or Kylix 3.<p>
          If Arrows is False and <See Var=ESBArrowsAsTab> is true then Up Arrow will move to previous
      field like Shift-Tab, and Down Arrow will move to next field like
          Tab. }
     TESBBaseEdit = class (TESBCustomEdit)
     private
          fMaxLength: Integer; // Replacement MaxLength for Zero padding

          fOnBoundsError: TESBBoundsValidationEvent; // User defined Routine when Bounds Validation problem occurs
          fOnExitStart: TESBExitStartEvent; // Called at the beginning of DoExit
     protected
          fBoundsValidation: Boolean; // Is Bounds Validation On
          fBoundsValidationType: TESBBoundsValidationType; // What sort of Validation problem
          fCheckforErrors: Boolean; // Check for errors in Conversions
          fColorFontNeg: TColor; // Font Color to display negative numbers
          fColorFontPos: TColor; // Font Color to display non-negative numbers
          fConvertOK: Boolean; // True when component has not been entered or has been successfully exited.
          fIgnoreConvertError: Boolean; // Controls how FConvertError is handled
          fOnConvertError: TESBConvertErrorEvent; // Called if a Conversion error occurs on Exiting
          fYearValidation: Boolean; // Enable Year Bounds Validation

          procedure Change; override;
          procedure DoEnter; override;
          procedure DoExit; override;
          procedure DoBoundsValidation; virtual;
          procedure DisplayText; override;
          procedure KeyPress (var Key: Char); override;

          procedure SetBoundsValidation (Value: Boolean); virtual;
          procedure SetColorFontNeg (Value: TColor);
          procedure SetColorFontPos (Value: TColor);
          function GetMaxLength: Integer;
          procedure SetMaxLength (Value: Integer);
          function GetConvertOK: Boolean;
          function GetYearValidation: Boolean;
          function GetIgnoreConvertError: Boolean;
          procedure SetIgnoreConvertError (Value: Boolean);
          function GetOnConvertError: TESBConvertErrorEvent;
          procedure SetOnConvertError (Value: TESBConvertErrorEvent);

          procedure SetColors2Defaults; override;
     public
          //: Creates the Edit Component.
          constructor Create (AOwner: TComponent); override;

          {: Font Color for the field when it contains a Negative Value.
           Color is changed, if required, when the field is exited.
           Font.Color Property is ignored. }
          property ColorFontNeg: TColor
               read fColorFontNeg
               write SetColorFontNeg
               default DefNegFontColor2;
          {: Font Color for the field when it contains a non-Negative Value
           Color is changed, if required, when the field is exited.
           Font.Color Property is ignored }
          property ColorFontPos: TColor
               read fColorFontPos
               write SetColorFontPos
               default DefPosFontColor2;

     published
          {: This controls whether Bound Validation Checking and
           resultant Error Messages will be displayed. }
          property BoundsEnabled: Boolean
               read fBoundsValidation
               write SetBoundsValidation
               default False;
          {: Overriden MaxLength that still works the same way that
           the Standard TEdit property does. For ZeroPad to work
           MaxLength must be greater 0. }
          property MaxLength: Integer
               read GetMaxLength
               write SetMaxLength
               default 0;

          {: Allows the User to handle the Bounds Validation Error. Event will only
           be called when there Validation is Enabled and there is
           a Validation Error. }
          property OnBoundsError: TESBBoundsValidationEvent
               read fOnBoundsError
               write fOnBoundsError;
          {: Exit Start Event is called at the start of the DoExit but before the
   final conversion of text to value and before any Bounds Checking
           or Conversion Checking is done. Set FurtherChecking to False to
           turn off Conversion & Bounds Checking. The value of Text can be
           altered. }
          property OnExitStart: TESBExitStartEvent
               read fOnExitStart
               write fOnExitStart;

          //: Published property from TCustomEdit
          property Align;
          //: Published property from TCustomEdit
          property Alignment;
          //: Published property from TCustomEdit
          property Anchors;
          //: Published property from TCustomEdit
          property AutoSelect;
          //: Published property from TCustomEdit
          property AutoSize;
          //: Published property from TCustomEdit
          property BorderStyle;
          //: Published property from TCustomEdit
          property CharCase;
          //: Published property from TCustomEdit
          property Constraints;
          //: Published property from TCustomEdit
          property DragMode;
          //: Published property from TCustomEdit
          property EchoMode;
          //: Published property from TCustomEdit
          property Enabled;
          //: Published property from TCustomEdit
          property HideSelection;
          //: Published property from TCustomEdit
          property ParentShowHint;
          //: Published property from TCustomEdit
          property PopupMenu;
          //: Published property from TCustomEdit
          property ShowHint;
          //: Published property from TCustomEdit
          property TabOrder;
          //: Published property from TCustomEdit
          property TabStop;
          //: Published property from TCustomEdit
          property Visible;
          //: Published property from TCustomEdit
          property OnClick;
          {$IFNDEF BelowD5}
          //: Published property from TCustomEdit
          property OnContextPopup;
          {$ENDIF}
          //: Published property from TCustomEdit
          property OnChange;
          //: Published property from TCustomEdit
          property OnDblClick;
          //: Published property from TCustomEdit
          property OnDragDrop;
          //: Published property from TCustomEdit
          property OnDragOver;
          //: Published property from TCustomEdit
          property OnEndDrag;
          //: Published property from TCustomEdit
          property OnKeyDown;
          //: Published property from TCustomEdit
          property OnKeyString;
          //: Published property from TCustomEdit
          property OnKeyUp;
          //: Published property from TCustomEdit
          property OnMouseDown;
          //: Published property from TCustomEdit
          property OnMouseMove;
          //: Published property from TCustomEdit
          property OnMouseUp;
          //: Published property from TCustomEdit
          property OnMouseWheel;
          //: Published property from TCustomEdit
          property OnMouseWheelDown;
          //: Published property from TCustomEdit
          property OnMouseWheelUp;
          //: Published property from TCustomEdit
          property OnReturnPressed;
          //: Published property from TCustomEdit
          property OnStartDrag;
     end;

implementation

uses
     QT, QForms, SysUtils,
     QESBPCS_RS_Globals,
     QESBPCSConvert, QESBPCSMsgs;

constructor TESBCustomEdit.Create (AOwner: TComponent);
begin
     inherited Create (AOwner);
     fStrValue := '';
     fStoreValue := True;
     fNoDisplayUpdate := False;
     fInUpdate := False;
     fValidChar := [#32..#255];
     fValidCharHold := fValidChar;
     fNull := False;
     fNullStr := ESBNullStr;
     fAllowKbdNull := True;
     fBlankWhenNull := False;
     fColor_Defaults := True;
     fBlankIsNull := False;
     fHotTrack := False;
     SetColors2Defaults;
     UpdateColors;
end;

procedure TESBCustomEdit.SetColors2Defaults;
begin
     fColorRO := ESBROColor;
     fColorRW := ESBRWColor;
     fColorFocus := ESBFocusColor;
     fColorDisabled := ESBDisabledColor;
end;

procedure TESBCustomEdit.SetColor_Defaults (Value: Boolean);
begin
     fColor_Defaults := Value;
     if fColor_Defaults then
     begin
          SetColors2Defaults;
          UpdateColors;
     end;
end;

function TESBCustomEdit.ValueStored: Boolean;
begin
     Result := fStoreValue;
end;

procedure TESBCustomEdit.Convert2Value;
begin
     // Currently Do Nothing
end;

procedure TESBCustomEdit.SelectNone;
begin
     SelLength := 0;
     SelStart := 0;
end;

function TESBCustomEdit.IsClear: Boolean;
begin
     Result := (inherited Text = '') or fNull;
end;

function TESBCustomEdit.GetStrValue: string;
begin
     fStrValue := inherited Text;
     if Null then
          Result := ''
     else
          Result := fStrValue;
end;

procedure TESBCustomEdit.SetStrValue (const Value: string);
begin
     if fBlankIsNull and (Value = '') then
     begin
          fNull := True;
          DisplayText;
          Exit;
     end;

     if (Value <> fStrValue) then
     begin
          fStrValue := Value;

          if fNull then
          begin
               if fBlankWhenNull then
                    fNull := (Value = '')
               else
                    fNull := (Value = fNullStr);
          end;

          inherited Text := fStrValue;
          Convert2Value;
          if not fNoDisplayUpdate then
               DisplayText;
     end;
end;

procedure TESBCustomEdit.DisplayText;
begin
     if fNull then
     begin
          if fBlankWhenNull then
               inherited Text := ''
          else
               inherited Text := fNullStr
     end;
end;

procedure TESBCustomEdit.SetBlankWhenNull (Value: Boolean);
begin
     if fBlankWhenNull <> Value then
     begin
          fBlankWhenNull := Value;
          DisplayText;
     end;
end;

procedure TESBCustomEdit.PasteFromClipboard;
begin
     if not fReadOnly then
     begin
          fNull := False;
          inherited;
     end;
end;

procedure TESBCustomEdit.CutToClipboard;
begin
     if not fReadOnly then
     begin
          inherited;
     end;
end;

function TESBCustomEdit.StoreNullStr: Boolean;
begin
     Result := (fNullStr <> ESBNullStr) and not fBlankWhenNull;
end;

procedure TESBCustomEdit.SetNullStr (const Value: string);
begin
     if Value <> fNullStr then
     begin
          fNullStr := Value;
          DisplayText;
     end;
end;

procedure TESBCustomEdit.SetNull (Value: Boolean);
begin
     if Value <> fNull then
     begin
          fNull := Value;
          DisplayText;
     end;
end;

function TESBCustomEdit.GetVersion: string;
begin
     Result := ESBPCSVersion;
end;

procedure TESBCustomEdit.SetVersion (const Value: string);
begin
     // Do nothing
end;

procedure TESBCustomEdit.KeyPress (var Key: Char);
begin
     if Assigned (fOnKeyPress) then
          fOnKeyPress (Self, Key);
     KeyProcess (Key);
end;

procedure TESBCustomEdit.KeyProcess (var Key: Char);
begin
     if (Key = #13) and ESBEnterAsTab and not Assigned (OnReturnPressed) then
     begin
          Key := #0;
          QApplication_postEvent (Self.Handle, QKeyEvent_create (QEventType_KeyPress,
               Key_Tab, 14, 0, nil, False, 1));
     end
     else if (Key = #14) and fAllowKbdNull then
     begin
          SetNull (True);
          if Assigned (fOnNull) then
               fOnNull (Self);
          Key := #0;
     end
     else if Key = ^A then
     begin
          SelectAll;
          Key := #0
     end
     else if fValidChar <> [] then
     begin
          if (Key >= #32) and not (Key in fValidChar) then
          begin
               Key := #0;
               Beep;
          end
          else if (Key in fValidChar) then
          begin
               if fNull then
               begin
                    SetNull (False);
                    Clear;
               end;
          end
     end
     else if (Key >= #32) then
     begin
          Key := #0;
          Beep;
     end;
end;

procedure TESBCustomEdit.KeyDown (var Key: Word; Shift: TShiftState);
begin
     if Shift = [] then
     begin
          case Key of
               Key_Escape:
                    begin
                         if ESBEscAsUndo then
                         begin
                              Key := 0;
                              GetStrValue;
                              SetStrValue (fEnterValue);
                         end;
                    end;
               Key_Delete:
                    begin
                         if ReadOnly then
                              Key := 0;
                    end;
          end;
     end
     else if Shift = [ssShift] then
     begin
          case Key of
               Key_Insert:
                    begin
                         if ReadOnly then
                              Key := 0
                    end;
          end;
     end;

     if ESBArrowsAsTab then
     begin
          if Shift = [] then
          begin
               case Key of
                    Key_UP:
                         begin
                              Key := 0;
                              {$IFDEF LINUX}
                              QApplication_postEvent (Self.Handle, QKeyEvent_create (QEventType_KeyPress,
                                   Key_BackTab, 14, 0, nil, False, 1));
                              {$ELSE}
                              QApplication_postEvent (Self.Handle, QKeyEvent_create (QEventType_KeyPress,
                                   Key_Tab, 14, 10, nil, False, 1));
                              {$ENDIF}
                         end;
                    Key_DOWN:
                         begin
                              Key := 0;
                              QApplication_postEvent (Self.Handle, QKeyEvent_create (QEventType_KeyPress,
                                   Key_Tab, 14, 0, nil, False, 1));
                         end;
                    Key_Delete:
                         begin
                              if ReadOnly then
                                   Key := 0;
                         end;
               end;
          end;
     end;
     inherited KeyDown (Key, Shift);
end;

procedure TESBCustomEdit.SetEnabled (const Value: Boolean);
begin
     inherited SetEnabled (Value);
     UpdateColors
end;

procedure TESBCustomEdit.MouseEnter;
begin
     inherited;
     fMouseOver := True;
     if Assigned (fOnMouseEnter) then
          fOnMouseEnter (Self);
     UpdateColors;
end;

procedure TESBCustomEdit.MouseLeave;
begin
     fMouseOver := False;
     if Assigned (fOnMouseExit) then
          fOnMouseExit (Self);
     UpdateColors;
     inherited;
end;

procedure TESBCustomEdit.BeginUpdate;
begin
     fInUpdate := True;
end;

procedure TESBCustomEdit.EndUpdate;
begin
     fInUpdate := False;
     UpdateColors;
end;

procedure TESBCustomEdit.UpdateColors;
begin
     if fInUpdate then
          Exit;

     if fColorRW = clNone then
     begin
          ParentColor := False;
          ParentColor := True;
     end;

     if not Enabled then
     begin
          if fColorDisabled = clNone then
          begin
               if fColorRW <> clNone then
                    Color := ColorRW;
          end
          else
               Color := fColorDisabled;
     end
     else if fReadOnly then
     begin
          if fColorRO = clNone then
          begin
               if fColorRW <> clNone then
                    Color := ColorRW;
          end
          else
               Color := fColorRO
     end
     else if (fFocused or (fHotTrack and fMouseOver)) and (fColorFocus <> clNone) then
          Color := fColorFocus
     else if fColorRW <> clNone then
          Color := fColorRW;

     Invalidate;
end;

procedure TESBCustomEdit.SetColorFocus (Value: TColor);
begin
     if Value <> fColorFocus then
     begin
          fColor_Defaults := False;
          fColorFocus := value;
          UpdateColors;
     end;
end;

procedure TESBCustomEdit.SetColorDisabled (Value: TColor);
begin
     if Value <> fColorDisabled then
     begin
          fColor_Defaults := False;
          fColorDisabled := value;
          if not Enabled then
          begin
               Enabled := True; // Toggle Enabled to update Color
               Enabled := False;
          end;
     end;
end;

procedure TESBCustomEdit.SetColorRO (Value: TColor);
begin
     if Value <> fColorRO then
     begin
          fColor_Defaults := False;
          fColorRO := value;
          UpdateColors;
     end;
end;

procedure TESBCustomEdit.SetColorRW (Value: TColor);
begin
     if Value <> fColorRW then
     begin
          fColor_Defaults := False;
          fColorRW := value;
          UpdateColors;
     end;
end;

procedure TESBCustomEdit.SetReadOnly (Value: Boolean);
begin
     if fReadOnly <> Value then
     begin
          fReadOnly := Value;
          inherited ReadOnly := Value;
          if fReadOnly then
               fValidChar := []
          else
               fValidChar := fValidCharHold;
          if ESBNoTabStopOnReadOnly then
               TabStop := not fReadOnly;
          UpdateColors;
          Invalidate;
     end;
end;

procedure TESBCustomEdit.DoEnter;
begin
     fEnterValue := Text;
     fFocused := True;
     UpdateColors;
     if AutoSelect then
          SelectAll; // When entering the field start with whole field selected
     // So overwriting will take place if typing starts
     if Assigned (fOnEnter) then
          fOnEnter (Self);
end;

procedure TESBCustomEdit.DoExit;
begin
     fFocused := False;
     UpdateColors;
     if Assigned (fOnExit) then
          fOnExit (Self);
end;

procedure TESBCustomEdit.MouseUp (Button: TMouseButton; Shift: TShiftState;
     X, Y: Integer);
begin
     inherited;
     if Button = mbRight then
     begin
          fFocused := False;
          fMouseOver := False;
          UpdateColors;
     end;
end;

constructor TESBPCSEdit.Create (AOwner: TComponent);
begin
     inherited Create (AOwner);
     fTrimTrailing := True;
     fTrimLeading := False;
     fProperAdjust := False;
     fAutoAdvance := False;
end;

procedure TESBPCSEdit.DoExit;
var
     S: string;
begin
     if not fNull then
     begin
          S := Text;
          if fTrimTrailing then
               S := TrimRight (S);
          if fTrimLeading then
               S := TrimLeft (S);
          if fProperAdjust then
               S := ESBProperStr (S);
          if S <> Text then
               Text := S;

          if fBlankIsNull and (S = '') then
          begin
               fNull := True;
               if fBlankWhenNull then
                    Text := ''
               else
                    Text := fNullStr
          end;
     end;
     inherited DoExit;
end;

function TESBPCSEdit.LowerText: string;
begin
     Result := AnsiLowerCase (Trim (Text));
end;

function TESBPCSEdit.TrimmedText: string;
begin
     Result := Trim (Text);
end;

function TESBPCSEdit.UpperText: string;
begin
     Result := AnsiUpperCase (Trim (Text));
end;

function TESBPCSEdit.ProperText: string;
begin
     Result := ESBProperStr (Trim (Text));
end;

procedure TESBPCSEdit.KeyUp (var Key: Word; Shift: TShiftState);
begin
     inherited KeyUp (Key, Shift);

     if not (csLoading in ComponentState) and (fAutoAdvance = True) and
          (MaxLength > 0) and (MaxLength = Length (Text)) and (Key >= 32) then
     begin
          QApplication_postEvent (Self.Handle, QKeyEvent_create (QEventType_KeyPress,
               Key_Tab, 14, 0, nil, False, 1));
     end;
end;

constructor TESBBaseEdit.Create (AOwner: TComponent);
begin
     inherited Create (AOwner);
     fBoundsValidation := False;
     fBoundsValidationType := evtNone;
     fMaxLength := 0;
     fCheckforErrors := False;
     Width := 81;
     fConvertOK := True;
end;

procedure TESBBaseEdit.SetColors2Defaults;
begin
     inherited SetColors2Defaults;
     fColorFontNeg := ESBNegFontColor;
     fColorFontPos := ESBPosFontColor;
     DisplayText;
end;

function TESBBaseEdit.GetConvertOK: Boolean;
begin
     Result := fConvertOK;
end;

function TESBBaseEdit.GetYearValidation: Boolean;
begin
     Result := fYearValidation;
end;

function TESBBaseEdit.GetOnConvertError: TESBConvertErrorEvent;
begin
     Result := fOnConvertError;
end;

procedure TESBBaseEdit.SetOnConvertError (Value: TESBConvertErrorEvent);
begin
     fOnConvertError := Value;
end;

function TESBBaseEdit.GetIgnoreConvertError: Boolean;
begin
     Result := fIgnoreConvertError;
end;

procedure TESBBaseEdit.SetIgnoreConvertError (Value: Boolean);
begin
     fIgnoreConvertError := Value;
end;

procedure TESBBaseEdit.Change;
begin
     if not (csLoading in ComponentState) then
     begin
          Convert2Value;
          inherited Change;
     end;
end;

procedure TESBBaseEdit.DoEnter;
begin
     inherited DoEnter;
     fConvertOK := False;
end;

procedure TESBBaseEdit.DoExit;
var
     FurtherChecking: Boolean;
begin
     FurtherChecking := True;
     if Assigned (fOnExitStart) then
     begin
          fOnExitStart (Self, fStrValue, FurtherChecking);
          inherited Text := fStrValue;
     end;
     if not fIgnoreConvertError then
     begin
          try
               fCheckforErrors := FurtherChecking;
               Convert2Value;
          except
               fCheckforErrors := False;
               if Assigned (fOnConvertError) then
               begin
                    fOnConvertError (Self, Text);
                    Self.SetFocus;
                    Exit;
               end
               else
                    raise;
          end;

          fCheckforErrors := False;
          DisplayText; // and that the Edit Box displays properly formatted value

          if (fBoundsValidation or fYearValidation) and FurtherChecking then // Handle Bounds Validation if enabled
               DoBoundsValidation;
     end;

     fConvertOK := True;
     inherited DoExit;
end;

function TESBBaseEdit.GetMaxLength: Integer;
begin
     fMaxLength := inherited MaxLength;
     Result := fMaxLength;
end;

procedure TESBBaseEdit.SetMaxLength (Value: Integer);
begin
     if Value <> fMaxLength then
     begin
          fMaxLength := Value;
          inherited MaxLength := Value;
          DisplayText;
     end;
end;

procedure TESBBaseEdit.SetColorFontNeg (Value: TColor);
begin
     if fColorFontNeg <> Value then
     begin
          fColor_Defaults := False;
          fColorFontNeg := Value;
          DisplayText;
     end;
end;

procedure TESBBaseEdit.SetColorFontPos (Value: TColor);
begin
     if fColorFontPos <> Value then
     begin
          fColor_Defaults := False;
          fColorFontPos := Value;
          DisplayText;
     end;
end;

procedure TESBBaseEdit.SetBoundsValidation (Value: Boolean);
begin
     if fBoundsValidation <> Value then
     begin
          fBoundsValidation := Value;
     end;
end;

procedure TESBBaseEdit.DoBoundsValidation;
begin
     // This is really a dummy routine as it should normally be overriden
     if fBoundsValidation then
     begin
          if Assigned (fOnBoundsError) then
               fOnBoundsError (Self, Text, fBoundsValidationType)
          else
               ErrorMsg (rsUnaccept);
     end;
end;

procedure TESBBaseEdit.KeyPress (var Key: Char);
begin
     if Assigned (fOnKeyPress) then
          fOnKeyPress (Self, Key);

     KeyProcess (Key);
end;

procedure TESBBaseEdit.DisplayText;
begin
     if fNull then
     begin
          if fBlankWhenNull then
          begin
               inherited Text := '';
               Text := ''
          end
          else
          begin
               inherited Text := fNullStr;
               Text := fNullStr
          end;
     end
     else
          Text := inherited Text;
end;

end.
