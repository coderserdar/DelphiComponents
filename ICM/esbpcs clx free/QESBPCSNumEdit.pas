{: Contains the Numeric Edit Components for ESBPCS for CLX.

 This is designed to work in Borland Delphi 6 CLX and above, Borland
 C++ Builder 6 CLX and above, and Borland Kylix 2 and above.
 Most if not all features will work in Kylix 1 but it is not currently supported.<p>

 This suite of Edit Components is aimed at making Data Entry of Integers
 and Floats easier by supporting Alignment, Read Only Colouring, On Focus
 Colouring, Handling of Enter as Tab, Handling of Arrow Keys as Tabs,
 formatting options dependent upon Data Type, Methods to aid in manipulation
 of Data Types.<p>

 For General Edit Components see <See Unit=ESBPCSEdit>. <p>

 Copyright © 1999-2002 ESB Consultancy<p>

 v2.3 - 14 September 2002
}
unit QESBPCSNumEdit;

interface
{$I esbpcs.inc}

uses
     Classes, QControls, QStdCtrls,
     QESBPCSGlobals, QESBPCSGlobals2, QESBPCSEdit;

type
     {: Enhanced Base Edit Control that forms the basis for the various
      Numerics Edit Controls. This Component is not actually
      used itself but as the Parent of the other controls.<p>
      This adds Displaying a Blank Field when the value is Zero,
      framework for handling of Floats and Integers.<p>
      Edit Control has Alignment and ReadOnly Color Changing.
      Includes Bounds Validation.<p>
      Flat controls whether the control has a MS Office 97/2000 type behaviour,
      where the "look" changes when the control has focus or the mouse passes
      over it. ColorBorderFocus & ColorBorderUnfocus are used for Border colors
      when the Control is Flat.<p>
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
      ColorFontPos and ColorFontNeg can be used so that Positives (or to
      be more precise non-negatives) and Negatives appear in a different
      Color.<p>
      If <See Var=ESBEnterAsTab> is true then the Enter Key will be treated as
      though it were the Tab Key.<p>
      If <See Var=ESBEscAsUndo> is true then the Esc Key will be cause an Undo
      to occur in the field.<p>
      If <See Var=ESBArrowsAsTab> is true then Up Arrow will move to previous
      field like Shift-Tab, and Down Arrow will move to next field like
      Tab. }
     TESBBaseNumericEdit = class (TESBBaseEdit)
     private
          fBlankWhenZero: Boolean; // Should Zeros be displayed as Blanks
          fFloatValue: Extended; // Value as Extended Float
          fIntValue: Int64; // Value as an Int64
          fShowPosSign: Boolean; // Should a Positive Sign be displayed
          fUseSignToggling: Boolean; // Should Sign Toggling be used
          fZeroPad: Boolean; // Should ZeroPadding be used
          fThousandSeparators: TESBSeparatorStyle;
     protected
          fCustomSeparators: Boolean;
          fCustomThousands: Char;
          fCustomDecimal: Char;
          fDecimals: Byte; // Number of Decimal Places
          fForceDecimals: Boolean; // Forces the Number of Decimal Places that can be entered
          procedure KeyPress (var Key: Char); override;
          procedure KeyUp (var Key: Word; Shift: TShiftState); override;

          procedure DoThousands; virtual;

          procedure SetBlankWhenZero (Value: Boolean);
          procedure SetDecimals (Value: Byte);
          function GetFloatValue: Extended; virtual;
          procedure SetFloatValue (const Value: Extended); virtual;
          function GetIntValue: Integer;
          procedure SetIntValue (Value: Integer);
          function GetInt64Value: Int64;
          procedure SetInt64Value (Value: Int64);
          function GetLWordValue: LongWord;
          procedure SetLWordValue (Value: LongWord);
          procedure SetShowPosSign (Value: Boolean);
          procedure SetSignToggling (Value: Boolean);
          procedure SetThousandSeparators (Value: TESBSeparatorStyle);
          procedure SetZeroPad (Value: Boolean);
          procedure SetCustomSeparators (Value: Boolean); virtual;
          procedure SetCustomThousands (Value: Char); virtual;
          procedure SetCustomDecimal (Value: Char); virtual;

          function StoreCustomSeparators: Boolean;
          procedure UpdateValidChars; virtual;
     public
          //: Creates the Edit Component.
          constructor Create (AOwner: TComponent); override;
     published
          {: When enabled, the Edit Box will display "Empty" when the
           Value is Zero. }
          property BlankWhenZero: Boolean
               read fBlankWhenZero
               write SetBlankWhenZero
               default False;
          {: Font Color for the field when it contains a Negative Value.
           Color is changed, if required, when the field is exited.
           Font.Color Property is ignored. }
          property ColorFontNeg;
          {: Font Color for the field when it contains a non-Negative Value
           Color is changed, if required, when the field is exited.
           Font.Color Property is ignored. }
          property ColorFontPos;
          {: When enabled, the Edit Box will display a '+' sign for
           Positive Values. }
          property ShowPosSign: Boolean
               read fShowPosSign
               write SetShowPosSign
               default False;
          {: Controls how Thousand Separators as defined in your Regional Settings,
           are displayed in the control.
           This property should be used instead of ShowThousandSeparators.
                   }
          property ThousandSeparators: TESBSeparatorStyle
               read fThousandSeparators
               write SetThousandSeparators
               default essNone;
          {: When Enabling, Pressing '+' will Toggle ShowPosSign.
           Pressing '-' will toggle negativity. }
          property UseSignToggling: Boolean
               read fUseSignToggling
               write SetSignToggling
               default True;
          {: When Enabled and MaxLength greater than 0 will LeftPad with Zeros if
           necessary. }
          property ZeroPad: Boolean
               read fZeroPad
               write SetZeroPad
               default False;
     end;

     {: Enhanced Edit Control that only allows Positive Integers
      to be entered. Use AsInteger to Read/Write the Integer Value.
      ThousandSeparators can control the output, Thousand Separators
      are ignored in input.<p>
      Sign Toggling means if a '+' is pressed then it is displayed,
      and if pressed again it disappears. If Sign Toggling is disabled
      ShowPosSign can be set to show '+' signs<p>
      BlankWhenZero displays a Blank Field when the value is Zero.<p>
      ZeroPad is used with MaxLength (can't be 0) to fill the field
      to the left with Zeroes. <p>
      Edit Control also has Alignment and ReadOnly Color Changing.
      Includes Bounds Validation.<p>
      Flat controls whether the control has a MS Office 97/2000 type behaviour,
      where the "look" changes when the control has focus or the mouse passes
      over it. ColorBorderFocus & ColorBorderUnfocus are used for Border colors
      when the Control is Flat.<p>
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
  If <See Var=ESBEnterAsTab> is true then the Enter Key will be treated as
  though it were the Tab Key.<p>
  If <See Var=ESBEscAsUndo> is true then the Esc Key will be cause an Undo
  to occur in the field.<p>
  If <See Var=ESBArrowsAsTab> is true then Up Arrow will move to previous
  field like Shift-Tab, and Down Arrow will move to next field like
  Tab. }
     TESBPosEdit = class (TESBBaseNumericEdit)
     private
          FBoundLower: Integer; // Lower Bound for Bounds Validation
          FBoundUpper: Integer; // Upper Bound for Bounds Validation
     protected
          procedure Convert2Value; override;
          procedure DisplayText; override;
          procedure DoBoundsValidation; override;
          procedure UpdateValidChars; override;
     public
          //: Creates the Edit Component.
          constructor Create (AOwner: TComponent); override;

     published
          //: Allows access to the Edit Field as an Integer.
          property AsInteger: Integer
               read GetIntValue
               write SetIntValue
               stored false;
          //: Allows access to the Edit Field as an Int64.
          property AsInt64: Int64
               read GetInt64Value
               write SetInt64Value
               stored false;
          //: Allows access to the Edit Field as a LongWord.
          property AsLongWord: LongWord
               read GetLWordValue
               write SetLWordValue
               stored ValueStored
               default 0;

          {: If Validation is Enabled, then this value is used for
           rejecting any values less than it. }
          property BoundLower: Integer
               read FBoundLower
               write FBoundLower
               default 0;
          {: If Validation is Enabled, then this value is used for
           rejecting any values greater than it. }
          property BoundUpper: Integer
               read FBoundUpper
               write FBoundUpper
               default 0;
          {: When CustomSeparators is True, this value is used for the Thousands
           Separator rather than the System Value.
           }
          property CustomThousands: Char
               read fCustomThousands
               write SetCustomThousands
               stored StoreCustomSeparators;
          {: When True CustomThousands and CustomDecimal are used for the
           Thousands Separator and Decimal Separator respectively rather
           than the System Values.
           }
          property CustomSeparators: Boolean
               read fCustomSeparators
               write SetCustomSeparators
               default False;
     end;

     {: Enhanced Edit Control that only allows Integers (Positive & Negative)
      to be entered. Use AsInteger to Read/Write the Integer Value
      ThousandSeparators can control the output, Thousand Separators
      are ignored in input.<p>
      Sign Toggling means if a '+' is pressed then it is displayed,
      and if pressed again it disappears. Similarly with a '-' sign.
      If Sign Toggling is disabled ShowPosSign can be set to show '+'
      sign<p>
      BlankWhenZero displays a Blank Field when the value is Zero.<p>
      ZeroPad is used with MaxLength (can't be 0) to fill the field
      to the left with Zeroes. <p>
      Edit Control also has Alignment and ReadOnly Color Changing.
      Includes Bounds Validation.<p>
      Flat controls whether the control has a MS Office 97/2000 type behaviour,
      where the "look" changes when the control has focus or the mouse passes
      over it. ColorBorderFocus & ColorBorderUnfocus are used for Border colors
      when the Control is Flat.<p>
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
      If <See Var=ESBEnterAsTab> is true then the Enter Key will be treated as
      though it were the Tab Key.<p>
      If <See Var=ESBEscAsUndo> is true then the Esc Key will be cause an Undo
      to occur in the field.<p>
      If <See Var=ESBArrowsAsTab> is true then Up Arrow will move to previous
      field like Shift-Tab, and Down Arrow will move to next field like
      Tab. }
     TESBIntEdit = class (TESBPosEdit)
     protected
          procedure UpdateValidChars; override;
     public
          //: Creates the Edit Component.
          constructor Create (AOwner: TComponent); override;
     published
          //: Allows access to the Edit Field as an Integer.
          property AsInteger
               stored ValueStored;
     end;

     {: Enhanced Edit Control that only allows Positive Floats (no Exponents)
      to be entered. Use AsFloat to Read/Write the Extended Value
      ThousandSeparators can control the output, Thousand Separators
      are ignored in input.<p>
      Decimals controls the number of decimal places shown/stored.<p>
      Sign Toggling means if a '+' is pressed then it is displayed,
      and if pressed again it disappears. If Sign Toggling is disabled
      ShowPosSign can be set to show '+' signs<p>
      BlankWhenZero displays a Blank Field when the value is Zero.<p>
      ZeroPad is used with MaxLength (can't be 0) to fill the field
      to the left with Zeroes. <p>
      Edit Control also has Alignment and ReadOnly Color Changing.
      Includes Methods to return the Text already Trimmed and in
      different cases, as well as Bounds Validation.<p>
      Flat controls whether the control has a MS Office 97/2000 type behaviour,
      where the "look" changes when the control has focus or the mouse passes
      over it. ColorBorderFocus & ColorBorderUnfocus are used for Border colors
      when the Control is Flat.<p>
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
      If <See Var=ESBEnterAsTab> is true then the Enter Key will be treated as
      though it were the Tab Key.<p>
      If <See Var=ESBEscAsUndo> is true then the Esc Key will be cause an Undo
      to occur in the field.<p>
      If <See Var=ESBArrowsAsTab> is true then Up Arrow will move to previous
      field like Shift-Tab, and Down Arrow will move to next field like
      Tab. }
     TESBPosFloatEdit = class (TESBBaseNumericEdit)
     private
          FBoundLower: Extended; // Lower Bound for Bounds Validation
          FBoundUpper: Extended; // Upper Bound for Bounds Validation
          FFullAccuracy: Boolean; // Value stored to full Accuracy
          FScale: Extended; // Value that the Displayed value is Scaled by
          fTrimTrailingZeroes: Boolean;
     protected
          procedure Convert2Value; override;
          procedure DisplayText; override;
          procedure DoBoundsValidation; override;
          procedure SetFullAccuracy (Value: Boolean);
          procedure SetForceDecimals (Value: Boolean);
          procedure SetScale (const Value: Extended);
          function GetScaledFloat: Extended;
          procedure SetScaledFloat (const Value: Extended);
          procedure SetScaledFloat2 (const Value: Extended);
          procedure SetTrimTrailingZeroes (Value: Boolean);
          procedure UpdateValidChars; override;
     public
          //: Creates the Edit Component.
          constructor Create (AOwner: TComponent); override;

     published
          {: When enabled, Trailing Zeroes to the right of the decimal point are removed,
          and if no decimal places at all then the Decimal Point is removed. }
          property TrimTrailingZeroes: Boolean
               read fTrimTrailingZeroes
               write SetTrimTrailingZeroes
               default False;
          {: Underlying Value stored to Full Accuracy. If you want the various
           properties to return the value to 	the same accuracy as displayed
           then set FullAccuracy property to false. If you want the underlying
           value to always be to as many decimal places as possible set this
           to true. }
          property FullAccuracy: Boolean
               read FFullAccuracy
               write SetFullAccuracy
               default False;
          {: When true doesn't allow the user to enter more than the specified
           number of decimal places. }
          property ForceDecimals: Boolean
               read fForceDecimals
               write SetForceDecimals
               default False;
          {: Value Displayed is the Underlying Value divided by Scale.
           Set to 1 for no Scale to by Applied. }
          property Scale: Extended
               read FScale
               write SetScale;
          //: Allows access to the Edit Field as a Float.
          property AsFloat: Extended
               read GetFloatValue
               write SetFloatValue
               stored ValueStored;
          //: Allows access to the Scaled Value (Displayed)
          property AsScaledFloat: Extended
               read GetScaledFloat
               write SetScaledFloat
               stored false;
          {: If Validation is Enabled, then this value is used for
           rejecting any values less than it. }
          property BoundLower: Extended
               read FBoundLower
               write FBoundLower;
          {: If Validation is Enabled, then this value is used for
           rejecting any values greater than it. }
          property BoundUpper: Extended
               read FBoundUpper
               write FBoundUpper;
          //: Number of Decimal Places to be displayed.
          property DecimalPlaces: Byte
               read fDecimals
               write SetDecimals
               default 4;
          {: When CustomSeparators is True, this value is used for the Decimal
           Separator rather than the System Value.
           }
          property CustomDecimal: Char
               read fCustomDecimal
               write SetCustomDecimal
               stored StoreCustomSeparators;
          {: When CustomSeparators is True, this value is used for the Thousands
           Separator rather than the System Value.
           }
          property CustomThousands: Char
               read fCustomThousands
               write SetCustomThousands
               stored StoreCustomSeparators;
          {: When True CustomThousands and CustomDecimal are used for the
           Thousands Separator and Decimal Separator respectively rather
           than the System Values.
           }
          property CustomSeparators: Boolean
               read fCustomSeparators
               write SetCustomSeparators
               default False;
     end;

     {: Enhanced Edit Control that only allows Floats (Positive & Negative)
      to be entered. Use AsFloat to Read/Write the Float Value
      ThousandSeparators can control the output, Thousand Separators
      are ignored in input.<p>
      Decimals controls the number of decimal places shown/stored.<p>
      Sign Toggling means if a '+' is pressed then it is displayed,
      and if pressed again it disappears. If Sign Toggling is disabled
      ShowPosSign can be set to show '+' signs<p>
      BlankWhenZero displays a Blank Field when the value is Zero.<p>
      ZeroPad is used with MaxLength (can't be 0) to fill the field
      to the left with Zeroes. <p>
      Edit Control also has Alignment and ReadOnly Color Changing.
      Includes Bounds Validation.<p>
      Flat controls whether the control has a MS Office 97/2000 type behaviour,
      where the "look" changes when the control has focus or the mouse passes
      over it. ColorBorderFocus & ColorBorderUnfocus are used for Border colors
      when the Control is Flat.<p>
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
      If <See Var=ESBEnterAsTab> is true then the Enter Key will be treated as
      though it were the Tab Key.<p>
      If <See Var=ESBEscAsUndo> is true then the Esc Key will be cause an Undo
      to occur in the field.<p>
      If <See Var=ESBArrowsAsTab> is true then Up Arrow will move to previous
      field like Shift-Tab, and Down Arrow will move to next field like
      Tab. }
     TESBFloatEdit = class (TESBPosFloatEdit)
     protected
          procedure UpdateValidChars; override;
     public
          //: Creates the Edit Component.
          constructor Create (AOwner: TComponent); override;
     end;

     {: Enhanced Edit Control that only allows Floats (Positive & Negative)
      to be entered as percentages - so the underlying value is the
      displayed divided by 100. Use AsFloat to Read/Write the Float Value
      ThousandSeparators can control the output, Thousand Separators
      are ignored in input.<p>
      Decimals controls the number of decimal places shown/stored.<p>
      Sign Toggling means if a '+' is pressed then it is displayed,
      and if pressed again it disappears. If Sign Toggling is disabled
      ShowPosSign can be set to show '+' signs<p>
      BlankWhenZero displays a Blank Field when the value is Zero.<p>
      ZeroPad is used with MaxLength (can't be 0) to fill the field
      to the left with Zeroes. <p>
      Edit Control also has Alignment and ReadOnly Color Changing.
      Includes Bounds Validation.<p>
      Flat controls whether the control has a MS Office 97/2000 type behaviour,
      where the "look" changes when the control has focus or the mouse passes
      over it. ColorBorderFocus & ColorBorderUnfocus are used for Border colors
      when the Control is Flat.<p>
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
      If <See Var=ESBEnterAsTab> is true then the Enter Key will be treated as
      though it were the Tab Key.<p>
      If <See Var=ESBEscAsUndo> is true then the Esc Key will be cause an Undo
      to occur in the field.<p>
      If <See Var=ESBArrowsAsTab> is true then Up Arrow will move to previous
      field like Shift-Tab, and Down Arrow will move to next field like
      Tab. }
     TESBPercentEdit = class (TESBBaseNumericEdit)
     private
          FBoundLower: Extended; // Lower Bound for Bounds Validation
          FBoundUpper: Extended; // Upper Bound for Bounds Validation
          FFullAccuracy: Boolean; // Value stored to full Accuracy
          FScaled: Boolean;
          fShowPercent: Boolean;
          fTrimTrailingZeroes: Boolean;
     protected
          procedure Convert2Value; override;
          procedure DisplayText; override;
          procedure DoBoundsValidation; override;
          procedure SetFullAccuracy (Value: Boolean);
          procedure SetForceDecimals (Value: Boolean);

          function GetFloatValue: Extended; override;
          procedure SetFloatValue (const Value: Extended); override;
          function GetPercentValue: Extended;
          procedure SetPercentValue (const Value: Extended);
          procedure SetScaled (Value: Boolean);
          procedure SetShowPercent (Value: Boolean);
          procedure SetTrimTrailingZeroes (Value: Boolean);
          procedure UpdateValidChars; override;
     public
          //: Creates the Edit Component.
          constructor Create (AOwner: TComponent); override;
     published
          {: When enabled, Trailing Zeroes to the right of the decimal point are removed,
          and if no decimal places at all then the Decimal Point is removed. }
          property TrimTrailingZeroes: Boolean
               read fTrimTrailingZeroes
               write SetTrimTrailingZeroes
               default False;
          {: Underlying Value stored to Full Accuracy. If you want the various
           properties to return the value to 	the same accuracy as displayed
           then set FullAccuracy property to false. If you want the underlying
           value to always be to as many decimal places as possible set this
           to true. }
          property FullAccuracy: Boolean
               read FFullAccuracy
               write SetFullAccuracy
               default False;
          {: When true doesn't allow the user to enter more than the specified
           number of decimal places. }
          property ForceDecimals: Boolean
               read fForceDecimals
               write SetForceDecimals
               default False;
          {:Scaled controls when true means that 25% is saved as 0.25, and when
          false as 25. The use of this will depend on how the data is stored
          and used in the Database. By default it is set to True. }
          property Scaled: Boolean
               read FScaled
               write SetScaled
               default True;
          //: Allows access to the Edit Field as an Percentage.
          property AsPercentage: Extended
               read GetPercentValue
               write SetPercentValue
               stored ValueStored;
          //: Allows access to the Edit Field as an Integer.
          property AsFloat: Extended
               read GetFloatValue
               write SetFloatValue
               stored False;
          //: Number of Decimal Places to Display.
          property DecimalPlaces: Byte
               read fDecimals
               write SetDecimals
               default 2;
          //: Controls whether a percentage sign is shown.
          property ShowPercent: Boolean
               read fShowPercent
               write SetShowPercent
               default False;
          {: If Validation is Enabled, then this value is used for
           rejecting any values less than it. }
          property BoundLower: Extended
               read FBoundLower
               write FBoundLower;
          {: If Validation is Enabled, then this value is used for
           rejecting any values greater than it. }
          property BoundUpper: Extended
               read FBoundUpper
               write FBoundUpper;
          {: When CustomSeparators is True, this value is used for the Decimal
           Separator rather than the System Value.
           }
          property CustomDecimal: Char
               read fCustomDecimal
               write SetCustomDecimal
               stored StoreCustomSeparators;
          {: When CustomSeparators is True, this value is used for the Thousands
           Separator rather than the System Value.
           }
          property CustomThousands: Char
               read fCustomThousands
               write SetCustomThousands
               stored StoreCustomSeparators;
          {: When True CustomThousands and CustomDecimal are used for the
           Thousands Separator and Decimal Separator respectively rather
           than the System Values.
           }
          property CustomSeparators: Boolean
               read fCustomSeparators
               write SetCustomSeparators
               default False;
     end;

     {: Enhanced Edit Control that only allows Floats in Scientific Notation
      to be entered - ie 1.234E-12 . Use AsFloat to Read/Write the Extended Value.
      Doesn't support Sign Toggling - so invalid data can be entered.
      Decimals controls the number of decimal places shown/stored.<p>
      ShowPosSign can be set to show '+' signs.<p>
      If SciRange is true then if SciMin < abs (Value) < SciMax will be
      displayed in normal XXXX.XXX format, if false then Scientific Notation
      will be used all the time.
      BlankWhenZero displays a Blank Field when the value is Zero.<p>
      Edit Control also has Alignment and ReadOnly Color Changing.
      Includes Bounds Validation.<p>
      Flat controls whether the control has a MS Office 97/2000 type behaviour,
      where the "look" changes when the control has focus or the mouse passes
      over it. ColorBorderFocus & ColorBorderUnfocus are used for Border colors
      when the Control is Flat.<p>
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
      ColorFontPos and ColorFontNeg can be used so that Positives (or to
      be more precise non-negatives) and Negatives appear in a different
      Color.<p>
      ColorDisabled controls the Color of the Control when Disabled, ie
      Enabled := False.<p>
      If <See Var=ESBEnterAsTab> is true then the Enter Key will be treated as
      though it were the Tab Key.<p>
      If <See Var=ESBEscAsUndo> is true then the Esc Key will be cause an Undo
      to occur in the field.<p>
      If <See Var=ESBArrowsAsTab> is true then Up Arrow will move to previous
      field like Shift-Tab, and Down Arrow will move to next field like
      Tab. }
     TESBSciFloatEdit = class (TESBBaseEdit)
     private
          fBlankWhenZero: Boolean; // If Value is Zero then display Blanks
          FBoundLower: Extended; // Lower Bound for Bounds Validation
          FBoundUpper: Extended; // Upper Bound for Bounds Validation
          fDecimals: Byte; // Number of Decimals
          fFloatValue: Extended; // Value as Extended Float
          FFullAccuracy: Boolean; // Value stored to full Accuracy
          FScale: Extended; // Value that the Displayed value is Scaled by
          fTrimTrailingZeroes: Boolean;
          fSciRange: Boolean;
          fSciMin: Extended;
          fSciMax: Extended;
     protected
          procedure Convert2Value; override;
          procedure DisplayText; override;
          procedure DoBoundsValidation; override;

          function GetFloatValue: Extended;
          procedure SetFloatValue (const Value: Extended);
          procedure SetBlankWhenZero (Value: Boolean);
          procedure SetDecimals (Value: Byte);
          procedure SetFullAccuracy (Value: Boolean);
          procedure SetScale (const Value: Extended);
          function GetScaledFloat: Extended;
          procedure SetScaledFloat (const Value: Extended);
          procedure SetScaledFloat2 (const Value: Extended);
          procedure SetTrimTrailingZeroes (Value: Boolean);
          procedure SetSciRange (Value: Boolean);
          procedure SetSciMin (const Value: Extended);
          procedure SetSciMax (const Value: Extended);
     public
          //: Creates the Edit Component.
          constructor Create (AOwner: TComponent); override;

     published
          {: When enabled, Trailing Zeroes to the right of the decimal point are removed,
          and if no decimal places at all then the Decimal Point is removed. }
          property TrimTrailingZeroes: Boolean
               read fTrimTrailingZeroes
               write SetTrimTrailingZeroes
               default False;
          {: If SciRange is true then if SciMin < abs (Value) < SciMax will be
          displayed in normal XXXX.XXX format, if false then Scientific Notation
          will be used all the time. }
          property SciRange: Boolean
               read fSciRange
               write SetSciRange
               default False;
          {: If SciRange is true then if SciMin < abs (Value) < SciMax will be
          displayed in normal XXXX.XXX format, if false then Scientific Notation
          will be used all the time. }
          property SciMax: Extended
               read fSciMax
               write SetSciMax;
          {: If SciRange is true then if SciMin < abs (Value) < SciMax will be
          displayed in normal XXXX.XXX format, if false then Scientific Notation
          will be used all the time. }
          property SciMin: Extended
               read fSciMin
               write SetSciMin;
          {: Underlying Value stored to Full Accuracy. If you want the various
           properties to return the value to 	the same accuracy as displayed
           then set FullAccuracy property to false. If you want the underlying
           value to always be to as many decimal places as possible set this
           to true. }
          property FullAccuracy: Boolean
               read FFullAccuracy
               write SetFullAccuracy
               default False;
          {: Value Displayed is the Underlying Value divided by Scale.
           Set to 1 for no Scale to by Applied. }
          property Scale: Extended
               read FScale
               write SetScale;
          //: Allows access to the Edit Field as a Float.
          property AsFloat: Extended
               read GetFloatValue
               write SetFloatValue
               stored ValueStored;
          //: Allows access to the Scaled Value (Displayed)
          property AsScaledFloat: Extended
               read GetScaledFloat
               write SetScaledFloat
               stored false;
          {: When enabled, the Edit Box will display "Empty" when the
           Value is Zero. }
          property BlankWhenZero: Boolean
               read fBlankWhenZero
               write SetBlankWhenZero
               default False;
          {: If Validation is Enabled, then this value is used for
           rejecting any values less than it. }
          property BoundLower: Extended
               read FBoundLower
               write FBoundLower;
          {: If Validation is Enabled, then this value is used for
           rejecting any values greater than it. }
          property BoundUpper: Extended
               read FBoundUpper
               write FBoundUpper;
          //: Number of Decimal Places to be displayed.
          property DecimalPlaces: Byte
               read fDecimals
               write SetDecimals
               default 4;
          {: Font Color for the field when it contains a Negative Value.
           Color is changed, if required, when the field is exited.
           Font.Color Property is ignored. }
          property ColorFontNeg;
          {: Font Color for the field when it contains a non-Negative Value
           Color is changed, if required, when the field is exited.
           Font.Color Property is ignored. }
          property ColorFontPos;
     end;

     {: Enhanced Edit Control that only allows IP Addresses
      to be entered. Use AsLongWord to Read/Write the LongWord Value or
      Text to Read/Write the formatted value.
      BlankWhenZero displays a Blank Field when the value is Zero.<p>
      Edit Control also has Alignment and ReadOnly Color Changing.
      Includes Bounds Validation.<p>
      Flat controls whether the control has a MS Office 97/2000 type behaviour,
      where the "look" changes when the control has focus or the mouse passes
      over it. ColorBorderFocus & ColorBorderUnfocus are used for Border colors
      when the Control is Flat.<p>
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
      If <See Var=ESBEnterAsTab> is true then the Enter Key will be treated as
      though it were the Tab Key.<p>
      If <See Var=ESBEscAsUndo> is true then the Esc Key will be cause an Undo
      to occur in the field.<p>
      If <See Var=ESBArrowsAsTab> is true then Up Arrow will move to previous
      field like Shift-Tab, and Down Arrow will move to next field like
      Tab. }
     TESBIPEdit = class (TESBBaseEdit)
     private
          FBoundLower: LongWord; // Lower Bound for Bounds Validation
          FBoundUpper: LongWord; // Upper Bound for Bounds Validation
          FLongWord: Longword;
          fBlankWhenZero: Boolean; // Should Zeros be displayed as Blanks
     protected
          procedure Convert2Value; override;
          procedure DisplayText; override;
          procedure DoBoundsValidation; override;

          procedure SetLongWord (Value: Longword);
          function GetLongWord: Longword;
          procedure SetBlankWhenZero (Value: Boolean);
          procedure SetBoundLower (const Value: string);
          function GetBoundLower: string;
          procedure SetBoundUpper (const Value: string);
          function GetBoundUpper: string;
     public
          //: Creates the Edit Component.
          constructor Create (AOwner: TComponent); override;

          {: ReadOnly Property is set to true if either the Control has not been
           entered or has been successfully exited without any
           Conversion Error. IgnoreConvertError causes this property
           to always be true. }
          property ConvertOK: Boolean
               read FConvertOK;
     published
          //: Allows access to the Edit Field as an Integer.
          property AsLongWord: LongWord
               read GetLongWord
               write SetLongWord
               stored ValueStored
               default 0;
          {: When enabled, the Edit Box will display "Empty" when the
           Value is '0.0.0.0'. }
          property BlankWhenZero: Boolean
               read fBlankWhenZero
               write SetBlankWhenZero
               default False;
          {: If Validation is Enabled, then this value is used for
           rejecting any values less than it. }
          property BoundLower: string
               read GetBoundLower
               write SetBoundLower;
          {: If Validation is Enabled, then this value is used for
           rejecting any values greater than it. }
          property BoundUpper: string
               read GetBoundUpper
               write SetBoundUpper;
          {: This controls whether Conversion Checking will be done
           as the control is exited.
           Use this property with caution as it will "allow" invalid
           dates and will prevent OnConvertError being called. }
          property IgnoreConvertError: Boolean
               read FIgnoreConvertError
               write FIgnoreConvertError
               default False;

          {: Event Called if conversion from IP Text is an invalid IP
           Address. Thus a component is not between 0 and 255 }
          property OnConvertError: TESBConvertErrorEvent
               read FOnConvertError
               write FOnConvertError;
     end;

     {: Enhanced Edit Control that only allows Hexadecimal Values to be entered.
      Use AsLongWord to Read/Write the LongWord Value or
      Text to Read/Write the Hexadecimal value.
      BlankWhenZero displays a Blank Field when the value is Zero.<p>
      ZeroPad is used with MaxLength (can't be 0) to fill the field
      to the left with Zeroes. <p>
      Edit Control also has Alignment and ReadOnly Color Changing.
      Includes Bounds Validation.<p>
      Flat controls whether the control has a MS Office 97/2000 type behaviour,
      where the "look" changes when the control has focus or the mouse passes
      over it. ColorBorderFocus & ColorBorderUnfocus are used for Border colors
      when the Control is Flat.<p>
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
      If <See Var=ESBEnterAsTab> is true then the Enter Key will be treated as
      though it were the Tab Key.<p>
      If <See Var=ESBEscAsUndo> is true then the Esc Key will be cause an Undo
      to occur in the field.<p>
      If <See Var=ESBArrowsAsTab> is true then Up Arrow will move to previous
      field like Shift-Tab, and Down Arrow will move to next field like
      Tab. }
     TESBHexEdit = class (TESBBaseEdit)
     private
          FBoundLower: LongWord; // Lower Bound for Bounds Validation
          FBoundUpper: LongWord; // Upper Bound for Bounds Validation
          FLongWord: Longword;
          fBlankWhenZero: Boolean; // Should Zeros be displayed as Blanks
          fZeroPad: Boolean; // Should ZeroPadding be used
     protected
          procedure Convert2Value; override;
          procedure DisplayText; override;
          procedure DoBoundsValidation; override;

          procedure SetLongWord (Value: Longword);
          function GetLongWord: Longword;
          procedure SetBlankWhenZero (Value: Boolean);
          procedure SetBoundLower (const Value: string);
          function GetBoundLower: string;
          procedure SetBoundUpper (const Value: string);
          function GetBoundUpper: string;
          procedure SetZeroPad (Value: Boolean);
     public
          //: Creates the Edit Component.
          constructor Create (AOwner: TComponent); override;

          {: ReadOnly Property is set to true if either the Control has not been
           entered or has been successfully exited without any
           Conversion Error. IgnoreConvertError causes this property
           to always be true. }
          property ConvertOK: Boolean
               read FConvertOK;
     published
          //: Allows access to the Edit Field as an Integer.
          property AsLongWord: LongWord
               read GetLongWord
               write SetLongWord
               stored ValueStored
               default 0;
          {: When enabled, the Edit Box will display "Empty" when the
           Value is '0.0.0.0'. }
          property BlankWhenZero: Boolean
               read fBlankWhenZero
               write SetBlankWhenZero
               default False;
          {: Overriden MaxLength that still works the same way that
           the Standard TEdit property does. For ZeroPad to work
           MaxLength must be greater 0. }
          property MaxLength
               default 8;
          {: When Enabled and MaxLength greater than 0 will LeftPad with Zeros if
           necessary. }
          property ZeroPad: Boolean
               read fZeroPad
               write SetZeroPad
               default True;
          {: If Validation is Enabled, then this value is used for
           rejecting any values less than it. }
          property BoundLower: string
               read GetBoundLower
               write SetBoundLower;
          {: If Validation is Enabled, then this value is used for
           rejecting any values greater than it. }
          property BoundUpper: string
               read GetBoundUpper
               write SetBoundUpper;
     end;

implementation

uses
     SysUtils, Qt,
     QESBPCS_RS_Math,
     QESBPCSConvert, QESBPCSMath, QESBPCSMsgs;

constructor TESBBaseNumericEdit.Create (AOwner: TComponent);
begin
     inherited Create (AOwner);
     fBlankWhenZero := False;
     fForceDecimals := False;
     fDecimals := 4;
     fIntValue := 0;
     fFloatValue := 0.0;
     fUseSignToggling := True;
     fZeroPad := False;
     fThousandSeparators := essNone;
     fCustomSeparators := False;
     fCustomDecimal := DecimalSeparator;
     fCustomThousands := ThousandSeparator;
end;

procedure TESBBaseNumericEdit.KeyUp (var Key: Word; Shift: TShiftState);
begin
     inherited KeyUp (Key, Shift);

     if (fThousandSeparators = essAuto) and (Key <> Key_RETURN) and (Key <> Key_Tab)
          and (Key <> Key_Prior) and (Key <> Key_Next) and (Key <> Key_Up)
          and (Key <> Key_Down) then
     begin
          fNoDisplayUpdate := True;
          try
               DoThousands;
          finally
               fNoDisplayUpdate := False;
          end;
     end;
end;

procedure TESBBaseNumericEdit.DoThousands;
var
     S: string;
     OldSelStart, OldLength: Integer;
     I, P, Cnt: Integer;
     HoldCh1, HoldCh2: Char;
begin
     HoldCh1 := ThousandSeparator;
     HoldCh2 := DecimalSeparator;
     try
          if fCustomSeparators then
          begin
               ThousandSeparator := fCustomThousands;
               DecimalSeparator := fCustomDecimal
          end;

          S := Text;
          OldSelStart := SelStart;
          OldLength := Length (S);
          S := StripThousandSeparators (S);
          if S = '' then
               Exit;

          P := ESBPosCh (DecimalSeparator, S);
          if P = 0 then
               P := Length (S) + 1;
          Cnt := 0;

          for I := P - 1 downto 0 do
          begin
               if S [I] in ['0'..'9'] then
                    Inc (Cnt)
               else
                    Cnt := 0;
               if (Cnt = 4) then
               begin
                    S := LeftStr (S, I) + ThousandSeparator + RightAfterStr (S, I);
                    Cnt := 1;
               end;
          end;
          Text := S;
          SelStart := OldSelStart + Length (S) - OldLength;
          SelLength := 0;
     finally
          if fCustomSeparators then
          begin
               ThousandSeparator := HoldCh1;
               DecimalSeparator := HoldCh2
          end;
     end;
end;

procedure TESBBaseNumericEdit.KeyPress (var Key: Char);
var
     P: Integer;
     HoldCh1, HoldCh2: Char;
begin
     HoldCh1 := ThousandSeparator;
     HoldCh2 := DecimalSeparator;
     try
          if fCustomSeparators then
          begin
               ThousandSeparator := fCustomThousands;
               DecimalSeparator := fCustomDecimal
          end;

          if Assigned (OnKeyPress) then
               OnKeyPress (Self, Key);

          if (FValidChar <> []) and (Key in FValidChar) then
          begin
               if (Key = '+') then // Handle '+' pressing
               begin
                    if fUseSignToggling then
                         fShowPosSign := not fShowPosSign;
                    if Null then
                    begin
                         Null := False;
                         Clear;
                    end
                    else
                         ClearSelection; // Ensure selected text is deleted
                    if Text <> '' then // if some text already present
                    begin
                         Convert2Value; // Get the Integer Value
                         if fIntValue < 0 then // If Negative make it Positive
                              fIntValue := -1 * fIntValue;
                         if fFloatValue < 0 then // If Negative make it Positive
                              fFloatValue := -1 * fFloatValue;
                         Key := #0; // Clear the KeyStroke
                         DisplayText; // Update Display - caret will be at the beginning
                    end
                    else if not fShowPosSign then // See if '+' should be displayed if no text present
                         Key := #0; // Clear the Keystroke
               end
               else if (Key = '-') then // Handle '-' pressing
               begin
                    if Null then
                    begin
                         Null := False;
                         Clear;
                    end
                    else
                         ClearSelection; // Ensure Selected text is deleted
                    if Text <> '' then // if some text already present
                    begin
                         Convert2Value; // Get the Integer Value
                         if fUseSignToggling or (fIntValue > 0) then // Change the sign as needed
                              fIntValue := -1 * fIntValue;
                         if fUseSignToggling or (fFloatValue > 0) then // Change the sign as needed
                              fFloatValue := -1 * fFloatValue;
                         Key := #0; // Clear the Keystroke
                         DisplayText; // Update Display - caret will be at the beginning
                    end
               end
               else if (Key = DecimalSeparator) then // Handle '.' pressing
               begin
                    if Null then
                    begin
                         Null := False;
                         Clear;
                    end
                    else
                         ClearSelection; // Ensure Selected Text is deleted ESBEdit
                    if ESBPosCh (DecimalSeparator, Text) > 0 then // See if there is already a Decimal Separator
                    begin // if so dispose of Key
                         Key := #0;
                         // MessageBeep (0);
                    end;
               end
               else if fForceDecimals and (Key in ['0'..'9']) then
               begin
                    P := ESBPosCh (DecimalSeparator, Text);
                    if (P > 0) and (SelStart > P) and
                         (Length (RightAfterStr (Text, P)) >= fDecimals) then
                    begin
                         Key := #0;
                    end;
               end;
          end;

          KeyProcess (Key);
     finally
          if fCustomSeparators then
          begin
               ThousandSeparator := HoldCh1;
               DecimalSeparator := HoldCh2
          end;
     end;
end;

function TESBBaseNumericEdit.GetIntValue: Integer;
begin
     Convert2Value;
     Result := fIntValue;
end;

function TESBBaseNumericEdit.GetLWordValue: LongWord;
begin
     Convert2Value;
     Result := fIntValue;
end;

function TESBBaseNumericEdit.GetInt64Value: Int64;
begin
     Convert2Value;
     Result := fIntValue;
end;

function TESBBaseNumericEdit.GetFloatValue: Extended;
begin
     Convert2Value;
     Result := fFloatValue;
end;

procedure TESBBaseNumericEdit.SetZeroPad (Value: Boolean);
begin
     if fZeroPad <> Value then
     begin
          fZeroPad := Value;
          DisplayText;
     end;
end;

procedure TESBBaseNumericEdit.UpdateValidChars;
begin
     // Currently do nothing
end;

procedure TESBBaseNumericEdit.SetCustomSeparators (Value: Boolean);
begin
     if fCustomSeparators <> Value then
     begin
          fCustomSeparators := Value;
          UpdateValidChars;
          DisplayText;
     end;
end;

procedure TESBBaseNumericEdit.SetCustomThousands (Value: Char);
begin
     if fCustomThousands <> Value then
     begin
          fCustomThousands := Value;
          UpdateValidChars;
          DisplayText;
     end;
end;

procedure TESBBaseNumericEdit.SetCustomDecimal (Value: Char);
begin
     if fCustomDecimal <> Value then
     begin
          fCustomDecimal := Value;
          UpdateValidChars;
          DisplayText;
     end;
end;

function TESBBaseNumericEdit.StoreCustomSeparators: Boolean;
begin
     Result := fCustomSeparators;
end;

procedure TESBBaseNumericEdit.SetThousandSeparators (Value: TESBSeparatorStyle);
begin
     if fThousandSeparators <> Value then
     begin
          fThousandSeparators := Value;
          DisplayText;
     end;
end;

procedure TESBBaseNumericEdit.SetShowPosSign (Value: Boolean);
begin
     if fShowPosSign <> Value then
     begin
          fShowPosSign := Value;
          DisplayText;
     end;
end;

procedure TESBBaseNumericEdit.SetSignToggling (Value: Boolean);
begin
     if fUseSignToggling <> Value then
     begin
          fUseSignToggling := Value;
     end;
end;

procedure TESBBaseNumericEdit.SetBlankWhenZero (Value: Boolean);
begin
     if fBlankWhenZero <> Value then
     begin
          fBlankWhenZero := Value;
          DisplayText;
     end;
end;

procedure TESBBaseNumericEdit.SetDecimals (Value: Byte);
begin
     if fDecimals <> Value then
     begin
          fDecimals := Value;
          DisplayText;
     end;
end;

procedure TESBBaseNumericEdit.SetFloatValue (const Value: Extended);
begin
     if (Value <> fFloatValue) or Null then
     begin
          fFloatValue := Value;
          Null := False;
          DisplayText;
     end;
end;

procedure TESBBaseNumericEdit.SetIntValue (Value: Integer);
begin
     if (Value <> fIntValue) or Null then
     begin
          fIntValue := Value;
          Null := False;
          DisplayText;
     end;
end;

procedure TESBBaseNumericEdit.SetLWordValue (Value: LongWord);
begin
     if (Value <> fIntValue) or Null then
     begin
          fIntValue := Value;
          Null := False;
          DisplayText;
     end;
end;

procedure TESBBaseNumericEdit.SetInt64Value (Value: Int64);
begin
     if (Value <> fIntValue) or Null then
     begin
          fIntValue := Value;
          Null := False;
          DisplayText;
     end;
end;

constructor TESBPosEdit.Create (AOwner: TComponent);
begin
     inherited Create (AOwner);
     UpdateValidChars;
     FBoundLower := 0;
     FBoundUpper := 0;
     DisplayText;
end;

procedure TESBPosEdit.UpdateValidChars;
begin
     if CustomSeparators then
          FValidChar := ['+', CustomThousands, '0'..'9']
     else
          FValidChar := ['+', ThousandSeparator, '0'..'9'];

     FValidCharHold := FValidChar;
end;

procedure TESBPosEdit.DisplayText;
var
     HoldCh: Char;
     Hold1, Hold2: Boolean;
     Len: Byte;
     S: string;
begin
     if Null then
     begin
          inherited DisplayText;
          Exit;
     end;
     Hold1 := ESBBlankWhenZero;
     Hold2 := ESBNumPosSign;
     ESBBlankWhenZero := fBlankWhenZero;
     ESBNumPosSign := fShowPosSign;
     try
          if fThousandSeparators <> essNone then // See if ThousandSeparator need to be used.
          begin
               HoldCh := ThousandSeparator;
               try
                    if CustomSeparators then
                         ThousandSeparator := CustomThousands;
                    S := Int2CEStr (fIntValue);
               finally
                    if CustomSeparators then
                         ThousandSeparator := HoldCh;
               end;
          end
          else
               S := Int2EStr (fIntValue);

          if fZeroPad and (MaxLength > 0) then // See if Zero Padding needed
          begin
               if (fIntValue < 0) or ((fIntValue > 0) and ESBNumPosSign) then
               begin
                    Len := MaxLength - 1; // Need to leave space for the sign
                    S := RightAfterStr (S, 1);
               end
               else
                    Len := MaxLength;

               S := PadChLeftStr (S, '0', Len); // Pad with Zeroes
               if fIntValue < 0 then // Add Sign if necessary
                    S := '-' + S
               else if (fIntValue > 0) and ESBNumPosSign then
                    S := '+' + S;
          end;

          Text := S;
     finally
          ESBBlankWhenZero := Hold1;
          ESBNumPosSign := Hold2;
     end;

     if (fIntValue < 0) and (Font.Color <> ColorFontNeg) then
     begin
          Font.Color := ColorFontNeg;
          Invalidate;
     end
     else if (fIntValue >= 0) and (Font.Color <> ColorFontPos) then
     begin
          Font.Color := ColorFontPos;
          Invalidate;
     end;
end;

procedure TESBPosEdit.Convert2Value;
var
     HoldCh: Char;
begin
     if Null then
          fIntValue := 0
     else
     begin
          HoldCh := ThousandSeparator;
          try
               if CustomSeparators then
                    ThousandSeparator := CustomThousands;
               fIntValue := Str2Int64 (Text); // This will strip out ThousandSeparators
          finally
               if CustomSeparators then
                    ThousandSeparator := HoldCh;
          end;
     end;
end;

procedure TESBPosEdit.DoBoundsValidation;
begin
     if FBoundsValidation then // Only do testing if Validation Enabled
     begin
          FBoundsValidationType := evtNone; // Default setting is no Error
          if fIntValue < FBoundLower then
               FBoundsValidationType := evtBelowLower
          else if fIntValue > FBoundUpper then
               FBoundsValidationType := evtAboveUpper;

          if FBoundsValidationType <> evtNone then // If an Error has Occurred
          begin
               if Assigned (OnBoundsError) then // Use User defined Routine
                    OnBoundsError (Self, Text, FBoundsValidationType)
               else
               begin
                    case FBoundsValidationType of
                         evtBelowLower:
                              begin
                                   ErrorMsg (rsLowerBound
                                        + Int2EStr (FBoundLower));
                                   SetFocus;
                              end;
                         evtAboveUpper:
                              begin
                                   ErrorMsg (rsUpperBound
                                        + Int2EStr (FBoundUpper));
                                   SetFocus;
                              end;
                    end;
               end;
          end;
     end;
end;

constructor TESBIntEdit.Create (AOwner: TComponent);
begin
     inherited Create (AOwner);
     UpdateValidChars;
     DisplayText;
end;

procedure TESBIntEdit.UpdateValidChars;
begin
     if CustomSeparators then
          FValidChar := ['+', CustomThousands, '-', '0'..'9']
     else
          FValidChar := ['+', ThousandSeparator, '-', '0'..'9'];

     FValidCharHold := FValidChar;
end;

constructor TESBPosFloatEdit.Create (AOwner: TComponent);
begin
     inherited Create (AOwner);
     UpdateValidChars;
     FBoundLower := 0.0;
     FBoundUpper := 0.0;
     FFullAccuracy := False;
     FScale := 1.0;
     fTrimTrailingZeroes := False;
     DisplayText;
end;

procedure TESBPosFloatEdit.UpdateValidChars;
begin
     if CustomSeparators then
          FValidChar := ['+', CustomThousands, CustomDecimal, '0'..'9']
     else
          FValidChar := ['+', ThousandSeparator, DecimalSeparator, '0'..'9'];

     FValidCharHold := FValidChar;
end;

procedure TESBPosFloatEdit.SetTrimTrailingZeroes (Value: Boolean);
begin
     if fTrimTrailingZeroes <> Value then
     begin
          fTrimTrailingZeroes := Value;
          DisplayText;
     end;
end;

procedure TESBPosFloatEdit.SetScaledFloat (const Value: Extended);
var
     X: Extended;
begin
     X := Value * Scale;
     if (X <> fFloatValue) or Null then
     begin
          fFloatValue := X;
          Null := False;
          DisplayText;
     end;
end;

procedure TESBPosFloatEdit.SetScaledFloat2 (const Value: Extended);
var
     X: Extended;
begin
     X := Value * Scale;
     if (X <> fFloatValue) or Null then
     begin
          fFloatValue := X;
          Null := False;
     end;
end;

function TESBPosFloatEdit.GetScaledFloat: Extended;
begin
     if not FloatIsZero (Scale) then
          Result := fFloatValue / Scale
     else
          Result := 0.0
end;

procedure TESBPosFloatEdit.SetScale (const Value: Extended);
var
     X: Extended;
begin
     X := abs (Value);
     if FloatIsZero (X) then
          X := 1.0;
     if (X <> FScale) then
     begin
          FScale := X;
          DisplayText;
     end;
end;

procedure TESBPosFloatEdit.SetFullAccuracy (Value: Boolean);
begin
     if FFullAccuracy <> Value then
     begin
          FFullAccuracy := Value;
          if not Value then
               Convert2Value;
     end;
end;

procedure TESBPosFloatEdit.SetForceDecimals (Value: Boolean);
begin
     if FForceDecimals <> Value then
     begin
          FForceDecimals := Value;
     end;
end;

procedure TESBPosFloatEdit.DisplayText;
var
     Hold1, Hold2: Boolean;
     HoldCh1, HoldCh2: Char;
     Len: Byte;
     S: string;
begin
     if Null then
     begin
          inherited DisplayText;
          Exit;
     end;
     Hold1 := ESBBlankWhenZero;
     Hold2 := ESBNumPosSign;
     ESBBlankWhenZero := fBlankWhenZero;
     ESBNumPosSign := fShowPosSign;
     HoldCh1 := ThousandSeparator;
     HoldCh2 := DecimalSeparator;
     if CustomSeparators then
     begin
          ThousandSeparator := CustomThousands;
          DecimalSeparator := CustomDecimal;
     end;
     try
          if fThousandSeparators <> essNone then // See if ThousandSeparator need to be used.
          begin
               if fTrimTrailingZeroes then
                    S := Float2CEStr2 (GetScaledFloat, fDecimals)
               else
                    S := Float2CEStr (GetScaledFloat, fDecimals);
          end
          else
          begin
               if fTrimTrailingZeroes then
                    S := Float2EStr2 (GetScaledFloat, fDecimals)
               else
                    S := Float2EStr (GetScaledFloat, fDecimals);
          end;

          if fZeroPad and (MaxLength > 0) then // See if Zero Padding needed
          begin
               if (fFloatValue < 0) or ((fFloatValue > 0) and ESBNumPosSign) then
               begin
                    Len := MaxLength - 1; // Need to leave space for the sign
                    S := RightAfterStr (S, 1);
               end
               else
                    Len := MaxLength;

               S := PadChLeftStr (S, '0', Len); // Pad with Zeroes
               if fFloatValue < 0 then // Add Sign if necessary
                    S := '-' + S
               else if (fFloatValue > 0) and ESBNumPosSign then
                    S := '+' + S;
          end;

          Text := S;
     finally
          if CustomSeparators then
          begin
               ThousandSeparator := HoldCh1;
               DecimalSeparator := HoldCh2;
          end;
          ESBBlankWhenZero := Hold1;
          ESBNumPosSign := Hold2;
     end;

     if (fFloatValue < 0) and (Font.Color <> ColorFontNeg) then
     begin
          Font.Color := FColorFontNeg;
          Invalidate;
     end
     else if (fFloatValue >= 0) and (Font.Color <> ColorFontPos) then
     begin
          Font.Color := ColorFontPos;
          Invalidate;
     end;
end;

procedure TESBPosFloatEdit.Convert2Value;
var
     S: string;
     HoldCh1, HoldCh2: Char;
begin
     HoldCh1 := ThousandSeparator;
     HoldCh2 := DecimalSeparator;
     try
          if CustomSeparators then
          begin
               ThousandSeparator := CustomThousands;
               DecimalSeparator := CustomDecimal;
          end;

          if Null then
               fFloatValue := 0
          else if not FFullAccuracy then
               SetScaledFloat2 (Str2Float (Text))
          else
          begin
               if fThousandSeparators <> essNone then
                    S := Float2CEStr (GetScaledFloat, fDecimals)
               else
                    S := Float2EStr (GetScaledFloat, fDecimals);
               if S <> Text then
                    SetScaledFloat2 (Str2Float (Text))
          end;

     finally
          if CustomSeparators then
          begin
               ThousandSeparator := HoldCh1;
               DecimalSeparator := HoldCh2;
          end;
     end;
end;

procedure TESBPosFloatEdit.DoBoundsValidation;
var
     HoldCh1, HoldCh2: Char;
begin
     if FBoundsValidation then // Only do testing if Validation Enabled
     begin
          FBoundsValidationType := evtNone; // Default setting is no Error
          if fFloatValue < FBoundLower then
               FBoundsValidationType := evtBelowLower
          else if fFloatValue > FBoundUpper then
               FBoundsValidationType := evtAboveUpper;

          if FBoundsValidationType <> evtNone then // If an Error has Occurred
          begin
               if Assigned (OnBoundsError) then // Use User defined Routine
                    OnBoundsError (Self, Text, FBoundsValidationType)
               else
               begin
                    HoldCh1 := ThousandSeparator;
                    HoldCh2 := DecimalSeparator;
                    try
                         if CustomSeparators then
                         begin
                              ThousandSeparator := CustomThousands;
                              DecimalSeparator := CustomDecimal;
                         end;

                         case FBoundsValidationType of
                              evtBelowLower:
                                   begin
                                        ErrorMsg (rsLowerBound
                                             + Float2EStr (FBoundLower, fDecimals));
                                        SetFocus;
                                   end;
                              evtAboveUpper:
                                   begin
                                        ErrorMsg (rsUpperBound
                                             + Float2EStr (FBoundUpper, fDecimals));
                                        SetFocus;
                                   end;
                         end;
                    finally
                         if CustomSeparators then
                         begin
                              ThousandSeparator := HoldCh1;
                              DecimalSeparator := HoldCh2;
                         end;
                    end;
               end;
          end;
     end;
end;

constructor TESBFloatEdit.Create (AOwner: TComponent);
begin
     inherited Create (AOwner);
     UpdateValidChars;
     DisplayText;
end;

procedure TESBFloatEdit.UpdateValidChars;
begin
     if CustomSeparators then
          FValidChar := ['+', '-', CustomThousands, CustomDecimal, '0'..'9']
     else
          FValidChar := ['+', '-', ThousandSeparator, DecimalSeparator, '0'..'9'];

     FValidCharHold := FValidChar;
end;

constructor TESBSciFloatEdit.Create (AOwner: TComponent);
begin
     inherited Create (AOwner);
     FValidChar := ['+', '-', ThousandSeparator, DecimalSeparator, '0'..'9', 'E', 'e'];
     FValidCharHold := FValidChar;
     fSciRange := False;
     fSciMin := 0.0001;
     fSciMax := 1000000.0;
     FBoundLower := 0.0;
     FBoundUpper := 0.0;
     fBlankWhenZero := False;
     fDecimals := 4;
     fFloatValue := 0.0;
     FFullAccuracy := False;
     FScale := 1.0;
     fTrimTrailingZeroes := False;
     DisplayText;
end;

procedure TESBSciFloatEdit.SetTrimTrailingZeroes (Value: Boolean);
begin
     if fTrimTrailingZeroes <> Value then
     begin
          fTrimTrailingZeroes := Value;
          DisplayText;
     end;
end;

procedure TESBSciFloatEdit.SetSciRange (Value: Boolean);
begin
     if fSciRange <> Value then
     begin
          fSciRange := Value;
          DisplayText;
     end;
end;

procedure TESBSciFloatEdit.SetSciMax (const Value: Extended);
begin
     if not SameFloat (fSciMax, Value) then
     begin
          fSciMax := Value;
          DisplayText;
     end;
end;

procedure TESBSciFloatEdit.SetSciMin (const Value: Extended);
begin
     if not SameFloat (fSciMin, Value) then
     begin
          fSciMin := Value;
          DisplayText;
     end;
end;

procedure TESBSciFloatEdit.SetScaledFloat (const Value: Extended);
var
     X: Extended;
begin
     X := Value * Scale;
     if (X <> fFloatValue) or Null then
     begin
          fFloatValue := X;
          Null := False;
          DisplayText;
     end;
end;

procedure TESBSciFloatEdit.SetScaledFloat2 (const Value: Extended);
var
     X: Extended;
begin
     X := Value * Scale;
     if (X <> fFloatValue) or Null then
     begin
          fFloatValue := X;
          Null := False;
     end;
end;

function TESBSciFloatEdit.GetScaledFloat: Extended;
begin
     if not FloatIsZero (Scale) then
          Result := fFloatValue / Scale
     else
          Result := 0.0
end;

procedure TESBSciFloatEdit.SetScale (const Value: Extended);
var
     X: Extended;
begin
     X := abs (Value);
     if FloatIsZero (X) then
          X := 1.0;
     if (X <> FScale) then
     begin
          FScale := X;
          DisplayText;
     end;
end;

procedure TESBSciFloatEdit.SetFullAccuracy (Value: Boolean);
begin
     if FFullAccuracy <> Value then
     begin
          FFullAccuracy := Value;
          if not Value then
               Convert2Value;
     end;
end;

procedure TESBSciFloatEdit.DisplayText;
var
     Hold1: Boolean;
     S: string;
     X: Extended;
begin
     if Null then
     begin
          inherited DisplayText;
          Exit;
     end;
     Hold1 := ESBBlankWhenZero;
     ESBBlankWhenZero := fBlankWhenZero;
     try
          X := abs (GetScaledFloat);
          if fSciRange and (((X > SciMin) and (X < SciMax)) or FloatIsZero (X)) then
          begin
               if fTrimTrailingZeroes then
                    S := Float2EStr2 (GetScaledFloat, fDecimals)
               else
                    S := Float2EStr (GetScaledFloat, fDecimals);
          end
          else if fTrimTrailingZeroes then
               S := SciFloat2EStr2 (GetScaledFloat, fDecimals)
          else
               S := SciFloat2EStr (GetScaledFloat, fDecimals);

          Text := S;
     finally
          ESBBlankWhenZero := Hold1;
     end;

     if (fFloatValue < 0) and (Font.Color <> ColorFontNeg) then
     begin
          Font.Color := FColorFontNeg;
          Invalidate;
     end
     else if (fFloatValue >= 0) and (Font.Color <> ColorFontPos) then
     begin
          Font.Color := ColorFontPos;
          Invalidate;
     end;
end;

procedure TESBSciFloatEdit.Convert2Value;
var
     S: string;
begin
     if Null then
          fFloatValue := 0
     else if not FFullAccuracy then
          SetScaledFloat2 (Str2Float (Text))
     else
     begin
          S := SciFloat2EStr (GetScaledFloat, fDecimals);
          if S <> Text then
               SetScaledFloat2 (Str2Float (Text))
     end;
end;

procedure TESBSciFloatEdit.DoBoundsValidation;
begin
     if FBoundsValidation then // Only do testing if Validation Enabled
     begin
          FBoundsValidationType := evtNone; // Default setting is no Error
          if fFloatValue < FBoundLower then
               FBoundsValidationType := evtBelowLower
          else if fFloatValue > FBoundUpper then
               FBoundsValidationType := evtAboveUpper;

          if FBoundsValidationType <> evtNone then // If an Error has Occurred
          begin
               if Assigned (OnBoundsError) then // Use User defined Routine
                    OnBoundsError (Self, Text, FBoundsValidationType)
               else
               begin
                    case FBoundsValidationType of
                         evtBelowLower:
                              begin
                                   ErrorMsg (rsLowerBound
                                        + Float2EStr (FBoundLower, fDecimals));
                                   SetFocus;
                              end;
                         evtAboveUpper:
                              begin
                                   ErrorMsg (rsUpperBound
                                        + Float2EStr (FBoundUpper, fDecimals));
                                   SetFocus;
                              end;
                    end;
               end;
          end;
     end;
end;

function TESBSciFloatEdit.GetFloatValue: Extended;
begin
     Convert2Value;
     Result := fFloatValue;
end;

procedure TESBSciFloatEdit.SetBlankWhenZero (Value: Boolean);
begin
     if fBlankWhenZero <> Value then
     begin
          fBlankWhenZero := Value;
          DisplayText;
     end;
end;

procedure TESBSciFloatEdit.SetDecimals (Value: Byte);
begin
     if fDecimals <> Value then
     begin
          fDecimals := Value;
          DisplayText;
     end;
end;

procedure TESBSciFloatEdit.SetFloatValue (const Value: Extended);
begin
     if (Value <> fFloatValue) or Null then
     begin
          fFloatValue := Value;
          Null := False;
          DisplayText;
     end;
end;

constructor TESBPercentEdit.Create (AOwner: TComponent);
begin
     inherited Create (AOwner);
     UpdateValidChars;
     fDecimals := 2;
     FScaled := True;
     fShowPercent := False;
     fTrimTrailingZeroes := False;
     DisplayText;
end;

procedure TESBPercentEdit.UpdateValidChars;
begin
     if CustomSeparators then
          FValidChar := ['%', '+', '-', CustomThousands, CustomDecimal, '0'..'9']
     else
          FValidChar := ['%', '+', '-', ThousandSeparator, DecimalSeparator, '0'..'9'];

     FValidCharHold := FValidChar;
end;

procedure TESBPercentEdit.SetTrimTrailingZeroes (Value: Boolean);
begin
     if fTrimTrailingZeroes <> Value then
     begin
          fTrimTrailingZeroes := Value;
          DisplayText;
     end;
end;

function TESBPercentEdit.GetPercentValue: Extended;
begin
     Convert2Value;
     Result := fFloatValue;
end;

procedure TESBPercentEdit.SetPercentValue (const Value: Extended);
begin
     if (Value <> fFloatValue) or Null then
     begin
          fFloatValue := Value;
          Null := False;
          DisplayText;
     end;
end;

function TESBPercentEdit.GetFloatValue: Extended;
begin
     if FScaled then
          Result := fFloatValue / 100
     else
          Result := fFloatValue;
end;

procedure TESBPercentEdit.SetFloatValue (const Value: Extended);
begin
     if FScaled then
     begin
          if (Value * 100 <> fFloatValue) or Null then
          begin
               fFloatValue := Value * 100;
               Null := False;
               DisplayText;
          end;
     end
     else
     begin
          if (Value <> fFloatValue) or Null then
          begin
               fFloatValue := Value;
               Null := False;
               DisplayText;
          end;
     end;
end;

procedure TESBPercentEdit.SetScaled (Value: Boolean);
begin
     if FScaled <> Value then
     begin
          FScaled := Value;
          if FScaled then
               fFloatValue := fFloatValue * 100
          else
               fFloatValue := fFloatValue / 100;
          DisplayText;
     end;
end;

procedure TESBPercentEdit.DisplayText;
var
     Hold1, Hold2: Boolean;
     HoldCh1, HoldCh2: Char;
     Len: Byte;
     S: string;
begin
     if Null then
     begin
          inherited DisplayText;
          Exit;
     end;
     Hold1 := ESBBlankWhenZero;
     Hold2 := ESBNumPosSign;
     HoldCh1 := ThousandSeparator;
     HoldCh2 := DecimalSeparator;
     ESBBlankWhenZero := fBlankWhenZero;
     ESBNumPosSign := fShowPosSign;
     if CustomSeparators then
     begin
          ThousandSeparator := CustomThousands;
          DecimalSeparator := CustomDecimal;
     end;
     try
          if fThousandSeparators <> essNone then // See if ThousandSeparator need to be used.
          begin
               if fTrimTrailingZeroes then
                    S := Float2CEStr2 (fFloatValue, fDecimals)
               else
                    S := Float2CEStr (fFloatValue, fDecimals);
          end
          else
          begin
               if fTrimTrailingZeroes then
                    S := Float2EStr2 (fFloatValue, fDecimals)
               else
                    S := Float2EStr (fFloatValue, fDecimals)
          end;

          if fShowPercent then
               S := S + '%';

          if fZeroPad and (MaxLength > 0) then // See if Zero Padding needed
          begin
               if (fFloatValue < 0) or ((fFloatValue > 0) and ESBNumPosSign) then
               begin
                    Len := MaxLength - 1; // Need to leave space for the sign
                    S := RightAfterStr (S, 1);
               end
               else
                    Len := MaxLength;

               S := PadChLeftStr (S, '0', Len); // Pad with Zeroes
               if fFloatValue < 0 then // Add Sign if necessary
                    S := '-' + S
               else if (fFloatValue > 0) and ESBNumPosSign then
                    S := '+' + S;
          end;

          Text := S;
     finally
          if CustomSeparators then
          begin
               ThousandSeparator := HoldCh1;
               DecimalSeparator := HoldCh2;
          end;
          ESBBlankWhenZero := Hold1;
          ESBNumPosSign := Hold2;
     end;

     if (fFloatValue < 0) and (Font.Color <> ColorFontNeg) then
     begin
          Font.Color := FColorFontNeg;
          Invalidate;
     end
     else if (fFloatValue >= 0) and (Font.Color <> ColorFontPos) then
     begin
          Font.Color := ColorFontPos;
          Invalidate;
     end;
end;

procedure TESBPercentEdit.SetShowPercent (Value: Boolean);
begin
     if fShowPercent <> Value then
     begin
          fShowPercent := Value;
          DisplayText;
     end;
end;

procedure TESBPercentEdit.Convert2Value;
var
     HoldCh1, HoldCh2: Char;
begin
     HoldCh1 := ThousandSeparator;
     HoldCh2 := DecimalSeparator;
     try
          if CustomSeparators then
          begin
               ThousandSeparator := CustomThousands;
               DecimalSeparator := CustomDecimal;
          end;

          if Null then
               fFloatValue := 0
          else
               fFloatValue := Str2Float (Text); // This will strip out ThousandSeparators

     finally
          if CustomSeparators then
          begin
               ThousandSeparator := HoldCh1;
               DecimalSeparator := HoldCh2;
          end;
     end;
end;

procedure TESBPercentEdit.DoBoundsValidation;
var
     HoldCh1, HoldCh2: Char;
begin
     if FBoundsValidation then // Only do testing if Validation Enabled
     begin
          FBoundsValidationType := evtNone; // Default setting is no Error
          if fFloatValue < FBoundLower then
               FBoundsValidationType := evtBelowLower
          else if fFloatValue > FBoundUpper then
               FBoundsValidationType := evtAboveUpper;

          if FBoundsValidationType <> evtNone then // If an Error has Occurred
          begin
               if Assigned (OnBoundsError) then // Use User defined Routine
                    OnBoundsError (Self, Text, FBoundsValidationType)
               else
               begin
                    HoldCh1 := ThousandSeparator;
                    HoldCh2 := DecimalSeparator;
                    try
                         if CustomSeparators then
                         begin
                              ThousandSeparator := CustomThousands;
                              DecimalSeparator := CustomDecimal;
                         end;

                         case FBoundsValidationType of
                              evtBelowLower:
                                   begin
                                        ErrorMsg (rsLowerBound
                                             + Float2EStr (FBoundLower, fDecimals));
                                        SetFocus;
                                   end;
                              evtAboveUpper:
                                   begin
                                        ErrorMsg (rsUpperBound
                                             + Float2EStr (FBoundUpper, fDecimals));
                                        SetFocus;
                                   end;
                         end;

                    finally
                         if CustomSeparators then
                         begin
                              ThousandSeparator := HoldCh1;
                              DecimalSeparator := HoldCh2;
                         end;
                    end;
               end;
          end;
     end;
end;

procedure TESBPercentEdit.SetFullAccuracy (Value: Boolean);
begin
     if FFullAccuracy <> Value then
     begin
          FFullAccuracy := Value;
          if not Value then
               Convert2Value;
     end;
end;

procedure TESBPercentEdit.SetForceDecimals (Value: Boolean);
begin
     if FForceDecimals <> Value then
     begin
          FForceDecimals := Value;
     end;
end;

constructor TESBIPEdit.Create (AOwner: TComponent);
begin
     inherited Create (AOwner);
     FValidChar := ['.', '0'..'9'];
     FValidCharHold := FValidChar;
     FBoundLower := 0;
     FBoundUpper := High (Longword);
     DisplayText;
end;

procedure TESBIPEdit.Convert2Value;
var
     HoldB: Boolean;
begin
     if Null then
          FLongWord := 0
     else
     begin
          HoldB := ESBRaiseIPError;
          ESBRaiseIPError := FCheckForErrors;
          try
               try
                    FLongWord := IPStr2LWord (Text); // This will strip out ThousandSeparators
               except
                    SetFocus;
                    raise;
               end;
          finally
               ESBRaiseIPError := HoldB;
          end;
     end;
end;

procedure TESBIPEdit.SetLongWord (Value: Longword);
begin
     if (Value <> FLongWord) or Null then
     begin
          FLongWord := Value;
          Null := False;
          DisplayText;
     end;
end;

function TESBIPEdit.GetLongWord: Longword;
begin
     Convert2Value;
     Result := FLongWord;
end;

procedure TESBIPEdit.DisplayText;
var
     Hold1: Boolean;
     S: string;
begin
     if Null then
     begin
          inherited DisplayText;
          Exit;
     end;
     Hold1 := ESBBlankWhenZero;
     ESBBlankWhenZero := fBlankWhenZero;
     try
          S := LWord2IPStr (FLongWord);

          Text := S;
     finally
          ESBBlankWhenZero := Hold1;
     end;
end;

procedure TESBIPEdit.SetBlankWhenZero (Value: Boolean);
begin
     if fBlankWhenZero <> Value then
     begin
          fBlankWhenZero := Value;
          DisplayText;
     end;
end;

procedure TESBIPEdit.DoBoundsValidation;
begin
     if FBoundsValidation then // Only do testing if Validation Enabled
     begin
          FBoundsValidationType := evtNone; // Default setting is no Error
          if FLongWord < FBoundLower then
               FBoundsValidationType := evtBelowLower
          else if FLongWord > FBoundUpper then
               FBoundsValidationType := evtAboveUpper;

          if FBoundsValidationType <> evtNone then // If an Error has Occurred
          begin
               if Assigned (OnBoundsError) then // Use User defined Routine
                    OnBoundsError (Self, Text, FBoundsValidationType)
               else
               begin
                    case FBoundsValidationType of
                         evtBelowLower:
                              begin
                                   ErrorMsg (rsLowerBound
                                        + LWord2IPStr (FBoundLower));
                                   SetFocus;
                              end;
                         evtAboveUpper:
                              begin
                                   ErrorMsg (rsUpperBound
                                        + LWord2IPStr (FBoundUpper));
                                   SetFocus;
                              end;
                    end;
               end;
          end;
     end;
end;

procedure TESBIPEdit.SetBoundLower (const Value: string);
begin
     FBoundLower := IPStr2LWord (Value);
end;

function TESBIPEdit.GetBoundLower: string;
begin
     Result := LWord2IPStr (FBoundLower);
end;

procedure TESBIPEdit.SetBoundUpper (const Value: string);
begin
     FBoundUpper := IPStr2LWord (Value);
end;

function TESBIPEdit.GetBoundUpper: string;
begin
     Result := LWord2IPStr (FBoundUpper);
end;

constructor TESBHexEdit.Create (AOwner: TComponent);
begin
     inherited Create (AOwner);
     FValidChar := ['0'..'9', 'A'..'F', 'a'..'f'];
     FValidCharHold := FValidChar;
     FBoundLower := 0;
     FBoundUpper := High (Longword);
     fZeroPad := True;
     MaxLength := 8;
     DisplayText;
end;

procedure TESBHexEdit.Convert2Value;
begin
     if Null then
          FLongWord := 0
     else
          FLongWord := Hex2LWord (Text); // This will strip out ThousandSeparators
end;

procedure TESBHexEdit.SetLongWord (Value: Longword);
begin
     if (Value <> FLongWord) or Null then
     begin
          FLongWord := Value;
          Null := False;
          DisplayText;
     end;
end;

function TESBHexEdit.GetLongWord: Longword;
begin
     Convert2Value;
     Result := FLongWord;
end;

procedure TESBHexEdit.DisplayText;
var
     Hold1: Boolean;
     S: string;
begin
     if Null then
     begin
          inherited DisplayText;
          Exit;
     end;
     Hold1 := ESBBlankWhenZero;
     ESBBlankWhenZero := fBlankWhenZero;
     try
          if fZeroPad and (MaxLength > 0) then // See if Zero Padding needed
               S := Int2ZHex (FLongWord, MaxLength)
          else
               S := Int2EHex (FLongWord);

          Text := S;
     finally
          ESBBlankWhenZero := Hold1;
     end;
end;

procedure TESBHexEdit.SetBlankWhenZero (Value: Boolean);
begin
     if fBlankWhenZero <> Value then
     begin
          fBlankWhenZero := Value;
          DisplayText;
     end;
end;

procedure TESBHexEdit.DoBoundsValidation;
begin
     if FBoundsValidation then // Only do testing if Validation Enabled
     begin
          FBoundsValidationType := evtNone; // Default setting is no Error
          if FLongWord < FBoundLower then
               FBoundsValidationType := evtBelowLower
          else if FLongWord > FBoundUpper then
               FBoundsValidationType := evtAboveUpper;

          if FBoundsValidationType <> evtNone then // If an Error has Occurred
          begin
               if Assigned (OnBoundsError) then // Use User defined Routine
                    OnBoundsError (Self, Text, FBoundsValidationType)
               else
               begin
                    case FBoundsValidationType of
                         evtBelowLower:
                              begin
                                   ErrorMsg (rsLowerBound
                                        + LWord2IPStr (FBoundLower));
                                   SetFocus;
                              end;
                         evtAboveUpper:
                              begin
                                   ErrorMsg (rsUpperBound
                                        + LWord2IPStr (FBoundUpper));
                                   SetFocus;
                              end;
                    end;
               end;
          end;
     end;
end;

procedure TESBHexEdit.SetBoundLower (const Value: string);
begin
     FBoundLower := Hex2LWord (Value);
end;

function TESBHexEdit.GetBoundLower: string;
begin
     Result := Int2ZHex (FBoundLower, 8)
end;

procedure TESBHexEdit.SetBoundUpper (const Value: string);
begin
     FBoundUpper := Hex2LWord (Value);
end;

function TESBHexEdit.GetBoundUpper: string;
begin
     Result := Int2ZHex (FBoundUpper, 8)
end;

procedure TESBHexEdit.SetZeroPad (Value: Boolean);
begin
     if fZeroPad <> Value then
     begin
          fZeroPad := Value;
          DisplayText;
     end;
end;

end.
