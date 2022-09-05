unit SpTBXEditors;

{==============================================================================
Version 2.4.8

The contents of this file are subject to the SpTBXLib License; you may
not use or distribute this file except in compliance with the
SpTBXLib License.
A copy of the SpTBXLib License may be found in SpTBXLib-LICENSE.txt or at:
  http://www.silverpointdevelopment.com/sptbxlib/SpTBXLib-LICENSE.htm

Alternatively, the contents of this file may be used under the terms of the
Mozilla Public License Version 1.1 (the "MPL v1.1"), in which case the provisions
of the MPL v1.1 are applicable instead of those in the SpTBXLib License.
A copy of the MPL v1.1 may be found in MPL-LICENSE.txt or at:
  http://www.mozilla.org/MPL/

If you wish to allow use of your version of this file only under the terms of
the MPL v1.1 and not to allow others to use your version of this file under the
SpTBXLib License, indicate your decision by deleting the provisions
above and replace them with the notice and other provisions required by the
MPL v1.1. If you do not delete the provisions above, a recipient may use your
version of this file under either the SpTBXLib License or the MPL v1.1.

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The initial developer of this code is Robert Lee.

Requirements:
For Delphi/C++Builder 2009 or newer:
  - Jordan Russell's Toolbar 2000
    http://www.jrsoftware.org
For Delphi/C++Builder 7-2007:
  - Jordan Russell's Toolbar 2000
    http://www.jrsoftware.org
  - Troy Wolbrink's TNT Unicode Controls
    http://www.tntware.com/delphicontrols/unicode/

Development notes:
  - All the Windows and Delphi bugs fixes are marked with '[Bugfix]'.
  - All the theme changes and adjustments are marked with '[Theme-Change]'.

To Do:
  - Rotated caption painting.

Known Issues:
  -

History:
15 April 2013 - version 2.4.8
  - No changes.

7 February 2012 - version 2.4.7
  - Minor bug fixes.
  - Added support for Delphi XE2.
  - Added support for 64 bit Delphi compiler.

25 June 2011 - version 2.4.6
  - No changes.

12 March 2010 - version 2.4.5
  - Fixed incorrect TSpTBXEditItem hottrack painting when the
    edit was disabled on Aero, thanks to Erwin Denissen for
    reporting this.

2 December 2009 - version 2.4.4
  - Fixed flicker on TSpTBXComboBox when changing the font,
    thanks to Simon H. for reporting this.

13 September 2009 - version 2.4.3
  - Fixed incorrect TSpTBXComboBox painting, when the Style
    is set to csDropDownList and the control is disabled the
    text is not painted with csGrayText. This is a VCL bug
    the same happens with TComboBox when you set it to
    csDropDownFixed (TSpTBXComboBox uses csDropDownFixed
    instead of csDropDownList).
    Thanks to Arvid for reporting this.
  - Fixed incorrect TSpTBXSpinEdit behavior, when the focus
    was changed the text wasn't validated, thanks to Stephan
    for reporting this.
  - Fixed incorrect caret positioning on TSpTBXSpinEdit when
    using a Postfix, thanks to Eric Rappsilber for reporting
    this.

8 May 2009 - version 2.4.2
  - Added AutoDropDownWidth property to TSpTBXComboBox, use
    this to automatically calculate the DropDown window
    size.
  - Changed params of OnDrawItem and OnDrawItemBackground events
    from TSpTBXComboBox.

15 March 2009 - version 2.4.1
  - Added HasEditButton public method to TSpTBXEdit.
  - Added GetDropDownButtonRect public method to TSpTBXComboBox.
  - Fixed TSpTBXEdit frame flicker, thanks to Marc Hoffmann for
    reporting this.
  - Fixed incorrect TSpTBXEdit frame painting when the edit was
    placed on a Toolbar, thanks to Alfred Vink for reporting this.
  - Fixed incorrect TSpTBXListBox painting when OnDrawItem event
    was used, thanks to Evgeny Efimov for reporting this.

17 January 2009 - version 2.4
  - Added AutoItemHeight property to TSpTBXComboBox, use this
    to automatically calculate the ItemHeight property when the
    Style is csDropDown, csDropDownList or csSimple.

26 September 2008 - version 2.3
  - Added skinning support to TSpTBXComboBox items.
  - Fixed incorrect TSpTBXEditItem OnChange event handling,
    the event was only fired if the text property was changed,
    thanks to Anta for reporting this.
  - Added hack to automatically adjust the ItemHeight based on
    the ComboBox font size. Delphi doesn't do this when the
    ComboBox is owner drawed.

29 July 2008 - version 2.2
  - Fixed incorrect TSpTBXEditItem size when the item is on
    a menu, thanks to David for reporting it.

26 June 2008 - version 2.1
  - Added EditImageIndex property to TSpTBXEditItem and
    TSpTBXSpinEditItem, use this to show an icon image
    on the left of the EditCaption when the item is on a menu.

3 May 2008 - version 2.0
  - No changes.

2 April 2008 - version 1.9.5
  - Added ExtendedAccept property to TSpTBXSpinEditor.
  - Fixed incorrect TSpTBXSpinEditor.ValueInc/ValueDec behavior
    when ValueSnap was true and Increment was a fraction, thanks
    to John for reporting this.

3 Febrary 2008 - version 1.9.4
  - Fixed incorrect TSpTBXSpinEditor.ValueInc/ValueDec behavior
    when ValueSnap was true and Increment was higher than
    10000, thanks to Yucel for reporting this.

19 January 2008 - version 1.9.3
  - Fixed incorrect TSpTBXComboBox painting on Windows Vista,
    thanks to Santiago Vila for reporting this.

26 December 2007 - version 1.9.2
  - Fixed incorrect TSpTBXSpinEdit font behavior, when the font
    was changed at runtime the edit rect was not updated, thanks
    to Beta Xiong for reporting this.

1 December 2007 - version 1.9.1
  - Fixed incorrect TSpTBXEditButton painting, when the caption
    was empty and DropDownMenu was assigned 2 arrows were painted
    instead of 1, thanks to Costas Stergiou for reporting this.

20 November 2007 - version 1.9
  - Removed TBX dependency.

20 April 2007 - version 1.8.4
  - Added CustomWidth and CustomHeight properties to the toolbar
    editor items.
  - Fixed incorrect TSpTBXEditItem.StartEditing behavior,
    thanks to Daniel Rikowski for reporting this.

8 February 2007 - version 1.8.3
  - Fixed incorrect OnChange event handling in TSpTBXEditItem,
    thanks to Daniel Rikowski for reporting this.

17 December 2006 - version 1.8.2
  - Fixed a BDS 2006 bug related to Comboboxes, CM_MOUSEENTER and
    CM_MOUSELEAVE are fired everytime the mouse is moved over the
    internal edit control. In D7 these messages were only fired when
    the mouse entered or leaved the combobox.

24 November 2006 - version 1.8.1
  - Fixed incorrect TSpTBXSpinEdit behavior, the Value was not updated
    when the control was unfocused, thanks to Steve and Sebastian for
    reporting this.

27 August 2006 - version 1.8
  - Improved editor's button painting.

15 June 2006 - version 1.7
  - Fixed edit items incorrect painting, the items were not painted
    using the color of FontSettings and EditorFontSettings properties,
    the same happens with the TBX items.

4 May 2006 - version 1.6
  - No changes.

12 April 2006 - version 1.5
  - Added ValueType, ValueAsInteger, Decimals, Prefix and Postfix
    properties to TSpTBXSpinEdit, thanks to Maxim Rylov for his
    code donation.
  - Fixed TSpTBXSpinEdit painting.

27 February 2006 - version 1.4
  - New component added, TSpTBXSpinEdit: a SpinEdit control
    that has TBX themes support.
  - Fixed TSpTBXComboBoxItem bug, when AutoComplete is set to
    false the ComboBox still autocompletes the text, thanks to
    Erwin Denissen for reporting this.
  - Fixed Delphi 2005/2006 bug, CM_MOUSEENTER and CM_MOUSELEAVE
    are fired everytime the mouse enters the combobox internal
    edit control. In prior versions of Delphi these messages
    were only fired when the mouse entered or leaved the combobox,
    including the internal edit control.

10 February 2006 - version 1.3
  - New component added, TSpTBXButtonEdit: an Edit control that
    has a multipurpose button attached.
  - Added new public method, AddEditButton, to TSpTBXEdit.

28 December 2005 - version 1.2
  - No changes.

18 October 2005 - version 1.1
  - New component added, TSpTBXListBox: a ListBox
    with Unicode and TBX themes support that paints
    a hottrack border and TBX theme style selection.
  - New component added, TSpTBXCheckListBox: a CheckListBox
    with Unicode and TBX themes support that paints
    a hottrack border and TBX theme style selection.
  - Fixed TSpTBXComboBoxItem dynamic creation problem.

18 August 2005 - version 1.0
  - No changes.

10 June 2005 - version 0.9
  - SpTBXLib may now alternatively, at your option, be used and/or
    distributed under the terms of the SpTBXLib License.
    Please see the updated LICENSE.TXT file for more information.

20 May 2005 - version 0.8
  - Fixed TSpTBXDropDownItem and TSpTBXComboBoxItem bugs, the popup list
    should be closed when F4 is pressed, thanks to Rune Moberg for
    reporting this.
  - Fixed TSpTBXComboBoxItem bug, the ComboBox didn't check the ItemIndex
    bounds when pressing Up or Down keys, thanks to Rune Moberg for
    reporting this.
  - Fixed AV when trying to dock a toolbar with a TSpTBXComboBoxItem
    on a vertical dock, thanks to Pavel for reporting this.

16 February 2005 - version 0.7
  - Fixed unicode support in W9x, thanks to Daniel Rikowski for
    reporting this.
  - Fixed editors bug, the editors autocomplete was case sensitive,
    this is a TBX bug but it was fixed without patching the source,
    thanks to Daniel Rikowski for reporting this.
  - Fixed TSpTBXComboBox painting bug, the edit frame was not
    correctly highlighted when using the Default theme.
  - Added HotTrack property to TSpTBXEdit and TSpTBXComboBox, when
    setted to true a TBX style frame will be painted when the mouse
    is over the control.
  - Added OnDrawBackground event to TSpTBXEdit and TSpTBXComboBox.

23 December 2004 - version 0.6
  - Initial release.

==============================================================================}

interface

{$BOOLEVAL OFF} // Unit depends on short-circuit boolean evaluation

uses
  Windows, Messages, Classes, SysUtils, Controls, Graphics, ImgList, Forms,
  Menus, StdCtrls, ExtCtrls, ActnList, CheckLst,
  {$IFNDEF UNICODE}
  TntClasses, TntControls, TntStdCtrls, TntCheckLst, TntSysUtils,
  {$ENDIF}
  TB2Toolbar, TB2Item, TB2ExtItems,
  SpTBXSkins, SpTBXItem, SpTBXControls;

{$IFDEF UNICODE}
type
  TTntComboBox = TComboBox;
  TTntListBox = TListBox;
  TTntCheckListBox = TCheckListBox;
{$ENDIF}

const
  CM_SPFONTCHANGED = CM_BASE + 2221;  // Message sent to the control when the font is changed

  { Change reasons for EditItem.Text property }
  tcrSetProperty = 0;  // direct assignment to Text property
  tcrActionLink  = 1;  // change comes from an action link
  tcrEditControl = 2;  // change is caused by typing in edit area

type
  TSpTBXSpinType = (
    spnInteger,
    spnFloat,
    spnHex
  );

  TSpTBXEditItemViewer = class;
  TSpTBXSpinEditViewer = class;

  TSpTBXEditChangeEvent = procedure(Sender: TObject; const AText: WideString) of object;
  TSpTBXEditAcceptTextEvent = procedure(Sender: TObject; var NewText: WideString; var Accept: Boolean) of object;
  TSpTBXEditGetTextEvent = procedure(Sender: TObject; var AText: WideString) of object;
  TSpTBXBeginEditEvent = procedure(Sender: TObject; Viewer: TSpTBXEditItemViewer; EditControl: TCustomEdit) of object;
  TSpTBXEditMessageEvent = procedure(Sender: TObject; Viewer: TSpTBXEditItemViewer; var Message: TMessage; var Handled: Boolean) of object;
  TSpTBXDrawListItemEvent = procedure(Sender: TObject; ACanvas: TCanvas; var ARect: TRect; Index: Integer; const State: TOwnerDrawState;
    const PaintStage: TSpTBXPaintStage; var PaintDefault: Boolean) of object;

  { TSpTBXEditButton }

  TSpTBXEditButton = class(TSpTBXSpeedButton)
  protected
    procedure AdjustFont(AFont: TFont); override;
    function DoDrawDropDownArrow(ACanvas: TCanvas; ARect: TRect): Boolean; override;
    function DoDrawItem(ACanvas: TCanvas; ARect: TRect; const PaintStage: TSpTBXPaintStage): Boolean; override;
    function GetFrameHotTrack: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Click; override;
  end;

  { TSpTBXSpinButton }

  TSpTBXSpinButton = class(TSpTBXEditButton)
  private
    FUpPushed: Boolean;
    FDownPushed: Boolean;
    FOnUpClick: TNotifyEvent;
    FOnDownClick: TNotifyEvent;
  protected
    function DoDrawItem(ACanvas: TCanvas; ARect: TRect; const PaintStage: TSpTBXPaintStage): Boolean; override;
    procedure DoMouseLeave; override;
    procedure MouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
    property OnUpClick: TNotifyEvent read FOnUpClick write FOnUpClick;
    property OnDownClick: TNotifyEvent read FOnDownClick write FOnDownClick;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Click; override;
    procedure IsHotTracking(out UpButton, DownButton, EditFrame: Boolean);
  published
    property Repeating default True;
  end;

  { TSpTBXUnicodeAdaptEdit }

  // Do not inherit from TTNTEdit, TBEditItemViewer.GetEditControlClass needs a TEditClass
  {$IFNDEF UNICODE}
  TSpTBXUnicodeAdaptEdit = class(TEdit)
  private
    FPasswordChar: WideChar;
    procedure SetSelText(const Value: WideString);
    function GetText: WideString;
    procedure SetText(const Value: WideString);
    function GetHint: WideString;
    procedure SetHint(const Value: WideString);
    function IsHintStored: Boolean;
    function GetPasswordChar: WideChar;
    procedure SetPasswordChar(const Value: WideChar);
  protected
    procedure CreateWindowHandle(const Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure DefineProperties(Filer: TFiler); override;
    function GetActionLinkClass: TControlActionLinkClass; override;
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
    function GetSelStart: Integer; reintroduce; virtual;
    procedure SetSelStart(const Value: Integer); reintroduce; virtual;
    function GetSelLength: Integer; reintroduce; virtual;
    procedure SetSelLength(const Value: Integer); reintroduce; virtual;
    function GetSelText: WideString; reintroduce; virtual;
  public
    property SelText: WideString read GetSelText write SetSelText;
    property SelStart: Integer read GetSelStart write SetSelStart;
    property SelLength: Integer read GetSelLength write SetSelLength;
  published
    // Don't let the streaming system store the WideStrings, use DefineProperties instead
    property Text: WideString read GetText write SetText;
    property Hint: WideString read GetHint write SetHint stored IsHintStored;
    property PasswordChar: WideChar read GetPasswordChar write SetPasswordChar default #0;
  end;
  {$ELSE}
  TSpTBXUnicodeAdaptEdit = class(TEdit);
  {$ENDIF}

  { TSpTBXUnicodeEdit }

  TSpTBXUnicodeEdit = class(TSpTBXUnicodeAdaptEdit)
  private
    FAlignment: TAlignment;
    procedure SetAlignment(Value: TAlignment);
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
  protected
    procedure CreateWnd; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure UpdateEditRect; virtual;
  public
    function AddEditButton(RightAligned: Boolean = True; AWidth: Integer = -1): TSpTBXEditButton;
    function HasEditButton: Boolean;
  published
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
  end;

  { TSpTBXEdit }

  TSpTBXEdit = class(TSpTBXUnicodeEdit)
  private
    FBorderStyle: TBorderStyle;
    FHotTrack: Boolean;
    FMouseInControl: Boolean;
    procedure SetBorderStyle(const Value: TBorderStyle);
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure WMNCPaint(var Message: TWMNCPaint); message WM_NCPAINT;
    procedure WMSpSkinChange(var Message: TMessage); message WM_SPSKINCHANGE;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure InvalidateFrame;
    property MouseInControl: Boolean read FMouseInControl;
  published
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle; // Hides the inherited BorderStyle
    property HotTrack: Boolean read FHotTrack write FHotTrack default True;
  end;

  { TSpTBXButtonEdit }

  TSpTBXCustomButtonEdit = class(TSpTBXEdit)
  private
    FEditButton: TSpTBXEditButton;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetName(const Value: TComponentName); override;
    property EditButton: TSpTBXEditButton read FEditButton;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TSpTBXButtonEdit = class(TSpTBXCustomButtonEdit)
  published
    property EditButton;
  end;

  { TSpTBXSpinEdit }

  TSpTBXSpinEditOptions = class(TPersistent)
  private
    FDecimal: Integer;
    FIncrement: Extended;
    FMinValue: Extended;
    FMaxValue: Extended;
    FValue: Extended;
    FValueSnap: Boolean;
    FValueType: TSpTBXSpinType;
    FPrefix: WideString;
    FPostfix: WideString;
    FOnGetText: TSpTBXEditAcceptTextEvent;
    FOnSetText: TSpTBXEditChangeEvent;
    FOnValueChanged: TNotifyEvent;
    function IsIncrementStored: Boolean;
    function IsMaxValueStored: Boolean;
    function IsMinValueStored: Boolean;
    function IsValueStored: Boolean;
    procedure SetDecimal(NewDecimal: Integer);
    procedure SetMaxValue(const NewValue: Extended);
    procedure SetMinValue(const NewValue: Extended);
    procedure SetValue(const NewValue: Extended);
    procedure SetValueType(NewType: TSpTBXSpinType);
    procedure SetPostfix(const ValueString: WideString);
    procedure SetPrefix(const ValueString: WideString);
    function GetValueAsInteger: Int64;
    procedure SetValueAsInteger(const NewValue: Int64);
  protected
    procedure DoValueChanged; virtual;
    procedure UpdateTextFromValue;
    procedure UpdateValueFromText(RevertWhenInvalid: Boolean = True);
    property OnGetText: TSpTBXEditAcceptTextEvent read FOnGetText write FOnGetText;
    property OnSetText: TSpTBXEditChangeEvent read FOnSetText write FOnSetText;
    property OnValueChanged: TNotifyEvent read FOnValueChanged write FOnValueChanged;
  public
    constructor Create; virtual;
    procedure ValueInc;
    procedure ValueDec;
    property ValueAsInteger: Int64 read GetValueAsInteger write SetValueAsInteger;
  published
    property Decimal: Integer read FDecimal write SetDecimal default 2;
    property Increment: Extended read FIncrement write FIncrement stored IsIncrementStored;
    property MaxValue: Extended read FMaxValue write SetMaxValue stored IsMaxValueStored;
    property MinValue: Extended read FMinValue write SetMinValue stored IsMinValueStored;
    property Postfix: WideString read FPostfix write SetPostfix;
    property Prefix: WideString read FPrefix write SetPrefix;
    property Value: Extended read FValue write SetValue stored IsValueStored;
    property ValueSnap: Boolean read FValueSnap write FValueSnap default True;
    property ValueType: TSpTBXSpinType read FValueType write SetValueType default spnInteger;
  end;

  TSpTBXSpinEdit = class(TSpTBXEdit)
  private
    FExtendedAccept: Boolean;
    FSpinButton: TSpTBXSpinButton;
    FSpinOptions: TSpTBXSpinEditOptions;
    procedure SpinOptionsGetText(Sender: TObject; var NewText: WideString; var Accept: Boolean);
    procedure SpinOptionsSetText(Sender: TObject; const AText: WideString);
    function GetValue: Extended;
    function GetValueChanged: TNotifyEvent;
    procedure SetValue(const Value: Extended);
    procedure SetValueChanged(const ValueChangedEvent: TNotifyEvent);
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
  protected
    procedure Change; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure UpClick(Sender: TObject); virtual;
    procedure DownClick(Sender: TObject); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Alignment default taRightJustify;
    property ExtendedAccept: Boolean read FExtendedAccept write FExtendedAccept default False;
    property Text stored False;
    property SpinButton: TSpTBXSpinButton read FSpinButton;
    property SpinOptions: TSpTBXSpinEditOptions read FSpinOptions write FSpinOptions;
    property Value: Extended read GetValue write SetValue stored False;
    property OnValueChanged: TNotifyEvent read GetValueChanged write SetValueChanged;
  end;

  { TSpTBXComboBox }

  TSpTBXComboBox = class(TTntComboBox)
  private
    FAutoDropDownWidth: Boolean;
    FAutoItemHeight: Boolean;
    FFontChanging: Boolean;
    FHotTrack: Boolean;
    FInternalItemHeight: Integer;
    FMouseInControl: Boolean;
    FMouseInDropDownButton: Boolean;
    FMouseTimer: TTimer;
    FOnDrawBackground: TSpTBXDrawEvent;
    FOnDrawItem: TSpTBXDrawListItemEvent;
    FOnDrawItemBackground: TSpTBXDrawListItemEvent;
    procedure MouseTimerHandler(Sender: TObject);
    procedure UpdateDropDownButton;
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure CNMeasureItem(var Message: TWMMeasureItem); message CN_MEASUREITEM;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMSPFontChanged(var Message: TMessage); message CM_SPFONTCHANGED;
    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
    procedure WMMouseMove(var Message: TWMMouseMove); message WM_MOUSEMOVE;
    procedure WMNCCalcSize(var Message: TWMNCCalcSize); message WM_NCCALCSIZE;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMSetFont(var Message: TWMSetFont); message WM_SETFONT;
    procedure WMSpSkinChange(var Message: TMessage); message WM_SPSKINCHANGE;
  protected
    FAutoDropDownWidthRightMargin: Integer;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure CloseUp; override;
    procedure DoCalcMaxDropDownWidth; virtual;
    procedure DoDrawBackground(ACanvas: TCanvas; ARect: TRect; const PaintStage: TSpTBXPaintStage; var PaintDefault: Boolean); virtual;
    procedure DoDrawItem(ACanvas: TCanvas; var ARect: TRect; Index: Integer; const State: TOwnerDrawState;
      const PaintStage: TSpTBXPaintStage; var PaintDefault: Boolean); virtual;
    procedure DoDrawItemBackground(ACanvas: TCanvas; var ARect: TRect; Index: Integer; const State: TOwnerDrawState;
      const PaintStage: TSpTBXPaintStage; var PaintDefault: Boolean); virtual;
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
    procedure DrawItemBackground(Index: Integer; Rect: TRect; State: TOwnerDrawState); virtual;
    {$IF CompilerVersion > 17}
    procedure EditWndProc(var Message: TMessage); override;
    {$IFEND}
    function GetItemHt: Integer; override;
    {$IF CompilerVersion > 20}
    function IsItemHeightStored: Boolean; override;
    {$IFEND}
    procedure SetItemHeight(Value: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetDropDownButtonRect: TRect;
    function GetMouseInDropDownButton: Boolean;
    procedure InvalidateFrame;
    property MouseInControl: Boolean read FMouseInControl;
  published
    property AutoDropDownWidth: Boolean read FAutoDropDownWidth write FAutoDropDownWidth default False;
    property AutoItemHeight: Boolean read FAutoItemHeight write FAutoItemHeight default True;
    property HotTrack: Boolean read FHotTrack write FHotTrack default True;
    property OnDrawBackground: TSpTBXDrawEvent read FOnDrawBackground write FOnDrawBackground;
    property OnDrawItem: TSpTBXDrawListItemEvent read FOnDrawItem write FOnDrawItem; // Hides the inherited OnDrawItem
    property OnDrawItemBackground: TSpTBXDrawListItemEvent read FOnDrawItemBackground write FOnDrawItemBackground;
    property OnMouseMove;
  end;

  { TSpTBXListBox }

  TSpTBXListBox = class(TTntListBox)
  private
    FHotTracking: Boolean;
    FHotTrack: Boolean;
    FChildFocused: Boolean;
    FOnDrawItem: TSpTBXDrawListItemEvent;
    FOnDrawItemBackground: TSpTBXDrawListItemEvent;
    procedure SetHotTrack(const Value: Boolean);
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMFocusChanged(var Message: TCMFocusChanged); message CM_FOCUSCHANGED;
    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
    procedure WMNCPaint(var Message: TWMNCPaint); message WM_NCPAINT;
    procedure WMSpSkinChange(var Message: TMessage); message WM_SPSKINCHANGE;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure DoDrawItem(ACanvas: TCanvas; var ARect: TRect; Index: Integer; const State: TOwnerDrawState;
      const PaintStage: TSpTBXPaintStage; var PaintDefault: Boolean); virtual;
    procedure DoDrawItemBackground(ACanvas: TCanvas; var ARect: TRect; Index: Integer; const State: TOwnerDrawState;
      const PaintStage: TSpTBXPaintStage; var PaintDefault: Boolean); virtual;
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
    procedure DrawItemBackground(Index: Integer; Rect: TRect; State: TOwnerDrawState); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure InvalidateBorders;
    property HotTracking: Boolean read FHotTracking;
  published
    property Style default lbOwnerDrawFixed;
    property HotTrack: Boolean read FHotTrack write SetHotTrack default True;
    property OnDrawItem: TSpTBXDrawListItemEvent read FOnDrawItem write FOnDrawItem; // Hides the inherited OnDrawItem
    property OnDrawItemBackground: TSpTBXDrawListItemEvent read FOnDrawItemBackground write FOnDrawItemBackground;
  end;

  { TSpTBXCheckListBox }

  TSpTBXCheckListBox = class(TTntCheckListBox)
  private
    FHotTracking: Boolean;
    FHotTrack: Boolean;
    FChildFocused: Boolean;
    FOnDrawItem: TSpTBXDrawListItemEvent;
    FOnDrawItemBackground: TSpTBXDrawListItemEvent;
    procedure SetHotTrack(const Value: Boolean);
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMFocusChanged(var Message: TCMFocusChanged); message CM_FOCUSCHANGED;
    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
    procedure WMNCPaint(var Message: TWMNCPaint); message WM_NCPAINT;
    procedure WMSpSkinChange(var Message: TMessage); message WM_SPSKINCHANGE;
  protected
    procedure DoDrawItem(ACanvas: TCanvas; var ARect: TRect; Index: Integer; const State: TOwnerDrawState;
      const PaintStage: TSpTBXPaintStage; var PaintDefault: Boolean); virtual;
    procedure DoDrawItemBackground(ACanvas: TCanvas; var ARect: TRect; Index: Integer; const State: TOwnerDrawState;
      const PaintStage: TSpTBXPaintStage; var PaintDefault: Boolean); virtual;
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
    procedure DrawItemBackground(Index: Integer; Rect: TRect; State: TOwnerDrawState); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure InvalidateBorders;
    property HotTracking: Boolean read FHotTracking;
  published
    property Style default lbOwnerDrawFixed;
    property HotTrack: Boolean read FHotTrack write SetHotTrack default True;
    property OnDrawItem: TSpTBXDrawListItemEvent read FOnDrawItem write FOnDrawItem;
    property OnDrawItemBackground: TSpTBXDrawListItemEvent read FOnDrawItemBackground write FOnDrawItemBackground;
  end;

  { TSpTBXEditItem }

  TSpTBXEditItem = class(TSpTBXCustomItem)
  private
    FEditCaption: WideString;
    FEditImageIndex: TImageIndex;
    FText: WideString;
    FAllowVerticalEditor: Boolean;
    FCharCase: TEditCharCase;
    FEditorFontSettings: TSpTBXFontSettings;
    FExtendedAccept: Boolean;
    FFontSettings: TSpTBXFontSettings;
    FMaxLength: Integer;
    FPasswordChar: WideChar;
    FReadOnly: Boolean;
    FShowImage: Boolean;
    FOnAcceptText: TSpTBXEditAcceptTextEvent;
    FOnBeginEdit: TSpTBXBeginEditEvent;
    FOnChange: TSpTBXEditChangeEvent;
    FOnEditMessage: TSpTBXEditMessageEvent;
    procedure FontSettingsChanged(Sender: TObject);
    procedure SetAllowVerticalEditor(const Value: Boolean);
    procedure SetCharCase(Value: TEditCharCase);
    procedure SetEditCaption(const Value: WideString);
    procedure SetEditorFontSettings(const Value: TSpTBXFontSettings);
    procedure SetMaxLength(Value: Integer);
    procedure SetPasswordChar(Value: WideChar);
    procedure SetShowImage(const Value: Boolean);
    procedure SetText(Value: WideString);
  protected
    function DoAcceptText(var NewText: WideString): Boolean; virtual;
    function DoAutoComplete(var AText: WideString): Boolean; virtual;
    procedure DoBeginEdit(Viewer: TSpTBXEditItemViewer); virtual;
    procedure DoChange(const AText: WideString); virtual;
    procedure DoTextChanging(const OldText: WideString; var NewText: WideString; Reason: Integer); virtual;
    function GetItemViewerClass(AView: TTBView): TTBItemViewerClass; override;
    function NeedToRecreateViewer(AViewer: TTBItemViewer): Boolean; override;
    procedure SetTextEx(Value: WideString; Reason: Integer); virtual;
  public
    function StartEditing(AView: TTBView): Boolean;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    // TSpTBXCustomItem properties
    property Action;
    property Alignment default taLeftJustify;
    property Caption;
    property CustomWidth default 64;
    property CustomHeight;
    property DisplayMode;
    property Enabled;
    property FontSettings;
    property GroupIndex;
    property HelpContext;
    property Hint;
    property ImageIndex;
    property Images;
    property ShortCut;
    property Visible;
    property OnClick;
    property OnDrawCaption;
    property OnDrawHint;
    property OnDrawImage;
    property OnDrawItem;
    property OnSelect;

    // Don't let the streaming system store the WideStrings, use DefineProperties instead
    property EditCaption: WideString read FEditCaption write SetEditCaption; // Hides the inherited EditCaption
    property EditImageIndex: TImageIndex read FEditImageIndex write FEditImageIndex default -1;
    property Text: WideString read FText write SetText; // Hides the inherited Text
    property AllowVerticalEditor: Boolean read FAllowVerticalEditor write SetAllowVerticalEditor default False;
    property CharCase: TEditCharCase read FCharCase write SetCharCase default ecNormal;
    property EditorFontSettings: TSpTBXFontSettings read FEditorFontSettings write SetEditorFontSettings;
    property ExtendedAccept: Boolean read FExtendedAccept write FExtendedAccept default False;
    property MaxLength: Integer read FMaxLength write SetMaxLength default 0;
    property PasswordChar: WideChar read FPasswordChar write SetPasswordChar default #0;
    property ReadOnly: Boolean read FReadOnly write FReadOnly default False;
    property ShowImage: Boolean read FShowImage write SetShowImage default False;
    property OnAcceptText: TSpTBXEditAcceptTextEvent read FOnAcceptText write FOnAcceptText; // Hides the inherited OnAcceptText
    property OnBeginEdit: TSpTBXBeginEditEvent read FOnBeginEdit write FOnBeginEdit;
    property OnChange: TSpTBXEditChangeEvent read FOnChange write FOnChange;
    property OnEditMessage: TSpTBXEditMessageEvent read FOnEditMessage write FOnEditMessage;
  end;

  TEditClass = class of TCustomEdit;

  TSpTBXEditItemViewer = class(TSpTBXItemViewer)
  private
    function EditLoop(const CapHandle: HWND): Boolean;
    procedure EditWndProc(var Message: TMessage);
    function GetEditControlText: WideString;
    procedure GetEditHeight(const DC: HDC; out EditHeight, ExternalLeading: Integer);
    function GetItem: TSpTBXEditItem;
    procedure MouseBeginEdit;
    function MeasureEditCaption: TSize;
    function MeasureTextHeight: Integer;
  protected
    FEditControl: TCustomEdit;
    FEditControlStatus: set of (ecsContinueLoop, ecsAccept, ecsClose);
    procedure CalcSize(const Canvas: TCanvas; var AWidth, AHeight: Integer); override;
    function CaptionShown: Boolean; override;
    procedure DoBeginEdit; virtual;
    function DoExecute: Boolean; override;
    function HandleEditMessage(var Message: TMessage): Boolean; virtual;
    function GetAccRole: Integer; override;
    function GetAccValue(var Value: WideString): Boolean; override;
    procedure GetCursor(const Pt: TPoint; var ACursor: HCURSOR); override;
    function GetEditControlClass: TEditClass; virtual;
    procedure GetEditRect(var R: TRect); virtual;
    function GetImageShown: Boolean; override;
    function GetIndentBefore: Integer; virtual;
    function GetIndentAfter: Integer; virtual;
    procedure InternalDrawFrame(ACanvas: TCanvas; ARect: TRect; ItemInfo: TSpTBXMenuItemInfo); virtual;
    procedure InternalEditControlChange(Sender: TObject); virtual;
    procedure InternalEditControlExit; virtual;
    procedure MouseDown(Shift: TShiftState; X, Y: Integer; var MouseDownOnMenu: Boolean); override;
    procedure MouseUp(X, Y: Integer; MouseWasDownOnMenu: Boolean); override;
    procedure Paint(const Canvas: TCanvas; const ClientAreaRect: TRect; IsSelected, IsPushed, UseDisabledShadow: Boolean); override;

    function ShowImage: Boolean; virtual;
    function UsesSameWidth: Boolean; override;
  public
    function GetCaptionText: WideString; override;
    property EditControl: TCustomEdit read FEditControl;
    property Item: TSpTBXEditItem read GetItem; // Hides the inherited TB2K Item property
  end;

  { TSpTBXSpinEditItem }

  TSpTBXSpinEditItem = class(TSpTBXEditItem)
  private
    FSpinOptions: TSpTBXSpinEditOptions;
    procedure SpinOptionsGetText(Sender: TObject; var NewText: WideString; var Accept: Boolean);
    procedure SpinOptionsSetText(Sender: TObject; const AText: WideString);
    function GetValue: Extended;
    function GetValueChanged: TNotifyEvent;
    procedure SetValue(const Value: Extended);
    procedure SetValueChanged(const ValueChangedEvent: TNotifyEvent);
  protected
    function GetItemViewerClass(AView: TTBView): TTBItemViewerClass; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Alignment default taRightJustify;
    property Text stored False;
    property SpinOptions: TSpTBXSpinEditOptions read FSpinOptions write FSpinOptions;
    property Value: Extended read GetValue write SetValue stored False;
    property OnValueChanged: TNotifyEvent read GetValueChanged write SetValueChanged;
  end;

  TSpTBXSpinEditViewer = class(TSpTBXEditItemViewer)
  private
    FUpPushed: Boolean;
    FDownPushed: Boolean;
    FTimer: TTimer;
    procedure TimerHandler(Sender: TObject);
    function GetItem: TSpTBXSpinEditItem;
  protected
    function GetAccRole: Integer; override;
    function GetIndentAfter: Integer; override;
    function HandleEditMessage(var Message: TMessage): Boolean; override;
    procedure InvalidateButtons;
    function IsPtInButtonPart(X, Y: Integer): Boolean; override;
    procedure LosingCapture; override;
    procedure MouseDown(Shift: TShiftState; X, Y: Integer; var MouseDownOnMenu: Boolean); override;
    procedure MouseUp(X, Y: Integer; MouseWasDownOnMenu: Boolean); override;
    procedure InternalDrawFrame(ACanvas: TCanvas; ARect: TRect; ItemInfo: TSpTBXMenuItemInfo); override;
    procedure InternalEditControlChange(Sender: TObject); override;
    procedure InternalEditControlExit; override;
  public
    destructor Destroy; override;
    property Item: TSpTBXSpinEditItem read GetItem; // Hides the inherited TB2K Item property
  end;

{ Helpers }
procedure SpCalcMaxDropDownWidth(Combo: TSpTBXComboBox; RightMargin: Integer = 8);
function SpFocusEditItem(Item: TTBCustomItem; View: TTBView): Boolean;
function SpStartsTextW(const ASubText, AText: WideString): Boolean;

{ Painting helpers }
function SpCanEditFrameBeHotTracked(BorderStyle: TBorderStyle): Boolean;
procedure SpDrawXPEditButton(ACanvas: TCanvas; ARect: TRect; Enabled, FrameHotTrack, HotTrack, Pushed, RightAligned: Boolean);
procedure SpDrawXPComboButton(ACanvas: TCanvas; ARect: TRect; Enabled, FrameHotTrack, HotTrack, DroppedDown, RightAligned: Boolean);
procedure SpDrawXPSpinButton(ACanvas: TCanvas; ARect: TRect; Enabled, FrameHotTrack, UpHotTrack, DownHotTrack, UpPushed, DownPushed, RightAligned: Boolean);

implementation

uses
  Themes, UxTheme,
  {$IFNDEF UNICODE} TntActnList, TntWindows, {$ENDIF}
  Math, TB2Common;

const
  DefaultSpinButtonSize = 14;

type
  TTBViewAccess = class(TTBView);
  TSpTBXFontSettingsAccess = class(TSpTBXFontSettings);
  TCustomEditAccess = class(TCustomEdit);

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ Helpers }

procedure SpCalcMaxDropDownWidth(Combo: TSpTBXComboBox; RightMargin: Integer = 8);
var
  I, MaxWidth: Integer;
  Sz: TSize;
  C: TControlCanvas;
begin
  MaxWidth := 0;

  C := TControlCanvas.Create;
  try
    C.Control := Combo;
    C.Font.Assign(Combo.Font);
    for I := 0 to Combo.Items.Count - 1 do begin
      Sz := SpGetTextSize(C.Handle, Combo.Items[I], False);
      if Sz.cx > MaxWidth then MaxWidth := Sz.cx;
    end;

    MaxWidth := MaxWidth + GetSystemMetrics(SM_CXVSCROLL) + RightMargin;
    if Combo.Width < MaxWidth then
      SendMessage(Combo.Handle, CB_SETDROPPEDWIDTH, MaxWidth, 0);
  finally
    C.Free;
  end;
end;

function SpFocusEditItem(Item: TTBCustomItem; View: TTBView): Boolean;
var
  IV: TTBItemViewer;
begin
  Result := False;
  IV := View.Find(Item);
  if Assigned(IV) then begin
    View.Select(IV, False);
    View.ExecuteSelected(False);
    Result := True;
  end;
end;

function SpStartsTextW(const ASubText, AText: WideString): Boolean;
var
  L, L2: Integer;
begin
  L := Length(ASubText);
  L2 := Length(AText);
  if L > L2 then Result := False
  else Result := CompareStringW(LOCALE_USER_DEFAULT, NORM_IGNORECASE,
    PWideChar(AText), L, PWideChar(ASubText), L) = 2;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ Painting helpers }

function SpCanEditFrameBeHotTracked(BorderStyle: TBorderStyle): Boolean;
var
  NormalB, HotTrackB: TSpTBXSkinOptionEntry;
begin
  Result := False;
  if (BorderStyle <> bsNone) and (SkinManager.GetSkinType <> sknNone) then begin
    // If the HotTrack borders are different than the Normal borders then
    // return true.
    NormalB := CurrentSkin.Options(skncEditFrame, sknsNormal).Borders;
    HotTrackB := CurrentSkin.Options(skncEditFrame, sknsHotTrack).Borders;
    if not NormalB.IsEqual(HotTrackB) then
      Result := True;
  end;
end;

procedure SpDrawXPEditButton(ACanvas: TCanvas; ARect: TRect; Enabled, FrameHotTrack,
  HotTrack, Pushed, RightAligned: Boolean);
var
  State: TSpTBXSkinStatesType;
begin
  State := CurrentSkin.GetState(Enabled, Pushed, HotTrack or FrameHotTrack, False);
  if FrameHotTrack then begin
    InflateRect(ARect, 1, 1);
    if RightAligned then
      ARect.Left := ARect.Left + 1
    else
      ARect.Right := ARect.Right - 1;
  end;
  CurrentSkin.PaintBackground(ACanvas, ARect, skncEditButton, State, True, True);
end;

procedure SpDrawXPComboButton(ACanvas: TCanvas; ARect: TRect; Enabled, FrameHotTrack,
  HotTrack, DroppedDown, RightAligned: Boolean);
var
  Flags: Integer;
  State: TSpTBXSkinStatesType;
  C: TColor;
  X, Y, Part: Integer;
begin
  case SkinManager.GetSkinType of
    sknNone:
      begin
        Inc(ARect.Left, 4);
        SpFillRect(ACanvas, ARect, clBtnFace, clWindow);
        if DroppedDown then
          Windows.DrawEdge(ACanvas.Handle, ARect, BDR_SUNKENOUTER, BF_RECT)
        else if FrameHotTrack or HotTrack then
          Windows.DrawEdge(ACanvas.Handle, ARect, BDR_RAISEDINNER, BF_RECT);
        State := CurrentSkin.GetState(Enabled, DroppedDown, HotTrack, False);
        C := CurrentSkin.GetTextColor(skncEditButton, State);
        X := (ARect.Left + ARect.Right) div 2;
        Y := (ARect.Top + ARect.Bottom) div 2 - 1;
        SpDrawArrow(ACanvas, X, Y, C, True, False, 2);
      end;
    sknWindows, sknDelphiStyle:
      begin
        if SpIsWinVistaOrUp then
          Part := 6 // (CP_DROPDOWNBUTTONRIGHT) Use the new API on Windows Vista
        else
          Part := CP_DROPDOWNBUTTON;
        if not Enabled then Flags := CBXS_DISABLED
        else if DroppedDown then Flags := CBXS_PRESSED
        else if HotTrack then Flags := CBXS_HOT
        else Flags := CBXS_NORMAL;
        DrawThemeBackground(SpTBXThemeServices.Theme[teComboBox], ACanvas.Handle, Part, Flags, ARect, nil);
      end;
    sknSkin:
      begin
        State := CurrentSkin.GetState(Enabled, DroppedDown, FrameHotTrack, False);
        ACanvas.FillRect(ARect);
        SpDrawXPEditButton(ACanvas, ARect, Enabled, FrameHotTrack, HotTrack, DroppedDown, RightAligned);
        C := CurrentSkin.GetTextColor(skncEditButton, State);
        X := (ARect.Left + ARect.Right) div 2;
        Y := (ARect.Top + ARect.Bottom) div 2 - 1;
        SpDrawArrow(ACanvas, X, Y, C, True, False, 2);
      end;
  end;
end;

procedure SpDrawXPSpinButton(ACanvas: TCanvas; ARect: TRect; Enabled, FrameHotTrack,
  UpHotTrack, DownHotTrack, UpPushed, DownPushed, RightAligned: Boolean);
var
  ButtonR, BR: TRect;
  StateFlags: TThemedSpin;
  Flags: Cardinal;
  X, Y: Integer;
  State: TSpTBXSkinStatesType;
  C: TColor;
begin
  ButtonR := ARect;

  case SkinManager.GetSkinType of
    sknNone:
      begin
        // Up button
        Flags := DFCS_SCROLLUP;
        if UpPushed then
          Flags := Flags or DFCS_PUSHED;
        BR := Rect(ButtonR.Left, ButtonR.Top, ButtonR.Right, (ButtonR.Bottom + ButtonR.Top) div 2);
        DrawFrameControl(ACanvas.Handle, BR, DFC_SCROLL, Flags);
        // Down button
        Flags := DFCS_SCROLLDOWN;
        if DownPushed then
          Flags := Flags or DFCS_PUSHED;
        BR := Rect(ButtonR.Left, BR.Bottom - 1, ButtonR.Right, ButtonR.Bottom);
        DrawFrameControl(ACanvas.Handle, BR, DFC_SCROLL, Flags);
      end;
    sknWindows, sknDelphiStyle:
      begin
        InflateRect(ButtonR, 1, 1);
        // Up button
        BR := ButtonR;
        BR.Bottom := (ButtonR.Top + ButtonR.Bottom) div 2;
        if not Enabled then StateFlags := tsUpDisabled
        else if UpPushed then StateFlags := tsUpPressed
        else if UpHotTrack then StateFlags := tsUpHot
        else StateFlags := tsUpNormal;
        CurrentSkin.PaintThemedElementBackground(ACanvas, BR, SpTBXThemeServices.GetElementDetails(StateFlags));
        // Down button
        BR := ButtonR;
        BR.Top := (ButtonR.Top + ButtonR.Bottom) div 2;
        if not Enabled then StateFlags := tsDownDisabled
        else if DownPushed then StateFlags := tsDownPressed
        else if DownHotTrack then StateFlags := tsDownHot
        else StateFlags := tsDownNormal;
        CurrentSkin.PaintThemedElementBackground(ACanvas, BR, SpTBXThemeServices.GetElementDetails(StateFlags));
      end;
    sknSkin:
      begin
        // Up button
        BR := Rect(ButtonR.Left, ButtonR.Top, ButtonR.Right, (ButtonR.Top + ButtonR.Bottom) div 2 + 1);
        X := (BR.Left + BR.Right) div 2;
        Y := (BR.Top + BR.Bottom) div 2 - 1;
        State := CurrentSkin.GetState(Enabled, UpPushed, UpHotTrack or FrameHotTrack, False);
        if FrameHotTrack then
          BR.Bottom := BR.Bottom - 1;
        SpDrawXPEditButton(ACanvas, BR, Enabled, FrameHotTrack, UpHotTrack, UpPushed, RightAligned);
        C := CurrentSkin.GetTextColor(skncEditButton, State);
        SpDrawArrow(ACanvas, X, Y, C, True, True, 2);
        if FrameHotTrack then
          BR.Bottom := BR.Bottom + 1;
        // Down button
        BR := Rect(ButtonR.Left, BR.Bottom - 1, ButtonR.Right, ButtonR.Bottom);
        X := (BR.Left + BR.Right) div 2;
        Y := (BR.Top + BR.Bottom) div 2 - 1;
        State := CurrentSkin.GetState(Enabled, DownPushed, DownHotTrack or FrameHotTrack, False);
        if FrameHotTrack then
          BR.Top := BR.Top + 1;
        SpDrawXPEditButton(ACanvas, BR, Enabled, FrameHotTrack, DownHotTrack, DownPushed, RightAligned);
        C := CurrentSkin.GetTextColor(skncEditButton, State);
        SpDrawArrow(ACanvas, X, Y, C, True, False, 2);
      end;
  end;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXEditButton }

constructor TSpTBXEditButton.Create(AOwner: TComponent);
var
  Index: Integer;
const
  DefaultName = 'SubEditButton';
begin
  inherited;

  // Find unique name
  if Assigned(AOwner) then begin
    Index := 0;
    while AOwner.FindComponent(DefaultName + IntToStr(Index)) <> nil do
      Inc(Index);
    Name := DefaultName + IntToStr(Index);
  end;

  // Change the FPopupControl, we need to align
  // the DropdownMenu to the Edit control not the button.
  // FPopupControl is used in TSpTBXCustomButton.Click
  if Assigned(AOwner) and (AOwner is TControl) then
    FPopupControl := AOwner as TControl;

  SetSubComponent(True);
end;

procedure TSpTBXEditButton.AdjustFont(AFont: TFont);
var
  State: TSpTBXSkinStatesType;
begin
  if (LinkText <> '') and MouseInControl then
    inherited
  else begin
    State := CurrentSkin.GetState(Enabled, Pushed, MouseInControl or GetFrameHotTrack, Checked);
    AFont.Color := CurrentSkin.GetTextColor(skncEditButton, State);
  end;
end;

procedure TSpTBXEditButton.Click;
begin
  if Assigned(Parent) and SpCanFocus(Parent) then
    Parent.SetFocus;
  inherited;
end;

function TSpTBXEditButton.DoDrawDropDownArrow(ACanvas: TCanvas;
  ARect: TRect): Boolean;
begin
  if (Caption = '') and not IsImageIndexValid then
    Result := False  // Paint the default Windows combo button
  else
    Result := inherited DoDrawDropDownArrow(ACanvas, ARect);
end;

function TSpTBXEditButton.DoDrawItem(ACanvas: TCanvas; ARect: TRect;
  const PaintStage: TSpTBXPaintStage): Boolean;
var
  FrameHotTrack, RightAligned: Boolean;
begin
  if (PaintStage = pstPrePaint) and not BitmapValid then begin
    Result := True;
    if Assigned(OnDraw) then OnDraw(Self, ACanvas, ARect, PaintStage, Result);
    if Result then begin
      FrameHotTrack := GetFrameHotTrack;
      RightAligned := Align <> alLeft;

      // Draw the ComboButton if the caption is not set
      if (Length(Caption) = 0) and not IsImageIndexValid then
        SpDrawXPComboButton(ACanvas, ARect, Enabled, FrameHotTrack, MouseInControl, Pushed, RightAligned)
      else begin
        case SkinManager.GetSkinType of
          sknNone:
            SpDrawXPButton(ACanvas, ARect, Enabled, Pushed, MouseInControl, Checked, Focused, Default);
          sknWindows, sknDelphiStyle:
            begin
              InflateRect(ARect, 1, 1);
              SpDrawXPButton(ACanvas, ARect, Enabled, Pushed, MouseInControl, Checked, Focused, Default);
            end;
          sknSkin:
            SpDrawXPEditButton(ACanvas, ARect, Enabled, FrameHotTrack, FrameHotTrack or MouseInControl, Pushed, RightAligned);
        end;
      end;
    end;
  end
  else
    Result := inherited DoDrawItem(ACanvas, ARect, PaintStage);
end;

function TSpTBXEditButton.GetFrameHotTrack: Boolean;
begin
  if Parent is TSpTBXEdit then
    Result := TSpTBXEdit(Parent).MouseInControl or Parent.Focused
  else
    Result := False;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXSpinButton }

constructor TSpTBXSpinButton.Create(AOwner: TComponent);
begin
  inherited;
  Repeating := True;
end;

procedure TSpTBXSpinButton.Click;
var
  P: TPoint;
begin
  FUpPushed := False;
  FDownPushed := False;
  if Enabled then begin
    GetCursorPos(P);
    P := ScreenToClient(P);
    if P.Y < Height div 2 then begin
      FUpPushed := True;
      if Assigned(FOnUpClick) then FOnUpClick(Self);
    end
    else begin
      FDownPushed := True;
      if Assigned(FOnDownClick) then FOnDownClick(Self);
    end;
  end;

  inherited;
end;

function TSpTBXSpinButton.DoDrawItem(ACanvas: TCanvas; ARect: TRect;
  const PaintStage: TSpTBXPaintStage): Boolean;
var
  UpHotTrack, DownHotTrack, EditFrameHotTrack, RightAligned: Boolean;
begin
  // Draw rectangle buttons
  if (PaintStage = pstPrePaint) and not BitmapValid then begin
    Result := True;
    if Assigned(OnDraw) then OnDraw(Self, ACanvas, ARect, PaintStage, Result);
    if Result then begin
      IsHotTracking(UpHotTrack, DownHotTrack, EditFrameHotTrack);
      RightAligned := Align <> alLeft;
      SpDrawXPSpinButton(ACanvas, ARect, Enabled, EditFrameHotTrack, UpHotTrack, DownHotTrack, FUpPushed, FDownPushed, RightAligned);
    end;
  end
  else
    Result := inherited DoDrawItem(ACanvas, ARect, PaintStage);
end;

procedure TSpTBXSpinButton.IsHotTracking(out UpButton, DownButton, EditFrame: Boolean);
var
  Edit: TSpTBXEdit;
  P: TPoint;
  R: TRect;
begin
  UpButton := False;
  DownButton := False;
  EditFrame := False;

  if GetCursorPos(P) then begin
    P := ScreenToClient(P);
    R := Rect(0, 0, Width, Height div 2);
    UpButton := PtInRect(R, P);
    if not UpButton then begin
      R := Rect(0, Height div 2, Width, Height);
      DownButton := PtInRect(R, P);
    end;
  end;

  if Assigned(Owner) and (Owner is TSpTBXEdit) then begin
    Edit := Owner as TSpTBXEdit;
    if Edit.HotTrack then
      EditFrame := Edit.MouseInControl or Edit.Focused;
  end;
end;

procedure TSpTBXSpinButton.DoMouseLeave;
begin
  FUpPushed := False;
  FDownPushed := False;
  inherited;
end;

procedure TSpTBXSpinButton.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FUpPushed := False;
  FDownPushed := False;
  inherited;
end;

procedure TSpTBXSpinButton.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if Enabled then
    Repaint;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXUnicodeAdaptEdit }

{$IFNDEF UNICODE}
procedure TSpTBXUnicodeAdaptEdit.CreateWindowHandle(const Params: TCreateParams);
begin
  TntCustomEdit_CreateWindowHandle(Self, Params);
end;

procedure TSpTBXUnicodeAdaptEdit.CreateWnd;
begin
  inherited;
  TntCustomEdit_AfterInherited_CreateWnd(Self, FPasswordChar);
end;

procedure TSpTBXUnicodeAdaptEdit.DefineProperties(Filer: TFiler);
begin
  inherited;
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;

function TSpTBXUnicodeAdaptEdit.GetSelStart: Integer;
begin
  Result := TntCustomEdit_GetSelStart(Self);
end;

procedure TSpTBXUnicodeAdaptEdit.SetSelStart(const Value: Integer);
begin
  TntCustomEdit_SetSelStart(Self, Value);
end;

function TSpTBXUnicodeAdaptEdit.GetSelLength: Integer;
begin
  Result := TntCustomEdit_GetSelLength(Self);
end;

procedure TSpTBXUnicodeAdaptEdit.SetSelLength(const Value: Integer);
begin
  TntCustomEdit_SetSelLength(Self, Value);
end;

function TSpTBXUnicodeAdaptEdit.GetSelText: WideString;
begin
  Result := TntCustomEdit_GetSelText(Self);
end;

procedure TSpTBXUnicodeAdaptEdit.SetSelText(const Value: WideString);
begin
  TntCustomEdit_SetSelText(Self, Value);
end;

function TSpTBXUnicodeAdaptEdit.GetPasswordChar: WideChar;
begin
  Result := TntCustomEdit_GetPasswordChar(Self, FPasswordChar);
end;

procedure TSpTBXUnicodeAdaptEdit.SetPasswordChar(const Value: WideChar);
begin
  TntCustomEdit_SetPasswordChar(Self, FPasswordChar, Value);
end;

function TSpTBXUnicodeAdaptEdit.GetText: WideString;
begin
  Result := TntControl_GetText(Self);
end;

procedure TSpTBXUnicodeAdaptEdit.SetText(const Value: WideString);
begin
  TntControl_SetText(Self, Value);
end;

function TSpTBXUnicodeAdaptEdit.IsHintStored: Boolean;
begin
  Result := TntControl_IsHintStored(Self);
end;

function TSpTBXUnicodeAdaptEdit.GetHint: WideString;
begin
  Result := TntControl_GetHint(Self);
end;

procedure TSpTBXUnicodeAdaptEdit.SetHint(const Value: WideString);
begin
  TntControl_SetHint(Self, Value);
end;

procedure TSpTBXUnicodeAdaptEdit.ActionChange(Sender: TObject; CheckDefaults: Boolean);
begin
  TntControl_BeforeInherited_ActionChange(Self, Sender, CheckDefaults);
  inherited;
end;

function TSpTBXUnicodeAdaptEdit.GetActionLinkClass: TControlActionLinkClass;
begin
  Result := TntControl_GetActionLinkClass(Self, inherited GetActionLinkClass);
end;
{$ENDIF}

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXUnicodeEdit }

procedure TSpTBXUnicodeEdit.CreateWnd;
begin
  inherited;
  if HandleAllocated then UpdateEditRect;
end;

procedure TSpTBXUnicodeEdit.CreateParams(var Params: TCreateParams);
const
  Alignments: array[TAlignment] of Cardinal = (ES_LEFT, ES_RIGHT, ES_CENTER);
begin
  inherited CreateParams(Params);
  // WS_CLIPCHILDREN needed for edit buttons
  Params.Style := Params.Style or Alignments[FAlignment] or WS_CLIPCHILDREN;
end;

procedure TSpTBXUnicodeEdit.SetAlignment(Value: TAlignment);
begin
  if Value <> FAlignment then begin
    FAlignment := Value;
    RecreateWnd;
  end;
end;

function TSpTBXUnicodeEdit.AddEditButton(RightAligned: Boolean;
  AWidth: Integer): TSpTBXEditButton;
begin
  Result := TSpTBXEditButton.Create(Self);
  Result.Parent := Self;
  Result.FreeNotification(Self);
  if RightAligned then
    Result.Align := alRight
  else
    Result.Align := alLeft;
  if AWidth = -1 then
    Result.Width := GetSystemMetrics(SM_CXVSCROLL)
  else
    Result.Width := AWidth;
  UpdateEditRect;
end;

function TSpTBXUnicodeEdit.HasEditButton: Boolean;
var
  I: Integer;
begin
  Result := False;
  if not HandleAllocated then Exit;
  for I := 0 to ControlCount - 1 do begin
    if Controls[I] is TSpTBXEditButton then begin
      Result := True;
      Break;
    end;
  end;
end;

procedure TSpTBXUnicodeEdit.UpdateEditRect;
var
  I, X1, X2: Integer;
  B: TSpTBXEditButton;
begin
  if not HandleAllocated then Exit;

  X1 := 0;
  X2 := 0;

  for I := 0 to ControlCount - 1 do begin
    if Controls[I] is TSpTBXEditButton then begin
      B := Controls[I] as TSpTBXEditButton;
      if B.Visible then
        case B.Align of
          alLeft: X1 := X1 + B.Width;
          alRight: X2 := X2 + B.Width;
        end;
    end;
  end;

  if X1 > 0 then Inc(X1, 2);
  if X2 > 0 then Inc(X2, 2);

  SendMessage(Handle, EM_SETMARGINS, EC_LEFTMARGIN or EC_RIGHTMARGIN, MakeLParam(X1, X2));
end;

procedure TSpTBXUnicodeEdit.CMEnabledChanged(var Message: TMessage);
var
  I: Integer;
begin
  inherited;
  for I := 0 to ControlCount - 1 do
    if Controls[I] is TSpTBXEditButton then
      TSpTBXEditButton(Controls[I]).Enabled := Enabled;
end;

procedure TSpTBXUnicodeEdit.CMFontChanged(var Message: TMessage);
begin
  inherited;
  UpdateEditRect;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXEdit }

constructor TSpTBXEdit.Create(AOwner: TComponent);
begin
  inherited;
  FHotTrack := True;
  FBorderStyle := bsSingle;
  SkinManager.AddSkinNotification(Self);

  if not (csDesigning in ComponentState) then
    DoubleBuffered := True;
end;

destructor TSpTBXEdit.Destroy;
begin
  SkinManager.RemoveSkinNotification(Self);
  inherited;
end;

procedure TSpTBXEdit.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  FMouseInControl := False;
  if FHotTrack then InvalidateFrame;
end;

procedure TSpTBXEdit.CMEnter(var Message: TCMEnter);
begin
  inherited;
  FMouseInControl := True;
  if FHotTrack then InvalidateFrame;
end;

procedure TSpTBXEdit.CMExit(var Message: TCMExit);
begin
  inherited;
  FMouseInControl := False;
  if FHotTrack then InvalidateFrame;
end;

procedure TSpTBXEdit.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if not FMouseInControl then begin
    FMouseInControl := True;
    if FHotTrack and not Focused and (HasEditButton or SpCanEditFrameBeHotTracked(BorderStyle)) then
      InvalidateFrame;
  end;
end;

procedure TSpTBXEdit.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if FMouseInControl then begin
    FMouseInControl := False;
    if FHotTrack and not Focused and (HasEditButton or SpCanEditFrameBeHotTracked(BorderStyle)) then
      InvalidateFrame;
  end;
end;

procedure TSpTBXEdit.InvalidateFrame;
begin
  if HandleAllocated then
    RedrawWindow(Handle, nil, 0, RDW_FRAME or RDW_ERASE or RDW_INVALIDATE or RDW_ALLCHILDREN);
end;

procedure TSpTBXEdit.SetBorderStyle(const Value: TBorderStyle);
begin
  if FBorderStyle <> Value then begin
    FBorderStyle := Value;
    InvalidateFrame;
  end;
end;

procedure TSpTBXEdit.WMNCPaint(var Message: TWMNCPaint);
var
  HotTrackFrame: Boolean;
begin
  if FHotTrack then
    HotTrackFrame := FMouseInControl or Focused
  else
    HotTrackFrame := False;

  if (SkinManager.GetSkinType = sknNone) and (FBorderStyle <> bsNone) then
    inherited
  else
    if Ctl3D then
      SpDrawXPEditFrame(Self, HotTrackFrame, False, FBorderStyle = bsNone);
end;

procedure TSpTBXEdit.WMSpSkinChange(var Message: TMessage);
begin
  inherited;
  InvalidateFrame;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXCustomButtonEdit }

constructor TSpTBXCustomButtonEdit.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle - [csSetCaption];
  FEditButton := AddEditButton(True, 19);
end;

destructor TSpTBXCustomButtonEdit.Destroy;
begin
  FreeAndNil(FEditButton);
  inherited;
end;

procedure TSpTBXCustomButtonEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (AComponent = FEditButton) and (Operation = opRemove) then
    FEditButton := nil;
end;

procedure TSpTBXCustomButtonEdit.SetName(const Value: TComponentName);
begin
  inherited SetName(Value);
  if not (csLoading in ComponentState) then begin
    FEditButton.Caption := '...';
    Text := '';
  end;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXSpinEditOptions }

constructor TSpTBXSpinEditOptions.Create;
begin
  inherited Create;
  FDecimal := 2;
  FIncrement := 1;
  FValueSnap := True;
  FValueType := spnInteger;
end;

procedure TSpTBXSpinEditOptions.DoValueChanged;
begin
  if Assigned(FOnValueChanged) then FOnValueChanged(Self);
end;

function TSpTBXSpinEditOptions.GetValueAsInteger: Int64;
begin
  Result := Round(Value);
end;

function TSpTBXSpinEditOptions.IsIncrementStored: Boolean;
begin
  Result := FIncrement <> 1;
end;

function TSpTBXSpinEditOptions.IsMaxValueStored: Boolean;
begin
  Result := FMaxValue <> 0;
end;

function TSpTBXSpinEditOptions.IsMinValueStored: Boolean;
begin
  Result := FMinValue <> 0;
end;

function TSpTBXSpinEditOptions.IsValueStored: Boolean;
begin
  Result := FValue <> 0;
end;

procedure TSpTBXSpinEditOptions.SetDecimal(NewDecimal: Integer);
begin
  if NewDecimal > 10 then NewDecimal := 10;
  if NewDecimal < 0 then NewDecimal := 0;
  if NewDecimal <> FDecimal then begin
    FDecimal := NewDecimal;
    UpdateTextFromValue;
  end;
end;

procedure TSpTBXSpinEditOptions.SetMaxValue(const NewValue: Extended);
begin
  if NewValue <> FMaxValue then begin
    FMaxValue := NewValue;
    if FValue > NewValue then SetValue(NewValue);
  end;
end;

procedure TSpTBXSpinEditOptions.SetMinValue(const NewValue: Extended);
begin
  if NewValue <> FMinValue then begin
    FMinValue := NewValue;
    if FValue < NewValue then SetValue(NewValue);
  end;
end;

procedure TSpTBXSpinEditOptions.SetPostfix(const ValueString: WideString);
begin
  if FPostfix <> ValueString then begin
    FPostfix := ValueString;
    UpdateTextFromValue;
  end;
end;

procedure TSpTBXSpinEditOptions.SetPrefix(const ValueString: WideString);
begin
  if FPrefix <> ValueString then begin
    FPrefix := ValueString;
    UpdateTextFromValue;
  end;
end;

procedure TSpTBXSpinEditOptions.SetValue(const NewValue: Extended);
begin
  if NewValue <> FValue then
    if (FMaxValue = FMinValue) or
      (FMaxValue <> FMinValue) and (NewValue >= FMinValue) and (NewValue <= FMaxValue) then
    begin
      FValue := NewValue;
      DoValueChanged;
      UpdateTextFromValue;
    end;
end;

procedure TSpTBXSpinEditOptions.SetValueAsInteger(const NewValue: Int64);
begin
  Value := NewValue;
end;

procedure TSpTBXSpinEditOptions.SetValueType(NewType: TSpTBXSpinType);
begin
  if NewType <> FValueType then begin
    FValueType := NewType;
    if NewType in [spnInteger, spnHex] then FIncrement := Max(Round(FIncrement), 1);
    UpdateTextFromValue;
  end;
end;

procedure TSpTBXSpinEditOptions.UpdateTextFromValue;
var
  WS: WideString;
begin
  WS := '';
  case FValueType of
    spnInteger: WS := IntToStr(Round(FValue));
    spnFloat:   WS := FloatToStrF(FValue, ffFixed, 15, FDecimal);
    spnHex:     WS := IntToHex(Round(FValue), 1);
  end;
  if Assigned(FOnSetText) then FOnSetText(Self, FPrefix + WS + FPostfix);
end;

procedure TSpTBXSpinEditOptions.UpdateValueFromText(RevertWhenInvalid: Boolean = True);
var
  WS: WideString;
  PrevValue, NewValue: Extended;
  I: Integer;
  Dummy: Boolean;
begin
  PrevValue := FValue;
  NewValue := FValue;
  Dummy := True;
  WS := '';
  if Assigned(FOnGetText) then FOnGetText(Self, WS, Dummy);

  // Remove the Prefix and Postfix from the text
  I := Pos(Prefix, WS);
  if I > 0 then
    Delete(WS, I, Length(Prefix));
  I := Pos(Postfix, WS);
  if I > 0 then
    Delete(WS, I, Length(Postfix));

  // Try to parse the text to get the value
  WS := Trim(WS);
  if Length(WS) > 0 then begin
    case FValueType of
      spnInteger: NewValue := StrToInt64Def(WS, Round(PrevValue));
      spnFloat:   NewValue := StrToFloatDef(WS, PrevValue);
      spnHex:     NewValue := StrToInt64Def('$' + WS, Round(PrevValue));
    end;
  end;

  if RevertWhenInvalid or (NewValue <> PrevValue) then begin
    SetValue(NewValue);
    UpdateTextFromValue;
  end;
end;

procedure TSpTBXSpinEditOptions.ValueInc;
var
  NewValue: Extended;
begin
  if FValueSnap then
    NewValue := Math.Floor(FValue / FIncrement + 1 + FIncrement * 0.0000000001) * FIncrement
  else
    NewValue := FValue + FIncrement;
  SetValue(NewValue);
end;

procedure TSpTBXSpinEditOptions.ValueDec;
var
  NewValue: Extended;
begin
  if FValueSnap then
    NewValue := Math.Ceil(FValue / FIncrement - 1 - FIncrement * 0.0000000001) * FIncrement
  else
    NewValue := FValue - FIncrement;
  SetValue(NewValue);
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXSpinEdit }

constructor TSpTBXSpinEdit.Create(AOwner: TComponent);
begin
  inherited;
  Alignment := taRightJustify;

  FSpinOptions := TSpTBXSpinEditOptions.Create;
  FSpinOptions.OnGetText := SpinOptionsGetText;
  FSpinOptions.OnSetText := SpinOptionsSetText;

  FSpinButton := TSpTBXSpinButton.Create(Self);
  FSpinButton.Parent := Self;
  FSpinButton.FreeNotification(Self);
  FSpinButton.OnUpClick := UpClick;
  FSpinButton.OnDownClick := DownClick;
  FSpinButton.Align := alRight;
  FSpinButton.Width := DefaultSpinButtonSize;
  UpdateEditRect;

  Text := '0';
end;

destructor TSpTBXSpinEdit.Destroy;
begin
  FreeAndNil(FSpinOptions);
  FreeAndNil(FSpinButton);
  inherited;
end;

procedure TSpTBXSpinEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (AComponent = FSpinButton) and (Operation = opRemove) then
    FSpinButton := nil;
end;

procedure TSpTBXSpinEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  case Key of
    VK_UP: SpinOptions.ValueInc;
    VK_DOWN: SpinOptions.ValueDec;
  end;
end;

procedure TSpTBXSpinEdit.KeyPress(var Key: Char);
begin
  inherited;
  if Key = #13 then begin
    Key := #0;
    SpinOptions.UpdateValueFromText;
  end;
end;

procedure TSpTBXSpinEdit.UpClick(Sender: TObject);
begin
  SpinOptions.ValueInc;
end;

procedure TSpTBXSpinEdit.DownClick(Sender: TObject);
begin
  SpinOptions.ValueDec;
end;

procedure TSpTBXSpinEdit.Change;
begin
  if FExtendedAccept then
    SpinOptions.UpdateValueFromText(False); // Don't revert when an invalid text is entered
  inherited;
end;

function TSpTBXSpinEdit.GetValue: Extended;
begin
  Result := SpinOptions.Value;
end;

procedure TSpTBXSpinEdit.SetValue(const Value: Extended);
begin
  SpinOptions.Value := Value;
end;

function TSpTBXSpinEdit.GetValueChanged: TNotifyEvent;
begin
  Result := SpinOptions.OnValueChanged;
end;

procedure TSpTBXSpinEdit.SetValueChanged(const ValueChangedEvent: TNotifyEvent);
begin
  SpinOptions.OnValueChanged := ValueChangedEvent;
end;

procedure TSpTBXSpinEdit.SpinOptionsGetText(Sender: TObject;
  var NewText: WideString; var Accept: Boolean);
begin
  // Event used by SpinOptions to get the text from the edit control
  NewText := Text;
end;

procedure TSpTBXSpinEdit.SpinOptionsSetText(Sender: TObject;
  const AText: WideString);
var
  L, L2: Integer;
begin
  // Event used by SpinOptions to set the edit control text
  if Text = AText then Exit;

  // Change the EditControl text and reposition the edit caret
  L := Length(Text);
  Text := AText;
  L2 := Length(Text);
  if L2 > L then
    SelStart := L + Length(SpinOptions.Prefix);
end;

procedure TSpTBXSpinEdit.WMKillFocus(var Message: TWMKillFocus);
begin
  SpinOptions.UpdateValueFromText;
  inherited;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXComboBox }

constructor TSpTBXComboBox.Create(AOwner: TComponent);
begin
  inherited;
  FAutoDropDownWidthRightMargin := 8;
  FAutoItemHeight := True;
  FMouseTimer := nil;
  FHotTrack := True;
  SkinManager.AddSkinNotification(Self);

//  Commented because of conflict when painting on Glass
//  if not (csDesigning in ComponentState) then
//    DoubleBuffered := True;
end;

procedure TSpTBXComboBox.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);

  // Force the ComboBox to be owner draw
  with Params do
    if Style and (CBS_OWNERDRAWFIXED or CBS_OWNERDRAWVARIABLE) = 0 then
      Style := Style or LBS_OWNERDRAWFIXED;
end;

procedure TSpTBXComboBox.CreateWnd;
begin
  inherited;
  DoCalcMaxDropDownWidth;
end;

destructor TSpTBXComboBox.Destroy;
begin
  SkinManager.RemoveSkinNotification(Self);
  if Assigned(FMouseTimer) then begin
    FMouseTimer.Enabled := False;
    FreeAndNil(FMouseTimer);
  end;
  inherited;
end;

procedure TSpTBXComboBox.CloseUp;
begin
  inherited;
  InvalidateFrame;
end;

procedure TSpTBXComboBox.CMEnter(var Message: TCMEnter);
begin
  inherited;
  FMouseInControl := True;
  InvalidateFrame;
end;

procedure TSpTBXComboBox.CMExit(var Message: TCMExit);
begin
  inherited;
  FMouseInControl := False;
  InvalidateFrame;
end;

procedure TSpTBXComboBox.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if Message.LParam = 0 then begin
    if not (csDestroying in ComponentState) and not FMouseInControl and HandleAllocated then begin
      FMouseInControl := True;
      if FHotTrack and not Focused then
        InvalidateFrame;
      if not Assigned(FMouseTimer) then begin
        FMouseTimer := TTimer.Create(nil);
        FMouseTimer.Enabled := False;
        FMouseTimer.Interval := 125;
        FMouseTimer.OnTimer := MouseTimerHandler;
        FMouseTimer.Enabled := True;
      end;
    end;
  end;
end;

procedure TSpTBXComboBox.CNDrawItem(var Message: TWMDrawItem);
var
  State: TOwnerDrawState;
begin
  with Message.DrawItemStruct^ do
  begin
    State := TOwnerDrawState(LongRec(itemState).Lo);
    if itemState and ODS_COMBOBOXEDIT <> 0 then
      Include(State, odComboBoxEdit);
    if itemState and ODS_DEFAULT <> 0 then
      Include(State, odDefault);

    Canvas.Handle := hDC;
    Canvas.Lock;
    try
      Canvas.Font := Font;
      Canvas.Brush := Brush;
      if (Integer(itemID) >= 0) and (Integer(itemID) < Items.Count) then begin
        DrawItemBackground(itemID, rcItem, State);
        TControlCanvas(Canvas).UpdateTextFlags;
        Canvas.Brush.Style := bsClear;
        DrawItem(itemID, rcItem, State);
      end
      else
        Canvas.FillRect(rcItem);
    finally
      Canvas.Unlock;
      Canvas.Handle := 0;
    end;
  end;
end;

procedure TSpTBXComboBox.MouseTimerHandler(Sender: TObject);
var
  P: TPoint;
  R: TRect;
  InControl: Boolean;
begin
  if not (csDestroying in ComponentState) and not DroppedDown and HandleAllocated and GetCursorPos(P) then begin
    GetWindowRect(Handle, R);
    InControl := PtInRect(R, P);
    if InControl <> FMouseInControl then begin
      FMouseInControl := InControl;
      if FHotTrack and not Focused then InvalidateFrame;
    end;

    if not InControl then begin
      FMouseTimer.Enabled := False;
      FreeAndNil(FMouseTimer);
    end;
  end;
end;

procedure TSpTBXComboBox.DoCalcMaxDropDownWidth;
begin
  if FAutoDropDownWidth then
    SpCalcMaxDropDownWidth(Self, FAutoDropDownWidthRightMargin);
end;

procedure TSpTBXComboBox.DoDrawBackground(ACanvas: TCanvas; ARect: TRect;
  const PaintStage: TSpTBXPaintStage; var PaintDefault: Boolean);
begin
  if Assigned(FOnDrawBackground) then FOnDrawBackground(Self, ACanvas, ARect,
    PaintStage, PaintDefault);
end;

procedure TSpTBXComboBox.DoDrawItem(ACanvas: TCanvas; var ARect: TRect;
  Index: Integer; const State: TOwnerDrawState;
  const PaintStage: TSpTBXPaintStage; var PaintDefault: Boolean);
begin
  if Assigned(FOnDrawItem) then FOnDrawItem(Self, ACanvas, ARect, Index, State, PaintStage, PaintDefault);
end;

procedure TSpTBXComboBox.DoDrawItemBackground(ACanvas: TCanvas;
  var ARect: TRect; Index: Integer; const State: TOwnerDrawState;
  const PaintStage: TSpTBXPaintStage; var PaintDefault: Boolean);
begin
  if Assigned(FOnDrawItemBackground) then FOnDrawItemBackground(Self, ACanvas, ARect, Index, State, PaintStage, PaintDefault);
end;

procedure TSpTBXComboBox.DrawItem(Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
var
  Flags: Integer;
  PaintDefault: Boolean;
begin
  // Draw the item text
  PaintDefault := True;
  DoDrawItem(Canvas, Rect, Index, State, pstPrePaint, PaintDefault);
  if PaintDefault then begin
    Flags := DrawTextBiDiModeFlags(DT_SINGLELINE or DT_VCENTER or DT_NOPREFIX);
    Inc(Rect.Left, 2);

    // [Bugfix] Delphi 7-2009 bug:
    // When the Style is set to csDropDownList and the control is disabled the
    // text is not painted with csGrayText. This is a VCL bug the same happens
    // with TComboBox when you set it to csDropDownFixed (TSpTBXComboBox uses
    // csDropDownFixed instead of csDropDownList).
    if odDisabled in State then
      Canvas.Font.Color := clGrayText;
    SpDrawXPText(Canvas, Items[Index], Rect, Flags);
  end;

  PaintDefault := True;
  DoDrawItem(Canvas, Rect, Index, State, pstPostPaint, PaintDefault);
end;

procedure TSpTBXComboBox.DrawItemBackground(Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
var
  PaintDefault: Boolean;
begin
  // Draw the list items background
  PaintDefault := True;
  DoDrawItemBackground(Canvas, Rect, Index, State, pstPrePaint, PaintDefault);
  if PaintDefault then
    SpDrawXPListItemBackground(Canvas, Rect, odSelected in State, False, odFocused in State);

  PaintDefault := True;
  DoDrawItemBackground(Canvas, Rect, Index, State, pstPostPaint, PaintDefault);
end;

{$IF CompilerVersion > 17}
procedure TSpTBXComboBox.EditWndProc(var Message: TMessage);
begin
  // [Bugfix] Delphi 2006/2007 bug:
  // CM_MOUSEENTER and CM_MOUSELEAVE are fired everytime the mouse
  // enters the combobox internal edit control.
  // In D7 these messages were only fired when the mouse entered or leaved
  // the combobox, including the internal edit control.
  // We need to block the mouse messages from the internal edit control
  // in EditWndProc
  if Message.Msg = WM_MOUSEMOVE then
    ComboWndProc(Message, FEditHandle, FDefEditProc)
  else
    inherited;
end;
{$IFEND}

function TSpTBXComboBox.GetDropDownButtonRect: TRect;
var
  ButtonWidth: Integer;
begin
  if Style = csSimple then
    ButtonWidth := 0
  else
    ButtonWidth := GetSystemMetrics(SM_CXHSCROLL);
  Result.Left := Width - ButtonWidth;
  Result.Top := 0;
  Result.Right := Result.Left + ButtonWidth;
  Result.Bottom := Height;

  case SkinManager.GetSkinType of
    sknNone:
      begin
        InflateRect(Result, 0, -1);
        OffsetRect(Result, -1, 0);
      end;
    sknWindows, sknDelphiStyle:
      begin
        InflateRect(Result, 0, -1);
        OffsetRect(Result, -1, 0);
      end;
    sknSkin:
      begin
        InflateRect(Result, 0, -2);
        OffsetRect(Result, -2, 0);
      end;
  end;
end;

function TSpTBXComboBox.GetMouseInDropDownButton: Boolean;
var
  P: TPoint;
  ButtonR: TRect;
  ButtonWidth: Integer;
begin
  Result := False;

  if not (csDesigning in ComponentState) and GetCursorPos(P) then begin
    P := ScreenToClient(P);
    if Style = csSimple then
      ButtonWidth := 0
    else
      ButtonWidth := GetSystemMetrics(SM_CXHSCROLL);
    ButtonR.Left := Width - ButtonWidth;
    ButtonR.Top := 0;
    ButtonR.Right := ButtonR.Left + ButtonWidth;
    ButtonR.Bottom := Height;

    Result := PtInRect(ButtonR, P);
  end;
end;

procedure TSpTBXComboBox.InvalidateFrame;
begin
  if HandleAllocated then
    Invalidate;
end;

procedure TSpTBXComboBox.UpdateDropDownButton;
var
  ButtonState: Boolean;
begin
  if not DroppedDown and not Focused then begin
    ButtonState := GetMouseInDropDownButton;
    if ButtonState <> FMouseInDropDownButton then
      InvalidateFrame;
    FMouseInDropDownButton := ButtonState;
  end;
end;

function TSpTBXComboBox.GetItemHt: Integer;
// Automatically update the Height/ItemHeight when Style is csDropDown,
// csDropDownList or csSimple
begin
  // CB_GETITEMHEIGHT doesn't work when Style is csOwnerDrawFixed or
  // csOwnerDrawVariable.
  // Since TSpTBXComboBox is always owner-drawed we must calculate the
  // ItemHeight based on the font when Style <> csOwnerDrawFixed/csOwnerDrawVariable.
  // Look at TCustomComboBox.GetItemHt
  if Style in [csOwnerDrawFixed, csOwnerDrawVariable] then
    Result := inherited GetItemHt
  else
    if not FAutoItemHeight then  // When AutoItemHeight is turned off return the ItemHeight
      Result := FInternalItemHeight
    else
      Result := SpGetControlTextHeight(Self, Font);
end;

{$IF CompilerVersion > 20}
function TSpTBXComboBox.IsItemHeightStored: Boolean;
// [Bugfix] Delphi 2010 bug:
// ItemHeight is not stored when Style is different than csOwnerDraw*
begin
  Result := True;
end;
{$IFEND}

procedure TSpTBXComboBox.SetItemHeight(Value: Integer);
begin
  if Value > 0 then
    FInternalItemHeight := Value;
  inherited;
end;

procedure TSpTBXComboBox.WMMouseMove(var Message: TWMMouseMove);
begin
  inherited;
  UpdateDropDownButton;
end;

procedure TSpTBXComboBox.WMNCCalcSize(var Message: TWMNCCalcSize);
begin
  // [Bugfix] Delphi 2006 bug:
  // Do nothing, fix Delphi 2005/2006 bug: http://qc.borland.com/wc/qcmain.aspx?d=13852
end;

procedure TSpTBXComboBox.WMPaint(var Message: TWMPaint);
var
  ACanvas: TControlCanvas;
  R, ButtonR: TRect;
  ButtonWidth: Integer;
  PaintDefault, HotTrackFrame, VistaNewComCtrls: Boolean;
begin
  inherited;

  ACanvas := TControlCanvas.Create;
  try
    ACanvas.Control := Self;
    ACanvas.Lock; // lock the canvas to prevent flicker on mouse click
    GetWindowRect(Handle, R);
    OffsetRect(R, -R.Left, -R.Top);

    if Style = csSimple then
      ButtonWidth := 0
    else
      ButtonWidth := GetSystemMetrics(SM_CXHSCROLL);

    ExcludeClipRect(ACanvas.Handle, 2, 2, R.Right - 2 - ButtonWidth, R.Bottom - 2);
    try
      PaintDefault := True;
      DoDrawBackground(ACanvas, R, pstPrePaint, PaintDefault);

      // Don't custom paint if we are on Vista with ComCtrls 6, let the
      // OS draw the frame
      VistaNewComCtrls := not (csDesigning in ComponentState) and SpIsWinVistaOrUp and (SkinManager.GetSkinType = sknWindows);

      if PaintDefault and (SkinManager.GetSkinType <> sknNone) and not VistaNewComCtrls then begin
        if csDesigning in ComponentState then
          HotTrackFrame := False
        else
          if FHotTrack then
            HotTrackFrame := FMouseInControl or Focused
          else
            HotTrackFrame := DroppedDown;

        ButtonR := GetDropDownButtonRect;
        if SkinManager.GetSkinType = sknSkin then
          SpDrawParentBackground(Self, ACanvas.Handle, R);
        SpDrawXPEditFrame(ACanvas, R, Enabled, HotTrackFrame);
        if Style <> csSimple then
          SpDrawXPComboButton(ACanvas, ButtonR, Enabled, HotTrackFrame, GetMouseInDropDownButton, DroppedDown, True);
      end;

      PaintDefault := True;
      DoDrawBackground(ACanvas, R, pstPostPaint, PaintDefault);
    finally
      SelectClipRgn(ACanvas.Handle, 0);
    end;
  finally
    ACanvas.Unlock;
    ACanvas.Free;
  end;
end;

procedure TSpTBXComboBox.CNMeasureItem(var Message: TWMMeasureItem);
// Automatically update the Height/ItemHeight when Style is csDropDown,
// csDropDownList or csSimple
// Recalc ItemHeight based on the font
var
  I: Integer;
begin
  inherited;
  if not (Style in [csOwnerDrawFixed, csOwnerDrawVariable]) then begin
    // When itemID = $FFFFFFFFFF the itemHeight is refering to the
    // editbox height
    if Message.MeasureItemStruct.itemID = High(LongWord) then begin
      I := SpGetControlTextHeight(Self, Font);
      Inc(I, 2);
    end
    else
      I := GetItemHt;
    Message.MeasureItemStruct^.itemHeight := I;
  end;
end;

procedure TSpTBXComboBox.CMSPFontChanged(var Message: TMessage);
// Automatically update the Height/ItemHeight when Style is csDropDown,
// csDropDownList or csSimple
// Recreate when the font is changed
begin
  if not FFontChanging and not (Style in [csOwnerDrawFixed, csOwnerDrawVariable]) then begin
    FFontChanging := True;
    if not DroppedDown then RecreateWnd;
    FFontChanging := False;
  end;
end;

procedure TSpTBXComboBox.WMSetFont(var Message: TWMSetFont);
// Automatically update the Height/ItemHeight when Style is csDropDown,
// csDropDownList or csSimple
// Recreate when the font is changed
begin
  inherited;
  if not FFontChanging and not (Style in [csOwnerDrawFixed, csOwnerDrawVariable]) then
    PostMessage(Handle, CM_SPFONTCHANGED, 0, 0);
end;

procedure TSpTBXComboBox.WMSpSkinChange(var Message: TMessage);
begin
  inherited;
  InvalidateFrame;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXListBox }

constructor TSpTBXListBox.Create(AOwner: TComponent);
begin
  inherited;
  FHotTrack := True;
  SkinManager.AddSkinNotification(Self);
  Style := lbOwnerDrawFixed;
end;

procedure TSpTBXListBox.CreateParams(var Params: TCreateParams);
begin
  inherited;
  // Force the ListBox to be owner draw
  with Params do
    if Style and (LBS_OWNERDRAWFIXED or LBS_OWNERDRAWVARIABLE) = 0 then
      Style := Style or LBS_OWNERDRAWFIXED;
end;

destructor TSpTBXListBox.Destroy;
begin
  SkinManager.RemoveSkinNotification(Self);
  inherited;
end;

procedure TSpTBXListBox.InvalidateBorders;
begin
  if HandleAllocated then
    RedrawWindow(Handle, nil, 0, RDW_FRAME or RDW_INVALIDATE);
end;

procedure TSpTBXListBox.SetHotTrack(const Value: Boolean);
begin
  if FHotTrack <> Value then begin
    FHotTrack := Value;
    InvalidateBorders;
  end;
end;

procedure TSpTBXListBox.CMFocusChanged(var Message: TCMFocusChanged);
begin
  inherited;
  if FHotTrack and Assigned(Message.Sender) then begin
    FChildFocused := Self = Message.Sender;
    if FChildFocused <> FHotTracking then begin
      FHotTracking := FChildFocused;
      InvalidateBorders;
    end;
  end;
end;

procedure TSpTBXListBox.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if FHotTrack and not FHotTracking then begin
    FHotTracking := True;
    if SpCanEditFrameBeHotTracked(BorderStyle) then
      InvalidateBorders;
  end;
end;

procedure TSpTBXListBox.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if FHotTrack and FHotTracking and not FChildFocused then begin
    FHotTracking := False;
    if SpCanEditFrameBeHotTracked(BorderStyle) then
      InvalidateBorders;
  end;
end;

procedure TSpTBXListBox.WMSpSkinChange(var Message: TMessage);
begin
  inherited;
  InvalidateBorders;
end;

procedure TSpTBXListBox.DoDrawItem(ACanvas: TCanvas; var ARect: TRect;
  Index: Integer; const State: TOwnerDrawState;
  const PaintStage: TSpTBXPaintStage; var PaintDefault: Boolean);
begin
  if Assigned(FOnDrawItem) then FOnDrawItem(Self, ACanvas, ARect, Index, State, PaintStage, PaintDefault);
end;

procedure TSpTBXListBox.DoDrawItemBackground(ACanvas: TCanvas; var ARect: TRect;
  Index: Integer; const State: TOwnerDrawState;
  const PaintStage: TSpTBXPaintStage; var PaintDefault: Boolean);
begin
  if Assigned(FOnDrawItemBackground) then FOnDrawItemBackground(Self, ACanvas, ARect, Index, State, PaintStage, PaintDefault);
end;

procedure TSpTBXListBox.DrawItem(Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
var
  R: TRect;
  Flags: Integer;
  PaintDefault: Boolean;
begin
  // Draw the item text
  PaintDefault := True;
  DoDrawItem(Canvas, Rect, Index, State, pstPrePaint, PaintDefault);
  if PaintDefault then begin
    Flags := DrawTextBiDiModeFlags(DT_SINGLELINE or DT_VCENTER or DT_NOPREFIX);

    // It seems TabWidth doesn't work on owner-drawed listboxes, we have to do
    // it manually using DT_EXPANDTABS.
    // It seems that DrawText uses a different unit metric than LB_SETTABSTOPS,
    // don't know how to calculate it correctly for more info:
    // http://news.jrsoftware.org/read/article.php?id=15427&group=jrsoftware.toolbar2000.thirdparty#15427
    // Using DrawTextEx doesn't solve the problem, GetDialogBaseUnits doesn't help either
    if TabWidth > 0 then
      Flags := ((Flags or DT_EXPANDTABS or DT_TABSTOP) and not $800) or (Round(TabWidth * 0.3) shl 8);

    // Add a margin to the rect
    R := Rect;
    if not UseRightToLeftAlignment then
      Inc(R.Left, 3)
    else
      Dec(R.Right, 3);
    SpDrawXPText(Canvas, Items[Index], R, Flags);
  end;

  PaintDefault := True;
  DoDrawItem(Canvas, Rect, Index, State, pstPostPaint, PaintDefault);
end;

procedure TSpTBXListBox.DrawItemBackground(Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
var
  PaintDefault: Boolean;
begin
  // Draw the item background
  PaintDefault := True;
  DoDrawItemBackground(Canvas, Rect, Index, State, pstPrePaint, PaintDefault);
  if PaintDefault then
    SpDrawXPListItemBackground(Canvas, Rect, odSelected in State, False, odFocused in State);

  PaintDefault := True;
  DoDrawItemBackground(Canvas, Rect, Index, State, pstPostPaint, PaintDefault);
end;

procedure TSpTBXListBox.CNDrawItem(var Message: TWMDrawItem);
var
  State: TOwnerDrawState;
begin
  with Message.DrawItemStruct^ do
  begin
    State := TOwnerDrawState(LoWord(itemState));
    Canvas.Handle := hDC;
    Canvas.Lock;
    try
      Canvas.Font := Font;
      Canvas.Brush := Brush;
      if (Integer(itemID) >= 0) and (Integer(itemID) < Items.Count) then begin
        DrawItemBackground(itemID, rcItem, State);
        Canvas.Brush.Style := bsClear;
        DrawItem(itemID, rcItem, State);
      end
      else
        Canvas.FillRect(rcItem);
    finally
      Canvas.Unlock;
      Canvas.Handle := 0;
    end;
  end;
end;

procedure TSpTBXListBox.WMNCPaint(var Message: TWMNCPaint);
begin
  inherited;
  if (BorderStyle <> bsNone) and (SkinManager.GetSkinType <> sknNone) then
    if Ctl3D then
      SpDrawXPEditFrame(Self, FHotTracking, True);
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXCheckListBox }

constructor TSpTBXCheckListBox.Create(AOwner: TComponent);
begin
  inherited;
  FHotTrack := True;
  SkinManager.AddSkinNotification(Self);
  Style := lbOwnerDrawFixed;
end;

destructor TSpTBXCheckListBox.Destroy;
begin
  SkinManager.RemoveSkinNotification(Self);
  inherited;
end;

procedure TSpTBXCheckListBox.InvalidateBorders;
begin
  if HandleAllocated then
    RedrawWindow(Handle, nil, 0, RDW_FRAME or RDW_INVALIDATE);
end;

procedure TSpTBXCheckListBox.SetHotTrack(const Value: Boolean);
begin
  if FHotTrack <> Value then begin
    FHotTrack := Value;
    InvalidateBorders;
  end;
end;

procedure TSpTBXCheckListBox.CMFocusChanged(var Message: TCMFocusChanged);
begin
  inherited;
  if FHotTrack and Assigned(Message.Sender) then begin
    FChildFocused := Self = Message.Sender;
    if FChildFocused <> FHotTracking then begin
      FHotTracking := FChildFocused;
      InvalidateBorders;
    end;
  end;
end;

procedure TSpTBXCheckListBox.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if FHotTrack and not FHotTracking then begin
    FHotTracking := True;
    if SpCanEditFrameBeHotTracked(BorderStyle) then
      InvalidateBorders;
  end;
end;

procedure TSpTBXCheckListBox.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if FHotTrack and FHotTracking and not FChildFocused then begin
    FHotTracking := False;
    if SpCanEditFrameBeHotTracked(BorderStyle) then
      InvalidateBorders;
  end;
end;

procedure TSpTBXCheckListBox.WMSpSkinChange(var Message: TMessage);
begin
  inherited;
  InvalidateBorders;
end;

procedure TSpTBXCheckListBox.DoDrawItem(ACanvas: TCanvas; var ARect: TRect;
  Index: Integer; const State: TOwnerDrawState;
  const PaintStage: TSpTBXPaintStage; var PaintDefault: Boolean);
begin
  if Assigned(FOnDrawItem) then FOnDrawItem(Self, ACanvas, ARect, Index, State, PaintStage, PaintDefault);
end;

procedure TSpTBXCheckListBox.DoDrawItemBackground(ACanvas: TCanvas;
  var ARect: TRect; Index: Integer; const State: TOwnerDrawState;
  const PaintStage: TSpTBXPaintStage; var PaintDefault: Boolean);
begin
  if Assigned(FOnDrawItemBackground) then FOnDrawItemBackground(Self, ACanvas, ARect, Index, State, PaintStage, PaintDefault);
end;

procedure TSpTBXCheckListBox.DrawItem(Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
var
  R: TRect;
  Flags: Integer;
  PaintDefault: Boolean;
begin
  // Draw the item text
  PaintDefault := True;
  DoDrawItem(Canvas, Rect, Index, State, pstPrePaint, PaintDefault);
  if PaintDefault then begin
    Flags := DrawTextBiDiModeFlags(DT_SINGLELINE or DT_VCENTER or DT_NOPREFIX);

    // Add a margin to the rect
    R := Rect;
    if not UseRightToLeftAlignment then
      Inc(R.Left, 3)
    else
      Dec(R.Right, 3);
    SpDrawXPText(Canvas, Items[Index], R, Flags);
  end;

  PaintDefault := True;
  DoDrawItem(Canvas, Rect, Index, State, pstPostPaint, PaintDefault);
end;

procedure TSpTBXCheckListBox.DrawItemBackground(Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
var
  ACheckWidth: Integer;
  R: TRect;
  PaintDefault: Boolean;
begin
  // Draw the checkbox, background and focus
  PaintDefault := True;
  DoDrawItemBackground(Canvas, Rect, Index, State, pstPrePaint, PaintDefault);
  if PaintDefault then begin
    if not Header[Index] then begin
      // Draw the checkbox
      ACheckWidth := GetCheckWidth;
      if not UseRightToLeftAlignment then begin
        R.Right := Rect.Left;
        R.Left := R.Right - ACheckWidth;
      end
      else begin
        R.Left := Rect.Right;
        R.Right := R.Left + ACheckWidth;
      end;
      R.Top := Rect.Top + (Rect.Bottom - Rect.Top - ACheckWidth) div 2;
      R.Bottom := R.Top + ACheckWidth;
      InflateRect(R, -1, -1);

      Canvas.FillRect(R);
      SpDrawXPCheckBoxGlyph(Canvas, R, ItemEnabled[Index], Self.State[Index], False, False);

      // Draw the background and focus
      SpDrawXPListItemBackground(Canvas, Rect, odSelected in State, False, odFocused in State);
    end
    else begin
      Canvas.Font.Color := HeaderColor;
      Canvas.Brush.Color := HeaderBackgroundColor;
      Canvas.FillRect(Rect);
      if odFocused in State then
        SpDrawFocusRect(Canvas, Rect);
    end;
  end;

  PaintDefault := True;
  DoDrawItemBackground(Canvas, Rect, Index, State, pstPostPaint, PaintDefault);
end;

procedure TSpTBXCheckListBox.CNDrawItem(var Message: TWMDrawItem);
var
  State: TOwnerDrawState;
begin
  if Items.Count = 0 then Exit;

  with Message.DrawItemStruct^ do
  begin
    State := TOwnerDrawState(LongRec(itemState).Lo);
    Canvas.Handle := hDC;
    Canvas.Lock;
    try
      Canvas.Font := Font;
      Canvas.Brush := Brush;
      if (Integer(itemID) >= 0) and (Integer(itemID) < Items.Count) then begin
        // Exclude the checkbox area
        if not Header[itemID] then
          if not UseRightToLeftAlignment then
            rcItem.Left := rcItem.Left + GetCheckWidth
          else
            rcItem.Right := rcItem.Right - GetCheckWidth;

        DrawItemBackground(itemID, rcItem, State);
        Canvas.Brush.Style := bsClear;
        DrawItem(itemID, rcItem, State);
      end
      else
        Canvas.FillRect(rcItem);
    finally
      Canvas.Unlock;
      Canvas.Handle := 0;
    end;
  end;
end;

procedure TSpTBXCheckListBox.WMNCPaint(var Message: TWMNCPaint);
begin
  inherited;
  if (BorderStyle <> bsNone) and (SkinManager.GetSkinType <> sknNone) then
    if Ctl3D then
      SpDrawXPEditFrame(Self, FHotTracking, True);
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXEditItem }

constructor TSpTBXEditItem.Create(AOwner: TComponent);
begin
  inherited;
  FEditImageIndex := -1;
  FEditorFontSettings := TSpTBXFontSettings.Create;
  TSpTBXFontSettingsAccess(FEditorFontSettings).OnChange := FontSettingsChanged;
  CustomWidth := 64;
  Alignment := taLeftJustify;  
end;

destructor TSpTBXEditItem.Destroy;
begin
  FFontSettings.Free;
  FEditorFontSettings.Free;
  inherited;
end;

function TSpTBXEditItem.DoAcceptText(var NewText: WideString): Boolean;
begin
  Result := True;
  if Assigned(FOnAcceptText) then FOnAcceptText(Self, NewText, Result);
end;

function TSpTBXEditItem.DoAutoComplete(var AText: WideString): Boolean;
begin
  Result := False;
end;

procedure TSpTBXEditItem.DoBeginEdit(Viewer: TSpTBXEditItemViewer);
begin
  if Assigned(FOnBeginEdit) then FOnBeginEdit(Self, Viewer, Viewer.EditControl);
end;

procedure TSpTBXEditItem.DoChange(const AText: WideString);
begin
  if Assigned(FOnChange) then FOnChange(Self, AText);
end;

procedure TSpTBXEditItem.DoTextChanging(const OldText: WideString;
  var NewText: WideString; Reason: Integer);
begin
  case CharCase of
    ecUpperCase: NewText := WideUpperCase(NewText);
    ecLowerCase: NewText := WideLowerCase(NewText);
  end;
end;

procedure TSpTBXEditItem.FontSettingsChanged(Sender: TObject);
begin
  Change(True);
end;

function TSpTBXEditItem.GetItemViewerClass(AView: TTBView): TTBItemViewerClass;
begin
  if not FAllowVerticalEditor and (AView.Orientation = tbvoVertical) then
    Result := inherited GetItemViewerClass(AView)
  else
    Result := TSpTBXEditItemViewer;
end;

function TSpTBXEditItem.NeedToRecreateViewer(AViewer: TTBItemViewer): Boolean;
begin
  Result := GetItemViewerClass(AViewer.View) <> AViewer.ClassType;
end;

procedure TSpTBXEditItem.SetPasswordChar(Value: WideChar);
begin
  if Value <> FPasswordChar then begin
    FPasswordChar := Value;
    Change(True);
  end;
end;

procedure TSpTBXEditItem.SetShowImage(const Value: Boolean);
begin
  if FShowImage <> Value then begin
    FShowImage := Value;
    Change(True);
  end;
end;

function TSpTBXEditItem.StartEditing(AView: TTBView): Boolean;
var
  SaveText: WideString;
begin
  SaveText := Text;
  SpFocusEditItem(Self, AView);
  // Case Sensitive, Result is true when the text is changed
  Result := Text <> SaveText;
end;

procedure TSpTBXEditItem.SetAllowVerticalEditor(const Value: Boolean);
begin
  if FAllowVerticalEditor <> Value then begin
    FAllowVerticalEditor := Value;
    Change(True);
  end;
end;

procedure TSpTBXEditItem.SetCharCase(Value: TEditCharCase);
begin
  if FCharCase <> Value then begin
    FCharCase := Value;
    SetText(Text);  // Updates case
  end;
end;

procedure TSpTBXEditItem.SetEditCaption(const Value: WideString);
begin
  if FEditCaption <> Value then begin
    FEditCaption := Value;
    Change(True);
  end;
end;

procedure TSpTBXEditItem.SetEditorFontSettings(const Value: TSpTBXFontSettings);
begin
  FEditorFontSettings.Assign(Value);
end;

procedure TSpTBXEditItem.SetMaxLength(Value: Integer);
begin
  if FMaxLength <> Value then begin
    FMaxLength := Value;
    Change(False);
  end;
end;

procedure TSpTBXEditItem.SetText(Value: WideString);
begin
  SetTextEx(Value, tcrSetProperty);
end;

procedure TSpTBXEditItem.SetTextEx(Value: WideString; Reason: Integer);
begin
  DoTextChanging(FText, Value, Reason);
  // Case Sensitive, fire the event when the text is changed
  if FText <> Value then begin
    FText := Value;
    Change(False);
    DoChange(Text);
  end;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXEditItemViewer }

procedure TSpTBXEditItemViewer.EditWndProc(var Message: TMessage);

  procedure AcceptText;
  var
    S: WideString;
  begin
    S := GetEditControlText;
    if Item.DoAcceptText(S) then Item.SetTextEx(S, tcrEditControl);
  end;

begin
  if FEditControl = nil then
    Exit;

  if not HandleEditMessage(Message) then begin
    if Message.Msg = WM_CHAR then
      case TWMChar(Message).CharCode of
        VK_TAB: begin
            FEditControlStatus := [ecsAccept];
            AcceptText;
            Exit;
          end;
        VK_RETURN: begin
            FEditControlStatus := [ecsAccept, ecsClose];
            AcceptText;
            Exit;
          end;
        VK_ESCAPE: begin
            FEditControlStatus := [];
            Exit;
          end;
      end;
    TCustomEditAccess(FEditControl).WndProc(Message);
  end;
  
  if Message.Msg = WM_KILLFOCUS then begin
    View.CancelMode;
    FEditControlStatus := [ecsClose];
  end;
end;

function TSpTBXEditItemViewer.GetEditControlClass: TEditClass;
begin
  Result := TSpTBXUnicodeEdit;
end;

function TSpTBXEditItemViewer.GetEditControlText: WideString;
begin
  Result := '';
  if Assigned(FEditControl) then begin
    if FEditControl is TSpTBXUnicodeEdit then
      Result := TSpTBXUnicodeEdit(FEditControl).Text
    else
      Result := TCustomEditAccess(FEditControl).Text;
  end;
end;

procedure TSpTBXEditItemViewer.GetEditHeight(const DC: HDC; out EditHeight,
  ExternalLeading: Integer);
var
  TextMetricW: TTextMetricW;
begin
  Windows.GetTextMetricsW(DC, TextMetricW);
  EditHeight := TextMetricW.tmHeight;
  ExternalLeading := TextMetricW.tmExternalLeading;
end;

procedure TSpTBXEditItemViewer.GetEditRect(var R: TRect);
var
  TextSize: TSize;
  MarginsInfo: TSpTBXMenuItemMarginsInfo;
begin
  R := BoundsRect;
  if not IsToolbarStyle then begin
    TextSize := MeasureEditCaption;
    CurrentSkin.GetMenuItemMargins(StockBitmap.Canvas, 0, MarginsInfo);
    Inc(R.Left, MarginsInfo.GutterSize + MarginsInfo.ImageTextSpace);
    if Length(Item.EditCaption) > 0 then
      Inc(R.Left, MarginsInfo.LeftCaptionMargin + TextSize.cx + MarginsInfo.RightCaptionMargin + 1);
  end;

  InflateRect(R, 1, 0);
  Inc(R.Left, GetIndentBefore);
  Dec(R.Right, GetIndentAfter);
end;

function TSpTBXEditItemViewer.GetImageShown: Boolean;
begin
  Result := (Item.EditImageIndex >= 0) and
    ((Item.DisplayMode in [nbdmDefault, nbdmImageAndText]) or
    (IsToolbarStyle and (Item.DisplayMode = nbdmTextOnlyInMenus)));

  if Assigned(View) and Assigned(View.Owner) and (View.Owner is TSpTBXToolbar) then
    if TSpTBXToolbar(View.Owner).DisplayMode = tbdmTextOnly then
      Result := False;
end;

function TSpTBXEditItemViewer.GetIndentAfter: Integer;
begin
  Result := 1;
end;

function TSpTBXEditItemViewer.GetIndentBefore: Integer;
var
  ImgList: TCustomImageList;
begin
  Result := 1;

  if ShowImage then begin
    ImgList := GetImageList;
    if Assigned(ImgList) and (Item.ImageIndex >= 0) and (Item.ImageIndex <= ImgList.Count - 1) then
      Result := ImgList.Width + 4;
  end;
end;

function TSpTBXEditItemViewer.GetItem: TSpTBXEditItem;
var
  TBItem: TTBCustomItem;
begin
  TBItem := inherited Item;
  if Assigned(TBItem) then
    Result := TBItem as TSpTBXEditItem
  else
    Result := nil;
end;

function TSpTBXEditItemViewer.HandleEditMessage(var Message: TMessage): Boolean;
begin
  Result := False;
  if Assigned(Item.FOnEditMessage) then Item.FOnEditMessage(Item, Self, Message, Result);
end;

procedure TSpTBXEditItemViewer.CalcSize(const Canvas: TCanvas;
  var AWidth, AHeight: Integer);
var
  TextSize: TSize;
  MarginsInfo: TSpTBXMenuItemMarginsInfo;
  EditBoxHeight: Integer;
begin
  if Item.CustomWidth > -1 then
    AWidth := Item.CustomWidth;

  if not IsToolbarStyle then begin
    TextSize := MeasureEditCaption;
    CurrentSkin.GetMenuItemMargins(StockBitmap.Canvas, 0, MarginsInfo);
    Inc(AWidth, MarginsInfo.GutterSize + MarginsInfo.ImageTextSpace);
    if Length(Item.EditCaption) > 0 then
      Inc(AWidth, MarginsInfo.LeftCaptionMargin + TextSize.cx + MarginsInfo.RightCaptionMargin + 2);
  end
  else begin
    TextSize.cx := 0;
    TextSize.cy := 0;
  end;

  EditBoxHeight := MeasureTextHeight + 1;
  Inc(EditBoxHeight, 2 + 4);
  AHeight := Max(EditBoxHeight, TextSize.cy);
  if not IsToolbarStyle then
    AHeight := AHeight
  else
    AHeight := AHeight or $01;

  if (Item.CustomHeight > -1) and IsToolbarStyle then
    AHeight := Item.CustomHeight;
end;

function TSpTBXEditItemViewer.CaptionShown: Boolean;
begin
  Result := not IsToolbarStyle and inherited CaptionShown;
end;

function TSpTBXEditItemViewer.GetCaptionText: WideString;
begin
  Result := TSpTBXEditItem(Item).EditCaption;
end;

procedure TSpTBXEditItemViewer.InternalDrawFrame(ACanvas: TCanvas;
  ARect: TRect; ItemInfo: TSpTBXMenuItemInfo);
begin
  if not (ItemInfo.HotTrack or ItemInfo.Pushed) and (SkinManager.CurrentSkinName = 'Default') and not SpIsWinVistaOrUp then
    SpFillRect(ACanvas, ARect, clWindow, clBtnFace)
  else begin
    SpDrawXPEditFrame(ACanvas, ARect, ItemInfo.Enabled, ItemInfo.HotTrack);
    InflateRect(ARect, -2, -2);
    SpFillRect(ACanvas, ARect, clWindow);
  end;
end;

procedure TSpTBXEditItemViewer.InternalEditControlChange(Sender: TObject);
begin
  // Used by descendants
  Item.DoChange(GetEditControlText);
end;

procedure TSpTBXEditItemViewer.InternalEditControlExit;
begin
  // Used by descendants
end;

procedure TSpTBXEditItemViewer.Paint(const Canvas: TCanvas;
  const ClientAreaRect: TRect; IsSelected, IsPushed, UseDisabledShadow: Boolean);
const
  Alignments: array [TAlignment] of Integer = (DT_LEFT, DT_RIGHT, DT_CENTER);
var
  DC: HDC;
  S: WideString;
  R, ImageRect: TRect;
  ImgList: TCustomImageList;
  TextSize: TSize;
  ItemInfo: TSpTBXMenuItemInfo;
  MarginsInfo: TSpTBXMenuItemMarginsInfo;
begin
  DC := Canvas.Handle;
  R := ClientAreaRect;
  SpFillItemInfo(Canvas, Self, ItemInfo);

  Canvas.Font.Assign(View.GetFont);
  Item.FontSettings.Apply(Canvas.Font);

  { Item Caption, only on MenuItems }
  if not IsToolbarStyle then begin
    S := Item.EditCaption;
    CurrentSkin.GetMenuItemMargins(Canvas, 0, MarginsInfo);
    TextSize := SpGetTextSize(DC, S, True);

    if Length(S) > 0 then
      R.Right := MarginsInfo.GutterSize + MarginsInfo.ImageTextSpace + TextSize.cx + MarginsInfo.LeftCaptionMargin + MarginsInfo.RightCaptionMargin
    else
      R.Right := MarginsInfo.GutterSize + MarginsInfo.ImageTextSpace - 1;
    SpDrawXPMenuItem(Canvas, R, ItemInfo);

    R.Right := ClientAreaRect.Right;
    Inc(R.Left, MarginsInfo.GutterSize + MarginsInfo.ImageTextSpace);

    if Length(S) > 0 then begin
      if Canvas.Font.Color = clNone then
        Canvas.Font.Color := CurrentSkin.GetTextColor(skncMenuItem, ItemInfo.State);
      Inc(R.Left, MarginsInfo.LeftCaptionMargin);
      SpDrawXPText(Canvas, S, R, DT_SINGLELINE or DT_LEFT or DT_VCENTER);
      Inc(R.Left, TextSize.cx + MarginsInfo.RightCaptionMargin + 1);
    end;

    ImageRect := ClientAreaRect;
    ImageRect.Right := ImageRect.Left + ItemInfo.MenuMargins.GutterSize;
    if ItemInfo.ImageShown then begin
      ImageRect.Left := ImageRect.Left + ((ImageRect.Right - ImageRect.Left) - ItemInfo.ImageSize.cx) div 2;
      ImageRect.Top := ImageRect.Top + ((ImageRect.Bottom - ImageRect.Top) - ItemInfo.ImageSize.cy) div 2;
      ImageRect.Right := ImageRect.Left + ItemInfo.ImageSize.cx;
      ImageRect.Bottom := ImageRect.Top + ItemInfo.ImageSize.cy;
      DrawItemImage(Canvas, ImageRect, ItemInfo, Item.EditImageIndex);
    end;
  end;

  { Edit Frame }
  InternalDrawFrame(Canvas, R, ItemInfo);
  InflateRect(R, 1, 0);

  { Editor Image }
  if ShowImage then begin
    ImgList := GetImageList;
    if Assigned(ImgList) and (Item.ImageIndex >= 0) and (Item.ImageIndex <= ImgList.Count - 1) then begin
      ImageRect.Left := R.Left + 4;
      ImageRect.Right := R.Left + ImgList.Width;
      ImageRect.Top := (R.Top + R.Bottom + 1 - ImgList.Height) div 2;
      ImageRect.Bottom := ImageRect.Top + ImgList.Height;

      SpDrawImageList(Canvas, ImageRect, ImgList, Item.ImageIndex, Item.Enabled, True);
    end;
  end;

  { Editor text }
  if Length(Item.Text) > 0 then begin
    if Item.PasswordChar <> #0 then
      S := StringOfChar(Item.PasswordChar, Length(S))
    else
      S := Item.Text;

    Canvas.Font.Assign(View.GetFont);
    Item.EditorFontSettings.Apply(Canvas.Font);
    if Canvas.Font.Color = clNone then
      if Item.Enabled then
        Canvas.Font.Color := clBtnText
      else
        Canvas.Font.Color := clGrayText;
    InflateRect(R, -2, -1);
    if not IsToolbarStyle then
      Inc(R.Left, GetIndentBefore + 1)
    else
      Inc(R.Left, GetIndentBefore + 2);
    Dec(R.Right, GetIndentAfter + 1);
    Dec(R.Top, 1);
    if IsToolbarStyle then
      Inc(R.Left, -1);
    SpDrawXPText(Canvas, S, R, DT_SINGLELINE or DT_VCENTER or DT_NOPREFIX or Alignments[Item.Alignment]);
  end;
end;

function TSpTBXEditItemViewer.ShowImage: Boolean;
begin
  Result := Item.ShowImage;
end;

procedure TSpTBXEditItemViewer.GetCursor(const Pt: TPoint; var ACursor: HCURSOR);
var
  R: TRect;
begin
  if not Item.Enabled then
    Exit;
  GetEditRect(R);
  OffsetRect(R, -BoundsRect.Left, -BoundsRect.Top);
  InflateRect(R, -2, -2);
  if PtInRect(R, Pt) then
    ACursor := LoadCursor(0, IDC_IBEAM);
end;

function TSpTBXEditItemViewer.EditLoop(const CapHandle: HWND): Boolean;

  procedure ControlMessageLoop;

    function PointInWindow(const Wnd: HWND; const P: TPoint): Boolean;
    var
      W: HWND;
    begin
      Result := False;
      W := WindowFromPoint(P);
      if W = 0 then Exit;
      if W = Wnd then
        Result := True
      else
        if IsChild(Wnd, W) then
          Result := True;
    end;

    function ContinueLoop: Boolean;
    begin
      Result := (ecsContinueLoop in FEditControlStatus) and
        not View.IsModalEnding and FEditControl.Focused and Item.Enabled;
      { Note: View.IsModalEnding is checked since TTBView.CancelMode doesn't
        destroy popup windows; it merely hides them and calls EndModal. So if
        IsModalEnding returns True we can infer that CancelMode was likely
        called. }
    end;

  var
    Msg: TMsg;
    IsKeypadDigit: Boolean;
    ScanCode: Byte;
    V: Integer;
  begin
    try
      while ContinueLoop do begin
        { Examine the next message before popping it out of the queue }
        if not PeekMessage(Msg, 0, 0, 0, PM_NOREMOVE) then begin
          WaitMessage;
          Continue;
        end;
        case Msg.message of
          WM_SYSKEYDOWN: begin
              { Exit immediately if Alt+[key] or F10 are pressed, but not
                Alt+Shift, Alt+`, or Alt+[keypad digit] }
              if (Msg.wParam <> VK_MENU) and (Msg.wParam <> VK_SHIFT) and
                 (Msg.wParam <> VK_HANJA) then begin
                IsKeypadDigit := False;
                { This detect digits regardless of whether Num Lock is on: }
                ScanCode := Byte(Msg.lParam shr 16);
                if ScanCode <> 0 then
                  for V := VK_NUMPAD0 to VK_NUMPAD9 do
                    if MapVirtualKey(V, 0) = ScanCode then begin
                      IsKeypadDigit := True;
                      Break;
                    end;
                if not IsKeypadDigit then begin
                  FEditControlStatus := [ecsClose];
                  Exit;
                end;
              end;
            end;
          WM_SYSKEYUP: begin
              { Exit when Alt is released by itself }
              if Msg.wParam = VK_MENU then begin
                FEditControlStatus := [ecsClose];
                Exit;
              end;
            end;
          WM_LBUTTONDOWN, WM_LBUTTONDBLCLK,
          WM_RBUTTONDOWN, WM_RBUTTONDBLCLK,
          WM_MBUTTONDOWN, WM_MBUTTONDBLCLK,
          WM_NCLBUTTONDOWN, WM_NCLBUTTONDBLCLK,
          WM_NCRBUTTONDOWN, WM_NCRBUTTONDBLCLK,
          WM_NCMBUTTONDOWN, WM_NCMBUTTONDBLCLK: begin
              { If a mouse click outside the edit control is in the queue,
                exit and let the upstream message loop deal with it }
              if Msg.hwnd <> FEditControl.Handle then
                Exit;
            end;
          WM_MOUSEMOVE, WM_NCMOUSEMOVE: begin
              if GetCapture = CapHandle then begin
                if PointInWindow(FEditControl.Handle, Msg.pt) then
                  ReleaseCapture;
              end
              else if GetCapture = 0 then begin
                if not PointInWindow(FEditControl.Handle, Msg.pt) then
                  SetCapture(CapHandle);
              end;
              if GetCapture = CapHandle then
                SetCursor(LoadCursor(0, IDC_ARROW));
            end;
        end;
        { Now pop the message out of the queue }
        if not PeekMessage(Msg, 0, Msg.message, Msg.message, PM_REMOVE or PM_NOYIELD) then
          Continue;
        if ((Msg.message >= WM_MOUSEFIRST) and (Msg.message <= WM_MOUSELAST)) and
           (Msg.hwnd = CapHandle) then
          { discard, so that the selection doesn't get changed }
        else begin
          TranslateMessage(Msg);
          DispatchMessage(Msg);
        end;
      end;
    finally
      { Make sure there are no outstanding WM_*CHAR messages }
      RemoveMessages(WM_CHAR, WM_DEADCHAR);
      RemoveMessages(WM_SYSCHAR, WM_SYSDEADCHAR);
    end;
  end;

var
  R: TRect;
  ActiveWnd, FocusWnd: HWND;
  S: WideString;
begin
  GetEditRect(R);
  if IsRectEmpty(R) then begin
    Result := False;
    Exit;
  end;

  ActiveWnd := GetActiveWindow;
  FocusWnd := GetFocus;

  { Create the edit control }
  InflateRect(R, -3, -3);
  FEditControl := GetEditControlClass.Create(nil);
  try
    FEditControl.Name := Format('%s_edit_control_%p', [ClassName, Pointer(FEditControl)]);
    FEditControl.Visible := False;
    TCustomEditAccess(FEditControl).ReadOnly := Item.ReadOnly;
    TCustomEditAccess(FEditControl).BorderStyle := bsNone;
    TCustomEditAccess(FEditControl).AutoSize := False;
    TCustomEditAccess(FEditControl).Font.Assign(View.GetFont);
    Item.EditorFontSettings.Apply(TCustomEditAccess(FEditControl).Font);
    if FEditControl is TSpTBXUnicodeEdit then begin
      TSpTBXUnicodeEdit(FEditControl).Alignment := Item.Alignment;
      TSpTBXUnicodeEdit(FEditControl).PasswordChar := Item.PasswordChar;
      TSpTBXUnicodeEdit(FEditControl).Text := Item.Text
    end
    else
      TCustomEditAccess(FEditControl).Text := Item.Text;
    TCustomEditAccess(FEditControl).CharCase := Item.FCharCase;
    TCustomEditAccess(FEditControl).MaxLength := Item.FMaxLength;
    FEditControl.BoundsRect := R;
    FEditControl.WindowProc := EditWndProc;
    FEditControl.ParentWindow := View.Window.Handle;
    TCustomEditAccess(FEditControl).OnChange := InternalEditControlChange;
    FEditControl.SelectAll;
    DoBeginEdit;
    FEditControl.Visible := True;
    FEditControl.SetFocus;
    if GetActiveWindow <> ActiveWnd then
      SendMessage(ActiveWnd, WM_NCACTIVATE, 1, 0) // Don't gray out title bar of old active window
    else
      ActiveWnd := 0;

    FEditControlStatus := [ecsContinueLoop];
    ControlMessageLoop;
  finally
    if FEditControlStatus = [ecsContinueLoop] then
      InternalEditControlExit;
    S := GetEditControlText;
    FreeAndNil(FEditControl);
  end;

  if (FEditControlStatus = [ecsContinueLoop]) and Item.ExtendedAccept then
    if Item.DoAcceptText(S) then Item.SetTextEx(S, tcrEditControl);

  { ensure the area underneath the edit control is repainted immediately }
  View.Window.Update;
  { If app is still active, set focus to previous control and restore capture
    to CapHandle if another control hasn't taken it }
  if GetActiveWindow <> 0 then begin
    SetFocus(FocusWnd);
    if GetCapture = 0 then
      SetCapture(CapHandle);
  end;
  if ActiveWnd <> 0 then
    SendMessage(ActiveWnd, WM_NCACTIVATE, Ord(GetActiveWindow = ActiveWnd), 0);
  { The SetFocus call above can change the Z order of windows. If the parent
    window is a popup window, reassert its topmostness. }
  if View.Window is TTBPopupWindow then
    SetWindowPos(View.Window.Handle, HWND_TOPMOST, 0, 0, 0, 0,
      SWP_NOACTIVATE or SWP_NOMOVE or SWP_NOSIZE);
  { Send an MSAA "focus" event now that we're returning to the regular modal loop }
  View.NotifyFocusEvent;

  Result := ecsClose in FEditControlStatus;
  if not Result and (GetCapture = CapHandle) then begin
    if ecsAccept in FEditControlStatus then
      { if we are accepting but not closing, Tab must have been pressed }
      View.Selected := View.NextSelectable(View.Selected,
        GetKeyState(VK_SHIFT) >= 0);
  end;
end;

procedure TSpTBXEditItemViewer.DoBeginEdit;
begin
  Item.DoBeginEdit(Self);
end;

function TSpTBXEditItemViewer.DoExecute: Boolean;
begin
  // Close any delay-close popup menus before entering the edit loop
  View.CancelChildPopups;
  Result := False;
  if EditLoop(View.GetCaptureWnd) then begin
    View.EndModal;
    if ecsAccept in FEditControlStatus then
      Result := True;
  end;
end;

function TSpTBXEditItemViewer.MeasureEditCaption: TSize;
begin
  StockBitmap.Canvas.Font.Assign(View.GetFont);
  Item.FontSettings.Apply(StockBitmap.Canvas.Font);
  Result := SpGetTextSize(StockBitmap.Canvas.Handle, Item.EditCaption, True);
end;

function TSpTBXEditItemViewer.MeasureTextHeight: Integer;
var
  I: Integer;
begin
  StockBitmap.Canvas.Font.Assign(View.GetFont);
  Item.EditorFontSettings.Apply(StockBitmap.Canvas.Font);
  GetEditHeight(StockBitmap.Canvas.Handle, Result, I);
  Inc(Result, I);
end;

procedure TSpTBXEditItemViewer.MouseBeginEdit;
begin
  if Item.Enabled then
    Execute(True)
  else begin
    if (View.ParentView = nil) and not View.IsPopup then
      View.EndModal;
  end;
end;

procedure TSpTBXEditItemViewer.MouseDown(Shift: TShiftState; X, Y: Integer;
  var MouseDownOnMenu: Boolean);
begin
  if IsPtInButtonPart(X, Y) then
    MouseBeginEdit
  else
    inherited;
end;

procedure TSpTBXEditItemViewer.MouseUp(X, Y: Integer; MouseWasDownOnMenu: Boolean);
begin
  if IsPtInButtonPart(X, Y) then
    MouseBeginEdit
  else
    inherited;
end;

function TSpTBXEditItemViewer.UsesSameWidth: Boolean;
begin
  Result := False;
end;

function TSpTBXEditItemViewer.GetAccRole: Integer;
const
  ROLE_SYSTEM_TEXT = $2a;  // from OleAcc.h
begin
  Result := ROLE_SYSTEM_TEXT;
end;

function TSpTBXEditItemViewer.GetAccValue(var Value: WideString): Boolean;
begin
  Value := Item.Text;
  Result := True;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXSpinEditItem }

constructor TSpTBXSpinEditItem.Create(AOwner: TComponent);
begin
  inherited;
  FSpinOptions := TSpTBXSpinEditOptions.Create;
  FSpinOptions.OnGetText := SpinOptionsGetText;
  FSpinOptions.OnSetText := SpinOptionsSetText;

  Alignment := taRightJustify;
  Text := '0';
end;

destructor TSpTBXSpinEditItem.Destroy;
begin
  FreeAndNil(FSpinOptions);
  inherited;
end;

function TSpTBXSpinEditItem.GetItemViewerClass(AView: TTBView): TTBItemViewerClass;
begin
  if not FAllowVerticalEditor and (AView.Orientation = tbvoVertical) then
    Result := inherited GetItemViewerClass(AView)
  else
    Result := TSpTBXSpinEditViewer;
end;

function TSpTBXSpinEditItem.GetValue: Extended;
begin
  Result := SpinOptions.Value;
end;

procedure TSpTBXSpinEditItem.SetValue(const Value: Extended);
begin
  SpinOptions.Value := Value;
end;

function TSpTBXSpinEditItem.GetValueChanged: TNotifyEvent;
begin
  Result := SpinOptions.OnValueChanged;
end;

procedure TSpTBXSpinEditItem.SetValueChanged(const ValueChangedEvent: TNotifyEvent);
begin
  SpinOptions.OnValueChanged := ValueChangedEvent;
end;

procedure TSpTBXSpinEditItem.SpinOptionsGetText(Sender: TObject;
  var NewText: WideString; var Accept: Boolean);
begin
  // Event used by SpinOptions to get the text from the edit control
  NewText := Text;
end;

procedure TSpTBXSpinEditItem.SpinOptionsSetText(Sender: TObject;
  const AText: WideString);
begin
  // Event used by SpinOptions to set the edit control text
  Text := AText;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXSpinEditViewer }

destructor TSpTBXSpinEditViewer.Destroy;
begin
  FreeAndNil(FTimer);
  inherited;
end;

function TSpTBXSpinEditViewer.GetAccRole: Integer;
const
  ROLE_SYSTEM_SPINBUTTON = $34;
begin
  Result := ROLE_SYSTEM_SPINBUTTON;
end;

function TSpTBXSpinEditViewer.GetIndentAfter: Integer;
begin
  if IsToolbarStyle then
    Result := DefaultSpinButtonSize + 1
  else
    Result := GetSystemMetrics(SM_CXMENUCHECK) + 1;
end;

function TSpTBXSpinEditViewer.GetItem: TSpTBXSpinEditItem;
begin
  Result := (inherited Item) as TSpTBXSpinEditItem;
end;

function TSpTBXSpinEditViewer.HandleEditMessage(var Message: TMessage): Boolean;
begin
  if Message.Msg = WM_CHAR then
    case TWMChar(Message).CharCode of
      VK_TAB, VK_RETURN:
        begin
          Item.Text := EditControl.Text;
          Item.SpinOptions.UpdateValueFromText;
          EditControl.Text := Item.Text;
        end;
    end
  else
    if Message.Msg = WM_KEYDOWN then
      case TWMKeyDown(Message).CharCode of
        VK_UP:
          begin
            Item.SpinOptions.ValueInc;
            EditControl.Text := Item.Text;
            EditControl.SelectAll;
            Result := True;
            Exit;
          end;
        VK_DOWN:
          begin
            Item.SpinOptions.ValueDec;
            EditControl.Text := Item.Text;
            EditControl.SelectAll;
            Result := True;
            Exit;
          end;
      end;

  Result := inherited HandleEditMessage(Message);
end;

procedure TSpTBXSpinEditViewer.InternalDrawFrame(ACanvas: TCanvas; ARect: TRect;
  ItemInfo: TSpTBXMenuItemInfo);
var
  IsHotTrack: Boolean;
  R: TRect;
begin
  inherited;
  R := ARect;
  InflateRect(R, -2, -2);
  R.Left := ARect.Right - GetIndentAfter;

  IsHotTrack := ItemInfo.HotTrack;
  SpDrawXPSpinButton(ACanvas, R, ItemInfo.Enabled, IsHotTrack, IsHotTrack, IsHotTrack, FUpPushed, FDownPushed, True);
end;

procedure TSpTBXSpinEditViewer.InternalEditControlChange(Sender: TObject);
var
  L, L2: Integer;
begin
  if Item.ExtendedAccept then begin
    Item.Text := EditControl.Text;
    Item.SpinOptions.UpdateValueFromText(False); // Don't revert when an invalid text is entered
    // Change the EditControl text and reposition the edit caret
    L := Length(EditControl.Text);
    EditControl.Text := Item.Text;
    L2 := Length(EditControl.Text);
    if L2 > L then
      EditControl.SelStart := L + Length(Item.SpinOptions.Prefix);
  end
  else
    inherited;
end;

procedure TSpTBXSpinEditViewer.InternalEditControlExit;
begin
  Item.Text := EditControl.Text;
  Item.SpinOptions.UpdateValueFromText;
  EditControl.Text := Item.Text;
end;

procedure TSpTBXSpinEditViewer.InvalidateButtons;
var
  R: TRect;
begin
  if Show and not IsRectEmpty(BoundsRect) then begin
    R := BoundsRect;
    R.Left := R.Right - GetIndentAfter;
    InvalidateRect(View.Window.Handle, @R, False);
    Include(State, tbisInvalidated);
  end;
end;

function TSpTBXSpinEditViewer.IsPtInButtonPart(X, Y: Integer): Boolean;
begin
  Result := X <= (BoundsRect.Right - BoundsRect.Left) - GetIndentAfter;
end;

procedure TSpTBXSpinEditViewer.LosingCapture;
begin
  FUpPushed := False;
  FDownPushed := False;
  FreeAndNil(FTimer);
  inherited;
end;

procedure TSpTBXSpinEditViewer.MouseDown(Shift: TShiftState; X, Y: Integer;
  var MouseDownOnMenu: Boolean);
begin
  if not Item.Enabled then Exit;

  FUpPushed := False;
  FDownPushed := False;

  if X >= BoundsRect.Right - BoundsRect.Left - GetIndentAfter then begin
    if Y < (BoundsRect.Bottom - BoundsRect.Top) div 2 then begin
      FUpPushed := True;
      Item.SpinOptions.ValueInc;
    end
    else begin
      FDownPushed := True;
      Item.SpinOptions.ValueDec;
    end;

    if not Assigned(FTimer) then begin
      FTimer := TTimer.Create(nil);
      FTimer.OnTimer := TimerHandler;
    end;
    FTimer.Interval := 400;
    FTimer.Enabled := True;
  end;

  if FUpPushed or FDownPushed then begin
    InvalidateButtons;
    inherited;
    View.SetCapture;
  end
  else
    inherited;
end;

procedure TSpTBXSpinEditViewer.MouseUp(X, Y: Integer; MouseWasDownOnMenu: Boolean);
begin
  if FUpPushed or FDownPushed then begin
    FUpPushed := False;
    FDownPushed := False;
    FreeAndNil(FTimer);
    InvalidateButtons;
  end;
  inherited;
end;

procedure TSpTBXSpinEditViewer.TimerHandler(Sender: TObject);
begin
  FTimer.Interval := 100;
  if FUpPushed then Item.SpinOptions.ValueInc
  else
    if FDownPushed then Item.SpinOptions.ValueDec
    else FreeAndNil(FTimer);
end;

end.
