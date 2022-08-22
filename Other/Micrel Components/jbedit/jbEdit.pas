{*******************************************************}
{                                                       }
{       Extended components like Edit type              }
{                                                       }
{       Copyright (C) 1997-2013 Jaro BENES              }
{       All right reserved                              }
{       E-mail:"JBenes@micrel.cz"                       }
{                                                       }
{       Procedure Parser                                }
{       (c) 1997 Antonie Baars, All right reserved      }
{       E-mail:"d950021@icpc00.icpc.fukui-u.ac.jp"      }
{                                                       }
{       RxLibrary                                       }
{       Can be use for descentants of edit components.  }
{       Please see jb.inc file for turn on/off support. }
{                                                       }
{       VG2 Library                                     }
{       Can be use for descentants of edit components.  }
{       Please see jb.inc file for turn on/off support. }
{                                                       }
{       Supported versions of Delphi:                   }
{                                                       }
{       Delphi 2005..2007/2009/2010/XE/XE2/XE3/XE4/XE5  }
{       Delphi for .net BDS 2005/2006/2007              }
{        (standard only, unsupported RX or VG2 on .NET) }
{                                                       }
{       Last Version 1.48                               }
{*******************************************************}

unit jbEdit;

interface
{basic options depend by compiler}
{$I jb.inc}

{$Define EmptyMaskRequired}

{When you don't want autocomplete, please comment next line}
{$Define UseAutocomplete}

{When you don't want buttons in edit, please comment next line}
{$Define UseButtons}

{Hint for large text in combo, when you don't want, do comment it}
{analoque function is build-in at TPubEdit}
{$Define CB_HINTER}

{$IFDEF CLR}
  {$DEFINE CLRUNI}
{$ENDIF}
{$IFDEF UNICODE}
  {$DEFINE CLRUNI}
{$ENDIF}
uses
  Windows, Messages, SysUtils,
  Classes, Graphics, Controls, Forms, Menus, Dialogs, StdCtrls, ExtCtrls,
  {$IFDEF VER17UP}UITypes,{$ENDIF}
  {$IFDEF USE_VG2LIB}
  vg2StdCtrls, vg2ListCombo, vg2FileCtrls, vg2DateCtrls, vg2CalcCtrls,
  {$ENDIF}
  {$IFDEF VER7UP}
  ComCtrls, Types,
  {$ENDIF}
  Buttons, Mask, Clipbrd
{$IFDEF CLR}
  ,WinUtils
{$ENDIF}
{$IFDEF USE_RXLIB}, RxToolEdit, RxCurrEdit, RxCtrls, FileCtrl, RxDateUtil, RxSpin{$ENDIF USE_RXLIB}
{$IFDEF VER6UP}, MaskUtils{$ENDIF};

{
Uvodem:
-------

Rozsireni vzniklo pro validaci dat a pro zmenu barvy pole pri disablovanem
poli. Pri teto prilezitosti jsem pridal dalsi eventy a nove property.

Rozsireni:
Nove property:
--------------

Alignment - zarovnani (left, center, right)

Capitalize - pocatecni pismena jsou konvertovana na velka

ColorOnDisabled
ColorOnFocus
ColorOnNotFocus - zmena barev pro zamerene pole, hlavne zmena
                  kdyz pole ma enabled=False

EditType - je mozno si vybrat mezi stringem, celym cislem, floatem a
           parserem pro vyhodnoceni jednoduchych vyrazu. Vyhodnoceni se
           deje pri ztrate focusu. Je-li typem cislo, je testovano, zda je
           cislem platnym a interne je sledovan vstup znaku na cisla.
           Parserem je mozne ohodnocovat vyrazu napr. vyraz
             sqrt(0.5*exp(x)-0.5*exp(y))

Mask - v tomto retezci se uschovava maska k formatovani dle nejakych
       pravidel. Zde se nic neformatuje, predpoklada se specialni vnejsi rutina,
       povesena na OnMask.

Required - nastavuje se na True, je-li pozadovano neprazdne pole.

ValidChars - retezec, kde se uschovavaji platne znaky, test se provadi pouze pro
             typ EditType = teString;

ButtonXXX - vlozeno tlacitko TSpeedbutton nebo TRxSpeedButton s moznosti nastavit
            zakladni vlastnosti (button z RXLib je lepsi, umoznuje nastavit menu)

Nove events:
------------

OnConvert - lze pouzit pro konverzi pri EditType = teInteg ze soustavy
            napr. oktalove do desitkove k editaci a zpet. Smer konverze je
            udavan promenou ToDirection, ktera nabyva dvou hodnot:

            ** zde je priklat pouziti s uzitim rutin z jbStr.Pas modulu **

            procedure TForm1.PubEdit1Convert(Sender: TObject;
              ToDirection: TToDirection; var Value: OpenString);
            begin
              If Value <> '' Then // jen kdyz je treba
                Case ToDirection of
                  // hodnota se meni z oktalove na desitkovou
                  eValueToText: Value := IntToStr(Num(Value,8));
                  // hodnota se meni z desitkove zpet na oktalovou
                  eTextToValue: Value := Doc(StrToInt(Value),8);
                End;
            end;

OnError - umoznuje zakryt standardni hlasky a nahradit je svymi.

OnMask - umoznuje formatovat retezec v editacnim poli pri
         EditType = teString; Znamena to, ze tam mohou byt ruzne
         znaky, ktere se az nasledne zaformatuji.

         ** zde je priklat pouziti s uzitim rutin z jbStr modulu **

         procedure TForm1.PubEdit1Mask(Sender: TObject; ToDirection: TToDirection;
           Mask: String; var Value: OpenString);
         begin
           If Value <> '' Then // jen kdyz je treba
             Case ToDirection of
               // formatujeme 10 mistne rodne cislo
               // editovat se bude pouze text, je vhodne jeste nastavit
               // ValidChars := '0123456789'
               // odstranuji se vsechny oddelovace pred editaci
               eValueToText:Value:=Strip('XX XX XX XXXX',Value);
               // vkladaji se oddelovace po editaci
               eTextToValue:Value:=Zip('XX.XX.XX/XXXX',Value);
             End;
         end;

         Pozn. Funkce je vyvolana dvakrat - poprve regulerne pro upravu
               a podruhe pri validaci, nebot validace musi probehnout na
               originalnim poli bez oddelovacu, kvuli snadne kontrole.
               Neni to zrovna dobre reseni...

OnValidate - hlavni funkce pro ohodnoceni zda je vysledne pole v normalu
             ci nikoliv, kvuli ktere jsem tohle delal.

             ** Priklad na overeni rodneho cisla **

             procedure TForm1.PubEdit1Validate(Sender: TObject;
               Value: String;
               var CanExit: Boolean);
             Var E:Extended; // lokalni promenna
             begin
               CanExit:=True; // signal dopredu vse OK
               If Value='' Then Exit; // kdyz je pole prazdne, ven
               E:=StrToFloat(Value); // mozna zde to dat do try-except
               E:=E-11*(E/11); // vlastni overeni spravnosti
               CanExit := E=0; // spravne, kdyz je zbytek 0
             end;

Pouziti:
--------

Komponent je mozne volne (FREE) pouzit, s prihlednutim k soubeznym pravum.
Pouziti je na vlastni odpovednost uzivatele, nedavam zadne dalsi zaruky
pokud to neni nekde vyslovne uvedeno. Budete-li chtit zmenit cokoliv v teto
verzi, naleznete-li nejakou chybu nebo hodlate zvysit pouzitelnost,
dejte mi to, prosim, na vedomi.

Kontakt:
--------
Jaro Benes
E-mail:JBenes@micrel.cz
http://www.micrel.cz/delphi/
}

const
  tyVersion = '1.48 CZ';
  tyToAuthor = 'mailto="JBenes@micrel.cz"';

type
  TPaintLineHintWindow = class(THintWindow)
  protected
    procedure Paint; override;
    procedure CreateParams(var Params: TCreateParams); override;
  private
  public
  end;

type
  EvaluatorError = class(Exception);
  EInvalidTypeConvert = class(Exception);
  {ohodnocujici procedura 11.11.1997}
  TValidate = procedure(Sender: TObject;
    var Value: string;
    var CanExit: Boolean) of object;
  TToDirection = (eValueToText, eTextToValue);
  TConvert = procedure(Sender: TObject;
    ToDirection: TToDirection;
    var Value: string) of object;
  TMaskForm = procedure(Sender: TObject;
    ToDirection: TToDirection;
    Mask: string;
    var Value: string) of object;
  TErrorOccur = procedure(Sender: TObject;
    const ErrorMsg: string) of object;
  TParsing = procedure(Sender: TObject;
    const ParseValue: string; var ValueParsed: string) of object;
  {jak se zachovat k poli}
  TTypeEdit = (teString, teInteg, teFloat, teCurrency, teDate, teTime, teDateTime, teParser);

  { TColorOptionsSet }

  TColorOptionsSet = class(TPersistent)
  private
    FColorFocus: TColor; {barvicky}
    FFontColorFocus: TColor; {barva pro focus fontu}
    FFontColorUnFocus: TColor;
    FColorUnFocus: TColor;
    FDisabledColor: TColor;
    FFontDisabledColor: TColor;
  public
    constructor Create;
    procedure Reset;
    procedure Show(Value: Boolean);
  published
    property ColorOnFocus: TColor read FColorFocus write FColorFocus default clYellow;
    property ColorOnFocusFont: TColor read FFontColorFocus write FFontColorFocus default clBlack;
    property ColorOnNotFocusFont: TColor read FFontColorUnFocus write FFontColorUnFocus default clBlack;
    property ColorOnNotFocus: TColor read FColorUnFocus write FColorUnFocus default clWindow;
    property ColorOnDisabled: TColor read FDisabledColor write FDisabledColor default clBtnFace;
    property ColorOnDisabledFont: TColor read FFontDisabledColor write FFontDisabledColor default clGrayText;
  end;

  { TButtonOptionsSet }

{$IfDef UseButtons}
  TButtonOptionsSet = class(TPersistent)
  private
    FParent: TWinControl;
    FButton: {$IFDEF USE_RXLIB}TRxSpeedButton{$ELSE}TSpeedButton{$ENDIF}; {tlacitko}
    FShortCut: TShortCut;
    procedure CreateButton;
    procedure UpdateFormatRect;
    function GetButtonCaption: string;
    function GetButtonEnabled: Boolean;
    function GetButtonGlyph: TBitmap;
    function GetButtonHint: string;
{$IFDEF USE_RXLIB}
    function GetButtonMenu: TPopupMenu;
    function GetNumGlyphs: Integer;
    procedure SetButtonMenu(const Value: TPopupMenu);
    procedure SetNumGlyphs(const Value: Integer);
{$ENDIF}
    function GetButtonVisible: Boolean;
    function GetButtonWidth: Integer;
    procedure SetButtonCaption(const Value: string);
    procedure SetButtonEnabled(const Value: Boolean);
    procedure SetButtonGlyph(const Value: TBitmap);
    procedure SetButtonHint(const Value: string);
    procedure SetButtonVisible(const Value: Boolean);
    procedure SetButtonWidth(const Value: Integer);
    procedure SetButtonExists(const Value: Boolean);
    function GetButtonExists: Boolean;
  protected
    constructor Create;
  public
    destructor Destroy; override;
    property Button: {$IFDEF USE_RXLIB}TRxSpeedButton{$ELSE}TSpeedButton{$ENDIF} read FButton write FButton;
  published
    property ButtonVisible: Boolean read GetButtonVisible write SetButtonVisible default False;
    property ButtonCaption: string read GetButtonCaption write SetButtonCaption;
    property ButtonExists: Boolean read GetButtonExists write SetButtonExists default False;
    property ButtonGlyph: TBitmap read GetButtonGlyph write SetButtonGlyph;
    property ButtonWidth: Integer read GetButtonWidth write SetButtonWidth default 19;
    property ButtonHint: string read GetButtonHint write SetButtonHint;
    property ButtonEnabled: Boolean read GetButtonEnabled write SetButtonEnabled default False;
    property ButtonShortCut: TShortCut read FShortCut write FShortCut stored GetButtonExists;
{$IFDEF USE_RXLIB}
    property ButtonMenu: TPopupMenu read GetButtonMenu write SetButtonMenu;
    property NumGlyphs: Integer read GetNumGlyphs write SetNumGlyphs stored GetButtonExists;
{$ENDIF}
  end;
{$EndIf UseButtons}

  { TAutoCompleteText}

{$IfDef UseAutocomplete}
  TAutoCompleteText = class(TPersistent)
  private
    FAutoCompleteItems: TStringList;
    FAutoComplete: Boolean;
    FForceComplete: Boolean;
    FStoreHistory: Boolean;
    function GetAutoComplete: Boolean;
    procedure SetAutoComplete(const Value: Boolean);
  public
    constructor Create;
    destructor Destroy; override;
    function Test(const incompletetxt: string; var completetxt: string): Boolean;
  published
    property AutoCompleteItems: TStringList read FAutoCompleteItems write FAutoCompleteItems;
    property AutoComplete: Boolean read GetAutoComplete write SetAutoComplete default False;
    property ForceComplete: Boolean read FForceComplete write FForceComplete default False;
    property StoreHistory: Boolean read FStoreHistory write FStoreHistory default False;
  end;
{$EndIf}

  TInsideLabel = class(TPersistent)
  private
    FActive: Boolean;
    FColor: TColor;
    FCaption: TCaption;
    procedure SetCaption(const Value: TCaption);
  public
    constructor Create;
  published
    property Active: Boolean read FActive write FActive default False;
    property Color: TColor read FColor write FColor default clDkGray;
    property Caption: TCaption read FCaption write SetCaption;
  end;

  { TPubEdit }

//------------------------------------------------------------------------------
// Main edit component of file
//------------------------------------------------------------------------------

  TPubEdit = class(TEdit)
  private
    { Private declarations }
    FShowing: Boolean; {True, pokud je hint okno aktivni}
    FSelectAll: Boolean;
    FWider: Boolean; {text je sirsi}
    FWindow: TForm; {extra instance formu zastupna pro canvas}
    FHintText: Boolean; {zvlastni instance}
    {buttons}
    {$IfDef UseButtons}
    FBtnSet1: TButtonOptionsSet;
    FBtnSet2: TButtonOptionsSet;
    {$EndIf}
    {autocomplete for file}
    {$IfDef UseAutocomplete}
    FAutocomplete: TAutoCompleteText;
    {$EndIf}
    FOnMouseExit: TNotifyEvent;
    FOnMouseEnter: TNotifyEvent;
    FAlignment: TAlignment;
    FTypeEdit: TTypeEdit; {typ pole s ruznymi akcemi}
    FValidate: TValidate; {eventy}
    FConvert: TConvert;
    FErrorOccur: TErrorOccur;
    FMaskForm: TMaskForm;
    FRequired: Boolean; {neprazdne pole}
    FCapitalize: Boolean; {prvni znaky slov velke}
    FOnEnter: TNotifyEvent;
    FOnExit: TNotifyEvent;
    FValidChars: string; {platne znaky napr. 'abcABC'}
    FMask: string; {maska napriklad 'XX.XX.XX/XX-XX'}
    FColorOptionsSet: TColorOptionsSet;
    FParsing: TParsing;
    FOnPaste: TNotifyEvent;
    FOnCopy: TNotifyEvent;
    FOnCut: TNotifyEvent;
    FOnClear: TNotifyEvent;
    FErrMsg: string;
    FInsideLabel: TInsideLabel;
    procedure SetAlignment(Value: TAlignment);
    procedure SetRequired(Value: Boolean);
    procedure SetCapitalize(Value: Boolean);
    procedure SetAsInteger(Value: Integer);
    function GetAsInteger: Integer;
    procedure SetAsFloat(Value: Real);
    function GetAsFloat: Real;
    procedure SetAsDate(Value: TDate);
    function GetAsDate: TDate;
    procedure SetAsTime(Value: TTime);
    function GetAsTime: TTime;
    procedure ReleaseIt;
    {$IfDef UseButtons}
    procedure SetOnButton1Click(const Value: TNotifyEvent);
    procedure SetOnButton2Click(const Value: TNotifyEvent);
    function GetOnButton1Click: TNotifyEvent;
    function GetOnButton2Click: TNotifyEvent;
    {$EndIf}
    function GetAsBoolean: Boolean;
    function GetAsCurrency: Currency;
    function GetAsDateTime: TDateTime;
    procedure SetAsBoolean(const Value: Boolean);
    procedure SetAsCurrency(const Value: Currency);
    procedure SetAsDateTime(const Value: TDateTime);
    procedure SetAsString(Value: string);
    function GetAsString: string;
    function ValidityCheck(Value: string; var ErrMsg: string): Boolean;
    function DoValidate(var oErrMsg: string): Boolean;
    function GetAsZeroPaddedString: string;
    procedure SetAsZeroPaddedString(const Value: string);
    function GetAsAnsiString: AnsiString;
    procedure SetAsAnsiString(const Value: AnsiString);
  protected
    { Protected declarations }
    procedure CreateParams(var Params: TCreateParams); override;
    procedure KeyPress(var Key: Char); override;
    procedure DoEnter; override;
    procedure DoExit; override;
    {$IfDef UseButtons}
    procedure WMSetCursor(var Msg: TWMSetCursor); message WM_SETCURSOR;
    procedure WMSize(var Msg: TWMSize); message WM_SIZE;
    {$EndIf}
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMMouseEnter(var message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var message: TMessage); message CM_MOUSELEAVE;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure WMPaste(var Message: TMessage); message WM_PASTE;
    procedure WMCut(var Message: TMessage); message WM_CUT;
    procedure WMCopy(var Message: TMessage); message WM_COPY;
    procedure WMClear(var Message: TMessage); message WM_CLEAR;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure NextControl(YesNo: Boolean); {nepouzil jsem}
    {$IfDef UseButtons}
    procedure CreateHandle; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    {$EndIf}
    {$IfDef UseAutocomplete}
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    {$EndIf}
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    HintWin: TPaintLineHintWindow;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Validated: Boolean;
    function Validator: Boolean;
    function Parsed(const S: string): string;
    procedure DefaultHandler(var message); override;
    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    property AsCurrency: Currency read GetAsCurrency write SetAsCurrency;
    property AsInteger: Integer read GetAsInteger write SetAsInteger;
    property AsFloat: Real read GetAsFloat write SetAsFloat;
    property AsDate: TDate read GetAsDate write SetAsDate;
    property AsTime: TTime read GetAsTime write SetAsTime;
    property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;
    property AsString: string read GetAsString write SetAsString;
    property AsAnsiString: AnsiString read GetAsAnsiString write SetAsAnsiString;
    property AsZeroPaddedString: string read GetAsZeroPaddedString write SetAsZeroPaddedString;
    property ErrMsg: string read FErrMsg write FErrMsg;
  published
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property AutoSelect;
    property AutoSize;
    property BorderStyle;
    property Capitalize: Boolean read FCapitalize write SetCapitalize default False;
    property CharCase;
    property Color;
    property ColorOptionsSet: TColorOptionsSet read FColorOptionsSet write FColorOptionsSet;
    property Ctl3D;
    property Cursor;
    property DragCursor;
    property DragMode;
    property EditType: TTypeEdit read FTypeEdit write FTypeEdit default teString;
    property Enabled;
    property Font;
    property Height;
    property HelpContext;
    property HideSelection;
    property Hint;
    property HintText: Boolean read FHintText write FHintText default False;
    property InsideLabel: TInsideLabel read FInsideLabel write FInsideLabel;
    property Left;
    property Mask: string read FMask write FMask;
    property MaxLength;
    property Name;
    property OEMConvert;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PopupMenu;
    property ReadOnly;
    property Required: Boolean read FRequired write SetRequired default False;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Tag;
    property Text;
    property Top;
    property ValidChars: string read FValidChars write FValidChars;
    property Visible;
    property Width;
    property OnChange;
    property OnClick;
    property OnConvert: TConvert read FConvert write FConvert;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter: TNotifyEvent read FOnEnter write FOnEnter;
    property OnError: TErrorOccur read FErrorOccur write FErrorOccur;
    property OnExit: TNotifyEvent read FOnExit write FOnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMask: TMaskForm read FMaskForm write FMaskForm;
    property OnMouseDown;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseExit: TNotifyEvent read FOnMouseExit write FOnMouseExit;
    property OnMouseMove;
    property OnMouseUp;
    property OnValidate: TValidate read FValidate write FValidate;
    {$IfDef UseButtons}
    property OnButton1Click: TNotifyEvent read GetOnButton1Click write SetOnButton1Click;
    property OnButton2Click: TNotifyEvent read GetOnButton2Click write SetOnButton2Click;
    property Button1: TButtonOptionsSet read FBtnSet1 write FBtnSet1;
    property Button2: TButtonOptionsSet read FBtnSet2 write FBtnSet2;
    {$EndIf}
    {$IfDef UseAutocomplete}
    property Autocomplete: TAutoCompleteText read FAutocomplete write FAutocomplete;
    {$EndIf}
    property OnParsing: TParsing read FParsing write FParsing;
  end;

{$IFDEF UNICODE}
{$IFNDEF CLR}
  {  TPubButtonedEdit  }

  TPubButtonedEdit = class(TButtonedEdit)
  private
    { Private declarations }
    FShowing: Boolean; {True, pokud je hint okno aktivni}
    FSelectAll: Boolean;
    FWider: Boolean; {text je sirsi}
    FWindow: TForm; {extra instance formu zastupna pro canvas}
    FHintText: Boolean; {zvlastni instance}
    {autocomplete for file}
    {$IfDef UseAutocomplete}
    FAutocomplete: TAutoCompleteText;
    {$EndIf}
    FOnMouseExit: TNotifyEvent;
    FOnMouseEnter: TNotifyEvent;
    FAlignment: TAlignment;
    FTypeEdit: TTypeEdit; {typ pole s ruznymi akcemi}
    FValidate: TValidate; {eventy}
    FConvert: TConvert;
    FErrorOccur: TErrorOccur;
    FMaskForm: TMaskForm;
    FRequired: Boolean; {neprazdne pole}
    FCapitalize: Boolean; {prvni znaky slov velke}
    FOnEnter: TNotifyEvent;
    FOnExit: TNotifyEvent;
    FValidChars: string; {platne znaky napr. 'abcABC'}
    FMask: string; {maska napriklad 'XX.XX.XX/XX-XX'}
    FColorOptionsSet: TColorOptionsSet;
    FParsing: TParsing;
    FOnPaste: TNotifyEvent;
    FOnCopy: TNotifyEvent;
    FOnCut: TNotifyEvent;
    FOnClear: TNotifyEvent;
    FErrMsg: string;
    procedure SetAlignment(Value: TAlignment);
    procedure SetRequired(Value: Boolean);
    procedure SetCapitalize(Value: Boolean);
    procedure SetAsInteger(Value: Integer);
    function GetAsInteger: Integer;
    procedure SetAsFloat(Value: Real);
    function GetAsFloat: Real;
    procedure SetAsDate(Value: TDate);
    function GetAsDate: TDate;
    procedure SetAsTime(Value: TTime);
    function GetAsTime: TTime;
    procedure ReleaseIt;
    function GetAsBoolean: Boolean;
    function GetAsCurrency: Currency;
    function GetAsDateTime: TDateTime;
    procedure SetAsBoolean(const Value: Boolean);
    procedure SetAsCurrency(const Value: Currency);
    procedure SetAsDateTime(const Value: TDateTime);
    procedure SetAsString(Value: string);
    function GetAsString: string;
    function ValidityCheck(Value: string; var ErrMsg: string): Boolean;
    function DoValidate(var oErrMsg: string): Boolean;
    function GetAsZeroPaddedString: string;
    procedure SetAsZeroPaddedString(const Value: string);
    function GetAsAnsiString: AnsiString;
    procedure SetAsAnsiString(const Value: AnsiString);
  protected
    { Protected declarations }
    procedure CreateParams(var Params: TCreateParams); override;
    procedure KeyPress(var Key: Char); override;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMMouseEnter(var message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var message: TMessage); message CM_MOUSELEAVE;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure WMPaste(var Message: TMessage); message WM_PASTE;
    procedure WMCut(var Message: TMessage); message WM_CUT;
    procedure WMCopy(var Message: TMessage); message WM_COPY;
    procedure WMClear(var Message: TMessage); message WM_CLEAR;
    procedure NextControl(YesNo: Boolean); {nepouzil jsem}
    {$IfDef UseAutocomplete}
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    {$EndIf}
  public
    HintWin: TPaintLineHintWindow;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Validated: Boolean;
    function Validator: Boolean;
    function Parsed(const S: string): string;
    procedure DefaultHandler(var message); override;
    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    property AsCurrency: Currency read GetAsCurrency write SetAsCurrency;
    property AsInteger: Integer read GetAsInteger write SetAsInteger;
    property AsFloat: Real read GetAsFloat write SetAsFloat;
    property AsDate: TDate read GetAsDate write SetAsDate;
    property AsTime: TTime read GetAsTime write SetAsTime;
    property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;
    property AsString: string read GetAsString write SetAsString;
    property AsAnsiString: AnsiString read GetAsAnsiString write SetAsAnsiString;
    property AsZeroPaddedString: string read GetAsZeroPaddedString write SetAsZeroPaddedString;
    property ErrMsg: string read FErrMsg write FErrMsg;
  published
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property AutoSelect;
    property AutoSize;
    property BorderStyle;
    property Capitalize: Boolean read FCapitalize write SetCapitalize default False;
    property CharCase;
    property Color;
    property ColorOptionsSet: TColorOptionsSet read FColorOptionsSet write FColorOptionsSet;
    property Ctl3D;
    property Cursor;
    property DragCursor;
    property DragMode;
    property EditType: TTypeEdit read FTypeEdit write FTypeEdit default teString;
    property Enabled;
    property Font;
    property Height;
    property HelpContext;
    property HideSelection;
    property Hint;
    property HintText: Boolean read FHintText write FHintText default False;
    property Left;
    property Mask: string read FMask write FMask;
    property MaxLength;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    property Name;
    property OEMConvert;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PopupMenu;
    property ReadOnly;
    property Required: Boolean read FRequired write SetRequired default False;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Tag;
    property Text;
    property Top;
    property ValidChars: string read FValidChars write FValidChars;
    property Visible;
    property Width;
    property OnChange;
    property OnClick;
    property OnConvert: TConvert read FConvert write FConvert;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter: TNotifyEvent read FOnEnter write FOnEnter;
    property OnError: TErrorOccur read FErrorOccur write FErrorOccur;
    property OnExit: TNotifyEvent read FOnExit write FOnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMask: TMaskForm read FMaskForm write FMaskForm;
    property OnMouseDown;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseExit: TNotifyEvent read FOnMouseExit write FOnMouseExit;
    property OnMouseMove;
    property OnMouseUp;
    property OnValidate: TValidate read FValidate write FValidate;
    {$IfDef UseAutocomplete}
    property Autocomplete: TAutoCompleteText read FAutocomplete write FAutocomplete;
    {$EndIf}
    property OnParsing: TParsing read FParsing write FParsing;
  end;

  {  TPubLabeledButtonedEdit  }

  TPubLabeledButtonedEdit = class(TPubButtonedEdit)
  private
    FEditLabel: {$IFDEF USE_RXLIB}TRxLabel{$ELSE}{$IFNDEF VER6UP}TLabel{$ELSE}TBoundLabel{$ENDIF}{$ENDIF};
    FLabelPosition: TLabelPosition;
    FLabelSpacing: Integer;
    procedure SetLabelPosition(const Value: TLabelPosition);
    procedure SetLabelSpacing(const Value: Integer);
    function GetLabelShowsRequired: Boolean;
    procedure SetLabelShowsRequired(const Value: Boolean);
  protected
    procedure SetParent(AParent: TWinControl); override;
    procedure SetName(const Value: TComponentName); override;
    procedure CMVisiblechanged(var Message: TMessage); message CM_VISIBLECHANGED;
    procedure CMEnabledchanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMBidimodechanged(var Message: TMessage); message CM_BIDIMODECHANGED;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer; AHeight: Integer); override;
  published
    property EditLabel: {$IFDEF USE_RXLIB}TRxLabel{$ELSE}{$IFNDEF VER6UP}TLabel{$ELSE}TBoundLabel{$ENDIF}{$ENDIF} read FEditLabel write FEditLabel;
    property LabelPosition: TLabelPosition read FLabelPosition write SetLabelPosition default lpAbove;
    property LabelSpacing: Integer read FLabelSpacing write SetLabelSpacing default 3;
    property LabelShowsRequired: Boolean read GetLabelShowsRequired write SetLabelShowsRequired;
  end;
{$ENDIF}
{$ENDIF}

{$IFNDEF VER6UP}
  { TLabelPosition }
  // special declaration for Delphi older than version 6
  TLabelPosition = (lpAbove, lpBelow, lpLeft, lpRight);
{$ENDIF}

  {  TPubLabeledEdit  }

  TPubLabeledEdit = class(TPubEdit)
  private
    FEditLabel: {$IFDEF USE_RXLIB}TRxLabel{$ELSE}{$IFNDEF VER6UP}TLabel{$ELSE}TBoundLabel{$ENDIF}{$ENDIF};
    FLabelPosition: TLabelPosition;
    FLabelSpacing: Integer;
    procedure SetLabelPosition(const Value: TLabelPosition);
    procedure SetLabelSpacing(const Value: Integer);
    function GetLabelShowsRequired: Boolean;
    procedure SetLabelShowsRequired(const Value: Boolean);
  protected
    procedure SetParent(AParent: TWinControl); override;
    procedure SetName(const Value: TComponentName); override;
    procedure CMVisiblechanged(var Message: TMessage); message CM_VISIBLECHANGED;
    procedure CMEnabledchanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMBidimodechanged(var Message: TMessage); message CM_BIDIMODECHANGED;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer; AHeight: Integer); override;
  published
    property EditLabel: {$IFDEF USE_RXLIB}TRxLabel{$ELSE}{$IFNDEF VER6UP}TLabel{$ELSE}TBoundLabel{$ENDIF}{$ENDIF} read FEditLabel write FEditLabel;
    property LabelPosition: TLabelPosition read FLabelPosition write SetLabelPosition default lpAbove;
    property LabelSpacing: Integer read FLabelSpacing write SetLabelSpacing default 3;
    property LabelShowsRequired: Boolean read GetLabelShowsRequired write SetLabelShowsRequired;
  end;

  {  TPubMaskEdit  }

  TPubMaskEdit = class(TMaskEdit)
  private
    FScrollBars: TScrollStyle;
    FAlignment: TAlignment;
    FMultiline: Boolean;
    FWordWrap: Boolean;
    FAfterPaint: TNotifyEvent;
    FValidate: TValidate;
    FErrorOccur: TErrorOccur;
    FRequired: Boolean; {neprazdne pole}
    FOnEnter: TNotifyEvent;
    FOnExit: TNotifyEvent;
    FColorOptionsSet: TColorOptionsSet;
    FAcceptEmpty: Boolean; {neze byt prazdne} {8.8.2001}
    FErrMsg: string;
    procedure SetRequired(Value: Boolean);
    procedure SetScrollBars(Value: TScrollStyle);
    procedure SetAlignment(Value: TAlignment);
    procedure SetMultiline(Value: Boolean);
    procedure SetWordWrap(Value: Boolean);
  protected
    procedure DoEnter; override;
    procedure DoExit; override;
    function DoValidate: Boolean;
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    {$IFDEF EmptyMaskRequired}
    procedure KeyPress(var Key: Char); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    {$ENDIF EmptyMaskRequired}
    procedure CreateParams(var Params: TCreateParams); override;
  public
    Canvas: TCanvas; {public version of the canvas}
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint(var Message: TWMPaint); message WM_PAINT;
    function Validator: Boolean;
    property ErrMsg: string read FErrMsg write FErrMsg;
  published
    property AcceptEmpty: Boolean read FAcceptEmpty write FAcceptEmpty default False;
    property AutoSelect;
    property AutoSize;
    property BorderStyle;
    property CharCase;
    property Color;
    property ColorOptionsSet: TColorOptionsSet read FColorOptionsSet write FColorOptionsSet;
    property Ctl3D;
    property Cursor;
    property DragCursor;
    property DragMode;
    property EditMask;
    property Enabled;
    property Font;
    property Height;
    property HelpContext;
    property Hint;
    property Left;
    property MaxLength;
    property Name;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PopupMenu;
    property ReadOnly;
    property Required: Boolean read FRequired write SetRequired default False;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Tag;
    property Text;
    property Top;
    property Visible;
    property Width;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter: TNotifyEvent read FOnEnter write FOnEnter;
    property OnError: TErrorOccur read FErrorOccur write FErrorOccur;
    property OnExit: TNotifyEvent read FOnExit write FOnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnValidate: TValidate read FValidate write FValidate;
    property ScrollBars: TScrollStyle read FScrollBars write SetScrollBars default ssNone;
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property Multiline: Boolean read FMultiline write SetMultiline default False;
    property WordWrap: Boolean read FWordWrap write SetWordWrap default False;
    property OnAfterPaint: TNotifyEvent read FAfterPaint write FAfterPaint;
  end;

  {  TPubLabeledMaskEdit  }

  TPubLabeledMaskEdit = class(TPubMaskEdit)
  private
    FEditLabel: {$IFDEF USE_RXLIB}TRxLabel{$ELSE}{$IFNDEF VER6UP}TLabel{$ELSE}TBoundLabel{$ENDIF}{$ENDIF};
    FLabelPosition: TLabelPosition;
    FLabelSpacing: Integer;
    procedure SetLabelPosition(const Value: TLabelPosition);
    procedure SetLabelSpacing(const Value: Integer);
    function GetLabelShowsRequired: Boolean;
    procedure SetLabelShowsRequired(const Value: Boolean);
  protected
    procedure SetParent(AParent: TWinControl); override;
    procedure SetName(const Value: TComponentName); override;
    procedure CMVisiblechanged(var Message: TMessage); message CM_VISIBLECHANGED;
    procedure CMEnabledchanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMBidimodechanged(var Message: TMessage); message CM_BIDIMODECHANGED;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer; AHeight: Integer); override;
  published
    property EditLabel: {$IFDEF USE_RXLIB}TRxLabel{$ELSE}{$IFNDEF VER6UP}TLabel{$ELSE}TBoundLabel{$ENDIF}{$ENDIF} read FEditLabel write FEditLabel;
    property LabelPosition: TLabelPosition read FLabelPosition write SetLabelPosition default lpAbove;
    property LabelSpacing: Integer read FLabelSpacing write SetLabelSpacing default 3;
    property LabelShowsRequired: Boolean read GetLabelShowsRequired write SetLabelShowsRequired;
  end;

  {  TPubComboBox  }

  TPubComboBox = class(TComboBox)
  private
    {$IFDEF CB_HINTER}
    FShowing: Boolean; {True, pokud je hint okno aktivni}
    FSelectAll: Boolean;
    FWider: Boolean; {text je sirsi}
    FWindow: TForm; {extra instance formu zastupna pro cancas}
    FHintText: Boolean; {zvlastni instance}
    {$ENDIF}
    {}
    FErrorOccur: TErrorOccur;
    FRequired: Boolean; {neprazdne pole}
    FOnEnter: TNotifyEvent;
    FOnExit: TNotifyEvent;
    FColorOptionsSet: TColorOptionsSet;
    FOnListPick: TNotifyEvent;
    FOnItemsChanged: TNotifyEvent;
    FDropDownFlexWidth: Boolean;
    FValidate: TValidate;
    procedure SetRequired(Value: Boolean);
    procedure DoDropDownFlexWidth; virtual;
    function GetAsInteger: Integer;
    procedure SetAsInteger(const Value: Integer);
  protected
    procedure ListPick; dynamic;
    procedure DoEnter; override;
    procedure DoExit; override;
    {$WARNINGS OFF}
    {need to, do not change it !!!!}
    procedure SetItems(const Value: TStrings); {$IFDEF VER5UP}reintroduce;{$ELSE}virtual;{$ENDIF} //full hide!
    {$WARNINGS ON}
    {$IFDEF CB_HINTER}
    procedure CMMouseLeave(var message: TMessage); message CM_MOUSELEAVE;
    {$ENDIF CB_HINTER}
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CNCommand(var Message: TWMCommand); message CN_COMMAND;
    procedure WndProc(var Message: TMessage); override;
    {$IFDEF CB_HINTER}
    procedure ReleaseIt;
    {$ENDIF CB_HINTER}
    function DoValidate: Boolean;
    {$IFDEF CB_HINTER}
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    {$ENDIF CB_HINTER}
  public
    {$IFDEF CB_HINTER}
    HintWin: TPaintLineHintWindow;
    {$ENDIF CB_HINTER}
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {$WARNINGS OFF}
    {need to, do not change it !!!!}
    procedure DropDown; {$IFDEF VER5UP}reintroduce;{$ENDIF} //full hide!
    {$WARNINGS ON}
    procedure Clear; {$IFDEF VER6UP}override;{$ENDIF}
    property AsInteger: Integer read GetAsInteger write SetAsInteger;
    {$IFDEF CB_HINTER}
    procedure DefaultHandler(var message); override;
    {$ENDIF CB_HINTER}
    function Validator: Boolean;
    procedure Sort;
  published
    property Color;
    property ColorOptionsSet: TColorOptionsSet read FColorOptionsSet write FColorOptionsSet;
    property Ctl3D;
    property Cursor;
    property DragCursor;
    property DragMode;
    property DropDownCount;
    property DropDownFlexWidth:Boolean read FDropDownFlexWidth write FDropDownFlexWidth default False;
    property Enabled;
    property Font;
    property Height;
    property HelpContext;
    property Hint;
    {$IFDEF CB_HINTER}
    property HintText: Boolean read FHintText write FHintText default False;
    {$ENDIF CB_HINTER}
    property ItemHeight;
    property Items;
    property Left;
    property MaxLength;
    property Name;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Required: Boolean read FRequired write SetRequired default False;
    property ShowHint;
    property Sorted;
    property Style;
    property TabOrder;
    property TabStop;
    property Tag;
    property Text;
    property Top;
    property Visible;
    property Width;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnDropDown;
    property OnEndDrag;
    property OnEnter: TNotifyEvent read FOnEnter write FOnEnter;
    property OnError: TErrorOccur read FErrorOccur write FErrorOccur;
    property OnExit: TNotifyEvent read FOnExit write FOnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnListPick: TNotifyEvent read FOnListPick write FOnListPick;
    property OnItemsChanged: TNotifyEvent read FOnItemsChanged write FOnItemsChanged;
    property OnMeasureItem;
    property OnValidate: TValidate read FValidate write FValidate;
  end;

  {  TPubLabeledComboBox  }

  TPubLabeledComboBox = class(TPubComboBox)
  private
    FEditLabel: {$IFDEF USE_RXLIB}TRxLabel{$ELSE}{$IFNDEF VER6UP}TLabel{$ELSE}TBoundLabel{$ENDIF}{$ENDIF};
    FLabelPosition: TLabelPosition;                                                          
    FLabelSpacing: Integer;
    procedure SetLabelPosition(const Value: TLabelPosition);
    procedure SetLabelSpacing(const Value: Integer);
    function GetLabelShowsRequired: Boolean;
    procedure SetLabelShowsRequired(const Value: Boolean);
  protected
    procedure SetParent(AParent: TWinControl); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetName(const Value: TComponentName); override;
    procedure CMVisiblechanged(var Message: TMessage); message CM_VISIBLECHANGED;
    procedure CMEnabledchanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMBidimodechanged(var Message: TMessage); message CM_BIDIMODECHANGED;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer; AHeight: Integer); override;
  published
    property EditLabel: {$IFDEF USE_RXLIB}TRxLabel{$ELSE}{$IFNDEF VER6UP}TLabel{$ELSE}TBoundLabel{$ENDIF}{$ENDIF} read FEditLabel;
    property LabelPosition: TLabelPosition read FLabelPosition write SetLabelPosition default lpAbove;
    property LabelSpacing: Integer read FLabelSpacing write SetLabelSpacing default 3;
    property LabelShowsRequired: Boolean read GetLabelShowsRequired write SetLabelShowsRequired;
  end;

{$IFDEF VER7UP}

  {  TPubComboBox  }

  TPubComboBoxEx = class(TComboBoxEx)
  private
    {$IFDEF CB_HINTER}
    FShowing: Boolean; {True, pokud je hint okno aktivni}
    FSelectAll: Boolean;
    FWider: Boolean; {text je sirsi}
    FWindow: TForm; {extra instance formu zastupna pro cancas}
    FHintText: Boolean; {zvlastni instance}
    {$ENDIF}
    {}
    FErrorOccur: TErrorOccur;
    FRequired: Boolean; {neprazdne pole}
    FOnEnter: TNotifyEvent;
    FOnExit: TNotifyEvent;
    FColorOptionsSet: TColorOptionsSet;
    FOnListPick: TNotifyEvent;
    FOnItemsChanged: TNotifyEvent;
    FDropDownFlexWidth: Boolean;
    FValidate: TValidate;
    procedure SetRequired(Value: Boolean);
    procedure DoDropDownFlexWidth; virtual;
    function GetAsInteger: Integer;
    procedure SetAsInteger(const Value: Integer);
  protected
    procedure ListPick; dynamic;
    procedure DoEnter; override;
    procedure DoExit; override;
    {$WARNINGS OFF}
    {need to, do not change it !!!!}
    procedure SetItems(const Value: TStrings); {$IFDEF VER5UP}reintroduce;{$ELSE}virtual;{$ENDIF} //full hide!
    {$WARNINGS ON}
    {$IFDEF CB_HINTER}
    procedure CMMouseLeave(var message: TMessage); message CM_MOUSELEAVE;
    {$ENDIF CB_HINTER}
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CNCommand(var Message: TWMCommand); message CN_COMMAND;
    procedure WndProc(var Message: TMessage); override;
    {$IFDEF CB_HINTER}
    procedure ReleaseIt;
    {$ENDIF CB_HINTER}
    function DoValidate: Boolean;
    {$IFDEF CB_HINTER}
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    {$ENDIF CB_HINTER}
  public
    { public }
    {$IFDEF CB_HINTER}
    HintWin: TPaintLineHintWindow;
    {$ENDIF CB_HINTER}
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {$WARNINGS OFF}
    {need to, do not change it !!!!}
    procedure DropDown; {$IFDEF VER5UP}reintroduce;{$ENDIF} //full hide!
    {$WARNINGS ON}
    procedure Clear; {$IFDEF VER7UP}override;{$ENDIF}
    property AsInteger: Integer read GetAsInteger write SetAsInteger;
    {$IFDEF CB_HINTER}
    procedure DefaultHandler(var message); override;
    {$ENDIF CB_HINTER}
    function Validator: Boolean;
    procedure Sort;
  published
    { published }
    property ColorOptionsSet: TColorOptionsSet read FColorOptionsSet write FColorOptionsSet;
    property DropDownFlexWidth:Boolean read FDropDownFlexWidth write FDropDownFlexWidth default False;
    {$IFDEF CB_HINTER}
    property HintText: Boolean read FHintText write FHintText default False;
    {$ENDIF CB_HINTER}
    property Required: Boolean read FRequired write SetRequired default False;
    property OnEnter: TNotifyEvent read FOnEnter write FOnEnter;
    property OnError: TErrorOccur read FErrorOccur write FErrorOccur;
    property OnExit: TNotifyEvent read FOnExit write FOnExit;
    property OnListPick: TNotifyEvent read FOnListPick write FOnListPick;
    property OnItemsChanged: TNotifyEvent read FOnItemsChanged write FOnItemsChanged;
    property OnValidate: TValidate read FValidate write FValidate;
  end;

  {  TPubLabeledComboBox  }

  TPubLabeledComboBoxEx = class(TPubComboBoxEx)
  private
    FEditLabel: {$IFDEF USE_RXLIB}TRxLabel{$ELSE}{$IFNDEF VER6UP}TLabel{$ELSE}TBoundLabel{$ENDIF}{$ENDIF};
    FLabelPosition: TLabelPosition;
    FLabelSpacing: Integer;
    procedure SetLabelPosition(const Value: TLabelPosition);
    procedure SetLabelSpacing(const Value: Integer);
    function GetLabelShowsRequired: Boolean;
    procedure SetLabelShowsRequired(const Value: Boolean);
  protected
    procedure SetParent(AParent: TWinControl); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetName(const Value: TComponentName); override;
    procedure CMVisiblechanged(var Message: TMessage); message CM_VISIBLECHANGED;
    procedure CMEnabledchanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMBidimodechanged(var Message: TMessage); message CM_BIDIMODECHANGED;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer; AHeight: Integer); override;
  published
    property EditLabel: {$IFDEF USE_RXLIB}TRxLabel{$ELSE}{$IFNDEF VER6UP}TLabel{$ELSE}TBoundLabel{$ENDIF}{$ENDIF} read FEditLabel;
    property LabelPosition: TLabelPosition read FLabelPosition write SetLabelPosition default lpAbove;
    property LabelSpacing: Integer read FLabelSpacing write SetLabelSpacing default 3;
    property LabelShowsRequired: Boolean read GetLabelShowsRequired write SetLabelShowsRequired;
  end;

{$ENDIF}

{$IFDEF USE_RXLIB}

  {  TPubCurrencyEdit  }

  TPubCurrencyEdit = class(TCurrencyEdit)
  private
    FRequired: Boolean; {neprazdne pole}
    FOnEnter: TNotifyEvent;
    FOnExit: TNotifyEvent;
    FColorOptionsSet: TColorOptionsSet;
    FValidate: TValidate;
    procedure SetRequired(Value: Boolean);
  protected
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    function DoValidate: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Validator: Boolean;
  published
    property Alignment;
    property AutoSelect;
    property AutoSize;
    property BeepOnError;
    property BorderStyle;
    property CheckOnExit;
    property Color;
    property ColorOptionsSet: TColorOptionsSet read FColorOptionsSet write FColorOptionsSet;
    property Ctl3D;
    property Cursor;
    property DecimalPlaces;
    property DisplayFormat;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property FormatOnEditing;
    property Height;
    property HelpContext;
    property HideSelection;
    property Hint;
    property Left;
    property MaxLength;
    property MaxValue;
    property MinValue;
    property Name;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property Required: Boolean read FRequired write SetRequired default False;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Tag;
    property Text;
    property Top;
    property Value;
    property Visible;
    property Width;
    property ZeroEmpty;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter: TNotifyEvent read FOnEnter write FOnEnter;
    property OnExit: TNotifyEvent read FOnExit write FOnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnValidate: TValidate read FValidate write FValidate;
  end;

  { TPubLabeledCurrencyEdit }

  TPubLabeledCurrencyEdit = class(TPubCurrencyEdit)
  private
    FEditLabel: TRxLabel;
    FLabelPosition: TLabelPosition;
    FLabelSpacing: Integer;
    procedure SetLabelPosition(const Value: TLabelPosition);
    procedure SetLabelSpacing(const Value: Integer);
    function GetLabelShowsRequired: Boolean;
    procedure SetLabelShowsRequired(const Value: Boolean);
  protected
    procedure SetParent(AParent: TWinControl); override;
    procedure SetName(const Value: TComponentName); override;
    procedure CMVisiblechanged(var Message: TMessage); message CM_VISIBLECHANGED;
    procedure CMEnabledchanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMBidimodechanged(var Message: TMessage); message CM_BIDIMODECHANGED;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer; AHeight: Integer); override;
  published
    property EditLabel: TRxLabel read FEditLabel write FEditLabel;
    property LabelPosition: TLabelPosition read FLabelPosition write SetLabelPosition default lpAbove;
    property LabelSpacing: Integer read FLabelSpacing write SetLabelSpacing default 3;
    property LabelShowsRequired: Boolean read GetLabelShowsRequired write SetLabelShowsRequired;
  end;

  {  TPubDateEdit  }

  TPubDateEdit = class(TDateEdit)
  private
    FRequired: Boolean; {neprazdne pole}
    FOnEnter: TNotifyEvent;
    FOnExit: TNotifyEvent;
    FColorOptionsSet: TColorOptionsSet;
    procedure SetRequired(Value: Boolean);
  protected
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property AutoSelect;
    property BlanksChar;
    property BorderStyle;
    property ButtonHint;
    property ButtonWidth;
    property CalendarHints;
    property CalendarStyle;
    property CheckOnExit;
    property Color;
    property ColorOptionsSet: TColorOptionsSet read FColorOptionsSet write FColorOptionsSet;
    property Ctl3D;
    property Cursor;
    property DefaultToday;
    property DialogTitle;
    property DirectInput;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property Glyph;
    property Height;
    property HelpContext;
    property HideSelection;
    property Hint;
    property Left;
    property MaxLength;
    property Name;
    property NumGlyphs;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupAlign;
    property PopupColor;
    property PopupMenu;
    property ReadOnly;
    property Required: Boolean read FRequired write SetRequired default False;
    property ShowHint;
    property StartOfWeek;
    property TabOrder;
    property TabStop;
    property Tag;
    property Text;
    property Top;
    property Visible;
    property WeekendColor;
    property Weekends;
    property Width;
    property YearDigits;
    property OnAcceptDate;
    property OnButtonClick;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter: TNotifyEvent read FOnEnter write FOnEnter;
    property OnExit: TNotifyEvent read FOnExit write FOnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

  {  TPubLabeledDateEdit  }

  TPubLabeledDateEdit = class(TPubDateEdit)
  private
    FEditLabel: TRxLabel;
    FLabelPosition: TLabelPosition;
    FLabelSpacing: Integer;
    procedure SetLabelPosition(const Value: TLabelPosition);
    procedure SetLabelSpacing(const Value: Integer);
    function GetLabelShowsRequired: Boolean;
    procedure SetLabelShowsRequired(const Value: Boolean);
  protected
    procedure SetParent(AParent: TWinControl); override;
    procedure SetName(const Value: TComponentName); override;
    procedure CMVisiblechanged(var Message: TMessage); message CM_VISIBLECHANGED;
    procedure CMEnabledchanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMBidimodechanged(var Message: TMessage); message CM_BIDIMODECHANGED;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer; AHeight: Integer); override;
  published
    property EditLabel: TRxLabel read FEditLabel write FEditLabel;
    property LabelPosition: TLabelPosition read FLabelPosition write SetLabelPosition default lpAbove;
    property LabelSpacing: Integer read FLabelSpacing write SetLabelSpacing default 3;
    property LabelShowsRequired: Boolean read GetLabelShowsRequired write SetLabelShowsRequired;
  end;

  {  TPubFilenameEdit  }

  TPubFilenameEdit = class(TFilenameEdit)
  private
    FRequired: Boolean; {neprazdne pole}
    FOnEnter: TNotifyEvent;
    FOnExit: TNotifyEvent;
    FColorOptionsSet: TColorOptionsSet;
    procedure SetRequired(Value: Boolean);
  protected
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property AcceptFiles;
    property AutoSelect;
    property BorderStyle;
    property ButtonHint;
    property ButtonWidth;
    property CharCase;
    property ClickKey;
    property Color;
    property ColorOptionsSet: TColorOptionsSet read FColorOptionsSet write FColorOptionsSet;
    property Ctl3D;
    property Cursor;
    property DefaultExt;
    property DialogKind;
    property DialogOptions;
    property DialogTitle;
    property DirectInput;
    property DragCursor;
    property DragMode;
    property EditMask;
    property Enabled;
    property FileEditStyle;
    property FileName;
    property Filter;
    property FilterIndex;
    property Font;
    property Glyph;
    property GlyphKind;
    property Height;
    property HelpContext;
    property HideSelection;
    property Hint;
    property HistoryList;
    property InitialDir;
    property Left;
    property Name;
    property NumGlyphs;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property Required: Boolean read FRequired write SetRequired default False;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Tag;
    property Text;
    property Top;
    property Visible;
    property Width;
    property OnAfterDialog;
    property OnBeforeDialog;
    property OnButtonClick;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDropFiles;
    property OnEndDrag;
    property OnEnter: TNotifyEvent read FOnEnter write FOnEnter;
    property OnExit: TNotifyEvent read FOnExit write FOnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

  {  TPubLabeledFilenameEdit  }

  TPubLabeledFilenameEdit = class(TPubFilenameEdit)
  private
    FEditLabel: TRxLabel;
    FLabelPosition: TLabelPosition;
    FLabelSpacing: Integer;
    procedure SetLabelPosition(const Value: TLabelPosition);
    procedure SetLabelSpacing(const Value: Integer);
    function GetLabelShowsRequired: Boolean;
    procedure SetLabelShowsRequired(const Value: Boolean);
  protected
    procedure SetParent(AParent: TWinControl); override;
    procedure SetName(const Value: TComponentName); override;
    procedure CMVisiblechanged(var Message: TMessage); message CM_VISIBLECHANGED;
    procedure CMEnabledchanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMBidimodechanged(var Message: TMessage); message CM_BIDIMODECHANGED;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer; AHeight: Integer); override;
  published
    property EditLabel: TRxLabel read FEditLabel write FEditLabel;
    property LabelPosition: TLabelPosition read FLabelPosition write SetLabelPosition default lpAbove;
    property LabelSpacing: Integer read FLabelSpacing write SetLabelSpacing default 3;
    property LabelShowsRequired: Boolean read GetLabelShowsRequired write SetLabelShowsRequired;
  end;

  {  TPubDirectoryEdit  }

  TPubDirectoryEdit = class(TDirectoryEdit)
  private
    FRequired: Boolean; {neprazdne pole}
    FOnEnter: TNotifyEvent;
    FOnExit: TNotifyEvent;
    FColorOptionsSet: TColorOptionsSet;
    procedure SetRequired(Value: Boolean);
  protected
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property AcceptFiles;
    property AutoSelect;
    property BorderStyle;
    property ButtonHint;
    property ButtonWidth;
    property CharCase;
    property ClickKey;
    property Color;
    property ColorOptionsSet: TColorOptionsSet read FColorOptionsSet write FColorOptionsSet;
    property Ctl3D;
    property Cursor;
    property DialogOptions;
    property DirectInput;
    property DragCursor;
    property DragMode;
    property EditMask;
    property Enabled;
    property Font;
    property Glyph;
    property GlyphKind;
    property Height;
    property HelpContext;
    property HideSelection;
    property Hint;
    property InitialDir;
    property Left;
    property MultipleDirs;
    property Name;
    property NumGlyphs;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property Required: Boolean read FRequired write SetRequired default False;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Tag;
    property Text;
    property Top;
    property Visible;
    property Width;
    property OnAfterDialog;
    property OnBeforeDialog;
    property OnButtonClick;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDropFiles;
    property OnEndDrag;
    property OnEnter: TNotifyEvent read FOnEnter write FOnEnter;
    property OnExit: TNotifyEvent read FOnExit write FOnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

  {  TPubLabeledDirectoryEdit  }

  TPubLabeledDirectoryEdit = class(TPubDirectoryEdit)
  private
    FEditLabel: TRxLabel;
    FLabelPosition: TLabelPosition;
    FLabelSpacing: Integer;
    procedure SetLabelPosition(const Value: TLabelPosition);
    procedure SetLabelSpacing(const Value: Integer);
    function GetLabelShowsRequired: Boolean;
    procedure SetLabelShowsRequired(const Value: Boolean);
  protected
    procedure SetParent(AParent: TWinControl); override;
    procedure SetName(const Value: TComponentName); override;
    procedure CMVisiblechanged(var Message: TMessage); message CM_VISIBLECHANGED;
    procedure CMEnabledchanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMBidimodechanged(var Message: TMessage); message CM_BIDIMODECHANGED;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer; AHeight: Integer); override;
  published
    property EditLabel: TRxLabel read FEditLabel write FEditLabel;
    property LabelPosition: TLabelPosition read FLabelPosition write SetLabelPosition default lpAbove;
    property LabelSpacing: Integer read FLabelSpacing write SetLabelSpacing default 3;
    property LabelShowsRequired: Boolean read GetLabelShowsRequired write SetLabelShowsRequired;
  end;

  {  TPubRxCalcEdit  }

  TPubRxCalcEdit = class(TRxCalcEdit)
  private
    FRequired: Boolean; {neprazdne pole}
    FOnEnter: TNotifyEvent;
    FOnExit: TNotifyEvent;
    FColorOptionsSet: TColorOptionsSet;
    procedure SetRequired(Value: Boolean);
  protected
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Alignment;
    property AutoSelect;
    property AutoSize;
    property BeepOnError;
    property BorderStyle;
    property ButtonHint;
    property ButtonWidth;
    property CheckOnExit;
    property ClickKey;
    property Color;
    property ColorOptionsSet: TColorOptionsSet read FColorOptionsSet write FColorOptionsSet;
    property Ctl3D;
    property Cursor;
    property DecimalPlaces;
    property DirectInput;
    property DisplayFormat;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property FormatOnEditing;
    property Glyph;
    property GlyphKind;
    property Height;
    property HelpContext;
    property HideSelection;
    property Hint;
    property Left;
    property MaxLength;
    property MaxValue;
    property MinValue;
    property Name;
    property NumGlyphs;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property Required: Boolean read FRequired write SetRequired default False;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Tag;
    property Text;
    property Top;
    property Value;
    property Visible;
    property Width;
    property ZeroEmpty;
    property OnButtonClick;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter: TNotifyEvent read FOnEnter write FOnEnter;
    property OnExit: TNotifyEvent read FOnExit write FOnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

  {  TPubLabeledRxCalcEdit  }

  TPubLabeledRxCalcEdit = class(TPubRxCalcEdit)
  private
    FEditLabel: TRxLabel;
    FLabelPosition: TLabelPosition;
    FLabelSpacing: Integer;
    procedure SetLabelPosition(const Value: TLabelPosition);
    procedure SetLabelSpacing(const Value: Integer);
    function GetLabelShowsRequired: Boolean;
    procedure SetLabelShowsRequired(const Value: Boolean);
  protected
    procedure SetParent(AParent: TWinControl); override;
    procedure SetName(const Value: TComponentName); override;
    procedure CMVisiblechanged(var Message: TMessage); message CM_VISIBLECHANGED;
    procedure CMEnabledchanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMBidimodechanged(var Message: TMessage); message CM_BIDIMODECHANGED;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer; AHeight: Integer); override;
  published
    property EditLabel: TRxLabel read FEditLabel write FEditLabel;
    property LabelPosition: TLabelPosition read FLabelPosition write SetLabelPosition default lpAbove;
    property LabelSpacing: Integer read FLabelSpacing write SetLabelSpacing default 3;
    property LabelShowsRequired: Boolean read GetLabelShowsRequired write SetLabelShowsRequired;
  end;

  {  TPubComboEdit  }

  TPubComboEdit = class(TComboEdit)
  private
    FValidate: TValidate;
    FConvert: TConvert;
    FErrorOccur: TErrorOccur;
    FMaskForm: TMaskForm;
    FRequired: Boolean; {neprazdne pole}
    FCapitalize: Boolean; {prvni znaky slov velke}
    FOnEnter: TNotifyEvent;
    FOnExit: TNotifyEvent;
    FValids: string; {platne znaky napr. 'abcABC'}
    FMask: string; {maska napriklad 'XX.XX.XX/XX-XX'}
    FColorOptionsSet: TColorOptionsSet;
    function DoValidate: Boolean;
    procedure SetRequired(Value: Boolean);
  protected
    procedure SetCapitalize(Value: Boolean);
    procedure KeyPress(var Key: Char); override;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Validator: Boolean;
  published
    property Alignment;
    property AutoSelect;
    property BorderStyle;
    property ButtonHint;
    property ButtonWidth;
    property Capitalize: Boolean read FCapitalize write SetCapitalize default False;
    property CharCase;
    property ClickKey;
    property Color;
    property ColorOptionsSet: TColorOptionsSet read FColorOptionsSet write FColorOptionsSet;
    property Ctl3D;
    property Cursor;
    property DirectInput;
    property DragCursor;
    property DragMode;
    property EditMask;
    property Enabled;
    property Font;
    property Glyph;
    property GlyphKind;
    property Height;
    property HelpContext;
    property HideSelection;
    property Hint;
    property Left;
    property MaxLength;
    property Name;
    property NumGlyphs;
    property OEMConvert;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property Required: Boolean read FRequired write SetRequired default False;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Tag;
    property Text;
    property Top;
    property ValidChars: string read FValids write FValids;
    property Visible;
    property Width;
    property OnButtonClick;
    property OnConvert: TConvert read FConvert write FConvert;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter: TNotifyEvent read FOnEnter write FOnEnter;
    property OnError: TErrorOccur read FErrorOccur write FErrorOccur;
    property OnExit: TNotifyEvent read FOnExit write FOnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMask: TMaskForm read FMaskForm write FMaskForm;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnValidate: TValidate read FValidate write FValidate;
  end;

  {  TPubLabeledComboEdit  }

  TPubLabeledComboEdit = class(TPubComboEdit)
  private
    FEditLabel: TRxLabel;
    FLabelPosition: TLabelPosition;
    FLabelSpacing: Integer;
    procedure SetLabelPosition(const Value: TLabelPosition);
    procedure SetLabelSpacing(const Value: Integer);
    function GetLabelShowsRequired: Boolean;
    procedure SetLabelShowsRequired(const Value: Boolean);
  protected
    procedure SetParent(AParent: TWinControl); override;
    procedure SetName(const Value: TComponentName); override;
    procedure CMVisiblechanged(var Message: TMessage); message CM_VISIBLECHANGED;
    procedure CMEnabledchanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMBidimodechanged(var Message: TMessage); message CM_BIDIMODECHANGED;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer; AHeight: Integer); override;
  published
    property EditLabel: TRxLabel read FEditLabel write FEditLabel;
    property LabelPosition: TLabelPosition read FLabelPosition write SetLabelPosition default lpAbove;
    property LabelSpacing: Integer read FLabelSpacing write SetLabelSpacing default 3;
    property LabelShowsRequired: Boolean read GetLabelShowsRequired write SetLabelShowsRequired;
  end;

  {  TPubSpinNumEdit  }

  TPubSpinNumEdit = class(TRxSpinEdit)
  private
    FColorOptionsSet: TColorOptionsSet;
    FOnEnter: TNotifyEvent;
    FOnExit: TNotifyEvent;
    FValidate: TValidate;
  protected
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    function DoValidate: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Validator: Boolean;
  published
    property ColorOptionsSet: TColorOptionsSet read FColorOptionsSet write FColorOptionsSet;
    property OnEnter: TNotifyEvent read FOnEnter write FOnEnter;
    property OnExit: TNotifyEvent read FOnExit write FOnExit;
    property OnValidate: TValidate read FValidate write FValidate;
  end;

  {  TPubLabeledSpinNumEdit  }

  TPubLabeledSpinNumEdit = class(TPubSpinNumEdit)
  private
    FEditLabel: TRxLabel;
    FLabelPosition: TLabelPosition;
    FLabelSpacing: Integer;
    procedure SetLabelPosition(const Value: TLabelPosition);
    procedure SetLabelSpacing(const Value: Integer);
    function GetLabelShowsRequired: Boolean;
    procedure SetLabelShowsRequired(const Value: Boolean);
  protected
    procedure SetParent(AParent: TWinControl); override;
    procedure SetName(const Value: TComponentName); override;
    procedure CMVisiblechanged(var Message: TMessage); message CM_VISIBLECHANGED;
    procedure CMEnabledchanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMBidimodechanged(var Message: TMessage); message CM_BIDIMODECHANGED;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer; AHeight: Integer); override;
  published
    property EditLabel: TRxLabel read FEditLabel write FEditLabel;
    property LabelPosition: TLabelPosition read FLabelPosition write SetLabelPosition default lpAbove;
    property LabelSpacing: Integer read FLabelSpacing write SetLabelSpacing default 3;
    property LabelShowsRequired: Boolean read GetLabelShowsRequired write SetLabelShowsRequired;
  end;

{$ENDIF USE_RXLIB}

const
  Button1ShortCutInvoke = scShift + VK_NEXT;
  Button2ShortCutInvoke = scShift + VK_PRIOR;

  ccBoolDefYes: string = '1';
  ccBoolDefNo: string = '0';

  InitRepeatPause = 400;  { pause before repeat timer (ms) }
  RepeatPause     = 100;  { pause before hint window displays (ms)}

type

  {  TNumGlyphs  }

  TNumGlyphs = Buttons.TNumGlyphs;  //remaped only

  {  TTimerSpeedButton  }

  TTimerSpeedButton = class;  //forwarding

  { TSpinButton }

  TPubSpinButton = class (TWinControl)
  private
    FUpButton: TTimerSpeedButton;
    FDownButton: TTimerSpeedButton;
    FFocusedButton: TTimerSpeedButton;
    FFocusControl: TWinControl;
    FOnUpClick: TNotifyEvent;
    FOnDownClick: TNotifyEvent;
    function CreateButton: TTimerSpeedButton;
    function GetUpGlyph: TBitmap;
    function GetDownGlyph: TBitmap;
    procedure SetUpGlyph(Value: TBitmap);
    procedure SetDownGlyph(Value: TBitmap);
    function GetUpNumGlyphs: TNumGlyphs;
    function GetDownNumGlyphs: TNumGlyphs;
    procedure SetUpNumGlyphs(Value: TNumGlyphs);
    procedure SetDownNumGlyphs(Value: TNumGlyphs);
    procedure BtnClick(Sender: TObject);
    procedure BtnMouseDown (Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SetFocusBtn (Btn: TTimerSpeedButton);
    procedure AdjustSize (var W, H: Integer); {$IFDEF VER4UP}reintroduce;{$ENDIF}
    procedure WMSize(var Message: TWMSize);  message WM_SIZE;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
  protected
    procedure Loaded; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  published
    property Align;
    {$IFDEF VER4UP}property Anchors;{$ENDIF}
    {$IFDEF VER4UP}property Constraints;{$ENDIF}
    property Ctl3D;
    property DownGlyph: TBitmap read GetDownGlyph write SetDownGlyph;
    property DownNumGlyphs: TNumGlyphs read GetDownNumGlyphs write SetDownNumGlyphs default 1;
    {$IFDEF VER4UP}property DragCursor;{$ENDIF}
    {$IFDEF VER4UP}property DragKind;{$ENDIF}
    property DragMode;
    property Enabled;
    property FocusControl: TWinControl read FFocusControl write FFocusControl;
    property ParentCtl3D;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property UpGlyph: TBitmap read GetUpGlyph write SetUpGlyph;
    property UpNumGlyphs: TNumGlyphs read GetUpNumGlyphs write SetUpNumGlyphs default 1;
    property Visible;
    property OnDownClick: TNotifyEvent read FOnDownClick write FOnDownClick;
    property OnDragDrop;
    property OnDragOver;
    {$IFDEF VER4UP}property OnEndDock;{$ENDIF}
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    {$IFDEF VER4UP}property OnStartDock;{$ENDIF}
    property OnStartDrag;
    property OnUpClick: TNotifyEvent read FOnUpClick write FOnUpClick;
  end;

  { TSpinEdit }

  TPubSpinEdit = class(TPubEdit)
  private
    FMinValue: LongInt;
    FMaxValue: LongInt;
    FIncrement: LongInt;
    FButton: TPubSpinButton;
    FEditorEnabled: Boolean;
    function GetMinHeight: Integer;
    function GetValue: LongInt;
    function CheckValue (NewValue: LongInt): LongInt;
    procedure SetValue (NewValue: LongInt);
    procedure SetEditRect;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure CMEnter(var Message: TCMGotFocus); message CM_ENTER;
    procedure CMExit(var Message: TCMExit);   message CM_EXIT;
    procedure WMPaste(var Message: TWMPaste);   message WM_PASTE;
    procedure WMCut(var Message: TWMCut);   message WM_CUT;
  protected
    function IsValidChar(Key: Char): Boolean; virtual;
    procedure UpClick (Sender: TObject); virtual;
    procedure DownClick (Sender: TObject); virtual;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
  public
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Button: TPubSpinButton read FButton;
  published
    {$IFDEF VER4UP}property Anchors;{$ENDIF}
    property AutoSelect;
    property AutoSize;
    property Color;
    {$IFDEF VER4UP}property Constraints;{$ENDIF}
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property EditorEnabled: Boolean read FEditorEnabled write FEditorEnabled default True;
    property Enabled;
    property Font;
    property Increment: LongInt read FIncrement write FIncrement default 1;
    property MaxLength;
    property MaxValue: LongInt read FMaxValue write FMaxValue;
    property MinValue: LongInt read FMinValue write FMinValue;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Value: LongInt read GetValue write SetValue;
    property Visible;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
  end;

  { TTimerSpeedButton }

  TTimeBtnState = set of (tbFocusRect, tbAllowTimer);

  {  TTimerSpeedButton  }

  TTimerSpeedButton = class({$IFDEF USE_RXLIB}TRxSpeedButton{$ELSE}TSpeedButton{$ENDIF})
  private
    FRepeatTimer: TTimer;
    FTimeBtnState: TTimeBtnState;
    procedure TimerExpired(Sender: TObject);
  protected
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
  public
    destructor Destroy; override;
    property TimeBtnState: TTimeBtnState read FTimeBtnState write FTimeBtnState;
  end;

{$IFDEF USE_VG2LIB}

{use own label from vg2Lib}
{$Define useVGLibrary}

  { TPub2Edit }

  TPub2Edit = class(Tvg2Edit)
  private
    { Private declarations }
    FShowing: Boolean; {True, pokud je hint okno aktivni}
    FSelectAll: Boolean;
    FWider: Boolean; {text je sirsi}
    FWindow: TForm; {extra instance formu zastupna pro cancas}
    FHintText: Boolean; {zvlastni instance}
{$IfDef UseAutocomplete}
    FAutocomplete: TAutoCompleteText;
{$EndIf}
    FOnMouseExit: TNotifyEvent;
    FOnMouseEnter: TNotifyEvent;
    FAlignment: TAlignment;
    FTypeEdit: TTypeEdit; {typ pole s ruznymi akcemi}
    FValidate: TValidate; {eventy}
    FConvert: TConvert;
    FErrorOccur: TErrorOccur;
    FMaskForm: TMaskForm;
    FRequired: Boolean; {neprazdne pole}
    FCapitalize: Boolean; {prvni znaky slov velke}
    FOnEnter: TNotifyEvent;
    FOnExit: TNotifyEvent;
    FValidChars: string; {platne znaky napr. 'abcABC'}
    FMask: string; {maska napriklad 'XX.XX.XX/XX-XX'}
    FColorOptionsSet: TColorOptionsSet;
    FParsing: TParsing;
    FOnPaste: TNotifyEvent;
    FOnCopy: TNotifyEvent;
    FOnCut: TNotifyEvent;
    FOnClear: TNotifyEvent;
    procedure SetAlignment(Value: TAlignment);
    procedure SetRequired(Value: Boolean);
    procedure SetCapitalize(Value: Boolean);
    procedure SetAsInteger(Value: Integer);
    function GetAsInteger: Integer;
    procedure SetAsFloat(Value: Real);
    function GetAsFloat: Real;
    procedure SetAsDate(Value: TDate);
    function GetAsDate: TDate;
    procedure SetAsTime(Value: TTime);
    function GetAsTime: TTime;
    procedure ReleaseIt;
    function GetAsBoolean: Boolean;
    function GetAsCurrency: Currency;
    function GetAsDateTime: TDateTime;
    procedure SetAsBoolean(const Value: Boolean);
    procedure SetAsCurrency(const Value: Currency);
    procedure SetAsDateTime(const Value: TDateTime);
    procedure SetAsString(Value: string);
    function GetAsString: string;
    function DoValidate(var ErrMsg: string): Boolean;
    procedure SetAsAnsiString(Value: AnsiString);
    function GetAsAnsiString: AnsiString;
  protected
    { Protected declarations }
    procedure CreateParams(var Params: TCreateParams); override;
    procedure KeyPress(var Key: Char); override;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMMouseEnter(var message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var message: TMessage); message CM_MOUSELEAVE;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure WMPaste(var Message: TMessage); message WM_PASTE;
    procedure WMCut(var Message: TMessage); message WM_CUT;
    procedure WMCopy(var Message: TMessage); message WM_COPY;
    procedure WMClear(var Message: TMessage); message WM_CLEAR;
    procedure NextControl(YesNo: Boolean); {nepouzil jsem}
{$IfDef UseAutocomplete}
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
{$EndIf}
  public
    HintWin: TPaintLineHintWindow;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Validate: Boolean;
    function Parsed(const S: string): string;
    procedure DefaultHandler(var message); override;
    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    property AsCurrency: Currency read GetAsCurrency write SetAsCurrency;
    property AsInteger: Integer read GetAsInteger write SetAsInteger;
    property AsFloat: Real read GetAsFloat write SetAsFloat;
    property AsDate: TDate read GetAsDate write SetAsDate;
    property AsTime: TTime read GetAsTime write SetAsTime;
    property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;
    property AsString: string read GetAsString write SetAsString;
    property AsAnsiString: AnsiString read GetAsAnsiString write SetAsAnsiString;
  published
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property AutoSelect;
    property AutoSize;
    property BorderStyle;
    property Capitalize: Boolean read FCapitalize write SetCapitalize default False;
    property CharCase;
    property Color;
    property ColorOptionsSet: TColorOptionsSet read FColorOptionsSet write FColorOptionsSet;
    property Ctl3D;
    property Cursor;
    property DragCursor;
    property DragMode;
    property EditType: TTypeEdit read FTypeEdit write FTypeEdit default teString;
    property Enabled;
    property Font;
    property Height;
    property HelpContext;
    property HideSelection;
    property Hint;
    property HintText: Boolean read FHintText write FHintText default False;
    property Left;
    property Mask: string read FMask write FMask;
    property MaxLength;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    property Name;
    property OEMConvert;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PopupMenu;
    property ReadOnly;
    property Required: Boolean read FRequired write SetRequired default False;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Tag;
    property Text;
    property Top;
    property ValidChars: string read FValidChars write FValidChars;
    property Visible;
    property Width;
    property OnChange;
    property OnClick;
    property OnConvert: TConvert read FConvert write FConvert;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter: TNotifyEvent read FOnEnter write FOnEnter;
    property OnError: TErrorOccur read FErrorOccur write FErrorOccur;
    property OnExit: TNotifyEvent read FOnExit write FOnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMask: TMaskForm read FMaskForm write FMaskForm;
    property OnMouseDown;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseExit: TNotifyEvent read FOnMouseExit write FOnMouseExit;
    property OnMouseMove;
    property OnMouseUp;
    property OnValidate: TValidate read FValidate write FValidate;
{$IfDef UseAutocomplete}
    property Autocomplete: TAutoCompleteText read FAutocomplete write FAutocomplete;
{$EndIf}
    property OnParsing: TParsing read FParsing write FParsing;
  end;

  {  TPub2LabeledEdit  }

  TPub2LabeledEdit = class(TPub2Edit)
  private
    FEditLabel: {$IFDEF useVGLibrary}Tvg2Label{$ELSE}{$IFNDEF VER6UP}TLabel{$ELSE}TBoundLabel{$ENDIF}{$ENDIF};
    FLabelPosition: TLabelPosition;
    FLabelSpacing: Integer;
    procedure SetLabelPosition(const Value: TLabelPosition);
    procedure SetLabelSpacing(const Value: Integer);
    function GetLabelShowsRequired: Boolean;
    procedure SetLabelShowsRequired(const Value: Boolean);
  protected
    procedure SetParent(AParent: TWinControl); override;
    procedure SetName(const Value: TComponentName); override;
    procedure CMVisiblechanged(var Message: TMessage); message CM_VISIBLECHANGED;
    procedure CMEnabledchanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMBidimodechanged(var Message: TMessage); message CM_BIDIMODECHANGED;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer; AHeight: Integer); override;
  published
    property EditLabel: {$IFDEF useVGlibrary}Tvg2Label{$ELSE}{$IFNDEF VER6UP}TLabel{$ELSE}TBoundLabel{$ENDIF}{$ENDIF} read FEditLabel write FEditLabel;
    property LabelPosition: TLabelPosition read FLabelPosition write SetLabelPosition default lpAbove;
    property LabelSpacing: Integer read FLabelSpacing write SetLabelSpacing default 3;
    property LabelShowsRequired: Boolean read GetLabelShowsRequired write SetLabelShowsRequired;
  end;

  {  TPub2SpinEdit  }

  TPub2SpinEdit = class(Tvg2SpinEdit)
  private
    FOnMouseExit: TNotifyEvent;
    FOnMouseEnter: TNotifyEvent;
    FColorOptionsSet: TColorOptionsSet;
  protected
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMMouseEnter(var message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var message: TMessage); message CM_MOUSELEAVE;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property ColorOptionsSet: TColorOptionsSet read FColorOptionsSet write FColorOptionsSet;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseExit: TNotifyEvent read FOnMouseExit write FOnMouseExit;
  end;

  {  TPub2CalculatorEdit  }

  TPub2CalculatorEdit = class(Tvg2CalculatorEdit)
  private
    FOnMouseExit: TNotifyEvent;
    FOnMouseEnter: TNotifyEvent;
    FColorOptionsSet: TColorOptionsSet;
  protected
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMMouseEnter(var message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var message: TMessage); message CM_MOUSELEAVE;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property ColorOptionsSet: TColorOptionsSet read FColorOptionsSet write FColorOptionsSet;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseExit: TNotifyEvent read FOnMouseExit write FOnMouseExit;
  end;

  {  TPub2DateTimeEdit  }

  TPub2DateTimeEdit = class(Tvg2DateTimeEdit)
  private
    FOnMouseExit: TNotifyEvent;
    FOnMouseEnter: TNotifyEvent;
    FColorOptionsSet: TColorOptionsSet;
  protected
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMMouseEnter(var message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var message: TMessage); message CM_MOUSELEAVE;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property ColorOptionsSet: TColorOptionsSet read FColorOptionsSet write FColorOptionsSet;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseExit: TNotifyEvent read FOnMouseExit write FOnMouseExit;
  end;

  {  TPub2FileNameEdit  }

  TPub2FileNameEdit = class(Tvg2FileNameEdit)
  private
    FOnMouseExit: TNotifyEvent;
    FOnMouseEnter: TNotifyEvent;
    FColorOptionsSet: TColorOptionsSet;
  protected
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMMouseEnter(var message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var message: TMessage); message CM_MOUSELEAVE;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property ColorOptionsSet: TColorOptionsSet read FColorOptionsSet write FColorOptionsSet;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseExit: TNotifyEvent read FOnMouseExit write FOnMouseExit;
  end;

  {  TPub2FolderEdit  }

  TPub2FolderEdit = class(Tvg2FolderEdit)
  private
    FOnMouseExit: TNotifyEvent;
    FOnMouseEnter: TNotifyEvent;
    FColorOptionsSet: TColorOptionsSet;
  protected
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMMouseEnter(var message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var message: TMessage); message CM_MOUSELEAVE;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property ColorOptionsSet: TColorOptionsSet read FColorOptionsSet write FColorOptionsSet;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseExit: TNotifyEvent read FOnMouseExit write FOnMouseExit;
  end;

  {  TPub2ComboBox  }

  TPub2ComboBox = class(Tvg2ComboBox)
  private
    FOnMouseExit: TNotifyEvent;
    FOnMouseEnter: TNotifyEvent;
    FColorOptionsSet: TColorOptionsSet;
    FDropDownFlexWidth: Boolean;
    FOnListPick: TNotifyEvent;
    FOnItemsChanged: TNotifyEvent;
    procedure DoDropDownFlexWidth; virtual;
  protected
    procedure ListPick; dynamic;
    procedure DoEnter; override;
    procedure DoExit; override;
    {$WARNINGS OFF}
    {need to, do not change it !!!!}
    procedure SetItems(const Value: TStrings); {$IFDEF VER5UP}reintroduce;{$ELSE}virtual;{$ENDIF} //full hide!
    {$WARNINGS ON}
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMMouseEnter(var message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var message: TMessage); message CM_MOUSELEAVE;
    procedure CNCommand(var Message: TWMCommand); message CN_COMMAND;
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property DropDownFlexWidth:Boolean read FDropDownFlexWidth write FDropDownFlexWidth default False;
    property ColorOptionsSet: TColorOptionsSet read FColorOptionsSet write FColorOptionsSet;
    property OnItemsChanged: TNotifyEvent read FOnItemsChanged write FOnItemsChanged;
    property OnListPick: TNotifyEvent read FOnListPick write FOnListPick;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseExit: TNotifyEvent read FOnMouseExit write FOnMouseExit;
  end;

{$ENDIF}

procedure Register;

implementation

{$IFDEF VER3UP}
resourcestring
{$ELSE}
const
{$ENDIF}
{$IFNDEF msgAsEngList}
  msgNoFloatValue = 'Pole neobsahuje platné FP èíslo';
  msgNoIntegerValue = 'Pole neobsahuje platné èíslo';
  msgFieldRequired = 'Pole je požadováno vyplnit';
  msgMissingLBracket = 'Chybí "("';
  msgMissingRBracket = 'Chybí ")"';
  msgSyntaxError = 'Chyba syntaxe výrazu';
  msgUnknownFnc = 'Neznamá funkce';
  msgInvalidOperation = 'Chybná operace';
  msgConvertValue = 'Chybné èíslo nebo operace na nìm';
  msgFieldInvalidate = 'Pole je chybné obsahem';
  StrExpectedBooleanValue = 'Vstupní hodnota není typu boolean!';
  StrExpectedIntegerValue = 'Vstupní hodnota není typu integer!';
  StrExpectedFloatValue = 'Vstupní hodnota není typu float!';
  StrInvalidMaskValue = 'Chybná hodnota pro maskování!';
  StrExpectedCurrencyValue = 'Vstupní hodnota není typu currency!';
  StrExpectedDateValue = 'Vstupní hodnota není typu datum!';
  StrExpectedDateTimeValue = 'Vstupní hodnota není typu datum a èas!';
  StrExpectedTimeValue = 'Vstupní hodnota není typu èas!';
  StrNoPasteFromClipboard = 'Nelze vložit ze schránky, text obsahuje nepovolené znaky.';
{$ELSE msgAsEngList}
  msgNoFloatValue = 'Field isn''t valid FP number';
  msgNoIntegerValue = 'Filed isn''t valid number';
  msgFieldRequired = 'Field required fill';
  msgMissingLBracket = 'Missing "("';
  msgMissingRBracket = 'Missing ")"';
  msgSyntaxError = 'Error expression syntax';
  msgUnknownFnc = 'Unknown function';
  msgInvalidOperation = 'Bad operation';
  msgConvertValue = 'Bad number or operation on it';
  msgFieldInvalidate = 'Field is bad';
  StrExpectedBooleanValue = 'Boolean type value expected!';
  StrExpectedIntegerValue = 'Integer type value expected!';
  StrExpectedFloatValue = 'Float type value expected!';
  StrInvalidMaskValue = 'Invalid masked value!';
  StrExpectedCurrencyValue = 'Currency type value expected!';
  StrExpectedDateValue = 'Date type value expected!';
  StrExpectedDateTimeValue = 'DateTime type value expected!';
  StrExpectedTimeValue = 'Time type value expected!';
  StrNoPasteFromClipboard = 'No paste, clipboard containt unexpected character(s).';
{$ENDIF msgAsEngList}

{$IFDEF CONDITIONALEXPRESSIONS}
   {$IF COMPILERVERSION <= 19.0}
     {$DEFINE NONUNICODE}
   {$IFEND}
{$ELSE}
  {$DEFINE NONUNICODE}
{$ENDIF}
{$IFDEF NONUNICODE}
function CharInSet(C: Char; const CharSet: TSysCharSet): Boolean;
begin
  Result := AnsiChar(C) in CharSet;
end;
{$ENDIF}

{$IFDEF VER80}

{artefact for Delphi 1}

function TrimLead(S: string): string;
  {-vraci zleva orezany retezec}
begin
  Result := S;
  while (Length(Result) > 0) and (Result[1] <= ' ') do Delete(Result, 1, 1);
end;

function TrimTrail(S: string): string;
  {-vraci zprava orezany retezec}
begin
  Result := S;
  while (Length(Result) > 0) and (Result[Length(Result)] <= ' ') do
    Delete(Result, Length(Result), 1);
end;

function Trim(S: string): string;
  {-vraci z obou stran orezany retezec}
begin
  Result := TrimLead(TrimTrail(S));
end;
  
{$ENDIF}

{  TColorOptionsSet  }

constructor TColorOptionsSet.Create;
begin
  inherited;
  ColorOnFocus := clYellow;
  ColorOnFocusFont := clBlack;
  ColorOnNotFocus := clWindow;
  ColorOnNotFocusFont := clBlack;
  ColorOnDisabled := clBtnFace;
  ColorOnDisabledFont := clGrayText;
end;

procedure TColorOptionsSet.Reset;
begin
  ColorOnFocus := clYellow;
  ColorOnFocusFont := clBlack;
  ColorOnNotFocusFont := clBlack;
end;

procedure TColorOptionsSet.Show(Value: Boolean);
begin
  if not Value then
  begin
   ColorOnFocus := clRed;
   ColorOnFocusFont := clWhite;
   ColorOnNotFocusFont := clRed;
  end
  else Reset;
end;

{  TPaintLineHintWindow  }

procedure TPaintLineHintWindow.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style and not ws_Border;
end;

procedure TPaintLineHintWindow.Paint;
begin
  inherited{$IFDEF VER80}Paint{$ENDIF}; {draw the original hint}
  with Canvas do
  begin {then, draw a new border}
    Pen.Width := 1;
    Pen.Color := clBtnShadow; {clBlack is the windows default;}
    Polyline([Point(0, 0), Point(ClipRect.Right - 1, 0),
      Point(ClipRect.Right - 1, ClipRect.Bottom - 1),
        Point(0, ClipRect.Bottom - 1), Point(0, 0)]);
  end;
end;

{ TButtonOptionsSet }

{$IfDef UseButtons}

constructor TButtonOptionsSet.Create;
begin
  inherited;
  FButton := nil
end;

destructor TButtonOptionsSet.Destroy;
begin
  if AsSigned(FButton) then
    FButton.Free;
  inherited;
end;

procedure TButtonOptionsSet.CreateButton;
begin
  FButton := {$IFDEF USE_RXLIB}TRxSpeedButton{$ELSE}TSpeedButton{$ENDIF}.Create(nil);
  FButton.Parent := FParent;
  FButton.Align := alRight;
  FButton.Caption := '';
  FButton.Height := FParent.Height - 2;
  FButton.Width := FButton.Height;
  FButton.Transparent := False;
  FButton.Layout := blGlyphLeft;
  FButton.ShowHint := False;
  FButton.Glyph := nil;
  FButton.Visible := False;
  FButton.Enabled := False;
end;

function TButtonOptionsSet.GetButtonCaption: string;
begin
  Result := '';
  if Assigned(FButton) then
    Result := FButton.Caption;
end;

function TButtonOptionsSet.GetButtonEnabled: Boolean;
begin
  Result := False;
  if Assigned(FButton) then
    Result := FButton.Enabled;
end;

function TButtonOptionsSet.GetButtonGlyph: TBitmap;
begin
  Result := nil;
  if Assigned(FButton) then
    Result := FButton.Glyph;
end;

function TButtonOptionsSet.GetButtonHint: string;
begin
  Result := '';
  if Assigned(FButton) then
    Result := FButton.Hint;
end;

{$IFDEF USE_RXLIB}
function TButtonOptionsSet.GetButtonMenu: TPopupMenu;
begin
  Result := nil;
  if Assigned(FButton) then
    Result := FButton.DropDownMenu;
end;
{$ENDIF}

function TButtonOptionsSet.GetButtonVisible: Boolean;
begin
  Result := False;
  if Assigned(FButton) then
    Result := FButton.Visible;
end;

function TButtonOptionsSet.GetButtonWidth: Integer;
begin
  Result := 19;
  if Assigned(FButton) then
    Result := FButton.Width;
end;

{$IFDEF USE_RXLIB}
function TButtonOptionsSet.GetNumGlyphs: Integer;
begin
  Result := 0;
  if Assigned(FButton) then
    Result := FButton.NumGlyphs;
end;
{$ENDIF}

procedure TButtonOptionsSet.SetButtonCaption(const Value: string);
begin
  if FButton = nil then CreateButton;
  FButton.Caption := Value;
end;

procedure TButtonOptionsSet.SetButtonEnabled(const Value: Boolean);
begin
  if FButton = nil then CreateButton;
  if FButton.Enabled <> Value then
    FButton.Enabled := Value
end;

procedure TButtonOptionsSet.SetButtonGlyph(const Value: TBitmap);
begin
  if FButton = nil then CreateButton;
  FButton.Glyph := Value;
end;

procedure TButtonOptionsSet.SetButtonHint(const Value: string);
begin
  if FButton = nil then CreateButton;
  FButton.Hint := Value;
  FButton.ShowHint := Value <> '';
end;

procedure TButtonOptionsSet.SetButtonVisible(const Value: Boolean);
begin
  if FButton = nil then CreateButton;
  FButton.Visible := Value;
  if FButton.Visible then
  begin
    UpdateFormatRect;
    FParent.Perform(EM_SETMARGINS, EC_RIGHTMARGIN, (FButton.Width + 2) shl 16);
    FButton.Invalidate;
  end;
  FParent.Invalidate;
end;

procedure TButtonOptionsSet.SetButtonWidth(const Value: Integer);
begin
  if FButton = nil then CreateButton;
  FButton.Width := Value;
  if FButton.Visible then
  begin
    {$IFDEF USE_RXLIB}
    ///if HandleAllocated then RecreateWnd;
    {$ENDIF}
    UpdateFormatRect;
    FParent.Perform(EM_SETMARGINS, EC_RIGHTMARGIN, (FButton.Width + 2) shl 16);
  end;
  FParent.Invalidate;
end;

{$IFDEF USE_RXLIB}
procedure TButtonOptionsSet.SetButtonMenu(const Value: TPopupMenu);
begin
  if FButton = nil then CreateButton;
  if FButton.DropDownMenu <> Value then
  begin
    FButton.DropDownMenu := Value;
    FButton.MarkDropDown := False;
  end;
end;

procedure TButtonOptionsSet.SetNumGlyphs(const Value: Integer);
begin
  if FButton = nil then CreateButton;
  if FButton.NumGlyphs <> Value then
    FButton.NumGlyphs := Value
end;
{$ENDIF}
procedure TButtonOptionsSet.UpdateFormatRect;
var
  Rect: TRect;
begin
  Rect := FParent.ClientRect;
  Dec(Rect.Right, FButton.Width);
  FParent.Perform(EM_SETRECTNP, 0, {$IFDEF CLR}Rect{$ELSE}LongInt(@Rect){$ENDIF});
end;

function TButtonOptionsSet.getButtonExists: Boolean;
begin
  Result := False;
  if (FButton <> Nil) then
    if AsSigned(ButtonGlyph) Then
      Result := True;
end;

procedure TButtonOptionsSet.setButtonExists(const Value: Boolean);
begin
  if value then
  begin
    if FButton = nil then CreateButton
  end
  else
    {$IFNDEF VER5UP}
    begin
      FButton.Free;
      FButton := nil;
    end;
    {$ELSE}
    FreeAndNil(FButton)
    {$ENDIF}
end;
{$EndIf UseButtons}

{ TAutoCompleteText }

{$IfDef UseAutocomplete}
constructor TAutoCompleteText.Create;
begin
  inherited;
  FAutocompleteItems := TStringList.Create;
  FAutocompleteItems.Sorted := True;
  FAutocompleteItems.Duplicates := dupIgnore;
  FAutocomplete := False;
end;

destructor TAutoCompleteText.Destroy;
begin
  FAutocompleteItems.Free;
  inherited;
end;

function TAutoCompleteText.GetAutoComplete: Boolean;
begin
  Result := FAutocomplete
end;

procedure TAutoCompleteText.SetAutoComplete(const Value: Boolean);
begin
  if Value <> FAutocomplete then
    FAutocomplete := Value;
end;

function TAutoCompleteText.Test(const incompletetxt: string;
  var completetxt: string): Boolean;
var
  I: Integer;
begin
  Result := False;
  completetxt := '';
  if (incompletetxt = '') or (FAutocompleteItems.Count = 0) then Exit;
  for I := 0 to FAutocompleteItems.Count - 1 do
  begin
    if Pos(AnsiLowerCase(incompletetxt), AnsiLowerCase(FAutocompleteItems[I])) = 1 then
    begin
      completetxt := FAutocompleteItems[I];
      Result := True;
      Exit;
    end;
  end;
end;
{$EndIf UseAutocomplete}

{ TInsideLabel }

constructor TInsideLabel.Create;
begin
  inherited;
  FActive := False;
  FColor := clDkGray;
  FCaption := '';
end;

procedure TInsideLabel.SetCaption(const Value: TCaption);
begin
  FCaption := Value;
  if FActive and (Value = '') then
    FActive := False;
end;


{  TPubEdit  }

procedure TPubEdit.CreateParams(var Params: TCreateParams);
const
  Alignments: array[TAlignment] of Cardinal{Longint} = (ES_LEFT, ES_RIGHT, ES_CENTER);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or Alignments[FAlignment];
end;

constructor TPubEdit.Create(AOwner: TComponent);
var
  TheForm: TComponent; {The form that ultimately owns this component}
begin
  inherited Create(AOwner);
  FInsideLabel := TInsideLabel.Create;
  FColorOptionsSet := TColorOptionsSet.Create;
  Color := ColorOptionsSet.ColorOnNotFocus;
  FTypeEdit := teString;
  FCapitalize := False;
  FRequired := False;
  FValidChars := '';
  FMask := '';
  FAlignMent := taLeftJustify;
  FErrMsg := '';
  {no button created}
  {$IfDef UseButtons}
  FBtnSet1 := TButtonOptionsSet.Create;
  FBtnSet1.FParent := Self;
  FBtnSet1.FShortCut := Button1ShortCutInvoke;
  FBtnSet2 := TButtonOptionsSet.Create;
  FBtnSet2.FParent := Self;
  FBtnSet2.FShortCut := Button2ShortCutInvoke;
  {$EndIf}
  {autocomplete feature}
  {$IfDef UseAutocomplete}
  FAutocomplete := TAutocompleteText.Create;
  {$EndIf}
  { start hint window functionality }
  HintWin := TPaintLineHintWindow.Create(Self);
  HintWin.Color := {$IFDEF VER80}clYellow{$ELSE}clInfoBk{$ENDIF};
  HintWin.Canvas.Font.Color := {$IFDEF VER80}clBlack{$ELSE}clInfoText{$ENDIF};
  {Check if owner is a form, if not, move up tree of owners until we either find a form or a nil reference}
  TheForm := AOwner;
  while (TheForm <> nil) and not (TheForm is TForm) do TheForm := TheForm.Owner;
  {Record form window reference appropriately}
  if TheForm <> nil then FWindow := TheForm as TForm else FWindow := nil;
end;

procedure TPubEdit.DefaultHandler(var message);
var
  MsgT: TMsg;
begin
  {convert message to TMsg}
  MsgT.message := TMessage(message).Msg;
  inherited{$IFDEF VER80}DefaultHandler(message){$ENDIF};
  if HintWin = nil then Exit;
  {if it's WM_Mousemove then we don't want to hide the hint window}
  if (HintWin.IsHintMsg(MsgT) and (MsgT.message <> WM_MouseMove)) then
    ReleaseIt;
end;

destructor TPubEdit.Destroy;
begin
  {$IfDef UseButtons}
  FBtnSet1.Free;
  FBtnSet2.Free;
  {$EndIf}
  {$IfDef UseAutocomplete}
  FAutocomplete.Free;
  {$EndIf}
  FColorOptionsSet.Free;
  FInsideLabel.Free;
  inherited Destroy;
end;

procedure TPubEdit.SetAlignment(Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    RecreateWnd;
  end;
end;

procedure TPubEdit.SetRequired(Value: Boolean);
begin
  if Value <> FRequired then
    FRequired := Value;
end;

function TPubEdit.Validated: Boolean;
var
  S, SZ: string;
  CanExit: Boolean;
  strmsg: string;
  i: Integer;
begin
  Result := True; //je to OK
  S := Text;
  if AsSigned(FMaskForm) then
    FMaskForm(Self, eValueToText, Mask, S); //odmaskuj holou hodnotu
  SZ := S;
  if FValidChars <> '' then
  begin
    for i := Length(S) downto 1 do
      if Pos(S[i], FValidChars) = 0 then
        Delete(S,i,1);
    if Length(S) <> Length(SZ) then
    begin
      Result := False;
      Exit; //containt some chars up length - it is wrong
    end;
  end;

  if Assigned(FValidate) then
  begin //uzivatelska validace
    FValidate(Self, S, CanExit);{Vlastni validace}
    {Oprava textu zde neni!}
    {A pri chybe zahlas}
    if not CanExit then
      Result := False
    else
    begin
      Text := S;
      if AsSigned(FMaskForm) then
        FMaskForm(Self, eTextToValue, Mask, S);
    end;
    {pokud je pole vyzadovane, musi byt neprazdne}
    if FRequired and (S = '') then
    begin
        Result := False;
    end;
  end
  else
  begin {zabudovane validace}
    Result := DoValidate(strmsg)
  end;
end;

function TPubEdit.Validator: Boolean;
  {uzivatelsky validator}
  {napr. pro vsechny edity se vola ve smycce a oznacuji se nevalidni pole}
  {
  for I := 0 to ComponentCount - 1 do begin
    if Components[I] is TPubEdit then
      if not (Components[I] as TPubEdit).DoValidate then begin
        //udelej jeste neco pro konkretni policko
      end;
  end;
  }
  {Volani funkce}
  {1. v onexit prislusneho pole}
  {--validuje se pouze jednine pole}
  {2. ve validacni procedure pro celou stranku/form/frame}
  {--validuji se vsechna pole stejneho typu (ve vysledku muse byt oznaceno vice chybnych poli)}
var
  S: string;
begin
  S := Text;
  if AsSigned(FMaskForm) then
    FMaskForm(Self, eValueToText, Mask, S); //odmaskuj holou hodnotu
  if Assigned(FValidate) then
    FValidate(Self, S, Result)
  else
    Result := ValidityCheck(Text, FErrMsg);

  ColorOptionsSet.Show(Result);
end;

procedure TPubEdit.SetCapitalize(Value: Boolean);
begin
  if Value <> FCapitalize then
    FCapitalize := Value;
end;

procedure TPubEdit.SetAsInteger(Value: Integer);
begin
  Text := IntToStr(Value)
end;

function TPubEdit.GetAsInteger: Integer;
begin
  Result := 0;
  if Text = '' then
    Exit;
  try
    Result := StrToInt(Text);
  except
    raise EInvalidTypeConvert.Create(StrExpectedIntegerValue);
  end
end;

procedure TPubEdit.SetAsFloat(Value: Real);
begin
  Text := FloatToStr(Value);
end;

function TPubEdit.GetAsFloat: Real;
begin
  Result := 0;
  if Text = '' then
    Exit;
  try
    Result := StrToFloat(Text);
  except
    raise EInvalidTypeConvert.Create(StrExpectedFloatValue);
  end
end;

procedure TPubEdit.SetAsAnsiString(const Value: AnsiString);
begin
  SetAsString(string(Value));
end;

procedure TPubEdit.SetAsBoolean(const Value: Boolean);
begin
  if Value then
    Text := ccBoolDefYes
  else
    Text := ccBoolDefNo
end;

procedure TPubEdit.SetAsCurrency(const Value: Currency);
begin
  Text := FloatToStr(Value);
end;

procedure TPubEdit.SetAsDate(Value: TDate);
begin
  Text := DateToStr(Value);
end;

function TPubEdit.GetAsAnsiString: AnsiString;
begin
  Result := AnsiString(GetAsString)
end;

function TPubEdit.GetAsBoolean: Boolean;
begin
  Result := False;
  if Text = '' then
    Exit;
  if (Text = ccBoolDefYes) or (Text = ccBoolDefNo) then
    Result := Text = ccBoolDefYes
  else
    raise EInvalidTypeConvert.Create(StrExpectedBooleanValue);
end;

function TPubEdit.GetAsCurrency: Currency;
begin
  Result := 0;
  if Text = '' then
    Exit;
  try
    Result := StrToFloat(Text);
  except
    raise EInvalidTypeConvert.Create(StrExpectedCurrencyValue);
  end
end;

procedure TPubEdit.SetAsDateTime(const Value: TDateTime);
begin
  Text := DateTimeToStr(Value);
end;

function TPubEdit.GetAsDate: TDate;
begin
  Result := SysUtils.Date;
  if Text = '' then
    Exit;
  try
    Result := StrToDate(Text);
  except
    raise EInvalidTypeConvert.Create(StrExpectedDateValue);
  end
end;

function TPubEdit.GetAsDateTime: TDateTime;
begin
  Result := SysUtils.Now;
  if Text = '' then
    Exit;
  try
    Result := StrToDateTime(Text);
  except
    raise EInvalidTypeConvert.Create(StrExpectedDateTimeValue);
  end
end;

procedure TPubEdit.SetAsTime(Value: TTime);
begin
  Text := TimeToStr(Value);
end;

{funkce z jbStr}
function LeftPadCh(S: string; CH: Char; Len: Integer): string;
  {-vraci zleva znakem ch zarovnany retezec v delce len}
var
  I: Integer;
begin
  Result := S;
  if Length(S) < Len then for I := Length(S) + 1 to Len do Result := CH + Result
end;

procedure TPubEdit.SetAsZeroPaddedString(const Value: string);
begin
  Text := LeftPadCh(Value, '0', MaxLength);
end;

function TPubEdit.GetAsTime: TTime;
begin
  Result := SysUtils.Time;
  if Text = '' then
    Exit;
  try
    Result := StrToTime(Text);
  except
    raise EInvalidTypeConvert.Create(StrExpectedTimeValue);
  end
end;

function TPubEdit.GetAsZeroPaddedString: string;
begin
  Result := Text;
  if Result = '' then
    Exit;
  try
    if AsSigned(FMaskForm) then
      FMaskForm(Self, eValueToText, FMask, Result) //odmaskuj holou hodnotu
    else
      Result := Text;

    Result := LeftPadCh(Result, '0', MaxLength);
  except
    Result := Text; //do nothing
  end
end;

procedure TPubEdit.SetAsString(Value: string);
var
  S: string;
begin
  S := Value;
  if AsSigned(FMaskForm) then
    FMaskForm(Self, eTextToValue, FMask, S); //odmaskuj holou hodnotu
  Text := S;
end;

function TPubEdit.GetAsString: string;
begin
  Result := Text;
  if Result = '' then
    Exit;
  try
    if AsSigned(FMaskForm) then
      FMaskForm(Self, eValueToText, FMask, Result) //odmaskuj holou hodnotu
    else
      Result := Text;
  except
    Result := Text; //do nothing
  end
end;

const
  ccControlKeys = [#8 {vk_TAB}, #22 {ctrl+V}, #3 {ctrl+C}, #24 {ctrl+X}];

procedure TPubEdit.KeyPress(var Key: Char);
var
  C: string;
  {$IFNDEF CLR}
  VCH: {$IFDEF UNICODE}TSysCharSet{$ELSE}set of Char{$ENDIF};
  //I: Integer;
  {$ELSE}
  procedure Check(VCH: string);
  begin
    if (not CharInSet(Key, ccControlKeys)) and (VCH <> '') then
      if Pos(Key, VCH) = 0 then Key := #0; {Delej, jako by nebyla}
  end;
  {$ENDIF}
begin
  case FTypeEdit of
    teString:
      begin
        {$IFDEF CLR}
        //Check(FValidChars);
        if Length(FValidChars) > 0 then
          if (not CharInSet(Key, ccControlKeys)) then
            if Pos(Key, FValidChars) <= 0 then
              Key := #0; {Delej, jako by nebyla}
        {$ELSE}
        {ohodnot}
        if (not CharInSet(Key, ccControlKeys)) and (FValidChars <> '') then
          if Pos(Key, FValidChars) = 0 then Key := #0; {Delej, jako by nebyla}
        {$ENDIF}
      end;

    {pro vstup cisel se predpoklada interne jina maska}
    teInteg:
      begin
        {$IFDEF CLR}
        //Check('0123456789+-');
        if (not CharInSet(Key, ccControlKeys)) then
          if not CharInSet(Key, ['0'..'9', '+', '-']) then
        {$ELSE}
        {nejprve vyrob mnozinu znaku}
        VCH := ['0'..'9', '+', '-'];
        {ohodnot}
        if (not CharInSet(Key, ccControlKeys)) and (VCH <> []) then
          if not CharInSet(Key, VCH) then
        {$ENDIF}
            Key := #0; {Delej, jako by nebyla}
      end;

    {pro vstup cisel se predpoklada interne jina maska}
    teFloat, teCurrency:
      begin
        {$IFDEF CLR}
        //Check('0123456789+-eE' + SysUtils.DecimalSeparator);
        if (not CharInSet(Key, ccControlKeys)) then
          if not CharInSet(Key, ['0'..'9', '+', '-', 'e', 'E']) or (Key <> SysUtils.DecimalSeparator) then
        {$ELSE}
        {nejprve vyrob mnozinu znaku}
        VCH := ['0'..'9', '+', '-', 'e', 'E'] + [{$IFDEF VER15UP}FormatSettings.{$ENDIF}DecimalSeparator];
        if {$IFDEF VER15UP}FormatSettings.{$ENDIF}ThousandSeparator <> #0 then
          VCH := VCH + [{$IFDEF VER15UP}FormatSettings.{$ENDIF}ThousandSeparator];
        {ohodnot}
        if (not CharInSet(Key, ccControlKeys)) and (VCH <> []) then
          if not CharInSet(Key, VCH) then
        {$ENDIF}
            Key := #0; {Delej, jako by nebyla}
      end;

    teTime:
      begin
        {$IFDEF CLR}
        //Check('0123456789' + SysUtils.TimeSeparator);
        if (not CharInSet(Key, ccControlKeys)) then
          if not CharInSet(Key, ['0'..'9']) or (Key <> SysUtils.TimeSeparator) then
        {$ELSE}
        VCH := ['0'..'9'] + [{$IFDEF VER15UP}FormatSettings.{$ENDIF}TimeSeparator];
        if (not CharInSet(Key, ccControlKeys)) and (VCH <> []) then
          if not CharInSet(Key, VCH) then
        {$ENDIF}
            Key := #0; {Delej, jako by nebyla}
      end;

    teDate:
      begin
        {$IFDEF CLR}
        //Check('0123456789' + SysUtils.DateSeparator);
        if (not CharInSet(Key, ccControlKeys)) then
          if not CharInSet(Key, ['0'..'9']) or (Key <> SysUtils.DateSeparator) then
        {$ELSE}
        VCH := ['0'..'9'] + [{$IFDEF VER15UP}FormatSettings.{$ENDIF}DateSeparator];
        if (not CharInSet(Key, ccControlKeys)) and (VCH <> []) then
          if not CharInSet(Key, VCH) then
        {$ENDIF}
            Key := #0; {Delej, jako by nebyla}
      end;

    teDateTime:
      {NOTE: Only DD.MM.YYYY HH:NN:SS suported}
      begin
        {$IFDEF CLR}
        //Check('0123456789' + SysUtils.DateSeparator + SysUtils.TimeSeparator);
        if (not CharInSet(Key, ccControlKeys)) then
          if not CharInSet(Key, ['0'..'9']) or (Key <> SysUtils.DateSeparator) or (Key <> SysUtils.TimeSeparator) then
        {$ELSE}
        VCH := ['0'..'9', ' '] + [{$IFDEF VER15UP}FormatSettings.{$ENDIF}DateSeparator] + [{$IFDEF VER15UP}FormatSettings.{$ENDIF}TimeSeparator];
        if (not CharInSet(Key, ccControlKeys)) and (VCH <> []) then
          if not CharInSet(Key, VCH) then
        {$ENDIF}
            Key := #0; {Delej, jako by nebyla}
      end;

  end {case};
  {jsou-li pozadovana velka pismena na zacatku slov, udelej to tady}
  if FCapitalize then
    if (SelStart = 0) or (Text[SelStart] = ' ') then
    begin
      C := Key;
      C := AnsiUpperCase(C);
      Key := C[1];
    end;

  if Key <> #0 then
    inherited KeyPress(Key);
end;

procedure TPubEdit.DoEnter;
var S: string;
begin
  {vybarvi pole pri zamereni}
  Color := ColorOptionsSet.ColorOnFocus;
  Font.Color := ColorOptionsSet.ColorOnFocusFont;

  case FTypeEdit of
    {je-li pozadovano prevedeni a jedna-li se o integ cislo}
    teInteg:
      if AsSigned(FConvert) then
      begin {je-li prislusna rutina}
        S := Text;
        FConvert(Self, eValueToText, S); {preved text na jiny format}
        Text := S;
      end;

    {je-li potreba zaformatovat podle masky, pak jenom pro string}
    teString:
      if AsSigned(FMaskForm) then
      begin
        S := Text;
        FMaskForm(Self, eValueToText, FMask, S);
        Text := S;
      end;

  end {case};

  if not AutoSelect then
    if Text = '' then SelStart := 0
    else SelStart := GetTextLen;

  if Assigned(FOnEnter) then
    FOnEnter(Self);
end;

function TPubEdit.ValidityCheck(Value: string; var ErrMsg: string): Boolean;
var
  S: string;
begin
  ErrMsg := ''; //no errors occurred
  S := Value;
  if S <> '' then
    case FTypeEdit of
      teInteg:
        begin
          if not Self.Focused then
          begin
            if AsSigned(FConvert) then  //deconvert -like- Binary to Integer
              FConvert(Self, eValueToText, S);
          end;
          {je-li ocekavano cislo, pak to cislo byt musi}
          try
            AsInteger; {predpokladam Longint}
          except
            on EInvalidTypeConvert do
            begin
              ErrMsg := StrExpectedIntegerValue;
            end;
          end;
        end;
      teCurrency:
        begin
          try
            AsFloat
          except
            on EInvalidTypeConvert do
            begin
              ErrMsg := StrExpectedCurrencyValue;
            end;
          end;
        end;
      teFloat:
        begin
          {je-li ocekavano cislo, pak to cislo byt musi}
          try
            AsFloat; {nejaky float}
          except
            on EInvalidTypeConvert do
            begin
              ErrMsg := StrExpectedFloatValue;
            end;
          end;
        end;
      teString:
        begin
          if AsSigned(FMaskForm) then
          try
            {je-li maska (ale i bez ni), pak formatuj externi rutinou}
            FMaskForm(Self, eTextToValue, FMask, S); //<- primo neformatuje viz funkcnost v onexit
          except
            ErrMsg := StrInvalidMaskValue;
          end;
        end;
      teDate:
        if Text <> '' then
        begin
          {je-li ocekavano cislo, pak to cislo byt musi}
          try
            AsDate; {nejaky float}
          except
            on EInvalidTypeConvert do
            begin
              ErrMsg := StrExpectedDateValue;
            end;
          end;
        end;
      teTime:
        if Text <> '' then
        begin
          {je-li ocekavano cislo, pak to cislo byt musi}
          try
            AsTime; {nejaky float}
          except
            on EInvalidTypeConvert do
            begin
              ErrMsg := StrExpectedTimeValue;
            end;
          end;
        end;
      teDateTime:
        if Text <> '' then
        begin
          {je-li ocekavano cislo, pak to cislo byt musi}
          try
            AsDateTime; {nejaky float}
          except
            on EInvalidTypeConvert do
            begin
              ErrMsg := StrExpectedDateTimeValue;
            end;
          end;
        end;
    end {case};

  {teprve po nem znovu overuji, zda je pole vyplneno dle pozadavku}
  if FRequired and (S = '') then
  begin
    if ErrMsg <> '' then ErrMsg := ErrMsg + #13 + msgFieldRequired
    else ErrMsg := msgFieldRequired;
  end;

  Result :=  ErrMsg <> '';
end;

function TPubEdit.DoValidate(var oErrMsg: string): Boolean;
begin
  Result := ValidityCheck(Text, oErrMsg);
  FErrMsg := oErrMsg;
  //ColorOptionsSet.Show(Result); --nelze zde, vyjimka
end;

procedure TPubEdit.DoExit;
var
  S: string;
  Err: Boolean;
begin
  {tady zmen barvu pole, kdyz ztracis zamereni}
  Color := ColorOptionsSet.ColorOnNotFocus;
  Font.Color := ColorOptionsSet.ColorOnNotFocusFont;
  Self.Repaint; //Self.Invalidate;
  Err := ValidityCheck(Text, S);
  if Err then
  begin
    {uzivatelska reakce na chybu}
    if S <> msgFieldRequired then {pozadavek na vyplneni je zobrazen pozdeji}
      if AsSigned(FErrorOccur) then
        FErrorOccur(Self, S)
      else
        MessageDlg(S, mtError, [mbOK], 0);
    {vybrani chybneho pole}
    if Self.CanFocus then Self.SetFocus;
  end
  else
  begin
    case FTypeEdit of
      teString:
        if Text <> '' then
        begin
          {je-li maska (ale i bez ni), pak formatuj externi rutinou}
          if AsSigned(FMaskForm) then
          begin
            S := Text;
            FMaskForm(Self, eValueToText, FMask, S); {zbav se pripadne masky}
            FMaskForm(Self, eTextToValue, FMask, S); {znovu zamaskuj}
            Text := S;
          end;
        end;
      teInteg:
        begin
          {je-li pozadovano prevedeni a jedna-li se o integ cislo}
          if Assigned(FConvert) then
          begin {je-li prislusna rutina}
            S := Text;
            FConvert(Self, eTextToValue, S); {preved text na jiny format}
            Text := S;
          end;
        end;
    end;
  end;
  {tohle je vlastni onexit}
  if Assigned(FOnExit) then FOnExit(Self);

  {teprve po nem znovu overuji, zda je pole vyplneno dle pozadavku}
  if FRequired and (Text = '') then
  begin
    if AsSigned(FErrorOccur) then
      {uzivatelska reakce na chybu}
      FErrorOccur(Self, msgFieldRequired)
    else
      MessageDlg(msgFieldRequired, mtWarning, [mbOK], 0);
    if CanFocus then SetFocus;
  end;

end;

procedure TPubEdit.CMExit(var Message: TCMExit);
var
  CanExit: Boolean;
  S: string;
  i: Integer;
begin
  inherited;
  if FTypeEdit <> teParser then
  begin
    {vyfiltrovani vlozenych znaku, na nepovolene zde}
    if FValidChars <> '' then
    begin
      S := Text;
      for i := Length(S) downto 1 do
        if Pos(S[i], FValidChars) = 0 then
          Delete(S,i,1);
      if Length(S) <> Length(Text) then
        Text := S; {oprave zde}
    end;
  end;
  {je-li pozadovano polozku vyplnit a ona je prazdna, pak znovu}
  {tady zadnou hlasku nezobrazuji}
  if FRequired and (Text = '') then
  begin
    MessageBeep($FFFF);
    if Self.CanFocus then Self.SetFocus;
    Exit;
  end;
  {je-li pozadovana externi validace a neprojde, pak taky znovu}
  if FTypeEdit <> teParser then
  begin
    {vlastni validace}
    if Assigned(FValidate) then
    begin
      S := Text; {<-- text z editu} {zmena maskovani 01.09.1999 J.B.}
      {pole nejlepe bez masek}
      if AsSigned(FMaskForm) then FMaskForm(Self, eValueToText, Mask, S);
      try
        {Vlastni validace}
        FValidate(Self, S, CanExit);
      finally {fixed 23.10.2001 J.B.}
        if AsSigned(FMaskForm) then FMaskForm(Self, eTextToValue, Mask, S);
      end;
      {Oprava textu zde}
      if Text <> S then {27.7.2001 J.B.}
        Text := S;
      {pri chybe zpet}
      if not CanExit then
      begin
        MessageBeep($FFFF); {pri chybe jenom pipni}
        (*If AsSigned(FErrorOccur) Then
          {uzivatelska reakce na chybu}
          FErrorOccur(Self,msgFieldInvalidate)
        Else
          MessageDlg(msgFieldInvalidate,mtWarning, [mbOK],0);*)
        if Self.CanFocus then Self.SetFocus;
        Exit;
      end;
    end;
  end
  else
  begin
    {pro parser je validace extra zde}
    if not Assigned(FParsing) then
      Text := Parsed(Text)
    else
    begin
      {external parsing pointed there}
      FParsing(Self, Text, S);
      if Text <> S then
        Text := S;
    end;
  end;
  {pokud je uschovavana historie pole, pak zde}
{$IfDef UseAutocomplete}
  if FAutocomplete.FStoreHistory then
  begin
    FAutocomplete.FAutoCompleteItems.Add(Text)
  end;
{$EndIf}
end;

procedure TPubEdit.CMEnter(var Message: TCMEnter);
begin
  inherited;
end;

procedure TPubEdit.CMMouseEnter(var message: TMessage);
begin
  inherited;
  if Assigned(FOnMouseEnter) then FOnMouseEnter(Self);
end;

procedure TPubEdit.CMMouseLeave(var message: TMessage);
begin
  inherited;
  ReleaseIt;
  if Assigned(FOnMouseExit) then FOnMouseExit(Self);
end;

procedure TPubEdit.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  {buttons enablity}
{$IfDef UseButtons}
  with FBtnSet1 do
    if AsSigned(FButton) then
      if ButtonVisible then
        Enabled := Self.Enabled;

  with FBtnSet2 do
    if AsSigned(FButton) then
      if ButtonVisible then
        Enabled := Self.Enabled;
{$EndIf}
  {obzvlast zajimave, nastavuji barvy :-( }
  if Enabled then
    if Focused then
    begin
      Color := ColorOptionsSet.ColorOnFocus;
      Font.Color := ColorOptionsSet.ColorOnFocusFont;
    end
    else
    begin
      Color := ColorOptionsSet.ColorOnNotFocus;
      Font.Color := ColorOptionsSet.ColorOnNotFocusFont;
    end
  else
  begin
    Color := ColorOptionsSet.ColorOnDisabled;
    Font.Color := ColorOptionsSet.ColorOnDisabledFont;
  end
end;

procedure TPubEdit.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if FSelectAll and FWider and Focused then
  begin
    SelectAll;
    FSelectAll := False;
  end;
  inherited{$IFDEF VER80}MouseUp(Button, Shift, X, Y){$ENDIF};
end;

{-----------------------------------------------------------------------------}

procedure TPubEdit.MouseMove(Shift: TShiftState; X, Y: Integer);
{ Since this was originally coded for a TListBox, where it makes sense being
  this way, and here it was adapted for a TEdit component, I think this could
  be better off inside the OnEnter event, but, since I'm naturally born lazy,
  I made only the minor modifications needed.  Someone can surely do a better
  job... }
var
  Rct: TRect;
begin
  if FWider and not Focused then
  begin
    FSelectAll := True;
    FWider := False;
  end;

  if not (FHintText) then
  begin {Item hinting is off}
    ReleaseIt;
    inherited{$IFDEF VER80}MouseMove(Shift, X, Y){$ENDIF};
    Exit;
  end;

  { Check if there is an owning window - get out if not}
  { If HintEdit is focused, do not show the tooltip to let user edit it}
  { Also, if Application has no focus, why should we care about showing a tip?}
  if Focused or (FWindow = nil) or (Trim(Text) = '') or
    not Application.Active
  then
    Exit;

  { the line that follows is important, otherwise the next line will calculate}
  { the wrong TextWidth via FWindows' Canvas - Thanks Peter Below (TeamB),}
  { for this handy tip that I found in Borland Newsgroup}
  FWindow.Canvas.Font := Font;
  { Check if the Text is wider then the Width}
  if FWindow.Canvas.TextWidth(Text) > Width - 6 then
  begin
    FWider := True;
    if FShowing then Exit;
    { Get the default item coordinates}
    Rct := ClientRect;
    { Stretch it to fit the whole edit text }
    Rct.Right := Rct.Left + FWindow.Canvas.TextWidth(Text) + 5;
    { Fine tune theses values for appearance }
    Rct.Top := Rct.Top - 1; {2}
    Rct.Bottom := Rct.Bottom - 4; {2}
    Rct.Left := Rct.Left - 1; {2}
    { now convert to screen coordinates so that THintWindow can use them}
    Rct.TopLeft := ClientToScreen(Rct.TopLeft);
    Rct.BottomRight := ClientToScreen(Rct.BottomRight);
    { And show it!}
    HintWin.ActivateHint(Rct, Text);
    FShowing := True;
  end
  else
    ReleaseIt;

  inherited{$IFDEF VER80}MouseMove(Shift, X, Y){$ENDIF};
end;

procedure TPubEdit.ReleaseIt;
begin
  HintWin.ReleaseHandle;
  FShowing := False;
end;
{$IFNDEF  VER9UP}const EnableOpcode: string = ('+-*/^');{$ENDIF}
type
  {  TEvalParser  }

  TEvalParser = class
  private
    {$IFDEF  VER9UP}const EnableOpcode: string = ('+-*/^');{$ENDIF}
    procedure MatchBracket(var I: Integer; S: string); {$IFDEF SUPP_INL}inline;{$ENDIF}
    function GetValue(S: string): Extended; {$IFDEF SUPP_INL}inline;{$ENDIF}
    function SpecialFn(p1: Integer; S: string): Extended; {$IFDEF SUPP_INL}inline;{$ENDIF}
    function Calculate(p1: Integer; S: string): Extended; {$IFDEF SUPP_INL}inline;{$ENDIF}
    function GetFirstOpp(tot: Integer; S: string): Integer; {$IFDEF SUPP_INL}inline;{$ENDIF}
  public
    function Evaluate(S: string): Extended; {$IFDEF SUPP_INL}inline;{$ENDIF}
    procedure CleanUp(var S: string); {$IFDEF SUPP_INL}inline;{$ENDIF}
  end;

{  TEvalParser  }

procedure TEvalParser.MatchBracket(var I: Integer; S: string);
var
  J, len: Integer;
begin
  if (Pos('(', S) = 0) and (Pos(')', S) = 0) then Exit;
  J := 1;
  len := Length(S);
  repeat
    Inc(i);
    if i > len then
      raise EvaluatorError.Create(msgMissingRBracket);
    if S[i] = '(' then Inc(j);
    if S[i] = ')' then Dec(j);
    if j < 0 then
      raise EvaluatorError.Create(msgMissingLBracket);
  until j = 0;
end;

function TEvalParser.GetValue(S: string): Extended;
begin
  if Length(S) < 1 then
    raise EvaluatorError.Create(msgSyntaxError);
  try
    if Length(S) = 1 then
      Result := StrToFloat(S)
    else
      Result := StrToFloat(S);
  except
    on EConvertError do
    begin
      raise EvaluatorError.Create(msgNoFloatValue);
    end;
  end;
end;

function TEvalParser.SpecialFn(p1: Integer; S: string): Extended;
var
  operstr: string;
  arg: Extended;
begin
  operstr := Copy(S, 1, p1 - 1);
  if S[Length(S)] <> ')' then
    EvaluatorError.CreateFmt(msgSyntaxError + ' %s', [S]);
  arg := Evaluate(Copy(S, p1 + 1, Length(S) - p1 - 1));
  if AnsiCompareText(operstr, 'sin') = 0 then Result := Sin(arg)
  else
    if AnsiCompareText(operstr, 'cos') = 0 then Result := Cos(arg)
    else
      if AnsiCompareText(operstr, 'tan') = 0 then Result := Sin(arg) / Cos(arg)
      else
        if AnsiCompareText(operstr, 'arctan') = 0 then Result := ArcTan(arg)
        else
          if AnsiCompareText(operstr, 'log') = 0 then Result := Ln(arg) / Ln(10)
          else
            if AnsiCompareText(operstr, 'ln') = 0 then Result := Ln(arg)
            else
              if AnsiCompareText(operstr, 'exp') = 0 then Result := Exp(arg)
              else
                if AnsiCompareText(operstr, 'sqrt') = 0 then Result := Sqrt(arg)
                {tady se daji pridat dalsi funkce}
                else
                begin
                  raise EvaluatorError.CreateFmt(msgUnknownFnc + ' %s', [S]);
                end
end;

function TEvalParser.Calculate(p1: Integer; S: string): Extended;
var
  v1, v2: Extended;
begin
  v1 := Evaluate(Copy(S, 1, p1 - 1));
  v2 := Evaluate(Copy(S, p1 + 1, Length(S) - p1));
  case S[p1] of
    '+': Result := v1 + v2;
    '-': Result := v1 - v2;
    '/': Result := v1 / v2;
    '*': Result := v1 * v2;
    '^': Result := Exp(v2 * Ln(v1));
  else
    raise EparserError.CreateFmt(msgInvalidOperation + ' %s', [S]);
  end;
end;

function TEvalParser.GetFirstOpp(tot: Integer; S: string): Integer;
var
  i: Integer;
begin
  if tot = 0 then tot := Length(S);
  for i := 1 to 5 do
  begin
    Result := Pos(EnableOpcode[i], S);
    if ((i < 3) and (Result > 0)) then
      if ((Result = 1) or (Pos(S[Result - 1], EnableOpcode) > 0)) then Result := 0;
    if Result > 0 then
      if Result < tot then Exit;
  end;
  if Result > tot then Result := 0;
end;

function TEvalParser.Evaluate(S: string): Extended;
var
  p1, p2, q1: integer;
begin
  p1 := Pos('(', S);
  p2 := p1;
  if p2 > 0 then MatchBracket(p2, S);
  if p1 = 1 then
  begin
    if p2 = Length(S) then
    begin
      Delete(S, p2, 1);
      Delete(S, 1, 1);
      Result := Evaluate(S);
    end
    else
      Result := Calculate(p2 + 1, S);
    Exit;
  end;
  q1 := GetFirstOpp(p1, S);
  if (p1 + q1 = 0) then
  begin
    Result := GetValue(S);
    Exit;
  end;
  if q1 <> 0 then
    Result := Calculate(q1, S)
  else
    if Length(S) > p2 then
      Result := Calculate(p2 + 1, S)
    else
      Result := SpecialFn(p1, S);
end;

procedure TEvalParser.CleanUp(var S: string);
var i: integer;
begin
  S := LowerCase(S); {nepredpokladam cestinu :-))  }
  i := Pos(' ', S);
  while i > 0 do
  begin
    delete(S, i, 1);
    i := Pos(' ', S);
  end;
end;

function TPubEdit.Parsed(const S: string): string;
var
  X: string;
  EP: TEvalParser;
begin
  if S = '' then
  begin
    Result := '';
    Exit;
  end;
  X := S;
  EP := TEvalParser.Create;
  try
    try
      EP.CleanUp(X);
      Result := FloatToStr(EP.Evaluate(X));
    except
      on E: Exception do
      begin
        if AsSigned(FErrorOccur) then
           {uzivatelska reakce na chybu}
          FErrorOccur(Self, msgConvertValue + #13 + '(' + E.Message + ')')
        else
          MessageDlg(msgConvertValue + #13 + '(' + E.Message + ')', mtError, [mbOK], 0);
        Result := '<error>'
      end
    end;
  finally
    EP.Free;
  end;
end;

procedure TPubEdit.NextControl(YesNo: Boolean);
var
  FTempForm: 
  {$IFDEF CLR}TCustomForm
  {$ELSE}
    {$IFNDEF VER5UP}
    TForm
    {$ELSE}
    TCustomForm
    {$ENDIF}
  {$ENDIF};
begin

  if YesNo then
  begin
    FTempForm := GetParentForm(Self);
    SendMessage(FTempForm.Handle, WM_NEXTDLGCTL, 0, 0);
  end

end;

{$IfDef UseButtons}
procedure TPubEdit.CreateHandle;
begin
  inherited CreateHandle;
  with FBtnSet1 do
    if AsSigned(FButton) then
      if ButtonVisible then
        UpdateFormatRect;
  with FBtnSet2 do
    if AsSigned(FButton) then
      if ButtonVisible then
        UpdateFormatRect;
end;

procedure TPubEdit.WMSize(var Msg: TWMSize);
begin
  inherited;
  with FBtnSet1 do
    if AsSigned(FButton) then
      if ButtonVisible then
        UpdateFormatRect;
  with FBtnSet2 do
    if AsSigned(FButton) then
      if ButtonVisible then
        UpdateFormatRect;
end;

procedure TPubEdit.WMSetCursor(var Msg: TWMSetCursor);
var
  P: TPoint;
  bw: Integer;
begin
  bw := 0;
  with FBtnSet1 do
    if AsSigned(FButton) then
      if ButtonVisible then
        Inc(bw, ButtonWidth);
  with FBtnSet2 do
    if AsSigned(FButton) then
      if ButtonVisible then
        Inc(bw, ButtonWidth);
  if bw > 0 then
  begin
    GetCursorPos(P);
    P := ScreenToClient(P);
    if (P.X >= ClientWidth - bw) then
      SetCursor(Screen.Cursors[crDefault])
    else
      inherited;
  end
  else
    inherited;
end;

procedure TPubEdit.KeyDown(var Key: Word; Shift: TShiftState);
  procedure UpdateEditArea(AboutWidth: Integer);
  var
    Loc: TRect;
  begin
    SetRect(Loc, 0, 0, ClientWidth - aboutwidth, ClientHeight + 1);
    {$IFDEF CLR}
    Perform(EM_SETRECTNP, 0, Loc);
    Perform(EM_SETMARGINS, EC_RIGHTMARGIN, AboutWidth shl 16);
    {$ELSE}
    Perform(EM_SETRECTNP, 0, LongInt(@Loc));
    Perform(EM_SETMARGINS, EC_RIGHTMARGIN, AboutWidth shl 16);
    {$ENDIF}
  end;
var
  lwidth: Integer;
begin
  lwidth := 0;
  with FBtnSet1 do
    if AsSigned(FButton) then
      if ButtonVisible then
      begin
        if ShortCut(Key, Shift) = ButtonShortCut then //Ctrl+PgDn
          FButton.Click;
        Inc(lwidth,FButton.Width)
        //UpdateFormatRect;
        //Invalidate;
      end;
  with FBtnSet2 do
    if AsSigned(FButton) then
      if ButtonVisible then
      begin
        if ShortCut(Key, Shift) = ButtonShortCut then //Ctrl+PgUp
          FButton.Click;
        //UpdateFormatRect;
        //Invalidate;
        Inc(lwidth,FButton.Width)
      end;
  if lwidth > 0 then
  begin
    UpdateEditArea(lwidth);
    Invalidate;
  end;
  inherited KeyDown(Key, Shift); ;
end;

procedure TPubEdit.SetOnButton1Click(const Value: TNotifyEvent);
begin
  with FBtnSet1 do
  begin
    if not AsSigned(FButton) then CreateButton;
    FButton.OnClick := Value;
  end;
end;

procedure TPubEdit.SetOnButton2Click(const Value: TNotifyEvent);
begin
  with FBtnSet2 do
  begin
    if not AsSigned(FButton) then CreateButton;
    FButton.OnClick := Value;
  end;
end;

function TPubEdit.GetOnButton1Click: TNotifyEvent;
begin
  Result := nil;
  with FBtnSet1 do
    if AsSigned(FButton) then
      Result := FButton.OnClick;
end;

function TPubEdit.GetOnButton2Click: TNotifyEvent;
begin
  with FBtnSet2 do
    if not AsSigned(FButton) then Result := nil
    else
      Result := FButton.OnClick;
end;
{$EndIf}

{$IfDef UseAutocomplete}
procedure TPubEdit.KeyUp(var Key: Word; Shift: TShiftState);
var
  s1: string;
  s2: string;
begin
  if FAutocomplete.FAutocomplete then
  begin
    if Text = '' then
      Exit;
    s1 := Text; //store here
    if FAutocomplete.Test(Text, S2) then
    begin
      Text := S2;
      SelStart := Length(S1);
      SelLength := Length(S2) - Length(S1);
    end
    else begin
      if FAutocomplete.FForceComplete then
      begin
        Text := '';
        Key := 0;
        Exit;
      end;
    end;
  end;
  inherited KeyUp(Key, Shift);
end;
{$EndIf}

procedure TPubEdit.CMTextChanged(var Message: TMessage);
var S: String;
begin
  inherited;
  if NOT Focused Then
    if FTypeEdit = teString then
    begin
      if Text <> '' then
      begin
        {$IfDef UseAutocomplete}
        {je-li autocomplete}
        if FAutocomplete.AutoComplete then
        begin
          FAutocomplete.AutoCompleteItems.Add(Text);
        end;
        {$EndIf}
        {je-li maska (ale i bez ni), pak formatuj externi rutinou}
        if AsSigned(FMaskForm) then
        begin
          S := Text;
          //co kdyby uz mel format
          FMaskForm(Self, eValueToText, FMask, S);
          FMaskForm(Self, eTextToValue, FMask, S);
          Text := S;
        end;
      end;
    end;
end;

procedure TPubEdit.WMCut(var Message: TMessage);
begin
  inherited;
  if Assigned(FOnCut) then
    FOnCut(Self);
end;

procedure TPubEdit.WMCopy(var Message: TMessage);
begin
  inherited;
  if Assigned(FOnCopy) then
    FOnCopy(Self);
end;

procedure TPubEdit.WMClear(var Message: TMessage);
begin
  inherited;
  if Assigned(FOnClear) then
    FOnClear(Self);
end;

procedure TPubEdit.WMPaint(var Message: TWMPaint);
var
  ControlCanvas: TControlCanvas;
  Boundaries: TRect;
begin
  if Self.Text <> '' then inherited
  else
  begin
    inherited;
    if Self.FInsideLabel.Active then
    begin
      ControlCanvas := TControlCanvas.Create;
      Boundaries := ClientRect;

      try
        ControlCanvas.Control := Self;

        ControlCanvas.Brush.Color := Self.Color;
        ControlCanvas.Font.Assign(Self.Font);
        ControlCanvas.Font.Color := Self.FInsideLabel.Color;
        ControlCanvas.Font.Style := ControlCanvas.Font.Style + [fsBold];

        ControlCanvas.TextRect(Boundaries, 2, 2, Self.FInsideLabel.Caption);
        //ControlCanvas.TextOut(2,2, Self.FInsideLabel.Caption);

      finally
        ControlCanvas.Free;
      end;
    end;
  end;
end;

procedure TPubEdit.WMPaste(var Message: TMessage);
var
{$IFNDEF VER5UP}
  Handle: THandle;
{$ENDIF}
  CText: string;
  LText: string;
  AText: string;
  i, z: Integer;
begin
  {$IFDEF VER5UP}
  CText := Clipboard.AsText;
  if CText = '' then Exit; {nothing do}
  z := Length(CText);
  if FValidChars <> '' then
  begin
    for i := Length(CText) downto 1 do
      if Pos(CText[i], FValidChars) = 0 then
        Delete(CText,i,1);
    if Length(CText) <> z then
    begin
      if AsSigned(FErrorOccur) then
        {uzivatelska reakce na chybu}
        FErrorOccur(Self, StrNoPasteFromClipboard)
      else
        MessageBeep($FFFF);
      Exit; {no match length, contain restricted characters}
    end;
  end;
  LText := '';
  if SelStart > 0 then
    LText := Copy(Text, 1, SelStart);
  LText := LText + CText;
  AText := '';
  if (SelStart + 1) < Length(Text) then
    AText := Copy(Text, SelStart + SelLength + 1, Length(Text) - SelStart + SelLength + 1);
  Text := LText + AText;
  SelStart := Length(Text);
  {$ELSE}
  if IsClipboardFormatAvailable(CF_TEXT) then
  begin
    try
      OpenClipBoard(Self.Handle);
      Handle := GetClipboardData(CF_TEXT);
      if Handle = 0 then
        Exit;
      CText := StrPas(GlobalLock(Handle));
      GlobalUnlock(Handle);
      LText := '';
      if SelStart > 0 then
        LText := Copy(Text, 1, SelStart);
      LText := LText + CText;
      AText := '';
      if (SelStart + 1) < Length(Text) then
        AText := Copy(Text, SelStart + SelLength + 1, Length(Text) - SelStart + SelLength + 1);
      Text := LText + AText;
      SelStart := Length(Text);
    finally
      CloseClipBoard;
    end;
  end;
  {$ENDIF}
  if Assigned(FOnPaste) then
    FOnPaste(Self);
end;

{$IFDEF UNICODE}
{$IFNDEF CLR}

{ TPubButtonedEdit }

procedure TPubButtonedEdit.CreateParams(var Params: TCreateParams);
const
  Alignments: array[TAlignment] of Cardinal{Longint} = (ES_LEFT, ES_RIGHT, ES_CENTER);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or Alignments[FAlignment];
end;

constructor TPubButtonedEdit.Create(AOwner: TComponent);
var
  TheForm: TComponent; {The form that ultimately owns this component}
begin
  inherited Create(AOwner);
  FColorOptionsSet := TColorOptionsSet.Create;
  Color := ColorOptionsSet.ColorOnNotFocus;
  FTypeEdit := teString;
  FCapitalize := False;
  FRequired := False;
  FValidChars := '';
  FMask := '';
  FAlignMent := taLeftJustify;
  FErrMsg := '';
  {no button created}
  {autocomplete feature}
  {$IfDef UseAutocomplete}
  FAutocomplete := TAutocompleteText.Create;
  {$EndIf}
  { start hint window functionality }
  HintWin := TPaintLineHintWindow.Create(Self);
  HintWin.Color := {$IFDEF VER80}clYellow{$ELSE}clInfoBk{$ENDIF};
  HintWin.Canvas.Font.Color := {$IFDEF VER80}clBlack{$ELSE}clInfoText{$ENDIF};
  {Check if owner is a form, if not, move up tree of owners until we either find a form or a nil reference}
  TheForm := AOwner;
  while (TheForm <> nil) and not (TheForm is TForm) do TheForm := TheForm.Owner;
  {Record form window reference appropriately}
  if TheForm <> nil then FWindow := TheForm as TForm else FWindow := nil;
end;

procedure TPubButtonedEdit.DefaultHandler(var message);
var
  MsgT: TMsg;
begin
  {convert message to TMsg}
  MsgT.message := TMessage(message).Msg;
  inherited{$IFDEF VER80}DefaultHandler(message){$ENDIF};
  if HintWin = nil then Exit;
  {if it's WM_Mousemove then we don't want to hide the hint window}
  if (HintWin.IsHintMsg(MsgT) and (MsgT.message <> WM_MouseMove)) then
    ReleaseIt;
end;

destructor TPubButtonedEdit.Destroy;
begin
  {$IfDef UseAutocomplete}
  FAutocomplete.Free;
  {$EndIf}
  FColorOptionsSet.Free;
  inherited Destroy;
end;

procedure TPubButtonedEdit.SetAlignment(Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    RecreateWnd;
  end;
end;

procedure TPubButtonedEdit.SetRequired(Value: Boolean);
begin
  if Value <> FRequired then
    FRequired := Value;
end;

function TPubButtonedEdit.Validated: Boolean;
var
  S, SZ: string;
  CanExit: Boolean;
  strmsg: string;
  i: Integer;
begin
  Result := True; //je to OK
  S := Text;
  if AsSigned(FMaskForm) then
    FMaskForm(Self, eValueToText, Mask, S); //odmaskuj holou hodnotu
  SZ := S;
  if FValidChars <> '' then
  begin
    for i := Length(S) downto 1 do
      if Pos(S[i], FValidChars) = 0 then
        Delete(S,i,1);
    if Length(S) <> Length(SZ) then
    begin
      Result := False;
      Exit; //containt some chars up length - it is wrong
    end;
  end;

  if Assigned(FValidate) then
  begin //uzivatelska validace
    FValidate(Self, S, CanExit);{Vlastni validace}
    {Oprava textu zde neni!}
    {A pri chybe zahlas}
    if not CanExit then
      Result := False;
    {pokud je pole vyzadovane, musi byt neprazdne}
    if FRequired and (S = '') then
    begin
        Result := False;
    end;
  end
  else
  begin {zabudovane validace}
    Result := DoValidate(strmsg)
  end;
end;

function TPubButtonedEdit.Validator: Boolean;
  {uzivatelsky validator}
  {napr. pro vsechny edity se vola ve smycce a oznacuji se nevalidni pole}
  {
  for I := 0 to ComponentCount - 1 do begin
    if Components[I] is TPubButtonedEdit then
      if not (Components[I] as TPubButtonedEdit).DoValidate then begin
        //udelej jeste neco pro konkretni policko
      end;
  end;
  }
  {Volani funkce}
  {1. v onexit prislusneho pole}
  {--validuje se pouze jednine pole}
  {2. ve validacni procedure pro celou stranku/form/frame}
  {--validuji se vsechna pole stejneho typu (ve vysledku muse byt oznaceno vice chybnych poli)}
var
  S: string;
begin
  S := Text;
  if Assigned(FValidate) then
    FValidate(Self, S, Result)
  else
    Result := ValidityCheck(Text, FErrMsg);

  ColorOptionsSet.Show(Result);
end;

procedure TPubButtonedEdit.SetCapitalize(Value: Boolean);
begin
  if Value <> FCapitalize then
    FCapitalize := Value;
end;

procedure TPubButtonedEdit.SetAsInteger(Value: Integer);
begin
  Text := IntToStr(Value)
end;

function TPubButtonedEdit.GetAsInteger: Integer;
begin
  Result := 0;
  if Text = '' then
    Exit;
  try
    Result := StrToInt(Text);
  except
    raise EInvalidTypeConvert.Create(StrExpectedIntegerValue);
  end
end;

procedure TPubButtonedEdit.SetAsFloat(Value: Real);
begin
  Text := FloatToStr(Value);
end;

function TPubButtonedEdit.GetAsFloat: Real;
begin
  Result := 0;
  if Text = '' then
    Exit;
  try
    Result := StrToFloat(Text);
  except
    raise EInvalidTypeConvert.Create(StrExpectedFloatValue);
  end
end;

procedure TPubButtonedEdit.SetAsAnsiString(const Value: AnsiString);
begin
  SetAsString(string(Value));
end;

procedure TPubButtonedEdit.SetAsBoolean(const Value: Boolean);
begin
  if Value then
    Text := ccBoolDefYes
  else
    Text := ccBoolDefNo
end;

procedure TPubButtonedEdit.SetAsCurrency(const Value: Currency);
begin
  Text := FloatToStr(Value);
end;

procedure TPubButtonedEdit.SetAsDate(Value: TDate);
begin
  Text := DateToStr(Value);
end;

function TPubButtonedEdit.GetAsAnsiString: AnsiString;
begin
  Result := AnsiString(GetAsString);
end;

function TPubButtonedEdit.GetAsBoolean: Boolean;
begin
  Result := False;
  if Text = '' then
    Exit;
  if (Text = ccBoolDefYes) or (Text = ccBoolDefNo) then
    Result := Text = ccBoolDefYes
  else
    raise EInvalidTypeConvert.Create(StrExpectedBooleanValue);
end;

function TPubButtonedEdit.GetAsCurrency: Currency;
begin
  Result := 0;
  if Text = '' then
    Exit;
  try
    Result := StrToFloat(Text);
  except
    raise EInvalidTypeConvert.Create(StrExpectedCurrencyValue);
  end
end;

procedure TPubButtonedEdit.SetAsDateTime(const Value: TDateTime);
begin
  Text := DateTimeToStr(Value);
end;

function TPubButtonedEdit.GetAsDate: TDate;
begin
  Result := SysUtils.Date;
  if Text = '' then
    Exit;
  try
    Result := StrToDate(Text);
  except
    raise EInvalidTypeConvert.Create(StrExpectedDateValue);
  end
end;

function TPubButtonedEdit.GetAsDateTime: TDateTime;
begin
  Result := SysUtils.Now;
  if Text = '' then
    Exit;
  try
    Result := StrToDateTime(Text);
  except
    raise EInvalidTypeConvert.Create(StrExpectedDateTimeValue);
  end
end;

procedure TPubButtonedEdit.SetAsTime(Value: TTime);
begin
  Text := TimeToStr(Value);
end;

procedure TPubButtonedEdit.SetAsZeroPaddedString(const Value: string);
begin
  Text := LeftPadCh(Value, '0', MaxLength);
end;

function TPubButtonedEdit.GetAsTime: TTime;
begin
  Result := SysUtils.Time;
  if Text = '' then
    Exit;
  try
    Result := StrToTime(Text);
  except
    raise EInvalidTypeConvert.Create(StrExpectedTimeValue);
  end
end;

function TPubButtonedEdit.GetAsZeroPaddedString: string;
begin
  Result := Text;
  if Result = '' then
    Exit;
  try
    if AsSigned(FMaskForm) then
      FMaskForm(Self, eValueToText, FMask, Result) //odmaskuj holou hodnotu
    else
      Result := Text;

    Result := LeftPadCh(Result, '0', MaxLength);
  except
    Result := Text; //do nothing
  end
end;

procedure TPubButtonedEdit.SetAsString(Value: string);
var
  S: string;
begin
  S := Value;
  if AsSigned(FMaskForm) then
    FMaskForm(Self, eTextToValue, FMask, S); //odmaskuj holou hodnotu
  Text := S;
end;

function TPubButtonedEdit.GetAsString: string;
begin
  Result := Text;
  if Result = '' then
    Exit;
  try
    if AsSigned(FMaskForm) then
      FMaskForm(Self, eValueToText, FMask, Result) //odmaskuj holou hodnotu
    else
      Result := Text;
  except
    Result := Text; //do nothing
  end
end;

procedure TPubButtonedEdit.KeyPress(var Key: Char);
var
  C: string;
  {$IFNDEF CLR}
  VCH: {$IFDEF UNICODE}TSysCharSet{$ELSE}set of Char{$ENDIF};
  //I: Integer;
  {$ELSE}
  procedure Check(VCH: string);
  begin
    if (not CharInSet(Key, ccControlKeys)) and (VCH <> '') then
      if Pos(Key, VCH) = 0 then Key := #0; {Delej, jako by nebyla}
  end;
  {$ENDIF}
begin
  case FTypeEdit of
    teString:
      begin
        {$IFDEF CLR}
        //Check(FValidChars);
        if Length(FValidChars) > 0 then
          if (not CharInSet(Key, ccControlKeys)) then
            if Pos(Key, FValidChars) <= 0 then
              Key := #0; {Delej, jako by nebyla}
        {$ELSE}
        {ohodnot}
        if (not CharInSet(Key, ccControlKeys)) and (FValidChars <> '') then
          if Pos(Key, FValidChars) = 0 then Key := #0; {Delej, jako by nebyla}
        {$ENDIF}
      end;

    {pro vstup cisel se predpoklada interne jina maska}
    teInteg:
      begin
        {$IFDEF CLR}
        //Check('0123456789+-');
        if (not CharInSet(Key, ccControlKeys)) then
          if not CharInSet(Key, ['0'..'9', '+', '-']) then
        {$ELSE}
        {nejprve vyrob mnozinu znaku}
        VCH := ['0'..'9', '+', '-'];
        {ohodnot}
        if (not CharInSet(Key, ccControlKeys)) and (VCH <> []) then
          if not CharInSet(Key, VCH) then
        {$ENDIF}
            Key := #0; {Delej, jako by nebyla}
      end;

    {pro vstup cisel se predpoklada interne jina maska}
    teFloat, teCurrency:
      begin
        {$IFDEF CLR}
        //Check('0123456789+-eE' + SysUtils.DecimalSeparator);
        if (not CharInSet(Key, ccControlKeys)) then
          if not CharInSet(Key, ['0'..'9', '+', '-', 'e', 'E']) or (Key <> SysUtils.DecimalSeparator) then
        {$ELSE}
        {nejprve vyrob mnozinu znaku}
        VCH := ['0'..'9', '+', '-', 'e', 'E'] + [{$IFDEF VER15UP}FormatSettings.{$ENDIF}DecimalSeparator];
        if {$IFDEF VER15UP}FormatSettings.{$ENDIF}ThousandSeparator <> #0 then
          VCH := VCH + [{$IFDEF VER15UP}FormatSettings.{$ENDIF}ThousandSeparator];
        {ohodnot}
        if (not CharInSet(Key, ccControlKeys)) and (VCH <> []) then
          if not CharInSet(Key, VCH) then
        {$ENDIF}
            Key := #0; {Delej, jako by nebyla}
      end;

    teTime:
      begin
        {$IFDEF CLR}
        //Check('0123456789' + SysUtils.TimeSeparator);
        if (not CharInSet(Key, ccControlKeys)) then
          if not CharInSet(Key, ['0'..'9']) or (Key <> SysUtils.TimeSeparator) then
        {$ELSE}
        VCH := ['0'..'9'] + [{$IFDEF VER15UP}FormatSettings.{$ENDIF}TimeSeparator];
        if (not CharInSet(Key, ccControlKeys)) and (VCH <> []) then
          if not CharInSet(Key, VCH) then
        {$ENDIF}
            Key := #0; {Delej, jako by nebyla}
      end;

    teDate:
      begin
        {$IFDEF CLR}
        //Check('0123456789' + SysUtils.DateSeparator);
        if (not CharInSet(Key, ccControlKeys)) then
          if not CharInSet(Key, ['0'..'9']) or (Key <> SysUtils.DateSeparator) then
        {$ELSE}
        VCH := ['0'..'9'] + [{$IFDEF VER15UP}FormatSettings.{$ENDIF}DateSeparator];
        if (not CharInSet(Key, ccControlKeys)) and (VCH <> []) then
          if not CharInSet(Key, VCH) then
        {$ENDIF}
            Key := #0; {Delej, jako by nebyla}
      end;

    teDateTime:
      {NOTE: Only DD.MM.YYYY HH:NN:SS suported}
      begin
        {$IFDEF CLR}
        //Check('0123456789' + SysUtils.DateSeparator + SysUtils.TimeSeparator);
        if (not CharInSet(Key, ccControlKeys)) then
          if not CharInSet(Key, ['0'..'9']) or (Key <> SysUtils.DateSeparator) or (Key <> SysUtils.TimeSeparator) then
        {$ELSE}
        VCH := ['0'..'9', ' '] + [{$IFDEF VER15UP}FormatSettings.{$ENDIF}DateSeparator] + [{$IFDEF VER15UP}FormatSettings.{$ENDIF}TimeSeparator];
        if (not CharInSet(Key, ccControlKeys)) and (VCH <> []) then
          if not CharInSet(Key, VCH) then
        {$ENDIF}
            Key := #0; {Delej, jako by nebyla}
      end;

  end {case};
  {jsou-li pozadovana velka pismena na zacatku slov, udelej to tady}
  if FCapitalize then
    if (SelStart = 0) or (Text[SelStart] = ' ') then
    begin
      C := Key;
      C := AnsiUpperCase(C);
      Key := C[1];
    end;

  if Key <> #0 then
    inherited KeyPress(Key);
end;

procedure TPubButtonedEdit.DoEnter;
var S: string;
begin
  {vybarvi pole pri zamereni}
  Color := ColorOptionsSet.ColorOnFocus;
  Font.Color := ColorOptionsSet.ColorOnFocusFont;

  case FTypeEdit of
    {je-li pozadovano prevedeni a jedna-li se o integ cislo}
    teInteg:
      if AsSigned(FConvert) then
      begin {je-li prislusna rutina}
        S := Text;
        FConvert(Self, eValueToText, S); {preved text na jiny format}
        Text := S;
      end;

    {je-li potreba zaformatovat podle masky, pak jenom pro string}
    teString:
      if AsSigned(FMaskForm) then
      begin
        S := Text;
        FMaskForm(Self, eValueToText, FMask, S);
        Text := S;
      end;

  end {case};

  if not AutoSelect then
    if Text = '' then SelStart := 0
    else SelStart := GetTextLen;

  if Assigned(FOnEnter) then
    FOnEnter(Self);
end;

function TPubButtonedEdit.ValidityCheck(Value: string; var ErrMsg: string): Boolean;
var
  S: string;
begin
  ErrMsg := ''; //no errors occurred
  S := Value;
  if S <> '' then
    case FTypeEdit of
      teInteg:
        begin
          if not Self.Focused then
          begin
            if AsSigned(FConvert) then  //deconvert -like- Binary to Integer
              FConvert(Self, eValueToText, S);
          end;
          {je-li ocekavano cislo, pak to cislo byt musi}
          try
            AsInteger; {predpokladam Longint}
          except
            on EInvalidTypeConvert do
            begin
              ErrMsg := StrExpectedIntegerValue;
            end;
          end;
        end;
      teCurrency:
        begin
          try
            AsFloat
          except
            on EInvalidTypeConvert do
            begin
              ErrMsg := StrExpectedCurrencyValue;
            end;
          end;
        end;
      teFloat:
        begin
          {je-li ocekavano cislo, pak to cislo byt musi}
          try
            AsFloat; {nejaky float}
          except
            on EInvalidTypeConvert do
            begin
              ErrMsg := StrExpectedFloatValue;
            end;
          end;
        end;
      teString:
        begin
          if AsSigned(FMaskForm) then
          try
            {je-li maska (ale i bez ni), pak formatuj externi rutinou}
            FMaskForm(Self, eTextToValue, FMask, S); //<- primo neformatuje viz funkcnost v onexit
          except
            ErrMsg := StrInvalidMaskValue;
          end;
        end;
      teDate:
        if Text <> '' then
        begin
          {je-li ocekavano cislo, pak to cislo byt musi}
          try
            AsDate; {nejaky float}
          except
            on EInvalidTypeConvert do
            begin
              ErrMsg := StrExpectedDateValue;
            end;
          end;
        end;
      teTime:
        if Text <> '' then
        begin
          {je-li ocekavano cislo, pak to cislo byt musi}
          try
            AsTime; {nejaky float}
          except
            on EInvalidTypeConvert do
            begin
              ErrMsg := StrExpectedTimeValue;
            end;
          end;
        end;
      teDateTime:
        if Text <> '' then
        begin
          {je-li ocekavano cislo, pak to cislo byt musi}
          try
            AsDateTime; {nejaky float}
          except
            on EInvalidTypeConvert do
            begin
              ErrMsg := StrExpectedDateTimeValue;
            end;
          end;
        end;
    end {case};

  {teprve po nem znovu overuji, zda je pole vyplneno dle pozadavku}
  if FRequired and (S = '') then
  begin
    if ErrMsg <> '' then ErrMsg := ErrMsg + #13 + msgFieldRequired
    else ErrMsg := msgFieldRequired;
  end;

  Result :=  ErrMsg <> '';
end;

function TPubButtonedEdit.DoValidate(var oErrMsg: string): Boolean;
begin
  Result := ValidityCheck(Text, oErrMsg);
  FErrMsg := oErrMsg;
  //ColorOptionsSet.Show(Result); --nelze zde, vyjimka
end;

procedure TPubButtonedEdit.DoExit;
var
  S: string;
  Err: Boolean;
begin
  {tady zmen barvu pole, kdyz ztracis zamereni}
  Color := ColorOptionsSet.ColorOnNotFocus;
  Font.Color := ColorOptionsSet.ColorOnNotFocusFont;
  Self.Repaint; //Self.Invalidate;
  Err := ValidityCheck(Text, S);
  if Err then
  begin
    {uzivatelska reakce na chybu}
    if S <> msgFieldRequired then {pozadavek na vyplneni je zobrazen pozdeji}
      if AsSigned(FErrorOccur) then
        FErrorOccur(Self, S)
      else
        MessageDlg(S, mtError, [mbOK], 0);
    {vybrani chybneho pole}
    if Self.CanFocus then Self.SetFocus;
  end
  else
  begin
    case FTypeEdit of
      teString:
        if Text <> '' then
        begin
          {je-li maska (ale i bez ni), pak formatuj externi rutinou}
          if AsSigned(FMaskForm) then
          begin
            S := Text;
            FMaskForm(Self, eValueToText, FMask, S); {zbav se pripadne masky}
            FMaskForm(Self, eTextToValue, FMask, S); {znovu zamaskuj}
            Text := S;
          end;
        end;
      teInteg:
        begin
          {je-li pozadovano prevedeni a jedna-li se o integ cislo}
          if Assigned(FConvert) then
          begin {je-li prislusna rutina}
            S := Text;
            FConvert(Self, eTextToValue, S); {preved text na jiny format}
            Text := S;
          end;
        end;
    end;
  end;
  {tohle je vlastni onexit}
  if Assigned(FOnExit) then FOnExit(Self);

  {teprve po nem znovu overuji, zda je pole vyplneno dle pozadavku}
  if FRequired and (Text = '') then
  begin
    if AsSigned(FErrorOccur) then
      {uzivatelska reakce na chybu}
      FErrorOccur(Self, msgFieldRequired)
    else
      MessageDlg(msgFieldRequired, mtWarning, [mbOK], 0);
    if CanFocus then SetFocus;
  end;

end;

procedure TPubButtonedEdit.CMExit(var Message: TCMExit);
var
  CanExit: Boolean;
  S: string;
  i: Integer;
begin
  inherited;
  if FTypeEdit <> teParser then
  begin
    {vyfiltrovani vlozenych znaku, na nepovolene zde}
    if FValidChars <> '' then
    begin
      S := Text;
      for i := Length(S) downto 1 do
        if Pos(S[i], FValidChars) = 0 then
          Delete(S,i,1);
      if Length(S) <> Length(Text) then
        Text := S; {oprave zde}
    end;
  end;
  {je-li pozadovano polozku vyplnit a ona je prazdna, pak znovu}
  {tady zadnou hlasku nezobrazuji}
  if FRequired and (Text = '') then
  begin
    MessageBeep($FFFF);
    if Self.CanFocus then Self.SetFocus;
    Exit;
  end;
  {je-li pozadovana externi validace a neprojde, pak taky znovu}
  if FTypeEdit <> teParser then
  begin
    {vlastni validace}
    if Assigned(FValidate) then
    begin
      S := Text; {<-- text z editu} {zmena maskovani 01.09.1999 J.B.}
      {pole nejlepe bez masek}
      if AsSigned(FMaskForm) then FMaskForm(Self, eValueToText, Mask, S);
      try
        {Vlastni validace}
        FValidate(Self, S, CanExit);
      finally {fixed 23.10.2001 J.B.}
        if AsSigned(FMaskForm) then FMaskForm(Self, eTextToValue, Mask, S);
      end;
      {Oprava textu zde}
      if Text <> S then {27.7.2001 J.B.}
        Text := S;
      {pri chybe zpet}
      if not CanExit then
      begin
        MessageBeep($FFFF); {pri chybe jenom pipni}
        (*If AsSigned(FErrorOccur) Then
          {uzivatelska reakce na chybu}
          FErrorOccur(Self,msgFieldInvalidate)
        Else
          MessageDlg(msgFieldInvalidate,mtWarning, [mbOK],0);*)
        if Self.CanFocus then Self.SetFocus;
        Exit;
      end;
    end;
  end
  else
  begin
    {pro parser je validace extra zde}
    if not Assigned(FParsing) then
      Text := Parsed(Text)
    else
    begin
      {external parsing pointed there}
      FParsing(Self, Text, S);
      if Text <> S then
        Text := S;
    end;
  end;
  {pokud je uschovavana historie pole, pak zde}
{$IfDef UseAutocomplete}
  if FAutocomplete.FStoreHistory then
  begin
    FAutocomplete.FAutoCompleteItems.Add(Text)
  end;
{$EndIf}
end;

procedure TPubButtonedEdit.CMEnter(var Message: TCMEnter);
begin
  inherited;
end;

procedure TPubButtonedEdit.CMMouseEnter(var message: TMessage);
begin
  inherited;
  if Assigned(FOnMouseEnter) then FOnMouseEnter(Self);
end;

procedure TPubButtonedEdit.CMMouseLeave(var message: TMessage);
begin
  inherited;
  ReleaseIt;
  if Assigned(FOnMouseExit) then FOnMouseExit(Self);
end;

procedure TPubButtonedEdit.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  {obzvlast zajimave, nastavuji barvy :-( }
  if Enabled then
    if Focused then
    begin
      Color := ColorOptionsSet.ColorOnFocus;
      Font.Color := ColorOptionsSet.ColorOnFocusFont;
    end
    else
    begin
      Color := ColorOptionsSet.ColorOnNotFocus;
      Font.Color := ColorOptionsSet.ColorOnNotFocusFont;
    end
  else
  begin
    Color := ColorOptionsSet.ColorOnDisabled;
    Font.Color := ColorOptionsSet.ColorOnDisabledFont;
  end
end;

procedure TPubButtonedEdit.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if FSelectAll and FWider and Focused then
  begin
    SelectAll;
    FSelectAll := False;
  end;
  inherited{$IFDEF VER80}MouseUp(Button, Shift, X, Y){$ENDIF};
end;

{-----------------------------------------------------------------------------}

procedure TPubButtonedEdit.MouseMove(Shift: TShiftState; X, Y: Integer);
{ Since this was originally coded for a TListBox, where it makes sense being
  this way, and here it was adapted for a TEdit component, I think this could
  be better off inside the OnEnter event, but, since I'm naturally born lazy,
  I made only the minor modifications needed.  Someone can surely do a better
  job... }
var
  Rct: TRect;
begin
  if FWider and not Focused then
  begin
    FSelectAll := True;
    FWider := False;
  end;

  if not (FHintText) then
  begin {Item hinting is off}
    ReleaseIt;
    inherited{$IFDEF VER80}MouseMove(Shift, X, Y){$ENDIF};
    Exit;
  end;

  { Check if there is an owning window - get out if not}
  { If HintEdit is focused, do not show the tooltip to let user edit it}
  { Also, if Application has no focus, why should we care about showing a tip?}
  if Focused or (FWindow = nil) or (Trim(Text) = '') or
    not Application.Active
  then
    Exit;

  { the line that follows is important, otherwise the next line will calculate}
  { the wrong TextWidth via FWindows' Canvas - Thanks Peter Below (TeamB),}
  { for this handy tip that I found in Borland Newsgroup}
  FWindow.Canvas.Font := Font;
  { Check if the Text is wider then the Width}
  if FWindow.Canvas.TextWidth(Text) > Width - 6 then
  begin
    FWider := True;
    if FShowing then Exit;
    { Get the default item coordinates}
    Rct := ClientRect;
    { Stretch it to fit the whole edit text }
    Rct.Right := Rct.Left + FWindow.Canvas.TextWidth(Text) + 5;
    { Fine tune theses values for appearance }
    Rct.Top := Rct.Top - 1; {2}
    Rct.Bottom := Rct.Bottom - 4; {2}
    Rct.Left := Rct.Left - 1; {2}
    { now convert to screen coordinates so that THintWindow can use them}
    Rct.TopLeft := ClientToScreen(Rct.TopLeft);
    Rct.BottomRight := ClientToScreen(Rct.BottomRight);
    { And show it!}
    HintWin.ActivateHint(Rct, Text);
    FShowing := True;
  end
  else
    ReleaseIt;

  inherited{$IFDEF VER80}MouseMove(Shift, X, Y){$ENDIF};
end;

procedure TPubButtonedEdit.ReleaseIt;
begin
  HintWin.ReleaseHandle;
  FShowing := False;
end;

function TPubButtonedEdit.Parsed(const S: string): string;
var
  X: string;
  EP: TEvalParser;
begin
  Result := S;
  if S = '' then Exit;
  X := S;
  EP := TEvalParser.Create;
  try
    try
      EP.CleanUp(X);
      Result := FloatToStr(EP.Evaluate(X));
    except
      on E: Exception do
        if AsSigned(FErrorOccur) then
           {uzivatelska reakce na chybu}
          FErrorOccur(Self, msgConvertValue + #13 + '(' + E.Message + ')')
        else
          MessageDlg(msgConvertValue + #13 + '(' + E.Message + ')', mtError, [mbOK], 0);
    end;
  finally
    EP.Free;
  end;
end;

procedure TPubButtonedEdit.NextControl(YesNo: Boolean);
var
  FTempForm:
  {$IFDEF CLR}TCustomForm
  {$ELSE}
    {$IFNDEF VER5UP}
    TForm
    {$ELSE}
    TCustomForm
    {$ENDIF}
  {$ENDIF};
begin

  if YesNo then
  begin
    FTempForm := GetParentForm(Self);
    SendMessage(FTempForm.Handle, WM_NEXTDLGCTL, 0, 0);
  end

end;

{$IfDef UseAutocomplete}
procedure TPubButtonedEdit.KeyUp(var Key: Word; Shift: TShiftState);
var
  s1: string;
  s2: string;
begin
  if FAutocomplete.FAutocomplete then
  begin
    if Text = '' then
      Exit;
    s1 := Text; //store here
    if FAutocomplete.Test(Text, S2) then
    begin
      Text := S2;
      SelStart := Length(S1);
      SelLength := Length(S2) - Length(S1);
    end
    else begin
      if FAutocomplete.FForceComplete then
      begin
        Text := '';
        Key := 0;
        Exit;
      end;
    end;
  end;
  inherited KeyUp(Key, Shift);
end;
{$EndIf}

procedure TPubButtonedEdit.CMTextChanged(var Message: TMessage);
var S: String;
begin
  inherited;
  if NOT Focused Then
    if FTypeEdit = teString then
    begin
      if Text <> '' then
      begin
        {$IfDef UseAutocomplete}
        {je-li autocomplete}
        if FAutocomplete.AutoComplete then
        begin
          FAutocomplete.AutoCompleteItems.Add(Text);
        end;
        {$EndIf}
        {je-li maska (ale i bez ni), pak formatuj externi rutinou}
        if AsSigned(FMaskForm) then
        begin
          S := Text;
          //co kdyby uz mel format
          FMaskForm(Self, eValueToText, FMask, S);
          FMaskForm(Self, eTextToValue, FMask, S);
          Text := S;
        end;
      end;
    end;
end;

procedure TPubButtonedEdit.WMCut(var Message: TMessage);
begin
  inherited;
  if Assigned(FOnCut) then
    FOnCut(Self);
end;

procedure TPubButtonedEdit.WMCopy(var Message: TMessage);
begin
  inherited;
  if Assigned(FOnCopy) then
    FOnCopy(Self);
end;

procedure TPubButtonedEdit.WMClear(var Message: TMessage);
begin
  inherited;
  if Assigned(FOnClear) then
    FOnClear(Self);
end;

procedure TPubButtonedEdit.WMPaste(var Message: TMessage);
var
{$IFNDEF VER5UP}
  Handle: THandle;
{$ENDIF}
  CText: string;
  LText: string;
  AText: string;
  i, z: Integer;
begin
  {$IFDEF VER5UP}
  CText := Clipboard.AsText;
  if CText = '' then Exit; {nothing do}
  z := Length(CText);
  if FValidChars <> '' then
  begin
    for i := Length(CText) downto 1 do
      if Pos(CText[i], FValidChars) = 0 then
        Delete(CText,i,1);
    if Length(CText) <> z then
    begin
      if AsSigned(FErrorOccur) then
        {uzivatelska reakce na chybu}
        FErrorOccur(Self, StrNoPasteFromClipboard)
      else
        MessageBeep($FFFF);
      Exit; {no match length, contain restricted characters}
    end;
  end;
  LText := '';
  if SelStart > 0 then
    LText := Copy(Text, 1, SelStart);
  LText := LText + CText;
  AText := '';
  if (SelStart + 1) < Length(Text) then
    AText := Copy(Text, SelStart + SelLength + 1, Length(Text) - SelStart + SelLength + 1);
  Text := LText + AText;
  {$ELSE}
  if IsClipboardFormatAvailable(CF_TEXT) then
  begin
    try
      OpenClipBoard(Self.Handle);
      Handle := GetClipboardData(CF_TEXT);
      if Handle = 0 then
        Exit;
      CText := StrPas(GlobalLock(Handle));
      GlobalUnlock(Handle);
      LText := '';
      if SelStart > 0 then
        LText := Copy(Text, 1, SelStart);
      LText := LText + CText;
      AText := '';
      if (SelStart + 1) < Length(Text) then
        AText := Copy(Text, SelStart + SelLength + 1, Length(Text) - SelStart + SelLength + 1);
      Text := LText + AText;
    finally
      CloseClipBoard;
    end;
  end;
  {$ENDIF}
  if Assigned(FOnPaste) then
    FOnPaste(Self);
end;

{  TPubLabeledButtonedEdit  }

procedure TPubLabeledButtonedEdit.SetLabelSpacing(const Value: Integer);
begin
  FLabelSpacing := Value;
  SetLabelPosition(FLabelPosition);
end;

constructor TPubLabeledButtonedEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLabelPosition := lpAbove;
  FLabelSpacing := 3;
  FEditLabel := {$IFDEF USE_RXLIB}TRxLabel{$ELSE}{$IFNDEF VER6UP}TLabel{$ELSE}TBoundLabel{$ENDIF}{$ENDIF}.Create(Self);
  {$IFDEF USE_RXLIB }
  FEditLabel.Name := 'SubLabel';  { do not localize }
  {$IFDEF VER6UP}
  FEditLabel.SetSubComponent(True);
  {$ENDIF}
  if Assigned(AOwner) then
    FEditLabel.Caption := AOwner.Name;
  {$ELSE}
    {$IFNDEF VER6UP}
  FEditLabel.Name := 'SubLabel';  { do not localize }
  FEditLabel.SetSubComponent(True);
  if Assigned(AOwner) then
    FEditLabel.Caption := AOwner.Name;
    {$ENDIF}
  {$ENDIF}
  //FEditLabel.FreeNotification(Self);
  FEditLabel.Parent := Parent;
  FEditLabel.ParentFont := ParentFont;
  FEditLabel.Font.Assign(Self.Font);
  FEditLabel.Name := 'EditLabel' + Name;
  FEditLabel.Caption := Self.Name;
  {$IFDEF USE_RXLIB}
  FEditLabel.FocusControl := Self;
  FEditLabel.ShadowPos := spRightBottom;
  FEditLabel.Transparent := True;
  FEditLabel.ShadowSize := 0;
  {$ELSE}
    {$IFNDEF VER6UP}
  FEditLabel.FocusControl := Self;
    {$ENDIF}
  {$ENDIF}
end;

destructor TPubLabeledButtonedEdit.Destroy;
begin
  {$IFDEF VER5UP}
  FreeAndNil(FEditLabel);
  {$ELSE}
  FEditLabel.Free;
  FEditLabel := nil;
  {$ENDIF}
  inherited Destroy;
end;

procedure TPubLabeledButtonedEdit.SetName(const Value: TComponentName);
begin
  if (csDesigning in ComponentState) and ((FEditlabel.GetTextLen = 0) or
     (CompareText(FEditLabel.Caption, Name) = 0))
  then
    FEditLabel.Caption := Value;
  inherited SetName(Value);
  if csDesigning in ComponentState then
    Text := '';
end;

procedure TPubLabeledButtonedEdit.CMBidimodechanged(var Message: TMessage);
begin
  inherited;
  FEditLabel.BiDiMode := BiDiMode;
end;

procedure TPubLabeledButtonedEdit.SetLabelPosition(const Value: TLabelPosition);
var
  P: TPoint;
begin
  if FEditLabel = nil then Exit;
  FLabelPosition := Value;
  case Value of
    lpAbove: P := Point(Left, Top - FEditLabel.Height - FLabelSpacing);
    lpBelow: P := Point(Left, Top + Height + FLabelSpacing);
    lpLeft : P := Point(Left - FEditLabel.Width - FLabelSpacing,
                    Top + ((Height - FEditLabel.Height) div 2));
    lpRight: P := Point(Left + Width + FLabelSpacing,
                    Top + ((Height - FEditLabel.Height) div 2));
  end;
  FEditLabel.SetBounds(P.x, P.y, FEditLabel.Width, FEditLabel.Height);
end;

procedure TPubLabeledButtonedEdit.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  if FEditLabel = nil then Exit;
  FEditLabel.Parent := AParent;
  FEditLabel.Visible := True;
end;

procedure TPubLabeledButtonedEdit.CMEnabledchanged(var Message: TMessage);
begin
  inherited;
  FEditLabel.Enabled := Enabled;
end;

procedure TPubLabeledButtonedEdit.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  SetLabelPosition(FLabelPosition);
end;

procedure TPubLabeledButtonedEdit.CMVisiblechanged(var Message: TMessage);
begin
  inherited;
  FEditLabel.Visible := Visible;
end;

function TPubLabeledButtonedEdit.GetLabelShowsRequired: Boolean;
begin
  Result := (FEditLabel.Font.Color = clPurple) and (fsUnderline in FEditLabel.Font.Style);
end;

procedure TPubLabeledButtonedEdit.SetLabelShowsRequired(const Value: Boolean);
begin
  if Value then
   begin
    FEditLabel.Font.Color := clPurple;
    FEditLabel.Font.Style := [fsUnderline];
  end
  else
  begin
    FEditLabel.Font.Color := clWindowText;
    FEditLabel.Font.Style := [];
  end;
end;
{$ENDIF}
{$ENDIF}

{  TPubLabeledEdit  }

procedure TPubLabeledEdit.SetLabelSpacing(const Value: Integer);
begin
  FLabelSpacing := Value;
  SetLabelPosition(FLabelPosition);
end;

constructor TPubLabeledEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLabelPosition := lpAbove;
  FLabelSpacing := 3;
  FEditLabel := {$IFDEF USE_RXLIB}TRxLabel{$ELSE}{$IFNDEF VER6UP}TLabel{$ELSE}TBoundLabel{$ENDIF}{$ENDIF}.Create(Self);
  {$IFDEF USE_RXLIB }
  FEditLabel.Name := 'SubLabel';  { do not localize }
  {$IFDEF VER6UP}
  FEditLabel.SetSubComponent(True);
  {$ENDIF}
  if Assigned(AOwner) then
    FEditLabel.Caption := AOwner.Name;
  {$ELSE}
    {$IFNDEF VER6UP}
  FEditLabel.Name := 'SubLabel';  { do not localize }
  FEditLabel.SetSubComponent(True);
  if Assigned(AOwner) then
    FEditLabel.Caption := AOwner.Name;
    {$ENDIF}
  {$ENDIF}
  //FEditLabel.FreeNotification(Self);
  FEditLabel.Parent := Parent;
  FEditLabel.ParentFont := ParentFont;
  FEditLabel.Font.Assign(Self.Font);
  FEditLabel.Name := 'EditLabel' + Name;
  FEditLabel.Caption := Self.Name;
  {$IFDEF USE_RXLIB}
  FEditLabel.FocusControl := Self;
  FEditLabel.ShadowPos := spRightBottom;
  FEditLabel.Transparent := True;
  FEditLabel.ShadowSize := 0;
  {$ELSE}
    {$IFNDEF VER6UP}
  FEditLabel.FocusControl := Self;
    {$ENDIF}
  {$ENDIF}
end;

destructor TPubLabeledEdit.Destroy;
begin
  {$IFDEF VER5UP}
  FreeAndNil(FEditLabel);
  {$ELSE}
  FEditLabel.Free;
  FEditLabel := nil;
  {$ENDIF}
  inherited Destroy;
end;

procedure TPubLabeledEdit.SetName(const Value: TComponentName);
begin
  if (csDesigning in ComponentState) and ((FEditlabel.GetTextLen = 0) or
     (CompareText(FEditLabel.Caption, Name) = 0))
  then
    FEditLabel.Caption := Value;
  inherited SetName(Value);
  if csDesigning in ComponentState then
    Text := '';
end;

procedure TPubLabeledEdit.CMBidimodechanged(var Message: TMessage);
begin
  inherited;
  FEditLabel.BiDiMode := BiDiMode;
end;

procedure TPubLabeledEdit.SetLabelPosition(const Value: TLabelPosition);
var
  P: TPoint;
begin
  if FEditLabel = nil then Exit;
  FLabelPosition := Value;
  case Value of
    lpAbove: P := Point(Left, Top - FEditLabel.Height - FLabelSpacing);
    lpBelow: P := Point(Left, Top + Height + FLabelSpacing);
    lpLeft : P := Point(Left - FEditLabel.Width - FLabelSpacing,
                    Top + ((Height - FEditLabel.Height) div 2));
    lpRight: P := Point(Left + Width + FLabelSpacing,
                    Top + ((Height - FEditLabel.Height) div 2));
  end;
  FEditLabel.SetBounds(P.x, P.y, FEditLabel.Width, FEditLabel.Height);
end;

procedure TPubLabeledEdit.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  if FEditLabel = nil then Exit;
  FEditLabel.Parent := AParent;
  FEditLabel.Visible := True;
end;

procedure TPubLabeledEdit.CMEnabledchanged(var Message: TMessage);
begin
  inherited;
  FEditLabel.Enabled := Enabled;
end;

procedure TPubLabeledEdit.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  SetLabelPosition(FLabelPosition);
end;

procedure TPubLabeledEdit.CMVisiblechanged(var Message: TMessage);
begin
  inherited;
  FEditLabel.Visible := Visible;
end;

function TPubLabeledEdit.GetLabelShowsRequired: Boolean;
begin
  Result := (FEditLabel.Font.Color = clPurple) and (fsUnderline in FEditLabel.Font.Style);
end;

procedure TPubLabeledEdit.SetLabelShowsRequired(const Value: Boolean);
begin
  if Value then
   begin
    FEditLabel.Font.Color := clPurple;
    FEditLabel.Font.Style := [fsUnderline];
  end
  else
  begin
    FEditLabel.Font.Color := clWindowText;
    FEditLabel.Font.Style := [];
  end;
end;

{  TPubLabeledComboBox  }

procedure TPubLabeledComboBox.SetLabelSpacing(const Value: Integer);
begin
  FLabelSpacing := Value;
  SetLabelPosition(FLabelPosition);
end;

constructor TPubLabeledComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLabelPosition := lpAbove;
  FLabelSpacing := 3;
  FEditLabel := {$IFDEF USE_RXLIB}TRxLabel{$ELSE}{$IFNDEF VER6UP}TLabel{$ELSE}TBoundLabel{$ENDIF}{$ENDIF}.Create(Self);
  {$IFDEF USE_RXLIB }
  FEditLabel.Name := 'SubLabel';  { do not localize }
  {$IFDEF VER6UP}
  FEditLabel.SetSubComponent(True);
  {$ENDIF}
  if Assigned(AOwner) then
    FEditLabel.Caption := AOwner.Name;
  {$ELSE}
    {$IFNDEF VER6UP}
  FEditLabel.Name := 'SubLabel';  { do not localize }
  FEditLabel.SetSubComponent(True);
  if Assigned(AOwner) then
    FEditLabel.Caption := AOwner.Name;
    {$ENDIF}
  {$ENDIF}
  //FEditLabel.FreeNotification(Self);
  FEditLabel.Parent := Parent;
  FEditLabel.ParentFont := ParentFont;
  FEditLabel.Font.Assign(Self.Font);
  FEditLabel.Name := 'EditLabel' + Name;
  FEditLabel.Caption := Self.Name;
  {$IFDEF USE_RXLIB}
  FEditLabel.FocusControl := Self;
  FEditLabel.ShadowPos := spRightBottom;
  FEditLabel.Transparent := True;
  FEditLabel.ShadowSize := 0;
  {$ELSE}
    {$IFNDEF VER6UP}
  FEditLabel.FocusControl := Self;
    {$ENDIF}
  {$ENDIF}
end;

destructor TPubLabeledComboBox.Destroy;
begin
  FEditLabel.Free;
  inherited;
end;

procedure TPubLabeledComboBox.SetName(const Value: TComponentName);
begin
  if (csDesigning in ComponentState) and ((FEditlabel.GetTextLen = 0) or
     (CompareText(FEditLabel.Caption, Name) = 0))
  then
    FEditLabel.Caption := Value;
  inherited SetName(Value);
  if csDesigning in ComponentState then
    Text := '';
end;

procedure TPubLabeledComboBox.CMBidimodechanged(var Message: TMessage);
begin
  inherited;
  FEditLabel.BiDiMode := BiDiMode;
end;

procedure TPubLabeledComboBox.SetLabelPosition(const Value: TLabelPosition);
var
  P: TPoint;
begin
  if FEditLabel = nil then Exit;
  FLabelPosition := Value;
  case Value of
    lpAbove: P := Point(Left, Top - FEditLabel.Height - FLabelSpacing);
    lpBelow: P := Point(Left, Top + Height + FLabelSpacing);
    lpLeft : P := Point(Left - FEditLabel.Width - FLabelSpacing,
                    Top + ((Height - FEditLabel.Height) div 2));
    lpRight: P := Point(Left + Width + FLabelSpacing,
                    Top + ((Height - FEditLabel.Height) div 2));
  end;
  FEditLabel.SetBounds(P.x, P.y, FEditLabel.Width, FEditLabel.Height);
end;

procedure TPubLabeledComboBox.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FEditLabel) and (Operation = opRemove) then
    FEditLabel := nil;
end;

procedure TPubLabeledComboBox.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  if FEditLabel = nil then Exit;
  FEditLabel.Parent := AParent;
  FEditLabel.Visible := True;
end;

procedure TPubLabeledComboBox.CMEnabledchanged(var Message: TMessage);
begin
  inherited;
  FEditLabel.Enabled := Enabled;
end;

procedure TPubLabeledComboBox.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  SetLabelPosition(FLabelPosition);
end;

procedure TPubLabeledComboBox.CMVisiblechanged(var Message: TMessage);
begin
  inherited;
  FEditLabel.Visible := Visible;
end;

function TPubLabeledComboBox.GetLabelShowsRequired: Boolean;
begin
  Result := (FEditLabel.Font.Color = clPurple) and (fsUnderline in FEditLabel.Font.Style);
end;

procedure TPubLabeledComboBox.SetLabelShowsRequired(const Value: Boolean);
begin
  if Value then
  begin
    FEditLabel.Font.Color := clPurple;
    FEditLabel.Font.Style := [fsUnderline];
  end
  else
  begin
    FEditLabel.Font.Color := clWindowText;
    FEditLabel.Font.Style := [];
  end;
end;

{$IFDEF VER7UP}

{  TPubLabeledComboBoxEx  }

procedure TPubLabeledComboBoxEx.SetLabelSpacing(const Value: Integer);
begin
  FLabelSpacing := Value;
  SetLabelPosition(FLabelPosition);
end;

constructor TPubLabeledComboBoxEx.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLabelPosition := lpAbove;
  FLabelSpacing := 3;
  FEditLabel := {$IFDEF USE_RXLIB}TRxLabel{$ELSE}{$IFNDEF VER6UP}TLabel{$ELSE}TBoundLabel{$ENDIF}{$ENDIF}.Create(Self);
  {$IFDEF USE_RXLIB }
  FEditLabel.Name := 'SubLabel';  { do not localize }
  {$IFDEF VER6UP}
  FEditLabel.SetSubComponent(True);
  {$ENDIF}
  if Assigned(AOwner) then
    FEditLabel.Caption := AOwner.Name;
  {$ELSE}
    {$IFNDEF VER6UP}
  FEditLabel.Name := 'SubLabel';  { do not localize }
  FEditLabel.SetSubComponent(True);
  if Assigned(AOwner) then
    FEditLabel.Caption := AOwner.Name;
    {$ENDIF}
  {$ENDIF}
  //FEditLabel.FreeNotification(Self);
  FEditLabel.Parent := Parent;
  FEditLabel.ParentFont := ParentFont;
  FEditLabel.Font.Assign(Self.Font);
  FEditLabel.Name := 'EditLabel' + Name;
  FEditLabel.Caption := Self.Name;
  {$IFDEF USE_RXLIB}
  FEditLabel.FocusControl := Self;
  FEditLabel.ShadowPos := spRightBottom;
  FEditLabel.Transparent := True;
  FEditLabel.ShadowSize := 0;
  {$ELSE}
    {$IFNDEF VER6UP}
  FEditLabel.FocusControl := Self;
    {$ENDIF}
  {$ENDIF}
end;

destructor TPubLabeledComboBoxEx.Destroy;
begin
  FEditLabel.Free;
  inherited;
end;

procedure TPubLabeledComboBoxEx.SetName(const Value: TComponentName);
begin
  if (csDesigning in ComponentState) and ((FEditlabel.GetTextLen = 0) or
     (CompareText(FEditLabel.Caption, Name) = 0))
  then
    FEditLabel.Caption := Value;
  inherited SetName(Value);
  if csDesigning in ComponentState then
    Text := '';
end;

procedure TPubLabeledComboBoxEx.CMBidimodechanged(var Message: TMessage);
begin
  inherited;
  FEditLabel.BiDiMode := BiDiMode;
end;

procedure TPubLabeledComboBoxEx.SetLabelPosition(const Value: TLabelPosition);
var
  P: TPoint;
begin
  if FEditLabel = nil then Exit;
  FLabelPosition := Value;
  case Value of
    lpAbove: P := Point(Left, Top - FEditLabel.Height - FLabelSpacing);
    lpBelow: P := Point(Left, Top + Height + FLabelSpacing);
    lpLeft : P := Point(Left - FEditLabel.Width - FLabelSpacing,
                    Top + ((Height - FEditLabel.Height) div 2));
    lpRight: P := Point(Left + Width + FLabelSpacing,
                    Top + ((Height - FEditLabel.Height) div 2));
  end;
  FEditLabel.SetBounds(P.x, P.y, FEditLabel.Width, FEditLabel.Height);
end;

procedure TPubLabeledComboBoxEx.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FEditLabel) and (Operation = opRemove) then
    FEditLabel := nil;
end;

procedure TPubLabeledComboBoxEx.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  if FEditLabel = nil then Exit;
  FEditLabel.Parent := AParent;
  FEditLabel.Visible := True;
end;

procedure TPubLabeledComboBoxEx.CMEnabledchanged(var Message: TMessage);
begin
  inherited;
  FEditLabel.Enabled := Enabled;
end;

procedure TPubLabeledComboBoxEx.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  SetLabelPosition(FLabelPosition);
end;

procedure TPubLabeledComboBoxEx.CMVisiblechanged(var Message: TMessage);
begin
  inherited;
  FEditLabel.Visible := Visible;
end;

function TPubLabeledComboBoxEx.GetLabelShowsRequired: Boolean;
begin
  Result := (FEditLabel.Font.Color = clPurple) and (fsUnderline in FEditLabel.Font.Style);
end;

procedure TPubLabeledComboBoxEx.SetLabelShowsRequired(const Value: Boolean);
begin
  if Value then
  begin
    FEditLabel.Font.Color := clPurple;
    FEditLabel.Font.Style := [fsUnderline];
  end
  else
  begin
    FEditLabel.Font.Color := clWindowText;
    FEditLabel.Font.Style := [];
  end;
end;

{$ENDIF}

{  TPubMaskEdit  }

constructor TPubMaskEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FColorOptionsSet := TColorOptionsSet.Create;
  Color := ColorOptionsSet.ColorOnNotFocus;
  FRequired := False;
  FAcceptEmpty := False;
  FScrollBars := ssNone;
  FAlignment := taLeftJustify;
  FMultiline := False;
  FWordWrap := False;
  FErrMsg := '';
  Canvas := TControlCanvas.Create;
  {aby se dalo kreslit, musi se to napojit takto !!!}
  TControlCanvas(Canvas).Control := Self;
end;

destructor TPubMaskEdit.Destroy;
begin
  FColorOptionsSet.Free;
  Canvas.Free;
  inherited Destroy;
end;

procedure TPubMaskEdit.SetRequired(Value: Boolean);
begin
  if Value <> FRequired then
    FRequired := Value;
end;

procedure TPubMaskEdit.DoEnter;
begin
  {vybarvi pole pri zamereni}
  Color := ColorOptionsSet.ColorOnFocus;
  Font.Color := ColorOptionsSet.ColorOnFocusFont;

  if not AutoSelect then
    if Trim(Text) = '' then SelStart := 0
    else SelStart := GetTextLen;

  {vlastni funkce onenter}
  if Assigned(FOnEnter) then
    FOnEnter(Self);
end;

procedure TPubMaskEdit.DoExit;
begin
  {tady zmen barvu pole, kdyz ztracis zamereni}
  Color := ColorOptionsSet.ColorOnNotFocus;
  Font.Color := ColorOptionsSet.ColorOnNotFocusFont;

  {tohle je vlastni onexit}
  if Assigned(FOnExit) then FOnExit(Self);

  {teprve po nem znovu overuji, zda je pole vyplneno dle pozadavku}
  if FRequired and (Text = '') then
  begin
    if AsSigned(FErrorOccur) then
      {uzivatelska reakce na chybu}
      FErrorOccur(Self, msgFieldRequired)
    else
      MessageDlg(msgFieldRequired, mtWarning, [mbOK], 0);
    if CanFocus then SetFocus;
  end;

end;

function TPubMaskEdit.DoValidate: Boolean;
var
  S: string;
begin
  S := Text;
  if Assigned(FValidate) then
    FValidate(Self, S, Result)
  else
    Result := True; {ostatni obj nemaji vlastni check}

  ColorOptionsSet.Show(Result);
end;

procedure TPubMaskEdit.CMExit(var Message: TCMExit);
var
  CanExit: Boolean;
  S: string;
  CurValue: string;
  CurMask: string;
begin
  {tento kod doplnen pro pozadavek prazdneho pole}
  {$IFDEF EmptyMaskRequired}
  CurValue := Text;
  if FAcceptEmpty and ((Text = FormatMaskText(EditMask, '')) or (Text = '')) then
  begin
    CurMask := EditMask;
    EditMask := '';
    try
      inherited;
    except
      Text := CurValue;
      raise;
      Exit;
    end;
    EditMask := CurMask;
    Exit;
  end;
  {$ENDIF EmptyMaskRequired}
  inherited;
  {je-li pozadovano polozku vyplnit a ona je prazdna, pak znovu}
  {tady zadnou hlasku nezobrazuji}
  if FRequired and (Text = '') then
  begin
    MessageBeep($FFFF);
    if Self.CanFocus then Self.SetFocus;
    Exit; {out without any message}
  end;
  {je-li pozadovana externi validace a neprojde, pak taky znovu}

  if Assigned(FValidate) then
  begin
    S := Text;
    FValidate(Self, S, CanExit);
    if S <> Text then {in validate can be changed, update it !}
      Text := S;
    if not CanExit then
    begin {fatal error in validate}
      MessageBeep($FFFF);
      if Self.CanFocus then Self.SetFocus;
      Exit;
    end;
  end;

end;

{$IFDEF EmptyMaskRequired}
procedure TPubMaskEdit.KeyPress(var Key: Char);
var
  KeptEditMask: string;
begin
  if ((Key = #13) and FAcceptEmpty and (Trim(Text) = Trim(FormatMaskText(EditMask, '')))) then
  try
    KeptEditMask := EditMask;
    EditMask := '';
    inherited KeyPress(Key);
  finally
    EditMask := KeptEditMask;
    Key := #0;
  end
  else
    inherited KeyPress(Key);
end;

procedure TPubMaskEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if ((ssCtrl in Shift) and (Key = vk_Return)) then
  begin
    DblClick;
  end;

  inherited KeyDown(Key, Shift);
end;
{$ENDIF EmptyMaskRequired}

procedure TPubMaskEdit.CMEnter(var Message: TCMEnter);
begin
  inherited;
end;

procedure TPubMaskEdit.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  if Enabled then
    if Focused then
    begin
      Color := ColorOptionsSet.ColorOnFocus;
      Font.Color := ColorOptionsSet.ColorOnFocusFont;
    end
    else
    begin
      Color := ColorOptionsSet.ColorOnNotFocus;
      Font.Color := ColorOptionsSet.ColorOnNotFocusFont;
    end
  else
  begin
    Color := ColorOptionsSet.ColorOnDisabled;
    Font.Color := ColorOptionsSet.ColorOnDisabledFont;
  end
end;

procedure TPubMaskEdit.CreateParams(var Params: TCreateParams);
const
  aAlignments: array[TAlignment] of DWORD = ( ES_LEFT, ES_RIGHT, ES_CENTER );
  aMultiline: array[boolean] of DWORD = ( 0, ES_MULTILINE );
  ScrollBar: array[TScrollStyle] of DWORD = (0, WS_HSCROLL, WS_VSCROLL,
    WS_HSCROLL or WS_VSCROLL);
  WordWraps: array[Boolean] of DWORD = (0, ES_AUTOHSCROLL);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or aMultiline[FMultiline] or WS_CLIPCHILDREN
    or aAlignments[FAlignment] or ScrollBar[FScrollBars] or WordWraps[FWordWrap];
end;

procedure TPubMaskEdit.SetMultiline(Value: Boolean);
begin
  FMultiline := Value;
  RecreateWnd;
end;

procedure TPubMaskEdit.SetScrollBars(Value: TScrollStyle);
begin
  FScrollBars := Value;
  RecreateWnd;
end;

procedure TPubMaskEdit.SetAlignment(Value: TAlignment);
begin
  FAlignment := Value;
  RecreateWnd;
end;

procedure TPubMaskEdit.SetWordWrap(Value: Boolean);
begin
  FWordWrap := Value;
  RecreateWnd;
end;

function TPubMaskEdit.Validator: Boolean;
begin
  Result := DoValidate;
end;

procedure TPubMaskEdit.Paint(var Message: TWMPaint);
begin
  inherited;
  if Assigned(FAfterPaint) then
    FAfterPaint(Self);
end;

{  TPubComboBox  }

constructor TPubComboBox.Create(AOwner: TComponent);
{$IFDEF CB_HINTER}
var
  TheForm: TComponent; {The form that ultimately owns this component}
{$ENDIF}
begin
  inherited Create(AOwner);
  FColorOptionsSet := TColorOptionsSet.Create;
  Color := ColorOptionsSet.ColorOnNotFocus;
  FRequired := False;
  FDropDownFlexWidth := False;
  {$IFDEF CB_HINTER}
  { start hint window functionality }
  HintWin := TPaintLineHintWindow.Create(Self);
  HintWin.Color := {$IFDEF VER80}clYellow{$ELSE}clInfoBk{$ENDIF};
  HintWin.Canvas.Font.Color := {$IFDEF VER80}clBlack{$ELSE}clInfoText{$ENDIF};
  {Check if owner is a form, if not, move up tree of owners until we either find a form or a nil reference}
  TheForm := AOwner;
  while (TheForm <> nil) and not (TheForm is TForm) do TheForm := TheForm.Owner;
  {Record form window reference appropriately}
  if TheForm <> nil then FWindow := TheForm as TForm else FWindow := nil;

  FHintText := False; //as default for combo
  {$ENDIF}
end;

{$IFDEF CB_HINTER}
procedure TPubComboBox.DefaultHandler(var message);
var
  MsgT: TMsg;
begin
  {convert message to TMsg}
  MsgT.message := TMessage(message).Msg;
  inherited{$IFDEF VER80}DefaultHandler(message){$ENDIF};
  if HintWin = nil then Exit;
  {if it's WM_Mousemove then we don't want to hide the hint window}
  if (HintWin.IsHintMsg(MsgT) and (MsgT.message <> WM_MouseMove)) then
    ReleaseIt;
end;
{$ENDIF}

destructor TPubComboBox.Destroy;
begin
  FColorOptionsSet.Free;
  { stop hint window functionality }
  {$IFDEF CB_HINTER}HintWin.Free;{$ENDIF}
  inherited Destroy;
end;

procedure TPubComboBox.SetRequired(Value: Boolean);
begin
  if Value <> FRequired then
    FRequired := Value;
end;

procedure TPubComboBox.Sort;
begin
  with TStringList.Create do
  try
    Assign(Items);
    Sort;
    Items.Text := Text;
  finally
    Free
  end;
end;

function TPubComboBox.Validator: Boolean;
begin
  Result := DoValidate;
end;

procedure TPubComboBox.DoEnter;
begin
  {vybarvi pole pri zamereni}
  Color := ColorOptionsSet.ColorOnFocus;
  Font.Color := ColorOptionsSet.ColorOnFocusFont;

  {vlastni funkce onenter}
  if Assigned(FOnEnter) then
    FOnEnter(Self);
end;

procedure TPubComboBox.DoExit;
var
  S: string;
  CanExit: Boolean;
begin
  {tady zmen barvu pole, kdyz ztracis zamereni}
  Color := ColorOptionsSet.ColorOnNotFocus;
  Font.Color := ColorOptionsSet.ColorOnNotFocusFont;

  {tohle je vlastni onexit}
  if Assigned(FOnExit) then FOnExit(Self);

  {teprve po nem znovu overuji, zda je pole vyplneno dle pozadavku}
  if FRequired and (Text = '') then
  begin
    if AsSigned(FErrorOccur) then
      {uzivatelska reakce na chybu}
      FErrorOccur(Self, msgFieldRequired)
    else
      MessageDlg(msgFieldRequired, mtWarning, [mbOK], 0);
    if CanFocus then SetFocus;
  end
  else
    if Assigned(FValidate) then
    begin
      S := Text;
      FValidate(Self, S, CanExit);
      if not CanExit then
        if CanFocus then SetFocus;
    end;
end;

function TPubComboBox.DoValidate: Boolean;
var
  S: string;
begin
  S := Text;
  if Assigned(FValidate) then
    FValidate(Self, S, Result)
  else
    Result := True; {ostatni obj nemaji vlastni check}

  ColorOptionsSet.Show(Result);
end;

procedure TPubComboBox.CMExit(var Message: TCMExit);
begin
  inherited;
  {je-li pozadovano polozku vyplnit a ona je prazdna, pak znovu}
  {tady zadnou hlasku nezobrazuji}
  if FRequired and (Text = '') then
  begin
    MessageBeep($FFFF);
    if Self.CanFocus then Self.SetFocus;
    Exit;
  end;
  {je-li pozadovana externi validace a neprojde, pak taky znovu}
end;
{$IFDEF CB_HINTER}
procedure TPubComboBox.CMMouseLeave(var message: TMessage);
begin
  inherited;
  ReleaseIt;
end;
{$ENDIF}
procedure TPubComboBox.CMEnter(var Message: TCMEnter);
begin
  inherited;
end;

procedure TPubComboBox.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  if Enabled then
    if Focused then
    begin
      Color := ColorOptionsSet.ColorOnFocus;
      Font.Color := ColorOptionsSet.ColorOnFocusFont;
    end
    else
    begin
      Color := ColorOptionsSet.ColorOnNotFocus;
      Font.Color := ColorOptionsSet.ColorOnNotFocusFont;
    end
  else
  begin
    Color := ColorOptionsSet.ColorOnDisabled;
    Font.Color := ColorOptionsSet.ColorOnDisabledFont;
  end
end;

{ lets see if there is something to do? }

procedure TPubComboBox.ListPick;
begin
  if Assigned(FOnListPick) then FOnListPick(Self) else Change;
end;
{$IFDEF CB_HINTER}
procedure TPubComboBox.MouseMove(Shift: TShiftState; X, Y: Integer);
{ Since this was originally coded for a TListBox, where it makes sense being
  this way, and here it was adapted for a TEdit component, I think this could
  be better off inside the OnEnter event, but, since I'm naturally born lazy,
  I made only the minor modifications needed.  Someone can surely do a better
  job... }
var
  Rct: TRect;
begin
  if FWider and not Focused then
  begin
    FSelectAll := True;
    FWider := False;
  end;

  if not (FHintText) then
  begin {Item hinting is off}
    ReleaseIt;
    inherited{$IFDEF VER80}MouseMove(Shift, X, Y){$ENDIF};
    Exit;
  end;

  { Check if there is an owning window - get out if not}
  { If HintEdit is focused, do not show the tooltip to let user edit it}
  { Also, if Application has no focus, why should we care about showing a tip?}
  if Focused or (FWindow = nil) or (Trim(Text) = '') or
    not Application.Active
  then
    Exit;

  { the line that follows is important, otherwise the next line will calculate}
  { the wrong TextWidth via FWindows' Canvas - Thanks Peter Below (TeamB),}
  { for this handy tip that I found in Borland Newsgroup}
  FWindow.Canvas.Font := Font;
  { Check if the Text is wider then the Width}
  if FWindow.Canvas.TextWidth(Text) > (ClientWidth - 16) then
  begin
    FWider := True;
    if FShowing then Exit;
    { Get the default item coordinates}
    Rct := ClientRect;
    { Stretch it to fit the whole edit text }
    Rct.Right := Rct.Left + FWindow.Canvas.TextWidth(Text) + 5;
    { Fine tune theses values for appearance }
    Rct.Top := Rct.Top - 1; {2}
    Rct.Bottom := Rct.Bottom - 4; {2}
    Rct.Left := Rct.Left - 1; {2}
    { now convert to screen coordinates so that THintWindow can use them}
    Rct.TopLeft := ClientToScreen(Rct.TopLeft);
    Rct.BottomRight := ClientToScreen(Rct.BottomRight);
    { And show it!}
    HintWin.ActivateHint(Rct, Text);
    FShowing := True;
  end
  else
    ReleaseIt;

  inherited{$IFDEF VER80}MouseMove(Shift, X, Y){$ENDIF};
end;

procedure TPubComboBox.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  if FSelectAll and FWider and Focused then
  begin
    SelectAll;
    FSelectAll := False;
  end;
  inherited{$IFDEF VER80}MouseUp(Button, Shift, X, Y){$ENDIF};
end;

procedure TPubComboBox.ReleaseIt;
begin
  HintWin.ReleaseHandle;
  FShowing := False;
end;
{$ENDIF}
procedure TPubComboBox.Clear;
begin
  inherited;
  if Assigned(FOnItemsChanged) then
    FOnItemsChanged(Self);
end;

procedure TPubComboBox.DoDropDownFlexWidth;
var
  V: Integer;
  I: Integer;
  J: Integer;
begin
  if not FDropDownFlexWidth then
    Exit;
  V := 0;
  if Items.Count > 0 then
  begin
    for I := 0 to Items.Count - 1 do
    begin
      if Items.Count > DropDownCount then
        J := Canvas.TextWidth(Items[I]) + 26
      else
        J := Canvas.TextWidth(Items[I]) + 6;
      if J > V then
        V := J;
    end;
    if V < Width then
      V := Width;
    Perform(CB_SETDROPPEDWIDTH, V, 0);
  end;
end;

procedure TPubComboBox.SetAsInteger(const Value: Integer);
begin
  Text := IntToStr(Value);
end;

procedure TPubComboBox.SetItems(const Value: TStrings);
begin
  {$IFDEF VER7UP}inherited{$ENDIF} SetItems(Value);
  if Assigned(FOnItemsChanged) then
    FOnItemsChanged(Self);
  DoDropDownFlexWidth;
end;

{ slightly modified CNCommand handler }

procedure TPubComboBox.CNCommand(var Message: TWMCommand);
begin
  case Message.NotifyCode of
    CBN_SELCHANGE: { in this case only, we will take over }
      begin
        Text := Items[ItemIndex];
        Click;
        ListPick;
      end;
  else { let mom handle the rest }
    inherited;
  end;
end;

procedure TPubComboBox.DropDown;
{invoke dropdown programaticaly}
begin
  // Check whether DropedDown
  if SendMessage(Self.Handle, CB_GETDROPPEDSTATE, 0, 0) <> 1 then
    // nop, so drop it
    SendMessage(Self.Handle, CB_SHOWDROPDOWN, 1, 0)
end;

function TPubComboBox.GetAsInteger: Integer;
begin
  Result := 0;
  if Text = '' then
    Exit;
  try
    Result := StrToInt(Text);
  except
    raise EInvalidTypeConvert.Create(StrExpectedIntegerValue);
  end
end;

procedure TPubComboBox.WndProc(var Message: TMessage);
begin
  case Message.Msg of
    CB_ADDSTRING, CB_INSERTSTRING:
      begin
        inherited WndProc(Message);
        if Assigned(FOnItemsChanged) then
          FOnItemsChanged(Self);
        DoDropDownFlexWidth;
      end;
    else
      inherited WndProc(Message);
  end;
end;

{$IFDEF VER7UP}

{  TPubComboBoxEx  }

constructor TPubComboBoxEx.Create(AOwner: TComponent);
{$IFDEF CB_HINTER}
var
  TheForm: TComponent; {The form that ultimately owns this component}
{$ENDIF}
begin
  inherited Create(AOwner);
  FColorOptionsSet := TColorOptionsSet.Create;
  Color := ColorOptionsSet.ColorOnNotFocus;
  FRequired := False;
  FDropDownFlexWidth := False;
  {$IFDEF CB_HINTER}
  { start hint window functionality }
  HintWin := TPaintLineHintWindow.Create(Self);
  HintWin.Color := {$IFDEF VER80}clYellow{$ELSE}clInfoBk{$ENDIF};
  HintWin.Canvas.Font.Color := {$IFDEF VER80}clBlack{$ELSE}clInfoText{$ENDIF};
  {Check if owner is a form, if not, move up tree of owners until we either find a form or a nil reference}
  TheForm := AOwner;
  while (TheForm <> nil) and not (TheForm is TForm) do TheForm := TheForm.Owner;
  {Record form window reference appropriately}
  if TheForm <> nil then FWindow := TheForm as TForm else FWindow := nil;

  FHintText := False; //as default for combo
  {$ENDIF}
end;

{$IFDEF CB_HINTER}
procedure TPubComboBoxEx.DefaultHandler(var message);
var
  MsgT: TMsg;
begin
  {convert message to TMsg}
  MsgT.message := TMessage(message).Msg;
  inherited{$IFDEF VER80}DefaultHandler(message){$ENDIF};
  if HintWin = nil then Exit;
  {if it's WM_Mousemove then we don't want to hide the hint window}
  if (HintWin.IsHintMsg(MsgT) and (MsgT.message <> WM_MouseMove)) then
    ReleaseIt;
end;
{$ENDIF}

destructor TPubComboBoxEx.Destroy;
begin
  FColorOptionsSet.Free;
  { stop hint window functionality }
  {$IFDEF CB_HINTER}HintWin.Free;{$ENDIF}
  inherited Destroy;
end;

procedure TPubComboBoxEx.SetRequired(Value: Boolean);
begin
  if Value <> FRequired then
    FRequired := Value;
end;

procedure TPubComboBoxEx.Sort;
begin
  with TStringList.Create do
  try
    Assign(Items);
    Sort;
    Items.Text := Text;
  finally
    Free
  end;
end;

function TPubComboBoxEx.Validator: Boolean;
begin
  Result := DoValidate;
end;

procedure TPubComboBoxEx.DoEnter;
begin
  {vybarvi pole pri zamereni}
  Color := ColorOptionsSet.ColorOnFocus;
  Font.Color := ColorOptionsSet.ColorOnFocusFont;

  {vlastni funkce onenter}
  if Assigned(FOnEnter) then
    FOnEnter(Self);
end;

procedure TPubComboBoxEx.DoExit;
var
  S: string;
  CanExit: Boolean;
begin
  {tady zmen barvu pole, kdyz ztracis zamereni}
  Color := ColorOptionsSet.ColorOnNotFocus;
  Font.Color := ColorOptionsSet.ColorOnNotFocusFont;

  {tohle je vlastni onexit}
  if Assigned(FOnExit) then FOnExit(Self);

  {teprve po nem znovu overuji, zda je pole vyplneno dle pozadavku}
  if FRequired and (Text = '') then
  begin
    if AsSigned(FErrorOccur) then
      {uzivatelska reakce na chybu}
      FErrorOccur(Self, msgFieldRequired)
    else
      MessageDlg(msgFieldRequired, mtWarning, [mbOK], 0);
    if CanFocus then SetFocus;
  end
  else
    if Assigned(FValidate) then
    begin
      S := Text;
      FValidate(Self, S, CanExit);
      if not CanExit then
        if CanFocus then SetFocus;
    end;
end;

function TPubComboBoxEx.DoValidate: Boolean;
var
  S: string;
begin
  S := Text;
  if Assigned(FValidate) then
    FValidate(Self, S, Result)
  else
    Result := True; {ostatni obj nemaji vlastni check}

  ColorOptionsSet.Show(Result);
end;

procedure TPubComboBoxEx.CMExit(var Message: TCMExit);
begin
  inherited;
  {je-li pozadovano polozku vyplnit a ona je prazdna, pak znovu}
  {tady zadnou hlasku nezobrazuji}
  if FRequired and (Text = '') then
  begin
    MessageBeep($FFFF);
    if Self.CanFocus then Self.SetFocus;
    Exit;
  end;
  {je-li pozadovana externi validace a neprojde, pak taky znovu}
end;
{$IFDEF CB_HINTER}
procedure TPubComboBoxEx.CMMouseLeave(var message: TMessage);
begin
  inherited;
  ReleaseIt;
end;
{$ENDIF}
procedure TPubComboBoxEx.CMEnter(var Message: TCMEnter);
begin
  inherited;
end;

procedure TPubComboBoxEx.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  if Enabled then
    if Focused then
    begin
      Color := ColorOptionsSet.ColorOnFocus;
      Font.Color := ColorOptionsSet.ColorOnFocusFont;
    end
    else
    begin
      Color := ColorOptionsSet.ColorOnNotFocus;
      Font.Color := ColorOptionsSet.ColorOnNotFocusFont;
    end
  else
  begin
    Color := ColorOptionsSet.ColorOnDisabled;
    Font.Color := ColorOptionsSet.ColorOnDisabledFont;
  end
end;

{ lets see if there is something to do? }

procedure TPubComboBoxEx.ListPick;
begin
  if Assigned(FOnListPick) then FOnListPick(Self) else Change;
end;
{$IFDEF CB_HINTER}
procedure TPubComboBoxEx.MouseMove(Shift: TShiftState; X, Y: Integer);
{ Since this was originally coded for a TListBox, where it makes sense being
  this way, and here it was adapted for a TEdit component, I think this could
  be better off inside the OnEnter event, but, since I'm naturally born lazy,
  I made only the minor modifications needed.  Someone can surely do a better
  job... }
var
  Rct: TRect;
begin
  if FWider and not Focused then
  begin
    FSelectAll := True;
    FWider := False;
  end;

  if not (FHintText) then
  begin {Item hinting is off}
    ReleaseIt;
    inherited{$IFDEF VER80}MouseMove(Shift, X, Y){$ENDIF};
    Exit;
  end;

  { Check if there is an owning window - get out if not}
  { If HintEdit is focused, do not show the tooltip to let user edit it}
  { Also, if Application has no focus, why should we care about showing a tip?}
  if Focused or (FWindow = nil) or (Trim(Text) = '') or
    not Application.Active
  then
    Exit;

  { the line that follows is important, otherwise the next line will calculate}
  { the wrong TextWidth via FWindows' Canvas - Thanks Peter Below (TeamB),}
  { for this handy tip that I found in Borland Newsgroup}
  FWindow.Canvas.Font := Font;
  { Check if the Text is wider then the Width}
  if FWindow.Canvas.TextWidth(Text) > (ClientWidth - 16) then
  begin
    FWider := True;
    if FShowing then Exit;
    { Get the default item coordinates}
    Rct := ClientRect;
    { Stretch it to fit the whole edit text }
    Rct.Right := Rct.Left + FWindow.Canvas.TextWidth(Text) + 5;
    { Fine tune theses values for appearance }
    Rct.Top := Rct.Top - 1; {2}
    Rct.Bottom := Rct.Bottom - 4; {2}
    Rct.Left := Rct.Left - 1; {2}
    { now convert to screen coordinates so that THintWindow can use them}
    Rct.TopLeft := ClientToScreen(Rct.TopLeft);
    Rct.BottomRight := ClientToScreen(Rct.BottomRight);
    { And show it!}
    HintWin.ActivateHint(Rct, Text);
    FShowing := True;
  end
  else
    ReleaseIt;

  inherited{$IFDEF VER80}MouseMove(Shift, X, Y){$ENDIF};
end;

procedure TPubComboBoxEx.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  if FSelectAll and FWider and Focused then
  begin
    SelectAll;
    FSelectAll := False;
  end;
  inherited{$IFDEF VER80}MouseUp(Button, Shift, X, Y){$ENDIF};
end;

procedure TPubComboBoxEx.ReleaseIt;
begin
  HintWin.ReleaseHandle;
  FShowing := False;
end;
{$ENDIF}
procedure TPubComboBoxEx.Clear;
begin
  inherited;
  if Assigned(FOnItemsChanged) then
    FOnItemsChanged(Self);
end;

procedure TPubComboBoxEx.DoDropDownFlexWidth;
var
  V: Integer;
  I: Integer;
  J: Integer;
begin
  if not FDropDownFlexWidth then
    Exit;
  V := 0;
  if Items.Count > 0 then
  begin
    for I := 0 to Items.Count - 1 do
    begin
      if Items.Count > DropDownCount then
        J := Canvas.TextWidth(Items[I]) + 26
      else
        J := Canvas.TextWidth(Items[I]) + 6;
      if J > V then
        V := J;
    end;
    if V < Width then
      V := Width;
    Perform(CB_SETDROPPEDWIDTH, V, 0);
  end;
end;

procedure TPubComboBoxEx.SetAsInteger(const Value: Integer);
begin
  Text := IntToStr(Value);
end;

procedure TPubComboBoxEx.SetItems(const Value: TStrings);
begin
  {$IFDEF VER7UP}inherited{$ENDIF} SetItems(Value);
  if Assigned(FOnItemsChanged) then
    FOnItemsChanged(Self);
  DoDropDownFlexWidth;
end;

{ slightly modified CNCommand handler }

procedure TPubComboBoxEx.CNCommand(var Message: TWMCommand);
begin
  case Message.NotifyCode of
    CBN_SELCHANGE: { in this case only, we will take over }
      begin
        Text := Items[ItemIndex];
        Click;
        ListPick;
      end;
  else { let mom handle the rest }
    inherited;
  end;
end;

procedure TPubComboBoxEx.DropDown;
{invoke dropdown programaticaly}
begin
  // Check whether DropedDown
  if SendMessage(Self.Handle, CB_GETDROPPEDSTATE, 0, 0) <> 1 then
    // nop, so drop it
    SendMessage(Self.Handle, CB_SHOWDROPDOWN, 1, 0)
end;

function TPubComboBoxEx.GetAsInteger: Integer;
begin
  Result := 0;
  if Text = '' then
    Exit;
  try
    Result := StrToInt(Text);
  except
    raise EInvalidTypeConvert.Create(StrExpectedIntegerValue);
  end
end;

procedure TPubComboBoxEx.WndProc(var Message: TMessage);
begin
  case Message.Msg of
    CB_ADDSTRING, CB_INSERTSTRING:
      begin
        inherited WndProc(Message);
        if Assigned(FOnItemsChanged) then
          FOnItemsChanged(Self);
        DoDropDownFlexWidth;
      end;
    else
      inherited WndProc(Message);
  end;
end;

{$ENDIF}

{$IFDEF USE_RXLIB}

{  TPubCurrencyEdit  }

constructor TPubCurrencyEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FColorOptionsSet := TColorOptionsSet.Create;
  Color := ColorOptionsSet.ColorOnNotFocus;
  FRequired := False;
end;

destructor TPubCurrencyEdit.Destroy;
begin
  FColorOptionsSet.Free;
  inherited Destroy;
end;

function TPubCurrencyEdit.DoValidate: Boolean;
var
  S: string;
begin
  S := Text;
  if Assigned(FValidate) then
    FValidate(Self, S, Result)
  else
    Result := True; {ostatni obj nemaji vlastni check}

  ColorOptionsSet.Show(Result);
end;

function TPubCurrencyEdit.Validator: Boolean;
begin
  Result := DoValidate;
end;

procedure TPubCurrencyEdit.SetRequired(Value: Boolean);
begin
  if Value <> FRequired then
    FRequired := Value;
end;

procedure TPubCurrencyEdit.DoEnter;
begin
  {vybarvi pole pri zamereni}
  Color := ColorOptionsSet.ColorOnFocus;
  Font.Color := ColorOptionsSet.ColorOnFocusFont;
  {vlastni funkce onenter}
  if Assigned(FOnEnter) then
    FOnEnter(Self);
end;

procedure TPubCurrencyEdit.DoExit;
begin
  {tady zmen barvu pole, kdyz ztracis zamereni}
  Color := ColorOptionsSet.ColorOnNotFocus;
  Font.Color := ColorOptionsSet.ColorOnNotFocusFont;

  {tohle je vlastni onexit}
  if Assigned(FOnExit) then FOnExit(Self);

  {teprve po nem znovu overuji, zda je pole vyplneno dle pozadavku}
  if FRequired and (Text = '') then
  begin
    if BeepOnError then MessageBeep($FFFF);
    if CanFocus then SetFocus;
  end;

end;

procedure TPubCurrencyEdit.CMExit(var Message: TCMExit);
begin
  inherited;
  {je-li pozadovano polozku vyplnit a ona je prazdna, pak znovu}
  {tady zadnou hlasku nezobrazuji}
  if FRequired and (Text = '') then
  begin
    if BeepOnError then MessageBeep($FFFF);
    if Self.CanFocus then Self.SetFocus;
    Exit;
  end;
end;

procedure TPubCurrencyEdit.CMEnter(var Message: TCMEnter);
begin
  inherited;
end;

procedure TPubCurrencyEdit.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  if Enabled then
    if Focused then
    begin
      Color := ColorOptionsSet.ColorOnFocus;
      Font.Color := ColorOptionsSet.ColorOnFocusFont;
    end
    else
    begin
      Color := ColorOptionsSet.ColorOnNotFocus;
      Font.Color := ColorOptionsSet.ColorOnNotFocusFont;
    end
  else
  begin
    Color := ColorOptionsSet.ColorOnDisabled;
    Font.Color := ColorOptionsSet.ColorOnDisabledFont;
  end
end;

{  TPubDateEdit  }

constructor TPubDateEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FColorOptionsSet := TColorOptionsSet.Create;
  Color := ColorOptionsSet.ColorOnNotFocus;
  FRequired := False;
end;

destructor TPubDateEdit.Destroy;
begin
  FColorOptionsSet.Free;
  inherited Destroy;
end;

procedure TPubDateEdit.SetRequired(Value: Boolean);
begin
  if Value <> FRequired then
    FRequired := Value;
end;

procedure TPubDateEdit.DoEnter;
begin
  {vybarvi pole pri zamereni}
  Color := ColorOptionsSet.ColorOnFocus;
  Font.Color := ColorOptionsSet.ColorOnFocusFont;

  if not AutoSelect then
    if Self.Date = 0 then SelStart := 0
    else SelStart := GetTextLen;

  {vlastni funkce onenter}
  if Assigned(FOnEnter) then
    FOnEnter(Self);
end;

procedure TPubDateEdit.DoExit;
begin
  {tady zmen barvu pole, kdyz ztracis zamereni}
  Color := ColorOptionsSet.ColorOnNotFocus;
  Font.Color := ColorOptionsSet.ColorOnNotFocusFont;

  {tohle je vlastni onexit}
  if Assigned(FOnExit) then FOnExit(Self);

  {teprve po nem znovu overuji, zda je pole vyplneno dle pozadavku}
  if FRequired and (Text = '') then
  begin
    MessageBeep($FFFF);
    if CanFocus then SetFocus;
  end;

end;

procedure TPubDateEdit.CMExit(var Message: TCMExit);
begin
  inherited;
  {je-li pozadovano polozku vyplnit a ona je prazdna, pak znovu}
  {tady zadnou hlasku nezobrazuji}
  if FRequired and (Text = '') then
  begin
    MessageBeep($FFFF);
    if Self.CanFocus then Self.SetFocus;
    Exit;
  end;
end;

procedure TPubDateEdit.CMEnter(var Message: TCMEnter);
begin
  inherited;
end;

procedure TPubDateEdit.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  if Enabled then
    if Focused then
    begin
      Color := ColorOptionsSet.ColorOnFocus;
      Font.Color := ColorOptionsSet.ColorOnFocusFont;
    end
    else
    begin
      Color := ColorOptionsSet.ColorOnNotFocus;
      Font.Color := ColorOptionsSet.ColorOnNotFocusFont;
    end
  else
  begin
    Color := ColorOptionsSet.ColorOnDisabled;
    Font.Color := ColorOptionsSet.ColorOnDisabledFont;
  end
end;

{  TPubFilenameEdit  }

constructor TPubFilenameEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FColorOptionsSet := TColorOptionsSet.Create;
  Color := ColorOptionsSet.ColorOnNotFocus;
  FRequired := False;
end;

destructor TPubFilenameEdit.Destroy;
begin
  FColorOptionsSet.Free;
  inherited Destroy;
end;

procedure TPubFilenameEdit.SetRequired(Value: Boolean);
begin
  if Value <> FRequired then
    FRequired := Value;
end;

procedure TPubFilenameEdit.DoEnter;
begin
  {vybarvi pole pri zamereni}
  Color := ColorOptionsSet.ColorOnFocus;
  Font.Color := ColorOptionsSet.ColorOnFocusFont;

  {vlastni funkce onenter}
  if Assigned(FOnEnter) then
    FOnEnter(Self);
end;

procedure TPubFilenameEdit.DoExit;
begin
  {tady zmen barvu pole, kdyz ztracis zamereni}
  Color := ColorOptionsSet.ColorOnNotFocus;
  Font.Color := ColorOptionsSet.ColorOnNotFocusFont;

  {tohle je vlastni onexit}
  if Assigned(FOnExit) then FOnExit(Self);

  {teprve po nem znovu overuji, zda je pole vyplneno dle pozadavku}
  if FRequired and (Text = '') then
  begin
    MessageBeep($FFFF);
    if CanFocus then SetFocus;
  end;

end;

procedure TPubFilenameEdit.CMExit(var Message: TCMExit);
begin
  inherited;
  {je-li pozadovano polozku vyplnit a ona je prazdna, pak znovu}
  {tady zadnou hlasku nezobrazuji}
  if FRequired and (Text = '') then
  begin
    MessageBeep($FFFF);
    if Self.CanFocus then Self.SetFocus;
    Exit;
  end;
end;

procedure TPubFilenameEdit.CMEnter(var Message: TCMEnter);
begin
  inherited;
end;

procedure TPubFilenameEdit.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  if Enabled then
    if Focused then
    begin
      Color := ColorOptionsSet.ColorOnFocus;
      Font.Color := ColorOptionsSet.ColorOnFocusFont;
    end
    else
    begin
      Color := ColorOptionsSet.ColorOnNotFocus;
      Font.Color := ColorOptionsSet.ColorOnNotFocusFont;
    end
  else
  begin
    Color := ColorOptionsSet.ColorOnDisabled;
    Font.Color := ColorOptionsSet.ColorOnDisabledFont;
  end
end;

{  TPubDirectoryEdit  }

constructor TPubDirectoryEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FColorOptionsSet := TColorOptionsSet.Create;
  Color := ColorOptionsSet.ColorOnNotFocus;
  FRequired := False;
end;

destructor TPubDirectoryEdit.Destroy;
begin
  FColorOptionsSet.Free;
  inherited Destroy;
end;

procedure TPubDirectoryEdit.SetRequired(Value: Boolean);
begin
  if Value <> FRequired then
    FRequired := Value;
end;

procedure TPubDirectoryEdit.DoEnter;
begin
  {vybarvi pole pri zamereni}
  Color := ColorOptionsSet.ColorOnFocus;
  Font.Color := ColorOptionsSet.ColorOnFocusFont;

  {vlastni funkce onenter}
  if Assigned(FOnEnter) then
    FOnEnter(Self);
end;

procedure TPubDirectoryEdit.DoExit;
begin
  {tady zmen barvu pole, kdyz ztracis zamereni}
  Color := ColorOptionsSet.ColorOnNotFocus;
  Font.Color := ColorOptionsSet.ColorOnNotFocusFont;

  {tohle je vlastni onexit}
  if Assigned(FOnExit) then FOnExit(Self);

  {teprve po nem znovu overuji, zda je pole vyplneno dle pozadavku}
  if FRequired and (Text = '') then
  begin
    MessageBeep($FFFF);
    if CanFocus then SetFocus;
  end;

end;

procedure TPubDirectoryEdit.CMExit(var Message: TCMExit);
begin
  inherited;
  {je-li pozadovano polozku vyplnit a ona je prazdna, pak znovu}
  {tady zadnou hlasku nezobrazuji}
  if FRequired and (Text = '') then
  begin
    MessageBeep($FFFF);
    if Self.CanFocus then Self.SetFocus;
    Exit;
  end;
end;

procedure TPubDirectoryEdit.CMEnter(var Message: TCMEnter);
begin
  inherited;
end;

procedure TPubDirectoryEdit.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  if Enabled then
    if Focused then
    begin
      Color := ColorOptionsSet.ColorOnFocus;
      Font.Color := ColorOptionsSet.ColorOnFocusFont;
    end
    else
    begin
      Color := ColorOptionsSet.ColorOnNotFocus;
      Font.Color := ColorOptionsSet.ColorOnNotFocusFont;
    end
  else
  begin
    Color := ColorOptionsSet.ColorOnDisabled;
    Font.Color := ColorOptionsSet.ColorOnDisabledFont;
  end
end;

{  TPubRxCalcEdit  }

constructor TPubRxCalcEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FColorOptionsSet := TColorOptionsSet.Create;
  Color := ColorOptionsSet.ColorOnNotFocus;
  FRequired := False;
end;

destructor TPubRxCalcEdit.Destroy;
begin
  FColorOptionsSet.Free;
  inherited Destroy;
end;

procedure TPubRxCalcEdit.SetRequired(Value: Boolean);
begin
  if Value <> FRequired then
    FRequired := Value;
end;

procedure TPubRxCalcEdit.DoEnter;
begin
  {vybarvi pole pri zamereni}
  Color := ColorOptionsSet.ColorOnFocus;
  Font.Color := ColorOptionsSet.ColorOnFocusFont;

  {vlastni funkce onenter}
  if Assigned(FOnEnter) then
    FOnEnter(Self);
end;

procedure TPubRxCalcEdit.DoExit;
begin
  {tady zmen barvu pole, kdyz ztracis zamereni}
  Color := ColorOptionsSet.ColorOnNotFocus;
  Font.Color := ColorOptionsSet.ColorOnNotFocusFont;

  {tohle je vlastni onexit}
  if Assigned(FOnExit) then FOnExit(Self);

  {teprve po nem znovu overuji, zda je pole vyplneno dle pozadavku}
  if FRequired and (Text = '') then
  begin
    if BeepOnError then MessageBeep($FFFF);
    if CanFocus then SetFocus;
  end;

end;

procedure TPubRxCalcEdit.CMExit(var Message: TCMExit);
begin
  inherited;
  {je-li pozadovano polozku vyplnit a ona je prazdna, pak znovu}
  {tady zadnou hlasku nezobrazuji}
  if FRequired and (Text = '') then
  begin
    if BeepOnError then MessageBeep($FFFF);
    if Self.CanFocus then Self.SetFocus;
    Exit;
  end;
end;

procedure TPubRxCalcEdit.CMEnter(var Message: TCMEnter);
begin
  inherited;
end;

procedure TPubRxCalcEdit.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  if Enabled then
    if Focused then
    begin
      Color := ColorOptionsSet.ColorOnFocus;
      Font.Color := ColorOptionsSet.ColorOnFocusFont;
    end
    else
    begin
      Color := ColorOptionsSet.ColorOnNotFocus;
      Font.Color := ColorOptionsSet.ColorOnNotFocusFont;
    end
  else
  begin
    Color := ColorOptionsSet.ColorOnDisabled;
    Font.Color := ColorOptionsSet.ColorOnDisabledFont;
  end
end;

{  TPubComboEdit  }

constructor TPubComboEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FColorOptionsSet := TColorOptionsSet.Create;
  Color := ColorOptionsSet.ColorOnNotFocus;
  FCapitalize := False;
  FRequired := False;
  FValids := '';
  FMask := '';
end;

function TPubComboEdit.DoValidate: Boolean;
var
  S: string; 
begin
  S := Text;
  if Assigned(FValidate) then
    FValidate(Self, S, Result)
  else
    Result := True; {ostatni obj nemaji vlastni check}

  ColorOptionsSet.Show(Result);
end;

function TPubComboEdit.Validator: Boolean;
begin
  Result := DoValidate;
end;

procedure TPubComboEdit.SetCapitalize(Value: Boolean);
begin
  if Value <> FCapitalize then
    FCapitalize := Value;
end;

destructor TPubComboEdit.Destroy;
begin
  FColorOptionsSet.Free;
  inherited Destroy;
end;

procedure TPubComboEdit.SetRequired(Value: Boolean);
begin
  if Value <> FRequired then
    FRequired := Value;
end;

procedure TPubComboEdit.KeyPress(var Key: Char);
var
  C: string;
  VCH: {$IFDEF VER12UP}TSysCharSet{$ELSE}set of Char{$ENDIF};
  I: Integer;
begin
  {nejprve vyrob mnozinu znaku}
  VCH := [];
  if Length(FValids) > 0 then
    for I := 1 to Length(FValids) do
      {$IFDEF VER12UP}
      VCH := VCH + [FValids[I]];
      {$ELSE}
      Include(VCH, Chr(Ord(FValids[I])));
      {$ENDIF}
  {ohodnot}
  if (not CharInSet(Key, ccControlKeys)) and (VCH <> []) then
    if not CharInSet(Key, VCH) then Key := #0; {Delej, jako by nebyla}

  {jsou-li pozadovana velka pismena na zacatku slov, udelej to tady}
  if FCapitalize then
    if (SelStart = 0) or (Text[SelStart] = ' ') then
    begin
      C := Key;
      C := AnsiUpperCase(C);
      Key := C[1];
    end;

  if Key <> #0 then
    inherited KeyPress(Key);
end;

procedure TPubComboEdit.DoEnter;
var S: string;
begin
  {vybarvi pole pri zamereni}
  Color := ColorOptionsSet.ColorOnFocus;
  Font.Color := ColorOptionsSet.ColorOnFocusFont;
  if AsSigned(FConvert) then
  begin {je-li prislusna rutina}
    S := Text;
    FConvert(Self, eValueToText, S); {preved text na jiny format}
    Text := S;
  end;

  {je-li potreba zaformatovat podle masky, pak jenom pro string}
  if AsSigned(FMaskForm) then
  begin
    S := Text;
    FMaskForm(Self, eValueToText, FMask, S);
    Text := S;
  end;

  {vlastni funkce onenter}
  if Assigned(FOnEnter) then
    FOnEnter(Self);
end;

procedure TPubComboEdit.DoExit;
var S: string;
begin
  {tady zmen barvu pole, kdyz ztracis zamereni}
  Color := ColorOptionsSet.ColorOnNotFocus;
  Font.Color := ColorOptionsSet.ColorOnNotFocusFont;

  {je-li potreba zaformatovat podle masky, pak jenom pro string}
  if AsSigned(FMaskForm) then
  begin
    S := Text;
    FMaskForm(Self, eTextToValue, FMask, S);
    Text := S;
  end;

  if AsSigned(FConvert) then
  begin {je-li prislusna rutina}
    S := Text;
    FConvert(Self, eTextToValue, S); {preved text na jiny format}
    Text := S;
  end;

  {tohle je vlastni onexit}
  if Assigned(FOnExit) then FOnExit(Self);

  {teprve po nem znovu overuji, zda je pole vyplneno dle pozadavku}
  if FRequired and (Text = '') then
  begin
    MessageBeep($FFFF);
    if CanFocus then SetFocus;
  end;
  {teprve po nem znovu overuji, zda je pole vyplneno dle pozadavku}
  if FRequired and (Text = '') then
  begin
    if AsSigned(FErrorOccur) then
      {uzivatelska reakce na chybu}
      FErrorOccur(Self, msgFieldRequired)
    else
      MessageDlg(msgFieldRequired, mtWarning, [mbOK], 0);
    if CanFocus then SetFocus;
  end;
end;

procedure TPubComboEdit.CMExit(var Message: TCMExit);
var
  S: string;
  CanExit: Boolean;
begin
  inherited;
  {je-li pozadovano polozku vyplnit a ona je prazdna, pak znovu}
  {tady zadnou hlasku nezobrazuji}
  if FRequired and (Text = '') then
  begin
    MessageBeep($FFFF);
    if Self.CanFocus then Self.SetFocus;
    Exit;
  end;
  if Assigned(FValidate) then
  begin
    S := Text;
    FValidate(Self, S, CanExit);
    if S <> Text then {in validate can be changed, update it !}
      Text := S;
    if not CanExit then
    begin {fatal error in validate}
      MessageBeep($FFFF);
      if Self.CanFocus then Self.SetFocus;
      Exit;
    end;
  end;
end;

procedure TPubComboEdit.CMEnter(var Message: TCMEnter);
begin
  inherited;
end;

procedure TPubComboEdit.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  if Enabled then
    if Focused then
    begin
      Color := ColorOptionsSet.ColorOnFocus;
      Font.Color := ColorOptionsSet.ColorOnFocusFont;
    end
    else
    begin
      Color := ColorOptionsSet.ColorOnNotFocus;
      Font.Color := ColorOptionsSet.ColorOnNotFocusFont;
    end
  else
  begin
    Color := ColorOptionsSet.ColorOnDisabled;
    Font.Color := ColorOptionsSet.ColorOnDisabledFont;
  end
end;
{$ENDIF USE_RXLIB}

{ TPubSpinButton }

constructor TPubSpinButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csAcceptsControls, csSetCaption] +
    [csFramed, csOpaque];

  FUpButton := CreateButton;
  FDownButton := CreateButton;
  UpGlyph := nil;
  DownGlyph := nil;

  Width := 20;
  Height := 25;
  FFocusedButton := FUpButton;
end;

function TPubSpinButton.CreateButton: TTimerSpeedButton;
begin
  Result := TTimerSpeedButton.Create (Self);
  Result.OnClick := BtnClick;
  Result.OnMouseDown := BtnMouseDown;
  Result.Visible := True;
  Result.Enabled := True;
  Result.TimeBtnState := [tbAllowTimer];
  Result.Parent := Self;
end;

procedure TPubSpinButton.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FFocusControl) then
    FFocusControl := nil;
end;

procedure TPubSpinButton.AdjustSize (var W, H: Integer);
var
  _h, _hz: Integer;
begin
  if (FUpButton = nil) or (csLoading in ComponentState) then Exit;
  if W < 15 then W := 15;
  _h :=  H div 2;
  _hz := H mod 2;
  FUpButton.SetBounds (0, 0, W, _h);
  FDownButton.SetBounds (0, _hz + _h, W, _h);
end;

procedure TPubSpinButton.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var
  W, H: Integer;
begin
  W := AWidth;
  H := AHeight;
  AdjustSize (W, H);
  inherited SetBounds (ALeft, ATop, W, H);
end;

procedure TPubSpinButton.WMSize(var Message: TWMSize);
var
  W, H: Integer;
begin
  inherited;

  { check for minimum size }
  W := Width;
  H := Height;
  AdjustSize (W, H);
  if (W <> Width) or (H <> Height) then
    inherited SetBounds(Left, Top, W, H);
  Message.Result := 0;
end;

procedure TPubSpinButton.WMSetFocus(var Message: TWMSetFocus);
begin
  FFocusedButton.TimeBtnState := FFocusedButton.TimeBtnState + [tbFocusRect];
  FFocusedButton.Invalidate;
end;

procedure TPubSpinButton.WMKillFocus(var Message: TWMKillFocus);
begin
  FFocusedButton.TimeBtnState := FFocusedButton.TimeBtnState - [tbFocusRect];
  FFocusedButton.Invalidate;
end;

procedure TPubSpinButton.KeyDown(var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_UP:
      begin
        SetFocusBtn (FUpButton);
        FUpButton.Click;
      end;
    VK_DOWN:
      begin
        SetFocusBtn (FDownButton);
        FDownButton.Click;
      end;
    VK_SPACE:
      FFocusedButton.Click;
  end;
end;

procedure TPubSpinButton.BtnMouseDown (Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    SetFocusBtn (TTimerSpeedButton (Sender));
    if (FFocusControl <> nil) and FFocusControl.TabStop and 
        FFocusControl.CanFocus and (GetFocus <> FFocusControl.Handle)
    then
      FFocusControl.SetFocus
    else if TabStop and (GetFocus <> Handle) and CanFocus then
      SetFocus;
  end;
end;

procedure TPubSpinButton.BtnClick(Sender: TObject);
begin
  if Sender = FUpButton then
  begin
    if Assigned(FOnUpClick) then FOnUpClick(Self);
  end
  else
    if Assigned(FOnDownClick) then FOnDownClick(Self);
end;

procedure TPubSpinButton.SetFocusBtn (Btn: TTimerSpeedButton);
begin
  if TabStop and CanFocus and  (Btn <> FFocusedButton) then
  begin
    FFocusedButton.TimeBtnState := FFocusedButton.TimeBtnState - [tbFocusRect];
    FFocusedButton := Btn;
    if (GetFocus = Handle) then 
    begin
       FFocusedButton.TimeBtnState := FFocusedButton.TimeBtnState + [tbFocusRect];
       Invalidate;
    end;
  end;
end;

procedure TPubSpinButton.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  Message.Result := DLGC_WANTARROWS;
end;

procedure TPubSpinButton.Loaded;
var
  W, H: Integer;
begin
  inherited Loaded;
  W := Width;
  H := Height;
  AdjustSize (W, H);
  if (W <> Width) or (H <> Height) then
    inherited SetBounds (Left, Top, W, H);
end;

function TPubSpinButton.GetUpGlyph: TBitmap;
begin
  Result := FUpButton.Glyph;
end;

procedure TPubSpinButton.SetUpGlyph(Value: TBitmap);
begin
  if Value <> nil then
    FUpButton.Glyph := Value
  else
  begin
    FUpButton.Glyph.Handle := LoadBitmap({$IFDEF CLR}WinUtils.HInstance{$ELSE}HInstance{$ENDIF}, 'SpinUp');
    FUpButton.NumGlyphs := 1;
    FUpButton.Invalidate;
  end;
end;

function TPubSpinButton.GetUpNumGlyphs: TNumGlyphs;
begin
  Result := FUpButton.NumGlyphs;
end;

procedure TPubSpinButton.SetUpNumGlyphs(Value: TNumGlyphs);
begin
  FUpButton.NumGlyphs := Value;
end;

function TPubSpinButton.GetDownGlyph: TBitmap;
begin
  Result := FDownButton.Glyph;
end;

procedure TPubSpinButton.SetDownGlyph(Value: TBitmap);
begin
  if Value <> nil then
    FDownButton.Glyph := Value
  else
  begin
    FDownButton.Glyph.Handle := LoadBitmap(HInstance, 'SpinDown');
    FUpButton.NumGlyphs := 1;
    FDownButton.Invalidate;
  end;
end;

function TPubSpinButton.GetDownNumGlyphs: TNumGlyphs;
begin
  Result := FDownButton.NumGlyphs;
end;

procedure TPubSpinButton.SetDownNumGlyphs(Value: TNumGlyphs);
begin
  FDownButton.NumGlyphs := Value;
end;

{ TPubSpinEdit }

constructor TPubSpinEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FButton := TPubSpinButton.Create (Self);
  FButton.Width := 16;
  FButton.Height := 18;
  FButton.Visible := True;  
  FButton.Parent := Self;
  FButton.FocusControl := Self;
  FButton.OnUpClick := UpClick;
  FButton.OnDownClick := DownClick;
  Text := '0';
  ControlStyle := ControlStyle - [csSetCaption];
  FIncrement := 1;
  FEditorEnabled := True;
end;

destructor TPubSpinEdit.Destroy;
begin
  FButton := nil;
  inherited Destroy;
end;

procedure TPubSpinEdit.GetChildren(Proc: TGetChildProc; Root: TComponent);
begin
end;

procedure TPubSpinEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if Key = VK_UP then UpClick (Self)
  else if Key = VK_DOWN then DownClick (Self);
  inherited KeyDown(Key, Shift);
end;

procedure TPubSpinEdit.KeyPress(var Key: Char);
begin
  if not IsValidChar(Key) then
  begin
    Key := #0;
    MessageBeep(0)
  end;
  if Key <> #0 then inherited KeyPress(Key);
end;

function TPubSpinEdit.IsValidChar(Key: Char): Boolean;
begin
  Result := 
    {$IFDEF CLR}
    CharInSet(Key, ['+', '-', '0'..'9']) or (Key = DecimalSeparator) or
    ((Key < ' ') and (Key <> Char(VK_RETURN)));
    {$ELSE}
    CharInSet(Key, [{$IFDEF VER15UP}FormatSettings.{$ENDIF}DecimalSeparator, '+', '-', '0'..'9']) or
    ((Key < #32) and (Key <> Chr(VK_RETURN)));
    {$ENDIF}
  if not FEditorEnabled and Result and ((Key >= #32) or
      (Key = Char(VK_BACK)) or (Key = Char(VK_DELETE)))
  then
    Result := False;
end;

procedure TPubSpinEdit.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
{  Params.Style := Params.Style and not WS_BORDER;  }
  Params.Style := Params.Style or ES_MULTILINE or WS_CLIPCHILDREN;
end;

procedure TPubSpinEdit.CreateWnd;
begin
  inherited CreateWnd;
  SetEditRect;
end;

procedure TPubSpinEdit.SetEditRect;
var
  Loc: TRect;
begin
  {$IFDEF CLR}
  Self.Perform(EM_GETRECT, 0, Loc);
  Loc.Bottom := ClientHeight + 1;  {+1 is workaround for windows paint bug}
  Loc.Right := ClientWidth - FButton.Width - 2;
  Loc.Top := 0;  
  Loc.Left := 0;  
  Self.Perform(EM_SETRECTNP, 0, Loc);
  Self.Perform(EM_GETRECT, 0, Loc);  {debug}
  {$ELSE}
  SendMessage(Handle, EM_GETRECT, 0, LongInt(@Loc));
  Loc.Bottom := ClientHeight + 1;  {+1 is workaround for windows paint bug}
  Loc.Right := ClientWidth - FButton.Width - 2;
  Loc.Top := 0;
  Loc.Left := 0;
  SendMessage(Handle, EM_SETRECTNP, 0, LongInt(@Loc));
  SendMessage(Handle, EM_GETRECT, 0, LongInt(@Loc));  {debug}
  {$ENDIF}
end;

procedure TPubSpinEdit.WMSize(var Message: TWMSize);
var
  MinHeight: Integer;
begin
  inherited;
  MinHeight := GetMinHeight;
    { text edit bug: if size to less than minheight, then edit ctrl does
      not display the text }
  if Height < MinHeight then   
    Height := MinHeight
  else
  if FButton <> nil then
  begin
    if NewStyleControls and Ctl3D then
      FButton.SetBounds(Width - FButton.Width - 5, 0, FButton.Width, Height - 5)
    else
      FButton.SetBounds (Width - FButton.Width, 1, FButton.Width, Height - 3);
    SetEditRect;
  end;
end;

function TPubSpinEdit.GetMinHeight: Integer;
var
  DC: HDC;
  SaveFont: HFont;
  I: Integer;
  SysMetrics, Metrics: TTextMetric;
begin
  DC := GetDC(0);
  GetTextMetrics(DC, SysMetrics);
  SaveFont := SelectObject(DC, Font.Handle);
  GetTextMetrics(DC, Metrics);
  SelectObject(DC, SaveFont);
  ReleaseDC(0, DC);
  I := SysMetrics.tmHeight;
  if I > Metrics.tmHeight then I := Metrics.tmHeight;
  Result := Metrics.tmHeight + I div 4 + GetSystemMetrics(SM_CYBORDER) * 4 + 2;
end;

procedure TPubSpinEdit.UpClick (Sender: TObject);
begin
  if ReadOnly then MessageBeep(0)
  else Value := Value + FIncrement;
end;

procedure TPubSpinEdit.DownClick (Sender: TObject);
begin
  if ReadOnly then MessageBeep(0)
  else Value := Value - FIncrement;
end;

procedure TPubSpinEdit.WMPaste(var Message: TWMPaste);
begin
  if not FEditorEnabled or ReadOnly then Exit;
  inherited;
end;

procedure TPubSpinEdit.WMCut(var Message: TWMPaste);
begin
  if not FEditorEnabled or ReadOnly then Exit;
  inherited;
end;

procedure TPubSpinEdit.CMExit(var Message: TCMExit);
begin
  inherited;
  if CheckValue (Value) <> Value then
    SetValue (Value);
end;

function TPubSpinEdit.GetValue: LongInt;
begin
  try
    Result := StrToInt (Text);
  except
    Result := FMinValue;
  end;
end;

procedure TPubSpinEdit.SetValue (NewValue: LongInt);
begin
  Text := IntToStr (CheckValue (NewValue));
end;

function TPubSpinEdit.CheckValue (NewValue: LongInt): LongInt;
begin
  Result := NewValue;
  if (FMaxValue <> FMinValue) then
  begin
    if NewValue < FMinValue then
      Result := FMinValue
    else if NewValue > FMaxValue then
      Result := FMaxValue;
  end;
end;

procedure TPubSpinEdit.CMEnter(var Message: TCMGotFocus);
begin
  if AutoSelect and not (csLButtonDown in ControlState) then
    SelectAll;
  inherited;
end;

{TTimerSpeedButton}

destructor TTimerSpeedButton.Destroy;
begin
  if FRepeatTimer <> nil then
    FRepeatTimer.Free;
  inherited Destroy;
end;

procedure TTimerSpeedButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown (Button, Shift, X, Y);
  if tbAllowTimer in FTimeBtnState then
  begin
    if FRepeatTimer = nil then
      FRepeatTimer := TTimer.Create(Self);

    FRepeatTimer.OnTimer := TimerExpired;
    FRepeatTimer.Interval := InitRepeatPause;
    FRepeatTimer.Enabled  := True;
  end;
end;

procedure TTimerSpeedButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
                                  X, Y: Integer);
begin
  inherited MouseUp (Button, Shift, X, Y);
  if FRepeatTimer <> nil then
    FRepeatTimer.Enabled  := False;
end;

procedure TTimerSpeedButton.TimerExpired(Sender: TObject);
begin
  FRepeatTimer.Interval := RepeatPause;
  if (FState = {$IFDEF USE_RXLIB}rbsDown{$ELSE}bsDown{$ENDIF}) and MouseCapture then
  begin
    try
      Click;
    except
      FRepeatTimer.Enabled := False;
      raise;
    end;
  end;
end;

procedure TTimerSpeedButton.Paint;
var
  R: TRect;
begin
  inherited Paint;
  if tbFocusRect in FTimeBtnState then
  begin
    R := Bounds(0, 0, Width, Height);
    InflateRect(R, -3, -3);
    if FState = {$IFDEF USE_RXLIB}rbsDown{$ELSE}bsDown{$ENDIF} then
      OffsetRect(R, 1, 1);
    DrawFocusRect(Canvas.Handle, R);
  end;
end;

{$IFDEF USE_RXLIB}

{ TPubSpinNumEdit }

procedure TPubSpinNumEdit.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  if Enabled then
    if Focused then
    begin
      Color := ColorOptionsSet.ColorOnFocus;
      Font.Color := ColorOptionsSet.ColorOnFocusFont;
    end
    else
    begin
      Color := ColorOptionsSet.ColorOnNotFocus;
      Font.Color := ColorOptionsSet.ColorOnNotFocusFont;
    end
  else
  begin
    Color := ColorOptionsSet.ColorOnDisabled;
    Font.Color := ColorOptionsSet.ColorOnDisabledFont;
  end
end;

constructor TPubSpinNumEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FColorOptionsSet := TColorOptionsSet.Create;
  Color := ColorOptionsSet.ColorOnNotFocus;
end;

function TPubSpinNumEdit.DoValidate: Boolean;
var
  S: string; 
begin
  S := Text;
  if Assigned(FValidate) then
    FValidate(Self, S, Result)
  else
    Result := True; {ostatni obj nemaji vlastni check}

  ColorOptionsSet.Show(Result);
end;

function TPubSpinNumEdit.Validator: Boolean;
begin
  Result := DoValidate;
end;

destructor TPubSpinNumEdit.Destroy;
begin
  FColorOptionsSet.Free;
  inherited Destroy;
end;

procedure TPubSpinNumEdit.DoEnter;
begin
  {vybarvi pole pri zamereni}
  Color := ColorOptionsSet.ColorOnFocus;
  Font.Color := ColorOptionsSet.ColorOnFocusFont;
  {vlastni funkce onenter}
  if Assigned(FOnEnter) then
    FOnEnter(Self);
end;

procedure TPubSpinNumEdit.DoExit;
begin
  {tady zmen barvu pole, kdyz ztracis zamereni}
  Color := ColorOptionsSet.ColorOnNotFocus;
  Font.Color := ColorOptionsSet.ColorOnNotFocusFont;

  {tohle je vlastni onexit}
  if Assigned(FOnExit) then FOnExit(Self);
end;
{$ENDIF USE_RXLIB}

{ TPubLabeledMaskEdit }

procedure TPubLabeledMaskEdit.SetLabelSpacing(const Value: Integer);
begin
  FLabelSpacing := Value;
  SetLabelPosition(FLabelPosition);
end;

constructor TPubLabeledMaskEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLabelPosition := lpAbove;
  FLabelSpacing := 3;
  FEditLabel := {$IFDEF USE_RXLIB}TRxLabel{$ELSE}{$IFNDEF VER6UP}TLabel{$ELSE}TBoundLabel{$ENDIF}{$ENDIF}.Create(Self);
  {$IFDEF USE_RXLIB }
  FEditLabel.Name := 'SubLabel';  { do not localize }
  {$IFDEF VER6UP}
  FEditLabel.SetSubComponent(True);
  {$ENDIF}
  if Assigned(AOwner) then
    FEditLabel.Caption := AOwner.Name;
  {$ELSE}
    {$IFNDEF VER6UP}
  FEditLabel.Name := 'SubLabel';  { do not localize }
  FEditLabel.SetSubComponent(True);
  if Assigned(AOwner) then
    FEditLabel.Caption := AOwner.Name;
    {$ENDIF}
  {$ENDIF}
  //FEditLabel.FreeNotification(Self);
  FEditLabel.Parent := Parent;
  FEditLabel.ParentFont := ParentFont;
  FEditLabel.Font.Assign(Self.Font);
  FEditLabel.Name := 'EditLabel' + Name;
  FEditLabel.Caption := Self.Name;
  {$IFDEF USE_RXLIB}
  FEditLabel.FocusControl := Self;
  FEditLabel.ShadowPos := spRightBottom;
  FEditLabel.Transparent := True;
  FEditLabel.ShadowSize := 0;
  {$ELSE}
    {$IFNDEF VER6UP}
  FEditLabel.FocusControl := Self;
    {$ENDIF}
  {$ENDIF}
end;

destructor TPubLabeledMaskEdit.Destroy;
begin
  {$IFDEF VER5UP}
  FreeAndNil(FEditLabel);
  {$ELSE}
  FEditLabel.Free;
  FEditLabel := nil;
  {$ENDIF}
  inherited Destroy;
end;

procedure TPubLabeledMaskEdit.SetName(const Value: TComponentName);
begin
  if (csDesigning in ComponentState) and ((FEditlabel.GetTextLen = 0) or
     (CompareText(FEditLabel.Caption, Name) = 0))
  then
    FEditLabel.Caption := Value;
  inherited SetName(Value);
  if csDesigning in ComponentState then
    Text := '';
end;

procedure TPubLabeledMaskEdit.CMBidimodechanged(var Message: TMessage);
begin
  inherited;
  FEditLabel.BiDiMode := BiDiMode;
end;

procedure TPubLabeledMaskEdit.SetLabelPosition(const Value: TLabelPosition);
var
  P: TPoint;
begin
  if FEditLabel = nil then Exit;
  FLabelPosition := Value;
  case Value of
    lpAbove: P := Point(Left, Top - FEditLabel.Height - FLabelSpacing);
    lpBelow: P := Point(Left, Top + Height + FLabelSpacing);
    lpLeft : P := Point(Left - FEditLabel.Width - FLabelSpacing,
                    Top + ((Height - FEditLabel.Height) div 2));
    lpRight: P := Point(Left + Width + FLabelSpacing,
                    Top + ((Height - FEditLabel.Height) div 2));
  end;
  FEditLabel.SetBounds(P.x, P.y, FEditLabel.Width, FEditLabel.Height);
end;

procedure TPubLabeledMaskEdit.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  if FEditLabel = nil then Exit;
  FEditLabel.Parent := AParent;
  FEditLabel.Visible := True;
end;

procedure TPubLabeledMaskEdit.CMEnabledchanged(var Message: TMessage);
begin
  inherited;
  FEditLabel.Enabled := Enabled;
end;

procedure TPubLabeledMaskEdit.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  SetLabelPosition(FLabelPosition);
end;

procedure TPubLabeledMaskEdit.CMVisiblechanged(var Message: TMessage);
begin
  inherited;
  FEditLabel.Visible := Visible;
end;

function TPubLabeledMaskEdit.GetLabelShowsRequired: Boolean;
begin
  Result := (FEditLabel.Font.Color = clPurple) and (fsUnderline in FEditLabel.Font.Style);
end;

procedure TPubLabeledMaskEdit.SetLabelShowsRequired(const Value: Boolean);
begin
  if Value then
  begin
    FEditLabel.Font.Color := clPurple;
    FEditLabel.Font.Style := [fsUnderline];
  end
  else
  begin
    FEditLabel.Font.Color := clWindowText;
    FEditLabel.Font.Style := [];
  end;
end;

{$IFDEF USE_RXLIB}

{ TPubLabeledCurrencyEdit }

procedure TPubLabeledCurrencyEdit.SetLabelSpacing(const Value: Integer);
begin
  FLabelSpacing := Value;
  SetLabelPosition(FLabelPosition);
end;

constructor TPubLabeledCurrencyEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLabelPosition := lpAbove;
  FLabelSpacing := 3;
  FEditLabel := TRxLabel.Create(Self);
  FEditLabel.Name := 'SubLabel';  { do not localize }
  {$IFDEF VER6UP}
  FEditLabel.SetSubComponent(True);
  {$ENDIF}
  if Assigned(AOwner) then
    FEditLabel.Caption := AOwner.Name;
  //FEditLabel.FreeNotification(Self);
  FEditLabel.Parent := Parent;
  FEditLabel.ParentFont := ParentFont;
  FEditLabel.Font.Assign(Self.Font);
  FEditLabel.Name := 'EditLabel' + Name;
  FEditLabel.Caption := Self.Name;
  FEditLabel.FocusControl := Self;
  FEditLabel.ShadowPos := spRightBottom;
  FEditLabel.Transparent := True;
  FEditLabel.ShadowSize := 0;
end;

destructor TPubLabeledCurrencyEdit.Destroy;
begin
  {$IFDEF VER5UP}
  FreeAndNil(FEditLabel);
  {$ELSE}
  FEditLabel.Free;
  FEditLabel := nil;
  {$ENDIF}
  inherited Destroy;
end;

procedure TPubLabeledCurrencyEdit.SetName(const Value: TComponentName);
begin
  if (csDesigning in ComponentState) and ((FEditlabel.GetTextLen = 0) or
     (CompareText(FEditLabel.Caption, Name) = 0))
  then
    FEditLabel.Caption := Value;
  inherited SetName(Value);
  if csDesigning in ComponentState then
    Text := '';
end;

procedure TPubLabeledCurrencyEdit.CMBidimodechanged(var Message: TMessage);
begin
  inherited;
  FEditLabel.BiDiMode := BiDiMode;
end;

procedure TPubLabeledCurrencyEdit.SetLabelPosition(const Value: TLabelPosition);
var
  P: TPoint;
begin
  if FEditLabel = nil then Exit;
  FLabelPosition := Value;
  case Value of
    lpAbove: P := Point(Left, Top - FEditLabel.Height - FLabelSpacing);
    lpBelow: P := Point(Left, Top + Height + FLabelSpacing);
    lpLeft : P := Point(Left - FEditLabel.Width - FLabelSpacing,
                    Top + ((Height - FEditLabel.Height) div 2));
    lpRight: P := Point(Left + Width + FLabelSpacing,
                    Top + ((Height - FEditLabel.Height) div 2));
  end;
  FEditLabel.SetBounds(P.x, P.y, FEditLabel.Width, FEditLabel.Height);
end;

procedure TPubLabeledCurrencyEdit.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  if FEditLabel = nil then Exit;
  FEditLabel.Parent := AParent;
  FEditLabel.Visible := True;
end;

procedure TPubLabeledCurrencyEdit.CMEnabledchanged(var Message: TMessage);
begin
  inherited;
  FEditLabel.Enabled := Enabled;
end;

procedure TPubLabeledCurrencyEdit.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  SetLabelPosition(FLabelPosition);
end;

procedure TPubLabeledCurrencyEdit.CMVisiblechanged(var Message: TMessage);
begin
  inherited;
  FEditLabel.Visible := Visible;
end;

function TPubLabeledCurrencyEdit.GetLabelShowsRequired: Boolean;
begin
  Result := (FEditLabel.Font.Color = clPurple) and (fsUnderline in FEditLabel.Font.Style);
end;

procedure TPubLabeledCurrencyEdit.SetLabelShowsRequired(const Value: Boolean);
begin
  if Value then
  begin
    FEditLabel.Font.Color := clPurple;
    FEditLabel.Font.Style := [fsUnderline];
  end
  else
  begin
    FEditLabel.Font.Color := clWindowText;
    FEditLabel.Font.Style := [];
  end;
end;

{ TPubLabeledDateEdit }

procedure TPubLabeledDateEdit.SetLabelSpacing(const Value: Integer);
begin
  FLabelSpacing := Value;
  SetLabelPosition(FLabelPosition);
end;

constructor TPubLabeledDateEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLabelPosition := lpAbove;
  FLabelSpacing := 3;
  FEditLabel := TRxLabel.Create(Self);
  FEditLabel.Name := 'SubLabel';  { do not localize }
  {$IFDEF VER6UP}
  FEditLabel.SetSubComponent(True);
  {$ENDIF}
  if Assigned(AOwner) then
    FEditLabel.Caption := AOwner.Name;
  //FEditLabel.FreeNotification(Self);
  FEditLabel.Parent := Parent;
  FEditLabel.ParentFont := ParentFont;
  FEditLabel.Font.Assign(Self.Font);
  FEditLabel.Name := 'EditLabel' + Name;
  FEditLabel.Caption := Self.Name;
  FEditLabel.FocusControl := Self;
  FEditLabel.ShadowPos := spRightBottom;
  FEditLabel.Transparent := True;
  FEditLabel.ShadowSize := 0;
end;

destructor TPubLabeledDateEdit.Destroy;
begin
  {$IFDEF VER5UP}
  FreeAndNil(FEditLabel);
  {$ELSE}
  FEditLabel.Free;
  FEditLabel := nil;
  {$ENDIF}
  inherited Destroy;
end;

procedure TPubLabeledDateEdit.SetName(const Value: TComponentName);
begin
  if (csDesigning in ComponentState) and ((FEditlabel.GetTextLen = 0) or
     (CompareText(FEditLabel.Caption, Name) = 0))
  then
    FEditLabel.Caption := Value;
  inherited SetName(Value);
  if csDesigning in ComponentState then
    Text := '';
end;

procedure TPubLabeledDateEdit.CMBidimodechanged(var Message: TMessage);
begin
  inherited;
  FEditLabel.BiDiMode := BiDiMode;
end;

procedure TPubLabeledDateEdit.SetLabelPosition(const Value: TLabelPosition);
var
  P: TPoint;
begin
  if FEditLabel = nil then Exit;
  FLabelPosition := Value;
  case Value of
    lpAbove: P := Point(Left, Top - FEditLabel.Height - FLabelSpacing);
    lpBelow: P := Point(Left, Top + Height + FLabelSpacing);
    lpLeft : P := Point(Left - FEditLabel.Width - FLabelSpacing,
                    Top + ((Height - FEditLabel.Height) div 2));
    lpRight: P := Point(Left + Width + FLabelSpacing,
                    Top + ((Height - FEditLabel.Height) div 2));
  end;
  FEditLabel.SetBounds(P.x, P.y, FEditLabel.Width, FEditLabel.Height);
end;

procedure TPubLabeledDateEdit.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  if FEditLabel = nil then Exit;
  FEditLabel.Parent := AParent;
  FEditLabel.Visible := True;
end;

procedure TPubLabeledDateEdit.CMEnabledchanged(var Message: TMessage);
begin
  inherited;
  FEditLabel.Enabled := Enabled;
end;

procedure TPubLabeledDateEdit.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  SetLabelPosition(FLabelPosition);
end;

procedure TPubLabeledDateEdit.CMVisiblechanged(var Message: TMessage);
begin
  inherited;
  FEditLabel.Visible := Visible;
end;

function TPubLabeledDateEdit.GetLabelShowsRequired: Boolean;
begin
  Result := (FEditLabel.Font.Color = clPurple) and (fsUnderline in FEditLabel.Font.Style);
end;

procedure TPubLabeledDateEdit.SetLabelShowsRequired(const Value: Boolean);
begin
  if Value then
  begin
    FEditLabel.Font.Color := clPurple;
    FEditLabel.Font.Style := [fsUnderline];
  end
  else
  begin
    FEditLabel.Font.Color := clWindowText;
    FEditLabel.Font.Style := [];
  end;
end;

{ TPubLabeledFilenameEdit }

procedure TPubLabeledFilenameEdit.SetLabelSpacing(const Value: Integer);
begin
  FLabelSpacing := Value;
  SetLabelPosition(FLabelPosition);
end;

constructor TPubLabeledFilenameEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLabelPosition := lpAbove;
  FLabelSpacing := 3;
  FEditLabel := TRxLabel.Create(Self);
  FEditLabel.Name := 'SubLabel';  { do not localize }
  {$IFDEF VER6UP}
  FEditLabel.SetSubComponent(True);
  {$ENDIF}
  if Assigned(AOwner) then
    FEditLabel.Caption := AOwner.Name;
  //FEditLabel.FreeNotification(Self);
  FEditLabel.Parent := Parent;
  FEditLabel.ParentFont := ParentFont;
  FEditLabel.Font.Assign(Self.Font);
  FEditLabel.Name := 'EditLabel' + Name;
  FEditLabel.Caption := Self.Name;
  FEditLabel.FocusControl := Self;
  FEditLabel.ShadowPos := spRightBottom;
  FEditLabel.Transparent := True;
  FEditLabel.ShadowSize := 0;
end;

destructor TPubLabeledFilenameEdit.Destroy;
begin
  {$IFDEF VER5UP}
  FreeAndNil(FEditLabel);
  {$ELSE}
  FEditLabel.Free;
  FEditLabel := nil;
  {$ENDIF}
  inherited Destroy;
end;

procedure TPubLabeledFilenameEdit.SetName(const Value: TComponentName);
begin
  if (csDesigning in ComponentState) and ((FEditlabel.GetTextLen = 0) or
     (CompareText(FEditLabel.Caption, Name) = 0))
  then
    FEditLabel.Caption := Value;
  inherited SetName(Value);
  if csDesigning in ComponentState then
    Text := '';
end;

procedure TPubLabeledFilenameEdit.CMBidimodechanged(var Message: TMessage);
begin
  inherited;
  FEditLabel.BiDiMode := BiDiMode;
end;

procedure TPubLabeledFilenameEdit.SetLabelPosition(const Value: TLabelPosition);
var
  P: TPoint;
begin
  if FEditLabel = nil then Exit;
  FLabelPosition := Value;
  case Value of
    lpAbove: P := Point(Left, Top - FEditLabel.Height - FLabelSpacing);
    lpBelow: P := Point(Left, Top + Height + FLabelSpacing);
    lpLeft : P := Point(Left - FEditLabel.Width - FLabelSpacing,
                    Top + ((Height - FEditLabel.Height) div 2));
    lpRight: P := Point(Left + Width + FLabelSpacing,
                    Top + ((Height - FEditLabel.Height) div 2));
  end;
  FEditLabel.SetBounds(P.x, P.y, FEditLabel.Width, FEditLabel.Height);
end;

procedure TPubLabeledFilenameEdit.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  if FEditLabel = nil then Exit;
  FEditLabel.Parent := AParent;
  FEditLabel.Visible := True;
end;

procedure TPubLabeledFilenameEdit.CMEnabledchanged(var Message: TMessage);
begin
  inherited;
  FEditLabel.Enabled := Enabled;
end;

procedure TPubLabeledFilenameEdit.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  SetLabelPosition(FLabelPosition);
end;

procedure TPubLabeledFilenameEdit.CMVisiblechanged(var Message: TMessage);
begin
  inherited;
  FEditLabel.Visible := Visible;
end;

function TPubLabeledFilenameEdit.GetLabelShowsRequired: Boolean;
begin
  Result := (FEditLabel.Font.Color = clPurple) and (fsUnderline in FEditLabel.Font.Style);
end;

procedure TPubLabeledFilenameEdit.SetLabelShowsRequired(const Value: Boolean);
begin
  if Value then
  begin
    FEditLabel.Font.Color := clPurple;
    FEditLabel.Font.Style := [fsUnderline];
  end
  else
  begin
    FEditLabel.Font.Color := clWindowText;
    FEditLabel.Font.Style := [];
  end;
end;

{ TPubLabeledDirectoryEdit }

procedure TPubLabeledDirectoryEdit.SetLabelSpacing(const Value: Integer);
begin
  FLabelSpacing := Value;
  SetLabelPosition(FLabelPosition);
end;

constructor TPubLabeledDirectoryEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLabelPosition := lpAbove;
  FLabelSpacing := 3;
  FEditLabel := TRxLabel.Create(Self);
  FEditLabel.Name := 'SubLabel';  { do not localize }
  {$IFDEF VER6UP}
  FEditLabel.SetSubComponent(True);
  {$ENDIF}
  if Assigned(AOwner) then
    FEditLabel.Caption := AOwner.Name;
  //FEditLabel.FreeNotification(Self);
  FEditLabel.Parent := Parent;
  FEditLabel.ParentFont := ParentFont;
  FEditLabel.Font.Assign(Self.Font);
  FEditLabel.Name := 'EditLabel' + Name;
  FEditLabel.Caption := Self.Name;
  FEditLabel.FocusControl := Self;
  FEditLabel.ShadowPos := spRightBottom;
  FEditLabel.Transparent := True;
  FEditLabel.ShadowSize := 0;
end;

destructor TPubLabeledDirectoryEdit.Destroy;
begin
  {$IFDEF VER5UP}
  FreeAndNil(FEditLabel);
  {$ELSE}
  FEditLabel.Free;
  FEditLabel := nil;
  {$ENDIF}
  inherited Destroy;
end;

procedure TPubLabeledDirectoryEdit.SetName(const Value: TComponentName);
begin
  if (csDesigning in ComponentState) and ((FEditlabel.GetTextLen = 0) or
     (CompareText(FEditLabel.Caption, Name) = 0))
  then
    FEditLabel.Caption := Value;
  inherited SetName(Value);
  if csDesigning in ComponentState then
    Text := '';
end;

procedure TPubLabeledDirectoryEdit.CMBidimodechanged(var Message: TMessage);
begin
  inherited;
  FEditLabel.BiDiMode := BiDiMode;
end;

procedure TPubLabeledDirectoryEdit.SetLabelPosition(const Value: TLabelPosition);
var
  P: TPoint;
begin
  if FEditLabel = nil then Exit;
  FLabelPosition := Value;
  case Value of
    lpAbove: P := Point(Left, Top - FEditLabel.Height - FLabelSpacing);
    lpBelow: P := Point(Left, Top + Height + FLabelSpacing);
    lpLeft : P := Point(Left - FEditLabel.Width - FLabelSpacing,
                    Top + ((Height - FEditLabel.Height) div 2));
    lpRight: P := Point(Left + Width + FLabelSpacing,
                    Top + ((Height - FEditLabel.Height) div 2));
  end;
  FEditLabel.SetBounds(P.x, P.y, FEditLabel.Width, FEditLabel.Height);
end;

procedure TPubLabeledDirectoryEdit.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  if FEditLabel = nil then Exit;
  FEditLabel.Parent := AParent;
  FEditLabel.Visible := True;
end;

procedure TPubLabeledDirectoryEdit.CMEnabledchanged(var Message: TMessage);
begin
  inherited;
  FEditLabel.Enabled := Enabled;
end;

procedure TPubLabeledDirectoryEdit.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  SetLabelPosition(FLabelPosition);
end;

procedure TPubLabeledDirectoryEdit.CMVisiblechanged(var Message: TMessage);
begin
  inherited;
  FEditLabel.Visible := Visible;
end;

function TPubLabeledDirectoryEdit.GetLabelShowsRequired: Boolean;
begin
  Result := (FEditLabel.Font.Color = clPurple) and (fsUnderline in FEditLabel.Font.Style);
end;

procedure TPubLabeledDirectoryEdit.SetLabelShowsRequired(const Value: Boolean);
begin
  if Value then
  begin
    FEditLabel.Font.Color := clPurple;
    FEditLabel.Font.Style := [fsUnderline];
  end
  else
  begin
    FEditLabel.Font.Color := clWindowText;
    FEditLabel.Font.Style := [];
  end;
end;

{ TPubLabeledComboEdit }

procedure TPubLabeledComboEdit.SetLabelSpacing(const Value: Integer);
begin
  FLabelSpacing := Value;
  SetLabelPosition(FLabelPosition);
end;

constructor TPubLabeledComboEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLabelPosition := lpAbove;
  FLabelSpacing := 3;
  FEditLabel := TRxLabel.Create(Self);
  FEditLabel.Name := 'SubLabel';  { do not localize }
  {$IFDEF VER6UP}
  FEditLabel.SetSubComponent(True);
  {$ENDIF}
  if Assigned(AOwner) then
    FEditLabel.Caption := AOwner.Name;
  //FEditLabel.FreeNotification(Self);
  FEditLabel.Parent := Parent;
  FEditLabel.ParentFont := ParentFont;
  FEditLabel.Font.Assign(Self.Font);
  FEditLabel.Name := 'EditLabel' + Name;
  FEditLabel.Caption := Self.Name;
  FEditLabel.FocusControl := Self;
  FEditLabel.ShadowPos := spRightBottom;
  FEditLabel.Transparent := True;
  FEditLabel.ShadowSize := 0;
end;

destructor TPubLabeledComboEdit.Destroy;
begin
  {$IFDEF VER5UP}
  FreeAndNil(FEditLabel);
  {$ELSE}
  FEditLabel.Free;
  FEditLabel := nil;
  {$ENDIF}
  inherited Destroy;
end;

procedure TPubLabeledComboEdit.SetName(const Value: TComponentName);
begin
  if (csDesigning in ComponentState) and ((FEditlabel.GetTextLen = 0) or
     (CompareText(FEditLabel.Caption, Name) = 0))
  then
    FEditLabel.Caption := Value;
  inherited SetName(Value);
  if csDesigning in ComponentState then
    Text := '';
end;

procedure TPubLabeledComboEdit.CMBidimodechanged(var Message: TMessage);
begin
  inherited;
  FEditLabel.BiDiMode := BiDiMode;
end;

procedure TPubLabeledComboEdit.SetLabelPosition(const Value: TLabelPosition);
var
  P: TPoint;
begin
  if FEditLabel = nil then Exit;
  FLabelPosition := Value;
  case Value of
    lpAbove: P := Point(Left, Top - FEditLabel.Height - FLabelSpacing);
    lpBelow: P := Point(Left, Top + Height + FLabelSpacing);
    lpLeft : P := Point(Left - FEditLabel.Width - FLabelSpacing,
                    Top + ((Height - FEditLabel.Height) div 2));
    lpRight: P := Point(Left + Width + FLabelSpacing,
                    Top + ((Height - FEditLabel.Height) div 2));
  end;
  FEditLabel.SetBounds(P.x, P.y, FEditLabel.Width, FEditLabel.Height);
end;

procedure TPubLabeledComboEdit.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  if FEditLabel = nil then Exit;
  FEditLabel.Parent := AParent;
  FEditLabel.Visible := True;
end;

procedure TPubLabeledComboEdit.CMEnabledchanged(var Message: TMessage);
begin
  inherited;
  FEditLabel.Enabled := Enabled;
end;

procedure TPubLabeledComboEdit.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  SetLabelPosition(FLabelPosition);
end;

procedure TPubLabeledComboEdit.CMVisiblechanged(var Message: TMessage);
begin
  inherited;
  FEditLabel.Visible := Visible;
end;

function TPubLabeledComboEdit.GetLabelShowsRequired: Boolean;
begin
  Result := (FEditLabel.Font.Color = clPurple) and (fsUnderline in FEditLabel.Font.Style);
end;

procedure TPubLabeledComboEdit.SetLabelShowsRequired(const Value: Boolean);
begin
  if Value then
  begin
    FEditLabel.Font.Color := clPurple;
    FEditLabel.Font.Style := [fsUnderline];
  end
  else
  begin
    FEditLabel.Font.Color := clWindowText;
    FEditLabel.Font.Style := [];
  end;
end;

{ TPubLabeledSpinNumEdit }

procedure TPubLabeledSpinNumEdit.SetLabelSpacing(const Value: Integer);
begin
  FLabelSpacing := Value;
  SetLabelPosition(FLabelPosition);
end;

constructor TPubLabeledSpinNumEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLabelPosition := lpAbove;
  FLabelSpacing := 3;
  FEditLabel := TRxLabel.Create(Self);
  FEditLabel.Name := 'SubLabel';  { do not localize }
  {$IFDEF VER6UP}
  FEditLabel.SetSubComponent(True);
  {$ENDIF}
  if Assigned(AOwner) then
    FEditLabel.Caption := AOwner.Name;
  //FEditLabel.FreeNotification(Self);
  FEditLabel.Parent := Parent;
  FEditLabel.ParentFont := ParentFont;
  FEditLabel.Font.Assign(Self.Font);
  FEditLabel.Name := 'EditLabel' + Name;
  FEditLabel.Caption := Self.Name;
  FEditLabel.FocusControl := Self;
  FEditLabel.ShadowPos := spRightBottom;
  FEditLabel.Transparent := True;
  FEditLabel.ShadowSize := 0;
end;

destructor TPubLabeledSpinNumEdit.Destroy;
begin
  {$IFDEF VER5UP}
  FreeAndNil(FEditLabel);
  {$ELSE}
  FEditLabel.Free;
  FEditLabel := nil;
  {$ENDIF}
  inherited Destroy;
end;

procedure TPubLabeledSpinNumEdit.SetName(const Value: TComponentName);
begin
  if (csDesigning in ComponentState) and ((FEditlabel.GetTextLen = 0) or
     (CompareText(FEditLabel.Caption, Name) = 0))
  then
    FEditLabel.Caption := Value;
  inherited SetName(Value);
  if csDesigning in ComponentState then
    Text := '';
end;

procedure TPubLabeledSpinNumEdit.CMBidimodechanged(var Message: TMessage);
begin
  inherited;
  FEditLabel.BiDiMode := BiDiMode;
end;

procedure TPubLabeledSpinNumEdit.SetLabelPosition(const Value: TLabelPosition);
var
  P: TPoint;
begin
  if FEditLabel = nil then Exit;
  FLabelPosition := Value;
  case Value of
    lpAbove: P := Point(Left, Top - FEditLabel.Height - FLabelSpacing);
    lpBelow: P := Point(Left, Top + Height + FLabelSpacing);
    lpLeft : P := Point(Left - FEditLabel.Width - FLabelSpacing,
                    Top + ((Height - FEditLabel.Height) div 2));
    lpRight: P := Point(Left + Width + FLabelSpacing,
                    Top + ((Height - FEditLabel.Height) div 2));
  end;
  FEditLabel.SetBounds(P.x, P.y, FEditLabel.Width, FEditLabel.Height);
end;

procedure TPubLabeledSpinNumEdit.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  if FEditLabel = nil then Exit;
  FEditLabel.Parent := AParent;
  FEditLabel.Visible := True;
end;

procedure TPubLabeledSpinNumEdit.CMEnabledchanged(var Message: TMessage);
begin
  inherited;
  FEditLabel.Enabled := Enabled;
end;

procedure TPubLabeledSpinNumEdit.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  SetLabelPosition(FLabelPosition);
end;

procedure TPubLabeledSpinNumEdit.CMVisiblechanged(var Message: TMessage);
begin
  inherited;
  FEditLabel.Visible := Visible;
end;

function TPubLabeledSpinNumEdit.GetLabelShowsRequired: Boolean;
begin
  Result := (FEditLabel.Font.Color = clPurple) and (fsUnderline in FEditLabel.Font.Style);
end;

procedure TPubLabeledSpinNumEdit.SetLabelShowsRequired(const Value: Boolean);
begin
  if Value then
  begin
    FEditLabel.Font.Color := clPurple;
    FEditLabel.Font.Style := [fsUnderline];
  end
  else
  begin
    FEditLabel.Font.Color := clWindowText;
    FEditLabel.Font.Style := [];
  end;
end;

{ TPubLabeledRxCalcEdit }

procedure TPubLabeledRxCalcEdit.SetLabelSpacing(const Value: Integer);
begin
  FLabelSpacing := Value;
  SetLabelPosition(FLabelPosition);
end;

constructor TPubLabeledRxCalcEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLabelPosition := lpAbove;
  FLabelSpacing := 3;
  FEditLabel := TRxLabel.Create(Self);
  FEditLabel.Name := 'SubLabel';  { do not localize }
  {$IFDEF VER6UP}
  FEditLabel.SetSubComponent(True);
  {$ENDIF}
  if Assigned(AOwner) then
    FEditLabel.Caption := AOwner.Name;
  //FEditLabel.FreeNotification(Self);
  FEditLabel.Parent := Parent;
  FEditLabel.ParentFont := ParentFont;
  FEditLabel.Font.Assign(Self.Font);
  FEditLabel.Name := 'EditLabel' + Name;
  FEditLabel.Caption := Self.Name;
  FEditLabel.FocusControl := Self;
  FEditLabel.ShadowPos := spRightBottom;
  FEditLabel.Transparent := True;
  FEditLabel.ShadowSize := 0;
end;

destructor TPubLabeledRxCalcEdit.Destroy;
begin
  {$IFDEF VER5UP}
  FreeAndNil(FEditLabel);
  {$ELSE}
  FEditLabel.Free;
  FEditLabel := nil;
  {$ENDIF}
  inherited Destroy;
end;

procedure TPubLabeledRxCalcEdit.SetName(const Value: TComponentName);
begin
  if (csDesigning in ComponentState) and ((FEditlabel.GetTextLen = 0) or
     (CompareText(FEditLabel.Caption, Name) = 0))
  then
    FEditLabel.Caption := Value;
  inherited SetName(Value);
  if csDesigning in ComponentState then
    Text := '';
end;

procedure TPubLabeledRxCalcEdit.CMBidimodechanged(var Message: TMessage);
begin
  inherited;
  FEditLabel.BiDiMode := BiDiMode;
end;

procedure TPubLabeledRxCalcEdit.SetLabelPosition(const Value: TLabelPosition);
var
  P: TPoint;
begin
  if FEditLabel = nil then Exit;
  FLabelPosition := Value;
  case Value of
    lpAbove: P := Point(Left, Top - FEditLabel.Height - FLabelSpacing);
    lpBelow: P := Point(Left, Top + Height + FLabelSpacing);
    lpLeft : P := Point(Left - FEditLabel.Width - FLabelSpacing,
                    Top + ((Height - FEditLabel.Height) div 2));
    lpRight: P := Point(Left + Width + FLabelSpacing,
                    Top + ((Height - FEditLabel.Height) div 2));
  end;
  FEditLabel.SetBounds(P.x, P.y, FEditLabel.Width, FEditLabel.Height);
end;

procedure TPubLabeledRxCalcEdit.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  if FEditLabel = nil then Exit;
  FEditLabel.Parent := AParent;
  FEditLabel.Visible := True;
end;

procedure TPubLabeledRxCalcEdit.CMEnabledchanged(var Message: TMessage);
begin
  inherited;
  FEditLabel.Enabled := Enabled;
end;

procedure TPubLabeledRxCalcEdit.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  SetLabelPosition(FLabelPosition);
end;

procedure TPubLabeledRxCalcEdit.CMVisiblechanged(var Message: TMessage);
begin
  inherited;
  FEditLabel.Visible := Visible;
end;

function TPubLabeledRxCalcEdit.GetLabelShowsRequired: Boolean;
begin
  Result := (FEditLabel.Font.Color = clPurple) and (fsUnderline in FEditLabel.Font.Style);
end;

procedure TPubLabeledRxCalcEdit.SetLabelShowsRequired(const Value: Boolean);
begin
  if Value then
  begin
    FEditLabel.Font.Color := clPurple;
    FEditLabel.Font.Style := [fsUnderline];
  end
  else
  begin
    FEditLabel.Font.Color := clWindowText;
    FEditLabel.Font.Style := [];
  end;
end;
{$ENDIF USE_RXLIB}

{$IFDEF USE_VG2LIB}

{ TPub2Edit }

procedure TPub2Edit.CreateParams(var Params: TCreateParams);
const
  Alignments: array[TAlignment] of Cardinal = (ES_LEFT, ES_RIGHT, ES_CENTER);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or Alignments[FAlignment];
end;

constructor TPub2Edit.Create(AOwner: TComponent);
var
  TheForm: TComponent; {The form that ultimately owns this component}
begin
  inherited Create(AOwner);
  FColorOptionsSet := TColorOptionsSet.Create;
  Color := ColorOptionsSet.ColorOnNotFocus;
  FTypeEdit := teString;
  FCapitalize := False;
  FRequired := False;
  FValidChars := '';
  FMask := '';
  FAlignMent := taLeftJustify;
  {}
{$IfDef UseAutocomplete}
  FAutocomplete := TAutocompleteText.Create;
{$EndIf}
  {}
  HintWin := TPaintLineHintWindow.Create(Self);
  HintWin.Color := {$IFDEF VER80}clYellow{$ELSE}clInfoBk{$ENDIF};
  HintWin.Canvas.Font.Color := {$IFDEF VER80}clBlack{$ELSE}clInfoText{$ENDIF};
  {Check if owner is a form, if not, move up tree of owners until we either find a form or a nil reference}
  TheForm := AOwner;
  while (TheForm <> nil) and not (TheForm is TForm) do TheForm := TheForm.Owner;
  {Record form window reference appropriately}
  if TheForm <> nil then FWindow := TheForm as TForm else FWindow := nil;
end;

procedure TPub2Edit.DefaultHandler(var message);
var
  MsgT: TMsg;
begin
  {convert message to TMsg}
  MsgT.message := TMessage(message).Msg;
  inherited{$IFDEF VER80}DefaultHandler(message){$ENDIF};
  if HintWin = nil then Exit;
  {if it's WM_Mousemove then we don't want to hide the hint window}
  if (HintWin.IsHintMsg(MsgT) and (MsgT.message <> WM_MouseMove)) then
    ReleaseIt;
end;

destructor TPub2Edit.Destroy;
begin
{$IfDef UseAutocomplete}
  FAutocomplete.Free;
{$EndIf}
  FColorOptionsSet.Free;
  inherited Destroy;
end;

procedure TPub2Edit.SetAlignment(Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    RecreateWnd;
  end;
end;

procedure TPub2Edit.SetRequired(Value: Boolean);
begin
  if Value <> FRequired then
    FRequired := Value;
end;

function TPub2Edit.Validate: Boolean;
var
  S, SZ: string;
  CanExit: Boolean;
  strmsg: string;
  i: Integer;
begin
  Result := True; //je to OK
  S := Text;
  if AsSigned(FMaskForm) then
    FMaskForm(Self, eValueToText, Mask, S); //odmaskuj holou hodnotu
  SZ := S;
  if FValidChars <> '' then
  begin
    for i := Length(S) downto 1 do
      if Pos(S[i], FValidChars) = 0 then
        Delete(S,i,1);
    if Length(S) <> Length(SZ) then
    begin
      Result := False;
      Exit; //containt some chars up length - it is wrong
    end;
  end;

  if Assigned(FValidate) then
  begin //uzivatelska validace
    FValidate(Self, S, CanExit);{Vlastni validace}
    {Oprava textu zde neni!}
    {A pri chybe zahlas}
    if not CanExit then
      Result := False;
    {pokud je pole vyzadovane, musi byt neprazdne}
    if FRequired and (S = '') then
    begin
        Result := False;
    end;
  end
  else
  begin {zabudovane validace}
    Result := DoValidate(strmsg)
  end;

end;

procedure TPub2Edit.SetCapitalize(Value: Boolean);
begin
  if Value <> FCapitalize then
    FCapitalize := Value;
end;

procedure TPub2Edit.SetAsInteger(Value: Integer);
begin
  Text := IntToStr(Value)
end;

function TPub2Edit.GetAsInteger: Integer;
begin
  Result := 0;
  if Text = '' then
    Exit;
  try
    Result := StrToInt(Text);
  except
    raise EInvalidTypeConvert.Create(StrExpectedIntegerValue);
  end
end;

procedure TPub2Edit.SetAsFloat(Value: Real);
begin
  Text := FloatToStr(Value);
end;

function TPub2Edit.GetAsFloat: Real;
begin
  Result := 0;
  if Text = '' then
    Exit;
  try
    Result := StrToFloat(Text);
  except
    raise EInvalidTypeConvert.Create(StrExpectedFloatValue);
  end
end;

procedure TPub2Edit.SetAsBoolean(const Value: Boolean);
begin
  if Value then
    Text := ccBoolDefYes
  else
    Text := ccBoolDefNo
end;

procedure TPub2Edit.SetAsCurrency(const Value: Currency);
begin
  Text := FloatToStr(Value);
end;

procedure TPub2Edit.SetAsDate(Value: TDate);
begin
  Text := DateToStr(Value);
end;

function TPub2Edit.GetAsBoolean: Boolean;
begin
  Result := False;
  if Text = '' then
    Exit;
  if (Text = ccBoolDefYes) or (Text = ccBoolDefNo) then
    Result := Text = ccBoolDefYes
  else
    raise EInvalidTypeConvert.Create(StrExpectedBooleanValue);
end;

function TPub2Edit.GetAsCurrency: Currency;
begin
  Result := 0;
  if Text = '' then
    Exit;
  try
    Result := StrToFloat(Text);
  except
    raise EInvalidTypeConvert.Create(StrExpectedCurrencyValue);
  end
end;

procedure TPub2Edit.SetAsDateTime(const Value: TDateTime);
begin
  Text := DateTimeToStr(Value);
end;

function TPub2Edit.GetAsDate: TDate;
begin
  Result := SysUtils.Date;
  if Text = '' then
    Exit;
  try
    Result := StrToDate(Text);
  except
    raise EInvalidTypeConvert.Create(StrExpectedDateValue);
  end
end;

function TPub2Edit.GetAsDateTime: TDateTime;
begin
  Result := SysUtils.Now;
  if Text = '' then
    Exit;
  try
    Result := StrToDateTime(Text);
  except
    raise EInvalidTypeConvert.Create(StrExpectedDateTimeValue);
  end
end;

procedure TPub2Edit.SetAsTime(Value: TTime);
begin
  Text := TimeToStr(Value);
end;

function TPub2Edit.GetAsTime: TTime;
begin
  Result := SysUtils.Time;
  if Text = '' then
    Exit;
  try
    Result := StrToTime(Text);
  except
    raise EInvalidTypeConvert.Create(StrExpectedTimeValue);
  end
end;

procedure TPub2Edit.SetAsString(Value: string);
var
  S: string;
begin
  S := Value;
  if AsSigned(FMaskForm) then
    FMaskForm(Self, eTextToValue, FMask, S); //odmaskuj holou hodnotu
  Text := S;
end;

function TPub2Edit.GetAsString: string;
begin
  Result := Text;
  if Result = '' then
    Exit;
  try
    if AsSigned(FMaskForm) then
      FMaskForm(Self, eValueToText, FMask, Result) //odmaskuj holou hodnotu
    else
      Result := Text;
  except
    Result := Text; //do nothing
  end
end;

procedure TPub2Edit.SetAsAnsiString(Value: AnsiString);
begin
  SetAsString(string(Value));
end;

function TPub2Edit.GetAsAnsiString: AnsiString;
begin
  Result := AnsiString(GetAsString);
end;

procedure TPub2Edit.KeyPress(var Key: Char);
var
  C: string;
  {$IFNDEF CLR}
  VCH: {$IFDEF UNICODE}TSysCharSet{$ELSE}set of Char{$ENDIF};
  //I: Integer;
  {$ELSE}
  procedure Check(VCH: string);
  begin
    if (not CharInSet(Key, ccControlKeys)) and (VCH <> '') then
      if Pos(Key, VCH) = 0 then Key := #0; {Delej, jako by nebyla}
  end;
  {$ENDIF}
begin
  case FTypeEdit of
    teString:
      begin
        {$IFDEF CLR}
        //Check(FValidChars);
        if Length(FValidChars) > 0 then
          if (not CharInSet(Key, ccControlKeys)) then
            if Pos(Key, FValidChars) <= 0 then
              Key := #0; {Delej, jako by nebyla}
        {$ELSE}
        {ohodnot}
        if (not CharInSet(Key, ccControlKeys)) and (FValidChars <> '') then
          if Pos(Key, FValidChars) = 0 then Key := #0; {Delej, jako by nebyla}
        {$ENDIF}
      end;

    {pro vstup cisel se predpoklada interne jina maska}
    teInteg:
      begin
        {$IFDEF CLR}
        //Check('0123456789+-');
        if (not CharInSet(Key, ccControlKeys)) then
          if not CharInSet(Key, ['0'..'9', '+', '-']) then
        {$ELSE}
        {nejprve vyrob mnozinu znaku}
        VCH := ['0'..'9', '+', '-'];
        {ohodnot}
        if (not CharInSet(Key, ccControlKeys)) and (VCH <> []) then
          if not CharInSet(Key, VCH) then
        {$ENDIF}
            Key := #0; {Delej, jako by nebyla}
      end;

    {pro vstup cisel se predpoklada interne jina maska}
    teFloat, teCurrency:
      begin
        {$IFDEF CLR}
        //Check('0123456789+-eE' + SysUtils.DecimalSeparator);
        if (not CharInSet(Key, ccControlKeys)) then
          if not CharInSet(Key, ['0'..'9', '+', '-', 'e', 'E']) or (Key <> SysUtils.DecimalSeparator) then
        {$ELSE}
        {nejprve vyrob mnozinu znaku}
        VCH := ['0'..'9', '+', '-', 'e', 'E'] + [{$IFDEF VER15UP}FormatSettings.{$ENDIF}DecimalSeparator];
        if {$IFDEF VER15UP}FormatSettings.{$ENDIF}ThousandSeparator <> #0 then
          VCH := VCH + [{$IFDEF VER15UP}FormatSettings.{$ENDIF}ThousandSeparator];
        {ohodnot}
        if (not CharInSet(Key, ccControlKeys)) and (VCH <> []) then
          if not CharInSet(Key, VCH) then
        {$ENDIF}
            Key := #0; {Delej, jako by nebyla}
      end;

    teTime:
      begin
        {$IFDEF CLR}
        //Check('0123456789' + SysUtils.TimeSeparator);
        if (not CharInSet(Key, ccControlKeys)) then
          if not CharInSet(Key, ['0'..'9']) or (Key <> SysUtils.TimeSeparator) then
        {$ELSE}
        VCH := ['0'..'9'] + [{$IFDEF VER15UP}FormatSettings.{$ENDIF}TimeSeparator];
        if (not CharInSet(Key, ccControlKeys)) and (VCH <> []) then
          if not CharInSet(Key, VCH) then
        {$ENDIF}
            Key := #0; {Delej, jako by nebyla}
      end;

    teDate:
      begin
        {$IFDEF CLR}
        //Check('0123456789' + SysUtils.DateSeparator);
        if (not CharInSet(Key, ccControlKeys)) then
          if not CharInSet(Key, ['0'..'9']) or (Key <> SysUtils.DateSeparator) then
        {$ELSE}
        VCH := ['0'..'9'] + [{$IFDEF VER15UP}FormatSettings.{$ENDIF}DateSeparator];
        if (not CharInSet(Key, ccControlKeys)) and (VCH <> []) then
          if not CharInSet(Key, VCH) then
        {$ENDIF}
            Key := #0; {Delej, jako by nebyla}
      end;

    teDateTime:
      {NOTE: Only DD.MM.YYYY HH:NN:SS suported}
      begin
        {$IFDEF CLR}
        //Check('0123456789' + SysUtils.DateSeparator + SysUtils.TimeSeparator);
        if (not CharInSet(Key, ccControlKeys)) then
          if not CharInSet(Key, ['0'..'9']) or (Key <> SysUtils.DateSeparator) or (Key <> SysUtils.TimeSeparator) then
        {$ELSE}
        VCH := ['0'..'9', ' '] + [{$IFDEF VER15UP}FormatSettings.{$ENDIF}DateSeparator] + [{$IFDEF VER15UP}FormatSettings.{$ENDIF}TimeSeparator];
        if (not CharInSet(Key, ccControlKeys)) and (VCH <> []) then
          if not CharInSet(Key, VCH) then
        {$ENDIF}
            Key := #0; {Delej, jako by nebyla}
      end;

  end {case};
  {jsou-li pozadovana velka pismena na zacatku slov, udelej to tady}
  if FCapitalize then
    if (SelStart = 0) or (Text[SelStart] = ' ') then
    begin
      C := Key;
      C := AnsiUpperCase(C);
      Key := C[1];
    end;

  if Key <> #0 then
    inherited KeyPress(Key);
end;

procedure TPub2Edit.DoEnter;
var S: string;
begin
  {vybarvi pole pri zamereni}
  Color := ColorOptionsSet.ColorOnFocus;
  Font.Color := ColorOptionsSet.ColorOnFocusFont;

  case FTypeEdit of
    {je-li pozadovano prevedeni a jedna-li se o integ cislo}
    teInteg:
      if AsSigned(FConvert) then
      begin {je-li prislusna rutina}
        S := Text;
        FConvert(Self, eValueToText, S); {preved text na jiny format}
        Text := S;
      end;

    {je-li potreba zaformatovat podle masky, pak jenom pro string}
    teString:
      if AsSigned(FMaskForm) then
      begin
        S := Text;
        FMaskForm(Self, eValueToText, FMask, S);
        Text := S;
      end;

  end {case};

  if not AutoSelect then
    if Text = '' then SelStart := 0
    else SelStart := GetTextLen;

  if Assigned(FOnEnter) then
    FOnEnter(Self);
end;

function TPub2Edit.DoValidate(var ErrMsg: string): Boolean;
var
  S: string;
begin
  Result := True; //no errors occurred
  ErrMsg := '';
  S := Text;
  if S <> '' then
    case FTypeEdit of
      teInteg:
        begin
          if not Self.Focused then
          begin
            if AsSigned(FConvert) then  //deconvert -like- Binary to Integer
              FConvert(Self, eValueToText, S);
          end;
          {je-li ocekavano cislo, pak to cislo byt musi}
          try
            AsInteger; {predpokladam Longint}
          except
            on EInvalidTypeConvert do
            begin
              ErrMsg := StrExpectedIntegerValue;
            end;
          end;
        end;
      teCurrency:
        begin
          try
            AsFloat
          except
            on EInvalidTypeConvert do
            begin
              ErrMsg := StrExpectedCurrencyValue;
            end;
          end;
        end;
      teFloat:
        begin
          {je-li ocekavano cislo, pak to cislo byt musi}
          try
            AsFloat; {nejaky float}
          except
            on EInvalidTypeConvert do
            begin
              ErrMsg := StrExpectedFloatValue;
            end;
          end;
        end;
      teString:
        begin
          {je-li maska (ale i bez ni), pak formatuj externi rutinou}
          if AsSigned(FMaskForm) then
          begin
            FMaskForm(Self, eTextToValue, FMask, S);
          end;
        end;
      teDate:
        if Text <> '' then
        begin
          {je-li ocekavano cislo, pak to cislo byt musi}
          try
            AsDate; {nejaky float}
          except
            on EInvalidTypeConvert do
            begin
              ErrMsg := StrExpectedDateValue;
            end;
          end;
        end;
      teTime:
        if Text <> '' then
        begin
          {je-li ocekavano cislo, pak to cislo byt musi}
          try
            AsTime; {nejaky float}
          except
            on EInvalidTypeConvert do
            begin
              ErrMsg := StrExpectedTimeValue;
            end;
          end;
        end;
      teDateTime:
        if Text <> '' then
        begin
          {je-li ocekavano cislo, pak to cislo byt musi}
          try
            AsDateTime; {nejaky float}
          except
            on EInvalidTypeConvert do
            begin
              ErrMsg := StrExpectedDateTimeValue;
            end;
          end;
        end;
    end {case};

  {teprve po nem znovu overuji, zda je pole vyplneno dle pozadavku}
  if FRequired and (S = '') then
  begin
    if ErrMsg <> '' then ErrMsg := ErrMsg + #13 + msgFieldRequired
    else ErrMsg := msgFieldRequired;
  end;
end;

procedure TPub2Edit.DoExit;
var
  S: string;
  Err: Boolean;
begin
  {tady zmen barvu pole, kdyz ztracis zamereni}
  Color := ColorOptionsSet.ColorOnNotFocus;
  Font.Color := ColorOptionsSet.ColorOnNotFocusFont;
  Self.Repaint; //Self.Invalidate;
  Err := False;
  case FTypeEdit of
    teInteg:
      if Text <> '' then
      begin
        {je-li ocekavano cislo, pak to cislo byt musi}
        try
          AsInteger; {predpokladam Longint}
        except
          on EInvalidTypeConvert do
          begin
            Err := True;
            if AsSigned(FErrorOccur) then
              {uzivatelska reakce na chybu}
              FErrorOccur(Self, StrExpectedIntegerValue)
            else
              MessageDlg(StrExpectedIntegerValue, mtError, [mbOK], 0);
            Self.SetFocus;
          end;
        end;
        {je-li pozadovano prevedeni a jedna-li se o integ cislo}
        if not Err and AsSigned(FConvert) then
        begin {je-li prislusna rutina}
          S := Text;
          FConvert(Self, eTextToValue, S); {preved text na jiny format}
          Text := S;
        end;
      end;
    teCurrency:
      if Text <> '' then
      begin
        try
          AsFloat
        except
          on EInvalidTypeConvert do
          begin
            if AsSigned(FErrorOccur) then
              {uzivatelska reakce na chybu}
              FErrorOccur(Self, StrExpectedCurrencyValue)
            else
              MessageDlg(StrExpectedCurrencyValue, mtError, [mbOK], 0);
            Self.SetFocus;
          end;
        end;
      end;
    teFloat:
      if Text <> '' then
      begin
        {je-li ocekavano cislo, pak to cislo byt musi}
        try
          AsFloat; {nejaky float}
        except
          on EInvalidTypeConvert do
          begin
            if AsSigned(FErrorOccur) then
              {uzivatelska reakce na chybu}
              FErrorOccur(Self, StrExpectedFloatValue)
            else
              MessageDlg(StrExpectedFloatValue, mtError, [mbOK], 0);
            Self.SetFocus;
          end;
        end;
      end;
    teString: if Text <> '' then
    begin
        {je-li maska (ale i bez ni), pak formatuj externi rutinou}
        if AsSigned(FMaskForm) then
        begin
          S := Text;
          FMaskForm(Self, eTextToValue, FMask, S);
          Text := S;
        end;
      end;
    teDate:
      if Text <> '' then
      begin
        {je-li ocekavano cislo, pak to cislo byt musi}
        try
          AsDate; {nejaky float}
        except
          on EInvalidTypeConvert do
          begin
            if AsSigned(FErrorOccur) then
              {uzivatelska reakce na chybu}
              FErrorOccur(Self, StrExpectedDateValue)
            else
              MessageDlg(StrExpectedDateValue, mtError, [mbOK], 0);
            Self.SetFocus;
          end;
        end;
      end;
    teTime:
      if Text <> '' then
      begin
        {je-li ocekavano cislo, pak to cislo byt musi}
        try
          AsTime; {nejaky float}
        except
          on EInvalidTypeConvert do
          begin
            if AsSigned(FErrorOccur) then
              {uzivatelska reakce na chybu}
              FErrorOccur(Self, StrExpectedTimeValue)
            else
              MessageDlg(StrExpectedTimeValue, mtError, [mbOK], 0);
            Self.SetFocus;
          end;
        end;
      end;
    teDateTime:
      if Text <> '' then
      begin
        {je-li ocekavano cislo, pak to cislo byt musi}
        try
          AsDateTime; {nejaky float}
        except
          on EInvalidTypeConvert do
          begin
            if AsSigned(FErrorOccur) then
              {uzivatelska reakce na chybu}
              FErrorOccur(Self, StrExpectedDateTimeValue)
            else
              MessageDlg(StrExpectedDateTimeValue, mtError, [mbOK], 0);
            Self.SetFocus;
          end;
        end;
      end;
  end {case};

  {tohle je vlastni onexit}
  if Assigned(FOnExit) then FOnExit(Self);

  {teprve po nem znovu overuji, zda je pole vyplneno dle pozadavku}
  if FRequired and (Text = '') then
  begin
    if AsSigned(FErrorOccur) then
      {uzivatelska reakce na chybu}
      FErrorOccur(Self, msgFieldRequired)
    else
      MessageDlg(msgFieldRequired, mtWarning, [mbOK], 0);
    SetFocus;
  end;

end;

procedure TPub2Edit.CMExit(var Message: TCMExit);
var
  CanExit: Boolean;
  S: string;
  i: Integer;
begin
  inherited;
  if FTypeEdit <> teParser then
  begin
    {vyfiltrovani vlozenych znaku, na nepovolene zde}
    if FValidChars <> '' then
    begin
      S := Text;
      for i := Length(S) downto 1 do
        if Pos(S[i], FValidChars) = 0 then
          Delete(S,i,1);
      if Length(S) <> Length(Text) then
        Text := S; {oprave zde}
    end;
  end;
  {je-li pozadovano polozku vyplnit a ona je prazdna, pak znovu}
  {tady zadnou hlasku nezobrazuji}
  if FRequired and (Text = '') then
  begin
    MessageBeep($FFFF);
    Self.SetFocus;
    Exit;
  end;
  {je-li pozadovana externi validace a neprojde, pak taky znovu}
  if FTypeEdit <> teParser then
  begin
    {vlastni validace}
    if Assigned(FValidate) then
    begin
      S := Text; {<-- text z editu} {zmena maskovani 01.09.1999 J.B.}
      {pole nejlepe bez masek}
      if AsSigned(FMaskForm) then FMaskForm(Self, eValueToText, Mask, S);
      try
        {Vlastni validace}
        FValidate(Self, S, CanExit);
      finally {fixed 23.10.2001 J.B.}
        if AsSigned(FMaskForm) then FMaskForm(Self, eTextToValue, Mask, S);
      end;
      {Oprava textu zde}
      if Text <> S then {27.7.2001 J.B.}
        Text := S;
      {pri chybe zpet}
      if not CanExit then
      begin
        MessageBeep($FFFF); {pri chybe jenom pipni}
        (*If AsSigned(FErrorOccur) Then
          {uzivatelska reakce na chybu}
          FErrorOccur(Self,msgFieldInvalidate)
        Else
          MessageDlg(msgFieldInvalidate,mtWarning, [mbOK],0);*)
        Self.SetFocus;
        Exit;
      end;
    end;
  end
  else
  begin
    {pro parser je validace extra zde}
    if not Assigned(FParsing) then
      Text := Parsed(Text)
    else
    begin
      {external parsing pointed there}
      FParsing(Self, Text, S);
      if Text <> S then
        Text := S;
    end;
  end;
  {pokud je uschovavana historie pole, pak zde}
{$IfDef UseAutocomplete}
  if FAutocomplete.StoreHistory then
  begin
    FAutocomplete.AutoCompleteItems.Add(Text)
  end;
{$EndIf}
end;

procedure TPub2Edit.CMEnter(var Message: TCMEnter);
begin
  inherited;
end;

procedure TPub2Edit.CMMouseEnter(var message: TMessage);
begin
  inherited;
  if Assigned(FOnMouseEnter) then FOnMouseEnter(Self);
end;

procedure TPub2Edit.CMMouseLeave(var message: TMessage);
begin
  inherited;
  ReleaseIt;
  if Assigned(FOnMouseExit) then FOnMouseExit(Self);
end;

procedure TPub2Edit.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  {obzvlast zajimave, nastavuji barvy :-( }
  if Enabled then
    if Focused then
    begin
      Color := ColorOptionsSet.ColorOnFocus;
      Font.Color := ColorOptionsSet.ColorOnFocusFont;
    end
    else
    begin
      Color := ColorOptionsSet.ColorOnNotFocus;
      Font.Color := ColorOptionsSet.ColorOnNotFocusFont;
    end
  else
  begin
    Color := ColorOptionsSet.ColorOnDisabled;
    Font.Color := ColorOptionsSet.ColorOnDisabledFont;
  end
end;

procedure TPub2Edit.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if FSelectAll and FWider and Focused then
  begin
    SelectAll;
    FSelectAll := False;
  end;
  inherited{$IFDEF VER80}MouseUp(Button, Shift, X, Y){$ENDIF};
end;

{-----------------------------------------------------------------------------}

procedure TPub2Edit.MouseMove(Shift: TShiftState; X, Y: Integer);
{ Since this was originally coded for a TListBox, where it makes sense being
  this way, and here it was adapted for a TEdit component, I think this could
  be better off inside the OnEnter event, but, since I'm naturally born lazy,
  I made only the minor modifications needed.  Someone can surely do a better
  job... }
var
  Rct: TRect;
begin
  if FWider and not Focused then
  begin
    FSelectAll := True;
    FWider := False;
  end;

  if not (HintText) then
  begin {Item hinting is off}
    ReleaseIt;
    inherited{$IFDEF VER80}MouseMove(Shift, X, Y){$ENDIF};
    Exit;
  end;

  { Check if there is an owning window - get out if not}
  { If HintEdit is focused, do not show the tooltip to let user edit it}
  { Also, if Application has no focus, why should we care about showing a tip?}
  if Focused or (FWindow = nil) or (Trim(Text) = '') or not Application.Active then
    Exit;

  { the line that follows is important, otherwise the next line will calculate}
  { the wrong TextWidth via FWindows' Canvas - Thanks Peter Below (TeamB),}
  { for this handy tip that I found in Borland Newsgroup}
  FWindow.Canvas.Font := Font;
  { Check if the Text is wider then the Width}
  if FWindow.Canvas.TextWidth(Text) > Width - 6 then
  begin
    FWider := True;
    if FShowing then Exit;
    { Get the default item coordinates}
    Rct := ClientRect;
    { Stretch it to fit the whole edit text }
    Rct.Right := Rct.Left + FWindow.Canvas.TextWidth(Text) + 5;
    { Fine tune theses values for appearance }
    Rct.Top := Rct.Top - 1; {2}
    Rct.Bottom := Rct.Bottom - 4; {2}
    Rct.Left := Rct.Left - 1; {2}
    { now convert to screen coordinates so that THintWindow can use them}
    Rct.TopLeft := ClientToScreen(Rct.TopLeft);
    Rct.BottomRight := ClientToScreen(Rct.BottomRight);
    { And show it!}
    HintWin.ActivateHint(Rct, Text);
    FShowing := True;
  end
  else
    ReleaseIt;

  inherited{$IFDEF VER80}MouseMove(Shift, X, Y){$ENDIF};
end;

procedure TPub2Edit.ReleaseIt;
begin
  HintWin.ReleaseHandle;
  FShowing := False;
end;

function TPub2Edit.Parsed(const S: string): string;
var
  X: string;
  EP: TEvalParser;
begin
  Result := S;
  if S = '' then Exit;
  X := S;
  EP := TEvalParser.Create;
  try
    try
      EP.CleanUp(X);
      Result := FloatToStr(EP.Evaluate(X));
    except
      on E: Exception do
        if AsSigned(FErrorOccur) then
           {uzivatelska reakce na chybu}
          FErrorOccur(Self, msgConvertValue + #13 + '(' + E.Message + ')')
        else
          MessageDlg(msgConvertValue + #13 + '(' + E.Message + ')', mtError, [mbOK], 0);
    end;
  finally
    EP.Free;
  end;
end;

procedure TPub2Edit.NextControl(YesNo: Boolean);
var
  FTempForm: {$IFDEF Ver90}TForm{$ELSE}TCustomForm{$ENDIF};
begin
  if YesNo then
  begin
    FTempForm := GetParentForm(Self);
    SendMessage(FTempForm.Handle, WM_NEXTDLGCTL, 0, 0);
  end
end;

{$IfDef UseAutocomplete}
procedure TPub2Edit.KeyUp(var Key: Word; Shift: TShiftState);
var
  s1: string;
  s2: string;
begin
  if FAutocomplete.Autocomplete then
  begin
    if Text = '' then
      Exit;
    s1 := Text; //store here
    if FAutocomplete.Test(Text, S2) then
    begin
      Text := S2;
      SelStart := Length(S1);
      SelLength := Length(S2) - Length(S1);
    end
    else
    begin
      if FAutocomplete.ForceComplete then
      begin
        Text := '';
        Key := 0;
        Exit;
      end;
    end;
  end;
  inherited KeyUp(Key, Shift);
end;
{$EndIf}

procedure TPub2Edit.CMTextChanged(var Message: TMessage);
var S: String;
begin
  inherited;
  if NOT Focused Then
    if FTypeEdit = teString then
    begin
      if Text <> '' then
      begin
        {$IfDef UseAutocomplete}
        {je-li autocomplete}
        if FAutocomplete.AutoComplete then
        begin
          FAutocomplete.AutoCompleteItems.Add(Text);
        end;
        {$EndIf}
        {je-li maska (ale i bez ni), pak formatuj externi rutinou}
        if AsSigned(FMaskForm) then
        begin
          S := Text;
          //co kdyby uz mel format
          FMaskForm(Self, eValueToText, FMask, S);
          FMaskForm(Self, eTextToValue, FMask, S);
          Text := S;
        end;
      end;
    end;
end;

procedure TPub2Edit.WMCut(var Message: TMessage);
begin
  inherited;
  if Assigned(FOnCut) then
    FOnCut(Self);
end;

procedure TPub2Edit.WMCopy(var Message: TMessage);
begin
  inherited;
  if Assigned(FOnCopy) then
    FOnCopy(Self);
end;

procedure TPub2Edit.WMClear(var Message: TMessage);
begin
  inherited;
  if Assigned(FOnClear) then
    FOnClear(Self);
end;

procedure TPub2Edit.WMPaste(var Message: TMessage);
var
{$IFNDEF VER5UP}
  Handle: THandle;
{$ENDIF}
  CText: string;
  LText: string;
  AText: string;
  i, z: Integer;
begin
  {$IFDEF VER5UP}
  CText := Clipboard.AsText;
  if CText = '' then Exit; {nothing do}
  z := Length(CText);
  if FValidChars <> '' then
  begin
    for i := Length(CText) downto 1 do
      if Pos(CText[i], FValidChars) = 0 then
        Delete(CText,i,1);
    if Length(CText) <> z then
    begin
      if AsSigned(FErrorOccur) then
        {uzivatelska reakce na chybu}
        FErrorOccur(Self, StrNoPasteFromClipboard)
      else
        MessageBeep($FFFF);
      Exit; {no match length, contain restricted characters}
    end;
  end;
  LText := '';
  if SelStart > 0 then
    LText := Copy(Text, 1, SelStart);
  LText := LText + CText;
  AText := '';
  if (SelStart + 1) < Length(Text) then
    AText := Copy(Text, SelStart + SelLength + 1, Length(Text) - SelStart + SelLength + 1);
  Text := LText + AText;
  {$ELSE}
  if IsClipboardFormatAvailable(CF_TEXT) then
  begin
    try
      OpenClipBoard(Self.Handle);
      Handle := GetClipboardData(CF_TEXT);
      if Handle = 0 then
        Exit;
      CText := StrPas(GlobalLock(Handle));
      GlobalUnlock(Handle);
      LText := '';
      if SelStart > 0 then
        LText := Copy(Text, 1, SelStart);
      LText := LText + CText;
      AText := '';
      if (SelStart + 1) < Length(Text) then
        AText := Copy(Text, SelStart + SelLength + 1, Length(Text) - SelStart + SelLength + 1);
      Text := LText + AText;
    finally
      CloseClipBoard;
    end;
  end;
  {$ENDIF}
  if Assigned(FOnPaste) then
    FOnPaste(Self);
end;

{ TPubLabeledEdit }

procedure TPub2LabeledEdit.SetLabelSpacing(const Value: Integer);
begin
  FLabelSpacing := Value;
  SetLabelPosition(FLabelPosition);
end;

constructor TPub2LabeledEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLabelPosition := lpAbove;
  FLabelSpacing := 3;
  FEditLabel := {$IFDEF useVGLibrary}Tvg2Label{$ELSE}{$IFNDEF VER6UP}TLabel{$ELSE}TBoundLabel{$ENDIF}{$ENDIF}.Create(Self);
  {$IFDEF VER6UP}
  FEditLabel.SetSubComponent(True);
  {$IFDEF useVGLibrary}
  FEditLabel.Name := 'SubLabel';  { do not localize }    
  {$ENDIF}
  if Assigned(AOwner) then
    FEditLabel.Caption := AOwner.Name;
  {$ELSE}
  if Assigned(AOwner) then
    FEditLabel.Caption := AOwner.Name;
  {$ENDIF}
  //FEditLabel.FreeNotification(Self);
  FEditLabel.Parent := Parent;
  FEditLabel.ParentFont := ParentFont;
  FEditLabel.Font.Assign(Self.Font);
  FEditLabel.Name := 'EditLabel' + Name;
  FEditLabel.Caption := Self.Name;
  {$IFDEF useVGLibrary}
  FEditLabel.FocusControl := Self;
  FEditLabel.Effects.ShadowDepth := 0;
  FEditLabel.Effects.ShadowAngle := 315;
  FEditLabel.Transparent := True;
  {$ELSE}
    {$IFNDEF VER6UP}
  FEditLabel.FocusControl := Self;
    {$ENDIF}
  {$ENDIF}
end;

destructor TPub2LabeledEdit.Destroy;
begin
  FreeAndNil(FEditLabel);
  inherited Destroy;
end;

procedure TPub2LabeledEdit.SetName(const Value: TComponentName);
begin
  if (csDesigning in ComponentState) and ((FEditlabel.GetTextLen = 0) or
     (CompareText(FEditLabel.Caption, Name) = 0)) then
    FEditLabel.Caption := Value;
  inherited SetName(Value);
  if csDesigning in ComponentState then
    Text := '';
end;

procedure TPub2LabeledEdit.CMBidimodechanged(var Message: TMessage);
begin
  inherited;
  FEditLabel.BiDiMode := BiDiMode;
end;

procedure TPub2LabeledEdit.SetLabelPosition(const Value: TLabelPosition);
var
  P: TPoint;
begin
  if FEditLabel = nil then Exit;
  FLabelPosition := Value;
  case Value of
    lpAbove: P := Point(Left, Top - FEditLabel.Height - FLabelSpacing);
    lpBelow: P := Point(Left, Top + Height + FLabelSpacing);
    lpLeft : P := Point(Left - FEditLabel.Width - FLabelSpacing,
                    Top + ((Height - FEditLabel.Height) div 2));
    lpRight: P := Point(Left + Width + FLabelSpacing,
                    Top + ((Height - FEditLabel.Height) div 2));
  end;
  FEditLabel.SetBounds(P.x, P.y, FEditLabel.Width, FEditLabel.Height);
end;

procedure TPub2LabeledEdit.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  if FEditLabel = nil then Exit;
  FEditLabel.Parent := AParent;
  FEditLabel.Visible := True;
end;

procedure TPub2LabeledEdit.CMEnabledchanged(var Message: TMessage);
begin
  inherited;
  FEditLabel.Enabled := Enabled;
end;

procedure TPub2LabeledEdit.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  SetLabelPosition(FLabelPosition);
end;

procedure TPub2LabeledEdit.CMVisiblechanged(var Message: TMessage);
begin
  inherited;
  FEditLabel.Visible := Visible;
end;

function TPub2LabeledEdit.GetLabelShowsRequired: Boolean;
begin
  Result := (FEditLabel.Font.Color = clPurple) and (fsUnderline in FEditLabel.Font.Style);
end;

procedure TPub2LabeledEdit.SetLabelShowsRequired(const Value: Boolean);
begin
  if Value then
  begin
    FEditLabel.Font.Color := clPurple;
    FEditLabel.Font.Style := [fsUnderline];
  end
  else
  begin
    FEditLabel.Font.Color := clWindowText;
    FEditLabel.Font.Style := [];
  end;
end;

{ TPub2SpinEdit }

constructor TPub2SpinEdit.Create(AOwner: TComponent);
begin
  inherited;
  FColorOptionsSet := TColorOptionsSet.Create;
  Color := ColorOptionsSet.ColorOnNotFocus;
end;

procedure TPub2SpinEdit.CMMouseLeave(var message: TMessage);
begin
  inherited;
  if Assigned(FOnMouseExit) then FOnMouseExit(Self);
end;

procedure TPub2SpinEdit.CMMouseEnter(var message: TMessage);
begin
  inherited;
  if Assigned(FOnMouseEnter) then FOnMouseEnter(Self);
end;

destructor TPub2SpinEdit.Destroy;
begin
  FColorOptionsSet.Free;
  inherited;
end;

procedure TPub2SpinEdit.CMEnter(var Message: TCMEnter);
begin
  inherited;
end;

procedure TPub2SpinEdit.DoEnter;
begin
  inherited;
  {vybarvi pole pri zamereni}
  Color := ColorOptionsSet.ColorOnFocus;
  Font.Color := ColorOptionsSet.ColorOnFocusFont;
end;

procedure TPub2SpinEdit.CMExit(var Message: TCMExit);
begin
  inherited;
end;

procedure TPub2SpinEdit.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  if Enabled then
    if Focused then
    begin
      Color := ColorOptionsSet.ColorOnFocus;
      Font.Color := ColorOptionsSet.ColorOnFocusFont;
    end
    else
    begin
      Color := ColorOptionsSet.ColorOnNotFocus;
      Font.Color := ColorOptionsSet.ColorOnNotFocusFont;
    end
  else
  begin
    Color := ColorOptionsSet.ColorOnDisabled;
    Font.Color := ColorOptionsSet.ColorOnDisabledFont;
  end
end;

procedure TPub2SpinEdit.DoExit;
begin
  {tady zmen barvu pole, kdyz ztracis zamereni}
  Color := ColorOptionsSet.ColorOnNotFocus;
  Font.Color := ColorOptionsSet.ColorOnNotFocusFont;
  inherited;
end;

{ TPub2CalculatorEdit }

constructor TPub2CalculatorEdit.Create(AOwner: TComponent);
begin
  inherited;
  FColorOptionsSet := TColorOptionsSet.Create;
  Color := ColorOptionsSet.ColorOnNotFocus;
end;

procedure TPub2CalculatorEdit.CMMouseLeave(var message: TMessage);
begin
  inherited;
  if Assigned(FOnMouseExit) then FOnMouseExit(Self);
end;

procedure TPub2CalculatorEdit.CMMouseEnter(var message: TMessage);
begin
  inherited;
  if Assigned(FOnMouseEnter) then FOnMouseEnter(Self);
end;

destructor TPub2CalculatorEdit.Destroy;
begin
  FColorOptionsSet.Free;
  inherited;
end;

procedure TPub2CalculatorEdit.CMEnter(var Message: TCMEnter);
begin
  inherited;
end;

procedure TPub2CalculatorEdit.DoEnter;
begin
  inherited;
  {vybarvi pole pri zamereni}
  Color := ColorOptionsSet.ColorOnFocus;
  Font.Color := ColorOptionsSet.ColorOnFocusFont;
end;

procedure TPub2CalculatorEdit.CMExit(var Message: TCMExit);
begin
  inherited;
end;

procedure TPub2CalculatorEdit.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  if Enabled then
    if Focused then
    begin
      Color := ColorOptionsSet.ColorOnFocus;
      Font.Color := ColorOptionsSet.ColorOnFocusFont;
    end
    else
    begin
      Color := ColorOptionsSet.ColorOnNotFocus;
      Font.Color := ColorOptionsSet.ColorOnNotFocusFont;
    end
  else
  begin
    Color := ColorOptionsSet.ColorOnDisabled;
    Font.Color := ColorOptionsSet.ColorOnDisabledFont;
  end
end;

procedure TPub2CalculatorEdit.DoExit;
begin
  {tady zmen barvu pole, kdyz ztracis zamereni}
  Color := ColorOptionsSet.ColorOnNotFocus;
  Font.Color := ColorOptionsSet.ColorOnNotFocusFont;
  inherited;
end;

{ TPub2DateTimeEdit }

constructor TPub2DateTimeEdit.Create(AOwner: TComponent);
begin
  inherited;
  FColorOptionsSet := TColorOptionsSet.Create;
  Color := ColorOptionsSet.ColorOnNotFocus;
end;

procedure TPub2DateTimeEdit.CMMouseLeave(var message: TMessage);
begin
  inherited;
  if Assigned(FOnMouseExit) then FOnMouseExit(Self);
end;

procedure TPub2DateTimeEdit.CMMouseEnter(var message: TMessage);
begin
  inherited;
  if Assigned(FOnMouseEnter) then FOnMouseEnter(Self);
end;

destructor TPub2DateTimeEdit.Destroy;
begin
  FColorOptionsSet.Free;
  inherited;
end;

procedure TPub2DateTimeEdit.CMEnter(var Message: TCMEnter);
begin
  inherited;
end;

procedure TPub2DateTimeEdit.DoEnter;
begin
  inherited;
  {vybarvi pole pri zamereni}
  Color := ColorOptionsSet.ColorOnFocus;
  Font.Color := ColorOptionsSet.ColorOnFocusFont;
end;

procedure TPub2DateTimeEdit.CMExit(var Message: TCMExit);
begin
  inherited;
end;

procedure TPub2DateTimeEdit.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  if Enabled then
    if Focused then
    begin
      Color := ColorOptionsSet.ColorOnFocus;
      Font.Color := ColorOptionsSet.ColorOnFocusFont;
    end
    else
    begin
      Color := ColorOptionsSet.ColorOnNotFocus;
      Font.Color := ColorOptionsSet.ColorOnNotFocusFont;
    end
  else
  begin
    Color := ColorOptionsSet.ColorOnDisabled;
    Font.Color := ColorOptionsSet.ColorOnDisabledFont;
  end
end;

procedure TPub2DateTimeEdit.DoExit;
begin
  {tady zmen barvu pole, kdyz ztracis zamereni}
  Color := ColorOptionsSet.ColorOnNotFocus;
  Font.Color := ColorOptionsSet.ColorOnNotFocusFont;
  inherited;
end;

{ TPub2FileNameEdit }

constructor TPub2FileNameEdit.Create(AOwner: TComponent);
begin
  inherited;
  FColorOptionsSet := TColorOptionsSet.Create;
  Color := ColorOptionsSet.ColorOnNotFocus;
end;

procedure TPub2FileNameEdit.CMMouseLeave(var message: TMessage);
begin
  inherited;
  if Assigned(FOnMouseExit) then FOnMouseExit(Self);
end;

procedure TPub2FileNameEdit.CMMouseEnter(var message: TMessage);
begin
  inherited;
  if Assigned(FOnMouseEnter) then FOnMouseEnter(Self);
end;

destructor TPub2FileNameEdit.Destroy;
begin
  FColorOptionsSet.Free;
  inherited;
end;

procedure TPub2FileNameEdit.CMEnter(var Message: TCMEnter);
begin
  inherited;
end;

procedure TPub2FileNameEdit.DoEnter;
begin
  inherited;
  {vybarvi pole pri zamereni}
  Color := ColorOptionsSet.ColorOnFocus;
  Font.Color := ColorOptionsSet.ColorOnFocusFont;
end;

procedure TPub2FileNameEdit.CMExit(var Message: TCMExit);
begin
  inherited;
end;

procedure TPub2FileNameEdit.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  if Enabled then
    if Focused then
    begin
      Color := ColorOptionsSet.ColorOnFocus;
      Font.Color := ColorOptionsSet.ColorOnFocusFont;
    end
    else
    begin
      Color := ColorOptionsSet.ColorOnNotFocus;
      Font.Color := ColorOptionsSet.ColorOnNotFocusFont;
    end
  else
  begin
    Color := ColorOptionsSet.ColorOnDisabled;
    Font.Color := ColorOptionsSet.ColorOnDisabledFont;
  end
end;

procedure TPub2FileNameEdit.DoExit;
begin
  {tady zmen barvu pole, kdyz ztracis zamereni}
  Color := ColorOptionsSet.ColorOnNotFocus;
  Font.Color := ColorOptionsSet.ColorOnNotFocusFont;
  inherited;
end;

{ TPub2FolderEdit }

constructor TPub2FolderEdit.Create(AOwner: TComponent);
begin
  inherited;
  FColorOptionsSet := TColorOptionsSet.Create;
  Color := ColorOptionsSet.ColorOnNotFocus;
end;

procedure TPub2FolderEdit.CMMouseLeave(var message: TMessage);
begin
  inherited;
  if Assigned(FOnMouseExit) then FOnMouseExit(Self);
end;

procedure TPub2FolderEdit.CMMouseEnter(var message: TMessage);
begin
  inherited;
  if Assigned(FOnMouseEnter) then FOnMouseEnter(Self);
end;

destructor TPub2FolderEdit.Destroy;
begin
  FColorOptionsSet.Free;
  inherited;
end;

procedure TPub2FolderEdit.CMEnter(var Message: TCMEnter);
begin
  inherited;
end;

procedure TPub2FolderEdit.DoEnter;
begin
  inherited;
  {vybarvi pole pri zamereni}
  Color := ColorOptionsSet.ColorOnFocus;
  Font.Color := ColorOptionsSet.ColorOnFocusFont;
end;

procedure TPub2FolderEdit.CMExit(var Message: TCMExit);
begin
  inherited;
end;

procedure TPub2FolderEdit.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  if Enabled then
    if Focused then
    begin
      Color := ColorOptionsSet.ColorOnFocus;
      Font.Color := ColorOptionsSet.ColorOnFocusFont;
    end
    else
    begin
      Color := ColorOptionsSet.ColorOnNotFocus;
      Font.Color := ColorOptionsSet.ColorOnNotFocusFont;
    end
  else
  begin
    Color := ColorOptionsSet.ColorOnDisabled;
    Font.Color := ColorOptionsSet.ColorOnDisabledFont;
  end
end;

procedure TPub2FolderEdit.DoExit;
begin
  {tady zmen barvu pole, kdyz ztracis zamereni}
  Color := ColorOptionsSet.ColorOnNotFocus;
  Font.Color := ColorOptionsSet.ColorOnNotFocusFont;
  inherited;
end;

{ TPub2ComboBox }

constructor TPub2ComboBox.Create(AOwner: TComponent);
begin
  inherited;
  FColorOptionsSet := TColorOptionsSet.Create;
  Color := ColorOptionsSet.ColorOnNotFocus;
  FDropDownFlexWidth := False;
end;

procedure TPub2ComboBox.CMMouseLeave(var message: TMessage);
begin
  inherited;
  if Assigned(FOnMouseExit) then FOnMouseExit(Self);
end;

procedure TPub2ComboBox.CMMouseEnter(var message: TMessage);
begin
  inherited;
  if Assigned(FOnMouseEnter) then FOnMouseEnter(Self);
end;

destructor TPub2ComboBox.Destroy;
begin
  FColorOptionsSet.Free;
  inherited;
end;

procedure TPub2ComboBox.CMEnter(var Message: TCMEnter);
begin
  inherited;
end;

procedure TPub2ComboBox.DoEnter;
begin
  inherited;
  {vybarvi pole pri zamereni}
  Color := ColorOptionsSet.ColorOnFocus;
  Font.Color := ColorOptionsSet.ColorOnFocusFont;
end;

procedure TPub2ComboBox.CMExit(var Message: TCMExit);
begin
  inherited;
end;

procedure TPub2ComboBox.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  if Enabled then
    if Focused then
    begin
      Color := ColorOptionsSet.ColorOnFocus;
      Font.Color := ColorOptionsSet.ColorOnFocusFont;
    end
    else
    begin
      Color := ColorOptionsSet.ColorOnNotFocus;
      Font.Color := ColorOptionsSet.ColorOnNotFocusFont;
    end
  else
  begin
    Color := ColorOptionsSet.ColorOnDisabled;
    Font.Color := ColorOptionsSet.ColorOnDisabledFont;
  end
end;

procedure TPub2ComboBox.DoExit;
begin
  {tady zmen barvu pole, kdyz ztracis zamereni}
  Color := ColorOptionsSet.ColorOnNotFocus;
  Font.Color := ColorOptionsSet.ColorOnNotFocusFont;
  inherited;
end;

procedure TPub2ComboBox.ListPick;
begin
  if Assigned(FOnListPick) then FOnListPick(Self) else Change;
end;

procedure TPub2ComboBox.CNCommand(var Message: TWMCommand);
begin
  case Message.NotifyCode of
    CBN_SELCHANGE: { in this case only, we will take over }
      begin
        Text := Items[ItemIndex];
        Click;
        ListPick;
      end;
  else { let mom handle the rest }
    inherited;
  end;
end;

procedure TPub2ComboBox.WndProc(var Message: TMessage);
begin
  case Message.Msg of
    CB_ADDSTRING, CB_INSERTSTRING:
      begin
        inherited WndProc(Message);
        if Assigned(FOnItemsChanged) then
          FOnItemsChanged(Self);
        DoDropDownFlexWidth;
      end;
    else
      inherited WndProc(Message);
  end;
end;

procedure TPub2ComboBox.DoDropDownFlexWidth;
var
  V, I, J: Integer;
begin
  if not FDropDownFlexWidth then
    Exit;
  V := 0;
  if Items.Count > 0 then
  begin
    for I := 0 to Items.Count - 1 do
    begin
      J := Canvas.TextWidth(Items[I]) + 6;
      if J > V then
        V := J;
    end;
    if V < Width then
      V := Width;
    Perform(CB_SETDROPPEDWIDTH, V, 0);
  end;
end;

{$WARNINGS OFF}
procedure TPub2ComboBox.SetItems(const Value: TStrings);
begin
  {$IFDEF VER7UP}inherited{$ENDIF} SetItems(Value);
  if Assigned(FOnItemsChanged) then
    FOnItemsChanged(Self);
  DoDropDownFlexWidth;
end;
{$WARNINGS ON}

{$ENDIF VG2}

{  Register  }

procedure Register;
begin
  RegisterComponents('Library', [
    TPubEdit,
    TPubLabeledEdit,
    {$IFDEF UNICODE}
    {$IFNDEF CLR}
    TPubButtonedEdit,
    TPubLabeledButtonedEdit,
    {$ENDIF}
    {$ENDIF}
    TPubMaskEdit,
    TPubLabeledMaskEdit,
    TPubComboBox,
    TPubLabeledComboBox,
    {$IFDEF VER7UP}
    TPubComboBoxEx,
    TPubLabeledComboBoxEx,
    {$ENDIF}
    TPubSpinEdit
    ]);
  {$IFDEF USE_RXLIB}
  RegisterComponents('Library_RX', [
    TPubCurrencyEdit,
    TPubLabeledCurrencyEdit,
    TPubDateEdit,
    TPubLabeledDateEdit,
    TPubFilenameEdit,
    TPubLabeledFilenameEdit,
    TPubDirectoryEdit,
    TPubLabeledDirectoryEdit,
    TPubComboEdit,
    TPubLabeledComboEdit,
    TPubSpinNumEdit,
    TPubLabeledSpinNumEdit,
    TPubRxCalcEdit,
    TPubLabeledRxCalcEdit
    ]);
  {$ENDIF USE_RXLIB}
  {$IFDEF USE_VG2LIB}
  RegisterComponents('Library_VG2', [
    TPub2Edit,
    TPub2LabeledEdit,
    TPub2SpinEdit,
    TPub2CalculatorEdit,
    TPub2DateTimeEdit,
    TPub2FilenameEdit,
    TPub2FolderEdit,
    TPub2ComboBox
    ]);
  {$ENDIF}
end;

{end-of-file}

end.