{***************************************************************************}
{                                                                           }
{  Copyright (c) 1999-2015 Sergiy Kurinny                                   }
{                                                                           }
{  This library is free software; you can redistribute it and/or            }
{  modify it under the terms of the GNU Lesser General Public               }
{  License version 2.1 as published by the Free Software Foundation         }
{  and appearing in the file license.txt which is included in the root      }
{  folder of the package.                                                   }
{                                                                           }
{  This library is distributed in the hope that it will be useful,          }
{  but WITHOUT ANY WARRANTY; without even the implied warranty of           }
{  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU        }
{  Lesser General Public License for more details.                          }
{                                                                           }
{***************************************************************************}
unit psc_const;

interface
{$I psc_defines.inc}
{$IFDEF D2009}
  {$M+}
{$ENDIF}

{---------------------------------------}

//{$DEFINE PSC_LANG_dutch}
//{$DEFINE PSC_LANG_greek}
//{$DEFINE PSC_LANG_hungarian}
//{$DEFINE PSC_LANG_german}
//{$DEFINE PSC_LANG_czech}
//{$DEFINE PSC_LANG_spanish}
//{$DEFINE PSC_LANG_russian}
//{$DEFINE PSC_LANG_italian}
//{$DEFINE PSC_LANG_french}
//{$DEFINE PSC_LANG_swedish}
//{$DEFINE PSC_LANG_danish}
//{$DEFINE PSC_LANG_portuguese}
//{$DEFINE PSC_LANG_bra_portuguese}  {brazilian portuguese}
//{$DEFINE PSC_LANG_gb_chinese}
//{$DEFINE PSC_LANG_polish}
//{$DEFINE PSC_LANG_norwegian}


//{$DEFINE PSC_LANG_custom}

resourcestring
  SPSCDummyString13257=''; // just a dummy string, don't translate and don't remove

//========================= not yet localized =================
//BeginSkipConst
//EndSkipConst

//========================= custom =================
{$IFDEF PSC_LANG_custom}
  {$I psc_lng_custom.inc}
  {$DEFINE PSC_LANG_SKIP}
{$ENDIF}

//========================= norwegian =================
{$IFDEF PSC_LANG_norwegian}
  {$I psc_lng_norwegian.inc}
  {$DEFINE PSC_LANG_SKIP}
{$ENDIF}

//========================= polish =================
{$IFDEF PSC_LANG_polish}
  {$I psc_lng_polish.inc}
  {$DEFINE PSC_LANG_SKIP}
{$ENDIF}

//========================= GB Chinese =================
{$IFDEF PSC_LANG_gb_chinese}
  {$I psc_lng_gb_chinese.inc}
  {$DEFINE PSC_LANG_SKIP}
{$ENDIF}

//========================= dutch =================
{$IFDEF PSC_LANG_dutch}
  {$I psc_lng_dutch.inc}
  {$DEFINE PSC_LANG_SKIP}
{$ENDIF}

//========================= hungarian =================
{$IFDEF PSC_LANG_hungarian}
  {$I psc_lng_hungarian.inc}
  {$DEFINE PSC_LANG_SKIP}
{$ENDIF}

//========================= german =================
{$IFDEF PSC_LANG_german}
  {$I psc_lng_german.inc}
  {$DEFINE PSC_LANG_SKIP}
{$ENDIF}

//========================= czech =================
{$IFDEF PSC_LANG_czech}
  {$I psc_lng_czech.inc}
  {$DEFINE PSC_LANG_SKIP}
{$ENDIF}

//========================= spanish =================
{$IFDEF PSC_LANG_spanish}
  {$I psc_lng_spanish.inc}
  {$DEFINE PSC_LANG_SKIP}
{$ENDIF}

//========================= russian =================
{$IFDEF PSC_LANG_russian}
  {$I psc_lng_russian.inc}
  {$DEFINE PSC_LANG_SKIP}
{$ENDIF}

//========================= greek =================
{$IFDEF PSC_LANG_greek}
  {$I psc_lng_greek.inc}
  {$DEFINE PSC_LANG_SKIP}
{$ENDIF}

//========================= italian =================
{$IFDEF PSC_LANG_italian}
  {$I psc_lng_italian.inc}
  {$DEFINE PSC_LANG_SKIP}
{$ENDIF}

//========================= french =================
{$IFDEF PSC_LANG_french}
  {$I psc_lng_french.inc}
  {$DEFINE PSC_LANG_SKIP}
{$ENDIF}

//========================= swedish =================
{$IFDEF PSC_LANG_swedish}
  {$I psc_lng_swedish.inc}
  {$DEFINE PSC_LANG_SKIP}
{$ENDIF}

//========================= danish =================
{$IFDEF PSC_LANG_danish}
  {$I psc_lng_danish.inc}
  {$DEFINE PSC_LANG_SKIP}
{$ENDIF}

//========================= portuguese =================
{$IFDEF PSC_LANG_portuguese}
  {$I psc_lng_portuguese.inc}
  {$DEFINE PSC_LANG_SKIP}
{$ENDIF}

//========================= portuguese =================
{$IFDEF PSC_LANG_bra_portuguese}
  {$I psc_lng_bra_portuguese.inc}
  {$DEFINE PSC_LANG_SKIP}
{$ENDIF}

{$IFNDEF PSC_LANG_SKIP}
//BeginSkipConst
  SPSCBlack                                  = 'Black';
  SPSCBrown                                  = 'Brown';
  SPSCOliveGreen                             = 'Olive Green';
  SPSCDarkGreen                              = 'Dark Green';
  SPSCDarkTeal                               = 'Dark Teal';
  SPSCDarkBlue                               = 'Dark Blue';
  SPSCIndigo                                 = 'Indigo';
  SPSCGray80                                 = 'Gray-80%';
  SPSCDarkRed                                = 'Dark Red';
  SPSCOrange                                 = 'Orange';
  SPSCDarkYellow                             = 'Dark Yellow';
  SPSCGreen                                  = 'Green';
  SPSCTeal                                   = 'Teal';
  SPSCBlue                                   = 'Blue';
  SPSCBlueGray                               = 'Blue-Gray';
  SPSCGray50                                 = 'Gray-50%';
  SPSCRed                                    = 'Red';
  SPSCLightOrange                            = 'Light Orange';
  SPSCLime                                   = 'Lime';
  SPSCSeaGreen                               = 'Sea Green';
  SPSCAqua                                   = 'Aqua';
  SPSCLightBlue                              = 'Light Blue';
  SPSCViolet                                 = 'Violet';
  SPSCGray40                                 = 'Gray-40%';
  SPSCPink                                   = 'Pink';
  SPSCGold                                   = 'Gold';
  SPSCYellow                                 = 'Yellow';
  SPSCBrightGreen                            = 'Bright Green';
  SPSCTurquoise                              = 'Turquoise';
  SPSCSkyBlue                                = 'Sky Blue';
  SPSCPlum                                   = 'Plum';
  SPSCGray25                                 = 'Gray-25%';
  SPSCRose                                   = 'Rose';
  SPSCTan                                    = 'Tan';
  SPSCLightYellow                            = 'Light Yellow';
  SPSCLightGreen                             = 'Light Green';
  SPSCLightTurquoise                         = 'Light Turquoise';
  SPSCPaleBlue                               = 'Pale Blue';
  SPSCLavender                               = 'Lavender';
  SPSCWhite                                  = 'White';
  SPSCHotLight                               = 'Hot Light';
  SPSCGradientActiveCaption                  = 'Gradient Active Caption';
  SPSCGradientInactiveCaption                = 'Gradient Inactive Caption';
//EndSkipConst
{$ENDIF}

{---------------------------------------}

const
//BeginSkipConst
  SPSCTextFalse:String = 'False';
  SPSCTextTrue:String = 'True';

  SPSC_PAS_absolute       ='absolute';
  SPSC_PAS_abstract       ='abstract';
  SPSC_PAS_and            ='and';
  SPSC_PAS_array          ='array';
  SPSC_PAS_as             ='as';
  SPSC_PAS_asm            ='asm';
  SPSC_PAS_assembler      ='assembler';
  SPSC_PAS_automated      ='automated';
  SPSC_PAS_begin          ='begin';
  SPSC_PAS_break          ='break';
  SPSC_PAS_case           ='case';
  SPSC_PAS_cdecl          ='cdecl';
  SPSC_PAS_class          ='class';
  SPSC_PAS_const          ='const';
  SPSC_PAS_constructor    ='constructor';
  SPSC_PAS_continue       ='continue';
  SPSC_PAS_default        ='default';
  SPSC_PAS_destructor     ='destructor';
  SPSC_PAS_dispid         ='dispid';
  SPSC_PAS_dispinterface  ='dispinterface';
  SPSC_PAS_div            ='div';
  SPSC_PAS_do             ='do';
  SPSC_PAS_downto         ='downto';
  SPSC_PAS_dynamic        ='dynamic';
  SPSC_PAS_else           ='else';
  SPSC_PAS_end            ='end';
  SPSC_PAS_except         ='except';
  SPSC_PAS_exit           ='exit';
  SPSC_PAS_export         ='export';
  SPSC_PAS_exports        ='exports';
  SPSC_PAS_external       ='external';
  SPSC_PAS_far            ='far';
  SPSC_PAS_file           ='file';
  SPSC_PAS_finalization   ='finalization';
  SPSC_PAS_finally        ='finally';
  SPSC_PAS_for            ='for';
  SPSC_PAS_forward        ='forward';
  SPSC_PAS_function       ='function';
  SPSC_PAS_goto           ='goto';
  SPSC_PAS_if             ='if';
  SPSC_PAS_implementation ='implementation';
  SPSC_PAS_in             ='in';
  SPSC_PAS_index          ='index';
  SPSC_PAS_inherited      ='inherited';
  SPSC_PAS_initialization ='initialization';
  SPSC_PAS_inline         ='inline';
  SPSC_PAS_interface      ='interface';
  SPSC_PAS_is             ='is';
  SPSC_PAS_label          ='label';
  SPSC_PAS_library        ='library';
  SPSC_PAS_message        ='message';
  SPSC_PAS_mod            ='mod';
  SPSC_PAS_near           ='near';
  SPSC_PAS_nil            ='nil';
  SPSC_PAS_nodefault      ='nodefault';
  SPSC_PAS_not            ='not';
  SPSC_PAS_object         ='object';
  SPSC_PAS_of             ='of';
  SPSC_PAS_or             ='or';
  SPSC_PAS_out            ='out';
  SPSC_PAS_overload       ='overload';
  SPSC_PAS_override       ='override';
  SPSC_PAS_packed         ='packed';
  SPSC_PAS_pascal         ='pascal';
  SPSC_PAS_platform       ='platform';
  SPSC_PAS_private        ='private';
  SPSC_PAS_procedure      ='procedure';
  SPSC_PAS_program        ='program';
  SPSC_PAS_property       ='property';
  SPSC_PAS_protected      ='protected';
  SPSC_PAS_public         ='public';
  SPSC_PAS_published      ='published';
  SPSC_PAS_raise          ='raise';
  SPSC_PAS_read           ='read';
  SPSC_PAS_readonly       ='readonly';
  SPSC_PAS_record         ='record';
  SPSC_PAS_register       ='register';
  SPSC_PAS_reintroduce    ='reintroduce';
  SPSC_PAS_repeat         ='repeat';
  SPSC_PAS_resident       ='resident';
  SPSC_PAS_resourcestring ='resourcestring';
  SPSC_PAS_safecall       ='safecall';
  SPSC_PAS_set            ='set';
  SPSC_PAS_shl            ='shl';
  SPSC_PAS_shr            ='shr';
  SPSC_PAS_stdcall        ='stdcall';
  SPSC_PAS_stored         ='stored';
  SPSC_PAS_string         ='string';
  SPSC_PAS_stringresource ='stringresource';
  SPSC_PAS_then           ='then';
  SPSC_PAS_threadvar      ='threadvar';
  SPSC_PAS_to             ='to';
  SPSC_PAS_try            ='try';
  SPSC_PAS_type           ='type';
  SPSC_PAS_unit           ='unit';
  SPSC_PAS_until          ='until';
  SPSC_PAS_uses           ='uses';
  SPSC_PAS_var            ='var';
  SPSC_PAS_virtual        ='virtual';
  SPSC_PAS_while          ='while';
  SPSC_PAS_with           ='with';
  SPSC_PAS_write          ='write';
  SPSC_PAS_writeonly      ='writeonly';
  SPSC_PAS_xor            ='xor';

  SPSCBeginSkip = '//BeginSkipConst';
  SPSCEndSkip = '//EndSkipConst';
  SPSCResourcedComment = 'don''t resource';
//EndSkipConst

// ------------------------ Delphi parser reserved words ------------------

  ID_absolute       =0;
  ID_abstract       =ID_absolute       +1;
  ID_and            =ID_abstract       +1;
  ID_array          =ID_and            +1;
  ID_as             =ID_array          +1;
  ID_asm            =ID_as             +1;
  ID_assembler      =ID_asm            +1;
  ID_automated      =ID_assembler      +1;
  ID_begin          =ID_automated      +1;
  ID_break          =ID_begin          +1;
  ID_case           =ID_break          +1;
  ID_cdecl          =ID_case           +1;
  ID_class          =ID_cdecl          +1;
  ID_const          =ID_class          +1;
  ID_constructor    =ID_const          +1;
  ID_continue       =ID_constructor    +1;
  ID_default        =ID_continue       +1;
  ID_destructor     =ID_default        +1;
  ID_dispid         =ID_destructor     +1;
  ID_dispinterface  =ID_dispid         +1;
  ID_div            =ID_dispinterface  +1;
  ID_do             =ID_div            +1;
  ID_downto         =ID_do             +1;
  ID_dynamic        =ID_downto         +1;
  ID_else           =ID_dynamic        +1;
  ID_end            =ID_else           +1;
  ID_except         =ID_end            +1;
  ID_exit           =ID_except         +1;
  ID_export         =ID_exit           +1;
  ID_exports        =ID_export         +1;
  ID_external       =ID_exports        +1;
  ID_far            =ID_external       +1;
  ID_file           =ID_far            +1;
  ID_finalization   =ID_file           +1;
  ID_finally        =ID_finalization   +1;
  ID_for            =ID_finally        +1;
  ID_forward        =ID_for            +1;
  ID_function       =ID_forward        +1;
  ID_goto           =ID_function       +1;
  ID_if             =ID_goto           +1;
  ID_implementation =ID_if             +1;
  ID_in             =ID_implementation +1;
  ID_index          =ID_in             +1;
  ID_inherited      =ID_index          +1;
  ID_initialization =ID_inherited      +1;
  ID_inline         =ID_initialization +1;
  ID_interface      =ID_inline         +1;
  ID_is             =ID_interface      +1;
  ID_label          =ID_is             +1;
  ID_library        =ID_label          +1;
  ID_message        =ID_library        +1;
  ID_mod            =ID_message        +1;
  ID_near           =ID_mod            +1;
  ID_nil            =ID_near           +1;
  ID_nodefault      =ID_nil            +1;
  ID_not            =ID_nodefault      +1;
  ID_object         =ID_not            +1;
  ID_of             =ID_object         +1;
  ID_or             =ID_of             +1;
  ID_out            =ID_or             +1;
  ID_overload       =ID_out            +1;
  ID_override       =ID_overload       +1;
  ID_packed         =ID_override       +1;
  ID_pascal         =ID_packed         +1;
  ID_platform       =ID_pascal         +1;
  ID_private        =ID_platform       +1;
  ID_procedure      =ID_private        +1;
  ID_program        =ID_procedure      +1;
  ID_property       =ID_program        +1;
  ID_protected      =ID_property       +1;
  ID_public         =ID_protected      +1;
  ID_published      =ID_public         +1;
  ID_raise          =ID_published      +1;
  ID_read           =ID_raise          +1;
  ID_readonly       =ID_read           +1;
  ID_record         =ID_readonly       +1;
  ID_register       =ID_record         +1;
  ID_reintroduce    =ID_register       +1;
  ID_repeat         =ID_reintroduce    +1;
  ID_resident       =ID_repeat         +1;
  ID_resourcestring =ID_resident       +1;
  ID_safecall       =ID_resourcestring +1;
  ID_set            =ID_safecall       +1;
  ID_shl            =ID_set            +1;
  ID_shr            =ID_shl            +1;
  ID_stdcall        =ID_shr            +1;
  ID_stored         =ID_stdcall        +1;
  ID_string         =ID_stored         +1;
  ID_stringresource =ID_string         +1;
  ID_then           =ID_stringresource +1;
  ID_threadvar      =ID_then           +1;
  ID_to             =ID_threadvar      +1;
  ID_try            =ID_to             +1;
  ID_type           =ID_try            +1;
  ID_unit           =ID_type           +1;
  ID_until          =ID_unit           +1;
  ID_uses           =ID_until          +1;
  ID_var            =ID_uses           +1;
  ID_virtual        =ID_var            +1;
  ID_while          =ID_virtual        +1;
  ID_with           =ID_while          +1;
  ID_write          =ID_with           +1;
  ID_writeonly      =ID_write          +1;
  ID_xor            =ID_writeonly      +1;

{---------------------------------------}

type
  TPSCConsts=class(TObject)
  private
  protected
  public
    ErrProgramFailed                         : String;
    ErrCharSetDef                            : String;
    ErrMaxIfDefReached                       : String;
    ErrCompilerDirective                     : String;
    CharExpected                             : String;
    SymbolExpected                           : String;
    ErrFileNotFound                          : String;
    ErrStringsNotSet                         : String;
    NotProcessedfiles                        : String;
    Directory                                : String;
    ErrUnkProp                               : String;
    ErrLoadFile                              : String;
    AssignError                              : String;
    OKButton                                 : String;
    CancelButton                             : String;
    HelpButton                               : String;
    ReadError                                : String;
    WriteError                               : String;
    AdditionalFilter                         : String;
    AutoUpdate                               : String;
    ErrAssignFilterSource                    : String;
    ErrConnectDataSet                        : String;
    ErrNoFilterSource                        : String;
    ErrCanNotDestroy                         : String;
    ErrBadPropElem                           : String;
    ThereAreNoItems                          : String;
    ClickHeretoChangeSortOrder               : String;
    SortAscending                            : String;
    SortDescending                           : String;
    ClearSortOrder                           : String;
    IgnoreCase                               : String;
    ClickHereToAddValue                      : String;
    Cap_In                                   : String;
    Cap_NotIn                                : String;
    AddAllFields                             : String;
    AddAllFieldParams                        : String;
    FindFirst                                : String;
    FindPrev                                 : String;
    FindNext                                 : String;
    FindLast                                 : String;
    ColorIndicatedCells                      : String;
    Filtered                                 : String;
    ErrHandlerAlreadyExists                  : String;
    ErrHandlerNotExists                      : String;
    ApplyChanges                             : String;
    CancelChanges                            : String;
    ErrDupItem                               : String;
    InvalidColor                             : String;
    InvalidClass                             : String;
    Black                                    : String;
    Brown                                    : String;
    OliveGreen                               : String;
    DarkGreen                                : String;
    DarkTeal                                 : String;
    DarkBlue                                 : String;
    Indigo                                   : String;
    Gray80                                   : String;
    DarkRed                                  : String;
    Orange                                   : String;
    DarkYellow                               : String;
    Green                                    : String;
    Teal                                     : String;
    Blue                                     : String;
    BlueGray                                 : String;
    Gray50                                   : String;
    Red                                      : String;
    LightOrange                              : String;
    Lime                                     : String;
    SeaGreen                                 : String;
    Aqua                                     : String;
    LightBlue                                : String;
    Violet                                   : String;
    Gray40                                   : String;
    Pink                                     : String;
    Gold                                     : String;
    Yellow                                   : String;
    BrightGreen                              : String;
    Turquoise                                : String;
    SkyBlue                                  : String;
    Plum                                     : String;
    Gray25                                   : String;
    Rose                                     : String;
    Tan                                      : String;
    LightYellow                              : String;
    LightGreen                               : String;
    LightTurquoise                           : String;
    PaleBlue                                 : String;
    Lavender                                 : String;
    White                                    : String;
    HotLight                                 : String;
    GradientActiveCaption                    : String;
    GradientInactiveCaption                  : String;
    Transparent                              : String;
    MoreColorsCapt                           : String;
    MoreColors                               : String;
    Auto                                     : String;
    AutoColor                                : String;
    None                                     : String;
    NoHighLight                              : String;
    StdColors                                : String;
    WindowsColors                            : String;
    DocColors                                : String;
    FontColors                               : String;
    CustomColors                             : String;
    CustomColorsCapt                         : String;
    AddSlot                                  : String;
    DeleteSlot                               : String;
    DefaultSection                           : String;
    ColorSection                             : String;
    ButtonSection                            : String;
    DragBarHint                              : String;
    OperationDispAnd                         : String;
    OperationDispOr                          : String;
    ErrSystemSlot                            : String;
    ErrMemSlotDelete                         : String;
    PrmBoxSaveToMem                          : String;
    PrmBoxLoadFromMem                        : String;
    ClickHereToAddItem                       : String;
    ClickHereToAddCond                       : String;
    FltSaveFileCat                           : String;
    ListPopupAdd                             : String;
    ListPopupDelete                          : String;
    ListPopupIndent                          : String;
    ListPopupOutDent                         : String;
    ListPopupSave                            : String;
    ListPopupLoad                            : String;
    ListPopupClear                           : String;
    DeleteButton                             : String;
    Cap_Cont                                 : String;
    Cap_Is                                   : String;
    Cap_NotCont                              : String;
    Cap_IsEmpty                              : String;
    Cap_IsNEmpty                             : String;
    Cap_Equals                               : String;
    Cap_NEquals                              : String;
    Cap_AtMost                               : String;
    Cap_AtLeast                              : String;
    Cap_More                                 : String;
    Cap_Less                                 : String;
    Cap_Between                              : String;
    Cap_Yesterday                            : String;
    Cap_Today                                : String;
    Cap_Tomorrow                             : String;
    Cap_Last7                                : String;
    Cap_Next7                                : String;
    Cap_LastWeek                             : String;
    Cap_ThisWeek                             : String;
    Cap_NextWeek                             : String;
    Cap_LastMon                              : String;
    Cap_ThisMon                              : String;
    Cap_NextMon                              : String;
    Cap_On                                   : String;
    Cap_OnAfter                              : String;
    Cap_Onbefore                             : String;
    Cap_After                                : String;
    Cap_Before                               : String;
    Cap_BlankEmpty                           : String;
    templ2                                   : String;
    Untitled                                 : String;
    CalcError                                : String;
    ErrCharPos                               : String;
    TokenListEmpty                           : String;
    ErrFutureState                           : String;
    ErrBadFilter                             : String;
    BoolDispTrue                             : String;
    BoolDispFalse                            : String;
    Cap_Begins                               : String;
    Cap_Ends                                 : String;
    ErrEmptyFilter                           : String;
    ErrNoMatchRecord                         : String;
    ErrEndReached                            : String;
    ErrBeginReached                          : String;
    SearchFromBegin                          : String;
    SearchFromEnd                            : String;
    SpecifyFilter                            : String;
    FilterLabel                              : String;
    ExcludeFilterLabel                       : String;
    Cap_OnDate                               : String;
    BadFieldType                             : String;
    ErrDataSetNilNoFields                    : String;
    ErrNoFields                              : String;
    ApplyButton                              : String;
    PrmBoxEditor                             : String;
    DefaultString                            : String;
    TodayButton                              : String;
    NoneButton                               : String;
    SelectMonth                              : String;
    AHC_Label                                : String;
    AHC_AddHolidaysToCal                     : String;
    PrintStatus                              : String;
    SearchPrinters                           : String;
    PrintProgress                            : String;
    PrintingPage                             : String;
    PrintingOnPrinter                        : String;
    PageNoStr                                : String;
    NowButton                                : String;

    ErrRefCount                              : String;
    ExceptionIn                              : String;
    ErrInvalidfieldkind                      : String;
    OperationDispAndNot                      : String;
    OperationDispOrNot                       : String;
  published
  end;

function PSCConsts:TPSCConsts;

{---------------------------------------}

implementation

{---------------------------------------}

//========================= ENGLISH =================

{$IFNDEF PSC_LANG_SKIP}
resourcestring
//BeginSkipConst
  SPSCMoreColorsCapt                         = '&More colors...';
  SPSCMoreColors                             = 'More Colors';
  SPSCAuto                                   = 'Automatic';
  SPSCAutoColor                              = 'Automatic Color';
  SPSCNone                                   = 'None';
  SPSCNoHighLight                            = 'No Highlight';
  SPSCStdColors                              = '&Standard Colors:';
  SPSCWindowsColors                          = '&Windows Colors:';
  SPSCDocColors                              = '&Document''s Colors:';
  SPSCFontColors                             = '&Font Colors:';
  SPSCCustomColors                           = '&Custom Colors:';
  SPSCCustomColorsCapt                       = 'Custom Colors';

  SPSCCap_In         = 'is in list';                  // 'Field in (Value1, Value2, Value3...)'
  SPSCCap_NotIn      = 'is not in list';              //
  SPSCCap_Cont       = 'contains';                    // 'Field LIKE %Value%'
  SPSCCap_Is         = 'is (exactly)';                // 'Field = Value' (for string fields)
  SPSCCap_NotCont    = 'does not contain';            //
  SPSCCap_IsEmpty    = 'is empty';                    //
  SPSCCap_IsNEmpty   = 'is not empty';                //
  SPSCCap_Equals     = 'is equal to';                 // 'Field = Value'
  SPSCCap_NEquals    = 'is not equal to';             //
  SPSCCap_AtMost     = 'is less than or equal to';    //
  SPSCCap_AtLeast    = 'is greater than or equal to'; //
  SPSCCap_More       = 'is greater than';             // 'Field > Value'
  SPSCCap_Less       = 'is less than';                // 'Field < Value'
  SPSCCap_Between    = 'is between';                  //
  SPSCCap_Yesterday  = 'is ~yesterday~';
  SPSCCap_Today      = 'is ~today~';
  SPSCCap_Tomorrow   = 'is ~tomorrow~';
  SPSCCap_Last7      = 'is in the ~last 7 days~';
  SPSCCap_Next7      = 'is in the ~next 7 days~';
  SPSCCap_LastWeek   = 'is on the ~last week~';
  SPSCCap_ThisWeek   = 'is on ~this week~';
  SPSCCap_NextWeek   = 'is on the ~next week~';
  SPSCCap_LastMon    = 'is in ~last month~';
  SPSCCap_ThisMon    = 'is in ~this month~';
  SPSCCap_NextMon    = 'is in the ~next month~';
  SPSCCap_On         = 'is ~on~';
  SPSCCap_OnAfter    = 'is ~on or after~';
  SPSCCap_Onbefore   = 'is ~on or before~';
  SPSCCap_After      = 'after';
  SPSCCap_Before     = 'before';
  SPSCCap_BlankEmpty = 'is ~blank or empty~';
  SPSCCap_Begins     = 'begins with';
  SPSCCap_Ends       = 'ends with';
  SPSCCap_OnDate     = 'is ~on date~';

  SPSCtempl2                                 = ':Field :Template :Value1 and :Value2';
     {The only thing you can translate in SPSCtempl2 is 'and' word}

  SPSCAssignError                            = 'Cannot assign a %s to a %s';
  SPSCOKButton                               = 'OK';
  SPSCCancelButton                           = 'Cancel';
  SPSCHelpButton                             = '&Help';
  SPSCReadError                              = 'Stream read error';
  SPSCWriteError                             = 'Stream write error';
  SPSCAdditionalFilter                       = '[Additional Filter]';
  SPSCAutoUpdate                             = 'Auto Update';
  SPSCErrAssignFilterSource                  = 'Assign FilterSource property with TPSCFltBld';
  SPSCErrConnectDataSet                      = 'Connect TDataSet or specify Fields';
  SPSCErrNoFilterSource                      = 'FilterSource is not assigned';
  SPSCErrCanNotDestroy                       = 'Cannot destroy this object';
  SPSCErrBadPropElem                         = 'Invalid property element: %s';
  SPSCThereAreNoItems                        = 'There are no items to show in this view';
  SPSCClickHeretoChangeSortOrder             = 'Click here to change sort order';
  SPSCSortAscending                          = 'Sort Ascending';
  SPSCSortDescending                         = 'Sort Descending';
  SPSCClearSortOrder                         = 'Clear Sort Order';
  SPSCIgnoreCase                             = '&Ignore Case';
  SPSCClickHereToAddValue                    = '<Click here to add value>';
  SPSCAddAllFields                           = 'Add All Fields';
  SPSCAddAllFieldParams                      = 'Add All FieldParams';
  SPSCFindFirst                              = 'Find First';
  SPSCFindPrev                               = 'Find Previous';
  SPSCFindNext                               = 'Find Next';
  SPSCFindLast                               = 'Find Last';
  SPSCColorIndicatedCells                    = 'Color indicated cell(s)';
  SPSCFiltered                               = 'Filtered';
  SPSCErrHandlerAlreadyExists                = 'Handler already exists';
  SPSCErrHandlerNotExists                    = 'Handler does not exist';
  SPSCApplyChanges                           = 'Apply changes';
  SPSCCancelChanges                          = 'Cancel changes';
  SPSCErrDupItem                             = 'Can''t insert duplicate item';
  SPSCInvalidColor                           = '''%s'' is invalid color value';
  SPSCInvalidClass                           = 'Can not accept ''%s''';
  SPSCTransparent                            = 'Transparent color';
  SPSCAddSlot                                = 'Add slot';
  SPSCDeleteSlot                             = 'Delete slot';
  SPSCDefaultSection                         = 'Default section';
  SPSCColorSection                           = 'Color section';
  SPSCButtonSection                          = 'Button section';
  SPSCDragBarHint                            = 'Drag to make this menu float';
  SPSCOperationDispAnd                       = 'and';
  SPSCOperationDispOr	                     = 'or';
  SPSCErrSystemSlot                          = 'You can not save to this memory slot';
  SPSCErrMemSlotDelete                       = 'Can''t delete this memory slot';
  SPSCPrmBoxSaveToMem                        = 'Sa&ve to Memory...';
  SPSCPrmBoxLoadFromMem                      = '&Load from Memory...';
  SPSCClickHereToAddItem                     = '<Click here to add item>';
  SPSCClickHereToAddCond                     = '<Click here to add condition>';
  SPSCFltSaveFileCat                         = 'Filters';
  SPSCListPopupAdd                           = '&Add';
  SPSCListPopupDelete                        = '&Delete';
  SPSCListPopupIndent                        = '&Indent';
  SPSCListPopupOutDent                       = 'O&utdent';
  SPSCListPopupSave                          = '&Save...';
  SPSCListPopupLoad                          = '&Open...';
  SPSCListPopupClear                         = '&Clear';
  SPSCDeleteButton                           = 'Delete';
  SPSCUntitled                               = 'untitled';
  SPSCCalcError                              = 'ERROR';
  SPSCErrCharPos                             = ' at Line:%d, Char:%d. ';
  SPSCTokenListEmpty                         = 'Token list is empty';
  SPSCErrFutureState                         = 'State is in future';
  SPSCErrBadFilter                           = 'Bad filter.';
  SPSCBoolDispTrue                           = 'True';
  SPSCBoolDispFalse                          = 'False';
  sPSCErrEmptyFilter                         = 'You can not activate filter with empty filter condition.';
  sPSCErrNoMatchRecord                       = 'There are no records that match the search criteria.';
  sPSCErrEndReached                          = 'The end of the table is reached and no matching records were found.';
  sPSCErrBeginReached                        = 'The begin of the table is reached and no matching record were found.';
  sPSCSearchFromBegin                        = ' Do you want to continue search from the begining of the table?';
  sPSCSearchFromEnd                          = ' Do you want to continue search from the end of the table?';
  SPSCSpecifyFilter                          = 'Specify Filter';
  SPSCFilterLabel                            = '&Filter:';
  SPSCExcludeFilterLabel                     = '&Exclude filter:';
  SPSCBadFieldType                           = 'Bad field type';
  SPSCErrDataSetNilNoFields                  = 'DataSet is NIL and Fields are not specified';
  SPSCErrNoFields                            = 'Fields are not specified';
  SPSCApplyButton                            = '&Apply';
  SPSCPrmBoxEditor                           = 'Edit component...';
  SPSCDefaultString                          = 'Default';
  SPSCTodayButton                            = '&Today';
  SPSCNoneButton                             = '&None';
  SPSCSelectMonth                            = 'Select a month';
  SPSC_AHC_Label                             = 'Select the locations whose holidays you would like copied to your Calendar:';
  SPSC_AHC_AddHolidaysToCal                  = 'Add Holidays to Calendar';
  SPSCPrintStatus                            = 'Printing Status';
  SPSCSearchPrinters                         = 'Searching printers ...';
  SPSCPrintProgress                          = 'Printing pages:    calculation ...';
  SPSCPrintingPage                           = 'Printing page %d from %s';
  SPSCPrintingOnPrinter                      = 'Printing on printer %s';
  SPSCPageNoStr                              = 'Page %d out of %d.';
  SPSCNowButton                              = 'No&w';

  SPSCErrRefCount='Object reference count is not zero';
  SPSCExceptionIn='Exception in %S';
  SPSCErrInvalidfieldkind='Invalid field kind';
  SPSCOperationDispAndNot                    = 'and not';
  SPSCOperationDispOrNot                     = 'or not';

  SPSCErrProgramFailed='Program failed with code (%d)';
  SPSCErrCharSetDef='Invalid charset definition';
  SPSCErrMaxIfDefReached='Only %d nested levels of compiler directives are allowed';
  SPSCErrCompilerDirective='Invalid compiler directive: "%s"';
  SPSCCharExpected = '''''%s'''' expected';
  SPSCSymbolExpected = '%s expected';
  SPSCErrFileNotFound='File %S not found';
  SPSCErrStringsNotSet = 'Strings are not specified';
  SPSCNotProcessedfiles = 'Not processed files:';
  SPSCDirectory  = 'Directory :  ';
  SPSCErrUnkProp = 'Property does not exist';
  SPSCErrLoadFile = 'Can not load %s';
//EndSkipConst
{$ENDIF}

{-------------------------------------------}

var
  FConsts:TPSCConsts;

procedure PSCInitializeConsts;
begin
  FConsts:=TPSCConsts.Create;

  With PSCConsts do
  begin
    ErrProgramFailed                       := SPSCErrProgramFailed;
    ErrCharSetDef                          := SPSCErrCharSetDef;
    ErrMaxIfDefReached                     := SPSCErrMaxIfDefReached;
    ErrCompilerDirective                   := SPSCErrCompilerDirective;
    CharExpected                           := SPSCCharExpected;
    SymbolExpected                         := SPSCSymbolExpected;
    ErrFileNotFound                        := SPSCErrFileNotFound;
    ErrStringsNotSet                       := SPSCErrStringsNotSet;
    NotProcessedfiles                      := SPSCNotProcessedfiles;
    Directory                              := SPSCDirectory;
    ErrUnkProp                             := SPSCErrUnkProp;
    ErrLoadFile                            := SPSCErrLoadFile;
    AssignError                            := SPSCAssignError                            ;
    OKButton                               := SPSCOKButton                               ;
    CancelButton                           := SPSCCancelButton                           ;
    HelpButton                             := SPSCHelpButton                             ;
    ReadError                              := SPSCReadError                              ;
    WriteError                             := SPSCWriteError                             ;
    AdditionalFilter                       := SPSCAdditionalFilter                       ;
    AutoUpdate                             := SPSCAutoUpdate                             ;
    ErrAssignFilterSource                  := SPSCErrAssignFilterSource                  ;
    ErrConnectDataSet                      := SPSCErrConnectDataSet                      ;
    ErrNoFilterSource                      := SPSCErrNoFilterSource                      ;
    ErrCanNotDestroy                       := SPSCErrCanNotDestroy                       ;
    ErrBadPropElem                         := SPSCErrBadPropElem                         ;
    ThereAreNoItems                        := SPSCThereAreNoItems                        ;
    ClickHeretoChangeSortOrder             := SPSCClickHeretoChangeSortOrder             ;
    SortAscending                          := SPSCSortAscending                          ;
    SortDescending                         := SPSCSortDescending                         ;
    ClearSortOrder                         := SPSCClearSortOrder                         ;
    IgnoreCase                             := SPSCIgnoreCase                             ;
    ClickHereToAddValue                    := SPSCClickHereToAddValue                    ;
    Cap_In                                 := SPSCCap_In                                 ;
    Cap_NotIn                              := SPSCCap_NotIn                              ;
    AddAllFields                           := SPSCAddAllFields                           ;
    AddAllFieldParams                      := SPSCAddAllFieldParams                      ;
    FindFirst                              := SPSCFindFirst                              ;
    FindPrev                               := SPSCFindPrev                               ;
    FindNext                               := SPSCFindNext                               ;
    FindLast                               := SPSCFindLast                               ;
    ColorIndicatedCells                    := SPSCColorIndicatedCells                    ;
    Filtered                               := SPSCFiltered                               ;
    ErrHandlerAlreadyExists                := SPSCErrHandlerAlreadyExists                ;
    ErrHandlerNotExists                    := SPSCErrHandlerNotExists                    ;
    ApplyChanges                           := SPSCApplyChanges                           ;
    CancelChanges                          := SPSCCancelChanges                          ;
    ErrDupItem                             := SPSCErrDupItem                             ;
    InvalidColor                           := SPSCInvalidColor                           ;
    InvalidClass                           := SPSCInvalidClass                           ;
    Black                                  := SPSCBlack                                  ;
    Brown                                  := SPSCBrown                                  ;
    OliveGreen                             := SPSCOliveGreen                             ;
    DarkGreen                              := SPSCDarkGreen                              ;
    DarkTeal                               := SPSCDarkTeal                               ;
    DarkBlue                               := SPSCDarkBlue                               ;
    Indigo                                 := SPSCIndigo                                 ;
    Gray80                                 := SPSCGray80                                 ;
    DarkRed                                := SPSCDarkRed                                ;
    Orange                                 := SPSCOrange                                 ;
    DarkYellow                             := SPSCDarkYellow                             ;
    Green                                  := SPSCGreen                                  ;
    Teal                                   := SPSCTeal                                   ;
    Blue                                   := SPSCBlue                                   ;
    BlueGray                               := SPSCBlueGray                               ;
    Gray50                                 := SPSCGray50                                 ;
    Red                                    := SPSCRed                                    ;
    LightOrange                            := SPSCLightOrange                            ;
    Lime                                   := SPSCLime                                   ;
    SeaGreen                               := SPSCSeaGreen                               ;
    Aqua                                   := SPSCAqua                                   ;
    LightBlue                              := SPSCLightBlue                              ;
    Violet                                 := SPSCViolet                                 ;
    Gray40                                 := SPSCGray40                                 ;
    Pink                                   := SPSCPink                                   ;
    Gold                                   := SPSCGold                                   ;
    Yellow                                 := SPSCYellow                                 ;
    BrightGreen                            := SPSCBrightGreen                            ;
    Turquoise                              := SPSCTurquoise                              ;
    SkyBlue                                := SPSCSkyBlue                                ;
    Plum                                   := SPSCPlum                                   ;
    Gray25                                 := SPSCGray25                                 ;
    Rose                                   := SPSCRose                                   ;
    Tan                                    := SPSCTan                                    ;
    LightYellow                            := SPSCLightYellow                            ;
    LightGreen                             := SPSCLightGreen                             ;
    LightTurquoise                         := SPSCLightTurquoise                         ;
    PaleBlue                               := SPSCPaleBlue                               ;
    Lavender                               := SPSCLavender                               ;
    White                                  := SPSCWhite                                  ;
    HotLight                               := SPSCHotLight                               ;
    GradientActiveCaption                  := SPSCGradientActiveCaption                  ;
    GradientInactiveCaption                := SPSCGradientInactiveCaption                ;
    Transparent                            := SPSCTransparent                            ;
    MoreColorsCapt                         := SPSCMoreColorsCapt                         ;
    MoreColors                             := SPSCMoreColors                             ;
    Auto                                   := SPSCAuto                                   ;
    AutoColor                              := SPSCAutoColor                              ;
    None                                   := SPSCNone                                   ;
    NoHighLight                            := SPSCNoHighLight                            ;
    StdColors                              := SPSCStdColors                              ;
    WindowsColors                          := SPSCWindowsColors                          ;
    DocColors                              := SPSCDocColors                              ;
    FontColors                             := SPSCFontColors                             ;
    CustomColors                           := SPSCCustomColors                           ;
    CustomColorsCapt                       := SPSCCustomColorsCapt                       ;
    AddSlot                                := SPSCAddSlot                                ;
    DeleteSlot                             := SPSCDeleteSlot                             ;
    DefaultSection                         := SPSCDefaultSection                         ;
    ColorSection                           := SPSCColorSection                           ;
    ButtonSection                          := SPSCButtonSection                          ;
    DragBarHint                            := SPSCDragBarHint                            ;
    OperationDispAnd                       := SPSCOperationDispAnd                       ;
    OperationDispOr                        := SPSCOperationDispOr	                 ;
    ErrSystemSlot                          := SPSCErrSystemSlot                          ;
    ErrMemSlotDelete                       := SPSCErrMemSlotDelete                       ;
    PrmBoxSaveToMem                        := SPSCPrmBoxSaveToMem                        ;
    PrmBoxLoadFromMem                      := SPSCPrmBoxLoadFromMem                      ;
    ClickHereToAddItem                     := SPSCClickHereToAddItem                     ;
    ClickHereToAddCond                     := SPSCClickHereToAddCond                     ;
    FltSaveFileCat                         := SPSCFltSaveFileCat                         ;
    ListPopupAdd                           := SPSCListPopupAdd                           ;
    ListPopupDelete                        := SPSCListPopupDelete                        ;
    ListPopupIndent                        := SPSCListPopupIndent                        ;
    ListPopupOutDent                       := SPSCListPopupOutDent                       ;
    ListPopupSave                          := SPSCListPopupSave                          ;
    ListPopupLoad                          := SPSCListPopupLoad                          ;
    ListPopupClear                         := SPSCListPopupClear                         ;
    DeleteButton                           := SPSCDeleteButton                           ;
    Cap_Cont                               := SPSCCap_Cont                               ;
    Cap_Is                                 := SPSCCap_Is                                 ;
    Cap_NotCont                            := SPSCCap_NotCont                            ;
    Cap_IsEmpty                            := SPSCCap_IsEmpty                            ;
    Cap_IsNEmpty                           := SPSCCap_IsNEmpty                           ;
    Cap_Equals                             := SPSCCap_Equals                             ;
    Cap_NEquals                            := SPSCCap_NEquals                            ;
    Cap_AtMost                             := SPSCCap_AtMost                             ;
    Cap_AtLeast                            := SPSCCap_AtLeast                            ;
    Cap_More                               := SPSCCap_More                               ;
    Cap_Less                               := SPSCCap_Less                               ;
    Cap_Between                            := SPSCCap_Between                            ;
    Cap_Yesterday                          := SPSCCap_Yesterday                          ;
    Cap_Today                              := SPSCCap_Today                              ;
    Cap_Tomorrow                           := SPSCCap_Tomorrow                           ;
    Cap_Last7                              := SPSCCap_Last7                              ;
    Cap_Next7                              := SPSCCap_Next7                              ;
    Cap_LastWeek                           := SPSCCap_LastWeek                           ;
    Cap_ThisWeek                           := SPSCCap_ThisWeek                           ;
    Cap_NextWeek                           := SPSCCap_NextWeek                           ;
    Cap_LastMon                            := SPSCCap_LastMon                            ;
    Cap_ThisMon                            := SPSCCap_ThisMon                            ;
    Cap_NextMon                            := SPSCCap_NextMon                            ;
    Cap_On                                 := SPSCCap_On                                 ;
    Cap_OnAfter                            := SPSCCap_OnAfter                            ;
    Cap_Onbefore                           := SPSCCap_Onbefore                           ;
    Cap_After                              := SPSCCap_After                              ;
    Cap_Before                             := SPSCCap_Before                             ;
    Cap_BlankEmpty                         := SPSCCap_BlankEmpty                         ;
    templ2                                 := SPSCtempl2                                 ;
    Untitled                               := SPSCUntitled                               ;
    CalcError                              := SPSCCalcError                              ;
    ErrCharPos                             := SPSCErrCharPos                             ;
    TokenListEmpty                         := SPSCTokenListEmpty                         ;
    ErrFutureState                         := SPSCErrFutureState                         ;
    ErrBadFilter                           := SPSCErrBadFilter                           ;
    BoolDispTrue                           := SPSCBoolDispTrue                           ;
    BoolDispFalse                          := SPSCBoolDispFalse                          ;
    Cap_Begins                             := SPSCCap_Begins                             ;
    Cap_Ends                               := SPSCCap_Ends                               ;
    ErrEmptyFilter                         := sPSCErrEmptyFilter                         ;
    ErrNoMatchRecord                       := sPSCErrNoMatchRecord                       ;
    ErrEndReached                          := sPSCErrEndReached                          ;
    ErrBeginReached                        := sPSCErrBeginReached                        ;
    SearchFromBegin                        := sPSCSearchFromBegin                        ;
    SearchFromEnd                          := sPSCSearchFromEnd                          ;
    SpecifyFilter                          := SPSCSpecifyFilter                          ;
    FilterLabel                            := SPSCFilterLabel                            ;
    ExcludeFilterLabel                     := SPSCExcludeFilterLabel                     ;
    Cap_OnDate                             := SPSCCap_OnDate                             ;
    BadFieldType                           := SPSCBadFieldType                           ;
    ErrDataSetNilNoFields                  := SPSCErrDataSetNilNoFields                  ;
    ErrNoFields                            := SPSCErrNoFields                            ;
    ApplyButton                            := SPSCApplyButton                            ;
    PrmBoxEditor                           := SPSCPrmBoxEditor                           ;
    DefaultString                          := SPSCDefaultString                          ;
    TodayButton                            := SPSCTodayButton                            ;
    NoneButton                             := SPSCNoneButton                             ;
    SelectMonth                            := SPSCSelectMonth                            ;
    AHC_Label                              := SPSC_AHC_Label                             ;
    AHC_AddHolidaysToCal                   := SPSC_AHC_AddHolidaysToCal                  ;
    PrintStatus                            := SPSCPrintStatus                            ;
    SearchPrinters                         := SPSCSearchPrinters                         ;
    PrintProgress                          := SPSCPrintProgress                          ;
    PrintingPage                           := SPSCPrintingPage                           ;
    PrintingOnPrinter                      := SPSCPrintingOnPrinter                      ;
    PageNoStr                              := SPSCPageNoStr                              ;
    NowButton                              := SPSCNowButton                              ;

    ErrRefCount                            := SPSCErrRefCount        ;
    ExceptionIn                            := SPSCExceptionIn        ;
    ErrInvalidfieldkind                    := SPSCErrInvalidfieldkind;
    OperationDispAndNot                    := SPSCOperationDispAndNot;
    OperationDispOrNot                     := SPSCOperationDispOrNot ;

  end;
end;

{-------------------------------------------}

function PSCConsts:TPSCConsts;
begin
  If FConsts=nil then
    PSCInitializeConsts;
  Result:=FConsts;
end;

{-------------------------------------------}
initialization
finalization
  FConsts.Free;
  FConsts:=nil;
end.

