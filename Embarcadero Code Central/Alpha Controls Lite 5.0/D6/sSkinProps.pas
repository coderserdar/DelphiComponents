unit sSkinProps;

interface

const

// Images
  s_BordersMask                           = 'BORDERSMASK';
  s_GripImage                             = 'GRIPIMAGE';
  s_StatusPanelBordersMask                = 'STATUSPANELMASK';
  s_SliderChannelMask                     = 'SLIDERCHANNEL';

  s_PatternFile                           = 'PATTERN';
  s_HotPatternFile                        = 'HOT' + s_PatternFile;

  s_TitleBorder                           = 'TITLEBORDER';
  s_NormalTitleBar                        = 'TITLEBAR';

  s_BorderIconClose                       = 'BICONCLOSE';
  s_BorderIconMaximize                    = 'BICONMAX';
  s_BorderIconNormalize                   = 'BICONNORM';
  s_BorderIconMinimize                    = 'BICONMIN';
  s_BorderIconHelp                        = 'BICONHELP';
  s_TitleButtonMask                       = 'TITLEBUTTON';

  s_SmallIconNormalize                    = 'SICONNORM';
  s_SmallIconMinimize                     = 'SICONMIN';
  s_SmallIconMaximize                     = 'SICONMAX';
  s_SmallIconClose                        = 'SICONCLOSE';

  s_ItemGlyph                             = 'GLYPHMASK';
  s_SliderHorzMask                        = 'SLIDERMASK';
  s_SliderVertMask                        = 'SLIDERVMASK';


// General properties
  // Text and text contours colors
//  s_FXColor                                = 'FXCOLOR';
  s_FontColor                             = 'FONTCOLOR';
  s_TCLeft                                = 'TCLEFT';
  s_TCTop                                 = 'TCTOP';
  s_TCRight                               = 'TCRIGHT';
  s_TCBottom                              = 'TCBOTTOM';
  // Hot text and text contours colors
  s_HotFontColor                          = 'HOTFONTCOLOR';
  s_HotTCLeft                             = 'HOTTCLEFT';
  s_HotTCTop                              = 'HOTTCTOP';
  s_HotTCRight                            = 'HOTTCRIGHT';
  s_HotTCBottom                           = 'HOTTCBOTTOM';

  s_ReservedBoolean                       = 'RESERVEDBOOLEAN';

  s_ParentClass                           = 'PARENTCLASS';
  s_Color                                 = 'COLOR';
  s_Bevel                                 = 'BEVEL';

  s_ShadowBlur                            = 'SHADOWBLUR';
  s_ShadowColor                           = 'SHADOWCOLOR';
  s_ShadowEnabled                         = 'SHADOWENABLED';
  s_ShadowOffset                          = 'SHADOWOFFSET';
  s_ShadowTransparency                    = 'SHADOWTRANSPARENCY';

  s_Transparency                          = 'TRANSPARENCY';
  s_GradientPercent                       = 'GRADIENTPERCENT';
  s_GradientData                          = 'GRADIENTDATA';
  s_ImagePercent                          = 'IMAGEPERCENT';
  s_ShowFocus                             = 'SHOWFOCUS';
  s_FadingEnabled                         = 'FADINGENABLED';
  s_FadingIntervalIn                      = 'FADINGINTERVALIN';
  s_FadingIntervalOut                     = 'FADINGINTERVALOUT';
  s_FadingIterations                      = 'FADINGITERATIONS';
  s_HotColor                              = 'HOTCOLOR';
  s_HotTransparency                       = 'HOTTRANSPARENCY';
  s_HotBevel                              = 'HOTBEVEL';
  s_HotGradientPercent                    = 'HOTGRADIENTPERCENT';
  s_HotGradientData                       = 'HOTGRADIENTDATA';
  s_HotImagePercent                       = 'HOTIMAGEPERCENT';
  s_BorderColor1                          = 'BORDERCOLOR1';
  s_BorderColor2                          = 'BORDERCOLOR2';

  // Standard SkinSections
  s_FormTitle                             = 'FORMTITLE';
  s_Form                                  = 'FORM';
  s_Dialog                                = 'DIALOG';
  s_MDIArea                               = 'MDIAREA';
  s_MainMenu                              = 'MAINMENU';
  s_MenuLine                              = 'MENULINE';
  s_MenuItem                              = 'MENUITEM';
  s_ScrollBar1H                           = 'SCROLLBAR1H';
  s_ScrollBar1V                           = 'SCROLLBAR1V';
  s_ScrollBar2H                           = 'SCROLLBAR2H';
  s_ScrollBar2V                           = 'SCROLLBAR2V';
  s_ScrollSliderV                         = 'SCROLLSLIDERV';
  s_ScrollSliderH                         = 'SCROLLSLIDERH';
  s_ScrollBtnLeft                         = 'SCROLLBTNLEFT';
  s_ScrollBtnTop                          = 'SCROLLBTNTOP';
  s_ScrollBtnRight                        = 'SCROLLBTNRIGHT';
  s_ScrollBtnBottom                       = 'SCROLLBTNBOTTOM';
  s_Divider                               = 'DIVIDER';
  s_ColHeader                             = 'COLHEADER';
  s_ProgressH                             = 'PROGRESSH';
  s_ProgressV                             = 'PROGRESSV';

  s_AlphaEdit                             = 'ALPHAEDIT';

  s_GlobalInfo                          = 'GLOBALINFO';
  s_MasterBitmap                        = 'MASTERBITMAP';
  s_CheckBoxChecked                     = 'BOXCHECKED';
  s_CheckBoxUnChecked                   = 'BOXUNCHECKED';
  s_CheckBoxGrayed                      = 'BOXGRAYED';
  s_RadioButtonChecked                  = 'RADIOCHECKED';
  s_RadioButtonUnChecked                = 'RADIOUNCHECKED';
  s_RadioButtonGrayed                   = 'RADIOGRAYED';
  s_SmallBoxChecked                     = 'SMALLCHECKED';
  s_SmallBoxUnChecked                   = 'SMALLUNCHECKED';
  s_SmallBoxGrayed                      = 'SMALLGRAYED';

  s_Version                             = 'VERSION';
  s_Author                              = 'AUTHOR';
  s_Description                         = 'DESCRIPTION';

  s_TabTop                              = 'TABTOP';
  s_TabBottom                           = 'TABBOTTOM';
  s_TabLeft                             = 'TABLEFT';
  s_TabRight                            = 'TABRIGHT';
  s_Edit                                = 'EDIT';
  s_ToolButton                          = 'TOOLBUTTON';
  s_ComboBox                            = 'COMBOBOX';
  s_AlphaComboBox                       = 'ALPHACOMBOBOX';
  s_Button                              = 'BUTTON';
  s_ButtonBig                           = 'BUTTON_BIG';
  s_SpeedButton                         = 'SPEEDBUTTON';
  s_SpeedButton_Small                   = 'SPEEDBUTTON_SMALL';
  s_Panel                               = 'PANEL';
  s_PanelLow                            = 'PANEL_LOW';
  s_TabControl                          = 'TABCONTROL';
  s_PageControl                         = 'PAGECONTROL';
  s_TabSheet                            = 'TABSHEET';
  s_StatusBar                           = 'STATUSBAR';
  s_ToolBar                             = 'TOOLBAR';
  s_Splitter                            = 'SPLITTER';
  s_GroupBox                            = 'GROUPBOX';
  s_Gauge                               = 'GAUGE';
  s_TrackBar                            = 'TRACKBAR';
  s_CheckBox                            = 'CHECKBOX';
  s_RadioButton                         = 'RADIOBUTTON';
  s_DragBar                             = 'DRAGBAR';
  s_Bar                                 = 'BAR';
  s_BarTitle                            = 'BARTITLE';
  s_BarPanel                            = 'BARPANEL';
  s_WebBtn                              = 'WEBBUTTON';

  s_Unknown                             = 'UNKNOWN';

  // Deprecated
  NormalForm                            = s_Form;
  PatternFile                           = s_PatternFile;
  HotPatternFile                        = s_HotPatternFile;

implementation

end.
