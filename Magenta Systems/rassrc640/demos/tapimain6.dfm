�
 TFORM1 0�
  TPF0TForm1Form1Left"TopQCaption/Magenta TAPI Functions Demo Unicode ApplicationClientHeightClientWidth�Color	clBtnFaceFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.Style OldCreateOrderOnClose	FormCloseOnCreate
FormCreatePixelsPerInch`
TextHeight TLabelLabel5LeftrTop
Width� HeightCaptionCanonical Telephone Number  TLabelLabel6LeftrTopiWidthrHeightCaptionDispayable and Dialable  TLabelLabel1Left
TopWidth� HeightCaption"Installed Modem Identified by TAPI  TLabelLabelVersionLeftTop� WidthHeight=AutoSizeCaptionhttp://www.magsys.co.uk/delphi/Font.CharsetDEFAULT_CHARSET
Font.ColorclRedFont.Height�	Font.NameMS Sans Serif
Font.Style 
ParentFontWordWrap	  TLabelLabel2Left� TopWidthpHeightCaptionTAPI Diagnostic Display  TLabelLabel3Left:TopWidth9HeightCaptionTAPI Events  TLabelLabel4LeftTopsWidth� HeightpCaption�Not a Commercial Application
This program demonstrates the Magenta TAPI Functions for Delphi,
how a list of modems is obtained and the TAPI events generated by
those modems.  Your application would normally act upon each 
TAPI event.WordWrap	  TMemoMemo1LeftTop,Width�Height� 
ScrollBarsssBothTabOrder   TListBoxDevListLeft
TopWidthKHeight� 
ItemHeightSorted	TabOrder  TButton	doTAPIMonLeft� Top� WidthtHeightCaptionStart TAPI MonitoringTabOrderOnClickdoTAPIMonClick  TButtondoConfigLeftTopAWidthoHeightCaptionModem PropertiesTabOrderOnClickdoConfigClick  TButtondoTranslateLeftTopWidthoHeightCaptionTelephony PropertiesTabOrderOnClickdoTranslateClick  TEditNumUserLeftrTopWidth� HeightTabOrderText0845 234 5676   TEditNumDispLeftrTop� Width� HeightColorclInactiveCaptionTabOrderTextNumDisp  TButton
doTranAddrLeft|TopAWidthtHeightCaptionCanonical TranslationTabOrderOnClickdoTranAddrClick  TEditNumDialLeftrTop� Width� HeightColorclInactiveCaptionTabOrderTextNumDisp  
TStatusBarStatusLeft Top�Width�HeightPanels SimplePanel	  TButtondoCloseLeft?Top� WidthKHeightCaptionCloseTabOrder
OnClickdoCloseClick  TMemoMemo2LeftTop,Width� Height� Lines.Strings  
ScrollBarsssBothTabOrder  TButtondoListModemsLeftTop� WidthoHeightCaptionList TAPI ModemsTabOrderOnClickdoListModemsClick  TButton
doDumpCapsLeftrTop� WidthVHeightCaption
Dump ModemTabOrderOnClickdoDumpCapsClick  TSaveDialogSaveDumpOptionsofOverwritePromptofHideReadOnlyofEnableSizing Left�Top�    