�
 TFRMMAIN 0L  TPF0TfrmMainfrmMainLeft� Top� BorderIconsbiSystemMenu
biMinimize BorderStylebsSingleCaptionBDE Transfer to FlashFilerClientHeightoClientWidth�
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style MenumnuMainPositionpoScreenCenterOnCreate
FormCreateOnShowFormShowPixelsPerInch`
TextHeight TImageimgCheckLeftVTopUWidthHeightPicture.Data
�   TBitmap�   BM�       v   (                                       �  �   �� �   � � ��  ��� ���   �  �   �� �   � � ��  ��� UUUUUUUP UU  UU UUUPUUU UUUPVisible  TBitBtnbtnTransferLeft� TopRWidthPHeightHelpContextCaption	Tra&nsferDefault	TabOrder OnClickbtnTransferClick	NumGlyphs  TBitBtnbtnExitLeft� TopRWidthPHeightCaptionE&xitTabOrderOnClickbtnExitClick	NumGlyphs  TBitBtnbtnHelpLeftJTopRWidthPHeightCaption&HelpTabOrderOnClickbtnHelpClick	NumGlyphs  TPageControl
pgTransferLeftTopWidth�HeightE
ActivePage
tabOptionsTabOrder 	TTabSheet	tabSourceHelpContextCaption
BDE Source TLabelLabel1LeftTopWidth� HeightCaptionChoose a source database:  TLabelLabel2LeftTop7Width{HeightCaptionSelect one or more tables:  TLabelLabel4Left� TopWidth� HeightCaptionSelect the &fields to transfer:FocusControllstBDEFields  TListBoxlstBDETablesLeftTopHWidth� Height� HelpContext
ItemHeightMultiSelect	Sorted	StylelbOwnerDrawFixedTabOrderOnClicklstBDETablesClick  TListBoxlstBDEFieldsLeft� TopWidth� HeightHelpContext
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style 
ItemHeight
ParentFontStylelbOwnerDrawFixedTabOrder
OnDblClicklstBDEFieldsDblClick
OnDrawItemlstBDEFieldsDrawItem  	TComboBoxcmbBDEAliasesLeftTopWidth� HeightHelpContextStylecsDropDownList
ItemHeight Sorted	TabOrder OnChangecmbBDEAliasesChange   	TTabSheet	tabTargetHelpContextCaptionFlashFiler Destination TLabelLabel3LeftTopWidth}HeightCaptionChoose a ta&rget database:  TLabelLabel5Left� TopWidth� HeightCaption&Choose a target ta&ble (double click):FocusControledtFFTableName  TEditedtFFTableNameLeft� TopWidth� HeightHelpContextTabOrderOnChangeedtFFTableNameChangeOnExitedtFFTableNameExit
OnKeyPressedtFFTableNameKeyPress  TListBoxlstFFTablesLeft� Top:Width� Height� HelpContext
ItemHeightSorted	TabOrder
OnDblClicklstFFTablesDblClick  	TComboBoxcmbFFAliasesLeftTopWidth� HeightHelpContextStylecsDropDownList
ItemHeight Sorted	TabOrder OnChangecmbFFAliasesChange   	TTabSheet
tabOptionsHelpContextCaptionTransfer Options 	TGroupBoxgrpStringHandlingLeftToppWidth�Height� CaptionString handlingTabOrder  	TCheckBoxchkClearEmptyStringsLeft	TopWidthkHeightCaption)&Clear empty strings (Convert "" to NULL)State	cbCheckedTabOrder OnClickchkClearEmptyStringsClick  	TCheckBoxchkEmptyStringsLeft	Top0WidthkHeightCaption)&Force empty strings (Convert NULL to "")TabOrderOnClickchkEmptyStringsClick  	TCheckBox
chkOEMAnsiLeft	TopHWidthkHeightCaption&OEM strings to ANSITabOrder  	TCheckBoxchkUseANSIFieldsLeft	Top`Width� HeightCaptionUse &ANSI stringsTabOrder  	TCheckBoxchkUseZeroTerminatedStringsLeft	TopxWidth� HeightCaptionUse &null-terminated stringsTabOrder   	TGroupBoxgrpMiscLeftTopWidth� HeightbCaptionMiscellaneousTabOrder 	TCheckBoxchkSchemaOnlyLeftTopWidth� HeightCaption&Schema OnlyTabOrder   	TCheckBoxchkUseSysToolsDatesLeftTop-Width� HeightCaptionUse SysTools &datesTabOrder  	TCheckBoxchkUseSysToolsTimesLeftTopEWidth� HeightCaptionUse SysTools &timesTabOrder   TRadioGroupgrpExistingDataLeft� TopWidth� HeightbCaption Existing Data 	ItemIndex Items.StringsKeep structure and &data&Keep structure, replace data&Replace structure and data TabOrder    TTable	tblSourceLeft.TopP  	TMainMenumnuMainLeft
TopP 	TMenuItemmnuOperationsCaptionO&perationsOnClickbtnExitClick 	TMenuItemmnuTransferActiveTableCaption	&TransferOnClickbtnTransferClick  	TMenuItemN1Caption-  	TMenuItemmnuExitCaptionE&xitOnClickbtnExitClick   	TMenuItemmnuHelpCaption&Help 	TMenuItemmnuHelpContentsCaptionContentsOnClickbtnHelpClick  	TMenuItemmnuAboutCaptionAboutOnClickmnuAboutClick    TffTabletblDestSessionName[automatic]LeftxTop'  TffDatabasedbDestSessionName[automatic]LeftXTop'   