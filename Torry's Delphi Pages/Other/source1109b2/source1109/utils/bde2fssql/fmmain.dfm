�
 TFRMMAIN 0�  TPF0TfrmMainfrmMainLeftZTop� BorderIconsbiSystemMenu
biMinimize BorderStylebsSingleCaptionBDE Transfer to FSSQLClientHeightoClientWidth�Color	clBtnFaceFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style MenumnuMainOldCreateOrder	PositionpoScreenCenterOnCreate
FormCreateOnShowFormShowPixelsPerInch`
TextHeight TImageimgCheckLeftVTopUWidthHeightPicture.Data
�   TBitmap�   BM�       v   (                                       �  �   �� �   � � ��  ��� ���   �  �   �� �   � � ��  ��� UUUUUUUP UU  UU UUUPUUU UUUPVisible  TBitBtnbtnTransferLeft� TopRWidthPHeightHelpContextCaption	Tra&nsferDefault	TabOrder OnClickbtnTransferClick	NumGlyphs  TBitBtnbtnExitLeftITopRWidthPHeightCaptionE&xitTabOrderOnClickbtnExitClick	NumGlyphs  TPageControl
pgTransferLeftTopWidth�HeightE
ActivePage	tabSourceTabOrder 	TTabSheet	tabSourceHelpContextCaption
BDE Source TLabelLabel1LeftTopWidth� HeightCaptionChoose a source database:  TLabelLabel2LeftTop7Width{HeightCaptionSelect one or more tables:  TLabelLabel4Left� TopWidth� HeightCaptionSelect the &fields to transfer:FocusControllstBDEFields  TListBoxlstBDETablesLeftTopHWidth� Height� HelpContext
ItemHeightMultiSelect	Sorted	StylelbOwnerDrawFixedTabOrderOnClicklstBDETablesClick  TListBoxlstBDEFieldsLeft� TopWidth� HeightHelpContextFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style 
ItemHeight
ParentFontStylelbOwnerDrawFixedTabOrder
OnDblClicklstBDEFieldsDblClick
OnDrawItemlstBDEFieldsDrawItem  	TComboBoxcmbBDEAliasesLeftTopWidth� HeightHelpContextStylecsDropDownList
ItemHeightSorted	TabOrder OnChangecmbBDEAliasesChange   	TTabSheet	tabTargetHelpContextCaptionDestination TLabelLabel3LeftTopWidth}HeightCaptionChoose a ta&rget database:  TLabelLabel5Left� TopWidth� HeightCaption&Choose a target ta&ble (double click):FocusControledtFsTableName  TEditedtFsTableNameLeft� TopWidth� HeightHelpContextTabOrderOnChangeedtFsTableNameChangeOnExitedtFsTableNameExit
OnKeyPressedtFsTableNameKeyPress  TListBoxlstFsTablesLeft� Top:Width� Height� HelpContext
ItemHeightSorted	TabOrder
OnDblClicklstFsTablesDblClick  	TComboBoxcmbFsAliasesLeftTopWidth� HeightHelpContextStylecsDropDownList
ItemHeightSorted	TabOrder OnChangecmbFsAliasesChange   	TTabSheet
tabOptionsHelpContextCaptionTransfer Options 	TGroupBoxgrpStringHandlingLeftToppWidth�Height� CaptionString handlingTabOrder  	TCheckBoxchkClearEmptyStringsLeft	TopWidthkHeightCaption)&Clear empty strings (Convert "" to NULL)Checked	State	cbCheckedTabOrder OnClickchkClearEmptyStringsClick  	TCheckBoxchkEmptyStringsLeft	Top0WidthkHeightCaption)&Force empty strings (Convert NULL to "")TabOrderOnClickchkEmptyStringsClick  	TCheckBox
chkOEMAnsiLeft	TopHWidthkHeightCaption&OEM strings to ANSITabOrder  	TCheckBoxchkUseZeroTerminatedStringsLeft	Top]Width� HeightCaptionUse &null-terminated stringsTabOrder   	TGroupBoxgrpMiscLeftTopWidth� HeightbCaptionMiscellaneousTabOrder 	TCheckBoxchkSchemaOnlyLeftTopWidth� HeightCaption&Schema OnlyTabOrder   	TCheckBoxchkUseSysToolsDatesLeftTop-Width� HeightCaptionUse SysTools &datesChecked	State	cbCheckedTabOrder  	TCheckBoxchkUseSysToolsTimesLeftTopEWidth� HeightCaptionUse SysTools &timesChecked	State	cbCheckedTabOrder   TRadioGroupgrpExistingDataLeft� TopWidth� HeightbCaption Existing Data 	ItemIndex Items.StringsKeep structure and &data&Keep structure, replace data&Replace structure and data TabOrder    TTable	tblSourceLeft.TopP  	TMainMenumnuMainLeft
TopP 	TMenuItemmnuOperationsCaptionO&perationsOnClickbtnExitClick 	TMenuItemmnuTransferActiveTableCaption	&TransferOnClickbtnTransferClick  	TMenuItemN1Caption-  	TMenuItemmnuExitCaptionE&xitOnClickbtnExitClick    TFSDatabasedbDestSessionNameSesTrans
RecLockingtlOptimisticNoWaitLeftbTop  TFSTabletblDestBlobAutoStartTransactionBlobModifiedErrorBlobModebmAutoCheckTimeout DeleteTimeout RecLockedBeforeEditRecLockedTypetluDatabase	FieldDefs 
FilterEvalfseLocalFilterTimeout�	FlipOrderBlobChunkSize   SessionNameSesTransSupportRecNoLeft� Top  
TFSSession	fsSession
ClientNametransSessionNameSesTransLeft� Top   	TFSClient	FSClient1
ClientNametransServerEngineFSRemoteServer1Left� Top   TFSRemoteServerFSRemoteServer1	TransportFSParamConnect1Left!Top   TFSParamConnectFSParamConnect1
ServerNameLocalLeftUTop    