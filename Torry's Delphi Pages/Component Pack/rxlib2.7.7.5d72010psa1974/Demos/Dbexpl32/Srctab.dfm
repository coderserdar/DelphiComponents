?
 TSRCTABLEDLG 0p  TPF0TSrcTableDlgSrcTableDlgLeftTop? BorderStylebsDialogCaptionImport to %sClientHeightClientWidth?
Font.ColorclWindowTextFont.Height?	Font.NameMS Sans Serif
Font.Style PositionpoScreenCenterOnCreate
FormCreatePixelsPerInch`
TextHeight TBevelExpandedLeft?Top WidthHeightShape
bsLeftLineVisible  TPanelTopPanelLeft Top Width?Height? AlignalClient
BevelOuterbvNoneTabOrder  TLabelLabel1LeftTopWidth^HeightCaption&Select source table:FocusControlSrcNameEdit  TLabelLabel4LeftTophWidth=HeightCaptionImport &mode:FocusControl	ModeCombo  	TGroupBoxRecordCountBoxLeftTop5Width;Height,Caption &Record count to import TabOrder  TLabelLabel2LeftTopWidth&HeightCaptionrecords FocusControlRecordCntEdit  TRadioButtonFirstRecsBtnLeft? TopWidth0HeightCaption &First TabOrderOnClickAllRecsBtnClick  TRadioButton
AllRecsBtnLeftTopWidth`HeightCaption &All recordsChecked	TabOrder TabStop	OnClickAllRecsBtnClick  TCurrencyEditRecordCntEditLeft? TopWidth7HeightDecimalPlaces DisplayFormat,0Enabled
Font.Color	clBtnFaceFont.Height?	Font.NameMS Sans Serif
Font.Style MaxValue    ????@ParentColor	
ParentFontTabOrder   	TComboBox	ModeComboLeftTopxWidth? HeightStylecsDropDownList
ItemHeightItems.StringsAppendUpdateAppendUpdate TabOrder  TFilenameEditSrcNameEditLeftTopWidth;HeightAcceptFiles	
DefaultExtDBFilter]Paradox or DBase files (*.db; *.dbf)|*.DB;*.DBF|ASCII files (*.txt)|*.TXT|All files (*.*)|*.*DialogOptionsofHideReadOnlyofPathMustExistofFileMustExist DialogTitleBrowse files
ButtonHintBrowse tables|	NumGlyphsParentShowHintShowHint	TabOrderOnChangeSrcNameEditChange  TButtonOkBtnLeftOTopWidthMHeightCaptionOKDefault	EnabledTabOrderOnClick
OkBtnClick  TButton	CancelBtnLeftOTop-WidthMHeightCancel	CaptionCancelModalResultTabOrder  TButtonMapBtnLeftOTopPWidthMHeightEnabledTabOrderOnClickMapBtnClick   TPanelBottomPanelLeft Top? Width?Height? AlignalBottom
BevelOuterbvNoneTabOrder TLabelLabel3LeftTopWidthIHeightCaption&Field mappings:  	TRxDBGridMapGridLeftTopWidth;HeightiColumns	FieldNameSRC_NAMEReadOnly	 	FieldNameDST_NAME  
DataSource
dsMappingsOptions	dgEditingdgTitles
dgColLines
dgRowLinesdgCancelOnExit TabOrder TitleFont.ColorclWindowTextTitleFont.Height?TitleFont.NameMS Sans SerifTitleFont.Style    TFormStorageFormStorageOptions StoredProps.StringsSrcNameEdit.InitialDirRecordCntEdit.Value LeftXTop|  TTableItemsTableFieldsLeftXTop?   TMemoryTableMappingsTab	TableNameMAPTABLeftXTop?  TStringFieldMappingsTabSRC_NAMEDisplayLabelSourceDisplayWidth	FieldNameSRC_NAMESize2  TStringFieldMappingsTabDST_NAMEDisplayLabelDestinationDisplayWidth	FieldNameDST_NAME	OnGetTextMappingsTabDST_NAMEGetText	OnSetTextMappingsTabDST_NAMESetTextSize2   TDataSource
dsMappingsDataSetMappingsTabLefttTop?    