џ
 TUSERADMINISTRATION 0(v  TPF0TUserAdministrationUserAdministrationLeft TopRBorderIconsbiSystemMenu BorderStylebsDialogCaption Administraчуo de UsuсriosClientHeightУClientWidthxColor	clBtnFaceFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Heightѕ	Font.NameArial
Font.Style 
KeyPreview	OldCreateOrder	PositionpoScreenCenterShowHint	
OnActivateFormActivateOnClose	FormCloseOnCloseQueryFormCloseQuery	OnDestroyFormDestroy	OnKeyDownFormKeyDownPixelsPerInch`
TextHeight TPanelPanel1Left Top WidthxHeightУAlignalClient
BevelOuterbvNoneBorderWidthTabOrder  TPageControlPageControlLeftTopWidthtHeightП
ActivePage
tsProgramsAlignalClientTabOrder OnChangePageControlChange 	TTabSheettsUsersCaptionUsuсrios TPageControlpcUsersLeft TopWidthlHeight
ActivePagetsUsersListAlignalClientTabOrder OnChangepcUsersChangeOnEnterpcUsersChangeOnExittsUsersDataExit 	TTabSheettsUsersListCaptionLista de UsuсriosOnEntertsUsersListEnter TDBGridDBGridUsersLeft Top WidthdHeightmHintPress F2 to SearchAlignalClient
DataSourceUsers.dsUsersOptionsdgTitlesdgIndicatordgColumnResize
dgColLines
dgRowLinesdgTabsdgRowSelectdgConfirmDeletedgCancelOnExit TabOrder TitleFont.CharsetDEFAULT_CHARSETTitleFont.ColorclWindowTextTitleFont.HeightѕTitleFont.NameArialTitleFont.Style ColumnsExpanded	FieldNameUSERCS_NAMEVisible	 Expanded	FieldName	REAL_NAMEWidthѓ Visible	 Expanded	FieldNamePROFILEWidthА Visible	 Expanded	FieldNameCalcUserActiveVisible	     	TTabSheettsUsersDataHintPress F2 to SearchCaptionDados do UsuсrioOnEntertsUsersDataEnterOnExittsUsersDataExit TLabellblLAST_PWD_CHANGELeft Top WidthiHeightCaptionLAST_PWD_CHANGEFocusControlDBEdit4  TLabellblUSER_NAMELeftTopWidthLHeightCaptionUSERCS_NAMEFocusControlDBEdit1  TLabellblREAL_NAMELeftfTopWidth>HeightCaption	REAL_NAMEFocusControlDBEdit2  TLabellblPASSWORDLeftTop0Width<HeightCaptionPASSWORDFocusControlDBEdit3  TLabellblEXPIRATION_DATELeft Top`Width\HeightCaptionEXPIRATION_DATE  TLabel
lblPROFILELeft Top0Width)HeightCaptionPROFILEFocusControldblkProfileName  TDBEditDBEdit4Left Top Width@Height	DataFieldLAST_PWD_CHANGE
DataSourceUsers.dsUsersReadOnly	TabOrder   TDBEditDBEdit1LeftTopWidth]Height	DataFieldUSERCS_NAME
DataSourceUsers.dsUsersTabOrder  TDBEditDBEdit2LeftfTopWidthє Height	DataField	REAL_NAME
DataSourceUsers.dsUsersTabOrder  TDBEditDBEdit3LeftTop@Width|Height	DataFieldUSER_PWD
DataSourceUsers.dsUsersEnabledPasswordChar*TabOrder  TDBCheckBoxcbxUSER_EXPIRELeftTopqWidthaHeightCaptionUSER_EXPIRE	DataFieldUSER_EXPIRE
DataSourceUsers.dsUserInfoTabOrderValueChecked1ValueUnchecked0OnClickcbxUSER_EXPIREClick  TDBCheckBoxcbxUSER_ACTIVELeftTop WidthaHeightCaptionUSER_ACTIVE	DataFieldUSER_ACTIVE
DataSourceUsers.dsUserInfoTabOrderValueChecked1ValueUnchecked0  TDBLookupComboBoxdblkProfileNameLeft Top@Widthж Height	DataFieldPROFILE
DataSourceUsers.dsUsersTabOrder  TButtonbtnChangePasswordLeftTopи WidthdHeightCaption&Mudar SenhaTabOrderOnClickbtnChangePasswordClick  TDateTimePickerDBDateTimePicker1Left ToprWidthiHeightCalAlignmentdtaLeftDate      @Time      @
DateFormatdfShortDateMode
dmComboBoxFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Heightє	Font.NameArial
Font.Style KinddtkDate
ParseInput
ParentFontTabOrderOnChangeDBDateTimePicker1Change  TDBCheckBoxcbxAUDIT_MODELeftTop WidthrHeightCaptionModo de Auditoria	DataField
AUDIT_MODE
DataSourceUsers.dsUsersTabOrder	ValueChecked1ValueUnchecked0OnClickcbxUSER_EXPIREClick  TDBNavigatorDBNavigator2LeftTopј Widthм Height
DataSourceUsers.dsUsersVisibleButtonsnbFirstnbPriornbNextnbLastnbPost TabOrder
BeforeActionDBNavigator2BeforeActionOnClickDBNavigator2Click  TDBCheckBoxcbxUSER_IS_ADMINLeftTopЗ Width HeightCaptionUSER_IS_ADMIN	DataFieldUSER_IS_ADMIN
DataSourceUsers.dsUserInfoTabOrderValueChecked1ValueUnchecked0   	TTabSheettabAdditionalInfoCaption!Informaчѕes Adicionais do UsuсrioOnEntertabAdditionalInfoEnterOnExittabAdditionalInfoExit   TPanelToolBarLeft Top WidthlHeightAlignalTop
BevelOuterbvNoneParentShowHintShowHint	TabOrder TSpeedButton	sbNewUserLeftTopWidthdHeightCaptionNovo UsuсrioOnClicksbNewUserClick  TSpeedButtonsbUserAccessLeftЩ TopWidthdHeightCaption     Acessos    OnClicksbUserAccessClick  TSpeedButtonsbChangeUserDataLeftfTopWidthdHeightCaptionMudar DadosOnClicksbChangeUserDataClick  TSpeedButtonsbDeleteUserLeft-TopWidthdHeightCaptionEliminar UsuсrioOnClicksbDeleteUserClick  TSpeedButtonsbPrintUserListLeftTopWidthdHeightCaptionImprimir ListaOnClicksbPrintUserListClick  TSpeedButtonsbPrintUserAccessLeftѕTopWidthdHeightCaptionImprimir PermissѕesOnClicksbPrintUserAccessClick    	TTabSheet
tsProfilesCaptionPerfis de Usuсrio TPageControl
pcProfilesLeft TopWidthlHeight
ActivePagetsProfilesListAlignalClientTabOrder  	TTabSheettsProfilesListCaptionLista de Perfis TDBGridDBGridProfilesLeft Top WidthdHeightmHintPress F2 to SearchAlignalClient
DataSourceUsers.dsProfilesOptionsdgTitlesdgIndicatordgColumnResize
dgColLines
dgRowLinesdgTabsdgRowSelectdgConfirmDeletedgCancelOnExit TabOrder TitleFont.CharsetDEFAULT_CHARSETTitleFont.ColorclWindowTextTitleFont.HeightѕTitleFont.NameArialTitleFont.Style ColumnsExpanded	FieldNamePROF_DESCRIPTIONWidthл Visible	 Expanded	FieldNameCalcMustChangePwdVisible	 Expanded	FieldNameINTERVAL_CHANGE_PWDVisible	     	TTabSheettsProfilesDataHintPress F2 to SearchCaptionDados do PerfilOnEntertsProfilesDataEnterOnExittsProfilesDataExit TLabellblPROF_DESCRIPTIONLeftTopWidthbHeightCaptionPROF_DESCRIPTIONFocusControlDBEdit5  TLabellblINTERVAL_CHANGE_PWDLeftTopXWidth HeightCaptionINTERVAL_CHANGE_PWDFocusControlDBEdit6  TDBEditDBEdit5LeftTopWidthИ Height	DataFieldPROF_DESCRIPTION
DataSourceUsers.dsProfilesTabOrder   TDBEditDBEdit6LeftTophWidth Height	DataFieldINTERVAL_CHANGE_PWD
DataSourceUsers.dsProfilesTabOrder  TDBCheckBoxcbxMUST_CHANGE_PWDLeftTop>Width HeightCaptionMUST_CHANGE_PWD	DataFieldMUST_CHANGE_PWD
DataSourceUsers.dsProfilesTabOrderValueChecked1ValueUnchecked0OnClickcbxMUST_CHANGE_PWDClick  TDBCheckBoxcbxAUDIT_MODEProfileLeftЄ TopiWidthrHeightCaption
AUDIT_MODE	DataField
AUDIT_MODE
DataSourceUsers.dsProfilesTabOrderValueChecked1ValueUnchecked0OnClickcbxUSER_EXPIREClick  TDBNavigatorDBNavigator3LeftTopи Widthм Height
DataSourceUsers.dsProfilesVisibleButtonsnbFirstnbPriornbNextnbLastnbPost TabOrder    TPanelPanel3Left Top WidthlHeightAlignalTop
BevelOuterbvNoneParentShowHintShowHint	TabOrder TSpeedButtonsbNewProfileLeftTopWidthdHeightCaptionNovo PerfilOnClicksbNewProfileClick  TSpeedButtonsbProfileAccessLeftЪ TopWidthdHeightCaption     Acessos    OnClicksbProfileAccessClick  TSpeedButtonsbChangeProfileDataLeftfTopWidthdHeightCaptionMudar DadosOnClicksbChangeProfileDataClick  TSpeedButtonsbDeleteProfileLeft.TopWidthdHeightCaptionEliminar PerfilOnClicksbDeleteProfileClick  TSpeedButtonsbPrintProfileListLeftTopWidthdHeightCaptionImprimir ListaOnClicksbPrintProfileListClick  TSpeedButtonsbPrintProfileAccessLeftіTopWidthdHeightCaptionImprimir AcessosOnClicksbPrintProfileAccessClick    	TTabSheet
tsProgramsCaption Controle de Acesso aos ProgramasOnEntertsProgramsEnterOnExittsProgramsExit TPageControl
pcProgramsLeftNTop WidthHeightЂ
ActivePagetsUsersProgsAlignalClientTabOrder OnChangepcProgramsChange 	TTabSheettsUsersProgsCaptionUsuсrios TDBGridDBGridUsersProgsLeft Top WidthHeightHintPress F2 to SearchAlignalClientCtl3D
DataSourceUsers.dsUsers
FixedColorclActiveCaptionFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Heightѕ	Font.NameArial
Font.Style OptionsdgTitles
dgColLines
dgRowLinesdgTabsdgRowSelectdgAlwaysShowSelectiondgConfirmDeletedgCancelOnExit ParentCtl3D
ParentFontTabOrder TitleFont.CharsetDEFAULT_CHARSETTitleFont.ColorclCaptionTextTitleFont.HeightѕTitleFont.NameArialTitleFont.StylefsBold ColumnsExpanded	FieldNameUSERCS_NAMEWidthdVisible	 Expanded	FieldNamePROFILEWidth Visible	     	TTabSheettsProfilesProgsCaptionPerfis de Usuсrio TDBGridDBGridProfilesProgsLeft Top WidthHeightHintPress F2 to SearchAlignalClientCtl3D
DataSourceUsers.dsProfiles
FixedColorclActiveCaptionFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Heightѕ	Font.NameArial
Font.Style OptionsdgTitles
dgColLines
dgRowLinesdgTabsdgRowSelectdgAlwaysShowSelectiondgConfirmDeletedgCancelOnExit ParentCtl3D
ParentFontTabOrder TitleFont.CharsetDEFAULT_CHARSETTitleFont.ColorclCaptionTextTitleFont.HeightѕTitleFont.NameArialTitleFont.StylefsBold ColumnsExpanded	FieldNamePROF_DESCRIPTIONWidthVisible	      TPanelPanel2Left Top WidthNHeightЂAlignalLeft
BevelOuterbvNoneTabOrder 	TTreeViewCompListLeft TopUWidthNHeightMAlignalClientFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Heightє	Font.NameArial
Font.Style Images	ImageListIndent
ParentFontParentShowHint	PopupMenuPopupTreeViewReadOnly	ShowHintTabOrder 
OnDeletionCompListDeletionOnMouseDownCompListMouseDown  TPanelPanel6Left Top WidthNHeightAlignalTop	AlignmenttaLeftJustify
BevelOuterbvNoneTabOrder TSpeedButtonsbApplyLeftTopWidthnHeightCaptionAplicarOnClicksbApplyClick  TSpeedButton	sbRestoreLeftpTopWidthnHeightCaption	RestaurarOnClicksbRestoreClick  TSpeedButtonsbRestoreProfileLeftо TopWidthnHeightCaptionRestaurar do PerfilOnClicksbRestoreProfileClick   TPanelPanelAboveProgsLeft TopAWidthNHeightAlignalTop	AlignmenttaLeftJustify
BevelInnerbvRaised
BevelOuterbvNoneCaption' Acessos para o usuсrio XXXXXXXXXXXXXXXColorclActiveCaptionFont.CharsetDEFAULT_CHARSET
Font.ColorclCaptionTextFont.Heightѕ	Font.NameArial
Font.StylefsBold 
ParentFontTabOrder  	TGroupBoxgbxFormListLeft TopWidthNHeight)AlignalTopCaption Lista de Formulсrios TabOrder TDBLookupComboBox	dblkFormsLeftTopWidthBHeightKeyFieldFORM_ID	ListFieldFORM_CAPTION
ListSourceUsers.dtsFormsTabOrder OnClickdblkFormsCloseUp   TPanelPanelLegendaLeft0Top Widthя HeightЁ TabOrderVisible TSpeedButtonSpeedButton1Left
TopWidthHeight
Glyph.Data
z  v  BMv      6   (               @                  џџџџџџџџџџџџџџџџџџџџџџџџџџџ         џџџ         џџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџ   џџџџџџџџџџџџ            џџџ            џџџџџџ   џџџџџџџџџ   џџџџџџџџџ   џџџ   џџџџџџџџџ   џџџ   џџџџџџџџџ   џџџџџџџџџ   џџџ   џџџџџџџџџ   џџџ   џџџџџџџџџџџџ            џџџ   џџџџџџџџџ   џџџ   џџџџџџџџџџџџџџџџџџџџџ   џџџ   џџџџџџџџџ   џџџ   џџџџџџџџџџџџ         џџџџџџ            џџџџџџ   џџџџџџџџџџџџџџџџџџџџџџџџџџџ   џџџџџџџџџџџџџџџ   џџџџџџџџџџџџџџџџџџџџџџџџџџџ   џџџџџџџџџџџџџџџ   џџџџџџџџџџџџџџџџџџџџџџџџџџџ   џџџџџџџџџџџџџџџ   џџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџ         џџџ           TSpeedButtonSpeedButton2Left
Top:WidthHeight
Glyph.Data
J  F  BMF      6   (                                 џџџџџџџџџџџџ            џџџџџџџџџ            џџџџџџџџџџџџ      џџ џџ       џџџ      џџ џџ       џџџџџџџџџ   џџџџџџџџ џџ    џџџ   џџџџџџџџ џџ    џџџџџџџџџ   џџџџџџџџ џџ    џџџ   џџџџџџџџ џџ    џџџџџџџџџ   џџџџџџџџџџџ    џџџ   џџџџџџџџџџџ    џџџџџџџџџ      џџџџџџ               џџџџџџ      џџџџџџџџџџџџ            џџџ   џџџ            џџџџџџџџџџџџџџџџџџ   џџџџџџџџџџџџџџџџџџџџџ   џџџџџџ   џџџџџџџџџ   џџџџџџџџџ   џџџџџџџџџ   џџџџџџџџџ   џџџџџџ   џџџџџџџџџџџџ   џџџџџџ   џџџџџџџџџџџџџџџ      џџџџџџџџџџџџџџџџџџ      џџџџџџџџџџџџџџџ  TSpeedButtonSpeedButton3Left
TopXWidthHeight
Glyph.Data
к  ж  BMж      6   (                                  џџџџџџџџџџџџџџџ            џџџџџџџџџџџџџџџџџџџџџџџџџџџ  џ  џ  џ  џ  џ  џ  џ          џџџџџџџџџџџџ  џ  џ  џџ       џџџ     џ  џ        џџџџџџ  џ  џ  џџџџџ џџ    џџџ  џ  џ  џ  џџ    џџџ  џ  џ  џџџџџџџџ џџ      џ  џ  џџџџ  џ     џџџ  џ  џ   џџџџџџџџџџџ   џ  џ  џџџџџџ  џ     џџџ  џ  џ      џџџџџџ  џ  џ        џџџ  џ     џџџ  џ  џџџџ        џ  џ     џџџ        џ  џџџџџџ  џ  џџџџџџџ  џ  џ  џџџџџџџџџџџџџџџ  џ  џџџ     џ  џџџџ  џ  џ  џџџ   џџџџџџџџџ  џ  џ  џџџ   џџџ  џ  џ  џ  џџџџџџџ   џџџџџџ  џ  џ  џџџџџџџџџџ        џ  џџџџџџџџџџџџџ     џ  џ  џџџџџџџџџџџџџџџџџџџџџџ  џ  џ  џ  џ  џ  џ  џ  џџџџџџџџџџџџџџџџџџџџџџџџџџџџ  џ  џ  џ  џ  џ  џџџџџџџџџџџџџџџџ  TPanelpnlLegendaEnabledLeft'TopWidthО Height	AlignmenttaLeftJustify
BevelInnerbvRaised
BevelOuterbvNoneCaption HabilitadoColorclActiveCaptionFont.CharsetANSI_CHARSET
Font.ColorclCaptionTextFont.Heightѕ	Font.NameArial
Font.StylefsBold 
ParentFontTabOrder   TPanelpnlTopLegendaLeftTopWidthэ HeightAlignalTop	AlignmenttaLeftJustify
BevelInnerbvRaised
BevelOuterbvNoneCaption LegendaColorclActiveCaptionFont.CharsetANSI_CHARSET
Font.ColorclCaptionTextFont.Heightѕ	Font.NameArial
Font.StylefsBold 
ParentFontTabOrder  TPanelpnlLegendaOnlyVisibleLeft'Top;WidthО Height	AlignmenttaLeftJustify
BevelInnerbvRaised
BevelOuterbvNoneCaptionVisэvel, mas DesabilitadoColorclActiveCaptionFont.CharsetANSI_CHARSET
Font.ColorclCaptionTextFont.Heightѕ	Font.NameArial
Font.StylefsBold 
ParentFontTabOrder  TPanelpnlLegendaInvisibleLeft'TopYWidthО Height	AlignmenttaLeftJustify
BevelInnerbvRaised
BevelOuterbvNoneCaption
 InvisэvelColorclActiveCaptionFont.CharsetANSI_CHARSET
Font.ColorclCaptionTextFont.Heightѕ	Font.NameArial
Font.StylefsBold 
ParentFontTabOrder  TBitBtn
btnLegendaLeftKTop WidthYHeightCaptionFechar LegendaTabOrderOnClickbtnLegendaClick     	TTabSheettsAuditCaption	Auditoria TPageControlpcAuditLeft Top WidthlHeightЂ
ActivePagetsCurrentUsersAlignalClientTabOrder OnChangepcAuditChangeOnEnterpcAuditEnterOnExitpcAuditExit 	TTabSheettsLoginActivityCaptionAtividades de Login do Usuсrio TDBGriddbgLoginActivityLeft TopFWidthdHeight?AlignalClient
DataSourceUsers.dtsLoginTraceOptionsdgTitlesdgIndicatordgColumnResize
dgColLines
dgRowLinesdgTabsdgRowSelectdgAlwaysShowSelectiondgConfirmDeletedgCancelOnExit TabOrder TitleFont.CharsetDEFAULT_CHARSETTitleFont.ColorclWindowTextTitleFont.HeightѕTitleFont.NameArialTitleFont.Style ColumnsExpanded	FieldNameLOGIN_DATE_TIMEWidth Visible	 Expanded	FieldNameLOGOUT_DATE_TIMEWidth Visible	 Expanded	FieldNameCOMPUTER_NAMEWidth3Visible	    TPanelPanel8Left Top WidthdHeightFAlignalTopTabOrder TLabellblUser1LeftTopWidth%HeightCaptionUsuсrio  TSpeedButtonsbDeleteUserLoginActivityLeft|Top,Width<HeightCaptionEliminarOnClicksbDeleteUserLoginActivityClick  TSpeedButtonsbPrintUserLoginActivityLeftЙ Top,Width<HeightCaptionImprimirOnClicksbPrintUserLoginActivityClick  TDBNavigatorDBNavigator6LeftTop,WidthtHeight
DataSourceUsers.dtsLoginTraceVisibleButtonsnbFirstnbPriornbNextnbLast TabOrder   TDBLookupComboBoxdblkUserName2LeftTopWidthг HeightKeyFieldUSER_ID	ListFieldUSERCS_NAME
ListSourceUsers.dsUserNameTabOrderOnClickdtpkInicioLTChange	OnCloseUpdtpkInicioLTChange  	TGroupBoxgbxPeriodActivities1LeftаTopWidth HeightDAlignalRightCaption Perэodo de Atividades TabOrder TLabellblFrom1LeftTopWidthHeightCaptionDesde  TLabellblTo1LeftTop,Width HeightCaption     Atщ  TDateTimePickerdtpkInicioLTLeft9TopWidthQHeightCalAlignmentdtaLeftDate      Ѓ@Time      Ѓ@
DateFormatdfShortDateMode
dmComboBoxKinddtkDate
ParseInputTabOrder OnClickdtpkInicioLTChangeOnChangedtpkInicioLTChange  TDateTimePicker	dtpkFimLTLeft9Top)WidthQHeightCalAlignmentdtaLeftDate      Ѓ@Time      Ѓ@
DateFormatdfShortDateMode
dmComboBoxKinddtkDate
ParseInputTabOrderOnClickdtpkInicioLTChangeOnChangedtpkInicioLTChange     	TTabSheettsUserActivityCaptionAtividades dos Usuсrios TPanelPanel5Left Top WidthdHeightFAlignalTopTabOrder  TLabellblUserLeftр TopWidth%HeightCaptionUsuсrio  TSpeedButtonsbDeleteAuditUserLeft|Top,Width<HeightCaptionEliminarOnClicksbDeleteAuditUserClick  TLabellblForm1LeftTopWidth2HeightCaption
Formulсrio  TSpeedButtonsbPrintUserActivityLeftЙ Top,Width<HeightCaptionImprimirOnClicksbPrintUserActivityClick  TDBNavigatorDBNavigator4LeftTop,WidthtHeight
DataSourceUsers.dsAuditVisibleButtonsnbFirstnbPriornbNextnbLast TabOrder   TDBLookupComboBoxdblkUserNameLeftр TopWidthг HeightKeyFieldUSER_ID	ListFieldUSERCS_NAME
ListSourceUsers.dsUserNameTabOrderOnClickdtpkInicioPChange	OnCloseUpdtpkInicioPChange  	TGroupBoxgbxPeriodActivitiesLeftаTopWidth HeightDAlignalRightCaption Perэodo de Atividades TabOrder TLabellblFromLeftTopWidthHeightCaptionDesde  TLabellblToLeftTop,Width HeightCaption     Atщ  TDateTimePickerdtpkInicioULeft9TopWidthQHeightCalAlignmentdtaLeftDate      Ѓ@Time      Ѓ@
DateFormatdfShortDateMode
dmComboBoxKinddtkDate
ParseInputTabOrder OnClickdtpkInicioPChangeOnChangedtpkInicioPChange  TDateTimePickerdtpkFimULeft9Top)WidthQHeightCalAlignmentdtaLeftDate      Ѓ@Time      Ѓ@
DateFormatdfShortDateMode
dmComboBoxKinddtkDate
ParseInputTabOrderOnClickdtpkInicioPChangeOnChangedtpkInicioPChange   TDBLookupComboBox
dblkForms1LeftTopWidthг HeightKeyFieldFORM_ID	ListFieldFORM_CAPTION
ListSourceUsers.dtsFormsTabOrderOnClickdblkForms1Click	OnCloseUpdblkForms1Click   TDBGridDBGridAudUsersLeft TopFWidthЋHeight?AlignalClient
DataSourceUsers.dsAuditOptionsdgTitlesdgIndicatordgColumnResize
dgColLines
dgRowLinesdgTabsdgRowSelectdgAlwaysShowSelectiondgConfirmDeletedgCancelOnExit ReadOnly	TabOrderTitleFont.CharsetDEFAULT_CHARSETTitleFont.ColorclWindowTextTitleFont.HeightѕTitleFont.NameArialTitleFont.Style ColumnsExpanded	FieldNameCOMP_CAPTIONWidth Visible	 Expanded	FieldName	DATE_UTILWidth Visible	 Expanded	FieldNameCOMPUTER_NAMEWidthnVisible	    TPanelPanel10LeftЋTopFWidthЙ Height?AlignalRight
BevelOuterbvNoneTabOrder 	TGroupBoxgbxAddInfo1Left Top WidthЙ Height?AlignalClientCaptionInformaчѕes AdicionaisTabOrder  
TScrollBox
ScrollBox1LeftTopWidthЕ Height-HorzScrollBar.Tracking	AlignalClient
AutoScrollBorderStylebsNoneTabOrder  TDBTextDBText1Left Top Width)HeightAlignalTopAutoSize		DataFieldADDITIONAL_INFO1
DataSourceUsers.dsAuditWordWrap	  TDBTextDBText2Left TopWidth)HeightAlignalTopAutoSize		DataFieldADDITIONAL_INFO2
DataSourceUsers.dsAuditWordWrap	  TBevelBevel1Left TopWidthЕ HeightAlignalTopShape	bsTopLine      	TTabSheettsCompUtilizationCaptionUtilizaчуo de Componentes TPanelPanel7Left Top WidthdHeightFAlignalTopTabOrder  TLabellblComponentLeftр TopWidth<HeightCaption
Componente  TSpeedButtonsbDeleteAuditProgLeft|Top,Width<HeightCaptionEliminarOnClicksbDeleteAuditProgClick  TLabellblForm2LeftTopWidth2HeightCaption
Formulсrio  TSpeedButtonsbPrintCompUtilizationLeftЙ Top,Width<HeightCaptionImprimirOnClicksbPrintCompUtilizationClick  TDBLookupComboBoxdblkCompNameLeftр TopWidthг HeightKeyFieldCOMP_ID	ListFieldCOMP_CAPTION
ListSourceUsers.dsCompsTabOrder OnClickdtpkInicioPChange	OnCloseUpdtpkInicioPChange  TDBNavigatorDBNavigator5LeftTop,WidthtHeight
DataSourceUsers.dsAuditVisibleButtonsnbFirstnbPriornbNextnbLast TabOrder  	TGroupBoxgbxPeriodActivities2LeftаTopWidth HeightDAlignalRightCaption Perэodo de Atividades TabOrder TLabellblFrom2LeftTopWidthHeightCaptionDesde  TLabellblTo2LeftTop,Width HeightCaption     Atщ  TDateTimePickerdtpkInicioPLeft9TopWidthQHeightCalAlignmentdtaLeftDate      Ѓ@Time      Ѓ@
DateFormatdfShortDateMode
dmComboBoxKinddtkDate
ParseInputTabOrder OnClickdtpkInicioPChangeOnChangedtpkInicioPChange  TDateTimePickerdtpkFimPLeft9Top)WidthQHeightCalAlignmentdtaLeftDate      Ѓ@Time      Ѓ@
DateFormatdfShortDateMode
dmComboBoxKinddtkDate
ParseInputTabOrderOnClickdtpkInicioPChangeOnChangedtpkInicioPChange   TDBLookupComboBox
dblkForms2LeftTopWidthг HeightKeyFieldFORM_ID	ListFieldFORM_CAPTION
ListSourceUsers.dtsFormsTabOrderOnClickdblkForms2Click	OnCloseUpdblkForms2Click   TDBGridDBGridAudCompsLeft TopFWidthЋHeight?AlignalClient
DataSourceUsers.dsAuditOptionsdgTitlesdgIndicator
dgColLines
dgRowLinesdgTabsdgRowSelectdgAlwaysShowSelectiondgConfirmDeletedgCancelOnExit ReadOnly	TabOrderTitleFont.CharsetDEFAULT_CHARSETTitleFont.ColorclWindowTextTitleFont.HeightѕTitleFont.NameArialTitleFont.Style ColumnsExpanded	FieldNameUSERCS_NAMEWidth Visible	 Expanded	FieldName	DATE_UTILWidth Visible	 Expanded	FieldNameCOMPUTER_NAMEWidthnVisible	    TPanelPanel9LeftЋTopFWidthЙ Height?AlignalRight
BevelOuterbvNoneTabOrder 	TGroupBoxgbxAddInfo2Left Top WidthЙ Height?AlignalClientCaptionInformaчѕes AdicionaisTabOrder  
TScrollBox
ScrollBox2LeftTopWidthЕ Height-HorzScrollBar.Tracking	AlignalClient
AutoScrollBorderStylebsNoneTabOrder  TDBTextDBText3Left Top Width)HeightAlignalTopAutoSize		DataFieldADDITIONAL_INFO1
DataSourceUsers.dsAuditWordWrap	  TDBTextDBText4Left TopWidth)HeightAlignalTopAutoSize		DataFieldADDITIONAL_INFO2
DataSourceUsers.dsAuditWordWrap	  TBevelBevel2Left TopWidthЕ HeightAlignalTopShape	bsTopLine      	TTabSheettsCurrentUsersCaptionUsuсrios utilizando a aplicaчуo TDBGriddbgCurrentUsersLeft Top)WidthdHeight\AlignalClient
DataSourceUsers.dtsCurrentUsersTabOrder TitleFont.CharsetDEFAULT_CHARSETTitleFont.ColorclWindowTextTitleFont.HeightѕTitleFont.NameArialTitleFont.Style ColumnsExpanded	FieldNameUSERCS_NAMEWidthtVisible	 Expanded	FieldNamePROF_DESCRIPTIONWidth Visible	 Expanded	FieldNameLOGIN_DATE_TIMEWidth Visible	 Expanded	FieldNameCOMPUTER_NAMEWidthЕ Visible	    TPanelPanel11Left Top WidthdHeight)AlignalTopTabOrder TButtonbtnRefreshCurrentUsersLeftTopWidth HeightCaptionAtualiza ListaTabOrder OnClickbtnRefreshCurrentUsersClick      	TTabSheettabAppConfigCaption#Configuraчѕes da Aplicaчуo em Geral TLabel
lblTimeOutLeftTopWidthkHeightCaptionTime Out (em minutos)FocusControlDBEdit7  TLabellblMaxBadLoginsLeftTop8Width HeightCaption NК mсximo de tentativas de loginFocusControlDBEdit8  TLabellblInativeDaysLeftTop WidthЬ HeightCaption*NК mсximo de dias sem utilizar a aplicaчуoFocusControlDBEdit9  TLabellblMaxPwdHistoryLeftTopА WidthЄ HeightCaption NК mсximo de senhas no histѓricoFocusControlDBEdit11  TDBEditDBEdit7LeftTop WidthAHeight	DataFieldTIME_OUT
DataSourceUsers.dtsApplicationTabOrder   TDBEditDBEdit8LeftTopHWidth@Height	DataFieldMAX_BAD_LOGINS
DataSourceUsers.dtsApplicationTabOrder  TDBEditDBEdit9LeftTop Width@Height	DataFieldMAX_DAYS_INNATIVE
DataSourceUsers.dtsApplicationTabOrder  TDBEditDBEdit11LeftTopР Width@Height	DataFieldMAX_PWD_HISTORY
DataSourceUsers.dtsApplicationTabOrder  TDBCheckBoxdbcbxDisableUserLeftTophWidthйHeightCaption4Desabilita usuсrio apѓs exceder tentantivas de login	DataFieldDISABLE_USER
DataSourceUsers.dtsApplicationTabOrderValueChecked1ValueUnchecked0  TDBCheckBoxdbcbxMultipleLoginsLeftTopр WidthHeightCaption)Permite logins mњltiplos do mesmo usuсrio	DataFieldALLOW_MULTIPLE_LOGINS
DataSourceUsers.dtsApplicationTabOrderValueChecked1ValueUnchecked0     
TPopupMenuPopupTreeViewLeft TopS 	TMenuItem
AllEnabledCaptionTodos HabilitadosOnClickAllEnabledClick  	TMenuItemAllInvisibleCaptionTodos InvisэveisOnClickAllEnabledClick  	TMenuItemAllDisabledCaption$Todos Desabilitados (Porem visэveis)OnClickAllEnabledClick  	TMenuItemN1Caption-  	TMenuItemShowLegendaCaptionVer LegendaOnClickShowLegendaClick  	TMenuItemN2Caption-  	TMenuItemPrintPermissionsCaptionImprimirOnClickPrintPermissionsClick   
TImageList	ImageListLeftTopYBitmap
&  IL     џџџџџџџџџџџџџBM6       6   (   @                                                                                                                                                                                                                                џџџ џџџ џџџ џџџ џџџ џџџ џџџ     џџџ џџџ џџџ џџ      џџџ     џџџ                                                                                                                                                                                                         {Н9 {Н9 {Н9 {Н9 {Н9 {Н9 {Н9 {Н9 {Н9 {Н9 {Н9 {Н9 {Н9 {Н9                                                                                       {   {   {   {   {   {                                                                                         џџџ џџџ џџџ џџџ џџџ џџџ џџџ џџџ џџџ џџџ џџџ џџџ   џ   џ џџџ                                                                                   џ   џ   џ   џ   џ   џ   џ   {   {                                                                             џџџ џџџ џџџ џџџ џџџ џџџ џџџ џџџ                 џџџ     џџџ                                                                                   џ   џ   { џџ                    џ   џ   {                                                                                                                                                                                                                   џ   џ   {     џџ  џџ            џ   џ   џ   { џџ                                                                          џџџ џџџ џџџ џџџ џџџ џџџ џџџ џџџ џџџ џџџ џџџ     џџџ   џ   џ                                                                       џ   џ   {         џџ  џџ        џ   џ   џ       џ   {                         џџ  џџ                      џџ  џџ          џџџ џџџ џџџ џџџ     џџџ џџџ џџџ     џџџ џџџ џџџ     џџџ џџџ џџџ                                                                       џ   џ                 џџ    џ   џ   {           џ   {                             џџ  џџ                      џџ  џџ                                                                                                                                            џ   џ                   џ   џ   {               џ   {                             џџ  џџ                      џџ  џџ          џџџ џџџ џџџ џџџ џџџ џџџ џџџ џџџ џџџ џџџ џџџ џџџ џџџ џџџ џџџ                                                                       џ   џ               џ   џ   {                   џ   {                                 џџ                          џџ      џџџ џџџ џџџ џџџ џџџ         џџџ џџџ џџџ џџџ џџџ џџџ         џџџ                                                                       џ   џ           џ   џ   {                       џ   {                                                                                                                                                                                                           џ   џ       џ   џ   {                       џ   џ   {                                                                     џџџ џџџ џџџ џџџ џџџ џџџ џџџ џџџ џџџ џџџ џџџ џџџ џџџ џџџ џџџ џџџ                                                                           џ   џ   џ   џ                       џ   џ   џ                                                                         џџџ џџџ џџџ џџџ џџџ џџџ џџџ џџџ џџџ џџџ џџџ џџџ џџџ џџџ џџџ џџџ                                                                               џ   џ                       џ   џ   џ                                                                             {Н9 {Н9 {Н9 {Н9 {Н9 {Н9 {Н9 {Н9 {Н9 {Н9                                                                                                           џ   џ   џ   џ   џ   џ   џ   џ                                                                                 џџџ џџџ џџџ џџџ џџџ џџџ џџџ џџџ џџџ џџџ џџџ џџџ џџџ џџџ џџџ џџџ                                                                                       џ   џ   џ   џ   џ   џ                                                                                     џџџ џџџ џџџ џџџ џџџ џџџ џџџ џџџ џџџ џџџ џџџ џџџ џџџ џџџ џџџ џџџ BM>       >   (   @                                   џџџ џџџџџџџџџџџџџџџџџџјџџџџџџ№џџџџџр@џџџџџїФ@№сџџ7р@џџuзьXџџuзьX  з!ю\џџѕзљц  7q№Ё  §їCcћћ  §їww  §ї№oo  џј                           