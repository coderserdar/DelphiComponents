?
 TUSERADMINISTRATION 0(v  TPF0TUserAdministrationUserAdministrationLeft? TopRBorderIconsbiSystemMenu BorderStylebsDialogCaption Administra??o de Usu?riosClientHeight?ClientWidthxColor	clBtnFaceFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height?	Font.NameArial
Font.Style 
KeyPreview	OldCreateOrder	PositionpoScreenCenterShowHint	
OnActivateFormActivateOnClose	FormCloseOnCloseQueryFormCloseQuery	OnDestroyFormDestroy	OnKeyDownFormKeyDownPixelsPerInch`
TextHeight TPanelPanel1Left Top WidthxHeight?AlignalClient
BevelOuterbvNoneBorderWidthTabOrder  TPageControlPageControlLeftTopWidthtHeight?
ActivePage
tsProgramsAlignalClientTabOrder OnChangePageControlChange 	TTabSheettsUsersCaptionUsu?rios TPageControlpcUsersLeft TopWidthlHeight?
ActivePagetsUsersListAlignalClientTabOrder OnChangepcUsersChangeOnEnterpcUsersChangeOnExittsUsersDataExit 	TTabSheettsUsersListCaptionLista de Usu?riosOnEntertsUsersListEnter TDBGridDBGridUsersLeft Top WidthdHeightmHintPress F2 to SearchAlignalClient
DataSourceUsers.dsUsersOptionsdgTitlesdgIndicatordgColumnResize
dgColLines
dgRowLinesdgTabsdgRowSelectdgConfirmDeletedgCancelOnExit TabOrder TitleFont.CharsetDEFAULT_CHARSETTitleFont.ColorclWindowTextTitleFont.Height?TitleFont.NameArialTitleFont.Style ColumnsExpanded	FieldNameUSERCS_NAMEVisible	 Expanded	FieldName	REAL_NAMEWidth? Visible	 Expanded	FieldNamePROFILEWidth? Visible	 Expanded	FieldNameCalcUserActiveVisible	     	TTabSheettsUsersDataHintPress F2 to SearchCaptionDados do Usu?rioOnEntertsUsersDataEnterOnExittsUsersDataExit TLabellblLAST_PWD_CHANGELeft? Top? WidthiHeightCaptionLAST_PWD_CHANGEFocusControlDBEdit4  TLabellblUSER_NAMELeftTopWidthLHeightCaptionUSERCS_NAMEFocusControlDBEdit1  TLabellblREAL_NAMELeftfTopWidth>HeightCaption	REAL_NAMEFocusControlDBEdit2  TLabellblPASSWORDLeftTop0Width<HeightCaptionPASSWORDFocusControlDBEdit3  TLabellblEXPIRATION_DATELeft? Top`Width\HeightCaptionEXPIRATION_DATE  TLabel
lblPROFILELeft? Top0Width)HeightCaptionPROFILEFocusControldblkProfileName  TDBEditDBEdit4Left? Top? Width@Height	DataFieldLAST_PWD_CHANGE
DataSourceUsers.dsUsersReadOnly	TabOrder   TDBEditDBEdit1LeftTopWidth]Height	DataFieldUSERCS_NAME
DataSourceUsers.dsUsersTabOrder  TDBEditDBEdit2LeftfTopWidth? Height	DataField	REAL_NAME
DataSourceUsers.dsUsersTabOrder  TDBEditDBEdit3LeftTop@Width|Height	DataFieldUSER_PWD
DataSourceUsers.dsUsersEnabledPasswordChar*TabOrder  TDBCheckBoxcbxUSER_EXPIRELeftTopqWidthaHeightCaptionUSER_EXPIRE	DataFieldUSER_EXPIRE
DataSourceUsers.dsUserInfoTabOrderValueChecked1ValueUnchecked0OnClickcbxUSER_EXPIREClick  TDBCheckBoxcbxUSER_ACTIVELeftTop? WidthaHeightCaptionUSER_ACTIVE	DataFieldUSER_ACTIVE
DataSourceUsers.dsUserInfoTabOrderValueChecked1ValueUnchecked0  TDBLookupComboBoxdblkProfileNameLeft? Top@Width? Height	DataFieldPROFILE
DataSourceUsers.dsUsersTabOrder  TButtonbtnChangePasswordLeftTop? WidthdHeightCaption&Mudar SenhaTabOrderOnClickbtnChangePasswordClick  TDateTimePickerDBDateTimePicker1Left? ToprWidthiHeightCalAlignmentdtaLeftDate      ??@Time      ??@
DateFormatdfShortDateMode
dmComboBoxFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height?	Font.NameArial
Font.Style KinddtkDate
ParseInput
ParentFontTabOrderOnChangeDBDateTimePicker1Change  TDBCheckBoxcbxAUDIT_MODELeftTop? WidthrHeightCaptionModo de Auditoria	DataField
AUDIT_MODE
DataSourceUsers.dsUsersTabOrder	ValueChecked1ValueUnchecked0OnClickcbxUSER_EXPIREClick  TDBNavigatorDBNavigator2LeftTop? Width? Height
DataSourceUsers.dsUsersVisibleButtonsnbFirstnbPriornbNextnbLastnbPost TabOrder
BeforeActionDBNavigator2BeforeActionOnClickDBNavigator2Click  TDBCheckBoxcbxUSER_IS_ADMINLeftTop? Width? HeightCaptionUSER_IS_ADMIN	DataFieldUSER_IS_ADMIN
DataSourceUsers.dsUserInfoTabOrderValueChecked1ValueUnchecked0   	TTabSheettabAdditionalInfoCaption!Informa??es Adicionais do Usu?rioOnEntertabAdditionalInfoEnterOnExittabAdditionalInfoExit   TPanelToolBarLeft Top WidthlHeightAlignalTop
BevelOuterbvNoneParentShowHintShowHint	TabOrder TSpeedButton	sbNewUserLeftTopWidthdHeightCaptionNovo Usu?rioOnClicksbNewUserClick  TSpeedButtonsbUserAccessLeft? TopWidthdHeightCaption     Acessos    OnClicksbUserAccessClick  TSpeedButtonsbChangeUserDataLeftfTopWidthdHeightCaptionMudar DadosOnClicksbChangeUserDataClick  TSpeedButtonsbDeleteUserLeft-TopWidthdHeightCaptionEliminar Usu?rioOnClicksbDeleteUserClick  TSpeedButtonsbPrintUserListLeft?TopWidthdHeightCaptionImprimir ListaOnClicksbPrintUserListClick  TSpeedButtonsbPrintUserAccessLeft?TopWidthdHeightCaptionImprimir Permiss?esOnClicksbPrintUserAccessClick    	TTabSheet
tsProfilesCaptionPerfis de Usu?rio TPageControl
pcProfilesLeft TopWidthlHeight?
ActivePagetsProfilesListAlignalClientTabOrder  	TTabSheettsProfilesListCaptionLista de Perfis TDBGridDBGridProfilesLeft Top WidthdHeightmHintPress F2 to SearchAlignalClient
DataSourceUsers.dsProfilesOptionsdgTitlesdgIndicatordgColumnResize
dgColLines
dgRowLinesdgTabsdgRowSelectdgConfirmDeletedgCancelOnExit TabOrder TitleFont.CharsetDEFAULT_CHARSETTitleFont.ColorclWindowTextTitleFont.Height?TitleFont.NameArialTitleFont.Style ColumnsExpanded	FieldNamePROF_DESCRIPTIONWidth? Visible	 Expanded	FieldNameCalcMustChangePwdVisible	 Expanded	FieldNameINTERVAL_CHANGE_PWDVisible	     	TTabSheettsProfilesDataHintPress F2 to SearchCaptionDados do PerfilOnEntertsProfilesDataEnterOnExittsProfilesDataExit TLabellblPROF_DESCRIPTIONLeftTopWidthbHeightCaptionPROF_DESCRIPTIONFocusControlDBEdit5  TLabellblINTERVAL_CHANGE_PWDLeftTopXWidth? HeightCaptionINTERVAL_CHANGE_PWDFocusControlDBEdit6  TDBEditDBEdit5LeftTopWidth? Height	DataFieldPROF_DESCRIPTION
DataSourceUsers.dsProfilesTabOrder   TDBEditDBEdit6LeftTophWidth? Height	DataFieldINTERVAL_CHANGE_PWD
DataSourceUsers.dsProfilesTabOrder  TDBCheckBoxcbxMUST_CHANGE_PWDLeftTop>Width? HeightCaptionMUST_CHANGE_PWD	DataFieldMUST_CHANGE_PWD
DataSourceUsers.dsProfilesTabOrderValueChecked1ValueUnchecked0OnClickcbxMUST_CHANGE_PWDClick  TDBCheckBoxcbxAUDIT_MODEProfileLeft? TopiWidthrHeightCaption
AUDIT_MODE	DataField
AUDIT_MODE
DataSourceUsers.dsProfilesTabOrderValueChecked1ValueUnchecked0OnClickcbxUSER_EXPIREClick  TDBNavigatorDBNavigator3LeftTop? Width? Height
DataSourceUsers.dsProfilesVisibleButtonsnbFirstnbPriornbNextnbLastnbPost TabOrder    TPanelPanel3Left Top WidthlHeightAlignalTop
BevelOuterbvNoneParentShowHintShowHint	TabOrder TSpeedButtonsbNewProfileLeftTopWidthdHeightCaptionNovo PerfilOnClicksbNewProfileClick  TSpeedButtonsbProfileAccessLeft? TopWidthdHeightCaption     Acessos    OnClicksbProfileAccessClick  TSpeedButtonsbChangeProfileDataLeftfTopWidthdHeightCaptionMudar DadosOnClicksbChangeProfileDataClick  TSpeedButtonsbDeleteProfileLeft.TopWidthdHeightCaptionEliminar PerfilOnClicksbDeleteProfileClick  TSpeedButtonsbPrintProfileListLeft?TopWidthdHeightCaptionImprimir ListaOnClicksbPrintProfileListClick  TSpeedButtonsbPrintProfileAccessLeft?TopWidthdHeightCaptionImprimir AcessosOnClicksbPrintProfileAccessClick    	TTabSheet
tsProgramsCaption Controle de Acesso aos ProgramasOnEntertsProgramsEnterOnExittsProgramsExit TPageControl
pcProgramsLeftNTop WidthHeight?
ActivePagetsUsersProgsAlignalClientTabOrder OnChangepcProgramsChange 	TTabSheettsUsersProgsCaptionUsu?rios TDBGridDBGridUsersProgsLeft Top WidthHeight?HintPress F2 to SearchAlignalClientCtl3D
DataSourceUsers.dsUsers
FixedColorclActiveCaptionFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height?	Font.NameArial
Font.Style OptionsdgTitles
dgColLines
dgRowLinesdgTabsdgRowSelectdgAlwaysShowSelectiondgConfirmDeletedgCancelOnExit ParentCtl3D
ParentFontTabOrder TitleFont.CharsetDEFAULT_CHARSETTitleFont.ColorclCaptionTextTitleFont.Height?TitleFont.NameArialTitleFont.StylefsBold ColumnsExpanded	FieldNameUSERCS_NAMEWidthdVisible	 Expanded	FieldNamePROFILEWidth? Visible	     	TTabSheettsProfilesProgsCaptionPerfis de Usu?rio TDBGridDBGridProfilesProgsLeft Top WidthHeight?HintPress F2 to SearchAlignalClientCtl3D
DataSourceUsers.dsProfiles
FixedColorclActiveCaptionFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height?	Font.NameArial
Font.Style OptionsdgTitles
dgColLines
dgRowLinesdgTabsdgRowSelectdgAlwaysShowSelectiondgConfirmDeletedgCancelOnExit ParentCtl3D
ParentFontTabOrder TitleFont.CharsetDEFAULT_CHARSETTitleFont.ColorclCaptionTextTitleFont.Height?TitleFont.NameArialTitleFont.StylefsBold ColumnsExpanded	FieldNamePROF_DESCRIPTIONWidthVisible	      TPanelPanel2Left Top WidthNHeight?AlignalLeft
BevelOuterbvNoneTabOrder 	TTreeViewCompListLeft TopUWidthNHeightMAlignalClientFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height?	Font.NameArial
Font.Style Images	ImageListIndent
ParentFontParentShowHint	PopupMenuPopupTreeViewReadOnly	ShowHintTabOrder 
OnDeletionCompListDeletionOnMouseDownCompListMouseDown  TPanelPanel6Left Top WidthNHeightAlignalTop	AlignmenttaLeftJustify
BevelOuterbvNoneTabOrder TSpeedButtonsbApplyLeftTopWidthnHeightCaptionAplicarOnClicksbApplyClick  TSpeedButton	sbRestoreLeftpTopWidthnHeightCaption	RestaurarOnClicksbRestoreClick  TSpeedButtonsbRestoreProfileLeft? TopWidthnHeightCaptionRestaurar do PerfilOnClicksbRestoreProfileClick   TPanelPanelAboveProgsLeft TopAWidthNHeightAlignalTop	AlignmenttaLeftJustify
BevelInnerbvRaised
BevelOuterbvNoneCaption' Acessos para o usu?rio XXXXXXXXXXXXXXXColorclActiveCaptionFont.CharsetDEFAULT_CHARSET
Font.ColorclCaptionTextFont.Height?	Font.NameArial
Font.StylefsBold 
ParentFontTabOrder  	TGroupBoxgbxFormListLeft TopWidthNHeight)AlignalTopCaption Lista de Formul?rios TabOrder TDBLookupComboBox	dblkFormsLeftTopWidthBHeightKeyFieldFORM_ID	ListFieldFORM_CAPTION
ListSourceUsers.dtsFormsTabOrder OnClickdblkFormsCloseUp   TPanelPanelLegendaLeft0Top? Width? Height? TabOrderVisible TSpeedButtonSpeedButton1Left
TopWidthHeight
Glyph.Data
z  v  BMv      6   (               @                  ???????????????????????????         ???         ????????????????????????????????????   ????????????            ???            ??????   ?????????   ?????????   ???   ?????????   ???   ?????????   ?????????   ???   ?????????   ???   ????????????            ???   ?????????   ???   ?????????????????????   ???   ?????????   ???   ????????????         ??????            ??????   ???????????????????????????   ???????????????   ???????????????????????????   ???????????????   ???????????????????????????   ???????????????   ????????????????????????????????????         ???           TSpeedButtonSpeedButton2Left
Top:WidthHeight
Glyph.Data
J  F  BMF      6   (                                 ????????????            ?????????            ????????????      ?? ??       ???      ?? ??       ?????????   ???????? ??    ???   ???????? ??    ?????????   ???????? ??    ???   ???????? ??    ?????????   ???????????    ???   ???????????    ?????????      ??????               ??????      ????????????            ???   ???            ??????????????????   ?????????????????????   ??????   ?????????   ?????????   ?????????   ?????????   ??????   ????????????   ??????   ???????????????      ??????????????????      ???????????????  TSpeedButtonSpeedButton3Left
TopXWidthHeight
Glyph.Data
?  ?  BM?      6   (               ?                  ???????????????            ???????????????????????????  ?  ?  ?  ?  ?  ?  ?          ????????????  ?  ?  ??       ???     ?  ?        ??????  ?  ?  ????? ??    ???  ?  ?  ?  ??    ???  ?  ?  ???????? ??      ?  ?  ????  ?     ???  ?  ?   ???????????   ?  ?  ??????  ?     ???  ?  ?      ??????  ?  ?        ???  ?     ???  ?  ????        ?  ?     ???        ?  ??????  ?  ???????  ?  ?  ???????????????  ?  ???     ?  ????  ?  ?  ???   ?????????  ?  ?  ???   ???  ?  ?  ?  ???????   ??????  ?  ?  ??????????        ?  ?????????????     ?  ?  ??????????????????????  ?  ?  ?  ?  ?  ?  ?  ????????????????????????????  ?  ?  ?  ?  ?  ????????????????  TPanelpnlLegendaEnabledLeft'TopWidth? Height	AlignmenttaLeftJustify
BevelInnerbvRaised
BevelOuterbvNoneCaption HabilitadoColorclActiveCaptionFont.CharsetANSI_CHARSET
Font.ColorclCaptionTextFont.Height?	Font.NameArial
Font.StylefsBold 
ParentFontTabOrder   TPanelpnlTopLegendaLeftTopWidth? HeightAlignalTop	AlignmenttaLeftJustify
BevelInnerbvRaised
BevelOuterbvNoneCaption LegendaColorclActiveCaptionFont.CharsetANSI_CHARSET
Font.ColorclCaptionTextFont.Height?	Font.NameArial
Font.StylefsBold 
ParentFontTabOrder  TPanelpnlLegendaOnlyVisibleLeft'Top;Width? Height	AlignmenttaLeftJustify
BevelInnerbvRaised
BevelOuterbvNoneCaptionVis?vel, mas DesabilitadoColorclActiveCaptionFont.CharsetANSI_CHARSET
Font.ColorclCaptionTextFont.Height?	Font.NameArial
Font.StylefsBold 
ParentFontTabOrder  TPanelpnlLegendaInvisibleLeft'TopYWidth? Height	AlignmenttaLeftJustify
BevelInnerbvRaised
BevelOuterbvNoneCaption
 Invis?velColorclActiveCaptionFont.CharsetANSI_CHARSET
Font.ColorclCaptionTextFont.Height?	Font.NameArial
Font.StylefsBold 
ParentFontTabOrder  TBitBtn
btnLegendaLeftKTop? WidthYHeightCaptionFechar LegendaTabOrderOnClickbtnLegendaClick     	TTabSheettsAuditCaption	Auditoria TPageControlpcAuditLeft Top WidthlHeight?
ActivePagetsCurrentUsersAlignalClientTabOrder OnChangepcAuditChangeOnEnterpcAuditEnterOnExitpcAuditExit 	TTabSheettsLoginActivityCaptionAtividades de Login do Usu?rio TDBGriddbgLoginActivityLeft TopFWidthdHeight?AlignalClient
DataSourceUsers.dtsLoginTraceOptionsdgTitlesdgIndicatordgColumnResize
dgColLines
dgRowLinesdgTabsdgRowSelectdgAlwaysShowSelectiondgConfirmDeletedgCancelOnExit TabOrder TitleFont.CharsetDEFAULT_CHARSETTitleFont.ColorclWindowTextTitleFont.Height?TitleFont.NameArialTitleFont.Style ColumnsExpanded	FieldNameLOGIN_DATE_TIMEWidth? Visible	 Expanded	FieldNameLOGOUT_DATE_TIMEWidth? Visible	 Expanded	FieldNameCOMPUTER_NAMEWidth3Visible	    TPanelPanel8Left Top WidthdHeightFAlignalTopTabOrder TLabellblUser1LeftTopWidth%HeightCaptionUsu?rio  TSpeedButtonsbDeleteUserLoginActivityLeft|Top,Width<HeightCaptionEliminarOnClicksbDeleteUserLoginActivityClick  TSpeedButtonsbPrintUserLoginActivityLeft? Top,Width<HeightCaptionImprimirOnClicksbPrintUserLoginActivityClick  TDBNavigatorDBNavigator6LeftTop,WidthtHeight
DataSourceUsers.dtsLoginTraceVisibleButtonsnbFirstnbPriornbNextnbLast TabOrder   TDBLookupComboBoxdblkUserName2LeftTopWidth? HeightKeyFieldUSER_ID	ListFieldUSERCS_NAME
ListSourceUsers.dsUserNameTabOrderOnClickdtpkInicioLTChange	OnCloseUpdtpkInicioLTChange  	TGroupBoxgbxPeriodActivities1Left?TopWidth? HeightDAlignalRightCaption Per?odo de Atividades TabOrder TLabellblFrom1LeftTopWidthHeightCaptionDesde  TLabellblTo1LeftTop,Width HeightCaption     At?  TDateTimePickerdtpkInicioLTLeft9TopWidthQHeightCalAlignmentdtaLeftDate      ??@Time      ??@
DateFormatdfShortDateMode
dmComboBoxKinddtkDate
ParseInputTabOrder OnClickdtpkInicioLTChangeOnChangedtpkInicioLTChange  TDateTimePicker	dtpkFimLTLeft9Top)WidthQHeightCalAlignmentdtaLeftDate      ??@Time      ??@
DateFormatdfShortDateMode
dmComboBoxKinddtkDate
ParseInputTabOrderOnClickdtpkInicioLTChangeOnChangedtpkInicioLTChange     	TTabSheettsUserActivityCaptionAtividades dos Usu?rios TPanelPanel5Left Top WidthdHeightFAlignalTopTabOrder  TLabellblUserLeft? TopWidth%HeightCaptionUsu?rio  TSpeedButtonsbDeleteAuditUserLeft|Top,Width<HeightCaptionEliminarOnClicksbDeleteAuditUserClick  TLabellblForm1LeftTopWidth2HeightCaption
Formul?rio  TSpeedButtonsbPrintUserActivityLeft? Top,Width<HeightCaptionImprimirOnClicksbPrintUserActivityClick  TDBNavigatorDBNavigator4LeftTop,WidthtHeight
DataSourceUsers.dsAuditVisibleButtonsnbFirstnbPriornbNextnbLast TabOrder   TDBLookupComboBoxdblkUserNameLeft? TopWidth? HeightKeyFieldUSER_ID	ListFieldUSERCS_NAME
ListSourceUsers.dsUserNameTabOrderOnClickdtpkInicioPChange	OnCloseUpdtpkInicioPChange  	TGroupBoxgbxPeriodActivitiesLeft?TopWidth? HeightDAlignalRightCaption Per?odo de Atividades TabOrder TLabellblFromLeftTopWidthHeightCaptionDesde  TLabellblToLeftTop,Width HeightCaption     At?  TDateTimePickerdtpkInicioULeft9TopWidthQHeightCalAlignmentdtaLeftDate      ??@Time      ??@
DateFormatdfShortDateMode
dmComboBoxKinddtkDate
ParseInputTabOrder OnClickdtpkInicioPChangeOnChangedtpkInicioPChange  TDateTimePickerdtpkFimULeft9Top)WidthQHeightCalAlignmentdtaLeftDate      ??@Time      ??@
DateFormatdfShortDateMode
dmComboBoxKinddtkDate
ParseInputTabOrderOnClickdtpkInicioPChangeOnChangedtpkInicioPChange   TDBLookupComboBox
dblkForms1LeftTopWidth? HeightKeyFieldFORM_ID	ListFieldFORM_CAPTION
ListSourceUsers.dtsFormsTabOrderOnClickdblkForms1Click	OnCloseUpdblkForms1Click   TDBGridDBGridAudUsersLeft TopFWidth?Height?AlignalClient
DataSourceUsers.dsAuditOptionsdgTitlesdgIndicatordgColumnResize
dgColLines
dgRowLinesdgTabsdgRowSelectdgAlwaysShowSelectiondgConfirmDeletedgCancelOnExit ReadOnly	TabOrderTitleFont.CharsetDEFAULT_CHARSETTitleFont.ColorclWindowTextTitleFont.Height?TitleFont.NameArialTitleFont.Style ColumnsExpanded	FieldNameCOMP_CAPTIONWidth? Visible	 Expanded	FieldName	DATE_UTILWidth? Visible	 Expanded	FieldNameCOMPUTER_NAMEWidthnVisible	    TPanelPanel10Left?TopFWidth? Height?AlignalRight
BevelOuterbvNoneTabOrder 	TGroupBoxgbxAddInfo1Left Top Width? Height?AlignalClientCaptionInforma??es AdicionaisTabOrder  
TScrollBox
ScrollBox1LeftTopWidth? Height-HorzScrollBar.Tracking	AlignalClient
AutoScrollBorderStylebsNoneTabOrder  TDBTextDBText1Left Top Width)HeightAlignalTopAutoSize		DataFieldADDITIONAL_INFO1
DataSourceUsers.dsAuditWordWrap	  TDBTextDBText2Left TopWidth)HeightAlignalTopAutoSize		DataFieldADDITIONAL_INFO2
DataSourceUsers.dsAuditWordWrap	  TBevelBevel1Left TopWidth? HeightAlignalTopShape	bsTopLine      	TTabSheettsCompUtilizationCaptionUtiliza??o de Componentes TPanelPanel7Left Top WidthdHeightFAlignalTopTabOrder  TLabellblComponentLeft? TopWidth<HeightCaption
Componente  TSpeedButtonsbDeleteAuditProgLeft|Top,Width<HeightCaptionEliminarOnClicksbDeleteAuditProgClick  TLabellblForm2LeftTopWidth2HeightCaption
Formul?rio  TSpeedButtonsbPrintCompUtilizationLeft? Top,Width<HeightCaptionImprimirOnClicksbPrintCompUtilizationClick  TDBLookupComboBoxdblkCompNameLeft? TopWidth? HeightKeyFieldCOMP_ID	ListFieldCOMP_CAPTION
ListSourceUsers.dsCompsTabOrder OnClickdtpkInicioPChange	OnCloseUpdtpkInicioPChange  TDBNavigatorDBNavigator5LeftTop,WidthtHeight
DataSourceUsers.dsAuditVisibleButtonsnbFirstnbPriornbNextnbLast TabOrder  	TGroupBoxgbxPeriodActivities2Left?TopWidth? HeightDAlignalRightCaption Per?odo de Atividades TabOrder TLabellblFrom2LeftTopWidthHeightCaptionDesde  TLabellblTo2LeftTop,Width HeightCaption     At?  TDateTimePickerdtpkInicioPLeft9TopWidthQHeightCalAlignmentdtaLeftDate      ??@Time      ??@
DateFormatdfShortDateMode
dmComboBoxKinddtkDate
ParseInputTabOrder OnClickdtpkInicioPChangeOnChangedtpkInicioPChange  TDateTimePickerdtpkFimPLeft9Top)WidthQHeightCalAlignmentdtaLeftDate      ??@Time      ??@
DateFormatdfShortDateMode
dmComboBoxKinddtkDate
ParseInputTabOrderOnClickdtpkInicioPChangeOnChangedtpkInicioPChange   TDBLookupComboBox
dblkForms2LeftTopWidth? HeightKeyFieldFORM_ID	ListFieldFORM_CAPTION
ListSourceUsers.dtsFormsTabOrderOnClickdblkForms2Click	OnCloseUpdblkForms2Click   TDBGridDBGridAudCompsLeft TopFWidth?Height?AlignalClient
DataSourceUsers.dsAuditOptionsdgTitlesdgIndicator
dgColLines
dgRowLinesdgTabsdgRowSelectdgAlwaysShowSelectiondgConfirmDeletedgCancelOnExit ReadOnly	TabOrderTitleFont.CharsetDEFAULT_CHARSETTitleFont.ColorclWindowTextTitleFont.Height?TitleFont.NameArialTitleFont.Style ColumnsExpanded	FieldNameUSERCS_NAMEWidth? Visible	 Expanded	FieldName	DATE_UTILWidth? Visible	 Expanded	FieldNameCOMPUTER_NAMEWidthnVisible	    TPanelPanel9Left?TopFWidth? Height?AlignalRight
BevelOuterbvNoneTabOrder 	TGroupBoxgbxAddInfo2Left Top Width? Height?AlignalClientCaptionInforma??es AdicionaisTabOrder  
TScrollBox
ScrollBox2LeftTopWidth? Height-HorzScrollBar.Tracking	AlignalClient
AutoScrollBorderStylebsNoneTabOrder  TDBTextDBText3Left Top Width)HeightAlignalTopAutoSize		DataFieldADDITIONAL_INFO1
DataSourceUsers.dsAuditWordWrap	  TDBTextDBText4Left TopWidth)HeightAlignalTopAutoSize		DataFieldADDITIONAL_INFO2
DataSourceUsers.dsAuditWordWrap	  TBevelBevel2Left TopWidth? HeightAlignalTopShape	bsTopLine      	TTabSheettsCurrentUsersCaptionUsu?rios utilizando a aplica??o TDBGriddbgCurrentUsersLeft Top)WidthdHeight\AlignalClient
DataSourceUsers.dtsCurrentUsersTabOrder TitleFont.CharsetDEFAULT_CHARSETTitleFont.ColorclWindowTextTitleFont.Height?TitleFont.NameArialTitleFont.Style ColumnsExpanded	FieldNameUSERCS_NAMEWidthtVisible	 Expanded	FieldNamePROF_DESCRIPTIONWidth? Visible	 Expanded	FieldNameLOGIN_DATE_TIMEWidth? Visible	 Expanded	FieldNameCOMPUTER_NAMEWidth? Visible	    TPanelPanel11Left Top WidthdHeight)AlignalTopTabOrder TButtonbtnRefreshCurrentUsersLeftTopWidth? HeightCaptionAtualiza ListaTabOrder OnClickbtnRefreshCurrentUsersClick      	TTabSheettabAppConfigCaption#Configura??es da Aplica??o em Geral TLabel
lblTimeOutLeftTopWidthkHeightCaptionTime Out (em minutos)FocusControlDBEdit7  TLabellblMaxBadLoginsLeftTop8Width? HeightCaption N? m?ximo de tentativas de loginFocusControlDBEdit8  TLabellblInativeDaysLeftTop? Width? HeightCaption*N? m?ximo de dias sem utilizar a aplica??oFocusControlDBEdit9  TLabellblMaxPwdHistoryLeftTop? Width? HeightCaption N? m?ximo de senhas no hist?ricoFocusControlDBEdit11  TDBEditDBEdit7LeftTop WidthAHeight	DataFieldTIME_OUT
DataSourceUsers.dtsApplicationTabOrder   TDBEditDBEdit8LeftTopHWidth@Height	DataFieldMAX_BAD_LOGINS
DataSourceUsers.dtsApplicationTabOrder  TDBEditDBEdit9LeftTop? Width@Height	DataFieldMAX_DAYS_INNATIVE
DataSourceUsers.dtsApplicationTabOrder  TDBEditDBEdit11LeftTop? Width@Height	DataFieldMAX_PWD_HISTORY
DataSourceUsers.dtsApplicationTabOrder  TDBCheckBoxdbcbxDisableUserLeftTophWidth?HeightCaption4Desabilita usu?rio ap?s exceder tentantivas de login	DataFieldDISABLE_USER
DataSourceUsers.dtsApplicationTabOrderValueChecked1ValueUnchecked0  TDBCheckBoxdbcbxMultipleLoginsLeftTop? WidthHeightCaption)Permite logins m?ltiplos do mesmo usu?rio	DataFieldALLOW_MULTIPLE_LOGINS
DataSourceUsers.dtsApplicationTabOrderValueChecked1ValueUnchecked0     
TPopupMenuPopupTreeViewLeft? TopS 	TMenuItem
AllEnabledCaptionTodos HabilitadosOnClickAllEnabledClick  	TMenuItemAllInvisibleCaptionTodos Invis?veisOnClickAllEnabledClick  	TMenuItemAllDisabledCaption$Todos Desabilitados (Porem vis?veis)OnClickAllEnabledClick  	TMenuItemN1Caption-  	TMenuItemShowLegendaCaptionVer LegendaOnClickShowLegendaClick  	TMenuItemN2Caption-  	TMenuItemPrintPermissionsCaptionImprimirOnClickPrintPermissionsClick   
TImageList	ImageListLeftTopYBitmap
&  IL     ?????????????BM6       6   (   @                                                                                                                                                                                                                                ??? ??? ??? ??? ??? ??? ???     ??? ??? ??? ??      ???     ???                                                                                                                                                                                                         {?9 {?9 {?9 {?9 {?9 {?9 {?9 {?9 {?9 {?9 {?9 {?9 {?9 {?9                                                                                       {   {   {   {   {   {                                                                                         ??? ??? ??? ??? ??? ??? ??? ??? ??? ??? ??? ???   ?   ? ???                                                                                   ?   ?   ?   ?   ?   ?   ?   {   {                                                                             ??? ??? ??? ??? ??? ??? ??? ???                 ???     ???                                                                                   ?   ?   { ??                    ?   ?   {                                                                                                                                                                                                                   ?   ?   {     ??  ??            ?   ?   ?   { ??                                                                          ??? ??? ??? ??? ??? ??? ??? ??? ??? ??? ???     ???   ?   ?                                                                       ?   ?   {         ??  ??        ?   ?   ?       ?   {                         ??  ??                      ??  ??          ??? ??? ??? ???     ??? ??? ???     ??? ??? ???     ??? ??? ???                                                                       ?   ?                 ??    ?   ?   {           ?   {                             ??  ??                      ??  ??                                                                                                                                            ?   ?                   ?   ?   {               ?   {                             ??  ??                      ??  ??          ??? ??? ??? ??? ??? ??? ??? ??? ??? ??? ??? ??? ??? ??? ???                                                                       ?   ?               ?   ?   {                   ?   {                                 ??                          ??      ??? ??? ??? ??? ???         ??? ??? ??? ??? ??? ???         ???                                                                       ?   ?           ?   ?   {                       ?   {                                                                                                                                                                                                           ?   ?       ?   ?   {                       ?   ?   {                                                                     ??? ??? ??? ??? ??? ??? ??? ??? ??? ??? ??? ??? ??? ??? ??? ???                                                                           ?   ?   ?   ?                       ?   ?   ?                                                                         ??? ??? ??? ??? ??? ??? ??? ??? ??? ??? ??? ??? ??? ??? ??? ???                                                                               ?   ?                       ?   ?   ?                                                                             {?9 {?9 {?9 {?9 {?9 {?9 {?9 {?9 {?9 {?9                                                                                                           ?   ?   ?   ?   ?   ?   ?   ?                                                                                 ??? ??? ??? ??? ??? ??? ??? ??? ??? ??? ??? ??? ??? ??? ??? ???                                                                                       ?   ?   ?   ?   ?   ?                                                                                     ??? ??? ??? ??? ??? ??? ??? ??? ??? ??? ??? ??? ??? ??? ??? ??? BM>       >   (   @            ?                       ??? ?????????????????????????????????@???????@?????7??@??u׎?X??u׆?X  ?א!?\???ט??  ?7q??  ??Cc??  ????ww  ???oo  ?????                           