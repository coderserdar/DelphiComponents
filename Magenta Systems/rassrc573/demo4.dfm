�
 TMAINFORM 0r  TPF0	TMainFormMainFormLeft7TopBBorderStylebsSingleCaption?RAS Delphi Component Demo - Not a fully functioning applicationClientHeight�ClientWidth{Color	clBtnFaceFont.CharsetANSI_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.Style OldCreateOrder	PositionpoScreenCenterOnClose	FormCloseOnCreate
FormCreatePixelsPerInch`
TextHeight TLabelLabel4Left	TopNWidth� Height8CaptiongThis demo monitors and controls connections started by other applications as well as those started hereWordWrap	  TLabelLabel1Left
TopWidth� HeightCaption*Defined Connection Entries aka Phone BooksWordWrap	  TLabelLabel60LeftdTop� Width-HeightCaption	ISDN Link  
TStatusBarStatusLeft Top�Width{HeightPanelsWidth, Width�     TButtondoExitLeft
Top�WidthMHeightCaptionExitTabOrder OnClickdoExitClick  TListBoxConnListLeftTop#Width� Height� 
ItemHeightTabOrderOnClickConnListClick  TPageControl	MainPagesLeft� Top Width�Height�
ActivePage	TabSheet5TabOrder 	TTabSheet	TabSheet1CaptionOnline 	TListViewConInfoListLeft Top Width�Height`AllocByColumnsCaptionNameWidthd CaptionStatusWidth�  CaptionLinkWidth( CaptionTel Nr (not Win9x)Width�    	GridLines	HideSelectionReadOnly		RowSelect	TabOrder 	ViewStylevsReport  	TListView
ConDevListLeft Top_Width�Height`AllocByColumnsCaptionNameWidthd CaptionDeviceWidth�  CaptionPort CaptionType CaptionLinkWidth( CaptionHandleWidthP Caption
Par HandleWidthP Caption	PhonebookWidth�  CaptionGUIDWidthF CaptionFlags CaptionLUID  	GridLines	ReadOnly	TabOrder	ViewStylevsReport  	TListViewConSpeedListLeft Top� Width�Height`AllocByColumnsCaptionNameWidthd CaptionSpeedWidthF Caption	Data SendWidthF Caption	Data RcvdWidthF CaptionAuth Caption
IP AddressWidth�   	GridLines	ReadOnly	TabOrder	ViewStylevsReport  TMemoConnLogLeft TopWidth�Heighty
ScrollBars
ssVerticalTabOrder   	TTabSheet	TabSheet2Caption
PhonebooksExplicitLeft ExplicitTop ExplicitWidth ExplicitHeight  TLabelLabel8Left� TopiWidth� HeightCaption1Available Dial-Up Adaptors
for Performance StatsWordWrap	  TLabelLabelRasDevicesLeft
Top� Width� HeightCaption!RAS Capable Modems and ISDN Cards  TLabelLabelDefPhonebookLeft
Top� Width`HeightCaptionLabelDefPhonebookWordWrap	  TLabelLabelFileConnsLeft
TopzWidth]HeightCaptionDefault Phonebook:  TListBoxListDUALeftBTop^Width� Height(
ItemHeightTabOrder  TListBox
DeviceListLeft
Top� Width�HeightQ
ItemHeightTabOrder   	TGroupBox	GroupBox1Left
Top
Width� HeighthCaptionDialog BoxesTabOrder TButtondoCreateConnLeft
TopWidthVHeightCaptionCreate EntryTabOrder OnClickdoCreateConnClick  TButton
doEditConnLeft
Top<WidthVHeightCaption
Edit EntryTabOrderOnClickdoEditConnClick  TButtondoDUNDialogLeftnTopWidthVHeightCaption
DUN DialogTabOrderOnClickdoDUNDialogClick  TButtondoDialDialogLeftnTop<WidthVHeightCaptionDial DialogTabOrderOnClickdoDialDialogClick   	TGroupBox	GroupBox2Left� Top
Width� HeighthCaption	FunctionsTabOrder TButtondoRenameConnLeft
Top<WidthVHeightCaptionRename EntryTabOrder OnClickdoRenameConnClick  TButton
doCopyConnLeft
TopWidthVHeightCaption
Copy EntryTabOrderOnClickdoCopyConnClick  TButtondoDeleteConnLeftjTopWidthiHeightCaptionDelete EntryTabOrderOnClickdoDeleteConnClick  TButton
doShortcutLeftjTop<WidthiHeightCaptionDesktop ShortcutTabOrderOnClickdoShortcutClick    	TTabSheet	TabSheet3Caption
Some PropsExplicitLeft ExplicitTop ExplicitWidth ExplicitHeight  TLabelLabel41Left7TopTWidthHeightCaption]This pages shows selected properties for the connection
selected for dialling in the list.    TButtondoLogonUpdateLeftTopwWidthdHeightCaptionUpdate DetailsTabOrderOnClickdoLogonUpdateClick  TButtondoDumpEntSomeLeft� TopwWidth[HeightCaptionDump RasentryTabOrderOnClickdoDumpEntSomeClick  	TGroupBox	SBoxLogonLeftTopWidth�HeightVCaptionLogon DetailsTabOrder  TLabelLabel2Left
TopWidth<HeightCaption
Logon Name  TLabelLabel3Left
Top2Width2HeightCaptionPassword  TLabelLabel49Left,Top
Width� HeightGAutoSizeCaptionxWarning - on Windows 2000 the number of asterisks in the password field do not reflect the actual length of the passwordWordWrap	  TEditConnUserLeftPTopWidth� HeightTabOrder TextConnUser  TEditConnPwLeftPTop-Width� HeightPasswordChar*TabOrderTextConnPw   	TGroupBoxSBoxDialLeftTop_Width�Height� CaptionDiallingTabOrder TLabelLabelCanLeft
TopWidth2HeightCaptionCanonical NumberWordWrap	  TLabel	ConnPhoneLeft
TopKWidthIHeightCaptionPhone Number:  TLabelLabelCountryLeft
Top7Width,HeightCaption	Country:   TLabelLabel10Left	TopWidthZHeightCaptionAlternate Numbers  TLabelConnDialNumLeft
Top_Width<HeightCaptionDial Number:  TLabelLabel7Left
TopsWidthKHeightCaptionPhonebook File:  TLabelLabelPhonebookPathLeftZTopsWidth_HeightAutoSizeCaptionLabelPhonebookPathWordWrap	  TEditConnCanonicalLeftPTopWidth� HeightTabOrder TextConnCanonicalOnChangeConnCanonicalChange  TMemo
AltNumListLeft	Top#Width� HeightGLines.Strings
AltNumList ReadOnly	TabOrder   	TGroupBox
SBoxDeviceLeftTop� Width�HeightQCaptionDeviceTabOrder TLabel
DeviceTypeLeft
Top&Width8HeightCaption
DeviceType  TLabel
DeviceNameLeft
TopWidthEHeightCaptionDevice Name:   TLabel
DevicePortLeft� Top&Width4HeightCaption
DevicePort  TLabelLabelSubEntLeft
Top:WidthSHeightCaptionMultiple Channels  TLabelLabelModemInfoLeftTop&Width7HeightCaption
Modem Info  TLabelLabelDialModeLeft� Top:WidthFHeightCaptionMulti Dial Mode    	TTabSheet	TabSheet4CaptionFull Props ExplicitLeft ExplicitTop ExplicitWidth ExplicitHeight  TButton	doPropNewLeft� TopwWidth8HeightCaptionNewTabOrderOnClickdoPropNewClick  TButton
doPropLoadLeft
TopwWidth8HeightCaptionLoadTabOrder OnClickdoPropLoadClick  TButton
doPropSaveLeft� TopwWidth8HeightCaptionSaveTabOrderOnClickdoPropSaveClick  TPageControlFullPropsPagesLeftTop Width�Heighti
ActivePageTabDialTabOrder 	TTabSheetTabDialCaptionDiallingExplicitLeft ExplicitTop ExplicitWidth ExplicitHeight  TLabelLabel5Left
Top
Width7HeightCaption
Entry Name  	TGroupBoxLocationBoxTagLeftTop#Width�Height#CaptionLocation and Phone NumberTabOrder TLabelLabel6Left
Top-WidthJHeightCaptionCountry/Region  TLabelLabel11Left� TopKWidth4HeightCaption	Area Code  TLabelLabel12Left
Top� WidthWHeightCaptionCanonical Number  TLabelLabel13Left
TopiWidthBHeightCaptionLocal Number  TLabelLabel14Left
Top� WidthZHeightCaptionAlternate Numbers
(NT4/W2K)  TLabelLabelNumberDispLeftTop� WidthTHeightCaptionLabelNumberDisp  TLabelLabelNumberDialLeftTop� WidthPHeightCaptionLabelNumberDial  TLabelLabel44Left
TopKWidthBHeightCaptionCountry Code  	TCheckBoxentUseCountryandAreaCodesLeft
TopWidth� HeightCaptionUse Country and Area CodesTabOrder OnClickentUseCountryandAreaCodesClick  	TComboBoxentCountryNameLeftnTop(Width� HeightStylecsDropDownList
ItemHeight TabOrderOnChangeentCountryNameChange  TEditentAreaCodeTagLeft� TopFWidthyHeightTabOrderTextentAreaCodeOnChangeNumberChanged  TEditentLocalNumberLeftnTopdWidthHeightTabOrderTextentLocalNumberOnChangeNumberChanged  TEditentCanonNumberLeftnTop� WidthHeightReadOnly	TabOrderTextentCanonNumberOnChangeCanonNumberChange  TMemoentAlternatesLeftnTop� WidthHeight=
ScrollBars
ssVerticalTabOrder  	TCheckBoxentPromoteAlternatesLeft
Top	Width� HeightCaptionPromote Alternate NumbersTabOrder	  TEditentCountryCodeLeftnTopFWidthBHeightTabOrderOnChangeNumberChanged  TButton
doPropDialLeftETop
WidthPHeightCaptionDialling PropsTabOrderOnClickdoPropDialClick  TEditentCountryIdLeftOTop%Width$HeightEnabledTabOrderTextentCountryId   TEditentEntryNameLeftKTopWidth� HeightReadOnly	TabOrder TextentEntryName   	TTabSheetTabLogonCaptionLogon && DeviceExplicitLeft ExplicitTop ExplicitWidth ExplicitHeight  	TGroupBox	DeviceBoxLeftTop� Width�HeightwCaptionDial DeviceTabOrder TLabelLabel15Left
TopWidthHeightCaptionName  TLabel	LabelPortLeft
TopPWidthHeightCaptionPort  TLabelLabel17Left
Top2WidthHeightCaptionType  TLabelLabel32LeftETop2Width>HeightCaptionIdle Seconds  TEditentDeviceTypeLeft2Top-WidthLHeightReadOnly	TabOrderTextentDeviceType  TEditentDevicePortLeft2TopKWidth=HeightReadOnly	TabOrderTextentDevicePort  	TComboBoxentDeviceNameLeft2TopWidthAHeightStylecsDropDownList
ItemHeight TabOrder OnChangeentDeviceNameChange  	TSpinEditentIdleDisconnectSecondsLeftETopFWidthLHeight	MaxLengthMaxValue MinValue TabOrderValue   TRadioGroupentIdleOptionLeft� Top(Width� HeightGCaptionIdle Disconnect (NT4/W2K/XP)	ItemIndex Items.StringsNoneFrom User PreferencesSpecified Period TabOrder   	TGroupBoxLogonBoxLeftTopWidth�Height� CaptionLogon Details TabOrder  TLabelLabel16Left
Top2Width2HeightCaptionPassword  TLabelLabel18Left
TopWidth<HeightCaption
Logon Name  TLabelLabel19Left
Top_Width#HeightCaptionDomain  TLabelLabel20Left
Top}WidthPHeightCaptionCallback Number  TEditentUsernameLeftiTopWidth� HeightTabOrder TextentUsername  TEditentPasswordLeftiTop(Width� HeightPasswordChar*TabOrderTextentPassword  TEdit	entDomainLeftiTopZWidth� HeightTabOrderText	entDomain  TEditentCallBackNumberLeftiTopxWidth� HeightTabOrderTextentCallBackNumber  	TCheckBoxentDefaultCredsLeft
TopFWidth<HeightCaption,Save logon for anyone who uses this computerTabOrderOnClickentUseCountryandAreaCodesClick   	TGroupBoxAutoDialBoxLeftTopWidth�Height.CaptionAutodial (not settable)TabOrder TLabelLabel21LeftTopWidth)HeightCaptionFunction  TLabelLabel22Left
TopWidth(HeightCaptionProgram  TEditentAutoDialDllLeft<TopWidth� HeightReadOnly	TabOrder TextentAutoDialDll  TEditentAutoDialFuncLeftJTopWidthQHeightReadOnly	TabOrderTextentAutoDialFunc    	TTabSheetTabProtocolCaption	ProtocolsExplicitLeft ExplicitTop ExplicitWidth ExplicitHeight  TRadioGroupentFramingProtocolLeft
TopWidth� HeightQCaptionNetwork Framing Protocol	ItemIndex Items.StringsPPP (internet)SLIP (UNIX)Async Netbeui (not W2K) TabOrder   	TGroupBoxProtocolBoxLeft� TopWidthyHeightQCaptionProtocolTabOrder 	TCheckBoxentNetTCPIPLeft
TopWidthjHeightCaptionTCP/IP (internet)TabOrder   	TCheckBox	entNetIPXLeft
Top#WidthaHeightCaptionIPX (Netware)TabOrder  	TCheckBox
entNetBEUILeft
Top7WidthjHeightCaptionNetBEUI (LANs)TabOrder   	TGroupBoxTCPIPBoxLeft
Top_Width�Height� CaptionTCP/IP SettingsTabOrder TLabelLabel23Left� TopPWidthHeightCaptionWINS  TLabelLabel24LeftTop2WidthHeightCaptionAlt  TLabelLabel25Left� Top2WidthHeightCaptionDNS  TLabelLabel26LeftTopPWidthHeightCaptionAlt  	TCheckBoxentSpecificIPAddressLeft
TopWidth� HeightCaptionSpecific Fixed IP AddressTabOrder   	TCheckBoxentSpecificNameServersLeft
Top7Width� HeightCaptionSpecific Name ServersTabOrder  	TCheckBoxentHeaderCompressionLeft
TopiWidth� HeightCaptionHeader CompressionTabOrder  	TCheckBoxentRemoteDefaultGatewayLeft� TopiWidth� HeightCaptionRemote Default GatewayTabOrder  	TMaskEditentIPAddressLeft� TopWidth[HeightEditMask999.999.999.999;1;_	MaxLengthTabOrderText   .   .   .     	TMaskEditentDNSAddressLeft� Top-Width[HeightEditMask999.999.999.999;1;_	MaxLengthTabOrderText   .   .   .     	TMaskEditentDNSAddressAltLeft6Top-Width[HeightEditMask999.999.999.999;1;_	MaxLengthTabOrderText   .   .   .     	TMaskEditentWINSAddressLeft� TopKWidth[HeightEditMask999.999.999.999;1;_	MaxLengthTabOrderText   .   .   .     	TMaskEditentWINSAddressAltLeft6TopKWidth[HeightEditMask999.999.999.999;1;_	MaxLengthTabOrderText   .   .   .      TRadioGroupentSlipFrameSizeLeftJTop
WidtheHeight8CaptionSLIP Frame Size	ItemIndex Items.Strings1,0061,500 TabOrder  	TGroupBox
SpecialBoxLeft
Top� Width�HeightVCaptionSpecial SettingsTabOrder 	TCheckBoxentNetworkLogonLeft
TopWidth� HeightCaption#Network Logon (Win9x, WAN/LAN only)TabOrder   	TCheckBoxentDisableLCPExtensionsLeft
Top(Width� HeightCaption*Disable LCP Extensions (for older system) TabOrder  	TCheckBoxentSoftwareCompressionLeft
Top<Width� HeightCaptionSoftware Compression (CCP)TabOrder  	TCheckBoxentTerminalAfterDialLeft� Top'Width� HeightCaptionTerminal After DiallingTabOrder  	TCheckBoxentTerminalBeforeDialLeft� TopWidth� HeightCaptionTerminal Before DiallingTabOrder  	TCheckBoxentModemLightsLeft� Top<WidthaHeightCaptionModem LightsTabOrder    	TTabSheetTabSecurityCaptionSecurityExplicitLeft ExplicitTop ExplicitWidth ExplicitHeight  	TGroupBoxPasswordBoxLeft
TopWidth�Height� CaptionAuthentification and EncyptionTabOrder  TLabelLabel33Left� TopsWidth@HeightCaptionCustom Auth KeyWordWrap	  	TCheckBoxentRequireEncryptedPasswordLeft
TopWidth� HeightCaptionRequire Encrypted PasswordTabOrder   	TCheckBoxentRequireMSEncryptedPasswordLeft
Top(Width� HeightCaptionRequire MS Encrypted PasswordTabOrder  	TCheckBoxentRequireDataEncryptionLeft
Top<Width� HeightCaptionRequire Data EncryptionTabOrder  	TCheckBoxentUseLogonCredentialsLeft
TopPWidth� HeightCaptionUse Logon CredentialsTabOrder  	TCheckBoxentRequireEAPLeft� TopWidth� HeightCaptionRequire EAP (W2K)TabOrder  	TCheckBoxentRequirePAPLeft� Top(Width� HeightCaptionRequire PAP (W2K)TabOrder  	TCheckBoxentRequireSPAPLeft� Top<Width� HeightCaptionRequire SPAP (W2K)TabOrder  	TCheckBoxentRequireCHAPLeft� TopPWidth� HeightCaptionRequire CHAP (W2K)TabOrder	  	TCheckBoxentRequireMsCHAPLeft� TopdWidth� HeightCaptionRequire MS CHAP (W2K)TabOrder
  	TCheckBoxentRequireMsCHAP2Left� TopxWidth� HeightCaptionRequire MS CHAP2 (W2K)TabOrder  	TCheckBoxentRequireW95MSCHAPLeft� Top� Width� HeightCaptionRequire W95 MS CHAP (W2K)TabOrder  	TCheckBox	entCustomLeft� Top� Width� HeightCaptionCustom Encryption (W2K)TabOrder  TRadioGroupentEncryptionTypeLeft
TopiWidthyHeightVCaptionEncryption Type (W2K)	ItemIndexItems.StringsNone40 bit128 bitOptional (typical) TabOrder  	TSpinEditentCustomAuthKeyLeft� Top� WidthBHeightMaxValue MinValue TabOrderValue    	TGroupBoxX25BoxLeft
Top� Width�HeightoCaption"X25 Packet Switching - NT/W2K onlyTabOrder TLabelLabel28Left� Top7Width)HeightCaption
Facilities  TLabelLabel29Left
Top7Width*HeightCaptionAddress  TLabelLabel30Left
TopPWidth0HeightCaption	User Data  TLabelLabel31Left
TopWidth,HeightCaptionPad Type  TEditentX25PadTypeLeftFTopWidth� HeightTabOrder TextentX25PadType  TEditentX25AddressLeftFTop2Width� HeightTabOrderTextentX25Address  TEditentX25UserDataLeftFTopPWidthKHeightTabOrderTextentX25UserData  TEditentX25FacilitiesLeftTop2WidthyHeightTabOrderTextentX25Facilities    	TTabSheet	TabScriptCaptionScriptExplicitLeft ExplicitTop ExplicitWidth ExplicitHeight  TLabelLabel51Left
Top,WidthKHeightCaptionPhonebook File:  TLabelLabelPhonebookPathFullLeftZTop,Width_HeightAutoSizeCaptionLabelPhonebookPathWordWrap	  	TGroupBox	ScriptBoxLeft
TopWidth�Height[CaptionScript FileTabOrder  TLabelLabel27Left
TopWidth.HeightCaption	File Name  TEdit	entScriptLeftATopWidthPHeightTabOrder Text	entScript  TButtondoScriptOpenLeft� Top2WidthKHeightCaptionBrowseTabOrderOnClickdoScriptOpenClick  TButtondoScriptViewLeftTop2WidthKHeightCaptionViewTabOrderOnClickdoScriptViewClick   TMemo
ViewScriptLeft
TopiWidth�Height� ReadOnly	
ScrollBarsssBothTabOrder   	TTabSheetTabMultilinkCaption	MultilinkExplicitLeft ExplicitTop ExplicitWidth ExplicitHeight  	TGroupBoxMultilinkBoxLeft
TopWidth�Height� CaptionMultilink - MPPPTabOrder  TLabelLabel34Left
TopWidthbHeightCaptionNumber of Channels  TLabelLabel42Left� TopWidthHHeightCaption(Old Channels)  TEditentSubEntriesLeft� TopWidth$HeightReadOnly	TabOrder TextentSubEntries  TEditentISDNChannelsLeftTopWidth.HeightTabOrderTextentISDNChannels  	TListViewMultilinkListLeftTop(Width�Height� 
Checkboxes	ColumnsCaptionDeviceWidth�  CaptionPortWidth< CaptionLocal Telephone NumberWidth�  CaptionTypeWidthF  ReadOnly	TabOrder	ViewStylevsReport   	TGroupBoxBAPBoxLeft
Top� Width�Height`Caption1Bandwidth Allocation Protocol (BAP) (W2K/XP only)TabOrder TLabelLabel35LeftTop<Width>HeightCaptionSample secs  TLabelLabel36LeftiTop<WidthUHeightCaptionHang-Up, percent  TLabelLabel37LeftTopWidth?HeightCaptionSample Secs  TLabelLabel38LeftiTopWidthXHeightCaptionDial, extra percent  TRadioGroupentDialModeLeft
TopWidthVHeightGCaption	Dial Mode	ItemIndex Items.StringsSingleAll	As Needed TabOrder   	TSpinEditentHangUpExtraPercentLeft� Top7Width=HeightMaxValue MinValue TabOrderValue   	TSpinEditentDialExtraPercentLeft� TopWidth=HeightMaxValue MinValue TabOrderValue   	TSpinEditentDialExtraSampleSecondsLeft^TopWidth=HeightMaxValue MinValue TabOrderValue   	TSpinEditentHangUpExtraSampleSecondsLeft^Top7Width=HeightMaxValue MinValue TabOrderValue     	TTabSheetTabW2000CaptionW2KExplicitLeft ExplicitTop ExplicitWidth ExplicitHeight  	TGroupBox	BoxExtrasLeft
TopWidth�HeightACaption Windows 2000/XP/Net ExtrasTabOrder  TLabelLabel39LeftTop"Width"HeightCaptionGuid ID  TLabelLabel40LeftTop	WidthNHeightCaptionCustom Dial DLL  	TCheckBoxentSecureLocalFilesLeft
TopWidth� HeightCaptionSecure Local FilesTabOrder   	TCheckBoxentPreviewPhoneNumberLeft
Top#Width� HeightCaptionPreview Phone NumberTabOrder  	TCheckBoxentSharedPhoneNumbersLeft� TopKWidth� HeightCaptionShared Phone NumbersTabOrder  	TCheckBoxentPreviewUserPwLeft� TopWidth� HeightCaptionPreview User PwTabOrder  	TCheckBoxentPreviewDomainLeft� Top#Width~HeightCaptionPreview DomainTabOrder  	TCheckBoxentShowDialingProgressLeft� Top7Width� HeightCaptionShow Dialing ProgressTabOrder  TRadioGroupentPTypeLeft
Top<Width� Height� CaptionPhonebook Entry Type	ItemIndex Items.StringsUnknownPhone Modem/ISDNVNPDirect Serial/ParallelConnection ManagerBroadband (XP only) TabOrder  TRadioGroupentVpnStrategyLeft� TopdWidth� Height~CaptionVPN Strategy	ItemIndex Items.StringsDefaultPptpOnly	PptpFirstL2tpOnly	L2tpFirst TabOrder  TEdit	entguidIdLeftdTop"Width-HeightReadOnly	TabOrder	  TEditentCustomDialDllLeftdTopWidth-HeightReadOnly	TabOrderTextentCustomDialDll  TRadioGroupentPhoneBookLeft
Top� Width� Height3CaptionPhonebook Location	ItemIndexItems.StringsCurrent User	All Users TabOrder
    	TTabSheet	TabSheet8CaptionXP
ImageIndexExplicitLeft ExplicitTop ExplicitWidth ExplicitHeight  	TGroupBox	GroupBox6Left
TopWidth�Height<Caption Windows XP ExtrasTabOrder  TLabelLabel52Left
Top� WidthVHeightCaptionTCP Window Size  TLabelLabel54Left
Top� Width5HeightCaption
DNS Suffix  TLabelLabel55Left
Top� WidthUHeightCaptionPrerequisite Entry  TLabelLabel56Left
Top� WidthqHeightCaptionPrerequisite Phonebook  TLabelLabel57Left
TopWidthhHeightCaption#Time Between Redial Attempts (secs)WordWrap	  TLabelLabel58Left
Top� WidthJHeightCaptionRedial Attempts  	TCheckBoxentSecureFileAndPrintLeft
TopWidth� HeightCaptionDon't Allow File and PrintTabOrder   	TCheckBoxentDontNegotiateMultilinkLeft
Top7Width� HeightCaption)Don't Negotiate Multilink for Single LinkTabOrder  	TCheckBoxentSecureClientForMSNetLeft
Top#Width� HeightCaption"Don't Allow Client for MS NetworksTabOrder  	TCheckBoxentDontUseRasCredentialsLeft
TopKWidth� HeightCaptionDon't Use RAS CredentialsTabOrder  	TCheckBoxentUsePreSharedKeyLeft
Top_Width� HeightCaption!Use Pre-Shared Authentication KeyTabOrder  	TCheckBoxentUseGlobalDeviceSettingsLeft� Top7Width� HeightCaptionUse Global Device SettingsTabOrder  	TCheckBoxentDisableNbtOverIPLeft� Top#Width� HeightCaptionDisable NBT Probing Over IPTabOrder  	TCheckBoxentInternetLeft� TopWidth� HeightCaptionInternetTabOrder  	TCheckBoxentReconnectIfDroppedLeft� TopKWidth� HeightCaptionRedial If Line DroppedTabOrder  	TCheckBoxentSharePhoneNumbersLeft� Top_Width� HeightCaption!Multilink Devices Use Same NumberTabOrder	  	TSpinEditentTcpWindowSizeLeft� Top� Width=HeightMaxValue MinValue TabOrder
Value   TEditentDnsSuffixLeft� Top}WidtheHeightTabOrder  TEditentPrerequisitePbkLeft� Top� Width
HeightTabOrder  TEditentPrerequisiteEntryLeft� Top� Width
HeightTabOrder  	TSpinEditentRedialCountLeft� Top� Width=HeightMaxValue MinValue TabOrderValue   	TSpinEditentRedialPauseLeft� TopWidth=HeightMaxValue MinValue TabOrderValue      TButton
doPropDumpLeftmTopwWidth[HeightCaptionDump RasentryTabOrderOnClickdoPropDumpClick  TButton
doPropCopyLeftKTopwWidth8HeightCaptionCopyTabOrderOnClickdoPropCopyClick   	TTabSheet	TabSheet5Caption	Quick New TLabelLabel46Left
Top(Width7HeightCaption
Entry Name  TLabelLabel48Left<Top
Width6HeightCaption<Create a New Connection/Phonebook Using Default PPP Settings  	TGroupBox	GroupBox3LeftTopFWidth�HeighttCaptionLogon DetailsTabOrder  TLabelLabel43Left
TopWidth<HeightCaption
Logon Name  TLabelLabel45Left
Top2Width2HeightCaptionPassword  TLabelLabel9Left(Top_Width�HeightCaptionNNote: Program Needs Admin Rights to create All Users entries (see Network tab)  TEditquickUserNameLeftPTopWidth� HeightTabOrder TextquickUserName  TEditquickPasswordLeftPTop-Width� HeightPasswordChar*TabOrderTextquickPassword  TRadioGroupquickPhoneBookLeft6TopWidth� Height3CaptionPhonebook Location	ItemIndexItems.StringsCurrent User	All Users TabOrder  	TCheckBoxquickDefaultCredsLeft
TopKWidth<HeightCaption,Save logon for anyone who uses this computerTabOrderOnClickentUseCountryandAreaCodesClick   TEditquickEntryNameLeftPTop#Width� HeightTabOrderTextquickEntryName  	TGroupBox	GroupBox4LeftTop$Width�Height1CaptionDial DeviceTabOrder TLabelLabel47Left
TopWidthHeightCaptionName  	TComboBoxquickDeviceNameLeft2TopWidthAHeightStylecsDropDownList
ItemHeightTabOrder    	TGroupBox	GroupBox5TagLeftTop� Width�Height[CaptionLocation and Phone NumberTabOrder TLabelLabel53LeftTopWidthWHeightCaptionCanonical Number  TLabelqLabelNumberDispLeftTop-WidthZHeightCaptionqLabelNumberDisp  TLabelqLabelNumberDialLeftTopAWidthVHeightCaptionqLabelNumberDial  TEditquickCanonNumberLeftnTopWidthHeightTabOrder TextquickCanonNumberOnChangequickCanonNumberChange   TButtondoQuickClearLeft#TopwWidthKHeightCaptionClearTabOrderOnClickdoQuickClearClick  TButtondoQuickCreateLeft� TopwWidthKHeightCaptionCreateTabOrderOnClickdoQuickCreateClick   	TTabSheet	TabSheet7Caption
Entry List
ImageIndex TLabelLabelEntryResLeft� Top
WidthHeight  TButtondoRefreshEntriesLeftTopWidthKHeightCaptionRefresh ListTabOrder OnClickdoRefreshEntriesClick  	TListViewEntriesListLeft Top-Width�HeightdColumnsCaptionNameWidthd CaptionNumber/HostWidthd CaptionDevice 1: NameWidthx CaptionPort CaptionTypeWidth< CaptionDevice 2: NameWidthx CaptionPort CaptionTypeWidth< CaptionLocationWidth< Caption	PhonebookWidthX  ReadOnly	TabOrder	ViewStylevsReport  	TCheckBoxEntryUseAPILeftiTop
WidthaHeightCaptionUse RAS APIsTabOrder   	TTabSheet	TabSheet9CaptionNetwork
ImageIndexExplicitLeft ExplicitTop ExplicitWidth ExplicitHeight  	TGroupBox	GroupBox7LeftTop}Width�HeighteCaptionNetwork InformationTabOrder  TLabelLabelMACAddrLeft
TopWidthCHeightCaptionMAC Address  TLabelLabelNetAliveLeft� TopWidth*HeightCaption	Net Alive  TLabelLabelQOSLeft� Top-Width*HeightCaptionNet QOS   	TGroupBox	GroupBox8LeftTop� Width�Height� CaptionInternet Options (for MSIE)TabOrder TLabelLabel59Left
TopiWidth^HeightCaptionDefault Connection   	TComboBoxoptMSIEDefConnLeftsTopdWidth2Height
ItemHeight TabOrder TextoptMSIEDefConn
OnDropDownoptMSIEDefConnDropDown  TButtondoMSIEUpdateLeft� Top� WidthKHeightCaptionUpdateTabOrderOnClickdoMSIEUpdateClick  TRadioGroupoptMSIEAutDialLeft
TopWidth<HeightGCaptionAuto Dial Option	ItemIndex Items.StringsNever dial a connection1Dial whenever a network connection is not present!Always dial my default connection TabOrder   	TGroupBox	GroupBox9LeftTop Width�HeighttCaptionWindows InformationTabOrder TLabelLabelRASVerLeft
Top#WidthGHeightCaptionLabelRASVerFont.CharsetANSI_CHARSET
Font.ColorclRedFont.Height�	Font.NameArial
Font.StylefsBold 
ParentFont  TLabelLabelWinVerLeft
TopWidthEHeightCaptionLabelWinVerFont.CharsetANSI_CHARSET
Font.ColorclRedFont.Height�	Font.NameArial
Font.StylefsBold 
ParentFont  TLabelLabelVersionLeft
TopKWidth�HeightAutoSizeCaptionVersion
More versionFont.CharsetANSI_CHARSET
Font.ColorclRedFont.Height�	Font.NameArial
Font.StylefsBold 
ParentFontWordWrap	  TLabel
LabelAdminLeft
Top7WidthBHeightCaption
LabelAdminFont.CharsetANSI_CHARSET
Font.ColorclRedFont.Height�	Font.NameArial
Font.StylefsBold 
ParentFont     TButton	doConnectLeft
Top� WidthMHeightCaptionConnectTabOrderOnClickdoConnectClick  TButton	doDisConnLeft
Top	WidthMHeightCaption
DisconnectEnabledTabOrderOnClickdoDisConnClick  	TCheckBoxDebugLeft_Top1Width8HeightCaptionDebugChecked	State	cbCheckedTabOrder  TButtondoStatLeft
Top,WidthKHeightCaption
Dump StatsTabOrderOnClickdoStatClick  	TCheckBox
OptSpeakerLeft
Top� WidthQHeightCaption
Speaker OnChecked	State	cbCheckedTabOrder  	TComboBoxDialLinkLeftdTop� Width.HeightStylecsDropDownList
ItemHeight	ItemIndex TabOrder	TextAllItems.StringsAll12   TTimerTimerEnabledOnTimer
TimerTimerLeftTopx  TSaveDialogSaveDumpTitle&Save RASENTRY Structure as Binary FileLeft6Top6  TOpenDialog
OpenScript
DefaultExtscrFilterDial-Up Script (*.SCP)|*.SCPTitleSelect Dial-Up Script FilleLeftSTop{   