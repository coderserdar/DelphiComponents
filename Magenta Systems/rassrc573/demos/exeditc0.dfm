�
 TFORM1 0�?  TPF0TForm1Form1LeftXTopTCaptionBTMagRas Edit Entry Complex Example  - not a commercial applicationClientHeightClientWidth�Color	clBtnFaceFont.CharsetANSI_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.Style OldCreateOrderOnClose	FormCloseOnCreate
FormCreatePixelsPerInch`
TextHeight TLabelLabel3LeftTop|Width� HeightCaption%Defined Connections/Phonebook Entries  TButtondoExitLeftYTop�WidthKHeightCaptionExitTabOrder OnClickdoExitClick  TListBoxConnListLeftTop�Width� Heighte
ItemHeightTabOrderOnClickdoLoadClick  TButtondoLoadLeftYTop�WidthKHeightCaptionLoadTabOrderOnClickdoLoadClick  TButtondoSaveLeftYTop�WidthKHeightCaptionSaveTabOrderOnClickdoSaveClick  
TStatusBarStatusLeft Top�Width�HeightPanels SimplePanel	  TPageControlFullPropsPagesLeftTopWidth�Heighti
ActivePageTabDialTabOrder 	TTabSheetTabDialCaptionDialling TLabelLabel5Left
Top
Width7HeightCaption
Entry Name  	TGroupBoxLocationBoxTagLeftTop#Width�Height#CaptionLocation and Phone NumberTabOrder TLabelLabel6Left
Top-WidthJHeightCaptionCountry/Region  TLabelLabel11Left� TopKWidth4HeightCaption	Area Code  TLabelLabel12Left
Top� WidthWHeightCaptionCanonical Number  TLabelLabel13Left
TopiWidthBHeightCaptionLocal Number  TLabelLabel14Left
Top� WidthZHeightCaptionAlternate Numbers
(NT4/W2K)  TLabelLabelNumberDispLeftTop� WidthTHeightCaptionLabelNumberDisp  TLabelLabelNumberDialLeftTop� WidthPHeightCaptionLabelNumberDial  TLabelLabel44Left
TopKWidthBHeightCaptionCountry Code  	TCheckBoxentUseCountryandAreaCodesLeft
TopWidth� HeightCaptionUse Country and Area CodesTabOrder OnClickentUseCountryandAreaCodesClick  	TComboBoxentCountryNameLeftnTop(Width� HeightStylecsDropDownListTabOrderOnChangeentCountryNameChange  TEditentAreaCodeTagLeft� TopFWidthyHeightTabOrderTextentAreaCodeOnChangeNumberChanged  TEditentLocalNumberLeftnTopdWidthHeightTabOrderTextentLocalNumberOnChangeNumberChanged  TEditentCanonNumberLeftnTop� WidthHeightReadOnly	TabOrderTextentCanonNumberOnChangeentCanonNumberChange  TMemoentAlternatesLeftnTop� WidthHeight=
ScrollBars
ssVerticalTabOrder  	TCheckBoxentPromoteAlternatesLeft
Top	Width� HeightCaptionPromote Alternate NumbersTabOrder	  TEditentCountryCodeLeftnTopFWidthBHeightTabOrderOnChangeNumberChanged  TButton
doPropDialLeftETop
WidthPHeightCaptionDialling PropsTabOrderOnClickdoPropDialClick  TEditentCountryIdLeftOTop(Width$HeightEnabledTabOrderTextentCountryId   TEditentEntryNameLeftKTopWidth� HeightReadOnly	TabOrder TextentEntryName   	TTabSheetTabLogonCaptionLogon && Device
ImageIndexExplicitLeft ExplicitTop ExplicitWidth ExplicitHeight  	TGroupBox	DeviceBoxLeftTop� Width�HeightwCaptionDial DeviceTabOrder TLabelLabel15Left
TopWidthHeightCaptionName  TLabel	LabelPortLeft
TopPWidthHeightCaptionPort  TLabelLabel17Left
Top2WidthHeightCaptionType  TLabelLabel32LeftETop2Width>HeightCaptionIdle Seconds  TEditentDeviceTypeLeft2Top-WidthLHeightReadOnly	TabOrderTextentDeviceType  TEditentDevicePortLeft2TopKWidth=HeightReadOnly	TabOrderTextentDevicePort  	TComboBoxentDeviceNameLeft2TopWidthAHeightStylecsDropDownListTabOrder OnChangeentDeviceNameChange  	TSpinEditentIdleDisconnectSecondsLeftETopFWidthLHeight	MaxLengthMaxValue MinValue TabOrderValue   TRadioGroupentIdleOptionLeft� Top(Width� HeightGCaptionIdle Disconnect (NT4/W2K)	ItemIndex Items.StringsNoneFrom User PreferencesSpecified Period TabOrder   	TGroupBoxLogonBoxLeftTopWidth�Height� CaptionLogon Details TabOrder  TLabelLabel16Left
Top2Width2HeightCaptionPassword  TLabelLabel18Left
TopWidth<HeightCaption
Logon Name  TLabelLabel19Left
TopPWidth#HeightCaptionDomain  TLabelLabel20Left
TopnWidthPHeightCaptionCallback Number  TEditentUsernameLeftiTopWidth� HeightTabOrder TextentUsername  TEditentPasswordLeftiTop-Width� HeightPasswordChar*TabOrderTextentPassword  TEdit	entDomainLeftiTopKWidth� HeightTabOrderText	entDomain  TEditentCallBackNumberLeftiTopiWidth� HeightTabOrderTextentCallBackNumber   	TGroupBoxAutoDialBoxLeftTopWidth�Height8CaptionAutodial (not settable)TabOrder TLabelLabel21LeftTopWidth)HeightCaptionFunction  TLabelLabel22Left
TopWidth(HeightCaptionProgram  TEditentAutoDialDllLeft<TopWidth� HeightReadOnly	TabOrder TextentAutoDialDll  TEditentAutoDialFuncLeftJTopWidthQHeightReadOnly	TabOrderTextentAutoDialFunc    	TTabSheetTabProtocolCaption	Protocols
ImageIndexExplicitLeft ExplicitTop ExplicitWidth ExplicitHeight  TRadioGroupentFramingProtocolLeft
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
Top<Width� HeightCaptionSoftware Compression (CCP)TabOrder  	TCheckBoxentTerminalAfterDialLeft� Top'Width� HeightCaptionTerminal After DiallingTabOrder  	TCheckBoxentTerminalBeforeDialLeft� TopWidth� HeightCaptionTerminal Before DiallingTabOrder  	TCheckBoxentModemLightsLeft� Top<WidthaHeightCaptionModem LightsTabOrder    	TTabSheetTabSecurityCaptionSecurity
ImageIndexExplicitLeft ExplicitTop ExplicitWidth ExplicitHeight  	TGroupBoxPasswordBoxLeft
Top
Width�Height� CaptionAuthentification and EncyptionTabOrder  TLabelLabel33Left� TopsWidth@HeightCaptionCustom Auth KeyWordWrap	  	TCheckBoxentRequireEncryptedPasswordLeft
TopWidth� HeightCaptionRequire Encrypted PasswordTabOrder   	TCheckBoxentRequireMSEncryptedPasswordLeft
Top(Width� HeightCaptionRequire MS Encrypted PasswordTabOrder  	TCheckBoxentRequireDataEncryptionLeft
Top<Width� HeightCaptionRequire Data EncryptionTabOrder  	TCheckBoxentUseLogonCredentialsLeft
TopPWidth� HeightCaptionUse Logon CredentialsTabOrder  	TCheckBoxentRequireEAPLeft� TopWidth� HeightCaptionRequire EAP (W2K)TabOrder  	TCheckBoxentRequirePAPLeft� Top(Width� HeightCaptionRequire PAP (W2K)TabOrder  	TCheckBoxentRequireSPAPLeft� Top<Width� HeightCaptionRequire SPAP (W2K)TabOrder  	TCheckBoxentRequireCHAPLeft� TopPWidth� HeightCaptionRequire CHAP (W2K)TabOrder	  	TCheckBoxentRequireMsCHAPLeft� TopdWidth� HeightCaptionRequire MS CHAP (W2K)TabOrder
  	TCheckBoxentRequireMsCHAP2Left� TopxWidth� HeightCaptionRequire MS CHAP2 (W2K)TabOrder  	TCheckBoxentRequireW95MSCHAPLeft� Top� Width� HeightCaptionRequire W95 MS CHAP (W2K)TabOrder  	TCheckBox	entCustomLeft� Top� Width� HeightCaptionCustom Encryption (W2K)TabOrder  TRadioGroupentEncryptionTypeLeft
TopiWidthyHeightQCaptionEncryption Type (W2K)	ItemIndex Items.StringsNone40 bit128 bitOptional (Typical) TabOrder  	TSpinEditentCustomAuthKeyLeft� Top� WidthBHeightMaxValue MinValue TabOrderValue    	TGroupBoxX25BoxLeft
Top� Width�HeightoCaption"X25 Packet Switching - NT/W2K onlyTabOrder TLabelLabel28Left� Top7Width)HeightCaption
Facilities  TLabelLabel29Left
Top7Width*HeightCaptionAddress  TLabelLabel30Left
TopPWidth0HeightCaption	User Data  TLabelLabel31Left
TopWidth,HeightCaptionPad Type  TEditentX25PadTypeLeftFTopWidth� HeightTabOrder TextentX25PadType  TEditentX25AddressLeftFTop2Width� HeightTabOrderTextentX25Address  TEditentX25UserDataLeftFTopPWidthKHeightTabOrderTextentX25UserData  TEditentX25FacilitiesLeftTop2WidthyHeightTabOrderTextentX25Facilities    	TTabSheet	TabScriptCaptionScript
ImageIndexExplicitLeft ExplicitTop ExplicitWidth ExplicitHeight  	TGroupBox	ScriptBoxLeft
TopWidth�Height[CaptionScript FileTabOrder  TLabelLabel27Left
TopWidth.HeightCaption	File Name  TEdit	entScriptLeftATopWidthPHeightTabOrder Text	entScript  TButtondoScriptOpenLeft� Top2WidthKHeightCaptionBrowseTabOrderOnClickdoScriptOpenClick  TButtondoScriptViewLeftTop2WidthKHeightCaptionViewTabOrderOnClickdoScriptViewClick   TMemo
ViewScriptLeft
TopiWidth�Height� ReadOnly	
ScrollBarsssBothTabOrder   	TTabSheetTabMultilinkCaption	Multilink
ImageIndexExplicitLeft ExplicitTop ExplicitWidth ExplicitHeight  	TGroupBoxMultilinkBoxLeft
Top
Width�Height� CaptionMultilink - MPPPTabOrder  TLabelLabel34Left
TopWidthbHeightCaptionNumber of Channels  TLabelLabel42Left� TopWidthHHeightCaption(Old Channels)  TEditentSubEntriesLeft� TopWidth$HeightReadOnly	TabOrder TextentSubEntries  TEditentISDNChannelsLeftTopWidth.HeightTabOrderTextentISDNChannels  	TListViewMultilinkListLeftTop(Width�Height� 
Checkboxes	ColumnsCaptionDeviceWidth�  CaptionPortWidth< CaptionLocal Telephone NumberWidth�  CaptionTypeWidthF  ReadOnly	TabOrder	ViewStylevsReport   	TGroupBoxBAPBoxLeft
Top� Width�Height`Caption.Bandwidth Allocation Protocol (BAP) (W2K only)TabOrder TLabelLabel35LeftTop<Width>HeightCaptionSample secs  TLabelLabel36LeftiTop<WidthUHeightCaptionHang-Up, percent  TLabelLabel37LeftTopWidth?HeightCaptionSample Secs  TLabelLabel38LeftiTopWidthXHeightCaptionDial, extra percent  TRadioGroupentDialModeLeft
TopWidthVHeightGCaption	Dial Mode	ItemIndex Items.StringsSingleAll	As Needed TabOrder   	TSpinEditentHangUpExtraPercentLeft� Top7Width=HeightMaxValue MinValue TabOrderValue   	TSpinEditentDialExtraPercentLeft� TopWidth=HeightMaxValue MinValue TabOrderValue   	TSpinEditentDialExtraSampleSecondsLeft^TopWidth=HeightMaxValue MinValue TabOrderValue   	TSpinEditentHangUpExtraSampleSecondsLeft^Top7Width=HeightMaxValue MinValue TabOrderValue     	TTabSheetTabW2000CaptionW2000
ImageIndexExplicitLeft ExplicitTop ExplicitWidth ExplicitHeight  	TGroupBox	BoxExtrasLeftTop
Width�Height<CaptionWindows 2000 ExtrasTabOrder  TLabelLabel39Left
TopWidth"HeightCaptionGuid ID  TLabelLabel40Left
Top� WidthNHeightCaptionCustom Dial DLL  TLabelLabelPBLocationLeft
Top� WidthdHeightCaptionPhonebook Location:  	TCheckBoxentSecureLocalFilesLeft
TopWidth� HeightCaptionSecure Local FilesTabOrder   	TCheckBoxentPreviewPhoneNumberLeft
Top(Width� HeightCaptionPreview Phone NumberTabOrder  	TCheckBoxentSharedPhoneNumbersLeft
Top<Width� HeightCaptionShared Phone NumbersTabOrder  	TCheckBoxentPreviewUserPwLeft� TopWidth� HeightCaptionPreview User PwTabOrder  	TCheckBoxentPreviewDomainLeft� Top(Width~HeightCaptionPreview DomainTabOrder  	TCheckBoxentShowDialingProgressLeft� Top<Width� HeightCaptionShow Dialing ProgressTabOrder  TRadioGroupentPTypeLeft
TopUWidth� HeightjCaptionPhonebook Entry Type	ItemIndex Items.StringsUnknownPhoneVNPDirectInternet TabOrder  TRadioGroupentVpnStrategyLeft� TopUWidth� Height~CaptionVPN Strategy	ItemIndex Items.StringsDefaultPptpOnly	PptpFirstL2tpOnly	L2tpFirst TabOrder  TEdit	entguidIdLeft_TopWidth-HeightReadOnly	TabOrder	  TEditentCustomDialDllLeft_Top� Width-HeightReadOnly	TabOrderTextentCustomDialDll     
TMagRasCon	MagRasConPhoneBookPath=C:\ProgramData\Microsoft\Network\Connections\Pbk\rasphone.pbk
PBLocationbDefaultCredsSubEntry 	CountryID CountryCode EntryOptions DialMode Left� Top�  
TMagRasEdt	MagRasEdtbUseCountryAndAreaCodesbSpecificIPAddressbSpecificNameServersbHeaderCompressionbRemoteDefaultGatewaybDisableLCPExtensionsbTerminalBeforeDialbTerminalAfterDialbModemLightsbSoftwareCompressionbRequireEncryptedPasswordbRequireMSEncryptedPasswordbRequireDataEncryptionbNetworkLogonbUseLogonCredentialsbPromoteAlternatesbSecureLocalFilesbRequireEAPbRequirePAPbRequireSPAPbCustombPreviewPhoneNumberbSharedPhoneNumbersbPreviewUserPwbPreviewDomainbShowDialingProgressbRequireCHAPbRequireMsCHAPbRequireMsCHAP2bRequireW95MSCHAPbCustomScriptCountryCode 	CountryID 	IPAddress0.0.0.0
DNSAddress0.0.0.0DNSAddressAlt0.0.0.0WINSAddress0.0.0.0WINSAddressAlt0.0.0.0	FrameSize FramingProtocolframePPPbNetBEUIbNetIPX	bNetTCPIPISDNChannels DialModedialAllDialExtraPercent DialExtraSampleSeconds HangUpExtraPercent HangUpExtraSampleSeconds IdleDisconnectSeconds�PType	typePhoneEncryptionTypeencryptNoneCustomAuthKey VpnStrategy
vpnDefaultbSecureFileAndPrintbSecureClientForMSNetbDontNegotiateMultilinkbDontUseRasCredentialsbUsePreSharedKey	bInternetbDisableNbtOverIPbUseGlobalDeviceSettingsbReconnectIfDroppedbSharePhoneNumbersTcpWindowSize RedialCount RedialPause PhoneBookPath=C:\ProgramData\Microsoft\Network\Connections\Pbk\rasphone.pbk
PBLocationbDefaultCredsPasswordFlagLeft� Top�  TOpenDialog
OpenScriptLeft� Top�   