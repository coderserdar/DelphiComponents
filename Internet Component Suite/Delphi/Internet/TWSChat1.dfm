�
 TTWSCHATFORM 0r
  TPF0TTWSChatFormTWSChatFormLeft� Top� Width�Height0Caption$TWSChat - http://www.rtfm.be/fpietteColor	clBtnFaceFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style OldCreateOrder	PositionpoDefaultPosOnlyOnShowFormShowPixelsPerInch`
TextHeight TPanelPanel1Left Top Width�Height)AlignalTopTabOrder  TLabelLabel1LeftTopWidthHeightCaptionServer  TEdit
ServerEditLeft0TopWidthyHeightTabOrder Text	localhost  TButtonConnectButtonLeft� TopWidth9HeightCaption&ConnectTabOrderOnClickConnectButtonClick  TButtonDisconnectButtonLeft� TopWidthAHeightCaption&DisconnectEnabledTabOrderOnClickDisconnectButtonClick  TRadioButtonRunningRadioButtonLeft@TopWidthqHeightCaption&RunningTabOrderOnClickRunningRadioButtonClick  TRadioButtonStoppedRadioButtonLeft@TopWidthqHeightCaptionSt&oppedTabOrderOnClickStoppedRadioButtonClick   TPanelPanel2Left Top� Width�Height)AlignalBottomTabOrder TEditMessageEditLeftTopWidthIHeightEnabledTabOrder TextMessageEdit  TButton
SendButtonLeftXTopWidth1HeightCaption&SendDefault	EnabledTabOrderOnClickSendButtonClick   TMemoDisplayMemoLeft Top)Width�Height� AlignalClientFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameCourier New
Font.Style Lines.StringsDisplayMemo 
ParentFontTabOrder  TWSocket
SrvWSocketLineMode	LineLimit   LineEnd
LineEchoLineEditPrototcp	LocalAddr0.0.0.0	LocalPort0MultiThreadedComponentOptions OnSessionAvailableSrvWSocketSessionAvailableFlushTimeout<	SendFlagswsSendNormalLingerOnOff
wsLingerOnLingerTimeout 
SocksLevel5SocksAuthenticationsocksNoAuthenticationLeftPTop8  TWSocket
CliWSocketLineMode	LineLimit   LineEnd
LineEchoLineEditPrototcp	LocalAddr0.0.0.0	LocalPort0MultiThreadedComponentOptions OnDataAvailableCliWSocketDataAvailableOnSessionClosedCliWSocketSessionClosedOnSessionConnectedCliWSocketSessionConnectedOnDnsLookupDoneCliWSocketDnsLookupDoneFlushTimeout<	SendFlagswsSendNormalLingerOnOff
wsLingerOnLingerTimeout 
SocksLevel5SocksAuthenticationsocksNoAuthenticationLeft� Top8  TWSocket
TmpWSocketLineMode	LineLimit   LineEnd
LineEchoLineEditPrototcp	LocalAddr0.0.0.0	LocalPort0MultiThreadedComponentOptions FlushTimeout<	SendFlagswsSendNormalLingerOnOff
wsLingerOnLingerTimeout 
SocksLevel5SocksAuthenticationsocksNoAuthenticationLeft� Topp   