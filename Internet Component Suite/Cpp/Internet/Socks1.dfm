�
 TSOCKSTESTFORM 0c
  TPF0TSocksTestFormSocksTestFormLeft� Top0Width	Height� Caption
Socks TestColor	clBtnFaceFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style OldCreateOrder	OnClose	FormCloseOnShowFormShowPixelsPerInch`
TextHeight TMemoDisplayMemoLeft TopmWidthHeightkAlignalClientLines.StringsDisplayMemo 
ScrollBarsssBothTabOrder WordWrap  TPanelPanel1Left Top WidthHeightmAlignalTopTabOrder TLabelLabel1LeftTopWidthRHeightCaptionTarget Hostname  TLabelLabel2LeftTop(Width5HeightCaptionTarget Port  TLabelLabel6Left� TopTWidthOHeightCaptionSocks Password  TLabelLabel5Left� Top<WidthOHeightCaptionSocks Usercode  TLabelLabel4LeftTop$Width4HeightCaption
Socks Port  TLabelLabel3Left� TopWidth@HeightCaptionSocks Server  TButtonConnectButtonLeftTopPWidthAHeightCaption&ConnectTabOrder OnClickConnectButtonClick  TButtonDisconnectButtonLeftPTopPWidthAHeightCaption&DisconnectTabOrderOnClickDisconnectButtonClick  TEditTargetHostEditLefthTopWidthyHeightTabOrderTextTargetHostEdit  TEditTargetPortEditLefthTop WidthyHeightTabOrderTextTargetPortEdit  TEditSocksServerEditLeftLTopWidthyHeightTabOrderTextSocksServerEdit  TEditSocksPortEditLeftLTop WidthyHeightTabOrderTextSocksPortEdit  TEditSocksUsercodeEditLeftLTop8WidthyHeightTabOrderTextSocksUsercodeEdit  TEditSocksPasswordEditLeftLTopPWidthyHeightTabOrderTextSocksPasswordEdit  TButtonClearButtonLeft� TopPWidthAHeightCaptionC&learTabOrderOnClickClearButtonClick  	TCheckBoxSocksAuthCheckBoxLeft� Top;WidthZHeight	AlignmenttaLeftJustifyCaptionAuthenticationTabOrder	  TRadioButtonSocks5RadioButtonLeftHTop;Width9Height	AlignmenttaLeftJustifyCaptionSocks 5Checked	TabOrder
TabStop	  TRadioButtonSocks4RadioButtonLeftTop;Width9Height	AlignmenttaLeftJustifyCaptionSocks 4TabOrder   TWSocketWSocket1LineModeLineEnd
LineEchoLineEditPrototcp	LocalAddr0.0.0.0	LocalPort0MultiThreadedOnDataAvailableWSocket1DataAvailableOnSessionClosedWSocket1SessionClosedOnSessionConnectedWSocket1SessionConnectedOnSocksConnectedWSocket1SocksConnectedFlushTimeout<	SendFlagswsSendNormalLingerOnOff
wsLingerOnLingerTimeout 
SocksLevel5SocksAuthenticationsocksNoAuthenticationOnSocksErrorWSocket1SocksErrorOnSocksAuthStateWSocket1SocksAuthStateLeft� Top|   