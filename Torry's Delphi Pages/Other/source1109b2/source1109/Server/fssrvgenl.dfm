�
 TFSGENCONFIGFORM 05  TPF0TfsGenConfigFormfsGenConfigFormLeft�TopWBorderStylebsDialogCaptionServerClientHeightDClientWidthMColor	clBtnFaceFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style OldCreateOrder	PositionpoScreenCenterShowHint	OnCreate
FormCreateOnShowFormShowPixelsPerInch`
TextHeight TBitBtn
btnDiscardLeftTop%WidthKHeightHintClick to discard all changesCancel	CaptionCancelModalResultTabOrder	NumGlyphs  TBitBtnbtnSaveLeft� Top%WidthKHeightHint1Click to save all changed information permanentlyCaption&OKDefault	TabOrder OnClickbtnSaveClick	NumGlyphs  TPanelPanel1Left Top WidthMHeightAlignalTop
BevelOuter	bvLoweredTabOrder TTabbedNotebookTabbedNotebook1LeftTopWidthKHeightAlignalClientTabFont.CharsetDEFAULT_CHARSETTabFont.Color	clBtnTextTabFont.Height�TabFont.NameMS Sans SerifTabFont.Style TabOrder  TTabPage LeftTopCaptionServer TPanelPanel2Left Top WidthCHeightAlignalClient
BevelOuterbvNoneTabOrder  TPanelPanel5Left Top WidthCHeightAlignalClient
BevelOuterbvNoneTabOrder  TLabellblServerNameLeftTopWidth?HeightCaptionSer&ver name:FocusControledtServerNameFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style 
ParentFont  TLabelLabel5LeftTop$Width[HeightCaption&Cache size (in MB):  TLabellblTempStoreSizeLeftTop=WidthHeightCaption&Temporary storage (in MB):FocusControledtTempStoreSize  TLabellblPriorityLeftTop� WidthCHeightHintServer PriorityCaptionServer &priority:FocusControlcbxPriorityFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style 
ParentFont  TLabelLabel6LeftTopWWidthMHeightCaptionTemporary path:FocusControledttmp  TSpeedButtonSpeedButton1Left(TopNWidthHeightCaption...OnClickSpeedButton1Click  TEditedtServerNameLeft� TopWidth� HeightHintThe server nameFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style 	MaxLength
ParentFontTabOrder   TEdit	edtMaxRAMLeft� TopWidth� HeightHint.Maximum number of RAM pages the server can useFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style 	MaxLength
ParentFontTabOrder  TEditedtTempStoreSizeLeft� Top6Width� HeightTabOrder  	TComboBoxcbxPriorityLeft� Top� Width� HeightStylecsDropDownListFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style 
ItemHeight
ParentFontTabOrderItems.StringsLowestBelow NormalNormalAbove NormalHighest   	TCheckBox
boxEncryptLeftTop� Width� HeightCaption&Encrypted configurationChecked	Font.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style 
ParentFontState	cbCheckedTabOrder  	TCheckBoxboxNoSaveCfgLeftTop� Width� HeightCaption%Disable saving &configuration changesFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style 
ParentFontTabOrder  	TCheckBoxboxSecurityLeftTop� WidthiHeightHintSelect if user logins requiredCaption&Security enabledFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style 
ParentFontTabOrder  	TCheckBoxboxReadOnlyLeftTop� WidthhHeightCaption
Read &OnlyFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style 
ParentFontTabOrder  	TCheckBox
boxTrigersLeftTop� WidthdHeightCaption&Enable TrigersFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style 
ParentFontTabOrder  	TCheckBoxboxConstrainsLeft� Top� WidthmHeightCaption&Enable ConstrainsFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style 
ParentFontTabOrder	  	TCheckBoxboxDebugLogLeft� Top� Width� HeightCaptionDebug &logging enabledFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style 
ParentFontTabOrder
  TEditedttmpLeft� TopNWidth� HeightTabOrder  	TCheckBoxboxEtmpLeft� TopjWidth� HeightCaptionEncrypt temp fileFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style 
ParentFontTabOrder     TTabPage LeftTopCaptionClient TPanelPanel4Left Top WidthCHeightAlignalClient
BevelOuterbvNoneTabOrder  	TGroupBox
gbxStartupLeftTopWidth>Height7Caption Startup Options Font.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style 
ParentFontTabOrder  	TCheckBoxboxServerUpLeftTopWidth� HeightHint3Select if the server is to be brought up on startupCaptionBring Server &up automaticallyFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style 
ParentFontTabOrder   	TCheckBoxboxMinimizeLeftTop WidthaHeightHint2Select if the server is to be minimized on startupCaptionStart minimi&zed Font.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style 
ParentFontTabOrder   	TGroupBox	gbCollectLeftTopIWidth>HeightVCaption$ Garbage Collection and Clear cache TabOrder TLabellblCollectFreqLeftTop(Width� HeightCaption!Collection &frequency (millisec):FocusControledtCollectFreq  	TCheckBoxboxCollectEnabledLeftTopWidth� HeightCaptionEna&bledChecked	State	cbCheckedTabOrder   TEditedtCollectFreqLeft� Top$WidthPHeightTabOrder  	TCheckBoxboxclearcacheLeftTop<Width[HeightCaptionClear cacheTabOrder  	TCheckBoxboxgarclosetablesLeftqTop;Width� HeightCaptionClose inactive tablesTabOrder   	TGroupBoxgbxKeepAliveLeftTop� Width>HeightZCaption Keep Alive Options Font.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style 
ParentFontTabOrder TLabellblLMIntervalLeftTopWidth� HeightAutoSizeCaption!&Interval from last message (ms):FocusControl
edtLastMsgFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style 
ParentFont  TLabellblBetKeepsLeftTop(Width� HeightAutoSizeCaption#Interval bet&ween Keep Alives (ms):FocusControledtKAIntervalFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style 
ParentFont  TLabellblKARetriesLeftTop?Width� HeightAutoSizeCaptionKeep &Alive retries:FocusControledtKARetriesFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style 
ParentFont  TEdit
edtLastMsgLeft� TopWidthPHeightFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style 
ParentFontTabOrder   TEditedtKAIntervalLeft� Top$WidthPHeightFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style 
ParentFontTabOrder  TEditedtKARetriesLeft� Top;WidthPHeightFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style 
ParentFontTabOrder     TTabPage LeftTopCaption
Connection TPanelPanel3Left Top WidthCHeightAlignalClient
BevelOuterbvNoneTabOrder  	TGroupBoxgrpTCPIPLeftTopWidth� Height� Caption Winsock TCP/IP Transport Font.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style 
ParentFontTabOrder  TLabelLabel2LeftTop� WidthWHeightCaptionNet&work interface:FocusControl
cmbTCPIntfFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style 
ParentFont  TLabel
lblTCPPortLeftTop4Width-HeightCaption
&TCP port:FocusControl
edtTCPPortFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style 
ParentFontLayouttlCenter  TLabellblUDPSrLeftTop\WidthOHeightCaption&UDP server port:FocusControledtUDPServerFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style 
ParentFontLayouttlCenter  TLabellblUDPClLeftTop� WidthKHeightCaptionU&DP client port:FocusControledtUDPClientFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style 
ParentFontLayouttlCenter  	TCheckBoxchkTCPEnabledLeftTopWidth� HeightCaption&EnabledFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style 
ParentFontTabOrder   	TCheckBoxchkTCPListenLeftTop Width� HeightCaption&Listen for broadcastsFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style 
ParentFontTabOrder  	TComboBox
cmbTCPIntfLeftTop� Width� HeightStylecsDropDownListFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style 
ItemHeight
ParentFontTabOrder  TEdit
edtTCPPortLeftTopDWidth� HeightFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style 
ParentFontTabOrder  TEditedtUDPServerLeftToplWidth� HeightFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style 
ParentFontTabOrder  TEditedtUDPClientLeftTop� Width� HeightFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style 
ParentFontTabOrder   	TGroupBox	grpIPXSPXLeft� TopWidth� Height� Caption Winsock IPX/SPX Transport Font.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style 
ParentFontTabOrder TLabellblIPXSocketLeftTop3WidthWHeightCaption&IPX server socket:FocusControledtIPXServerFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style 
ParentFontLayouttlCenter  TLabellblIPXClientLeftTop[WidthSHeightCaptionI&PX client socket:FocusControledtIPXClientFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style 
ParentFontLayouttlCenter  TLabellblSPXLeftTop� Width;HeightCaption&SPX socket:FocusControledtSPXSocketFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style 
ParentFontLayouttlCenter  	TCheckBoxchkIPXEnabledLeftTopWidth� HeightCaptionE&nabledFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style 
ParentFontTabOrder   	TCheckBoxchkIPXListenLeftTop Width� HeightCaptionListen for &broadcastsFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style 
ParentFontTabOrder  TEditedtIPXServerLeftTopCWidth� HeightFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style 
ParentFontTabOrder  TEditedtIPXClientLeftTopkWidth� HeightFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style 
ParentFontTabOrder  TEditedtSPXSocketLeftTop� Width� HeightFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style 
ParentFontTabOrder   	TGroupBoxgrpSUPLeft� Top� Width� Height&Caption Single User Transport Font.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style 
ParentFontTabOrder 	TCheckBoxchkSUPEnabledLeftTopWidth|HeightCaptionEn&abledFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style 
ParentFontTabOrder      TTabPage LeftTopCaptionOther TPanelPanel6Left Top WidthCHeightAlignalClient
BevelOuterbvNoneTabOrder  TLabelLabel4Left� TopUWidth^HeightCaptionMax database open  TLabelLabel3Left� TopnWidth`HeightCaptionMax duplicate Users  TLabelLabel1LeftToplWidth5HeightCaptionMax clients  	TCheckBox	boxupdateLeftTop!Width� HeightCaption,Clear cache if update tables (per operation)TabOrder   	TCheckBoxboxclosetableLeftTop6Width� HeightCaption.Close inactive tables after commit or roolbackTabOrder  	TSpinEditmFlushPerOperationLeft� TopWidthHHeight	IncrementdMaxValue@B MinValue TabOrderValue   	TSpinEditMMaxDatabaseLeftTopMWidth*HeightMaxValue�MinValue TabOrderValue   	TSpinEditMMaxDuplicateUsersLeftTopfWidth*HeightMaxValue�MinValue TabOrderValue   	TSpinEditMClientsLeftFTopgWidth*HeightMaxValue�MinValue TabOrderValue        