object POP3ExcercizerForm: TPOP3ExcercizerForm
  Left = 42
  Top = 43
  Caption = 'POP3 Excercizer - http://www.overbyte.be'
  ClientHeight = 348
  ClientWidth = 518
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object DisplayMemo: TMemo
    Left = 0
    Top = 233
    Width = 518
    Height = 115
    Hint = 'This area show the activity with the host'
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      'DisplayMemo')
    ParentFont = False
    ParentShowHint = False
    ScrollBars = ssBoth
    ShowHint = True
    TabOrder = 1
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 518
    Height = 233
    Align = alTop
    TabOrder = 0
    object InfoLabel: TLabel
      Left = 32
      Top = 156
      Width = 44
      Height = 13
      Caption = 'InfoLabel'
    end
    object Label1: TLabel
      Left = 20
      Top = 11
      Width = 53
      Height = 13
      Caption = 'POP3 Host'
    end
    object Label2: TLabel
      Left = 184
      Top = 11
      Width = 50
      Height = 13
      Caption = 'UserName'
    end
    object Label3: TLabel
      Left = 188
      Top = 35
      Width = 46
      Height = 13
      Caption = 'Password'
    end
    object Label4: TLabel
      Left = 416
      Top = 11
      Width = 42
      Height = 13
      Caption = 'MsgNum'
    end
    object Label5: TLabel
      Left = 432
      Top = 35
      Width = 25
      Height = 13
      Caption = 'Lines'
    end
    object Label6: TLabel
      Left = 23
      Top = 35
      Width = 50
      Height = 13
      Caption = 'POP3 Port'
    end
    object Label7: TLabel
      Left = 8
      Top = 183
      Width = 39
      Height = 13
      Caption = 'Subject:'
    end
    object Label8: TLabel
      Left = 8
      Top = 207
      Width = 29
      Height = 13
      Caption = 'From :'
    end
    object Label9: TLabel
      Left = 288
      Top = 207
      Width = 16
      Height = 13
      Caption = 'To:'
    end
    object Label11: TLabel
      Left = 5
      Top = 60
      Width = 68
      Height = 13
      Caption = 'Authentication'
    end
    object ConnectButton: TButton
      Left = 6
      Top = 79
      Width = 60
      Height = 21
      Caption = '&Connect'
      TabOrder = 6
      OnClick = ConnectButtonClick
    end
    object QuittButton: TButton
      Left = 264
      Top = 80
      Width = 60
      Height = 21
      Caption = '&Quit'
      TabOrder = 10
      OnClick = QuittButtonClick
    end
    object UserButton: TButton
      Left = 72
      Top = 80
      Width = 60
      Height = 21
      Caption = '&User'
      TabOrder = 7
      OnClick = UserButtonClick
    end
    object HostEdit: TEdit
      Left = 80
      Top = 8
      Width = 97
      Height = 21
      TabOrder = 0
      Text = 'HostEdit'
    end
    object UserNameEdit: TEdit
      Left = 244
      Top = 8
      Width = 89
      Height = 21
      TabOrder = 2
      Text = 'UserNameEdit'
    end
    object PassWordEdit: TEdit
      Left = 244
      Top = 32
      Width = 89
      Height = 21
      TabOrder = 3
      Text = 'PassWordEdit'
    end
    object PassButton: TButton
      Left = 136
      Top = 80
      Width = 60
      Height = 21
      Caption = '&Pass'
      TabOrder = 8
      OnClick = PassButtonClick
    end
    object MsgNumEdit: TEdit
      Left = 464
      Top = 8
      Width = 33
      Height = 21
      TabOrder = 4
      Text = '1'
    end
    object RetrButton: TButton
      Left = 200
      Top = 80
      Width = 60
      Height = 21
      Caption = '&Retr'
      TabOrder = 9
      OnClick = RetrButtonClick
    end
    object StatButton: TButton
      Left = 8
      Top = 104
      Width = 60
      Height = 21
      Caption = '&Stat'
      TabOrder = 14
      OnClick = StatButtonClick
    end
    object ListAllButton: TButton
      Left = 72
      Top = 104
      Width = 60
      Height = 21
      Caption = 'List All'
      TabOrder = 15
      OnClick = ListAllButtonClick
    end
    object ListButton: TButton
      Left = 136
      Top = 104
      Width = 60
      Height = 21
      Caption = 'List'
      TabOrder = 16
      OnClick = ListButtonClick
    end
    object DeleteButton: TButton
      Left = 200
      Top = 104
      Width = 60
      Height = 21
      Caption = 'Delete'
      TabOrder = 17
      OnClick = DeleteButtonClick
    end
    object NoopButton: TButton
      Left = 264
      Top = 104
      Width = 60
      Height = 21
      Caption = 'Noop'
      TabOrder = 18
      OnClick = NoopButtonClick
    end
    object LastButton: TButton
      Left = 328
      Top = 104
      Width = 60
      Height = 21
      Caption = 'Last'
      TabOrder = 19
      OnClick = LastButtonClick
    end
    object ResetButton: TButton
      Left = 392
      Top = 104
      Width = 60
      Height = 21
      Caption = 'Reset'
      TabOrder = 20
      OnClick = ResetButtonClick
    end
    object TopButton: TButton
      Left = 328
      Top = 80
      Width = 60
      Height = 21
      Caption = 'Top'
      TabOrder = 11
      OnClick = TopButtonClick
    end
    object MsgLinesEdit: TEdit
      Left = 464
      Top = 32
      Width = 33
      Height = 21
      TabOrder = 5
      Text = '0'
    end
    object RpopButton: TButton
      Left = 392
      Top = 80
      Width = 60
      Height = 21
      Caption = 'Rpop'
      TabOrder = 12
      OnClick = RpopButtonClick
    end
    object UidlButton: TButton
      Left = 328
      Top = 128
      Width = 60
      Height = 21
      Caption = 'Uidl'
      TabOrder = 27
      OnClick = UidlButtonClick
    end
    object ApopButton: TButton
      Left = 392
      Top = 128
      Width = 60
      Height = 21
      Caption = 'Apop'
      TabOrder = 28
      OnClick = ApopButtonClick
    end
    object NextButton: TButton
      Left = 456
      Top = 128
      Width = 60
      Height = 21
      Caption = '&Next'
      Default = True
      TabOrder = 29
      OnClick = NextButtonClick
    end
    object GetAllButton: TButton
      Left = 456
      Top = 80
      Width = 60
      Height = 21
      Caption = 'Get All'
      TabOrder = 13
      OnClick = GetAllButtonClick
    end
    object PortEdit: TEdit
      Left = 80
      Top = 32
      Width = 97
      Height = 21
      TabOrder = 1
      Text = 'PortEdit'
    end
    object OpenButton: TButton
      Left = 8
      Top = 128
      Width = 60
      Height = 21
      Caption = '&Open'
      TabOrder = 22
      OnClick = OpenButtonClick
    end
    object AbortButton: TButton
      Left = 264
      Top = 128
      Width = 60
      Height = 21
      Caption = 'Abort'
      TabOrder = 26
      OnClick = AbortButtonClick
    end
    object SubjectEdit: TEdit
      Left = 56
      Top = 180
      Width = 457
      Height = 21
      TabStop = False
      Color = clSilver
      ReadOnly = True
      TabOrder = 30
      Text = 'SubjectEdit'
    end
    object FromEdit: TEdit
      Left = 56
      Top = 204
      Width = 225
      Height = 21
      TabStop = False
      Color = clSilver
      ReadOnly = True
      TabOrder = 31
      Text = 'FromEdit'
    end
    object ToEdit: TEdit
      Left = 312
      Top = 204
      Width = 201
      Height = 21
      TabStop = False
      Color = clSilver
      ReadOnly = True
      TabOrder = 32
      Text = 'ToEdit'
    end
    object AuthComboBox: TComboBox
      Left = 80
      Top = 56
      Width = 97
      Height = 21
      Style = csDropDownList
      TabOrder = 33
      Items.Strings = (
        'None'
        'Login'
        'CramMD5'
        'CramSHA1'
        'NTLM')
    end
    object AuthButton: TButton
      Left = 200
      Top = 128
      Width = 60
      Height = 21
      Caption = 'Auth'
      TabOrder = 25
      OnClick = AuthButtonClick
    end
    object OpenExButton: TButton
      Left = 72
      Top = 128
      Width = 60
      Height = 21
      Caption = 'OpenE&x'
      TabOrder = 23
      OnClick = OpenExButtonClick
    end
    object CapaButton: TButton
      Left = 136
      Top = 128
      Width = 60
      Height = 21
      Caption = '&Capa'
      TabOrder = 24
      OnClick = CapaButtonClick
    end
    object LoginButton: TButton
      Left = 456
      Top = 104
      Width = 60
      Height = 21
      Caption = 'Login'
      TabOrder = 21
      OnClick = LoginButtonClick
    end
  end
  object Pop3Client: TPop3Cli
    Tag = 0
    Host = 'localhost'
    SocketFamily = sfIPv4
    LocalAddr = '0.0.0.0'
    LocalAddr6 = '::'
    Port = 'pop3'
    UserName = 'fpiette'
    PassWord = 'fp'
    AuthType = popAuthNone
    MsgLines = 0
    MsgNum = 0
    OnDisplay = Pop3ClientDisplay
    OnMessageBegin = Pop3ClientMessageBegin
    OnMessageEnd = Pop3ClientMessageEnd
    OnMessageLine = Pop3ClientMessageLine
    OnListBegin = Pop3ClientListBegin
    OnListEnd = Pop3ClientListEnd
    OnListLine = Pop3ClientListLine
    OnUidlBegin = Pop3ClientUidlBegin
    OnUidlEnd = Pop3ClientUidlEnd
    OnUidlLine = Pop3ClientUidlLine
    OnCapaLine = Pop3ClientCapaLine
    OnHeaderEnd = Pop3ClientHeaderEnd
    OnRequestDone = Pop3ClientRequestDone
    Left = 332
    Top = 256
  end
end
