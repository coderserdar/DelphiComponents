object POP3ExcercizerForm: TPOP3ExcercizerForm
  Left = 247
  Top = 187
  Caption = 'POP3ExcercizerForm'
  ClientHeight = 341
  ClientWidth = 536
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object DisplayMemo: TMemo
    Left = 0
    Top = 141
    Width = 536
    Height = 200
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
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 536
    Height = 141
    Align = alTop
    TabOrder = 1
    object InfoLabel: TLabel
      Left = 32
      Top = 120
      Width = 44
      Height = 13
      Caption = 'InfoLabel'
    end
    object Label1: TLabel
      Left = 8
      Top = 16
      Width = 53
      Height = 13
      Caption = 'POP3 Host'
    end
    object Label2: TLabel
      Left = 176
      Top = 16
      Width = 50
      Height = 13
      Caption = 'UserName'
    end
    object Label3: TLabel
      Left = 176
      Top = 40
      Width = 46
      Height = 13
      Caption = 'Password'
    end
    object Label4: TLabel
      Left = 416
      Top = 16
      Width = 42
      Height = 13
      Caption = 'MsgNum'
    end
    object Label5: TLabel
      Left = 432
      Top = 40
      Width = 25
      Height = 13
      Caption = 'Lines'
    end
    object Label6: TLabel
      Left = 8
      Top = 40
      Width = 50
      Height = 13
      Caption = 'POP3 Port'
    end
    object ConnectButton: TButton
      Left = 8
      Top = 64
      Width = 60
      Height = 21
      Caption = '&Connect'
      TabOrder = 6
      OnClick = ConnectButtonClick
    end
    object QuittButton: TButton
      Left = 264
      Top = 64
      Width = 60
      Height = 21
      Caption = '&Quit'
      TabOrder = 10
      OnClick = QuittButtonClick
    end
    object UserButton: TButton
      Left = 72
      Top = 64
      Width = 60
      Height = 21
      Caption = '&User'
      TabOrder = 7
      OnClick = UserButtonClick
    end
    object HostEdit: TEdit
      Left = 72
      Top = 8
      Width = 97
      Height = 21
      TabOrder = 0
      Text = 'HostEdit'
    end
    object UserNameEdit: TEdit
      Left = 232
      Top = 8
      Width = 89
      Height = 21
      TabOrder = 2
      Text = 'UserNameEdit'
    end
    object PassWordEdit: TEdit
      Left = 232
      Top = 32
      Width = 89
      Height = 21
      TabOrder = 3
      Text = 'PassWordEdit'
    end
    object PassButton: TButton
      Left = 136
      Top = 64
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
      Top = 64
      Width = 60
      Height = 21
      Caption = '&Retr'
      TabOrder = 9
      OnClick = RetrButtonClick
    end
    object StatButton: TButton
      Left = 8
      Top = 88
      Width = 60
      Height = 21
      Caption = '&Stat'
      TabOrder = 14
      OnClick = StatButtonClick
    end
    object ListAllButton: TButton
      Left = 72
      Top = 88
      Width = 60
      Height = 21
      Caption = 'List All'
      TabOrder = 15
      OnClick = ListAllButtonClick
    end
    object ListButton: TButton
      Left = 136
      Top = 88
      Width = 60
      Height = 21
      Caption = 'List'
      TabOrder = 16
      OnClick = ListButtonClick
    end
    object DeleteButton: TButton
      Left = 200
      Top = 88
      Width = 60
      Height = 21
      Caption = 'Delete'
      TabOrder = 17
      OnClick = DeleteButtonClick
    end
    object NoopButton: TButton
      Left = 264
      Top = 88
      Width = 60
      Height = 21
      Caption = 'Noop'
      TabOrder = 18
      OnClick = NoopButtonClick
    end
    object LastButton: TButton
      Left = 328
      Top = 88
      Width = 60
      Height = 21
      Caption = 'Last'
      TabOrder = 19
      OnClick = LastButtonClick
    end
    object ResetButton: TButton
      Left = 392
      Top = 88
      Width = 60
      Height = 21
      Caption = 'Reset'
      TabOrder = 20
      OnClick = ResetButtonClick
    end
    object TopButton: TButton
      Left = 328
      Top = 64
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
      Top = 64
      Width = 60
      Height = 21
      Caption = 'Rpop'
      TabOrder = 12
      OnClick = RpopButtonClick
    end
    object UidlButton: TButton
      Left = 328
      Top = 112
      Width = 60
      Height = 21
      Caption = 'Uidl'
      TabOrder = 21
      OnClick = UidlButtonClick
    end
    object ApopButton: TButton
      Left = 392
      Top = 112
      Width = 60
      Height = 21
      Caption = 'Apop'
      TabOrder = 22
      OnClick = ApopButtonClick
    end
    object NextButton: TButton
      Left = 456
      Top = 112
      Width = 60
      Height = 21
      Caption = '&Next'
      Default = True
      TabOrder = 23
      OnClick = NextButtonClick
    end
    object GetAllButton: TButton
      Left = 456
      Top = 64
      Width = 60
      Height = 21
      Caption = 'Get All'
      TabOrder = 13
      OnClick = GetAllButtonClick
    end
    object PortEdit: TEdit
      Left = 72
      Top = 32
      Width = 97
      Height = 21
      TabOrder = 1
      Text = 'PortEdit'
    end
    object OpenButton: TButton
      Left = 456
      Top = 88
      Width = 60
      Height = 21
      Caption = 'Open'
      TabOrder = 24
      OnClick = OpenButtonClick
    end
  end
  object Pop3Client: TPop3Cli
    Tag = 0
    Host = 'localhost'
    LocalAddr = '0.0.0.0'
    Port = 'pop3'
    UserName = 'fpiette'
    PassWord = 'fp'
    AuthType = popAuthNone
    MsgLines = 0
    MsgNum = 0
    OnMessageBegin = Pop3ClientMessageBegin
    OnMessageEnd = Pop3ClientMessageEnd
    OnMessageLine = Pop3ClientMessageLine
    OnListBegin = Pop3ClientListBegin
    OnListEnd = Pop3ClientListEnd
    OnListLine = Pop3ClientListLine
    OnUidlBegin = Pop3ClientUidlBegin
    OnUidlEnd = Pop3ClientUidlEnd
    OnUidlLine = Pop3ClientUidlLine
    OnRequestDone = Pop3ClientRequestDone
    Left = 332
    Top = 168
  end
end
