object Form1: TForm1
  Left = 485
  Top = 214
  Caption = 'NNTP Test'
  ClientHeight = 564
  ClientWidth = 584
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object MsgMemo: TMemo
    Left = 0
    Top = 191
    Width = 584
    Height = 120
    Hint = 'Enter the message text in this memo'
    Align = alTop
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      'Enter the message text in this memo')
    ParentFont = False
    ParentShowHint = False
    ScrollBars = ssBoth
    ShowHint = True
    TabOrder = 0
    ExplicitWidth = 575
  end
  object DisplayMemo: TMemo
    Left = 0
    Top = 394
    Width = 584
    Height = 170
    Hint = 'This memo shows info messages'
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ParentShowHint = False
    ReadOnly = True
    ScrollBars = ssBoth
    ShowHint = True
    TabOrder = 2
    ExplicitWidth = 575
    ExplicitHeight = 179
  end
  object ToolsPanel: TPanel
    Left = 0
    Top = 0
    Width = 584
    Height = 191
    Align = alTop
    TabOrder = 3
    ExplicitWidth = 575
    object Label1: TLabel
      Left = 15
      Top = 11
      Width = 62
      Height = 13
      Caption = 'NNTP server'
    end
    object Label2: TLabel
      Left = 21
      Top = 36
      Width = 56
      Height = 13
      Caption = 'NewsGroup'
    end
    object Label3: TLabel
      Left = 212
      Top = 36
      Width = 23
      Height = 13
      Caption = 'From'
    end
    object Subject: TLabel
      Left = 41
      Top = 82
      Width = 36
      Height = 13
      Caption = 'Subject'
    end
    object Label4: TLabel
      Left = 216
      Top = 11
      Width = 19
      Height = 13
      Caption = 'Port'
    end
    object Label5: TLabel
      Left = 8
      Top = 176
      Width = 66
      Height = 13
      Caption = 'Message text:'
    end
    object Label9: TLabel
      Left = 29
      Top = 60
      Width = 48
      Height = 13
      Caption = 'Username'
    end
    object Label10: TLabel
      Left = 212
      Top = 60
      Width = 23
      Height = 13
      Caption = 'Pass'
    end
    object Label11: TLabel
      Left = 34
      Top = 106
      Width = 43
      Height = 13
      Caption = 'Article ID'
    end
    object HostEdit: TEdit
      Left = 80
      Top = 8
      Width = 121
      Height = 21
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      Text = 'server.amis.tv'
    end
    object NewsGroupEdit: TEdit
      Left = 80
      Top = 32
      Width = 121
      Height = 21
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      Text = 'test'
    end
    object FromEdit: TEdit
      Left = 240
      Top = 32
      Width = 121
      Height = 21
      ParentShowHint = False
      ShowHint = True
      TabOrder = 3
      Text = 'toto@toto.com'
    end
    object SubjectEdit: TEdit
      Left = 80
      Top = 80
      Width = 281
      Height = 21
      ParentShowHint = False
      ShowHint = True
      TabOrder = 4
      Text = 'Message subject'
    end
    object PortEdit: TEdit
      Left = 240
      Top = 8
      Width = 121
      Height = 21
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      Text = '119'
    end
    object NextButton: TButton
      Left = 472
      Top = 48
      Width = 96
      Height = 17
      Caption = 'Next'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 15
      OnClick = NextButtonClick
    end
    object ConnectButton: TButton
      Left = 368
      Top = 8
      Width = 96
      Height = 17
      Caption = 'Connect'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 7
      OnClick = ConnectButtonClick
    end
    object AuthInfoButton: TButton
      Left = 368
      Top = 28
      Width = 96
      Height = 17
      Caption = 'AuthInfo'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 8
      OnClick = AuthInfoButtonClick
    end
    object ArticleByIDButton: TButton
      Left = 368
      Top = 88
      Width = 96
      Height = 17
      Caption = 'Article'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 11
      OnClick = ArticleByIDButtonClick
    end
    object HeadByIDButton: TButton
      Left = 368
      Top = 108
      Width = 96
      Height = 17
      Caption = 'Head'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 12
      OnClick = HeadByIDButtonClick
    end
    object BodyByIDButton: TButton
      Left = 472
      Top = 8
      Width = 96
      Height = 17
      Caption = 'Body'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 13
      OnClick = BodyByIDButtonClick
    end
    object QuitButton: TButton
      Left = 472
      Top = 108
      Width = 96
      Height = 17
      Caption = 'Quit'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 14
      OnClick = QuitButtonClick
    end
    object UsernameEdit: TEdit
      Left = 80
      Top = 56
      Width = 121
      Height = 21
      TabOrder = 5
    end
    object PasswordEdit: TEdit
      Left = 240
      Top = 56
      Width = 121
      Height = 21
      TabOrder = 6
    end
    object ListButton: TButton
      Left = 368
      Top = 48
      Width = 96
      Height = 17
      Caption = 'List'
      TabOrder = 9
      OnClick = ListButtonClick
    end
    object GroupButton: TButton
      Left = 368
      Top = 68
      Width = 96
      Height = 17
      Caption = 'Group'
      TabOrder = 10
      OnClick = GroupButtonClick
    end
    object StatByIDButton: TButton
      Left = 472
      Top = 28
      Width = 96
      Height = 17
      Caption = 'Stat'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 16
      OnClick = StatByIDButtonClick
    end
    object Panel1: TPanel
      Left = 151
      Top = 131
      Width = 417
      Height = 46
      BevelOuter = bvNone
      BorderStyle = bsSingle
      Ctl3D = False
      ParentColor = True
      ParentCtl3D = False
      TabOrder = 17
      object Label8: TLabel
        Left = 5
        Top = 8
        Width = 397
        Height = 26
        Caption = 
          'Please add in your website a link to http://www.arkadia.com or s' +
          'end me an email to svanderclock@arkadia.com if you like this com' +
          'ponent!'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clTeal
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        WordWrap = True
      end
    end
    object ArticleIDEdit: TEdit
      Left = 80
      Top = 104
      Width = 281
      Height = 21
      ParentShowHint = False
      ShowHint = True
      TabOrder = 18
    end
    object ButtonLast: TButton
      Left = 471
      Top = 68
      Width = 96
      Height = 17
      Caption = 'Last'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 19
      OnClick = ButtonLastClick
    end
    object ButtonPost: TButton
      Left = 471
      Top = 88
      Width = 96
      Height = 17
      Caption = 'Post'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 20
      OnClick = ButtonPostClick
    end
  end
  object AttachPanel: TPanel
    Left = 0
    Top = 311
    Width = 584
    Height = 17
    Align = alTop
    TabOrder = 4
    ExplicitWidth = 575
    object Label6: TLabel
      Left = 8
      Top = 2
      Width = 67
      Height = 13
      Caption = 'Attached files:'
    end
  end
  object FileAttachMemo: TMemo
    Left = 0
    Top = 328
    Width = 584
    Height = 49
    Hint = 'Enter the attached file path, one per line'
    Align = alTop
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ParentShowHint = False
    ScrollBars = ssVertical
    ShowHint = True
    TabOrder = 1
    ExplicitWidth = 575
  end
  object InfoPanel: TPanel
    Left = 0
    Top = 377
    Width = 584
    Height = 17
    Align = alTop
    TabOrder = 5
    ExplicitWidth = 575
    object Label7: TLabel
      Left = 8
      Top = 2
      Width = 71
      Height = 13
      Caption = 'Info messages:'
    end
  end
end
