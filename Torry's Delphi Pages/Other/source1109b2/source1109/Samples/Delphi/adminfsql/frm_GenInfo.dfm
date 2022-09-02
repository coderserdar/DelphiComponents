object frmGenInfo: TfrmGenInfo
  Left = 205
  Top = 133
  Width = 486
  Height = 319
  Caption = 'FSSQL Remote Administration [General Configuration]'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pnlButtons: TPanel
    Left = 0
    Top = 235
    Width = 478
    Height = 50
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object btnOK: TButton
      Left = 132
      Top = 12
      Width = 75
      Height = 25
      Caption = 'OK'
      ModalResult = 1
      TabOrder = 0
      OnClick = btnOKClick
    end
    object btnCancel: TButton
      Left = 236
      Top = 12
      Width = 75
      Height = 25
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
  object PageCtrl: TPageControl
    Left = 0
    Top = 0
    Width = 478
    Height = 235
    ActivePage = tsGeneral
    Align = alClient
    Style = tsFlatButtons
    TabOrder = 1
    object tsGeneral: TTabSheet
      Caption = 'General Settings'
      object Label1: TLabel
        Left = 53
        Top = 52
        Width = 63
        Height = 13
        Caption = 'Server name:'
        FocusControl = edtServerName
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object Label2: TLabel
        Left = 53
        Top = 75
        Width = 110
        Height = 13
        Caption = 'Maximum RAM (in MB):'
        FocusControl = edtMaxRAM
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object Label3: TLabel
        Left = 53
        Top = 100
        Width = 127
        Height = 13
        Caption = 'Temporary storage (in MB):'
        FocusControl = edtTempSize
      end
      object Label4: TLabel
        Left = 53
        Top = 123
        Width = 67
        Height = 13
        Hint = 'Server Priority'
        Caption = 'Server priority:'
        FocusControl = cboPriority
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object edtServerName: TEdit
        Left = 190
        Top = 48
        Width = 121
        Height = 21
        Hint = 'The server name'
        Color = clBtnFace
        Enabled = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        MaxLength = 16
        ParentFont = False
        TabOrder = 0
      end
      object edtMaxRAM: TEdit
        Left = 190
        Top = 71
        Width = 121
        Height = 21
        Hint = 'Maximum number of RAM pages the server can use'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        MaxLength = 4
        ParentFont = False
        TabOrder = 1
        OnExit = Validate
      end
      object edtTempSize: TEdit
        Left = 190
        Top = 96
        Width = 121
        Height = 21
        TabOrder = 2
        OnExit = Validate
      end
      object cboPriority: TComboBox
        Left = 190
        Top = 119
        Width = 122
        Height = 21
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ItemHeight = 13
        ParentFont = False
        TabOrder = 3
        Text = 'cboPriority'
        Items.Strings = (
          'Lowest'
          'Below Normal'
          'Normal'
          'Above Normal'
          'Highest')
      end
    end
    object tsOperating: TTabSheet
      Caption = 'Operating Mode'
      ImageIndex = 1
      object cbEncrypt: TCheckBox
        Left = 108
        Top = 44
        Width = 231
        Height = 17
        Caption = 'Creation of &encrypted tables enabled'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
      end
      object cbReadOnly: TCheckBox
        Left = 108
        Top = 65
        Width = 249
        Height = 17
        Caption = '&Disable all server output'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
        OnClick = SetControls
      end
      object cbNoSaveCfg: TCheckBox
        Left = 108
        Top = 86
        Width = 249
        Height = 17
        Caption = 'Disable saving &configuration changes'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 2
      end
      object cbSecurity: TCheckBox
        Left = 108
        Top = 106
        Width = 249
        Height = 17
        Hint = 'Select if user logins required'
        Caption = '&Security enabled (force logins)'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 3
      end
      object cbDebugLog: TCheckBox
        Left = 108
        Top = 127
        Width = 249
        Height = 17
        Caption = 'Debug &logging enabled'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 4
      end
    end
    object tsStartup: TTabSheet
      Caption = 'Startup Mode'
      ImageIndex = 2
      object cbAutoStart: TCheckBox
        Left = 131
        Top = 63
        Width = 209
        Height = 17
        Hint = 'Select if the server is to be brought up on startup'
        Caption = 'Bring Server &up automatically'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
      end
      object cbMinimize: TCheckBox
        Left = 131
        Top = 80
        Width = 97
        Height = 17
        Hint = 'Select if the server is to be minimized on startup'
        Caption = 'Start minimi&zed '
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
      end
    end
    object tsGarbage: TTabSheet
      Caption = 'Garbage Collection'
      ImageIndex = 3
      object Label5: TLabel
        Left = 107
        Top = 88
        Width = 141
        Height = 13
        Caption = 'Collection frequency (millisec):'
        FocusControl = edtCollectFreq
      end
      object cbCollectEnabled: TCheckBox
        Left = 107
        Top = 66
        Width = 177
        Height = 17
        Caption = 'Ena&bled'
        Checked = True
        State = cbChecked
        TabOrder = 0
        OnClick = SetControls
      end
      object edtCollectFreq: TEdit
        Left = 267
        Top = 84
        Width = 60
        Height = 21
        TabOrder = 1
        OnExit = Validate
      end
    end
    object tsKeepAlive: TTabSheet
      Caption = 'Keep Alive'
      ImageIndex = 4
      object Label6: TLabel
        Left = 107
        Top = 65
        Width = 145
        Height = 13
        AutoSize = False
        Caption = 'Interval from last message:'
        FocusControl = edtLastMsg
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object Label7: TLabel
        Left = 107
        Top = 88
        Width = 145
        Height = 13
        AutoSize = False
        Caption = 'Interval between Keep Alives'
        FocusControl = edtKAInterval
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object Label8: TLabel
        Left = 107
        Top = 111
        Width = 145
        Height = 13
        AutoSize = False
        Caption = 'Keep Alive retries'
        FocusControl = edtKARetries
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object edtKARetries: TEdit
        Left = 267
        Top = 107
        Width = 60
        Height = 21
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 2
        OnExit = Validate
      end
      object edtKAInterval: TEdit
        Left = 267
        Top = 84
        Width = 60
        Height = 21
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
        OnExit = Validate
      end
      object edtLastMsg: TEdit
        Left = 267
        Top = 61
        Width = 60
        Height = 21
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        OnExit = Validate
      end
    end
  end
end
