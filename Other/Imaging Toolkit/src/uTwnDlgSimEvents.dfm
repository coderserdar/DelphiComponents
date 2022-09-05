object FormEvents: TFormEvents
  Left = 200
  Top = 108
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Simulate Device Events'
  ClientHeight = 153
  ClientWidth = 361
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object gbEvents: TGroupBox
    Left = 8
    Top = 8
    Width = 345
    Height = 105
    Caption = 'Device Events'
    TabOrder = 0
    object lEvents: TLabel
      Left = 24
      Top = 64
      Width = 267
      Height = 26
      Caption = 
        'NOTE: Only the "Device Events" enabled by the calling applicatio' +
        'n can be fired back to the application.'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      WordWrap = True
    end
    object cbEvents: TComboBox
      Left = 24
      Top = 26
      Width = 217
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      Items.Strings = (
        'TWDE_CHECKAUTOMATICCAPTURE'
        'TWDE_CHECKBATTERY'
        'TWDE_CHECKDEVICEONLINE'
        'TWDE_CHECKFLASH'
        'TWDE_CHECKPOWERSUPPLY'
        'TWDE_CHECKRESOLUTION'
        'TWDE_DEVICEADDED'
        'TWDE_DEVICEOFFLINE'
        'TWDE_DEVICEREADY'
        'TWDE_DEVICEREMOVED'
        'TWDE_IMAGECAPTURED'
        'TWDE_IMAGEDELETED'
        'TWDE_PAPERDOUBLEFEED'
        'TWDE_PAPERJAM'
        'TWDE_LAMPFAILURE'
        'TWDE_POWERSAVE'
        'TWDE_POWERSAVENOTIFY')
      TabOrder = 0
      OnChange = cbEventsChange
    end
    object btnSend: TButton
      Left = 256
      Top = 24
      Width = 75
      Height = 25
      Caption = '&Send'
      ModalResult = 1
      TabOrder = 1
      OnClick = btnSendClick
    end
  end
  object btnClose: TButton
    Left = 8
    Top = 120
    Width = 75
    Height = 25
    Caption = '&Close'
    ModalResult = 2
    TabOrder = 1
  end
end
