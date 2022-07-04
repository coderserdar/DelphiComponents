object frmAudioSettings: TfrmAudioSettings
  Left = 281
  Top = 213
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Settings'
  ClientHeight = 360
  ClientWidth = 310
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl: TPageControl
    Left = 0
    Top = 0
    Width = 310
    Height = 360
    ActivePage = tsSettings
    Align = alClient
    TabOrder = 0
    object tsSettings: TTabSheet
      Caption = 'Settings'
      object gbCodec: TGroupBox
        Left = 8
        Top = 165
        Width = 287
        Height = 124
        Caption = ' Codec '
        TabOrder = 0
        object Label3: TLabel
          Left = 11
          Top = 18
          Width = 87
          Height = 13
          Caption = 'Current codec info'
        end
        object memCodecInfo: TMemo
          Left = 8
          Top = 34
          Width = 141
          Height = 72
          ReadOnly = True
          ScrollBars = ssVertical
          TabOrder = 0
        end
        object bChooseFormat: TButton
          Left = 158
          Top = 81
          Width = 98
          Height = 25
          Caption = 'Choose Codec'
          TabOrder = 1
          OnClick = bChooseFormatClick
        end
        object cbSameBits: TCheckBox
          Left = 159
          Top = 32
          Width = 125
          Height = 17
          Caption = 'Same bits per sample'
          Checked = True
          State = cbChecked
          TabOrder = 2
        end
        object cbSameFrequency: TCheckBox
          Left = 159
          Top = 56
          Width = 97
          Height = 17
          Caption = 'Same frequency'
          Checked = True
          State = cbChecked
          TabOrder = 3
        end
      end
      object BitBtn1: TBitBtn
        Left = 54
        Top = 300
        Width = 75
        Height = 25
        TabOrder = 2
        OnClick = BitBtn1Click
        Kind = bkOK
      end
      object BitBtn2: TBitBtn
        Left = 166
        Top = 300
        Width = 75
        Height = 25
        TabOrder = 1
        Kind = bkCancel
      end
      inline SelectAudioDevice: TSelectAudioInput
        Left = 10
        Top = 12
        Width = 284
        Height = 148
        TabOrder = 3
        inherited Panel12: TPanel
          Width = 284
        end
      end
    end
    object tsTests: TTabSheet
      Caption = 'Tests'
      ImageIndex = 1
      object gbTestAudio: TGroupBox
        Left = 24
        Top = 18
        Width = 253
        Height = 55
        Caption = 'Test Audio '
        TabOrder = 0
        object bStartRec: TButton
          Left = 12
          Top = 19
          Width = 67
          Height = 25
          Caption = 'Start Rec'
          TabOrder = 0
          OnClick = bStartRecClick
        end
        object bStopRec: TButton
          Left = 91
          Top = 19
          Width = 66
          Height = 25
          Caption = 'Finish Rec'
          Enabled = False
          TabOrder = 1
          OnClick = bStopRecClick
        end
        object bStartPlay: TButton
          Left = 171
          Top = 19
          Width = 67
          Height = 25
          Caption = 'Play'
          TabOrder = 2
          OnClick = bStartPlayClick
        end
      end
      object cbCodec: TGroupBox
        Left = 23
        Top = 79
        Width = 254
        Height = 240
        Caption = ' Test Codec '
        TabOrder = 1
        object Label2: TLabel
          Left = 12
          Top = 16
          Width = 86
          Height = 13
          Caption = 'Initial delay (msec)'
        end
        object Label1: TLabel
          Left = 11
          Top = 70
          Width = 54
          Height = 13
          Caption = 'Test results'
        end
        object bStartTest: TButton
          Left = 89
          Top = 35
          Width = 69
          Height = 25
          Caption = 'Start Test'
          TabOrder = 0
          OnClick = bStartTestClick
        end
        object bFinishTest: TButton
          Left = 169
          Top = 35
          Width = 67
          Height = 25
          Caption = 'Finish Test'
          Enabled = False
          TabOrder = 1
          OnClick = bFinishTestClick
        end
        object edIniDelay: TSpinEdit
          Left = 13
          Top = 35
          Width = 69
          Height = 22
          MaxValue = 0
          MinValue = 0
          TabOrder = 2
          Value = 20
        end
        object memResults: TMemo
          Left = 11
          Top = 86
          Width = 228
          Height = 139
          ReadOnly = True
          ScrollBars = ssVertical
          TabOrder = 3
        end
      end
    end
  end
  object MainMenu: TMainMenu
    Left = 248
    Top = 8
    object mnuOptions: TMenuItem
      Caption = 'Options'
      object mnuViewAllCodecs: TMenuItem
        Caption = 'View All Audio Codecs'
        OnClick = mnuViewAllCodecsClick
      end
    end
  end
end
