object SelectAudio: TSelectAudio
  Left = 0
  Top = 0
  Width = 176
  Height = 148
  TabOrder = 0
  object Panel12: TPanel
    Left = 0
    Top = 0
    Width = 176
    Height = 148
    Align = alClient
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 0
    object Label1: TLabel
      Left = 8
      Top = 9
      Width = 67
      Height = 13
      Caption = 'Audio devices'
    end
    object lbWaveDevices: TListBox
      Left = 5
      Top = 24
      Width = 157
      Height = 86
      ItemHeight = 13
      TabOrder = 0
    end
    object cbWaveMapper: TCheckBox
      Left = 8
      Top = 120
      Width = 97
      Height = 17
      Caption = 'Wave Mapper'
      TabOrder = 1
      OnClick = cbWaveMapperClick
    end
  end
end
