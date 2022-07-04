object AudioOutputParams: TAudioOutputParams
  Left = 0
  Top = 0
  Width = 230
  Height = 148
  TabOrder = 0
  object cbVolume: TGroupBox
    Left = 9
    Top = 7
    Width = 84
    Height = 134
    Caption = ' Volume '
    TabOrder = 0
    object Label1: TLabel
      Left = 10
      Top = 18
      Width = 18
      Height = 13
      Caption = 'Left'
    end
    object Label2: TLabel
      Left = 47
      Top = 18
      Width = 25
      Height = 13
      Caption = 'Right'
    end
    object tbRight: TTrackBar
      Left = 45
      Top = 32
      Width = 32
      Height = 81
      Max = 0
      Min = -11
      Orientation = trVertical
      TabOrder = 0
      OnChange = tbVolumeChange
    end
    object tbLeft: TTrackBar
      Left = 8
      Top = 32
      Width = 32
      Height = 81
      Max = 0
      Min = -11
      Orientation = trVertical
      TabOrder = 1
      OnChange = tbVolumeChange
    end
    object cbMono: TCheckBox
      Left = 12
      Top = 111
      Width = 60
      Height = 15
      Caption = 'Mono'
      TabOrder = 2
    end
  end
  object GroupBox3: TGroupBox
    Left = 103
    Top = 7
    Width = 117
    Height = 135
    TabOrder = 1
    object Label5: TLabel
      Left = 10
      Top = 18
      Width = 24
      Height = 13
      Caption = 'Pitch'
    end
    object Label6: TLabel
      Left = 54
      Top = 18
      Width = 50
      Height = 13
      Caption = 'Frequency'
    end
    object tbFrequency: TTrackBar
      Left = 52
      Top = 32
      Width = 32
      Height = 81
      Max = 0
      Min = -11
      Orientation = trVertical
      TabOrder = 0
      OnChange = tbFrequencyChange
    end
    object tbPitch: TTrackBar
      Left = 8
      Top = 32
      Width = 32
      Height = 81
      Max = 0
      Min = -11
      Orientation = trVertical
      TabOrder = 1
      OnChange = tbPitchChange
    end
  end
end
