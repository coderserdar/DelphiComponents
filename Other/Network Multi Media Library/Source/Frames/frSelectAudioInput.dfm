inherited SelectAudioInput: TSelectAudioInput
  Width = 266
  inherited Panel12: TPanel
    Width = 266
    object Label2: TLabel [1]
      Left = 175
      Top = 9
      Width = 71
      Height = 13
      Caption = 'Bits per sample'
    end
    object Label3: TLabel [2]
      Left = 175
      Top = 48
      Width = 82
      Height = 26
      Caption = 'Frequency (samples per sec)'
      WordWrap = True
    end
    object Label4: TLabel [3]
      Left = 175
      Top = 100
      Width = 44
      Height = 13
      Caption = 'Channels'
    end
    inherited lbWaveDevices: TListBox
      Width = 156
    end
    object cbBits: TComboBox
      Left = 174
      Top = 22
      Width = 80
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 2
    end
    object cbFrequency: TComboBox
      Left = 174
      Top = 75
      Width = 80
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 3
    end
    object cbChannels: TComboBox
      Left = 174
      Top = 115
      Width = 80
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 4
    end
  end
end
