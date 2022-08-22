object DBPropertyForm: TDBPropertyForm
  Left = 271
  Top = 133
  BorderStyle = bsDialog
  Caption = 'Database Property'
  ClientHeight = 315
  ClientWidth = 289
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 14
  object PageControl: TPageControl
    Left = 5
    Top = 4
    Width = 279
    Height = 277
    ActivePage = MainTabSheet
    HotTrack = True
    TabOrder = 1
    TabStop = False
    object MainTabSheet: TTabSheet
      Caption = 'Property'
      object GroupBox1: TGroupBox
        Left = 7
        Top = 2
        Width = 256
        Height = 239
        TabOrder = 0
        object TypeLabel: TLabel
          Left = 13
          Top = 17
          Width = 82
          Height = 14
          Caption = 'Database Type'
        end
        object DateLabel: TLabel
          Left = 13
          Top = 60
          Width = 64
          Height = 14
          Caption = 'Modify date'
        end
        object SizeLabel: TLabel
          Left = 13
          Top = 82
          Width = 40
          Height = 14
          Caption = 'File size'
        end
        object TableCountLabel: TLabel
          Left = 13
          Top = 104
          Width = 66
          Height = 14
          Caption = 'Table count'
        end
        object EncryptLabel: TLabel
          Left = 13
          Top = 126
          Width = 56
          Height = 14
          Caption = 'Encrypted'
        end
        object EncAlgoLabel: TLabel
          Left = 13
          Top = 147
          Width = 74
          Height = 14
          Caption = 'Encrypt Algo.'
        end
        object CompressLabel: TLabel
          Left = 13
          Top = 169
          Width = 66
          Height = 14
          Caption = 'Compressed'
        end
        object CompLevelLabel: TLabel
          Left = 13
          Top = 211
          Width = 80
          Height = 14
          Caption = 'Compress level'
        end
        object FileNameLabel: TLabel
          Left = 13
          Top = 39
          Width = 51
          Height = 14
          Caption = 'File name'
        end
        object CompAlgoLabel: TLabel
          Left = 13
          Top = 190
          Width = 84
          Height = 14
          Caption = 'Compress Algo.'
        end
        object TypeEdit: TEdit
          Left = 113
          Top = 17
          Width = 121
          Height = 15
          TabStop = False
          BorderStyle = bsNone
          Color = clBtnFace
          ReadOnly = True
          TabOrder = 0
          Text = '-'
        end
        object DateEdit: TEdit
          Left = 113
          Top = 60
          Width = 121
          Height = 15
          TabStop = False
          BorderStyle = bsNone
          Color = clBtnFace
          ReadOnly = True
          TabOrder = 2
          Text = '-'
        end
        object SizeEdit: TEdit
          Left = 113
          Top = 82
          Width = 121
          Height = 15
          TabStop = False
          BorderStyle = bsNone
          Color = clBtnFace
          ReadOnly = True
          TabOrder = 3
          Text = '-'
        end
        object TableCountEdit: TEdit
          Left = 113
          Top = 103
          Width = 121
          Height = 15
          TabStop = False
          BorderStyle = bsNone
          Color = clBtnFace
          ReadOnly = True
          TabOrder = 4
          Text = '-'
        end
        object EncryptEdit: TEdit
          Left = 113
          Top = 125
          Width = 121
          Height = 15
          TabStop = False
          BorderStyle = bsNone
          Color = clBtnFace
          ReadOnly = True
          TabOrder = 5
          Text = '-'
        end
        object EncAlgEdit: TEdit
          Left = 113
          Top = 147
          Width = 121
          Height = 15
          TabStop = False
          BorderStyle = bsNone
          Color = clBtnFace
          ReadOnly = True
          TabOrder = 6
          Text = '-'
        end
        object CompressEdit: TEdit
          Left = 113
          Top = 168
          Width = 121
          Height = 15
          TabStop = False
          BorderStyle = bsNone
          Color = clBtnFace
          ReadOnly = True
          TabOrder = 7
          Text = '-'
        end
        object CompLevelEdit: TEdit
          Left = 113
          Top = 210
          Width = 121
          Height = 15
          TabStop = False
          BorderStyle = bsNone
          Color = clBtnFace
          ReadOnly = True
          TabOrder = 9
          Text = '-'
        end
        object FileNameEdit: TEdit
          Left = 113
          Top = 38
          Width = 121
          Height = 15
          TabStop = False
          BorderStyle = bsNone
          Color = clBtnFace
          ReadOnly = True
          TabOrder = 1
          Text = '-'
        end
        object CompAlgoEdit: TEdit
          Left = 113
          Top = 189
          Width = 121
          Height = 15
          TabStop = False
          BorderStyle = bsNone
          Color = clBtnFace
          ReadOnly = True
          TabOrder = 8
          Text = '-'
        end
      end
    end
  end
  object CloseButton: TButton
    Left = 209
    Top = 287
    Width = 75
    Height = 23
    Cancel = True
    Caption = '&Close'
    TabOrder = 0
    OnClick = CloseButtonClick
  end
end
