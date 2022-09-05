object FormCompressOption: TFormCompressOption
  Left = 403
  Top = 221
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Format Options'
  ClientHeight = 102
  ClientWidth = 259
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl: TPageControl
    Left = 0
    Top = 0
    Width = 169
    Height = 102
    ActivePage = tsJPEG
    Align = alClient
    TabOrder = 0
    object tsGIF: TTabSheet
      Caption = 'tsGIF'
      TabVisible = False
      object rgGIF: TRadioGroup
        Left = 0
        Top = 0
        Width = 161
        Height = 92
        Align = alClient
        Caption = 'GIF'
        Items.Strings = (
          'Interlaced'
          'Noninterlaced')
        TabOrder = 0
        OnClick = rgGIFClick
      end
    end
    object tsJPEG: TTabSheet
      Caption = 'tsJPEG'
      TabVisible = False
      object gbJPEG: TGroupBox
        Left = 0
        Top = 0
        Width = 161
        Height = 92
        Align = alClient
        Caption = 'JPEG'
        TabOrder = 0
        object lJPEGQuality: TLabel
          Left = 16
          Top = 40
          Width = 35
          Height = 13
          Caption = 'Quality:'
        end
        object seQuality: TmcmIntSpin
          Left = 69
          Top = 35
          Width = 57
          Height = 22
          TabOrder = 0
          OnChange = seQuality2Change
          Value = 100
          MaxValue = 100
          MinValue = 1
        end
      end
    end
    object tsPNG: TTabSheet
      Caption = 'tsPNG'
      TabVisible = False
      object gbPNG: TGroupBox
        Left = 0
        Top = 0
        Width = 161
        Height = 92
        Align = alClient
        Caption = 'PNG'
        TabOrder = 0
        object lPNGQuality: TLabel
          Left = 16
          Top = 40
          Width = 49
          Height = 13
          Caption = 'Compress:'
        end
        object cbPNGQuality: TComboBox
          Left = 72
          Top = 36
          Width = 73
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          Items.Strings = (
            'Low'
            'Medium'
            'High'
            'Maximum')
          TabOrder = 0
          OnChange = cbPNGQualityChange
        end
      end
    end
  end
  object Panel: TPanel
    Left = 169
    Top = 0
    Width = 90
    Height = 102
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 1
    object btnOK: TButton
      Left = 8
      Top = 8
      Width = 75
      Height = 25
      Caption = '&OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
    object btnCancel: TButton
      Left = 8
      Top = 40
      Width = 75
      Height = 25
      Cancel = True
      Caption = '&Cancel'
      ModalResult = 2
      TabOrder = 1
    end
    object btnHelp: TButton
      Left = 8
      Top = 72
      Width = 75
      Height = 25
      Caption = '&Help'
      TabOrder = 2
      OnClick = btnHelpClick
    end
  end
end
