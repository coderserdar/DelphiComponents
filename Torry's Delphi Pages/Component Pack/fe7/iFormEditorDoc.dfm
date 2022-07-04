object FormEditorDoc: TFormEditorDoc
  Left = 113
  Top = 113
  Width = 458
  Height = 345
  Caption = 'Document properties'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 450
    Height = 277
    ActivePage = TabSheet2
    Align = alClient
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'Info'
      object L: TLabel
        Left = 16
        Top = 16
        Width = 28
        Height = 13
        Caption = 'Name'
      end
      object Label1: TLabel
        Left = 16
        Top = 40
        Width = 43
        Height = 13
        Caption = 'Created :'
      end
      object Label2: TLabel
        Left = 16
        Top = 64
        Width = 68
        Height = 13
        Caption = 'Last updated :'
      end
      object Label3: TLabel
        Left = 16
        Top = 88
        Width = 60
        Height = 13
        Caption = 'Author name'
      end
      object Label4: TLabel
        Left = 16
        Top = 112
        Width = 77
        Height = 13
        Caption = 'Author company'
      end
      object Label5: TLabel
        Left = 16
        Top = 136
        Width = 42
        Height = 13
        Caption = 'Remarks'
      end
      object E_NAME: TEdit
        Left = 104
        Top = 16
        Width = 337
        Height = 21
        TabOrder = 0
        Text = 'E_NAME'
      end
      object E_CREATED: TEdit
        Left = 104
        Top = 40
        Width = 121
        Height = 21
        ReadOnly = True
        TabOrder = 1
        Text = 'E_CREATED'
      end
      object E_UPDATED: TEdit
        Left = 104
        Top = 64
        Width = 121
        Height = 21
        ReadOnly = True
        TabOrder = 2
        Text = 'E_UPDATED'
      end
      object E_AUTHOR: TEdit
        Left = 104
        Top = 88
        Width = 337
        Height = 21
        TabOrder = 3
        Text = 'E_AUTHOR'
      end
      object E_COMPANY: TEdit
        Left = 104
        Top = 112
        Width = 337
        Height = 21
        TabOrder = 4
        Text = 'E_COMPANY'
      end
      object E_REM: TMemo
        Left = 104
        Top = 136
        Width = 337
        Height = 113
        Lines.Strings = (
          'E_REM')
        TabOrder = 5
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Units && Size'
      ImageIndex = 1
      object Label6: TLabel
        Left = 120
        Top = 32
        Width = 77
        Height = 13
        Caption = 'Document width'
      end
      object Label7: TLabel
        Left = 120
        Top = 56
        Width = 81
        Height = 13
        Caption = 'Document height'
      end
      object E_UNITS: TRadioGroup
        Left = 8
        Top = 8
        Width = 89
        Height = 105
        Caption = 'Units'
        Items.Strings = (
          'Pixels'
          'MM'
          'CM'
          'Inch')
        TabOrder = 0
      end
      object E_WIDTH: TEdit
        Left = 224
        Top = 24
        Width = 57
        Height = 21
        TabOrder = 1
      end
      object E_HEIGHT: TEdit
        Left = 224
        Top = 48
        Width = 57
        Height = 21
        TabOrder = 2
      end
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 277
    Width = 450
    Height = 41
    Align = alBottom
    TabOrder = 1
    object SpeedButton2: TSpeedButton
      Left = 8
      Top = 11
      Width = 89
      Height = 22
      Caption = '&OK'
      Glyph.Data = {
        42010000424D4201000000000000760000002800000011000000110000000100
        040000000000CC00000000000000000000001000000010000000000000000000
        BF0000BF000000BFBF00BF000000BF00BF00BFBF0000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777777
        7777700000007777777777777777700000007777777774F77777700000007777
        7777444F77777000000077777774444F777770000000700000444F44F7777000
        000070FFF444F0744F777000000070F8884FF0774F777000000070FFFFFFF077
        74F77000000070F88888F077774F7000000070FFFFFFF0777774F000000070F8
        8777F07777774000000070FFFF00007777777000000070F88707077777777000
        000070FFFF007777777770000000700000077777777770000000777777777777
        777770000000}
      OnClick = SpeedButton2Click
    end
    object SpeedButton1: TSpeedButton
      Left = 104
      Top = 11
      Width = 97
      Height = 22
      Caption = '&Cancel'
      Glyph.Data = {
        42010000424D4201000000000000760000002800000011000000110000000100
        040000000000CC00000000000000000000001000000010000000000000000000
        BF0000BF000000BFBF00BF000000BF00BF00BFBF0000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777777
        777770000000777777777777777770000000777777777777770F700000007777
        0F777777777770000000777000F7777770F770000000777000F777770F777000
        00007777000F77700F777000000077777000F700F7777000000077777700000F
        7777700000007777777000F777777000000077777700000F7777700000007777
        7000F70F7777700000007770000F77700F7770000000770000F7777700F77000
        00007700F7777777700F70000000777777777777777770000000777777777777
        777770000000}
      OnClick = SpeedButton1Click
    end
  end
end
