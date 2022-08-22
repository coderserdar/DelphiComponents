inherited DBGridForm: TDBGridForm
  Caption = 'List o indexes'
  PixelsPerInch = 96
  TextHeight = 13
  inherited ToolBar: TToolBar
    TabOrder = 1
  end
  object DBGrid: TDBGrid [1]
    Left = 0
    Top = 24
    Width = 384
    Height = 238
    Align = alClient
    DataSource = DataSource
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
  end
end
