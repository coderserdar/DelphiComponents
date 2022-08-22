inherited BlobDataForm: TBlobDataForm
  Width = 349
  Height = 242
  Caption = 'BlobDataForm'
  Font.Height = -12
  Icon.Data = {
    0000010001001010100000000000280100001600000028000000100000002000
    00000100040000000000C0000000000000000000000000000000000000000000
    0000000080000080000000808000800000008000800080800000C0C0C0008080
    80000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF000000
    0000000000000000000000000000000000000000000088888888888888008777
    778FFFFFF8008788888F8888F8008777778FFFFFF8008788888F8888F8008777
    778FFFFFF8008788888F8888F8008777778FFFFFF8008788888888888800874C
    4C8F4F0F08008777778FFFFFF80088888888888888800000000000000000FFFF
    0000FFFF00000001000000010000000100000001000000010000000100000001
    0000000100000001000000010000000100000001000000010000FFFF0000}
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 14
  object BlobPanel: TPanel
    Left = 0
    Top = 0
    Width = 341
    Height = 176
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 6
    TabOrder = 0
  end
  object BottomPanel: TPanel
    Left = 0
    Top = 176
    Width = 341
    Height = 32
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    OnResize = BottomPanelResize
    object CloseButton: TButton
      Left = 261
      Top = 1
      Width = 75
      Height = 25
      Cancel = True
      Caption = '&Close'
      Default = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Tahoma'
      Font.Style = []
      ModalResult = 1
      ParentFont = False
      TabOrder = 0
    end
  end
end
