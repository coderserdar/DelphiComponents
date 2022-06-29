object Form1: TForm1
  Left = 291
  Top = 254
  Caption = 'Form1'
  ClientHeight = 92
  ClientWidth = 290
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -10
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 12
  object BtnGzip: TButton
    Left = 12
    Top = 12
    Width = 56
    Height = 19
    Caption = 'GZip File...'
    TabOrder = 0
    OnClick = BtnGzipClick
  end
  object OpenDialog: TOpenDialog
    Filter = '*.*'
    Left = 128
    Top = 16
  end
end
