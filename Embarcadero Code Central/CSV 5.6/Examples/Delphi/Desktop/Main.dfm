object FormMain: TFormMain
  Left = 259
  Top = 258
  Width = 460
  Height = 502
  BorderWidth = 8
  Caption = 'CSV'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Panel: TPanel
    Left = 0
    Top = 0
    Width = 428
    Height = 36
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      428
      36)
    object ButtonOpenFile: TButton
      Left = 160
      Top = 4
      Width = 108
      Height = 25
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Open file...'
      TabOrder = 0
      OnClick = ButtonOpenFileClick
    end
  end
  object Memo: TMemo
    Left = 0
    Top = 36
    Width = 428
    Height = 411
    Align = alClient
    BorderStyle = bsNone
    ScrollBars = ssBoth
    TabOrder = 1
  end
  object XPManifest: TXPManifest
  end
  object OpenDialog: TOpenDialog
    DefaultExt = 'csv'
    Filter = 
      'CSV Files (*.csv)|*.csv|CSV Files (*.txt)|*.txt|All Files (*.*)|' +
      '*.*'
    Left = 32
  end
end
