object FingerDemoForm: TFingerDemoForm
  Left = 154
  Top = 120
  Caption = 'Finger Demo - http://www.overbyte.be'
  ClientHeight = 318
  ClientWidth = 521
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object DisplayMemo: TMemo
    Left = 0
    Top = 41
    Width = 521
    Height = 277
    Align = alClient
    Lines.Strings = (
      'DisplayMemo')
    ScrollBars = ssBoth
    TabOrder = 1
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 521
    Height = 41
    Align = alTop
    TabOrder = 0
    object QueryEdit: TEdit
      Left = 17
      Top = 8
      Width = 121
      Height = 21
      TabOrder = 0
      Text = 'user@mimosa'
    end
    object QueryButton: TButton
      Left = 151
      Top = 8
      Width = 75
      Height = 21
      Caption = '&Query'
      Default = True
      TabOrder = 1
      OnClick = QueryButtonClick
    end
    object CancelButton: TButton
      Left = 232
      Top = 8
      Width = 75
      Height = 21
      Cancel = True
      Caption = '&Cancel'
      Enabled = False
      TabOrder = 2
      OnClick = CancelButtonClick
    end
  end
  object FingerCli1: TFingerCli
    OnSessionConnected = FingerCli1SessionConnected
    OnDataAvailable = FingerCli1DataAvailable
    OnQueryDone = FingerCli1QueryDone
    Left = 32
    Top = 104
  end
end
