object bvFindDialogForm: TbvFindDialogForm
  Left = 113
  Top = 106
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'FindDialogForm'
  ClientHeight = 105
  ClientWidth = 520
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  KeyPreview = True
  OldCreateOrder = False
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyPress = FormKeyPress
  OnKeyUp = FormKeyUp
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 0
    Top = 0
    Width = 520
    Height = 105
    Align = alClient
  end
  object LabTextCap: TLabel
    Left = 24
    Top = 16
    Width = 58
    Height = 13
    Caption = 'LabTextCap'
  end
  object EditText: TEdit
    Left = 88
    Top = 16
    Width = 305
    Height = 21
    Cursor = crIBeam
    TabOrder = 0
    OnChange = EditTextChange
  end
  object BitBtnFind: TBitBtn
    Left = 408
    Top = 24
    Width = 100
    Height = 33
    Cursor = crHandPoint
    Caption = 'BitBtnFind'
    Enabled = False
    TabOrder = 1
    OnClick = BitBtnFindClick
    Glyph.Data = {
      96010000424D9601000000000000760000002800000018000000180000000100
      0400000000002001000000000000000000001000000000000000000000000000
      8000008000000080800080000000800080008080000080808000C0C0C0000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
      3333333333333333333333333333333333333700000000000000000000033788
      8888888888888888880337880088888880078700080337800008888888088800
      88033780F000888888700007880337880F000888888080088803378880F00000
      0787007888033788880F0078870800888803378888800788F870888888033788
      888078888F877888880337888880788888F8088888033788888078FF88880888
      88033788888777FF888778888803378888880778887088888803378888888077
      7708888888033788888888700788888888033788888888888888888888033700
      0000000000000000000337F0CCCCCCCCCCCCCC0F0F0337777777777777777777
      7773333333333333333333333333333333333333333333333333}
  end
  object BitBtnClose: TBitBtn
    Left = 408
    Top = 64
    Width = 100
    Height = 25
    Cursor = crHandPoint
    Cancel = True
    Caption = 'BitBtnClose'
    TabOrder = 2
    OnClick = BitBtnCloseClick
    Glyph.Data = {
      E6000000424DE60000000000000076000000280000000E0000000E0000000100
      0400000000007000000000000000000000001000000000000000000000000000
      80000080000000808000800000008000800080800000C0C0C000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00CCCCCCCCCCCC
      CC00CCCCFFFFFFCCCC00CCCF000000FCCC00CCF00000000FCC00CF000000000F
      FF00C000000000FF0000C00000000FF00000C0000000FC000000C00000000FC0
      0000C000000000FC0000CC000000000CCC00CCC00000000CCC00CCCC000000CC
      CC00CCCCCCCCCCCCCC00}
  end
  object CheckWholeWordsOnly: TCheckBox
    Left = 88
    Top = 64
    Width = 153
    Height = 17
    Cursor = crHandPoint
    Caption = 'CheckWholeWordsOnly'
    TabOrder = 3
  end
  object CheckMatchCase: TCheckBox
    Left = 88
    Top = 80
    Width = 137
    Height = 17
    Cursor = crHandPoint
    Caption = 'CheckMatchCase'
    TabOrder = 4
  end
  object EditDirection: TRadioGroup
    Left = 256
    Top = 48
    Width = 137
    Height = 49
    Cursor = crHandPoint
    Caption = 'EditDirection'
    ItemIndex = 1
    Items.Strings = (
      'Up'
      'Down')
    TabOrder = 5
  end
  object CheckOneColumn: TCheckBox
    Left = 88
    Top = 48
    Width = 161
    Height = 17
    Cursor = crHandPoint
    Caption = 'CheckOneColumn'
    TabOrder = 6
  end
  object AnimateGlobe: TAnimate
    Left = 20
    Top = 36
    Width = 48
    Height = 50
    Active = False
    AutoSize = False
    StopFrame = 5
    Visible = False
  end
end
