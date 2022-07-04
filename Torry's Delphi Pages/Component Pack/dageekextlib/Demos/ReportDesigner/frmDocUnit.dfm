object frmDoc: TfrmDoc
  Left = 239
  Top = 213
  Width = 502
  Height = 321
  Caption = 'frmDoc'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsMDIChild
  Icon.Data = {
    0000010001001010100000000000280100001600000028000000100000002000
    00000100040000000000C0000000000000000000000000000000000000000000
    0000000080000080000000808000800000008000800080800000C0C0C0008080
    80000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF000000
    000000000000008FFFFFFFFF7000008F0777770F7000008F7FFF7F7F7000008F
    7777777F7000008F7FFF7F7F7000008F7777777F7000008F7FFF7F7F7000008F
    0777770F7000008FFFFFFFFF7000008F0000F77F7000008F0000FFFF7000008F
    FFFFF7888000008FFFFFFF8F800000888888888800000000000000000000C003
    0000C0030000C0030000C0030000C0030000C0030000C0030000C0030000C003
    0000C0030000C0030000C0030000C0030000C0070000C00F0000FFFF0000}
  OldCreateOrder = False
  Position = poDefault
  Scaled = False
  Visible = True
  OnActivate = FormActivate
  OnClose = FormClose
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object ELDesignPanel1: TELDesignPanel
    Left = 0
    Top = 0
    Width = 494
    Height = 294
    AutoScroll = True
    Align = alClient
    TabOrder = 0
    TabStop = True
  end
  object ELDesigner1: TELDesigner
    DesignPanel = ELDesignPanel1
    ShowingHints = [htControl]
    PopupMenu = frmMain.PopupMenu1
    ClipboardFormat = 'Extension Library designer components'
    OnModified = ELDesigner1Modified
    OnControlInserting = ELDesigner1ControlInserting
    OnControlInserted = ELDesigner1ControlInserted
    OnChangeSelection = ELDesigner1ChangeSelection
    OnControlHint = ELDesigner1ControlHint
    OnKeyDown = ELDesigner1KeyDown
    OnKeyPress = ELDesigner1KeyPress
    OnDblClick = ELDesigner1DblClick
    Left = 24
    Top = 24
  end
end
