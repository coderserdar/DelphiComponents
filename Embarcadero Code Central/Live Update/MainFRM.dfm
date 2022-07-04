object MainForm: TMainForm
  Left = 445
  Top = 258
  BorderStyle = bsDialog
  Caption = 'Net Update Demo'
  ClientHeight = 98
  ClientWidth = 306
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 120
  TextHeight = 16
  object btnUpdate: TSpeedButton
    Left = 104
    Top = 68
    Width = 100
    Height = 22
    Caption = 'Net Update'
    OnClick = btnUpdateClick
  end
  object Label1: TLabel
    Left = 0
    Top = 0
    Width = 306
    Height = 60
    Align = alTop
    Alignment = taCenter
    Caption = 
      'When you are connected to the Internet, click on the "Net Update' +
      '" button to download the latest program update'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNavy
    Font.Height = -17
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object NetUpdate: TNetUpdate
    TargetDir = AppDir
    SourceURL = 'http://lgman.freeyellow.com/components/netupdate/netupdatedemo'
    OnUpdateEnd = NetUpdateUpdateEnd
    Left = 256
    Top = 10
  end
end
