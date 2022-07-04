object frmFileProperties: TfrmFileProperties
  Left = 192
  Top = 107
  ActiveControl = edtDepotPath
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'File Information'
  ClientHeight = 336
  ClientWidth = 397
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    397
    336)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 14
    Top = 17
    Width = 57
    Height = 13
    Caption = 'Depot Path:'
  end
  object Label2: TLabel
    Left = 14
    Top = 45
    Width = 54
    Height = 13
    Caption = 'Client Path:'
  end
  object Label3: TLabel
    Left = 14
    Top = 84
    Width = 46
    Height = 13
    Caption = 'File Type:'
  end
  object Label4: TLabel
    Left = 14
    Top = 113
    Width = 73
    Height = 13
    Caption = 'Head Revision:'
  end
  object Label5: TLabel
    Left = 14
    Top = 142
    Width = 62
    Height = 13
    Caption = 'Head Action:'
  end
  object Label6: TLabel
    Left = 14
    Top = 172
    Width = 73
    Height = 13
    Caption = 'Last Mod Time:'
  end
  object Label7: TLabel
    Left = 14
    Top = 205
    Width = 56
    Height = 13
    Caption = 'Opened By:'
  end
  object Label8: TLabel
    Left = 14
    Top = 279
    Width = 54
    Height = 13
    Caption = 'Locked By:'
  end
  object Label9: TLabel
    Left = 226
    Top = 113
    Width = 73
    Height = 13
    Caption = 'Have Revision:'
  end
  object Label10: TLabel
    Left = 225
    Top = 142
    Width = 69
    Height = 13
    Caption = 'Head Change:'
  end
  object edtDepotPath: TEdit
    Left = 110
    Top = 13
    Width = 280
    Height = 21
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 0
  end
  object edtClientPath: TEdit
    Left = 110
    Top = 41
    Width = 280
    Height = 21
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 1
  end
  object edtFileType: TEdit
    Left = 110
    Top = 80
    Width = 80
    Height = 21
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 2
  end
  object edtHeadRev: TEdit
    Left = 110
    Top = 109
    Width = 80
    Height = 21
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 3
  end
  object edtHeadAction: TEdit
    Left = 110
    Top = 138
    Width = 80
    Height = 21
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 5
  end
  object edtHaveRev: TEdit
    Left = 310
    Top = 109
    Width = 80
    Height = 21
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 4
  end
  object edtHeadChange: TEdit
    Left = 310
    Top = 138
    Width = 80
    Height = 21
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 6
  end
  object edtLastModTime: TEdit
    Left = 110
    Top = 168
    Width = 280
    Height = 21
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 7
  end
  object edtLockedBy: TEdit
    Left = 110
    Top = 275
    Width = 280
    Height = 21
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 9
  end
  object btnOK: TButton
    Left = 318
    Top = 308
    Width = 75
    Height = 25
    Anchors = [akTop, akRight, akBottom]
    Cancel = True
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 10
  end
  object lbxOpenedBy: TListBox
    Left = 111
    Top = 203
    Width = 280
    Height = 57
    Color = clBtnFace
    ItemHeight = 13
    TabOrder = 8
  end
end
