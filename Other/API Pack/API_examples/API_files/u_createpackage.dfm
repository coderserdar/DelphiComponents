object f_createpackage: Tf_createpackage
  Left = 195
  Top = 114
  BorderStyle = bsDialog
  Caption = 'Create Package'
  ClientHeight = 96
  ClientWidth = 402
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poDefault
  OnActivate = FormActivate
  OnCreate = FormCreate
  DesignSize = (
    402
    96)
  PixelsPerInch = 96
  TextHeight = 13
  object Label3: TLabel
    Left = 8
    Top = 8
    Width = 32
    Height = 13
    Caption = 'Folder:'
  end
  object Label1: TLabel
    Left = 8
    Top = 35
    Width = 19
    Height = 13
    Caption = 'File:'
  end
  object Button5: TButton
    Left = 314
    Top = 8
    Width = 80
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Browse'
    TabOrder = 2
    OnClick = Button5Click
  end
  object Button4: TButton
    Left = 8
    Top = 63
    Width = 300
    Height = 25
    Anchors = [akLeft, akRight, akBottom]
    Caption = 'Compress'
    TabOrder = 0
    OnClick = Button4Click
  end
  object Edit2: TEdit
    Left = 48
    Top = 8
    Width = 260
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
  end
  object Button2: TButton
    Left = 314
    Top = 63
    Width = 81
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Close'
    TabOrder = 3
    OnClick = Button2Click
  end
  object Edit1: TEdit
    Left = 48
    Top = 35
    Width = 346
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 4
  end
  object API_files1: TAPI_files
    ShowProgress = True
    Confirmation = False
    OnFindFileReady = API_files1FindFileReady
    Left = 136
    Top = 16
  end
  object SaveDialog1: TSaveDialog
    Left = 88
    Top = 16
  end
  object Zip1: TZip
    ShowProgressDialog = True
    Left = 240
    Top = 16
  end
end
