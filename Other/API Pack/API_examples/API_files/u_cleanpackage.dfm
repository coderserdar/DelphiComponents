object f_cleanpackage: Tf_cleanpackage
  Left = 192
  Top = 107
  Caption = 'Clean Package'
  ClientHeight = 232
  ClientWidth = 622
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  OnActivate = FormActivate
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    622
    232)
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
    Top = 206
    Width = 6
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = '1'
    ExplicitTop = 192
  end
  object Button5: TButton
    Left = 540
    Top = 8
    Width = 74
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Browse'
    TabOrder = 4
    OnClick = Button5Click
    ExplicitLeft = 418
  end
  object Button1: TButton
    Left = 8
    Top = 73
    Width = 606
    Height = 25
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Start'
    TabOrder = 0
    OnClick = Button1Click
  end
  object ListBox1: TListBox
    Left = 8
    Top = 104
    Width = 606
    Height = 91
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 1
  end
  object Button4: TButton
    Left = 72
    Top = 199
    Width = 455
    Height = 25
    Anchors = [akLeft, akRight, akBottom]
    Caption = 'Delete'
    TabOrder = 2
    OnClick = Button4Click
    ExplicitTop = 185
    ExplicitWidth = 333
  end
  object Edit2: TEdit
    Left = 46
    Top = 8
    Width = 488
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 3
    ExplicitWidth = 366
  end
  object Button2: TButton
    Left = 533
    Top = 199
    Width = 81
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Close'
    TabOrder = 5
    OnClick = Button2Click
    ExplicitLeft = 411
    ExplicitTop = 185
  end
  object CheckBox1: TCheckBox
    Left = 46
    Top = 35
    Width = 231
    Height = 17
    Caption = 'Do not remove Executable files (*.EXE)'
    TabOrder = 6
  end
  object CheckBox2: TCheckBox
    Left = 46
    Top = 50
    Width = 231
    Height = 17
    Caption = 'Do not remove project files (*.dproj*)'
    TabOrder = 7
  end
  object API_files1: TAPI_files
    ShowProgress = True
    Confirmation = False
    Left = 12
    Top = 36
  end
  object SaveDialog1: TSaveDialog
    Left = 40
    Top = 80
  end
end
