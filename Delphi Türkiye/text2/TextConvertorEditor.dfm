object formTextConvertorEditor: TformTextConvertorEditor
  Left = 192
  Top = 103
  BorderStyle = bsDialog
  Caption = 'Dosyalar'
  ClientHeight = 200
  ClientWidth = 350
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 5
    Top = 3
    Width = 340
    Height = 155
    Caption = 'Dosya listesi'
    TabOrder = 0
    object ListBox1: TListBox
      Left = 10
      Top = 20
      Width = 320
      Height = 90
      ItemHeight = 13
      MultiSelect = True
      TabOrder = 0
      OnClick = ItemClick
    end
    object btnAdd: TButton
      Left = 60
      Top = 120
      Width = 60
      Height = 25
      Caption = '&Ekle...'
      TabOrder = 1
      OnClick = btnAddClick
    end
    object btnDelete: TButton
      Left = 140
      Top = 120
      Width = 60
      Height = 25
      Caption = '&Sil'
      Enabled = False
      TabOrder = 2
      OnClick = btnDeleteClick
    end
    object btnClear: TButton
      Left = 220
      Top = 120
      Width = 60
      Height = 25
      Caption = '&Temizle'
      Enabled = False
      TabOrder = 3
      OnClick = btnClearClick
    end
  end
  object btnOK: TButton
    Left = 55
    Top = 170
    Width = 75
    Height = 25
    Caption = 'Tamam'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object btnCancel: TButton
    Left = 137
    Top = 170
    Width = 75
    Height = 25
    Cancel = True
    Caption = #304'ptal'
    ModalResult = 2
    TabOrder = 2
  end
  object Help: TButton
    Left = 220
    Top = 170
    Width = 75
    Height = 25
    Caption = '&Yard'#305'm'
    TabOrder = 3
  end
  object OpenDialog: TOpenDialog
    Filter = 'T'#252'm dosyalar (*.*)|*.*'
    FilterIndex = 0
    Title = 'Open'
    Left = 186
    Top = 81
  end
end
