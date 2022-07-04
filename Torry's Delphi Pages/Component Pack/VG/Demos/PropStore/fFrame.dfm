object StoredFrame: TStoredFrame
  Left = 0
  Top = 0
  Width = 270
  Height = 239
  TabOrder = 0
  object cmLoad: TButton
    Left = 12
    Top = 207
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&Load frame'
    TabOrder = 2
    OnClick = cmLoadClick
  end
  object cmSave: TButton
    Left = 99
    Top = 207
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&Save frame'
    TabOrder = 3
    OnClick = cmSaveClick
  end
  object me: TMemo
    Left = 8
    Top = 8
    Width = 252
    Height = 152
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      'Use the TPropStorage.MasterStorage '
      'property to set the root storage for frames.'
      ''
      'When the TPropStorage.MasterStorage property'
      'is set the master component will load and save '
      'their child storages automatically.')
    ReadOnly = True
    ScrollBars = ssHorizontal
    TabOrder = 0
    WordWrap = False
  end
  object cmFont: TButton
    Left = 11
    Top = 171
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Font...'
    TabOrder = 1
    OnClick = cmFontClick
  end
  object psFrame: TPropStorage
    AppIniFile = dm.afIniFile
    StoredProps.Strings = (
      'me.Font')
    StoredValues = <>
    Left = 20
    Top = 104
  end
  object fnFont: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    MinFontSize = 0
    MaxFontSize = 0
    Left = 64
    Top = 104
  end
end
