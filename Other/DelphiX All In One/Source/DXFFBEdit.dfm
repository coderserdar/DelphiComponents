object DelphiXFFEditForm: TDelphiXFFEditForm
  Left = 198
  Top = 110
  BorderStyle = bsDialog
  Caption = 'Effect Editor'
  ClientHeight = 412
  ClientWidth = 527
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  PopupMenu = PopupMenu
  Position = poScreenCenter
  Scaled = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object ListGroupBox: TGroupBox
    Left = 8
    Top = 8
    Width = 193
    Height = 369
    Caption = 'Effect list'
    TabOrder = 2
    object AddButton: TButton
      Left = 8
      Top = 306
      Width = 86
      Height = 25
      Caption = '&Add'
      TabOrder = 1
      OnClick = AddButtonClick
    end
    object DelButton: TButton
      Left = 99
      Top = 306
      Width = 86
      Height = 25
      Caption = '&Delete'
      TabOrder = 2
      OnClick = DelButtonClick
    end
    object AddFromFileButton: TButton
      Left = 8
      Top = 335
      Width = 177
      Height = 25
      Caption = '&Load from File'
      TabOrder = 3
      OnClick = AddFromFileButtonClick
    end
    object EffectView: TTreeView
      Left = 8
      Top = 16
      Width = 177
      Height = 284
      DragMode = dmAutomatic
      HideSelection = False
      Indent = 19
      TabOrder = 0
      OnChange = EffectViewChange
      OnDragDrop = EffectViewDragDrop
      OnDragOver = EffectViewDragOver
      OnEdited = EffectViewEdited
    end
  end
  object EditGroupBox: TGroupBox
    Left = 208
    Top = 8
    Width = 313
    Height = 369
    Caption = 'Editor'
    TabOrder = 3
    object EffectTypeLabel: TLabel
      Left = 16
      Top = 67
      Width = 23
      Height = 13
      Caption = '&Time'
      FocusControl = EffectTypeBox
    end
    object ConstantLabel: TLabel
      Left = 16
      Top = 249
      Width = 45
      Height = 13
      Caption = '&Constant:'
      FocusControl = ConstantXEdit
    end
    object ConstantXLabel: TLabel
      Left = 96
      Top = 249
      Width = 7
      Height = 13
      Caption = 'X'
    end
    object ConstantYLabel: TLabel
      Left = 192
      Top = 249
      Width = 7
      Height = 13
      Caption = 'Y'
    end
    object FadeTimeLabel: TLabel
      Left = 80
      Top = 199
      Width = 26
      Height = 13
      Caption = 'Time:'
    end
    object PeriodLabel: TLabel
      Left = 16
      Top = 275
      Width = 33
      Height = 13
      Caption = '&Period:'
      FocusControl = PeriodEdit
    end
    object PeriodLabel2: TLabel
      Left = 190
      Top = 275
      Width = 13
      Height = 13
      Caption = 'ms'
    end
    object PowerLabel: TLabel
      Left = 16
      Top = 123
      Width = 33
      Height = 13
      Caption = '&Power:'
    end
    object TimeLabel: TLabel
      Left = 16
      Top = 99
      Width = 26
      Height = 13
      Caption = '&Time:'
    end
    object TimeLabel2: TLabel
      Left = 190
      Top = 99
      Width = 13
      Height = 13
      Caption = 'ms'
    end
    object AttackTimeEditLabel: TLabel
      Left = 80
      Top = 149
      Width = 26
      Height = 13
      Caption = 'Time:'
      FocusControl = AttackTimeEdit
    end
    object NameEditLabel: TLabel
      Left = 16
      Top = 20
      Width = 28
      Height = 13
      Caption = '&Name'
      FocusControl = NameEdit
    end
    object Bevel1: TBevel
      Left = 16
      Top = 44
      Width = 281
      Height = 2
    end
    object Bevel2: TBevel
      Left = 16
      Top = 328
      Width = 281
      Height = 2
    end
    object ConditionLabel: TLabel
      Left = 16
      Top = 299
      Width = 47
      Height = 13
      Caption = 'Con&dition:'
      FocusControl = ConditionXEdit
    end
    object ConditionXLabel: TLabel
      Left = 96
      Top = 299
      Width = 7
      Height = 13
      Caption = 'X'
    end
    object ConditionYLabel: TLabel
      Left = 192
      Top = 297
      Width = 7
      Height = 13
      Caption = 'Y'
    end
    object Label1: TLabel
      Left = 80
      Top = 173
      Width = 29
      Height = 13
      Caption = 'Level:'
      FocusControl = AttackLevelEdit
    end
    object Label2: TLabel
      Left = 80
      Top = 223
      Width = 29
      Height = 13
      Caption = 'Level:'
      FocusControl = FadeLevelEdit
    end
    object Label3: TLabel
      Left = 16
      Top = 149
      Width = 34
      Height = 13
      Caption = '&Attack:'
      FocusControl = AttackTimeEdit
    end
    object Label4: TLabel
      Left = 16
      Top = 199
      Width = 27
      Height = 13
      Caption = '&Fade:'
      FocusControl = FadeTimeEdit
    end
    object Label5: TLabel
      Left = 190
      Top = 150
      Width = 13
      Height = 13
      Caption = 'ms'
    end
    object Label6: TLabel
      Left = 190
      Top = 198
      Width = 13
      Height = 13
      Caption = 'ms'
    end
    object EffectTypeBox: TComboBox
      Left = 72
      Top = 64
      Width = 225
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 1
      OnChange = EffectTypeBoxChange
    end
    object ConstantYEdit: TSpinEdit
      Left = 208
      Top = 245
      Width = 73
      Height = 22
      MaxValue = 10000
      MinValue = -10000
      TabOrder = 9
      Value = 0
      OnChange = ChangeEvent
    end
    object ConstantXEdit: TSpinEdit
      Left = 112
      Top = 245
      Width = 73
      Height = 22
      MaxValue = 10000
      MinValue = -10000
      TabOrder = 8
      Value = 0
      OnChange = ChangeEvent
    end
    object FadeTimeEdit: TSpinEdit
      Left = 112
      Top = 194
      Width = 73
      Height = 22
      MaxValue = 1000000
      MinValue = 0
      TabOrder = 6
      Value = 0
      OnChange = ChangeEvent
    end
    object PeriodEdit: TSpinEdit
      Left = 112
      Top = 270
      Width = 73
      Height = 22
      MaxValue = 1000000
      MinValue = 0
      TabOrder = 10
      Value = 0
      OnChange = ChangeEvent
    end
    object TimeEdit: TSpinEdit
      Left = 112
      Top = 94
      Width = 73
      Height = 22
      MaxValue = 1000000
      MinValue = -1
      TabOrder = 3
      Value = 0
      OnChange = ChangeEvent
    end
    object AttackTimeEdit: TSpinEdit
      Left = 112
      Top = 145
      Width = 73
      Height = 22
      MaxValue = 1000000
      MinValue = 0
      TabOrder = 4
      Value = 0
      OnChange = ChangeEvent
    end
    object RunButton: TButton
      Left = 16
      Top = 336
      Width = 81
      Height = 25
      Caption = '&Run'
      TabOrder = 13
      OnClick = RunButtonClick
    end
    object RunGroupButton: TButton
      Left = 104
      Top = 336
      Width = 105
      Height = 25
      Caption = 'Run &group'
      TabOrder = 14
      OnClick = RunGroupButtonClick
    end
    object NameEdit: TEdit
      Left = 72
      Top = 17
      Width = 225
      Height = 21
      TabOrder = 0
      OnChange = NameEditChange
    end
    object StopButton: TButton
      Left = 216
      Top = 336
      Width = 81
      Height = 25
      Caption = '&Stop'
      TabOrder = 15
      OnClick = StopButtonClick
    end
    object PowerEdit: TSpinEdit
      Left = 112
      Top = 118
      Width = 73
      Height = 22
      MaxValue = 10000
      MinValue = 0
      TabOrder = 2
      Value = 0
      OnChange = ChangeEvent
    end
    object ConditionXEdit: TSpinEdit
      Left = 112
      Top = 295
      Width = 73
      Height = 22
      MaxValue = 10000
      MinValue = -10000
      TabOrder = 11
      Value = 0
      OnChange = ChangeEvent
    end
    object ConditionYEdit: TSpinEdit
      Left = 208
      Top = 295
      Width = 73
      Height = 22
      MaxValue = 10000
      MinValue = -10000
      TabOrder = 12
      Value = 0
      OnChange = ChangeEvent
    end
    object AttackLevelEdit: TSpinEdit
      Left = 112
      Top = 169
      Width = 73
      Height = 22
      MaxValue = 10000
      MinValue = 0
      TabOrder = 5
      Value = 0
      OnChange = ChangeEvent
    end
    object FadeLevelEdit: TSpinEdit
      Left = 112
      Top = 218
      Width = 73
      Height = 22
      MaxValue = 10000
      MinValue = 0
      TabOrder = 7
      Value = 0
      OnChange = ChangeEvent
    end
  end
  object OKButton: TButton
    Left = 352
    Top = 384
    Width = 81
    Height = 25
    Caption = 'OK'
    TabOrder = 0
    OnClick = OKButtonClick
  end
  object CancelButton: TButton
    Left = 440
    Top = 384
    Width = 81
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 1
    OnClick = CancelButtonClick
  end
  object DXInput: TDXInput
    ActiveOnly = True
    Joystick.BindInputStates = True
    Joystick.Effects.Effects = {
      FF0A0044454C50484958464F524345464545444241434B454646454354003010
      7F000000545046301D54466F726365466565646261636B456666656374436F6D
      706F6E656E74025F3107456666656374730E01044E616D650607456666656374
      730A45666665637454797065070665744E6F6E6506506572696F64023205506F
      7765720310270454696D6503E8030E537461727444656C617954696D65020000
      000000}
    Joystick.Enabled = False
    Joystick.ForceFeedback = True
    Joystick.AutoCenter = False
    Joystick.DeadZoneX = 50
    Joystick.DeadZoneY = 50
    Joystick.DeadZoneZ = 50
    Joystick.ID = 0
    Joystick.RangeX = 1000
    Joystick.RangeY = 1000
    Joystick.RangeZ = 1000
    Keyboard.BindInputStates = True
    Keyboard.Effects.Effects = {
      FF0A0044454C50484958464F524345464545444241434B454646454354003010
      7F000000545046301D54466F726365466565646261636B456666656374436F6D
      706F6E656E74025F3107456666656374730E01044E616D650607456666656374
      730A45666665637454797065070665744E6F6E6506506572696F64023205506F
      7765720310270454696D6503E8030E537461727444656C617954696D65020000
      000000}
    Keyboard.Enabled = False
    Keyboard.ForceFeedback = False
    Keyboard.Assigns = {
      4B00000026000000680000004A00000028000000620000004800000025000000
      640000004C00000027000000660000005A000000200000000000000058000000
      0D00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000071000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000}
    Mouse.BindInputStates = False
    Mouse.Effects.Effects = {
      FF0A0044454C50484958464F524345464545444241434B454646454354003010
      7F000000545046301D54466F726365466565646261636B456666656374436F6D
      706F6E656E74025F3107456666656374730E01044E616D650607456666656374
      730A45666665637454797065070665744E6F6E6506506572696F64023205506F
      7765720310270454696D6503E8030E537461727444656C617954696D65020000
      000000}
    Mouse.Enabled = False
    Mouse.ForceFeedback = False
    UseDirectInput = False
    Left = 32
    Top = 40
  end
  object PopupMenu: TPopupMenu
    Left = 72
    Top = 88
    object A1: TMenuItem
      Caption = '&Add'
      ShortCut = 16449
      OnClick = AddButtonClick
    end
    object DeleteEffectItem: TMenuItem
      Caption = '&Delete'
      ShortCut = 46
      OnClick = DelButtonClick
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object N2: TMenuItem
      Caption = '&Load from File'
      OnClick = AddFromFileButtonClick
    end
    object SaveToFileItem: TMenuItem
      Caption = '&Save from File'
      OnClick = SaveToFileItemClick
    end
  end
  object Timer: TTimer
    Interval = 50
    OnTimer = TimerTimer
    Left = 72
    Top = 40
  end
  object OpenDialog: TOpenDialog
    DefaultExt = 'ffe'
    Filter = 'Effect file(*.ffe)|*.ffe'
    Options = [ofAllowMultiSelect, ofPathMustExist, ofFileMustExist]
    Left = 32
    Top = 88
  end
  object SaveDialog: TSaveDialog
    DefaultExt = 'ffe'
    Filter = 'Effect file(*.ffe)|*.ffe'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist]
    Left = 32
    Top = 120
  end
end
