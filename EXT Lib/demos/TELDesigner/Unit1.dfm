object Form1: TForm1
  Left = 6
  Top = 77
  BorderStyle = bsSingle
  Caption = 'TELDesigner demo'
  ClientHeight = 467
  ClientWidth = 484
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 2
    Top = 458
    Width = 481
    Height = 3
    Shape = bsTopLine
  end
  object Label4: TLabel
    Left = 8
    Top = 451
    Width = 80
    Height = 13
    Caption = 'Extension Library'
    Enabled = False
  end
  object Label3: TLabel
    Left = 8
    Top = 314
    Width = 151
    Height = 13
    Caption = 'Events trace (only some events)'
  end
  object Button1: TButton
    Left = 8
    Top = 8
    Width = 129
    Height = 25
    Caption = 'Activate designer'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 144
    Top = 8
    Width = 129
    Height = 25
    Caption = 'Deactivate designer'
    TabOrder = 1
    OnClick = Button2Click
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 40
    Width = 129
    Height = 265
    Caption = 'Form grid'
    TabOrder = 2
    object Label6: TLabel
      Left = 16
      Top = 56
      Width = 29
      Height = 13
      Caption = 'XStep'
    end
    object Label7: TLabel
      Left = 16
      Top = 80
      Width = 29
      Height = 13
      Caption = 'YStep'
    end
    object Label8: TLabel
      Left = 16
      Top = 104
      Width = 24
      Height = 13
      Caption = 'Color'
    end
    object Label1: TLabel
      Left = 56
      Top = 56
      Width = 6
      Height = 13
      Caption = '8'
    end
    object Label2: TLabel
      Left = 56
      Top = 80
      Width = 6
      Height = 13
      Caption = '8'
    end
    object CheckBox2: TCheckBox
      Left = 16
      Top = 24
      Width = 97
      Height = 17
      Caption = 'Visible'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = CheckBox2Click
    end
    object ColorGrid2: TColorGrid
      Left = 16
      Top = 120
      Width = 100
      Height = 100
      BackgroundEnabled = False
      TabOrder = 1
      OnChange = ColorGrid2Change
    end
    object CheckBox1: TCheckBox
      Left = 16
      Top = 232
      Width = 97
      Height = 17
      Caption = 'Snap to grid'
      Checked = True
      State = cbChecked
      TabOrder = 2
      OnClick = CheckBox1Click
    end
    object UpDown2: TUpDown
      Left = 80
      Top = 49
      Width = 16
      Height = 24
      Min = 2
      Max = 32
      Position = 8
      TabOrder = 3
      Wrap = False
      OnClick = UpDown2Click
    end
    object UpDown1: TUpDown
      Left = 80
      Top = 76
      Width = 16
      Height = 24
      Min = 2
      Max = 32
      Position = 8
      TabOrder = 4
      Wrap = False
      OnClick = UpDown1Click
    end
  end
  object GroupBox2: TGroupBox
    Left = 144
    Top = 40
    Width = 153
    Height = 265
    Caption = 'Selected controls'
    TabOrder = 3
    object Memo2: TMemo
      Left = 8
      Top = 24
      Width = 137
      Height = 169
      TabOrder = 0
    end
    object Button3: TButton
      Left = 8
      Top = 232
      Width = 137
      Height = 25
      Caption = 'Clear selection'
      TabOrder = 1
      OnClick = Button3Click
    end
    object Button4: TButton
      Left = 8
      Top = 200
      Width = 137
      Height = 25
      Caption = 'Select all'
      TabOrder = 2
      OnClick = Button4Click
    end
  end
  object GroupBox3: TGroupBox
    Left = 304
    Top = 40
    Width = 169
    Height = 121
    Caption = 'Hints'
    TabOrder = 4
    object CheckBox3: TCheckBox
      Left = 16
      Top = 16
      Width = 97
      Height = 17
      Caption = 'Control'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = CheckBox3Click
    end
    object CheckBox4: TCheckBox
      Left = 16
      Top = 32
      Width = 97
      Height = 17
      Caption = 'Size'
      Checked = True
      State = cbChecked
      TabOrder = 1
      OnClick = CheckBox4Click
    end
    object CheckBox5: TCheckBox
      Left = 16
      Top = 48
      Width = 97
      Height = 17
      Caption = 'Move'
      Checked = True
      State = cbChecked
      TabOrder = 2
      OnClick = CheckBox5Click
    end
    object CheckBox6: TCheckBox
      Left = 16
      Top = 64
      Width = 97
      Height = 17
      Caption = 'Insert'
      Checked = True
      State = cbChecked
      TabOrder = 3
      OnClick = CheckBox6Click
    end
    object CheckBox7: TCheckBox
      Left = 16
      Top = 80
      Width = 121
      Height = 17
      Caption = 'Custom control hint'
      TabOrder = 4
      OnClick = CheckBox7Click
    end
    object Edit1: TEdit
      Left = 32
      Top = 96
      Width = 105
      Height = 21
      Enabled = False
      TabOrder = 5
      Text = 'Some text: %s'
    end
  end
  object GroupBox4: TGroupBox
    Left = 304
    Top = 168
    Width = 169
    Height = 89
    Caption = 'Inserting new controls'
    TabOrder = 5
    object RadioButton1: TRadioButton
      Left = 8
      Top = 16
      Width = 113
      Height = 17
      Caption = 'None'
      Checked = True
      TabOrder = 0
      TabStop = True
    end
    object RadioButton2: TRadioButton
      Left = 8
      Top = 32
      Width = 113
      Height = 17
      Caption = 'TEdit'
      TabOrder = 1
    end
    object RadioButton3: TRadioButton
      Left = 8
      Top = 48
      Width = 113
      Height = 17
      Caption = 'TLabel'
      TabOrder = 2
    end
    object RadioButton4: TRadioButton
      Left = 8
      Top = 64
      Width = 113
      Height = 17
      Caption = 'TMyPanel (custom)'
      TabOrder = 3
    end
  end
  object Memo1: TMemo
    Left = 8
    Top = 357
    Width = 465
    Height = 92
    ScrollBars = ssBoth
    TabOrder = 6
  end
  object Panel1: TPanel
    Left = 304
    Top = 264
    Width = 169
    Height = 41
    Caption = 'Drag me on the designing form'
    DragMode = dmAutomatic
    TabOrder = 7
  end
  object CheckBox8: TCheckBox
    Left = 24
    Top = 336
    Width = 129
    Height = 17
    Caption = 'Trace mouse events'
    TabOrder = 8
  end
  object CheckBox9: TCheckBox
    Left = 168
    Top = 336
    Width = 145
    Height = 17
    Caption = 'Trace keyboard events'
    TabOrder = 9
  end
  object Button5: TButton
    Left = 400
    Top = 328
    Width = 75
    Height = 25
    Caption = 'Clear'
    TabOrder = 10
    OnClick = Button5Click
  end
  object CheckBox10: TCheckBox
    Left = 280
    Top = 16
    Width = 161
    Height = 17
    Caption = 'Recreate components'
    TabOrder = 11
  end
  object ELDesigner1: TELDesigner
    PopupMenu = PopupMenu1
    ClipboardFormat = 'Extension Library designer components'
    OnChange = ELDesigner1Change
    OnValidateName = ELDesigner1ValidateName
    OnGetUniqueName = ELDesigner1GetUniqueName
    OnControlInserting = ELDesigner1ControlInserting
    OnControlInserted = ELDesigner1ControlInserted
    OnNotification = ELDesigner1Notification
    OnChangeSelection = ELDesigner1ChangeSelection
    OnControlHint = ELDesigner1ControlHint
    OnKeyDown = ELDesigner1KeyDown
    OnKeyPress = ELDesigner1KeyPress
    OnKeyUp = ELDesigner1KeyUp
    OnMouseDown = ELDesigner1MouseDown
    OnMouseMove = ELDesigner1MouseMove
    OnMouseUp = ELDesigner1MouseUp
    OnMouseWheel = ELDesigner1MouseWheel
    OnMouseWheelDown = ELDesigner1MouseWheelDown
    OnMouseWheelUp = ELDesigner1MouseWheelUp
    OnDblClick = ELDesigner1DblClick
    OnDragDrop = ELDesigner1DragDrop
    OnDragOver = ELDesigner1DragOver
    Left = 216
    Top = 104
  end
  object PopupMenu1: TPopupMenu
    OnPopup = PopupMenu1Popup
    Left = 184
    Top = 104
    object Delete1: TMenuItem
      Caption = 'Delete (Del)'
      OnClick = Delete1Click
    end
    object Aligntogrid1: TMenuItem
      Caption = 'Align to grid'
      OnClick = Aligntogrid1Click
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object SetVisibletoTrue1: TMenuItem
      Caption = 'Set Visible to True'
      OnClick = SetVisibletoTrue1Click
    end
    object SetVisibletoFalse1: TMenuItem
      Caption = 'Set Visible to False'
      OnClick = SetVisibletoFalse1Click
    end
    object SetEnabledtoTrue1: TMenuItem
      Caption = 'Set Enabled to True'
      OnClick = SetEnabledtoTrue1Click
    end
    object SetEnabledtoFalse1: TMenuItem
      Caption = 'Set Enabled to False'
      OnClick = SetEnabledtoFalse1Click
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object Nodelete1: TMenuItem
      Caption = 'No delete'
      OnClick = Nodelete1Click
    end
    object NoMove1: TMenuItem
      Caption = 'No move'
      OnClick = NoMove1Click
    end
    object Noresize1: TMenuItem
      Caption = 'No resize'
      OnClick = Noresize1Click
    end
    object Noinsertin1: TMenuItem
      Caption = 'No insert in'
      OnClick = Noinsertin1Click
    end
    object Nocopy1: TMenuItem
      Caption = 'No copy'
      OnClick = Nocopy1Click
    end
    object lmCustom11: TMenuItem
      Caption = 'lmCustom1'
      OnClick = lmCustom11Click
    end
    object lmCustom21: TMenuItem
      Caption = 'lmCustom2'
      OnClick = lmCustom21Click
    end
    object N3: TMenuItem
      Caption = '-'
    end
    object Copy1: TMenuItem
      Caption = 'Copy (Ctrl+C)'
      OnClick = Copy1Click
    end
    object Cut1: TMenuItem
      Caption = 'Cut (Ctrl+X)'
      OnClick = Cut1Click
    end
    object Paste1: TMenuItem
      Caption = 'Paste (Ctrl+V)'
      OnClick = Paste1Click
    end
  end
end
