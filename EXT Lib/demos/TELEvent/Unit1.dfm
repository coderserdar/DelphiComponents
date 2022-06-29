object Form1: TForm1
  Left = 214
  Top = 119
  Width = 492
  Height = 373
  Caption = 'TELEvent, TELEventSource demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsMDIForm
  Menu = MainMenu1
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 309
    Width = 484
    Height = 18
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object Bevel2: TBevel
      Left = -4
      Top = 11
      Width = 10000
      Height = 3
      Shape = bsTopLine
    end
    object Label1: TLabel
      Left = 8
      Top = 3
      Width = 80
      Height = 13
      Caption = 'Extension Library'
      Enabled = False
    end
  end
  object ActionList1: TActionList
    Left = 16
    Top = 48
    object actNew: TAction
      Category = 'File'
      Caption = '&New'
      OnExecute = actNewExecute
    end
    object actOpen: TAction
      Category = 'File'
      Caption = '&Open...'
      OnExecute = actOpenExecute
    end
    object actSave: TAction
      Category = 'File'
      Caption = '&Save'
      OnExecute = actSaveExecute
      OnUpdate = actSaveUpdate
    end
    object actSaveAs: TAction
      Category = 'File'
      Caption = 'SaveAs...'
      OnExecute = actSaveAsExecute
      OnUpdate = actSaveAsUpdate
    end
    object actClose: TAction
      Category = 'File'
      Caption = '&Close'
      OnExecute = actCloseExecute
      OnUpdate = actCloseUpdate
    end
    object actCloseAll: TAction
      Category = 'File'
      Caption = 'Close&All'
      OnExecute = actCloseAllExecute
      OnUpdate = actCloseAllUpdate
    end
    object actExit: TAction
      Category = 'File'
      Caption = '&Exit'
      OnExecute = actExitExecute
    end
    object actWindowCascade: TWindowCascade
      Category = 'Window'
      Caption = '&Cascade'
      Enabled = False
      Hint = 'Cascade'
      ImageIndex = 17
    end
    object actWindowTileHorizontal: TWindowTileHorizontal
      Category = 'Window'
      Caption = 'Tile &Horizontally'
      Enabled = False
      Hint = 'Tile Horizontal'
      ImageIndex = 15
    end
    object actWindowTileVertical: TWindowTileVertical
      Category = 'Window'
      Caption = '&Tile Vertically'
      Enabled = False
      Hint = 'Tile Vertical'
      ImageIndex = 16
    end
    object actWindowMinimizeAll: TWindowMinimizeAll
      Category = 'Window'
      Caption = '&Minimize All'
      Enabled = False
      Hint = 'Minimize All'
    end
    object actWindowArrange: TWindowArrange
      Category = 'Window'
      Caption = '&Arrange'
      Enabled = False
    end
    object actOptions: TAction
      Category = 'Window'
      Caption = '&Options...'
      OnExecute = actOptionsExecute
    end
  end
  object MainMenu1: TMainMenu
    Left = 16
    Top = 16
    object File1: TMenuItem
      Caption = '&File'
      object New1: TMenuItem
        Action = actNew
      end
      object Open1: TMenuItem
        Action = actOpen
      end
      object Close1: TMenuItem
        Action = actSave
      end
      object SaveAs1: TMenuItem
        Action = actSaveAs
      end
      object Close2: TMenuItem
        Action = actClose
      end
      object CloseAll1: TMenuItem
        Action = actCloseAll
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Action = actExit
      end
    end
    object Window1: TMenuItem
      Caption = '&Window'
      object Cascade1: TMenuItem
        Action = actWindowCascade
      end
      object ileHorizontally1: TMenuItem
        Action = actWindowTileHorizontal
      end
      object ileVertically1: TMenuItem
        Action = actWindowTileVertical
      end
      object Arrange1: TMenuItem
        Action = actWindowArrange
      end
      object MinimizeAll1: TMenuItem
        Action = actWindowMinimizeAll
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object actOptions1: TMenuItem
        Action = actOptions
      end
    end
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = 'txt'
    Filter = 'Text documents|*.txt|All files|*.*'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Left = 48
    Top = 16
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'txt'
    Filter = 'Text documents|*.txt'
    Left = 48
    Top = 48
  end
  object ELStringList1: TELStringList
    Lines.Strings = (
      'This example illustrates how to use TELEvent and '
      'TELEventSender.'
      ''
      'TELEvent can join to TELEventSender (using '
      'TELEvent.Source). To one TELEventSender there '
      'can be joined several TELEvent components. '
      'When calling SendEvent of TELEventSender, the '
      'OnEvent event occures in all TELEvent components '
      'joined to TELEventSender. '
      ''
      'The Group property sets the sorting order of the '
      'TELEvent components. Components with less '
      'values of the Group property get the event earlier '
      'than those with more than that.'
      ''
      'The components are created with the ability to '
      '(dis)connect TELEvent in the course of work of '
      'TELEventSender.SendEvent, that is in '
      'TELEvent.OnEvent event handlers. At the same '
      'time new connected TELEvent components do not '
      'get the current event. '
      ''
      'The advantage of this approach is  independence '
      'of the code of sending message block (uses TELEventSender)'
      'from the number and code of blocks, which need to '
      'get these messages (the blocks use the TELEvent components)'
      ''
      'So, as you see from the example, in such a way the form '
      'of options sends a message on option change, '
      'which is received by other project forms. ')
    Left = 48
    Top = 80
  end
  object ELEvent1: TELEvent
    Source = Form3.ELEventSender1
    OnEvent = ELEvent1Event
    Left = 80
    Top = 16
  end
end
