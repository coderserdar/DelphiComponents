object MainForm: TMainForm
  Left = 297
  Top = 138
  Caption = 'Buffered Streams Test'
  ClientHeight = 495
  ClientWidth = 645
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label5: TLabel
    Left = 10
    Top = 11
    Width = 47
    Height = 13
    Caption = 'FileName:'
  end
  object Label6: TLabel
    Left = 4
    Top = 37
    Width = 53
    Height = 13
    Caption = 'BufferSize:'
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 68
    Width = 645
    Height = 427
    ActivePage = TabSheet1
    Align = alBottom
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'Speed Test TIcsBufferedFileStream'
      object Label8: TLabel
        Left = 54
        Top = 40
        Width = 58
        Height = 13
        Caption = 'WriteLoops:'
      end
      object Label9: TLabel
        Left = 6
        Top = 14
        Width = 105
        Height = 13
        Caption = 'Read/Write BlockSize:'
      end
      object Label12: TLabel
        Left = 198
        Top = 40
        Width = 31
        Height = 13
        Caption = 'Label6'
      end
      object GroupBox1: TGroupBox
        Left = 6
        Top = 66
        Width = 185
        Height = 189
        Caption = ' TIcsBufferedFileStream '
        TabOrder = 0
        DesignSize = (
          185
          189)
        object Label10: TLabel
          Left = 12
          Top = 80
          Width = 37
          Height = 13
          Caption = 'Label10'
        end
        object ButtonBufferedFileStreamRead: TButton
          Left = 92
          Top = 148
          Width = 75
          Height = 25
          Anchors = [akLeft, akBottom]
          Caption = 'Read'
          TabOrder = 0
          OnClick = ButtonBufferedFileStreamReadClick
        end
        object ButtonBufferedFileStreamWrite: TButton
          Left = 12
          Top = 148
          Width = 75
          Height = 25
          Anchors = [akLeft, akBottom]
          Caption = 'Write'
          TabOrder = 1
          OnClick = ButtonBufferedFileStreamWriteClick
        end
      end
      object EditBlockSize: TEdit
        Left = 116
        Top = 10
        Width = 65
        Height = 21
        TabOrder = 1
        Text = '1024'
        OnChange = EditBlockSizeChange
      end
      object EditLoops: TEdit
        Left = 116
        Top = 36
        Width = 65
        Height = 21
        TabOrder = 2
        Text = '9000'
        OnChange = EditBlockSizeChange
      end
      object GroupBox2: TGroupBox
        Left = 390
        Top = 66
        Width = 179
        Height = 189
        Caption = ' TFileStream '
        TabOrder = 3
        DesignSize = (
          179
          189)
        object Label13: TLabel
          Left = 10
          Top = 82
          Width = 37
          Height = 13
          Caption = 'Label13'
        end
        object ButtonFileStreamRead: TButton
          Left = 90
          Top = 148
          Width = 75
          Height = 25
          Anchors = [akLeft, akBottom]
          Caption = 'Read'
          TabOrder = 0
          OnClick = ButtonFileStreamReadClick
        end
        object ButtonFileStreamWrite: TButton
          Left = 10
          Top = 148
          Width = 75
          Height = 25
          Anchors = [akLeft, akBottom]
          Caption = 'Write'
          TabOrder = 1
          OnClick = ButtonFileStreamWriteClick
        end
      end
      object GroupBox3: TGroupBox
        Left = 198
        Top = 66
        Width = 185
        Height = 189
        Caption = ' TIcsBufferedStream '
        TabOrder = 4
        DesignSize = (
          185
          189)
        object Label7: TLabel
          Left = 12
          Top = 80
          Width = 31
          Height = 13
          Caption = 'Label7'
        end
        object ButtonBufferdStreamRead: TButton
          Left = 93
          Top = 148
          Width = 75
          Height = 25
          Anchors = [akLeft, akBottom]
          Caption = 'Read'
          TabOrder = 0
          OnClick = ButtonBufferedStreamReadClick
        end
        object ButtonBufferedStreamWrite: TButton
          Left = 12
          Top = 148
          Width = 75
          Height = 25
          Anchors = [akLeft, akBottom]
          Caption = 'Write'
          TabOrder = 1
          OnClick = ButtonBufferedStreamWriteClick
        end
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'TIcsStreamReader/TIcsStreamWriter And Other Stream Tests'
      ImageIndex = 1
      DesignSize = (
        637
        399)
      object Label1: TLabel
        Left = 8
        Top = 20
        Width = 23
        Height = 13
        Caption = 'Line:'
      end
      object Label2: TLabel
        Left = 8
        Top = 76
        Width = 47
        Height = 13
        Caption = 'Encoding:'
      end
      object Label3: TLabel
        Left = 8
        Top = 104
        Width = 55
        Height = 13
        Caption = 'LineBreaks:'
      end
      object Label4: TLabel
        Left = 8
        Top = 48
        Width = 52
        Height = 13
        Caption = 'LineCount:'
      end
      object EditLine: TEdit
        Left = 66
        Top = 16
        Width = 565
        Height = 21
        TabOrder = 0
        Text = 'Angus fait la f'#234'te '#224' Fran'#231'ois quand l'#39#233't'#233' arrive'
      end
      object Memo1: TMemo
        Left = 6
        Top = 204
        Width = 625
        Height = 165
        Anchors = [akLeft, akTop, akRight, akBottom]
        Lines.Strings = (
          'Memo1')
        ScrollBars = ssVertical
        TabOrder = 1
      end
      object ButtonIcsStreamReader: TButton
        Left = 6
        Top = 375
        Width = 147
        Height = 21
        Anchors = [akLeft, akBottom]
        Caption = 'Read File TIcsStreamReader'
        TabOrder = 2
        OnClick = ButtonIcsStreamReaderClick
      end
      object ButtonIcsStreamWriter: TButton
        Left = 66
        Top = 150
        Width = 143
        Height = 21
        Caption = 'Write File TIcsStreamWriter'
        TabOrder = 3
        OnClick = ButtonIcsStreamWriterClick
      end
      object ComboBoxCodePage: TComboBox
        Left = 66
        Top = 72
        Width = 145
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        ItemIndex = 0
        TabOrder = 4
        Text = 'ANSI (Current)'
        Items.Strings = (
          'ANSI (Current)'
          'UTF8'
          'UTF16'
          'UTF16 (big endian)')
      end
      object ComboBoxLineBreaks: TComboBox
        Left = 66
        Top = 100
        Width = 145
        Height = 21
        ItemHeight = 13
        ItemIndex = 0
        TabOrder = 5
        Text = 'CRLF'
        Items.Strings = (
          'CRLF'
          'LF'
          'CR')
      end
      object EditLineCount: TEdit
        Left = 66
        Top = 44
        Width = 145
        Height = 21
        TabOrder = 6
        Text = '1000'
      end
      object CheckBoxDisplay: TCheckBox
        Left = 295
        Top = 377
        Width = 56
        Height = 17
        Anchors = [akLeft, akBottom]
        Caption = 'Display'
        TabOrder = 7
      end
      object ButtonClearMemo: TButton
        Left = 562
        Top = 375
        Width = 71
        Height = 21
        Anchors = [akLeft, akBottom]
        Caption = 'Clear Memo'
        TabOrder = 8
        OnClick = ButtonClearMemoClick
      end
      object ButtonStreamWriter: TButton
        Left = 66
        Top = 172
        Width = 143
        Height = 21
        Caption = 'Write File TStreamWriter'
        TabOrder = 9
        OnClick = ButtonFileStreamWriteClick
      end
      object ButtonStreamReader: TButton
        Left = 156
        Top = 375
        Width = 134
        Height = 21
        Anchors = [akLeft, akBottom]
        Caption = 'Read File TStreamReader'
        TabOrder = 10
      end
      object ButtonDetectLineBreakStyle: TButton
        Left = 354
        Top = 375
        Width = 205
        Height = 21
        Anchors = [akLeft, akBottom]
        Caption = 'TIcsStreamReader Detect LineBreakStyle'
        TabOrder = 11
        OnClick = ButtonDetectLineBreakStyleClick
      end
      object CheckBoxAppend: TCheckBox
        Left = 66
        Top = 127
        Width = 65
        Height = 17
        Caption = 'Append'
        TabOrder = 12
      end
      object StreamGroup: TRadioGroup
        Left = 318
        Top = 44
        Width = 313
        Height = 149
        Caption = ' Other Stream Tests '
        ItemIndex = 0
        Items.Strings = (
          'TFileStream'
          'TBufferedFileStream'
          'TIcsBufferedStream')
        TabOrder = 13
      end
      object ButtonSeekTest: TButton
        Left = 450
        Top = 64
        Width = 75
        Height = 21
        Caption = 'SeekTest'
        TabOrder = 14
        OnClick = ButtonSeekTestClick
      end
      object ButtonReadWriteTest: TButton
        Left = 526
        Top = 64
        Width = 89
        Height = 21
        Caption = 'ReadWriteTest'
        TabOrder = 15
        OnClick = ButtonReadWriteTestClick
      end
      object ButtonWriteTest: TButton
        Left = 450
        Top = 86
        Width = 75
        Height = 21
        Caption = 'WriteTest'
        TabOrder = 16
        OnClick = ButtonWriteTestClick
      end
      object ButtonSeekAndReplace: TButton
        Left = 526
        Top = 86
        Width = 89
        Height = 21
        Caption = 'SeekAndReplace'
        TabOrder = 17
        OnClick = ButtonSeekAndReplaceClick
      end
      object ButtonRandomReadWrite: TButton
        Left = 450
        Top = 113
        Width = 165
        Height = 21
        Caption = 'Main Random Read Write Test'
        TabOrder = 18
        OnClick = ButtonRandomReadWriteClick
      end
      object Panel1: TPanel
        Left = 510
        Top = 146
        Width = 43
        Height = 19
        BevelOuter = bvNone
        Caption = 'Panel1'
        TabOrder = 19
      end
    end
  end
  object EditFileName: TEdit
    Left = 64
    Top = 7
    Width = 571
    Height = 21
    TabOrder = 1
    Text = 'xtestfile.txt'
  end
  object EditBufSize: TEdit
    Left = 63
    Top = 33
    Width = 84
    Height = 21
    TabOrder = 2
    Text = '4096'
  end
end
