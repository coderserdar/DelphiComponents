object Form1: TForm1
  Left = 184
  Top = 122
  BorderIcons = [biSystemMenu, biMinimize, biMaximize, biHelp]
  Caption = 
    'SXSkinComponents by Alexey Sadovnikov - Components Features Demo' +
    ' Application'
  ClientHeight = 665
  ClientWidth = 671
  Color = clBtnFace
  Constraints.MinHeight = 400
  Constraints.MinWidth = 400
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clBlack
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 671
    Height = 665
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object SXSkinImage2: TSXSkinImage
      Width = 671
      Height = 665
      Align = alClient
      SkinLibrary = SXSkinLibrary1
      SkinStyle = 'Background'
      TabOrder = 0
      object SXSkinNotebook1: TSXSkinNotebook
        Top = 58
        Width = 671
        Height = 607
        Align = alClient
        SkinLibrary = SXSkinLibrary1
        TabOrder = 0
        ActivePage = SelectiveStylesPage
        object LabelPage: TSXSkinNotebookPage
          Width = 671
          Height = 607
          Align = alClient
          SkinLibrary = SXSkinLibrary1
          TabOrder = 0
          Visible = False
          DesignSize = (
            671
            607)
          object SXSkinLabel6: TSXSkinLabel
            Top = 584
            Width = 342
            Height = 23
            Align = alBottom
            Caption = 
              'This is a bottom-aligned label with left and bottom text offsets' +
              ' set to 10.'
            SkinLibrary = SXSkinLibrary1
            TextLeftOffset = 10
            TextBottomOffset = 10
          end
          object SXSkinLabel7: TSXSkinLabel
            Left = 8
            Top = 32
            Width = 412
            Height = 13
            Caption = 
              'This is a disabled label (when Windows Classic skin is used, it ' +
              'is painted with a shadow)'
            Enabled = False
            SkinLibrary = SXSkinLibrary1
          end
          object SXSkinLabel8: TSXSkinLabel
            Left = 8
            Top = 56
            Width = 272
            Height = 13
            Caption = 'All single-line labels can be autosized by width and height.'
            SkinLibrary = SXSkinLibrary1
          end
          object SXSkinLabel9: TSXSkinLabel
            Left = 8
            Top = 80
            Width = 145
            Height = 39
            AutoSizeWidth = False
            Caption = 
              'Labels with word wrap support can be autosized only by their hei' +
              'ght.'
            SkinLibrary = SXSkinLibrary1
            WordWrap = True
          end
          object SXSkinLabel10: TSXSkinLabel
            Left = 8
            Top = 128
            Width = 123
            Height = 26
            Caption = 'This is a multiline label.'#13#10'And this is it'#39's second line.'
            SkinLibrary = SXSkinLibrary1
          end
          object SXSkinLabel11: TSXSkinLabel
            Left = 240
            Top = 80
            Width = 70
            Height = 39
            Alignment = taCenter
            Caption = 'Text on labels'#13#10'can be aligned'#13#10'horizontally.'
            SkinLibrary = SXSkinLibrary1
          end
          object SXSkinLabel12: TSXSkinLabel
            Left = 8
            Top = 8
            Width = 275
            Height = 13
            Caption = 'This is a simple label created with one-click in the designer'
            SkinLibrary = SXSkinLibrary1
          end
          object SXSkinLabel13: TSXSkinLabel
            Left = 8
            Top = 168
            Width = 576
            Height = 23
            Caption = 
              'This label unlike all those are higher has custom font, style an' +
              'd color.'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clRed
            Font.Height = -20
            Font.Name = 'Times New Roman'
            Font.Style = [fsBold, fsUnderline]
            ParentFont = False
            SkinLibrary = SXSkinLibrary1
            UseCustomFont = True
            UseCustomFontColor = True
          end
          object SXSkinLabel14: TSXSkinLabel
            Left = 8
            Top = 208
            Width = 337
            Height = 18
            Caption = 'Here you can see two labels with one pixel offset.'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clAqua
            Font.Height = -16
            Font.Name = 'Arial'
            Font.Style = []
            ParentFont = False
            SkinLibrary = SXSkinLibrary1
            UseCustomFont = True
            UseCustomFontColor = True
          end
          object SXSkinLabel15: TSXSkinLabel
            Left = 9
            Top = 209
            Width = 337
            Height = 18
            Caption = 'Here you can see two labels with one pixel offset.'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clNavy
            Font.Height = -16
            Font.Name = 'Arial'
            Font.Style = []
            ParentFont = False
            SkinLibrary = SXSkinLibrary1
            UseCustomFont = True
            UseCustomFontColor = True
          end
          object SXSkinLabel16: TSXSkinLabel
            Left = 8
            Top = 509
            Width = 586
            Height = 60
            Anchors = [akLeft, akBottom]
            Caption = 
              'This label has a special "Transparent" skin. Try to see the tran' +
              'cparency'#13#10'when a picture is used as the background skin. Also it' +
              #39's skin is changed'#13#10'when the mouse cursor is over. This label is' +
              ' bottom-aligned. Try to resize.'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -16
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentFont = False
            SkinLibrary = SXSkinLibrary1
            SkinStyle = '_Label_Transparent'
          end
          object SXSkinLabel17: TSXSkinLabel
            Left = 8
            Top = 240
            Width = 158
            Height = 26
            Caption = 'When used with some skins,'#13#10'this label looks very-very cool.'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -11
            Font.Name = 'Times New Roman'
            Font.Style = [fsBold]
            ParentFont = False
            SkinLibrary = SXSkinLibrary1
            SkinStyle = '_Label_Cool'
          end
          object SXSkinLabel18: TSXSkinLabel
            Left = 416
            Top = 280
            Width = 189
            Height = 26
            Alignment = taCenter
            Caption = 
              'Font, style and colors of these labels are'#13#10'set in the designer,' +
              ' not in skin.ini file.'
            SkinLibrary = SXSkinLibrary1
          end
          object SXSkinImage3: TSXSkinImage
            Left = 350
            Top = 194
            Width = 167
            Height = 83
            SkinLibrary = SXSkinLibrary1
            SkinStyle = 'Arrows'
            TabOrder = 13
          end
        end
        object CheckBoxPage: TSXSkinNotebookPage
          Width = 671
          Height = 607
          Align = alClient
          SkinLibrary = SXSkinLibrary1
          TabOrder = 1
          Visible = False
          object SXSkinCheckBox4: TSXSkinCheckBox
            Left = 8
            Top = 8
            Width = 141
            Height = 15
            Caption = 'This is a simple checkbox'
            SkinLibrary = SXSkinLibrary1
            State = cbChecked
            TabOrder = 0
          end
          object SXSkinCheckBox6: TSXSkinCheckBox
            Left = 8
            Top = 32
            Width = 235
            Height = 28
            Caption = 
              'This checkbox unlike the previous is multiline.'#13#10'And this is it'#39 +
              's second line.'
            SkinLibrary = SXSkinLibrary1
            TabOrder = 1
          end
          object SXSkinCheckBox7: TSXSkinCheckBox
            Left = 8
            Top = 64
            Width = 204
            Height = 28
            AutoSizeWidth = False
            Caption = 
              'This checkbox has fixed width and it'#39's WordWrap property is set ' +
              'to True'
            SkinLibrary = SXSkinLibrary1
            TabOrder = 2
            WordWrap = True
          end
          object SXSkinCheckBox8: TSXSkinCheckBox
            Left = 8
            Top = 96
            Width = 142
            Height = 15
            Caption = 'This checkbox is disabled'
            Enabled = False
            SkinLibrary = SXSkinLibrary1
            TabOrder = 3
          end
          object SXSkinCheckBox9: TSXSkinCheckBox
            Left = 336
            Top = 8
            Width = 207
            Height = 15
            Caption = 'Checkbox with Glyph on the Right Side'
            GlyphPosition = gpRightTop
            SkinLibrary = SXSkinLibrary1
            TabOrder = 4
            TextOffset = 4
          end
          object SXSkinCheckBox10: TSXSkinCheckBox
            Left = 336
            Top = 40
            Width = 75
            Height = 43
            Alignment = taCenter
            Caption = 'Checkbox with'#13#10'Glyph on Top'
            GlyphPosition = gpTop
            SkinLibrary = SXSkinLibrary1
            TabOrder = 5
          end
          object SXSkinCheckBox11: TSXSkinCheckBox
            Left = 424
            Top = 40
            Width = 83
            Height = 43
            Alignment = taCenter
            Caption = 'Checkbox with'#13#10'Glyph on Bottom'
            GlyphPosition = gpBottom
            SkinLibrary = SXSkinLibrary1
            TabOrder = 6
          end
          object SXSkinCheckBox12: TSXSkinCheckBox
            Left = 8
            Top = 125
            Width = 203
            Height = 41
            AllowGrayed = True
            Caption = 
              'This checkbox has three states:'#13#10'Checked, Unchecked and Grayed.'#13 +
              #10'It'#39's AllowGrayed property is set to True.'
            GlyphPosition = gpLeft
            SkinLibrary = SXSkinLibrary1
            State = cbGrayed
            TabOrder = 7
          end
          object SXSkinButton61: TSXSkinButton
            Left = 176
            Top = 96
            Width = 113
            Height = 25
            OnClick = SXSkinButton61Click
            Caption = 'Enable CheckBox'
            SkinLibrary = SXSkinLibrary1
            TabOrder = 8
          end
        end
        object RadioButtonPage: TSXSkinNotebookPage
          Width = 671
          Height = 607
          Align = alClient
          SkinLibrary = SXSkinLibrary1
          TabOrder = 2
          Visible = False
          object SXSkinGroupBox6: TSXSkinGroupBox
            Left = 8
            Top = 8
            Width = 209
            Height = 161
            Caption = 'This group box contains radio buttons'
            SkinLibrary = SXSkinLibrary1
            TabOrder = 0
            object SXSkinRadioButton11: TSXSkinRadioButton
              Left = 8
              Top = 24
              Width = 158
              Height = 15
              Caption = 'Simple single line radio button'
              SkinLibrary = SXSkinLibrary1
              TabOrder = 0
            end
            object SXSkinRadioButton12: TSXSkinRadioButton
              Left = 8
              Top = 48
              Width = 150
              Height = 15
              Caption = 'This radio button is disabled'
              Enabled = False
              SkinLibrary = SXSkinLibrary1
              TabOrder = 1
            end
            object SXSkinRadioButton13: TSXSkinRadioButton
              Left = 8
              Top = 72
              Width = 179
              Height = 41
              Checked = True
              Caption = 
                'When created this radio button'#13#10'was checked. Is it checked now?'#13 +
                #10'P.S. Also this one is multiline.'
              SkinLibrary = SXSkinLibrary1
              TabOrder = 2
            end
            object SXSkinRadioButton14: TSXSkinRadioButton
              Left = 27
              Top = 120
              Width = 137
              Height = 28
              Caption = 'Another radio button with'#13#10'a glyph on right, not left'
              GlyphPosition = gpRight
              SkinLibrary = SXSkinLibrary1
              TabOrder = 3
            end
          end
          object SXSkinRadioButton1: TSXSkinRadioButton
            Left = 264
            Top = 8
            Width = 87
            Height = 43
            Checked = True
            Alignment = taCenter
            Caption = 'Radio button with'#13#10'Glyph on Top'
            GlyphPosition = gpTop
            SkinLibrary = SXSkinLibrary1
            TabOrder = 1
          end
          object SXSkinRadioButton2: TSXSkinRadioButton
            Left = 264
            Top = 72
            Width = 87
            Height = 43
            Alignment = taCenter
            Caption = 'Radio button with'#13#10'Glyph on Bottom'
            GlyphPosition = gpTop
            SkinLibrary = SXSkinLibrary1
            TabOrder = 2
          end
        end
        object ButtonPage: TSXSkinNotebookPage
          Width = 671
          Height = 607
          Align = alClient
          SkinLibrary = SXSkinLibrary1
          TabOrder = 3
          Visible = False
          DesignSize = (
            671
            607)
          object SXSkinButton1: TSXSkinButton
            Left = 8
            Top = 16
            Width = 225
            Height = 41
            Caption = 'Button with Glyph on the Left Side'
            GlyphStyle = 'MyComputer'
            SkinLibrary = SXSkinLibrary1
            TabOrder = 0
          end
          object SXSkinButton3: TSXSkinButton
            Left = 8
            Top = 64
            Width = 225
            Height = 57
            Caption = 
              'Button with Glyph on the Right Side.'#13#10'Also it is multiline. This' +
              ' is a 2nd line.'
            GlyphPosition = gpRight
            GlyphStyle = 'MyComputer'
            SkinLibrary = SXSkinLibrary1
            TabOrder = 1
          end
          object SXSkinButton2: TSXSkinButton
            Left = 9
            Top = 216
            Width = 224
            Height = 73
            CanBeChecked = True
            Caption = 
              'This button has CanBeChecked propety set to True. Glyph is the d' +
              'efault for CheckBoxes.'
            GlyphStyle = '_MultiStateCheck_CheckBox'
            SkinLibrary = SXSkinLibrary1
            TabOrder = 4
            WordWrap = True
          end
          object SXSkinButton4: TSXSkinButton
            Left = 239
            Top = 15
            Width = 137
            Height = 105
            Caption = 'Button with Glyph on Top'
            GlyphPosition = gpTop
            GlyphStyle = 'MyComputer'
            SkinLibrary = SXSkinLibrary1
            TabOrder = 6
          end
          object SXSkinButton5: TSXSkinButton
            Left = 8
            Top = 128
            Width = 225
            Height = 25
            Caption = 'This button is disabled'
            Enabled = False
            SkinLibrary = SXSkinLibrary1
            TabOrder = 2
          end
          object SXSkinButton10: TSXSkinButton
            Left = 9
            Top = 159
            Width = 224
            Height = 49
            Caption = 'This button has a MultiState Glyph Style'
            GlyphStyle = '_MultiState_TestGlyph'
            SkinLibrary = SXSkinLibrary1
            TabOrder = 3
          end
          object SXSkinGroupBox5: TSXSkinGroupBox
            Left = 487
            Top = 443
            Width = 175
            Height = 153
            Anchors = [akRight, akBottom]
            Caption = 'Commands'
            CaptionAlignment = taCenter
            SkinLibrary = SXSkinLibrary1
            TabOrder = 9
            DesignSize = (
              175
              153)
            object SXSkinButton57: TSXSkinButton
              Left = 8
              Top = 56
              Width = 159
              Height = 25
              Anchors = [akLeft, akTop, akRight]
              Caption = 'COMMAND 2'
              Enabled = False
              SkinLibrary = SXSkinLibrary1
              TabOrder = 1
            end
            object SXSkinButton17: TSXSkinButton
              Left = 8
              Top = 88
              Width = 159
              Height = 25
              Anchors = [akLeft, akTop, akRight]
              Caption = 'COMMAND 3'
              SkinLibrary = SXSkinLibrary1
              TabOrder = 2
            end
            object SXSkinButton18: TSXSkinButton
              Left = 3
              Top = 125
              Width = 168
              Height = 25
              Align = alBottom
              Caption = 'COMMAND 4 (bottom-aligned)'
              SkinLibrary = SXSkinLibrary1
              TabOrder = 3
            end
            object SXSkinButton19: TSXSkinButton
              Left = 3
              Top = 23
              Width = 168
              Height = 25
              Align = alTop
              Caption = 'COMMAND 1 (top-aligned)'
              SkinLibrary = SXSkinLibrary1
              TabOrder = 0
            end
          end
          object SXSkinGroupBox1: TSXSkinGroupBox
            Left = 487
            Top = 283
            Width = 175
            Height = 153
            Anchors = [akRight, akBottom]
            Caption = 'Commands'
            CaptionAlignment = taCenter
            SkinLibrary = SXSkinLibrary1
            TabOrder = 8
            object SXSkinButton11: TSXSkinButton
              Left = 3
              Top = 48
              Width = 168
              Height = 25
              Align = alTop
              Caption = 'COMMAND 2 (top-aligned)'
              Enabled = False
              SkinLibrary = SXSkinLibrary1
              TabOrder = 1
            end
            object SXSkinButton12: TSXSkinButton
              Left = 3
              Top = 73
              Width = 168
              Height = 25
              Align = alTop
              Caption = 'COMMAND 3 (top-aligned)'
              SkinLibrary = SXSkinLibrary1
              TabOrder = 2
            end
            object SXSkinButton13: TSXSkinButton
              Left = 3
              Top = 98
              Width = 168
              Height = 25
              Align = alTop
              Caption = 'COMMAND 4 (top-aligned)'
              SkinLibrary = SXSkinLibrary1
              TabOrder = 3
            end
            object SXSkinButton14: TSXSkinButton
              Left = 3
              Top = 23
              Width = 168
              Height = 25
              Align = alTop
              Caption = 'COMMAND 1 (top-aligned)'
              SkinLibrary = SXSkinLibrary1
              TabOrder = 0
            end
          end
          object SXSkinButton15: TSXSkinButton
            Left = 8
            Top = 296
            Width = 225
            Height = 45
            Caption = 'DropDown Button'
            DropDown = True
            DropDownMenu = PopupMenu1
            GlyphStyle = 'MyComputer'
            SkinLibrary = SXSkinLibrary1
            TabOrder = 5
          end
          object SXSkinButton16: TSXSkinButton
            Left = 240
            Top = 128
            Width = 137
            Height = 45
            Caption = 'Disabled drop-'#13#10'down Button'
            DropDown = True
            DropDownMenu = PopupMenu1
            Enabled = False
            GlyphStyle = 'MyComputer'
            SkinLibrary = SXSkinLibrary1
            TabOrder = 7
          end
          object SXSkinButton20: TSXSkinButton
            Left = 240
            Top = 179
            Width = 137
            Height = 30
            OnClick = SXSkinButton20Click
            Caption = 'Enable Button'
            SkinLibrary = SXSkinLibrary1
            TabOrder = 10
          end
        end
        object ImagePage: TSXSkinNotebookPage
          Width = 671
          Height = 607
          Align = alClient
          SkinLibrary = SXSkinLibrary1
          TabOrder = 4
          Visible = False
          object SXSkinImage1: TSXSkinImage
            Left = 8
            Top = 24
            Width = 240
            Height = 120
            SkinLibrary = SXSkinLibrary1
            SkinStyle = 'MyComputer'
            TabOrder = 0
          end
          object SXSkinLabel19: TSXSkinLabel
            Left = 8
            Top = 8
            Width = 165
            Height = 13
            Caption = 'Size of this image is 240x120 pixels'
            SkinLibrary = SXSkinLibrary1
          end
          object SXSkinLabel20: TSXSkinLabel
            Left = 304
            Top = 8
            Width = 153
            Height = 13
            Caption = 'Size of this image is 24x24 pixels'
            SkinLibrary = SXSkinLibrary1
          end
          object SXSkinImage4: TSXSkinImage
            Left = 304
            Top = 24
            Width = 24
            Height = 24
            SkinLibrary = SXSkinLibrary1
            SkinStyle = 'MyComputer'
            TabOrder = 3
          end
          object SXSkinLabel21: TSXSkinLabel
            Left = 304
            Top = 72
            Width = 238
            Height = 39
            Caption = 
              'Both these images use the same skin style, which'#13#10'uses file myco' +
              'mputer.png (24x24) to draw controls.'#13#10'ResizeMode property of the' +
              ' skin style is set to Tile.'
            SkinLibrary = SXSkinLibrary1
          end
          object SXSkinLabel22: TSXSkinLabel
            Left = 8
            Top = 176
            Width = 341
            Height = 13
            Caption = 
              'These skins also use the same file mycomputer.png, but with filt' +
              'ers used:'
            SkinLibrary = SXSkinLibrary1
          end
          object SXSkinPanel6: TSXSkinPanel
            Left = 3
            Top = 198
            Width = 609
            Height = 49
            SkinLibrary = SXSkinLibrary1
            TabOrder = 6
            object SXSkinPanel8: TSXSkinPanel
              Left = 55
              Width = 51
              Height = 49
              Align = alLeft
              SkinLibrary = SXSkinLibrary1
              TabOrder = 0
              object SXSkinLabel23: TSXSkinLabel
                Top = 36
                Width = 51
                Height = 13
                Align = alBottom
                Caption = 'Lighten'
                SkinLibrary = SXSkinLibrary1
                TextLeftOffset = 8
                TextRightOffset = 8
              end
              object SXSkinImage6: TSXSkinImage
                Left = 14
                Top = 3
                Width = 24
                Height = 24
                SkinLibrary = SXSkinLibrary1
                SkinStyle = 'MyComputer_F1'
                TabOrder = 1
              end
            end
            object SXSkinPanel10: TSXSkinPanel
              Width = 55
              Height = 49
              Align = alLeft
              SkinLibrary = SXSkinLibrary1
              TabOrder = 1
              object SXSkinLabel27: TSXSkinLabel
                Top = 36
                Width = 55
                Height = 13
                Align = alBottom
                Caption = 'No Filter'
                SkinLibrary = SXSkinLibrary1
                TextLeftOffset = 8
                TextRightOffset = 8
              end
              object SXSkinImage9: TSXSkinImage
                Left = 15
                Top = 3
                Width = 24
                Height = 24
                SkinLibrary = SXSkinLibrary1
                SkinStyle = 'MyComputer'
                TabOrder = 1
              end
            end
            object SXSkinPanel12: TSXSkinPanel
              Left = 189
              Width = 78
              Height = 49
              Align = alLeft
              SkinLibrary = SXSkinLibrary1
              TabOrder = 2
              object SXSkinLabel28: TSXSkinLabel
                Top = 36
                Width = 78
                Height = 13
                Align = alBottom
                Caption = 'LightenVertG'
                SkinLibrary = SXSkinLibrary1
                TextLeftOffset = 8
                TextRightOffset = 8
              end
              object SXSkinImage10: TSXSkinImage
                Left = 27
                Top = 3
                Width = 24
                Height = 24
                SkinLibrary = SXSkinLibrary1
                SkinStyle = 'MyComputer_F3'
                TabOrder = 1
              end
            end
            object SXSkinPanel13: TSXSkinPanel
              Left = 106
              Width = 83
              Height = 49
              Align = alLeft
              SkinLibrary = SXSkinLibrary1
              TabOrder = 3
              object SXSkinLabel29: TSXSkinLabel
                Top = 36
                Width = 83
                Height = 13
                Align = alBottom
                Caption = 'LightenHorizG'
                SkinLibrary = SXSkinLibrary1
                TextLeftOffset = 8
                TextRightOffset = 8
              end
              object SXSkinImage11: TSXSkinImage
                Left = 29
                Top = 3
                Width = 24
                Height = 24
                SkinLibrary = SXSkinLibrary1
                SkinStyle = 'MyComputer_F2'
                TabOrder = 1
              end
            end
            object SXSkinPanel14: TSXSkinPanel
              Left = 267
              Width = 51
              Height = 49
              Align = alLeft
              SkinLibrary = SXSkinLibrary1
              TabOrder = 4
              object SXSkinLabel24: TSXSkinLabel
                Top = 36
                Width = 51
                Height = 13
                Align = alBottom
                Caption = 'Darken'
                SkinLibrary = SXSkinLibrary1
                TextLeftOffset = 8
                TextRightOffset = 8
              end
              object SXSkinImage5: TSXSkinImage
                Left = 14
                Top = 3
                Width = 24
                Height = 24
                SkinLibrary = SXSkinLibrary1
                SkinStyle = 'MyComputer_F4'
                TabOrder = 1
              end
            end
            object SXSkinPanel15: TSXSkinPanel
              Left = 318
              Width = 83
              Height = 49
              Align = alLeft
              SkinLibrary = SXSkinLibrary1
              TabOrder = 5
              object SXSkinLabel25: TSXSkinLabel
                Top = 36
                Width = 83
                Height = 13
                Align = alBottom
                Caption = 'DarkenHorizG'
                SkinLibrary = SXSkinLibrary1
                TextLeftOffset = 8
                TextRightOffset = 8
              end
              object SXSkinImage7: TSXSkinImage
                Left = 29
                Top = 3
                Width = 24
                Height = 24
                SkinLibrary = SXSkinLibrary1
                SkinStyle = 'MyComputer_F5'
                TabOrder = 1
              end
            end
            object SXSkinPanel16: TSXSkinPanel
              Left = 401
              Width = 78
              Height = 49
              Align = alLeft
              SkinLibrary = SXSkinLibrary1
              TabOrder = 6
              object SXSkinLabel26: TSXSkinLabel
                Top = 36
                Width = 78
                Height = 13
                Align = alBottom
                Caption = 'DarkenVertG'
                SkinLibrary = SXSkinLibrary1
                TextLeftOffset = 8
                TextRightOffset = 8
              end
              object SXSkinImage8: TSXSkinImage
                Left = 27
                Top = 3
                Width = 24
                Height = 24
                SkinLibrary = SXSkinLibrary1
                SkinStyle = 'MyComputer_F6'
                TabOrder = 1
              end
            end
            object SXSkinPanel17: TSXSkinPanel
              Left = 479
              Width = 43
              Height = 49
              Align = alLeft
              SkinLibrary = SXSkinLibrary1
              TabOrder = 7
              object SXSkinLabel30: TSXSkinLabel
                Top = 36
                Width = 43
                Height = 13
                Align = alBottom
                Caption = 'Alpha'
                SkinLibrary = SXSkinLibrary1
                TextLeftOffset = 8
                TextRightOffset = 8
              end
              object SXSkinImage12: TSXSkinImage
                Left = 10
                Top = 3
                Width = 24
                Height = 24
                SkinLibrary = SXSkinLibrary1
                SkinStyle = 'MyComputer_F7'
                TabOrder = 1
              end
            end
            object SXSkinPanel1: TSXSkinPanel
              Left = 522
              Width = 78
              Height = 49
              Align = alLeft
              SkinLibrary = SXSkinLibrary1
              TabOrder = 8
              object SXSkinLabel2: TSXSkinLabel
                Top = 36
                Width = 78
                Height = 13
                Align = alBottom
                Caption = 'Monochrome'
                SkinLibrary = SXSkinLibrary1
                TextLeftOffset = 8
                TextRightOffset = 8
              end
              object SXSkinImage13: TSXSkinImage
                Left = 27
                Top = 3
                Width = 24
                Height = 24
                SkinLibrary = SXSkinLibrary1
                SkinStyle = 'MyComputer_F8'
                TabOrder = 1
              end
            end
          end
          object SXSkinPanel2: TSXSkinPanel
            Left = 3
            Top = 253
            Width = 609
            Height = 49
            SkinLibrary = SXSkinLibrary1
            TabOrder = 7
            object SXSkinPanel5: TSXSkinPanel
              Width = 76
              Height = 49
              Align = alLeft
              SkinLibrary = SXSkinLibrary1
              TabOrder = 0
              object SXSkinLabel31: TSXSkinLabel
                Top = 36
                Width = 76
                Height = 13
                Align = alBottom
                Caption = 'ColorOverlay'
                SkinLibrary = SXSkinLibrary1
                TextLeftOffset = 8
                TextRightOffset = 8
              end
              object SXSkinImage15: TSXSkinImage
                Left = 26
                Top = 3
                Width = 24
                Height = 24
                SkinLibrary = SXSkinLibrary1
                SkinStyle = 'MyComputer_F9'
                TabOrder = 1
              end
            end
            object SXSkinPanel23: TSXSkinPanel
              Left = 76
              Width = 108
              Height = 49
              Align = alLeft
              SkinLibrary = SXSkinLibrary1
              TabOrder = 1
              object SXSkinLabel37: TSXSkinLabel
                Top = 36
                Width = 108
                Height = 13
                Align = alBottom
                Caption = 'ColorOverlayHorizG'
                SkinLibrary = SXSkinLibrary1
                TextLeftOffset = 8
                TextRightOffset = 8
              end
              object SXSkinImage21: TSXSkinImage
                Left = 42
                Top = 3
                Width = 24
                Height = 24
                SkinLibrary = SXSkinLibrary1
                SkinStyle = 'MyComputer_F10'
                TabOrder = 1
              end
            end
            object SXSkinPanel24: TSXSkinPanel
              Left = 184
              Width = 103
              Height = 49
              Align = alLeft
              SkinLibrary = SXSkinLibrary1
              TabOrder = 2
              object SXSkinLabel38: TSXSkinLabel
                Top = 36
                Width = 103
                Height = 13
                Align = alBottom
                Caption = 'ColorOverlayVertG'
                SkinLibrary = SXSkinLibrary1
                TextLeftOffset = 8
                TextRightOffset = 8
              end
              object SXSkinImage22: TSXSkinImage
                Left = 39
                Top = 3
                Width = 24
                Height = 24
                SkinLibrary = SXSkinLibrary1
                SkinStyle = 'MyComputer_F11'
                TabOrder = 1
              end
            end
          end
          object SXSkinLabel78: TSXSkinLabel
            Left = 8
            Top = 352
            Width = 384
            Height = 13
            Caption = 
              'The same image is used in these skins, but with stretching and v' +
              'arious filters used'
            SkinLibrary = SXSkinLibrary1
          end
          object SXSkinImage17: TSXSkinImage
            Left = 8
            Top = 376
            Width = 100
            Height = 100
            SkinLibrary = SXSkinLibrary1
            SkinStyle = 'MyComputer_S1'
            TabOrder = 9
          end
          object SXSkinLabel79: TSXSkinLabel
            Left = 8
            Top = 488
            Width = 100
            Height = 13
            Alignment = taCenter
            AutoSizeWidth = False
            Caption = 'Nearest'
            SkinLibrary = SXSkinLibrary1
          end
          object SXSkinImage18: TSXSkinImage
            Left = 136
            Top = 376
            Width = 100
            Height = 100
            SkinLibrary = SXSkinLibrary1
            SkinStyle = 'MyComputer_S2'
            TabOrder = 11
          end
          object SXSkinLabel80: TSXSkinLabel
            Left = 136
            Top = 488
            Width = 100
            Height = 13
            Alignment = taCenter
            AutoSizeWidth = False
            Caption = 'Linear'
            SkinLibrary = SXSkinLibrary1
          end
          object SXSkinImage19: TSXSkinImage
            Left = 264
            Top = 376
            Width = 100
            Height = 100
            SkinLibrary = SXSkinLibrary1
            SkinStyle = 'MyComputer_S3'
            TabOrder = 13
          end
          object SXSkinLabel81: TSXSkinLabel
            Left = 264
            Top = 488
            Width = 100
            Height = 13
            Alignment = taCenter
            AutoSizeWidth = False
            Caption = 'Spline'
            SkinLibrary = SXSkinLibrary1
          end
          object SXSkinImage20: TSXSkinImage
            Left = 392
            Top = 376
            Width = 100
            Height = 100
            SkinLibrary = SXSkinLibrary1
            SkinStyle = 'MyComputer_S4'
            TabOrder = 15
          end
          object SXSkinLabel82: TSXSkinLabel
            Left = 392
            Top = 488
            Width = 100
            Height = 13
            Alignment = taCenter
            AutoSizeWidth = False
            Caption = 'Mitchell'
            SkinLibrary = SXSkinLibrary1
          end
          object SXSkinImage23: TSXSkinImage
            Left = 528
            Top = 376
            Width = 100
            Height = 100
            SkinLibrary = SXSkinLibrary1
            SkinStyle = 'MyComputer_S5'
            TabOrder = 17
          end
          object SXSkinLabel83: TSXSkinLabel
            Left = 528
            Top = 488
            Width = 100
            Height = 13
            Alignment = taCenter
            AutoSizeWidth = False
            Caption = 'Lanczos'
            SkinLibrary = SXSkinLibrary1
          end
          object SXSkinImage24: TSXSkinImage
            Left = 88
            Top = 464
            Width = 18
            Height = 18
            SkinLibrary = SXSkinLibrary1
            SkinStyle = 'MyComputer_S1'
            TabOrder = 19
          end
          object SXSkinImage25: TSXSkinImage
            Left = 216
            Top = 464
            Width = 18
            Height = 18
            SkinLibrary = SXSkinLibrary1
            SkinStyle = 'MyComputer_S2'
            TabOrder = 20
          end
          object SXSkinImage26: TSXSkinImage
            Left = 344
            Top = 464
            Width = 18
            Height = 18
            SkinLibrary = SXSkinLibrary1
            SkinStyle = 'MyComputer_S3'
            TabOrder = 21
          end
          object SXSkinImage27: TSXSkinImage
            Left = 472
            Top = 464
            Width = 18
            Height = 18
            SkinLibrary = SXSkinLibrary1
            SkinStyle = 'MyComputer_S4'
            TabOrder = 22
          end
          object SXSkinImage28: TSXSkinImage
            Left = 608
            Top = 464
            Width = 18
            Height = 18
            SkinLibrary = SXSkinLibrary1
            SkinStyle = 'MyComputer_S5'
            TabOrder = 23
          end
        end
        object GroupBoxPage: TSXSkinNotebookPage
          Width = 671
          Height = 607
          Align = alClient
          SkinLibrary = SXSkinLibrary1
          TabOrder = 5
          Visible = False
          object SXSkinGroupBox4: TSXSkinGroupBox
            Left = 8
            Top = 8
            Width = 249
            Height = 66
            Caption = 'GroupBox with Glyph on Left'
            GlyphStyle = 'MyComputer'
            SkinLibrary = SXSkinLibrary1
            TabOrder = 0
            object SXSkinCheckBox1: TSXSkinCheckBox
              Left = 10
              Top = 40
              Width = 148
              Height = 15
              Caption = 'CheckBox in the GroupBox'
              SkinLibrary = SXSkinLibrary1
              TabOrder = 0
            end
          end
          object SXSkinGroupBox2: TSXSkinGroupBox
            Left = 8
            Top = 88
            Width = 249
            Height = 66
            Caption = 'GroupBox with Glyph on Right'
            GlyphPosition = gbgpRight
            GlyphStyle = 'MyComputer'
            SkinLibrary = SXSkinLibrary1
            TabOrder = 1
            object SXSkinCheckBox2: TSXSkinCheckBox
              Left = 10
              Top = 40
              Width = 148
              Height = 15
              Caption = 'CheckBox in the GroupBox'
              SkinLibrary = SXSkinLibrary1
              TabOrder = 0
            end
          end
          object SXSkinGroupBox3: TSXSkinGroupBox
            Left = 263
            Top = 8
            Width = 249
            Height = 66
            Alignment = taCenter
            Caption = 
              'GroupBox with multiline caption'#13#10'and center-aligned text and cap' +
              'tion'
            CaptionAlignment = taCenter
            GlyphStyle = 'MyComputer'
            GlyphType = gtSimple
            SkinLibrary = SXSkinLibrary1
            TabOrder = 2
            object SXSkinCheckBox13: TSXSkinCheckBox
              Left = 10
              Top = 40
              Width = 148
              Height = 15
              Caption = 'CheckBox in the GroupBox'
              SkinLibrary = SXSkinLibrary1
              TabOrder = 0
            end
          end
          object SXSkinGroupBox7: TSXSkinGroupBox
            Left = 264
            Top = 88
            Width = 249
            Height = 66
            Alignment = taRightJustify
            Caption = 'GroupBox with no Glyph and'#13#10'with right-aligned caption'
            CaptionAlignment = taRightJustify
            GlyphPosition = gbgpRight
            SkinLibrary = SXSkinLibrary1
            TabOrder = 3
            object SXSkinCheckBox5: TSXSkinCheckBox
              Left = 10
              Top = 40
              Width = 148
              Height = 15
              Caption = 'CheckBox in the GroupBox'
              SkinLibrary = SXSkinLibrary1
              TabOrder = 0
            end
          end
          object SXSkinGroupBox8: TSXSkinGroupBox
            Left = 8
            Top = 168
            Width = 249
            Height = 66
            Alignment = taRightJustify
            Caption = 'Disabled GroupBox'
            Enabled = False
            GlyphPosition = gbgpRight
            SkinLibrary = SXSkinLibrary1
            TabOrder = 4
            object SXSkinCheckBox14: TSXSkinCheckBox
              Left = 10
              Top = 40
              Width = 148
              Height = 15
              Caption = 'CheckBox in the GroupBox'
              Enabled = False
              SkinLibrary = SXSkinLibrary1
              TabOrder = 0
            end
          end
          object SXSkinGroupBox9: TSXSkinGroupBox
            Left = 8
            Top = 248
            Width = 249
            Height = 66
            Alignment = taRightJustify
            Caption = 'GroupBox with CheckBox Glyph'
            GlyphStyle = '_CheckBox'
            GlyphType = gtCheckBox
            SkinLibrary = SXSkinLibrary1
            State = cbChecked
            TabOrder = 5
            TabStop = True
            object SXSkinCheckBox15: TSXSkinCheckBox
              Left = 10
              Top = 40
              Width = 148
              Height = 15
              Caption = 'CheckBox in the GroupBox'
              SkinLibrary = SXSkinLibrary1
              TabOrder = 0
            end
          end
          object SXSkinGroupBox10: TSXSkinGroupBox
            Left = 264
            Top = 168
            Width = 249
            Height = 66
            Alignment = taRightJustify
            Caption = 'GroupBox with RadioButton Glyph'
            GlyphStyle = '_RadioButton'
            GlyphType = gtRadioButton
            SkinLibrary = SXSkinLibrary1
            State = cbChecked
            TabOrder = 6
            TabStop = True
            object SXSkinCheckBox16: TSXSkinCheckBox
              Left = 10
              Top = 40
              Width = 148
              Height = 15
              Caption = 'CheckBox in the GroupBox'
              SkinLibrary = SXSkinLibrary1
              TabOrder = 0
            end
          end
          object SXSkinRadioButton3: TSXSkinRadioButton
            Left = 273
            Top = 240
            Width = 134
            Height = 15
            Caption = 'Another RadioButton #1'
            SkinLibrary = SXSkinLibrary1
            TabOrder = 7
          end
          object SXSkinRadioButton4: TSXSkinRadioButton
            Left = 273
            Top = 260
            Width = 134
            Height = 15
            Caption = 'Another RadioButton #2'
            SkinLibrary = SXSkinLibrary1
            TabOrder = 8
          end
          object SXSkinRadioButton5: TSXSkinRadioButton
            Left = 273
            Top = 280
            Width = 134
            Height = 15
            Caption = 'Another RadioButton #3'
            SkinLibrary = SXSkinLibrary1
            TabOrder = 9
          end
          object SXSkinGroupBox11: TSXSkinGroupBox
            Left = 8
            Top = 320
            Width = 249
            Height = 66
            Caption = 'GroupBox with MultiState Glyph'
            GlyphStyle = '_MultiState_TestGlyph'
            SkinLibrary = SXSkinLibrary1
            TabOrder = 10
            object SXSkinCheckBox17: TSXSkinCheckBox
              Left = 10
              Top = 40
              Width = 148
              Height = 15
              Caption = 'CheckBox in the GroupBox'
              SkinLibrary = SXSkinLibrary1
              TabOrder = 0
            end
          end
        end
        object EditPage: TSXSkinNotebookPage
          Width = 671
          Height = 607
          Align = alClient
          SkinLibrary = SXSkinLibrary1
          TabOrder = 6
          Visible = False
          object SXSkinEdit1: TSXSkinEdit
            Left = 8
            Top = 48
            Width = 153
            Height = 23
            TabOrder = 1
            Color = clWhite
            Enabled = False
            SkinLibrary = SXSkinLibrary1
            Text = 'Disabled Edit'
          end
          object SXSkinEdit3: TSXSkinEdit
            Left = 8
            Top = 80
            Width = 153
            Height = 49
            TabOrder = 2
            AutoSizeHeight = False
            SkinLibrary = SXSkinLibrary1
            Text = 'Edit with no AutoSize set'
          end
          object SXSkinEdit5: TSXSkinEdit
            Left = 8
            Top = 136
            Width = 153
            Height = 32
            TabOrder = 3
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clGreen
            Font.Height = -20
            Font.Name = 'Times New Roman'
            Font.Style = []
            ParentFont = False
            SkinLibrary = SXSkinLibrary1
            Text = 'CustomFontEdit'
          end
          object SXSkinEdit2: TSXSkinEdit
            Left = 8
            Top = 16
            Width = 153
            Height = 23
            TabOrder = 0
            SkinLibrary = SXSkinLibrary1
            Text = 'Simple AutoSized Edit'
          end
          object SXSkinLabel3: TSXSkinLabel
            Left = 200
            Top = 96
            Width = 77
            Height = 13
            Caption = 'Enter Password:'
            SkinLibrary = SXSkinLibrary1
          end
          object SXSkinEdit6: TSXSkinEdit
            Left = 200
            Top = 112
            Width = 121
            Height = 23
            TabOrder = 5
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
            Password = True
            SkinLibrary = SXSkinLibrary1
            Text = 'SXSkinEdit6'
          end
          object SXSkinEdit4: TSXSkinEdit
            Left = 200
            Top = 144
            Width = 121
            Height = 23
            TabOrder = 6
            ReadOnly = True
            SkinLibrary = SXSkinLibrary1
            Text = 'ReadOnly Edit'
          end
          object SXSkinButton62: TSXSkinButton
            Left = 200
            Top = 48
            Width = 121
            Height = 25
            OnClick = SXSkinButton62Click
            Caption = 'Enable Edit'
            SkinLibrary = SXSkinLibrary1
            TabOrder = 7
          end
        end
        object SlideTransformPage: TSXSkinNotebookPage
          Width = 671
          Height = 607
          Align = alClient
          SkinLibrary = SXSkinLibrary1
          TabOrder = 7
          Visible = False
          object SXSkinButton21: TSXSkinButton
            Left = 8
            Top = 8
            Width = 217
            Height = 33
            Caption = 'Double Slide RightBottom and Back'
            GlyphStyle = 'MyComputer'
            SkinLibrary = SXSkinLibrary1
            SkinStyle = '_Button_TE1'
            TabOrder = 0
          end
          object SXSkinButton22: TSXSkinButton
            Left = 8
            Top = 48
            Width = 217
            Height = 33
            Caption = 'Double Slide RightBottom'
            GlyphStyle = 'MyComputer'
            SkinLibrary = SXSkinLibrary1
            SkinStyle = '_Button_TE2'
            TabOrder = 1
          end
          object SXSkinButton23: TSXSkinButton
            Left = 8
            Top = 88
            Width = 217
            Height = 33
            Caption = 'Double Slide Left and Back'
            GlyphStyle = 'MyComputer'
            SkinLibrary = SXSkinLibrary1
            SkinStyle = '_Button_TE3'
            TabOrder = 2
          end
          object SXSkinButton24: TSXSkinButton
            Left = 8
            Top = 128
            Width = 217
            Height = 33
            Caption = 'Double Slide Left'
            GlyphStyle = 'MyComputer'
            SkinLibrary = SXSkinLibrary1
            SkinStyle = '_Button_TE4'
            TabOrder = 3
          end
          object SXSkinButton25: TSXSkinButton
            Left = 8
            Top = 168
            Width = 217
            Height = 33
            Caption = 'Double Slide Top and Back'
            GlyphStyle = 'MyComputer'
            SkinLibrary = SXSkinLibrary1
            SkinStyle = '_Button_TE5'
            TabOrder = 4
          end
          object SXSkinButton26: TSXSkinButton
            Left = 8
            Top = 208
            Width = 217
            Height = 33
            Caption = 'Double Slide Top'
            GlyphStyle = 'MyComputer'
            SkinLibrary = SXSkinLibrary1
            SkinStyle = '_Button_TE6'
            TabOrder = 5
          end
          object SXSkinButton27: TSXSkinButton
            Left = 8
            Top = 328
            Width = 217
            Height = 33
            Caption = 'Double Slide LeftRight'
            GlyphStyle = 'MyComputer'
            SkinLibrary = SXSkinLibrary1
            SkinStyle = '_Button_TE7'
            TabOrder = 6
          end
          object SXSkinButton28: TSXSkinButton
            Left = 8
            Top = 368
            Width = 217
            Height = 33
            Caption = 'Double Slide TopBottom'
            GlyphStyle = 'MyComputer'
            SkinLibrary = SXSkinLibrary1
            SkinStyle = '_Button_TE8'
            TabOrder = 7
          end
          object SXSkinButton29: TSXSkinButton
            Left = 8
            Top = 288
            Width = 217
            Height = 33
            Caption = 'Slide Old TopBottom'
            GlyphStyle = 'MyComputer'
            SkinLibrary = SXSkinLibrary1
            SkinStyle = '_Button_TE9'
            TabOrder = 8
          end
          object SXSkinButton30: TSXSkinButton
            Left = 8
            Top = 248
            Width = 217
            Height = 33
            Caption = 'Slide Old LeftRight'
            GlyphStyle = 'MyComputer'
            SkinLibrary = SXSkinLibrary1
            SkinStyle = '_Button_TE10'
            TabOrder = 9
          end
          object SXSkinButton31: TSXSkinButton
            Left = 336
            Top = 8
            Width = 217
            Height = 33
            Caption = 'Slide RightBottom and Back'
            GlyphStyle = 'MyComputer'
            SkinLibrary = SXSkinLibrary1
            SkinStyle = '_Button_TE11'
            TabOrder = 10
          end
          object SXSkinButton32: TSXSkinButton
            Left = 336
            Top = 48
            Width = 217
            Height = 33
            Caption = 'Slide RightBottom'
            GlyphStyle = 'MyComputer'
            SkinLibrary = SXSkinLibrary1
            SkinStyle = '_Button_TE12'
            TabOrder = 11
          end
          object SXSkinButton33: TSXSkinButton
            Left = 336
            Top = 88
            Width = 217
            Height = 33
            Caption = 'Slide Left and Back'
            GlyphStyle = 'MyComputer'
            SkinLibrary = SXSkinLibrary1
            SkinStyle = '_Button_TE13'
            TabOrder = 12
          end
          object SXSkinButton34: TSXSkinButton
            Left = 336
            Top = 128
            Width = 217
            Height = 33
            Caption = 'Slide Left'
            GlyphStyle = 'MyComputer'
            SkinLibrary = SXSkinLibrary1
            SkinStyle = '_Button_TE14'
            TabOrder = 13
          end
          object SXSkinButton35: TSXSkinButton
            Left = 336
            Top = 168
            Width = 217
            Height = 33
            Caption = 'Slide Top and Back'
            GlyphStyle = 'MyComputer'
            SkinLibrary = SXSkinLibrary1
            SkinStyle = '_Button_TE15'
            TabOrder = 14
          end
          object SXSkinButton36: TSXSkinButton
            Left = 336
            Top = 208
            Width = 217
            Height = 33
            Caption = 'Slide Top'
            GlyphStyle = 'MyComputer'
            SkinLibrary = SXSkinLibrary1
            SkinStyle = '_Button_TE16'
            TabOrder = 15
          end
          object SXSkinButton37: TSXSkinButton
            Left = 336
            Top = 288
            Width = 217
            Height = 33
            Caption = 'Slide New TopBottom'
            GlyphStyle = 'MyComputer'
            SkinLibrary = SXSkinLibrary1
            SkinStyle = '_Button_TE17'
            TabOrder = 16
          end
          object SXSkinButton38: TSXSkinButton
            Left = 336
            Top = 248
            Width = 217
            Height = 33
            Caption = 'Slide New LeftRight'
            GlyphStyle = 'MyComputer'
            SkinLibrary = SXSkinLibrary1
            SkinStyle = '_Button_TE18'
            TabOrder = 17
          end
          object SXSkinButton39: TSXSkinButton
            Left = 336
            Top = 328
            Width = 217
            Height = 33
            Caption = 'Slide LeftRight and Back'
            GlyphStyle = 'MyComputer'
            SkinLibrary = SXSkinLibrary1
            SkinStyle = '_Button_TE19'
            TabOrder = 18
          end
          object SXSkinButton40: TSXSkinButton
            Left = 336
            Top = 368
            Width = 217
            Height = 33
            Caption = 'Slide TopBottom and Back'
            GlyphStyle = 'MyComputer'
            SkinLibrary = SXSkinLibrary1
            SkinStyle = '_Button_TE20'
            TabOrder = 19
          end
          object SXSkinButton41: TSXSkinButton
            Left = 8
            Top = 408
            Width = 217
            Height = 33
            Caption = 'Mixed: Slide Top and Blend'
            GlyphStyle = 'MyComputer'
            SkinLibrary = SXSkinLibrary1
            SkinStyle = '_Button_TE21'
            TabOrder = 20
          end
          object SXSkinButton52: TSXSkinButton
            Left = 336
            Top = 408
            Width = 217
            Height = 33
            Caption = 'Slide Transform with Caption'
            GlyphStyle = 'MyComputer'
            SkinLibrary = SXSkinLibrary1
            SkinStyle = '_Button_TE32'
            TabOrder = 21
          end
        end
        object OverDrawTransformPage: TSXSkinNotebookPage
          Width = 671
          Height = 607
          Align = alClient
          SkinLibrary = SXSkinLibrary1
          TabOrder = 8
          Visible = False
          object SXSkinButton42: TSXSkinButton
            Left = 8
            Top = 8
            Width = 217
            Height = 33
            Caption = 'OverDraw from Left'
            GlyphStyle = 'MyComputer'
            SkinLibrary = SXSkinLibrary1
            SkinStyle = '_Button_TE22'
            TabOrder = 0
          end
          object SXSkinButton43: TSXSkinButton
            Left = 8
            Top = 48
            Width = 217
            Height = 33
            Caption = 'OverDraw from Right'
            GlyphStyle = 'MyComputer'
            SkinLibrary = SXSkinLibrary1
            SkinStyle = '_Button_TE23'
            TabOrder = 1
          end
          object SXSkinButton44: TSXSkinButton
            Left = 8
            Top = 88
            Width = 217
            Height = 33
            Caption = 'OverDraw from Top'
            GlyphStyle = 'MyComputer'
            SkinLibrary = SXSkinLibrary1
            SkinStyle = '_Button_TE24'
            TabOrder = 2
          end
          object SXSkinButton45: TSXSkinButton
            Left = 8
            Top = 128
            Width = 217
            Height = 33
            Caption = 'OverDraw from Bottom'
            GlyphStyle = 'MyComputer'
            SkinLibrary = SXSkinLibrary1
            SkinStyle = '_Button_TE25'
            TabOrder = 3
          end
          object SXSkinButton46: TSXSkinButton
            Left = 8
            Top = 168
            Width = 217
            Height = 33
            Caption = 'OverDraw from TopLeft'
            GlyphStyle = 'MyComputer'
            SkinLibrary = SXSkinLibrary1
            SkinStyle = '_Button_TE26'
            TabOrder = 4
          end
          object SXSkinButton47: TSXSkinButton
            Left = 336
            Top = 8
            Width = 217
            Height = 33
            Caption = 'OverDraw from Left and Back'
            GlyphStyle = 'MyComputer'
            SkinLibrary = SXSkinLibrary1
            SkinStyle = '_Button_TE27'
            TabOrder = 7
          end
          object SXSkinButton48: TSXSkinButton
            Left = 336
            Top = 48
            Width = 217
            Height = 33
            Caption = 'OverDraw from Right and Back'
            GlyphStyle = 'MyComputer'
            SkinLibrary = SXSkinLibrary1
            SkinStyle = '_Button_TE28'
            TabOrder = 8
          end
          object SXSkinButton49: TSXSkinButton
            Left = 336
            Top = 88
            Width = 217
            Height = 33
            Caption = 'OverDraw from Top and Back'
            GlyphStyle = 'MyComputer'
            SkinLibrary = SXSkinLibrary1
            SkinStyle = '_Button_TE29'
            TabOrder = 9
          end
          object SXSkinButton50: TSXSkinButton
            Left = 336
            Top = 128
            Width = 217
            Height = 33
            Caption = 'OverDraw from Bottom and Back'
            GlyphStyle = 'MyComputer'
            SkinLibrary = SXSkinLibrary1
            SkinStyle = '_Button_TE30'
            TabOrder = 10
          end
          object SXSkinButton51: TSXSkinButton
            Left = 336
            Top = 168
            Width = 217
            Height = 33
            Caption = 'OverDraw from TopLeft and Back'
            GlyphStyle = 'MyComputer'
            SkinLibrary = SXSkinLibrary1
            SkinStyle = '_Button_TE31'
            TabOrder = 11
          end
          object SXSkinButton53: TSXSkinButton
            Left = 8
            Top = 208
            Width = 217
            Height = 33
            Caption = 'OverDraw from LeftRight'
            GlyphStyle = 'MyComputer'
            SkinLibrary = SXSkinLibrary1
            SkinStyle = '_Button_TE33'
            TabOrder = 5
          end
          object SXSkinButton54: TSXSkinButton
            Left = 8
            Top = 248
            Width = 217
            Height = 33
            Caption = 'OverDraw from TopBottom'
            GlyphStyle = 'MyComputer'
            SkinLibrary = SXSkinLibrary1
            SkinStyle = '_Button_TE34'
            TabOrder = 6
          end
          object SXSkinButton55: TSXSkinButton
            Left = 336
            Top = 208
            Width = 217
            Height = 33
            Caption = 'OverDraw from LeftRight and Back'
            GlyphStyle = 'MyComputer'
            SkinLibrary = SXSkinLibrary1
            SkinStyle = '_Button_TE35'
            TabOrder = 12
          end
          object SXSkinButton56: TSXSkinButton
            Left = 336
            Top = 248
            Width = 217
            Height = 33
            Caption = 'OverDraw from TopBottom and Back'
            GlyphStyle = 'MyComputer'
            SkinLibrary = SXSkinLibrary1
            SkinStyle = '_Button_TE36'
            TabOrder = 13
          end
        end
        object CustomEffectPage: TSXSkinNotebookPage
          Width = 671
          Height = 607
          Align = alClient
          SkinLibrary = SXSkinLibrary1
          TabOrder = 9
          Visible = False
          object SXSkinButton58: TSXSkinButton
            Left = 8
            Top = 24
            Width = 169
            Height = 41
            Caption = 'Button Caption'
            GlyphStyle = 'MyComputer'
            SkinLibrary = SXSkinLibrary1
            SkinStyle = '_Button_TestEffect1'
            TabOrder = 0
          end
          object SXSkinLabel4: TSXSkinLabel
            Left = 8
            Top = 8
            Width = 161
            Height = 13
            Caption = 'Only Transform Old Image Shown:'
            SkinLibrary = SXSkinLibrary1
          end
          object SXSkinLabel32: TSXSkinLabel
            Left = 192
            Top = 8
            Width = 167
            Height = 13
            Caption = 'Only Transform New Image Shown:'
            SkinLibrary = SXSkinLibrary1
          end
          object SXSkinButton59: TSXSkinButton
            Left = 192
            Top = 24
            Width = 169
            Height = 41
            Caption = 'Button Caption'
            GlyphStyle = 'MyComputer'
            SkinLibrary = SXSkinLibrary1
            SkinStyle = '_Button_TestEffect2'
            TabOrder = 3
          end
          object SXSkinLabel33: TSXSkinLabel
            Left = 416
            Top = 8
            Width = 137
            Height = 13
            Caption = 'Full-Drawn Transform Button:'
            SkinLibrary = SXSkinLibrary1
          end
          object SXSkinButton60: TSXSkinButton
            Left = 416
            Top = 24
            Width = 169
            Height = 41
            Caption = 'Button Caption'
            GlyphStyle = 'MyComputer'
            SkinLibrary = SXSkinLibrary1
            SkinStyle = '_Button_TestEffect3'
            TabOrder = 5
          end
          object SXSkinLabel34: TSXSkinLabel
            Left = 8
            Top = 88
            Width = 101
            Height = 13
            Caption = 'Old Image Transform:'
            SkinLibrary = SXSkinLibrary1
          end
          object SXSkinPanel4: TSXSkinPanel
            Left = 8
            Top = 104
            Width = 67
            Height = 90
            AutoSize = True
            SkinLibrary = SXSkinLibrary1
            TabOrder = 7
            object SXSkinRadioButton6: TSXSkinRadioButton
              Width = 45
              Height = 15
              OnUserModified = SXSkinRadioButton6UserModified
              Align = alTop
              Caption = 'None'
              SkinLibrary = SXSkinLibrary1
              TabOrder = 0
            end
            object SXSkinRadioButton7: TSXSkinRadioButton
              Top = 15
              Width = 46
              Height = 15
              OnUserModified = SXSkinRadioButton6UserModified
              Align = alTop
              Caption = 'Blend'
              SkinLibrary = SXSkinLibrary1
              TabOrder = 1
            end
            object SXSkinRadioButton8: TSXSkinRadioButton
              Top = 30
              Width = 43
              Height = 15
              OnUserModified = SXSkinRadioButton6UserModified
              Align = alTop
              Caption = 'Clear'
              SkinLibrary = SXSkinLibrary1
              TabOrder = 2
            end
            object SXSkinRadioButton9: TSXSkinRadioButton
              Top = 45
              Width = 43
              Height = 15
              Checked = True
              OnUserModified = SXSkinRadioButton6UserModified
              Align = alTop
              Caption = 'Fade'
              SkinLibrary = SXSkinLibrary1
              TabOrder = 3
            end
            object SXSkinRadioButton10: TSXSkinRadioButton
              Top = 60
              Width = 67
              Height = 15
              OnUserModified = SXSkinRadioButton6UserModified
              Align = alTop
              Caption = 'OverDraw'
              SkinLibrary = SXSkinLibrary1
              TabOrder = 4
            end
            object SXSkinRadioButton15: TSXSkinRadioButton
              Top = 75
              Width = 42
              Height = 15
              OnUserModified = SXSkinRadioButton6UserModified
              Align = alTop
              Caption = 'Slide'
              SkinLibrary = SXSkinLibrary1
              TabOrder = 5
            end
          end
          object SXSkinPanel18: TSXSkinPanel
            Left = 104
            Top = 104
            Width = 77
            Height = 150
            AutoSize = True
            SkinLibrary = SXSkinLibrary1
            TabOrder = 8
            object SXSkinRadioButton25: TSXSkinRadioButton
              Top = 135
              Width = 71
              Height = 15
              OnUserModified = SXSkinRadioButton6UserModified
              Align = alTop
              Caption = 'TopBottom'
              SkinLibrary = SXSkinLibrary1
              TabOrder = 0
            end
            object SXSkinRadioButton24: TSXSkinRadioButton
              Top = 120
              Width = 62
              Height = 15
              OnUserModified = SXSkinRadioButton6UserModified
              Align = alTop
              Caption = 'LeftRight'
              SkinLibrary = SXSkinLibrary1
              TabOrder = 1
            end
            object SXSkinRadioButton23: TSXSkinRadioButton
              Top = 105
              Width = 77
              Height = 15
              OnUserModified = SXSkinRadioButton6UserModified
              Align = alTop
              Caption = 'RightBottom'
              SkinLibrary = SXSkinLibrary1
              TabOrder = 2
            end
            object SXSkinRadioButton22: TSXSkinRadioButton
              Top = 90
              Width = 63
              Height = 15
              OnUserModified = SXSkinRadioButton6UserModified
              Align = alTop
              Caption = 'RightTop'
              SkinLibrary = SXSkinLibrary1
              TabOrder = 3
            end
            object SXSkinRadioButton21: TSXSkinRadioButton
              Top = 75
              Width = 70
              Height = 15
              OnUserModified = SXSkinRadioButton6UserModified
              Align = alTop
              Caption = 'LeftBottom'
              SkinLibrary = SXSkinLibrary1
              TabOrder = 4
            end
            object SXSkinRadioButton16: TSXSkinRadioButton
              Width = 37
              Height = 15
              Checked = True
              OnUserModified = SXSkinRadioButton6UserModified
              Align = alTop
              Caption = 'Left'
              SkinLibrary = SXSkinLibrary1
              TabOrder = 5
            end
            object SXSkinRadioButton17: TSXSkinRadioButton
              Top = 15
              Width = 44
              Height = 15
              OnUserModified = SXSkinRadioButton6UserModified
              Align = alTop
              Caption = 'Right'
              SkinLibrary = SXSkinLibrary1
              TabOrder = 6
            end
            object SXSkinRadioButton18: TSXSkinRadioButton
              Top = 30
              Width = 38
              Height = 15
              OnUserModified = SXSkinRadioButton6UserModified
              Align = alTop
              Caption = 'Top'
              SkinLibrary = SXSkinLibrary1
              TabOrder = 7
            end
            object SXSkinRadioButton19: TSXSkinRadioButton
              Top = 45
              Width = 52
              Height = 15
              OnUserModified = SXSkinRadioButton6UserModified
              Align = alTop
              Caption = 'Bottom'
              SkinLibrary = SXSkinLibrary1
              TabOrder = 8
            end
            object SXSkinRadioButton20: TSXSkinRadioButton
              Top = 60
              Width = 56
              Height = 15
              OnUserModified = SXSkinRadioButton6UserModified
              Align = alTop
              Caption = 'LeftTop'
              SkinLibrary = SXSkinLibrary1
              TabOrder = 9
            end
          end
          object SXSkinPanel19: TSXSkinPanel
            Left = 208
            Top = 104
            Width = 56
            Height = 45
            AutoSize = True
            SkinLibrary = SXSkinLibrary1
            TabOrder = 9
            object SXSkinCheckBox18: TSXSkinCheckBox
              Width = 37
              Height = 15
              OnUserModified = SXSkinRadioButton6UserModified
              Align = alTop
              Caption = 'Out'
              SkinLibrary = SXSkinLibrary1
              State = cbChecked
              TabOrder = 0
            end
            object SXSkinCheckBox19: TSXSkinCheckBox
              Top = 15
              Width = 47
              Height = 15
              OnUserModified = SXSkinRadioButton6UserModified
              Align = alTop
              Caption = 'Invert'
              SkinLibrary = SXSkinLibrary1
              TabOrder = 1
            end
            object SXSkinCheckBox20: TSXSkinCheckBox
              Top = 30
              Width = 56
              Height = 15
              OnUserModified = SXSkinRadioButton6UserModified
              Align = alTop
              Caption = 'Caption'
              SkinLibrary = SXSkinLibrary1
              State = cbChecked
              TabOrder = 2
            end
          end
          object SXSkinLabel36: TSXSkinLabel
            Left = 8
            Top = 72
            Width = 123
            Height = 13
            Caption = 'Transform on MouseOver:'
            SkinLibrary = SXSkinLibrary1
          end
          object SXSkinLabel35: TSXSkinLabel
            Left = 296
            Top = 88
            Width = 107
            Height = 13
            Caption = 'New Image Transform:'
            SkinLibrary = SXSkinLibrary1
          end
          object SXSkinPanel20: TSXSkinPanel
            Left = 296
            Top = 104
            Width = 67
            Height = 90
            AutoSize = True
            SkinLibrary = SXSkinLibrary1
            TabOrder = 12
            object SXSkinRadioButton26: TSXSkinRadioButton
              Width = 45
              Height = 15
              OnUserModified = SXSkinRadioButton6UserModified
              Align = alTop
              Caption = 'None'
              SkinLibrary = SXSkinLibrary1
              TabOrder = 0
            end
            object SXSkinRadioButton27: TSXSkinRadioButton
              Top = 15
              Width = 46
              Height = 15
              OnUserModified = SXSkinRadioButton6UserModified
              Align = alTop
              Caption = 'Blend'
              SkinLibrary = SXSkinLibrary1
              TabOrder = 1
            end
            object SXSkinRadioButton28: TSXSkinRadioButton
              Top = 30
              Width = 43
              Height = 15
              OnUserModified = SXSkinRadioButton6UserModified
              Align = alTop
              Caption = 'Clear'
              SkinLibrary = SXSkinLibrary1
              TabOrder = 2
            end
            object SXSkinRadioButton29: TSXSkinRadioButton
              Top = 45
              Width = 43
              Height = 15
              OnUserModified = SXSkinRadioButton6UserModified
              Align = alTop
              Caption = 'Fade'
              SkinLibrary = SXSkinLibrary1
              TabOrder = 3
            end
            object SXSkinRadioButton30: TSXSkinRadioButton
              Top = 60
              Width = 67
              Height = 15
              OnUserModified = SXSkinRadioButton6UserModified
              Align = alTop
              Caption = 'OverDraw'
              SkinLibrary = SXSkinLibrary1
              TabOrder = 4
            end
            object SXSkinRadioButton31: TSXSkinRadioButton
              Top = 75
              Width = 42
              Height = 15
              Checked = True
              OnUserModified = SXSkinRadioButton6UserModified
              Align = alTop
              Caption = 'Slide'
              SkinLibrary = SXSkinLibrary1
              TabOrder = 5
            end
          end
          object SXSkinPanel21: TSXSkinPanel
            Left = 392
            Top = 104
            Width = 77
            Height = 150
            AutoSize = True
            SkinLibrary = SXSkinLibrary1
            TabOrder = 13
            object SXSkinRadioButton32: TSXSkinRadioButton
              Top = 135
              Width = 71
              Height = 15
              Checked = True
              OnUserModified = SXSkinRadioButton6UserModified
              Align = alTop
              Caption = 'TopBottom'
              SkinLibrary = SXSkinLibrary1
              TabOrder = 0
            end
            object SXSkinRadioButton33: TSXSkinRadioButton
              Top = 120
              Width = 62
              Height = 15
              OnUserModified = SXSkinRadioButton6UserModified
              Align = alTop
              Caption = 'LeftRight'
              SkinLibrary = SXSkinLibrary1
              TabOrder = 1
            end
            object SXSkinRadioButton34: TSXSkinRadioButton
              Top = 105
              Width = 77
              Height = 15
              OnUserModified = SXSkinRadioButton6UserModified
              Align = alTop
              Caption = 'RightBottom'
              SkinLibrary = SXSkinLibrary1
              TabOrder = 2
            end
            object SXSkinRadioButton35: TSXSkinRadioButton
              Top = 90
              Width = 63
              Height = 15
              OnUserModified = SXSkinRadioButton6UserModified
              Align = alTop
              Caption = 'RightTop'
              SkinLibrary = SXSkinLibrary1
              TabOrder = 3
            end
            object SXSkinRadioButton36: TSXSkinRadioButton
              Top = 75
              Width = 70
              Height = 15
              OnUserModified = SXSkinRadioButton6UserModified
              Align = alTop
              Caption = 'LeftBottom'
              SkinLibrary = SXSkinLibrary1
              TabOrder = 4
            end
            object SXSkinRadioButton37: TSXSkinRadioButton
              Width = 37
              Height = 15
              OnUserModified = SXSkinRadioButton6UserModified
              Align = alTop
              Caption = 'Left'
              SkinLibrary = SXSkinLibrary1
              TabOrder = 5
            end
            object SXSkinRadioButton38: TSXSkinRadioButton
              Top = 15
              Width = 44
              Height = 15
              OnUserModified = SXSkinRadioButton6UserModified
              Align = alTop
              Caption = 'Right'
              SkinLibrary = SXSkinLibrary1
              TabOrder = 6
            end
            object SXSkinRadioButton39: TSXSkinRadioButton
              Top = 30
              Width = 38
              Height = 15
              OnUserModified = SXSkinRadioButton6UserModified
              Align = alTop
              Caption = 'Top'
              SkinLibrary = SXSkinLibrary1
              TabOrder = 7
            end
            object SXSkinRadioButton40: TSXSkinRadioButton
              Top = 45
              Width = 52
              Height = 15
              OnUserModified = SXSkinRadioButton6UserModified
              Align = alTop
              Caption = 'Bottom'
              SkinLibrary = SXSkinLibrary1
              TabOrder = 8
            end
            object SXSkinRadioButton41: TSXSkinRadioButton
              Top = 60
              Width = 56
              Height = 15
              OnUserModified = SXSkinRadioButton6UserModified
              Align = alTop
              Caption = 'LeftTop'
              SkinLibrary = SXSkinLibrary1
              TabOrder = 9
            end
          end
          object SXSkinPanel22: TSXSkinPanel
            Left = 496
            Top = 104
            Width = 47
            Height = 30
            AutoSize = True
            SkinLibrary = SXSkinLibrary1
            TabOrder = 14
            object SXSkinCheckBox21: TSXSkinCheckBox
              Width = 37
              Height = 15
              OnUserModified = SXSkinRadioButton6UserModified
              Align = alTop
              Caption = 'Out'
              SkinLibrary = SXSkinLibrary1
              TabOrder = 0
            end
            object SXSkinCheckBox22: TSXSkinCheckBox
              Top = 15
              Width = 47
              Height = 15
              OnUserModified = SXSkinRadioButton6UserModified
              Align = alTop
              Caption = 'Invert'
              SkinLibrary = SXSkinLibrary1
              TabOrder = 1
            end
          end
          object SXSkinLabel39: TSXSkinLabel
            Left = 8
            Top = 328
            Width = 117
            Height = 13
            Caption = 'Transform on MouseOut:'
            SkinLibrary = SXSkinLibrary1
          end
          object SXSkinLabel40: TSXSkinLabel
            Left = 8
            Top = 344
            Width = 101
            Height = 13
            Caption = 'Old Image Transform:'
            SkinLibrary = SXSkinLibrary1
          end
          object SXSkinPanel25: TSXSkinPanel
            Left = 8
            Top = 360
            Width = 67
            Height = 90
            AutoSize = True
            SkinLibrary = SXSkinLibrary1
            TabOrder = 17
            object SXSkinRadioButton42: TSXSkinRadioButton
              Width = 45
              Height = 15
              OnUserModified = SXSkinRadioButton6UserModified
              Align = alTop
              Caption = 'None'
              SkinLibrary = SXSkinLibrary1
              TabOrder = 0
            end
            object SXSkinRadioButton43: TSXSkinRadioButton
              Top = 15
              Width = 46
              Height = 15
              OnUserModified = SXSkinRadioButton6UserModified
              Align = alTop
              Caption = 'Blend'
              SkinLibrary = SXSkinLibrary1
              TabOrder = 1
            end
            object SXSkinRadioButton44: TSXSkinRadioButton
              Top = 30
              Width = 43
              Height = 15
              OnUserModified = SXSkinRadioButton6UserModified
              Align = alTop
              Caption = 'Clear'
              SkinLibrary = SXSkinLibrary1
              TabOrder = 2
            end
            object SXSkinRadioButton45: TSXSkinRadioButton
              Top = 45
              Width = 43
              Height = 15
              Checked = True
              OnUserModified = SXSkinRadioButton6UserModified
              Align = alTop
              Caption = 'Fade'
              SkinLibrary = SXSkinLibrary1
              TabOrder = 3
            end
            object SXSkinRadioButton46: TSXSkinRadioButton
              Top = 60
              Width = 67
              Height = 15
              OnUserModified = SXSkinRadioButton6UserModified
              Align = alTop
              Caption = 'OverDraw'
              SkinLibrary = SXSkinLibrary1
              TabOrder = 4
            end
            object SXSkinRadioButton47: TSXSkinRadioButton
              Top = 75
              Width = 42
              Height = 15
              OnUserModified = SXSkinRadioButton6UserModified
              Align = alTop
              Caption = 'Slide'
              SkinLibrary = SXSkinLibrary1
              TabOrder = 5
            end
          end
          object SXSkinPanel26: TSXSkinPanel
            Left = 104
            Top = 360
            Width = 77
            Height = 150
            AutoSize = True
            SkinLibrary = SXSkinLibrary1
            TabOrder = 18
            object SXSkinRadioButton48: TSXSkinRadioButton
              Top = 135
              Width = 71
              Height = 15
              OnUserModified = SXSkinRadioButton6UserModified
              Align = alTop
              Caption = 'TopBottom'
              SkinLibrary = SXSkinLibrary1
              TabOrder = 0
            end
            object SXSkinRadioButton49: TSXSkinRadioButton
              Top = 120
              Width = 62
              Height = 15
              OnUserModified = SXSkinRadioButton6UserModified
              Align = alTop
              Caption = 'LeftRight'
              SkinLibrary = SXSkinLibrary1
              TabOrder = 1
            end
            object SXSkinRadioButton50: TSXSkinRadioButton
              Top = 105
              Width = 77
              Height = 15
              OnUserModified = SXSkinRadioButton6UserModified
              Align = alTop
              Caption = 'RightBottom'
              SkinLibrary = SXSkinLibrary1
              TabOrder = 2
            end
            object SXSkinRadioButton51: TSXSkinRadioButton
              Top = 90
              Width = 63
              Height = 15
              OnUserModified = SXSkinRadioButton6UserModified
              Align = alTop
              Caption = 'RightTop'
              SkinLibrary = SXSkinLibrary1
              TabOrder = 3
            end
            object SXSkinRadioButton52: TSXSkinRadioButton
              Top = 75
              Width = 70
              Height = 15
              OnUserModified = SXSkinRadioButton6UserModified
              Align = alTop
              Caption = 'LeftBottom'
              SkinLibrary = SXSkinLibrary1
              TabOrder = 4
            end
            object SXSkinRadioButton53: TSXSkinRadioButton
              Width = 37
              Height = 15
              Checked = True
              OnUserModified = SXSkinRadioButton6UserModified
              Align = alTop
              Caption = 'Left'
              SkinLibrary = SXSkinLibrary1
              TabOrder = 5
            end
            object SXSkinRadioButton54: TSXSkinRadioButton
              Top = 15
              Width = 44
              Height = 15
              OnUserModified = SXSkinRadioButton6UserModified
              Align = alTop
              Caption = 'Right'
              SkinLibrary = SXSkinLibrary1
              TabOrder = 6
            end
            object SXSkinRadioButton55: TSXSkinRadioButton
              Top = 30
              Width = 38
              Height = 15
              OnUserModified = SXSkinRadioButton6UserModified
              Align = alTop
              Caption = 'Top'
              SkinLibrary = SXSkinLibrary1
              TabOrder = 7
            end
            object SXSkinRadioButton56: TSXSkinRadioButton
              Top = 45
              Width = 52
              Height = 15
              OnUserModified = SXSkinRadioButton6UserModified
              Align = alTop
              Caption = 'Bottom'
              SkinLibrary = SXSkinLibrary1
              TabOrder = 8
            end
            object SXSkinRadioButton57: TSXSkinRadioButton
              Top = 60
              Width = 56
              Height = 15
              OnUserModified = SXSkinRadioButton6UserModified
              Align = alTop
              Caption = 'LeftTop'
              SkinLibrary = SXSkinLibrary1
              TabOrder = 9
            end
          end
          object SXSkinPanel27: TSXSkinPanel
            Left = 208
            Top = 360
            Width = 56
            Height = 45
            AutoSize = True
            SkinLibrary = SXSkinLibrary1
            TabOrder = 19
            object SXSkinCheckBox24: TSXSkinCheckBox
              Width = 37
              Height = 15
              OnUserModified = SXSkinRadioButton6UserModified
              Align = alTop
              Caption = 'Out'
              SkinLibrary = SXSkinLibrary1
              State = cbChecked
              TabOrder = 0
            end
            object SXSkinCheckBox25: TSXSkinCheckBox
              Top = 15
              Width = 47
              Height = 15
              OnUserModified = SXSkinRadioButton6UserModified
              Align = alTop
              Caption = 'Invert'
              SkinLibrary = SXSkinLibrary1
              TabOrder = 1
            end
            object SXSkinCheckBox26: TSXSkinCheckBox
              Top = 30
              Width = 56
              Height = 15
              OnUserModified = SXSkinRadioButton6UserModified
              Align = alTop
              Caption = 'Caption'
              SkinLibrary = SXSkinLibrary1
              State = cbChecked
              TabOrder = 2
            end
          end
          object SXSkinPanel28: TSXSkinPanel
            Left = 296
            Top = 360
            Width = 67
            Height = 90
            AutoSize = True
            SkinLibrary = SXSkinLibrary1
            TabOrder = 20
            object SXSkinRadioButton58: TSXSkinRadioButton
              Width = 45
              Height = 15
              OnUserModified = SXSkinRadioButton6UserModified
              Align = alTop
              Caption = 'None'
              SkinLibrary = SXSkinLibrary1
              TabOrder = 0
            end
            object SXSkinRadioButton59: TSXSkinRadioButton
              Top = 15
              Width = 46
              Height = 15
              OnUserModified = SXSkinRadioButton6UserModified
              Align = alTop
              Caption = 'Blend'
              SkinLibrary = SXSkinLibrary1
              TabOrder = 1
            end
            object SXSkinRadioButton60: TSXSkinRadioButton
              Top = 30
              Width = 43
              Height = 15
              OnUserModified = SXSkinRadioButton6UserModified
              Align = alTop
              Caption = 'Clear'
              SkinLibrary = SXSkinLibrary1
              TabOrder = 2
            end
            object SXSkinRadioButton61: TSXSkinRadioButton
              Top = 45
              Width = 43
              Height = 15
              OnUserModified = SXSkinRadioButton6UserModified
              Align = alTop
              Caption = 'Fade'
              SkinLibrary = SXSkinLibrary1
              TabOrder = 3
            end
            object SXSkinRadioButton62: TSXSkinRadioButton
              Top = 60
              Width = 67
              Height = 15
              OnUserModified = SXSkinRadioButton6UserModified
              Align = alTop
              Caption = 'OverDraw'
              SkinLibrary = SXSkinLibrary1
              TabOrder = 4
            end
            object SXSkinRadioButton63: TSXSkinRadioButton
              Top = 75
              Width = 42
              Height = 15
              Checked = True
              OnUserModified = SXSkinRadioButton6UserModified
              Align = alTop
              Caption = 'Slide'
              SkinLibrary = SXSkinLibrary1
              TabOrder = 5
            end
          end
          object SXSkinLabel41: TSXSkinLabel
            Left = 296
            Top = 344
            Width = 107
            Height = 13
            Caption = 'New Image Transform:'
            SkinLibrary = SXSkinLibrary1
          end
          object SXSkinPanel29: TSXSkinPanel
            Left = 392
            Top = 360
            Width = 77
            Height = 150
            AutoSize = True
            SkinLibrary = SXSkinLibrary1
            TabOrder = 22
            object SXSkinRadioButton64: TSXSkinRadioButton
              Top = 135
              Width = 71
              Height = 15
              Checked = True
              OnUserModified = SXSkinRadioButton6UserModified
              Align = alTop
              Caption = 'TopBottom'
              SkinLibrary = SXSkinLibrary1
              TabOrder = 0
            end
            object SXSkinRadioButton65: TSXSkinRadioButton
              Top = 120
              Width = 62
              Height = 15
              OnUserModified = SXSkinRadioButton6UserModified
              Align = alTop
              Caption = 'LeftRight'
              SkinLibrary = SXSkinLibrary1
              TabOrder = 1
            end
            object SXSkinRadioButton66: TSXSkinRadioButton
              Top = 105
              Width = 77
              Height = 15
              OnUserModified = SXSkinRadioButton6UserModified
              Align = alTop
              Caption = 'RightBottom'
              SkinLibrary = SXSkinLibrary1
              TabOrder = 2
            end
            object SXSkinRadioButton67: TSXSkinRadioButton
              Top = 90
              Width = 63
              Height = 15
              OnUserModified = SXSkinRadioButton6UserModified
              Align = alTop
              Caption = 'RightTop'
              SkinLibrary = SXSkinLibrary1
              TabOrder = 3
            end
            object SXSkinRadioButton68: TSXSkinRadioButton
              Top = 75
              Width = 70
              Height = 15
              OnUserModified = SXSkinRadioButton6UserModified
              Align = alTop
              Caption = 'LeftBottom'
              SkinLibrary = SXSkinLibrary1
              TabOrder = 4
            end
            object SXSkinRadioButton69: TSXSkinRadioButton
              Width = 37
              Height = 15
              OnUserModified = SXSkinRadioButton6UserModified
              Align = alTop
              Caption = 'Left'
              SkinLibrary = SXSkinLibrary1
              TabOrder = 5
            end
            object SXSkinRadioButton70: TSXSkinRadioButton
              Top = 15
              Width = 44
              Height = 15
              OnUserModified = SXSkinRadioButton6UserModified
              Align = alTop
              Caption = 'Right'
              SkinLibrary = SXSkinLibrary1
              TabOrder = 6
            end
            object SXSkinRadioButton71: TSXSkinRadioButton
              Top = 30
              Width = 38
              Height = 15
              OnUserModified = SXSkinRadioButton6UserModified
              Align = alTop
              Caption = 'Top'
              SkinLibrary = SXSkinLibrary1
              TabOrder = 7
            end
            object SXSkinRadioButton72: TSXSkinRadioButton
              Top = 45
              Width = 52
              Height = 15
              OnUserModified = SXSkinRadioButton6UserModified
              Align = alTop
              Caption = 'Bottom'
              SkinLibrary = SXSkinLibrary1
              TabOrder = 8
            end
            object SXSkinRadioButton73: TSXSkinRadioButton
              Top = 60
              Width = 56
              Height = 15
              OnUserModified = SXSkinRadioButton6UserModified
              Align = alTop
              Caption = 'LeftTop'
              SkinLibrary = SXSkinLibrary1
              TabOrder = 9
            end
          end
          object SXSkinPanel30: TSXSkinPanel
            Left = 496
            Top = 360
            Width = 47
            Height = 30
            AutoSize = True
            SkinLibrary = SXSkinLibrary1
            TabOrder = 23
            object SXSkinCheckBox27: TSXSkinCheckBox
              Width = 37
              Height = 15
              OnUserModified = SXSkinRadioButton6UserModified
              Align = alTop
              Caption = 'Out'
              SkinLibrary = SXSkinLibrary1
              TabOrder = 0
            end
            object SXSkinCheckBox28: TSXSkinCheckBox
              Top = 15
              Width = 47
              Height = 15
              OnUserModified = SXSkinRadioButton6UserModified
              Align = alTop
              Caption = 'Invert'
              SkinLibrary = SXSkinLibrary1
              TabOrder = 1
            end
          end
          object SXSkinLabel42: TSXSkinLabel
            Left = 208
            Top = 184
            Width = 52
            Height = 13
            Caption = 'StepsNum:'
            SkinLibrary = SXSkinLibrary1
          end
          object SXSkinLabel43: TSXSkinLabel
            Left = 208
            Top = 440
            Width = 52
            Height = 13
            Caption = 'StepsNum:'
            SkinLibrary = SXSkinLibrary1
          end
          object SXSkinLabel44: TSXSkinLabel
            Left = 208
            Top = 232
            Width = 59
            Height = 13
            Caption = 'EffectOffset:'
            SkinLibrary = SXSkinLibrary1
          end
          object SXSkinLabel45: TSXSkinLabel
            Left = 208
            Top = 488
            Width = 59
            Height = 13
            Caption = 'EffectOffset:'
            SkinLibrary = SXSkinLibrary1
          end
          object SXSkinCheckBox23: TSXSkinCheckBox
            Left = 152
            Top = 80
            Width = 126
            Height = 15
            OnUserModified = SXSkinRadioButton6UserModified
            Caption = 'Don'#39't use Glyph Effect'
            SkinLibrary = SXSkinLibrary1
            State = cbChecked
            TabOrder = 28
          end
          object SXSkinCheckBox29: TSXSkinCheckBox
            Left = 152
            Top = 336
            Width = 126
            Height = 15
            OnUserModified = SXSkinRadioButton6UserModified
            Caption = 'Don'#39't use Glyph Effect'
            SkinLibrary = SXSkinLibrary1
            State = cbChecked
            TabOrder = 29
          end
          object SXSkinSpinEdit15: TSXSkinSpinEdit
            Left = 208
            Top = 200
            Width = 65
            Height = 23
            Alignment = taRightJustify
            MaxLength = 4
            MaxValue = 9999
            MinValue = 1
            SkinLibrary = SXSkinLibrary1
            Text = '10'
            Value = 10
            OnUserModified = SXSkinRadioButton6UserModified
          end
          object SXSkinSpinEdit16: TSXSkinSpinEdit
            Left = 208
            Top = 248
            Width = 65
            Height = 23
            Alignment = taRightJustify
            MaxLength = 4
            MaxValue = 9999
            MinValue = 1
            SkinLibrary = SXSkinLibrary1
            Text = '10'
            Value = 10
            OnUserModified = SXSkinRadioButton6UserModified
          end
          object SXSkinSpinEdit17: TSXSkinSpinEdit
            Left = 208
            Top = 456
            Width = 65
            Height = 23
            Alignment = taRightJustify
            MaxLength = 4
            MaxValue = 9999
            MinValue = 1
            SkinLibrary = SXSkinLibrary1
            Text = '10'
            Value = 10
            OnUserModified = SXSkinRadioButton6UserModified
          end
          object SXSkinSpinEdit18: TSXSkinSpinEdit
            Left = 208
            Top = 504
            Width = 65
            Height = 23
            Alignment = taRightJustify
            MaxLength = 4
            MaxValue = 9999
            MinValue = 1
            SkinLibrary = SXSkinLibrary1
            Text = '10'
            Value = 10
            OnUserModified = SXSkinRadioButton6UserModified
          end
        end
        object AboutPage: TSXSkinNotebookPage
          Width = 671
          Height = 607
          Align = alClient
          SkinLibrary = SXSkinLibrary1
          TabOrder = 10
          Visible = False
          object SXSkinLabel48: TSXSkinLabel
            Left = 50
            Top = 10
            Width = 571
            Height = 76
            Caption = 'SXSkinComponents'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clGray
            Font.Height = -67
            Font.Name = 'Times New Roman'
            Font.Style = [fsBold, fsUnderline]
            ParentFont = False
            SkinLibrary = SXSkinLibrary1
            UseCustomFont = True
            UseCustomFontColor = True
          end
          object SXSkinLabel47: TSXSkinLabel
            Left = 49
            Top = 9
            Width = 571
            Height = 76
            Caption = 'SXSkinComponents'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clNavy
            Font.Height = -67
            Font.Name = 'Times New Roman'
            Font.Style = [fsBold, fsUnderline]
            ParentFont = False
            SkinLibrary = SXSkinLibrary1
            UseCustomFont = True
            UseCustomFontColor = True
          end
          object SXSkinLabel46: TSXSkinLabel
            Left = 48
            Top = 8
            Width = 571
            Height = 76
            Caption = 'SXSkinComponents'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlue
            Font.Height = -67
            Font.Name = 'Times New Roman'
            Font.Style = [fsBold, fsUnderline]
            ParentFont = False
            SkinLibrary = SXSkinLibrary1
            UseCustomFont = True
            UseCustomFontColor = True
            object SXSkinLabel111: TSXSkinLabel
              Left = 344
              Top = 6
              Width = 64
              Height = 24
              Caption = 'version'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clBlue
              Font.Height = -21
              Font.Name = 'Times New Roman'
              Font.Style = [fsBold]
              ParentFont = False
              SkinLibrary = SXSkinLibrary1
            end
          end
          object SXSkinCheckBox30: TSXSkinCheckBox
            Left = 48
            Top = 96
            Width = 577
            Height = 41
            TabStop = False
            AutoSizeWidth = False
            Caption = 
              '2 non-visual (SXSkinLibrary, SXStoredSkin) and 13 visual compone' +
              'nts (SXSkinForm, SXSkinImage, SXSkinPaintBox, SXSkinPanel, SXSki' +
              'nLabel, SXSkinButton, SXSkinCheckBox, SXSkinRadioButton, SXSkinE' +
              'dit, SXSkinSpinEdit, SXSkinUpDown, SXSkinGroupBox, and SXSkinNot' +
              'ebook) are available right now'
            SkinLibrary = SXSkinLibrary1
            State = cbChecked
            TabOrder = 3
            WordWrap = True
          end
          object SXSkinCheckBox31: TSXSkinCheckBox
            Left = 48
            Top = 168
            Width = 577
            Height = 28
            TabStop = False
            AutoSizeWidth = False
            Caption = 
              'All skins can be stored externally (in skin.ini or skin.sxs file' +
              's and graphic PNG and JPEG files or in a zipped skin file) or in' +
              'ternally in EXE-file (only compressed skin in .zip-file).'
            SkinLibrary = SXSkinLibrary1
            State = cbChecked
            TabOrder = 5
            WordWrap = True
          end
          object SXSkinCheckBox32: TSXSkinCheckBox
            Left = 48
            Top = 204
            Width = 577
            Height = 28
            TabStop = False
            AutoSizeWidth = False
            Caption = 
              'Skin.ini file consists of skin styles. There are two types of sk' +
              'in styles: special and general ones.'#13#10'Special styles are used to' +
              ' describe specific properties of controls (checkboxes, buttons e' +
              'tc.)'
            SkinLibrary = SXSkinLibrary1
            State = cbChecked
            TabOrder = 6
            WordWrap = True
          end
          object SXSkinCheckBox33: TSXSkinCheckBox
            Left = 48
            Top = 240
            Width = 577
            Height = 28
            TabStop = False
            AutoSizeWidth = False
            Caption = 
              'Each general style consists one or more skin style elements.'#13#10'Th' +
              'ese types of elements are supported now: Image, BoxTile, Figure,' +
              ' Style and Text.'
            SkinLibrary = SXSkinLibrary1
            State = cbChecked
            TabOrder = 7
            WordWrap = True
          end
          object SXSkinCheckBox34: TSXSkinCheckBox
            Left = 48
            Top = 276
            Width = 577
            Height = 15
            TabStop = False
            AutoSizeWidth = False
            Caption = 
              'You can use any JPEG and PNG files as Image elements. PNG files ' +
              'may have 8-bit alpha channel.'
            SkinLibrary = SXSkinLibrary1
            State = cbChecked
            TabOrder = 8
            WordWrap = True
          end
          object SXSkinCheckBox35: TSXSkinCheckBox
            Left = 48
            Top = 144
            Width = 577
            Height = 15
            TabStop = False
            AutoSizeWidth = False
            Caption = 
              'All visual components have 100% alpha channel (trasparency) supp' +
              'ort.'
            GlyphPosition = gpLeft
            SkinLibrary = SXSkinLibrary1
            State = cbChecked
            TabOrder = 9
            WordWrap = True
          end
          object SXSkinLabel49: TSXSkinLabel
            Left = 136
            Top = 424
            Width = 207
            Height = 13
            Cursor = crHandPoint
            Caption = 'http://www.saarixx.info/sxskincomponents/'
            SkinLibrary = SXSkinLibrary1
            SkinStyle = '_Label_Hyperlink'
            OnClick = SXSkinLabel49Click
          end
          object SXSkinLabel50: TSXSkinLabel
            Left = 48
            Top = 424
            Width = 78
            Height = 13
            Caption = 'Home Web Site:'
            SkinLibrary = SXSkinLibrary1
          end
          object SXSkinCheckBox36: TSXSkinCheckBox
            Left = 48
            Top = 300
            Width = 577
            Height = 15
            TabStop = False
            AutoSizeWidth = False
            Caption = 
              'You can freely use these components in your freeware Delphi and ' +
              'C++Builder applications'
            SkinLibrary = SXSkinLibrary1
            State = cbChecked
            TabOrder = 12
            WordWrap = True
          end
          object SXSkinCheckBox37: TSXSkinCheckBox
            Left = 48
            Top = 324
            Width = 577
            Height = 15
            TabStop = False
            AutoSizeWidth = False
            Caption = 
              'SXSkinComponents are the only 100% open source skinnable compone' +
              'nts'
            SkinLibrary = SXSkinLibrary1
            State = cbChecked
            TabOrder = 13
            WordWrap = True
          end
          object SXSkinCheckBox38: TSXSkinCheckBox
            Left = 48
            Top = 348
            Width = 577
            Height = 15
            TabStop = False
            AutoSizeWidth = False
            Caption = 
              'Can be used in Delphi 6, 7, 2005, 2006, C++Builder 6 and 2006. D' +
              'esigned for Windows 98/ME/NT/2000/XP/2003'
            SkinLibrary = SXSkinLibrary1
            State = cbChecked
            TabOrder = 14
            WordWrap = True
          end
          object SXSkinCheckBox39: TSXSkinCheckBox
            Left = 48
            Top = 372
            Width = 577
            Height = 28
            TabStop = False
            AutoSizeWidth = False
            Caption = 
              'Control can have some styles depending of it'#39's state: Normal, Hi' +
              'ghlighted (MouseOver), Disabled, Down, Checked, Focused etc. The' +
              're are lots of style changing animations: Blending, Sliding, Fad' +
              'ing, OverDrawing.'
            SkinLibrary = SXSkinLibrary1
            State = cbChecked
            TabOrder = 15
            WordWrap = True
          end
          object SXSkinLabel51: TSXSkinLabel
            Left = 48
            Top = 440
            Width = 72
            Height = 13
            Caption = 'E-Mail Support:'
            SkinLibrary = SXSkinLibrary1
          end
          object SXSkinLabel52: TSXSkinLabel
            Left = 136
            Top = 440
            Width = 150
            Height = 13
            Cursor = crHandPoint
            Caption = 'sxskincomponents@saarixx.info'
            SkinLibrary = SXSkinLibrary1
            SkinStyle = '_Label_Hyperlink'
            OnClick = SXSkinLabel52Click
          end
          object SXSkinPanel32: TSXSkinPanel
            Left = 16
            Top = 91
            Width = 641
            Height = 318
            SkinLibrary = SXSkinLibrary1
            TabOrder = 4
          end
          object SXSkinCheckBox40: TSXSkinCheckBox
            Left = 480
            Top = 424
            Width = 143
            Height = 41
            Caption = 'Move Background.'#13#10'Try this effect with'#13#10'Opera - WoodWorks skin!'
            GlyphPosition = gpLeft
            SkinLibrary = SXSkinLibrary1
            TabOrder = 18
          end
        end
        object PaintBoxPage: TSXSkinNotebookPage
          Width = 671
          Height = 607
          Align = alClient
          SkinLibrary = SXSkinLibrary1
          TabOrder = 11
          Visible = False
          OnResize = PaintBoxPageResize
          object SXSkinPaintBox1: TSXSkinPaintBox
            Left = 8
            Top = 40
            Width = 649
            Height = 281
            SkinLibrary = SXSkinLibrary1
            TabOrder = 0
            OnMouseDown = SXSkinPaintBox1MouseDown
            OnMouseMove = SXSkinPaintBox1MouseMove
            OnMouseUp = SXSkinPaintBox1MouseUp
            OnResize = SXSkinPaintBox1Resize
          end
          object SXSkinButton63: TSXSkinButton
            Left = 8
            Top = 8
            Width = 89
            Height = 25
            OnClick = SXSkinButton63Click
            Caption = 'Clear'
            SkinLibrary = SXSkinLibrary1
            TabOrder = 1
          end
          object SXSkinLabel53: TSXSkinLabel
            Left = 112
            Top = 16
            Width = 276
            Height = 13
            Caption = 'Click mouse button on the following PaintBox to paint on it.'
            SkinLibrary = SXSkinLibrary1
          end
          object SXSkinLabel54: TSXSkinLabel
            Left = 440
            Top = 16
            Width = 53
            Height = 13
            Caption = 'Brush Size:'
            SkinLibrary = SXSkinLibrary1
          end
          object SXSkinLabel55: TSXSkinLabel
            Left = 8
            Top = 328
            Width = 283
            Height = 13
            Caption = 'Brush Color changes from Red to Light Red - it'#39's not a bug ;)'
            SkinLibrary = SXSkinLibrary1
          end
          object SXSkinPaintBox2: TSXSkinPaintBox
            Left = 8
            Top = 376
            Width = 257
            Height = 185
            SkinLibrary = SXSkinLibrary1
            TabOrder = 5
            OnPaint = SXSkinPaintBox2Paint
          end
          object SXSkinLabel56: TSXSkinLabel
            Left = 8
            Top = 360
            Width = 196
            Height = 13
            Caption = 'This PaintBox is drawn on OnPaint event:'
            SkinLibrary = SXSkinLibrary1
          end
          object SXSkinLabel57: TSXSkinLabel
            Left = 296
            Top = 360
            Width = 216
            Height = 13
            Caption = 'This PaintBox is drawn on OnFastPaint event:'
            SkinLibrary = SXSkinLibrary1
          end
          object SXSkinPaintBox3: TSXSkinPaintBox
            Left = 296
            Top = 376
            Width = 257
            Height = 185
            SkinLibrary = SXSkinLibrary1
            TabOrder = 8
            OnFastPaint = SXSkinPaintBox3FastPaint
          end
          object SXSkinSpinEdit10: TSXSkinSpinEdit
            Left = 504
            Top = 12
            Width = 49
            Height = 23
            Alignment = taRightJustify
            MaxLength = 2
            MaxValue = 99
            MinValue = 1
            SkinLibrary = SXSkinLibrary1
            Text = '8'
            Value = 8
          end
        end
        object MousePage: TSXSkinNotebookPage
          Width = 671
          Height = 607
          Align = alClient
          SkinLibrary = SXSkinLibrary1
          TabOrder = 12
          Visible = False
          object SXSkinButton64: TSXSkinButton
            Left = 8
            Top = 56
            Width = 150
            Height = 150
            Caption = 'BUTTON1'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -16
            Font.Name = 'Times New Roman'
            Font.Style = [fsBold]
            ParentFont = False
            SkinLibrary = SXSkinLibrary1
            SkinStyle = '_Button_Rnd'
            TabOrder = 0
            OnMouseEnter = SXSkinButton64MouseEnter
          end
          object SXSkinButton67: TSXSkinButton
            Left = 100
            Top = 120
            Width = 150
            Height = 150
            Caption = 'BUTTON4'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -16
            Font.Name = 'Times New Roman'
            Font.Style = [fsBold]
            ParentFont = False
            SkinLibrary = SXSkinLibrary1
            SkinStyle = '_Button_Rnd'
            TabOrder = 3
            OnMouseEnter = SXSkinButton64MouseEnter
          end
          object SXSkinButton66: TSXSkinButton
            Left = 192
            Top = 56
            Width = 150
            Height = 150
            Caption = 'BUTTON3'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -16
            Font.Name = 'Times New Roman'
            Font.Style = [fsBold]
            ParentFont = False
            SkinLibrary = SXSkinLibrary1
            SkinStyle = '_Button_Rnd'
            TabOrder = 2
            OnMouseEnter = SXSkinButton64MouseEnter
          end
          object SXSkinButton65: TSXSkinButton
            Left = 100
            Top = -8
            Width = 150
            Height = 150
            Caption = 'BUTTON2'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -16
            Font.Name = 'Times New Roman'
            Font.Style = [fsBold]
            ParentFont = False
            SkinLibrary = SXSkinLibrary1
            SkinStyle = '_Button_Rnd'
            TabOrder = 1
            OnMouseEnter = SXSkinButton64MouseEnter
          end
          object SXSkinButton68: TSXSkinButton
            Left = 8
            Top = 312
            Width = 150
            Height = 150
            Caption = 'BUTTON1'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -16
            Font.Name = 'Times New Roman'
            Font.Style = [fsBold]
            ParentFont = False
            SkinLibrary = SXSkinLibrary1
            SkinStyle = '_Button_Rnd'
            TabOrder = 5
          end
          object SXSkinButton69: TSXSkinButton
            Left = 72
            Top = 256
            Width = 150
            Height = 150
            Caption = 'BUTTON2'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -16
            Font.Name = 'Times New Roman'
            Font.Style = [fsBold]
            ParentFont = False
            SkinLibrary = SXSkinLibrary1
            SkinStyle = '_Button_Rnd'
            TabOrder = 6
          end
          object SXSkinButton70: TSXSkinButton
            Left = 120
            Top = 328
            Width = 150
            Height = 150
            Caption = 'BUTTON3'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -16
            Font.Name = 'Times New Roman'
            Font.Style = [fsBold]
            ParentFont = False
            SkinLibrary = SXSkinLibrary1
            SkinStyle = '_Button_Rnd'
            TabOrder = 7
          end
          object SXSkinButton71: TSXSkinButton
            Left = 184
            Top = 280
            Width = 150
            Height = 150
            Caption = 'BUTTON4'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -16
            Font.Name = 'Times New Roman'
            Font.Style = [fsBold]
            ParentFont = False
            SkinLibrary = SXSkinLibrary1
            SkinStyle = '_Button_Rnd'
            TabOrder = 8
          end
          object SXSkinImage16: TSXSkinImage
            Left = 51
            Top = 279
            Width = 254
            Height = 178
            SkinLibrary = SXSkinLibrary1
            SkinStyle = 'StarPanel'
            TabOrder = 9
          end
          object SXSkinLabel58: TSXSkinLabel
            Left = 368
            Top = 16
            Width = 289
            Height = 39
            Alignment = taCenter
            AutoSizeWidth = False
            Caption = 
              'Can you create such an example using'#13#10'another VCL components and' +
              ' default'#13#10'Borland VCL drawing technology?'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentFont = False
            SkinLibrary = SXSkinLibrary1
          end
          object SXSkinLabel59: TSXSkinLabel
            Left = 368
            Top = 72
            Width = 289
            Height = 143
            AutoSizeWidth = False
            Caption = 
              'The answer is YES.'#13#10#13#10'A default visual control has a rectangular' +
              ' client area. So, it doesn'#39't work in this sample, because parts ' +
              'of neighbour controls can not be processed properly.'#13#10#13#10'However,' +
              ' you can really solve this overlapping problem using Window Regi' +
              'ons functions (SetWindowRgn and others).'#13#10#13#10'But when using SetWi' +
              'ndowRegion, you can not draw a semi-transparent border of a cont' +
              'rol.'
            SkinLibrary = SXSkinLibrary1
            WordWrap = True
          end
          object SXSkinLabel60: TSXSkinLabel
            Left = 368
            Top = 256
            Width = 289
            Height = 26
            Alignment = taCenter
            AutoSizeWidth = False
            Caption = 
              'And what can you say about this example?'#13#10'Have you ever seen som' +
              'ething like this before?'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentFont = False
            SkinLibrary = SXSkinLibrary1
          end
          object SXSkinLabel61: TSXSkinLabel
            Left = 368
            Top = 296
            Width = 289
            Height = 156
            AutoSizeWidth = False
            Caption = 
              'Here you can see two TSXSkinImage controls with stars on them (t' +
              'he first - 4x3 and the second - inside of the first - 3x2).'#13#10#13#10'4' +
              ' overlapped buttons are in back of the images. However, you can ' +
              'freely press each of them.'#13#10#13#10'Every general skin style has Mouse' +
              'Capture property, and every TSXSkinPanel component has CapturesM' +
              'ouse property.'#13#10#13#10'Using these features of SXSkinComponents you c' +
              'an create such an example in a minute.'
            SkinLibrary = SXSkinLibrary1
            WordWrap = True
          end
          object SXSkinButton72: TSXSkinButton
            Left = 16
            Top = 480
            Width = 100
            Height = 100
            Alignment = taCenter
            Caption = 'Button'#13#10'<1>'
            SkinLibrary = SXSkinLibrary1
            SkinStyle = '_Button_Cube'
            TabOrder = 14
          end
          object SXSkinButton73: TSXSkinButton
            Left = 112
            Top = 480
            Width = 100
            Height = 100
            Alignment = taCenter
            Caption = 'Button'#13#10'<2>'
            SkinLibrary = SXSkinLibrary1
            SkinStyle = '_Button_Cube'
            TabOrder = 15
          end
          object SXSkinButton74: TSXSkinButton
            Left = 208
            Top = 480
            Width = 100
            Height = 100
            Alignment = taCenter
            Caption = 'Button'#13#10'<3>'
            SkinLibrary = SXSkinLibrary1
            SkinStyle = '_Button_Cube'
            TabOrder = 16
          end
          object SXSkinLabel62: TSXSkinLabel
            Left = 368
            Top = 496
            Width = 289
            Height = 78
            AutoSizeWidth = False
            Caption = 
              'This examples shows how to use custom Mouse Capture Regions feat' +
              'ure.'#13#10#13#10'The cube is highlighted when mouse is over a front face.' +
              ' Highlighting is taken away when mouse is out of a larger (highl' +
              'ighted) cube'#39's front face.'
            SkinLibrary = SXSkinLibrary1
            WordWrap = True
          end
          object SXSkinImage14: TSXSkinImage
            Left = 11
            Top = 239
            Width = 314
            Height = 249
            SkinLibrary = SXSkinLibrary1
            SkinStyle = 'StarPanel'
            TabOrder = 4
          end
        end
        object EventsCheckPage: TSXSkinNotebookPage
          Width = 671
          Height = 607
          Align = alClient
          SkinLibrary = SXSkinLibrary1
          TabOrder = 13
          Visible = False
          object SXSkinCheckBox41: TSXSkinCheckBox
            Left = 128
            Top = 120
            Width = 69
            Height = 15
            OnUserModified = SXSkinEdit12UserModified
            OnMouseEnter = SXSkinButton75MouseEnter
            OnMouseLeave = SXSkinButton75MouseLeave
            AllowGrayed = True
            Caption = 'CheckBox'
            SkinLibrary = SXSkinLibrary1
            State = cbChecked
            TabOrder = 21
            OnClick = SXSkinButton75Click
            OnDblClick = SXSkinButton75DblClick
            OnMouseDown = SXSkinButton75MouseDown
            OnMouseMove = SXSkinButton75MouseMove
            OnMouseUp = SXSkinButton75MouseUp
          end
          object SXSkinLabel77: TSXSkinLabel
            Left = 107
            Top = 181
            Width = 157
            Height = 93
            OnMouseEnter = SXSkinButton75MouseEnter
            OnMouseLeave = SXSkinButton75MouseLeave
            Alignment = taCenter
            Caption = 'IT IS A'#13#10'MULTILINE'#13#10'LABEL'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -27
            Font.Name = 'Times New Roman'
            Font.Style = [fsBold]
            ParentFont = False
            SkinLibrary = SXSkinLibrary1
            OnClick = SXSkinButton75Click
            OnDblClick = SXSkinButton75DblClick
            OnMouseDown = SXSkinButton75MouseDown
            OnMouseMove = SXSkinButton75MouseMove
            OnMouseUp = SXSkinButton75MouseUp
          end
          object SXSkinLabel63: TSXSkinLabel
            Left = 392
            Top = 16
            Width = 18
            Height = 13
            Caption = '___'
            SkinLibrary = SXSkinLibrary1
            Visible = False
          end
          object SXSkinLabel65: TSXSkinLabel
            Left = 264
            Top = 16
            Width = 114
            Height = 13
            Caption = 'Last MouseEnter Event:'
            SkinLibrary = SXSkinLibrary1
          end
          object SXSkinLabel64: TSXSkinLabel
            Left = 264
            Top = 32
            Width = 119
            Height = 13
            Caption = 'Last MouseLeave Event:'
            SkinLibrary = SXSkinLibrary1
          end
          object SXSkinLabel66: TSXSkinLabel
            Left = 392
            Top = 32
            Width = 18
            Height = 13
            Caption = '___'
            SkinLibrary = SXSkinLibrary1
            Visible = False
          end
          object SXSkinLabel67: TSXSkinLabel
            Left = 264
            Top = 48
            Width = 116
            Height = 13
            Caption = 'Last MouseMove Event:'
            SkinLibrary = SXSkinLibrary1
          end
          object SXSkinLabel68: TSXSkinLabel
            Left = 392
            Top = 48
            Width = 18
            Height = 13
            Caption = '___'
            SkinLibrary = SXSkinLibrary1
            Visible = False
          end
          object SXSkinLabel69: TSXSkinLabel
            Left = 264
            Top = 64
            Width = 117
            Height = 13
            Caption = 'Last MouseDown Event:'
            SkinLibrary = SXSkinLibrary1
          end
          object SXSkinLabel70: TSXSkinLabel
            Left = 392
            Top = 64
            Width = 18
            Height = 13
            Caption = '___'
            SkinLibrary = SXSkinLibrary1
            Visible = False
          end
          object SXSkinLabel71: TSXSkinLabel
            Left = 264
            Top = 80
            Width = 103
            Height = 13
            Caption = 'Last MouseUp Event:'
            SkinLibrary = SXSkinLibrary1
          end
          object SXSkinLabel72: TSXSkinLabel
            Left = 392
            Top = 80
            Width = 18
            Height = 13
            Caption = '___'
            SkinLibrary = SXSkinLibrary1
            Visible = False
          end
          object SXSkinLabel73: TSXSkinLabel
            Left = 264
            Top = 96
            Width = 80
            Height = 13
            Caption = 'Last Click Event:'
            SkinLibrary = SXSkinLibrary1
          end
          object SXSkinLabel74: TSXSkinLabel
            Left = 392
            Top = 96
            Width = 18
            Height = 13
            Caption = '___'
            SkinLibrary = SXSkinLibrary1
            Visible = False
          end
          object SXSkinLabel75: TSXSkinLabel
            Left = 264
            Top = 112
            Width = 96
            Height = 13
            Caption = 'Last DblClick Event:'
            SkinLibrary = SXSkinLibrary1
          end
          object SXSkinLabel76: TSXSkinLabel
            Left = 392
            Top = 112
            Width = 18
            Height = 13
            Caption = '___'
            SkinLibrary = SXSkinLibrary1
            Visible = False
          end
          object SXSkinButton75: TSXSkinButton
            Left = 32
            Top = 24
            Width = 169
            Height = 81
            OnClick = SXSkinButton75Click
            Caption = 'Standard Button'
            SkinLibrary = SXSkinLibrary1
            TabOrder = 0
            OnDblClick = SXSkinButton75DblClick
            OnMouseDown = SXSkinButton75MouseDown
            OnMouseEnter = SXSkinButton75MouseEnter
            OnMouseLeave = SXSkinButton75MouseLeave
            OnMouseMove = SXSkinButton75MouseMove
            OnMouseUp = SXSkinButton75MouseUp
          end
          object SXSkinButton76: TSXSkinButton
            Left = -4
            Top = 60
            Width = 150
            Height = 150
            OnClick = SXSkinButton75Click
            Caption = 'Round Button'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
            SkinLibrary = SXSkinLibrary1
            SkinStyle = '_Button_Rnd'
            TabOrder = 1
            OnDblClick = SXSkinButton75DblClick
            OnMouseDown = SXSkinButton75MouseDown
            OnMouseEnter = SXSkinButton75MouseEnter
            OnMouseLeave = SXSkinButton75MouseLeave
            OnMouseMove = SXSkinButton75MouseMove
            OnMouseUp = SXSkinButton75MouseUp
          end
          object SXSkinImage29: TSXSkinImage
            Left = 30
            Top = 160
            Width = 100
            Height = 120
            SkinLibrary = SXSkinLibrary1
            SkinStyle = 'MyComputer_Caption'
            TabOrder = 2
            OnClick = SXSkinButton75Click
            OnDblClick = SXSkinButton75DblClick
            OnMouseDown = SXSkinButton75MouseDown
            OnMouseEnter = SXSkinButton75MouseEnter
            OnMouseLeave = SXSkinButton75MouseLeave
            OnMouseMove = SXSkinButton75MouseMove
            OnMouseUp = SXSkinButton75MouseUp
          end
          object SXSkinEdit12: TSXSkinEdit
            Left = 32
            Top = 296
            Width = 193
            Height = 34
            OnClick = SXSkinButton75Click
            OnDblClick = SXSkinButton75DblClick
            OnMouseDown = SXSkinButton75MouseDown
            OnMouseMove = SXSkinButton75MouseMove
            OnMouseUp = SXSkinButton75MouseUp
            TabOrder = 3
            OnMouseEnter = SXSkinButton75MouseEnter
            OnMouseLeave = SXSkinButton75MouseLeave
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -19
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            FrequentChange = True
            ParentFont = False
            SkinLibrary = SXSkinLibrary1
            Text = 'EDIT'
            OnUserModified = SXSkinEdit12UserModified
          end
          object SXSkinLabel84: TSXSkinLabel
            Left = 264
            Top = 128
            Width = 119
            Height = 13
            Caption = 'Last UserModified Event:'
            SkinLibrary = SXSkinLibrary1
          end
          object SXSkinLabel85: TSXSkinLabel
            Left = 392
            Top = 128
            Width = 18
            Height = 13
            Caption = '___'
            SkinLibrary = SXSkinLibrary1
            Visible = False
          end
          object SXSkinRadioButton74: TSXSkinRadioButton
            Left = 128
            Top = 140
            Width = 84
            Height = 15
            Checked = True
            OnUserModified = SXSkinEdit12UserModified
            OnMouseEnter = SXSkinButton75MouseEnter
            OnMouseLeave = SXSkinButton75MouseLeave
            Caption = 'RadioButton1'
            SkinLibrary = SXSkinLibrary1
            TabOrder = 22
            OnClick = SXSkinButton75Click
            OnDblClick = SXSkinButton75DblClick
            OnMouseDown = SXSkinButton75MouseDown
            OnMouseMove = SXSkinButton75MouseMove
            OnMouseUp = SXSkinButton75MouseUp
          end
          object SXSkinRadioButton75: TSXSkinRadioButton
            Left = 128
            Top = 160
            Width = 84
            Height = 15
            OnUserModified = SXSkinEdit12UserModified
            OnMouseEnter = SXSkinButton75MouseEnter
            OnMouseLeave = SXSkinButton75MouseLeave
            Caption = 'RadioButton2'
            SkinLibrary = SXSkinLibrary1
            TabOrder = 23
            OnClick = SXSkinButton75Click
            OnDblClick = SXSkinButton75DblClick
            OnMouseDown = SXSkinButton75MouseDown
            OnMouseMove = SXSkinButton75MouseMove
            OnMouseUp = SXSkinButton75MouseUp
          end
          object SXSkinGroupBox12: TSXSkinGroupBox
            Left = 32
            Top = 336
            Width = 177
            Height = 57
            OnMouseEnter = SXSkinButton75MouseEnter
            OnMouseLeave = SXSkinButton75MouseLeave
            Caption = 'Simple GroupBox'
            SkinLibrary = SXSkinLibrary1
            TabOrder = 24
            OnClick = SXSkinButton75Click
            OnDblClick = SXSkinButton75DblClick
            OnMouseDown = SXSkinButton75MouseDown
            OnMouseMove = SXSkinButton75MouseMove
            OnMouseUp = SXSkinButton75MouseUp
            object SXSkinLabel88: TSXSkinLabel
              Left = 16
              Top = 32
              Width = 140
              Height = 13
              Caption = 'A control inside the GroupBox'
              SkinLibrary = SXSkinLibrary1
            end
          end
          object SXSkinGroupBox13: TSXSkinGroupBox
            Left = 40
            Top = 400
            Width = 177
            Height = 57
            OnMouseEnter = SXSkinButton75MouseEnter
            OnMouseLeave = SXSkinButton75MouseLeave
            Caption = 'GroupBox with CheckBox'
            GlyphStyle = '_CheckBox'
            GlyphType = gtCheckBox
            SkinLibrary = SXSkinLibrary1
            TabOrder = 25
            TabStop = True
            OnClick = SXSkinButton75Click
            OnDblClick = SXSkinButton75DblClick
            OnMouseDown = SXSkinButton75MouseDown
            OnMouseMove = SXSkinButton75MouseMove
            OnMouseUp = SXSkinButton75MouseUp
            object SXSkinLabel87: TSXSkinLabel
              Left = 16
              Top = 32
              Width = 140
              Height = 13
              Caption = 'A control inside the GroupBox'
              SkinLibrary = SXSkinLibrary1
            end
          end
          object SXSkinGroupBox14: TSXSkinGroupBox
            Left = 48
            Top = 464
            Width = 177
            Height = 57
            OnMouseEnter = SXSkinButton75MouseEnter
            OnMouseLeave = SXSkinButton75MouseLeave
            Caption = 'GroupBox with RadioButton'
            GlyphStyle = '_RadioButton'
            GlyphType = gtRadioButton
            SkinLibrary = SXSkinLibrary1
            TabOrder = 26
            TabStop = True
            OnClick = SXSkinButton75Click
            OnDblClick = SXSkinButton75DblClick
            OnMouseDown = SXSkinButton75MouseDown
            OnMouseMove = SXSkinButton75MouseMove
            OnMouseUp = SXSkinButton75MouseUp
            object SXSkinLabel86: TSXSkinLabel
              Left = 16
              Top = 32
              Width = 140
              Height = 13
              Caption = 'A control inside the GroupBox'
              SkinLibrary = SXSkinLibrary1
            end
          end
          object SXSkinSpinEdit6: TSXSkinSpinEdit
            Left = 112
            Top = 528
            Width = 113
            Height = 34
            Alignment = taRightJustify
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -19
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            MaxValue = 100
            ParentFont = False
            SkinLibrary = SXSkinLibrary1
            Text = '3'
            Value = 3
            OnUserModified = SXSkinEdit12UserModified
            OnClick = SXSkinButton75Click
            OnDblClick = SXSkinButton75DblClick
            OnMouseDown = SXSkinButton75MouseDown
            OnMouseEnter = SXSkinButton75MouseEnter
            OnMouseLeave = SXSkinButton75MouseLeave
            OnMouseMove = SXSkinButton75MouseMove
            OnMouseUp = SXSkinButton75MouseUp
          end
        end
        object SelectiveStylesPage: TSXSkinNotebookPage
          Width = 671
          Height = 607
          Align = alClient
          SkinLibrary = SXSkinLibrary1
          TabOrder = 14
          object SXSkinImage30: TSXSkinImage
            Left = 16
            Top = 16
            Width = 10
            Height = 10
            SkinLibrary = SXSkinLibrary1
            SkinStyle = '_Selective_Folder'
            TabOrder = 0
          end
          object SXSkinImage31: TSXSkinImage
            Left = 96
            Top = 16
            Width = 20
            Height = 20
            SkinLibrary = SXSkinLibrary1
            SkinStyle = '_Selective_Folder'
            TabOrder = 1
          end
          object SXSkinImage32: TSXSkinImage
            Left = 136
            Top = 16
            Width = 30
            Height = 30
            SkinLibrary = SXSkinLibrary1
            SkinStyle = '_Selective_Folder'
            TabOrder = 2
          end
          object SXSkinImage33: TSXSkinImage
            Left = 56
            Top = 16
            Width = 15
            Height = 15
            SkinLibrary = SXSkinLibrary1
            SkinStyle = '_Selective_Folder'
            TabOrder = 3
          end
          object SXSkinImage34: TSXSkinImage
            Left = 176
            Top = 16
            Width = 45
            Height = 45
            SkinLibrary = SXSkinLibrary1
            SkinStyle = '_Selective_Folder'
            TabOrder = 4
          end
          object SXSkinImage35: TSXSkinImage
            Left = 232
            Top = 24
            Width = 60
            Height = 60
            SkinLibrary = SXSkinLibrary1
            SkinStyle = '_Selective_Folder'
            TabOrder = 5
          end
          object SXSkinImage36: TSXSkinImage
            Left = 312
            Top = 24
            Width = 80
            Height = 80
            SkinLibrary = SXSkinLibrary1
            SkinStyle = '_Selective_Folder'
            TabOrder = 6
          end
          object SXSkinImage37: TSXSkinImage
            Left = 408
            Top = 24
            Width = 100
            Height = 100
            SkinLibrary = SXSkinLibrary1
            SkinStyle = '_Selective_Folder'
            TabOrder = 7
          end
          object SXSkinImage38: TSXSkinImage
            Left = 16
            Top = 264
            Width = 16
            Height = 16
            SkinLibrary = SXSkinLibrary1
            SkinStyle = '_Selective_Folder'
            TabOrder = 8
          end
          object SXSkinImage39: TSXSkinImage
            Left = 16
            Top = 288
            Width = 32
            Height = 32
            SkinLibrary = SXSkinLibrary1
            SkinStyle = '_Selective_Folder'
            TabOrder = 9
          end
          object SXSkinImage41: TSXSkinImage
            Left = 16
            Top = 384
            Width = 96
            Height = 96
            SkinLibrary = SXSkinLibrary1
            SkinStyle = '_Selective_Folder'
            TabOrder = 10
          end
          object SXSkinImage40: TSXSkinImage
            Left = 16
            Top = 328
            Width = 48
            Height = 48
            SkinLibrary = SXSkinLibrary1
            SkinStyle = '_Selective_Folder'
            TabOrder = 11
          end
          object SXSkinLabel89: TSXSkinLabel
            Left = 16
            Top = 32
            Width = 29
            Height = 13
            Caption = '10x10'
            SkinLibrary = SXSkinLibrary1
          end
          object SXSkinLabel90: TSXSkinLabel
            Left = 56
            Top = 40
            Width = 29
            Height = 13
            Caption = '15x15'
            SkinLibrary = SXSkinLibrary1
          end
          object SXSkinLabel91: TSXSkinLabel
            Left = 96
            Top = 40
            Width = 29
            Height = 13
            Caption = '20x20'
            SkinLibrary = SXSkinLibrary1
          end
          object SXSkinLabel92: TSXSkinLabel
            Left = 136
            Top = 48
            Width = 29
            Height = 13
            Caption = '30x30'
            SkinLibrary = SXSkinLibrary1
          end
          object SXSkinLabel93: TSXSkinLabel
            Left = 176
            Top = 64
            Width = 29
            Height = 13
            Caption = '45x45'
            SkinLibrary = SXSkinLibrary1
          end
          object SXSkinLabel94: TSXSkinLabel
            Left = 232
            Top = 88
            Width = 29
            Height = 13
            Caption = '60x60'
            SkinLibrary = SXSkinLibrary1
          end
          object SXSkinLabel95: TSXSkinLabel
            Left = 312
            Top = 108
            Width = 29
            Height = 13
            Caption = '80x80'
            SkinLibrary = SXSkinLibrary1
          end
          object SXSkinLabel96: TSXSkinLabel
            Left = 408
            Top = 128
            Width = 41
            Height = 13
            Caption = '100x100'
            SkinLibrary = SXSkinLibrary1
          end
          object SXSkinLabel97: TSXSkinLabel
            Left = 16
            Top = 136
            Width = 353
            Height = 117
            AutoSizeWidth = False
            Caption = 
              'Above you can see 8 TSXSkinImage components.'#13#10'All they have Skin' +
              'Style property set to "_Selective_Folder".'#13#10'Style _Selective_Fol' +
              'der is drawn depending on destination region width:'#13#10'if Width<=1' +
              '6 - folder16.png is used;'#13#10'if Width<=32 - folder32.png is used;'#13 +
              #10'if Width<=48 - folder48.png is used;'#13#10'else folder96.png is used' +
              '.'#13#10#13#10'All PNG-images are shown below:'
            SkinLibrary = SXSkinLibrary1
            WordWrap = True
          end
          object SXSkinLabel98: TSXSkinLabel
            Left = 40
            Top = 264
            Width = 65
            Height = 16
            AutoSizeHeight = False
            Caption = '- folder16.png'
            SkinLibrary = SXSkinLibrary1
            VerticalAlignment = taVerticalCenter
          end
          object SXSkinLabel99: TSXSkinLabel
            Left = 56
            Top = 288
            Width = 65
            Height = 32
            AutoSizeHeight = False
            Caption = '- folder32.png'
            SkinLibrary = SXSkinLibrary1
            VerticalAlignment = taVerticalCenter
          end
          object SXSkinLabel100: TSXSkinLabel
            Left = 72
            Top = 328
            Width = 65
            Height = 48
            AutoSizeHeight = False
            Caption = '- folder48.png'
            SkinLibrary = SXSkinLibrary1
            VerticalAlignment = taVerticalCenter
          end
          object SXSkinLabel101: TSXSkinLabel
            Left = 120
            Top = 384
            Width = 65
            Height = 96
            AutoSizeHeight = False
            Caption = '- folder96.png'
            SkinLibrary = SXSkinLibrary1
            VerticalAlignment = taVerticalCenter
          end
          object SXSkinLabel102: TSXSkinLabel
            Left = 304
            Top = 264
            Width = 197
            Height = 13
            Caption = 'Try to resize an image with selective-style:'
            SkinLibrary = SXSkinLibrary1
          end
          object SXSkinLabel103: TSXSkinLabel
            Left = 304
            Top = 292
            Width = 31
            Height = 13
            Caption = 'Width:'
            SkinLibrary = SXSkinLibrary1
          end
          object SXSkinLabel104: TSXSkinLabel
            Left = 408
            Top = 292
            Width = 26
            Height = 13
            Caption = 'pixels'
            SkinLibrary = SXSkinLibrary1
          end
          object SXSkinImage42: TSXSkinImage
            Left = 304
            Top = 320
            Width = 100
            Height = 100
            SkinLibrary = SXSkinLibrary1
            SkinStyle = '_Selective_Folder'
            TabOrder = 28
          end
          object SXSkinSpinEdit19: TSXSkinSpinEdit
            Left = 344
            Top = 288
            Width = 57
            Height = 23
            Alignment = taRightJustify
            MaxLength = 3
            MaxValue = 300
            MinValue = 1
            SkinLibrary = SXSkinLibrary1
            Text = '100'
            Value = 100
            OnUserModified = SXSkinSpinEdit19UserModified
          end
        end
        object AllInOnePage: TSXSkinNotebookPage
          Width = 671
          Height = 607
          Align = alClient
          SkinLibrary = SXSkinLibrary1
          TabOrder = 15
          Visible = False
          object SXSkinImage43: TSXSkinImage
            Left = 8
            Top = 8
            Width = 100
            Height = 100
            SkinLibrary = SXSkinLibrary1
            SkinStyle = 'MyComputer_S3'
            TabOrder = 0
          end
          object SXSkinLabel105: TSXSkinLabel
            Left = 8
            Top = 104
            Width = 100
            Height = 29
            Alignment = taCenter
            AutoSizeWidth = False
            Caption = 'IMAGE'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -20
            Font.Name = 'Comic Sans MS'
            Font.Style = [fsBold]
            ParentFont = False
            SkinLibrary = SXSkinLibrary1
            UseCustomFont = True
          end
          object SXSkinButton77: TSXSkinButton
            Left = 208
            Top = 152
            Width = 153
            Height = 41
            Caption = 'Normal Button'
            GlyphStyle = 'MyComputer'
            SkinLibrary = SXSkinLibrary1
            TabOrder = 2
          end
          object SXSkinGroupBox15: TSXSkinGroupBox
            Left = 8
            Top = 144
            Width = 177
            Height = 89
            Caption = 'GroupBox with CheckBox'
            GlyphStyle = '_CheckBox'
            GlyphType = gtCheckBox
            SkinLibrary = SXSkinLibrary1
            TabOrder = 3
            TabStop = True
            object SXSkinRadioButton76: TSXSkinRadioButton
              Left = 8
              Top = 24
              Width = 124
              Height = 15
              Caption = 'Checked RadioButton'
              SkinLibrary = SXSkinLibrary1
              TabOrder = 0
            end
            object SXSkinRadioButton77: TSXSkinRadioButton
              Left = 8
              Top = 40
              Width = 137
              Height = 15
              Caption = 'Unchecked RadioButton'
              SkinLibrary = SXSkinLibrary1
              TabOrder = 1
            end
            object SXSkinRadioButton78: TSXSkinRadioButton
              Left = 8
              Top = 56
              Width = 122
              Height = 15
              Checked = True
              Caption = 'Disabled RadioButton'
              Enabled = False
              SkinLibrary = SXSkinLibrary1
              TabOrder = 2
            end
          end
          object SXSkinGroupBox16: TSXSkinGroupBox
            Left = 384
            Top = 144
            Width = 121
            Height = 81
            Caption = 'Disabled GroupBox'
            Enabled = False
            SkinLibrary = SXSkinLibrary1
            TabOrder = 4
          end
          object SXSkinGroupBox17: TSXSkinGroupBox
            Left = 128
            Top = 24
            Width = 185
            Height = 81
            Caption = 'GroupBox with RadioButton'
            GlyphStyle = '_RadioButton'
            GlyphType = gtRadioButton
            SkinLibrary = SXSkinLibrary1
            State = cbChecked
            TabOrder = 5
            TabStop = True
            object SXSkinEdit14: TSXSkinEdit
              Left = 8
              Top = 24
              Width = 169
              Height = 23
              TabOrder = 0
              SkinLibrary = SXSkinLibrary1
              Text = 'Normal Edit'
            end
            object SXSkinEdit15: TSXSkinEdit
              Left = 8
              Top = 48
              Width = 169
              Height = 23
              TabOrder = 1
              Enabled = False
              SkinLibrary = SXSkinLibrary1
              Text = 'Disabled Edit'
            end
          end
          object SXSkinGroupBox18: TSXSkinGroupBox
            Left = 328
            Top = 24
            Width = 177
            Height = 81
            Caption = 'GroupBox with RadioButton'
            GlyphStyle = '_RadioButton'
            GlyphType = gtRadioButton
            SkinLibrary = SXSkinLibrary1
            TabOrder = 6
            TabStop = True
            object SXSkinLabel106: TSXSkinLabel
              Left = 8
              Top = 24
              Width = 62
              Height = 13
              Caption = 'Normal Label'
              SkinLibrary = SXSkinLibrary1
            end
            object SXSkinLabel107: TSXSkinLabel
              Left = 8
              Top = 40
              Width = 70
              Height = 13
              Caption = 'Disabled Label'
              Enabled = False
              SkinLibrary = SXSkinLibrary1
            end
          end
          object SXSkinGroupBox19: TSXSkinGroupBox
            Left = 8
            Top = 240
            Width = 177
            Height = 97
            Caption = 'Simple GroupBox'
            SkinLibrary = SXSkinLibrary1
            TabOrder = 7
            object SXSkinCheckBox42: TSXSkinCheckBox
              Left = 8
              Top = 40
              Width = 128
              Height = 15
              Caption = 'Unchecked CheckBox'
              SkinLibrary = SXSkinLibrary1
              TabOrder = 0
            end
            object SXSkinCheckBox43: TSXSkinCheckBox
              Left = 8
              Top = 56
              Width = 106
              Height = 15
              AllowGrayed = True
              Caption = 'Grayed CheckBox'
              SkinLibrary = SXSkinLibrary1
              State = cbGrayed
              TabOrder = 1
            end
            object SXSkinCheckBox44: TSXSkinCheckBox
              Left = 8
              Top = 24
              Width = 115
              Height = 15
              Caption = 'Checked CheckBox'
              SkinLibrary = SXSkinLibrary1
              State = cbChecked
              TabOrder = 2
            end
            object SXSkinCheckBox45: TSXSkinCheckBox
              Left = 8
              Top = 72
              Width = 113
              Height = 15
              Caption = 'Disabled CheckBox'
              Enabled = False
              SkinLibrary = SXSkinLibrary1
              State = cbChecked
              TabOrder = 3
            end
          end
          object SXSkinButton78: TSXSkinButton
            Left = 208
            Top = 200
            Width = 153
            Height = 41
            Caption = 'Disabled Button'
            Enabled = False
            GlyphStyle = 'MyComputer'
            SkinLibrary = SXSkinLibrary1
            TabOrder = 8
          end
          object SXSkinButton79: TSXSkinButton
            Left = 208
            Top = 248
            Width = 153
            Height = 41
            Caption = 'DropDown Button'
            DropDown = True
            DropDownMenu = PopupMenu1
            GlyphStyle = 'MyComputer'
            SkinLibrary = SXSkinLibrary1
            TabOrder = 9
          end
          object SXSkinButton80: TSXSkinButton
            Left = 208
            Top = 296
            Width = 153
            Height = 41
            Caption = 'Disabled DropDown'
            DropDown = True
            Enabled = False
            GlyphStyle = 'MyComputer'
            SkinLibrary = SXSkinLibrary1
            TabOrder = 10
          end
          object SXSkinButton81: TSXSkinButton
            Left = 385
            Top = 240
            Width = 120
            Height = 97
            Alignment = taCenter
            CanBeChecked = True
            Caption = 'Button with CheckBox Glyph'
            CaptionLeftOffset = 2
            CaptionRightOffset = 2
            GlyphPosition = gpTop
            GlyphStyle = '_MultiStateCheck_CheckBox'
            SkinLibrary = SXSkinLibrary1
            TabOrder = 11
            WordWrap = True
          end
          object SXSkinGroupBox20: TSXSkinGroupBox
            Left = 8
            Top = 344
            Width = 281
            Height = 129
            Caption = 'Form Parameters'
            SkinLibrary = SXSkinLibrary1
            TabOrder = 12
            object SXSkinPanel33: TSXSkinPanel
              Left = 8
              Top = 16
              Width = 89
              Height = 105
              SkinLibrary = SXSkinLibrary1
              TabOrder = 0
              object SXSkinRadioButton79: TSXSkinRadioButton
                Top = 24
                Width = 59
                Height = 15
                Checked = True
                OnUserModified = SXSkinRadioButton79UserModified
                Caption = 'Sizeable'
                SkinLibrary = SXSkinLibrary1
                TabOrder = 0
              end
              object SXSkinRadioButton80: TSXSkinRadioButton
                Top = 40
                Width = 48
                Height = 15
                OnUserModified = SXSkinRadioButton79UserModified
                Caption = 'Single'
                SkinLibrary = SXSkinLibrary1
                TabOrder = 1
              end
              object SXSkinLabel108: TSXSkinLabel
                Left = 16
                Top = 8
                Width = 57
                Height = 13
                Caption = 'BorderStyle:'
                SkinLibrary = SXSkinLibrary1
              end
              object SXSkinRadioButton81: TSXSkinRadioButton
                Top = 56
                Width = 49
                Height = 15
                OnUserModified = SXSkinRadioButton79UserModified
                Caption = 'Dialog'
                SkinLibrary = SXSkinLibrary1
                TabOrder = 3
              end
              object SXSkinRadioButton82: TSXSkinRadioButton
                Top = 72
                Width = 79
                Height = 15
                OnUserModified = SXSkinRadioButton79UserModified
                Caption = 'ToolWindow'
                SkinLibrary = SXSkinLibrary1
                TabOrder = 4
              end
              object SXSkinRadioButton83: TSXSkinRadioButton
                Top = 88
                Width = 79
                Height = 15
                OnUserModified = SXSkinRadioButton79UserModified
                Caption = 'SizeToolWin'
                SkinLibrary = SXSkinLibrary1
                TabOrder = 5
              end
            end
            object SXSkinPanel34: TSXSkinPanel
              Left = 104
              Top = 16
              Width = 73
              Height = 105
              SkinLibrary = SXSkinLibrary1
              TabOrder = 1
              object SXSkinLabel109: TSXSkinLabel
                Top = 8
                Width = 70
                Height = 13
                Caption = 'CaptionHeight:'
                SkinLibrary = SXSkinLibrary1
              end
              object SXSkinRadioButton84: TSXSkinRadioButton
                Left = 8
                Top = 24
                Width = 53
                Height = 15
                Checked = True
                OnUserModified = SXSkinRadioButton84UserModified
                Caption = 'Default'
                SkinLibrary = SXSkinLibrary1
                TabOrder = 1
              end
              object SXSkinRadioButton85: TSXSkinRadioButton
                Left = 8
                Top = 40
                Width = 31
                Height = 15
                OnUserModified = SXSkinRadioButton84UserModified
                Caption = '15'
                SkinLibrary = SXSkinLibrary1
                TabOrder = 2
              end
              object SXSkinRadioButton86: TSXSkinRadioButton
                Left = 8
                Top = 56
                Width = 31
                Height = 15
                OnUserModified = SXSkinRadioButton84UserModified
                Caption = '25'
                SkinLibrary = SXSkinLibrary1
                TabOrder = 3
              end
              object SXSkinRadioButton87: TSXSkinRadioButton
                Left = 8
                Top = 72
                Width = 31
                Height = 15
                OnUserModified = SXSkinRadioButton84UserModified
                Caption = '50'
                SkinLibrary = SXSkinLibrary1
                TabOrder = 4
              end
              object SXSkinRadioButton88: TSXSkinRadioButton
                Left = 8
                Top = 88
                Width = 37
                Height = 15
                OnUserModified = SXSkinRadioButton84UserModified
                Caption = '100'
                SkinLibrary = SXSkinLibrary1
                TabOrder = 5
              end
            end
            object SXSkinPanel35: TSXSkinPanel
              Left = 184
              Top = 16
              Width = 89
              Height = 105
              SkinLibrary = SXSkinLibrary1
              TabOrder = 2
              object SXSkinLabel110: TSXSkinLabel
                Left = 16
                Top = 8
                Width = 60
                Height = 13
                Caption = 'BorderIcons:'
                SkinLibrary = SXSkinLibrary1
              end
              object SXSkinCheckBox46: TSXSkinCheckBox
                Top = 24
                Width = 81
                Height = 15
                OnUserModified = SXSkinCheckBox46UserModified
                Caption = 'SystemMenu'
                SkinLibrary = SXSkinLibrary1
                State = cbChecked
                TabOrder = 1
              end
              object SXSkinCheckBox47: TSXSkinCheckBox
                Top = 40
                Width = 63
                Height = 15
                OnUserModified = SXSkinCheckBox46UserModified
                Caption = 'Maximize'
                SkinLibrary = SXSkinLibrary1
                State = cbChecked
                TabOrder = 2
              end
              object SXSkinCheckBox48: TSXSkinCheckBox
                Top = 56
                Width = 60
                Height = 15
                OnUserModified = SXSkinCheckBox46UserModified
                Caption = 'Minimize'
                SkinLibrary = SXSkinLibrary1
                State = cbChecked
                TabOrder = 3
              end
              object SXSkinCheckBox49: TSXSkinCheckBox
                Top = 72
                Width = 42
                Height = 15
                OnUserModified = SXSkinCheckBox46UserModified
                Caption = 'Help'
                SkinLibrary = SXSkinLibrary1
                TabOrder = 4
              end
            end
          end
          object SXSkinUpDown2: TSXSkinUpDown
            Left = 304
            Top = 352
            Width = 25
            Height = 49
            SkinLibrary = SXSkinLibrary1
            TabOrder = 13
          end
          object SXSkinSpinEdit11: TSXSkinSpinEdit
            Left = 336
            Top = 352
            Width = 81
            Height = 23
            SkinLibrary = SXSkinLibrary1
            Text = '12345'
            Value = 12345
          end
          object SXSkinSpinEdit12: TSXSkinSpinEdit
            Left = 336
            Top = 376
            Width = 81
            Height = 23
            ButtonsPosition = bpLeft
            Horizontal = True
            SkinLibrary = SXSkinLibrary1
            SkinStyle = '_SpinEditLH'
            Text = '12345'
            Value = 12345
          end
          object SXSkinSpinEdit13: TSXSkinSpinEdit
            Left = 424
            Top = 352
            Width = 81
            Height = 23
            Enabled = False
            SkinLibrary = SXSkinLibrary1
            Text = '12345'
            Value = 12345
          end
          object SXSkinSpinEdit14: TSXSkinSpinEdit
            Left = 424
            Top = 376
            Width = 81
            Height = 23
            ButtonsPosition = bpLeft
            Enabled = False
            Horizontal = True
            SkinLibrary = SXSkinLibrary1
            SkinStyle = '_SpinEditLH'
            Text = '12345'
            Value = 12345
          end
        end
        object UpDownPage: TSXSkinNotebookPage
          Width = 671
          Height = 607
          Align = alClient
          SkinLibrary = SXSkinLibrary1
          TabOrder = 16
          Visible = False
          object SXSkinUpDown3: TSXSkinUpDown
            Left = 8
            Top = 32
            Width = 15
            Height = 15
            OnClick = SXSkinUpDown1Click
            SkinLibrary = SXSkinLibrary1
            TabOrder = 0
          end
          object SXSkinUpDown1: TSXSkinUpDown
            Left = 56
            Top = 32
            Width = 49
            Height = 19
            OnClick = SXSkinUpDown1Click
            SkinLibrary = SXSkinLibrary1
            TabOrder = 1
          end
          object SXSkinLabel112: TSXSkinLabel
            Left = 8
            Top = 8
            Width = 121
            Height = 13
            Caption = 'Vertical UpDown Controls'
            SkinLibrary = SXSkinLibrary1
          end
          object SXSkinUpDown4: TSXSkinUpDown
            Left = 32
            Top = 32
            Width = 15
            Height = 20
            OnClick = SXSkinUpDown1Click
            SkinLibrary = SXSkinLibrary1
            TabOrder = 3
          end
          object SXSkinEdit16: TSXSkinEdit
            Left = 8
            Top = 112
            Width = 121
            Height = 23
            TabOrder = 4
            Alignment = taRightJustify
            SkinLibrary = SXSkinLibrary1
            Text = '0'
          end
          object SXSkinUpDown5: TSXSkinUpDown
            Left = 112
            Top = 32
            Width = 17
            Height = 73
            OnClick = SXSkinUpDown1Click
            SkinLibrary = SXSkinLibrary1
            TabOrder = 5
          end
          object SXSkinUpDown6: TSXSkinUpDown
            Left = 8
            Top = 56
            Width = 97
            Height = 49
            OnClick = SXSkinUpDown1Click
            SkinLibrary = SXSkinLibrary1
            TabOrder = 6
          end
          object SXSkinUpDown7: TSXSkinUpDown
            Left = 8
            Top = 192
            Width = 97
            Height = 49
            OnClick = SXSkinUpDown1Click
            Enabled = False
            SkinLibrary = SXSkinLibrary1
            TabOrder = 7
          end
          object SXSkinUpDown8: TSXSkinUpDown
            Left = 112
            Top = 168
            Width = 17
            Height = 73
            OnClick = SXSkinUpDown1Click
            Enabled = False
            SkinLibrary = SXSkinLibrary1
            TabOrder = 8
          end
          object SXSkinUpDown9: TSXSkinUpDown
            Left = 56
            Top = 168
            Width = 49
            Height = 19
            OnClick = SXSkinUpDown1Click
            Enabled = False
            SkinLibrary = SXSkinLibrary1
            TabOrder = 9
          end
          object SXSkinUpDown10: TSXSkinUpDown
            Left = 32
            Top = 168
            Width = 15
            Height = 20
            OnClick = SXSkinUpDown1Click
            Enabled = False
            SkinLibrary = SXSkinLibrary1
            TabOrder = 10
          end
          object SXSkinUpDown11: TSXSkinUpDown
            Left = 8
            Top = 168
            Width = 15
            Height = 15
            OnClick = SXSkinUpDown1Click
            Enabled = False
            SkinLibrary = SXSkinLibrary1
            TabOrder = 11
          end
          object SXSkinLabel113: TSXSkinLabel
            Left = 8
            Top = 144
            Width = 82
            Height = 13
            Caption = 'Disabled Controls'
            SkinLibrary = SXSkinLibrary1
          end
          object SXSkinLabel114: TSXSkinLabel
            Left = 8
            Top = 248
            Width = 121
            Height = 13
            Caption = 'Partially Disabled Controls'
            SkinLibrary = SXSkinLibrary1
          end
          object SXSkinUpDown12: TSXSkinUpDown
            Left = 8
            Top = 272
            Width = 15
            Height = 15
            EnabledDown = False
            OnClick = SXSkinUpDown1Click
            SkinLibrary = SXSkinLibrary1
            TabOrder = 14
          end
          object SXSkinUpDown13: TSXSkinUpDown
            Left = 32
            Top = 272
            Width = 15
            Height = 20
            EnabledUp = False
            OnClick = SXSkinUpDown1Click
            SkinLibrary = SXSkinLibrary1
            TabOrder = 15
          end
          object SXSkinUpDown14: TSXSkinUpDown
            Left = 56
            Top = 272
            Width = 49
            Height = 19
            EnabledDown = False
            OnClick = SXSkinUpDown1Click
            SkinLibrary = SXSkinLibrary1
            TabOrder = 16
          end
          object SXSkinUpDown15: TSXSkinUpDown
            Left = 112
            Top = 272
            Width = 17
            Height = 73
            EnabledUp = False
            OnClick = SXSkinUpDown1Click
            SkinLibrary = SXSkinLibrary1
            TabOrder = 17
          end
          object SXSkinUpDown16: TSXSkinUpDown
            Left = 8
            Top = 296
            Width = 97
            Height = 49
            EnabledDown = False
            OnClick = SXSkinUpDown1Click
            SkinLibrary = SXSkinLibrary1
            TabOrder = 18
          end
          object SXSkinUpDown17: TSXSkinUpDown
            Left = 152
            Top = 32
            Width = 15
            Height = 15
            OnClick = SXSkinUpDown17Click
            SkinLibrary = SXSkinLibrary1
            TabOrder = 19
          end
          object SXSkinUpDown18: TSXSkinUpDown
            Left = 200
            Top = 32
            Width = 57
            Height = 19
            Horizontal = True
            OnClick = SXSkinUpDown17Click
            SkinLibrary = SXSkinLibrary1
            SkinStyle = '_UpDownH'
            TabOrder = 20
          end
          object SXSkinLabel115: TSXSkinLabel
            Left = 152
            Top = 8
            Width = 133
            Height = 13
            Caption = 'Horizontal UpDown Controls'
            SkinLibrary = SXSkinLibrary1
          end
          object SXSkinUpDown19: TSXSkinUpDown
            Left = 176
            Top = 32
            Width = 15
            Height = 20
            Horizontal = True
            OnClick = SXSkinUpDown17Click
            SkinLibrary = SXSkinLibrary1
            SkinStyle = '_UpDownH'
            TabOrder = 22
          end
          object SXSkinEdit17: TSXSkinEdit
            Left = 152
            Top = 112
            Width = 137
            Height = 23
            TabOrder = 23
            Alignment = taRightJustify
            SkinLibrary = SXSkinLibrary1
            Text = '0'
          end
          object SXSkinUpDown20: TSXSkinUpDown
            Left = 263
            Top = 33
            Width = 25
            Height = 57
            Horizontal = True
            OnClick = SXSkinUpDown17Click
            SkinLibrary = SXSkinLibrary1
            SkinStyle = '_UpDownH'
            TabOrder = 24
          end
          object SXSkinUpDown21: TSXSkinUpDown
            Left = 152
            Top = 56
            Width = 105
            Height = 33
            Horizontal = True
            OnClick = SXSkinUpDown17Click
            SkinLibrary = SXSkinLibrary1
            SkinStyle = '_UpDownH'
            TabOrder = 25
          end
          object SXSkinUpDown22: TSXSkinUpDown
            Left = 152
            Top = 192
            Width = 105
            Height = 33
            Horizontal = True
            OnClick = SXSkinUpDown17Click
            Enabled = False
            SkinLibrary = SXSkinLibrary1
            SkinStyle = '_UpDownH'
            TabOrder = 26
          end
          object SXSkinUpDown23: TSXSkinUpDown
            Left = 264
            Top = 168
            Width = 25
            Height = 57
            Horizontal = True
            OnClick = SXSkinUpDown17Click
            Enabled = False
            SkinLibrary = SXSkinLibrary1
            SkinStyle = '_UpDownH'
            TabOrder = 27
          end
          object SXSkinUpDown24: TSXSkinUpDown
            Left = 200
            Top = 168
            Width = 57
            Height = 19
            Horizontal = True
            OnClick = SXSkinUpDown17Click
            Enabled = False
            SkinLibrary = SXSkinLibrary1
            SkinStyle = '_UpDownH'
            TabOrder = 28
          end
          object SXSkinUpDown25: TSXSkinUpDown
            Left = 176
            Top = 168
            Width = 15
            Height = 20
            Horizontal = True
            OnClick = SXSkinUpDown17Click
            Enabled = False
            SkinLibrary = SXSkinLibrary1
            SkinStyle = '_UpDownH'
            TabOrder = 29
          end
          object SXSkinUpDown26: TSXSkinUpDown
            Left = 152
            Top = 168
            Width = 15
            Height = 15
            Horizontal = True
            OnClick = SXSkinUpDown17Click
            Enabled = False
            SkinLibrary = SXSkinLibrary1
            SkinStyle = '_UpDownH'
            TabOrder = 30
          end
          object SXSkinLabel116: TSXSkinLabel
            Left = 152
            Top = 144
            Width = 82
            Height = 13
            Caption = 'Disabled Controls'
            SkinLibrary = SXSkinLibrary1
          end
          object SXSkinLabel117: TSXSkinLabel
            Left = 152
            Top = 248
            Width = 121
            Height = 13
            Caption = 'Partially Disabled Controls'
            SkinLibrary = SXSkinLibrary1
          end
          object SXSkinUpDown27: TSXSkinUpDown
            Left = 152
            Top = 272
            Width = 15
            Height = 15
            EnabledDown = False
            Horizontal = True
            OnClick = SXSkinUpDown17Click
            SkinLibrary = SXSkinLibrary1
            SkinStyle = '_UpDownH'
            TabOrder = 33
          end
          object SXSkinUpDown28: TSXSkinUpDown
            Left = 176
            Top = 272
            Width = 15
            Height = 20
            EnabledUp = False
            Horizontal = True
            OnClick = SXSkinUpDown17Click
            SkinLibrary = SXSkinLibrary1
            SkinStyle = '_UpDownH'
            TabOrder = 34
          end
          object SXSkinUpDown29: TSXSkinUpDown
            Left = 200
            Top = 272
            Width = 57
            Height = 19
            EnabledDown = False
            Horizontal = True
            OnClick = SXSkinUpDown17Click
            SkinLibrary = SXSkinLibrary1
            SkinStyle = '_UpDownH'
            TabOrder = 35
          end
          object SXSkinUpDown30: TSXSkinUpDown
            Left = 264
            Top = 272
            Width = 25
            Height = 57
            EnabledUp = False
            Horizontal = True
            OnClick = SXSkinUpDown17Click
            SkinLibrary = SXSkinLibrary1
            SkinStyle = '_UpDownH'
            TabOrder = 36
          end
          object SXSkinUpDown31: TSXSkinUpDown
            Left = 152
            Top = 296
            Width = 105
            Height = 33
            EnabledDown = False
            Horizontal = True
            OnClick = SXSkinUpDown17Click
            SkinLibrary = SXSkinLibrary1
            SkinStyle = '_UpDownH'
            TabOrder = 37
          end
          object SXSkinSpinEdit1: TSXSkinSpinEdit
            Left = 328
            Top = 88
            Width = 93
            Height = 23
            Alignment = taRightJustify
            SkinLibrary = SXSkinLibrary1
            Text = '0'
            Value = 0
          end
          object SXSkinSpinEdit5: TSXSkinSpinEdit
            Left = 328
            Top = 32
            Width = 113
            Height = 34
            Alignment = taRightJustify
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -19
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
            SkinLibrary = SXSkinLibrary1
            Text = '1234'
            Value = 1234
          end
          object SXSkinButton82: TSXSkinButton
            Left = 448
            Top = 32
            Width = 73
            Height = 33
            OnClick = SXSkinButton82Click
            Caption = 'Disable'
            SkinLibrary = SXSkinLibrary1
            TabOrder = 40
          end
          object SXSkinSpinEdit2: TSXSkinSpinEdit
            Left = 432
            Top = 88
            Width = 89
            Height = 23
            Alignment = taCenter
            Horizontal = True
            SkinLibrary = SXSkinLibrary1
            SkinStyle = '_SpinEditRH'
            Text = '0'
            Value = 0
          end
          object SXSkinSpinEdit3: TSXSkinSpinEdit
            Left = 328
            Top = 120
            Width = 93
            Height = 23
            ButtonsPosition = bpLeft
            SkinLibrary = SXSkinLibrary1
            SkinStyle = '_SpinEditLV'
            Text = '0'
            Value = 0
          end
          object SXSkinSpinEdit4: TSXSkinSpinEdit
            Left = 432
            Top = 120
            Width = 89
            Height = 23
            Alignment = taRightJustify
            ButtonsPosition = bpLeft
            Horizontal = True
            SkinLibrary = SXSkinLibrary1
            SkinStyle = '_SpinEditLH'
            Text = '0'
            Value = 0
          end
          object SXSkinLabel118: TSXSkinLabel
            Left = 328
            Top = 152
            Width = 117
            Height = 13
            Caption = 'Custom Height Spin Edit:'
            SkinLibrary = SXSkinLibrary1
          end
          object SXSkinSpinEdit7: TSXSkinSpinEdit
            Left = 328
            Top = 168
            Width = 193
            Height = 41
            AutoSizeHeight = False
            SkinLibrary = SXSkinLibrary1
            Text = '0'
            Value = 0
          end
          object SXSkinLabel119: TSXSkinLabel
            Left = 328
            Top = 8
            Width = 80
            Height = 13
            Caption = 'SpinEdit Controls'
            SkinLibrary = SXSkinLibrary1
          end
          object SXSkinLabel120: TSXSkinLabel
            Left = 328
            Top = 70
            Width = 35
            Height = 13
            Caption = 'Vertical'
            SkinLibrary = SXSkinLibrary1
          end
          object SXSkinLabel121: TSXSkinLabel
            Left = 432
            Top = 70
            Width = 47
            Height = 13
            Caption = 'Horizontal'
            SkinLibrary = SXSkinLibrary1
          end
          object SXSkinSpinEdit8: TSXSkinSpinEdit
            Left = 328
            Top = 232
            Width = 105
            Height = 23
            Alignment = taRightJustify
            EditorEnabled = False
            SkinLibrary = SXSkinLibrary1
            Text = '0'
            Value = 0
          end
          object SXSkinLabel122: TSXSkinLabel
            Left = 328
            Top = 214
            Width = 106
            Height = 13
            Caption = 'Without EditorEnabled'
            SkinLibrary = SXSkinLibrary1
          end
          object SXSkinLabel123: TSXSkinLabel
            Left = 448
            Top = 214
            Width = 47
            Height = 13
            Caption = 'ReadOnly'
            SkinLibrary = SXSkinLibrary1
          end
          object SXSkinSpinEdit9: TSXSkinSpinEdit
            Left = 448
            Top = 232
            Width = 73
            Height = 23
            Alignment = taRightJustify
            EditorEnabled = False
            ReadOnly = True
            SkinLibrary = SXSkinLibrary1
            Text = '2454'
            Value = 2454
          end
        end
      end
      object SXSkinPanel3: TSXSkinPanel
        Width = 671
        Height = 33
        Align = alTop
        SkinLibrary = SXSkinLibrary1
        TabOrder = 1
        object SXSkinLabel1: TSXSkinLabel
          Width = 63
          Height = 33
          Align = alLeft
          AutoSizeHeight = False
          Caption = '  Select Skin:'
          SkinLibrary = SXSkinLibrary1
          VerticalAlignment = taVerticalCenter
        end
        object SXSkinPanel7: TSXSkinPanel
          Left = 63
          Width = 543
          Height = 33
          Align = alClient
          SkinLibrary = SXSkinLibrary1
          TabOrder = 1
          DesignSize = (
            543
            33)
          object SXSkinButton7: TSXSkinButton
            Left = 476
            Top = 5
            Width = 23
            Height = 23
            OnClick = SXSkinButton7Click
            Anchors = [akTop, akRight]
            GlyphStyle = '_MultiState_Left'
            SkinLibrary = SXSkinLibrary1
            TabOrder = 0
          end
          object SXSkinButton8: TSXSkinButton
            Left = 504
            Top = 5
            Width = 23
            Height = 23
            OnClick = SXSkinButton8Click
            Anchors = [akTop, akRight]
            GlyphStyle = '_MultiState_Right'
            SkinLibrary = SXSkinLibrary1
            TabOrder = 1
          end
          object ComboBox1: TComboBox
            Left = 6
            Top = 6
            Width = 464
            Height = 22
            Style = csOwnerDrawFixed
            Anchors = [akLeft, akTop, akRight]
            ItemHeight = 16
            TabOrder = 2
            OnChange = ComboBox1Change
          end
        end
        object SXSkinCheckBox3: TSXSkinCheckBox
          Left = 612
          Width = 59
          Height = 33
          OnUserModified = SXSkinCheckBox3UserModified
          Align = alRight
          AutoSizeHeight = False
          Caption = 'Regions'
          Enabled = False
          GlyphPosition = gpLeft
          SkinLibrary = SXSkinLibrary1
          TabOrder = 2
          VerticalAlignment = taVerticalCenter
          OnResize = SXSkinCheckBox3Resize
        end
        object SXSkinPanel31: TSXSkinPanel
          Left = 606
          Width = 6
          Height = 33
          Align = alRight
          PopupMenu = PopupMenu2
          SkinLibrary = SXSkinLibrary1
          TabOrder = 3
        end
      end
      object SXSkinPanel9: TSXSkinPanel
        Top = 33
        Width = 671
        Height = 25
        Align = alTop
        SkinLibrary = SXSkinLibrary1
        TabOrder = 2
        object SXSkinLabel5: TSXSkinLabel
          Width = 63
          Height = 25
          Align = alLeft
          AutoSizeHeight = False
          Caption = '  Select Test:'
          SkinLibrary = SXSkinLibrary1
          VerticalAlignment = taVerticalCenter
        end
        object SXSkinPanel11: TSXSkinPanel
          Left = 63
          Width = 608
          Height = 25
          Align = alClient
          SkinLibrary = SXSkinLibrary1
          TabOrder = 1
          DesignSize = (
            608
            25)
          object SXSkinButton6: TSXSkinButton
            Left = 557
            Top = 1
            Width = 23
            Height = 23
            OnClick = SXSkinButton6Click
            Anchors = [akTop, akRight]
            GlyphStyle = '_MultiState_Right'
            SkinLibrary = SXSkinLibrary1
            TabOrder = 0
          end
          object SXSkinButton9: TSXSkinButton
            Left = 529
            Top = 1
            Width = 23
            Height = 23
            OnClick = SXSkinButton9Click
            Anchors = [akTop, akRight]
            Enabled = False
            GlyphStyle = '_MultiState_Left'
            SkinLibrary = SXSkinLibrary1
            TabOrder = 1
          end
          object ComboBox2: TComboBox
            Left = 6
            Top = 2
            Width = 518
            Height = 22
            Style = csOwnerDrawFixed
            Anchors = [akLeft, akTop, akRight]
            ItemHeight = 16
            ItemIndex = 0
            TabOrder = 2
            Text = 'About'
            OnChange = ComboBox2Change
            Items.Strings = (
              'About'
              'All Components'
              'PaintBox'
              'Mouse Capture'
              'Label'
              'CheckBox'
              'RadioButton'
              'Button'
              'UpDown & SpinEdit'
              'Image'
              'GroupBox'
              'Edit'
              'Slide Transform Effects'
              'OverDraw Transform Effects'
              'Custom Effect Tool'
              'Events Check'
              'Selective Styles')
          end
        end
      end
    end
  end
  object SXSkinLibrary1: TSXSkinLibrary
    SkinDir = 'I:\Program Files\Borland\Delphi6\SXComponents\SXSkin\Test\Skins'
    SkinFile = '911_911.zip'
    SkinFile2 = 'App_Skins\skin.ini'
    Left = 8
    Top = 568
  end
  object Timer1: TTimer
    Interval = 20
    OnTimer = Timer1Timer
    Left = 8
    Top = 600
  end
  object PopupMenu1: TPopupMenu
    Left = 8
    Top = 632
    object DropDownMenu2: TMenuItem
      Caption = 'Drop Down Menu'
    end
    object MenuItem12: TMenuItem
      Caption = 'Menu Item 1'
    end
    object MenuItem22: TMenuItem
      Caption = 'Menu Item 2'
    end
  end
  object Timer2: TTimer
    Interval = 50
    OnTimer = Timer2Timer
    Left = 40
    Top = 600
  end
  object Timer3: TTimer
    Interval = 40
    OnTimer = Timer3Timer
    Left = 72
    Top = 600
  end
  object PopupMenu2: TPopupMenu
    Left = 40
    Top = 632
    object EnableThisCheckBoxUsewithCaution1: TMenuItem
      Caption = 'Enable This Check Box: Use with Caution!!!'
      OnClick = EnableThisCheckBoxUsewithCaution1Click
    end
  end
  object SXSkinForm1: TSXSkinForm
    IconStyle = '_Selective_StdIcon'
    SkinLibrary = SXSkinLibrary1
    Left = 40
    Top = 568
  end
  object Timer4: TTimer
    Left = 448
    Top = 416
  end
end
