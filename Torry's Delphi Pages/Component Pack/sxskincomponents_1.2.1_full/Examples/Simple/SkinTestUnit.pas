unit SkinTestUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, SXSkinLibrary, SXSkinControl, SXSkinImage, SXSkinPanel, StdCtrls,
  ExtCtrls, SXSkinCheckBox, IniFiles, SXSkinLabel, SXSkinRadioButton,
  SXSkinGroupBox, SXSkinButton, SXSkinNotebook, SXSkinEdit, Menus, ShellAPI,
  SXSkinPaintBox, SXSkinUpDown, GR32_Polygons, GR32, Types, SXSkinForm,
  ComCtrls, SXSkinSpinEdit;

type
  TForm1 = class(TForm)
    SXSkinLibrary1: TSXSkinLibrary;
    SXSkinImage2: TSXSkinImage;
    Panel1: TPanel;
    SXSkinNotebook1: TSXSkinNotebook;
    SXSkinPanel3: TSXSkinPanel;
    SXSkinLabel1: TSXSkinLabel;
    SXSkinPanel7: TSXSkinPanel;
    LabelPage: TSXSkinNotebookPage;
    SXSkinPanel9: TSXSkinPanel;
    SXSkinLabel5: TSXSkinLabel;
    SXSkinPanel11: TSXSkinPanel;
    CheckBoxPage: TSXSkinNotebookPage;
    RadioButtonPage: TSXSkinNotebookPage;
    ButtonPage: TSXSkinNotebookPage;
    SXSkinButton1: TSXSkinButton;
    ImagePage: TSXSkinNotebookPage;
    GroupBoxPage: TSXSkinNotebookPage;
    SXSkinGroupBox4: TSXSkinGroupBox;
    SXSkinCheckBox4: TSXSkinCheckBox;
    SXSkinImage1: TSXSkinImage;
    SXSkinLabel6: TSXSkinLabel;
    SXSkinLabel7: TSXSkinLabel;
    SXSkinLabel8: TSXSkinLabel;
    SXSkinLabel9: TSXSkinLabel;
    SXSkinLabel10: TSXSkinLabel;
    SXSkinLabel11: TSXSkinLabel;
    SXSkinLabel12: TSXSkinLabel;
    SXSkinLabel13: TSXSkinLabel;
    SXSkinLabel14: TSXSkinLabel;
    SXSkinLabel15: TSXSkinLabel;
    SXSkinLabel16: TSXSkinLabel;
    SXSkinLabel17: TSXSkinLabel;
    SXSkinLabel18: TSXSkinLabel;
    SXSkinImage3: TSXSkinImage;
    SXSkinButton3: TSXSkinButton;
    SXSkinButton2: TSXSkinButton;
    SXSkinButton4: TSXSkinButton;
    SXSkinButton6: TSXSkinButton;
    SXSkinButton7: TSXSkinButton;
    SXSkinButton8: TSXSkinButton;
    SXSkinButton9: TSXSkinButton;
    SXSkinButton5: TSXSkinButton;
    Timer1: TTimer;
    SXSkinButton10: TSXSkinButton;
    SXSkinGroupBox5: TSXSkinGroupBox;
    SXSkinButton57: TSXSkinButton;
    SXSkinButton17: TSXSkinButton;
    SXSkinButton18: TSXSkinButton;
    SXSkinButton19: TSXSkinButton;
    SXSkinCheckBox6: TSXSkinCheckBox;
    SXSkinCheckBox7: TSXSkinCheckBox;
    SXSkinGroupBox6: TSXSkinGroupBox;
    SXSkinRadioButton11: TSXSkinRadioButton;
    SXSkinRadioButton12: TSXSkinRadioButton;
    SXSkinRadioButton13: TSXSkinRadioButton;
    SXSkinRadioButton14: TSXSkinRadioButton;
    SXSkinCheckBox8: TSXSkinCheckBox;
    SXSkinCheckBox9: TSXSkinCheckBox;
    SXSkinCheckBox10: TSXSkinCheckBox;
    SXSkinCheckBox11: TSXSkinCheckBox;
    SXSkinCheckBox12: TSXSkinCheckBox;
    SXSkinLabel19: TSXSkinLabel;
    SXSkinLabel20: TSXSkinLabel;
    SXSkinImage4: TSXSkinImage;
    SXSkinLabel21: TSXSkinLabel;
    SXSkinLabel22: TSXSkinLabel;
    SXSkinPanel6: TSXSkinPanel;
    SXSkinPanel8: TSXSkinPanel;
    SXSkinLabel23: TSXSkinLabel;
    SXSkinImage6: TSXSkinImage;
    SXSkinPanel10: TSXSkinPanel;
    SXSkinLabel27: TSXSkinLabel;
    SXSkinImage9: TSXSkinImage;
    SXSkinPanel12: TSXSkinPanel;
    SXSkinLabel28: TSXSkinLabel;
    SXSkinImage10: TSXSkinImage;
    SXSkinPanel13: TSXSkinPanel;
    SXSkinLabel29: TSXSkinLabel;
    SXSkinImage11: TSXSkinImage;
    SXSkinPanel14: TSXSkinPanel;
    SXSkinLabel24: TSXSkinLabel;
    SXSkinImage5: TSXSkinImage;
    SXSkinPanel15: TSXSkinPanel;
    SXSkinLabel25: TSXSkinLabel;
    SXSkinImage7: TSXSkinImage;
    SXSkinPanel16: TSXSkinPanel;
    SXSkinLabel26: TSXSkinLabel;
    SXSkinImage8: TSXSkinImage;
    SXSkinPanel17: TSXSkinPanel;
    SXSkinLabel30: TSXSkinLabel;
    SXSkinImage12: TSXSkinImage;
    SXSkinRadioButton1: TSXSkinRadioButton;
    SXSkinRadioButton2: TSXSkinRadioButton;
    SXSkinGroupBox1: TSXSkinGroupBox;
    SXSkinButton11: TSXSkinButton;
    SXSkinButton12: TSXSkinButton;
    SXSkinButton13: TSXSkinButton;
    SXSkinButton14: TSXSkinButton;
    SXSkinGroupBox2: TSXSkinGroupBox;
    SXSkinGroupBox3: TSXSkinGroupBox;
    SXSkinGroupBox7: TSXSkinGroupBox;
    SXSkinCheckBox1: TSXSkinCheckBox;
    SXSkinCheckBox2: TSXSkinCheckBox;
    SXSkinCheckBox5: TSXSkinCheckBox;
    SXSkinCheckBox13: TSXSkinCheckBox;
    SXSkinGroupBox8: TSXSkinGroupBox;
    SXSkinCheckBox14: TSXSkinCheckBox;
    SXSkinGroupBox9: TSXSkinGroupBox;
    SXSkinCheckBox15: TSXSkinCheckBox;
    SXSkinGroupBox10: TSXSkinGroupBox;
    SXSkinCheckBox16: TSXSkinCheckBox;
    SXSkinRadioButton3: TSXSkinRadioButton;
    SXSkinRadioButton4: TSXSkinRadioButton;
    SXSkinRadioButton5: TSXSkinRadioButton;
    SXSkinGroupBox11: TSXSkinGroupBox;
    SXSkinCheckBox17: TSXSkinCheckBox;
    EditPage: TSXSkinNotebookPage;
    SXSkinEdit1: TSXSkinEdit;
    SXSkinEdit3: TSXSkinEdit;
    SXSkinEdit5: TSXSkinEdit;
    SXSkinEdit2: TSXSkinEdit;
    SXSkinButton15: TSXSkinButton;
    SXSkinButton16: TSXSkinButton;
    SXSkinLabel3: TSXSkinLabel;
    SXSkinEdit6: TSXSkinEdit;
    SXSkinEdit4: TSXSkinEdit;
    SXSkinPanel1: TSXSkinPanel;
    SXSkinLabel2: TSXSkinLabel;
    SXSkinImage13: TSXSkinImage;
    SXSkinPanel2: TSXSkinPanel;
    SXSkinPanel5: TSXSkinPanel;
    SXSkinLabel31: TSXSkinLabel;
    SXSkinImage15: TSXSkinImage;
    SXSkinPanel23: TSXSkinPanel;
    SXSkinLabel37: TSXSkinLabel;
    SXSkinImage21: TSXSkinImage;
    SXSkinPanel24: TSXSkinPanel;
    SXSkinLabel38: TSXSkinLabel;
    SXSkinImage22: TSXSkinImage;
    SXSkinButton20: TSXSkinButton;
    SlideTransformPage: TSXSkinNotebookPage;
    SXSkinButton21: TSXSkinButton;
    SXSkinButton22: TSXSkinButton;
    SXSkinButton23: TSXSkinButton;
    SXSkinButton24: TSXSkinButton;
    SXSkinButton25: TSXSkinButton;
    SXSkinButton26: TSXSkinButton;
    SXSkinButton27: TSXSkinButton;
    SXSkinButton28: TSXSkinButton;
    SXSkinButton29: TSXSkinButton;
    SXSkinButton30: TSXSkinButton;
    SXSkinButton31: TSXSkinButton;
    SXSkinButton32: TSXSkinButton;
    SXSkinButton33: TSXSkinButton;
    SXSkinButton34: TSXSkinButton;
    SXSkinButton35: TSXSkinButton;
    SXSkinButton36: TSXSkinButton;
    SXSkinButton37: TSXSkinButton;
    SXSkinButton38: TSXSkinButton;
    SXSkinButton39: TSXSkinButton;
    SXSkinButton40: TSXSkinButton;
    SXSkinButton41: TSXSkinButton;
    OverDrawTransformPage: TSXSkinNotebookPage;
    SXSkinButton42: TSXSkinButton;
    SXSkinButton43: TSXSkinButton;
    SXSkinButton44: TSXSkinButton;
    SXSkinButton45: TSXSkinButton;
    SXSkinButton46: TSXSkinButton;
    SXSkinButton47: TSXSkinButton;
    SXSkinButton48: TSXSkinButton;
    SXSkinButton49: TSXSkinButton;
    SXSkinButton50: TSXSkinButton;
    SXSkinButton51: TSXSkinButton;
    SXSkinButton52: TSXSkinButton;
    SXSkinButton53: TSXSkinButton;
    SXSkinButton54: TSXSkinButton;
    SXSkinButton55: TSXSkinButton;
    SXSkinButton56: TSXSkinButton;
    CustomEffectPage: TSXSkinNotebookPage;
    SXSkinButton58: TSXSkinButton;
    SXSkinLabel4: TSXSkinLabel;
    SXSkinLabel32: TSXSkinLabel;
    SXSkinButton59: TSXSkinButton;
    SXSkinLabel33: TSXSkinLabel;
    SXSkinButton60: TSXSkinButton;
    SXSkinLabel34: TSXSkinLabel;
    SXSkinPanel4: TSXSkinPanel;
    SXSkinRadioButton6: TSXSkinRadioButton;
    SXSkinRadioButton7: TSXSkinRadioButton;
    SXSkinRadioButton8: TSXSkinRadioButton;
    SXSkinRadioButton9: TSXSkinRadioButton;
    SXSkinRadioButton10: TSXSkinRadioButton;
    SXSkinRadioButton15: TSXSkinRadioButton;
    SXSkinPanel18: TSXSkinPanel;
    SXSkinRadioButton25: TSXSkinRadioButton;
    SXSkinRadioButton24: TSXSkinRadioButton;
    SXSkinRadioButton23: TSXSkinRadioButton;
    SXSkinRadioButton22: TSXSkinRadioButton;
    SXSkinRadioButton21: TSXSkinRadioButton;
    SXSkinRadioButton16: TSXSkinRadioButton;
    SXSkinRadioButton17: TSXSkinRadioButton;
    SXSkinRadioButton18: TSXSkinRadioButton;
    SXSkinRadioButton19: TSXSkinRadioButton;
    SXSkinRadioButton20: TSXSkinRadioButton;
    SXSkinPanel19: TSXSkinPanel;
    SXSkinCheckBox18: TSXSkinCheckBox;
    SXSkinCheckBox19: TSXSkinCheckBox;
    SXSkinCheckBox20: TSXSkinCheckBox;
    SXSkinLabel36: TSXSkinLabel;
    SXSkinLabel35: TSXSkinLabel;
    SXSkinPanel20: TSXSkinPanel;
    SXSkinRadioButton26: TSXSkinRadioButton;
    SXSkinRadioButton27: TSXSkinRadioButton;
    SXSkinRadioButton28: TSXSkinRadioButton;
    SXSkinRadioButton29: TSXSkinRadioButton;
    SXSkinRadioButton30: TSXSkinRadioButton;
    SXSkinRadioButton31: TSXSkinRadioButton;
    SXSkinPanel21: TSXSkinPanel;
    SXSkinRadioButton32: TSXSkinRadioButton;
    SXSkinRadioButton33: TSXSkinRadioButton;
    SXSkinRadioButton34: TSXSkinRadioButton;
    SXSkinRadioButton35: TSXSkinRadioButton;
    SXSkinRadioButton36: TSXSkinRadioButton;
    SXSkinRadioButton37: TSXSkinRadioButton;
    SXSkinRadioButton38: TSXSkinRadioButton;
    SXSkinRadioButton39: TSXSkinRadioButton;
    SXSkinRadioButton40: TSXSkinRadioButton;
    SXSkinRadioButton41: TSXSkinRadioButton;
    SXSkinPanel22: TSXSkinPanel;
    SXSkinCheckBox21: TSXSkinCheckBox;
    SXSkinCheckBox22: TSXSkinCheckBox;
    SXSkinLabel39: TSXSkinLabel;
    SXSkinLabel40: TSXSkinLabel;
    SXSkinPanel25: TSXSkinPanel;
    SXSkinRadioButton42: TSXSkinRadioButton;
    SXSkinRadioButton43: TSXSkinRadioButton;
    SXSkinRadioButton44: TSXSkinRadioButton;
    SXSkinRadioButton45: TSXSkinRadioButton;
    SXSkinRadioButton46: TSXSkinRadioButton;
    SXSkinRadioButton47: TSXSkinRadioButton;
    SXSkinPanel26: TSXSkinPanel;
    SXSkinRadioButton48: TSXSkinRadioButton;
    SXSkinRadioButton49: TSXSkinRadioButton;
    SXSkinRadioButton50: TSXSkinRadioButton;
    SXSkinRadioButton51: TSXSkinRadioButton;
    SXSkinRadioButton52: TSXSkinRadioButton;
    SXSkinRadioButton53: TSXSkinRadioButton;
    SXSkinRadioButton54: TSXSkinRadioButton;
    SXSkinRadioButton55: TSXSkinRadioButton;
    SXSkinRadioButton56: TSXSkinRadioButton;
    SXSkinRadioButton57: TSXSkinRadioButton;
    SXSkinPanel27: TSXSkinPanel;
    SXSkinCheckBox24: TSXSkinCheckBox;
    SXSkinCheckBox25: TSXSkinCheckBox;
    SXSkinCheckBox26: TSXSkinCheckBox;
    SXSkinPanel28: TSXSkinPanel;
    SXSkinRadioButton58: TSXSkinRadioButton;
    SXSkinRadioButton59: TSXSkinRadioButton;
    SXSkinRadioButton60: TSXSkinRadioButton;
    SXSkinRadioButton61: TSXSkinRadioButton;
    SXSkinRadioButton62: TSXSkinRadioButton;
    SXSkinRadioButton63: TSXSkinRadioButton;
    SXSkinLabel41: TSXSkinLabel;
    SXSkinPanel29: TSXSkinPanel;
    SXSkinRadioButton64: TSXSkinRadioButton;
    SXSkinRadioButton65: TSXSkinRadioButton;
    SXSkinRadioButton66: TSXSkinRadioButton;
    SXSkinRadioButton67: TSXSkinRadioButton;
    SXSkinRadioButton68: TSXSkinRadioButton;
    SXSkinRadioButton69: TSXSkinRadioButton;
    SXSkinRadioButton70: TSXSkinRadioButton;
    SXSkinRadioButton71: TSXSkinRadioButton;
    SXSkinRadioButton72: TSXSkinRadioButton;
    SXSkinRadioButton73: TSXSkinRadioButton;
    SXSkinPanel30: TSXSkinPanel;
    SXSkinCheckBox27: TSXSkinCheckBox;
    SXSkinCheckBox28: TSXSkinCheckBox;
    SXSkinLabel42: TSXSkinLabel;
    SXSkinLabel43: TSXSkinLabel;
    SXSkinLabel44: TSXSkinLabel;
    SXSkinLabel45: TSXSkinLabel;
    SXSkinButton61: TSXSkinButton;
    SXSkinCheckBox23: TSXSkinCheckBox;
    SXSkinCheckBox29: TSXSkinCheckBox;
    SXSkinButton62: TSXSkinButton;
    PopupMenu1: TPopupMenu;
    DropDownMenu2: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem22: TMenuItem;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    AboutPage: TSXSkinNotebookPage;
    SXSkinLabel46: TSXSkinLabel;
    SXSkinCheckBox3: TSXSkinCheckBox;
    SXSkinPanel31: TSXSkinPanel;
    SXSkinLabel47: TSXSkinLabel;
    SXSkinLabel48: TSXSkinLabel;
    SXSkinCheckBox30: TSXSkinCheckBox;
    SXSkinPanel32: TSXSkinPanel;
    SXSkinCheckBox31: TSXSkinCheckBox;
    SXSkinCheckBox32: TSXSkinCheckBox;
    SXSkinCheckBox33: TSXSkinCheckBox;
    SXSkinCheckBox34: TSXSkinCheckBox;
    SXSkinCheckBox35: TSXSkinCheckBox;
    SXSkinLabel49: TSXSkinLabel;
    SXSkinLabel50: TSXSkinLabel;
    SXSkinCheckBox36: TSXSkinCheckBox;
    SXSkinCheckBox37: TSXSkinCheckBox;
    SXSkinCheckBox38: TSXSkinCheckBox;
    SXSkinCheckBox39: TSXSkinCheckBox;
    SXSkinLabel51: TSXSkinLabel;
    SXSkinLabel52: TSXSkinLabel;
    PaintBoxPage: TSXSkinNotebookPage;
    SXSkinPaintBox1: TSXSkinPaintBox;
    SXSkinButton63: TSXSkinButton;
    SXSkinLabel53: TSXSkinLabel;
    SXSkinLabel54: TSXSkinLabel;
    SXSkinLabel55: TSXSkinLabel;
    SXSkinPaintBox2: TSXSkinPaintBox;
    SXSkinLabel56: TSXSkinLabel;
    SXSkinLabel57: TSXSkinLabel;
    SXSkinPaintBox3: TSXSkinPaintBox;
    SXSkinCheckBox40: TSXSkinCheckBox;
    Timer2: TTimer;
    MousePage: TSXSkinNotebookPage;
    SXSkinButton64: TSXSkinButton;
    SXSkinButton65: TSXSkinButton;
    SXSkinButton66: TSXSkinButton;
    SXSkinButton67: TSXSkinButton;
    SXSkinImage14: TSXSkinImage;
    SXSkinButton68: TSXSkinButton;
    SXSkinButton69: TSXSkinButton;
    SXSkinButton70: TSXSkinButton;
    SXSkinButton71: TSXSkinButton;
    SXSkinImage16: TSXSkinImage;
    SXSkinLabel58: TSXSkinLabel;
    SXSkinLabel59: TSXSkinLabel;
    SXSkinLabel60: TSXSkinLabel;
    SXSkinLabel61: TSXSkinLabel;
    SXSkinButton72: TSXSkinButton;
    SXSkinButton73: TSXSkinButton;
    SXSkinButton74: TSXSkinButton;
    SXSkinLabel62: TSXSkinLabel;
    EventsCheckPage: TSXSkinNotebookPage;
    SXSkinLabel63: TSXSkinLabel;
    SXSkinLabel65: TSXSkinLabel;
    SXSkinLabel64: TSXSkinLabel;
    SXSkinLabel66: TSXSkinLabel;
    SXSkinLabel67: TSXSkinLabel;
    SXSkinLabel68: TSXSkinLabel;
    SXSkinLabel69: TSXSkinLabel;
    SXSkinLabel70: TSXSkinLabel;
    SXSkinLabel71: TSXSkinLabel;
    SXSkinLabel72: TSXSkinLabel;
    SXSkinLabel73: TSXSkinLabel;
    SXSkinLabel74: TSXSkinLabel;
    SXSkinLabel75: TSXSkinLabel;
    SXSkinLabel76: TSXSkinLabel;
    Timer3: TTimer;
    SXSkinButton75: TSXSkinButton;
    SXSkinButton76: TSXSkinButton;
    SXSkinLabel77: TSXSkinLabel;
    SXSkinLabel78: TSXSkinLabel;
    SXSkinImage17: TSXSkinImage;
    SXSkinLabel79: TSXSkinLabel;
    SXSkinImage18: TSXSkinImage;
    SXSkinLabel80: TSXSkinLabel;
    SXSkinImage19: TSXSkinImage;
    SXSkinLabel81: TSXSkinLabel;
    SXSkinImage20: TSXSkinImage;
    SXSkinLabel82: TSXSkinLabel;
    SXSkinImage23: TSXSkinImage;
    SXSkinLabel83: TSXSkinLabel;
    SXSkinImage24: TSXSkinImage;
    SXSkinImage25: TSXSkinImage;
    SXSkinImage26: TSXSkinImage;
    SXSkinImage27: TSXSkinImage;
    SXSkinImage28: TSXSkinImage;
    SXSkinImage29: TSXSkinImage;
    SXSkinEdit12: TSXSkinEdit;
    SXSkinLabel84: TSXSkinLabel;
    SXSkinLabel85: TSXSkinLabel;
    PopupMenu2: TPopupMenu;
    EnableThisCheckBoxUsewithCaution1: TMenuItem;
    SXSkinCheckBox41: TSXSkinCheckBox;
    SXSkinRadioButton74: TSXSkinRadioButton;
    SXSkinRadioButton75: TSXSkinRadioButton;
    SXSkinGroupBox12: TSXSkinGroupBox;
    SXSkinGroupBox13: TSXSkinGroupBox;
    SXSkinGroupBox14: TSXSkinGroupBox;
    SXSkinLabel86: TSXSkinLabel;
    SXSkinLabel87: TSXSkinLabel;
    SXSkinLabel88: TSXSkinLabel;
    SXSkinForm1: TSXSkinForm;
    SelectiveStylesPage: TSXSkinNotebookPage;
    SXSkinImage30: TSXSkinImage;
    SXSkinImage31: TSXSkinImage;
    SXSkinImage32: TSXSkinImage;
    SXSkinImage33: TSXSkinImage;
    SXSkinImage34: TSXSkinImage;
    SXSkinImage35: TSXSkinImage;
    SXSkinImage36: TSXSkinImage;
    SXSkinImage37: TSXSkinImage;
    SXSkinImage38: TSXSkinImage;
    SXSkinImage39: TSXSkinImage;
    SXSkinImage41: TSXSkinImage;
    SXSkinImage40: TSXSkinImage;
    SXSkinLabel89: TSXSkinLabel;
    SXSkinLabel90: TSXSkinLabel;
    SXSkinLabel91: TSXSkinLabel;
    SXSkinLabel92: TSXSkinLabel;
    SXSkinLabel93: TSXSkinLabel;
    SXSkinLabel94: TSXSkinLabel;
    SXSkinLabel95: TSXSkinLabel;
    SXSkinLabel96: TSXSkinLabel;
    SXSkinLabel97: TSXSkinLabel;
    SXSkinLabel98: TSXSkinLabel;
    SXSkinLabel99: TSXSkinLabel;
    SXSkinLabel100: TSXSkinLabel;
    SXSkinLabel101: TSXSkinLabel;
    SXSkinLabel102: TSXSkinLabel;
    SXSkinLabel103: TSXSkinLabel;
    SXSkinLabel104: TSXSkinLabel;
    SXSkinImage42: TSXSkinImage;
    AllInOnePage: TSXSkinNotebookPage;
    SXSkinImage43: TSXSkinImage;
    SXSkinLabel105: TSXSkinLabel;
    SXSkinButton77: TSXSkinButton;
    SXSkinGroupBox15: TSXSkinGroupBox;
    SXSkinRadioButton76: TSXSkinRadioButton;
    SXSkinRadioButton77: TSXSkinRadioButton;
    SXSkinRadioButton78: TSXSkinRadioButton;
    SXSkinGroupBox16: TSXSkinGroupBox;
    SXSkinGroupBox17: TSXSkinGroupBox;
    SXSkinGroupBox18: TSXSkinGroupBox;
    SXSkinEdit14: TSXSkinEdit;
    SXSkinEdit15: TSXSkinEdit;
    SXSkinGroupBox19: TSXSkinGroupBox;
    SXSkinCheckBox42: TSXSkinCheckBox;
    SXSkinCheckBox43: TSXSkinCheckBox;
    SXSkinCheckBox44: TSXSkinCheckBox;
    SXSkinCheckBox45: TSXSkinCheckBox;
    SXSkinLabel106: TSXSkinLabel;
    SXSkinLabel107: TSXSkinLabel;
    SXSkinButton78: TSXSkinButton;
    SXSkinButton79: TSXSkinButton;
    SXSkinButton80: TSXSkinButton;
    SXSkinButton81: TSXSkinButton;
    SXSkinGroupBox20: TSXSkinGroupBox;
    SXSkinPanel33: TSXSkinPanel;
    SXSkinRadioButton79: TSXSkinRadioButton;
    SXSkinRadioButton80: TSXSkinRadioButton;
    SXSkinLabel108: TSXSkinLabel;
    SXSkinRadioButton81: TSXSkinRadioButton;
    SXSkinRadioButton82: TSXSkinRadioButton;
    SXSkinRadioButton83: TSXSkinRadioButton;
    SXSkinPanel34: TSXSkinPanel;
    SXSkinLabel109: TSXSkinLabel;
    SXSkinRadioButton84: TSXSkinRadioButton;
    SXSkinRadioButton85: TSXSkinRadioButton;
    SXSkinRadioButton86: TSXSkinRadioButton;
    SXSkinRadioButton87: TSXSkinRadioButton;
    SXSkinRadioButton88: TSXSkinRadioButton;
    SXSkinPanel35: TSXSkinPanel;
    SXSkinLabel110: TSXSkinLabel;
    SXSkinCheckBox46: TSXSkinCheckBox;
    SXSkinCheckBox47: TSXSkinCheckBox;
    SXSkinCheckBox48: TSXSkinCheckBox;
    SXSkinCheckBox49: TSXSkinCheckBox;
    SXSkinLabel111: TSXSkinLabel;
    UpDownPage: TSXSkinNotebookPage;
    SXSkinUpDown3: TSXSkinUpDown;
    SXSkinUpDown1: TSXSkinUpDown;
    SXSkinLabel112: TSXSkinLabel;
    SXSkinUpDown4: TSXSkinUpDown;
    SXSkinEdit16: TSXSkinEdit;
    SXSkinUpDown5: TSXSkinUpDown;
    SXSkinUpDown6: TSXSkinUpDown;
    SXSkinUpDown7: TSXSkinUpDown;
    SXSkinUpDown8: TSXSkinUpDown;
    SXSkinUpDown9: TSXSkinUpDown;
    SXSkinUpDown10: TSXSkinUpDown;
    SXSkinUpDown11: TSXSkinUpDown;
    SXSkinLabel113: TSXSkinLabel;
    SXSkinLabel114: TSXSkinLabel;
    SXSkinUpDown12: TSXSkinUpDown;
    SXSkinUpDown13: TSXSkinUpDown;
    SXSkinUpDown14: TSXSkinUpDown;
    SXSkinUpDown15: TSXSkinUpDown;
    SXSkinUpDown16: TSXSkinUpDown;
    SXSkinUpDown17: TSXSkinUpDown;
    SXSkinUpDown18: TSXSkinUpDown;
    SXSkinLabel115: TSXSkinLabel;
    SXSkinUpDown19: TSXSkinUpDown;
    SXSkinEdit17: TSXSkinEdit;
    SXSkinUpDown20: TSXSkinUpDown;
    SXSkinUpDown21: TSXSkinUpDown;
    SXSkinUpDown22: TSXSkinUpDown;
    SXSkinUpDown23: TSXSkinUpDown;
    SXSkinUpDown24: TSXSkinUpDown;
    SXSkinUpDown25: TSXSkinUpDown;
    SXSkinUpDown26: TSXSkinUpDown;
    SXSkinLabel116: TSXSkinLabel;
    SXSkinLabel117: TSXSkinLabel;
    SXSkinUpDown27: TSXSkinUpDown;
    SXSkinUpDown28: TSXSkinUpDown;
    SXSkinUpDown29: TSXSkinUpDown;
    SXSkinUpDown30: TSXSkinUpDown;
    SXSkinUpDown31: TSXSkinUpDown;
    SXSkinSpinEdit1: TSXSkinSpinEdit;
    SXSkinSpinEdit5: TSXSkinSpinEdit;
    SXSkinSpinEdit6: TSXSkinSpinEdit;
    Timer4: TTimer;
    SXSkinButton82: TSXSkinButton;
    SXSkinSpinEdit2: TSXSkinSpinEdit;
    SXSkinSpinEdit3: TSXSkinSpinEdit;
    SXSkinSpinEdit4: TSXSkinSpinEdit;
    SXSkinLabel118: TSXSkinLabel;
    SXSkinSpinEdit7: TSXSkinSpinEdit;
    SXSkinLabel119: TSXSkinLabel;
    SXSkinLabel120: TSXSkinLabel;
    SXSkinLabel121: TSXSkinLabel;
    SXSkinSpinEdit8: TSXSkinSpinEdit;
    SXSkinLabel122: TSXSkinLabel;
    SXSkinLabel123: TSXSkinLabel;
    SXSkinSpinEdit9: TSXSkinSpinEdit;
    SXSkinSpinEdit10: TSXSkinSpinEdit;
    SXSkinUpDown2: TSXSkinUpDown;
    SXSkinSpinEdit11: TSXSkinSpinEdit;
    SXSkinSpinEdit12: TSXSkinSpinEdit;
    SXSkinSpinEdit13: TSXSkinSpinEdit;
    SXSkinSpinEdit14: TSXSkinSpinEdit;
    SXSkinSpinEdit15: TSXSkinSpinEdit;
    SXSkinSpinEdit16: TSXSkinSpinEdit;
    SXSkinSpinEdit17: TSXSkinSpinEdit;
    SXSkinSpinEdit18: TSXSkinSpinEdit;
    SXSkinSpinEdit19: TSXSkinSpinEdit;
    procedure SXSkinCheckBox46UserModified(Sender: TObject);
    procedure SXSkinRadioButton84UserModified(Sender: TObject);
    procedure SXSkinRadioButton79UserModified(Sender: TObject);
    procedure SXSkinButton64MouseEnter(Sender: TObject);
    procedure EnableThisCheckBoxUsewithCaution1Click(Sender: TObject);
    procedure SXSkinEdit12UserModified(Sender: TObject);
    procedure Timer3Timer(Sender: TObject);
    procedure SXSkinButton75DblClick(Sender: TObject);
    procedure SXSkinButton75Click(Sender: TObject);
    procedure SXSkinButton75MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SXSkinButton75MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SXSkinButton75MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure SXSkinButton75MouseLeave(Sender: TObject);
    procedure SXSkinButton75MouseEnter(Sender: TObject);
    procedure Timer2Timer(Sender: TObject);
    procedure SXSkinPaintBox3FastPaint(Rect: TRect; Rgn: HRGN;
      Bitmap: TBitmap32; X, Y: Integer);
    procedure PaintBoxPageResize(Sender: TObject);
    procedure SXSkinPaintBox2Paint(Bitmap: TBitmap32; DstRect: TRect);
    procedure SXSkinButton63Click(Sender: TObject);
    procedure SXSkinPaintBox1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SXSkinPaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SXSkinPaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure SXSkinPaintBox1Resize(Sender: TObject);
    procedure SXSkinLabel52Click(Sender: TObject);
    procedure SXSkinLabel49Click(Sender: TObject);
    procedure SXSkinCheckBox3Resize(Sender: TObject);
    procedure ComboBox2Change(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure SXSkinButton62Click(Sender: TObject);
    procedure SXSkinButton61Click(Sender: TObject);
    procedure SXSkinRadioButton6UserModified(Sender: TObject);
    procedure SXSkinButton20Click(Sender: TObject);
    procedure SXSkinCheckBox3UserModified(Sender: TObject);
    procedure SXSkinGroupBox10UserEnterFinished(Sender: TObject);
    procedure SXSkinGroupBox9UserEnterFinished(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure SXSkinButton9Click(Sender: TObject);
    procedure SXSkinButton8Click(Sender: TObject);
    procedure SXSkinButton7Click(Sender: TObject);
    procedure SXSkinButton6Click(Sender: TObject);
    procedure SXSkinGroupBox1Change(Sender: TObject);
    procedure SXSkinCheckBox5Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SXSkinUpDown1Click(Sender: TObject; UpButton: Boolean);
    procedure SXSkinUpDown17Click(Sender: TObject; UpButton: Boolean);
    procedure SXSkinButton82Click(Sender: TObject);
    procedure SXSkinSpinEdit19UserModified(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    SkinNames:TStringList;
    SkinPaths:TStringList;
    TickCount:Integer;
    BrushDown:Boolean;
    LastBrushX:Integer;
    LastBrushY:Integer;
    BrushCounter:Integer;
    IgnoreEvents:Boolean;
    procedure ResetTestEffectStyles;
    procedure ClearPaintBox;
    procedure PaintBrushDot;
    procedure UpdateEventCaption(Sender:TObject;SXLabel:TSXSkinLabel);
  end;

var
  Form1: TForm1;

implementation

uses SXSkinUtils, SXBitmap32Utils;

{$R *.dfm}

procedure TForm1.ComboBox1Change(Sender: TObject);
var S:String;
 Skin:TSXStoredSkin;
begin
 SXSkinButton7.Enabled:=ComboBox1.ItemIndex>0;
 SXSkinButton8.Enabled:=ComboBox1.ItemIndex<ComboBox1.Items.Count-1;
 SXSkinLibrary1.Active:=False;
 if ComboBox1.ItemIndex<0 then exit;
 S:=SkinPaths[ComboBox1.ItemIndex];

 if (S<>'') and (S[1]=':') then
  begin
   Skin:=GetStoredSkinByZIPName(Copy(S,2,MaxInt));
   SXSkinLibrary1.StoredSkin:=Skin;
  end else SXSkinLibrary1.SkinFile:=S;

 SXSkinLibrary1.Active:=True;
 ResetTestEffectStyles;
 SXSkinCheckBox3Resize(nil);
end;

procedure TForm1.ComboBox2Change(Sender: TObject);
begin
 SXSkinButton9.Enabled:=ComboBox2.ItemIndex>0;
 SXSkinButton6.Enabled:=ComboBox2.ItemIndex<ComboBox2.Items.Count-1;
 case ComboBox2.ItemIndex of
   0: SXSkinNotebook1.ActivePage:=AboutPage;
   1: SXSkinNotebook1.ActivePage:=AllInOnePage;
   2: SXSkinNotebook1.ActivePage:=PaintBoxPage;
   3: SXSkinNotebook1.ActivePage:=MousePage;
   4: SXSkinNotebook1.ActivePage:=LabelPage;
   5: SXSkinNotebook1.ActivePage:=CheckBoxPage;
   6: SXSkinNotebook1.ActivePage:=RadioButtonPage;
   7: SXSkinNotebook1.ActivePage:=ButtonPage;
   8: SXSkinNotebook1.ActivePage:=UpDownPage;
   9: SXSkinNotebook1.ActivePage:=ImagePage;
  10: SXSkinNotebook1.ActivePage:=GroupBoxPage;
  11: SXSkinNotebook1.ActivePage:=EditPage;
  12: SXSkinNotebook1.ActivePage:=SlideTransformPage;
  13: SXSkinNotebook1.ActivePage:=OverDrawTransformPage;
  14: SXSkinNotebook1.ActivePage:=CustomEffectPage;
  15: SXSkinNotebook1.ActivePage:=EventsCheckPage;
  16: SXSkinNotebook1.ActivePage:=SelectiveStylesPage;
 end;
 PaintBoxPageResize(nil);
end;

procedure TForm1.EnableThisCheckBoxUsewithCaution1Click(Sender: TObject);
begin
 SXSkinCheckBox3.Enabled:=True;
end;

procedure TForm1.FormCreate(Sender: TObject);
var SR:TSearchRec;
   A,B:Integer;
     L:TStringList;
    SL:TStringList;
    S1:String;
  Skin:TSXStoredSkin;
begin
 randomize;
 SkinNames:=TStringList.Create;
 SkinPaths:=TStringList.Create;
 S1:=WithlastSlash(ExtractFilePath(Application.ExeName))+'Skins\';
 SXSkinLibrary1.SkinDir:=WithlastSlash(ExtractFilePath(Application.ExeName))+'Skins';
 if FileExists(SXSkinLibrary1.SkinDir+'\App_Skins.zip') then
  SXSkinLibrary1.SkinFile2:=SXSkinLibrary1.SkinDir+'\App_Skins.zip' else 
 if FileExists(SXSkinLibrary1.SkinDir+'\App_Skins\skin.sxs') then
  SXSkinLibrary1.SkinFile2:=SXSkinLibrary1.SkinDir+'\App_Skins\skin.sxs' else
 if FileExists(SXSkinLibrary1.SkinDir+'\App_Skins\skin.ini') then
  SXSkinLibrary1.SkinFile2:=SXSkinLibrary1.SkinDir+'\App_Skins\skin.ini' else
   SXSkinLibrary1.SkinFile2:='';
 L:=TStringList.Create;
 try
  SL:=TStringList.Create;
  try
   for A:=0 to StoredSkinCount-1 do
    begin
     Skin:=GetStoredSkinByIndex(A);
     LoadStringsFromZIPSkinStream(Skin.Stream,SL);
     if SL.Values['SkinName']<>'' then
      begin
       SkinNames.Add(SL.Values['SkinName']+' (internal)');
       SkinPaths.Add(':'+Skin.FileName);
      end;
    end;
  finally
   SL.Free;
  end;
  //
  A:=0;
  repeat
   Inc(A);
   if A=1 then B:=FindFirst(S1+'*.*',faDirectory,SR) else
    B:=FindNext(SR);
   if (B=0) and (SR.Name<>'.') and (SR.Name<>'..') then
    L.Add(S1+SR.Name);
  until B<>0;
  FindClose(SR);
  //
  SL:=TStringList.Create;
  try
   for A:=0 to L.Count-1 do
    begin
     if FileExists(L[A]+'\skin.sxs') then
      begin
       LoadStringsFromSkinFile(L[A]+'\skin.sxs',SL);
       if SL.Values['SkinName']<>'' then
        begin
         SkinNames.Add(SL.Values['SkinName']+' (SXS)');
         SkinPaths.Add(L[A]+'\skin.sxs');
        end;
      end else
     if FileExists(L[A]+'\skin.ini') then
      begin
       LoadStringsFromSkinFile(L[A]+'\skin.ini',SL);
       if SL.Values['SkinName']<>'' then
        begin
         SkinNames.Add(SL.Values['SkinName']+' (INI)');
         SkinPaths.Add(L[A]+'\skin.ini');
        end;
      end;
    end;
  finally
   SL.Free;
  end;
  //
  L.Clear;
  A:=0;
  repeat
   Inc(A);
   if A=1 then B:=FindFirst(S1+'*.zip',faAnyFile,SR) else
    B:=FindNext(SR);
   if (B=0) and (SR.Name<>'.') and (SR.Name<>'..') then
    L.Add(S1+SR.Name);
  until B<>0;
  FindClose(SR);
  //
  SL:=TStringList.Create;
  try
   for A:=0 to L.Count-1 do
    begin
     if FileExists(L[A]) then
      begin
       LoadStringsFromSkinFile(L[A],SL);
       if SL.Values['SkinName']<>'' then
        begin
         SkinNames.Add(SL.Values['SkinName']+' (ZIP)');
         SkinPaths.Add(L[A]);
        end;
      end
    end;
  finally
   SL.Free;
  end;
 finally
  L.Free;
 end;
 for A:=0 to SkinNames.Count-1 do
  ComboBox1.Items.Add(SkinNames[A]);
 if ComboBox1.Items.Count>0 then
  ComboBox1.ItemIndex:=0;
 ComboBox1Change(nil);
 SXSkinNotebook1.ActivePage:=AboutPage;
 ResetTestEffectStyles;
 LastBrushX:=-1;
 ClearPaintBox;
 SXSkinLabel111.Caption:='version '+SXSkinComponents_VersionStr;
end;

procedure TForm1.ClearPaintBox;
begin
 SXSkinPaintBox1.Bitmap.Clear(0);
 RectVFadeT(SXSkinPaintBox1.Bitmap,$30FFFFFF,$60FFFFFF,0,0,SXSkinPaintBox1.Width,SXSkinPaintBox1.Height);
 SXSkinPaintBox1.Bitmap.FrameRectTS(0,0,SXSkinPaintBox1.Width,  SXSkinPaintBox1.Height,  $A0000000);
 SXSkinPaintBox1.Bitmap.FrameRectTS(1,1,SXSkinPaintBox1.Width-1,SXSkinPaintBox1.Height-1,$70000000);
 SXSkinPaintBox1.Bitmap.FrameRectTS(2,2,SXSkinPaintBox1.Width-2,SXSkinPaintBox1.Height-2,$40000000);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
 SkinNames.Free;
 SkinPaths.Free;
end;

procedure TForm1.SXSkinCheckBox5Click(Sender: TObject);
begin
 SXSkinCheckBox1.Enabled:=SXSkinCheckBox5.Checked;
 SXSkinCheckBox2.Enabled:=SXSkinCheckBox5.Checked;
end;

procedure TForm1.SXSkinEdit12UserModified(Sender: TObject);
begin
 UpdateEventCaption(Sender,SXSkinLabel85);
end;

procedure TForm1.SXSkinGroupBox10UserEnterFinished(Sender: TObject);
begin
 SXSkinCheckBox16.Enabled:=SXSkinGroupBox10.Checked;
end;

procedure TForm1.SXSkinGroupBox1Change(Sender: TObject);
begin
 SetComponentEnabled(SXSkinGroupBox2,SXSkinGroupBox1.Checked);
end;

procedure TForm1.SXSkinGroupBox9UserEnterFinished(Sender: TObject);
begin
 SXSkinCheckBox15.Enabled:=SXSkinGroupBox9.Checked;
end;

procedure TForm1.SXSkinLabel49Click(Sender: TObject);
begin
 ShellExecute(0,'open',PChar('http://www.saarixx.info/sxskincomponents/'),nil,nil,SW_SHOWNORMAL);
end;

procedure TForm1.SXSkinLabel52Click(Sender: TObject);
begin
 ShellExecute(0,'open',PChar('mailto:sxskincomponents@saarixx.info'),nil,nil,SW_SHOWNORMAL);
end;

procedure TForm1.PaintBoxPageResize(Sender: TObject);
begin
 if SXSkinNotebook1.ActivePage=PaintBoxPage then
  SXSkinPaintBox1.Width:=PaintBoxPage.Width-16;
end;

procedure TForm1.PaintBrushDot;
var PT:TPoint;
     P:TPolygon32;
   A,L:Integer;
    BS:Single;

 procedure DotAt(X,Y:Integer);
 var R:Integer;
 begin
  P.Offset(Fixed(X),Fixed(Y));
  R:=BrushCounter mod 256;
  if R>128 then R:=255-R;
  R:=R*$101;
  P.Draw(SXSkinPaintBox1.Bitmap,0,$10FF0000+R);
  P.Offset(Fixed(-X),Fixed(-Y));
 end;

begin
 Inc(BrushCounter,2);
 P:=TPolygon32.Create;
 try
  BS:=Abs(StrToIntDef(SXSkinSpinEdit10.Text,8))/2;
  if BS>50 then BS:=50;
  SetEllipse(P,-BS,-BS,BS*2,BS*2);
  PT:=SXSkinPaintBox1.ScreenToClient(Mouse.CursorPos);
  if LastBrushX<0 then
   DotAt(PT.X,PT.Y) else
    begin
     L:=round(Sqrt(Sqr(PT.X-LastBrushX)+Sqr(PT.Y-LastBrushY)));
     for A:=1 to L do
      DotAt(LastBrushX+round((PT.X-LastBrushX)*A/L),
            LastBrushY+round((PT.Y-LastBrushY)*A/L));
    end;
 finally
  P.Free;
 end;
 LastBrushX:=PT.X;
 LastBrushY:=PT.Y;
end;

procedure TForm1.SXSkinPaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 BrushDown:=True;
 PaintBrushDot;
end;

procedure TForm1.SXSkinPaintBox1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
 if BrushDown then
  PaintBrushDot;
end;

procedure TForm1.SXSkinPaintBox1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 BrushDown:=False;
 LastBrushX:=-1;
end;

procedure TForm1.SXSkinPaintBox1Resize(Sender: TObject);
begin
 ClearPaintBox;
end;

procedure TForm1.SXSkinPaintBox2Paint(Bitmap: TBitmap32; DstRect: TRect);
begin
 Bitmap.PenColor:=$40000000;
 RoundRectangle(Bitmap,DstRect.Left+3,DstRect.Top+3,SXSkinPaintBox2.Width-4,SXSkinPaintBox2.Height-4,16);
 RoundRectHFadeT(Bitmap,$100000FF,$400000FF,DstRect.Left+2,DstRect.Top+2,SXSkinPaintBox2.Width-3,SXSkinPaintBox2.Height-4,16);
 Bitmap.PenColor:=$FF000000;
 RoundRectangle(Bitmap,DstRect.Left+2,DstRect.Top+2,SXSkinPaintBox2.Width-4,SXSkinPaintBox2.Height-4,16);
 Bitmap.Font.Name:='Times New Roman';
 Bitmap.Font.Size:=40;
 DrawSmoothText(Bitmap,'Hello!',DstRect,DT_CENTER or DT_VCENTER or DT_SINGLELINE,1,$80000000);
 OffsetRect(DstRect,-1,-1);
 DrawSmoothText(Bitmap,'Hello!',DstRect,DT_CENTER or DT_VCENTER or DT_SINGLELINE,1,$8000FF00);
end;

procedure TForm1.SXSkinPaintBox3FastPaint(Rect: TRect; Rgn: HRGN;
  Bitmap: TBitmap32; X, Y: Integer);
var R:TRect;
begin
 Bitmap.FillRectTS(X,Y,X+Rect.Right-Rect.Left,Y+Rect.Bottom-Rect.Top,$40008000);
 Bitmap.Font.Name:='Times New Roman';
 Bitmap.Font.Size:=40;
 R:=SXSkinPaintBox3.ClientRect;
 OffsetRect(R,X-Rect.Left,Y-Rect.Top);
 DrawSmoothText(Bitmap,'Hello!',R,DT_CENTER or DT_VCENTER or DT_SINGLELINE,1,$80000000);
end;

procedure TForm1.SXSkinRadioButton6UserModified(Sender: TObject);
begin
 ResetTestEffectStyles;
end;

procedure TForm1.SXSkinRadioButton79UserModified(Sender: TObject);
begin
 if SXSkinRadioButton79.Checked then
  BorderStyle:=bsSizeable else
 if SXSkinRadioButton80.Checked then
  BorderStyle:=bsSingle else
 if SXSkinRadioButton81.Checked then
  BorderStyle:=bsDialog else
 if SXSkinRadioButton82.Checked then
  BorderStyle:=bsToolWindow else
 if SXSkinRadioButton83.Checked then
  BorderStyle:=bsSizeToolWin;
end;

procedure TForm1.SXSkinRadioButton84UserModified(Sender: TObject);
begin
 if SXSkinRadioButton84.Checked then
  SXSkinForm1.CustomCaptionHeight:=0 else
 if SXSkinRadioButton85.Checked then
  SXSkinForm1.CustomCaptionHeight:=15 else
 if SXSkinRadioButton86.Checked then
  SXSkinForm1.CustomCaptionHeight:=25 else
 if SXSkinRadioButton87.Checked then
  SXSkinForm1.CustomCaptionHeight:=50 else
 if SXSkinRadioButton88.Checked then
  SXSkinForm1.CustomCaptionHeight:=100;
end;

procedure TForm1.SXSkinSpinEdit19UserModified(Sender: TObject);
var A:Integer;
begin
 A:=StrToIntDef(SXSkinSpinEdit19.Text,100);
 if A<5 then A:=5;
 if A>300 then A:=300;
 SXSkinImage42.Width:=A;
 SXSkinImage42.Height:=A;
end;

procedure TForm1.SXSkinUpDown17Click(Sender: TObject; UpButton: Boolean);
begin
 if UpButton then
  begin
   SXSkinEdit17.Text:=inttostr(strtointdef(SXSkinEdit17.Text,0)+1);
  end else
   begin
    SXSkinEdit17.Text:=inttostr(strtointdef(SXSkinEdit17.Text,0)-1);
   end;
end;

procedure TForm1.SXSkinUpDown1Click(Sender: TObject; UpButton: Boolean);
begin
 if UpButton then
  begin
   SXSkinEdit16.Text:=inttostr(strtointdef(SXSkinEdit16.Text,0)+1);
  end else
   begin
    SXSkinEdit16.Text:=inttostr(strtointdef(SXSkinEdit16.Text,0)-1);
   end;
end;

procedure TForm1.SXSkinCheckBox3Resize(Sender: TObject);
begin
 SXSkinPanel31.Left:=SXSkinCheckBox3.Left+1000;
end;

procedure TForm1.SXSkinCheckBox3UserModified(Sender: TObject);
begin
 TestingRegions:=SXSkinCheckBox3.Checked;
end;

procedure TForm1.SXSkinCheckBox46UserModified(Sender: TObject);
var T:TBorderIcons;
begin
 T:=[];
 if SXSkinCheckBox46.Checked then
  Include(T,biSystemMenu);
 if SXSkinCheckBox47.Checked then
  Include(T,biMaximize);
 if SXSkinCheckBox48.Checked then
  Include(T,biMinimize);
 if SXSkinCheckBox49.Checked then
  Include(T,biHelp);
 BorderIcons:=T; 
end;

procedure TForm1.SXSkinButton20Click(Sender: TObject);
begin
 if SXSkinButton16.Enabled then
  begin
   SXSkinButton20.Caption:='Enable Button';
   SXSkinButton16.Enabled:=False;
  end else
   begin
    SXSkinButton20.Caption:='Disable Button';
    SXSkinButton16.Enabled:=True;
   end;
end;

procedure TForm1.SXSkinButton61Click(Sender: TObject);
begin
 if SXSkinCheckBox8.Enabled then
  begin
   SXSkinButton61.Caption:='Enable CheckBox';
   SXSkinCheckBox8.Enabled:=False;
  end else
   begin
    SXSkinButton61.Caption:='Disable CheckBox';
    SXSkinCheckBox8.Enabled:=True;
   end;
end;

procedure TForm1.SXSkinButton62Click(Sender: TObject);
begin
 if SXSkinEdit1.Enabled then
  begin
   SXSkinButton62.Caption:='Enable Edit';
   SXSkinEdit1.Enabled:=False;
  end else
   begin
    SXSkinButton62.Caption:='Disable Edit';
    SXSkinEdit1.Enabled:=True;
   end;
end;

procedure TForm1.SXSkinButton63Click(Sender: TObject);
begin
 ClearPaintBox;
end;

procedure TForm1.SXSkinButton64MouseEnter(Sender: TObject);
begin
 TControl(Sender).BringToFront;
end;

procedure TForm1.SXSkinButton6Click(Sender: TObject);
begin
 ComboBox2.ItemIndex:=ComboBox2.ItemIndex+1;
 ComboBox2Change(nil);
end;

procedure TForm1.UpdateEventCaption(Sender:TObject;SXLabel:TSXSkinLabel);
var R:String;
begin
 if IgnoreEvents and (SXLabel=SXSkinLabel68) then exit;
 R:='... '+inttostr(random(10000));
 if Sender=SXSkinButton75 then
  SXLabel.Caption:='from Standard Button'+R else
 if Sender=SXSkinButton76 then
  SXLabel.Caption:='from Round Button'+R else
 if Sender=SXSkinLabel77 then
  SXLabel.Caption:='from Label'+R else
 if Sender=SXSkinImage29 then
  SXLabel.Caption:='from Image'+R else
 if Sender=SXSkinEdit12 then
  SXLabel.Caption:='from Edit'+R else
 if Sender=SXSkinCheckBox41 then
  SXLabel.Caption:='from CheckBox'+R else
 if Sender=SXSkinRadioButton74 then
  SXLabel.Caption:='from RadioButton1'+R else
 if Sender=SXSkinRadioButton75 then
  SXLabel.Caption:='from RadioButton2'+R else
 if Sender=SXSkinGroupBox12 then
  SXLabel.Caption:='from Simple GroupBox'+R else
 if Sender=SXSkinGroupBox13 then
  SXLabel.Caption:='from GroupBox with CheckBox'+R else
 if Sender=SXSkinGroupBox14 then
  SXLabel.Caption:='from GroupBox with RadioButton'+R else
 if Sender=SXSkinSpinEdit6 then
  SXLabel.Caption:='from SpinEdit'+R; 
 SXLabel.Left:=392;
 SXLabel.Visible:=True;
end;

procedure TForm1.SXSkinButton75Click(Sender: TObject);
begin
 UpdateEventCaption(Sender,SXSkinLabel74);
end;

procedure TForm1.SXSkinButton75DblClick(Sender: TObject);
begin
 UpdateEventCaption(Sender,SXSkinLabel76);
end;

procedure TForm1.SXSkinButton75MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 UpdateEventCaption(Sender,SXSkinLabel70);
end;

procedure TForm1.SXSkinButton75MouseEnter(Sender: TObject);
begin
 UpdateEventCaption(Sender,SXSkinLabel63);
end;

procedure TForm1.SXSkinButton75MouseLeave(Sender: TObject);
begin
 UpdateEventCaption(Sender,SXSkinLabel66);
end;

procedure TForm1.SXSkinButton75MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
 UpdateEventCaption(Sender,SXSkinLabel68);
end;

procedure TForm1.SXSkinButton75MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 UpdateEventCaption(Sender,SXSkinLabel72);
end;

procedure TForm1.SXSkinButton7Click(Sender: TObject);
begin
 ComboBox1.ItemIndex:=ComboBox1.ItemIndex-1;
 ComboBox1Change(nil);
end;

procedure TForm1.SXSkinButton82Click(Sender: TObject);
begin
 if SXSkinSpinEdit5.Enabled then
  begin
   SXSkinButton82.Caption:='Enable';
   SXSkinSpinEdit5.Enabled:=False;
  end else
   begin
    SXSkinButton82.Caption:='Disable';
    SXSkinSpinEdit5.Enabled:=True;
   end;
end;

procedure TForm1.SXSkinButton8Click(Sender: TObject);
begin
 ComboBox1.ItemIndex:=ComboBox1.ItemIndex+1;
 ComboBox1Change(nil);
end;

procedure TForm1.SXSkinButton9Click(Sender: TObject);
begin
 ComboBox2.ItemIndex:=ComboBox2.ItemIndex-1;
 ComboBox2Change(nil);
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
 Inc(TickCount);
end;

procedure TForm1.Timer2Timer(Sender: TObject);
var A:Integer;
 Image:TSXSkinStyleImageElement;
begin
 if SXSkinCheckBox40.Checked then
  begin
   A:=SXSkinLibrary1.Styles.GetIndexByName('Background');
   if (A>=0) and (SXSkinLibrary1.Styles[A] is TSXSkinGeneralStyle) then
    if TSXSkinGeneralStyle(SXSkinLibrary1.Styles[A]).Elements[0] is TSXSkinStyleImageElement then
     begin
      Image:=TSXSkinStyleImageElement(TSXSkinGeneralStyle(SXSkinLibrary1.Styles[A]).Elements[0]);
      Inc(Image.OffsetY);
      InvalidateRect(SXSkinImage2.Handle,nil,False);
     end;
  end;
end;

procedure TForm1.Timer3Timer(Sender: TObject);
begin
 if SXSkinNotebook1.ActivePage=EventsCheckPage then
  begin
   IgnoreEvents:=True;
   SXSkinLabel63.Left:=SXSkinLabel63.Left+1;
   SXSkinLabel66.Left:=SXSkinLabel66.Left+1;
   SXSkinLabel68.Left:=SXSkinLabel68.Left+1;
   SXSkinLabel70.Left:=SXSkinLabel70.Left+1;
   SXSkinLabel72.Left:=SXSkinLabel72.Left+1;
   SXSkinLabel74.Left:=SXSkinLabel74.Left+1;
   SXSkinLabel76.Left:=SXSkinLabel76.Left+1;
   SXSkinLabel85.Left:=SXSkinLabel85.Left+1;
   Application.ProcessMessages;
   IgnoreEvents:=False;
  end;
end;

procedure TForm1.ResetTestEffectStyles;

 procedure SetStyle(StyleName:String;StyleIndex:Integer);
 var Style:TSXSkinButtonStyle;
         A:Integer;
 begin
  A:=SXSkinLibrary1.Styles.GetIndexByName(StyleName);
  if (A>=0) and (SXSkinLibrary1.Styles[A] is TSXSkinButtonStyle) then
   begin
    Style:=TSXSkinButtonStyle(SXSkinLibrary1.Styles[A]);
    //HIn
    //Old
    if SXSkinRadioButton6.Checked then
     Style.HInButtonEffect.OldType:=tetNone else
    if SXSkinRadioButton7.Checked then
     Style.HInButtonEffect.OldType:=tetBlend else
    if SXSkinRadioButton8.Checked then
     Style.HInButtonEffect.OldType:=tetClear else
    if SXSkinRadioButton9.Checked then
     Style.HInButtonEffect.OldType:=tetFade else
    if SXSkinRadioButton10.Checked then
     Style.HInButtonEffect.OldType:=tetOverDraw else
    if SXSkinRadioButton15.Checked then
     Style.HInButtonEffect.OldType:=tetSlide;
    //
    if SXSkinRadioButton16.Checked then
     Style.HInButtonEffect.OldDirection:=tedLeft else
    if SXSkinRadioButton17.Checked then
     Style.HInButtonEffect.OldDirection:=tedRight else
    if SXSkinRadioButton18.Checked then
     Style.HInButtonEffect.OldDirection:=tedTop else
    if SXSkinRadioButton19.Checked then
     Style.HInButtonEffect.OldDirection:=tedBottom else
    if SXSkinRadioButton20.Checked then
     Style.HInButtonEffect.OldDirection:=tedLeftTop else
    if SXSkinRadioButton21.Checked then
     Style.HInButtonEffect.OldDirection:=tedLeftBottom else
    if SXSkinRadioButton22.Checked then
     Style.HInButtonEffect.OldDirection:=tedRightTop else
    if SXSkinRadioButton23.Checked then
     Style.HInButtonEffect.OldDirection:=tedRightBottom else
    if SXSkinRadioButton24.Checked then
     Style.HInButtonEffect.OldDirection:=tedLeftRight else
    if SXSkinRadioButton25.Checked then
     Style.HInButtonEffect.OldDirection:=tedTopBottom;
    //
    Style.HInButtonEffect.OldDirOut:=SXSkinCheckBox18.Checked;
    Style.HInButtonEffect.OldInverted:=SXSkinCheckBox19.Checked;
    //New
    if SXSkinRadioButton26.Checked then
     Style.HInButtonEffect.NewType:=tetNone else
    if SXSkinRadioButton27.Checked then
     Style.HInButtonEffect.NewType:=tetBlend else
    if SXSkinRadioButton28.Checked then
     Style.HInButtonEffect.NewType:=tetClear else
    if SXSkinRadioButton29.Checked then
     Style.HInButtonEffect.NewType:=tetFade else
    if SXSkinRadioButton30.Checked then
     Style.HInButtonEffect.NewType:=tetOverDraw else
    if SXSkinRadioButton31.Checked then
     Style.HInButtonEffect.NewType:=tetSlide;
    //
    if SXSkinRadioButton37.Checked then
     Style.HInButtonEffect.NewDirection:=tedLeft else
    if SXSkinRadioButton38.Checked then
     Style.HInButtonEffect.NewDirection:=tedRight else
    if SXSkinRadioButton39.Checked then
     Style.HInButtonEffect.NewDirection:=tedTop else
    if SXSkinRadioButton40.Checked then
     Style.HInButtonEffect.NewDirection:=tedBottom else
    if SXSkinRadioButton41.Checked then
     Style.HInButtonEffect.NewDirection:=tedLeftTop else
    if SXSkinRadioButton36.Checked then
     Style.HInButtonEffect.NewDirection:=tedLeftBottom else
    if SXSkinRadioButton35.Checked then
     Style.HInButtonEffect.NewDirection:=tedRightTop else
    if SXSkinRadioButton34.Checked then
     Style.HInButtonEffect.NewDirection:=tedRightBottom else
    if SXSkinRadioButton33.Checked then
     Style.HInButtonEffect.NewDirection:=tedLeftRight else
    if SXSkinRadioButton32.Checked then
     Style.HInButtonEffect.NewDirection:=tedTopBottom;
    //
    Style.HInButtonEffect.NewDirOut:=SXSkinCheckBox21.Checked;
    Style.HInButtonEffect.NewInverted:=SXSkinCheckBox22.Checked;
    //General
    Style.HInButtonEffect.DrawCaption:=SXSkinCheckBox20.Checked;
    Style.HInButtonEffect.StepsNum:=StrToIntDef(SXSkinSpinEdit15.Text,0);
    Style.HInButtonEffect.Offset:=StrToIntDef(SXSkinSpinEdit16.Text,0);
    if SXSkinCheckBox23.Checked then
     begin
      Style.HInGlyphEffect.OldType:=tetNone;
      Style.HInGlyphEffect.NewType:=tetNone;
     end else Style.HInGlyphEffect:=Style.HInButtonEffect;
    //
    //HOut
    //Old
    if SXSkinRadioButton42.Checked then
     Style.HOutButtonEffect.OldType:=tetNone else
    if SXSkinRadioButton43.Checked then
     Style.HOutButtonEffect.OldType:=tetBlend else
    if SXSkinRadioButton44.Checked then
     Style.HOutButtonEffect.OldType:=tetClear else
    if SXSkinRadioButton45.Checked then
     Style.HOutButtonEffect.OldType:=tetFade else
    if SXSkinRadioButton46.Checked then
     Style.HOutButtonEffect.OldType:=tetOverDraw else
    if SXSkinRadioButton47.Checked then
     Style.HOutButtonEffect.OldType:=tetSlide;
    //
    if SXSkinRadioButton53.Checked then
     Style.HOutButtonEffect.OldDirection:=tedLeft else
    if SXSkinRadioButton54.Checked then
     Style.HOutButtonEffect.OldDirection:=tedRight else
    if SXSkinRadioButton55.Checked then
     Style.HOutButtonEffect.OldDirection:=tedTop else
    if SXSkinRadioButton56.Checked then
     Style.HOutButtonEffect.OldDirection:=tedBottom else
    if SXSkinRadioButton57.Checked then
     Style.HOutButtonEffect.OldDirection:=tedLeftTop else
    if SXSkinRadioButton52.Checked then
     Style.HOutButtonEffect.OldDirection:=tedLeftBottom else
    if SXSkinRadioButton51.Checked then
     Style.HOutButtonEffect.OldDirection:=tedRightTop else
    if SXSkinRadioButton50.Checked then
     Style.HOutButtonEffect.OldDirection:=tedRightBottom else
    if SXSkinRadioButton49.Checked then
     Style.HOutButtonEffect.OldDirection:=tedLeftRight else
    if SXSkinRadioButton48.Checked then
     Style.HOutButtonEffect.OldDirection:=tedTopBottom;
    //
    Style.HOutButtonEffect.OldDirOut:=SXSkinCheckBox24.Checked;
    Style.HOutButtonEffect.OldInverted:=SXSkinCheckBox25.Checked;
    //New
    if SXSkinRadioButton58.Checked then
     Style.HOutButtonEffect.NewType:=tetNone else
    if SXSkinRadioButton59.Checked then
     Style.HOutButtonEffect.NewType:=tetBlend else
    if SXSkinRadioButton60.Checked then
     Style.HOutButtonEffect.NewType:=tetClear else
    if SXSkinRadioButton61.Checked then
     Style.HOutButtonEffect.NewType:=tetFade else
    if SXSkinRadioButton62.Checked then
     Style.HOutButtonEffect.NewType:=tetOverDraw else
    if SXSkinRadioButton63.Checked then
     Style.HOutButtonEffect.NewType:=tetSlide;
    //
    if SXSkinRadioButton69.Checked then
     Style.HOutButtonEffect.NewDirection:=tedLeft else
    if SXSkinRadioButton70.Checked then
     Style.HOutButtonEffect.NewDirection:=tedRight else
    if SXSkinRadioButton71.Checked then
     Style.HOutButtonEffect.NewDirection:=tedTop else
    if SXSkinRadioButton72.Checked then
     Style.HOutButtonEffect.NewDirection:=tedBottom else
    if SXSkinRadioButton73.Checked then
     Style.HOutButtonEffect.NewDirection:=tedLeftTop else
    if SXSkinRadioButton68.Checked then
     Style.HOutButtonEffect.NewDirection:=tedLeftBottom else
    if SXSkinRadioButton67.Checked then
     Style.HOutButtonEffect.NewDirection:=tedRightTop else
    if SXSkinRadioButton66.Checked then
     Style.HOutButtonEffect.NewDirection:=tedRightBottom else
    if SXSkinRadioButton65.Checked then
     Style.HOutButtonEffect.NewDirection:=tedLeftRight else
    if SXSkinRadioButton64.Checked then
     Style.HOutButtonEffect.NewDirection:=tedTopBottom;
    //
    Style.HOutButtonEffect.NewDirOut:=SXSkinCheckBox27.Checked;
    Style.HOutButtonEffect.NewInverted:=SXSkinCheckBox28.Checked;
    //General
    Style.HOutButtonEffect.DrawCaption:=SXSkinCheckBox26.Checked;
    Style.HOutButtonEffect.StepsNum:=StrToIntDef(SXSkinSpinEdit17.Text,0);
    Style.HOutButtonEffect.Offset:=StrToIntDef(SXSkinSpinEdit18.Text,0);
    if SXSkinCheckBox29.Checked then
     begin
      Style.HOutGlyphEffect.OldType:=tetNone;
      Style.HOutGlyphEffect.NewType:=tetNone;
     end else Style.HOutGlyphEffect:=Style.HOutButtonEffect;
    //
    //
    case StyleIndex of
     1: begin
         Style.HInButtonEffect.NewType:=tetClear;
         Style.HInGlyphEffect.NewType:=tetClear;
         Style.HOutButtonEffect.NewType:=tetClear;
         Style.HOutGlyphEffect.NewType:=tetClear;
        end;
     2: begin
         Style.HInButtonEffect.OldType:=tetClear;
         Style.HInGlyphEffect.OldType:=tetClear;
         Style.HOutButtonEffect.OldType:=tetClear;
         Style.HOutGlyphEffect.OldType:=tetClear;
        end;
    end;
   end;
 end;

begin
 SetStyle('_Button_TestEffect1',1);
 SetStyle('_Button_TestEffect2',2);
 SetStyle('_Button_TestEffect3',3);
end;

end.
