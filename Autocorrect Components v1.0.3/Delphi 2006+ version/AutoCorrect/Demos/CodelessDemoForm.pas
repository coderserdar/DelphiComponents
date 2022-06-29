unit CodelessDemoForm;

interface                  

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, StdActns, ActnList, Menus, ExtActns, ToolWin, ImgList,
  CCR.AutoCorrect, CCR.AutoCorrect.Controls;

type
  TForm1 = class(TForm)
    AutoCorrectEngine1: TAutoCorrectEngine;
    RichEditWithAutoCorrect1: TRichEditWithAutoCorrect;
    ActionList1: TActionList;
    EditCut1: TEditCut;
    EditCopy1: TEditCopy;
    EditPaste1: TEditPaste;
    EditSelectAll1: TEditSelectAll;
    EditUndo1: TEditUndo;
    PopupMenu1: TPopupMenu;
    Undo1: TMenuItem;
    N1: TMenuItem;
    Cut1: TMenuItem;
    Copy1: TMenuItem;
    Paste1: TMenuItem;
    N2: TMenuItem;
    SelectAll1: TMenuItem;
    ImageList: TImageList;
    DisabledImageList: TImageList;
    tbrActions: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    ToolButton14: TToolButton;
    ToolButton15: TToolButton;
    RichEditBold1: TRichEditBold;
    RichEditItalic1: TRichEditItalic;
    RichEditUnderline1: TRichEditUnderline;
    RichEditStrikeOut1: TRichEditStrikeOut;
    RichEditAlignLeft1: TRichEditAlignLeft;
    RichEditAlignCenter1: TRichEditAlignCenter;
    RichEditAlignRight1: TRichEditAlignRight;
    RichEditBullets1: TRichEditBullets;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

end.
