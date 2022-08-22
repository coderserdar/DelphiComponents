unit TB97TlbrEd;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, TB97Ctls, TB97, TB97Tlwn, TB97Tlbr;

type
  TFToolbar97Editor = class(TForm)
    GroupBox1: TGroupBox;
    tbbBlank: TToolbarButton97;
    tbbSep: TToolbarButton97;
    tbbEditBox: TToolbarButton97;

    GroupBox2: TGroupBox;
    tbbNew: TToolbarButton97;
    tbbNew2: TToolbarButton97;
    tbbOpen: TToolbarButton97;
    tbbSave: TToolbarButton97;
    tbbSaveAll: TToolbarButton97;
    tbbDelete: TToolbarButton97;
    tbbCut: TToolbarButton97;
    tbbCopy: TToolbarButton97;
    tbbPaste: TToolbarButton97;
    tbbUndo: TToolbarButton97;
    tbbRedo: TToolbarButton97;
    tbbFind: TToolbarButton97;
    tbbFindNext: TToolbarButton97;
    tbbReplace: TToolbarButton97;
    tbbTrash: TToolbarButton97;
    tbbRecycle: TToolbarButton97;
    tbbClose: TToolbarButton97;
    tbbTrash2: TToolbarButton97;
    tbbReplace2: TToolbarButton97;
    tbbFindNext2: TToolbarButton97;
    tbbFind2: TToolbarButton97;
    GroupBox3: TGroupBox;
    tbbFirstRecord: TToolbarButton97;
    tbbPriorRecord: TToolbarButton97;
    tbbNextRecord: TToolbarButton97;
    tbbLastRecord: TToolbarButton97;
    tbbInsertRecord: TToolbarButton97;
    tbbDeleteRecord: TToolbarButton97;
    tbbEditRecord: TToolbarButton97;
    GroupBox4: TGroupBox;
    tbbPrint2: TToolbarButton97;
    tbbPrint: TToolbarButton97;
    tbbPreview: TToolbarButton97;
    tbbPrintSetup: TToolbarButton97;
    tbbPageSetup: TToolbarButton97;
    tbbFax: TToolbarButton97;
    tbbDatabase: TToolbarButton97;
    tbbMonitor: TToolbarButton97;
    tbbAttachment: TToolbarButton97;
    tbbMail: TToolbarButton97;
    tbbMail2: TToolbarButton97;
    GroupBox6: TGroupBox;
    GroupBox7: TGroupBox;
    GroupBox8: TGroupBox;
    tbbDate: TToolbarButton97;
    tbbTime: TToolbarButton97;
    tbbMonth: TToolbarButton97;
    tbbBold: TToolbarButton97;
    tbbItalic: TToolbarButton97;
    tbbUnderline: TToolbarButton97;
    tbbLeftJustify: TToolbarButton97;
    tbbCenter: TToolbarButton97;
    tbbRightJustify: TToolbarButton97;
    tbbJustify: TToolbarButton97;
    tbbBulletList: TToolbarButton97;
    tbbFont: TToolbarButton97;
    tbbOKAll: TToolbarButton97;
    tbbCancel: TToolbarButton97;
    tbbNo: TToolbarButton97;
    tbbOk: TToolbarButton97;
    tbbCritical: TToolbarButton97;
    tbbError: TToolbarButton97;
    tbbWarning2: TToolbarButton97;
    tbbWarning: TToolbarButton97;
    tbbUp: TToolbarButton97;
    tbbDown: TToolbarButton97;
    tbbLeft: TToolbarButton97;
    tbbRigth: TToolbarButton97;
    tbbMoveLeft: TToolbarButton97;
    tbbMoveRight: TToolbarButton97;
    tbbMoveAllLeft: TToolbarButton97;
    tbbMoveAllRight: TToolbarButton97;
    tbbStepBack: TToolbarButton97;
    tbbStepForward: TToolbarButton97;
    tbbSpellCheck: TToolbarButton97;
    tbbTrueTypeFont: TToolbarButton97;
    tbbWindow: TToolbarButton97;
    tbbStop: TToolbarButton97;
    tbbRun: TToolbarButton97;
    tbbExecute: TToolbarButton97;
    tbbExecute2: TToolbarButton97;
    tbbUpOneLevel: TToolbarButton97;
    tbbNotes: TToolbarButton97;
    tbbPushPin: TToolbarButton97;
    tbbEdit: TToolbarButton97;
    tbbZoom: TToolbarButton97;
    tbbZoomIn: TToolbarButton97;
    tbbZoomOut: TToolbarButton97;
    tbbLook: TToolbarButton97;
    tbbInvoice: TToolbarButton97;
    tbbAccounting: TToolbarButton97;
    tbbRestore: TToolbarButton97;
    tbbBackup: TToolbarButton97;
    tbbLargeIcons: TToolbarButton97;
    tbbList: TToolbarButton97;
    tbbSmallIcons: TToolbarButton97;
    tbbDetails: TToolbarButton97;
    tbbAccess: TToolbarButton97;
    tbbExcel: TToolbarButton97;
    tbbPowerPoint: TToolbarButton97;
    tbbWord: TToolbarButton97;
    tbbExportTableToExcel: TToolbarButton97;
    tbbExportToExcel: TToolbarButton97;
    tbbExportToWord: TToolbarButton97;
    BitBtn1: TBitBtn;
    tbbHelp: TToolbarButton97;

    procedure Button97Click(Sender: TObject);
    procedure tbbSepClick(Sender: TObject);
    procedure tbbEditBoxClick(Sender: TObject);
  private
    { Private declarations }
    function CreateName(Form : TForm; const AName : string) : string;
  public
    { Public declarations }
    ToolBar : TToolBar97;
    ParentForm : TForm;
    SelectedBtn : TToolbarButton97;
  end;


implementation

{$R *.DFM}

procedure TFToolbar97Editor.Button97Click(Sender: TObject);
var
  NewButton : TToolbarButton97;
begin
  SelectedBtn := TToolbarButton97(Sender);

  if ParentForm <> nil then
  begin
    NewButton := TToolbarButton97.Create(ParentForm);
    NewButton.Parent := ToolBar;
    if Toolbar.Width < 36 then
    begin
      NewButton.Left := 0;
    end else
    begin
      NewButton.Left := Toolbar.Width - 13;
    end;

    try
      NewButton.Name := SelectedBtn.Name;
    except
      NewButton.Name := CreateName(ParentForm, SelectedBtn.Name);
    end;

    NewButton.Glyph.Assign(SelectedBtn.Glyph);
    NewButton.NumGlyphs := SelectedBtn.NumGlyphs;
    NewButton.Hint := SelectedBtn.Hint;
    NewButton.ShowHint := True;
  end;
end;

function TFToolbar97Editor.CreateName(Form : TForm; const AName : string) : string;
var
  I : Integer;
begin
  I := 0;
  repeat
    Inc(I);
    Result := AName + IntToStr(I);
  until Form.FindComponent(Result) = nil;
end;

procedure TFToolbar97Editor.tbbSepClick(Sender: TObject);
var
  NewButton : TToolbarSep97;
begin
  if ParentForm <> nil then
  begin
    NewButton := TToolbarSep97.Create(ParentForm);
    NewButton.Parent := ToolBar;
    if Toolbar.Width < 36 then
    begin
      NewButton.Left := 0;
    end else
    begin
      NewButton.Left := Toolbar.Width - 13;
    end;

    try
      NewButton.Name := 'tbbSep';
    except
      NewButton.Name := CreateName(ParentForm, 'tbbSep');
    end;
  end;
end;

procedure TFToolbar97Editor.tbbEditBoxClick(Sender: TObject);
var
  NewButton : TEdit97;
begin
  if ParentForm <> nil then
  begin
    NewButton := TEdit97.Create(ParentForm);
    NewButton.Parent := ToolBar;
    if Toolbar.Width < 36 then
    begin
      NewButton.Left := 0;
    end else
    begin
      NewButton.Left := Toolbar.Width - 13;
    end;
    try
      NewButton.Name := 'tbEdit';
    except
      NewButton.Name := CreateName(ParentForm, 'tbEdit');
    end;
  end;
end;

end.
