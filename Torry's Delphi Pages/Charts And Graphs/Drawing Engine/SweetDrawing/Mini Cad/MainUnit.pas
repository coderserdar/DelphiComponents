unit MainUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  SCDEConsts, SCDEControl, SCDrawingCommons, SCDrawingSurface, SCDrawingEditor,
  SCDrawingShapes, ToolWin, ComCtrls, ImgList, Menus, ActnList, ExtDlgs,
  Jpeg, StdCtrls, Buttons, FileCtrl, ExtCtrls, SCStdControls, SCControl,
  SCSplitter, SCDeLayerManager, SCFontsAndColors, SCPopupColors,
  SCAdvEdits, SCEdits, SCMaskEdit, SCGraphicButton, SCPanels, SCSpinEdits;

type
  TMainForm = class(TForm)
    SCCAD1: TSCDeEditor;
    StatusBar1: TStatusBar;
    ImageList1: TImageList;
    MainMenu1: TMainMenu;
    MenuEdit: TMenuItem;
    MenuInsert: TMenuItem;
    MenuRectangle: TMenuItem;
    ActionList1: TActionList;
    InsertRect: TAction;
    InsertPolygon: TAction;
    InsertBezier: TAction;
    InsertEllipse: TAction;
    InsertCircle: TAction;
    InsertText: TAction;
    MenuCircle: TMenuItem;
    MenuEllipse: TMenuItem;
    MenuPolygon: TMenuItem;
    MenuBezier: TMenuItem;
    MenuText: TMenuItem;
    InsertArc: TAction;
    MenuArc: TMenuItem;
    ToolSelect: TAction;
    MenuSelect: TMenuItem;
    ToolSelectAll: TAction;
    MenuSelectAll: TMenuItem;
    N2: TMenuItem;
    ToolPen: TAction;
    EditDelete: TAction;
    EditNewLayer: TAction;
    InsertPicture: TAction;
    MenuPicture: TMenuItem;
    MenuDelete: TMenuItem;
    InsertFreehand: TAction;
    MenuFreehand: TMenuItem;
    ArrangeSendToBack: TAction;
    ArrangeBringToFront: TAction;
    MenuArrange1: TMenuItem;
    MenuBringToFront: TMenuItem;
    MenuSendToBack: TMenuItem;
    ArrangeBringForward: TAction;
    ArrangeSendBackward: TAction;
    MenuBringForward: TMenuItem;
    MenuSendBackward: TMenuItem;
    ToolDeselect: TAction;
    Select1: TMenuItem;
    MenuDeselect: TMenuItem;
    ToolRemovePoint: TAction;
    MenuPen: TMenuItem;
    N1: TMenuItem;
    MenuRemovePoint: TMenuItem;
    FileSave: TAction;
    FileSaveAs: TAction;
    FileOpen: TAction;
    FileClose: TAction;
    File1: TMenuItem;
    MenuOpen: TMenuItem;
    MenuSave: TMenuItem;
    MenuSaveAs: TMenuItem;
    N4: TMenuItem;
    MenuClose1: TMenuItem;
    OpenPictureDialog1: TOpenPictureDialog;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    ArrangeGroup: TAction;
    ArrangeUngroup: TAction;
    N5: TMenuItem;
    MenuGroup: TMenuItem;
    MenuUngroup: TMenuItem;
    FileNew: TAction;
    MenuNew: TMenuItem;
    ToolBar1: TToolBar;
    ButtonNew: TToolButton;
    ButtonOpen: TToolButton;
    ButtonSave: TToolButton;
    ToolButton10: TToolButton;
    ButtonBringToFront: TToolButton;
    ButtonBringForward: TToolButton;
    ButtonSendToBack: TToolButton;
    ButtonSendBackward: TToolButton;
    ToolButton32: TToolButton;
    ButtonGroup: TToolButton;
    ButtonUngroup: TToolButton;
    ToolBar3: TToolBar;
    ButtonSelect: TToolButton;
    ToolButton2: TToolButton;
    ButtonRect: TToolButton;
    ButtonEllipse: TToolButton;
    ButtonCircle: TToolButton;
    ButtonArc: TToolButton;
    ButtonPolygon: TToolButton;
    ButtonBezier: TToolButton;
    ButtonFreeHand: TToolButton;
    ButtonText: TToolButton;
    ButtonLabel: TToolButton;
    InsertLabel: TAction;
    ButtonPicture: TToolButton;
    ToolButton1: TToolButton;
    ButtonDelete: TToolButton;
    ButtonRemovePoint: TToolButton;
    ButtonPen: TToolButton;
    ToolButton3: TToolButton;
    ButtonCut: TToolButton;
    ToolButton5: TToolButton;
    ButtonCopy: TToolButton;
    ButtonPaste: TToolButton;
    EditCut: TAction;
    EditCopy: TAction;
    EditPaste: TAction;
    ButtonUndo: TToolButton;
    ButtonRedo: TToolButton;
    ToolButton7: TToolButton;
    EditUndo: TAction;
    EditRedo: TAction;
    MenuUndo: TMenuItem;
    MenuRedo: TMenuItem;
    N7: TMenuItem;
    MenuCut: TMenuItem;
    MenuCopy: TMenuItem;
    MenuPaste: TMenuItem;
    ToolPan: TAction;
    ButtonPan: TToolButton;
    ToolButton6: TToolButton;
    ButtonZoom: TToolButton;
    ViewZoomIn: TAction;
    ViewZoomOut: TAction;
    ViewZoom10: TAction;
    ViewZoom25: TAction;
    ViewZoom50: TAction;
    ViewZoom100: TAction;
    ViewZoom200: TAction;
    ViewZoom400: TAction;
    ViewZoom800: TAction;
    ViewZoom1600: TAction;
    ViewZoom3200: TAction;
    ViewZoom6400: TAction;
    ViewZoom10000: TAction;
    View1: TMenuItem;
    ZoomIn1: TMenuItem;
    ZoomOut1: TMenuItem;
    Zoom1: TMenuItem;
    Zoom101: TMenuItem;
    Zoom251: TMenuItem;
    Zoom501: TMenuItem;
    Zoom1001: TMenuItem;
    Zoom2001: TMenuItem;
    Zoom4001: TMenuItem;
    Zoom8001: TMenuItem;
    Zoom16001: TMenuItem;
    Zoom32001: TMenuItem;
    Zoom64001: TMenuItem;
    Zoom100001: TMenuItem;
    PopupMenu1: TPopupMenu;
    Zoom102: TMenuItem;
    Zoom252: TMenuItem;
    Zoom502: TMenuItem;
    Zoom1002: TMenuItem;
    Zoom2002: TMenuItem;
    Zoom4002: TMenuItem;
    Zoom8002: TMenuItem;
    Zoom16002: TMenuItem;
    Zoom32002: TMenuItem;
    Zoom64002: TMenuItem;
    Zoom100002: TMenuItem;
    ButtonZoomOut: TToolButton;
    GridType1: TMenuItem;
    GridLine: TMenuItem;
    GridDot: TMenuItem;
    GridNone: TMenuItem;
    N10: TMenuItem;
    Clearallguides1: TMenuItem;
    Options1: TMenuItem;
    SnaptoGrid1: TMenuItem;
    Hottracking1: TMenuItem;
    CanSelect1: TMenuItem;
    Showcursorguide1: TMenuItem;
    Showguides1: TMenuItem;
    Multiselect1: TMenuItem;
    Keyactions1: TMenuItem;
    Mouseactions1: TMenuItem;
    N11: TMenuItem;
    ExporttoBitmap1: TMenuItem;
    N9: TMenuItem;
    SaveDialog2: TSaveDialog;
    ExporttoWMF1: TMenuItem;
    N3: TMenuItem;
    FileSaveAsLibraryItem: TAction;
    SaveasLibraryItem1: TMenuItem;
    N12: TMenuItem;
    InsertLibraryItem: TAction;
    LibraryItem1: TMenuItem;
    SaveDialog3: TSaveDialog;
    ArrangePack: TAction;
    ArrangeUnpack: TAction;
    N13: TMenuItem;
    Pack1: TMenuItem;
    Unpack1: TMenuItem;
    OpenDialog2: TOpenDialog;
    ArrangeLockSelection: TAction;
    N6: TMenuItem;
    Lock1: TMenuItem;
    SCSplitter1: TSCSplitter;
    SCGroupContainer2: TSCGroupContainer;
    SCAdvPanel4: TSCAdvPanel;
    Panel2: TPanel;
    SCGroupContainer1: TSCGroupContainer;
    SCAdvPanel1: TSCAdvPanel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    SCFrameEdit1: TSCFrameEdit;
    SCFrameEdit2: TSCFrameEdit;
    SCFrameEdit3: TSCFrameEdit;
    SCAdvPanel2: TSCAdvPanel;
    Label1: TLabel;
    Label2: TLabel;
    SCBrushStyleCombobox1: TSCBrushStyleCombobox;
    SCPopupColors1: TSCPopupColors;
    SCAdvPanel3: TSCAdvPanel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    SCPopupColors2: TSCPopupColors;
    SCPenStyleCombobox1: TSCPenStyleCombobox;
    SCPenModeCombobox1: TSCPenModeCombobox;
    SCIntSpinEdit1: TSCIntSpinEdit;
    SCAdvPanel5: TSCAdvPanel;
    ToolBar2: TToolBar;
    ToolButton4: TToolButton;
    ToolButton8: TToolButton;
    LayerManager: TSCDeLayerManager;
    ImageList2: TImageList;
    OpenPictureDialog2: TOpenPictureDialog;
    ColorDialog: TColorDialog;
    SCAdvPanel6: TSCAdvPanel;
    Label11: TLabel;
    SCPopupColors3: TSCPopupColors;
    Label10: TLabel;
    SCPopupColors4: TSCPopupColors;
    SCCheckbox1: TSCCheckbox;
    SCCombobox1: TSCCombobox;
    Label12: TLabel;
    SCIntSpinEdit2: TSCIntSpinEdit;
    SCIntSpinEdit3: TSCIntSpinEdit;
    Label13: TLabel;
    Label14: TLabel;
    SCAdvPanel7: TSCAdvPanel;
    SCSpeedButton2: TSCSpeedButton;
    SCSpeedButton1: TSCSpeedButton;
    Label15: TLabel;
    SCCombobox2: TSCCombobox;
    procedure SCCombobox1Change(Sender: TObject);
    procedure SCIntSpinEdit3Change(Sender: TObject);
    procedure SCIntSpinEdit2Change(Sender: TObject);
    procedure SCCheckbox1Click(Sender: TObject);
    procedure SCPopupColors4SelectedColorChange(Sender: TObject);
    procedure SCPopupColors3SelectedColorChange(Sender: TObject);
    procedure SCPopupColors4MoreButtonClick(Sender: TObject);
    procedure SCPopupColors3MoreButtonClick(Sender: TObject);
    procedure InsertPolygonExecute(Sender: TObject);
    procedure InsertBezierExecute(Sender: TObject);
    procedure InsertEllipseExecute(Sender: TObject);
    procedure InsertCircleExecute(Sender: TObject);
    procedure InsertTextExecute(Sender: TObject);
    procedure InsertArcExecute(Sender: TObject);
    procedure InsertRectExecute(Sender: TObject);
    procedure ToolSelectExecute(Sender: TObject);
    procedure ToolSelectAllExecute(Sender: TObject);
    procedure ToolPenExecute(Sender: TObject);
    procedure InsertPictureExecute(Sender: TObject);
    procedure EditDeleteExecute(Sender: TObject);
    procedure SCCAD1EditStateChange(Sender: TObject);
    procedure SCCAD1SelectionChange(Sender: TObject);
    procedure EditNewLayerExecute(Sender: TObject);
    procedure InsertFreehandExecute(Sender: TObject);
    procedure ArrangeBringToFrontExecute(Sender: TObject);
    procedure ArrangeSendToBackExecute(Sender: TObject);
    procedure ArrangeBringForwardExecute(Sender: TObject);
    procedure ArrangeSendBackwardExecute(Sender: TObject);
    procedure ToolDeselectExecute(Sender: TObject);
    procedure ToolRemovePointExecute(Sender: TObject);
    procedure FileCloseExecute(Sender: TObject);
    procedure FileSaveExecute(Sender: TObject);
    procedure FileSaveAsExecute(Sender: TObject);
    procedure FileOpenExecute(Sender: TObject);
    procedure ArrangeGroupExecute(Sender: TObject);
    procedure ArrangeUngroupExecute(Sender: TObject);
    procedure FileNewExecute(Sender: TObject);
    procedure SCCAD1EndNewShape(Sender: TObject; Shape: TSCDeShapeBase);
    procedure InsertLabelExecute(Sender: TObject);
    procedure EditCutExecute(Sender: TObject);
    procedure EditCopyExecute(Sender: TObject);
    procedure EditPasteExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SCCAD1ClipboardChange(Sender: TObject);
    procedure EditUndoExecute(Sender: TObject);
    procedure EditRedoExecute(Sender: TObject);
    procedure SCCAD1Change(Sender: TObject);
    procedure ToolPanExecute(Sender: TObject);
    procedure ViewZoom10Execute(Sender: TObject);
    procedure ViewZoom25Execute(Sender: TObject);
    procedure ViewZoom50Execute(Sender: TObject);
    procedure ViewZoom100Execute(Sender: TObject);
    procedure ViewZoom200Execute(Sender: TObject);
    procedure ViewZoom400Execute(Sender: TObject);
    procedure ViewZoom800Execute(Sender: TObject);
    procedure ViewZoom1600Execute(Sender: TObject);
    procedure ViewZoom3200Execute(Sender: TObject);
    procedure ViewZoom6400Execute(Sender: TObject);
    procedure ViewZoom10000Execute(Sender: TObject);
    procedure ViewZoomInExecute(Sender: TObject);
    procedure ViewZoomOutExecute(Sender: TObject);
    procedure GridLineClick(Sender: TObject);
    procedure GridDotClick(Sender: TObject);
    procedure GridNoneClick(Sender: TObject);
    procedure Clearallguides1Click(Sender: TObject);
    procedure SCCAD1GuideChange(Sender: TObject);
    procedure SnaptoGrid1Click(Sender: TObject);
    procedure CanSelect1Click(Sender: TObject);
    procedure Multiselect1Click(Sender: TObject);
    procedure Hottracking1Click(Sender: TObject);
    procedure Mouseactions1Click(Sender: TObject);
    procedure Keyactions1Click(Sender: TObject);
    procedure Showguides1Click(Sender: TObject);
    procedure Showcursorguide1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ExporttoBitmap1Click(Sender: TObject);
    procedure ExporttoWMF1Click(Sender: TObject);
    procedure FileSaveAsLibraryItemExecute(Sender: TObject);
    procedure InsertLibraryItemExecute(Sender: TObject);
    procedure ArrangePackExecute(Sender: TObject);
    procedure ArrangeUnpackExecute(Sender: TObject);
    procedure ArrangeLockSelectionExecute(Sender: TObject);
    procedure SCSpeedButton2Click(Sender: TObject);
    procedure SCSpeedButton1Click(Sender: TObject);
    procedure SCFrameEdit1Exit(Sender: TObject);
    procedure SCFrameEdit1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure SCFrameEdit2Exit(Sender: TObject);
    procedure SCFrameEdit2KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure SCFrameEdit3Exit(Sender: TObject);
    procedure SCFrameEdit3KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure SCBrushStyleCombobox1SetStyle(Sender: TObject);
    procedure SCPopupColors1MoreButtonClick(Sender: TObject);
    procedure SCPopupColors1SelectedColorChange(Sender: TObject);
    procedure SCPenStyleCombobox1SetStyle(Sender: TObject);
    procedure SCPenModeCombobox1SetMode(Sender: TObject);
    procedure SCPopupColors2MoreButtonClick(Sender: TObject);
    procedure SCPopupColors2SelectedColorChange(Sender: TObject);
    procedure SCIntSpinEdit1Change(Sender: TObject);
    procedure ToolButton4Click(Sender: TObject);
    procedure ToolButton8Click(Sender: TObject);
    procedure SCCombobox2Change(Sender: TObject);
  private
    FFileName: String;
    FUpdating: Integer;
    procedure UpdateProperties;
    procedure UpdateGridTypeMenu;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

type
  TSCDeFakeShape = class(TSCDeShapeBase);

const
  ZoomArray: array[0..10] of Integer = (10, 25, 50, 100, 200, 400, 800,
    1600, 3200, 6400, 10000);

{$R *.DFM}

procedure TMainForm.InsertPolygonExecute(Sender: TObject);
begin
  SCCAD1.SetNewShapeClass(TSCDePolygon);
end;

procedure TMainForm.InsertBezierExecute(Sender: TObject);
begin
  SCCAD1.SetNewShapeClass(TSCDePolyBezier);
end;

procedure TMainForm.InsertEllipseExecute(Sender: TObject);
begin
  SCCAD1.SetNewShapeClass(TSCDeEllipse);
end;

procedure TMainForm.InsertCircleExecute(Sender: TObject);
begin
  SCCAD1.SetNewShapeClass(TSCDeCircle);
end;

procedure TMainForm.InsertTextExecute(Sender: TObject);
begin
  SCCAD1.SetNewShapeClass(TSCDeText);
end;

procedure TMainForm.InsertArcExecute(Sender: TObject);
begin
  SCCAD1.SetNewShapeClass(TSCDeArc);
end;

procedure TMainForm.InsertRectExecute(Sender: TObject);
begin
  SCCAD1.SetNewShapeClass(TSCDeRectangle);
end;

procedure TMainForm.ToolSelectExecute(Sender: TObject);
begin
  SCCAD1.SetSelecting;
end;

procedure TMainForm.ToolSelectAllExecute(Sender: TObject);
begin
  SCCAD1.SelectAll;
end;

procedure TMainForm.ToolPenExecute(Sender: TObject);
begin
  SCCAD1.BeginPointAdd;
end;

procedure TMainForm.InsertPictureExecute(Sender: TObject);
begin
  SCCAD1.SetNewShapeClass(TSCDePicture);
end;

procedure TMainForm.EditDeleteExecute(Sender: TObject);
begin
  SCCAD1.DeleteSelection;
end;

procedure TMainForm.SCCAD1EditStateChange(Sender: TObject);
var
  InSelect, InCreation: Boolean;
begin
  InSelect := SCCAD1.EditState in [SCDrawingCommons.scesNone, scesRectSelect,
    scesMoving, scesSizing, scesRotating, scesNewGuide, scesToolEdit];

  InCreation := SCCAD1.EditState = scesCreateNew;

  ToolSelect.Checked := InSelect;
  ToolPen.Checked := SCCAD1.EditState = scesAddPoint;
  ToolRemovePoint.Checked := SCCAD1.EditState = scesRemovePoint;
  ToolPan.Checked := SCCAD1.EditState = scesZoomRect;

  InsertRect.Checked := InCreation and (SCCAD1.NewShapeClass = TSCDeRectangle);
  InsertPolygon.Checked := InCreation and (SCCAD1.NewShapeClass = TSCDePolygon);
  InsertBezier.Checked := InCreation and (SCCAD1.NewShapeClass = TSCDePolyBezier);
  InsertCircle.Checked := InCreation and (SCCAD1.NewShapeClass = TSCDeCircle);
  InsertEllipse.Checked := InCreation and (SCCAD1.NewShapeClass = TSCDeEllipse);
  InsertText.Checked := InCreation and (SCCAD1.NewShapeClass = TSCDeText);
  InsertArc.Checked := InCreation and (SCCAD1.NewShapeClass = TSCDeArc);
  InsertFreehand.Checked := InCreation and (SCCAD1.NewShapeClass = TSCDeFreeline);
  InsertPicture.Checked := InCreation and (SCCAD1.NewShapeClass = TSCDePicture);
end;

procedure TMainForm.SCCAD1SelectionChange(Sender: TObject);
begin
  ToolDeselect.Enabled := SCCAD1.SelectionCount > 0;
  ToolPen.Enabled := SCCAD1.SelectionCount > 0;
  ToolRemovePoint.Enabled := SCCAD1.SelectionCount > 0;

  EditCut.Enabled := SCCAD1.SelectionCount > 0;
  EditCopy.Enabled := SCCAD1.SelectionCount > 0;
  EditDelete.Enabled := SCCAD1.SelectionCount > 0;

  ArrangeBringToFront.Enabled := SCCAD1.SelectionCount > 0;
  ArrangeSendToBack.Enabled := SCCAD1.SelectionCount > 0;
  ArrangeBringForward.Enabled := SCCAD1.SelectionCount > 0;
  ArrangeSendBackward.Enabled := SCCAD1.SelectionCount > 0;

  ArrangeGroup.Enabled := SCCAD1.SelectionCount > 0;
  ArrangeUngroup.Enabled := SCCAD1.SelectionCount > 0;

  ArrangePack.Enabled := SCCAD1.SelectionCount > 0;
  ArrangeUnpack.Enabled := SCCAD1.SelectionCount > 0;

  ArrangeLockSelection.Enabled := SCCAD1.SelectionCount > 0;

  FileSaveAsLibraryItem.Enabled := (SCCAD1.SelectionCount = 1)
    and (SCCAD1.Selections[0] is TSCDePackage);

  UpdateProperties;
end;

procedure TMainForm.SCCheckbox1Click(Sender: TObject);
var
  I: Integer;
  S: TSCDeFakeShape;
begin
  if FUpdating = 0 then
  begin
    if SCCAD1.SelectionCount > 0 then
    begin
      SCCAD1.BeginUpdate;
      try
        for I := SCCAD1.SelectionCount-1 downto 0 do
        begin
          S := TSCDeFakeShape(SCCAD1.Selections[I]);
          if (scssUsesGradient in S.ShapeStyle) and (S.Gradient <> nil) then
            S.Gradient.Reverse := SCCheckbox1.Checked;
        end;
      finally
        SCCAD1.EndUpdate;
      end;
    end;
  end;
end;

procedure TMainForm.SCCombobox1Change(Sender: TObject);
var
  I, Index: Integer;
  S: TSCDeFakeShape;
  Gs: TSCDeShapeGradientStyle;
begin
  Index := SCCombobox1.ItemIndex;
  if (Index < 0) then Index := 0;
  
  Gs := TSCDeShapeGradientStyle(Index);

  if FUpdating = 0 then
  begin
    if SCCAD1.SelectionCount > 0 then
    begin
      SCCAD1.BeginUpdate;
      try
        for I := SCCAD1.SelectionCount-1 downto 0 do
        begin
          S := TSCDeFakeShape(SCCAD1.Selections[I]);
          if (scssUsesGradient in S.ShapeStyle) and (S.Gradient <> nil) then
            S.Gradient.Style := Gs;
        end;
      finally
        SCCAD1.EndUpdate;
      end;
    end;
  end;

  UpdateProperties;
end;

procedure TMainForm.EditNewLayerExecute(Sender: TObject);
begin
  SCCAD1.ActiveLayer := TSCDeLayer.Create(SCCAD1);
end;

procedure TMainForm.InsertFreehandExecute(Sender: TObject);
begin
  SCCAD1.SetNewShapeClass(TSCDeFreeLine);
end;

procedure TMainForm.ArrangeBringToFrontExecute(Sender: TObject);
begin
  SCCAD1.BringToFront;
end;

procedure TMainForm.ArrangeSendToBackExecute(Sender: TObject);
begin
  SCCAD1.SendToBack;
end;

procedure TMainForm.ArrangeBringForwardExecute(Sender: TObject);
begin
  SCCAD1.BringForward;
end;

procedure TMainForm.ArrangeSendBackwardExecute(Sender: TObject);
begin
  SCCAD1.SendBackward;
end;

procedure TMainForm.ToolDeselectExecute(Sender: TObject);
begin
  SCCAD1.ClearSelection;
end;

procedure TMainForm.ToolRemovePointExecute(Sender: TObject);
begin
  SCCAD1.BeginPointRemove;
end;

procedure TMainForm.FileCloseExecute(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.FileSaveExecute(Sender: TObject);
begin
  if (FFileName <> '') and DirectoryExists(ExtractFilePath(FFileName)) then
    SCCAD1.SaveToFile(FFileName)
  else
    FileSaveAsExecute(FileSaveAs);
end;

procedure TMainForm.FileSaveAsExecute(Sender: TObject);
var
  FileName: String;
begin
  SaveDialog1.FileName := FFileName;

  if SaveDialog1.Execute then
  begin
    FileName := SaveDialog1.FileName;
    if ExtractFileExt(FileName) = '' then FileName := FileName + '.sde';

    SCCAD1.SaveToFile(FileName);

    FFileName := FileName;
    Caption := 'Mini CAD - [' + FFileName + ']';
  end;
end;

procedure TMainForm.FileOpenExecute(Sender: TObject);
var
  FileName: String;
begin
  if OpenDialog1.Execute then
  begin
    FileName := OpenDialog1.FileName;

    SCCAD1.LoadFromFile(FileName);
    FFileName := FileName;

    Caption := 'Mini CAD - [' + FFileName + ']';
  end;
end;

procedure TMainForm.ArrangeGroupExecute(Sender: TObject);
begin
  SCCAD1.GroupSelection;
end;

procedure TMainForm.ArrangeUngroupExecute(Sender: TObject);
begin
  SCCAD1.UngroupSelection;
end;

procedure TMainForm.FileNewExecute(Sender: TObject);
begin
  SCCAD1.Clear;
  FFileName := '';
  Caption := 'Mini CAD - [New]';
end;

procedure TMainForm.SCCAD1EndNewShape(Sender: TObject;
  Shape: TSCDeShapeBase);
var
  S: String;
begin
  if Shape <> nil then
  begin
    if Shape is TSCDePicture then
    begin
      if not OpenPictureDialog1.Execute then
        Shape.Free
      else
        TSCDePicture(Shape).Picture.LoadFromFile(OpenPictureDialog1.FileName);
    end else
    if Shape is TSCDeText then
    begin
      S := '';
      if not InputQuery('Text', 'Enter text value', S) then
        S := '';

      S := Trim(S);
      if S = '' then
      begin
        Shape.Free;
        Exit;
      end;

      TSCDeText(Shape).Caption := S;
    end else
    if Shape is TSCDeLabel then
    begin
      S := '';
      if not InputQuery('Label', 'Enter label value', S) then
        S := '';

      S := Trim(S);
      if S = '' then
      begin
        Shape.Free;
        Exit;
      end;

      TSCDeLabel(Shape).Caption := S;
    end;
  end;
end;

procedure TMainForm.InsertLabelExecute(Sender: TObject);
begin
  SCCAD1.SetNewShapeClass(TSCDeLabel);
end;

procedure TMainForm.EditCutExecute(Sender: TObject);
begin
  SCCAD1.CutToClipboard;
end;

procedure TMainForm.EditCopyExecute(Sender: TObject);
begin
  SCCAD1.CopyToClipboard;
end;

procedure TMainForm.EditPasteExecute(Sender: TObject);
begin
  SCCAD1.PasteFromClipboard;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  SCCAD1.ActiveLayer := TSCDeLayer.Create(SCCAD1);
end;

procedure TMainForm.SCCAD1ClipboardChange(Sender: TObject);
begin
  EditPaste.Enabled := SCCAD1.HasClipboardData;
end;

procedure TMainForm.EditUndoExecute(Sender: TObject);
begin
  SCCAD1.Undo;
  UpdateProperties;
end;

procedure TMainForm.EditRedoExecute(Sender: TObject);
begin
  SCCAD1.Redo;
  UpdateProperties;
end;

procedure TMainForm.SCCAD1Change(Sender: TObject);
begin
  EditUndo.Enabled := SCCAD1.CanUndo;
  EditRedo.Enabled := SCCAD1.CanRedo;
end;

procedure TMainForm.ToolPanExecute(Sender: TObject);
begin
  SCCAD1.SetPanView;
end;

procedure TMainForm.ViewZoom10Execute(Sender: TObject);
begin
  SCCAD1.Zoom := 10;
end;

procedure TMainForm.ViewZoom25Execute(Sender: TObject);
begin
  SCCAD1.Zoom := 25;
end;

procedure TMainForm.ViewZoom50Execute(Sender: TObject);
begin
  SCCAD1.Zoom := 50;
end;

procedure TMainForm.ViewZoom100Execute(Sender: TObject);
begin
  SCCAD1.Zoom := 100;
end;

procedure TMainForm.ViewZoom200Execute(Sender: TObject);
begin
  SCCAD1.Zoom := 200;
end;

procedure TMainForm.ViewZoom400Execute(Sender: TObject);
begin
  SCCAD1.Zoom := 400;
end;

procedure TMainForm.ViewZoom800Execute(Sender: TObject);
begin
  SCCAD1.Zoom := 800;
end;

procedure TMainForm.ViewZoom1600Execute(Sender: TObject);
begin
  SCCAD1.Zoom := 1600;
end;

procedure TMainForm.ViewZoom3200Execute(Sender: TObject);
begin
  SCCAD1.Zoom := 3200;
end;

procedure TMainForm.ViewZoom6400Execute(Sender: TObject);
begin
  SCCAD1.Zoom := 6400;
end;

procedure TMainForm.ViewZoom10000Execute(Sender: TObject);
begin
  SCCAD1.Zoom := 10000;
end;

procedure TMainForm.ViewZoomInExecute(Sender: TObject);
var
  Z, I, Min: Integer;
begin
  Z := SCCAD1.Zoom;
  
  if Z < 10 then Z := 10
  else if Z > 10000 then Z := 10000;

  Min := 0;
  for I := 0 to 10 do
  begin
    if (Z >= Min) and (Z < ZoomArray[I]) then
    begin
      Z := ZoomArray[I];
      Break;
    end;

    Min := ZoomArray[I];
  end;

  SCCAD1.Zoom := Z;
end;

procedure TMainForm.ViewZoomOutExecute(Sender: TObject);
var
  Z, I, Max: Integer;
begin
  Z := SCCAD1.Zoom;
  
  if Z < 10 then Z := 10
  else if Z > 10000 then Z := 10000;

  Max := 10000;
  for I := 10 downto 0 do
  begin
    if (Z <= Max) and (Z > ZoomArray[I]) then
    begin
      Z := ZoomArray[I];
      Break;
    end;

    Max := ZoomArray[I];
  end;

  SCCAD1.Zoom := Z;
end;

procedure TMainForm.UpdateProperties;
var
  Pm: TPenMode;
  I, Pw: Integer;
  Ps: TPenStyle;
  Bs: TBrushStyle;
  S: TSCDeFakeShape;
  Bc, Pc: TColor;
  N, T, C: String;
  PenFound, BrushFound,
  NameFound, CaptionFound,
  TextFound, PictureFound,
  GradientFound, HasPicture: Boolean;
  AReverse: Boolean;
  AColorBegin, AColorEnd: TColor;
  ARotation: TSCDeGradientRotation;
  AShift: TSCDeGradientShift;
  AStyle: TSCDeShapeGradientStyle;
  PicStyle: TSCDeShapePictureStyle;
begin
  Inc(FUpdating);
  try
    Bs := SCCAD1.DefaultBrush.Style;
    Bc := SCCAD1.DefaultBrush.Color;

    Ps := SCCAD1.DefaultPen.Style;
    Pm := SCCAD1.DefaultPen.Mode;
    Pc := SCCAD1.DefaultPen.Color;
    Pw := SCCAD1.DefaultPen.Width;

    T := '';
    N := '';
    C := '';

    PenFound := False;
    BrushFound := False;
    CaptionFound := False;
    NameFound := False;
    TextFound := False;
    PictureFound := False;
    HasPicture := False;
    GradientFound := False;

    AReverse := False;
    AColorBegin := clLime;
    AColorEnd := clWhite;
    AStyle := scdgsNone;
    ARotation := 0;
    AShift := 0;

    PicStyle := scdspStretch;

    for I := 0 to SCCAD1.SelectionCount-1 do
    begin
      S := TSCDeFakeShape(SCCAD1.Selections[I]);

      if (scssUsesPicture in S.ShapeStyle) then
      begin
        if not PictureFound then
        begin
          PictureFound := True;
          PicStyle := S.PictureStyle;
          HasPicture := S.HasPicture;
        end else
        if PicStyle <> S.PictureStyle then
          PicStyle := scdspStretch;
      end;

      if (scssUsesGradient in S.ShapeStyle) and (S.Gradient <> nil) then
      begin
        if not GradientFound then
        begin
          GradientFound := True;

          Ps := S.Pen.Style;
          Pm := S.Pen.Mode;
          Pc := S.Pen.Color;
          Pw := S.Pen.Width;

          AReverse := S.Gradient.Reverse;
          AColorBegin := S.Gradient.ColorBegin;
          AColorEnd := S.Gradient.ColorEnd;
          AStyle := S.Gradient.Style;
          ARotation := S.Gradient.Rotation;
          AShift := S.Gradient.Shift;
        end else
        begin
          if AReverse <> S.Gradient.Reverse then AReverse := False;
          if AColorBegin <> S.Gradient.ColorBegin then AColorBegin := clLime;
          if AColorEnd <> S.Gradient.ColorEnd then AColorEnd := clWhite;
          if AStyle <> S.Gradient.Style then AStyle := scdgsNone;
          if ARotation <> S.Gradient.Rotation then ARotation := 0;
          if AShift <> S.Gradient.Shift then AShift := 0;
        end;
      end;

      if scssUsesBrush in S.ShapeStyle then
      begin
        if not BrushFound then
        begin
          BrushFound := True;

          Bs := S.Brush.Style;
          Bc := S.Brush.Color;
        end else
        begin
          if Bs <> S.Brush.Style then Bs := bsSolid;
          if Bc <> S.Brush.Color then Bc := clNone;
        end;
      end;

      if scssUsesPen in S.ShapeStyle then
      begin
        if not PenFound then
        begin
          PenFound := True;

          Ps := S.Pen.Style;
          Pm := S.Pen.Mode;
          Pc := S.Pen.Color;
          Pw := S.Pen.Width;
        end else
        begin
          if Ps <> S.Pen.Style then Ps := psSolid;
          if Pm <> S.Pen.Mode then Pm := pmCopy;
          if Pc <> S.Pen.Color then Pc := clNone;
          if Pw <> S.Pen.Width then Pw := 0;
        end;
      end;

      if scssUsesCaption in S.ShapeStyle then
      begin
        if not CaptionFound then
        begin
          CaptionFound := True;
          C := S.Caption;
        end else
        if C <> S.Caption then
          C := '';
      end;

      if not TextFound then
      begin
        TextFound := True;
        T := S.Text;
      end else
      if T <> S.Text then
        T := '';

      if not NameFound then
      begin
        NameFound := True;
        N := S.Name;
      end else
      if N <> S.Name then
        N := '';
    end;

    if SCCAD1.SelectionCount = 0 then
    begin
      if not GradientFound then
        AColorBegin := SCCAD1.DefaultBrush.Color;

      if not BrushFound then
      begin
        Bc := SCCAD1.DefaultBrush.Color;
        Bs := SCCAD1.DefaultBrush.Style;
      end;
    end;

    SCFrameEdit1.Text := N;
    SCFrameEdit2.Text := T;
    SCFrameEdit3.Text := C;

    SCPopupColors1.SelectedColor := Bc;
    SCBrushStyleCombobox1.SelectedStyle := Bs;
    SCBrushStyleCombobox1.BrushColor := Bc;

    SCPenStyleCombobox1.SelectedStyle := Ps;
    SCPenModeCombobox1.SelectedMode := Pm;
    SCPopupColors2.SelectedColor := Pc;
    SCIntSpinEdit1.IntValue := Pw;

    SCSpeedButton1.Enabled := PictureFound;
    SCSpeedButton2.Enabled := HasPicture;
    SCCombobox2.ItemIndex  := Integer(PicStyle);

    SCPopupColors3.SelectedColor := AColorBegin;
    SCPopupColors4.SelectedColor := AColorEnd;
    SCCheckbox1.Checked := AReverse;
    SCCombobox1.ItemIndex := Integer(AStyle);
    SCIntSpinEdit2.IntValue := ARotation;
    SCIntSpinEdit3.IntValue := AShift;
  finally
    Dec(FUpdating);
  end;
end;

procedure TMainForm.GridLineClick(Sender: TObject);
begin
  SCCAD1.Grid.GridType := scgtLine;
  UpdateGridTypeMenu;
end;

procedure TMainForm.GridDotClick(Sender: TObject);
begin
  SCCAD1.Grid.GridType := scgtDot;
  UpdateGridTypeMenu;
end;

procedure TMainForm.GridNoneClick(Sender: TObject);
begin
  SCCAD1.Grid.GridType := scgtNone;
  UpdateGridTypeMenu;
end;

procedure TMainForm.Clearallguides1Click(Sender: TObject);
begin
  SCCAD1.Guides.Clear;
end;

procedure TMainForm.SCCAD1GuideChange(Sender: TObject);
begin
  Clearallguides1.Enabled := SCCAD1.Guides.Count > 0;
end;

procedure TMainForm.SnaptoGrid1Click(Sender: TObject);
begin
  SnaptoGrid1.Checked := not SnaptoGrid1.Checked;
  if SnaptoGrid1.Checked then
    SCCAD1.EditOptions := SCCAD1.EditOptions + [scdoSnapToGrid]
  else
    SCCAD1.EditOptions := SCCAD1.EditOptions - [scdoSnapToGrid];
end;

procedure TMainForm.CanSelect1Click(Sender: TObject);
begin
  CanSelect1.Checked := not CanSelect1.Checked;
  if CanSelect1.Checked then
    SCCAD1.EditOptions := SCCAD1.EditOptions + [scdoCanSelect]
  else
    SCCAD1.EditOptions := SCCAD1.EditOptions - [scdoCanSelect];
end;

procedure TMainForm.Multiselect1Click(Sender: TObject);
begin
  Multiselect1.Checked := not Multiselect1.Checked;
  if Multiselect1.Checked then
    SCCAD1.EditOptions := SCCAD1.EditOptions + [scdoMultiSelect]
  else
    SCCAD1.EditOptions := SCCAD1.EditOptions - [scdoMultiSelect];
end;

procedure TMainForm.Hottracking1Click(Sender: TObject);
begin
  Hottracking1.Checked := not Hottracking1.Checked;
  if Hottracking1.Checked then
    SCCAD1.EditOptions := SCCAD1.EditOptions + [scdoHottrack]
  else
    SCCAD1.EditOptions := SCCAD1.EditOptions - [scdoHottrack];
end;

procedure TMainForm.Mouseactions1Click(Sender: TObject);
begin
  Mouseactions1.Checked := not Mouseactions1.Checked;
  if Mouseactions1.Checked then
    SCCAD1.EditOptions := SCCAD1.EditOptions + [scdoMouseActions]
  else
    SCCAD1.EditOptions := SCCAD1.EditOptions - [scdoMouseActions];
end;

procedure TMainForm.Keyactions1Click(Sender: TObject);
begin
  Keyactions1.Checked := not Keyactions1.Checked;
  if Keyactions1.Checked then
    SCCAD1.EditOptions := SCCAD1.EditOptions + [scdoKeyActions]
  else
    SCCAD1.EditOptions := SCCAD1.EditOptions - [scdoKeyActions];
end;

procedure TMainForm.Showguides1Click(Sender: TObject);
begin
  Showguides1.Checked := not Showguides1.Checked;
  if Showguides1.Checked then
    SCCAD1.EditOptions := SCCAD1.EditOptions + [scdoShowGuides]
  else
    SCCAD1.EditOptions := SCCAD1.EditOptions - [scdoShowGuides];
end;

procedure TMainForm.Showcursorguide1Click(Sender: TObject);
begin
  Showcursorguide1.Checked := not Showcursorguide1.Checked;
  if Showcursorguide1.Checked then
    SCCAD1.EditOptions := SCCAD1.EditOptions + [scdoShowCursorGuide]
  else
    SCCAD1.EditOptions := SCCAD1.EditOptions - [scdoShowCursorGuide];
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  SnaptoGrid1.Checked := scdoSnapToGrid in SCCAD1.EditOptions;
  CanSelect1.Checked := scdoCanSelect in SCCAD1.EditOptions;
  Multiselect1.Checked := scdoMultiSelect in SCCAD1.EditOptions;
  Hottracking1.Checked := scdoHottrack in SCCAD1.EditOptions;
  Mouseactions1.Checked := scdoMouseActions in SCCAD1.EditOptions;
  Keyactions1.Checked := scdoKeyActions in SCCAD1.EditOptions;
  Showguides1.Checked := scdoShowGuides in SCCAD1.EditOptions;
  Showcursorguide1.Checked := scdoShowCursorGuide in SCCAD1.EditOptions;

  UpdateGridTypeMenu;
end;

procedure TMainForm.UpdateGridTypeMenu;
begin
  GridLine.Checked := SCCAD1.Grid.GridType = scgtLine;
  GridDot.Checked := SCCAD1.Grid.GridType = scgtDot;
  GridNone.Checked := SCCAD1.Grid.GridType = scgtNone;
end;

procedure TMainForm.ExporttoBitmap1Click(Sender: TObject);
var
  R: TRect;
  I: Integer;
  Bmp: TBitmap;
begin
  SaveDialog2.DefaultExt := 'bmp';
  SaveDialog2.Filter := 'Bitmap File (*.bmp)|*.bmp';

  if SaveDialog2.Execute then
  begin
    Bmp := TBitmap.Create;
    try
      R := Rect(0, 0, SCCAD1.Layer.Width, SCCAD1.Layer.Height);
      Bmp.Width := R.Right - R.Left;
      Bmp.Height := R.Bottom - R.Top;

      with Bmp.Canvas do
      begin
        Brush.Color := clWhite;
        Brush.Style := bsSolid;

        FillRect(R);
      end;

      for I := 0 to SCCAD1.LayerCount-1 do
        SCCAD1.Layers[I].Paint(Bmp.Canvas, 0, 0, 1);

      Bmp.SaveToFile(SaveDialog2.FileName);
    finally
      Bmp.Free;
    end;
  end;
end;

procedure TMainForm.ExporttoWMF1Click(Sender: TObject);
var
  R: TRect;
  I: Integer;
  WMF: TMetaFile;
  WMFC: TMetafileCanvas;
begin
  SaveDialog2.DefaultExt := 'wmf';
  SaveDialog2.Filter := 'Windows Meta File (*.wmf)|*.wmf';

  if SaveDialog2.Execute then
  begin
    WMF := TMetaFile.Create;
    try
      R := Rect(0, 0, SCCAD1.Layer.Width, SCCAD1.Layer.Height);

      WMF.Enhanced := false;

      WMF.Width := R.Right - R.Left;
      WMF.Height := R.Bottom - R.Top;

      WMFC := TMetafileCanvas.Create(WMF, 0);
      try
        with WMFC do
        begin
          Brush.Color := clWhite;
          Brush.Style := bsSolid;

          FillRect(R);
        end;

        for I := 0 to SCCAD1.LayerCount-1 do
          SCCAD1.Layers[I].Paint(WMFC, 0, 0, 1);
      finally
        WMFC.Free;
      end;

      WMF.SaveToFile(SaveDialog2.FileName);
    finally
      WMF.Free;
    end;
  end;
end;

procedure TMainForm.FileSaveAsLibraryItemExecute(Sender: TObject);
var
  P: TSCDePackage;
begin
  if (SCCAD1.SelectionCount = 1) and (SCCAD1.Selections[0] is TSCDePackage) and
    SaveDialog3.Execute then
  begin
    P := TSCDePackage(SCCAD1.Selections[0]);
    P.SaveToFile(SaveDialog3.FileName);
  end;
end;

procedure TMainForm.InsertLibraryItemExecute(Sender: TObject);
var
  B: TDoubleRect;
  L: TSCDeLayer;
  P: TSCDePackage;
begin
  SCCAD1.SetSelecting;
  
  if OpenDialog2.Execute and FileExists(OpenDialog2.FileName) then
  begin
    SCCAD1.BeginUpdate;
    try
      P := TSCDePackage.Create(SCCAD1, nil);
      try
        P.LoadFromFile(OpenDialog2.FileName);

        B := P.GetBounds;
        P.MoveBy(-B.Left, -B.Top);

        L := SCCAD1.HandleActiveLayer;
        L.Add(P);

        SCCAD1.ClearSelection;
        SCCAD1.AddSelection(P);
      except
        P.Free;
      end;
    finally
      SCCAD1.EndUpdate;
    end;
  end;
end;

procedure TMainForm.ArrangePackExecute(Sender: TObject);
begin
  SCCAD1.PackSelection;
end;

procedure TMainForm.ArrangeUnpackExecute(Sender: TObject);
begin
  SCCAD1.UnpackSelection;
end;

procedure TMainForm.ArrangeLockSelectionExecute(Sender: TObject);
begin
  SCCAD1.LockSelection;
end;

procedure TMainForm.SCSpeedButton2Click(Sender: TObject);
var
  I: Integer;
  S: TSCDeFakeShape;
begin
  if (FUpdating = 0) and (SCCAD1.SelectionCount > 0) then
  begin
    SCCAD1.BeginUpdate;
    try
      for I := SCCAD1.SelectionCount-1 downto 0 do
      begin
        S := TSCDeFakeShape(SCCAD1.Selections[I]);
        if scssUsesPicture in S.ShapeStyle then
          S.Picture.Assign(nil);
      end;
    finally
      SCCAD1.EndUpdate;
      UpdateProperties;
    end;
  end;
end;

procedure TMainForm.SCSpeedButton1Click(Sender: TObject);
var
  I: Integer;
  P: TPicture;
  S: TSCDeFakeShape;
begin
  if (FUpdating = 0) and (SCCAD1.SelectionCount > 0) and OpenPictureDialog2.Execute then
  begin
    P := TPicture.Create;
    try
      P.LoadFromFile(OpenPictureDialog2.FileName);

      SCCAD1.BeginUpdate;
      try
        for I := SCCAD1.SelectionCount-1 downto 0 do
        begin
          S := TSCDeFakeShape(SCCAD1.Selections[I]);
          if scssUsesPicture in S.ShapeStyle then
            S.Picture.Assign(P);
        end;
      finally
        SCCAD1.EndUpdate;
      end;
    finally
      P.Free;
      UpdateProperties;
    end;
  end;
end;

procedure TMainForm.SCFrameEdit1Exit(Sender: TObject);
var
  I: Integer;
  S: TSCDeFakeShape;
begin
  if FUpdating = 0 then
  begin
    SCCAD1.BeginUpdate;
    try
      for I := SCCAD1.SelectionCount-1 downto 0 do
      begin
        S := TSCDeFakeShape(SCCAD1.Selections[I]);
        S.Name := SCFrameEdit1.Text;
      end;
    finally
      SCCAD1.EndUpdate;
    end;
  end;
end;

procedure TMainForm.SCFrameEdit1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then
  begin
    Key := 0;
    SCFrameEdit1Exit(nil);
  end;
end;

procedure TMainForm.SCFrameEdit2Exit(Sender: TObject);
var
  I: Integer;
  S: TSCDeFakeShape;
begin
  if FUpdating = 0 then
  begin
    SCCAD1.BeginUpdate;
    try
      for I := SCCAD1.SelectionCount-1 downto 0 do
      begin
        S := TSCDeFakeShape(SCCAD1.Selections[I]);
        S.Text := SCFrameEdit2.Text;
      end;
    finally
      SCCAD1.EndUpdate;
    end;
  end;
end;

procedure TMainForm.SCFrameEdit2KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then
  begin
    Key := 0;
    SCFrameEdit2Exit(nil);
  end;
end;

procedure TMainForm.SCFrameEdit3Exit(Sender: TObject);
var
  I: Integer;
  S: TSCDeFakeShape;
begin
  if FUpdating = 0 then
  begin
    SCCAD1.BeginUpdate;
    try
      for I := SCCAD1.SelectionCount-1 downto 0 do
      begin
        S := TSCDeFakeShape(SCCAD1.Selections[I]);
        if scssUsesCaption in S.ShapeStyle then
          S.Caption := SCFrameEdit3.Text;
      end;
    finally
      SCCAD1.EndUpdate;
    end;
  end;
end;

procedure TMainForm.SCFrameEdit3KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then
  begin
    Key := 0;
    SCFrameEdit3Exit(nil);
  end;
end;

procedure TMainForm.SCBrushStyleCombobox1SetStyle(Sender: TObject);
var
  I: Integer;
  S: TSCDeFakeShape;
  Bs: TBrushStyle;
begin
  if FUpdating = 0 then
  begin
    Bs := SCBrushStyleCombobox1.SelectedStyle;

    if SCCAD1.SelectionCount = 0 then
      SCCAD1.DefaultBrush.Style := Bs
    else begin
      SCCAD1.BeginUpdate;
      try
        for I := SCCAD1.SelectionCount-1 downto 0 do
        begin
          S := TSCDeFakeShape(SCCAD1.Selections[I]);
          if scssUsesBrush in S.ShapeStyle then
            S.Brush.Style := Bs;
        end;
      finally
        SCCAD1.EndUpdate;
      end;
    end;
  end;

  UpdateProperties;
end;

procedure TMainForm.SCPopupColors1MoreButtonClick(Sender: TObject);
begin
  ColorDialog.Color := SCPopupColors1.SelectedColor;
  if ColorDialog.Execute then
    SCPopupColors1.SelectedColor := ColorDialog.Color;
end;

procedure TMainForm.SCPopupColors1SelectedColorChange(Sender: TObject);
var
  Bc: TColor;
  I: Integer;
  S: TSCDeFakeShape;
begin
  Bc := SCPopupColors1.SelectedColor;

  if FUpdating = 0 then
  begin
    if SCCAD1.SelectionCount = 0 then
      SCCAD1.DefaultBrush.Color := Bc
    else begin
      SCCAD1.BeginUpdate;
      try
        for I := SCCAD1.SelectionCount-1 downto 0 do
        begin
          S := TSCDeFakeShape(SCCAD1.Selections[I]);
          if scssUsesBrush in S.ShapeStyle then
            S.Brush.Color := Bc;
        end;
      finally
        SCCAD1.EndUpdate;
      end;
    end;
  end;

  UpdateProperties;
end;

procedure TMainForm.SCPenStyleCombobox1SetStyle(Sender: TObject);
var
  I: Integer;
  Ps: TPenStyle;
  S: TSCDeFakeShape;
begin
  if FUpdating = 0 then
  begin
    Ps := SCPenStyleCombobox1.SelectedStyle;

    if SCCAD1.SelectionCount = 0 then
      SCCAD1.DefaultPen.Style := Ps
    else begin
      SCCAD1.BeginUpdate;
      try
        for I := SCCAD1.SelectionCount-1 downto 0 do
        begin
          S := TSCDeFakeShape(SCCAD1.Selections[I]);
          if scssUsesPen in S.ShapeStyle then
            S.Pen.Style := Ps;
        end;
      finally
        SCCAD1.EndUpdate;
      end;
    end;
  end;
end;

procedure TMainForm.SCPenModeCombobox1SetMode(Sender: TObject);
var
  I: Integer;
  Pm: TPenMode;
  S: TSCDeFakeShape;
begin
  if FUpdating = 0 then
  begin
    Pm := SCPenModeCombobox1.SelectedMode;

    if SCCAD1.SelectionCount = 0 then
      SCCAD1.DefaultPen.Mode := Pm
    else begin
      SCCAD1.BeginUpdate;
      try
        for I := SCCAD1.SelectionCount-1 downto 0 do
        begin
          S := TSCDeFakeShape(SCCAD1.Selections[I]);
          if scssUsesPen in S.ShapeStyle then
            S.Pen.Mode := Pm;
        end;
      finally
        SCCAD1.EndUpdate;
      end;
    end;
  end;
end;

procedure TMainForm.SCPopupColors2MoreButtonClick(Sender: TObject);
begin
  ColorDialog.Color := SCPopupColors2.SelectedColor;
  if ColorDialog.Execute then
    SCPopupColors2.SelectedColor := ColorDialog.Color;
end;

procedure TMainForm.SCPopupColors2SelectedColorChange(Sender: TObject);
var
  Pc: TColor;
  I: Integer;
  S: TSCDeFakeShape;
begin
  Pc := SCPopupColors2.SelectedColor;
  SCPenStyleCombobox1.PenColor := Pc;

  if FUpdating = 0 then
  begin
    if SCCAD1.SelectionCount = 0 then
      SCCAD1.DefaultPen.Color := Pc
    else begin
      SCCAD1.BeginUpdate;
      try
        for I := SCCAD1.SelectionCount-1 downto 0 do
        begin
          S := TSCDeFakeShape(SCCAD1.Selections[I]);
          if scssUsesPen in S.ShapeStyle then
            S.Pen.Color := Pc;
        end;
      finally
        SCCAD1.EndUpdate;
      end;
    end;
  end;
end;

procedure TMainForm.SCPopupColors3MoreButtonClick(Sender: TObject);
begin
  ColorDialog.Color := SCPopupColors3.SelectedColor;
  if ColorDialog.Execute then
    SCPopupColors3.SelectedColor := ColorDialog.Color;
end;

procedure TMainForm.SCPopupColors3SelectedColorChange(Sender: TObject);
var
  Bc: TColor;
  I: Integer;
  S: TSCDeFakeShape;
begin
  Bc := SCPopupColors3.SelectedColor;

  if FUpdating = 0 then
  begin
    if SCCAD1.SelectionCount = 0 then
      SCCAD1.DefaultBrush.Color := Bc
    else begin
      SCCAD1.BeginUpdate;
      try
        for I := SCCAD1.SelectionCount-1 downto 0 do
        begin
          S := TSCDeFakeShape(SCCAD1.Selections[I]);
          if (scssUsesGradient in S.ShapeStyle) and (S.Gradient <> nil) then
            S.Gradient.ColorBegin := Bc;
        end;
      finally
        SCCAD1.EndUpdate;
      end;
    end;
  end;

  UpdateProperties;
end;

procedure TMainForm.SCPopupColors4MoreButtonClick(Sender: TObject);
begin
  ColorDialog.Color := SCPopupColors4.SelectedColor;
  if ColorDialog.Execute then
    SCPopupColors4.SelectedColor := ColorDialog.Color;
end;

procedure TMainForm.SCPopupColors4SelectedColorChange(Sender: TObject);
var
  Bc: TColor;
  I: Integer;
  S: TSCDeFakeShape;
begin
  Bc := SCPopupColors4.SelectedColor;

  if FUpdating = 0 then
  begin
    if SCCAD1.SelectionCount > 0 then
    begin
      SCCAD1.BeginUpdate;
      try
        for I := SCCAD1.SelectionCount-1 downto 0 do
        begin
          S := TSCDeFakeShape(SCCAD1.Selections[I]);
          if (scssUsesGradient in S.ShapeStyle) and (S.Gradient <> nil) then
            S.Gradient.ColorEnd := Bc;
        end;
      finally
        SCCAD1.EndUpdate;
      end;
    end;
  end;

  UpdateProperties;
end;

procedure TMainForm.SCIntSpinEdit1Change(Sender: TObject);
var
  I, W: Integer;
  S: TSCDeFakeShape;
begin
  if FUpdating = 0 then
  begin
    W := SCIntSpinEdit1.IntValue;

    if SCCAD1.SelectionCount = 0 then
      SCCAD1.DefaultPen.Width := W
    else begin
      SCCAD1.BeginUpdate;
      try
        for I := SCCAD1.SelectionCount-1 downto 0 do
        begin
          S := TSCDeFakeShape(SCCAD1.Selections[I]);
          if scssUsesPen in S.ShapeStyle then
            S.Pen.Width := W;
        end;
      finally
        SCCAD1.EndUpdate;
      end;
    end;
  end;
end;

procedure TMainForm.SCIntSpinEdit2Change(Sender: TObject);
var
  I, Value: Integer;
  S: TSCDeFakeShape;
begin
  try
    Value := SCIntSpinEdit2.IntValue;

    if FUpdating = 0 then
    begin
      if SCCAD1.SelectionCount > 0 then
      begin
        SCCAD1.BeginUpdate;
        try
          for I := SCCAD1.SelectionCount-1 downto 0 do
          begin
            S := TSCDeFakeShape(SCCAD1.Selections[I]);
            if (scssUsesGradient in S.ShapeStyle) and (S.Gradient <> nil) then
              S.Gradient.Rotation := Value;
          end;
        finally
          SCCAD1.EndUpdate;
        end;
      end;
    end;
  except
  end;
end;

procedure TMainForm.SCIntSpinEdit3Change(Sender: TObject);
var
  I, Value: Integer;
  S: TSCDeFakeShape;
begin
  try
    Value := SCIntSpinEdit3.IntValue;

    if FUpdating = 0 then
    begin
      if SCCAD1.SelectionCount > 0 then
      begin
        SCCAD1.BeginUpdate;
        try
          for I := SCCAD1.SelectionCount-1 downto 0 do
          begin
            S := TSCDeFakeShape(SCCAD1.Selections[I]);
            if (scssUsesGradient in S.ShapeStyle) and (S.Gradient <> nil) then
              S.Gradient.Shift := Value;
          end;
        finally
          SCCAD1.EndUpdate;
        end;
      end;
    end;
  except
  end;
end;

procedure TMainForm.ToolButton4Click(Sender: TObject);
var
  S: String;
  L: TSCDeLayer;
  Found: Boolean;
  I, J, Cnt: Integer;
begin
  SCCAD1.BeginUpdate;
  try
    L := TSCDeLayer.Create(SCCAD1);
    SCCAD1.BringToFront(L);

    S := '';
    Cnt := SCCAD1.LayerCount;

    for I := 0 to Cnt-1 do
    begin
      S := 'Layer - ' + IntToStr(I + 1);

      Found := False;
      for J := 0 to Cnt-1 do
        if SCCAD1.Layers[J].Name = S then
        begin
          Found := True;
          Break;
        end;

      if not Found then
      begin
        L.Name := S;
        Break;
      end;
    end;

    L.Name := S;
    SCCAD1.ActiveLayer := L;
  finally
    SCCAD1.EndUpdate;
  end;
end;

procedure TMainForm.ToolButton8Click(Sender: TObject);
var
  L: TSCDeLayer;
begin
  L := SCCAD1.ActiveLayer;
  if L <> nil then
  begin
    if SCCAD1.LayerCount > 1 then
      L.Free
    else
      L.Clear();
  end;
end;

procedure TMainForm.SCCombobox2Change(Sender: TObject);
var
  I, Index: Integer;
  S: TSCDeFakeShape;
  Ps: TSCDeShapePictureStyle;
begin
  Index := SCCombobox2.ItemIndex;
  if (Index < 0) then Index := 0;

  Ps := TSCDeShapePictureStyle(Index);

  if FUpdating = 0 then
  begin
    if SCCAD1.SelectionCount > 0 then
    begin
      SCCAD1.BeginUpdate;
      try
        for I := SCCAD1.SelectionCount-1 downto 0 do
        begin
          S := TSCDeFakeShape(SCCAD1.Selections[I]);
          if (scssUsesPicture in S.ShapeStyle) then
            S.PictureStyle := Ps;
        end;
      finally
        SCCAD1.EndUpdate;
      end;
    end;
  end;

  UpdateProperties;
end;

end.
