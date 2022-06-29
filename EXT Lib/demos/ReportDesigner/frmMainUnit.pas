{**********************************************************}
{                                                          }
{  Report Designer                                         }
{  Devrace Extension Library example of                    }
{  TELDesigner, TELDesignPanel                             }
{                                                          }
{  Copyright (c) 2001 - 2002, Balabuyev Yevgeny            }
{  Contact: yebalabuyev@devrace.com                        }
{                                                          }
{ -------------------------------------------------------- }
{  ExtLib home page      : http://www.devrace.com/extlib   }
{  ExtLib support e-mail : extlib@devrace.com              }
{ -------------------------------------------------------- }
{                                                          }
{  Please see the file License.txt for full license        }
{  information                                             }
{                                                          }
{**********************************************************}

unit frmMainUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, ActnList, DB, DBTables, StdCtrls, ELDsgnr, Buttons, ExtCtrls,
  QuickRpt, QRCtrls, ImgList, ComCtrls, ToolWin, StdActns;

type
  TfrmMain = class(TForm)
    ActionList1: TActionList;
    MainMenu1: TMainMenu;
    actNew: TAction;
    New1: TMenuItem;
    New2: TMenuItem;
    Panel1: TPanel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton5: TSpeedButton;
    SpeedButton6: TSpeedButton;
    SpeedButton7: TSpeedButton;
    SpeedButton8: TSpeedButton;
    SpeedButton9: TSpeedButton;
    SpeedButton10: TSpeedButton;
    actPreview: TAction;
    Report1: TMenuItem;
    actPreview1: TMenuItem;
    actPropsView: TAction;
    View1: TMenuItem;
    Properties1: TMenuItem;
    SaveDialog1: TSaveDialog;
    actSave: TAction;
    actSaveAs: TAction;
    actOpen: TAction;
    OpenDialog1: TOpenDialog;
    actCloseAll: TAction;
    Open1: TMenuItem;
    Save1: TMenuItem;
    SaveAs1: TMenuItem;
    Closeall1: TMenuItem;
    actClose: TAction;
    Close1: TMenuItem;
    ControlBar1: TControlBar;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ImageList1: TImageList;
    actSaveSaveAll: TAction;
    actCopy: TAction;
    actCut: TAction;
    actPaste: TAction;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    Edit1: TMenuItem;
    Copy1: TMenuItem;
    Cut1: TMenuItem;
    Paste1: TMenuItem;
    N1: TMenuItem;
    Close2: TMenuItem;
    PopupMenu1: TPopupMenu;
    Copy2: TMenuItem;
    Cut2: TMenuItem;
    Paste2: TMenuItem;
    N2: TMenuItem;
    actDelete: TAction;
    N3: TMenuItem;
    Delete1: TMenuItem;
    Delete2: TMenuItem;
    actLock: TAction;
    Lock1: TMenuItem;
    Lock2: TMenuItem;
    actUnlock: TAction;
    actUnlock1: TMenuItem;
    Unlock1: TMenuItem;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    actUnlockAll: TAction;
    UnlockAll1: TMenuItem;
    actSelectAll: TAction;
    actSelectAll1: TMenuItem;
    actAlignToGrid: TAction;
    actAlignToGrid1: TMenuItem;
    AlignToGrid1: TMenuItem;
    actBringToFront: TAction;
    actSendToBack: TAction;
    N5: TMenuItem;
    AlignToGrid2: TMenuItem;
    Sendtoback1: TMenuItem;
    Bringtofront1: TMenuItem;
    Sendtoback2: TMenuItem;
    N6: TMenuItem;
    ToolButton14: TToolButton;
    ToolButton15: TToolButton;
    ToolButton16: TToolButton;
    ToolButton17: TToolButton;
    WindowCascade1: TWindowCascade;
    WindowTileHorizontal1: TWindowTileHorizontal;
    WindowTileVertical1: TWindowTileVertical;
    WindowMinimizeAll1: TWindowMinimizeAll;
    WindowArrange1: TWindowArrange;
    Window1: TMenuItem;
    Arrange1: TMenuItem;
    Cascade1: TMenuItem;
    MinimizeAll1: TMenuItem;
    ileHorizontally1: TMenuItem;
    ileVertically1: TMenuItem;
    actEnabled: TAction;
    actEnable1: TMenuItem;
    Enable1: TMenuItem;
    actEnableAll: TAction;
    EnableAll1: TMenuItem;
    N7: TMenuItem;
    N8: TMenuItem;
    actChangeData: TAction;
    Changedata1: TMenuItem;
    ToolButton18: TToolButton;
    ComboBox1: TComboBox;
    actZoom100: TAction;
    Zoomto1001: TMenuItem;
    actPrint: TAction;
    Print1: TMenuItem;
    N4: TMenuItem;
    ToolButton19: TToolButton;
    ToolButton20: TToolButton;
    ToolButton21: TToolButton;
    ToolBar2: TToolBar;
    actALLeft: TAction;
    actALRight: TAction;
    actALTop: TAction;
    actALBottom: TAction;
    actALHSpace: TAction;
    actALVSpace: TAction;
    actALHCenter: TAction;
    actALVCenter: TAction;
    actALHCenterWindow: TAction;
    actALVCenterWindow: TAction;
    ToolButton22: TToolButton;
    ToolButton23: TToolButton;
    ToolButton24: TToolButton;
    ToolButton25: TToolButton;
    ToolButton26: TToolButton;
    ToolButton27: TToolButton;
    ToolButton28: TToolButton;
    ToolButton29: TToolButton;
    ToolButton30: TToolButton;
    ToolButton31: TToolButton;
    ToolButton32: TToolButton;
    Align1: TMenuItem;
    actALLeft1: TMenuItem;
    actALRight1: TMenuItem;
    actALHSpace1: TMenuItem;
    actALHCenter1: TMenuItem;
    actALHCenterWindow1: TMenuItem;
    N9: TMenuItem;
    actALTop1: TMenuItem;
    actALBottom1: TMenuItem;
    actALVSpace1: TMenuItem;
    actALVCenter1: TMenuItem;
    actALVCenterWindow1: TMenuItem;
    Align2: TMenuItem;
    actALLeft2: TMenuItem;
    actALRight2: TMenuItem;
    actALHSpace2: TMenuItem;
    actALHCenter2: TMenuItem;
    actALHCenterWindow2: TMenuItem;
    N10: TMenuItem;
    actALTop2: TMenuItem;
    actALBottom2: TMenuItem;
    actALVSpace2: TMenuItem;
    actALVCenter2: TMenuItem;
    actALVCenterWindow2: TMenuItem;
    actRepProps: TAction;
    actRepProps1: TMenuItem;
    Panel2: TPanel;
    Label4: TLabel;
    Bevel1: TBevel;
    procedure actNewExecute(Sender: TObject);
    procedure actPreviewExecute(Sender: TObject);
    procedure actPreviewUpdate(Sender: TObject);
    procedure actPropsViewExecute(Sender: TObject);
    procedure actPropsViewUpdate(Sender: TObject);
    procedure actOpenExecute(Sender: TObject);
    procedure actSaveExecute(Sender: TObject);
    procedure actSaveUpdate(Sender: TObject);
    procedure actSaveAsUpdate(Sender: TObject);
    procedure actSaveAsExecute(Sender: TObject);
    procedure actCloseAllUpdate(Sender: TObject);
    procedure actCloseAllExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure actCloseUpdate(Sender: TObject);
    procedure actCloseExecute(Sender: TObject);
    procedure actSaveSaveAllUpdate(Sender: TObject);
    procedure actSaveSaveAllExecute(Sender: TObject);
    procedure actCopyUpdate(Sender: TObject);
    procedure actCutUpdate(Sender: TObject);
    procedure actPasteUpdate(Sender: TObject);
    procedure actCopyExecute(Sender: TObject);
    procedure actCutExecute(Sender: TObject);
    procedure actPasteExecute(Sender: TObject);
    procedure actDeleteExecute(Sender: TObject);
    procedure actDeleteUpdate(Sender: TObject);
    procedure actLockUpdate(Sender: TObject);
    procedure actLockExecute(Sender: TObject);
    procedure actUnlockUpdate(Sender: TObject);
    procedure actUnlockExecute(Sender: TObject);
    procedure actUnlockAllUpdate(Sender: TObject);
    procedure actUnlockAllExecute(Sender: TObject);
    procedure actSelectAllUpdate(Sender: TObject);
    procedure actSelectAllExecute(Sender: TObject);
    procedure actAlignToGridUpdate(Sender: TObject);
    procedure actAlignToGridExecute(Sender: TObject);
    procedure actBringToFrontUpdate(Sender: TObject);
    procedure actSendToBackUpdate(Sender: TObject);
    procedure actBringToFrontExecute(Sender: TObject);
    procedure actSendToBackExecute(Sender: TObject);
    procedure actEnabledUpdate(Sender: TObject);
    procedure actEnabledExecute(Sender: TObject);
    procedure actEnableAllUpdate(Sender: TObject);
    procedure actEnableAllExecute(Sender: TObject);
    procedure actChangeDataUpdate(Sender: TObject);
    procedure actChangeDataExecute(Sender: TObject);
    procedure Close2Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure actZoom100Update(Sender: TObject);
    procedure actZoom100Execute(Sender: TObject);
    procedure actPrintUpdate(Sender: TObject);
    procedure actPrintExecute(Sender: TObject);
    procedure actALLeftUpdate(Sender: TObject);
    procedure actALLeftExecute(Sender: TObject);
    procedure actRepPropsUpdate(Sender: TObject);
    procedure actRepPropsExecute(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure Save(ADoc: TForm);
    function SaveAs(ADoc: TForm): Boolean;
    function CloseAll: Boolean;
    procedure ControlInserting(var AControlClass: TControlClass);
    procedure ControlInserted;
    procedure UpdateZoomComboBox(AClosingForm: Boolean);
  end;

var
  frmMain: TfrmMain;

implementation

uses dlgDataUnit, frmDocUnit, frmPropsUnit, dlgReportPropsUnit;

{$R *.dfm}

procedure TfrmMain.actNewExecute(Sender: TObject);
var
  LDataSet: TBDEDataSet;
  LForm: TfrmDoc;
begin
  LForm := TfrmDoc.Create(Application);
  if dlgData.Execute(LDataSet, LForm) then
    LForm.DataSet := LDataset
  else
    LForm.Free;
end;

procedure TfrmMain.ControlInserted;
begin
  SpeedButton1.Down := True;
end;

procedure TfrmMain.ControlInserting(var AControlClass: TControlClass);
begin
  if SpeedButton10.Down then
    AControlClass := TQRBand
  else if SpeedButton2.Down then
    AControlClass := TQRLabel
  else if SpeedButton3.Down then
    AControlClass := TQRDBText
  else if SpeedButton5.Down then
    AControlClass := TQRExpr
  else if SpeedButton6.Down then
    AControlClass := TQRSysData
  else if SpeedButton7.Down then
    AControlClass := TQRMemo
  else if SpeedButton8.Down then
    AControlClass := TQRExprMemo
  else if SpeedButton9.Down then
    AControlClass := TQRShape;
end;

procedure TfrmMain.actPreviewExecute(Sender: TObject);
var
  LV: Boolean;
begin
  LV := frmProps.Visible;
  frmProps.Visible := False;
  try
    TfrmDoc(ActiveMDIChild).DataSet.Open;
    TfrmDoc(ActiveMDIChild).Report.PreviewModal;
  finally
    frmProps.Visible := LV;
  end;
end;

procedure TfrmMain.actPreviewUpdate(Sender: TObject);
begin
  actPreview.Enabled := (ActiveMDIChild <> nil);
end;

procedure TfrmMain.actPropsViewExecute(Sender: TObject);
begin
  frmProps.Visible := not frmProps.Visible;
end;

procedure TfrmMain.actPropsViewUpdate(Sender: TObject);
begin
  actPropsView.Checked := frmProps.Visible;
end;

procedure TfrmMain.Save(ADoc: TForm);
begin
  TfrmDoc(ADoc).Save;
end;

function TfrmMain.SaveAs(ADoc: TForm): Boolean;
var
  LS: string;
begin
  if TfrmDoc(ADoc).FileName <> '' then
    LS := TfrmDoc(ADoc).FileName
  else
    LS := TfrmDoc(ADoc).Caption;
  SaveDialog1.FileName := LS;
  Result := SaveDialog1.Execute;
  if Result then TfrmDoc(ADoc).SaveAs(SaveDialog1.FileName);
end;

procedure TfrmMain.actOpenExecute(Sender: TObject);
begin
  if OpenDialog1.Execute then
    TfrmDoc.CreateDocument(Application, OpenDialog1.FileName);
end;

procedure TfrmMain.actSaveExecute(Sender: TObject);
begin
  Save(ActiveMDIChild);
end;

procedure TfrmMain.actSaveUpdate(Sender: TObject);
begin
  actSave.Enabled := (ActiveMDIChild <> nil) and
    (TfrmDoc(ActiveMDIChild).Modified) and (TfrmDoc(ActiveMDIChild).FileName <> '');
end;

procedure TfrmMain.actSaveAsUpdate(Sender: TObject);
begin
  actSaveAs.Enabled := (ActiveMDIChild <> nil);
end;

procedure TfrmMain.actSaveAsExecute(Sender: TObject);
begin
  SaveAs(ActiveMDIChild);
end;

function TfrmMain.CloseAll: Boolean;
var
  LI: Integer;
begin
  Result := True;
  for LI := MDIChildCount - 1 downto 0 do
  begin
    MDIChildren[LI].Close;
    if not FormWasClosed then
    begin
      Result := False;
      Break;
    end;
  end;
end;

procedure TfrmMain.actCloseAllUpdate(Sender: TObject);
begin
  actCloseAll.Enabled := MDIChildCount > 0;
end;

procedure TfrmMain.actCloseAllExecute(Sender: TObject);
begin
  CloseAll;
end;

procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if not CloseAll then Action := caNone;
end;

procedure TfrmMain.actCloseUpdate(Sender: TObject);
begin
  actClose.Enabled := (ActiveMDIChild <> nil);
end;

procedure TfrmMain.actCloseExecute(Sender: TObject);
begin
  ActiveMDIChild.Close;
end;

procedure TfrmMain.actSaveSaveAllUpdate(Sender: TObject);
begin
  actSaveSaveAll.Enabled := (ActiveMDIChild <> nil) and
    ((TfrmDoc(ActiveMDIChild).Modified) or (TfrmDoc(ActiveMDIChild).FileName = ''));
end;

procedure TfrmMain.actSaveSaveAllExecute(Sender: TObject);
begin
  if TfrmDoc(ActiveMDIChild).FileName = '' then
    SaveAs(ActiveMDIChild)
  else
    Save(ActiveMDIChild);
end;

procedure TfrmMain.actCopyUpdate(Sender: TObject);
begin
  actCopy.Enabled := (ActiveMDIChild <> nil) and
    TfrmDoc(ActiveMDIChild).Designer.CanCopyToClipboard;
end;

procedure TfrmMain.actCutUpdate(Sender: TObject);
begin
  actCut.Enabled := (ActiveMDIChild <> nil) and
    TfrmDoc(ActiveMDIChild).Designer.CanCutToClipboard;
end;

procedure TfrmMain.actPasteUpdate(Sender: TObject);
begin
  actPaste.Enabled := (ActiveMDIChild <> nil) and
    TfrmDoc(ActiveMDIChild).Designer.CanPasteFromClipboard;
end;

procedure TfrmMain.actCopyExecute(Sender: TObject);
begin
  TfrmDoc(ActiveMDIChild).Designer.CopyToClipboard;
end;

procedure TfrmMain.actCutExecute(Sender: TObject);
begin
  TfrmDoc(ActiveMDIChild).Designer.CutToClipboard;
end;

procedure TfrmMain.actPasteExecute(Sender: TObject);
begin
  TfrmDoc(ActiveMDIChild).Designer.PasteFromClipboard;
end;

procedure TfrmMain.actDeleteExecute(Sender: TObject);
begin
  TfrmDoc(ActiveMDIChild).Designer.DeleteSelectedControls;
end;

procedure TfrmMain.actDeleteUpdate(Sender: TObject);
begin
  actDelete.Enabled := (ActiveMDIChild <> nil);
end;

procedure TfrmMain.actLockUpdate(Sender: TObject);
begin
  actLock.Enabled := (ActiveMDIChild <> nil);
end;

procedure TfrmMain.actLockExecute(Sender: TObject);
begin
  TfrmDoc(ActiveMDIChild).Designer.SelectedControls.Lock(
    [lmNoResize, lmNoMove, lmNoDelete]);
end;

procedure TfrmMain.actUnlockUpdate(Sender: TObject);
begin
  actUnlock.Enabled := (ActiveMDIChild <> nil);
end;

procedure TfrmMain.actUnlockExecute(Sender: TObject);
begin
  TfrmDoc(ActiveMDIChild).Designer.SelectedControls.Lock([]);
end;

procedure TfrmMain.actUnlockAllUpdate(Sender: TObject);
begin
  actUnlockAll.Enabled := (ActiveMDIChild <> nil);
end;

procedure TfrmMain.actUnlockAllExecute(Sender: TObject);
begin
  TfrmDoc(ActiveMDIChild).Designer.LockAll([]);
end;

procedure TfrmMain.actSelectAllUpdate(Sender: TObject);
begin
  actSelectAll.Enabled := (ActiveMDIChild <> nil);
end;

procedure TfrmMain.actSelectAllExecute(Sender: TObject);
begin
  TfrmDoc(ActiveMDIChild).Designer.SelectAll;
end;

procedure TfrmMain.actAlignToGridUpdate(Sender: TObject);
begin
  actAlignToGrid.Enabled := (ActiveMDIChild <> nil);
end;

procedure TfrmMain.actAlignToGridExecute(Sender: TObject);
begin
  TfrmDoc(ActiveMDIChild).Designer.SelectedControls.AlignToGrid;
end;

procedure TfrmMain.actBringToFrontUpdate(Sender: TObject);
begin
  actBringToFront.Enabled := (ActiveMDIChild <> nil);
end;

procedure TfrmMain.actSendToBackUpdate(Sender: TObject);
begin
  actSendToBack.Enabled := (ActiveMDIChild <> nil);
end;

procedure TfrmMain.actBringToFrontExecute(Sender: TObject);
begin
  TfrmDoc(ActiveMDIChild).Designer.SelectedControls.BringToFront;
end;

procedure TfrmMain.actSendToBackExecute(Sender: TObject);
begin
  TfrmDoc(ActiveMDIChild).Designer.SelectedControls.SendToBack;
end;

procedure TfrmMain.actEnabledUpdate(Sender: TObject);
begin
  actEnabled.Enabled := (ActiveMDIChild <> nil) and
    (TfrmDoc(ActiveMDIChild).Designer.SelectedControls.Count = 1) and
    (TfrmDoc(ActiveMDIChild).Designer.SelectedControls.DefaultControl <>
    TfrmDoc(ActiveMDIChild).Report);
  actEnabled.Checked := actEnabled.Enabled and
    TfrmDoc(ActiveMDIChild).Designer.SelectedControls.DefaultControl.Enabled;
end;

procedure TfrmMain.actEnabledExecute(Sender: TObject);
begin
  TfrmDoc(ActiveMDIChild).Designer.SelectedControls.DefaultControl.Enabled :=
    not TfrmDoc(ActiveMDIChild).Designer.SelectedControls.DefaultControl.Enabled;
  frmProps.PropInsp.UpdateContent;
  TfrmDoc(ActiveMDIChild).Modify;
end;

procedure TfrmMain.actEnableAllUpdate(Sender: TObject);
begin
  actEnableAll.Enabled := (ActiveMDIChild <> nil);
end;

procedure TfrmMain.actEnableAllExecute(Sender: TObject);
var
  LI: Integer;
begin
  for LI := 0 to TfrmDoc(ActiveMDIChild).Report.ComponentCount - 1 do
    if TfrmDoc(ActiveMDIChild).Report.Components[LI] is TControl then
      TControl(TfrmDoc(ActiveMDIChild).Report.Components[LI]).Enabled := True;
  frmProps.PropInsp.UpdateContent;
  TfrmDoc(ActiveMDIChild).Modify;
end;

procedure TfrmMain.actChangeDataUpdate(Sender: TObject);
begin
  actChangeData.Enabled := (ActiveMDIChild <> nil);
end;

procedure TfrmMain.actChangeDataExecute(Sender: TObject);
var
  LDataSet: TBDEDataSet;
begin
  if dlgData.Execute(LDataSet, ActiveMDIChild) then
  begin
    TfrmDoc(ActiveMDIChild).DataSet := LDataSet;
    frmProps.PropInsp.UpdateContent;
    TfrmDoc(ActiveMDIChild).Modify;
  end;
end;

procedure TfrmMain.Close2Click(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.ComboBox1Change(Sender: TObject);
var
  LZoom: Integer;
begin
  if ActiveMDIChild <> nil then
  begin
    LZoom := TfrmDoc(ActiveMDIChild).Report.Zoom;
    case ComboBox1.ItemIndex of
      0: LZoom := 25;
      1: LZoom := 50;
      2: LZoom := 75;
      3: LZoom := 100;
      4: LZoom := 150;
      5: LZoom := 200;
    end;
    TfrmDoc(ActiveMDIChild).Report.Zoom := LZoom;
    TfrmDoc(ActiveMDIChild).Report.Left := 0;
    TfrmDoc(ActiveMDIChild).Report.Top := 0;
    
    TfrmDoc(ActiveMDIChild).Designer.SelectedControls.Update;
  end
  else ComboBox1.ItemIndex := -1;
end;

procedure TfrmMain.actZoom100Update(Sender: TObject);
begin
  actZoom100.Enabled := (ActiveMDIChild <> nil);
end;

procedure TfrmMain.actZoom100Execute(Sender: TObject);
begin
  TfrmDoc(ActiveMDIChild).Report.Zoom := 100;
  TfrmDoc(ActiveMDIChild).Designer.SelectedControls.Update;
  ComboBox1.ItemIndex := 3;
end;

procedure TfrmMain.UpdateZoomComboBox(AClosingForm: Boolean);
var
  LIndex: Integer;
begin
  if (ActiveMDIChild <> nil) and not AClosingForm then
  begin
    case TfrmDoc(ActiveMDIChild).Report.Zoom of
      25: LIndex := 0;
      50: LIndex := 1;
      75: LIndex := 2;
      100: LIndex := 3;
      150: LIndex := 4;
      200: LIndex := 5;
    else
      LIndex := -1;
    end;
    ComboBox1.ItemIndex := LIndex;
  end
  else ComboBox1.ItemIndex := -1;
end;

procedure TfrmMain.actPrintUpdate(Sender: TObject);
begin
  actPrint.Enabled := (ActiveMDIChild <> nil);
end;

procedure TfrmMain.actPrintExecute(Sender: TObject);
var
  LV: Boolean;
begin
  LV := frmProps.Visible;
  frmProps.Visible := False;
  try
    TfrmDoc(ActiveMDIChild).DataSet.Open;
    TfrmDoc(ActiveMDIChild).Report.Print;
  finally
    frmProps.Visible := LV;
  end;
end;

procedure TfrmMain.actALLeftUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (ActiveMDIChild <> nil);
end;

procedure TfrmMain.actALLeftExecute(Sender: TObject);
var
  LHorzAlignType, LVertAlignType: TELDesignerAlignType;
begin
  LHorzAlignType := atNoChanges;
  LVertAlignType := atNoChanges;
  case TAction(Sender).Tag of
    0: LHorzAlignType := atLeftTop;
    1: LHorzAlignType := atRightBottom;
    2: LVertAlignType := atLeftTop;
    3: LVertAlignType := atRightBottom;
    4: LHorzAlignType := atSpaceEqually;
    5: LVertAlignType := atSpaceEqually;
    6: LHorzAlignType := atCenter;
    7: LVertAlignType := atCenter;
    8: LHorzAlignType := atCenterInWindow;
    9: LVertAlignType := atCenterInWindow;
  end;
  TfrmDoc(ActiveMDIChild).Designer.SelectedControls.Align(LHorzAlignType, LVertAlignType);
end;

procedure TfrmMain.actRepPropsUpdate(Sender: TObject);
begin
  actRepProps.Enabled := (ActiveMDIChild <> nil);
end;

procedure TfrmMain.actRepPropsExecute(Sender: TObject);
begin
  if dlgReportProps.Execute(TfrmDoc(ActiveMDIChild).Report) then
  begin
    frmProps.PropInsp.UpdateContent;
    TfrmDoc(ActiveMDIChild).Modify;
  end;
end;

initialization
  ForceCurrentDirectory := True;
  RegisterClasses([TQuickRep, TQRBand, TQRLabel, TQRDBText, TQRExpr,
    TQRSysData, TQRMemo, TQRExprMemo, TQRShape]);

end.

