{*********************************************************}
{* FlashFiler: Main form for verification utility        *}
{*********************************************************}

(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TurboPower FlashFiler
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1996-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

{$I ffdefine.inc}

unit frMain;

interface

uses
  Windows, Messages, SysUtils,
  {$IFDEF DCC6OrLater}
  Variants,
  {$ENDIF}
  Classes, Graphics, Controls, Forms,
  Dialogs, Menus, ExtCtrls, ComCtrls, FFRepair, FFFileInt, StdCtrls;

{ TODO::

  Tasks listed by order of development:

  - UI: view individual blocks within the file
    - display index block data
    - display data block data

  - test index verify/repair

  - file interface needs property to identify if a file is currently opened.
  - backup of existing file to another directory
  - incorporate chain gang for verification of deleted block chain
  - verify/repair data block
  - unknown block type error should result in need to restructure
  - verify/repair stream block
  - BLOB verify/repair
  - display file size
  - allow max ram of repair engine to be adjusted
  - display max ram being used while verify/repair in progress
  - duration of verification & repair

  FUTURE development tasks:

  - handle multi-file tables
  - BLOB stats
  - View block map of file
}

type
  TfrmMain = class(TForm)
    pnlTop: TPanel;
    mnuMain: TMainMenu;
    mnuFile: TMenuItem;
    mnuFileOpen: TMenuItem;
    mnuFileClose: TMenuItem;
    mnuFileSep1: TMenuItem;
    mnuFileExit: TMenuItem;
    mnuFileSep2: TMenuItem;
    mnuFileVerify: TMenuItem;
    mnuFileRepair: TMenuItem;
    tvMain: TTreeView;
    Splitter: TSplitter;
    dlgOpen: TOpenDialog;
    Notebook: TPageControl;
    pgProps: TTabSheet;
    lvProps: TListView;
    pgData: TTabSheet;
    lvData: TListView;
    pgStatus: TTabSheet;
    pnlStatusBottom: TPanel;
    progressBar: TProgressBar;
    memStatus: TMemo;
    lblStatus: TLabel;
    pgRawData: TTabSheet;
    lvRawData: TListView;
    mnuFileSep3: TMenuItem;
    mnuFileViewBlock: TMenuItem;
    mnuChain: TMenuItem;
    mnuChainViewData: TMenuItem;
    mnuChainViewFree: TMenuItem;
    pgReadMe: TTabSheet;
    memReadMe: TMemo;
    mnuOptions: TMenuItem;
    procedure FormShow(Sender: TObject);
    procedure mnuFileOpenClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure mnuFileExitClick(Sender: TObject);
    procedure tvMainClick(Sender: TObject);
    procedure mnuFileCloseClick(Sender: TObject);
    procedure tvMainGetSelectedIndex(Sender: TObject; Node: TTreeNode);
    procedure mnuFileVerifyClick(Sender: TObject);
    procedure mnuFileRepairClick(Sender: TObject);
    procedure NotebookChange(Sender: TObject);
    procedure mnuFileViewBlockClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure mnuChainViewDataClick(Sender: TObject);
    procedure mnuChainViewFreeClick(Sender: TObject);
    procedure mnuOptionsClick(Sender: TObject);
  private
    { Private declarations }
    FBlockNumToNodeMap : TStringList;
    FCurNode : TTreeNode;
    FDataBlocksNode : TTreeNode;
    FFileHeaderBlock : IFileHeaderBlock;
    FFileName : string;
    FIndexBlocksNode : TTreeNode;
    FLastItem : TffRepairItem;
    FOtherBlocksNode : TTreeNode;
    FOutputVersion : Longint;
    FRepair : TffRepairEngine;
    FState : TffRepairState;
    FViewedBlocks : TInterfaceList;

    procedure ClearAll;
    procedure ClearData;
    procedure ClearProps;
    procedure ClearRawData;
    procedure ClearRepair;
    procedure ClearStatus;
    procedure ClearTreeView;
    procedure ClearUI;
    procedure DisplayData(const Block : ICommonBlock);
    procedure DisplayProps(const Block : ICommonBlock);
    procedure DisplayRawData(const Block : ICommonBlock);
    procedure LoadUI;
    procedure OnComplete(Sender : TObject);
    procedure OnProgress(Repairer : TffRepairEngine;
                         State : TffRepairState;
                         Item : TffRepairItem;
                   const ActionStr : string;
                   const Position, Maximum : Integer);
    procedure OnReportError(Block : ICommonBlock;
                      const ErrCode : Integer;
                      const ErrorStr : string);
    procedure OnReportFix(Block : ICommonBlock;
                    const ErrCode : Integer;
                    const RepairStr : string);
    procedure PositionToNode(Node : TTreeNode);
    procedure ReleaseBlocksAndNodes;
    procedure SetCtrlStates;
    procedure Status(const Msg : string; args : array of const);
    procedure VerifyRepair;
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

uses
  frmBlock,
  FFLLBase,
  FFSrBase,
  FFRepCnst, frmOptions;

const
  csBlock  = 'Block %d';
  csDataBlocks = 'Data blocks';
  csDataDict   = 'Data dictionary';
  csFileHeader = 'File header';
  csIndexBlocks = 'Index blocks';
  csIndexHeader = 'Index header';
  csOtherBlocks = 'Other blocks';
  csStatusSep = '============================================================';

function Singular(const Value : Integer;
                  const Singular, Plural : string) : string;
begin
  Result := IntToStr(Value) + ' ';
  if Value = 1 then
    Result := Result + Singular
  else
    Result := Result + Plural;
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  ClearTreeView;
  NoteBook.ActivePage := pgReadMe;
//  NoteBook.ActivePage := pgProps;
  SetCtrlStates;
end;

procedure TfrmMain.ClearAll;
begin
  ClearRepair;
  ClearTreeView;
  ClearProps;
  ClearData;
  ClearRawData;
  ClearStatus;
end;

procedure TfrmMain.ClearData;
begin
  lvData.Columns.Clear;
  lvData.Items.Clear;
end;

procedure TfrmMain.ClearProps;
begin
  lvProps.Columns.Clear;
  lvProps.Items.Clear;
end;

procedure TfrmMain.ClearRawData;
begin
  lvRawData.Columns.Clear;
  lvRawData.Items.Clear;
end;

procedure TfrmMain.ClearUI;
begin
  ClearTreeView;
  ClearProps;
  ClearData;
  ClearRawData;
  { Note: This method does not clear the status page. }
end;

procedure TfrmMain.ReleaseBlocksAndNodes;
begin
  FFileHeaderBlock := nil;
  FDataBlocksNode := nil;
  FIndexBlocksNode := nil;
  FOtherBlocksNode := nil;
  FViewedBlocks.Clear;
end;

procedure TfrmMain.ClearRepair;
begin
  if FRepair <> nil then begin
    ReleaseBlocksAndNodes;
    FRepair.Free;
    FRepair := nil;
  end;
end;

procedure TfrmMain.ClearStatus;
begin
  memStatus.Clear;
  FLastItem := riNone;
end;

procedure TfrmMain.ClearTreeView;
begin
  FCurNode := nil;
  tvMain.Items.Clear;
  tvMain.Items.Add(nil, '<open a FlashFiler table>');
end;

procedure TfrmMain.DisplayData(const Block : ICommonBlock);
var
  Col, ColCount, Row : Integer;
  Column : TListColumn;
  Item : TListItem;
begin
  ClearData;
  ColCount := Block.DataColCount;
  for Col := 0 to Pred(ColCount) do begin
    Column := lvData.Columns.Add;
    Column.Caption := Block.DataColCaption[Col];
    Column.Width := Block.DataColWidth[Col];
  end;

  for Row := 0 to Pred(Block.DataRowCount) do begin
    Item := lvData.Items.Add;
    for Col := 0 to Pred(ColCount) do begin
      if Col = 0 then
        Item.Caption := Block.DataCell[Row, Col]
      else
        Item.SubItems.Add(Block.DataCell[Row, Col]);
    end;  { for }
  end;  { for }
end;

procedure TfrmMain.DisplayProps(const Block : ICommonBlock);
var
  Col, ColCount, Row : Integer;
  Column : TListColumn;
  Item : TListItem;
begin
  ClearProps;
  ColCount := Block.PropertyColCount;
  for Col := 0 to Pred(ColCount) do begin
    Column := lvProps.Columns.Add;
    Column.Caption := Block.PropertyColCaption[Col];
    Column.Width := Block.PropertyColWidth[Col];
  end;

  for Row := 0 to Pred(Block.PropertyRowCount) do begin
    Item := lvProps.Items.Add;
    for Col := 0 to Pred(ColCount) do begin
      if Col = 0 then
        Item.Caption := Block.PropertyCell[Row, Col]
      else
        Item.SubItems.Add(Block.PropertyCell[Row, Col]);
    end;  { for }
  end;  { for }
end;

procedure TfrmMain.DisplayRawData(const Block : ICommonBlock);
var
  Row : Integer;
  Column : TListColumn;
  Item : TListItem;
  RawData : PffBlock;
  Strings : TStringList;
begin
  ClearRawData;
  RawData := Block.RawData;
  Strings := TStringList.Create;
  try
    { Format the raw data. }
    GenerateHexLines(RawData, FFileHeaderBlock.BlockSize, Strings);

    { Set up the columns. }
    Column := lvRawData.Columns.Add;
    Column.Caption := 'Offset';
    Column.Width := 70;

    Column := lvRawData.Columns.Add;
    Column.Caption := 'Bytes';
    Column.Width := 475;

    for Row := 0 to Pred(Strings.Count) do begin
      Item := lvRawData.Items.Add;
      Item.Caption := LongintToHex(Row * 16);
      Item.SubItems.Add(Strings[Row]);
    end;

  finally
    Strings.Free;
  end;
end;

procedure TfrmMain.LoadUI;
var
  DictRootNode,
  FileHeaderNode,
  RootNode : TTreeNode;
  Inx : Integer;
  DictBlock : IStreamBlock;
  IndexHeaderBlock : IIndexHeaderBlock;
begin
  { Set up the tree view. Display a root node identifying the file. Add
    child nodes that provide access to the header block, dictionary blocks,
    & index header. }
  tvMain.Items.Clear;
  RootNode := tvMain.Items.Add(nil, ExtractFileName(FFileName));
  FFileHeaderBlock := FRepair.GetFileHeaderBlock;
  FileHeaderNode := tvMain.Items.AddChildObject(RootNode, csFileHeader,
                                                Pointer(FFileHeaderBlock));

  DictRootNode := tvMain.Items.AddChild(RootNode, csDataDict);

  for Inx := 0 to Pred(FRepair.DictBlockCount) do begin
    DictBlock := FRepair.DictBlocks[Inx];
    tvMain.Items.AddChildObject(DictRootNode,
                                Format(csBlock,
                                       [DictBlock.BlockNum]),
                                Pointer(DictBlock));
    FViewedBlocks.Add(DictBlock);
  end;


  { Create a node for the index header. }
  IndexHeaderBlock := FRepair.GetIndexHeaderBlock;
  tvMain.Items.AddChildObject(RootNode, csIndexHeader,
                              Pointer(IndexHeaderBlock));
  FViewedBlocks.Add(IndexHeaderBlock);

  { Create nodes for viewed data, index, & other blocks. }
  FDataBlocksNode := tvMain.Items.AddChild(RootNode, csDataBlocks);
  FIndexBlocksNode := tvMain.Items.AddChild(RootNode, csIndexBlocks);
  FOtherBlocksNode := tvMain.Items.AddChild(RootNode, csOtherBlocks);

  { By default, select the file header node & display its information. }
  RootNode.Expand(True);
  PositionToNode(FileHeaderNode);
end;

procedure TfrmMain.mnuFileOpenClick(Sender: TObject);
begin
  if dlgOpen.Execute then begin
    FFileName := dlgOpen.FileName;
    ClearAll;
    FRepair := TffRepairEngine.Create;
    FRepair.Open(FFileName);
    LoadUI;
  end;
end;

procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  ClearRepair;
end;

procedure TfrmMain.mnuFileExitClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.tvMainClick(Sender: TObject);
var
  Node : TTreeNode;
begin
  Node := tvMain.Selected;
  if (Node <> nil) and (Node <> FCurNode) then begin
    { Set up the list view columns. Raw data will be displayed when the user
      views that page. }
    ClearRawData;
    if Node.Data <> nil then begin
      DisplayProps(ICommonBlock(Node.Data));
      DisplayData(ICommonBlock(Node.Data));
      if Notebook.ActivePage = pgRawData then
        DisplayRawData(ICommonBlock(Node.Data))
      else if (FState = rmIdle) and (NoteBook.ActivePage = pgStatus) then
        { If state is idle (i.e., we did not just finish repairing) &
          on the status page then switch to the props page. }
        NoteBook.ActivePage := pgProps;
    end
    else begin
      ClearProps;
      ClearData;
    end;
    FCurNode := Node;
  end
  else if (Node <> nil) and (FState = rmIdle) and
          (Notebook.ActivePage = pgStatus) then
    { If user clicked on the current node & the status page is displayed then
      flip over to the properties page. }
    NoteBook.ActivePage := pgProps;
end;

procedure TfrmMain.mnuFileCloseClick(Sender: TObject);
begin
  if FRepair <> nil then
    ClearAll;
end;

procedure TfrmMain.tvMainGetSelectedIndex(Sender: TObject;
  Node: TTreeNode);
begin
  tvMainClick(Sender);
end;

procedure TfrmMain.OnComplete(Sender : TObject);
var
  Action, HighestAction : TffRepairAction;
  Inx : Integer;
  SelfRepairing : Boolean;
  AbortMsg,
  Recommendation,
  StatusMsg,
  RepairedErrSummary,
  Summary : string;
begin
  progressBar.Position := 0;
  Status(csStatusSep, []);

  { Determine the highest repair action. }
  SelfRepairing := False;
  HighestAction := raDecide;
  for Inx := 0 to Pred(FRepair.ErrorCount) do begin
    Action := rcAction[FRepair.ErrorCodes[Inx]];
    if Action = raSelfRepair then
      SelfRepairing := True;
    if Action > HighestAction then
      HighestAction := Action;
  end;  { for }

  if FState = rmVerify then begin
    lblStatus.Caption := 'Verification complete.';
    if FRepair.ErrorCount = 0 then
      StatusMsg := 'Verification complete. No errors were found.'
    else begin
      StatusMsg := Format('Verification complete. Found %s.',
                          [Singular(FRepair.ErrorCount, 'error', 'errors')]);
      if FRepair.Aborted then
        AbortMsg := 'The error limit was reached. There may be additional errors.';

      { Build a summary/recommended course of action. }
      case HighestAction of
        raSelfRepair :
          begin
            Summary := 'All errors can be successfully repaired without ' +
                       'packing the file.';
            Recommendation := 'Allow this utility to repair the file.';
          end;
        raDecide, raPack :
          begin
            if SelfRepairing then begin
              Summary := 'Some of the errors can be manually repaired ' +
                         'but other errors require the file to be packed.';
              Recommendation := 'Allow this utility to repair and restructure ' +
                                'the file.';
            end
            else begin
              Summary := 'The errors in the file require the file to be ' +
                         'packed.';
              Recommendation := 'Allow this utility to pack the file.';
            end;
          end;
        raUnsalvageable :
          begin
            Summary := 'The file and its data cannot be salvaged.';
            Recommendation := 'Restore this file from the last known good backup.';
          end;
      end;  { case }

      if FRepair.Aborted then
        StatusMsg := StatusMsg + #13#10#13#10 + AbortMsg;

      StatusMsg := StatusMsg + #13#10#13#10 + Summary + #13#10#13#10 +
                   Recommendation;
    end;  { if }
  end
  else begin
    lblStatus.Caption := 'Repair complete.';
    if FRepair.ErrorCount = 0 then
      StatusMsg := 'Repair complete. No errors were found.'
    else begin
      { Generate a summary count for found & repaired errors. }
      RepairedErrSummary := Format('Found %s and repaired %s.',
                                   [Singular(FRepair.ErrorCount, 'error', 'errors'),
                                    Singular(FRepair.FixCount, 'error', 'errors')]);

      { Did a pack or reindex fail? }
      if HighestAction = raUnsalvageable then
        StatusMsg := 'Repair did not complete successfully. ' +
                     RepairedErrSummary
      else begin
        { No, the repair was entirely successful. Indicate if table was packed
          or reindex. }
        if HighestAction = raPack then
          RepairedErrSummary := RepairedErrSummary +
                                ' The table was packed.';

        StatusMsg := 'Repair complete. ' + RepairedErrSummary;
      end;  { if..else }
    end;  { if..else }
  end;
  Status(StatusMsg, []);
  Status(csStatusSep, []);
  ShowMessage(StatusMsg);
end;

procedure TfrmMain.OnProgress(Repairer : TffRepairEngine;
                              State : TffRepairState;
                              Item : TffRepairItem;
                        const ActionStr : string;
                        const Position, Maximum : Integer);
begin
  ProgressBar.Min := 1;
  ProgressBar.Max := Maximum;
  ProgressBar.Position := Position;
  lblStatus.Caption := ActionStr;
  if Item <> FLastItem then begin
    Status(ActionStr, []);
    FLastItem := Item;
  end;
  Application.ProcessMessages;
end;

procedure TfrmMain.OnReportError(Block : ICommonBlock;
                           const ErrCode : Integer;
                           const ErrorStr : string);
begin
  if Block = nil then
    Status('Error %d: %s', [ErrCode, ErrorStr])
  else
    Status('Block %d (%d): %s', [Block.BlockNum, ErrCode, ErrorStr]);
end;

procedure TfrmMain.OnReportFix(Block : ICommonBlock;
                           const ErrCode : Integer;
                           const RepairStr : string);
begin
  if Block = nil then
    Status('..Fix, code %d: %s', [ErrCode, RepairStr])
  else
    Status('..Block %d (%d): %s', [Block.BlockNum, ErrCode, RepairStr]);
end;

procedure TfrmMain.Status(const Msg : string; args : array of const);
begin
  memStatus.Lines.Add(Format(Msg, args));
  Application.ProcessMessages;
end;

procedure TfrmMain.mnuFileVerifyClick(Sender: TObject);
begin
  if FState = rmIdle then begin
    FState := rmVerify;
    try
      VerifyRepair;
    finally
      Application.ProcessMessages;
      FState := rmIdle;
    end;
  end
  else
    ShowMessage('Verify can be performed only when this utility is Idle.');
end;

procedure TfrmMain.SetCtrlStates;
var
  Opened : Boolean;
begin
  Opened := (FRepair <> nil);
  mnuFileClose.Enabled := Opened;
  mnuFileVerify.Enabled := Opened;
  mnuFileRepair.Enabled := Opened;
  mnuChainViewData.Enabled := Opened;
  mnuChainViewFree.Enabled := Opened;
  mnuFileViewBlock.Enabled := Opened;
end;

procedure TfrmMain.mnuFileRepairClick(Sender: TObject);
begin
  if FState = rmIdle then begin
    FState := rmRepair;
    try
      ReleaseBlocksAndNodes;
      ClearUI;
      Application.ProcessMessages;
      VerifyRepair;
    finally
      LoadUI;
      Application.ProcessMessages;
      FState := rmIdle;
    end;
  end
  else
    ShowMessage('Repair can be performed only when this utility is Idle.');
end;

procedure TfrmMain.VerifyRepair;
var
  SavCursor : TCursor;
begin
  if FRepair <> nil then begin
    Notebook.ActivePage := pgStatus;
    SavCursor := Screen.Cursor;
    Screen.Cursor := crHourGlass;
    try
      ClearStatus;
      FRepair.OnComplete := OnComplete;
      FRepair.OnProgress := OnProgress;
      FRepair.OnReportError := OnReportError;
      FRepair.OnReportFix := OnReportFix;
      if FState = rmVerify then
        FRepair.Verify
      else begin
        FRepair.OutputVersion := FOutputVersion;
        FRepair.Repair;
      end;
    finally
      Screen.Cursor := SavCursor;
    end;
  end;  { if }
end;

procedure TfrmMain.NotebookChange(Sender: TObject);
var
  Node : TTreeNode;
begin
  if (Notebook.ActivePage = pgRawData) and (lvRawData.Items.Count = 0) then begin
    Node := tvMain.Selected;
    if (Node <> nil) and (Node.Data <> nil) then
      DisplayRawData(ICommonBlock(Node.Data));
  end;
end;

procedure TfrmMain.mnuFileViewBlockClick(Sender: TObject);
var
  BlockNumber : TffWord32;
  Block : ICommonBlock;
  Inx : Integer;
  Node : TTreeNode;
begin
  { Have the user enter the block number. }
  if Assigned(FFileHeaderBlock) then
    with TfrmBlockNum.Create(nil) do
      try
        MaxBlockNum := Pred(FFileHeaderBlock.UsedBlocks);
        ShowModal;
        BlockNumber := BlockNum;
        { If a block number was specified, see if it is the same as
          an existing node or if a new node must be added. }
        if BlockNumber <> ffc_W32NoValue then begin
  (*        { TODO:: If this is a preloaded block then go to the appropriate tree node. }
          if BlockNumber = xxx then
          else if BlockNumber = xxx then
          else if BlockNumber = xxx then
          else if BlockNumber = xxx then
          else if BlockNumber = xxx then*)

          { Determine if this is already available via an existing node in the
            tree. }
          Inx := FBlockNumToNodeMap.IndexOf(IntToStr(BlockNumber));
          if Inx > -1 then begin

          end
          else begin
            { The block has not been viewed. Load the block & put it into the
              tree view. }
            Block := FRepair.GetBlock(BlockNumber);
            FViewedBlocks.Add(Block);
            if Block.Signature = ffc_SigDataBlock then begin
              { Add this under the data blocks node. }
              Node := tvMain.Items.AddChildObject(FDataBlocksNode,
                                                  Format(csBlock,
                                                         [Block.BlockNum]),
                                                  Pointer(Block));
            end
            else if Block.Signature = ffc_SigIndexBlock then begin
              { Add this under the index blocks node. }
              Node := tvMain.Items.AddChildObject(FIndexBlocksNode,
                                                  Format(csBlock,
                                                         [Block.BlockNum]),
                                                  Pointer(Block));
            end
            else begin
              { Add this under the other blocks node. }
              Node := tvMain.Items.AddChildObject(FOtherBlocksNode,
                                                  Format(csBlock,
                                                         [Block.BlockNum]),
                                                  Pointer(Block));
            end;  { if..else }
            { Add this block to the blocknumber-to-node map. }
            FBlockNumToNodeMap.AddObject(IntToStr(BlockNumber), Node);

            { Position the tree view to the node. }
            PositionToNode(Node);
          end;
        end;
      finally
        Free;
      end;
end;

procedure TfrmMain.PositionToNode(Node : TTreeNode);
begin
  tvMain.Selected := Node;
{$IFDEF DCC6OrLater}
  tvMain.Select(Node);
{$ELSE}
  tvMain.Selected := Node;
{$ENDIF}
  Node.Focused := True;
  Node.Selected := True;
  FCurNode := Node;
  SetCtrlStates;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FBlockNumToNodeMap := TStringList.Create;
  with TffVerifyOptions.Create do
    try
      FOutputVersion := OutputVersion;
    finally
      Free;
    end;
  FViewedBlocks := TInterfaceList.Create;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FViewedBlocks.Free;
  FBlockNumToNodeMap.Free;
end;

procedure TfrmMain.mnuChainViewDataClick(Sender: TObject);
var
  SavCursor : TCursor;
begin
  if FRepair <> nil then begin
    Notebook.ActivePage := pgStatus;
    SavCursor := Screen.Cursor;
    Screen.Cursor := crHourGlass;
    FState := rmAcquireInfo;
    try
      ClearStatus;
      memStatus.Text := FRepair.GetDataChainDetails.Text;
    finally
      FState := rmIdle;
      Screen.Cursor := SavCursor;
    end;
  end;
end;

procedure TfrmMain.mnuChainViewFreeClick(Sender: TObject);
var
  SavCursor : TCursor;
begin
  if FRepair <> nil then begin
    Notebook.ActivePage := pgStatus;
    SavCursor := Screen.Cursor;
    Screen.Cursor := crHourGlass;
    FState := rmAcquireInfo;
    try
      ClearStatus;
      memStatus.Text := FRepair.GetFreeChainDetails.Text;
    finally
      FState := rmIdle;
      Screen.Cursor := SavCursor;
    end;
  end;
end;

procedure TfrmMain.mnuOptionsClick(Sender: TObject);
var
  Options : TfrmOptionsConfig;
begin
  Options := TfrmOptionsConfig.Create(nil);
  try
    Options.ShowModal;
    if Options.ModalResult = mrOK then
      FOutputVersion := Options.OutputVersion;
  finally
    Options.Free;
  end;
end;

end.

