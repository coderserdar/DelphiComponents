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
 * The Original Code is TurboPower XMLPartner
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1997-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

{*********************************************************}
{* XMLPartner: ExChildW.PAS 2.57                         *}
{*********************************************************}
{* XMLPartner: XML Editor Child Window                   *}
{*********************************************************}
{$I XpDefine.inc}
unit ExChildW;

interface

uses
{$IFDEF WIN32}
  Windows,
  Graphics,
  Forms,
  Controls,
  StdCtrls,
  ExtCtrls,
  ToolWin,
  Messages,
  Menus,
  ComCtrls,
{$ENDIF}
{$IFDEF LINUX}
  Types,
  Qt,
  QGraphics,
  QForms,
  QControls,
  QStdCtrls,
  QExtCtrls,
  QMenus,
  QComCtrls,
  QImgList,
  QDialogs,
  QTypes,
  QClipbrd,
{$ENDIF}
{$IFDEF XPDPRO}
  ExView,
{$ENDIF}
{$IFDEF Delphi4orLater}
  ImgList,
{$ENDIF}
  Classes,
  XpDOM,
  ExProps,
  XpBase,
  INIFiles;


type
  TXpMDIChildCount = function(Sender : TObject) : Integer of object;
  TXpGetXSLDocElement = function(Sender : TObject) : TXpElement of object;

  TXmlChild = class(TForm)
    ToolBar1: TToolBar;
    btnInsert: TToolButton;
    btnDuplicate: TToolButton;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    btnPromote: TToolButton;
    btnDemote: TToolButton;
    btnMoveUp: TToolButton;
    btnMoveDown: TToolButton;
    btnCollapseAll: TToolButton;
    btnExpandAll: TToolButton;
    ToolButton3: TToolButton;
    PageControl: TPageControl;
    StructTab: TTabSheet;
    SourceTab: TTabSheet;
    Splitter1: TSplitter;
    Panel2: TPanel;
    Splitter2: TSplitter;
    HeaderControl1: THeaderControl;
    TreeView: TTreeView;
    Panel1: TPanel;
    Panel3: TPanel;
    ListView: TListView;
    NodeImageList: TImageList;
    AttrPopupMenu: TPopupMenu;
    mnuAddAttr: TMenuItem;
    mnuRemoveAttr: TMenuItem;
    mnuEditAttr: TMenuItem;
    InsertPopupMenu: TPopupMenu;
    mnuInsertElement: TMenuItem;
    mnuInsertText: TMenuItem;
    mnuAddTextAsChild: TMenuItem;
    mnuInsertComment: TMenuItem;
    mnuInsertPI: TMenuItem;
    mnuInsertCDATA: TMenuItem;
    mnuAddCDATA: TMenuItem;
    mnuAddElementasChild: TMenuItem;
    TreePopupMenu: TPopupMenu;
    mnuDeleteNode: TMenuItem;
    btnNormalize: TToolButton;
    btnAddChildElement: TToolButton;
    btnAddChildText: TToolButton;
    StatusBar: TStatusBar;
    ToolbarImageList: TImageList;
    mnuPreserveSpace: TMenuItem;
    mnuSelectXSLAttr: TMenuItem;
    EditView: TMemo;
    mnuAddGUID: TMenuItem;
    DOM: TXpObjModel;
    mnuCopy: TMenuItem;
    mnuCut: TMenuItem;
    mnuPaste: TMenuItem;
    mnuTreeSep1: TMenuItem;
    Splitter3: TSplitter;
    lvValidate: TListView;
    btnValidate: TToolButton;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ListViewData(Sender: TObject; Item: TListItem);
    procedure TreeViewClick(Sender: TObject);
    procedure TreeViewCustomDraw(Sender: TCustomTreeView;
      const ARect: TRect; var DefaultDraw: Boolean);
    procedure TreeViewChange(Sender: TObject; Node: TTreeNode);
    procedure AttrListViewData(Sender: TObject; Item: TListItem);
    procedure FormCreate(Sender: TObject);
    procedure PageControlChange(Sender: TObject);
    procedure btnCollapseAllClick(Sender: TObject);
    procedure btnExpandAllClick(Sender: TObject);
    procedure TreeViewGetImageIndex(Sender: TObject; Node: TTreeNode);
    procedure TreeViewGetSelectedIndex(Sender: TObject; Node: TTreeNode);
    procedure PropertiesWindow1PropertyName(oOwner: TObject;
      wIndex: Integer; var sData: string);
    procedure PropertiesWindow1PropertyValue(oOwner: TObject;
      wIndex: Integer; var sData: string);
    procedure PropertiesWindow1ValueQueryEdit(oOwner: TObject;
      wIndex: Integer; var oEditType: EXpEditType;
      var sFunctionName: string);
    procedure AttrPropertiesPropertyName(oOwner: TObject; wIndex: Integer;
      var sData: string);
    procedure AttrPropertiesPropertyValue(oOwner: TObject; wIndex: Integer;
      var sData: string);
    procedure AttrPropertiesValueQueryEdit(oOwner: TObject;
      wIndex: Integer; var oEditType: EXpEditType;
      var sFunctionName: string);
    procedure AttrPropertiesValueChange(oOwner: TObject; wIndex: Integer;
      sValue: string);
    procedure mnuRemoveAttrClick(Sender: TObject);
    procedure mnuAddAttrClick(Sender: TObject);
    procedure mnuEditAttrClick(Sender: TObject);
    procedure TreeViewEdited(Sender: TObject; Node: TTreeNode;
      var S: string);
    procedure mnuInsertElementClick(Sender: TObject);
    procedure mnuAddElementasChildClick(Sender: TObject);
    procedure mnuInsertTextClick(Sender: TObject);
    procedure mnuAddTextAsChildClick(Sender: TObject);
    procedure mnuInsertCDATAClick(Sender: TObject);
    procedure mnuAddCDATAClick(Sender: TObject);
    procedure mnuInsertCommentClick(Sender: TObject);
    procedure mnuInsertPIClick(Sender: TObject);
    procedure mnuDeleteNodeClick(Sender: TObject);
    procedure btnDuplicateClick(Sender: TObject);
    procedure btnPromoteClick(Sender: TObject);
    procedure btnDemoteClick(Sender: TObject);
    procedure btnMoveUpClick(Sender: TObject);
    procedure btnMoveDownClick(Sender: TObject);
    procedure ListViewEdited(Sender: TObject; Item: TListItem;
      var S: string);
    procedure ListViewDblClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure btnNormalizeClick(Sender: TObject);
    procedure PageControlChanging(Sender: TObject;
      var AllowChange: Boolean);
    procedure TreeViewKeyPress(Sender: TObject; var Key: Char);
    procedure ListViewKeyPress(Sender: TObject; var Key: Char);
    procedure ListViewEditing(Sender: TObject; Item: TListItem;
      var AllowEdit: Boolean);
    procedure TreeViewEditing(Sender: TObject; Node: TTreeNode;
      var AllowEdit: Boolean);
    procedure FormShow(Sender: TObject);
    procedure mnuPreserveSpaceClick(Sender: TObject);
    procedure mnuAddGUIDClick(Sender: TObject);
    procedure TreeViewMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure AttrPropertiesClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure EditViewChange(Sender: TObject);
    procedure mnuCutClick(Sender: TObject);
    procedure mnuCopyClick(Sender: TObject);
    procedure mnuPasteClick(Sender: TObject);
    procedure btnValidateClick(Sender: TObject);
    procedure DOMInvalidDocument(oOwner: TObject; wCode: Integer;
      oNode: TXpNode; var bStop: Boolean);
    procedure lvValidateEditing(Sender: TObject; Item: TListItem;
      var AllowEdit: Boolean);
    procedure TreeViewCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure lvValidateSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
  protected
  private
    { Private declarations }
    AttrProperties : TXpPropertiesWindow;
    FAttrNode: TXpElement;
    FChanged: Boolean;
    FFileName: string;
    FINIFile : TINIFile;
    FLastIndex: Integer;
    FLastVErr : Integer;
    FLastItem: TTreeNode;
    FListViewWidth : Integer;
    FLoadingSrc : Boolean;
    FOnClosing : TNotifyEvent;
    FOnGetXSLDocElement : TXpGetXSLDocElement;
    FOnMDIChildCount : TXpMDIChildCount;
    FOnNextChild : TNotifyEvent;
    FOnPrevChild : TNotifyEvent;
    FOnSaveAs : TNotifyEvent;
    FOnSetCtrlStates : TNotifyEvent;
{$IFDEF XPDPRO}
    FPreviewForm : TfrmPreview;
    FPreviewTab : TTabSheet;
{$ENDIF}


    procedure EditNodeDialog(oNode: TXpNode);
    function GetTreeNodeForNode(oNode : TXpNode) : TTreeNode;
    function GetErrors : TstringList;
    function GetFormattedOutput : Boolean;
    function GetListViewText(oNode: TXpNode): string;
    function GetNormalize : Boolean;
    function GetTreeNodeImageIndex(oNode: TXpNode): Integer;
    function GetTreeNodeText(oNode: TXpNode): string;
    function GetXmlData: DOMString;
{$IFDEF XPDPRO}
    procedure IntegratePreviewPage;
{$ENDIF}
    function MapValidationCode(const wCode : Integer;
                                     oNode : TXpNode) : string;
    procedure ResetValidationNode(const wInx : Integer);
    procedure SetCtrlStates;
    procedure SetFormattedOutput(aValue : Boolean);
    procedure SetListViewText(oNode: TXpNode; sValue: string);
    procedure SetNormalize(aValue : Boolean);
    function SetTreeNodeText(oNode: TXpNode; var sValue: string): Boolean;
    procedure ShowValidate(const bShow : Boolean);
    {$IFDEF XPDPRO}
    procedure UpdatePreviewPage;
      { Updates the preview page with the latest object model & filename. }
    {$ENDIF}
    procedure UpdateTreeChildrenFromObjModel(oNode: TXpNode; oTreeNode: TTreeNode);

  public
    { Public declarations }
    function LoadDataSource(sFile: string; sUserId: string; sPassword: string): Boolean;
    procedure RefreshNode(Node: TTreeNode);
    function SaveFile(sFile: string = ''): Boolean;
    function SaveToURL(sURL, sUserId, sPassword: string): Boolean;
    procedure NewDocument;

{$IFDEF XPDPRO}
    procedure AddStylesheet(const sName : string; oChildWin : TForm);
      { Add a stylesheet to this window's list of stylesheets. }

    procedure RemoveStylesheet(const sName : string);
      { Remove a stylesheet from this window's list of stylesheets. }

    procedure RenameStylesheet(const sOldName, sNewName : string);
      { Rename a stylesheet in this window's list of stylesheets. }
{$ENDIF}

    procedure CopyToClipboard(bIncludeChildren: Boolean = true);
    procedure CutToClipboard;
    procedure PasteFromClipboard;
    procedure UpdateTreeFromObjModel(oNewSelNode: TXpNode = nil);
    procedure RefreshList;
    function CreateNewNode(sElemName: string): TXpElement;

    property Errors : TstringList read GetErrors;
    property FileName : string read FFileName;
    property FormattedOutput : Boolean read GetFormattedOutput
                                       write SetFormattedOutput;
    property INIFile : TINIFile read FINIFile write FINIFile;
    property IsChanged : Boolean read FChanged;
    property NormalizeData : Boolean read GetNormalize write SetNormalize;

    { Events }
    property OnClosing : TNotifyEvent
      read FOnClosing write FonClosing;
      { Raised when the child window is closing. Allows parent window to notify
        other child windows of the event. }
    property OnGetXSLDocElement : TXpGetXSLDocElement
      read FOnGetXSLDocElement write FOnGetXSLDocElement;
    property OnMDIChildCount : TXpMDIChildCount
      read FOnMDIChildCount write FOnMDIChildCount;
    property OnNextChild : TNotifyEvent
      read FOnNextChild write FOnNextChild;
    property OnPrevChild : TNotifyEvent
      read FOnPrevChild write FOnPrevChild;
    property OnSaveAs : TNotifyEvent
      read FOnSaveAs write FOnSaveAs;
    property OnSetCtrlStates : TNotifyEvent
      read FOnSetCtrlStates write FOnSetCtrlStates;
    property XMLData : DOMString read GetXMLData;
  end;

implementation

uses
{$IFDEF WIN32}
  Dialogs,
  ComObj,
  Clipbrd,
{$ENDIF}
{$IFDEF LINUX}
{$ENDIF}
  Sysutils, ExAttr, ExElemnt, ExText,
  ExProcIn, ExCommnt, XpParser, ExSelAtt,
  XpExcept;

{$R *.dfm}


procedure TXmlChild.FormCreate(Sender: TObject);
begin
  FAttrNode := nil;
  FLoadingSrc := False;
{$IFDEF XPDPRO}
  FPreviewForm := nil;
  FPreviewTab := nil;
{$ENDIF}
  FLastVErr := -1;
  AttrProperties := TXpPropertiesWindow.Create(Self);
  PageControl.ActivePage := StructTab;
end;

procedure TXmlChild.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
  if Assigned(FOnClosing) then
    FOnClosing(Sender);
end;

procedure TXmlChild.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  wRet: Integer;
begin
  CanClose := True;
  if FChanged then begin
    wRet := MessageDlg('File ' + Caption +
                       ' has changed.  Do you want to save the changes?',
                       mtConfirmation, [mbYes, mbNo, mbCancel], 0);
    case wRet of
      mrYes: begin
        if FFileName = '' then begin
          CanClose := False;
          if assigned(FOnSaveAs) then
            FOnSaveAs(Self);
        end
        else
          SaveFile;
      end;
      mrCancel: CanClose := False;
    end;
  end;
end;

function TXmlChild.GetErrors : TstringList;
begin
  Result := DOM.Errors;
end;

function TXmlChild.LoadDataSource(sFile: string; sUserId: string;
                                  sPassword: string): Boolean;
var
  oSavCursor : TCursor;
begin
  Result := False;
  if FileExists(sFile) then begin
    FFileName := LowerCase(sFile);
    Caption := FFileName;
  end
  else begin
    FFileName := '';
    Caption := sFile;
  end;

  try
    DOM.UserName := sUserId;
    DOM.Password := sPassword;
{$IFDEF WIN32}
    SetCapture(Handle);
{$ENDIF}
    oSavCursor := Screen.Cursor;
    Screen.Cursor := crHourglass;
    try
      Result := DOM.LoadDataSource(sFile);
{$IFDEF XPDPRO}
      IntegratePreviewPage;
{$ENDIF}
      TreeView.Items.Clear;
      UpdateTreeFromObjModel;
      GetTreeNodeForNode(DOM.Document.DocumentElement).Expand(False);
    finally
      Screen.Cursor := oSavCursor;
{$IFDEF WIN32}
      ReleaseCapture;
{$ENDIF}
    end;
  except
    on X: Exception do begin
      if MessageDlg('An error occurred while parsing the document: ' +
                    X.Message + #13#10 + #13#10 +
                    'Would you like to load the document into the ' +
                    'source code editor?',
                    mtError, [mbYes, mbNo], 0) = mrYes then begin
        PageControl.ActivePage := SourceTab;
        EditView.Lines.LoadFromFile(sFile);
        if X is EXpParserError then
          with X as EXpParserError do begin
            StatusBar.SimpleText := 'Error on line: ' + IntToStr(Line) +
                                    ' position: ' + IntToStr(LinePos) +
                                    ' abs at: ' + IntToStr(FilePos) +
                                    ' error msg: ' + Reason;
            EditView.SelStart := FilePos;
          end;
        EditView.Modified := True;
        EditView.SetFocus;
        Result := True;
      end
      else
        Close;
    end;
  end;
end;

function TXmlChild.SaveFile(sFile: string): Boolean;
var
  sData: string;
begin
  { Backup file }
  if sFile = '' then
    sFile := FFileName;

{$IFDEF WIN32}
  SetCapture(Handle);
{$ENDIF}
  Screen.Cursor := crHourglass;
  if FileExists(sFile) then begin
    sData := sFile;
    Delete(sData, Length(sData) - 2, 3);
    sData := sData + '~ml';
    DeleteFile(sData);
    RenameFile(sFile, sData);
  end;

  Result := False;

  { Are we on the source view? }
  if PageControl.ActivePage = SourceTab then
    { Yes. Save the contents of the source view to the file. }
    try
      EditView.Lines.SaveToFile(sFile);
      Result := True;
    except
      on E:Exception do
        ShowMessage(E.Message);
    end
  else begin
    { No. Save the object model to the file. }
    Result := DOM.SaveToFile(sFile);
  end;

  if Result then begin
    FFileName := LowerCase(sFile);
    Caption := FFileName;
    FChanged := False;
  end;
  Screen.Cursor := crDefault;
{$IFDEF WIN32}
  ReleaseCapture;
{$ENDIF}
  SetCtrlStates;
end;

function TXmlChild.SaveToURL(sURL, sUserId, sPassword: string): Boolean;
var
  savCursor : TCursor;
begin
  DOM.UserName := sUserId;
  DOM.Password := sPassword;
{$IFDEF WIN32}
  SetCapture(Handle);
{$ENDIF}
  savCursor := Screen.Cursor;
  try
    Screen.Cursor := crHourglass;
    Result := DOM.SaveToFile(sURL);
    FChanged := not Result;
  finally
    Screen.Cursor := savCursor;
    SetCtrlStates;
{$IFDEF WIN32}
    ReleaseCapture;
{$ENDIF}
  end;
end;

procedure TXmlChild.NewDocument;
var
  aBuffer : string;
begin
  aBuffer := '<?xml version="1.0"?><root/>';
  DOM.LoadMemory(aBuffer[1], Length(aBuffer));
{$IFDEF XPDPRO}
  IntegratePreviewPage;
{$ENDIF}

  FFileName := '';
  UpdateTreeFromObjModel;
end;

function TXmlChild.GetTreeNodeText(oNode: TXpNode): string;
begin
  Result := '[UNKNOWN]';
  case oNode.NodeType of
    TEXT_NODE: Result := '[TEXT]';
    CDATA_SECTION_NODE: Result := '[CDATA]';
    COMMENT_NODE: Result := '[COMMENT]';
  else
    Result := oNode.NodeName;
  end;
{$IFDEF NodeMemoryUsageEnabled}
  Result := Result + ' (' + IntToStr(oNode.MemoryUsed) + ')';
{$ENDIF}
end;

function TXmlChild.SetTreeNodeText(oNode: TXpNode; var sValue: string): Boolean;
begin
  Result := False;
  case oNode.NodeType of
    TEXT_NODE: sValue := '[TEXT]';
    CDATA_SECTION_NODE: sValue := '[CDATA]';
    COMMENT_NODE: sValue := '[COMMENT]';
    else begin
      if sValue = '' then
        sValue := oNode.NodeName
      else begin
        oNode.NodeName := sValue;
        Result := True;
      end;
    end;
  end;  { case }
end;

function TXmlChild.GetTreeNodeImageIndex(oNode: TXpNode): Integer;
begin
  case oNode.NodeType of
    TEXT_NODE: Result := 2;
    CDATA_SECTION_NODE: Result := 3;
    PROCESSING_INSTRUCTION_NODE: Result := 4;
    COMMENT_NODE: Result := 5;
  else
    if oNode.HasAttributes then
      Result := 1
    else
      Result := 0;
  end;  { case }
end;

function TXmlChild.GetListViewText(oNode: TXpNode): string;
var
  i: Integer;
  oAttr: TXpAttribute;
begin
  Result := '';
  case oNode.NodeType of
    PROCESSING_INSTRUCTION_NODE, COMMENT_NODE,
    TEXT_NODE, CDATA_SECTION_NODE: Result := oNode.NodeValue;
    ELEMENT_NODE: begin
      if oNode.HasAttributes then begin
        for i := 0 to oNode.Attributes.Length - 1 do begin
          oAttr := TXpAttribute(oNode.Attributes.Item(i));
          Result := Result + '(' + oAttr.NodeName + '="' + oAttr.NodeValue + '") ';
        end;
      end;
    end;
  end;
end;

procedure TXmlChild.SetListViewText(oNode: TXpNode; sValue: string);
begin
  case oNode.NodeType of
    PROCESSING_INSTRUCTION_NODE, COMMENT_NODE,
    TEXT_NODE, CDATA_SECTION_NODE: begin
      oNode.NodeValue := sValue;
      FChanged := True;
    end;
  end;
end;

procedure TXmlChild.RefreshList;
{$IFDEF LINUX}
var
  i : integer;
  Item : TTreeNode;
{$ENDIF}
begin
{$IFDEF WIN32}
  if ListView.Items.Count <> ListView.VisibleRowCount then
    ListView.Items.Count := ListView.VisibleRowCount;
  InvalidateRect(ListView.Handle, nil, false);
{$ENDIF}
{$IFDEF LINUX}
  ListView.Items.Clear;
  ListView.Sorted := false;
  while ListView.Items.Count <= TreeView.Items.Count do
    ListView.Items.Add;
  i := 0;
  Item := TreeView.TopItem;
  while true do begin
    ListViewData(Self, ListView.Items[i]);
    Item := Item.GetNextVisible;
    if( Item = nil )then break;
    inc( i );
  end;
  QWidget_repaint(ListView.Handle);
{$ENDIF}
end;

procedure TXmlChild.UpdateTreeChildrenFromObjModel(oNode: TXpNode;
                                                   oTreeNode: TTreeNode);
var
  aCount : Integer;
  i: Integer;
  oChild: TXpNode;
  oRefNode: TTreeNode;
begin
  aCount := oTreeNode.Count;
  for i := 0 to oNode.ChildNodes.Length - 1 do begin
    oChild := oNode.ChildNodes.Item(i);
    if i >= aCount then
      oRefNode := TreeView.Items.AddChildObject(oTreeNode,
                                                GetTreeNodeText(oChild), oChild)
    else if oTreeNode.Item[i].Data = oChild then begin
      oRefNode := oTreeNode.Item[i];
      oRefNode.Text := GetTreeNodeText(oChild);
    end
    else
{$IFDEF WIN32}
      oRefNode := TreeView.Items.InsertObject(oTreeNode.Item[i],
                                              GetTreeNodeText(oChild), oChild);
{$ENDIF}
{$IFDEF LINUX}
      oRefNode := TreeView.Items.Insert(oTreeNode.GetNextSibling,
                                        GetTreeNodeText(oChild));
      oRefNode.Data := oChild;
{$ENDIF}
    oChild.Tag := Longint(oRefNode);
    if oChild.HasChildNodes then
      UpdateTreeChildrenFromObjModel(oChild, oRefNode)
    else if oRefNode.Count > 0 then
      oRefNode.DeleteChildren;
  end;

  while oTreeNode.Count > oNode.ChildNodes.Length do
    oTreeNode.Item[oTreeNode.Count-1].Delete;
end;

procedure TXmlChild.UpdateTreeFromObjModel(oNewSelNode: TXpNode);
var
  aCount : Integer;
  oNode, oSelNode, oChild: TXpNode;
  oTreeNode: TTreeNode;
  oRefNode: TTreeNode;
  i, j: Integer;
  bCallEnd: Boolean;
begin
  { Get selected node }
  oSelNode := oNewSelNode;
  if (oSelNode = nil) and (TreeView.Selected <> nil) then
    oSelNode := TreeView.Selected.Data;
  oNode := DOM.Document;
  oTreeNode := TreeView.Items.GetFirstNode;
  bCallEnd := False;
  if oTreeNode = nil then begin
    TreeView.Items.BeginUpdate;
    bCallEnd := True;
  end;
  for i := 0 to oNode.ChildNodes.Length - 1 do begin
    oChild := oNode.ChildNodes.Item(i);
    if oTreeNode = nil then
      oRefNode := TreeView.Items.AddObject(nil, GetTreeNodeText(oChild), oChild)
    else if oTreeNode.Data = oChild then begin
      oRefNode := oTreeNode;
      oRefNode.Text := GetTreeNodeText(oChild);
      oTreeNode := oTreeNode.GetNextSibling;
    end
    else
{$IFDEF WIN32}
      oRefNode := TreeView.Items.InsertObject(oTreeNode, GetTreeNodeText(oChild), oChild);
{$ENDIF}
{$IFDEF LINUX}
      oRefNode := TreeView.Items.Insert( oTreeNode.GetNextSibling,
                                                       GetTreeNodeText(oChild));
      oRefNode.Data := oChild;
{$ENDIF}
    if oChild.HasChildNodes then
      UpdateTreeChildrenFromObjModel(oChild, oRefNode)
    else if oRefNode.Count > 0 then
      oRefNode.DeleteChildren;
    oChild.Tag := Longint(oRefNode);
  end;
  if bCallEnd then
    TreeView.Items.EndUpdate;
  if oTreeNode <> nil then begin
    j := oTreeNode.AbsoluteIndex;
    aCount := pred(TreeView.Items.Count);
    for i := aCount downto j do
      TreeView.Items[i].Delete;
  end;
  { Set selected node }
  if oSelNode <> nil then begin
    j := pred(TreeView.Items.Count);
    for i := 0 to j do begin
      if TreeView.Items[i].Data = oSelNode then begin
        TreeView.Selected := TreeView.Items[i];
        break;
      end;
    end;
  end;
  RefreshList;
end;

procedure TXmlChild.ListViewData(Sender: TObject; Item: TListItem);
var
  i: Integer;
  oItem: TTreeNode;
begin
  if TreeView.TopItem = nil then
    exit;
  if Item.Index = 0 then begin
    Item.Caption := GetListViewText(TreeView.TopItem.Data);
    FLastIndex := 0;
    FLastItem := TreeView.TopItem;
  end
  else if (Item.Index = FLastIndex + 1) and (FLastItem <> nil) then begin
    FLastIndex := Item.Index;
    FLastItem := FLastItem.GetNextVisible;
    if FLastItem <> nil then
      Item.Caption := GetListViewText(FLastItem.Data);
  end
  else begin
    i := Item.Index;
    oItem := TreeView.TopItem;
    while (i > 0) and (oItem <> nil) do begin
      oItem := oItem.GetNextVisible;
      Dec(i);
    end;
    if oItem <> nil then
      Item.Caption := GetListViewText(oItem.Data);
  end;
end;

procedure TXmlChild.TreeViewClick(Sender: TObject);
begin
  RefreshList;
  SetCtrlStates;
end;

procedure TXmlChild.TreeViewCustomDraw(Sender: TCustomTreeView;
  const ARect: TRect; var DefaultDraw: Boolean);
begin
  RefreshList;
end;

procedure TXmlChild.TreeViewChange(Sender: TObject; Node: TTreeNode);
begin
  FAttrNode := nil;
  if TreeView.Selected <> nil then begin
    if TXpNode(TreeView.Selected.Data).NodeType = ELEMENT_NODE then begin
      FAttrNode := TreeView.Selected.Data;
      if FAttrNode.HasAttributes then
        AttrProperties.RowCount := FAttrNode.Attributes.Length
      else
        AttrProperties.RowCount := 0;
    end
    else
      AttrProperties.RowCount := 0;
  end;
  SetCtrlStates;
end;

procedure TXmlChild.AttrListViewData(Sender: TObject; Item: TListItem);
var
  i: Integer;
begin
  i := Item.Index;
  if (FAttrNode <> nil) and (FAttrNode.HasAttributes) and
     (i < FAttrNode.Attributes.Length) then begin
    Item.Caption := FAttrNode.Attributes.Item(i).NodeName;
    Item.SubItems.Add(FAttrNode.Attributes.Item(i).NodeValue);
  end;
end;

procedure TXmlChild.PageControlChange(Sender: TObject);
var
  oSavCursor : TCursor;
  sTmp: string;
begin
  if PageControl.ActivePage = SourceTab then begin
    FLoadingSrc := True;
    try
      EditView.Text := '';
      StatusBar.SimpleText := '';
{$IFDEF WIN32}
      SetCapture(Handle);
{$ENDIF}
      oSavCursor := Screen.Cursor;
      Screen.Cursor := crHourglass;
      try
        sTmp := DOM.XmlDocument;
        EditView.Text := sTmp;
{$IFDEF WIN32}
      if SendMessage(EditView.Handle, EM_GETLIMITTEXT, 0, 0) <=
            (Length(sTmp) + 32768) then
        SendMessage(EditView.Handle, EM_SETLIMITTEXT, Length(sTmp) + 32736, 0);
{$ENDIF}
      finally
        Screen.Cursor := oSavCursor;
{$IFDEF WIN32}
        ReleaseCapture;
{$ENDIF}
      end;
      EditView.Modified := False;
      EditView.SetFocus;
    finally
      FLoadingSrc := False;
    end;
  end;
  SetCtrlStates;
end;

procedure TXmlChild.btnCollapseAllClick(Sender: TObject);
var
  savCursor : TCursor;
begin
{$IFDEF WIN32}
  SetCapture(Handle);
{$ENDIF}
  savCursor := Screen.Cursor;
  Screen.Cursor := crHourglass;
  TreeView.FullCollapse;
  Screen.Cursor := savCursor;
{$IFDEF WIN32}
  ReleaseCapture;
{$ENDIF}
end;

procedure TXmlChild.btnExpandAllClick(Sender: TObject);
var
  savCursor : TCursor;
  oNode : TTreeNode;
begin
{$IFDEF WIN32}
  SetCapture(Handle);
{$ENDIF}
  savCursor := Screen.Cursor;
  Screen.Cursor := crHourglass;
  oNode := TreeView.TopItem;
  TreeView.FullExpand;
  TreeView.TopItem := oNode;
  Screen.Cursor := savCursor;
{$IFDEF WIN32}
  ReleaseCapture;
{$ENDIF}
end;

procedure TXmlChild.TreeViewGetImageIndex(Sender: TObject;
  Node: TTreeNode);
begin
  if Node.Data <> nil then
    if Node.ImageIndex <= 5 then
      Node.ImageIndex := GetTreeNodeImageIndex(Node.Data);
end;

procedure TXmlChild.TreeViewGetSelectedIndex(Sender: TObject;
  Node: TTreeNode);
begin
  if Node.Data <> nil then
    if Node.SelectedIndex <= 5 then
      Node.SelectedIndex := GetTreeNodeImageIndex(Node.Data);
end;

procedure TXmlChild.PropertiesWindow1PropertyName(oOwner: TObject;
  wIndex: Integer; var sData: string);
begin
  if (FAttrNode <> nil) and (FAttrNode.HasAttributes) and
     (wIndex < FAttrNode.Attributes.Length) then begin
    sData := FAttrNode.Attributes.Item(wIndex).NodeName;
  end;
end;

procedure TXmlChild.PropertiesWindow1PropertyValue(oOwner: TObject;
  wIndex: Integer; var sData: string);
begin
  if (FAttrNode <> nil) and (FAttrNode.HasAttributes) and
     (wIndex < FAttrNode.Attributes.Length) then begin
    sData := FAttrNode.Attributes.Item(wIndex).NodeValue;
  end;
end;

procedure TXmlChild.PropertiesWindow1ValueQueryEdit(oOwner: TObject;
  wIndex: Integer; var oEditType: EXpEditType; var sFunctionName: string);
begin
  oEditType := etEdit;
end;

procedure TXmlChild.AttrPropertiesPropertyName(oOwner: TObject;
  wIndex: Integer; var sData: string);
begin
  if (FAttrNode <> nil) and (FAttrNode.HasAttributes) and
     (wIndex < FAttrNode.Attributes.Length) then
    sData := FAttrNode.Attributes.Item(wIndex).NodeName;
end;

procedure TXmlChild.AttrPropertiesPropertyValue(oOwner: TObject;
  wIndex: Integer; var sData: string);
begin
  if (FAttrNode <> nil) and (FAttrNode.HasAttributes) and
     (wIndex < FAttrNode.Attributes.Length) then
    sData := FAttrNode.Attributes.Item(wIndex).NodeValue;
end;

procedure TXmlChild.AttrPropertiesValueQueryEdit(oOwner: TObject;
  wIndex: Integer; var oEditType: EXpEditType; var sFunctionName: string);
begin
  oEditType := etEdit;
end;

procedure TXmlChild.AttrPropertiesValueChange(oOwner: TObject;
  wIndex: Integer; sValue: string);
begin
  if (FAttrNode <> nil) and (FAttrNode.HasAttributes) and
     (wIndex < FAttrNode.Attributes.Length) then begin
    if sValue <> FAttrNode.Attributes.Item(wIndex).NodeValue then begin
      FAttrNode.Attributes.Item(wIndex).NodeValue := sValue;
{$IFDEF WIN32}
      InvalidateRect(ListView.Handle, nil, false);
{$ENDIF}
{$IFDEF LINUX}
      QWidget_repaint(ListView.Handle);
{$ENDIF}
      FChanged := True;
    end;
  end;
  SetCtrlStates;
end;

procedure TXmlChild.mnuRemoveAttrClick(Sender: TObject);
var
  oAttr: TXpAttribute;
begin
  if (FAttrNode <> nil) and (FAttrNode.HasAttributes) and
     (AttrProperties.Selected <> -1) and
     (AttrProperties.Selected < FAttrNode.Attributes.Length) then begin
    oAttr := FAttrNode.RemoveAttributeNode(TXpAttribute(FAttrNode.Attributes.Item(AttrProperties.Selected)));
    oAttr.Release;
    AttrProperties.RowCount := FAttrNode.Attributes.Length;
    TreeView.Invalidate;
    FChanged := True;
  end;
end;

procedure TXmlChild.mnuAddAttrClick(Sender: TObject);
var
  oAttrForm: TAttributeForm;
begin
  if FAttrNode <> nil then begin
    oAttrForm := TAttributeForm.Create(self);
    if oAttrForm.ShowModal = mrOk then begin
      FAttrNode.SetAttribute(oAttrForm.AttrNameEdit.Text, oAttrForm.AttrValueEdit.Text);
      AttrProperties.RowCount := FAttrNode.Attributes.Length;
      ListView.Invalidate;
      FChanged := True;
    end;
    oAttrForm.Free;
  end;
end;

procedure TXmlChild.mnuEditAttrClick(Sender: TObject);
var
  oAttrForm: TAttributeForm;
begin
  if (FAttrNode <> nil) and (FAttrNode.HasAttributes) and
     (AttrProperties.Selected <> -1) and
     (AttrProperties.Selected < FAttrNode.Attributes.Length) then begin
    oAttrForm := TAttributeForm.Create(self);
    oAttrForm.AttrValueEdit.Text := FAttrNode.Attributes.Item(AttrProperties.Selected).NodeValue;
    oAttrForm.AttrNameEdit.Text := FAttrNode.Attributes.Item(AttrProperties.Selected).NodeName;
    if oAttrForm.ShowModal = mrOk then begin
      FAttrNode.Attributes.Item(AttrProperties.Selected).NodeValue :=
        oAttrForm.AttrValueEdit.Text;
      FAttrNode.Attributes.Item(AttrProperties.Selected).NodeName :=
        oAttrForm.AttrNameEdit.Text;
      AttrProperties.Invalidate;
      FChanged := True;
    end;
    oAttrForm.Free;
  end;
end;

procedure TXmlChild.TreeViewEdited(Sender: TObject; Node: TTreeNode;
  var S: string);
begin
  S := Trim(S);
  if SetTreeNodeText(Node.Data, S) then begin
    FChanged := True;
    SetCtrlStates;
  end;
end;

function TXmlChild.GetTreeNodeForNode(oNode : TXpNode) : TTreeNode;
var
  oTreeNode : TTreeNode;
begin
  Result := nil;
  oTreeNode := TreeView.Items.GetFirstNode;
  while oTreeNode <> nil do
    if oTreeNode.Data = oNode then begin
      Result := oTreeNode;
      break;
    end
    else
      oTreeNode := oTreeNode.GetNextSibling;
end;

procedure TXmlChild.mnuInsertElementClick(Sender: TObject);
var
  oElem: TXpElement;
  oNode: TXpNode;
  oTreeNode : TTreeNode;
begin
  if ElementForm.ShowModal = mrOk then begin
    oElem := DOM.Document.CreateElement(ElementForm.NameEdit.Text);
    oTreeNode := TreeView.Selected;
    if (oTreeNode <> nil) and
       (TXpNode(oTreeNode.Data).ParentNode.NodeType <> DOCUMENT_NODE) then begin
      oNode := oTreeNode.Data;
      oNode.ParentNode.InsertBefore(oElem, oNode);
      TreeView.Items.InsertObject(oTreeNode, GetTreeNodeText(oElem), oElem);
    end
    else begin
      oNode := DOM.Document.DocumentElement;
      oNode.AppendChild(oElem);
      TreeView.Items.AddChildObject(GetTreeNodeForNode(oNode),
                                    GetTreeNodeText(oElem), oElem);
    end;
    oElem.Release;
    FChanged := True;
    SetCtrlStates;
  end;
end;

procedure TXmlChild.mnuAddElementasChildClick(Sender: TObject);
var
  oElem: TXpElement;
  oNode: TXpNode;
  oTreeNode : TTreeNode;
begin
  oTreeNode := TreeView.Selected;
  if (oTreeNode <> nil) and
     (TXpNode(oTreeNode.Data).NodeType = ELEMENT_NODE) then begin
    if ElementForm.ShowModal = mrOk then begin
      oElem := DOM.Document.CreateElement(ElementForm.NameEdit.Text);
      oNode := oTreeNode.Data;
      oNode.AppendChild(oElem);
      TreeView.Items.AddChildObject(oTreeNode, GetTreeNodeText(oElem), oElem);
      if not oTreeNode.Expanded then
        oTreeNode.Expand(false);
      oElem.Release;
      FChanged := True;
      SetCtrlStates;
    end;
  end
  else
    MessageDlg('No node selected or cannot add a child to this kind of node.',
               mtError, [mbOk], 0);
end;

procedure TXmlChild.mnuInsertTextClick(Sender: TObject);
var
  oText: TXpText;
  oNode: TXpNode;
  oTreeNode : TTreeNode;
begin
  oTreeNode := TreeView.Selected;
  if (oTreeNode <> nil) and
     (TXpNode(oTreeNode.Data).ParentNode.NodeType <> DOCUMENT_NODE) then begin
    TextForm.TextEdit.Text := '';
    if TextForm.ShowModal = mrOk then begin
      oText := DOM.Document.CreateTextNode(TextForm.TextEdit.Text);
      oNode := oTreeNode.Data;
      oNode.ParentNode.InsertBefore(oText, oNode);
      TreeView.Items.InsertObject(oTreeNode, GetTreeNodeText(oText), oText);
      oText.Release;
      FChanged := True;
      SetCtrlStates;
    end;
  end
  else
    MessageDlg('No node selected or cannot insert text at this level.',
               mtError, [mbOk], 0);
end;

procedure TXmlChild.mnuAddTextAsChildClick(Sender: TObject);
var
  oText: TXpText;
  oNode: TXpNode;
  oTreeNode : TTreeNode;
begin
  oTreeNode := TreeView.Selected;
  if (oTreeNode <> nil) and
     (TXpNode(oTreeNode.Data).NodeType = ELEMENT_NODE) then begin
    TextForm.TextEdit.Text := '';
    if TextForm.ShowModal = mrOk then begin
      oText := DOM.Document.CreateTextNode(TextForm.TextEdit.Text);
      oNode := oTreeNode.Data;
      oNode.AppendChild(oText);
      TreeView.Items.AddChildObject(oTreeNode, GetTreeNodeText(oText), oText);
      if not oTreeNode.Expanded then
        oTreeNode.Expand(false);
      oText.Release;
      FChanged := True;
      SetCtrlStates;
    end;
  end
  else
    MessageDlg('No node selected or cannot add text below this node.',
               mtError, [mbOk], 0);
end;

procedure TXmlChild.mnuInsertCDATAClick(Sender: TObject);
var
  oText: TXpCDATASection;
  oNode: TXpNode;
  oTreeNode : TTreeNode;
begin
  oTreeNode := TreeView.Selected;
  if (oTreeNode <> nil) and
     (TXpNode(oTreeNode.Data).ParentNode.NodeType <> DOCUMENT_NODE) then begin
    TextForm.TextEdit.Text := '';
    if TextForm.ShowModal = mrOk then begin
      oText := DOM.Document.CreateCDATASection(TextForm.TextEdit.Text);
      oNode := oTreeNode.Data;
      oNode.ParentNode.InsertBefore(oText, oNode);
      TreeView.Items.InsertObject(oTreeNode, GetTreeNodeText(oText), oText);
      oText.Release;
      FChanged := True;
      SetCtrlStates;
    end;
  end
  else
    MessageDlg('No node selected or cannot insert CDATA section at this level.',
               mtError, [mbOk], 0);
end;

procedure TXmlChild.mnuAddCDATAClick(Sender: TObject);
var
  oText: TXpCDATASection;
  oNode: TXpNode;
  oTreeNode : TTreeNode;
begin
  oTreeNode := TreeView.Selected;
  if (oTreeNode <> nil) and
     (TXpNode(oTreeNode.Data).NodeType = ELEMENT_NODE) then begin
    TextForm.TextEdit.Text := '';
    if TextForm.ShowModal = mrOk then begin
      oText := DOM.Document.CreateCDATASection(TextForm.TextEdit.Text);
      oNode := oTreeNode.Data;
      oNode.AppendChild(oText);
      TreeView.Items.AddChildObject(oTreeNode, GetTreeNodeText(oText), oText);
      if not oTreeNode.Expanded then
        oTreeNode.Expand(false);
      oText.Release;
      FChanged := True;
      SetCtrlStates;
    end;
  end
  else
    MessageDlg('No node selected or cannot add CDATA section below this node.',
               mtError, [mbOk], 0);
end;

procedure TXmlChild.mnuInsertCommentClick(Sender: TObject);
var
  oComment: TXpComment;
  oNode: TXpNode;
  oTreeNode : TTreeNode;
begin
  oTreeNode := TreeView.Selected;
  if oTreeNode <> nil then begin
    CommentForm.CommentEdit.Text := '';
    if CommentForm.ShowModal = mrOk then begin
      oComment := DOM.Document.CreateComment(CommentForm.CommentEdit.Text);
      oNode := oTreeNode.Data;
      oNode.ParentNode.InsertBefore(oComment, oNode);
      TreeView.Items.InsertObject(oTreeNode, GetTreeNodeText(oComment), oComment);
      oComment.Release;
      FChanged := True;
      SetCtrlStates;
    end;
  end
  else
    MessageDlg('No node selected.', mtError, [mbOk], 0);
end;

procedure TXmlChild.mnuInsertPIClick(Sender: TObject);
var
  oPI: TXpProcessingInstruction;
  oNode: TXpNode;
  oPIForm: TPIForm;
  oTreeNode : TTreeNode;
begin
  oTreeNode := TreeView.Selected;
  if oTreenode <> nil then begin
    oPIForm := TPIForm.Create(self);
    if oPIForm.ShowModal = mrOk then begin
      oPI := DOM.Document.CreateProcessingInstruction(oPIForm.PINameEdit.Text);
      oPI.NodeValue := oPIForm.PIValueEdit.Text;
      oNode := oTreeNode.Data;
      oNode.ParentNode.InsertBefore(oPI, oNode);
      TreeView.Items.InsertObject(oTreeNode, GetTreeNodeText(oPI), oPI);
      oPI.Release;
      FChanged := True;
      SetCtrlStates;
    end;
    oPIForm.Free;
  end
  else
    MessageDlg('No node selected.', mtError, [mbOk], 0);
end;

procedure TXmlChild.mnuDeleteNodeClick(Sender: TObject);
var
  oTreeNode : TTreeNode;
  oNode : TXpNode;
begin
  { Obtain the selected tree node. }
  oTreeNode := TreeView.Selected;
  if oTreeNode <> nil then begin
    { Obtain the corresponding DOM node. }
    oNode := oTreeNode.Data;
    if oNode = DOM.Document.DocumentElement then begin
      MessageDlg('You are not allowed to remove the document element.',
                 mtError, [mbOk], 0);
      exit;
    end;

    { The tree's new selected node is the next sibling of the currently selected
      tree node. }
    TreeView.Selected := oTreeNode.GetNextSibling;
    oNode.ParentNode.RemoveChild(oNode);
    oNode.Release;
    oTreeNode.Delete;
    FChanged := True;
    SetCtrlStates;
  end
  else
    MessageDlg('No node selected.', mtError, [mbOk], 0);
end;

procedure TXmlChild.btnDuplicateClick(Sender: TObject);
var
  oNode, oNewNode: TXpNode;
  oNewTreeNode, oTreeNode : TTreeNode;
begin
  oTreeNode := TreeView.Selected;
  if oTreeNode <> nil then begin
    oNode := oTreeNode.Data;
    if oNode = DOM.Document.DocumentElement then begin
      MessageDlg('You are not allowed to duplicate the document element.',
                 mtError, [mbOk], 0);
      Exit;
    end;

    { Duplicate via button click is always recursive. }
    oNewNode := oNode.CloneNode(True);
    if oNewNode = nil then
      Exit;

    oNode.ParentNode.InsertBefore(oNewNode, oNode.NextSibling);
{$IFDEF WIN32}
    oNewTreeNode := TreeView.Items.InsertObject(oTreeNode.GetNextSibling,
                                                GetTreeNodeText(oNewNode),
                                                oNewNode);
{$ENDIF}
{$IFDEF LINUX}
    oNewTreeNode := TreeView.Items.Insert(oTreeNode, GetTreeNodeText(oNewNode));
    oNewTreeNode.Data := oNewNode;
{$ENDIF}
    { Add tree nodes for the new node's children. }
    UpdateTreeChildrenFromObjModel(oNewNode, oNewTreeNode);
    oNewNode.Release;
    FChanged := True;
    SetCtrlStates;
  end
  else
    MessageDlg('No node selected.', mtError, [mbOk], 0);
end;

procedure TXmlChild.btnPromoteClick(Sender: TObject);
var
  oNode, oParent: TXpNode;
  oTreeNode : TTreeNode;
begin
  oTreeNode := TreeView.Selected;
  if (oTreeNode <> nil) and
     (TXpNode(oTreeNode.Data).ParentNode.NodeType <> DOCUMENT_NODE) then begin
    oNode := oTreeNode.Data;
    oParent := oNode.ParentNode;
    oNode := oParent.RemoveChild(oNode);
    oParent.ParentNode.InsertBefore(oNode, oParent.NextSibling);
    oNode.Release;
    if (oTreeNode.Parent.GetNextSibling <> nil) then                   {!!.57 - Start}
      oTreeNode.MoveTo(oTreeNode.Parent.GetNextSibling, naInsert)
    else
      oTreeNode.MoveTo(oTreeNode.Parent, naAdd);                       {!!.57 - End}
    FChanged := True;
    SetCtrlStates;
  end
  else
    MessageDlg('No node selected or cannot promote this node.', mtError,
               [mbOk], 0);
end;

procedure TXmlChild.btnDemoteClick(Sender: TObject);
var
  oNode, oPrev: TXpNode;
  oTreeNode : TTreeNode;
begin
  oTreeNode := TreeView.Selected;
  if (oTreeNode <> nil) and
     (TXpNode(oTreeNode.Data).PreviousSibling <> nil) and
     (TXpNode(oTreeNode.Data).PreviousSibling.NodeType = ELEMENT_NODE) then begin
    oNode := oTreeNode.Data;
    oPrev := oNode.PreviousSibling;
    oNode := oNode.ParentNode.RemoveChild(oNode);
    oPrev.InsertBefore(oNode, oPrev.FirstChild);
    oNode.Release;
    oTreeNode.MoveTo(oTreeNode.GetPrevSibling, naAddChildFirst);
    FChanged := True;
    SetCtrlStates;
  end
  else
    MessageDlg('No node selected or cannot demote this node.', mtError,
               [mbOk], 0);
end;

procedure TXmlChild.btnMoveUpClick(Sender: TObject);
var
  bExpanded : Boolean;
  oNode: TXpNode;
  oList: TXpNodeList;
  oTreeNode, oTreeNodeSibling : TTreeNode;
begin
  oTreeNode := TreeView.Selected;
  if (oTreeNode <> nil) then begin
    oTreeNodeSibling := oTreeNode.GetPrevSibling;
    if oTreeNodeSibling <> nil then begin
      TreeView.Items.BeginUpdate;
      try
        { Swap the nodes in the tree view & the DOM. }
        oNode := oTreeNode.Data;
        oList := oNode.ParentNode.ChildNodes;
        oList.Move(oList.IndexOf(oNode), oList.IndexOf(oNode) - 1);
        bExpanded := oTreeNode.Expanded;
        oTreeNode.MoveTo(oTreeNodeSibling, naInsert);
        oTreeNode.Expanded := bExpanded;
        FChanged := True;
        SetCtrlStates;
      finally
        TreeView.Items.EndUpdate;
      end;
    end
    else
      MessageDlg('No node selected or cannot move this node up.',
                 mtError, [mbOk], 0);
  end;
end;

procedure TXmlChild.btnMoveDownClick(Sender: TObject);
var
  bExpanded : Boolean;
  oNode: TXpNode;
  oList: TXpNodeList;
  oTreeNode, oTreeNodeSibling : TTreeNode;
begin
  oTreeNode := TreeView.Selected;
  if (oTreeNode <> nil) then begin
    oTreeNodeSibling := oTreeNode.GetNextSibling;
    if oTreeNodeSibling <> nil then begin
      TreeView.Items.BeginUpdate;
      try
        { Swap the nodes in the tree view & the DOM. }
        oNode := oTreeNode.Data;
        oList := oNode.ParentNode.ChildNodes;
        oList.Move(oList.IndexOf(oNode), oList.IndexOf(oNode) + 1);
        bExpanded := oTreeNodeSibling.Expanded;
        oTreeNodeSibling.MoveTo(oTreeNode, naInsert);
        oTreeNodeSibling.Expanded := bExpanded;
        FChanged := True;
        SetCtrlStates;
      finally
        TreeView.Items.EndUpdate;
      end;
    end
    else
      MessageDlg('No node selected or cannot move this node down.',
                 mtError, [mbOk], 0);
  end;  { if }
end;

procedure TXmlChild.ListViewEdited(Sender: TObject; Item: TListItem;
  var S: string);
var
  i: Integer;
  oItem: TTreeNode;
begin
  if TreeView.TopItem = nil then
    exit;

  i := Item.Index;
  oItem := TreeView.TopItem;
  while (i > 0) and (oItem <> nil) do begin
    oItem := oItem.GetNextVisible;
    Dec(i);
  end;
  if oItem <> nil then begin
    SetListViewText(oItem.Data, S);
    oItem.Text := GetTreeNodeText(oItem.Data);
  end;
  SetCtrlStates;
end;

procedure TXmlChild.ListViewDblClick(Sender: TObject);
var
  i: Integer;
  oItem: TTreeNode;
{$IFDEF LINUX}
  l : tListItem;
{$ENDIF}
begin
{$IFDEF WIN32}
  if (TreeView.TopItem = nil) or (ListView.Selected = nil) then
    exit;
  i := ListView.Selected.Index;
{$ENDIF}
{$IFDEF LINUX}
  l := ListView.ItemFocused;
  i := l.Index;
{$ENDIF}
  oItem := TreeView.TopItem;
  while (i > 0) and (oItem <> nil) do begin
    oItem := oItem.GetNextVisible;
    Dec(i);
  end;
  if oItem <> nil then begin
    EditNodeDialog(oItem.Data);
    oItem.Text := GetTreeNodeText(oItem.Data);
  end;
{$IFDEF LINUX}
  RefreshList;
{$ENDIF}
  SetCtrlStates;
end;

procedure TXmlChild.EditNodeDialog(oNode: TXpNode);
var
  oPIForm: TPIForm;
begin
  case oNode.NodeType of
    PROCESSING_INSTRUCTION_NODE: begin
      oPIForm := TPIForm.Create(self);
      oPIForm.PINameEdit.Text := oNode.NodeName;
      oPIForm.PIValueEdit.Text := oNode.NodeValue;
      if oPIForm.ShowModal = mrOk then begin
        oNode.NodeName := oPIForm.PINameEdit.Text;
        oNode.NodeValue := oPIForm.PIValueEdit.Text;
        FChanged := True;
      end;
      oPIForm.Free;
    end;
    COMMENT_NODE: begin
      CommentForm.CommentEdit.Text := oNode.NodeValue;
      if CommentForm.ShowModal = mrOk then begin
        oNode.NodeValue := CommentForm.CommentEdit.Text;
        FChanged := True;
      end;
    end;
    TEXT_NODE, CDATA_SECTION_NODE: begin
      TextForm.TextEdit.Text := oNode.NodeValue;
      if TextForm.ShowModal = mrOk then begin
        oNode.NodeValue := TextForm.TextEdit.Text;
        FChanged := True;
      end;
    end;
  end;
end;

procedure TXmlChild.btnNormalizeClick(Sender: TObject);
var
  oNode: TXpElement;
  oTreeNode : TTreeNode;
begin
  oTreeNode := TreeView.Selected;
  if (oTreeNode <> nil) and
     (TXpNode(oTreeNode.Data).NodeType = ELEMENT_NODE) then begin
    oNode := oTreeNode.Data;
    oNode.Normalize(true);
    FChanged := True;
    UpdateTreeFromObjModel;
    SetCtrlStates;
  end
  else
    MessageDlg('No element node selected...cannot normalize.', mtError,
               [mbOk], 0);
end;

procedure TXmlChild.PageControlChanging(Sender: TObject;
  var AllowChange: Boolean);
var
  aBuffer : string;
  i,
  wRet: Integer;
  oNode : TXpNode;
  oSavCursor : TCursor;
  oWorkDOM : TXpObjModel;
begin
  AllowChange := True;
  if (PageControl.ActivePage = SourceTab) and EditView.Modified then
  begin
    wRet := MessageDlg('The document has changed.  Would you like to parse ' +
                       'the changes back into the object model?',
                       mtConfirmation, [mbYes, mbNo, mbCancel], 0);
    if (wRet = mrCancel) or ((wRet = mrNo) and (DOM = nil)) then begin
      AllowChange := False;
      exit;
    end;

    if wRet = mrYes then begin
      try
{$IFDEF WIN32}
        SetCapture(Handle);
{$ENDIF}
        oSavCursor := Screen.Cursor;
        Screen.Cursor := crHourglass;
        oWorkDOM := TXpObjModel.Create(nil);
        try
          aBuffer := EditView.Text;
          oWorkDOM.RaiseErrors := True;
          oWorkDOM.LoadMemory(aBuffer[1], Length(aBuffer));
          DOM.ClearDocument;
{Begin !!.57}
          for i := 0 to Pred(oWorkDOM.Document.ChildNodes.Length) do begin
            oNode := DOM.Document.ImportNode(oWorkDOM.Document.ChildNodes.Item(i),
                                             True);
            DOM.Document.AppendChild(oNode);
          end;
{End !!.57}
{$IFDEF XPDPRO}
          UpdatePreviewPage;
{$ENDIF}
          FChanged := True;
          TreeView.Items.Clear;
          ShowValidate(False);
          UpdateTreeFromObjModel;
          GetTreeNodeForNode(DOM.Document.DocumentElement).Expand(False);
        finally
          oWorkDOM.Free;
          Screen.Cursor := oSavCursor;
{$IFDEF WIN32}
          ReleaseCapture;
{$ENDIF}
        end;
      except
        on X: EXpParserError do begin
          StatusBar.SimpleText := 'Error on line: ' + IntToStr(X.Line) +
                                  ' position: ' + IntToStr(X.LinePos) +
                                  ' abs at: ' + IntToStr(X.FilePos) +
                                  ' error msg: ' + X.Reason;
          EditView.SelStart := X.FilePos;
          EditView.SetFocus;
          AllowChange := False;
        end;
      end;
    end;
  end;
end;

procedure TXmlChild.CopyToClipboard(bIncludeChildren: Boolean);
var
{$IFDEF WIN32}
  aHandle : THandle;
  iLen : Integer;
  pBuffer : Pointer;
{$ENDIF}
  oNode: TXpNode;
  sXMLText : DOMString;
begin
  if PageControl.ActivePage = SourceTab then
    EditView.CopyToClipboard
{$IFDEF XPDPRO}
  else if PageControl.ActivePage = FPreviewTab then
    FPreviewForm.CopyToClipboard
{$ENDIF}
  else begin
    { The Clipboard methods do not support Unicode. So we must obtain a buffer
      via global memory
      (see http://msdn.microsoft.com/library/psdk/winbase/clipbrd_5nfo.htm),
      manually move the bytes, and then associate the buffer with the
      clipboard. }

    { First, obtain the set of text describing the nodes to be copied. }
    if TreeView.Selected = nil then
      sXMLText := DOM.XmlDocument
    else if bIncludeChildren then
      sXMLText := TXpNode(TreeView.Selected.Data).XmlDocument
    else begin
      oNode := TXpNode(TreeView.Selected.Data).CloneNode(false);
      sXMLText := oNode.XmlDocument;
      oNode.Free;
    end;
{$IFDEF WIN32}
    { Next, obtain a global memory buffer and move the text into the buffer. }
    iLen := Length(sXMLText);
    aHandle := GlobalAlloc(GMEM_MOVEABLE + GMEM_DDESHARE, 2 * iLen + 2);
    try
      { Convert the handle into a pointer so that we may load the memory with
        the XML text. }
      pBuffer := GlobalLock(aHandle);
      try
        { Load the buffer. }
        Move(PWideChar(sXMLText)^, pBuffer^, (iLen * 2) + 2);

        { Associate the buffer with the clipboard. }
        ClipBoard.SetAsHandle(CF_UNICODETEXT, aHandle);
      finally
        GlobalUnlock(aHandle);
      end;
    except
      GlobalFree(aHandle);
      raise;
    end;
{$ENDIF}
{$IFDEF LINUX}
    { Copy to clipboard. }
    ClipBoard.AsText := sXMLText;
{$ENDIF}
  end;
end;

procedure TXmlChild.CutToClipboard;
begin
  if PageControl.ActivePage = SourceTab then
    EditView.CutToClipboard
  else if TreeView.Selected <> nil then begin
    CopyToClipboard(True);
    mnuDeleteNodeClick(self);
  end;
end;

procedure TXmlChild.PasteFromClipboard;
resourcestring
  csNotXMLData = 'Not valid XML data...cannot paste!';
var
{$IFDEF WIN32}
  aHandle : THandle;
  pBuffer, pTmpBuffer : PWideChar;
  iLen : Integer;
{$ENDIF}
  bIsBEorLE : Boolean;
{$IFDEF LINUX}
  String1 : widestring;
  String2 : widestring;
{$ENDIF}
  i: Integer;
  oWorkDOM: TXpObjModel;
  oNode, oInsertNode: TXpNode;
  oNewTreeNode, oTreeNode : TTreeNode;
begin
  if PageControl.ActivePage = SourceTab then
    EditView.PasteFromClipboard
{$IFDEF XPDPRO}
  else if PageControl.ActivePage = FPreviewTab then
    Exit
{$ENDIF}
  else begin
    oWorkDOM := TXpObjModel.Create(nil);
    try
{$IFDEF WIN32}
      iLen := 0;
      bIsBEorLE := False;
      pTmpBuffer := nil;
      try
        { Clipboard must contain text or unicode text. }
        if not ClipBoard.HasFormat(CF_TEXT) then begin
          MessageDlg(csNotXMLData, mtError, [mbOk], 0);
          Exit;
        end;
        { Ask for text in Unicode format. The clipboard will automatically
          perform conversion if standard text was pasted to clipboard
          (see http://msdn.microsoft.com/library/psdk/winbase/clipbrd_64j7.htm). }
        aHandle := ClipBoard.GetAsHandle(CF_UNICODETEXT);
        if aHandle <> 0 then
          try
            { Convert the handle into a pointer. Obtain the buffer length. }
            pBuffer := GlobalLock(aHandle);
            iLen := Length(pBuffer) * 2 + 2;
            { Are there any marks to indicate it is big endian or little endian? }
            bIsBEorLE := ( ((pBuffer[0] = #$FE) and (pBuffer[1] = #$FF)) or
                           ((pBuffer[0] = #$FF) and (pBuffer[1] = #$FE)) );

            if not bIsBEorLE then begin
              { No. Must force it to be recognized as little endian. }
              GetMem(pTmpBuffer, iLen + 2);
              Move(pBuffer^, (pTmpBuffer + 1)^, iLen);
              pTmpBuffer[0] := #$FEFF;
            end else
              pTmpBuffer := pBuffer;
            { Load the buffer into the DOM. }
            oWorkDOM.LoadMemory(pTmpBuffer^, iLen + 2);
          finally
            { The operating system handles freeing of the text. }
            GlobalUnlock(aHandle);
            if not bIsBEorLE then
              FreeMem(pTmpBuffer, iLen);
          end;
{$ENDIF}
{$IFDEF LINUX}
      try
        String1 := ClipBoard.AsText;
        { Are there any marks to indicate it is big endian or little endian? }
        bIsBEorLE := ( ((String1[1] = #$FE) and (String1[2] = #$FF)) or
                       ((String1[1] = #$FF) and (String1[2] = #$FE)) );
        if not bIsBEorLE then begin
          { No. Must force it to be recognized as little endian. }
          SetLength(String2, 1{Succ(Length(String1))});
          String2[1] := #$FEFF;//' ';
          String2 := String2 + String1;
        end else
        String2 := ClipBoard.AsText;
        oWorkDOM.LoadMemory(String2[1], ( Length(String2) * 2 ));
{$ENDIF}
        { Add contents of new tree to existing tree }
        oTreenode := TreeView.Selected;
        for i := 0 to oWorkDOM.Document.ChildNodes.Length - 1 do begin
          oNode := oWorkDOM.Document.ChildNodes.Item(i).CloneNode;
          DOM.Document.DocumentElement.ForceOwnerDocument(oNode);
          if (oTreeNode <> nil) and
             (TXpNode(oTreenode.Data).ParentNode.NodeType <>
              DOCUMENT_NODE) then begin
{$IFDEF WIN32}
            oInsertNode := oTreeNode.Data;
            oInsertNode.ParentNode.InsertBefore(oNode, oInsertNode.NextSibling);
            if oTreeNode.GetNextSibling = nil then
              oNewTreeNode := TreeView.Items.AddObject(oTreeNode,
                                                       GetTreeNodeText(oNode),
                                                       oNode)
            else
              oNewTreeNode := TreeView.Items.InsertObject(oTreeNode.GetNextSibling,
                                                          GetTreeNodeText(oNode),
                                                          oNode);
{$ENDIF}
{$IFDEF LINUX}
            oInsertNode := oTreeNode.Data;
            oInsertNode.ParentNode.InsertBefore(oNode, oInsertNode.NextSibling);
            oNewTreeNode := TreeView.Items.Insert( oTreeNode.GetNextSibling,
                                                   GetTreeNodeText(oNode));
            oNewTreeNode.Data := oNode;
{$ENDIF}
          end
          else begin
            oInsertNode := DOM.Document.DocumentElement;
            oInsertNode.AppendChild(oNode);
            oNewTreeNode := TreeView.Items.AddChildObject
                              (GetTreeNodeForNode(oInsertNode),
                               GetTreeNodeText(oNode), oNode);
          end;
          { Add tree nodes for the new node's children. }
          UpdateTreeChildrenFromObjModel(oNode, oNewTreeNode);
          oNode.Release;
        end;
        FChanged := True;
        SetCtrlStates;
      except
        on X: EXpParserError do
          MessageDlg(csNotXMLData, mtError, [mbOk], 0);
      end;
    finally
      oWorkDOM.Free;
    end;
  end;
end;

procedure TXmlChild.SetCtrlStates;
var
  OnStructTab : Boolean;
  InTree : Boolean;
  NodeIsElement : Boolean;
  ParentNotDoc : Boolean;
  ParentNotDocElement : Boolean;
  NextSiblingExists : Boolean;
  PrevSiblingExists : Boolean;
  PrevSiblingIsElement : Boolean;
  AttrSelected : Boolean;
begin
{$IFDEF LINUX}
  AttrSelected := false;
{$ENDIF}
  { Check preconditions. }
  OnStructTab := (PageControl.ActivePage = StructTab);

{$IFDEF LINUX}
  if assigned(AttrProperties)then
{$ENDIF}
    AttrSelected := (AttrProperties.Selected >= 0);

  InTree := OnStructTab and (TreeView.Selected <> nil);

  NextSiblingExists := InTree and
                       (TXpNode(TreeView.Selected.Data).NextSibling <> nil);

  NodeIsElement := InTree and (TXpNode(TreeView.Selected.Data).NodeType = ELEMENT_NODE);

  ParentNotDoc := InTree and (TXpNode(TreeView.Selected.Data).ParentNode.NodeType <> DOCUMENT_NODE);

  ParentNotDocElement := InTree and (TXpNode(TreeView.Selected.Data).ParentNode <>
                          DOM.Document.DocumentElement);

  PrevSiblingExists := InTree and (TXpNode(TreeView.Selected.Data).PreviousSibling <> nil);

  PrevSiblingIsElement := PrevSiblingExists and
                          (TXpNode(TreeView.Selected.Data).PreviousSibling.NodeType = ELEMENT_NODE);

  { Enable controls. }
  btnInsert.Enabled := (PageControl.ActivePage = StructTab);

{$IFDEF LINUX}
  if assigned(mnuAddElementAsChild)then
{$ENDIF}
    mnuAddElementAsChild.Enabled := NodeIsElement;
  btnAddChildElement.Enabled := NodeIsElement;
{$IFDEF LINUX}
  if assigned(mnuInsertText)then
{$ENDIF}
    mnuInsertText.Enabled := ParentNotDoc;
{$IFDEF LINUX}
  if assigned(mnuAddTextAsChild)then
{$ENDIF}
    mnuAddTextAsChild.Enabled := NodeIsElement;
  btnAddChildText.Enabled := NodeIsElement;
{$IFDEF LINUX}
  if assigned(mnuInsertComment)then
{$ENDIF}
    mnuInsertComment.Enabled := InTree;
{$IFDEF LINUX}
  if assigned(mnuInsertPI)then
{$ENDIF}
    mnuInsertPI.Enabled := InTree;
{$IFDEF LINUX}
  if assigned(mnuInsertCDATA)then
{$ENDIF}
    mnuInsertCDATA.Enabled := ParentNotDoc;
{$IFDEF LINUX}
  if assigned(mnuAddCDATA)then
{$ENDIF}
    mnuAddCDATA.Enabled := NodeIsElement;

  btnDuplicate.Enabled := InTree;

  btnPromote.Enabled := ParentNotDoc and ParentNotDocElement;

  btnDemote.Enabled := PrevSiblingIsElement;

  btnMoveUp.Enabled := PrevSiblingExists;

  btnMoveDown.Enabled := NextSiblingExists;

  btnCollapseAll.Enabled := OnStructTab;

  btnExpandAll.Enabled := OnStructTab;

  btnNormalize.Enabled := NodeIsElement;

{$IFDEF LINUX}
  if assigned(mnuPreserveSpace)then begin
{$ENDIF}
    mnuPreserveSpace.Enabled := NodeIsElement;
    mnuPreserveSpace.Checked := NodeIsElement and
      (TXpElement(TreeView.Selected.Data).GetAttribute('xml:space') = 'preserve');
{$IFDEF LINUX}
    end;
{$ENDIF}

{$IFDEF LINUX}
  if assigned(mnuAddAttr)then
{$ENDIF}
    mnuAddAttr.Enabled := NodeIsElement;

{$IFDEF LINUX}
  if assigned(mnuRemoveAttr)then
{$ENDIF}
    mnuRemoveAttr.Enabled := OnStructTab and AttrSelected;

{$IFDEF LINUX}
  if assigned(mnuEditAttr)then
{$ENDIF}
    mnuEditAttr.Enabled := OnStructTab and AttrSelected;

{$IFDEF LINUX}
  if assigned(mnuAddGUID)then
{$ENDIF}
    mnuAddGUID.Enabled := NodeIsElement;

  if assigned(FOnSetCtrlStates) then
    FOnSetCtrlStates(Self);

end;

function TXmlChild.CreateNewNode(sElemName: string): TXpElement;
begin
  Result := TXpElement(TreeView.Selected.Data).CreateChildElement(sElemName);
  FChanged := True;
end;

procedure TXmlChild.TreeViewKeyPress(Sender: TObject; var Key: Char);
begin
  inherited;
  if (Key = #13) and (TreeView.Selected <> nil) and
    not TreeView.IsEditing then begin
      TreeView.Selected.EditText;
      Key := #0;
    end;
end;

procedure TXmlChild.ListViewKeyPress(Sender: TObject; var Key: Char);
begin
  inherited;
  if (Key = #13) and (ListView.Selected <> nil) and
    not ListView.IsEditing then begin
{$IFDEF WIN32}
      ListView.Selected.EditCaption;
{$ENDIF}
      Key := #0;
    end;
end;

procedure TXmlChild.ListViewEditing(Sender: TObject; Item: TListItem;
  var AllowEdit: Boolean);
var
  i: Integer;
  oItem: TTreeNode;
begin
  AllowEdit := False;
  if (TreeView.TopItem = nil) or (ListView.Selected = nil) then
    exit;

  i := ListView.Selected.Index;
  oItem := TreeView.TopItem;
  while (i > 0) and (oItem <> nil) do begin
    oItem := oItem.GetNextVisible;
    Dec(i);
  end;
  if oItem <> nil then begin
    case TXpNode(oItem.Data).NodeType of
      PROCESSING_INSTRUCTION_NODE, COMMENT_NODE,
      TEXT_NODE, CDATA_SECTION_NODE: AllowEdit := True;
    end;
  end;
end;

procedure TXmlChild.TreeViewEditing(Sender: TObject; Node: TTreeNode;
  var AllowEdit: Boolean);
begin
  AllowEdit := False;
  case TXpNode(Node.Data).NodeType of
    PROCESSING_INSTRUCTION_NODE, ELEMENT_NODE: AllowEdit := True;
  end;
end;

function TXmlChild.GetFormattedOutput : Boolean;
begin
  Result := DOM.FormattedOutput;
end;

function TXmlChild.GetNormalize : Boolean;
begin
  Result := DOM.NormalizeData;
end;

procedure TXmlChild.SetFormattedOutput(aValue : Boolean);
begin
  DOM.FormattedOutput := aValue;
end;

procedure TXmlChild.SetNormalize(aValue : Boolean);
begin
  DOM.NormalizeData := aValue;
end;

procedure TXmlChild.FormShow(Sender: TObject);
begin
  with AttrProperties do begin
    Parent := Panel2;
    Height := 105;
    Width := 177;
    Top := 140;
    ValueHeader := 'Attr. Value';
    NameHeader := 'Attr. Name';
    NameWidth := 75;
    PopupMenu := AttrPopupMenu;
    RowHeight := 15;
    ShowHeader := True;
    ShowLines := True;
    OnClick := AttrPropertiesClick;
    OnPropertyName := AttrPropertiesPropertyName;
    OnPropertyValue := AttrPropertiesPropertyValue;
    OnValueChange := AttrPropertiesValueChange;
    OnValueQueryEdit := AttrPropertiesValueQueryEdit;
    Align := alBottom;
  end;
  Splitter2.Align := alBottom;
  TreeView.Align := alClient;
  ShowValidate(False);
  SetCtrlStates;
end;

procedure TXmlChild.mnuPreserveSpaceClick(Sender: TObject);
begin
  if mnuPreserveSpace.Checked then
    TXpElement(TreeView.Selected.Data).RemoveAttribute('xml:space')
  else
    TXpElement(TreeView.Selected.Data).SetAttribute('xml:space', 'preserve');
  if FAttrNode.HasAttributes then
    AttrProperties.RowCount := FAttrNode.Attributes.Length
  else
    AttrProperties.RowCount := 0;
  FChanged := True;
end;

procedure TXmlChild.mnuAddGUIDClick(Sender: TObject);
begin
  if FAttrNode <> nil then begin
{$IFDEF WIN32}
    FAttrNode.SetAttribute('id', CreateClassId);
{$ENDIF}
    AttrProperties.RowCount := FAttrNode.Attributes.Length;
    ListView.Invalidate;
    FChanged := True;
  end;
end;

procedure TXmlChild.TreeViewMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  aNode : TTreeNode;
begin
  if Button = mbRight then begin
    aNode := TreeView.GetNodeAt(X,Y);
    if assigned(aNode) then begin
      TreeView.Selected := aNode;
      SetCtrlStates;
      TreeView.PopupMenu.Popup(TreeView.ClientToScreen(Point(X, Y)).X + 5,
                               TreeView.ClientToScreen(Point(X, Y)).Y + 5);
    end;
  end;
end;

procedure TXmlChild.AttrPropertiesClick(Sender: TObject);
begin
  SetCtrlStates;
end;

procedure TXmlChild.FormDestroy(Sender: TObject);
begin
  AttrProperties.Free;
end;

procedure TXmlChild.EditViewChange(Sender: TObject);
begin
  if not FLoadingSrc then begin
    FChanged := True;
    if assigned(FOnSetCtrlStates) then
      FOnSetCtrlStates(Self);
  end;
end;

procedure TXmlChild.mnuCutClick(Sender: TObject);
begin
  CutToClipboard;
end;

procedure TXmlChild.mnuCopyClick(Sender: TObject);
begin
  CopyToClipboard;
end;

procedure TXmlChild.mnuPasteClick(Sender: TObject);
begin
  PasteFromClipboard;
end;

function TXmlChild.GetXmlData: DOMString;
begin
  Result := '';
  if DOM <> nil then begin
    if PageControl.ActivePage = SourceTab then
      Result := EditView.Text
    else
      Result := DOM.XmlDocument;
  end;
end;

{$IFDEF XPDPRO}
procedure TxmlChild.IntegratePreviewPage;
begin
  { Assumption: This method called only after a document has been loaded into
    the child window. }

  { If this is an XML document then integrate the XSL preview page into the
    notebook. }
  if UpperCase(ExtractFileExt(FFileName)) = '.XML' then begin
    FPreviewTab := TTabSheet.Create(Self);
    FPreviewTab.Caption := 'XSL Preview';
    FPreviewTab.PageControl := PageControl;
    FPreviewForm := TfrmPreview.Create(Self);
    FPreviewForm.Parent := FPreviewTab;
    FPreviewForm.Align := alClient;
    FPreviewForm.BorderStyle := bsNone;
    FPreviewForm.Show;
    UpdatePreviewPage;
  end;
end;

procedure TXmlChild.UpdatePreviewPage;
begin
  { Does this document have a preview page? }
  if FPreviewForm <> nil then begin
    { Assign DOM & filename. }
    FPreviewForm.DOM := DOM;
    FPreviewForm.FileName := FFileName;
  end;
end;

procedure TXmlChild.AddStylesheet(const sName : string; oChildWin : TForm);
begin
  if FPreviewForm <> nil then
    FPreviewForm.AddStylesheet(sName, oChildWin);
end;

procedure TXmlChild.RemoveStylesheet(const sName : string);
begin
  if FPreviewForm <> nil then
    FPreviewForm.RemoveStylesheet(sName);
end;

procedure TXmlChild.RenameStylesheet(const sOldName, sNewName : string);
begin
  if FPreviewForm <> nil then
    FPreviewForm.RenameStylesheet(sOldName, sNewName);
end;
{$ENDIF}

procedure TXmlChild.btnValidateClick(Sender: TObject);
var
  oItem : TListItem;
begin
  { Make sure we are on the correct page. }
  PageControl.ActivePage := StructTab;

  { If already showing validation error then reset
    the image index of the affected node. }
  { Previous validation error selected? }
  if (FLastVErr >= 0) and (FLastVErr < lvValidate.Items.Count) then
    { Yes. Reset the node. }
    ResetValidationNode(FLastVErr);

  if not lvValidate.Visible then
    ShowValidate(True);

  { Perform validation. }
  lvValidate.Items.Clear;
  if DOM.ValidateDocument then begin
    oItem := lvValidate.Items.Add;
    oItem.Caption := '<Document is valid>';
  end
  else if lvValidate.Items.Count > 0 then begin
    lvValidate.SetFocus;
    lvValidate.Selected := lvValidate.Items[0];
  end;
end;

procedure TXmlChild.ShowValidate(const bShow : Boolean);
begin
  if bShow then begin
    { Note: Ordering of these operations is important. }
    ListView.Align := alLeft;
    ListView.Width := FListViewWidth;
    Splitter3.Visible := True;
    Splitter3.Align := alRight;
    lvValidate.Visible := True;
    Splitter3.Align := alLeft;
  end
  else begin
    FListViewWidth := ListView.Width;
    ListView.Align := alClient;
    lvValidate.Visible := False;
    Splitter3.Visible := False;
  end;
end;

procedure TXmlChild.DOMInvalidDocument(oOwner: TObject;
  wCode: Integer; oNode: TXpNode; var bStop: Boolean);
var
  oItem : TListItem;
begin
  oItem := lvValidate.Items.Add;
  oItem.Caption := MapValidationCode(wCode, oNode);
  oItem.Data := oNode;
end;

function TXmlChild.MapValidationCode(const wCode : Integer;
                                           oNode : TXpNode) : string;
begin
  case wCode of
    V_NODTD :
      Result := 'Document does not contain or reference a DTD.';
    V_BADDTD :
      Result := 'Bad DTD structure.';
    V_NODOCUMENT :
      Result := 'No document to check.';
    V_MISMATCH :
      Result := 'Document name does not match DTD name.';
    V_ELEMENTNOTDEFINED :
      Result := Format('Element ''%s'' not defined in the DTD.',
                       [oNode.NodeName]);
    V_ELEMENTNOTEMPTY :
      Result := Format('Element ''%s'' is declared as empty but is not empty.',
                       [oNode.NodeName]);
    V_ELEMENTNOTCONTENT :
      Result := Format('Element ''%s'' not allowed as a child element of ''%s''.',
                       [oNode.NodeName, oNode.ParentNode.NodeName]);
    V_ELEMENTCHILDMISSING :
      Result := Format('Element ''%s'' is missing a required child element.',
                       [oNode.NodeName]);
    V_ELEMENTCHILDNOTEXPECTED :
      Result := Format('Element ''%s'' contains a child element that is not ' +
                       'defined in the DTD.', [oNode.NodeName]);
    V_ELEMENTATTRNOTDEFINED :
      Result := Format('Element ''%s'' is defined as having attributes in the DTD ' +
                       'but does not contain any attributes in the document.',
                       [oNode.NodeName]);
    V_ATTRIBUTEFIXED :
      Result := Format('Attribute ''%s'' of element ''%s'' is assigned a fixed value ' +
                       'in the DTD but is assigned a different value in the ' +
                       'document.',
                       [oNode.NodeName, oNode.ParentNode.NodeName]);
    V_ATTRIBUTENOTENUM :
      Result := Format('Attribute ''%s'' is not an enumerated value.',
                       [oNode.NodeName]);
    V_ATTRIBUTEREQUIRED :
      Result := Format('Required attribute ''%s'' but not present.',
                       [oNode.NodeName]);
{Begin !!.57}
    V_ATTRIBUTENOTEXPECTED :
      Result := Format('Attribute ''%s'' was not expected.',
                       [oNode.NodeName]);
{End !!.57}
  else
    Result := Format('Unknown validation error %d, node ''%s''',
                     [wCode, oNode.NodeName]);
  end;  { case }
end;

procedure TXmlChild.lvValidateEditing(Sender: TObject; Item: TListItem;
  var AllowEdit: Boolean);
begin
  AllowEdit := False;
end;

procedure TXmlChild.RefreshNode(Node: TTreeNode);
var
  R : TRect;
begin
  if Node <> nil then
  begin
    if not Node.IsVisible then
      Exit;
    R := Node.DisplayRect(False);
    InvalidateRect(TreeView.Handle, @R, False);
  end;
end;

procedure TXmlChild.TreeViewCustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
begin
  if Node.ImageIndex > 5 then begin
    Sender.Canvas.Brush.Color := clRed;
    Sender.Canvas.Font.Color := clWhite;
    Sender.Canvas.Font.Style := Sender.Canvas.Font.Style + [fsBold];
  end;
end;

procedure TXmlChild.lvValidateSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
var
  oNode : TXpNode;
  oTreeNode : TTreeNode;
begin
  { Previous validation error selected? }
  if (FLastVErr >= 0) and (FLastVErr < lvValidate.Items.Count) then
    { Yes. Reset the node. }
    ResetValidationNode(FLastVErr);

  { Get the node referenced by the item. }
  if lvValidate.Selected <> nil then begin
    oNode := lvValidate.Selected.Data;
    { Did the item reference a node? }
    if oNode <> nil then begin
      { Yes. Is this an attribute node? }
      if oNode is TXpAttribute then
        { Yes. Get its parent node. }
        oNode := oNode.ParentNode;
      { Expand & highlight the related tree item. }
      if oNode <> nil then begin
        oTreeNode := TTreeNode(oNode.Tag);
        if oTreeNode.ImageIndex <= 5 then begin
          oTreeNode.Expand(False);
          oTreeNode.Selected := True;
          oTreeNode.ImageIndex := oTreeNode.ImageIndex + 6;
          RefreshNode(oTreeNode);
        end;  { if }
      end;
    end;
    FLastVErr := Item.Index;
  end;
end;

procedure TXmlChild.ResetValidationNode(const wInx : Integer);
var
  oNode : TXpNode;
  oTreeNode : TTreeNode;
begin
  { Previous validation error selected? }
  if (FLastVErr >= 0) and (FLastVErr < lvValidate.Items.Count) then begin
    oNode := lvValidate.Items[FLastVErr].Data;
    if oNode <> nil then begin
      { Yes. Is this an attribute node? }
      if oNode is TXpAttribute then
        { Yes. Get its parent node. }
        oNode := oNode.ParentNode;
      oTreeNode := TTreeNode(oNode.Tag);
      if oTreeNode.ImageIndex > 5 then begin
        oTreeNode.ImageIndex := oTreeNode.ImageIndex - 6;
        RefreshNode(oTreeNode);
      end;  { if }
    end;
  end;
end;

end.
