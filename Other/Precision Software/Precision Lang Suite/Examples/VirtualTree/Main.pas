unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  VirtualTrees, StdCtrls, ExtCtrls, plsController, Menus;

type
  TMainForm = class(TForm)
    VST: TVirtualStringTree;
    ClearButton: TButton;
    AddOneButton: TButton;
    Edit1: TEdit;
    Button1: TButton;
    Label1: TLabel;
    CloseButton: TButton;
    plsController1: TplsController;
    sbLang: TButton;
    pumLangs: TPopupMenu;
    procedure FormCreate(Sender: TObject);
    procedure ClearButtonClick(Sender: TObject);
    procedure AddButtonClick(Sender: TObject);
    procedure VSTFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VSTInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates);
    procedure CloseButtonClick(Sender: TObject);
    procedure VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: string);
    procedure plsController1InitLangManager(Sender: TObject);
    procedure ReadAvailableLanguages;
    procedure miChangeLanguageClick(Sender:TObject);
    procedure plsController1LanguageChanged(Sender: TObject);
    procedure sbLangClick(Sender: TObject);
    procedure plsController1BeforeLangChange(Sender: TObject);
  end;

var
  MainForm: TMainForm;

//----------------------------------------------------------------------------------------------------------------------

implementation

uses
  LangConsts;

{$R *.DFM}

type
  PMyRec = ^TMyRec;
  TMyRec = record
    Caption: String;
  end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.FormCreate(Sender: TObject);

begin
  // Let the tree know how much data space we need.
  VST.NodeDataSize := SizeOf(TMyRec);
  // Set an initial number of nodes.
  VST.RootNodeCount := 20;
end;

procedure TMainForm.plsController1BeforeLangChange(Sender: TObject);
begin
  LangConsts.LanguageChanged;
end;

procedure TMainForm.plsController1InitLangManager(Sender: TObject);
begin
  ReadAvailableLanguages;
end;

procedure TMainForm.plsController1LanguageChanged(Sender: TObject);
begin
  sbLang.Caption := TplsController(Sender).LanguageName;
  if (VST.RootNodeCount>0) and (vsInitialized in VST.GetFirstNoInit.States) then
    VST.ReinitChildren(nil, True);
end;

//////////////////////////// Language selection methods and events ///////////////////////////
procedure TMainForm.ReadAvailableLanguages;
var
  MI:TMenuItem;
  i:Integer;
begin
  // fill menu with available languages
  pumLangs.Items.Clear;
  for i := 0 To plsController1.LanguagesCount - 1 do
  begin
    MI:=TMenuItem.Create(self);
    MI.Hint:=plsController1.LanguageCodes[i];
    MI.Caption:=plsController1.LanguageNames[i];
    MI.OnClick:=miChangeLanguageClick;
    MI.RadioItem:=True;
    MI.GroupIndex:=1;
    pumLangs.Items.Add(MI);
  end;
end;

procedure TMainForm.sbLangClick(Sender: TObject);
var
  p:TPoint;
begin
  p:=sbLang.ClientToScreen(point(0,sbLang.Height));
  pumLangs.Popup(p.x,p.y);
end;

procedure TMainForm.miChangeLanguageClick(Sender:TObject);
begin
  // load another translation
  plsController1.LanguageCode:=TMenuItem(Sender).Hint;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.ClearButtonClick(Sender: TObject);

var
  Start: Cardinal;

begin
  Screen.Cursor := crHourGlass;
  try
    Start := GetTickCount;
    VST.Clear;
    Label1.Caption := Format(STR_LASTOPDURATION, [GetTickCount - Start]);
  finally
    Screen.Cursor := crDefault;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.AddButtonClick(Sender: TObject);

var
  Count: Cardinal;
  Start: Cardinal;

begin
  // Add some nodes to the treeview.
  Screen.Cursor := crHourGlass;
  with VST do
  begin
    BeginUpdate;
    try
      Start := GetTickCount;
      case (Sender as TButton).Tag of
        0: // add to root
          begin
            Count := StrToInt(Edit1.Text);
            RootNodeCount := RootNodeCount + Count;
          end;
        1: // add as child
          if Assigned(FocusedNode) then
          begin
            Count := StrToInt(Edit1.Text);
            ChildCount[FocusedNode] := ChildCount[FocusedNode] + Count;
            Expanded[FocusedNode] := True;
            InvalidateToBottom(FocusedNode);
          end;
      end;
      Label1.Caption := Format(STR_LASTOPDURATION, [GetTickCount - Start]);
    finally
      EndUpdate;
      Screen.Cursor := crDefault;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.VSTFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);

var
  Data: PMyRec;

begin
  Data := Sender.GetNodeData(Node);
  Finalize(Data^);
end;

procedure TMainForm.VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  Data: PMyRec;
begin
  Data := Sender.GetNodeData(Node);
  case Column of
    0:CellText := Data.Caption;
    1:if Node.ChildCount>0 then
        CellText := IntToStr(Node.ChildCount);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.VSTInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);

var
  Data: PMyRec;

begin
  if Assigned(Node) then
    with Sender do
    begin
      Data := GetNodeData(Node);
      if Assigned(Data) then
        Data.Caption := Format(STR_NODECAPTION, [GetNodeLevel(Node), Node.Index]);
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.CloseButtonClick(Sender: TObject);

begin
  Close;
end;

//----------------------------------------------------------------------------------------------------------------------

end.


