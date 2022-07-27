{================================================================================
Copyright (C) 1997-2002 Mills Enterprise

Unit     : rmTVComboBox
Purpose  : TreeView and PathTreeView based ComboBoxes
Date     : 02-01-2000
Author   : Ryan J. Mills
Version  : 1.90
================================================================================}

unit rmTVComboBox;

interface

{$I CompilerDefines.INC}

uses
   Windows, Messages, Classes, Controls, ComCtrls, CommCtrl, rmBtnEdit,
   rmPathTreeView, ImgList, rmScrnCtrls {$ifDef rmDebug}, rmMsgList{$endif};

type
   TrmTreeViewComboStyle = (tvcsDropDown, tvcsDropDownList); 

   TrmCustomComboTreeView = class(TrmCustomBtnEdit)
   private
   { Private declarations }
     FScreenTreeview: TrmCustomScreenTreeView;
     fDropDownWidth: integer;
     fSelectedNode: TTreeNode;
     FDropDownHeight: integer;
     FOnDropDown: TNotifyEvent;
     fChanged: TTVChangedEvent;
     fChanging: TTVChangingEvent;
     fLeafOnly: boolean;
     fOnDestroy: TNotifyEvent;
{$ifdef rmDebug}
     fMsg: TrmMsgEvent;
{$endif}

     procedure ToggleTreeView(Sender: TObject);

     procedure DoClick(Sender: TObject);
     procedure DoMyKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
     procedure DoMyExit(Sender: Tobject);

     procedure CMFontchanged(var Message: TMessage); message CM_FontChanged;
     procedure cmCancelMode(var Message:TCMCancelMode); Message cm_cancelMode;
     procedure wmKillFocus(var Message:TMessage); message wm_KillFocus;
     procedure wmGetDLGCode(var Message:TWMGetDlgCode); message wm_GetDLGCode;

     function GetAutoExpand: boolean;
     function GetCollapsedEvent: TTVExpandedEvent;
     function GetCollapsingEvent: TTVCollapsingEvent;
     function GetCompareEvent: TTVCompareEvent;
     function GetExpandedEvent: TTVExpandedEvent;
     function GetExpandingEvent: TTVExpandingEvent;
     function GetImages: TCustomImageList;
     function GetItems: TTreeNodes;
     function GetSortType: TSortType;
     function GetStateImages: TCustomImageList;

     procedure SetAutoExpand(const Value: boolean);
     procedure SetCollapsedEvent(const Value: TTVExpandedEvent);
     procedure SetCollapsingEvent(const Value: TTVCollapsingEvent);
     procedure SetCompareEvent(const Value: TTVCompareEvent);
     procedure SetExpandingEvent(const Value: TTVExpandingEvent);
     procedure SetExpandedEvent(const Value: TTVExpandedEvent);
     procedure SetImages(const Value: TCustomImageList);
     procedure SetSelectedNode(const Value: TTreeNode);
     procedure SetSortType(const Value: TSortType);
     procedure SetStateImages(const Value: TCustomImageList);
     procedure SetImageIndexEvent(const Value: TTVExpandedEvent);
     procedure SetSelectedIndexEvent(const Value: TTVExpandedEvent);
     function GetImageIndexEvent: TTVExpandedEvent;
     function GetSelectedIndexEvent: TTVExpandedEvent;
   protected
   { Protected declarations }
     procedure KeyDown(var Key: Word; Shift: TShiftState); override;

     property AllowLeafOnly : boolean read fLeafOnly write fLeafOnly default false;
     property AutoExpand: boolean read getAutoExpand write setAutoExpand;
     property DropDownHeight: integer read FDropDownHeight write fDropDownHeight default 0;
     property DropDownWidth: integer read fDropDownWidth write fDropDownWidth default 0;
     property Images: TCustomImageList read GetImages write SetImages;
     property Items: TTreeNodes read GetItems;
     property SelectedNode: TTreeNode read fSelectedNode write SetSelectedNode;
     property SortType: TSortType read GetSortType write SetSortType;
     property StateImages: TCustomImageList read GetStateImages write SetStateImages;

     property OnChanged: TTVChangedEvent read fChanged write fChanged;
     property OnChanging: TTVChangingEvent read fChanging write fChanging;
     property OnCollapsing: TTVCollapsingEvent read GetCollapsingEvent write SetCollapsingEvent;
     property OnCollapsed: TTVExpandedEvent read GetCollapsedEvent write SetCollapsedEvent;
     property OnCompare: TTVCompareEvent read GetCompareEvent write SetCompareEvent;
     property OnDropDown: TNotifyEvent read FOnDropDown write fOnDropDown;
     property OnExpanding: TTVExpandingEvent read GetExpandingEvent write SetExpandingEvent;
     property OnExpanded: TTVExpandedEvent read GetExpandedEvent write SetExpandedEvent;
     property OnGetImageIndex : TTVExpandedEvent read GetImageIndexEvent write SetImageIndexEvent;
     property OnGetSelectedIndex : TTVExpandedEvent read GetSelectedIndexEvent write SetSelectedIndexEvent;
     property OnDestroy : TNotifyEvent read fOnDestroy write fOnDestroy;
   public
   { Public declarations }
     constructor Create(AOwner: TComponent); override;
     destructor Destroy; override;
     procedure WndProc(var Message: TMessage); override;

     function CustomSort(SortProc: TTVCompare; Data: Longint): Boolean;
     procedure FullExpand;
     procedure FullCollapse;
{$ifdef rmDebug}
     property OnMessage:TrmMsgEvent read fMsg write fMsg;
{$endif}
   end;

   TrmComboTreeView = class(TrmCustomComboTreeView)
   public
   { Public declarations }
     property Items;
     property SelectedNode;
   published
   { Published declarations }
     {$ifdef D4_OR_HIGHER}
     property Anchors;
     property Constraints;
     {$endif}
     property Enabled;
     property Font;
     property MaxLength;
     property ParentColor;
     property ParentCtl3D;
     property ParentFont;
     property ParentShowHint;
     property PopupMenu;
     property ShowHint;
     property TabOrder;
     property TabStop;
     property Visible;

     property AllowLeafOnly;
     property AutoExpand;
     property DropDownHeight;
     property DropDownWidth;
     property Images;
     property SortType;
     property StateImages;
     property OnDropDown;
     property OnChanged;
     property OnChanging;
     property OnCollapsed;
     property OnCollapsing;
     property OnCompare;
     property OnExpanding;
     property OnExpanded;
     property OnGetImageIndex;
     property OnGetSelectedIndex;
     property OnDestroy;
   end;

   TrmCustomComboPathTreeView = class(TrmCustomBtnEdit)
   private
   { Private declarations }
     FScreenTreeview: TrmCustomScreenPathTreeView;
     fDropDownWidth: integer;
     fSelectedNode: TrmTreeNode;
     FDropDownHeight: integer;
     FOnDropDown: TNotifyEvent;
     fChanged: TrmTVChangedEvent;
     fChanging: TrmTVChangingEvent;
     fLeafOnly: boolean;
     fOnDestroy: TNotifyEvent;
{$ifdef rmDebug}
     fMsg: TrmMsgEvent;
{$endif}
     fComboStyle: TrmTreeViewComboStyle;

     procedure ToggleTreeView(Sender: TObject);

     procedure DoClick(Sender: TObject);
     procedure DoMyKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
     procedure DoMyExit(Sender: Tobject);

     procedure CheckForModifiedText;

     procedure CMFontchanged(var Message: TMessage); message CM_FontChanged;
     procedure cmCancelMode(var Message:TCMCancelMode); Message cm_cancelMode;
     procedure wmKillFocus(var Message:TMessage); message wm_KillFocus;
     procedure wmGetDLGCode(var Message:TWMGetDlgCode); message wm_GetDLGCode;

     function GetAutoExpand: boolean;
     function GetCollapsedEvent: TrmTVExpandedEvent;
     function GetCollapsingEvent: TrmTVCollapsingEvent;
     function GetCompareEvent: TrmTVCompareEvent;
     function GetExpandedEvent: TrmTVExpandedEvent;
     function GetExpandingEvent: TrmTVExpandingEvent;
     function GetImages: TCustomImageList;
     function GetItems: TrmTreeNodes;
     function GetSepChar: Char;
     function GetSortType: TSortType;
     function GetStateImages: TCustomImageList;

     procedure SetAutoExpand(const Value: boolean);
     procedure SetCollapsedEvent(const Value: TrmTVExpandedEvent);
     procedure SetCollapsingEvent(const Value: TrmTVCollapsingEvent);
     procedure SetCompareEvent(const Value: TrmTVCompareEvent);
     procedure SetExpandingEvent(const Value: TrmTVExpandingEvent);
     procedure SetExpandedEvent(const Value: TrmTVExpandedEvent);
     procedure SetImages(const Value: TCustomImageList);
     procedure SetSelectedNode(const Value: TrmTreeNode);
     procedure SetSepChar(const Value: Char);
     procedure SetSortType(const Value: TSortType);
     procedure SetStateImages(const Value: TCustomImageList);
     procedure SetImageIndexEvent(const Value: TrmTVExpandedEvent);
     procedure SetSelectedIndexEvent(const Value: TrmTVExpandedEvent);
     function GetImageIndexEvent: TrmTVExpandedEvent;
     function GetSelectedIndexEvent: TrmTVExpandedEvent;
     procedure SetComboStyle(const Value: TrmTreeViewComboStyle);
   protected
   { Protected declarations }
     procedure KeyDown(var Key: Word; Shift: TShiftState); override;

     property ComboStyle: TrmTreeViewComboStyle read fComboStyle write SetComboStyle default tvcsDropDownList;
     property AllowLeafOnly : boolean read fLeafOnly write fLeafOnly default false;
     property AutoExpand: boolean read getAutoExpand write setAutoExpand;
     property DropDownHeight: integer read FDropDownHeight write fDropDownHeight default 0;
     property DropDownWidth: integer read fDropDownWidth write fDropDownWidth default 0;
     property Images: TCustomImageList read GetImages write SetImages;
     property Items: TrmTreeNodes read GetItems;
     property SelectedNode: TrmTreeNode read fSelectedNode write SetSelectedNode;
     property SepChar: Char read GetSepChar write SetSepChar;
     property SortType: TSortType read GetSortType write SetSortType;
     property StateImages: TCustomImageList read GetStateImages write SetStateImages;

     property OnChanged: TrmTVChangedEvent read fChanged write fChanged;
     property OnChanging: TrmTVChangingEvent read fChanging write fChanging;
     property OnCollapsing: TrmTVCollapsingEvent read GetCollapsingEvent write SetCollapsingEvent;
     property OnCollapsed: TrmTVExpandedEvent read GetCollapsedEvent write SetCollapsedEvent;
     property OnCompare: TrmTVCompareEvent read GetCompareEvent write SetCompareEvent;
     property OnDropDown: TNotifyEvent read FOnDropDown write fOnDropDown;
     property OnExpanding: TrmTVExpandingEvent read GetExpandingEvent write SetExpandingEvent;
     property OnExpanded: TrmTVExpandedEvent read GetExpandedEvent write SetExpandedEvent;
     property OnGetImageIndex : TrmTVExpandedEvent read GetImageIndexEvent write SetImageIndexEvent;
     property OnGetSelectedIndex : TrmTVExpandedEvent read GetSelectedIndexEvent write SetSelectedIndexEvent;
     property OnDestroy : TNotifyEvent read fOnDestroy write fOnDestroy;
   public
   { Public declarations }
     constructor Create(AOwner: TComponent); override;
     destructor Destroy; override;
     procedure WndProc(var Message: TMessage); override;

     function AddPathNode(Node: TrmTreeNode; Path: string): TrmTreeNode;
     function FindPathNode(Path: string): TrmTreeNode;
     function NodePath(Node: TrmTreeNode): string;
     function CustomSort(SortProc: TTVCompare; Data: Longint): Boolean;
     procedure FullExpand;
     procedure FullCollapse;
{$ifdef rmDebug}
    property OnMessage:TrmMsgEvent read fMsg write fMsg;
{$endif}
   end;

   TrmComboPathTreeView = class(TrmCustomComboPathTreeView)
   public
   { Public declarations }
     property Items;
     property SelectedNode;
   published
   { Published declarations }

     {$ifdef D4_OR_HIGHER}
     property Anchors;
     property Constraints;
     {$endif}
     property ComboStyle;
     property Enabled;
     property Font;
     property MaxLength;
     property ParentColor;
     property ParentCtl3D;
     property ParentFont;
     property ParentShowHint;
     property PopupMenu;
     property ShowHint;
     property TabOrder;
     property TabStop;
     property Visible;

     property AllowLeafOnly;
     property AutoExpand;
     property DropDownHeight;
     property DropDownWidth;
     property Images;
     property SepChar;
     property SortType;
     property StateImages;
     property OnDropDown;
     property OnChanged;
     property OnChanging;
     property OnCollapsed;
     property OnCollapsing;
     property OnCompare;
     property OnExpanding;
     property OnExpanded;
     property OnGetImageIndex;
     property OnGetSelectedIndex;
     property OnDestroy;
   end;

implementation

uses forms, sysutils;

{ TrmCustomComboTreeView }

constructor TrmCustomComboTreeView.Create(AOwner: TComponent);
begin
   inherited create(aowner);

   fSelectedNode := nil;

   OnBtn1Click := ToggleTreeView;
   OnExit := DoMyExit;
   readonly := true;
   fDropDownHeight := 0;
   fDropDownWidth := 0;
   fLeafOnly := false;

   with GetButton(1) do
   begin
      Font.name := 'Marlett';
      font.size := 10;
      Caption := '6';
      Glyph := nil;
   end;

   FScreenTreeview := TrmCustomScreenTreeView.create(nil);
   with FScreenTreeview do
   begin
      width := self.width;
      height := self.height * 8;
      visible := false;
      Parent := self;
      ReadOnly := true;
      OnClick := DoClick;
      OnKeyDown := DoMyKeyDown;
   end;
   FScreenTreeview.hide;
end;

function TrmCustomComboTreeView.CustomSort(SortProc: TTVCompare;
  Data: Integer): Boolean;
begin
   result := FScreenTreeview.CustomSort(SortProc, Data);
end;

procedure TrmCustomComboTreeView.DoClick(Sender: TObject);
var
   allowchange:boolean;
   wPoint : TPoint;
   HitTest : THitTests;
begin
   if not assigned(FScreenTreeview.Selected) then
      exit;

   wPoint := FScreenTreeview.ScreenToClient(Mouse.CursorPos);
   if not assigned(FScreenTreeview.GetNodeAt(wPoint.x, wPoint.y)) then
      exit;

   HitTest := FScreenTreeview.GetHitTestInfoAt(wPoint.x, wPoint.y);
   if not (htOnItem in HitTest) then
      exit;

   if not (fLeafOnly) or ((fLeafOnly) and (FScreenTreeview.Selected.Count = 0)) then
   begin
      allowchange := true;
      if Assigned(fChanging) then
         fChanging(self, FScreenTreeview.Selected, allowchange);
      if allowchange then
      begin
         fSelectedNode := FScreenTreeview.Selected;
         FScreenTreeview.hide;
         Text := fSelectedNode.Text;
         if assigned(fChanged) then
            fChanged(self, fSelectedNode);
      end
      else
         FScreenTreeview.Selected := fSelectedNode;
   end;
end;

procedure TrmCustomComboTreeView.DoMyKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
   AllowChange : boolean;
begin
   if (key = vk_escape) then
   begin
      FScreenTreeview.hide;
      self.setfocus;
      self.SelectAll;
   end
   else
   if (key = vk_Return) then
   begin
      if not assigned(FScreenTreeview.Selected) then
         exit;

      if not (fLeafOnly) or ((fLeafOnly) and (FScreenTreeview.Selected.Count = 0)) then
      begin
         allowchange := true;
         if Assigned(fChanging) then
            fChanging(self, FScreenTreeview.Selected, allowchange);
         if allowchange then
         begin
            fSelectedNode := FScreenTreeview.Selected;
            FScreenTreeview.hide;
            Text := fSelectedNode.Text;
            if assigned(fChanged) then
               fChanged(self, fSelectedNode);
            self.setfocus;
            self.SelectAll;
         end
         else
            FScreenTreeview.Selected := fSelectedNode;
      end;
   end
end;

procedure TrmCustomComboTreeView.DoMyExit(Sender: Tobject);
begin
   if FScreenTreeview.visible then
      FScreenTreeview.visible := false;
end;

procedure TrmCustomComboTreeView.FullCollapse;
begin
   FScreenTreeview.FullCollapse;  
end;

procedure TrmCustomComboTreeView.FullExpand;
begin
   FScreenTreeview.FullExpand;
end;

function TrmCustomComboTreeView.getAutoExpand: boolean;
begin
   result := FScreenTreeview.AutoExpand;
end;

function TrmCustomComboTreeView.GetCollapsedEvent: TTVExpandedEvent;
begin
   Result := FScreenTreeview.OnCollapsed;
end;

function TrmCustomComboTreeView.GetCollapsingEvent: TTVCollapsingEvent;
begin
   Result := FScreenTreeview.OnCollapsing;
end;

function TrmCustomComboTreeView.GetCompareEvent: TTVCompareEvent;
begin
   Result := FScreenTreeview.OnCompare;
end;

function TrmCustomComboTreeView.GetExpandedEvent: TTVExpandedEvent;
begin
   Result := FScreenTreeview.OnExpanded;
end;

function TrmCustomComboTreeView.GetExpandingEvent: TTVExpandingEvent;
begin
   result := FScreenTreeview.OnExpanding;
end;

function TrmCustomComboTreeView.GetImageIndexEvent: TTVExpandedEvent;
begin
   Result := FScreenTreeview.OnGetImageIndex;
end;

function TrmCustomComboTreeView.GetImages: TCustomImageList;
begin
   result := FScreenTreeview.images;
end;

function TrmCustomComboTreeView.GetItems: TTreeNodes;
begin
   Result := FScreenTreeview.Items;
end;

function TrmCustomComboTreeView.GetSelectedIndexEvent: TTVExpandedEvent;
begin
   Result := FScreenTreeview.OnGetSelectedIndex;
end;

function TrmCustomComboTreeView.GetSortType: TSortType;
begin
   Result := FScreenTreeview.SortType;
end;

function TrmCustomComboTreeView.GetStateImages: TCustomImageList;
begin
   result := FScreenTreeview.StateImages;
end;

procedure TrmCustomComboTreeView.KeyDown(var Key: Word;
   Shift: TShiftState);
begin
   if ((Key = VK_DOWN) or (key = VK_UP)) and (ssAlt in Shift) then
   begin
      if not FScreenTreeview.visible then
         ToggleTreeView(self)
      else
         FScreenTreeview.hide;
   end
   else
      inherited KeyDown(Key, Shift);
end;

procedure TrmCustomComboTreeView.setAutoExpand(const Value: boolean);
begin
   FScreenTreeview.AutoExpand := Value;
end;

procedure TrmCustomComboTreeView.SetCollapsedEvent(
   const Value: TTVExpandedEvent);
begin
   FScreenTreeview.OnCollapsed := Value;
end;

procedure TrmCustomComboTreeView.SetCollapsingEvent(
   const Value: TTVCollapsingEvent);
begin
   FScreenTreeview.OnCollapsing := Value;
end;

procedure TrmCustomComboTreeView.SetCompareEvent(
   const Value: TTVCompareEvent);
begin
   FScreenTreeview.OnCompare := Value;
end;

procedure TrmCustomComboTreeView.SetExpandedEvent(
   const Value: TTVExpandedEvent);
begin
   FScreenTreeview.OnExpanded := Value;
end;

procedure TrmCustomComboTreeView.SetExpandingEvent(
   const Value: TTVExpandingEvent);
begin
   FScreenTreeview.OnExpanding := Value;
end;

procedure TrmCustomComboTreeView.SetImageIndexEvent(
  const Value: TTVExpandedEvent);
begin
   FScreenTreeview.OnGetImageIndex := value;  
end;

procedure TrmCustomComboTreeView.SetImages(const Value: TCustomImageList);
begin
   FScreenTreeview.Images := value;
end;

procedure TrmCustomComboTreeView.SetSelectedIndexEvent(
  const Value: TTVExpandedEvent);
begin
   FScreenTreeview.OnGetSelectedIndex := value;  
end;

procedure TrmCustomComboTreeView.SetSelectedNode(const Value: TTreeNode);
begin
   fSelectedNode := Value;
   if assigned(fSelectedNode) then
      Text := fSelectedNode.Text;
end;

procedure TrmCustomComboTreeView.SetSortType(const Value: TSortType);
begin
   FScreenTreeview.SortType := value;
end;

procedure TrmCustomComboTreeView.SetStateImages(
   const Value: TCustomImageList);
begin
   FScreenTreeview.StateImages := value;
end;

procedure TrmCustomComboTreeView.ToggleTreeView(Sender: TObject);
var
   CP, SP: TPoint;
begin
   CP.X := Left;
   CP.Y := Top + Height;
   SP := parent.ClientToScreen(CP);

   SetFocus;
   SelectAll;

   with FScreenTreeview do
   begin
      if fDropDownWidth = 0 then
         Width := self.width
      else
         width := fDropDownWidth;

      if fDropDownHeight = 0 then
         Height := self.Height * 8
      else
         Height := fDropDownHeight;

      try
         Selected := fSelectedNode;
         if assigned(Selected) then
            Selected.MakeVisible;
      except
         Selected := nil;
      end;
      Left := SP.X;

      if assigned(screen.ActiveForm) then
      begin
        if (SP.Y + FScreenTreeview.height < screen.activeForm.Monitor.Height) then
          FScreenTreeview.Top := SP.Y
        else
          FScreenTreeview.Top := (SP.Y - self.height) - FScreenTreeview.height;
      end
      else
      begin
        if (SP.Y + FScreenTreeview.height < screen.Height) then
          FScreenTreeview.Top := SP.Y
        else
          FScreenTreeview.Top := (SP.Y - self.height) - FScreenTreeview.height;
      end;

      Show;
      SetWindowPos(handle, hwnd_topMost, 0, 0, 0, 0, swp_nosize or swp_NoMove);
   end; { Calendar }
end;

procedure TrmCustomComboTreeView.WndProc(var Message: TMessage);
begin
{$ifdef rmDebug}
  if assigned(OnMessage) then
  try
     OnMessage(Message);
  except
  end;
{$endif}

  case Message.Msg of
    WM_CHAR,
    WM_KEYDOWN,
    WM_KEYUP   : if (FScreenTreeview.visible) then
                 begin
                      if message.WParam = 9 then
                      begin
                        message.result := 0;
                        if message.msg = wm_keydown then
                           messagebeep(0);
                        exit;
                      end
                      else
                      if GetCaptureControl = nil then
                      begin
                         Message.result := SendMessage(FScreenTreeview.Handle, message.msg, message.wParam, message.LParam);
                         if message.result = 0 then exit;
                      end;
                 end;
  end;
  inherited WndProc(Message);
end;

destructor TrmCustomComboTreeView.Destroy;
begin
  if assigned(fOnDestroy) then
     fOnDestroy(self);
  FScreenTreeview.free;
  inherited;
end;

procedure TrmCustomComboTreeView.wmKillFocus(var Message: TMessage);
begin
   inherited;
   if FScreenTreeview.visible then
      FScreenTreeview.Hide;
end;

procedure TrmCustomComboTreeView.cmCancelMode(var Message: TCMCancelMode);
begin
   inherited;
   if Message.Sender = fScreenTreeView then
      exit;
   if FScreenTreeview.visible then
      FScreenTreeview.Hide;
end;

procedure TrmCustomComboTreeView.CMFontchanged(var Message: TMessage);
begin
   inherited;
   FScreenTreeview.Font.Assign(self.font);  
end;

procedure TrmCustomComboTreeView.wmGetDLGCode(var Message: TWMGetDlgCode);
begin
  if FScreenTreeview.Visible then
     message.Result := DLGC_WANTARROWS or DLGC_WANTTAB or DLGC_WANTCHARS or DLGC_WANTMESSAGE;
end;

{ TrmCustomComboPathTreeView }

function TrmCustomComboPathTreeView.AddPathNode(Node: TrmTreeNode;
   Path: string): TrmTreeNode;
begin
   result := FScreenTreeview.AddPathNode(Node, Path);
end;

constructor TrmCustomComboPathTreeView.Create(AOwner: TComponent);
begin
   inherited create(aowner);

   fSelectedNode := nil;
   ComboStyle := tvcsDropDownList;

   OnBtn1Click := ToggleTreeView;
   OnExit := DoMyExit;
   fDropDownHeight := 0;
   fDropDownWidth := 0;
   fLeafOnly := false;

   with GetButton(1) do
   begin
      Font.name := 'Marlett';
      font.size := 10;
      Caption := '6';
      Glyph := nil;
   end;

   FScreenTreeview := TrmCustomScreenPathTreeView.create(nil);
   with FScreenTreeview do
   begin
      width := self.width;
      height := self.height * 8;
      visible := false;
      Parent := self;
      ReadOnly := true;
      OnClick := DoClick;
      OnKeyDown := DoMyKeyDown;
   end;
   FScreenTreeview.hide;
end;

function TrmCustomComboPathTreeView.CustomSort(SortProc: TTVCompare;
  Data: Integer): Boolean;
begin
   result := FScreenTreeview.CustomSort(SortProc, Data);
end;

procedure TrmCustomComboPathTreeView.DoClick(Sender: TObject);
var
   allowchange:boolean;
   wPoint : TPoint;
   HitTest : THitTests;
begin
   if not assigned(FScreenTreeview.Selected) then
      exit;

   wPoint := FScreenTreeview.ScreenToClient(Mouse.CursorPos);
   if not assigned(FScreenTreeview.GetNodeAt(wPoint.x, wPoint.y)) then
      exit;

   HitTest := FScreenTreeview.GetHitTestInfoAt(wPoint.x, wPoint.y);
   if not (htOnItem in HitTest) then
      exit;

   if not (fLeafOnly) or ((fLeafOnly) and (FScreenTreeview.Selected.Count = 0)) then
   begin
      allowchange := true;
      if Assigned(fChanging) then
         fChanging(self, FScreenTreeview.Selected, allowchange);
      if allowchange then
      begin
         fSelectedNode := FScreenTreeview.Selected;
         FScreenTreeview.hide;
         Text := fSelectedNode.Text;
         if assigned(fChanged) then
            fChanged(self, fSelectedNode);
      end
      else
         FScreenTreeview.Selected := fSelectedNode;
   end;
end;

procedure TrmCustomComboPathTreeView.DoMyKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
   AllowChange : boolean;
begin
   if (key = vk_escape) then
   begin
      FScreenTreeview.hide;
      self.setfocus;
      self.SelectAll;
   end
   else
   if (key = vk_Return) then
   begin
      if not assigned(FScreenTreeview.Selected) then
         exit;

      if not (fLeafOnly) or ((fLeafOnly) and (FScreenTreeview.Selected.Count = 0)) then
      begin
         allowchange := true;
         if Assigned(fChanging) then
            fChanging(self, FScreenTreeview.Selected, allowchange);
         if allowchange then
         begin
            fSelectedNode := FScreenTreeview.Selected;
            FScreenTreeview.hide;
            Text := fSelectedNode.Text;
            if assigned(fChanged) then
               fChanged(self, fSelectedNode);
            self.setfocus;
            self.SelectAll;
         end
         else
            FScreenTreeview.Selected := fSelectedNode;
      end;
   end
end;

procedure TrmCustomComboPathTreeView.DoMyExit(Sender: Tobject);
begin
   if FScreenTreeview.visible then
      FScreenTreeview.visible := false;
end;

function TrmCustomComboPathTreeView.FindPathNode(Path: string): TrmTreeNode;
begin
   result := FScreenTreeview.FindPathNode(Path);
end;

procedure TrmCustomComboPathTreeView.FullCollapse;
begin
   FScreenTreeview.FullCollapse;  
end;

procedure TrmCustomComboPathTreeView.FullExpand;
begin
   FScreenTreeview.FullExpand;
end;

function TrmCustomComboPathTreeView.getAutoExpand: boolean;
begin
   result := FScreenTreeview.AutoExpand;
end;

function TrmCustomComboPathTreeView.GetCollapsedEvent: TrmTVExpandedEvent;
begin
   Result := FScreenTreeview.OnCollapsed;
end;

function TrmCustomComboPathTreeView.GetCollapsingEvent: TrmTVCollapsingEvent;
begin
   Result := FScreenTreeview.OnCollapsing;
end;

function TrmCustomComboPathTreeView.GetCompareEvent: TrmTVCompareEvent;
begin
   Result := FScreenTreeview.OnCompare;
end;

function TrmCustomComboPathTreeView.GetExpandedEvent: TrmTVExpandedEvent;
begin
   Result := FScreenTreeview.OnExpanded;
end;

function TrmCustomComboPathTreeView.GetExpandingEvent: TrmTVExpandingEvent;
begin
   result := FScreenTreeview.OnExpanding;
end;

function TrmCustomComboPathTreeView.GetImageIndexEvent: TrmTVExpandedEvent;
begin
   Result := FScreenTreeview.OnGetImageIndex;
end;

function TrmCustomComboPathTreeView.GetImages: TCustomImageList;
begin
   result := FScreenTreeview.images;
end;

function TrmCustomComboPathTreeView.GetItems: TrmTreeNodes;
begin
   Result := FScreenTreeview.Items;
end;

function TrmCustomComboPathTreeView.GetSelectedIndexEvent: TrmTVExpandedEvent;
begin
   Result := FScreenTreeview.OnGetSelectedIndex;
end;

function TrmCustomComboPathTreeView.GetSepChar: Char;
begin
   Result := FScreenTreeview.SepChar;
end;

function TrmCustomComboPathTreeView.GetSortType: TSortType;
begin
   Result := FScreenTreeview.SortType;
end;

function TrmCustomComboPathTreeView.GetStateImages: TCustomImageList;
begin
   result := FScreenTreeview.StateImages;
end;

procedure TrmCustomComboPathTreeView.KeyDown(var Key: Word;
   Shift: TShiftState);
begin
   if ((Key = VK_DOWN) or (key = VK_UP)) and (ssAlt in Shift) then
   begin
      if not FScreenTreeview.visible then
         ToggleTreeView(self)
      else
         FScreenTreeview.hide;
   end
   else
      inherited KeyDown(Key, Shift);
end;

function TrmCustomComboPathTreeView.NodePath(Node: TrmTreeNode): string;
begin
   result := FScreenTreeview.NodePath(Node);
end;

procedure TrmCustomComboPathTreeView.setAutoExpand(const Value: boolean);
begin
   FScreenTreeview.AutoExpand := Value;
end;

procedure TrmCustomComboPathTreeView.SetCollapsedEvent(
   const Value: TrmTVExpandedEvent);
begin
   FScreenTreeview.OnCollapsed := Value;
end;

procedure TrmCustomComboPathTreeView.SetCollapsingEvent(
   const Value: TrmTVCollapsingEvent);
begin
   FScreenTreeview.OnCollapsing := Value;
end;

procedure TrmCustomComboPathTreeView.SetCompareEvent(
   const Value: TrmTVCompareEvent);
begin
   FScreenTreeview.OnCompare := Value;
end;

procedure TrmCustomComboPathTreeView.SetExpandedEvent(
   const Value: TrmTVExpandedEvent);
begin
   FScreenTreeview.OnExpanded := Value;
end;

procedure TrmCustomComboPathTreeView.SetExpandingEvent(
   const Value: TrmTVExpandingEvent);
begin
   FScreenTreeview.OnExpanding := Value;
end;

procedure TrmCustomComboPathTreeView.SetImageIndexEvent(
  const Value: TrmTVExpandedEvent);
begin
   FScreenTreeview.OnGetImageIndex := value;
end;

procedure TrmCustomComboPathTreeView.SetImages(const Value: TCustomImageList);
begin
   FScreenTreeview.Images := value;
end;

procedure TrmCustomComboPathTreeView.SetSelectedIndexEvent(
  const Value: TrmTVExpandedEvent);
begin
   FScreenTreeview.OnGetSelectedIndex := value;
end;

procedure TrmCustomComboPathTreeView.SetSelectedNode(const Value: TrmTreeNode);
begin
   fSelectedNode := Value;
   if assigned(fSelectedNode) then
      Text := fSelectedNode.Text;
end;

procedure TrmCustomComboPathTreeView.SetSepChar(const Value: Char);
begin
   FScreenTreeview.SepChar := value;
end;

procedure TrmCustomComboPathTreeView.SetSortType(const Value: TSortType);
begin
   FScreenTreeview.SortType := value;
end;

procedure TrmCustomComboPathTreeView.SetStateImages(
   const Value: TCustomImageList);
begin
   FScreenTreeview.StateImages := value;
end;

procedure TrmCustomComboPathTreeView.ToggleTreeView(Sender: TObject);
var
   CP, SP: TPoint;
begin
   CP.X := Left;
   CP.Y := Top + Height;
   SP := parent.ClientToScreen(CP);

   SetFocus;
   SelectAll;

   with FScreenTreeview do
   begin
      if fDropDownWidth = 0 then
         Width := self.width
      else
         width := fDropDownWidth;

      if fDropDownHeight = 0 then
         Height := self.Height * 8
      else
         Height := fDropDownHeight;

      Selected := fSelectedNode;
      if assigned(selected) then
         Selected.MakeVisible;

      Left := SP.X;
      if assigned(screen.ActiveForm) then
      begin
        if (SP.Y + FScreenTreeview.height < screen.activeForm.Monitor.Height) then
          FScreenTreeview.Top := SP.Y
        else
          FScreenTreeview.Top := (SP.Y - self.height) - FScreenTreeview.height;
      end
      else
      begin
        if (SP.Y + FScreenTreeview.height < screen.Height) then
          FScreenTreeview.Top := SP.Y
        else
          FScreenTreeview.Top := (SP.Y - self.height) - FScreenTreeview.height;
      end;

      Show;
      SetWindowPos(handle, hwnd_topMost, 0, 0, 0, 0, swp_nosize or swp_NoMove);
   end; { Calendar }
end;

procedure TrmCustomComboPathTreeView.WndProc(var Message: TMessage);
begin
{$ifdef rmDebug}
  if assigned(OnMessage) then
  try
     OnMessage(Message);
  except
  end;
{$endif}

  case Message.Msg of
    WM_CHAR,
    WM_KEYDOWN,
    WM_KEYUP   : if (FScreenTreeview.visible) then
                 begin
                      if message.WParam = 9 then
                      begin
                        message.result := 0;
                        if message.msg = wm_keydown then
                           messagebeep(0);
                        exit;
                      end
                      else
                      if GetCaptureControl = nil then
                      begin
                         Message.result := SendMessage(FScreenTreeview.Handle, message.msg, message.wParam, message.LParam);
                         if (message.result = 0) then exit;
                      end;
                 end;
  end;
  inherited WndProc(Message);
end;

destructor TrmCustomComboPathTreeView.Destroy;
begin
  if assigned(fOnDestroy) then
     fOnDestroy(self);
  FScreenTreeview.free;
  inherited;
end;

procedure TrmCustomComboPathTreeView.cmCancelMode(
  var Message: TCMCancelMode);
begin
   inherited;
   if Message.Sender = fScreenTreeView then
      exit;

   if FScreenTreeview.visible then
      FScreenTreeview.Hide;

   CheckForModifiedText;
end;

procedure TrmCustomComboPathTreeView.wmKillFocus(var Message: TMessage);
begin
   inherited;
   if FScreenTreeview.visible then
      FScreenTreeview.Hide;

   CheckForModifiedText;
end;

procedure TrmCustomComboPathTreeView.CMFontchanged(var Message: TMessage);
begin
   inherited;
   FScreenTreeview.Font.assign(self.Font);
end;

procedure TrmCustomComboPathTreeView.SetComboStyle(
  const Value: TrmTreeViewComboStyle);
begin
  fComboStyle := Value;
  case fComboStyle of
    tvcsDropDown : readonly := false;
    tvcsDropDownList : readonly := true;
  end;
end;

procedure TrmCustomComboPathTreeView.CheckForModifiedText;
var
   wNode : TrmTreeNode;
begin
   if (fComboStyle = tvcsDropDown) and (Modified) then
   begin
      if fSelectedNode <> nil then
      begin
         if lowercase(fSelectedNode.Text) <> lowercase(text) then
            fSelectedNode := nil;
      end;

      if fSelectedNode = nil then
      begin
         wNode := FScreenTreeview.Items.GetFirstNode;
         while (wNode <> nil) and (lowercase(wNode.Text) <> lowercase(Text)) do
            wNode := wNode.GetNext;
         fSelectedNode := wNode;
      end;

      if fSelectedNode <> nil then
      begin
         if (fLeafOnly and (fSelectedNode.Count = 0)) or (not fLeafOnly) then
            text := fSelectedNode.Text
         else
         begin
            fSelectedNode := nil;
            messagebeep(0);
            setFocus;
            exit;
         end;
      end;

      modified := false;
   end;
end;

procedure TrmCustomComboPathTreeView.wmGetDLGCode(
  var Message: TWMGetDlgCode);
begin
  if FScreenTreeview.Visible then
     message.Result := DLGC_WANTARROWS or DLGC_WANTTAB or DLGC_WANTCHARS or DLGC_WANTMESSAGE;
end;

end.

