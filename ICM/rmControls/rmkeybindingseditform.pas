{================================================================================
Copyright (C) 1997-2002 Mills Enterprise

Unit     : rmKeyBindingsEditForm
Purpose  : Runtime editing form for looking at the assigned key bindings. 
Date     : 05-03-2000
Author   : Ryan J. Mills
Version  : 1.90
================================================================================}

unit rmKeyBindingsEditForm;

interface

{$I CompilerDefines.INC}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, rmLabel, rmKeyBindings, imglist, ActnList, rmTreeNonView, CheckLst;

type
  TFrmEditKeyBindings = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    lbxCategories: TListBox;
    lbxCommands: TListBox;
    btnClose: TButton;
    btnChange: TButton;
    btnResetAll: TButton;
    lblErrorInfo: TrmLabel;
    ActionList1: TActionList;
    actChange: TAction;
    actResetAll: TAction;
    GroupBox1: TGroupBox;
    lblDescription: TrmLabel;
    GroupBox2: TGroupBox;
    lblKeys: TrmLabel;
    cbxDesignLock: TCheckBox;
    btnSave: TButton;
    btnLoad: TButton;
    actSaveBindings: TAction;
    actLoadBindings: TAction;
    procedure ActionList1Update(Action: TBasicAction; var Handled: Boolean);
    procedure lbxCategoriesClick(Sender: TObject);
    procedure lbxCommandsDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure lbxCommandsClick(Sender: TObject);
    procedure actChangeExecute(Sender: TObject);
    procedure actResetAllExecute(Sender: TObject);
    procedure cbxDesignLockClick(Sender: TObject);
    procedure actSaveBindingsExecute(Sender: TObject);
    procedure actLoadBindingsExecute(Sender: TObject);
  private
    { Private declarations }
    fItems,fItems2: TrmKeyBindingCollection;
    fnvTree: TrmTreeNonView;
    fImages: TCustomImageList;
    fDisplayName: boolean;
    fModified: boolean;
    fDesigning: boolean;
    fMultiBinding: boolean;

    procedure SetItems(const Value: TrmKeyBindingCollection);
    function GetItems: TrmKeyBindingCollection;
    procedure SetImages(const Value: TCustomImageList);
    procedure UpdateKeyInfo(Index: integer);
    procedure BuildTree;
    procedure SetDesigning(const Value: boolean);
    procedure SaveBindingsToFile(FileName:string; Binary:Boolean);
    procedure LoadBindingsFromFile(FileName:string; Binary:boolean);
  protected
    { Protected declarations }
    procedure notification(AComponent: TComponent; Operation: TOperation); override;
  public
    { Public declarations }
    constructor create(AOwner: TComponent); override;
    destructor destroy; override;
    property Items: TrmKeyBindingCollection read GetItems write SetItems;
    property Images: TCustomImageList read fImages write SetImages;
    property DisplayName: boolean read fDisplayName write fDisplayName default false;
    property Designing : boolean read fDesigning write SetDesigning;
    property MultiBinding : boolean read fMultiBinding write fMultiBinding default true;
  end;

implementation

{$R *.DFM}

uses rmFormEditBinding,Menus;

constructor TFrmEditKeyBindings.create(AOwner: TComponent);
begin
  inherited;
  fnvTree := TrmTreeNonView.create(self);
  fitems := TrmKeyBindingCollection.create(self);
  fItems2 := TrmKeyBindingCollection.create(self);
  fDisplayName := false;
  fMultiBinding := true;
  fModified := false;
end;

destructor TFrmEditKeyBindings.destroy;
begin
  fnvTree.Items.Clear;
  fnvTree.Free;
  fItems2.clear;
  fItems2.free;
  fitems.Clear;
  fItems.Free;
  inherited;
end;

procedure TFrmEditKeyBindings.SetItems(const Value: TrmKeyBindingCollection);
begin
  fItems.clear;
  fItems2.clear;

  if assigned(value) then
  begin
    fItems.Assign(Value);
    fItems2.assign(value);
    BuildTree;
    if assigned(fImages) then
       lbxCommands.ItemHeight := fImages.Height;
  end;
end;

function TFrmEditKeyBindings.GetItems: TrmKeyBindingCollection;
begin
  result := fItems;
end;

procedure TFrmEditKeyBindings.SetImages(const Value: TCustomImageList);
begin
  fimages := value;
  if assigned(fImages) then
    fImages.FreeNotification(self);
end;

procedure TFrmEditKeyBindings.notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (operation = opRemove) then
  begin
    if aComponent = fImages then
      fimages := nil;
  end;

  inherited;
end;

procedure TFrmEditKeyBindings.ActionList1Update(Action: TBasicAction;
  var Handled: Boolean);
begin
   UpdateKeyInfo(lbxCommands.ItemIndex);

   if lbxCategories.itemindex = -1 then
      lbxCommands.clear;

   if not fDesigning then
      actChange.enabled := (lbxCommands.ItemIndex <> -1) and (not cbxDesignLock.checked)
   else
   begin
      actChange.Enabled := (lbxCommands.ItemIndex <> -1);
      cbxDesignLock.enabled := actChange.enabled;
   end;

   actResetAll.Enabled := fModified;

   if fModified then
     btnClose.ModalResult := mrok
   else
     btnClose.ModalResult := mrCancel;
   handled := true;
end;

procedure TFrmEditKeyBindings.lbxCategoriesClick(Sender: TObject);
var
  wSNode: TrmTreeNonViewNode;
begin
  lbxCommands.Clear;
  if lbxCategories.itemindex <> -1 then
  begin
    wSNode := fnvTree.Items.GetFirstNode;
    while (wSNode <> nil) and (wsNode.text <> lbxCategories.items[lbxCategories.ItemIndex]) do
      wsNode := wSNode.GetNextSibling;

    if wSNode <> nil then
    begin
      wSNode := wSNode.GetFirstChild;
      while wSNode <> nil do
      begin
        lbxCommands.Items.AddObject(wSNode.Text,TrmKeyBindingItem(wSNode.Data));
        wSNode := wSNode.GetNextSibling;
      end;
    end;
  end;
end;

procedure TFrmEditKeyBindings.lbxCommandsDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  wDisplayText: string;
  wKeyData : TrmKeyBindingItem;
begin
  lbxCommands.Canvas.Font.assign(lbxCommands.font);
  wKeyData := TrmKeyBindingItem(lbxCommands.Items.objects[index]);

  if odSelected in State then
  begin
    lbxCommands.Canvas.Brush.Color := clHighlight;
    lbxCommands.Canvas.Font.Color := clHighlightText;
  end
  else
  begin
    lbxCommands.Canvas.Brush.Color := clWindow;
    lbxCommands.Canvas.Font.Color := clWindowText;
  end;
  lbxCommands.Canvas.FillRect(rect);

  if (wKeyData.DesignLocked) then
  begin
     lbxCommands.Canvas.Font.Style := [fsitalic];
     lbxCommands.Canvas.Font.color := clGrayText;
  end;

  if assigned(fImages) then
  begin
     fImages.Draw(lbxCommands.Canvas,rect.left,rect.top,wKeyData.ImageIndex);
     rect.Left := rect.left + (fimages.width) + 1;
  end
  else
     rect.Left := rect.left + 1;


  if displayName then
    wDisplayText := wKeyData.ActionName
  else
    wDisplayText := StriphotKey(wKeyData.ActionCaption);

  lbxCommands.Canvas.TextRect(rect,rect.left,rect.top,wDisplayText);
end;

procedure TFrmEditKeyBindings.UpdateKeyInfo(Index: integer);
var
  wKeyData, wLoopData: TrmKeyBindingItem;
  wAlsoUsedBy: string;
  loop: integer;
begin
  if Index > -1 then
  begin
    wKeyData := TrmKeyBindingItem(lbxCommands.Items.objects[index]);
    lblKeys.caption := ShortCutToText(wKeyData.KeyBinding);
    if lblKeys.caption = '' then
       lblKeys.Caption := '(None)';
    lblDescription.caption := wKeyData.Description;
    cbxDesignLock.checked := wKeyData.DesignLocked;
    wAlsoUsedBy := '';
    if wKeyData.KeyBinding <> 0 then
    begin
      for loop := 0 to fItems.count - 1 do
      begin
        wLoopData := fItems[loop];
        if (wLoopData <> wKeyData) and (wKeyData.KeyBinding = wLoopData.KeyBinding) then
        begin
          if DisplayName then
            wAlsoUsedBy := ', ' + wLoopData.ActionName + wAlsoUsedBy
          else
            wAlsoUsedBy := ', "' + wLoopData.ActionCaption + '" ' + wAlsoUsedBy
        end;
      end;
      if wAlsoUsedBy <> '' then
      begin
        delete(wAlsoUsedBy,1,1);
        lblErrorInfo.Caption := 'The same binding is also used by' + wAlsoUsedBy;
      end
      else
        lblErrorInfo.Caption := '';
    end
    else
      lblErrorInfo.Caption := '';
  end
  else
  begin
    lblKeys.caption := '(None)';
    lblErrorInfo.caption := '';
    lblDescription.caption := '';
  end;
end;

procedure TFrmEditKeyBindings.lbxCommandsClick(Sender: TObject);
begin
  UpdateKeyInfo(lbxCommands.ItemIndex);
end;

procedure TFrmEditKeyBindings.actChangeExecute(Sender: TObject);
var
  wForm: TrmFrmEditBinding;
  wKeyData: TrmKeyBindingItem;
  wFound : boolean;
  loop : integer;
begin
  wForm := TrmFrmEditBinding.create(self);
  try
    wKeyData := TrmKeyBindingItem(lbxCommands.items.objects[lbxCommands.itemindex]);
    wForm.Binding := wKeyData.KeyBinding;
    if fMultiBinding then
    begin
       if wForm.showModal = mrok then
       begin
         wKeyData.KeyBinding := wForm.Binding;
         fModified := true;
       end;
    end
    else
    begin
       wFound := true;
       while wFound do
       begin
          if wForm.showModal = mrok then
          begin
            for loop := 0 to fitems.count-1 do
            begin
               wFound := (wForm.Binding = fItems[loop].KeyBinding);
               if wFound then
               begin
                  if not fDesigning then
                  begin
                     MessageDlg('That binding is already in use.', mtError, [mbok], 0);
                     break;
                  end
                  else
                  begin
                     if MessageDlg('That binding is already in use.'#13#10#13#10'Do you wish to set it anyways?', mtConfirmation, [mbyes, mbNo], 0) = idyes then
                        wFound := false;
                     break;
                  end;
               end
            end;
            if not wFound then
            begin
               wKeyData.KeyBinding := wForm.Binding;
               fModified := true;
            end;
          end
          else
          wFound := false;
       end;
    end;
  finally
    wForm.free;
  end;
end;

procedure TFrmEditKeyBindings.BuildTree;
var
  wPNode: TrmTreeNonViewNode;
  loop: integer;
  wkeydata: TrmKeyBindingItem;
begin
  fnvTree.Items.clear;
  lbxCommands.Clear;
  lbxCategories.Clear;
  for loop := 0 to fItems.Count - 1 do
  begin
    wPNode := fnvTree.Items.GetFirstNode;
    wKeyData := fItems[loop];

    while (wPNode <> nil) and (wPNode.Text <> wKeyData.Category) do
      wPNode := wPNode.GetNextSibling;

    if wPNode = nil then
    begin
      if pos('_hidden',lowercase(wKeyData.Category)) = 0 then
      begin
         wPNode := fnvTree.Items.Add(nil,wKeyData.Category);
         lbxCategories.items.add(wKeyData.Category);
      end;
    end;

    if wPNode <> nil then
       fnvTree.Items.AddChildObject(wPNode,wKeyData.ActionCaption,wKeyData);
  end;

end;

procedure TFrmEditKeyBindings.actResetAllExecute(Sender: TObject);
begin
  fItems.assign(fItems2);
  BuildTree;
  fModified := false;
end;

procedure TFrmEditKeyBindings.SetDesigning(const Value: boolean);
begin
  fDesigning := Value;
  if not fDesigning then
  begin
     cbxDesignLock.Visible := false;
     lblKeys.Align := alClient;
     btnSave.visible := false;
     btnLoad.visible := false;
  end;
end;

procedure TFrmEditKeyBindings.cbxDesignLockClick(Sender: TObject);
var
   wKeyData : TrmKeyBindingItem;
begin
   if not cbxDesignLock.Focused then exit;
   wKeyData := TrmKeyBindingItem(lbxCommands.Items.objects[lbxCommands.ItemIndex]);
   wKeyData.DesignLocked := not wKeyData.DesignLocked;
   fModified := true;
   lbxCommands.Invalidate;
end;

procedure TFrmEditKeyBindings.actSaveBindingsExecute(Sender: TObject);
var
   wSaveBinary : boolean;
   wFilename : string;
begin
   with TSaveDialog.create(nil) do
   try
      Title := 'Save bindings to file...';
      Filter := 'Binary File|*.bin|Text File|*.txt';
      if Execute then
      begin
         wSaveBinary := FilterIndex = 1;
         if wSaveBinary then
            wFileName := ChangeFileExt(filename,'.bin')
         else
            wFileName := ChangeFileExt(filename,'.txt');
         SaveBindingsToFile(wfilename, wSaveBinary);
      end;
   finally
      free;
   end;
end;

procedure TFrmEditKeyBindings.SaveBindingsToFile(FileName: string;
  Binary: Boolean);
var
   wStorage : TrmBindingStorage;
   wTemp : TMemoryStream;
   wStrm : TFileStream;
begin
   wStrm := TFileStream.Create(filename, fmCreate);
   try
      wStorage := TrmBindingStorage.create(self);
      try
         wStrm.Position := 0;
         wStorage.Items := fItems;
         if Binary then
            wStrm.WriteComponent(wStorage)
         else
         begin
            wTemp := TMemoryStream.create;
            try
               wTemp.WriteComponent(wStorage);
               wTemp.Position := 0;
               ObjectBinaryToText(wTemp, wStrm)
            finally
               wTemp.free;
            end;
         end;
      finally
         wStorage.free;
      end;
   finally
      wStrm.free;
   end;
end;

procedure TFrmEditKeyBindings.LoadBindingsFromFile(FileName: string; Binary: boolean);
var
   wStorage : TComponent;
   wTemp : TMemoryStream;
   wStrm : TFileStream;
begin
   wStrm := TFileStream.create(filename, fmOpenRead);
   try
      wStrm.Position := 0;

      if Binary then
         wStorage := TrmBindingStorage(wStrm.ReadComponent(nil))
      else
      begin
         wTemp := TMemoryStream.create;
         try
            ObjectTextToBinary(wStrm, wTemp);
            wTemp.position := 0;
            wStorage := TrmBindingStorage(wTemp.ReadComponent(nil));
         finally
            wTemp.free;
         end;
      end;

      try
         fItems.Assign(TrmBindingStorage(wStorage).items);
      finally
         wStorage.free;
      end;
   finally
      wStrm.free;
   end;
   BuildTree;
end;

procedure TFrmEditKeyBindings.actLoadBindingsExecute(Sender: TObject);
var
   wLoadBinary : boolean;
   wFilename : string;
begin
   with TOpenDialog.create(nil) do
   try
      Title := 'Load bindings from file...';
      Filter := 'Binary File|*.bin|Text File|*.txt';
      if Execute then
      begin
         wLoadBinary := FilterIndex = 1;
         if wLoadBinary then
            wFileName := ChangeFileExt(filename,'.bin')
         else
            wFileName := ChangeFileExt(filename,'.txt');
         LoadBindingsFromFile(wfilename, wLoadBinary);
      end;
   finally
      free;
   end;
end;

end.

