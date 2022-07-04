unit TB2KAddOnCustomizeForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, TB2Dock, TB2Toolbar, TB2Item, ComCtrls, Buttons,
  ExtDlgs, ExtCtrls;

type
  TTB2KCDOptions = set of (tcoCustomizeMenu, tcoCustomizeToolbar,
    tcoCustomizeDock, tcoCustomizeBitmap);

  TTB2KCDLanguage = (tclGerman, tclEnglish);

  TTB2KCustomizeDialog = class(TComponent)
  private
    { Variables for properties }
    FCaption: string;
    FOptions: TTB2KCDOptions;
    FBitmapFileNames: TStringList;
    FLanguage: TTB2KCDLanguage;

    { Private variables }
    DlgForm: TForm;
    MainForm: TForm;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Execute;
  published
    property Caption: string read FCaption write FCaption;
    property Options: TTB2KCDOptions read FOptions write FOptions;
    property BitmapFileNames: TStringList read FBitmapFileNames write
      FBitmapFileNames;
    property Language: TTB2KCDLanguage read FLanguage write FLanguage;
  end;

type
  TTB2KCustomizeForm = class(TForm)
    ddbToolbars: TComboBox;
    Label1: TLabel;
    cbToolbarSichtbar: TCheckBox;
    btnClose: TButton;
    cbIconOverCaption: TCheckBox;
    tvSelectedItems: TTreeView;
    tvUnselectedItems: TTreeView;
    Label2: TLabel;
    Label3: TLabel;
    PageControl1: TPageControl;
    tsToolbar: TTabSheet;
    tsDockbar: TTabSheet;
    Label4: TLabel;
    ddbDockbars: TComboBox;
    cbDockLimitToOneRow: TCheckBox;
    cbDockBmpOnToolbar: TCheckBox;
    cbDockAllowDrag: TCheckBox;
    OpenPictureDialog1: TOpenPictureDialog;
    Label5: TLabel;
    ddbBackgrounds: TComboBox;
    imBitmap: TImage;
    btnLoadBMP: TButton;
    procedure ddbToolbarsChange(Sender: TObject);
    procedure cbToolbarSichtbarClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure cbIconOverCaptionClick(Sender: TObject);
    procedure tvUnselectedItemsDblClick(Sender: TObject);
    procedure tvSelectedItemsDblClick(Sender: TObject);
    procedure EnableItems(ANode: TTreeNode; IsVisible: Boolean);
    procedure EnableParents(ANode: TTreeNode);
    procedure ddbDockbarsChange(Sender: TObject);
    procedure cbDockLimitToOneRowClick(Sender: TObject);
    procedure cbDockBmpOnToolbarClick(Sender: TObject);
    procedure cbDockAllowDragClick(Sender: TObject);
    procedure ddbBackgroundsChange(Sender: TObject);
    procedure btnLoadBMPClick(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
    FOptions: TTB2KCDOptions;
    FBitmapFileNames: TStringList;
    constructor Create(AOwner: TComponent); override;
    procedure FillDDBToolbars(AForm: TForm);
    procedure SetLanguage(ALanguage: TTB2KCDLanguage);
  end;

  ETB2KCustomizeDialog = class(Exception);

  { TTBCustomizeDialogItem }

  TTBCustomizeDialogItem = class(TTBCustomItem)
  private
    FDialog: TTB2KCustomizeDialog;
    procedure SetDialog(Value: TTB2KCustomizeDialog);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Click; override;
  published
    property Caption;
    property Dialog: TTB2KCustomizeDialog read FDialog write SetDialog;
    property DisplayMode;
    property Enabled;
    property HelpContext;
    property Hint;
    property ImageIndex;
    property Images;
    property InheritOptions;
    property MaskOptions;
    property Options;
    property ShortCut;
    property Visible;

    property OnClick;
    property OnSelect;
  end;

var
  TB2KCustomizeForm: TTB2KCustomizeForm;

implementation

{$R *.dfm}

{ TTB2KCustomizeForm }

constructor TTB2KCustomizeForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  tsToolbar.TabVisible := False;
  tsDockbar.TabVisible := False;
end;

procedure TTB2KCustomizeForm.SetLanguage(ALanguage: TTB2KCDLanguage);
begin
  if ALanguage = tclGerman then
  begin
    tsToolbar.Caption := 'Toolbar-Optionen';
    tsDockbar.Caption := 'Dockbar-Optionen';
    Label1.Caption := 'Toolbar:';
    Label2.Caption := 'Sichtbar:';
    Label3.Caption := 'Ausgeblendet:';
    cbToolbarSichtbar.Caption := 'Toolbar sichtbar';
    cbIconOverCaption.Caption := 'Text und Grafik anzeigen';
    Label4.Caption := 'Dockbar:';
    cbDockLimitToOneRow.Caption := 'Auf eine Reihe/Spalte beschränken';
    cbDockAllowDrag.Caption := 'An-/Abdocken von Elementen erlauben';
    cbDockBmpOnToolbar.Caption := 'Grafik auch in Toolbar anzeigen';
    Label5.Caption := 'Grafik-Element:';
    btnClose.Caption := 'Schliessen';
    OpenPictureDialog1.Title := 'Hintergrundgrafik auswählen';
  end;

  if ALanguage = tclEnglish then
  begin
    tsToolbar.Caption := 'Toolbar-Options';
    tsDockbar.Caption := 'Dockbar-Options';
    Label1.Caption := 'Toolbar:';
    Label2.Caption := 'Visible:';
    Label3.Caption := 'Invisible:';
    cbToolbarSichtbar.Caption := 'Toolbar visible';
    cbIconOverCaption.Caption := 'Show Text and Bitmap';
    Label4.Caption := 'Dockbar:';
    cbDockLimitToOneRow.Caption := 'Limit to one Row/Column';
    cbDockAllowDrag.Caption := 'Allow Drag-N-Drop';
    cbDockBmpOnToolbar.Caption := 'Show Bitmap in Toolbar';
    Label5.Caption := 'Backgr.element:';
    btnClose.Caption := 'Close';
    OpenPictureDialog1.Title := 'Select Background-Bitmap';
  end;

end;

procedure TTB2KCustomizeForm.FillDDBToolbars(AForm: TForm);
var
  i: Integer;
begin
  ddbToolbars.Clear;
  ddbToolbars.Items.Clear;

  for i := 0 to Pred(AForm.ComponentCount) do
  begin
    if AForm.Components[i].ClassNameIs('TTBToolbar') then
    begin
      if ((tcoCustomizeMenu in FOptions) and
        (TTBToolbar(AForm.Components[i]).MenuBar)) or
        ((tcoCustomizeToolbar in FOptions) and (not
        TTBToolbar(AForm.Components[i]).MenuBar)) then
      begin
        ddbToolbars.Items.AddObject(
          TTBToolbar(AForm.Components[i]).Caption, AForm.Components[i]);
      end;
    end;

    if AForm.Components[i].ClassNameIs('TTBDock') then
    begin
      if (tcoCustomizeDock in FOptions) then
      begin
        ddbDockbars.Items.AddObject(
          TTBDock(AForm.Components[i]).Name, AForm.Components[i]);
      end;
    end;

    if AForm.Components[i].ClassNameIs('TTBBackground') then
    begin
      if (tcoCustomizeDock in FOptions) and
        (tcoCustomizeBitmap in FOptions) then
      begin
        ddbBackgrounds.Items.AddObject(
          TTBBackground(AForm.Components[i]).Name, AForm.Components[i]);
      end;
    end;
  end;

  if ddbToolbars.Items.Count > 0 then
  begin
    ddbToolbars.ItemIndex := 0;
    ddbToolbarsChange(ddbToolbars);
    tsToolbar.TabVisible := True;
    PageControl1.ActivePage := tsToolbar;
  end;

  if ddbDockbars.Items.Count > 0 then
  begin
    if ddbBackgrounds.Items.Count = 0 then
    begin
      Label5.Visible := False;
      ddbBackgrounds.Visible := False;
      btnLoadBMP.Visible := False;
      imBitmap.Visible := False;
    end;

    ddbDockbars.ItemIndex := 0;
    ddbDockbarsChange(ddbDockbars);
    tsDockbar.TabVisible := True;

    if ddbToolbars.Items.Count > 0 then
    begin
      PageControl1.ActivePage := tsToolbar;
    end
    else
    begin
      PageControl1.ActivePage := tsDockbar;
    end;
  end;
end;

procedure TTB2KCustomizeForm.ddbToolbarsChange(Sender: TObject);

  function GetCaption(AItem: TTBCustomItem): string;
  begin
    if AItem.ClassNameIs('TTBSeparatorItem') then
    begin
      result := '----------';
      exit;
    end;
    if AItem.Caption = '' then
    begin
      result := '???';
      exit;
    end;
    result := AItem.Caption;
  end;

  function FindTreeNode(ATreeView: TTreeView; AList: TStringList): TTreeNode;
  var
    i: Integer;
    tn, pn: TTreeNode;
    nList: TStringList;
  begin
    for i := 0 to Pred(ATreeView.Items.Count) do
    begin
      if TTBCustomItem(ATreeView.Items.Item[i].Data).Name =
        TTBCustomItem(AList.Objects[Pred(AList.Count)]).Name then
      begin
        result := ATreeView.Items.Item[i];
        exit;
      end;
    end;
    if AList.Count > 1 then
    begin
      nList := TStringList.Create;
      for i := 0 to Pred(Pred(AList.Count)) do
      begin
        nList.AddObject(AList.Strings[i], AList.Objects[i]);
      end;
      pn := FindTreeNode(ATreeView, nList);
      tn := ATreeView.Items.AddChildObject(pn,
        GetCaption(TTBCustomItem(AList.Objects[Pred(AList.Count)])),
        TTBCustomItem(AList.Objects[Pred(AList.Count)]));
      tn.ImageIndex :=
        TTBCustomItem(AList.Objects[Pred(AList.Count)]).ImageIndex;
      tn.SelectedIndex :=
        TTBCustomItem(AList.Objects[Pred(AList.Count)]).ImageIndex;
      nList.Free;
      result := tn;
      exit;
    end
    else
    begin
      tn := ATreeView.Items.AddChildObject(nil,
        GetCaption(TTBCustomItem(AList.Objects[Pred(AList.Count)])),
        TTBCustomItem(AList.Objects[Pred(AList.Count)]));
      tn.ImageIndex :=
        TTBCustomItem(AList.Objects[Pred(AList.Count)]).ImageIndex;
      tn.SelectedIndex :=
        TTBCustomItem(AList.Objects[Pred(AList.Count)]).ImageIndex;
      result := tn;
      exit;
    end;
    result := nil;
  end;

  procedure AddItemToTV(AItem: TTBCustomItem; AList: TStringList);
  var
    i: Integer;
    tn, pn: TTreeNode;
  begin
    if (AItem.Name = '') then
      exit;

    if AItem.Visible then
    begin
      if AList.Count > 0 then
      begin
        pn := FindTreeNode(tvSelectedItems, AList);
      end
      else
      begin
        pn := nil;
      end;
      tn := tvSelectedItems.Items.AddChildObject(pn, GetCaption(AItem), AItem);
      tn.ImageIndex := AItem.ImageIndex;
      tn.SelectedIndex := AItem.ImageIndex;
    end
    else
    begin
      if AList.Count > 0 then
      begin
        pn := FindTreeNode(tvUnselectedItems, AList);
      end
      else
      begin
        pn := nil;
      end;
      tn := tvUnselectedItems.Items.AddChildObject(pn, GetCaption(AItem),
        AItem);
      tn.ImageIndex := AItem.ImageIndex;
      tn.SelectedIndex := AItem.ImageIndex;
    end;
    if (AItem.Count > 0) or (AItem.LinkSubitems <> nil) then
    begin
      AList.AddObject(AItem.Name, AItem);
      if AItem.LinkSubitems <> nil then
      begin
        for i := 0 to Pred(AItem.LinkSubitems.Count) do
        begin
          AddItemToTV(AItem.LinkSubitems.Items[i], AList);
        end;
      end
      else
      begin
        for i := 0 to Pred(AItem.Count) do
        begin
          AddItemToTV(AItem.Items[i], AList);
        end;
      end;
      AList.Delete(Pred(AList.Count));
    end;
  end;

  procedure AddItemToLV(AItem: TTBCustomItem);
  var
    tn: TTreeNode;
  begin
    if AItem.Visible then
    begin
      tn := tvSelectedItems.Items.AddObject(nil, GetCaption(AItem), AItem);
      tn.ImageIndex := AItem.ImageIndex;
      tn.SelectedIndex := AItem.ImageIndex;
    end
    else
    begin
      tn := tvUnselectedItems.Items.AddObject(nil, GetCaption(AItem), AItem);
      tn.ImageIndex := AItem.ImageIndex;
      tn.SelectedIndex := AItem.ImageIndex;
    end;
  end;

var
  i: Integer;
  toolbar: TTBToolbar;
  list: TStringList;
begin
  list := TStringList.Create;
  tvSelectedItems.Items.Clear;
  tvUnselectedItems.Items.Clear;

  if ddbToolbars.ItemIndex >= 0 then
  begin
    toolbar := TTBToolbar(ddbToolbars.Items.Objects[ddbToolbars.ItemIndex]);
    cbToolbarSichtbar.Checked := toolbar.Visible;
    cbToolbarSichtbar.Enabled := (toolbar.CloseButton) or
      (toolbar.CloseButtonWhenDocked);

    if toolbar.MenuBar then
    begin
      tvSelectedItems.ShowButtons := True;
      tvSelectedItems.ShowLines := True;
      tvUnselectedItems.ShowButtons := True;
      tvUnselectedItems.ShowLines := True;
    end
    else
    begin
      tvSelectedItems.ShowButtons := False;
      tvSelectedItems.ShowLines := False;
      tvUnselectedItems.ShowButtons := False;
      tvUnselectedItems.ShowLines := False;
    end;
    tvSelectedItems.Images := toolbar.Images;
    tvUnselectedItems.Images := toolbar.Images;
    if toolbar.LinkSubitems <> nil then
    begin
      if toolbar.LinkSubitems.SubMenuImages <> nil then
      begin
        tvSelectedItems.Images := toolbar.LinkSubitems.SubMenuImages;
        tvUnselectedItems.Images := toolbar.LinkSubitems.SubMenuImages;
      end;
      cbIconOverCaption.Checked := (tboImageAboveCaption in
        toolbar.linkSubitems.Options);
      if toolbar.MenuBar then
      begin
        for i := 0 to Pred(toolbar.LinkSubitems.Count) do
        begin
          AddItemToTV(toolbar.LinkSubitems.Items[i], list);
        end;
      end
      else
      begin
        for i := 0 to Pred(toolbar.LinkSubitems.Count) do
        begin
          AddItemToLV(toolbar.LinkSubitems.Items[i]);
        end;
      end;
    end
    else
    begin
      cbIconOverCaption.Checked := (tboImageAboveCaption in toolbar.Options);
      cbIconOverCaption.Enabled := not toolbar.MenuBar;
      if toolbar.MenuBar then
      begin
        for i := 0 to Pred(toolbar.Items.Count) do
        begin
          AddItemToTV(toolbar.Items.Items[i], list);
        end;
      end
      else
      begin
        for i := 0 to Pred(toolbar.Items.Count) do
        begin
          AddItemToLV(toolbar.Items.Items[i]);
        end;
      end;
    end;
  end;
  list.Free;
end;

procedure TTB2KCustomizeForm.ddbDockbarsChange(Sender: TObject);
var
  dock: TTBDock;
begin
  if ddbDockbars.ItemIndex >= 0 then
  begin
    dock := TTBDock(ddbDockbars.Items.Objects[ddbDockbars.ItemIndex]);
    cbDockLimitToOneRow.Checked := dock.LimitToOneRow;
    cbDockAllowDrag.Checked := dock.AllowDrag;
    cbDockBmpOnToolbar.Checked := dock.BackgroundOnToolbars;
    if dock.Background = nil then
    begin
      ddbBackgrounds.ItemIndex := -1;
    end
    else
    begin
      ddbBackgrounds.ItemIndex :=
        ddbBackgrounds.Items.IndexOfObject(dock.Background);
    end;
    ddbBackgroundsChange(ddbBackgrounds);
  end;
end;

procedure TTB2KCustomizeForm.ddbBackgroundsChange(Sender: TObject);
var
  background: TTBBackground;
  dock: TTBDock;
begin
  if (ddbDockbars.ItemIndex >= 0) and (tcoCustomizeBitmap in FOptions) then
  begin
    dock := TTBDock(ddbDockbars.Items.Objects[ddbDockbars.ItemIndex]);
    if ddbBackgrounds.ItemIndex >= 0 then
    begin
      background :=
        TTBBackground(ddbBackgrounds.Items.Objects[ddbBackgrounds.ItemIndex]);
      btnLoadBMP.Enabled := True;
      imBitmap.Picture.Bitmap := background.Bitmap;
      if dock.Background <> background then
      begin
        dock.Background := background;
      end;
    end
    else
    begin
      btnLoadBMP.Enabled := False;
      imBitmap.Picture := nil;
      if dock.Background <> nil then
      begin
        dock.Background := nil;
      end;
    end;
  end;
end;

procedure TTB2KCustomizeForm.EnableItems(ANode: TTreeNode; IsVisible: Boolean);
var
  i: Integer;
begin
  TTBCustomItem(ANode.Data).Visible := IsVisible;
  if (not IsVisible) and
    ((TTBCustomItem(ANode.Data).ClassNameIs('TTBCustomizeDialogItem')) or
    (TTBCustomItem(ANode.Data).ClassNameIs('TTBAVListItem')) or
    (TTBCustomItem(ANode.Data).ClassNameIs('TTBMRUListItem')) or
    (TTBCustomItem(ANode.Data).ClassNameIs('TTBMDIWindowItem'))) then
  begin
    EnableParents(ANode);
  end;
  if ANode.Count > 0 then
  begin
    for i := 0 to Pred(ANode.Count) do
    begin
      EnableItems(ANode.Item[i], IsVisible);
    end;
  end;
end;

procedure TTB2KCustomizeForm.EnableParents(ANode: TTreeNode);
begin
  if (ANode = nil) or (TTBCustomItem(ANode.Data).Visible = True) then
    exit;
  TTBCustomItem(ANode.Data).Visible := True;
  EnableParents(ANode.Parent);
end;

procedure TTB2KCustomizeForm.tvSelectedItemsDblClick(Sender: TObject);

begin
  if tvSelectedItems.Selected <> nil then
  begin
    TTBCustomItem(tvSelectedItems.Selected.Data).Visible := False;
    if tvSelectedItems.ShowButtons then
    begin
      EnableItems(tvSelectedItems.Selected, False);
    end;
    ddbToolbarsChange(Sender);
  end;
end;

procedure TTB2KCustomizeForm.tvUnselectedItemsDblClick(Sender: TObject);
begin
  if tvUnselectedItems.Selected <> nil then
  begin
    TTBCustomItem(tvUnselectedItems.Selected.Data).Visible := True;
    if tvUnselectedItems.ShowButtons then
    begin
      EnableItems(tvUnselectedItems.Selected, True);
      EnableParents(tvUnselectedItems.Selected.Parent);
    end;
    ddbToolbarsChange(Sender);
  end;
end;

procedure TTB2KCustomizeForm.cbToolbarSichtbarClick(Sender: TObject);
var
  toolbar: TTBToolbar;
begin
  if ddbToolbars.ItemIndex >= 0 then
  begin
    toolbar := TTBToolbar(ddbToolbars.Items.Objects[ddbToolbars.ItemIndex]);
    toolbar.Visible := cbToolbarSichtbar.Checked;
  end;
end;

procedure TTB2KCustomizeForm.cbIconOverCaptionClick(Sender: TObject);
var
  toolbar: TTBToolbar;
begin
  if ddbToolbars.ItemIndex >= 0 then
  begin
    toolbar := TTBToolbar(ddbToolbars.Items.Objects[ddbToolbars.ItemIndex]);
    if toolbar.LinkSubitems <> nil then
    begin
      if cbIconOverCaption.Checked then
      begin
        toolbar.LinkSubitems.Options := toolbar.LinkSubitems.Options +
          [tboImageAboveCaption];
      end
      else
      begin
        toolbar.LinkSubitems.Options := toolbar.LinkSubitems.Options -
          [tboImageAboveCaption];
      end;
    end
    else
    begin
      if cbIconOverCaption.Checked then
      begin
        toolbar.Options := toolbar.Options + [tboImageAboveCaption];
      end
      else
      begin
        toolbar.Options := toolbar.Options - [tboImageAboveCaption];
      end;
    end;
  end;
end;

procedure TTB2KCustomizeForm.cbDockLimitToOneRowClick(Sender: TObject);
var
  dock: TTBDock;
begin
  if ddbDockbars.ItemIndex >= 0 then
  begin
    dock := TTBDock(ddbDockbars.Items.Objects[ddbDockbars.ItemIndex]);
    dock.LimitToOneRow := cbDockLimitToOneRow.Checked;
    dock.Realign;
  end;
end;

procedure TTB2KCustomizeForm.cbDockBmpOnToolbarClick(Sender: TObject);
var
  dock: TTBDock;
begin
  if ddbDockbars.ItemIndex >= 0 then
  begin
    dock := TTBDock(ddbDockbars.Items.Objects[ddbDockbars.ItemIndex]);
    dock.BackgroundOnToolbars := cbDockBmpOnToolbar.Checked;
    dock.Repaint;
  end;
end;

procedure TTB2KCustomizeForm.cbDockAllowDragClick(Sender: TObject);
var
  dock: TTBDock;
begin
  if ddbDockbars.ItemIndex >= 0 then
  begin
    dock := TTBDock(ddbDockbars.Items.Objects[ddbDockbars.ItemIndex]);
    dock.AllowDrag := cbDockAllowDrag.Checked;
  end;
end;

procedure TTB2KCustomizeForm.btnLoadBMPClick(Sender: TObject);
var
  background: TTBBackground;
begin
  if OpenPictureDialog1.Execute then
  begin
    background :=
      TTBBackground(ddbBackgrounds.Items.Objects[ddbBackgrounds.ItemIndex]);
    background.Bitmap.LoadFromFile(OpenPictureDialog1.FileName);
    if FBitmapFileNames.IndexOfName(background.Name) = -1 then
    begin
      FBitmapFileNames.Append(background.Name + '=' +
        OpenPictureDialog1.FileName);
    end
    else
    begin
      FBitmapFileNames.Values[background.Name] := OpenPictureDialog1.FileName;
    end;
    imBitmap.Picture.Bitmap := background.Bitmap;
  end;
end;

procedure TTB2KCustomizeForm.btnCloseClick(Sender: TObject);
begin
  Self.Close;
end;

{ TTB2KCustomizeDialog }

resourcestring
  // Error messages
  sErrFormRequired = 'TB2KCustomizeDialog components must be placed on a form';
  sErrSingleInstance = 'Only one TB2KCustomizeDialog component is permitted on a '
    + 'form: %s is already present on %s';

constructor TTB2KCustomizeDialog.Create(AOwner: TComponent);
var
  i: Integer;
begin
  if not Assigned(AOwner) or not (AOwner is TForm) then
    raise ETB2KCustomizeDialog.Create(sErrFormRequired);
  MainForm := AOwner as TForm;
  Include(FOptions, tcoCustomizeToolbar);
  // Ensure there is only one TTB2KCustomizeDialog component on a form
  for i := 0 to Pred(AOwner.ComponentCount) do
    if AOwner.Components[i] is TTB2KCustomizeDialog then
      raise ETB2KCustomizeDialog.CreateFmt(sErrSingleInstance,
        [AOwner.Components[i].Name, AOwner.Name]);
  inherited Create(AOwner);
  if FBitmapFileNames = nil then
    FBitmapFileNames := TStringList.Create;
  FLanguage := tclGerman;
end;

destructor TTB2KCustomizeDialog.Destroy;
begin
  MainForm := nil;
  FBitmapFileNames.Free;
  inherited;
end;

procedure TTB2KCustomizeDialog.Execute;
begin
  if FOptions = [] then
    exit;
  DlgForm := TTB2KCustomizeForm.Create(Application);
  try
    if FCaption <> '' then
      DlgForm.Caption := FCaption;
    TTB2KCustomizeForm(DlgForm).FOptions := FOptions;
    TTB2KCustomizeForm(DlgForm).FBitmapFileNames := FBitmapFileNames;
    TTB2KCustomizeForm(DlgForm).FillDDBToolbars(MainForm);
    TTB2KCustomizeForm(DlgForm).SetLanguage(FLanguage);
    DlgForm.ShowModal;
    DlgForm.Free;
  except
    DlgForm.Free;
    raise;
  end;
end;

{ TTBCustomizeDialogItem }

constructor TTBCustomizeDialogItem.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Caption := 'Anpassen';
end;

procedure TTBCustomizeDialogItem.Click;
begin
  if Assigned(FDialog) then
    FDialog.Execute;
  inherited;
end;

procedure TTBCustomizeDialogItem.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FDialog) then
    Dialog := nil;
end;

procedure TTBCustomizeDialogItem.SetDialog(Value: TTB2KCustomizeDialog);
begin
  if FDialog <> Value then
  begin
    FDialog := Value;
    if Assigned(Value) then
    begin
      Value.FreeNotification(Self);
    end;
  end;
end;

end.

