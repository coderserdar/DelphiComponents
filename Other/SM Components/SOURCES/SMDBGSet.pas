{ Copyright (C) 1998-2005, written by Mike Shkolnik, Scalabium Software
  E-Mail:  mshkolnik@scalabium.com
           mshkolnik@yahoo.com
  WEB: http://www.scalabium.com

  In this unit I described the visual dialog for TDBGrid's Columns property tuning.
}

unit SMDBGSet;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, stdctrls, extctrls, checklst, DBGrids, Menus, Buttons;

type
  TSMSetDBGridDialog = class(TComponent)
  private
    { Private declarations }
    FCaption: TCaption;
    FDBGrid: TCustomDBGrid;
    FAllowedFields: TStrings;

    FOnBeforeExecute: TNotifyEvent;
    FOnAfterExecute: TNotifyEvent;
    FOnShow: TNotifyEvent;

    procedure SetAllowedFields(Value: TStrings);
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: Boolean;
  published
    { Published declarations }
    property Caption: TCaption read FCaption write FCaption;
    property DBGrid: TCustomDBGrid read FDBGrid write FDBGrid;
    property AllowedFields: TStrings read FAllowedFields write SetAllowedFields;

    property OnBeforeExecute: TNotifyEvent read FOnBeforeExecute write FOnBeforeExecute;
    property OnAfterExecute: TNotifyEvent read FOnAfterExecute write FOnAfterExecute;
    property OnShow: TNotifyEvent read FOnShow write FOnShow;
  end;

  TSMGridSetupItem = class
    FieldIndex: Integer;
    FieldName: string;

    TitleAlignment: TAlignment;
    TitleCaption: string;
    TitleColor: TColor;
    TitleFont: TFont;

    DataAlignment: TAlignment;
    DataColor: TColor;
    DataFont: TFont;
    Width: Integer;
  end;

  TfrmGridSetup = class(TForm)
    btnOk: TButton;
    btnCancel: TButton;
    bvlButton: TBevel;
    clbFields: TCheckListBox;
    gbTitle: TGroupBox;
    lblTitleCaption: TLabel;
    lblTitleAlignment: TLabel;
    lblTitleColor: TLabel;
    lblTitleFont: TLabel;
    edTitleCaption: TEdit;
    edTitleFont: TEdit;
    cbTitleAlignment: TComboBox;
    gbData: TGroupBox;
    lblDataAlignment: TLabel;
    lblDataColor: TLabel;
    lblDataFont: TLabel;
    edDataFont: TEdit;
    cbDataAlignment: TComboBox;
    lblWidth: TLabel;
    lblWidthFix: TLabel;
    edWidth: TEdit;
    FontDlg: TFontDialog;
    SMColorsCBTitle: TComboBox;
    SMColorsCBData: TComboBox;
    btnTitleFont: TButton;
    btnDataFont: TButton;
    pmColumns: TPopupMenu;
    miSelectAll: TMenuItem;
    miUnselectAll: TMenuItem;
    miInvertSelection: TMenuItem;
    btnApplyAll: TButton;
    procedure PropertyExit(Sender: TObject);
    procedure clbFieldsClick(Sender: TObject);
    procedure clbFieldsDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure clbFieldsDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure edTitleFontButtonClick(Sender: TObject);
    procedure SMColorsCBTitleDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure miSelectAllClick(Sender: TObject);
    procedure SMColorsCBTitleClick(Sender: TObject);
    procedure SMColorsCBTitleChange(Sender: TObject);
    procedure btnApplyAllClick(Sender: TObject);
  private
    { Private declarations }
    function GetItemCaption(item: TSMGridSetupItem): string;
    function GetCaptionFont(Font: TFont): string;
  public
    { Public declarations }
  end;

procedure Register;

implementation
{$R *.DFM}
uses SMCnst;

procedure Register;
begin
  RegisterComponents('SMComponents', [TSMSetDBGridDialog]);
end;

const
  clCream = TColor($A6CAF0);
  clMoneyGreen = TColor($C0DCC0);
  clSkyBlue = TColor($FFFBF0);

  ColorsInList = 46;

var
  ColorValues: array [0..ColorsInList - 1] of TColor = (
    clBlack, clMaroon, clGreen, clOlive, clNavy, clPurple, clTeal, clGray,
    clSilver, clRed, clLime, clYellow, clBlue, clFuchsia, clAqua, clWhite,
    clScrollBar, clBackground, clActiveCaption, clInactiveCaption,
    clMenu, clWindow, clWindowFrame, clMenuText, clWindowText, clCaptionText,
    clActiveBorder, clInactiveBorder, clAppWorkSpace, clHighlight,
    clHighlightText, clBtnFace, clBtnShadow, clGrayText, clBtnText,
    clInactiveCaptionText, clBtnHighlight, cl3DDkShadow, cl3DLight,
    clInfoText, clInfoBk, clCream, clMoneyGreen, clSkyBlue, 0, 0);


{ TSMSetDBGridDialog }
constructor TSMSetDBGridDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FCaption := 'Grid setup';
  FDBGrid := nil;
  FAllowedFields := TStringList.Create;
end;

destructor TSMSetDBGridDialog.Destroy;
begin
  FAllowedFields.Free;

  inherited Destroy;
end;

procedure TSMSetDBGridDialog.SetAllowedFields(Value: TStrings);
begin
  FAllowedFields.Assign(Value);
end;

type
  THackDBGrid = class(TDBGrid);
  
function TSMSetDBGridDialog.Execute: Boolean;
var
  i, j: Integer;
  item: TSMGridSetupItem;
  TM: TTextMetric;
  IsSMDBGrid: Boolean;
begin
  if Assigned(FOnBeforeExecute) then
    FOnBeforeExecute(Self);

  Result := False;

  with TDBGrid(DBGrid), TfrmGridSetup.Create(Application) do
    try
      if not (Assigned(DataSource) and
         Assigned(DataSource.DataSet)) then exit;

      Caption := FCaption;
      btnApplyAll.Caption := SApplyAll;
      btnOk.Caption := SBtnOk;
      btnCancel.Caption := SBtnCancel;

      gbTitle.Caption := SgbTitle;
      lblTitleCaption.Caption := STitleCaption;
      lblTitleAlignment.Caption := STitleAlignment;
      cbTitleAlignment.Items.Add(SAlignLeft);
      cbTitleAlignment.Items.Add(SAlignRight);
      cbTitleAlignment.Items.Add(SAlignCenter);
      lblTitleColor.Caption := STitleColor;
      lblTitleFont.Caption := STitleFont;

      gbData.Caption := SgbData;
      lblDataAlignment.Caption := STitleAlignment;
      cbDataAlignment.Items.Add(SAlignLeft);
      cbDataAlignment.Items.Add(SAlignRight);
      cbDataAlignment.Items.Add(SAlignCenter);
      lblDataColor.Caption := STitleColor;
      lblDataFont.Caption := STitleFont;

      lblWidth.Caption := SWidth;
      lblWidthFix.Caption := SWidthFix;


      {fill the field list}
      with Columns do
        for i := 0 to Count-1 do
        begin
          if (AllowedFields.Count = 0) or
             (AllowedFields.IndexOf(Items[i].FieldName) > -1) then
          begin
            item := TSMGridSetupItem.Create;
            item.TitleFont := TFont.Create;
            item.DataFont := TFont.Create;

            item.FieldIndex := i;
            item.FieldName := Items[i].FieldName;

            item.TitleAlignment := Items[i].Title.Alignment;
            item.TitleCaption := Items[i].Title.Caption;
            item.TitleColor := Items[i].Title.Color;
            item.TitleFont.Assign(Items[i].Title.Font);

            item.DataAlignment := Items[i].Alignment;
            item.DataColor := Items[i].Color;
            item.DataFont.Assign(Items[i].Font);

            if (Items[i].Width > 0) then
            begin
              GetTextMetrics(Canvas.Handle, TM);
              item.Width := (Items[i].Width - TM.tmOverhang - 4) div (Canvas.TextWidth('0') - TM.tmOverhang);
            end;


            j := clbFields.Items.AddObject(GetItemCaption(item), item);
            {$IFDEF VER120} //D4
            clbFields.Checked[j] := Items[i].Visible;
            {$ELSE}
            {$IFDEF VER125} //CB4
            clbFields.Checked[j] := Items[i].Visible;
            {$ELSE}
            {$IFDEF VER130} //D5
            clbFields.Checked[j] := Items[i].Visible;
            {$ELSE}
            clbFields.Checked[j] := True;
            {$ENDIF}
            {$ENDIF}
            {$ENDIF}
          end;
        end;

      IsSMDBGrid := (Columns.Count > 0);
      for i := 0 to DataSource.DataSet.FieldCount - 1 do
        if (AllowedFields.Count = 0) or
           (AllowedFields.IndexOf(DataSource.DataSet.Fields[i].FieldName) > -1) then
        begin
          for j := 0 to clbFields.Items.Count-1 do
          begin
            IsSMDBGrid := (TSMGridSetupItem(clbFields.Items.Objects[j]).FieldName = DataSource.DataSet.Fields[i].FieldName);
            if IsSMDBGrid then
              break;
          end;
          if not IsSMDBGrid then
          begin
            item := TSMGridSetupItem.Create;
            item.TitleFont := TFont.Create;
            item.DataFont := TFont.Create;

            item.FieldIndex := clbFields.Items.Count;
            item.FieldName := DataSource.DataSet.Fields[i].FieldName;

            item.TitleAlignment := DataSource.DataSet.Fields[i].Alignment;
            item.TitleCaption := DataSource.DataSet.Fields[i].DisplayName;
            item.TitleColor := FixedColor;
            item.TitleFont.Assign(TDBGrid(DBGrid).TitleFont);

            item.DataAlignment := DataSource.DataSet.Fields[i].Alignment;
            item.DataColor := TDBGrid(DBGrid).Color;
            item.DataFont.Assign(TDBGrid(DBGrid).Font);

            item.Width := DataSource.DataSet.Fields[i].DisplayWidth;

            j := clbFields.Items.AddObject(GetItemCaption(item), item);
            clbFields.Checked[j] := False;
          end;
      end;

      clbFields.ItemIndex := 0;
      clbFieldsClick(clbFields);

      if Assigned(FOnShow) then
        FOnShow(Self);

      Result := (ShowModal = mrOk);
      if Result then
      begin
        THackDBGrid(DBGrid).BeginLayout;
//        Columns.BeginUpdate;
        try
          if (Columns.Count > 0) then
          begin
            Columns.Clear;
            for i := 0 to clbFields.Items.Count-1 do
              if clbFields.Checked[i] then
              begin
                item := TSMGridSetupItem(clbFields.Items.Objects[i]);
                with Columns.Add do
                begin
                  FieldName := item.FieldName;
                  Title.Alignment := item.TitleAlignment;
                  Title.Caption := item.TitleCaption;
                  Title.Color := item.TitleColor;
                  Title.Font.Assign(item.TitleFont);

                  Alignment := item.DataAlignment;
                  Color := item.DataColor;
                  Font.Assign(item.DataFont);

                  if (item.Width > 0) then
                  begin
                    GetTextMetrics(Canvas.Handle, TM);
                    Width := item.Width*(Canvas.TextWidth('0') - TM.tmOverhang)
                               + TM.tmOverhang + 4;
                  end;
                end;
              end;
          end
          else
          begin
            for i := 0 to clbFields.Items.Count-1 do
            begin
              item := TSMGridSetupItem(clbFields.Items.Objects[i]);
              with DataSource.DataSet.Fields[i] do
              begin
                FieldName := item.FieldName;

                Alignment := item.DataAlignment;
                DisplayLabel := item.TitleCaption;
                Color := item.DataColor;
                Font.Assign(item.DataFont);

                if (item.Width > 0) then
                begin
                  GetTextMetrics(Canvas.Handle, TM);
                  DisplayWidth := item.Width*(Canvas.TextWidth('0') - TM.tmOverhang)
                                   + TM.tmOverhang + 4;
                end;
                Visible := clbFields.Checked[i];
              end;
            end;
          end
        finally
          THackDBGrid(DBGrid).EndLayout;
          //Columns.EndUpdate;
        end
      end
    finally
      for i := clbFields.Items.Count-1 downto 0 do
        with TSMGridSetupItem(clbFields.Items.Objects[i]) do
        begin
          TitleFont.Free;
          DataFont.Free;
          Free;
        end;
      Free
    end;

  if Assigned(FOnAfterExecute) then
    FOnAfterExecute(Self);
end;

{ TfrmGridSetup }
procedure TfrmGridSetup.clbFieldsClick(Sender: TObject);

  function GetColorID(cl: TColor): Integer;
  var
    i: Integer;
  begin
    Result := -1;

    for i := 0 to ColorsInList do
      if ColorValues[i] = cl then
      begin
        Result := i;
        break;
      end;
    if Result < 0 then
      Result := 0;
  end;

begin
  if clbFields.ItemIndex > -1 then
    with TSMGridSetupItem(clbFields.Items.Objects[clbFields.ItemIndex]) do
    begin
      edTitleCaption.Text := TitleCaption;

      cbTitleAlignment.ItemIndex := Ord(TitleAlignment);
      SMColorsCBTitle.ItemIndex := GetColorID(TitleColor);
      edTitleFont.Font.Assign(TitleFont);
      edTitleFont.Text := GetCaptionFont(edTitleFont.Font);

      cbDataAlignment.ItemIndex := Ord(DataAlignment);
      SMColorsCBData.ItemIndex := GetColorID(DataColor);
      edDataFont.Font.Assign(DataFont);
      edDataFont.Text := GetCaptionFont(edDataFont.Font);

      edWidth.Text := IntToStr(Width);

      ColorValues[ColorsInList-2] := TitleColor;
      ColorValues[ColorsInList-1] := DataColor;

      SMColorsCBTitleChange(SMColorsCBTitle)
    end;
end;

procedure TfrmGridSetup.clbFieldsDragDrop(Sender, Source: TObject; X,
  Y: Integer);
var intItemIndex, intNewItemIndex: Integer;
    boolChecked: Boolean;
    coordXY: TPoint;
begin
  with Source as TCheckListBox do
  begin
    intItemIndex := clbFields.ItemIndex;
    coordXY.x := X;
    coordXY.y := Y;
    intNewItemIndex := clbFields.ItemAtPos(coordXY, True);
    if (intNewItemIndex <> -1) then
    begin
      boolChecked := clbFields.Checked[intItemIndex];
      clbFields.Items.Move(intItemIndex, intNewItemIndex);
      clbFields.Checked[intNewItemIndex] := boolChecked;
      clbFields.ItemIndex := intNewItemIndex;
    end;
  end;
end;

procedure TfrmGridSetup.clbFieldsDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  if (Source = clbFields) then
    Accept := True
  else
    Accept := False;
end;

procedure TfrmGridSetup.PropertyExit(Sender: TObject);
var
  item: TSMGridSetupItem;
  boolChecked: Boolean;
begin
  if clbFields.ItemIndex > -1 then
  begin
    item := TSMGridSetupItem(clbFields.Items.Objects[clbFields.ItemIndex]);
    if not Assigned(item) then exit;

    boolChecked := clbFields.Checked[clbFields.ItemIndex];
    case (Sender as TControl).Tag of
      1: begin
           item.TitleCaption := edTitleCaption.Text;
           clbFields.Items[clbFields.ItemIndex] := GetItemCaption(item);
         end;
      2: item.TitleAlignment := TAlignment(cbTitleAlignment.ItemIndex);
      3: item.TitleColor := ColorValues[SMColorsCBTitle.ItemIndex];
      4: item.TitleFont.Assign(edTitleFont.Font);

      5: item.DataAlignment := TAlignment(cbDataAlignment.ItemIndex);
      6: if (SMColorsCBData.ItemIndex = SMColorsCBData.Items.Count-1) then
           item.DataColor := ColorValues[SMColorsCBData.ItemIndex+1]
         else
           item.DataColor := ColorValues[SMColorsCBData.ItemIndex];
      7: item.DataFont.Assign(edDataFont.Font);

      8, 9: item.Width := StrToIntDef(edWidth.Text, 0);
      10: item.Width := 10;
    end;
    clbFields.Checked[clbFields.ItemIndex] := boolChecked;
  end;
end;

procedure TfrmGridSetup.edTitleFontButtonClick(Sender: TObject);
var cntr: TEdit;
begin
  if TButton(Sender) = btnTitleFont then
    cntr := edTitleFont
  else
    cntr := edDataFont;

  with FontDlg do
  begin
    Font.Assign(cntr.Font);
    if Execute then
    begin
      cntr.Font.Assign(Font);
      cntr.Text := GetCaptionFont(Font);

      PropertyExit(Sender);
    end;
  end;
end;

function TfrmGridSetup.GetItemCaption(item: TSMGridSetupItem): string;
begin
  Result := item.FieldName + '  :  ' + item.TitleCaption;
end;

function TfrmGridSetup.GetCaptionFont(Font: TFont): string;
begin
  Result := Font.Name + ', ' + IntToStr(Font.Size);
end;


procedure TfrmGridSetup.SMColorsCBTitleDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  ARect: TRect;
  Safer: TColor;
  i: Integer;
begin
  ARect := Rect;
  InflateRect(ARect, -2, -2);

  with (Control as TComboBox) do
  begin
    Canvas.FillRect(Rect);
    Safer := Canvas.Brush.Color;
    Canvas.Pen.Color := clWindowText;
    Canvas.Rectangle(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);
    i := Index;
    if (Control = SMColorsCBData) and
       (i = SMColorsCBData.Items.Count-1) then
      Inc(i);
    Canvas.Brush.Color := ColorValues[i];
    try
      InflateRect(ARect, -1, -1);
      Canvas.FillRect(ARect);

{      if (Index = Items.Count-1) then
      begin
        Canvas.Pen.Color := clWhite;
        Canvas.TextOut(ARect.Left, ARect.Top, 'Custom...')
      end;
}    finally
      Canvas.Brush.Color := Safer;
    end;
  end;
end;

procedure TfrmGridSetup.miSelectAllClick(Sender: TObject);
var
  i, intTag: Integer;
begin
  intTag := TComponent(Sender).Tag;
  for i := 0 to clbFields.Items.Count-1 do
    case intTag of
      1: //unselect all
         clbFields.Checked[i] := False;
      2: //invert selection
         clbFields.Checked[i] := not clbFields.Checked[i];
    else //select all
      clbFields.Checked[i] := True;
    end;
  clbFields.Invalidate  
end;

procedure TfrmGridSetup.SMColorsCBTitleClick(Sender: TObject);
var
  i: Integer;
begin
  with TCombobox(Sender) do
    if (ItemIndex = ColorsInList-2) then
    begin
      with TColorDialog.Create(Self) do
        try
          i := ColorsInList-1;
          if (Sender = SMColorsCBTitle) then
            Dec(i);

          Color := ColorValues[i];
          if Execute then
            ColorValues[i] := Color
        finally
          Free
        end
    end;
end;

procedure TfrmGridSetup.SMColorsCBTitleChange(Sender: TObject);
begin
  edTitleFont.Color := ColorValues[SMColorsCBTitle.ItemIndex];
  if (SMColorsCBData.ItemIndex = SMColorsCBData.Items.Count-1) then
    edDataFont.Color := ColorValues[SMColorsCBData.ItemIndex+1]
  else
    edDataFont.Color := ColorValues[SMColorsCBData.ItemIndex];
end;

procedure TfrmGridSetup.btnApplyAllClick(Sender: TObject);
var
  item, subItem: TSMGridSetupItem;
  i: Integer;
begin
  if clbFields.ItemIndex > -1 then
    item := TSMGridSetupItem(clbFields.Items.Objects[clbFields.ItemIndex])
  else
    item := nil;

  if Assigned(item) then
  begin
    for i := 0 to clbFields.Items.Count-1 do
    begin
      if (clbFields.ItemIndex <> i) and clbFields.Checked[i] then
      begin
        subItem := TSMGridSetupItem(clbFields.Items.Objects[i]);
        if Assigned(subItem) then
        begin
          subItem.TitleAlignment := item.TitleAlignment;
          subItem.TitleColor := item.TitleColor;
          subItem.TitleFont.Assign(item.TitleFont);

          subItem.DataAlignment := item.DataAlignment;
          subItem.DataColor := item.DataColor;
          subItem.DataFont.Assign(item.DataFont);

          subItem.Width := item.Width;
        end
      end
    end;
  end;
end;

end.
