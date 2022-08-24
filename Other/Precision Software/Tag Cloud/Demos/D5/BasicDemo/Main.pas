unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, TagCloud, StdCtrls, ComCtrls;

type
  TfrmMain = class(TForm)
    StatusBar: TStatusBar;
    odFile: TOpenDialog;
    tagCloud: TTagCloud;
    pRight: TPanel;
    pHead1: TPanel;
    Bevel2: TBevel;
    pLeft: TPanel;
    Panel1: TPanel;
    Label1: TLabel;
    sbLoadCSV: TButton;
    sbTasks: TButton;
    pItem: TPanel;
    lbSelected: TLabel;
    lbCaption: TLabel;
    lbValue: TLabel;
    lbHint: TLabel;
    lbTag: TLabel;
    edCaption: TEdit;
    edHint: TEdit;
    edValue: TEdit;
    edTag: TEdit;
    sbAdd: TButton;
    sbDelete: TButton;
    sbClear: TButton;
    pImport: TPanel;
    sbOpenFile: TButton;
    Label2: TLabel;
    Panel3: TPanel;
    Panel2: TPanel;
    Label3: TLabel;
    cbVistaGlass: TCheckBox;
    Bevel1: TBevel;
    Bevel3: TBevel;
    Panel4: TPanel;
    Bevel4: TBevel;
    Panel5: TPanel;
    Bevel5: TBevel;
    cbAlphaSort: TCheckBox;
    cbLowerCase: TCheckBox;
    cbSkipFirstRow: TCheckBox;
    lbSep: TLabel;
    edSep: TEdit;
    Label4: TLabel;
    edMaxItems: TEdit;
    Label5: TLabel;
    edFCaption: TEdit;
    Label6: TLabel;
    edFValue: TEdit;
    Label7: TLabel;
    edFHint: TEdit;
    Label8: TLabel;
    edFTag: TEdit;
    Label9: TLabel;
    Bevel6: TBevel;
    lbFont: TLabel;
    cbFont: TComboBox;
    Label10: TLabel;
    cbFontSize: TComboBox;
    cbMaxSize: TComboBox;
    Label11: TLabel;
    cbLogScale: TCheckBox;
    sbFont: TButton;
    dlgFont: TFontDialog;
    Label12: TLabel;
    cbGlow: TComboBox;
    sbBackground: TButton;
    dlgColor: TColorDialog;
    Label13: TLabel;
    cbAlignment: TComboBox;
    Label14: TLabel;
    edColSpacing: TEdit;
    edRowSpacing: TEdit;
    Label15: TLabel;
    udColSpace: TUpDown;
    udRowSpace: TUpDown;
    cbHoverEnlarge: TCheckBox;
    Label16: TLabel;
    cbHoverBold: TCheckBox;
    cbHoverUnderline: TCheckBox;
    cbHoverHandCursor: TCheckBox;
    pColors: TPanel;
    Label17: TLabel;
    Bevel7: TBevel;
    cbClr1: TCheckBox;
    cbClr2: TCheckBox;
    cbClr3: TCheckBox;
    cbClr4: TCheckBox;
    procedure sbLoadCSVClick(Sender: TObject);
    procedure tagCloudTagHint(Sender: TObject; Item: TTagCloudItem; var HintText: string);
    procedure sbTasksClick(Sender: TObject);
    procedure tagCloudTagClick(Sender: TObject; Item: TTagCloudItem);
    procedure sbDeleteClick(Sender: TObject);
    procedure sbClearClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure sbAddClick(Sender: TObject);
    procedure edValueChange(Sender: TObject);
    procedure edValueKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure tagCloudHoverChange(Sender: TObject; Item: TTagCloudItem);
    procedure sbOpenFileClick(Sender: TObject);
    procedure cbFontSelect(Sender: TObject);
    procedure cbFontSizeChange(Sender: TObject);
    procedure cbMaxSizeChange(Sender: TObject);
    procedure cbLogScaleClick(Sender: TObject);
    procedure sbFontClick(Sender: TObject);
    procedure dlgFontApply(Sender: TObject; Wnd: HWND);
    procedure cbGlowChange(Sender: TObject);
    procedure sbBackgroundClick(Sender: TObject);
    procedure cbAlignmentChange(Sender: TObject);
    procedure tagCloudTagColor(Sender: TObject; Item: TTagCloudItem; var Color: TColor);
    procedure udRowSpaceChangingEx(Sender: TObject; var AllowChange: Boolean; NewValue: SmallInt; Direction: TUpDownDirection);
    procedure udColSpaceChangingEx(Sender: TObject; var AllowChange: Boolean; NewValue: SmallInt; Direction: TUpDownDirection);
    procedure cbHoverEnlargeClick(Sender: TObject);
    procedure cbClr1Click(Sender: TObject);
  private
    { Private declarations }
    FUpdatingInfo:Boolean;
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

uses
  PsAPI;

{$R *.dfm}

procedure TfrmMain.FormCreate(Sender: TObject);
var
  i:Integer;
begin
  DoubleBuffered:=True;
  FUpdatingInfo:=True;
  try
    cbFont.Items.Assign(Screen.Fonts);
    i:=cbFont.Items.IndexOf('Calibri');
    if i>=0 then
    begin
      cbFont.ItemIndex:=i;
      tagCloud.Font.Name:='Calibri';
    end
    else
    begin
      i:=cbFont.Items.IndexOf(Font.Name);
      cbFont.ItemIndex:=i;
    end;
    cbFontSize.ItemIndex:=3; // 7pt
    cbMaxSize.ItemIndex:=27; // 36pt
    cbAlignment.ItemIndex:=1; // center
  finally
    FUpdatingInfo:=False;
  end;
  sbLoadCSVClick(nil);
end;

procedure TfrmMain.tagCloudHoverChange(Sender: TObject; Item: TTagCloudItem);
begin
  if Assigned(Item) then
    StatusBar.Panels[0].Text:=Format('%s: %d',[Item.Caption,Item.Value])
  else
    StatusBar.Panels[0].Text:='';
end;

procedure TfrmMain.tagCloudTagHint(Sender: TObject; Item: TTagCloudItem; var HintText: string);
begin
  if Assigned(Item) then
  begin
    HintText:=
      Item.Caption+#13#10+
      'Value: '+IntToStr(Item.Value)+#13#10+
      'Hint: '+Item.Hint+#13#10+
      'Tag: '+IntToStr(Item.Tag);
  end;
end;

procedure TfrmMain.tagCloudTagClick(Sender: TObject; Item: TTagCloudItem);
begin
  FUpdatingInfo:=True;
  try
    if Assigned(Item) then
    begin
      lbSelected.Tag:=Item.Index;
      edCaption.Text:=Item.Caption;
      edValue.Text:=IntToStr(Item.Value);
      edHint.Text:=Item.Hint;
      edTag.Text:=IntToStr(Item.Tag);
    end
    else
    begin
      lbSelected.Tag:=-1;
      edCaption.Text:='';
      edValue.Text:='';
      edHint.Text:='';
      edTag.Text:='';
    end
  finally
    FUpdatingInfo:=False;
  end;
end;

procedure TfrmMain.tagCloudTagColor(Sender: TObject; Item: TTagCloudItem; var Color: TColor);
var
  tmp:string;
begin
  // colors handling example for PC Tasks source (show windows system apps as maroon)
  if Assigned(Item) and (Color=tagCloud.Font.Color) and (Length(Item.Hint)>0) then
  begin
    tmp:=lowercase(Item.Hint);
    if (Pos('\system32\',tmp)>0) or (Pos('\system\',tmp)>0) or (Pos('\sysWOW64\',tmp)>0) then
      Color:=clMaroon
  end;
end;

procedure TfrmMain.sbBackgroundClick(Sender: TObject);
begin
  if not FUpdatingInfo then
  begin
    dlgColor.Color:=tagCloud.Color;
    if dlgColor.Execute then
      tagCloud.Color:=dlgColor.Color;
  end;
end;

procedure TfrmMain.cbAlignmentChange(Sender: TObject);
begin
  if not FUpdatingInfo then
    case cbAlignment.ItemIndex of
      0:tagCloud.Alignment:=taLeftJustify;
      2:tagCloud.Alignment:=taRightJustify;
    else
      tagCloud.Alignment:=taCenter;
    end;
end;

procedure TfrmMain.cbClr1Click(Sender: TObject);
begin
  if not FUpdatingInfo then
  begin
    tagCloud.Colors.BeginUpdate;
    try
      tagCloud.Colors.Clear;
      if cbClr1.Checked then tagCloud.Colors.Add.Color:=cbClr1.Color;
      if cbClr2.Checked then tagCloud.Colors.Add.Color:=cbClr2.Color;
      if cbClr3.Checked then tagCloud.Colors.Add.Color:=cbClr3.Color;
      if cbClr4.Checked then tagCloud.Colors.Add.Color:=cbClr4.Color;
    finally
      tagCloud.Colors.EndUpdate;
    end;
  end;
end;

procedure TfrmMain.cbFontSelect(Sender: TObject);
begin
  if not FUpdatingInfo then
    tagCloud.Font.Name:=cbFont.Text;
end;

procedure TfrmMain.cbFontSizeChange(Sender: TObject);
begin
  if not FUpdatingInfo then
    tagCloud.Font.Size:=StrToIntDef(cbFontSize.Text,8);
end;

procedure TfrmMain.udColSpaceChangingEx(Sender: TObject; var AllowChange: Boolean; NewValue: SmallInt;
  Direction: TUpDownDirection);
begin
  if not FUpdatingInfo then
    tagCloud.ColSpacing:=NewValue;
end;

procedure TfrmMain.udRowSpaceChangingEx(Sender: TObject; var AllowChange: Boolean; NewValue: SmallInt;
  Direction: TUpDownDirection);
begin
  if not FUpdatingInfo then
    tagCloud.RowSpacing:=NewValue;
end;

procedure TfrmMain.cbGlowChange(Sender: TObject);
begin
{  if not FUpdatingInfo then
    tagCloud.GlowSize:=StrToIntDef(cbGlow.Text,0);}
end;

procedure TfrmMain.cbHoverEnlargeClick(Sender: TObject);
begin
  if not FUpdatingInfo then
  begin
    tagCloud.HoverEnlarge:=cbHoverEnlarge.Checked;
    if cbHoverHandCursor.Checked then
      tagCloud.HoverCursor:=crHandPoint
    else
      tagCloud.HoverCursor:=crDefault;
    tagCloud.HoverStyle:=[];
    if cbHoverBold.Checked then
      tagCloud.HoverStyle:=tagCloud.HoverStyle+[fsBold];
    if cbHoverUnderline.Checked then
      tagCloud.HoverStyle:=tagCloud.HoverStyle+[fsUnderline];
  end;
end;

procedure TfrmMain.cbLogScaleClick(Sender: TObject);
begin
  if not FUpdatingInfo then
    tagCloud.LogScale:=cbLogScale.Checked;
end;

procedure TfrmMain.cbMaxSizeChange(Sender: TObject);
begin
  if not FUpdatingInfo then
    tagCloud.MaxFontSize:=StrToIntDef(cbMaxSize.Text,17);
end;

procedure TfrmMain.edValueChange(Sender: TObject);
begin
  if (not FUpdatingInfo) and (lbSelected.Tag>=0) and (lbSelected.Tag<tagCloud.Items.Count) then
  begin
    if Sender=edCaption then
      tagCloud.Items[lbSelected.Tag].Caption:=edCaption.text
    else
    if Sender=edHint then
      tagCloud.Items[lbSelected.Tag].Hint:=edHint.text
    else
    if Sender=edTag then
      tagCloud.Items[lbSelected.Tag].Tag:=StrToIntDef(edHint.Text,0)
    else
    if Sender=edValue then
      tagCloud.Items[lbSelected.Tag].Value:=StrToTagCloudValue(edValue.Text);
  end;
end;

procedure TfrmMain.edValueKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  offset:Integer;
  tmpVal:Int64;
begin
  if key in [VK_UP,VK_DOWN] then
  begin
    case key of
      VK_DOWN:offset:=-1;
    else
      offset:=1;
    end;
    Key:=0;
    tmpVal:=StrToTagCloudValue(edValue.Text);
    if ssShift in Shift then
    begin
      if ssCtrl in Shift then
        tmpVal:=tmpVal+offset*(tmpVal)
      else
        tmpVal:=tmpVal+offset*(tmpVal div 10);
    end
    else
      tmpVal:=tmpVal+offset;
    edValue.Text:=IntToStr(tmpVal);
  end;
end;

procedure TfrmMain.sbAddClick(Sender: TObject);
var
  Item:TTagCloudItem;
begin
  Item:=tagCloud.Items.Add;
  Item.Caption:='New item';
  Item.Value:=0;
  Item.Hint:='';
  Item.Tag:=0;
  tagCloudTagClick(tagCloud,Item);
end;

procedure TfrmMain.sbClearClick(Sender: TObject);
begin
  tagCloud.Items.Clear;
  tagCloudTagClick(nil,nil);
end;

procedure TfrmMain.sbDeleteClick(Sender: TObject);
begin
  if (lbSelected.Tag>=0) and (lbSelected.Tag<tagCloud.Items.Count) then
  begin
    tagCloud.Items.Delete(lbSelected.Tag);
    tagCloudTagClick(nil,nil);
  end;
end;

procedure TfrmMain.sbFontClick(Sender: TObject);
begin
  if not FUpdatingInfo then
  begin
    dlgFont.Font.Assign(tagCloud.Font);
    if dlgFont.Execute then
      dlgFontApply(dlgFont,0);
  end;
end;

procedure TfrmMain.dlgFontApply(Sender: TObject; Wnd: HWND);
begin
  FUpdatingInfo:=True;
  try
    cbFont.ItemIndex:=cbFont.Items.IndexOf(dlgFont.Font.Name);
    if cbFont.ItemIndex=-1 then
      cbFont.Text:=dlgFont.Font.Name;
    cbFontSize.ItemIndex:=cbFontSize.Items.IndexOf(IntToStr(dlgFont.Font.Size));
    if cbFontSize.ItemIndex=-1 then
      cbFontSize.Text:=IntToStr(dlgFont.Font.Size);
    tagCloud.Font.Assign(dlgFont.Font);
  finally
    FUpdatingInfo:=False;
  end;
end;

procedure TfrmMain.sbLoadCSVClick(Sender: TObject);
begin
  tagCloud.OnTagColor:=nil;
  tagCloud.LoadingOptions.AlphaSort:=True;
  tagCloud.LoadingOptions.LowerCase:=cbLowerCase.Checked;
  tagCloud.LoadingOptions.SkipFirstRow:=True;
  tagCloud.LoadingOptions.MaxItemsCount:=StrToIntDef(edMaxItems.Text,-1);
  tagCloud.LoadingOptions.ColCaption:=0;
  tagCloud.LoadingOptions.ColValue:=1;
  tagCloud.LoadingOptions.ColHint:=2;
  tagCloud.LoadingOptions.ColTag:=-1;
  tagCloud.LoadingOptions.ColData:=-1;
  tagCloud.LoadingOptions.Separator:=';';
  tagCloud.LoadFromFile(ExtractFilePath(ParamStr(0))+'Country.csv');
  tagCloudTagClick(nil,nil);
end;

procedure TfrmMain.sbOpenFileClick(Sender: TObject);
begin
  if odFile.FileName='' then
    odFile.FileName:=ExtractFilePath(ParamStr(0))+'Country.csv';
  if odFile.Execute then
  begin
    tagCloud.OnTagColor:=nil;
    if edSep.Text<>'' then
      tagCloud.LoadingOptions.Separator:=Copy(edSep.Text,1,1)[1]
    else
      tagCloud.LoadingOptions.Separator:=';';
    tagCloud.LoadingOptions.AlphaSort:=cbAlphaSort.Checked;
    tagCloud.LoadingOptions.LowerCase:=cbLowerCase.Checked;
    tagCloud.LoadingOptions.SkipFirstRow:=cbSkipFirstRow.Checked;
    tagCloud.LoadingOptions.MaxItemsCount:=StrToIntDef(edMaxItems.Text,-1);
    tagCloud.LoadingOptions.ColCaption:=StrToIntDef(edFCaption.Text,0);
    tagCloud.LoadingOptions.ColValue:=StrToIntDef(edFValue.Text,1);
    tagCloud.LoadingOptions.ColHint:=StrToIntDef(edFHint.Text,-1);
    tagCloud.LoadingOptions.ColTag:=StrToIntDef(edFTag.Text,-1);
    tagCloud.LoadingOptions.ColData:=-1;

    tagCloud.LoadFromFile(odFile.FileName);
    tagCloudTagClick(nil,nil);
  end;
end;

///////////////////////////// TagCloud for current PC tasks /////////////////////////////
procedure TfrmMain.sbTasksClick(Sender: TObject);
var
  ProcessList:array[0..1024] of DWORD;
  aProcess:THandle;
	PMC: TProcessMemoryCounters;
  i:Integer;
  Count,Needed:DWORD;
  Item:TTagCloudItem;
  fn:array[0..MAX_PATH-1] of char;
begin
  if Win32Platform=VER_PLATFORM_WIN32_NT then
  begin
    try
      tagCloud.Items.BeginUpdate;
      tagCloud.OnTagColor:=tagCloudTagColor;
      try
        tagCloud.Items.Clear;
        if EnumProcesses(@processlist,SizeOf(ProcessList),Needed) then
        begin
          Count:=Needed div SizeOf(DWORD);
          for i:=0 to Count-1 do
          begin
            aProcess:=OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, False, ProcessList[i]);
            if aProcess <> 0 then
            begin
              Item:=tagCloud.Items.Add;
              try
                case ProcessList[i] of
                  8:Item.Caption:='System idle process';
                  0,2:Item.Caption:='System process';
                else
                  if GetModuleFileNameEx(aProcess, 0, fn, SizeOf(FN)) > 0 then
                  begin
                    Item.Hint:=fn;
                    Item.Caption:=ChangeFileExt(ExtractFileName(fn),'');
                  end
                  else
                    Item.Caption:='?';
                end;
                Win32Check(GetProcessMemoryInfo(aProcess, @PMC, SizeOf(PMC)));
                Item.Value:=PMC.WorkingSetSize;

              finally
                CloseHandle(aProcess);
              end;
            end;
          end;
        end;
        tagCloud.Items.Sort;
      finally
        tagCloud.Items.EndUpdate;
      end;
    except
      on E:Exception do
        ShowMessage(e.message);
    end;
  end
  else
    ShowMessage('This demo works on Win2000 and later');
  tagCloudTagClick(nil,nil);
end;

end.
