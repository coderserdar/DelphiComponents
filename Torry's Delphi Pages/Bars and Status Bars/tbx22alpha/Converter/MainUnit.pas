unit MainUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Contnrs, TBX, TBXDkPanels, TB2Dock, ExtCtrls, TB2Item,
  TB2Toolbar, StdCtrls, ComCtrls, TBXOfficeXPTheme, TBXSwitcher, TBXStrUtils,
  TB2ToolWindow, TB2ExtItems, TBXExtItems, ShellCtrls, ImgList, XPMan,
  TBXControls;


const
  CM_INIT = WM_USER + 229;
  CDatFile = 'conversions.ini';

type
  TConversion = class
    OldClassName: string;
    NewClassName: string;
    CheckBox: TTBXCheckBox;
  end;

  { TConverterForm }
  TConverterForm = class(TForm)
    TBDock1: TTBDock;
    TBDock2: TTBDock;
    TBDock3: TTBDock;
    TBDock4: TTBDock;
    Panel1: TTBXDockablePanel;
    ConversionsPanel: TTBXAlignmentPanel;
    TBXLabel1: TTBXLabel;
    TBXLabel2: TTBXLabel;
    TBXAlignmentPanel1: TTBXAlignmentPanel;
    FilterPas: TTBXCheckBox;
    FilterHpp: TTBXCheckBox;
    FilterCPP: TTBXCheckBox;
    FilterDFM: TTBXCheckBox;
    FilterBinaryDFM: TTBXCheckBox;
    TBXToolbar1: TTBXToolbar;
    TBXItem1: TTBXItem;
    TBXItem2: TTBXItem;
    TBXSeparatorItem1: TTBXSeparatorItem;
    TBXToolbar2: TTBXToolbar;
    TBXVisibilityToggleItem1: TTBXVisibilityToggleItem;
    Memo: TMemo;
    TBXSwitcher1: TTBXSwitcher;
    TBXPageScroller1: TTBXPageScroller;
    TBXAlignmentPanel3: TTBXAlignmentPanel;
    ConversionCombo: TTBXComboBoxItem;
    TBXLabelItem1: TTBXLabelItem;
    Panel2: TPanel;
    Memo1: TMemo;
    FilePanel: TTBXDockablePanel;
    ShellTreeView: TShellTreeView;
    TBXVisibilityToggleItem2: TTBXVisibilityToggleItem;
    ImageList1: TImageList;
    TBXSeparatorItem3: TTBXSeparatorItem;
    XPManifest1: TXPManifest;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TBXItem1Click(Sender: TObject);
    procedure TBXItem2Click(Sender: TObject);
    procedure ConversionComboPopup(Sender: TTBCustomItem; FromLink: Boolean);
    procedure ConversionComboChange(Sender: TObject);
    procedure ShellTreeViewAddFolder(Sender: TObject; AFolder: TShellFolder; var CanAdd: Boolean);
    procedure FilterChange(Sender: TObject);
    procedure ShellTreeViewDblClick(Sender: TObject);
    procedure TBXVisibilityToggleItem2Click(Sender: TObject);
    procedure MemoDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure MemoDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure ShellTreeViewEditing(Sender: TObject; Node: TTreeNode;
      var AllowEdit: Boolean);
  private
    FConversions: TObjectList;
    procedure CMInit(var Message: TMessage); message CM_INIT;
    procedure WMDropFiles(var Message: TWMDropFiles); message WM_DROPFILES;
  protected
    procedure ShowConversions;
    procedure GetConversionTypes(L: TStringListW);
    procedure GetConversions(const ConvType: string);
  public
    procedure ProcessFiles(FileNames: TStringList);
  end;

var
  ConverterForm: TConverterForm;

implementation

uses
  ShellApi;

const
  CNameFirst = ['a'..'z', 'A'..'Z', '_'];
  CNameChar = ['a'..'z', 'A'..'Z', '_', '0'..'9'];

type
  TTreeViewAccess = class(TCustomTreeView);

function WordReplace(const Src, OldWord, NewWord: string): string;
var
  P, PStart: PChar;
  S: string;
begin
  Result := '';
  P := PChar(Src);
  PStart := P;
  while P^ <> #0 do
  begin
    if P^ in CNameFirst then
    begin
      SetString(S, PStart, P - PStart);
      Result := Result + S;
      PStart := P;
      Inc(P);
      while P^ in CNameChar do Inc(P);
      SetString(S, PStart, P - PStart);
      if SameText(S, OldWord) then S := NewWord;
      Result := Result + S;
      PStart := P;
    end
    else Inc(P);
  end;
  if P <> PStart then
  begin
    SetString(S, PStart, P - PStart);
    Result := Result + S;
  end;
end;

{$R *.dfm}

{ TConverterForm }

procedure TConverterForm.CMInit(var Message: TMessage);
begin
  DragAcceptFiles(Handle, True);
  ShowConversions;
  TTreeViewAccess(ShellTreeView).MultiSelect := True;
  TTreeViewAccess(ShellTreeView).MultiSelectStyle := [msControlSelect, msShiftSelect, msSiblingOnly];
end;

procedure TConverterForm.FormCreate(Sender: TObject);
begin
  FConversions := TObjectList.Create;
  GetConversionTypes(ConversionCombo.Lines);
  if ConversionCombo.Lines.Count = 0 then raise Exception.Create('Invalid conversion settings');
  ConversionCombo.ItemIndex := 0;
  GetConversions(ConversionCombo.Lines[0]);
  PostMessage(Handle, CM_INIT, 0, 0);
end;

procedure TConverterForm.FormDestroy(Sender: TObject);
begin
  FConversions.Free;
end;

procedure TConverterForm.ProcessFiles(FileNames: TStringList);
var
  I, J: Integer;
  S, E: string;
  BinaryStream: TFileStream;
  StringStream: TStringStream;
  S1, S2: string;
  DFM, BinaryDFM: Boolean;

  function IsTextDFM(const FN: string): Boolean;
  var
    Stream: TFileStream;
    Buffer: array [0..7] of Char;
  begin
    Stream := TFileStream.Create(FN, fmOpenRead);
    FillChar(Buffer[0], 8, 0);
    Stream.Read(Buffer[0], 7);
    Result := SameText(Buffer, 'object ');
    Stream.Free;
  end;

  function IsBinaryDFM(const FN: string): Boolean;
  var
    Stream: TFileStream;
    TS: TStream;
  begin
    Result := True; Exit;
    Stream := TFileStream.Create(FN, fmOpenRead);
    try
      TS := TMemoryStream.Create;
      try
        try
          Result := True;
          ObjectResourceToText(Stream, TS);
        except
          Result := False;
        end;
      finally
        TS.Free;
      end;
    finally
      Stream.Free;
    end;
  end;

  function GetBackupFileName(F: string): string;
  var
    I: Integer;
  begin
    I := 0;
    repeat
      Inc(I);
      Result := Format('%s.###%.3d', [F, I]);
    until not FileExists(Result);
  end;

begin
  { Filter files }
  for I := FileNames.Count - 1 downto 0 do
  begin
    S := FileNames[I];
    E := ExtractFileExt(S);
    if (SameText(E, '.pas') and FilterPAS.Checked) or
      ((SameText(E, '.h') or SameText(E, '.hpp')) and FilterHPP.Checked) or
      (SameText(E, '.cpp') and FilterCPP.Checked) or
      (SameText(E, '.dfm') and FilterDFM.Checked and IsTextDFM(S)) or
      (SameText(E, '.dfm') and FilterBinaryDFM.Checked and IsBinaryDFM(S)) then
      Continue
    else
      FileNames.Delete(I);
  end;
  if FileNames.Count = 0 then Memo.Lines.Add('No files to process');

  { Get each file as string }
  for I := FileNames.Count - 1 downto 0 do
  begin
    DFM := SameText(ExtractFileExt(FileNames[I]), '.dfm');
    if DFM then BinaryDFM := not IsTextDFM(FileNames[I]) else BinaryDFM := False;
    if DFM and BinaryDFM then
    begin
      BinaryStream := TFileStream.Create(FileNames[I], fmOpenRead);
      try
        StringStream := TStringStream.Create('');
        try
          ObjectResourceToText(BinaryStream, StringStream);
          S := StringStream.DataString;
        finally
          StringStream.Free;
        end;
      finally
        BinaryStream.Free;
      end;
    end
    else
    begin
      with TFileStream.Create(FileNames[I], fmOpenRead) do
      try
        SetLength(S, Size);
        ReadBuffer(S[1], Size);
      finally
        Free;
      end;
    end;

    Memo.Lines.Add('Converting ' + FileNames[I]);
    { Now S contains text representation of the file }
    for J := 0 to FConversions.Count - 1 do
      with TConversion(FConversions[J]) do
        if CheckBox.Checked then
          if DFM then
          begin
            S1 := ': ' + OldClassName + #13;
            S2 := ': ' + NewClassName + #13;
            S := StringReplace(S, S1, S2, [rfReplaceAll, rfIgnoreCase]);
          end
          else
          begin
            S := WordReplace(S, OldClassName, NewClassName);
          end;

    E := GetBackupFileName(FileNames[I]);
    Memo.Lines.Add('Saving original file to ' + E);
    RenameFile(FileNames[I], E);

    if DFM and BinaryDFM then
    begin
      BinaryStream := TFileStream.Create(FileNames[I], fmCreate);
      try
        StringStream := TStringStream.Create(S);
        try
          ObjectTextToResource(StringStream, BinaryStream);
        finally
          StringStream.Free;
        end;
      finally
        BinaryStream.Free;
      end;
    end
    else
    begin
      with TFileStream.Create(FileNames[I], fmCreate) do
      try
        WriteBuffer(S[1], Length(S));
      finally
        Free;
      end;
    end;
  end;
end;

procedure TConverterForm.ShowConversions;
var
  I: Integer;
  C: TConversion;
  CB: TTBXCheckBox;
begin
  ConversionsPanel.DisableAlign;
  for I := ConversionsPanel.ControlCount - 1 downto 0 do
    ConversionsPanel.Controls[I].Free;
  for I := 0 to FConversions.Count - 1 do
  begin
    C := TConversion(FConversions[I]);
    CB := TTBXCheckBox.Create(Self);
    CB.Caption := C.OldClassName + ' > ' + C.NewClassName;
    CB.Wrapping := twEndEllipsis;
    CB.Top := 10000;
    CB.Parent := ConversionsPanel;
    CB.Align := alTop;
    CB.Checked := True;
    C.CheckBox := CB;
  end;
  ConversionsPanel.EnableAlign;
  ConversionsPanel.Realign;
end;

procedure TConverterForm.WMDropFiles(var Message: TWMDropFiles);
var
  SL: TStringList;
  Buffer: array [0..1024] of Char;
  NumFiles, I: Integer;
  S: string;
begin
  SL := TStringList.Create;
  try
    NumFiles := DragQueryFile(Message.Drop, $FFFFFFFF, nil, 0);
    for I := 0 to NumFiles - 1 do
    begin
      DragQueryFile(Message.Drop,I, Buffer, SizeOf(Buffer));
      S := StrPas(Buffer);
      if FileExists(S) then SL.Add(S);
    end;
    if SL.Count > 0 then ProcessFiles(SL);
  finally
    SL.Free;
  end;
end;

procedure TConverterForm.TBXItem1Click(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to FConversions.Count - 1 do
    TConversion(FConversions[I]).CheckBox.Checked := True;
end;

procedure TConverterForm.TBXItem2Click(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to FConversions.Count - 1 do
    TConversion(FConversions[I]).CheckBox.Checked := False;
end;

procedure TConverterForm.ConversionComboPopup(Sender: TTBCustomItem; FromLink: Boolean);
begin
  GetConversionTypes(ConversionCombo.Lines);
end;

procedure TConverterForm.GetConversionTypes(L: TStringListW);
var
  SL: TStringList;
  S: string;
  I, J: Integer;
begin
  SL := TStringList.Create;
  try
    SL.LoadFromFile(CDatFile);
    L.Clear;
    for I := 0 to SL.Count - 1 do
    begin
      S := Trim(SL[I]);
      if (Length(S) > 3) and (S[1] = '[') and (S[Length(S)] = ']') then
      begin
        S := Copy(S, 2, Length(S) - 2);
        L.Add(S);
      end;
    end;
  finally
    SL.Free;
  end;
end;

procedure TConverterForm.ConversionComboChange(Sender: TObject);
begin
  GetConversions(ConversionCombo.Text);
  ShowConversions;
end;

procedure TConverterForm.GetConversions(const ConvType: string);
var
  SL: TStringList;
  S, S1, S2: string;
  P: Integer;
  C: TConversion;
  I: Integer;
begin
  SL := TStringList.Create;
  try
    SL.LoadFromFile(CDatFile);
    I := 0;
    while I < SL.Count - 1 do
    begin
      S := Trim(SL[I]);
      if (Length(S) > 3) and (S[1] = '[') and (S[Length(S)] = ']') and
        SameText(Copy(S, 2, Length(S) - 2), ConvType) then Break
      else Inc(I);
    end;

    FConversions.Clear;
    Inc(I);
    while I < SL.Count do
    begin
      S := Trim(SL[I]);
      Inc(I);
      if (Length(S) = 0) or (S[1] in [';', '''', '/', '%']) then Continue;
      if S[1] = '[' then Break;
      P := Pos('->', S);
      if P < 2 then raise Exception.Create('Invalid conversion settings');
      S1 := Trim(Copy(S, 1, P - 1));
      S2 := Trim(Copy(S, P + 2, Length(S)));
      if (Length(S1) = 0) or (Length(S2) = 0) then raise Exception.Create('Invalid conversion settings');
      C := TConversion.Create;
      FConversions.Add(C);
      C.OldClassName := S1;
      C.NewClassName := S2;
    end;
  finally
    SL.Free;
  end;
end;

procedure TConverterForm.ShellTreeViewAddFolder(Sender: TObject;
  AFolder: TShellFolder; var CanAdd: Boolean);
var
  Ext: string;
begin
  if not AFolder.IsFolder then
  begin
    Ext := LowerCase(ExtractFileExt(AFolder.DisplayName));
    CanAdd :=
      (FilterPas.Checked and (Ext = '.pas')) or
      (FilterHpp.Checked and ((Ext = '.h') or (Ext = '.hpp'))) or
      (FilterCpp.Checked and (Ext = '.cpp')) or
      ((FilterDfm.Checked or FilterBinaryDfm.Checked) and (Ext = '.dfm'));
  end;
end;

procedure TConverterForm.FilterChange(Sender: TObject);
begin
  if FilePanel.Visible then
    ShellTreeView.Refresh(ShellTreeView.Items[0]);
end;

procedure TConverterForm.ShellTreeViewDblClick(Sender: TObject);
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    SL.Add(ShellTreeView.SelectedFolder.PathName);
    if FileExists(SL[0]) then ProcessFiles(SL);
  finally
    SL.Free;
  end;
end;

procedure TConverterForm.TBXVisibilityToggleItem2Click(Sender: TObject);
begin
  if FilePanel.Visible then
    ShellTreeView.Refresh(ShellTreeView.Items[0]);
end;

procedure TConverterForm.MemoDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  I: Integer;
  SL: TStringList;
begin
  if Source <> ShellTreeView then Exit;
  SL := TStringList.Create;
  try
    for I := 0 to ShellTreeView.SelectionCount - 1 do
      SL.Add(TShellFolder(ShellTreeView.Selections[I].Data).PathName);
    ProcessFiles(SL);
  finally
    SL.Free;
  end;
end;

procedure TConverterForm.MemoDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var
  I: Integer;
begin
  if Source <> ShellTreeView then Exit;
  for I := 0 to ShellTreeView.SelectionCount - 1 do
    if not FileExists(TShellFolder(ShellTreeView.Selections[I].Data).PathName) then
    begin
      Accept := False;
      Break;
    end;
end;

procedure TConverterForm.ShellTreeViewEditing(Sender: TObject;
  Node: TTreeNode; var AllowEdit: Boolean);
begin
  AllowEdit := False;
end;

end.
