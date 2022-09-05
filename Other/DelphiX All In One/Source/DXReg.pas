unit DXReg;

interface

{$I DelphiXcfg.inc}
{$WARNINGS OFF}
uses
  Windows, SysUtils, Classes, Forms, Dialogs, Graphics, TypInfo,
  Controls, StdCtrls, ExtCtrls, Buttons,
{$IFDEF D3DRM}Colli3DX, {$ENDIF}
{$IFNDEF VER6UP}DsgnIntf,
{$ELSE}Designintf, DesignEditors, VCLEditors, PropertyCategories,
{$ENDIF}
  DXDraws, DXSounds, DIB, DXWave, DXInput, DXPlay, DXSprite, DXClass;

type

  {  TDXDrawDisplayProperty  }

  TDXDrawDisplayProperty = class(TClassProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

  {  TDIBProperty  }

  TDIBProperty = class(TPropertyEditor)
  public
    procedure Edit; override;
    function GetValue: string; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

  {  TDXDIBEditor  }

  TDXDIBEditor = class(TComponentEditor)
  public
    procedure Edit; override;
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  {  TPictureCollectionItem_PictureProperty  }

  TPictureCollectionItem_PictureProperty = class(TPropertyEditor)
  public
    procedure Edit; override;
    function GetValue: string; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

  {  TDXImageListEditor  }

  TDXImageListEditor = class(TComponentEditor)
  private
    procedure ListBox1DblClick(Sender: TObject);
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  { TDXSpriteEngineEditor}

  TDXSpriteEngineEditor = class(TComponentEditor)
  private
    procedure ListBox1DblClick(Sender: TObject);
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  {  TWaveProperty  }

  TWaveProperty = class(TPropertyEditor)
  public
    procedure Edit; override;
    function GetValue: string; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

  {  TDXWaveEditor  }

  TDXWaveEditor = class(TComponentEditor)
  public
    procedure Edit; override;
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  {  TDXWaveListEditor  }

  TDXWaveListEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  {  TForceFeedbackEffectsProperty  }

  TForceFeedbackEffectsProperty = class(TClassProperty)
  public
    procedure Edit; override;
    function GetValue: string; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

  {  TDXInputEditor  }

  TDXInputEditor = class(TComponentEditor)
  public
    procedure Edit; override;
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  {  TGUIDProperty  }

  TGUIDProperty = class(TStringProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

  {  TSpriteProperty  }

  TSpriteProperty = class(TClassProperty)
  public
    procedure Edit; override;
    function GetValue: string; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

  {  TMidiProperty  }

  TMidiProperty = class(TClassProperty)
  public
    procedure Edit; override;
    function GetValue: string; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

  TMidiEditor = class(TDefaultEditor)
  public
{$IFDEF VER6UP}
    procedure EditProperty(const Prop: IProperty; var Continue: Boolean); override;
{$ELSE}
    procedure EditProperty(PropertyEditor: TPropertyEditor;
      var continue, FreeEditor: Boolean); override;
{$ENDIF}
  end;

  {  TDXMidiListEditor  }

  TDXMidiListEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  {  Trace editor}

  TDXDrawEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

procedure Register;

implementation

uses DXPictEdit, DXWaveEdit, DXFFBEdit, DXInptEdit, DXGUIDEdit, DXSpriteEdit,
  DXMidiEdit, DXDIBEffectEdit, {$IFDEF VER4UP}DXGlueItEdit,{$ENDIF} DXPathEdit;

const
  SNone = '(None)';

  SSettingImage = '&Image...';
  SSettingWave = '&Wave...';
  SDXGFileFilter = 'DXG file(*.dxg)|*.dxg|All files(*.*)|*.*';
  SDXGOpenFileFilter = 'DXG file(*.dxg)|*.dxg|Bitmap file(*.bmp)|*.bmp|All files(*.*)|*.*';
  SDXWFileFilter = 'DXW file(*.dxw)|*.dxg|All files(*.*)|*.*';
  SDXWOpenFileFilter = 'DXW file(*.dxw)|*.dxw|Wave file(*.wav)|*.wav|All files(*.*)|*.*';
  SDXMFileFilter = 'DXM file(*.dxm)|*.dxm|All files(*.*)|*.*';
  SDXMOpenFileFilter = 'DXM file(*.dxm)|*.dxm|Midi file(*.mid)|*.mid|All files(*.*)|*.*';

  SSinglePlayer = '&Single player';
  SMultiPlayer1 = 'Multi player &1';
  SMultiPlayer2 = 'Multi player &2';

  SOpen = '&Open...';
  SSave = '&Save...';

procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(TDXDrawDisplay), nil, '',
    TDXDrawDisplayProperty);

  RegisterPropertyEditor(TypeInfo(TDIB), nil, '', TDIBProperty);
  RegisterComponentEditor(TCustomDXDIB, TDXDIBEditor);

  RegisterPropertyEditor(TypeInfo(TPicture), TPictureCollectionItem, 'Picture', TPictureCollectionItem_PictureProperty);
  RegisterComponentEditor(TCustomDXImageList, TDXImageListEditor);

  RegisterPropertyEditor(TypeInfo(TWave), nil, '', TWaveProperty);
  RegisterComponentEditor(TCustomDXWave, TDXWaveEditor);

  RegisterComponentEditor(TCustomDXWaveList, TDXWaveListEditor);

  RegisterPropertyEditor(TypeInfo(TForceFeedbackEffects), nil, '', TForceFeedbackEffectsProperty);

  RegisterComponentEditor(TCustomDXInput, TDXInputEditor);

  RegisterPropertyEditor(TypeInfo(string), TCustomDXPlay, 'GUID', TGUIDProperty);

  RegisterPropertyEditor(TypeInfo(TImageSprite), NIL, '', TSpriteProperty);
  RegisterPropertyEditor(TypeInfo(TImageSpriteEx), NIL, '', TSpriteProperty);
  RegisterPropertyEditor(TypeInfo(TSprite), NIL, '', TSpriteProperty);
  RegisterPropertyEditor(TypeInfo(TBackgroundSprite), NIL, '', TSpriteProperty);

  RegisterPropertyEditor(TypeInfo(TMusicDataProp), nil, 'MIDI', TMidiProperty);
  RegisterComponentEditor(TDXMusic, TDXMidiListEditor);
  RegisterComponentEditor(TDXSpriteEngine, TDXSpriteEngineEditor);

  RegisterComponents('DelphiX',
    [TDXDraw,
     TDXDIB,
     TDXImageList,
{$IFDEF DX3D_deprecated}
     TDX3D,
{$ENDIF}
     TDXSound,
     TDXWave,
     TDXWaveList,
     TDXInput,
     TDXPlay,
     TDXSpriteEngine,
     TDXTimer,
     TDXPaintBox,
     TDXFont,
     TDXPowerFont,
     TDXMusic
    ]);
  RegisterComponentEditor(TCustomDXDraw, TDXDrawEditor);
end;

{ TDXDrawDisplayProperty }

function TDXDrawDisplayProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paValueList] - [paReadOnly];
end;

const
  SDisplayMode = '%dx%dx%d';

function TDXDrawDisplayProperty.GetValue: string;
begin
  with TDXDrawDisplay(GetOrdValue) do
    Result := Format(SDisplayMode, [Width, Height, BitCount]);
end;

procedure TDXDrawDisplayProperty.GetValues(Proc: TGetStrProc);
const
  List: array[0..2] of TPoint = (
    (X: 640; Y: 480),
    (X: 800; Y: 600),
    (X: 1024; Y: 768));
var
  BitCount, i: Integer;
begin
  for i := Low(List) to High(List) do
    for BitCount := 1 to 3 do
      Proc(Format(SDisplayMode, [List[i].x, List[i].y, BitCount * 8]));
end;

procedure TDXDrawDisplayProperty.SetValue(const Value: string);
var
  s: string;
  i, AWidth, AHeight, ABitCount: Integer;
begin
  s := Value;

  i := Pos('x', s);
  AWidth := StrToInt(Copy(s, 1, i - 1));
  s := Copy(s, i + 1, Length(s));

  i := Pos('x', s);
  AHeight := StrToInt(Copy(s, 1, i - 1));
  s := Copy(s, i + 1, Length(s));

  ABitCount := StrToInt(s);

  with TDXDrawDisplay(GetOrdValue) do
  begin
    Width := AWidth;
    Height := AHeight;
    BitCount := ABitCount;
  end;

  SetOrdValue(GetOrdValue);
end;

{  TDIBProperty  }

procedure TDIBProperty.Edit;
var
  Form: TDelphiXPictureEditForm;
begin
  Form := TDelphiXPictureEditForm.Create(nil);
  try
    Form.ViewBox.Picture.Assign(TDIB(GetOrdValue));
    Form.DIBClassOnly := True;
    Form.ShowModal;
    if Form.Tag <> 0 then
    begin
      SetOrdValue(Integer(Form.ViewBox.Picture.Graphic));
      Designer.Modified;
    end;
  finally
    Form.Free;
  end;
end;

function TDIBProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

function TDIBProperty.GetValue: string;
begin
  if TDIB(GetOrdValue).Size = 0 then
    Result := SNone
  else
    Result := Format('(%s)', [TObject(GetOrdValue).ClassName]);
end;

{  TDXDIBEditor  }

procedure TDXDIBEditor.Edit;
var
  Form: TDelphiXPictureEditForm;
begin
  Form := TDelphiXPictureEditForm.Create(nil);
  try
    Form.ViewBox.Picture.Assign(TCustomDXDIB(Component).DIB);
    Form.DIBClassOnly := True;
    Form.ShowModal;
    if Form.Tag <> 0 then
    begin
      TCustomDXDIB(Component).DIB.Assign(TGraphic(Form.ViewBox.Picture.Graphic));
      Designer.Modified;
    end;
  finally
    Form.Free;
  end;
end;

procedure TDXDIBEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0: Edit;
  end;
end;

function TDXDIBEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := SSettingImage;
  end;
end;

function TDXDIBEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

{  TPictureCollectionItem_PictureProperty  }

procedure TPictureCollectionItem_PictureProperty.Edit;
var
  Form: TDelphiXPictureEditForm;
  Item: TPictureCollectionItem;
  TempDIB: TDIB;
begin
  Form := TDelphiXPictureEditForm.Create(nil);
  try
    Form.ViewBox.Picture := TPicture(GetOrdValue);
    Form.ShowModal;
    if Form.Tag <> 0 then
    begin
      SetOrdValue(Integer(Form.ViewBox.Picture));

      Item := GetComponent(0) as TPictureCollectionItem;
      if Item.Picture.Graphic <> nil then
      begin
        TempDIB := TDIB.Create;
        try
          TempDIB.SetSize(1, 1, 24);
          TempDIB.Canvas.Draw(0, 0, Item.Picture.Graphic);
          Item.TransparentColor := TempDIB.Pixels[0, 0];
        finally
          TempDIB.Free;
        end;
      end;
      Designer.Modified;
    end;
  finally
    Form.Free;
  end;
end;

function TPictureCollectionItem_PictureProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

function TPictureCollectionItem_PictureProperty.GetValue: string;
begin
  if (TPicture(GetOrdValue).Graphic = nil) or (TPicture(GetOrdValue).Graphic.Empty) then
    Result := SNone
  else
    Result := Format('(%s)', [TPicture(GetOrdValue).Graphic.ClassName]);
end;

{  dialog  }
function CreateListBox(DblClck: TNotifyEvent; out lstbx: TListBox): TForm;
var
  Panel1: TPanel;
  Panel2: TPanel;
  BitBtn1: TBitBtn;
  BitBtn2: TBitBtn;
begin
  Result := TForm.Create(nil);
  Panel1 := TPanel.Create(Result);
  lstbx := TListBox.Create(Result);
  Panel2 := TPanel.Create(Result);
  BitBtn1 := TBitBtn.Create(Result);
  BitBtn2 := TBitBtn.Create(Result);
  with Result do
  begin
    Name := 'Form12';
    Left := 0;
    Top := 0;
    BorderStyle := bsDialog;
    Caption := 'Select Item';
    ClientHeight := 206;
    ClientWidth := 228;
    Color := clBtnFace;
    Font.Charset := DEFAULT_CHARSET;
    Font.Color := clWindowText;
    Font.Height := -11;
    Font.Name := 'Tahoma';
    Font.Style := [];
    Position := poScreenCenter;
    PixelsPerInch := 96;
  end;
  with Panel1 do
  begin
    Name := 'Panel1';
    Parent := Result;
    Left := 0;
    Top := 0;
    Width := 228;
    Height := 165;
    Align := alClient;
    BevelOuter := bvNone;
    BorderWidth := 4;
    Caption := '';
    TabOrder := 0;
  end;
  with lstbx do
  begin
    Name := 'ListBox1';
    Parent := Panel1;
    Left := 4;
    Top := 4;
    Width := 220;
    Height := 157;
    Align := alClient;
    ItemHeight := 13;
    TabOrder := 0;
    OnDblClick := DblClck;
  end;
  with Panel2 do
  begin
    Name := 'Panel2';
    Parent := Result;
    Left := 0;
    Top := 165;
    Width := 228;
    Height := 41;
    Align := alBottom;
    BevelOuter := bvNone;
    Caption := '';
    TabOrder := 1;
  end;
  with BitBtn1 do
  begin
    Name := 'BitBtn1';
    Parent := Panel2;
    Left := 24;
    Top := 8;
    Width := 75;
    Height := 25;
    TabOrder := 0;
    Kind := bkOK;
  end;
  with BitBtn2 do
  begin
    Name := 'BitBtn2';
    Parent := Panel2;
    Left := 128;
    Top := 8;
    Width := 75;
    Height := 25;
    TabOrder := 1;
    Kind := bkCancel;
  end;
end;
function Alter(const str, altstr: string): string;
begin
  if str = '' then Result := altstr
  else Result := str;
end;

{  TDXImageListEditor  }

procedure TDXImageListEditor.ExecuteVerb(Index: Integer);
var
  OpenDialog: TOpenDialog;
  SaveDialog: TSaveDialog;
  DelphiXDIBEffectEditForm: TTDelphiXDIBEffectEditForm;
  {$IFDEF VER4UP}
  DXGlueItEditForm: TDXGlueItEditor;
  {$ENDIF}
  Q: TPictureCollectionItem;
  I, N: Integer;
  S, Z: string;
  {$IFDEF VER4UP}
  QQ: TCustomDXImageList;
  FrmListBox: TForm;
  ListBox1: TListBox;
  {$ENDIF}
begin
  case Index of
    0: begin
        OpenDialog := TOpenDialog.Create(nil);
        try
          OpenDialog.DefaultExt := 'dxg';
          OpenDialog.Filter := SDXGOpenFileFilter;
          OpenDialog.Options := [ofPathMustExist, ofFileMustExist, ofAllowMultiSelect];
          if OpenDialog.Execute then
          begin
            if OpenDialog.FilterIndex = 2 then
            begin
              for i := 0 to OpenDialog.Files.Count - 1 do
                with TPictureCollectionItem.Create(TCustomDXImageList(Component).Items) do
                begin
                  try
                    Picture.LoadFromFile(OpenDialog.Files[i]);
                    Name := ExtractFileName(OpenDialog.Files[i]);
                  except
                    Free;
                    raise;
                  end;
                end;
            end else
              TCustomDXImageList(Component).Items.LoadFromFile(OpenDialog.FileName);
            Designer.Modified;
          end;
        finally
          OpenDialog.Free;
        end;
      end;
    1: begin
        SaveDialog := TSaveDialog.Create(nil);
        try
          SaveDialog.DefaultExt := 'dxg';
          SaveDialog.Filter := SDXGFileFilter;
          SaveDialog.Options := [ofOverwritePrompt, ofPathMustExist];
          if SaveDialog.Execute then
            TCustomDXImageList(Component).Items.SaveToFile(SaveDialog.FileName);
        finally
          SaveDialog.Free;
        end;
      end;
    2:
      begin {Create shine effect...}
        {special effect}
        DelphiXDIBEffectEditForm := TTDelphiXDIBEffectEditForm.Create(nil);
        try
          DelphiXDIBEffectEditForm.ShowModal;
          if DelphiXDIBEffectEditForm.Tag = 1 then begin
            {check all names in list of images}
            N := 0;
            Z := DelphiXDIBEffectEditForm.eName.Text; S := Z;
            I := TCustomDXImageList(Component).Items.IndexOf(Z);
            {hleda jmeno}
            while I <> -1 do begin
              S := Format('%s_%d', [Z, N]); {new name}
              I := TCustomDXImageList(Component).Items.IndexOf(S);
              Inc(N);
            end;
            {add item}
            Q := TPictureCollectionItem(TCustomDXImageList(Component).Items.Add);
            Q.Picture.Assign(DelphiXDIBEffectEditForm.ResultDIB);
            Q.Name := S; {it has to name!}
            Q.Transparent := False; {transparend will be set in future}
            Designer.Modified;
          end;
        finally
          DelphiXDIBEffectEditForm.Free;
        end;
      end;
    {$IFDEF VER4UP}
    3:  {Glue-it editor}
      begin
        DXGlueItEditForm := TDXGlueItEditor.Create(nil);
        try
          QQ := TCustomDXImageList(Component); Q := nil;

          if QQ.Items.Count > 0 then begin
            FrmListBox := CreateListBox(ListBox1DblClick, ListBox1);
            try
              for I := 0 to QQ.Items.Count - 1 do begin
                S := QQ.Items[I].Name;
                ListBox1.Items.Add(Alter(S, '(unnamed).' + IntToStr(I)));
              end;

              case FrmListBox.ShowModal of
              mrOk: //when select one
                begin
                  //when image selected
                  if ListBox1.ItemIndex <> -1 then begin
                    Q := QQ.Items[ListBox1.ItemIndex];
                    //load one image into editor
                    DXGlueItEditForm.LoadImageFromList(Q.Name, Q.Picture, Q.Width,
                      Q.Height, Q.Transparent, Q.TransparentColor);
                    //image loadet, noe se up edit mode
                    DXGlueItEditForm.Operation := ogiEdit;
                  end;
                end;
              mrCancel: DXGlueItEditForm.Operation := ogiNew;
              else
                Exit
              end {case};
            finally
              FrmListBox.Free;
            end;
          end
          else
            DXGlueItEditForm.Operation := ogiNew;
          DXGlueItEditForm.ShowModal;
          if DXGlueItEditForm.Tag = 1 then begin
            //when image as new. it has to create new item
            if DXGlueItEditForm.Operation = ogiNew then
              Q := TPictureCollectionItem(TCustomDXImageList(Component).Items.Add);
            //and store edited image into
            if Assigned(Q) then
              DXGlueItEditForm.SaveImageIntoList(Q);
            //signal to designer that anything was changed;
            Designer.Modified;
          end;
        finally
          DXGlueItEditForm.Free;
        end;
      end;
    {$ENDIF}  
  end;
end;

function TDXImageListEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := SOpen;
    1: Result := SSave;
    2: Result := 'Shine effect...';
    {$IFDEF VER4UP}
    //fix bug by Pásztor Károly [fenistil@hu.hu]
    3: Result := 'Glue it...';
    {$ENDIF}
  end;
end;

function TDXImageListEditor.GetVerbCount: Integer;
begin
  Result := {$IFDEF VER4UP}4{$ELSE}3{$ENDIF};
end;

procedure TDXImageListEditor.ListBox1DblClick(Sender: TObject);
begin
  if Sender is TListBox then with (Sender as TListBox) do
    if ItemIndex <> -1 then
      (Owner as TForm).ModalResult := mrOk;
end;

{  TWaveProperty  }

procedure TWaveProperty.Edit;
var
  Form: TDelphiXWaveEditForm;
begin
  Form := TDelphiXWaveEditForm.Create(nil);
  try
    Form.Wave := TWave(GetOrdValue);
    Form.ShowModal;
    if Form.Tag <> 0 then
    begin
      SetOrdValue(Integer(Form.Wave));
      Designer.Modified;
    end;
  finally
    Form.Free;
  end;
end;

function TWaveProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

function TWaveProperty.GetValue: string;
begin
  if TWave(GetOrdValue).Size = 0 then
    Result := SNone
  else
    Result := Format('(%s)', [TObject(GetOrdValue).ClassName]);
end;

{  TDXWaveEditor  }

procedure TDXWaveEditor.Edit;
var
  Form: TDelphiXWaveEditForm;
begin
  Form := TDelphiXWaveEditForm.Create(nil);
  try
    Form.Wave := TCustomDXWave(Component).Wave;
    Form.ShowModal;
    if Form.Tag <> 0 then
    begin
      TCustomDXWave(Component).Wave := Form.Wave;
      Designer.Modified;
    end;
  finally
    Form.Free;
  end;
end;

procedure TDXWaveEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0: Edit;
  end;
end;

function TDXWaveEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := SSettingWave;
  end;
end;

function TDXWaveEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

{  TDXWaveListEditor  }

procedure TDXWaveListEditor.ExecuteVerb(Index: Integer);
var
  OpenDialog: TOpenDialog;
  SaveDialog: TSaveDialog;
  i: Integer;
begin
  case Index of
    0: begin
        OpenDialog := TOpenDialog.Create(nil);
        try
          OpenDialog.DefaultExt := 'dxw';
          OpenDialog.Filter := SDXWOpenFileFilter;
          OpenDialog.Options := [ofPathMustExist, ofFileMustExist, ofAllowMultiSelect];
          if OpenDialog.Execute then
          begin
            if OpenDialog.FilterIndex = 2 then
            begin
              for i := 0 to OpenDialog.Files.Count - 1 do
                with TWaveCollectionItem.Create(TCustomDXWaveList(Component).Items) do
                begin
                  try
                    Wave.LoadFromFile(OpenDialog.Files[i]);
                    Name := ExtractFileName(OpenDialog.Files[i]);
                  except
                    Free;
                    raise;
                  end;
                end;
            end else
              TCustomDXWaveList(Component).Items.LoadFromFile(OpenDialog.FileName);
            Designer.Modified;
          end;
        finally
          OpenDialog.Free;
        end;
      end;
    1: begin
        SaveDialog := TSaveDialog.Create(nil);
        try
          SaveDialog.DefaultExt := 'dxw';
          SaveDialog.Filter := SDXWFileFilter;
          SaveDialog.Options := [ofOverwritePrompt, ofPathMustExist];
          if SaveDialog.Execute then
            TCustomDXWaveList(Component).Items.SaveToFile(SaveDialog.FileName);
        finally
          SaveDialog.Free;
        end;
      end;
  end;
end;

function TDXWaveListEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := SOpen;
    1: Result := SSave;
  end;
end;

function TDXWaveListEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;

{  TForceFeedbackEffectsProperty  }

procedure TForceFeedbackEffectsProperty.Edit;
var
  Form: TDelphiXFFEditForm;
  Effects: TForceFeedbackEffects;
begin
  Effects := TForceFeedbackEffects(GetOrdValue);

  Form := TDelphiXFFEditForm.Create(nil);
  try
    if Effects.Input is TJoystick then
      Form.Effects := Form.DXInput.Joystick.Effects
    else if Effects.Input is TKeyboard then
      Form.Effects := Form.DXInput.Keyboard.Effects
    else if Effects.Input is TMouse then
      Form.Effects := Form.DXInput.Mouse.Effects
    else Exit;

    Form.Effects.Assign(TForceFeedbackEffects(GetOrdValue));
    Form.ShowModal;
    if Form.Tag <> 0 then
    begin
      SetOrdValue(Integer(Form.Effects));
      Designer.Modified;
    end;
  finally
    Form.Free;
  end;
end;

function TForceFeedbackEffectsProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

function TForceFeedbackEffectsProperty.GetValue: string;
begin
  if TForceFeedbackEffects(GetOrdValue).Count = 0 then
    Result := SNone
  else
    Result := Format('(%s)', [TObject(GetOrdValue).ClassName]);
end;

{  TDXInputEditor  }

procedure TDXInputEditor.Edit;
var
  Form: TDelphiXInputEditForm;
begin
  Form := TDelphiXInputEditForm.Create(nil);
  try
    Form.DXInput := TCustomDXInput(Component);
    Form.ShowModal;
    if Form.Tag <> 0 then
      Designer.Modified;
  finally
    Form.Free;
  end;
end;

procedure TDXInputEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0: begin
        with TCustomDXInput(Component) do
        begin
          Joystick.ID := 0;
          Keyboard.KeyAssigns := DefKeyAssign;
        end;
        Designer.Modified;
      end;
    1: begin
        with TCustomDXInput(Component) do
        begin
          Joystick.ID := 0;
          Keyboard.KeyAssigns := DefKeyAssign2_1;
        end;
        Designer.Modified;
      end;
    2: begin
        with TCustomDXInput(Component) do
        begin
          Joystick.ID := 1;
          Keyboard.KeyAssigns := DefKeyAssign2_2;
        end;
        Designer.Modified;
      end;
  end;
end;

function TDXInputEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := SSinglePlayer;
    1: Result := SMultiPlayer1;
    2: Result := SMultiPlayer2;
  end;
end;

function TDXInputEditor.GetVerbCount: Integer;
begin
  Result := 3;
end;

{  TGUIDProperty  }

procedure TGUIDProperty.Edit;
var
  Form: TDelphiXGUIDEditForm;
begin
  Form := TDelphiXGUIDEditForm.Create(nil);
  try
    Form.GUID := GetStrValue;
    Form.ShowModal;
    if Form.Tag <> 0 then
    begin
      SetStrValue(Form.GUID);
      Designer.Modified;
    end;
  finally
    Form.Free;
  end;
end;

function TGUIDProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog];
end;

{ TSpriteProperty }

procedure TSpriteProperty.Edit;
var
  DirectAccessToSprite: TSprite;
  Form: TDelphiXSpriteEditForm;
  //FormDesigner: IDesigner;
begin
  DirectAccessToSprite := TSprite(GetOrdValue);
  //FormDesigner := Designer;
  Form := TDelphiXSpriteEditForm.Create(nil);
  {FormDesigner.GetComponentNames(GetTypeData(GetPropType), Proc);}
  try
    Form.LoadDataToForm(DirectAccessToSprite);
    //Form.Sprite.AsSign(TPersistent(GetOrdValue));
    Form.ShowModal;
    if Form.Tag <> 0 then
    begin
      DirectAccessToSprite := TSprite(Form.SaveDataFromForm);
      SetOrdValue(Integer(DirectAccessToSprite));
      Designer.Modified;
    end;
  finally
    Form.Free;
  end;
end;

function TSpriteProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

function TSpriteProperty.GetValue: string;
begin
  Result := Format('(%s)', [TObject(GetOrdValue).ClassName]);
end;

{  TMidiProperty  }

procedure TMidiProperty.Edit;
var
  DelphiXMidiEditForm: TDelphiXMidiEditForm;
  DirectAccessToMidiData: TMusicDataProp;
  S: string; I: Integer;
begin
  DirectAccessToMidiData := TMusicDataProp(GetOrdValue);
  DelphiXMidiEditForm := TDelphiXMidiEditForm.Create(nil);
  try
    DelphiXMidiEditForm.MidiData := DirectAccessToMidiData.MusicData;
    DelphiXMidiEditForm.MidiFileName := DirectAccessToMidiData.MidiName;
    DelphiXMidiEditForm.Showmodal;
    if DelphiXMidiEditForm.Tag = 1 then begin
      DirectAccessToMidiData.MusicData := DelphiXMidiEditForm.MidiData;
      S := '';
      if DelphiXMidiEditForm.MidiFileName <> '' then begin
        S := ExtractFileName(DelphiXMidiEditForm.MidiFileName);
        I := Pos(ExtractFileExt(S), S);
        if I > 0 then S := Copy(S, 1, I - 1);
      end;
      DirectAccessToMidiData.MidiName := S;
      Designer.Modified;
    end;
  finally
    DelphiXMidiEditForm.Free;
  end;
end;

function TMidiProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

function TMidiProperty.GetValue: string;
var
  S: string;
begin
  S := TMusicDataProp(GetOrdValue).MusicData;
  if Length(S) = 0 then
    Result := SNone
  else
    Result := '(Midi)';
end;

{$IFDEF VER6UP}
procedure TMidiEditor.EditProperty(const Prop: IProperty; var Continue: Boolean);
{$ELSE}
procedure TMidiEditor.EditProperty(PropertyEditor: TPropertyEditor;
  var continue, FreeEditor: Boolean);
{$ENDIF}
var
  PropName: string;
begin
  PropName := {$IFDEF VER6UP}Prop{$ELSE}PropertyEditor{$ENDIF}.GetName;
  if (CompareText(PropName, 'Midi') = 0) then
  begin
{$IFDEF VER6UP}Prop{$ELSE}PropertyEditor{$ENDIF}.edit;
    continue := false;
  end;
end;

{ TDXMidiListEditor }

procedure TDXMidiListEditor.ExecuteVerb(Index: Integer);
var
  OpenDialog: TOpenDialog;
  SaveDialog: TSaveDialog;
  i: Integer;
begin
  case Index of
    0: begin
        OpenDialog := TOpenDialog.Create(nil);
        try
          OpenDialog.DefaultExt := 'dxm';
          OpenDialog.Filter := SDXMOpenFileFilter;
          OpenDialog.Options := [ofPathMustExist, ofFileMustExist, ofAllowMultiSelect];
          if OpenDialog.Execute then
          begin
            if OpenDialog.FilterIndex = 2 then
            begin
              for i := 0 to OpenDialog.Files.Count - 1 do
                with TMusicListCollectionItem.Create(TDXMusic(Component).Midis) do
                begin
                  try
                    LoadFromFile(OpenDialog.Files[i]);
                    Name := ExtractFileName(OpenDialog.Files[i]);
                  except
                    Free;
                    raise;
                  end;
                end;
            end
            else
              TDXMusic(Component).Midis.LoadFromFile(OpenDialog.FileName);
            Designer.Modified;
          end;
        finally
          OpenDialog.Free;
        end;
      end;
    1: begin
        SaveDialog := TSaveDialog.Create(nil);
        try
          SaveDialog.DefaultExt := 'dxm';
          SaveDialog.Filter := SDXMFileFilter;
          SaveDialog.Options := [ofOverwritePrompt, ofPathMustExist];
          if SaveDialog.Execute then
            TCustomDXWaveList(Component).Items.SaveToFile(SaveDialog.FileName);
        finally
          SaveDialog.Free;
        end;
      end;
  end;
end;

function TDXMidiListEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;

function TDXMidiListEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := SOpen;
    1: Result := SSave;
  end;
end;

{ TDXSpriteEngineEditor }

procedure TDXSpriteEngineEditor.ListBox1DblClick(Sender: TObject);
begin
  if Sender is TListBox then with (Sender as TListBox) do
    if ItemIndex <> -1 then
      (Owner as TForm).ModalResult := mrOk;
end;

procedure TDXSpriteEngineEditor.ExecuteVerb(Index: Integer);
var
  FrmListBox: TForm;
  ListBox1: TListBox;
  DelphiXSpriteEditForm: TDelphiXSpriteEditForm;
  ASprite: TSprite;
  I, Z: Integer;
  S: string;
  Q: TCustomDXSpriteEngine;
begin
  case Index of
    0: begin
        FrmListBox := nil;
        Z := 0; //default value
        DelphiXSpriteEditForm := TDelphiXSpriteEditForm.Create(nil);
        try
          Q := TCustomDXSpriteEngine(Component);
          case Q.Items.Count of
            0: begin
              ShowMessage('You must create any item of sprite first!');
              Exit;
            end;
            1: ASprite := Q.Items[Z].Sprite;
          else
            FrmListBox := CreateListBox(ListBox1DblClick, ListBox1);
            for I := 0 to Q.Items.Count - 1 do begin
              S := Q.Items[I].Name;
              ListBox1.Items.Add(Alter(S, '(unnamed).' + IntToStr(I)));
            end;
            if FrmListBox.ShowModal <> mrOk then Exit;
            Z := ListBox1.ItemIndex;
            if Z = -1 then Exit;
            ASprite := Q.Items[Z].Sprite;
            {synchronize of names}
            if ASprite.Caption = '' then
              if Q.Items[ListBox1.ItemIndex].Name <> '' then
                ASprite.Caption := Q.Items[Z].Name;
          end {case};
          DelphiXSpriteEditForm.LoadDataToForm(ASprite);
          DelphiXSpriteEditForm.ShowModal;
          if DelphiXSpriteEditForm.Tag <> 0 then begin
            ASprite := TSprite(DelphiXSpriteEditForm.SaveDataFromForm);
            if Q.Items[Z].Name = '' then
              if ASprite.Caption <> '' then
                Q.Items[Z].Name := ASprite.Caption;
            Designer.Modified;
          end;
        finally
          if Assigned(FrmListBox) then FrmListBox.Free;
          DelphiXSpriteEditForm.Free;
        end;
      end;
  end;
end;

function TDXSpriteEngineEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

function TDXSpriteEngineEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'Sprite Editor';
  end;
end;

{ TDXDrawEditor }

procedure TDXDrawEditor.ExecuteVerb(Index: Integer);
var
  ediform: TDelphiXPathsEditForm;
  Q: TCustomDXDraw;
  I: Integer;
  S: string;
  T: TTrace;
  {$IFNDEF VER4UP}
  H: TTrace;
  J: Integer;
  {$ENDIF}
begin
  case Index of
    0: begin
      Q := TCustomDXDraw(Component);
      {paths editor}
      ediform := TDelphiXPathsEditForm.Create(nil);
      try
        ediform.Pane.Width := Q.Display.Width;
        ediform.Pane.Height := Q.Display.Width;
        for I := 0 to Q.Traces.Count - 1 do begin
          S := Q.Traces.Items[I].Name;
          T := ediform.PrivateTraces.Add;
          T.Name := S;
          {$IFDEF VER4UP}
          T.Assign(Q.Traces.Items[I]);
          {$ELSE}
          T.Blit := Q.Traces.Items[I].Blit;
          {$ENDIF}
          if Trim(S) = '' then S := Format('(unnamed[%d])', [I]);
          ediform.cbListOfTraces.Items.Add(S);
        end;
        ediform.ShowTracesOnPane;

        ediform.ShowModal;

        if ediform.Tag = 1 then begin
          {clear traces}
          Q.Traces.Clear;
          {rewrite backward}
          for i := 0 to ediform.PrivateTraces.Count -1 do begin
            T := Q.Traces.Add;
            T.Name := ediform.PrivateTraces.Items[I].Name;
            {$IFDEF VER4UP}
            T.Assign(ediform.PrivateTraces.Items[i]);
            {$ELSE}
            H := ediform.PrivateTraces.Items[i];
            T.Blit := H.Blit;
            T.Blit.SetPathLen(H.Blit.GetPathCount);
            for J := 0 to H.Blit.GetPathCount - 1 do begin
              T.Blit.Path[J] := H.Blit.Path[J]
            end
            {$ENDIF}
          end;
          {prepis zmeny}
          Designer.Modified;
        end;
      finally
        ediform.Free;
      end;
    end;
  end;
end;

function TDXDrawEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

function TDXDrawEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'Traces Editor';
  end;
end;

end.