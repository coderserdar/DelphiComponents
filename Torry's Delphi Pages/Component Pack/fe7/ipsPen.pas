unit ipsPen;

interface

uses Classes, Controls, Graphics, StdCtrls, ExtCtrls, SysUtils, iGrCombo;

type

        TpsEditorType = (etPenEditor, etBrushEditor);

        TpsPenBrushEditor = class(TPanel)
        private
                FPen    : TPen;
                FBrush  : TBrush;
                E_COLOR : TpsColorComboBox;
                E_STYLE : TpsColorComboBox;
                E_MODE  : TpsColorComboBox;
                E_WIDTH : TEdit;
                Lbl_Color, Lbl_Style, Lbl_Mode, lbl_width : TLabel;

                FOffsetLabels: Integer;
                FOffsetControls: Integer;
                FVerticalOffset: Integer;
                FBoldStyle: Boolean;
                FEditorType: TpsEditorType;

                procedure SetPen(const Value: TPen);
                procedure SetOffsetLabels(const Value: Integer);
                procedure SetOffsetControls(const Value: Integer);
                procedure SetVerticalOffset(const Value: Integer);
                procedure SetBoldStyle(const Value: Boolean);
                procedure SetEditorType(const Value: TpsEditorType);
                procedure SetBrush(const Value: TBrush);
        public
                constructor Create(AOwner:TComponent); override;
                destructor  Destroy; override;
                procedure   CreateWindowHandle(const Params:TCreateParams); override;
                procedure   ArrangeControls; virtual;
                procedure   ControlEnter(Sender:TObject);
                procedure   ControlExit(Sender:TObject);
                procedure   UpdatePenBrush(Sender:TObject);
                procedure   UpdateControls(Sender:TObject);
                // procedure   SetBounds(ALeft,ATop,ARight,ABottom:Integer); override;
                // procedure   Loaded; override;
        published
                property Pen:TPen read FPen write SetPen;
                property Brush:TBrush read FBrush write SetBrush;
                property OffsetLabels : Integer read FOffsetLabels write SetOffsetLabels;
                property OffsetControls:Integer read FOffsetControls write SetOffsetControls;
                property VerticalOffset:Integer read FVerticalOffset write SetVerticalOffset;
                property BoldStyle:Boolean read FBoldStyle write SetBoldStyle;
                property EditorType:TpsEditorType read FEditorType write SetEditorType;
        end;

procedure UpdateFocusControl(Parent:TWinControl; Control:TControl; Show:Boolean);



implementation

{ TpsPenBrushEditor }

uses ipsConst;

procedure CreateLabel(Parent:TPanel; var lbl:TLabel;
        Caption : String; FocusControl:TWinControl);
begin
        lbl            := TLabel.Create(Parent);
        Lbl.Parent     := Parent;
        Lbl.Caption    := Caption;
        Lbl.ParentFont := True;
        Lbl.FocusControl := FocusControl;
end;

procedure UpdateFocusControl(Parent:TWinControl; Control:TControl; Show:Boolean);
var i:Integer;
    L:TLabel;
begin
        for i:=0 to Parent.ComponentCount-1 do
                if Parent.Components[i] is TCustomLabel then begin
                        L:=TLabel(Parent.Components[i]);
                        if L.FocusControl=Control then begin
                                if Show then L.Font.Style := L.Font.Style + [fsBold]
                                else         L.Font.Style := L.Font.Style - [fsBold];
                                Break;
                        end;
                end;
end;

procedure TpsPenBrushEditor.ArrangeControls;
var dy,w:Integer;

        procedure MovePSControl(C:TControl; L:Integer; var T:Integer; W:Integer);
        begin
                C.Left := L;
                C.Top  := T;
                if W>0 then C.Width := W;
                Inc(T, VerticalOffset);
                C.Visible := True;
        end;
begin
    if not HandleAllocated then Exit;


    dy := 5;
    E_COLOR.TypeOfBox := tyColor;
    MovePSControl(lbl_color, OffsetLabels, dy, 0 );
    MovePSControl(lbl_Style, OffsetLabels, dy, 0 );

    if FEditorType=etPenEditor then begin
        MovePSControl(lbl_Mode,  OffsetLabels, dy, 0);
        MovePSControl(lbl_Width, OffsetLabels, dy, 0);
    end else begin
        lbl_mode.Visible  := False;
        lbl_width.Visible := False;
    end;

    dy := 5;
    W  := Width - 2 - OffsetControls;

    MovePSControl(E_color, OffsetControls, dy, W);
    MovePSControl(E_Style, OffsetControls, dy, W);
    if FEditorType=etPenEditor then begin
        MovePSControl(E_Mode,  OffsetControls, dy, W);
        MovePSControl(E_Width, OffsetControls, dy, W);
        E_STYLE.TypeOfBox := tyPenStyle;
    end else begin
        E_MODE.Visible    := False;
        E_WIDTH.Visible   := False;
        E_STYLE.TypeOfBox := tyBrush;
    end;
    Height := dy;
end;

constructor TpsPenBrushEditor.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);

  FEditorType     := etPenEditor;

  FPen            := TPen.Create;
  FPen.OnChange   := UpdateControls;
  FBrush          := TBrush.Create;
  FBrush.OnChange := UpdateControls;

  FOffsetLabels   := 5;
  FOffsetControls := 80;
  FVerticalOffset := 24;
  FBoldStyle      := True;

  E_COLOR           := TpsColorComboBox.Create(Self);
  E_COLOR.Parent    := Self;
  E_COLOR.Name      := Name+'_COLOR';
  E_COLOR.OnEnter   := ControlEnter;
  E_COLOR.OnExit    := ControlExit;
  E_COLOR.OnChange  := UpdatePenBrush;

  E_STYLE := TpsColorComboBox.Create(Self);
  E_STYLE.Parent    := Self;
  E_STYLE.Name      := Name+'_STYLE';
  E_STYLE.OnEnter   := ControlEnter;
  E_STYLE.OnExit    := ControlExit;
  E_STYLE.OnChange  := UpdatePenBrush;

  E_MODE  := TpsColorComboBox.Create(Self);
  E_MODE.Parent    := Self;
  E_MODE.Name      := Name+'_MODE';
  E_MODE.OnEnter   := ControlEnter;
  E_MODE.OnExit    := ControlExit;
  E_MODE.OnChange  := UpdatePenBrush;

  E_WIDTH := TEdit.Create(Self);
  E_WIDTH.Parent := Self;
  E_WIDTH.Name      := Name+'_WIDTH';
  E_WIDTH.Text      := '';
  E_WIDTH.OnEnter   := ControlEnter;
  E_WIDTH.OnExit    := ControlExit;
  E_WIDTH.OnChange  := UpdatePenBrush;

  CreateLabel(Self, lbl_color, rsColor,  E_COLOR);
  CreateLabel(Self, lbl_Style, rsStyle,  E_STYLE);
  CreateLabel(Self, lbl_Mode,  rsMode,   E_MODE);
  CreateLabel(Self, lbl_Width, rsWidth,  E_WIDTH);

  ArrangeControls;
end;

destructor TpsPenBrushEditor.Destroy;
begin
  FPen.Free;
  FBrush.Free;
  inherited Destroy;
end;

procedure TpsPenBrushEditor.CreateWindowHandle(const Params:TCreateParams);
begin
     inherited CreateWindowHandle(Params);
     E_COLOR.TypeOfBox := tyColor;
     E_STYLE.TypeOfBox := tyPenStyle;
     E_MODE.TypeOfBox := tyPenMode;
     ArrangeControls;
     Caption := '';
end;


procedure TpsPenBrushEditor.SetOffsetControls(const Value: Integer);
begin
        if FOffsetControls<>Value then begin
                FOffsetControls := Value;
                ArrangeControls;
        end;
end;

procedure TpsPenBrushEditor.SetOffsetLabels(const Value: Integer);
begin
        if FOffsetLabels<>Value then begin
                FOffsetLabels := Value;
                ArrangeControls;
        end;
end;

procedure TpsPenBrushEditor.SetPen(const Value: TPen);
begin
        FPen.Assign(Value);
        UpdateControls(Self);
end;

procedure TpsPenBrushEditor.SetVerticalOffset(const Value: Integer);
begin
        if FVerticalOffset<>Value then begin
                FVerticalOffset := Value;
                ArrangeControls;
        end;
end;

procedure TpsPenBrushEditor.ControlEnter(Sender: TObject);
begin
        if BoldStyle then
                UpdateFocusControl(Self, TControl(Sender), True);
end;

procedure TpsPenBrushEditor.SetBoldStyle(const Value: Boolean);
begin
  FBoldStyle := Value;
end;

procedure TpsPenBrushEditor.ControlExit(Sender: TObject);
begin
        if BoldStyle then
                UpdateFocusControl(Self, TControl(Sender), False);
end;

procedure TpsPenBrushEditor.UpdatePenBrush(Sender:TObject);
begin
     if FEditorType=etPenEditor then begin
        FPen.Color := E_COLOR.SelectedColor;
        FPen.Mode  := E_MODE.SelectedPenMode;
        FPen.Style := E_STYLE.SelectedPenStyle;
        FPen.Width := StrToIntDef(E_Width.Text,1);
     end else begin
        FBrush.Color := E_COLOR.SelectedColor;
        FBrush.Style := TBrushStyle(E_STYLE.ItemIndex);
     end;
end;

procedure TpsPenBrushEditor.UpdateControls(Sender:TObject);
begin
    if FEditorType=etPenEditor then begin
        E_COLOR.SelectedColor    := FPen.Color;
        E_MODE.SelectedPenMode   := FPen.Mode;
        E_STYLE.SelectedPenStyle := FPen.Style;
        E_Width.Text             := IntToStr(FPen.Width);
    end else begin
        E_COLOR.SelectedColor    := FBrush.Color;
        E_STYLE.ItemIndex        := Integer(FBrush.Style);
    end;
end;



procedure TpsPenBrushEditor.SetEditorType(const Value: TpsEditorType);
begin
        if FEditorType<>Value then begin
                FEditorType := Value;
                ArrangeControls;
        end;
end;

procedure TpsPenBrushEditor.SetBrush(const Value: TBrush);
begin
        FBrush.Assign(Value);
        UpdateControls(Self);
end;


end.
