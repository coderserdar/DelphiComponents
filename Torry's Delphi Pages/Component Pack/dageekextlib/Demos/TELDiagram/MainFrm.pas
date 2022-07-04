unit MainFrm;

interface

uses
    Windows, Messages, SysUtils, {$IFNDEF VER130}Variants, {$ENDIF}Classes,
    Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, ELDgrm;

type
	TfrmMain = class(TForm)
        ELDiagram1: TELDiagram;
        Memo1: TMemo;
        Label3: TLabel;
		CheckBox8: TCheckBox;
        CheckBox9: TCheckBox;
        Button5: TButton;
        Panel1: TPanel;
        Button1: TButton;
        Button2: TButton;
        Label1: TLabel;
        CheckBox1: TCheckBox;
        CheckBox2: TCheckBox;
        procedure ELDiagram1Change(Sender: TObject);
        procedure ELDiagram1ChangeSelection(Sender: TObject);
        procedure ELDiagram1Click(Sender: TObject);
        procedure ELDiagram1DblClick(Sender: TObject);
        procedure ELDiagram1DeleteItem(Sender: TObject;
            AItem: TElDiagramItem);
        procedure ELDiagram1DeleteLink(Sender: TObject;
            ALink: TElDiagramLink);
        procedure ELDiagram1Enter(Sender: TObject);
        procedure ELDiagram1Exit(Sender: TObject);
        procedure ELDiagram1InsertItem(Sender: TObject;
            AItem: TElDiagramItem);
        procedure ELDiagram1InsertLink(Sender: TObject;
            ALink: TElDiagramLink);
        procedure ELDiagram1KeyDown(Sender: TObject; var Key: Word;
            Shift: TShiftState);
        procedure ELDiagram1KeyPress(Sender: TObject; var Key: Char);
        procedure ELDiagram1KeyUp(Sender: TObject; var Key: Word;
			Shift: TShiftState);
        procedure ELDiagram1MouseDown(Sender: TObject; Button: TMouseButton;
            Shift: TShiftState; X, Y: Integer);
        procedure ELDiagram1MouseMove(Sender: TObject; Shift: TShiftState; X,
            Y: Integer);
        procedure ELDiagram1MouseUp(Sender: TObject; Button: TMouseButton;
            Shift: TShiftState; X, Y: Integer);
        procedure Button5Click(Sender: TObject);
        procedure Panel1DragOver(Sender, Source: TObject; X, Y: Integer;
            State: TDragState; var Accept: Boolean);
        procedure FormDestroy(Sender: TObject);
        procedure Panel1DragDrop(Sender, Source: TObject; X, Y: Integer);
        procedure Button1Click(Sender: TObject);
        procedure Button2Click(Sender: TObject);
        procedure ELDiagram1DragOver(Sender, Source: TObject; X, Y: Integer;
            State: TDragState; var Accept: Boolean);
        procedure ELDiagram1DragDrop(Sender, Source: TObject; X, Y: Integer);
        procedure ELDiagram1PaintItem(Sender: TObject;
            APanel: TElDiagramItemPanel);
        procedure CheckBox1Click(Sender: TObject);
        procedure CheckBox2Click(Sender: TObject);
        procedure ELDiagram1CreateLinkDrawInfo(Sender: TObject;
            ALink: TElDiagramLink; var ADrawInfo: TElLinkDrawInfo);
        procedure ELDiagram1IsEqualDrawInfos(Sender: TObject; ADrawInfo1,
            ADrawInfo2: TElLinkDrawInfo; var AIsEqual: Boolean);
        procedure ELDiagram1IsOnLink(Sender: TObject;
            ADrawInfo: TElLinkDrawInfo; AX, AY: Integer;
			var AIsOnLink: Boolean);
        procedure ELDiagram1PaintLink(Sender: TObject;
            ADrawInfo: TElLinkDrawInfo; AClear: Boolean);
    private
        { Private declarations }
    public
        { Public declarations }
    end;

    TCustomLinkDrawInfo = class(TElLinkDrawInfo)
        BeginP: TPoint;
        EndP: TPoint;
        Color: TColor;
        Selected: Boolean;
    end;

var
    frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.ELDiagram1Change(Sender: TObject);
begin
    Memo1.Lines.Add('OnChange');
end;

procedure TfrmMain.ELDiagram1ChangeSelection(Sender: TObject);
begin
    Memo1.Lines.Add('OnChangeSelection');
end;

procedure TfrmMain.ELDiagram1Click(Sender: TObject);
begin
    Memo1.Lines.Add('OnClick');
end;

procedure TfrmMain.ELDiagram1DblClick(Sender: TObject);
begin
    Memo1.Lines.Add('OnDblClick');
end;

procedure TfrmMain.ELDiagram1DeleteItem(Sender: TObject;
    AItem: TElDiagramItem);
begin
    Memo1.Lines.Add(Format('OnDeleteItem: %s', [AItem.DisplayName]));
end;

procedure TfrmMain.ELDiagram1DeleteLink(Sender: TObject;
    ALink: TElDiagramLink);
begin
    Memo1.Lines.Add(Format('OnDeleteLink: %s', [ALink.DisplayName]));
end;

procedure TfrmMain.ELDiagram1Enter(Sender: TObject);
begin
    Memo1.Lines.Add('OnEnter');
end;

procedure TfrmMain.ELDiagram1Exit(Sender: TObject);
begin
    Memo1.Lines.Add('OnExit');
end;

procedure TfrmMain.ELDiagram1InsertItem(Sender: TObject;
    AItem: TElDiagramItem);
begin
    Memo1.Lines.Add(Format('OnInsertItem: %s', [AItem.DisplayName]));
end;

procedure TfrmMain.ELDiagram1InsertLink(Sender: TObject;
    ALink: TElDiagramLink);
begin
    Memo1.Lines.Add(Format('OnInsertLink: %s', [ALink.DisplayName]));
end;

procedure TfrmMain.ELDiagram1KeyDown(Sender: TObject; var Key: Word;
    Shift: TShiftState);
begin
    if CheckBox9.Checked then
        Memo1.Lines.Add(Format('OnKeyDown: (Key: $%x)', [Key]));
end;

procedure TfrmMain.ELDiagram1KeyUp(Sender: TObject; var Key: Word;
    Shift: TShiftState);
begin
    if CheckBox9.Checked then
        Memo1.Lines.Add(Format('OnKeyUp: (Key: %x)', [Key]));
end;

procedure TfrmMain.ELDiagram1MouseDown(Sender: TObject;
    Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
    LItem: TElDiagramItem;
    LLink: TElDiagramLink;
    LS, LS1: string;
begin
    if CheckBox8.Checked then
        begin
            LItem := ELDiagram1.Items.ItemAtPos(X, Y);
            LLink := ELDiagram1.Links.ItemAtPos(X, Y);
            if LItem <> nil then
                LS := Format('ItemAtPos: %s', [LItem.DisplayName])
            else
                LS := 'ItemAtPos: nil';
            if LLink <> nil then
                LS1 := Format('LinkAtPos: %s', [LLink.DisplayName])
            else
                LS1 := 'LinkAtPos: nil';
            Memo1.Lines.Add(Format('OnMouseDown: (X: %d, Y: %d, %s, %s)',
                [X, Y, LS, LS1]));
        end;
end;

procedure TfrmMain.ELDiagram1MouseMove(Sender: TObject; Shift: TShiftState;
    X, Y: Integer);
var
    LItem: TElDiagramItem;
    LLink: TElDiagramLink;
    LS, LS1: string;
begin
    if CheckBox8.Checked then
        begin
            LItem := ELDiagram1.ITems.ItemAtPos(X, Y);
            LLink := ELDiagram1.Links.ItemAtPos(X, Y);
            if LItem <> nil then
                LS := Format('ItemAtPos: %s', [LItem.DisplayName])
            else
                LS := 'ItemAtPos: nil';
            if LLink <> nil then
                LS1 := Format('LinkAtPos: %s', [LLink.DisplayName])
            else
                LS1 := 'LinkAtPos: nil';
            Memo1.Lines.Add(Format('OnMouseMove: (X: %d, Y: %d, %s, %s)',
                [X, Y, LS, LS1]));
        end;
end;

procedure TfrmMain.ELDiagram1MouseUp(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer);
var
    LItem: TElDiagramItem;
    LLink: TElDiagramLink;
    LS, LS1: string;
begin
    if CheckBox8.Checked then
        begin
            LItem := ELDiagram1.ITems.ItemAtPos(X, Y);
            LLink := ELDiagram1.Links.ItemAtPos(X, Y);
            if LItem <> nil then
                LS := Format('ItemAtPos: %s', [LItem.DisplayName])
            else
                LS := 'ItemAtPos: nil';
            if LLink <> nil then
                LS1 := Format('LinkAtPos: %s', [LLink.DisplayName])
            else
                LS1 := 'LinkAtPos: nil';
            Memo1.Lines.Add(Format('OnMouseUp: (X: %d, Y: %d, %s, %s)',
                [X, Y, LS, LS1]));
        end;
end;

procedure TfrmMain.Button5Click(Sender: TObject);
begin
    Memo1.Lines.Clear;
end;

procedure TfrmMain.Panel1DragOver(Sender, Source: TObject; X, Y: Integer;
    State: TDragState; var Accept: Boolean);
var
    LSource, LItem: string;
begin
    if Source <> nil then
        LSource := (Source as TComponent).Name
    else
        LSource := 'nil';
    if (Source is TElDiagram) and (TElDiagram(Source).SelectedItem <> nil) then
        LItem := TElDiagram(Source).SelectedItem.DisplayName
    else
        LItem := 'nil';

    Memo1.Lines.Add(Format('Panel1.OnDragOver: %s (Item: %s) -> %s (X: %d, Y: %d)',
        [LSource, LItem, Panel1.Name, X, Y]));
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
    ELDiagram1.Free;
end;

procedure TfrmMain.Panel1DragDrop(Sender, Source: TObject; X, Y: Integer);
var
    LSource, LItem: string;
begin
    if Source <> nil then
        LSource := (Source as TComponent).Name
    else
        LSource := 'nil';
    if (Source is TElDiagram) and (TElDiagram(Source).SelectedItem <> nil) then
        LItem := TElDiagram(Source).SelectedItem.DisplayName
    else
        LItem := 'nil';

    Memo1.Lines.Add(Format('Panel1.OnDragDrop: %s (Item: %s) -> %s (X: %d, Y: %d)',
        [LSource, LItem, Panel1.Name, X, Y]));
end;

procedure TfrmMain.Button1Click(Sender: TObject);
begin
    ELDiagram1.Items.Add;
end;

procedure TfrmMain.Button2Click(Sender: TObject);
begin
    if ELDiagram1.SelectedItem <> nil then
        ELDiagram1.SelectedItem.Free;
    if ELDiagram1.SelectedLink <> nil then
        ELDiagram1.SelectedLink.Free;
end;

procedure TfrmMain.ELDiagram1DragOver(Sender, Source: TObject; X,
    Y: Integer; State: TDragState; var Accept: Boolean);
begin
    if (Source = ELDiagram1) and (ELDiagram1.SelectedItem <> nil) and
        (ELDiagram1.ITems.ItemAtPos(X, Y) <> nil) and
        (ELDiagram1.SelectedItem <> ELDiagram1.ITems.ItemAtPos(X, Y)) then
        Accept := True
    else
        Accept := False;
end;

procedure TfrmMain.ELDiagram1DragDrop(Sender, Source: TObject; X,
    Y: Integer);
begin
    if (Source = ELDiagram1) and (ELDiagram1.SelectedItem <> nil) and
        (ELDiagram1.ITems.ItemAtPos(X, Y) <> nil) and
        (ELDiagram1.SelectedItem <> ELDiagram1.ITems.ItemAtPos(X, Y)) then
        with TElDiagramLink(ELDiagram1.Links.Add) do
            begin
                BeginItemName := ELDiagram1.SelectedItem.Name;
                EndItemName := ELDiagram1.ITems.ItemAtPos(X, Y).Name;
            end;
end;

procedure TfrmMain.ELDiagram1KeyPress(Sender: TObject; var Key: Char);
begin
    if CheckBox9.Checked then
        Memo1.Lines.Add(Format('OnKeyPress: (Key: "%s")', [Key]));
end;

procedure TfrmMain.ELDiagram1PaintItem(Sender: TObject;
    APanel: TElDiagramItemPanel);
var
    LI: Integer;
    LRect: TRect;
begin
    APanel.Canvas.Pen.Color := clBtnShadow;
    APanel.Canvas.MoveTo(10, 0);
    APanel.Canvas.LineTo(10, APanel.ClientHeight - 10);
    LI := 10;
    while LI < APanel.ClientHeight - 10 do
        begin
            APanel.Canvas.MoveTo(8, LI);
            APanel.Canvas.LineTo(13, LI);
            Inc(LI, 10);
        end;
    APanel.Canvas.MoveTo(10, APanel.ClientHeight - 10);
    APanel.Canvas.LineTo(APanel.ClientWidth, APanel.ClientHeight - 10);
    LI := 10;
    while LI < APanel.ClientWidth - 10 do
        begin
            APanel.Canvas.MoveTo(LI, APanel.ClientHeight - 8);
            APanel.Canvas.LineTo(LI, APanel.ClientHeight - 13);
            Inc(LI, 10);
        end;

    APanel.Canvas.Pen.Color := clRed;
    APanel.Canvas.MoveTo(10, APanel.ClientHeight - 10);
    for LI := 10 to APanel.ClientWidth - 1 do
        APanel.Canvas.LineTo(LI, APanel.ClientHeight - (LI - 10) * (LI - 10) div 70 - 10);

    APanel.Canvas.TextOut(20, 5, 'y = F(x)');

    if APanel.Focused then
        begin
            LRect := APanel.ClientRect;
            InflateRect(LRect, -2, -2);
            APanel.Canvas.DrawFocusRect(LRect);
        end;
end;

procedure TfrmMain.CheckBox1Click(Sender: TObject);
begin
    ELDiagram1.CustomDrawItems := CheckBox1.Checked;
end;

procedure TfrmMain.CheckBox2Click(Sender: TObject);
begin
    ELDiagram1.CustomDrawLinks := CheckBox2.Checked;
end;

procedure TfrmMain.ELDiagram1CreateLinkDrawInfo(Sender: TObject;
    ALink: TElDiagramLink; var ADrawInfo: TElLinkDrawInfo);
begin
    // Create TCustomLinkDrawInfo object and fill it with information
    // for drawing link. This information must be sufficient for drawing link.
    // Later this information will be used to draw the ALink object.
    // You may not use any other information (except represented in the ADrawInfo),
    // including ALink properties for drawing link and custom
    // ADrawInfo object may not contains references to other objects.
    // You may not offset any coordinates using diagram HScrollPos and VScrollPos
    // properties.

    if (ALink.BeginItem <> nil) and (ALink.EndItem <> nil) and
        (ALink.BeginItem.Visible) and (ALink.EndItem.Visible) and ALink.Visible then
        begin
            ADrawInfo := TCustomLinkDrawInfo.Create;
            with TCustomLinkDrawInfo(ADrawInfo) do
                begin
                    BeginP := Point(ALink.BeginItem.Left + ALink.BeginItem.Width div 2,
                        ALink.BeginItem.Top - 10);
                    EndP := Point(ALink.EndItem.Left + ALink.EndItem.Width div 2,
                        ALink.EndItem.Top - 10);
                    Color := ALink.Color;
                    Selected := ALink.Selected;
                end;
        end;
end;

procedure TfrmMain.ELDiagram1IsEqualDrawInfos(Sender: TObject; ADrawInfo1,
    ADrawInfo2: TElLinkDrawInfo; var AIsEqual: Boolean);
begin
    // Compare draw infos. This is used to optimize links drawing.
    AIsEqual :=
        (TCustomLinkDrawInfo(ADrawInfo1).BeginP.X = TCustomLinkDrawInfo(ADrawInfo2).BeginP.X) and
        (TCustomLinkDrawInfo(ADrawInfo1).BeginP.Y = TCustomLinkDrawInfo(ADrawInfo2).BeginP.Y) and
        (TCustomLinkDrawInfo(ADrawInfo1).EndP.X = TCustomLinkDrawInfo(ADrawInfo2).EndP.X) and
        (TCustomLinkDrawInfo(ADrawInfo1).EndP.Y = TCustomLinkDrawInfo(ADrawInfo2).EndP.Y) and
        (TCustomLinkDrawInfo(ADrawInfo1).Color = TCustomLinkDrawInfo(ADrawInfo2).Color) and
        (TCustomLinkDrawInfo(ADrawInfo1).Selected = TCustomLinkDrawInfo(ADrawInfo2).Selected);
end;

procedure TfrmMain.ELDiagram1IsOnLink(Sender: TObject;
    ADrawInfo: TElLinkDrawInfo; AX, AY: Integer; var AIsOnLink: Boolean);

const
    LR = 3;

var
    LXMin, LXMax, LYMin, LYMax: Integer;
    LA, LB, LA_2, LB_2, LC_2: Double;
    LP, LP1, LP2: TPoint;
    LDI: TCustomLinkDrawInfo;

begin
    // Check if the Point(AX, AY) is on Line(ADrawInfo.BeginP; ADrawInfo.EndP)
    // or near this line (e.g. the circle with (AX, AY) center and LR radius
    // intersects the Line). This is used when user clicks diagram to selecting
    // links and in the LinkAtPos function.

    LDI := ADrawInfo as TCustomLinkDrawInfo;

    LP := Point(AX, AY);
    LP1 := LDI.BeginP;
    Dec(LP1.X, ELDiagram1.HScrollPos);
    Dec(LP1.Y, ELDiagram1.VScrollPos);
    LP2 := LDI.EndP;
    Dec(LP2.X, ELDiagram1.HScrollPos);
    Dec(LP2.Y, ELDiagram1.VScrollPos);

    { Calc Min and Max }
    if LP1.X < LP2.X then
        begin
            LXMin := LP1.X;
            LXMax := LP2.X;
        end
    else
        begin
            LXMin := LP2.X;
            LXMax := LP1.X;
        end;
    if LP1.Y < LP2.Y then
        begin
            LYMin := LP1.Y;
            LYMax := LP2.Y;
        end
    else
        begin
            LYMin := LP2.Y;
            LYMax := LP1.Y;
        end;

    { Check }
    AIsOnLink := (LP.Y - LR <= LYMax) and (LP.Y + LR >= LYMin) and
        (LP.X - LR <= LXMax) and (LP.X + LR >= LXMin);
    if AIsOnLink and not ((LXMin = LXMax) or (LYMin = LYMax)) then
        begin
            LA := (LP1.Y - LP2.Y) / (LP1.X - LP2.X);
            LB := LP1.Y - LA * LP1.X;

            LA_2 := 1 + LA * LA;
            LB_2 := 2 * LA * LB - 2 * LP.X - 2 * LA * LP.Y;
            LC_2 := LP.X * LP.X + LB * LB - 2 * LB * LP.Y + LP.Y * LP.Y - LR * LR;

            if (LB_2 * LB_2 - 4 * LA_2 * LC_2) < 0 then AIsOnLink := False;
        end;
end;

procedure TfrmMain.ELDiagram1PaintLink(Sender: TObject;
    ADrawInfo: TElLinkDrawInfo; AClear: Boolean);
var
    LXOffset, LYOffset: Integer;
    LDI: TCustomLinkDrawInfo;
begin
    // Paint link using ADrawInfo draw info object. You mast not use any other
    // information in this procedure to paint link!
    // AClear parameter is determined whether the link must be erased from diagram
    // (AClear = True) or normal painting is expected (AClear = False)
    // You must use diagram HScrollPos and VScrollPos properties to offset
    // coordinates represented in the ADrawInfo object.

    LDI := ADrawInfo as TCustomLinkDrawInfo;

    //	LDI.Ca

    if AClear then
        begin
            ELDiagram1.Canvas.Pen.Color := Color;
            ELDiagram1.Canvas.Brush.Color := Color;
        end
    else
        begin
            ELDiagram1.Canvas.Pen.Color := LDI.Color;
            ELDiagram1.Canvas.Brush.Color := LDI.Color;
        end;

    LXOffset := -ELDiagram1.HScrollPos;
    LYOffset := -ELDiagram1.VScrollPos;

    ELDiagram1.Canvas.Ellipse(Rect(
        LDI.BeginP.X + LXOffset - 2,
        LDI.BeginP.Y + LYOffset - 2,
        LDI.BeginP.X + LXOffset + 2,
        LDI.BeginP.Y + LYOffset + 2));

    ELDiagram1.Canvas.Ellipse(Rect(
        LDI.EndP.X + LXOffset - 2,
        LDI.EndP.Y + LYOffset - 2,
        LDI.EndP.X + LXOffset + 2,
        LDI.EndP.Y + LYOffset + 2));

    if LDI.Selected then
        ELDiagram1.Canvas.Pen.Width := 2
    else
        ELDiagram1.Canvas.Pen.Width := 1;

    ELDiagram1.Canvas.Polyline([
        Point(
            LDI.BeginP.X + LXOffset,
            LDI.BeginP.Y + LYOffset),
            Point(
            LDI.EndP.X + LXOffset,
            LDI.EndP.Y + LYOffset)
            ]);
end;

end.

