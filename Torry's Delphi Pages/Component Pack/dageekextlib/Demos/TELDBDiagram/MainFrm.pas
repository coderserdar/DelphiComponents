unit MainFrm;

interface

uses
    Windows, Messages, SysUtils, {$IFNDEF VER130}Variants, {$ENDIF}Classes, Graphics, Controls, Forms,
    Dialogs, ExtCtrls, ELDgrm, StdCtrls;

type
    TfrmMain = class(TForm)
    ELDBDiagram1: TELDBDiagram;
        Memo1: TMemo;
        Label3: TLabel;
        CheckBox8: TCheckBox;
        CheckBox9: TCheckBox;
        Button5: TButton;
        Panel1: TPanel;
        Button1: TButton;
        Button2: TButton;
        Label1: TLabel;
        Label2: TLabel;
        procedure ELDBDiagram1Change(Sender: TObject);
        procedure ELDBDiagram1ChangeSelection(Sender: TObject);
        procedure ELDBDiagram1Click(Sender: TObject);
        procedure ELDBDiagram1DblClick(Sender: TObject);
        procedure ELDBDiagram1DeleteItem(Sender: TObject;
            AItem: TElDiagramItem);
        procedure ELDBDiagram1DeleteLink(Sender: TObject;
            ALink: TElDiagramLink);
        procedure ELDBDiagram1Enter(Sender: TObject);
        procedure ELDBDiagram1Exit(Sender: TObject);
        procedure ELDBDiagram1InsertItem(Sender: TObject;
            AItem: TElDiagramItem);
        procedure ELDBDiagram1InsertLink(Sender: TObject;
            ALink: TElDiagramLink);
        procedure ELDBDiagram1KeyDown(Sender: TObject; var Key: Word;
            Shift: TShiftState);
        procedure ELDBDiagram1KeyPress(Sender: TObject; var Key: Char);
        procedure ELDBDiagram1KeyUp(Sender: TObject; var Key: Word;
            Shift: TShiftState);
        procedure ELDBDiagram1MouseDown(Sender: TObject; Button: TMouseButton;
            Shift: TShiftState; X, Y: Integer);
        procedure ELDBDiagram1MouseMove(Sender: TObject; Shift: TShiftState; X,
            Y: Integer);
        procedure ELDBDiagram1MouseUp(Sender: TObject; Button: TMouseButton;
            Shift: TShiftState; X, Y: Integer);
        procedure Button5Click(Sender: TObject);
        procedure Panel1DragOver(Sender, Source: TObject; X, Y: Integer;
            State: TDragState; var Accept: Boolean);
        procedure FormDestroy(Sender: TObject);
        procedure Panel1DragDrop(Sender, Source: TObject; X, Y: Integer);
        procedure Button1Click(Sender: TObject);
        procedure Button2Click(Sender: TObject);
		procedure ELDBDiagram1DragOver(Sender, Source: TObject; X, Y: Integer;
            State: TDragState; var Accept: Boolean);
        procedure ELDBDiagram1DragDrop(Sender, Source: TObject; X, Y: Integer);
    private
        { Private declarations }
    public
        { Public declarations }
    end;

var
    frmMain: TfrmMain;

implementation

uses ItemPropsFrm, LinkPropsFrm;

{$R *.dfm}

procedure TfrmMain.ELDBDiagram1Change(Sender: TObject);
begin
    Memo1.Lines.Add('OnChange');
end;

procedure TfrmMain.ELDBDiagram1ChangeSelection(Sender: TObject);
begin
    Memo1.Lines.Add('OnChangeSelection');
end;

procedure TfrmMain.ELDBDiagram1Click(Sender: TObject);
begin
    Memo1.Lines.Add('OnClick');
end;

procedure TfrmMain.ELDBDiagram1DblClick(Sender: TObject);
begin
    Memo1.Lines.Add('OnDblClick');

    if ELDBDiagram1.SelectedItem <> nil then
		with TfrmItemProps.Create(Application) do
            begin
                Lines.Assign(ELDBDiagram1.SelectedItem.Lines);
                if Execute then
                    ELDBDiagram1.SelectedItem.Lines.Assign(Lines);
                Free;
            end
    else
        if ELDBDiagram1.SelectedLink <> nil then
            with TfrmLinkProps.Create(Application) do
                begin
                    BeginType := ELDBDiagram1.SelectedLink.BeginPointType;
                    EndType := ELDBDiagram1.SelectedLink.EndPointType;
                    BeginLines.Assign(ELDBDiagram1.SelectedLink.BeginItem.Lines);
                    EndLines.Assign(ELDBDiagram1.SelectedLink.EndItem.Lines);
                    BeginLineIndex := ELDBDiagram1.SelectedLink.BeginLineIndex;
                    EndLineIndex := ELDBDiagram1.SelectedLink.EndLineIndex;
                    if Execute then
                        begin
                            ELDBDiagram1.SelectedLink.BeginPointType := BeginType;
                            ELDBDiagram1.SelectedLink.EndPointType := EndType;
                            ELDBDiagram1.SelectedLink.BeginLineIndex := BeginLineIndex;
                            ELDBDiagram1.SelectedLink.EndLineIndex := EndLineIndex;
                        end;
                    Free;
                end;
end;

procedure TfrmMain.ELDBDiagram1DeleteItem(Sender: TObject;
    AItem: TElDiagramItem);
begin
    Memo1.Lines.Add(Format('OnDeleteItem: %s', [AItem.DisplayName]));
end;

procedure TfrmMain.ELDBDiagram1DeleteLink(Sender: TObject;
    ALink: TElDiagramLink);
begin
    Memo1.Lines.Add(Format('OnDeleteLink: %s', [ALink.DisplayName]));
end;

procedure TfrmMain.ELDBDiagram1Enter(Sender: TObject);
begin
    Memo1.Lines.Add('OnEnter');
end;

procedure TfrmMain.ELDBDiagram1Exit(Sender: TObject);
begin
    Memo1.Lines.Add('OnExit');
end;

procedure TfrmMain.ELDBDiagram1InsertItem(Sender: TObject;
    AItem: TElDiagramItem);
begin
    Memo1.Lines.Add(Format('OnInsertItem: %s', [AItem.DisplayName]));
end;

procedure TfrmMain.ELDBDiagram1InsertLink(Sender: TObject;
    ALink: TElDiagramLink);
begin
    Memo1.Lines.Add(Format('OnInsertLink: %s', [ALink.DisplayName]));
end;

procedure TfrmMain.ELDBDiagram1KeyDown(Sender: TObject; var Key: Word;
    Shift: TShiftState);
begin
    if CheckBox9.Checked then
        Memo1.Lines.Add(Format('OnKeyDown: (Key: $%x)', [Key]));
end;

procedure TfrmMain.ELDBDiagram1KeyUp(Sender: TObject; var Key: Word;
    Shift: TShiftState);
begin
    if CheckBox9.Checked then
        Memo1.Lines.Add(Format('OnKeyUp: (Key: %x)', [Key]));
end;

procedure TfrmMain.ELDBDiagram1MouseDown(Sender: TObject;
    Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
    LItem: TElDiagramItem;
    LLink: TElDiagramLink;
    LS, LS1: string;
begin
    if CheckBox8.Checked then
        begin
            LItem := ELDBDiagram1.Items.ItemAtPos(X, Y);
            LLink := ELDBDiagram1.Links.ItemAtPos(X, Y);
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

procedure TfrmMain.ELDBDiagram1MouseMove(Sender: TObject; Shift: TShiftState;
    X, Y: Integer);
var
    LItem: TElDiagramItem;
    LLink: TElDiagramLink;
    LS, LS1: string;
begin
    if CheckBox8.Checked then
        begin
            LItem := ELDBDiagram1.Items.ItemAtPos(X, Y);
            LLink := ELDBDiagram1.Links.ItemAtPos(X, Y);
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

procedure TfrmMain.ELDBDiagram1MouseUp(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer);
var
    LItem: TElDiagramItem;
    LLink: TElDiagramLink;
    LS, LS1: string;
begin
    if CheckBox8.Checked then
        begin
            LItem := ELDBDiagram1.Items.ItemAtPos(X, Y);
            LLink := ELDBDiagram1.Links.ItemAtPos(X, Y);
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
    if (Source is TElDBDiagram) and (TElDBDiagram(Source).SelectedItem <> nil) then
        LItem := TElDBDiagram(Source).SelectedItem.DisplayName
    else
        LItem := 'nil';

    Memo1.Lines.Add(Format('Panel1.OnDragOver: %s (Item: %s) -> %s (X: %d, Y: %d)',
        [LSource, LItem, Panel1.Name, X, Y]));
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
    ELDBDiagram1.Free;
end;

procedure TfrmMain.Panel1DragDrop(Sender, Source: TObject; X, Y: Integer);
var
    LSource, LItem: string;
begin
    if Source <> nil then
        LSource := (Source as TComponent).Name
    else
        LSource := 'nil';
    if (Source is TElDBDiagram) and (TElDBDiagram(Source).SelectedItem <> nil) then
        LItem := TElDBDiagram(Source).SelectedItem.DisplayName
    else
        LItem := 'nil';

    Memo1.Lines.Add(Format('Panel1.OnDragDrop: %s (Item: %s) -> %s (X: %d, Y: %d)',
        [LSource, LItem, Panel1.Name, X, Y]));
end;

procedure TfrmMain.Button1Click(Sender: TObject);
begin
    ELDBDiagram1.Items.Add;
end;

procedure TfrmMain.Button2Click(Sender: TObject);
begin
    if ELDBDiagram1.SelectedItem <> nil then
        ELDBDiagram1.SelectedItem.Free;
    if ELDBDiagram1.SelectedLink <> nil then
        ELDBDiagram1.SelectedLink.Free;
end;

procedure TfrmMain.ELDBDiagram1DragOver(Sender, Source: TObject; X,
    Y: Integer; State: TDragState; var Accept: Boolean);
begin
    if (Source = ELDBDiagram1) and
        (ELDBDiagram1.SelectedItem <> nil) and
        (ELDBDiagram1.SelectedItemLine <> -1) and
        (ELDBDiagram1.Items.ItemAtPos(X, Y) <> nil) and
        (ELDBDiagram1.Items.ItemAtPos(X, Y).LineAtPos(Point(X, Y), True) <> -1) and
        (ELDBDiagram1.SelectedItem <> ELDBDiagram1.Items.ItemAtPos(X, Y)) then
        Accept := True
    else
        Accept := False;
end;

procedure TfrmMain.ELDBDiagram1DragDrop(Sender, Source: TObject; X,
    Y: Integer);
begin
    if (Source = ELDBDiagram1) and
        (ELDBDiagram1.SelectedItem <> nil) and
        (ELDBDiagram1.SelectedItemLine <> -1) and
        (ELDBDiagram1.Items.ItemAtPos(X, Y) <> nil) and
        (ELDBDiagram1.Items.ItemAtPos(X, Y).LineAtPos(Point(X, Y), True) <> -1) and
        (ELDBDiagram1.SelectedItem <> ELDBDiagram1.Items.ItemAtPos(X, Y)) then
        with TElDBDiagramLink(ELDBDiagram1.Links.Add) do
			begin
                BeginItemName := ELDBDiagram1.SelectedItem.Name;
                EndItemName := ELDBDiagram1.Items.ItemAtPos(X, Y).Name;
                BeginLineIndex := ELDBDiagram1.SelectedItemLine;
				EndLineIndex := ELDBDiagram1.Items.ItemAtPos(X, Y).LineAtPos(Point(X, Y), True);
                BeginPointType := lptOne;
                EndPointType := lptInfinity;
            end;
end;

procedure TfrmMain.ELDBDiagram1KeyPress(Sender: TObject; var Key: Char);
begin
    if CheckBox9.Checked then
        Memo1.Lines.Add(Format('OnKeyPress: (Key: "%s")', [Key]));
end;

end.

