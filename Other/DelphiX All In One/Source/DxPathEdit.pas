unit DXPathEdit;
//(c)2007 Jaro Benes
//All Rights Reserved

{
Complex application for users of unDelphiX as component editor:

Supported:
 a) create path for default shape.
 b) allow do change like move or rotate path layout.
 c) create new trace by free-hand.

}
interface
                             
{$INCLUDE DelphiXcfg.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Menus, Buttons, Math, ComCtrls,         
  DXClass, DXDraws, DIB;

type
  {  TEdit  }
  TEdit = class(StdCtrls.TEdit) {injected class}
  private
    function GetAsInteger: Integer;
    procedure SetAsInteger(const Value: Integer);
  published
  public
    property AsInteger: Integer read GetAsInteger write SetAsInteger;
  end;
  {  TShape  }
  TShape = class(ExtCtrls.TShape)
    procedure CMMouseEnter(var Msg: TMessage); message CM_MouseEnter;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MouseLeave;
  end;
  {  TDelphiXTracesEditForm  }
  TDPoint = record
    X, Y: Double;
    StayOn: Double;
  end;
  TDPointArr = array{$IFNDEF VER4UP} [0..0]{$ENDIF} of TDPoint;
{$IFNDEF VER4UP}
  PDPointArr = ^TDPointArr;
{$ENDIF}
  TDelphiXPathsEditForm = class(TForm)
    ScrollBox1: TScrollBox;
    Pane: TPanel;
    Shape1: TShape;
    Panel2: TPanel;
    Panel1: TPanel;
    Label1: TLabel;
    LAmount: TLabel;
    cbListOfTraces: TComboBox;
    eAmount: TEdit;
    btnNewTrace: TButton;
    PopupMenu1: TPopupMenu;
    Activate1: TMenuItem;
    Label2: TLabel;
    eShowOn: TEdit;
    Panel12: TPanel;
    btnSetTimming: TSpeedButton;
    btnLine: TSpeedButton;
    btnCircle: TSpeedButton;
    btnSelectionArea: TSpeedButton;
    btnSelectAll: TSpeedButton;
    btnGrid: TSpeedButton;
    brnSelectAsOne: TSpeedButton;
    btnBringToFront: TSpeedButton;
    btnMoveDown: TSpeedButton;
    btnSendToBack: TSpeedButton;
    btnMoveUp: TSpeedButton;
    btnMoveLeft: TSpeedButton;
    btnMoveRight: TSpeedButton;
    Panel3: TPanel;
    OKButton: TButton;
    CancelButton: TButton;
    btnCurve: TSpeedButton;
    btnProperties: TSpeedButton;
    btnRect: TSpeedButton;
    Image1: TImage;
    btnRefresh: TBitBtn;
    Label3: TLabel;
    StatusBar1: TStatusBar;
    Button1: TBitBtn;
    eDist: TEdit;
    LDist: TLabel;
    btnRotateLeft: TSpeedButton;
    btnRotateRight: TSpeedButton;
    procedure btnRotateLeftClick(Sender: TObject);
    procedure btnRotateRightClick(Sender: TObject);
    procedure btnMoveRightClick(Sender: TObject);
    procedure btnMoveLeftClick(Sender: TObject);
    procedure btnMoveDownClick(Sender: TObject);
    procedure btnMoveUpClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure btnLineClick(Sender: TObject);
    procedure btnGridClick(Sender: TObject);
    procedure btnSelectionAreaClick(Sender: TObject);
    procedure btnRefreshClick(Sender: TObject);
    procedure PaneResize(Sender: TObject);
    procedure Image1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Image1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure Image1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure OKButtonClick(Sender: TObject);
    procedure cbListOfTracesChange(Sender: TObject);
    procedure rgShapeClick(Sender: TObject);
    procedure btnNewTraceClick(Sender: TObject);
    procedure ShapeMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure ShapeMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ShapeMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnSendToBackClick(Sender: TObject);
    procedure btnBringToFrontClick(Sender: TObject);
    procedure btnSetTimmingClick(Sender: TObject);
  private
    { Private declarations }
    FCapture, FClicked: Boolean;
    MouseDownSpot: TPoint;
    LastShape: TShape;
    FTracesList: TTraces;
    tmpRect: TRect;
{$IFNDEF VER4UP}
    tmpPointArrSize: Integer;
{$ENDIF}
    tmpPointArr: {$IFNDEF VER4UP}PDPointArr{$ELSE}TDPointArr{$ENDIF};
    X0, Y0, LX, LY: Integer;
    IsDownNow: Boolean;
    procedure btnCreateNewTrace(Sender: TObject);
    procedure DoMakePoints;
    procedure CreatePathFromActiveTrace(index: Integer);
    function GetSizesOfTrace(out x, y, oWidth, oHeight: Integer): Boolean;
    procedure RotatePathForAngle(Angle: Integer);
    
  public
    { Public declarations }
    property PrivateTraces: TTraces read FTracesList write FTracesList;
    procedure ShowTracesOnPane_;
    procedure RewriteTracesFromPane;
    procedure ShowTracesOnPane;
    procedure RefreshShowTracesOnPaneOnly;
  end;

var
  DelphiXPathsEditForm: TDelphiXPathsEditForm;

implementation

{$R *.dfm}

{ TEdit }

procedure TEdit.SetAsInteger(const Value: Integer);
begin
  Self.Text := IntToStr(Value)
end;

function TEdit.GetAsInteger: Integer;
begin
  try
    Result := StrToInt(Self.Text);
  except
    Result := 0;
  end;
end;

{ TShape }

procedure TShape.CMMouseLeave(var Msg: TMessage);
begin
  Shape := stRectangle;
end;

procedure TShape.CMMouseEnter(var Msg: TMessage);
begin
  Shape := stCircle;
end;

{  TDelphiXTracesEditForm  }

procedure TDelphiXPathsEditForm.FormCreate(Sender: TObject);
begin
  FTracesList := TTraces.Create(Self);
{$IFNDEF VER4UP}
  tmpPointArrSize := 0;
  tmpPointArr := nil;
{$ENDIF}
  Image1.Picture.Bitmap.Width := Pane.Width;
  Image1.Picture.Bitmap.Height := Pane.Height;
  btnGrid.Click;
end;

procedure TDelphiXPathsEditForm.FormDestroy(Sender: TObject);
begin
{$IFNDEF VER4UP}
  if tmpPointArrSize > 0 then
    System.ReallocMem(tmpPointArr, 0);
{$ENDIF}
  FTracesList.Free;
  FTracesList := nil;
end;

procedure SetActiveColor(Active: Boolean; S: TShape);
begin
  if Active then S.Pen.Color := clRed
  else S.Pen.Color := $008080FF;
  if Active then
    if Active then S.Brush.Color := clYellow
    else S.Brush.Color := $0095FFFF
  else
    if Active then S.Brush.Color := clGray
    else S.Brush.Color := $00C4C4C4;
end;

procedure TDelphiXPathsEditForm.ShowTracesOnPane_;
var
  I, J: Integer;
  S: TShape;
  B: Boolean;
begin
  Screen.Cursor := crHourGlass;
  {uvolni predchozi}
  for I := ComponentCount - 1 downto 0 do
    if Components[I] is TShape then with Components[I] as TShape do begin
        if Parent = Pane then
          Free;
      end;
  {projdi seznam}
  for I := 0 to FTracesList.Count - 1 do begin
    {slozky-udelej pomocne pole}
    CreatePathFromActiveTrace(I);
    B := cbListOfTraces.ItemIndex = I; {aktivni radek}
    {vlastni stopy}
{$IFNDEF VER4UP}
    for J := 0 to tmpPointArrSize - 1 do
{$ELSE}
    for J := Low(tmpPointArr) to High(tmpPointArr) do
{$ENDIF}
    begin
      S := TShape.Create(Self);
      //----------
      S.Parent := Pane;
      S.Width := 16;
      S.Height := 16;
      SetActiveColor(B, S);
      //----------
      S.Left := Round(tmpPointArr[J].X) - 8; {na stred}
      S.Top := Round(tmpPointArr[J].Y) - 8; {na stred}
      S.ShowHint := True;
      S.Hint := FTracesList.Items[I].Name;
      if Trim(S.Hint) = '' then S.Hint := Format('(unnamed[%d])', [I]);
      S.ShowHint := True;
      //Upravovat ale lze pouze jen tu cestu, ktera je aktivni v combobox
      if cbListOfTraces.ItemIndex = I then begin
        S.OnMouseDown := ShapeMouseDown;
        S.OnMouseMove := ShapeMouseMove;
        S.OnMouseUp := ShapeMouseUp;
      end;
      S.Tag := Integer(J);

    end;
  end;
  btnGrid.Click;
  Screen.Cursor := crDefault;
end;

procedure TDelphiXPathsEditForm.ShowTracesOnPane;
var
  I, J, index: Integer;
  S: TShape;
  P: TPath;
begin
  Screen.Cursor := crHourGlass;
  {uvolni predchozi}
  for I := ComponentCount - 1 downto 0 do
    if Components[I] is TShape then with Components[I] as TShape do begin
        if Parent = Pane then
          Free;
      end;
  {projdi seznam}
  for I := 0 to FTracesList.Count - 1 do begin
    {slozky-udelej pomocne pole}
    index := i;
    if index = -1 then Exit;
    {vlastni stopy}
    with FTracesList.Items[index].Blit do
      if GetPathCount > 0 then begin
        for J := 0 to GetPathCount - 1 do
        begin
          S := TShape.Create(Self);
          //----------
          S.Parent := Pane;
          S.Width := 16;
          S.Height := 16;
          SetActiveColor(cbListOfTraces.ItemIndex = I, S);
          //----------
          S.Left := Round(Path[J].X) - 8; {na stred}
          S.Top := Round(Path[J].Y) - 8; {na stred}
          S.ShowHint := True;
          S.Hint := FTracesList.Items[I].Name;
          if Trim(S.Hint) = '' then S.Hint := Format('(unnamed[%d])', [I]);
          S.ShowHint := True;
          //Upravovat ale lze pouze jen tu cestu, ktera je aktivni v combobox
          if cbListOfTraces.ItemIndex = I then begin
            S.OnMouseDown := ShapeMouseDown;
            S.OnMouseMove := ShapeMouseMove;
            S.OnMouseUp := ShapeMouseUp;
          end;
          S.Tag := Integer(J);
          P := Path[J];
          P.Tag := Integer(S);
          Path[J] := P;
        end;
      end;
  end;
  btnGrid.Click;
  Screen.Cursor := crDefault;
end;

procedure TDelphiXPathsEditForm.RefreshShowTracesOnPaneOnly;
var
  I, J, index: Integer;
  S: TShape;
//  P: TPath;
begin
  Screen.Cursor := crHourGlass;
  {projdi seznam}
  for I := 0 to FTracesList.Count - 1 do begin
    {slozky-udelej pomocne pole}
    index := i;
    if index = -1 then Exit;
    {vlastni stopy}
    with FTracesList.Items[index].Blit do
      if GetPathCount > 0 then begin
        for J := 0 to GetPathCount - 1 do
        begin
          S := TShape(Path[J].Tag);
          if Assigned(S) then begin
            S.Left := Round(Path[J].X) - 8;
            S.Top := Round(Path[J].Y) - 8;
            SetActiveColor(cbListOfTraces.ItemIndex = I, S);
            //----------
            //Upravovat ale lze pouze jen tu cestu, ktera je aktivni v combobox
            if cbListOfTraces.ItemIndex = I then begin
              S.OnMouseDown := ShapeMouseDown;
              S.OnMouseMove := ShapeMouseMove;
              S.OnMouseUp := ShapeMouseUp;
            end
            else
            begin
              S.OnMouseDown := nil;
              S.OnMouseMove := nil;
              S.OnMouseUp := nil;
            end;
          end;
        end;
      end;
  end;
  btnGrid.Click;
  Screen.Cursor := crDefault;
end;

procedure TDelphiXPathsEditForm.ShapeMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  M: TPoint;
begin
  if FCapture and (ssLeft in Shift) then begin
    TShape(Sender).Left := TShape(Sender).Left - (MouseDownSpot.x - x);
    TShape(Sender).Top := TShape(Sender).Top - (MouseDownSpot.y - y);
  end;
  //pro zmenu velikosti
  if FClicked and (ssRight in Shift) and Assigned(LastShape) then begin
    M := Pane.ScreenToClient({$IFNDEF VER4UP}Point(X, Y){$ELSE}Mouse.CursorPos{$ENDIF});
    LastShape.Width := M.X - LastShape.Left;
    LastShape.Height := M.Y - LastShape.Top;
  end;
end;

procedure TDelphiXPathsEditForm.ShapeMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if FCapture then
  begin
    ReleaseCapture;
    FCapture := False;
    TShape(Sender).Left := TShape(Sender).Left - (MouseDownSpot.x - x);
    TShape(Sender).Top := TShape(Sender).Top - (MouseDownSpot.y - y);
  end;
  LastShape := nil;
  FClicked := False;
  RewriteTracesFromPane;
  ShowTracesOnPane;
  Cursor := crDefault;
end;

procedure TDelphiXPathsEditForm.ShapeMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
begin
  FCapture := ssLeft in Shift;
  MouseDownSpot.X := X;
  MouseDownSpot.Y := Y;
  FClicked := ssRight in Shift;
  if FClicked and (Sender is TShape) then begin
    P := TShape(Sender).ClientToScreen(Point(X, Y));
    PopupMenu1.Popup(P.X, P.Y);
    Exit;
  end;
  ShapeMouseMove(Sender, Shift, X, Y);
  if (Sender is TShape) then
    LastShape := TShape(Sender);
  Cursor := {$IFNDEF VER4UP}crSIZE{$ELSE}crSizeAll{$ENDIF};
end;

procedure TDelphiXPathsEditForm.RewriteTracesFromPane;
var
  I: Integer;
  S: TShape;
  //TT: TTracePoint;
  T: TPath;
begin
  for I := ComponentCount - 1 downto 0 do
    if Components[I] is TShape then begin
      S := Components[I] as TShape;
      if S.Parent = Pane then
        if S.Hint = cbListOfTraces.Text then //active item only
        begin
          T := PrivateTraces.Items[cbListOfTraces.ItemIndex].Blit.Path[S.Tag];
          T.X := S.Left + 8;
          T.Y := S.Top + 8;
          {tady lze prepsat jine atributy treba Rychlost...}
          PrivateTraces.Items[cbListOfTraces.ItemIndex].Blit.Path[S.Tag] := T;
        end;
    end;
end;

procedure TDelphiXPathsEditForm.btnBringToFrontClick(Sender: TObject);
var
  T: TTrace;
begin
  if cbListOfTraces.ItemIndex <> -1 then begin
    T := FTracesList.Add;
    T.Assign(FTracesList.Items[cbListOfTraces.ItemIndex]);
    {$IFDEF VER5UP}
    FTracesList.Delete(cbListOfTraces.ItemIndex);
    {$ELSE}
    FTracesList.Items[cbListOfTraces.ItemIndex].Free;
    {$ENDIF}
    cbListOfTraces.Items.Move(cbListOfTraces.ItemIndex, cbListOfTraces.Items.Count - 1);
    cbListOfTraces.ItemIndex := cbListOfTraces.Items.Count - 1;
    ShowTracesOnPane
  end;
end;

procedure TDelphiXPathsEditForm.btnCreateNewTrace(Sender: TObject);
var
  S: string;
  T: TTrace;
begin
  if InputQuery('Name of new Trace:', 'Trace name', S) then begin
    if Trim(S) = '' then begin
      ShowMessage('Name for new trace mustn''t be empty.');
      Exit;
    end;
    if cbListOfTraces.Items.IndexOf(S) <> -1 then begin
      ShowMessage('Name for new trace has to be unique.');
      Exit;
    end;
    T := FTracesList.Add;
    T.Name := S;
    cbListOfTraces.Items.AddObject(S, Pointer(PrivateTraces.Count - 1));
    cbListOfTraces.ItemIndex := cbListOfTraces.Items.IndexOf(S);
    cbListOfTracesChange(cbListOfTraces);
  end;
end;

procedure TDelphiXPathsEditForm.btnNewTraceClick(Sender: TObject);
begin
  btnCreateNewTrace(Sender);
end;

procedure TDelphiXPathsEditForm.rgShapeClick(Sender: TObject);
begin
  btnNewTrace.Enabled := btnLine.Down or btnCircle.Down or btnCurve.Down;
end;

procedure TDelphiXPathsEditForm.cbListOfTracesChange(Sender: TObject);
begin
  RewriteTracesFromPane;
  RefreshShowTracesOnPaneOnly
end;

procedure TDelphiXPathsEditForm.OKButtonClick(Sender: TObject);
begin
  RewriteTracesFromPane;
  Tag := 1;
end;

procedure TDelphiXPathsEditForm.Image1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  {zapnou se funkce pro sber}
begin
  if ssleft in Shift then begin
    X0 := X; LX := X;
    Y0 := Y; LY := Y;
    Image1.Picture.Bitmap.Canvas.Pen.Mode := pmNotXor;
    Image1.Picture.Bitmap.Canvas.Pen.Color := clRed;
    Image1.Picture.Bitmap.Canvas.Brush.Style := bsClear;
    IsDownNow := True;
    if btnCurve.Down then begin
{$IFNDEF VER4UP}
      tmpPointArrSize := 1;
      System.ReallocMem(tmpPointArr, tmpPointArrSize * SizeOf(TDPoint));
      tmpPointArr[tmpPointArrSize - 1].X := X;
      tmpPointArr[tmpPointArrSize - 1].Y := Y;
{$ELSE}
      SetLength(tmpPointArr, 1);
      tmpPointArr[High(tmpPointArr)].X := X;
      tmpPointArr[High(tmpPointArr)].Y := Y;
{$ENDIF}
    end;
  end;
end;

procedure TDelphiXPathsEditForm.Image1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
  {zabira ze plocha}
begin
  if IsDownNow then with Image1.Picture.Bitmap.Canvas do begin
      if btnSelectionArea.Down then begin
        Rectangle(X0, Y0, LX, LY);
        Rectangle(X0, Y0, X, Y);
      end;
      if btnLine.Down then begin
        MoveTo(x0, y0);
        LineTo(lx, ly);
        MoveTo(x0, y0);
        LineTo(x, y);
      end;
      if btnCircle.Down or btnRect.Down then begin
        Rectangle(X0, Y0, LX, LY);
        Rectangle(X0, Y0, X, Y);
      end;
      if btnCurve.Down then begin
        if (X <> LX) or (Y <> LY) then begin
{$IFNDEF VER4UP}
          Inc(tmpPointArrSize);
          System.ReallocMem(tmpPointArr, tmpPointArrSize * SizeOf(TDPoint));
          tmpPointArr[tmpPointArrSize - 1].X := X;
          tmpPointArr[tmpPointArrSize - 1].Y := Y;
{$ELSE}
          SetLength(tmpPointArr, Length(tmpPointArr) + 1);
          tmpPointArr[High(tmpPointArr)].X := X;
          tmpPointArr[High(tmpPointArr)].Y := Y;
{$ENDIF}

          MoveTo(LX, LY);
          LineTo(x, y);
        end;
      end;
      LX := X;
      LY := Y;
    end;
  StatusBar1.Panels[1].Text := Format('(x,y)=(%d,%d)', [X, Y]);
end;

{$IFNDEF VER4UP}
function Min(i1, i2: integer): integer;
begin
  if i1 < i2 then Result := i1 else Result := i2;
end;

function Max(i1, i2: integer): integer;
begin
  if i1 > i2 then Result := i1 else Result := i2;
end;
{$ENDIF}

procedure TDelphiXPathsEditForm.Image1MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
{koncovy bod}
var
  i, v, a, b, c: Integer;
  beta, sinbeta, cosbeta, angle, step, ii, vx, vy, alpha, sinalpha, cosalpha, p, vv, a1, b1: Double;
begin
  if IsDownNow then with Image1.Picture.Bitmap.Canvas do begin
      if btnCurve.Down then begin
        DoMakePoints;
        Label3.Caption := '';
        Image1.OnMouseDown := nil;
        Image1.OnMouseMove := nil;
        Image1.OnMouseUp := nil;
        btnCurve.Down := False;
      end;
      if btnSelectionArea.Down then begin
        if ssShift in Shift then begin
          Rectangle(X0, Y0, LX, LY); //smazat
          Pen.Mode := pmCopy; //napevno
          v := Max(Abs(X0 - x), Abs(X0 - y));
          Rectangle(X0, y0, X0 + v, Y0 + v); //vykreslit
        end
        else begin
          Pen.Mode := pmCopy; //napevno
          Rectangle(x0, y0, x, y);
        end;
        tmpRect := Rect(x0, y0, x, y);
        Label3.Caption := Format('R:((%d,%d),(%d,%d))', [x0, y0, x, y]);
        Image1.OnMouseDown := nil;
        Image1.OnMouseMove := nil;
        Image1.OnMouseUp := nil;
        btnSelectionArea.Down := False;
      end;
      if btnLine.Down then begin
        MoveTo(x0, y0);
        LineTo(x, y);
{$IFNDEF VER4UP}
        tmpPointArrSize := 2;
        System.ReallocMem(tmpPointArr, tmpPointArrSize * SizeOf(TDPoint));
{$ELSE}
        SetLength(tmpPointArr, 2);
{$ENDIF}
        C := 0;
        tmpPointArr[C].X := X0;
        tmpPointArr[C].Y := Y0;
        Inc(C);
        tmpPointArr[C].X := X;
        tmpPointArr[C].Y := Y;
        DoMakePoints;
        Label3.Caption := '';
        Image1.OnMouseDown := nil;
        Image1.OnMouseMove := nil;
        Image1.OnMouseUp := nil;
        btnLine.Down := False;
      end;
      if btnCircle.Down then begin
        Rectangle(X0, Y0, LX, LY); //smazat
{$IFNDEF VER4UP}
        tmpPointArrSize := eAmount.AsInteger;
        System.ReallocMem(tmpPointArr, tmpPointArrSize * SizeOf(TDPoint));
{$ELSE}
        SetLength(tmpPointArr, eAmount.AsInteger);
{$ENDIF}
        {neni pootocena}
        angle := 0;
        beta := -angle / 180 * PI;

        sinbeta := Sin(beta);
        cosbeta := Cos(beta);
        step := 360 / eAmount.AsInteger;
        ii := 0; v := {$IFNDEF VER4UP}0{$ELSE}Low(tmpPointArr){$ENDIF};
        a := Abs(LX - X0) div 2; //mayor
        b := Abs(LY - Y0) div 2; //minor
        vx := X0 + a; //center x
        vy := Y0 + b; //center y
        while ii < 360 do begin
          alpha := ii / 180 * PI;
          sinalpha := Sin(alpha);
          cosalpha := Cos(alpha);
          tmpPointArr[v].X := vx + (a * cosalpha * cosbeta - b * sinalpha * sinbeta);
          tmpPointArr[v].Y := vy + (a * cosalpha * sinbeta + b * sinalpha * cosbeta);
          inc(v);
          ii := ii + step;
        end;
        DoMakePoints;
        Label3.Caption := '';
        Image1.OnMouseDown := nil;
        Image1.OnMouseMove := nil;
        Image1.OnMouseUp := nil;
        btnCircle.Down := False;
      end;
      if btnRect.Down then begin
        Rectangle(X0, Y0, LX, LY); //smazat
{$IFNDEF VER4UP}
        tmpPointArrSize := eAmount.AsInteger;
        System.ReallocMem(tmpPointArr, tmpPointArrSize * SizeOf(TDPoint));
{$ELSE}
        SetLength(tmpPointArr, eAmount.AsInteger);
{$ENDIF}
        a1 := LX - X0;
        b1 := LY - Y0;
        //c := 2 * (LX - X0) + 2 * (LY - Y0); //delka
        ii := (2 * a1 + 2 * b1) / eAmount.AsInteger; //delka useku
        //first point is here
        vv := 0;
        tmpPointArr[0].X := X0; p := X0;
        tmpPointArr[0].Y := Y0;
        {rozhodit body po obdelniku}
        for I := 1 to eAmount.AsInteger - 1 do begin
          p := p + ii;
          vv := vv + ii;
          if vv < a1 then begin
            tmpPointArr[I].X := p;
            tmpPointArr[I].Y := Y0;
          end
          else
            if vv < (a1 + b1) then begin
              tmpPointArr[I].X := LX;
              tmpPointArr[I].Y := Y0 + (vv - a1);
            end
            else
              if vv < (2 * a1 + b1) then begin
                tmpPointArr[I].X := LX - (vv - (a1 + b1));
                tmpPointArr[I].Y := LY;
              end
              else
                if vv < (2 * a1 + 2 * b1) then begin
                  tmpPointArr[I].X := X0;
                  tmpPointArr[I].Y := LY - (vv - (2 * a1 + b1));
                end;
        end;
        DoMakePoints;
        Label3.Caption := '';
        Image1.OnMouseDown := nil;
        Image1.OnMouseMove := nil;
        Image1.OnMouseUp := nil;
        btnRect.Down := False;
      end;
    end;
  IsDownNow := False;
end;

procedure TDelphiXPathsEditForm.PaneResize(Sender: TObject);
begin
  Image1.Picture.Bitmap.Width := Pane.Width;
  Image1.Picture.Bitmap.Height := Pane.Height;
end;

procedure TDelphiXPathsEditForm.DoMakePoints;
  function distance2d(x1, z1, x2, z2: single): single;
  var
    diffx, diffz: single;
  begin
    diffX := x1 - x2;
    diffZ := z1 - z2;
    result := system.Sqrt(diffX * diffX + diffZ * diffZ);
  end;
var
  T: TTrace;
  Q: TPath;
  I, D, C: Integer;
  DX, DY, TX, TY: Single;
begin
  if btnLine.Down then begin
    C := 0;
    if {$IFNDEF VER4UP}tmpPointArrSize{$ELSE}Length(tmpPointArr){$ENDIF} = 2 then begin
      D := Round(distance2d(tmpPointArr[C].X, tmpPointArr[C].Y, tmpPointArr[C + 1].X, tmpPointArr[C + 1].Y));
      if cbListOfTraces.ItemIndex <> -1 then begin
        {ziskej aktivni stopu}
        T := PrivateTraces.Items[cbListOfTraces.ItemIndex];
        T.Blit.SetPathLen(0); //smaz
        {vytvoreni slozek}
        {korekce, je-li bodu vic nez delka cary}
        if eAmount.AsInteger > D then
          eAmount.AsInteger := D;
        {nastaveni velikosti cesty}
        T.Blit.SetPathLen(eAmount.AsInteger);
        {rozhozeni bodu na caru}
        DX := (tmpPointArr[C + 1].X - tmpPointArr[C].X) / eAmount.AsInteger;
        DY := (tmpPointArr[C + 1].Y - tmpPointArr[C].Y) / eAmount.AsInteger;
        TX := tmpPointArr[C].X;
        TY := tmpPointArr[C].Y;
        for I := 1 to eAmount.AsInteger do begin
          FillChar(Q, SizeOf(Q), 0);
          Q.X := Round(TX + (I - 1) * DX);
          Q.Y := Round(TY + (I - 1) * DY);
          Q.StayOn := eShowOn.AsInteger;
          T.Blit.Path[I - 1] := Q;
        end;
        T.Active := True;
        ShowTracesOnPane;
      end;
    end;
  end;
  if btnCircle.Down or btnRect.Down or btnCurve.Down then begin
    if cbListOfTraces.ItemIndex <> -1 then begin
      {ziskej aktivni stopu}
      T := PrivateTraces.Items[cbListOfTraces.ItemIndex];
      T.Blit.SetPathLen(0); //smaz
      {vytvoreni slozek}
{$IFNDEF VER4UP}
      T.Blit.SetPathLen(tmpPointArrSize);
      for I := 0 to tmpPointArrSize - 1 do
{$ELSE}
      T.Blit.SetPathLen(Length(tmpPointArr));
      for I := Low(tmpPointArr) to High(tmpPointArr) do
{$ENDIF}
      begin
        FillChar(Q, SizeOf(Q), 0);
        Q.X := Round(tmpPointArr[I].X);
        Q.Y := Round(tmpPointArr[I].Y);
        Q.StayOn := eShowOn.AsInteger;
        T.Blit.Path[I] := Q;
      end;
      T.Active := True;
      ShowTracesOnPane;
    end;
  end;
end;

procedure TDelphiXPathsEditForm.btnRefreshClick(Sender: TObject);
begin
  DoMakePoints;
end;

procedure TDelphiXPathsEditForm.btnSelectionAreaClick(Sender: TObject);
begin
  Image1.OnMouseDown := Image1MouseDown;
  Image1.OnMouseMove := Image1MouseMove;
  Image1.OnMouseUp := Image1MouseUp;
end;

procedure TDelphiXPathsEditForm.btnSendToBackClick(Sender: TObject);
var
  T: TTrace;
  I: Integer;
begin
  if cbListOfTraces.ItemIndex <> -1 then begin
    T := FTracesList.Items[cbListOfTraces.ItemIndex];   //saved
    //from selected to first
    for I := cbListOfTraces.ItemIndex-1 downto 0 do begin
      FTracesList.Items[I] := FTracesList.Items[I + 1];
    end;
    FTracesList.Items[0] := T;
    cbListOfTraces.Items.Move(cbListOfTraces.ItemIndex, 0);
    cbListOfTraces.ItemIndex := 0; {it is first now}
    ShowTracesOnPane
  end;
end;

procedure TDelphiXPathsEditForm.btnSetTimmingClick(Sender: TObject);
var
  T: TTrace;
  I: Integer;
  P: TPath;
begin
  if MessageDlg(Format('Do you want change show time to %d ms for each point ?', [eShowOn.AsInteger]), mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    {move selected path to down}
    if cbListOfTraces.ItemIndex <> -1 then begin
      {ziskej aktivni stopu}
      T := PrivateTraces.Items[cbListOfTraces.ItemIndex];
      for I := 0 to T.Blit.GetPathCount - 1 do
      begin
        P := T.Blit.Path[I];
        P.StayOn := eShowOn.AsInteger;
        T.Blit.Path[I] := P;
      end;
    end;
    ShowTracesOnPane;
  end;
end;

procedure TDelphiXPathsEditForm.btnGridClick(Sender: TObject);
const
  ccGrid = 32;
  ccShift = 16;
var I: Integer;
{$IFNDEF VER4UP}
//  pp: Pointer;
{$ELSE}
  pp: array of TPoint;
{$ENDIF}
begin
  if btnGrid.Down then
    with Image1.Picture.Bitmap.Canvas do begin
      Brush.Color := clBlack;
      FillRect(Bounds(0, 0, Image1.Picture.Bitmap.Width, Image1.Picture.Bitmap.Width));
      Pen.Color := clDkGray;
      Pen.Style := psDot;
      Pen.Mode := pmCopy;
      Pen.Width := 1;
      for I := 0 to Image1.Picture.Bitmap.Width div ccGrid do begin
        MoveTo(I * ccGrid + ccShift, 0);
        LineTo(I * ccGrid + ccShift, Image1.Picture.Bitmap.Height);
      end;
      for I := 0 to Image1.Picture.Bitmap.Width div ccGrid do begin
        MoveTo(0, I * ccGrid + ccShift);
        LineTo(Image1.Picture.Bitmap.Width, I * ccGrid + ccShift);
      end;
      Pen.Color := clLtGray;
      Pen.Style := psSolid;
      Pen.Width := 1;
      for I := 0 to Image1.Picture.Bitmap.Width div 32 do begin
        MoveTo(I * 32, 0);
        LineTo(I * 32, Image1.Picture.Bitmap.Height);
      end;
      for I := 0 to Image1.Picture.Bitmap.Width div 32 do begin
        MoveTo(0, I * 32);
        LineTo(Image1.Picture.Bitmap.Width, I * 32);
      end;
    end
  else
    with Image1.Picture.Bitmap.Canvas do begin
      Brush.Color := clBlack;
      FillRect(Bounds(0, 0, Image1.Picture.Bitmap.Width, Image1.Picture.Bitmap.Width));
    end;
  if (tmpRect.Right > 0) and (tmpRect.Bottom > 0) then
    with Image1.Picture.Bitmap.Canvas do begin
      Pen.Color := clGreen;
      Pen.Width := 1;
      Pen.Mode := pmCopy;
      Brush.Style := bsClear;
{$IFDEF VER5UP}
      Rectangle(tmpRect);
{$ELSE}
      Rectangle(tmpRect.Left, tmpRect.Top, tmpRect.Right, tmpRect.Bottom);
{$ENDIF}
    end;
  with Image1.Picture.Bitmap.Canvas do begin
    CreatePathFromActiveTrace(cbListOfTraces.ItemIndex);
{$IFNDEF VER4UP}
    if tmpPointArrSize <= 0 then Exit;
    MoveTo(Round(tmpPointArr[0].X), Round(tmpPointArr[0].Y));
    for I := 1 to tmpPointArrSize - 1 do
      LineTo(Round(tmpPointArr[I].X), Round(tmpPointArr[I].Y));
{$ELSE}
    if Length(tmpPointArr) = 0 then Exit;
    SetLength(pp, Length(tmpPointArr));
    for I := Low(tmpPointArr) to High(tmpPointArr) do
      pp[I] := Point(Round(tmpPointArr[I].X), Round(tmpPointArr[I].Y));
    Pen.Color := clWhite;
    Pen.Width := 1;
    Pen.Mode := pmCopy;
    Brush.Style := bsClear;
    Polyline(pp);
{$ENDIF}
  end;
end;

procedure TDelphiXPathsEditForm.btnLineClick(Sender: TObject);
begin
  Image1.OnMouseDown := Image1MouseDown;
  Image1.OnMouseMove := Image1MouseMove;
  Image1.OnMouseUp := Image1MouseUp;
end;

procedure TDelphiXPathsEditForm.Button1Click(Sender: TObject);
begin
  tmpRect := Rect(0, 0, 0, 0);
  Label3.Caption := 'R:<none>';
end;

procedure TDelphiXPathsEditForm.CreatePathFromActiveTrace(index: Integer);
var
  J: Integer;
begin
{$IFNDEF VER4UP}
  tmpPointArrSize := 0;
  System.ReallocMem(tmpPointArr, tmpPointArrSize * SizeOf(TDPoint));
{$ELSE}
  SetLength(tmpPointArr, 0);
{$ENDIF}
  if index = -1 then Exit;
  {vlastni stopy}
  with FTracesList.Items[index].Blit do
    if GetPathCount > 0 then begin
{$IFNDEF VER4UP}
      tmpPointArrSize := GetPathCount;
      System.ReallocMem(tmpPointArr, tmpPointArrSize * SizeOf(TDPoint));
{$ELSE}
      SetLength(tmpPointArr, GetPathCount);
{$ENDIF}
      for J := 0 to GetPathCount - 1 do
      begin
        tmpPointArr[J].X := Path[J].X;
        tmpPointArr[J].Y := Path[J].Y;
        tmpPointArr[J].StayOn := Path[J].StayOn;
      end;
    end;
end;

procedure TDelphiXPathsEditForm.btnMoveUpClick(Sender: TObject);
var
  T: TTrace;
  I: Integer;
  P: TPath;
begin
  {move selected path to up}
  if cbListOfTraces.ItemIndex <> -1 then begin
    {ziskej aktivni stopu}
    T := PrivateTraces.Items[cbListOfTraces.ItemIndex];
    for I := 0 to T.Blit.GetPathCount - 1 do
    begin
      P := T.Blit.Path[I];
      P.Y := P.Y - eDist.AsInteger;
      T.Blit.Path[I] := P;
    end;
  end;
  ShowTracesOnPane;
end;

procedure TDelphiXPathsEditForm.btnMoveDownClick(Sender: TObject);
var
  T: TTrace;
  I: Integer;
  P: TPath;
begin
  {move selected path to down}
  if cbListOfTraces.ItemIndex <> -1 then begin
    {ziskej aktivni stopu}
    T := PrivateTraces.Items[cbListOfTraces.ItemIndex];
    for I := 0 to T.Blit.GetPathCount - 1 do
    begin
      P := T.Blit.Path[I];
      P.Y := P.Y + eDist.AsInteger;
      T.Blit.Path[I] := P;
    end;
  end;
  ShowTracesOnPane;
end;

procedure TDelphiXPathsEditForm.btnMoveLeftClick(Sender: TObject);
var
  T: TTrace;
  I: Integer;
  P: TPath;
begin
  {move selected path to left}
  if cbListOfTraces.ItemIndex <> -1 then begin
    {ziskej aktivni stopu}
    T := PrivateTraces.Items[cbListOfTraces.ItemIndex];
    for I := 0 to T.Blit.GetPathCount - 1 do
    begin
      P := T.Blit.Path[I];
      P.X := P.X - eDist.AsInteger;
      T.Blit.Path[I] := P;
    end;
  end;
  ShowTracesOnPane;
end;

procedure TDelphiXPathsEditForm.btnMoveRightClick(Sender: TObject);
var
  T: TTrace;
  I: Integer;
  P: TPath;
begin
  {move selected path to right}
  if cbListOfTraces.ItemIndex <> -1 then begin
    {ziskej aktivni stopu}
    T := PrivateTraces.Items[cbListOfTraces.ItemIndex];
    for I := 0 to T.Blit.GetPathCount - 1 do
    begin
      P := T.Blit.Path[I];
      P.X := P.X + eDist.AsInteger;
      T.Blit.Path[I] := P;
    end;
  end;
  ShowTracesOnPane;
end;

procedure Rotate(iRotAng: Single; x, y: Double; var Nx, Ny: Double);
  procedure SinCosS(const Theta: Single; var Sin, Cos: Single); register;
  // EAX contains address of Sin
  // EDX contains address of Cos
  // Theta is passed over the stack
  asm
    FLD  Theta
    FSINCOS
    FSTP DWORD PTR [EDX]    // cosine
    FSTP DWORD PTR [EAX]    // sine
  end;
const PI256 = 2 * PI / 256;
var
  SinVal, CosVal, RotAng: Single;
begin
  RotAng := iRotAng * PI256;
  SinCosS(RotAng, SinVal, CosVal);
  Nx := x * CosVal - y * SinVal;
  Ny := y * CosVal + x * SinVal;
end;

procedure RotateO(RotAng: Double; x, y, ox, oy: Double; var Nx, Ny: Double);
begin
  Rotate(RotAng, x - ox, y - oy, Nx, Ny);
  Nx := Nx + ox;
  Ny := Ny + oy;
end;

function TDelphiXPathsEditForm.GetSizesOfTrace(out x, y, oWidth, oHeight: Integer): Boolean;
var
  T: TTrace;
  I: Integer;
  P: TPath;
  maxX, minX, maxY, minY: Single;
begin
  Result := False;
  oWidth := 0;
  oHeight := 0;
  maxX := 0;
  minX := MaxInt;
  maxY := 0;
  minY := MaxInt;
  if cbListOfTraces.ItemIndex <> -1 then begin
    {ziskej aktivni stopu}
    T := PrivateTraces.Items[cbListOfTraces.ItemIndex];
    for I := 0 to T.Blit.GetPathCount - 1 do
    begin
      P := T.Blit.Path[I];
      if P.X > maxX then maxX := P.X;
      if P.Y > maxY then maxY := P.Y;
      if P.X < minX then minX := P.X;
      if P.Y < minY then minY := P.Y;
    end;
    x := Round(minX);
    y := Round(minY);
    oWidth := Abs(Round(maxX) - Round(minX));
    oHeight := Abs(Round(maxY) - Round(minY));
    Result := True;
  end;
end;

procedure TDelphiXPathsEditForm.RotatePathForAngle(Angle: Integer);
var
  T: TTrace;
  I, x, y, width, height: Integer;
  P: TPath;
  nX, nY, dX, dY: Double;
begin
  if GetSizesOfTrace(x, y, Width, Height) then
  begin
    dX := (x + width / 2);
    dY := (y + height / 2);
    T := PrivateTraces.Items[cbListOfTraces.ItemIndex];
    for I := 0 to T.Blit.GetPathCount - 1 do
    begin
      P := T.Blit.Path[I];
      RotateO(Angle, P.X, P.Y, dX, dY, nX, nY);
      P.X := nX;
      P.Y := nY;
      T.Blit.Path[I] := P;
    end;
  end;
end;

procedure TDelphiXPathsEditForm.btnRotateLeftClick(Sender: TObject);
begin
  RotatePathForAngle(-1 * eDist.AsInteger);
  RefreshShowTracesOnPaneOnly
end;

procedure TDelphiXPathsEditForm.btnRotateRightClick(Sender: TObject);
begin
  RotatePathForAngle(eDist.AsInteger);
  RefreshShowTracesOnPaneOnly
end;

end.
