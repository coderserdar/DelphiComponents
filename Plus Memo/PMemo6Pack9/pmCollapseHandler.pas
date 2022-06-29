unit pmCollapseHandler;

{ © Electro-Concept Mauricie, 2003 }
{ Implements TpmCollapseHandler object, used for rendering collapse bar and sign
  in a TPlusMemo }

{ Note: to install this component on your palette, add this file to your package }

{$DEFINE pmCollapseHandler}

{UCONVERT}
{$IFDEF PMCollapseHandlerClx}
  {$DEFINE pmClx}
{$ENDIF}
{/UCONVERT}

{$IFDEF VER150} {$DEFINE D7New} {$ENDIF}    // Avoid unsafe warnings under D7
{$IFDEF VER170} {$DEFINE D7New} {$ENDIF}

{$IFDEF D7New}
  {$WARN UNSAFE_CAST OFF}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_TYPE OFF}
{$ENDIF}

interface

{$IFDEF pmClx}
uses Classes, QGraphics, PMSupportClx, QForms, QControls;
{$ELSE}
uses Classes, Graphics, PMSupport;
{$ENDIF}

type
  TpmCollapseHandler = class(TpmsCollapseHandler, IpmCollapseHandler, IpmsNotify)
    private
      fBarPen: TPen;
      fCollapseSignPen: TPen;
      fMaxLevels: Integer;
      fMemoList: TList;
      fNav: TPlusNavigator;
      fMouseDown: Boolean;
      {$IFDEF pmClx} fSaveCursor: TCursor; {$ENDIF}
      procedure setMaxLevels(const Value: Integer);
      procedure SetBarPen(const Value: TPen);
      procedure SetCollapseSignPen(const Value: TPen);

      procedure GetLevels(const Par: ParInfo; const Line: LineInfo; var StartLevel, EndLevel, BarLevel: Integer;
                          var Collapsed: Boolean);
      procedure InvalidateMemos(Sender: TObject);
      procedure LinkMemo(Sender: TComponent; Attach: Boolean);
      procedure PaintLine(Sender: TComponent; Canvas: pmHDC; XPos, Height: Integer; const par: ParInfo; const line: LineInfo);
      procedure Notify(Sender: TComponent; Events: TpmEvents);

    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
    published
      property BarPen: TPen read fBarPen write SetBarPen;
      property CollapseSignPen: TPen read fCollapseSignPen write SetCollapseSignPen;
      property MaxLevels: Integer read fMaxLevels write setMaxLevels default 4;
    end;

procedure Register;

implementation

{$R pmCollapseHandler.res}

{$IFDEF pmClx}
uses Qt, PlusMemoClx, SysUtils;
{$ELSE}
uses PlusMemo, Windows, Messages, SysUtils;
{$ENDIF}

{$IFDEF pmClx}
const
  WM_LBUTTONDBLCLK = QEventType_MouseButtonDblClick;
  WM_LBUTTONUP = QEventType_MouseButtonRelease;
  WM_MOUSEMOVE = QEventType_MouseMove;
  WM_LBUTTONDOWN = QEventType_MouseButtonPress;
{$ENDIF}


{ TpmCollapseHandler }
const CollapseCursorResName = {$IFDEF PMCollapseHandlerU} 'PMUCOLLAPSECURSOR' {$ELSE} 'PMCOLLAPSECURSOR' {$ENDIF};
      ExpandCursorResName = {$IFDEF PMCollapseHandlerU} 'PMUEXPANDCURSOR' {$ELSE}  'PMEXPANDCURSOR' {$ENDIF};

var CollapseCursor: THandle = 0;
    ExpandCursor: THandle = 0;

constructor TpmCollapseHandler.Create(AOwner: TComponent);
begin
  inherited;
  fMemoList:= TList.Create;
  fMaxLevels:= 4;
  fBarPen:= TPen.Create;
  fBarPen.Color:= $F0CAA6;
  fBarPen.Width:= 7;
  fBarPen.OnChange:= InvalidateMemos;
  fCollapseSignPen:= TPen.Create;
  fCollapseSignPen.OnChange:= InvalidateMemos;
  fNav:= TPlusNavigator.Create(nil);
end;

destructor TpmCollapseHandler.Destroy;
var i: Integer;
begin
  fBarPen.Free;
  fCollapseSignPen.Free;
  fNav.Free;
  for i:= 0 to fMemoList.Count-1 do LinkMemo(TComponent(fMemoList[i]), False);
  fMemoList.Free;
  inherited;
end;

procedure TpmCollapseHandler.GetLevels(const Par: ParInfo; const Line: LineInfo; var StartLevel, EndLevel, BarLevel: Integer;
                                       var Collapsed: Boolean);
var sdnb: Integer;
begin
  BarLevel:= Byte(Par.BlockState*pmsCBlockLevel);
  if BarLevel<>0 then
    begin   // static blocks take precedence over dynamic ones
      StartLevel:= pmsGetParBlockStartLevel(Par);
      EndLevel:= pmsGetParBlockEndLevel(Par);
      // find the first collapsed level, set BarLevel equal to this
      Collapsed:= False;
      for sdnb:= 1 to BarLevel do
          if pmsGetParCollapsed(Par, sdnb) then
            begin
              BarLevel:= sdnb;
              Collapsed:= True;
              if EndLevel>BarLevel then EndLevel:= BarLevel;
              Break
            end
    end

  else
    begin   // process dynamic block section
      sdnb:= Line.StartDynNb;
      if sdnb>0 then BarLevel:= DynToCollapseLevel(Par.ParExtra.DynCodes[sdnb-1])
      else
          if pmpHasExtra in Par.ParState then BarLevel:= DynToCollapseLevel(Par.ParExtra.StartDynAttrib^)
                                         else BarLevel:= 0;
      StartLevel:= BarLevel;
      EndLevel:= BarLevel;
      Collapsed:= False;

      if pmpHasExtra in Par.ParState then
          while (sdnb<Length(Par.ParExtra.DynCodes)) and (Par.ParExtra.DynCodes[sdnb].DynOffset<=Line.Stop) do
            begin
              EndLevel:= DynToCollapseLevel(Par.ParExtra.DynCodes[sdnb]);
              if EndLevel>BarLevel then
                begin
                  BarLevel:= EndLevel;
                  if pmdCollapsed in Par.ParExtra.DynCodes[sdnb].CollpsState then
                    begin
                      Collapsed:= True;
                      Break
                    end
                end;
              Inc(sdnb)
            end;
      if EndLevel<=StartLevel then BarLevel:= StartLevel
    end
end;

procedure TpmCollapseHandler.InvalidateMemos(Sender: TObject);
var i: Integer;
begin
  for i:= 0 to fMemoList.Count-1 do TPlusMemo(fMemoList[i]).Invalidate
end;

procedure TpmCollapseHandler.LinkMemo(Sender: TComponent;  Attach: Boolean);
var snotify: IpmsNotify; smemo: TPlusMemo; sindex: Integer;
begin
  snotify:= Self;
  sindex:= fMemoList.IndexOf(Sender);
  smemo:= Sender as TPlusMemo;
  if Attach then
    begin
      if sindex<0 then
        begin
          fMemoList.Add(Sender);
          smemo.MsgList.Add(Pointer(snotify))
        end
    end
  else
      if sindex>=0 then
        begin
          fMemoList.Delete(sindex);
          sindex:= smemo.MsgList.IndexOf(Pointer(snotify));
          if sindex>=0 then smemo.MsgList.Items[sindex]:= nil
        end
end;

procedure TpmCollapseHandler.Notify(Sender: TComponent; Events: TpmEvents);
var
  smemo: TPlusMemo;
  x, y: Integer;
  sal, sol, sbl, slevel, sbw, sly, spc2: Integer;
  scl: Boolean;
  scur: Integer;
  sMsg: {$IFDEF pmClx} QEventType {$ELSE} Cardinal {$ENDIF};
  {$IFDEF pmClx} soldlevel: Integer; {$ENDIF}

begin
  if csDesigning in ComponentState then Exit;
  smemo:= TPlusMemo(Sender);

  if pmeMessage in Events then
    begin
      sMsg:= {$IFDEF pmClx} QEvent_Type(QEventH(smemo.WinMsg.Msg)) {$ELSE} smemo.WinMsg.Msg {$ENDIF};
      case sMsg of
        WM_LBUTTONDOWN:
          if smemo.MouseNav.fExtra<>0 then
            begin
              sal:= Byte(smemo.MouseNav.Par.BlockState*pmsCBlockLevel);
              if sal<>0 then  // static block
                  if smemo.MouseNav.fExtra>0 then smemo.CollapseBlock(smemo.MouseNav.ParNumber, smemo.MouseNav.fExtra)
                                             else smemo.ExpandBlock(smemo.MouseNav.ParNumber, -smemo.MouseNav.fExtra)
              else
                begin
                  fNav.fPMemo:= Sender;
                  fNav.Assign(smemo.MouseNav);
                  sal:= Abs(smemo.MouseNav.fExtra);
                  fNav.DynNb:= 0;
                  while DynToCollapseLevel(fNav.pDynAttr^)<>sal do
                    begin
                      x:= fNav.DynNb;
                      fNav.DynNb:= x+1;
                      if fNav.DynNb<x+1 then Break  // property DynNb is range limited to the number of dyn codes
                    end;

                  if smemo.MouseNav.fExtra>0 then fNav.Collapse
                                             else fNav.Expand;
                  fNav.Invalidate;
                  fNav.fPMemo:= nil
                end;
              fMouseDown:= True;
              {$IFNDEF pmClx} smemo.WinMsg.Result:= 127 {$ENDIF}
            end;

        WM_LBUTTONDBLCLK, WM_LBUTTONUP:
            if smemo.MouseNav.fExtra<>0 then {$IFNDEF pmClx} smemo.WinMsg.Result:= 127 {$ENDIF};


        WM_MOUSEMOVE:
        if fMouseDown then {$IFNDEF pmClx} smemo.WinMsg.Result:= 127 {$ENDIF}   // cancel further processing by smemo
      end
    end;  // pmeMessage

  if pmeAfterMessage in Events then
    begin
      sMsg:= {$IFDEF pmClx} QEvent_Type(QEventH(smemo.WinMsg.Msg)) {$ELSE} smemo.WinMsg.Msg {$ENDIF};
      if ((sMsg=WM_MOUSEMOVE) and (not fMouseDown)) or ((sMsg=WM_LBUTTONUP) and fMouseDown) then
        begin
          {$IFDEF pmClx} soldlevel:= smemo.MouseNav.fExtra; {$ENDIF}
          smemo.MouseNav.fExtra:= 0;
          fMouseDown:= False;
          if not smemo.MouseIsDown then
            begin
              {$IFDEF pmClx}
              x:= QMouseEvent_x(QMouseEventH(smemo.WinMsg.Msg));
              y:= QMouseEvent_y(QMouseEventH(smemo.WinMsg.Msg));
              {$ELSE}
              x:= smemo.WinMsg.LParamLo - smemo.LeftOrigin;
              y:= smemo.WinMsg.LParamHi;
              {$ENDIF}
              if x<smemo.LeftMargin then
                begin
                  sbw:= fBarPen.Width;
                  if sbw>smemo.LineHeightRT then sbw:= smemo.LineHeightRT;
                  sly:= (y+smemo.TopOrigin) mod smemo.LineHeightRT;
                  spc2:= (smemo.LineHeightRT - sbw) div 2;
                  if (sly>=spc2) and (sly<sbw+spc2) and (x<sbw*MaxLevels) then
                    begin
                      slevel:= x div (sbw + 1);
                      fNav.fPMemo:= smemo;
                      fNav.DisplayY:= y;
                      GetLevels(fNav.Par^, fNav.NavLines.LinePointers[fNav.ParLine]^, sal, sol, sbl, scl);
                      if sbl>0 then
                        begin
                          if (slevel=MaxLevels-1) and (slevel<sal) then slevel:= sal;
                          if (slevel>=sal) and (slevel<sbl) then
                            begin
                              if slevel+1<sbl then
                                if fNav.fPar.BlockState*pmsCBlockLevel<>[] then scl:= pmsGetParCollapsed(fNav.fPar^, slevel+1)
                                else
                                  begin
                                    scl:= DynToCollapseLevel(fNav.pDynAttr^)=slevel+1;
                                    while (not scl) and fNav.ForwardToDyn(fNav.fPar.StartOffset+GetParLength(fNav.fPar^)) do
                                      begin
                                        fNav.RightOfDyn;
                                        scl:= DynToCollapseLevel(fNav.pDynAttr^)=slevel+1
                                      end;
                                    if scl then scl:= pmdCollapsed in fNav.pDynAttr.CollpsState
                                  end;

                              if scl then
                                begin
                                  {$IFDEF pmClx}
                                  scur:= crSizeNS;
                                  {$ELSE}
                                  if ExpandCursor=0 then ExpandCursor:= LoadCursor(HInstance, ExpandCursorResName);
                                  scur:= ExpandCursor;
                                  {$ENDIF}
                                  smemo.MouseNav.fExtra:= - (slevel+1)
                                end
                              else
                                begin
                                  {$IFDEF pmClx}
                                  scur:= crSizeAll;
                                  {$ELSE}
                                  if CollapseCursor=0 then CollapseCursor:= LoadCursor(HInstance, CollapseCursorResName);
                                  scur:= CollapseCursor;
                                  {$ENDIF}
                                  smemo.MouseNav.fExtra:= slevel+1
                                end;
                              {$IFDEF pmClx}
                              if soldlevel=0 then fSaveCursor:= smemo.Cursor;
                              smemo.Cursor:= scur;//} Screen.Cursor:= scur;
                              {$ELSE}
                              Windows.SetCursor(scur);
                              smemo.WinMsg.Result:= 127
                              {$ENDIF}
                            end
                        end;
                      fNav.Invalidate;
                      fNav.fPMemo:= nil
                    end    // x, y in collapse sign area
                end;    // x in left margin
              {$IFDEF pmClx}
              if (soldlevel<>0) and (smemo.MouseNav.fExtra=0) then smemo.Cursor:= fSaveCursor
              {$ENDIF}
            end   // not smemo.MouseIsDown
        end    //  MouseMove and not fMouseDown

    end  // pmeAfterMessage

end;

procedure TpmCollapseHandler.PaintLine(Sender: TComponent; Canvas: pmHDC; XPos, Height: Integer;
                                       const par: ParInfo; const line: LineInfo);
var i, x, y, sbarlevel, startlevel, endlevel, sbw2: Integer; sbarcollapsed: Boolean;
    {$IFNDEF pmClx} spen: HPen; smode: Integer; {$ENDIF}
begin
  GetLevels(Par, Line, startlevel, endlevel, sbarlevel, sbarcollapsed);

  if sbarlevel>0 then
    begin
      {$IFDEF pmClx}
      Canvas.Pen.Assign(fBarPen);
      {$ELSE}
      smode:= SetROP2(Canvas, R2_MASKPEN);
      spen:= SelectObject(Canvas, fBarPen.Handle);
      {$ENDIF}
      for i:= 0 to sbarlevel-1 do
        if i<MaxLevels then
          begin
            x:= i*(fBarPen.Width+1) + fBarPen.Width div 2 - XPos;
            {$IFDEF pmClx}
            Canvas.MoveTo(x, 0);
            if i>=endlevel then Canvas.LineTo(x, Height-fBarPen.Width)
                           else Canvas.LineTo(x, Height);
            {$ELSE}
            MoveToEx(Canvas, x, 0, nil);
            if i>=endlevel then LineTo(Canvas, x, Height - fBarPen.Width)
                           else LineTo(Canvas, x, Height)
            {$ENDIF}
          end;
      {$IFNDEF pmClx} SetROP2(Canvas, smode); {$ENDIF}

      if startlevel<>endlevel then
        begin
          {$IFDEF pmClx}
          Canvas.Pen.Assign(fCollapseSignPen);
          {$ELSE}
          SelectObject(Canvas, fCollapseSignPen.Handle);
          {$ENDIF}
          sbw2:= fBarPen.Width;
          if Height<sbw2 then sbw2:= Height;
          y:= (Height - sbw2) div 2;
          for i:= startlevel to endlevel-1 do
            begin
              if i<MaxLevels then x:= i
                             else x:= MaxLevels-1;
              x:= x*(fBarPen.Width+1) + (fBarPen.Width-sbw2) div 2 - XPos;
              {$IFDEF pmClx}
              Canvas.Rectangle(x, y, x+sbw2, y+sbw2);
              Canvas.MoveTo(x, y + sbw2 div 2);
              Canvas.LineTo(x + sbw2, y + sbw2 div 2);
              if (i=endlevel-1) and sbarcollapsed then
                begin
                  Canvas.MoveTo(x + sbw2 div 2, y);
                  Canvas.LineTo(x + sbw2 div 2, y + sbw2)
                end

              {$ELSE}
              Rectangle(Canvas, x, y, x+sbw2, y+sbw2);
              MoveToEx(Canvas, x, y + sbw2 div 2, nil);
              LineTo(Canvas, x + sbw2, y + sbw2 div 2);
              if (i=endlevel-1) and sbarcollapsed then
                begin
                  MoveToEx(Canvas, x + sbw2 div 2, y, nil);
                  LineTo(Canvas, x + sbw2 div 2, y + sbw2)
                end
              {$ENDIF}
            end;

          for i:= startlevel-1 downto endlevel do
            begin
              if i<MaxLevels then x:= i
                             else x:= MaxLevels-1;
              x:= x*(fBarPen.Width+1) + (fBarPen.Width-sbw2) div 2 - XPos;
              {$IFDEF pmClx}
              Canvas.MoveTo(x, y+sbw2);
              Canvas.LineTo(x + sbw2, y+sbw2)
              {$ELSE}
              MoveToEx(Canvas, x, y+sbw2, nil);
              LineTo(Canvas, x + sbw2, y+sbw2)
              {$ENDIF}
            end
        end;

      {$IFNDEF pmClx} SelectObject(Canvas, spen) {$ENDIF}
    end
end;

procedure TpmCollapseHandler.SetBarPen(const Value: TPen);
begin
  fBarPen.Assign(Value);
  InvalidateMemos(Self)
end;

procedure TpmCollapseHandler.SetCollapseSignPen(const Value: TPen);
begin
  fCollapseSignPen.Assign(Value);
  InvalidateMemos(Self)
end;

procedure TpmCollapseHandler.setMaxLevels(const Value: Integer);
begin
  if Value<>fMaxLevels then
    begin
      fMaxLevels := Value;
      InvalidateMemos(Self)
    end
end;

procedure Register;
begin
  RegisterComponents({UCONVERT}'PlusMemo'{/UCONVERT}, [TpmCollapseHandler]);
end;

end.
