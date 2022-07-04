unit DsBandRow;
//------------------------------------------------------------------------------
interface
//------------------------------------------------------------------------------
uses
  Windows, Controls, Contnrs, DsBand;
//------------------------------------------------------------------------------
const
  cGrabberSize = 10;
  cGrabberWidth = 2;
//------------------------------------------------------------------------------
type
//------------------------------------------------------------------------------
  // Спомага да се върне Band на първоначалното си местоположение
  TRedoItem = class
  private
    fPos: Integer;
    fBand: TDsBand;
  public
    // Първоначалната позиция на Band-a преди да започне наместването
    property Pos: Integer read fPos write fPos;
    // Band-а за който се пази първоначалната позиция
    property Band: TDsBand read fBand write fBand;
  end;//TRedoItem
//------------------------------------------------------------------------------
  // TBandList don't work direct with TDsBand. Instead uses this facade.
  TBandFacade = class
  private
    fBand: TDsBand;
  public
    procedure setInsets(aInsets: TRect); virtual; abstract;
    function BandLength: Integer; virtual; abstract;
    function Width: Integer; virtual; abstract;
    function BandStart: Integer; virtual; abstract;
    procedure MoveTo(aPos: Integer); virtual; abstract;
    procedure setPos(aPos, aTop: Integer); virtual; abstract;
    function Top: Integer; virtual; abstract;
    // The position after the band
    function PosAfter: Integer; virtual;
    procedure setTop(aTop: Integer); virtual; abstract;
    property Band: TDsBand read fBand write fBand;
  end;//TBandFacade
//------------------------------------------------------------------------------
  // Списък от Band-ове и операции с тях
  TBandList = class
  private
    fItems: TObjectList;
    fBand: TDsBand;
    fRedo: TObjectList;
    fLength: Integer;
    fTop: Integer;
    fBandFacade: TBandFacade;
    function getCount: integer;
    function getBand(Idx: integer): TDsBand;
    procedure setLength(const Value: Integer);
    procedure setTop(const Value: Integer);
  protected
    function getWidth: integer;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    // Добавя Band в списъка
    function addBandToRow(aControl: TControl): TDsBand;
    function addBandToRowAndStop(aControl: TControl): TDsBand;
    // Маха Band от списъка
    procedure RemoveBand(aControl: TControl);
    //
    procedure Extract(aBand: TDsBand);
    // Проверява дали позицията е на този ред
    function IsInside(aTop: Integer): Boolean;
    // Намира Band по контролата, която обгражда
    function Find(aControl: TControl): TDsBand; overload;
    // Намира Band-а на дадената позиция
    function Find(aX, aY: Integer): TDsBand; overload;
    // Започва наместването на Band
    procedure StartBand(aBand: TDsBand);
    // Moves the active band to a new position given by aPos
    function MoveBand(aPos: Integer): Boolean;
    // Прекратява наместването на Band
    procedure StopBand;
    // Избутва Band за да може да застане намествания Band
    function PushBandRight(aBand: TDsBand; aLen: Integer; aShrink: Boolean): Boolean;
    function PushBandLeft(aBand: TDsBand; aLen: Integer): Boolean;
    //
    procedure AddRedoBand(aBand: TDsBand);
    // Опитва да върне променените Band-ове на първоначалната им позиция
    procedure RedoBands;
    // Finds the band on the left side of the given band
    function FindLeftBandCur(aBand: TDsBand): TDsBand;
    function FindLeftBand(aPos: Integer): TDsBand;
    function FindRightBand(aPos: Integer): TDsBand;
    function FindRedo(aBand: TDsBand): TRedoItem;

    //
    function ClosestFreePlace(aBand: TDsBand; aPos: Integer): Integer;
    property Top: Integer read fTop write setTop;
    // The width of the row
    property Width: integer read getWidth;
    // Връща Band по индекса му в списъка
    property Items[Idx: integer]: TDsBand read getBand; default;
    // Броя на Band-овете в списъка
    property Count: integer read getCount;

    // Широчина на контейнера
    property Length: Integer read fLength write setLength;

    property BandFacade: TBandFacade read fBandFacade write fBandFacade;
    property Band: TDsBand read fBand;
  end;//TBandList
//------------------------------------------------------------------------------
implementation

uses
  SysUtils, Types;

//------------------------------------------------------------------------------
// TBandList
//------------------------------------------------------------------------------
constructor TBandList.Create;
begin
  inherited Create;
  fItems := TObjectList.Create;
  fRedo := TObjectList.Create;
  BandFacade := nil;
end;//Create
//------------------------------------------------------------------------------
destructor TBandList.Destroy;
begin
  FreeAndNil(fItems);
  FreeAndNil(fRedo);
  FreeAndNil(fBandFacade);
  inherited Destroy;
end;procedure TBandList.Extract(aBand: TDsBand);
begin
  fItems.Extract(aBand);
end;

//Destroy
//------------------------------------------------------------------------------
function TBandList.getWidth: integer;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to fItems.Count -1 do
  begin
    BandFacade.Band := Items[i];
    if BandFacade.Width > Result then
      Result := BandFacade.Width;
  end;
end;//getHeight
//------------------------------------------------------------------------------
function TBandList.IsInside(aTop: Integer): Boolean;
begin
  Result := (aTop >= Top) and (aTop < Top + Width);
end;//IsInside
//------------------------------------------------------------------------------
function TBandList.addBandToRow(aControl: TControl): TDsBand;
begin
  Result := TDsBand.Create;
  BandFacade.Band := Result;
  BandFacade.setInsets(Rect(cGrabberSize, cGrabberWidth, cGrabberWidth, cGrabberWidth));
  Result.Control := aControl;
  fItems.Add(Result);
end;//AddBand
//------------------------------------------------------------------------------
function TBandList.addBandToRowAndStop(aControl: TControl): TDsBand;
begin
  Result := AddBandToRow(aControl);
  StartBand(Result);
  BandFacade.Band := Result;
  MoveBand(BandFacade.BandStart);
  StopBand;
end;
//------------------------------------------------------------------------------
procedure TBandList.RemoveBand(aControl: TControl);
begin
  fItems.Remove(Find(aControl));
  StopBand;
end;//RemoveBand
//------------------------------------------------------------------------------
function TBandList.Find(aControl: TControl): TDsBand;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to fItems.Count - 1 do
    if TDsBand(fItems[i]).Control = aControl then
    begin
      Result := TDsBand(fItems[i]);
      Break;
    end;
end;//Find
//------------------------------------------------------------------------------
function TBandList.Find(aX, aY: Integer): TDsBand;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to fItems.Count - 1 do
    if PtInRect(Items[i].BoundsRect, Point(aX, aY)) and
       not PtInRect(Items[i].Control.BoundsRect, Point(aX, aY)) then
    begin
      Result := Items[i];
      Break;
    end;
end;//Find
//------------------------------------------------------------------------------
procedure TBandList.StartBand(aBand: TDsBand);
begin
  if aBand = nil then
    Exit;
  fBand := aBand;
end;//StartBand
//------------------------------------------------------------------------------
function TBandList.MoveBand(aPos: Integer): Boolean;
var
  vLen, vOld: Integer;
  vLeftFacade, vRightFacade: TBandFacade;
  vLeftBand, vRightBand: TDsBand;
begin
  Result := False;

  if fBand = nil then
    Exit;

  BandFacade.Band := fBand;

  vLeftFacade := TBandFacade(BandFacade.NewInstance);
  vRightFacade := TBandFacade(BandFacade.NewInstance);
  try
    BandFacade.Band := fBand;
    vLeftBand := FindLeftBand(aPos);
    vRightBand := FindRightBand(aPos);
    vLeftFacade.Band := vLeftBand;
    vRightFacade.Band := vRightBand;
    //
    if aPos < 0 then // If outside the left border
    begin
      aPos := 0;
    end
    else             
    begin
      if (vLeftBand <> nil) then // If there is a band on the left side
      begin
        if (aPos < vLeftFacade.PosAfter) then // May overlap the left band
        begin
          vOld := vLeftFacade.BandStart;
          vLen := vLeftFacade.PosAfter - aPos; // what is the length to move to the end
          if vLen > 0 then // Да не би вече да е било преместено
          begin
            PushBandLeft(vLeftBand, vLen);
            BandFacade.Band := fBand;
            // Can't free space so we change our mind
            if (vOld - vLeftFacade.BandStart) < vLen then
              aPos := vLeftFacade.PosAfter;
          end;
        end;
      end;
    end;

    if (vRightBand <> nil) then // If there is a band on the right side
    begin
      // May overlap with the right band
      if aPos + BandFacade.BandLength > vRightFacade.BandStart then // If overlap
      begin
        vOld := vRightFacade.BandStart;
        vLen := aPos + BandFacade.BandLength - vRightFacade.BandStart;
        PushBandRight(vRightBand, vLen, False);
        BandFacade.Band := fBand;
        if (vRightFacade.BandStart - vOld) < vLen then // if the push do not succeed
        begin
          aPos := vRightFacade.BandStart - BandFacade.BandLength;
          if (vLeftBand = nil) and (aPos < 0) then
          begin
            // The last band must shrink
            aPos := 0;
            vLen := aPos + BandFacade.BandLength - vRightFacade.BandStart;
            PushBandRight(vRightBand, vLen, True);
          end
          else
          if (vLeftBand <> nil) then
          begin
            if (aPos < vLeftFacade.PosAfter) then
            begin
              vOld := vLeftFacade.BandStart;
              vLen := vLeftFacade.PosAfter - aPos;
              PushBandLeft(vLeftBand, vLen);
              BandFacade.Band := fBand;
              if (vOld - vLeftFacade.BandStart) < vLen then
              begin
                aPos := vLeftFacade.BandStart - BandFacade.BandLength;
                vLen := vLeftFacade.PosAfter + BandFacade.BandLength - vRightFacade.BandStart;
                PushBandRight(vRightBand, vLen, True);
                // Not enough space. Must last band shrink but for now goes out
              end;
            end;
          end;
        end;
      end;
    end
    else
    begin
      BandFacade.Band := fBand;
      // if is outside the right border
      if aPos + BandFacade.BandLength > Length then
      begin
        aPos := Length - BandFacade.BandLength;
        // if band can not fit in the row
        if aPos < 0 then
        begin
          // Have to shrink
          // Не може да измести вдясно за да застане. Трябва последния бенд на реда да се свие
          // Но не може и да се слага на нулева позиция, защото там може да има друг бенд
          // aPos := 0;
          if (vLeftBand <> nil) then
            aPos := vLeftFacade.PosAfter
          else
            aPos := 0;
        end
        else
        if (vLeftBand <> nil) then
        begin
          if (aPos < vLeftFacade.PosAfter) then
          begin
            vOld := vLeftFacade.BandStart;
            vLen := vLeftFacade.PosAfter - aPos;
            PushBandLeft(vLeftBand, vLen);
            BandFacade.Band := fBand;
            if (vOld - vLeftFacade.BandStart) < vLen then
            begin
              // Not enough space. Must shrink, but for now go outside the right border
              aPos := vLeftFacade.PosAfter;
            end;
          end;
        end;
      end;
    end;

    BandFacade.Band := fBand;
    Result := (BandFacade.BandStart <> aPos) or (BandFacade.Top <> Top);
    if Result then
    begin
      BandFacade.setPos(aPos, Top);
      RedoBands;
    end;
  finally
    vLeftFacade.Free;
    vRightFacade.Free;
  end;
end;//MoveBand
//------------------------------------------------------------------------------
procedure TBandList.StopBand;
begin
  fBand := nil;
  fRedo.Clear;
end;//StopBand
//------------------------------------------------------------------------------
function TBandList.PushBandRight(aBand: TDsBand; aLen: Integer; aShrink: Boolean): Boolean;
var
  i: Integer;
  vOver: TDsBand;
  vLen, vOld: Integer;
  vBandRight, vOverStart: Integer;
begin
  BandFacade.Band := aBand;
  vBandRight := BandFacade.PosAfter - 1;
  vOver := nil;
  vOverStart := 0;
  // Find the right standing band
  for i := 0 to Count - 1 do
    if (Items[i] <> aBand) and (Items[i] <> fBand) then
    begin
      BandFacade.Band := Items[i];
      if (BandFacade.BandStart > vBandRight) and // May stay on the way
         (BandFacade.BandStart <= vBandRight + aLen) then // it stays on the way
      begin
        if vOver = nil then
        begin
          vOver := Items[i];
          vOverStart := BandFacade.BandStart;
        end
        else
        if BandFacade.BandStart < vOverStart then
        begin
          vOver := Items[i];
          vOverStart := BandFacade.BandStart;
        end;
      end;
    end;
  //   
  if vOver <> nil then
  begin
    BandFacade.Band := vOver;
    vOld := BandFacade.BandStart;
    vLen := (vBandRight + aLen) - BandFacade.BandStart + 1;
    PushBandRight(vOver, vLen, aShrink);
    BandFacade.Band := vOver;
    if (BandFacade.BandStart - vOld) < vLen then
      aLen := aLen - (vLen - (BandFacade.BandStart - vOld));
  end
  else
  if vBandRight + aLen >= Length then
  begin
    if not aShrink then
      if (Length - vBandRight - 1) > 0 then
        aLen := Length - vBandRight - 1
      else
        aLen := 0;
  end;

  Result := aLen <> 0;
  if Result then
  begin
    BandFacade.Band := aBand;
    AddRedoBand(aBand);
    BandFacade.MoveTo(BandFacade.BandStart + aLen);
  end;
end;
//------------------------------------------------------------------------------
function TBandList.PushBandLeft(aBand: TDsBand; aLen: Integer): Boolean;
var
  i: Integer;
  vOver: TDsBand;
  vLen, vOld: Integer;
  vBandStart, vOverStart: Integer;
begin
  BandFacade.Band := aBand;
  vBandStart := BandFacade.BandStart;
  //Find Left over
  vOver := nil;
  vOverStart := 0;
  for i := 0 to Count - 1 do
    if (Items[i] <> aBand) and (Items[i] <> fBand) then
    begin
      BandFacade.Band := Items[i];
      if (BandFacade.PosAfter <= vBandStart) and // May disallaw moving left
         (BandFacade.PosAfter > vBandStart - aLen) then // Really disallow it
      begin
        if vOver = nil then
        begin
          vOver := Items[i];
          vOverStart := BandFacade.BandStart;
        end
        else
        if BandFacade.BandStart > vOverStart then
        begin
          vOver := Items[i];
          vOverStart := BandFacade.BandStart;
        end;
      end;
    end;

  if vOver <> nil then
  begin
    BandFacade.Band := vOver;
    vOld := BandFacade.BandStart;
    vLen := BandFacade.PosAfter - (vBandStart - aLen);
    PushBandLeft(vOver, vLen);
    BandFacade.Band := vOver;
    if (vOld - BandFacade.BandStart) < vLen then
      aLen := aLen - (vLen - (vOld - BandFacade.BandStart));
  end
  else
  begin
    BandFacade.Band := aBand;
    if BandFacade.BandStart - aLen < 0 then
      aLen := BandFacade.BandStart;
  end;

  Result := aLen <> 0;
  if Result then
  begin
    BandFacade.Band := aBand;
    AddRedoBand(aBand);
    BandFacade.MoveTo(BandFacade.BandStart - aLen);
  end;
end;//PushBandLeft
//------------------------------------------------------------------------------
procedure TBandList.AddRedoBand(aBand: TDsBand);
var
  i: Integer;
  vExists: Boolean;
  vRedo: TRedoItem;
  vRedoPos: Integer;
begin
  vExists := False;
  vRedoPos := 0;
  for i := 0 to fRedo.Count - 1 do
    if TRedoItem(fRedo[i]).Band = aBand then
    begin
      vRedoPos := TRedoItem(fRedo[i]).Pos;
      vExists := True;
      Break;
    end;

  if vExists then
    if TRedoItem(fRedo[fRedo.Count-1]).Band = aBand then
      Exit;

  begin
    vRedo := TRedoItem.Create;
    if vExists then
      vRedo.Pos := vRedoPos
    else
    begin
      BandFacade.Band := aBand;
      vRedo.Pos := BandFacade.BandStart;
    end;
    vRedo.Band := aBand;
    fRedo.Add(vRedo);
  end;
end;//AddRedoBand
//------------------------------------------------------------------------------
function TBandList.FindRedo(aBand: TDsBand): TRedoItem;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to fRedo.Count - 1 do
  if TRedoItem(fRedo[i]).Band = aBand then
  begin
    Result := TRedoItem(fRedo[i]);
    Break;
  end
end;
//------------------------------------------------------------------------------
function TBandList.FindLeftBand(aPos: Integer): TDsBand;
var
  i, vTempStart: Integer;
  vRedo: TRedoItem;
  vBandStart: Integer;
  vFacade: TBandFacade;
begin
  vFacade := TBandFacade(BandFacade.NewInstance);
  Result := nil;
  vTempStart := 0;
  for i := 0 to Count - 1 do
    if Items[i] <> fBand then
    begin
      vFacade.Band := Items[i];

      vRedo := FindRedo(Items[i]);
      if vRedo = nil then
        vBandStart := vFacade.BandStart
      else
        vBandStart := vRedo.Pos;

      if (aPos > vBandStart) then
      begin
        if Result = nil then
        begin
          Result := Items[i];
          vTempStart := vBandStart;
        end
        else
        begin
          if vBandStart > vTempStart then
          begin
            Result := Items[i];
            vTempStart := vBandStart;
          end;
        end;
      end
    end;
end;
//------------------------------------------------------------------------------
function TBandList.FindRightBand(aPos: Integer): TDsBand;
var
  i, vTempStart: Integer;
  vRedo: TRedoItem;
  vBandStart: Integer;
  vFacade: TBandFacade;
begin
  vFacade := TBandFacade(BandFacade.NewInstance);
  Result := nil;
  vTempStart := 0;
  for i := 0 to Count - 1 do
    if Items[i] <> fBand then
    begin
      vFacade.Band := Items[i];
      vRedo := FindRedo(Items[i]);
      if vRedo = nil then
        vBandStart := vFacade.BandStart
      else
        vBandStart := vRedo.Pos;

      if (aPos <= vBandStart) then
      begin
        if Result = nil then
        begin
          Result := Items[i];
          vTempStart := vBandStart;
        end
        else
        begin
          if vBandStart < vTempStart then
          begin
            Result := Items[i];
            vTempStart := vBandStart;
          end;
        end;
      end;
    end;
end;
//------------------------------------------------------------------------------
function TBandList.FindLeftBandCur(aBand: TDsBand): TDsBand;
var
  i, vBandStart, vTempStart: Integer;
begin
  Result := nil;
  BandFacade.Band := aBand;
  vBandStart := BandFacade.BandStart;
  vTempStart := 0;
  for i := 0 to Count - 1 do
    if Items[i] <> aBand then
    begin
      BandFacade.Band := Items[i];
      if (vBandStart >= BandFacade.PosAfter) then
      begin
        if Result = nil then
        begin
          Result := Items[i];
          vTempStart := BandFacade.BandStart;
        end
        else
        begin
          if BandFacade.BandStart > vTempStart then
          begin
            Result := Items[i];
            vTempStart := BandFacade.BandStart;
          end;
        end;
      end
    end;
end;//FindLeftBandCur
//------------------------------------------------------------------------------
// Проверяваме ако го сложим на желаната позиция дали няма да се застъпи с друг
// Band или със стената на контейнера.
// Ако се застъпи го слагаме пред Band-а с който се застъпва. Проверяваме
// пак дали се застъпва с друг Band и така докато не открием място. Ако не открием
// място връщаме сегашното място
function TBandList.ClosestFreePlace(aBand: TDsBand; aPos: Integer): Integer;
var
  vBand: TDsBand;
  vBandStart, vBandLength: Integer;

  function FindRightBandCur: TDsBand;
  var
    i, vTempStart: Integer;
  begin
    Result := nil;
    vTempStart := 0;
    for i := 0 to Count - 1 do
      if Items[i] <> aBand then
      begin
        BandFacade.Band := Items[i];
        if (vBandStart + vBandLength <= BandFacade.BandStart) then
        begin
          if Result = nil then
          begin
            Result := Items[i];
            vTempStart := BandFacade.BandStart;
          end
          else
          begin
            if BandFacade.BandStart < vTempStart then
            begin
              Result := Items[i];
              vTempStart := BandFacade.BandStart;
            end;
          end;
        end;
      end;
  end;

begin
  BandFacade.Band := aBand;
  vBandStart := BandFacade.BandStart;
  vBandLength := BandFacade.BandLength;

  Result := aPos;
  if vBandStart = aPos then // Already on the original position
    Result := aPos
  else
  if vBandStart > aPos then // Move left to original position
  begin
    vBand := FindLeftBandCur(aBand);
    if vBand = nil then
      Result := aPos
    else
    begin
      BandFacade.Band := vBand;
      if (aPos <= BandFacade.PosAfter) then
        Result := BandFacade.PosAfter
      else
        Result := aPos;
    end;
  end
  else
  if vBandStart < aPos then // Move right to original position
  begin
    vBand := FindRightBandCur;
    if vBand = nil then
      Result := aPos
    else
    begin
      BandFacade.Band := vBand;
      if aPos + vBandLength >= BandFacade.BandStart then
        Result := BandFacade.BandStart - vBandLength
      else
        Result := aPos;
    end;
  end
end;//ClosestFreePlace
//------------------------------------------------------------------------------
procedure TBandList.RedoBands;
var
  i, vX: Integer;
begin
  for i := fRedo.Count - 1 downto 0 do
  begin
    vX := ClosestFreePlace(TRedoItem(fRedo[i]).Band, TRedoItem(fRedo[i]).Pos);
    BandFacade.Band := TRedoItem(fRedo[i]).Band;
    if BandFacade.BandStart <> vX then
      BandFacade.MoveTo(vX);
  end;
end;//RedoBands
//------------------------------------------------------------------------------
function TBandList.getCount: integer;
begin
  Result := fItems.Count;
end;
//------------------------------------------------------------------------------
function TBandList.getBand(Idx: integer): TDsBand;
begin
  Result := TDsBand(fItems[Idx]);
end;
//------------------------------------------------------------------------------
procedure TBandList.setLength(const Value: Integer);
var
  i, vX: Integer;
  vTemp: Integer;
  vRight: TDsBand;
begin
//  if fLength = Value then Exit;


  fLength := Value;
//  Exit;
  // Find Most Right band
  vTemp := -1;
  vRight := nil;
  for i := 0 to Count - 1 do
  begin
    BandFacade.Band := Items[i];
    if BandFacade.BandStart > vTemp then
    begin
      vRight := Items[i];
      vTemp := BandFacade.BandStart;
    end;
  end;

  if vRight <> nil then
  begin
    BandFacade.Band := vRight;
    if BandFacade.PosAfter > Length then
    begin
      vX := BandFacade.PosAfter - Length;
      if PushBandLeft(vRight, vX) then;
//        RedoBands;
      fRedo.Clear;
    end;
  end;
end;//setWidth
//------------------------------------------------------------------------------
procedure TBandList.setTop(const Value: Integer);
var
  i: Integer;
begin
  fTop := Value;
  for i := 0 to Count - 1 do
  begin
    BandFacade.Band := Items[i];
    BandFacade.setTop(fTop);
  end;
end;
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
{ TBandFacade }
//------------------------------------------------------------------------------
function TBandFacade.PosAfter: Integer;
begin
  Result := BandStart + BandLength;
end;
//------------------------------------------------------------------------------
end.
