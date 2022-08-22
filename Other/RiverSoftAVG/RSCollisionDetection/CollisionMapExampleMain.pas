unit CollisionMapExampleMain;
//=== File Prolog ============================================================
//	This code was developed by RiverSoftAVG.
//
//--- Notes ------------------------------------------------------------------
//
//--- Development History  ---------------------------------------------------
//
//	  03/2004	T. Grubb
//		        Initial version.
//
//      File Contents:
//           Simple Collision Map Example.  See Readme
//
//  © 2004, RiverSoftAVG.com
//
//=== End File Prolog ========================================================

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, RSCollisionMap, ExtCtrls, ComCtrls, StdCtrls, Types;

type
  TfrmExample = class;
  TGameObjects = class;
  TGameObject = class(TCollectionItem)
  { Purpose: }
  private
    { Private declarations }
    FName: String;
    FBoundsRect: TRect;
    FColor: TColor;
    function GetCollection: TGameObjects;
    procedure SetCollection(const Value: TGameObjects); reintroduce;
    procedure SetName(const Value: String);
    procedure SetBoundsRect(const Value: TRect);
    procedure SetColor(const Value: TColor);
    function GetGOOwner: TfrmExample;
  protected
    { Protected declarations }
    function GetDisplayName: String; override;
  public
    { Public declarations }
    procedure Assign(Source: TPersistent); override;
    procedure Draw(const Canvas: TCanvas); virtual;
    procedure Initialize; virtual;
    property Collection: TGameObjects read GetCollection write SetCollection;
    property Owner: TfrmExample read GetGOOwner;
  published
    { Published declarations }
    property Name: String read FName write SetName;
    property BoundsRect: TRect read FBoundsRect write SetBoundsRect;
    property Color: TColor read FColor write SetColor;
  end; { TGameObject }

  TGameObjects = class(TOwnedCollection)
  { Purpose: }
  private
    { Private declarations }
    function GetItem(Index: Integer): TGameObject;
    procedure SetItem(Index: Integer; const Value: TGameObject);
  protected
    { Protected declarations }
    procedure Notify(Item: TCollectionItem;
      Action: TCollectionNotification); override;
    procedure Update(Item: TCollectionItem); override;
  public
    { Public declarations }
    constructor Create(AOwner: TfrmExample);
    function Add: TGameObject;
    function FindItemID(ID: Integer): TGameObject;
    function Insert(Index: Integer): TGameObject;
    property Items[Index: Integer]: TGameObject read GetItem write SetItem; default;
    function Owner: TfrmExample; reintroduce;
  published
    { Published declarations }
  end; { TGameObjects }

  TfrmExample = class(TForm)
    Image1: TImage;
    Panel1: TPanel;
    Label1: TLabel;
    tbNumObjects: TTrackBar;
    PaintBox1: TPaintBox;
    Panel2: TPanel;
    Timer1: TTimer;
    Memo1: TMemo;
    rgColCheck: TRadioGroup;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure tbNumObjectsChange(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    FCollisionMap: TRSCollisionMap;
    FGameObjects: TGameObjects;
    procedure BoundsRectChanging(Item: TGameObject); dynamic;
    procedure BoundsRectChanged(Item: TGameObject); dynamic;
    procedure GONotify(Item: TGameObject;
      Action: TCollectionNotification);
    procedure GOUpdate(Item: TGameObject);
    procedure SetGameObjects(const Value: TGameObjects);
  public
    { Public declarations }
    property CollisionMap: TRSCollisionMap read FCollisionMap;
    property GameObjects: TGameObjects read FGameObjects write SetGameObjects;
  end;

var
  frmExample: TfrmExample;

implementation

uses
    Math;

{$R *.dfm}

{ TGameObject }

procedure TGameObject.Assign(Source: TPersistent);
begin
     if Source is TGameObject then
     begin
          // Copy properties here
          FName := TGameObject(Source).Name;
          FBoundsRect := TGameObject(Source).BoundsRect;
          FColor := TGameObject(Source).Color;
          Changed(False);
     end
     else
         inherited Assign(Source);
end;

function TGameObject.GetCollection: TGameObjects;
begin
     result := TGameObjects(inherited Collection);
end;

procedure TGameObject.SetCollection(const Value: TGameObjects);
begin
     inherited Collection := Value;
end;

function TGameObject.GetDisplayName: String;
begin
     result := Name;
     if result = '' then
        result := inherited GetDisplayName;
end;

procedure TGameObject.SetName(const Value: String);
begin
     if Value <> Name then
     begin
          FName := Value;
          Changed(False);
     end;
end;

procedure TGameObject.SetBoundsRect(const Value: TRect);
begin
     if Owner <> nil then
        Owner.BoundsRectChanging(Self);
     FBoundsRect := Value;
     if Owner <> nil then
        Owner.BoundsRectChanged(Self);
end;

procedure TGameObject.SetColor(const Value: TColor);
begin
     if Value <> Color then
     begin
          FColor := Value;
          Changed(False);
     end;
end;

procedure TGameObject.Initialize;
var
   Left, Top: Integer;
begin
     if Owner = nil then Exit;
     FColor := TColor(Random(High(Integer)));
     Left := Random(Owner.Image1.Width);
     Top := Random(Owner.Image1.Height);

     Self.BoundsRect := Rect(Left, Top, Left+Random(40)+10, Top+Random(40)+10);
end;

procedure TGameObjects.Notify(Item: TCollectionItem;
  Action: TCollectionNotification);
begin
     inherited Notify(Item, Action);
     if Owner <> nil then
        Owner.GONotify(TGameObject(Item), Action);
end;

procedure TGameObjects.Update(Item: TCollectionItem);
begin
     inherited Update(Item);
     if Owner <> nil then
        Owner.GOUpdate(TGameObject(Item));
end;

function TGameObject.GetGOOwner: TfrmExample;
begin
     if Collection <> nil then
        result := Collection.Owner
     else
         result := nil;
end;

procedure TGameObject.Draw(const Canvas: TCanvas);
begin
     Canvas.Brush.Color := Color;
     if Index = 0 then
     begin
         case Owner.rgColCheck.ItemIndex of
              1: {circle}          Canvas.Ellipse(BoundsRect);
         else
             Canvas.FillRect(BoundsRect);
         end;
     end
     else
         Canvas.FillRect(BoundsRect);
end;

{ TGameObjects }

function TGameObjects.Add: TGameObject;
begin
     result := TGameObject(inherited Add);
end;

function TGameObjects.FindItemID(ID: Integer): TGameObject;
begin
     result := TGameObject(inherited FindItemID(ID));
end;

function TGameObjects.GetItem(Index: Integer): TGameObject;
begin
     result := TGameObject(inherited Items[Index]);
end;

function TGameObjects.Owner: TfrmExample;
var
   AOwner: TPersistent;
begin
     AOwner := inherited Owner;
     if AOwner is TfrmExample then
        result := TfrmExample(AOwner)
     else
         result := nil;
end;

function TGameObjects.Insert(Index: Integer): TGameObject;
begin
     result := TGameObject(inherited Insert(Index));
end;

procedure TGameObjects.SetItem(Index: Integer; const Value: TGameObject);
begin
     inherited Items[Index] := Value;
end;

constructor TGameObjects.Create(AOwner: TfrmExample);
begin
     inherited Create(AOwner, TGameObject);
end;

procedure TfrmExample.FormCreate(Sender: TObject);
begin
     // Create the collision map
     FCollisionMap := TRSCollisionBitmap.Create;
     FGameObjects := TGameObjects.Create(Self);
end;

procedure TfrmExample.FormDestroy(Sender: TObject);
begin
     FCollisionMap.Free;
     FGameObjects.Free;
end;

procedure TfrmExample.GONotify(Item: TGameObject;
  Action: TCollectionNotification);
begin
     case Action of
          cnAdded:
          begin
          end;
          cnExtracting:
          begin
          end;
     end;
     Invalidate;
end;

procedure TfrmExample.SetGameObjects(const Value: TGameObjects);
begin
  FGameObjects.Assign( Value );
end;

procedure TfrmExample.GOUpdate(Item: TGameObject);
begin
     Invalidate;
end;

procedure TfrmExample.tbNumObjectsChange(Sender: TObject);
begin
     with Sender as TTrackBar do
     begin
       GameObjects.BeginUpdate;
       try
          while Position <> GameObjects.Count do
          begin
               if Position < GameObjects.Count then
                  GameObjects.Delete(GameObjects.Count-1)
               else if Position > GameObjects.Count then
                    GameObjects.Add.Initialize;
          end;
       finally
          GameObjects.EndUpdate;
       end;
     end;
end;

procedure TfrmExample.BoundsRectChanged(Item: TGameObject);
begin
     // fill with plus 1 since collections are 0 based
     // (also could use pointers here)
     // we are not going to put the first item in here, that will be the
     // players
     if Item.Index > 0 then
     begin
          CollisionMap.FillRect(Item.BoundsRect, Item.Index + 1);
     end;
     Invalidate;
end;

procedure TfrmExample.BoundsRectChanging(Item: TGameObject);
begin
     // remove old rect from collision map
     if Item.Index > 0 then
     begin
          CollisionMap.Clear(Item.BoundsRect);
     end;
end;

procedure TfrmExample.PaintBox1Paint(Sender: TObject);
var
   i: Integer;
begin
     with Sender as TPaintBox do
     for i := 0 to GameObjects.Count - 1 do
         GameObjects[i].Draw(Canvas);
end;

procedure TfrmExample.Timer1Timer(Sender: TObject);
var
   ARect: TRect;
   Move: TPoint;
   IsCol: Boolean;
begin
     if GameObjects.Count = 0 then Exit;
     // move object 0 towards user
     with PaintBox1.ScreenToClient(Mouse.CursorPos) do
          Move := Point(Sign(X-GameObjects[0].BoundsRect.Left), Sign(Y-GameObjects[0].BoundsRect.Top));
     ARect := GameObjects[0].BoundsRect;
     OffsetRect(ARect, Move.X, Move.Y);
     case rgColCheck.ItemIndex of
          0: {corners}         IsCol := CollisionMap.IsCollision(ARect);
          1: {circle}          IsCol := CollisionMap.IsCircleCollision(ARect, Move);
          2: {center point}    IsCol := CollisionMap.IsCollision(CenterPoint(ARect));
          3: {full rect}       IsCol := CollisionMap.IsCollision(ARect, False);
     else
         IsCol := False;
     end;
     if not IsCol then
     begin
          GameObjects[0].BoundsRect := ARect;
          PaintBox1.Repaint;
     end;
end;

procedure TfrmExample.FormResize(Sender: TObject);
var
   i: Integer;
begin
     // make sure to resize collision bitmap
     CollisionMap.Height := PaintBox1.Height;
     CollisionMap.Width := PaintBox1.Width;
     // update the collision rectangles too
     for i := 1 to GameObjects.Count - 1 do
         CollisionMap.FillRect(GameObjects[i].BoundsRect, i+1);
end;

procedure TfrmExample.FormShow(Sender: TObject);
begin
     // initialize some objects
     if (GameObjects.Count = 0) and Assigned(tbNumObjects.OnChange) then
     begin
          // add player's object right in middle
          with GameObjects.Add do
          begin
               Color := clRed;
               BoundsRect := Rect(200, 200, 225, 225);
          end;
          // make the rest
          tbNumObjects.OnChange(tbNumObjects);
     end;
end;

end.

