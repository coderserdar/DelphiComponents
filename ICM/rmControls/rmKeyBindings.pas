{================================================================================
Copyright (C) 1997-2002 Mills Enterprise

Unit     : rmKeyBindings
Purpose  : To allow the end user to assign or change the hot keys assigned to
           actions in an action list.
Date     : 05-03-2000
Author   : Ryan J. Mills
Version  : 1.90
================================================================================}

unit rmKeyBindings;

interface

{$I CompilerDefines.INC}

uses classes, ActnList;

type
  TrmKeyBindingItem = class(TCollectionItem)
  private
   { Private }
    fDesignLocked : boolean;
    fCategory : string;
    fActionCaption : string;
    fActionName : string;
    fShortCut : TShortCut;
    fDescription : string;
    fImageIndex : integer;
    procedure SetShortcut(const Value: TShortCut);
    procedure setDesignLocked(const Value: boolean);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property ActionName : string read fActionName write fACtionName;
    property ActionCaption : string read fActionCaption write fActionCaption;
    property Category : string read fCategory write fCategory;
    property Description : String read fDescription write fDescription;
    property ImageIndex : integer read fImageIndex write fImageIndex default -1;
    property KeyBinding : TShortCut read fShortCut write SetShortcut default scNone;
    property DesignLocked : boolean read fDesignLocked write setDesignLocked default false;
  end;

  TrmKeyBindingCollection = class(TCollection)
  private
   { Private }
    FOwner: TPersistent;
    function GetItem(Index: Integer): TrmKeyBindingItem;
    procedure SetItem(Index: Integer; Value: TrmKeyBindingItem);
  protected
   { Protected }
    function GetOwner: TPersistent; override;
  public
   { Public }
    constructor Create(AOwner: TPersistent);
    function Add: TrmKeyBindingItem;
    property Items[Index: Integer]: TrmKeyBindingItem read GetItem write SetItem; default;
  end;

  TrmBindingStorage = class(TComponent)
  private
     fItems: TrmKeyBindingCollection;
     procedure SetItem(const Value: TrmKeyBindingCollection);
  public
     constructor create(AOwner:TComponent); override;
     destructor destroy; override;
  published
     property Items: TrmKeyBindingCollection read fItems write SetItem;
  end;

  TrmKeyBindings = class(TComponent)
  private
   { Private }
    fActions : TCustomActionList;
    fItems : TrmKeyBindingCollection;
    fDisplayName: boolean;
    fMultiBinds: boolean;
  protected
   { Protected }
    procedure SetActionList(const Value: TCustomActionList); Virtual;
  public
   { Public }
    constructor create(AOwner:TComponent); override;
    destructor destroy; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    function EditBindings:boolean;
    procedure ApplyBindings;
    procedure ClearBindings;

    procedure LoadBindingsFromFile(fileName:string; Binary:Boolean);
    procedure LoadBindingsFromStream(Strm:TStream; Binary:Boolean);
    procedure SaveBindingsToFile(FileName:string; Binary:Boolean);
    procedure SaveBindingsToStream(Strm:TStream; Binary:Boolean);
  published
    { Published }
    property Actions : TCustomActionList read fActions write SetActionList;
    property DisplayActionName:boolean read fDisplayName write fDisplayName default false;
    property AllowMultiBinds:boolean read fMultiBinds write fMultiBinds default true;
  end;

implementation

uses Forms, Controls, sysutils, rmKeyBindingsEditForm;

{ TrmKeyBindings }

procedure TrmKeyBindings.ApplyBindings;
var
   loop, loop1 : integer;
   wAction : TCustomAction;
   wCursor : TCursor;
begin
   wCursor := screen.cursor;
   try
      screen.Cursor := crHourGlass;
      for loop := 0 to fItems.Count-1 do
      begin
         wAction := TCustomAction(fActions.Actions[loop]);
         if wAction.Name = fItems[loop].ActionName then
         begin
            wAction.shortcut := fItems[loop].KeyBinding;
            fActions.UpdateAction(wAction);
         end
         else
         begin
            for loop1 := 0 to fActions.ActionCount-1 do
            begin
               wAction := TCustomAction(fActions.Actions[loop1]);
               if wAction.Name = fItems[loop].ActionName then
               begin
                  wAction.shortcut := fItems[loop].KeyBinding;
                  fActions.UpdateAction(wAction);
                  break;
               end;
            end;
         end
      end;
   finally
      screen.cursor := wCursor;
   end;
end;

procedure TrmKeyBindings.ClearBindings;
begin
   fItems.Clear;
end;

constructor TrmKeyBindings.create(AOwner: TComponent);
begin
  inherited;
  fItems := TrmKeyBindingCollection.create(self);
  fDisplayName := false;
  fMultiBinds := true;
end;

destructor TrmKeyBindings.destroy;
begin
  fItems.Clear;   
  fItems.free;   
  inherited;
end;

function TrmKeyBindings.EditBindings:boolean;
var
   frmEditor : TFrmEditKeyBindings;
begin
   frmEditor := TFrmEditKeyBindings.create(nil);
   try
      if assigned(Actions) and assigned(Actions.images) then
         frmEditor.images := Actions.images;
      frmEditor.Items := Self.fItems;
      frmEditor.DisplayName := fDisplayName;
      frmEditor.MultiBinding := fMultiBinds;
      frmEditor.Designing := (csDesigning in ComponentState);
      if (frmEditor.ShowModal = mrOK) then
      begin
         result := true;
         Self.fItems.assign(frmEditor.Items);
      end
      else
        result := false;
   finally
      frmEditor.free;
   end;
end;

procedure TrmKeyBindings.LoadBindingsFromFile(fileName: string; Binary:Boolean);
var
   wFile : TFileStream;
begin
   if fileexists(filename) then
   begin
      wFile := TFileStream.create(filename, fmOpenRead);
      try
         LoadBindingsFromStream(wFile, Binary);
      finally
         wFile.free;
      end;
   end;
end;

procedure TrmKeyBindings.LoadBindingsFromStream(Strm: TStream; Binary:Boolean);
var
   wStorage : TComponent;
   wTemp : TMemoryStream;
begin
   Strm.Position := 0;

   if Binary then
      wStorage := TrmBindingStorage(Strm.ReadComponent(nil))
   else
   begin
      wTemp := TMemoryStream.create;
      try
         ObjectTextToBinary(Strm, wTemp);
         wTemp.position := 0;
         wStorage := TrmBindingStorage(wTemp.ReadComponent(nil));
      finally
         wTemp.free;
      end;
   end;

   try
      fItems.Assign(TrmBindingStorage(wStorage).items);
   finally
      wStorage.free;
   end;
   ApplyBindings;
end;

procedure TrmKeyBindings.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) then
  begin
       if (aComponent = fActions) then
          fActions := nil;
  end;

  inherited;
end;

procedure TrmKeyBindings.SaveBindingsToFile(FileName: string; Binary:Boolean);
var
   wFile : TFileStream;
   {$ifdef D5_or_higher}
   wAttr : integer;
   {$endif}
begin
   {$ifdef D5_or_higher}
   if fileexists(filename) then
   begin
      wAttr := filegetAttr(filename);
      if (wAttr and faReadonly <> 0) or (wAttr and faSysFile <> 0) then
         Raise Exception.create('Unable to open file for writing');
   end;
   {$endif}
   wFile := TFileStream.create(filename, fmCreate);
   try
      SaveBindingsToStream(wFile, Binary);
   finally
      wFile.free;
   end;
end;

procedure TrmKeyBindings.SaveBindingsToStream(Strm: TStream; Binary:Boolean);
var
   wStorage : TrmBindingStorage;
   wTemp : TMemoryStream;
begin
   wStorage := TrmBindingStorage.create(self);
   try
      Strm.Position := 0;
      wStorage.Items := fItems;
      if Binary then
         Strm.WriteComponent(wStorage)
      else
      begin
         wTemp := TMemoryStream.create;
         try
            wTemp.WriteComponent(wStorage);
            wTemp.Position := 0;
            ObjectBinaryToText(wTemp, Strm)
         finally
            wTemp.free;
         end;
      end;
   finally
      wStorage.free;
   end;
end;

procedure TrmKeyBindings.SetActionList(const Value: TCustomActionList);
var
   loop : integer;
   wAction : TCustomAction;
begin
   fActions := Value;
   if assigned(fActions) then
   begin
      fActions.FreeNotification(self);
      fItems.Clear;
      loop := 0;
      while loop < fActions.ActionCount do
      begin
         if fActions[loop] is TCustomAction then
         begin
            wAction := TCustomAction(factions[loop]);
            with fItems.Add do
            begin
               DesignLocked := false;
               ActionCaption := wAction.Caption;
               ActionName := wAction.Name;
               Category := wAction.Category;
               KeyBinding := wAction.Shortcut;
               ImageIndex := wAction.ImageIndex;
               Description := wAction.Hint;
            end;
         end;
         inc(loop);
      end;
   end;
end;

{ TrmKeyBindingItem }

procedure TrmKeyBindingItem.Assign(Source: TPersistent);
begin
  if Source is TrmKeyBindingItem then
  begin
    fActionCaption := TrmKeyBindingItem(Source).ActionCaption;
    fActionName := TrmKeyBindingItem(Source).ActionName;
    fCategory := TrmKeyBindingItem(Source).Category;
    fDesignLocked := TrmKeyBindingItem(Source).DesignLocked;
    fShortCut := TrmKeyBindingItem(Source).KeyBinding;
    fDescription := TrmKeyBindingItem(Source).Description;
    fImageIndex := TrmKeyBindingItem(Source).ImageIndex;
  end
  else
  inherited Assign(Source);
end;

constructor TrmKeyBindingItem.Create(Collection: TCollection);
begin
  inherited;
  fShortCut := scNone;
  fActionCaption := '';
  fActionName := '';
  fCategory := '';
  fDesignLocked := false;
  fDescription := '';
  fImageIndex := -1;
end;

destructor TrmKeyBindingItem.Destroy;
begin
  inherited;
end;

procedure TrmKeyBindingItem.setDesignLocked(const Value: boolean);
begin
  fDesignLocked := Value;
end;

procedure TrmKeyBindingItem.SetShortcut(const Value: TShortCut);
begin
  fShortCut := Value;
end;

{ TrmKeyBindingCollection }

function TrmKeyBindingCollection.Add: TrmKeyBindingItem;
begin
  Result := TrmKeyBindingItem(inherited Add);
end;

constructor TrmKeyBindingCollection.Create(AOwner: TPersistent);
begin
  inherited Create(TrmKeyBindingItem);
  fOwner := AOwner;
end;

function TrmKeyBindingCollection.GetItem(Index: Integer): TrmKeyBindingItem;
begin
  Result := TrmKeyBindingItem(inherited GetItem(Index));
end;

function TrmKeyBindingCollection.GetOwner: TPersistent;
begin
   Result := FOwner;
end;

procedure TrmKeyBindingCollection.SetItem(Index: Integer; Value: TrmKeyBindingItem);
begin
  inherited SetItem(Index, Value);
end;

{ TrmBindingStorage }

constructor TrmBindingStorage.create(AOwner: TComponent);
begin
  inherited;
  fItems := TrmKeyBindingCollection.Create(self);
end;

destructor TrmBindingStorage.destroy;
begin
  fItems.Clear;
  fItems.Free;
  inherited;
end;

procedure TrmBindingStorage.SetItem(const Value: TrmKeyBindingCollection);
begin
   fItems.Assign(Value);
end;

end.
