{================================================================================
Copyright (C) 1997-2002 Mills Enterprise

Unit     : rmColumns
Purpose  : Saves column information and details from a listview to the registry
Date     : 02-11-1999
Author   : Ryan J. Mills
Version  : 1.90
================================================================================}

unit rmColumns;

interface

{$I CompilerDefines.INC}

uses
    Classes, ComCtrls, dialogs;

type
  TrmColumnTypes = (ctString, ctDateTime, ctInteger, ctFloat);
  TrmListColumns = class;

  TrmListColumn = class(TCollectionItem)
  private
    FAlignment: TAlignment;
    FAutoSize: Boolean;
    FCaption: string;
    FMaxWidth: TWidth;
    FMinWidth: TWidth;
    FImageIndex: Integer;
    FWidth: TWidth;
    FColumnType:TrmColumnTypes;
    fVisible: Boolean;
    FTag: integer;
    function IsWidthStored: Boolean;
    procedure SetAlignment(Value: TAlignment);
    procedure SetAutoSize(Value: Boolean);
    procedure SetCaption(const Value: string);
    procedure SetImageIndex(Value: Integer);
    procedure SetMaxWidth(Value: TWidth);
    procedure SetMinWidth(Value: TWidth);
    procedure SetWidth(Value: TWidth);
    function GetWidth:TWidth;
    procedure SetVisible(const Value: Boolean);
    procedure SetColumnIndex(const Value: integer);
    function GetColumnIndex: integer;
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
  published
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property AutoSize: Boolean read FAutoSize write SetAutoSize default False;
    property Caption: string read FCaption write SetCaption;
    property ColumnType : TrmColumnTypes read fColumnType write fColumnType;
    property ImageIndex: Integer read FImageIndex write SetImageIndex default -1;
    property MaxWidth: TWidth read FMaxWidth write SetMaxWidth default 0;
    property MinWidth: TWidth read FMinWidth write SetMinWidth default 0;
    property Width: TWidth read GetWidth write SetWidth stored IsWidthStored default 50;
    property Visible: Boolean read fVisible write SetVisible default true;
    property Tag: integer read FTag write fTag default -1;
    property ColumnIndex: integer read GetColumnIndex write SetColumnIndex;
  end;

  TrmListColumns = class(TCollection)
  private
    FOwner : TComponent;
    function GetItem(Index: Integer): TrmListColumn;
    procedure SetItem(Index: Integer; Value: TrmListColumn);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner:TComponent);
    function Add: TrmListColumn;
    property Items[Index: Integer]: TrmListColumn read GetItem write SetItem; default;
  end;

  TrmColumns = class(TComponent)
  private
    { Private declarations }
    FVersionID : Integer;
    FColumns : TrmListColumns;
    FSortColumn : integer;
    FSortDsc : boolean;
    procedure SetListColumns(Value: TrmListColumns);
    procedure SetSortColumn(const Value: integer);
    procedure SetSortDsc(const Value: boolean);
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AOwner:TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure SaveToFile(FileName:string; Binary:Boolean);
    procedure LoadFromFile(FileName:String; Binary:Boolean);
    procedure SaveToReg(Key, Value:string; Binary:Boolean);
    procedure LoadFromReg(key, Value:string; Binary:Boolean);
    procedure SetListViewCols(lvObject:TListView);
    procedure GetListViewCols(lvObject:TListView);
  published
    { Published declarations }
    property SortColumn : integer read fSortColumn write SetSortColumn;
    property SortDsc : boolean read FSortDsc write SetSortDsc;
    property Columns: TrmListColumns read FColumns write SetListColumns;
    property VersionID:integer read FVersionID write FVersionID;
  end;


implementation

Uses SysUtils,  Registry;

{ TrmListColumn }

constructor TrmListColumn.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FWidth := 50;
  FAlignment := taLeftJustify;
  FImageIndex := -1;
  fVisible := true;
  FTag := -1;
end;

procedure TrmListColumn.SetCaption(const Value: string);
begin
  if FCaption <> Value then
  begin
    FCaption := Value;
  end;
end;

function TrmListColumn.GetWidth: TWidth;
begin
  Result := FWidth;
end;

function TrmListColumn.IsWidthStored: Boolean;
begin
  Result := not FAutoSize;
end;

procedure TrmListColumn.SetWidth(Value: TWidth);
begin
  if FWidth <> Value then
  begin
    FWidth := Value;
  end;
end;

procedure TrmListColumn.SetAlignment(Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
  end;
end;

procedure TrmListColumn.SetAutoSize(Value: Boolean);
begin
  if FAutoSize <> Value then
  begin
    FAutoSize := Value;
  end;
end;

procedure TrmListColumn.SetImageIndex(Value: Integer);
begin
  if FImageIndex <> Value then
  begin
    FImageIndex := Value;
  end;
end;

procedure TrmListColumn.SetMaxWidth(Value: TWidth);
begin
  if FMaxWidth <> Value then
  begin
    FMaxWidth := Value;
  end;
end;

procedure TrmListColumn.SetMinWidth(Value: TWidth);
begin
  if FMinWidth <> Value then
  begin
    FMinWidth := Value;
  end;
end;

procedure TrmListColumn.Assign(Source: TPersistent);
var
  Column: TrmListColumn;
begin
  if Source is TrmListColumn then
  begin
    Column := TrmListColumn(Source);
    Alignment := Column.Alignment;
    AutoSize := Column.AutoSize;
    Caption := Column.Caption;
    ImageIndex := Column.ImageIndex;
    MaxWidth := Column.MaxWidth;
    MinWidth := Column.MinWidth;
    Width := Column.Width;
    ColumnType := Column.ColumnType;
    Tag := Column.Tag;
  end
  else inherited Assign(Source);
end;

function TrmListColumn.GetDisplayName: string;
begin
  Result := Caption;
  if Result = '' then Result := inherited GetDisplayName;
end;

procedure TrmListColumn.SetVisible(const Value: Boolean);
begin
  if fVisible <> value then
  begin
     fVisible := Value;
  end;
end;

procedure TrmListColumn.SetColumnIndex(const Value: integer);
begin
   Self.Index := value;
end;

function TrmListColumn.GetColumnIndex: integer;
begin
   result := Self.Index;
end;

{ TrmListColumns }

function TrmListColumns.GetItem(Index: Integer): TrmListColumn;
begin
  Result := TrmListColumn(inherited GetItem(Index));
end;

procedure TrmListColumns.SetItem(Index: Integer; Value: TrmListColumn);
begin
  inherited SetItem(Index, Value);
end;

function TrmListColumns.Add: TrmListColumn;
begin
  Result := TrmListColumn(inherited Add);
end;

function TrmListColumns.GetOwner: TPersistent;
begin
     Result := FOwner;
end;

constructor TrmListColumns.Create(AOwner: TComponent);
begin
     inherited Create(TrmListColumn);
     FOwner := AOwner;
end;

{ TrmColumns }

constructor TrmColumns.Create(AOwner: TComponent);
begin
     inherited Create(AOwner);
     FColumns := TrmListColumns.create(self);
end;

destructor TrmColumns.Destroy;
begin
     FColumns.free;
     inherited;
end;

procedure TrmColumns.SetListViewCols(lvObject: TListView);
var
   index : integer;
begin
     lvObject.Columns.BeginUpdate;
     try
        lvObject.Columns.Clear;
        index := 0;
        While index < Columns.Count do
        begin
             if Columns[index].visible then
             with lvObject.Columns.Add do
             begin
                  Alignment := Columns[index].Alignment;
                  AutoSize := Columns[index].AutoSize;
                  Caption := Columns[index].Caption;
                  ImageIndex := Columns[index].ImageIndex;
                  MaxWidth := Columns[index].MaxWidth;
                  MinWidth := Columns[index].MinWidth;
                  Width := Columns[index].Width;
                  Tag := Columns[index].Tag;
             end;
             inc(index);
        end;
     finally
        lvObject.Columns.EndUpdate;
     end;
end;

procedure TrmColumns.GetListViewCols(lvObject: TListView);
var
   index : integer;
begin
     Columns.Clear;
     index := 0;
     While index < lvObject.Columns.Count do
     begin
          Columns.Add;
          with Columns[index] do
          begin
               Alignment := lvObject.Columns[index].Alignment;
               AutoSize := lvObject.Columns[index].AutoSize;
               Caption := lvObject.Columns[index].Caption;
               ImageIndex := lvObject.Columns[index].ImageIndex;
               MaxWidth := lvObject.Columns[index].MaxWidth;
               MinWidth := lvObject.Columns[index].MinWidth;
               Width := lvObject.Columns[index].Width;
               Tag := lvObject.Columns[index].Tag;
          end;
          inc(index);
     end;
end;

procedure TrmColumns.LoadFromFile(FileName: String; Binary: Boolean);
var
   StrmIn, TempStrm : TStream;
   TmpCols : TrmColumns;
begin
     TmpCols := TrmColumns.Create(nil);
     try
        StrmIn := TFileStream.Create(fileName,fmOpenRead);
        try
           if Binary then
              StrmIn.ReadComponent(TmpCols)
           else
           begin
                TempStrm := TMemoryStream.Create;
                try
                   ObjectTextToBinary(StrmIn,TempStrm);
                   TempStrm.Position := 0;
                   TempStrm.ReadComponent(TmpCols);
                finally
                   TempStrm.Free;
                end;
           end;
        finally
           StrmIn.Free;
        end;
        Self.Assign(TmpCols);
     finally
        TmpCols.free;
     end;
end;

procedure TrmColumns.LoadFromReg(key, value: string; Binary: Boolean);
var
   StrmIn, TempStrm : TStream;
   TmpCols : TrmColumns;
   Reg : TRegistry;
   Buf : Pointer;
   BufSize : integer;
begin
     BufSize := -1;
     StrmIn := TMemoryStream.Create;
     try
        Reg := TRegistry.Create;
        try
           if reg.OpenKey(key,false) then
           begin
                if Reg.ValueExists(Value) then
                begin
                     BufSize := Reg.GetDataSize(Value);
                     if BufSize > -1 then
                     begin
                          GetMem(Buf,BufSize);
                          try
                             Reg.ReadBinaryData(Value,Buf^,BufSize);
                             StrmIn.WriteBuffer(Buf^,BufSize);
                          finally
                             FreeMem(Buf,BufSize);
                          end;
                     end;
                end;
                Reg.CloseKey;
           end;
        finally
           Reg.CloseKey;
           Reg.free;
        end;

        if BufSize > -1 then
        begin
             StrmIn.Position := 0;
             TmpCols := TrmColumns.Create(nil);
             try
                if Binary then
                   StrmIn.ReadComponent(TmpCols)
                else
                begin
                     TempStrm := TMemoryStream.Create;
                     try
                        ObjectTextToBinary(StrmIn,TempStrm);
                        TempStrm.Position := 0;
                        TempStrm.ReadComponent(TmpCols);
                     finally
                        TempStrm.Free;
                     end;
                end;
                Self.Assign(TmpCols);
             finally
                TmpCols.free;
             end;
        end;
     finally
        StrmIn.Free;
     end;
end;

procedure TrmColumns.SaveToFile(FileName: string; Binary: Boolean);
var
   StrmOut, TempStrm : TStream;
   Name : string;
begin
     Name := Self.Name;
     Self.Name := '';
     StrmOut := TFileStream.Create(fileName,fmCreate);
     try
        if Binary then
           StrmOut.WriteComponent(Self)
        else
        begin
             TempStrm := TMemoryStream.Create;
             try
                TempStrm.WriteComponent(Self);
                TempStrm.Position := 0;
                ObjectBinaryToText(TempStrm,StrmOut);
             finally
                TempStrm.Free;
             end;
        end;
     finally
        StrmOut.Free;
     end;
     Self.Name := Name;
end;

procedure TrmColumns.SaveToReg(Key, Value: string; Binary: Boolean);
var
   StrmOut, TempStrm : TStream;
   Name : string;
   reg : TRegistry;
   Buf : pointer;
begin
     Name := Self.Name;
     Self.Name := '';
     StrmOut := TMemoryStream.Create;
     try
        if Binary then
           StrmOut.WriteComponent(Self)
        else
        begin
             TempStrm := TMemoryStream.Create;
             try
                TempStrm.WriteComponent(Self);
                TempStrm.Position := 0;
                ObjectBinaryToText(TempStrm,StrmOut);
             finally
                TempStrm.Free;
             end;
        end;
        Reg := TRegistry.Create;
        try
           GetMem(buf,StrmOut.Size);
           try
              StrmOut.Position := 0;
              StrmOut.ReadBuffer(Buf^,StrmOut.Size);
              if reg.OpenKey(key,true) then
              begin
                   Reg.WriteBinaryData(Value,Buf^,StrmOut.Size);
                   Reg.CloseKey;
              end;
           finally
              FreeMem(Buf,StrmOut.Size);
           end;
        finally
           Reg.CloseKey;
           Reg.free;
        end;
     finally
        StrmOut.Free;
     end;
     Self.Name := Name;
end;

procedure TrmColumns.SetListColumns(Value: TrmListColumns);
begin
  FColumns.Assign(Value);
end;

procedure TrmColumns.Assign(Source: TPersistent);
begin
     if source is TrmColumns then
     begin
          VersionID := TrmColumns(Source).VersionID;
          SortColumn := TrmColumns(Source).SortColumn;
          SortDsc := TrmColumns(Source).SortDsc;
          Columns.assign(TrmColumns(Source).Columns);
     end
     else
     inherited assign(source);
end;

procedure TrmColumns.SetSortColumn(const Value: integer);
begin
  fSortColumn := Value;
end;

procedure TrmColumns.SetSortDsc(const Value: boolean);
begin
  FSortDsc := Value;
end;

end.
