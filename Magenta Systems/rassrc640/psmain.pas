unit psmain;

{$IFNDEF VER140}
  {$WARN UNSAFE_TYPE off}
  {$WARN UNSAFE_CAST off}
  {$WARN UNSAFE_CODE off}
{$ENDIF}

// http://www.cobans.net/pslib.php

{ Last updated: 27th August 2008

This application demos the Protected Storage functions from www.cobans.net

The Protected Storage service is used in Windows to store confidential
information in a secure, encrypted manner. Data like private keys,
Outlook Express mail and news passwords is all stored in the Protected Storage.
Using some undocumented libraries, it is possible to read this data,
PStorageLib also gives you a posibility to write your own data into
Protected Storage. Encryption is not a problem since the same undocumented
functions used decrypt the data for you themselves.

Note RAS passwords don't use Protected Storage

See also http://www.nirsoft.net/ for password reading tools, there is
Protected Storage tool that displays the same passwords, but
also the Outlook Express logons and account type from the registry.

}

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Forms, StdCtrls,
  PStorage, ExtCtrls, ComCtrls, Menus, ActiveX, ComObj;

const
  PSTORAGE_TYPE         = 0;
  PSTORAGE_SUBTYPE      = 1;
  PSTORAGE_ITEM         = 2;

type
  TMainForm = class(TForm)
    LogMemo: TMemo;
    TreeView: TTreeView;
    Splitter1: TSplitter;
    PopupMenu1: TPopupMenu;
    UpdateItem: TMenuItem;
    DeleteItem: TMenuItem;
    MainMenu1: TMainMenu;
    FileItem: TMenuItem;
    ExitItem: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure UpdateItemClick(Sender: TObject);
    procedure TreeViewChange(Sender: TObject; Node: TTreeNode);
    procedure ExitItemClick(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure DeleteItemClick(Sender: TObject);
    procedure TreeViewKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    FStorage: TPStorage;
    FProviderNode: TTreeNode;
    procedure ExpandPSProvider(Node: TTreeNode);
    procedure FreeTreeViewItems;
    procedure FreePSProviderList(Node: TTreeNode);
  public
  end;

  PStorageType = ^TStorageType;
  TStorageType = record
    T: BYTE;
    pType: TGUID;
  end;

  PStorageSubtype = ^TStorageSubtype;
  TStorageSubtype = record
    T: BYTE;
    pType: TGUID;
    pSubtype: TGUID;
  end;

  PStorageItem = ^TStorageItem;
  TStorageItem = record
    T: BYTE;
    pType: TGUID;
    pSubtype: TGUID;
    pItem: ShortString;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

type
  PTChar = ^AnsiChar;  

function DumpData(Buffer: Pointer; BufLen: DWord): String;
var
  i, j, c: Integer;
begin
  c := 0;
  Result := '';
  for i := 1 to BufLen div 16 do begin
    for j := c to c + 15 do begin
      Result := Result + IntToHex(PByte(Integer(Buffer) + j)^, 2);
      if j = c + 7 then
        Result := Result + '-'
      else
        Result := Result + ' ';
    end;

    Result := Result + '  ';
    for j := c to c + 15 do
      if (PByte(Integer(Buffer) + j)^ < $20) or (PByte(Integer(Buffer) + j)^ > $7F) then
        Result := Result + '.'
      else
        Result := Result + PTChar(Integer(Buffer) + j)^;

    c := c + 16;
    Result := Result + #13#10;
  end;

  for i := c to BufLen-1 do begin
    Result := Result + IntToHex(PByte(Integer(Buffer) + i)^, 2);
    if (i = c + 7) and (LongWord(i) <> BufLen-1) then
      Result := Result + '-'
    else
      Result := Result + ' ';
  end;

  if BufLen mod 16 <> 0 then begin
    for i := 0 to 15 - (BufLen mod 16) do
      Result := Result + '   ';
    Result := Result + '  ';
    for i := BufLen mod 16 downto 1 do begin
      if (PByte(Integer(Buffer) + Integer(BufLen) - i)^ < $20) or (PByte(Integer(Buffer) + Integer(BufLen) - i)^ > $7F) then
        Result := Result + '.'
      else
        Result := Result + PTChar(Integer(Buffer) + Integer(BufLen) - i)^;
    end;
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FProviderNode := nil;

  FStorage := TPStorage.Create;
  if not FStorage.Initialized then begin
    LogMemo.Lines.Add('Could not initialize object...');
    Exit;
  end;
  LogMemo.Lines.Add('Object initialized...');

  if not FStorage.Connect(nil) then begin
    LogMemo.Lines.Add('Could not connect to base storage provider'#13#10);
    Exit;
  end;
  LogMemo.Lines.Add('Connected to ' + FStorage.ProviderInfo.ProviderName + '...'#13#10);

  UpdateItemClick(Self);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FreeTreeViewItems;
  FStorage.Free;
end;

procedure TMainForm.ExitItemClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.ExpandPSProvider(Node: TTreeNode);
var
  TypesList: TIDList;
  SubtypesList: TIDList;
  ItemsList: TItemList;

  i, j, k: Integer;
  pType: PStorageType;
  pSubtype: PStorageSubType;
  pItem: PStorageItem;

  Item: TTreeNode;
  SubItem: TTreeNode;
  SubSubItem: TTreeNode;
begin
  TypesList := FStorage.GetTypes;
  for i := 0 to TypesList.Count-1 do begin
    New(pType);
    pType.T := PSTORAGE_TYPE;
    pType.pType := TypesList.Get(i);
    Item := TreeView.Items.AddChild(Node, FStorage.GetTypeName(TypesList.Get(i)));
    Item.Data := pType;

    SubtypesList := FStorage.GetSubtypes(pType.pType);
    for j := 0 to SubtypesList.Count-1 do begin
      New(pSubtype);
      pSubtype.T := PSTORAGE_SUBTYPE;
      pSubtype.pType := pType.pType;
      pSubtype.pSubtype := SubTypesList.Get(j);
      SubItem := TreeView.Items.AddChild(Item, FStorage.GetSubtypeName(pType.pType,
        pSubtype.pSubtype));
      SubItem.Data := pSubtype;

      ItemsList := FStorage.GetItems(pType.pType, pSubtype.pSubtype);
      for k := 0 to ItemsList.Count-1 do begin
        New(pItem);
        pItem.T := PSTORAGE_ITEM;
        pItem.pType := pType.pType;
        pItem.pSubtype := pSubtype.pSubtype;
        pItem.pItem := ItemsList.Get(k);
        SubSubItem := TreeView.Items.AddChild(SubItem, pItem.pItem);
        SubSubItem.Data := pItem;
      end;
      ItemsList.Free;
    end;
    SubtypesList.Free;
  end;
  TypesList.Free;
end;

procedure TMainForm.FreeTreeViewItems;
begin
  if FProviderNode <> nil then begin
    FreePSProviderList(FProviderNode.getFirstChild);
    TreeView.Items.Clear;
    FProviderNode := nil;
  end;
end;

procedure TMainForm.FreePSProviderList(Node: TTreeNode);
var
  Item: TTreeNode;
begin
  if Node = nil then Exit;
  Item := Node;
  while Item <> nil do begin
    if Item.Data <> nil then
      Dispose(Item.Data);
    if (Item <> nil) and (Item.getFirstChild <> nil) then
      FreePSProviderList(Item.getFirstChild);
    Item := Node.GetNextChild(Item);
  end;
end;

procedure TMainForm.UpdateItemClick(Sender: TObject);
begin
  FreeTreeViewItems;

  if FStorage.Initialized then begin
    FProviderNode := TreeView.Items.Add(nil, FStorage.ProviderInfo.ProviderName);
    ExpandPSProvider(FProviderNode);
    FProviderNode.Expand(False);
  end;
end;

procedure TMainForm.TreeViewChange(Sender: TObject; Node: TTreeNode);
var
  Mem: Pointer;
  MemLen: Cardinal;
begin
  LogMemo.Clear;
  if not FStorage.Connected then Exit;
  if Node.Data = nil then
    LogMemo.Lines.Add('PStorage Provider: ' + Node.Text)
  else
    case PByte(Node.Data)^ of
      PSTORAGE_TYPE:
        LogMemo.Lines.Add('Type: ' + GUIDToString(PStorageType(Node.Data).pType));
      PSTORAGE_SUBTYPE: begin
        LogMemo.Lines.Add('Type: ' + GUIDToString(PStorageSubtype(Node.Data).pType));
        LogMemo.Lines.Add('Subtype: ' + GUIDToString(PStorageSubtype(Node.Data).pSubtype));
      end;
      PSTORAGE_ITEM: begin
        LogMemo.Lines.Add('Type: ' + GUIDToString(PStorageItem(Node.Data).pType));
        LogMemo.Lines.Add('Subtype: ' + GUIDToString(PStorageItem(Node.Data).pSubtype));
        LogMemo.Lines.Add('Item: ' + Node.Text + #13#10);
        if FStorage.ReadItemData(PStorageItem(Node.Data).pType, PStorageItem(Node.Data).pSubtype,
          PStorageItem(Node.Data).pItem, Mem, MemLen) and (Mem <> nil)
        then begin
          LogMemo.Lines.Add('Data Length: ' + IntToStr(MemLen));
          LogMemo.Lines.Add(DumpData(Mem, MemLen));
          CoTaskMemFree(Mem);
        end else
          LogMemo.Lines.Add('Coult not read item data');
      end;
    end;
end;

procedure TMainForm.PopupMenu1Popup(Sender: TObject);
begin
  if (TreeView.Selected = nil) or (TreeView.Selected.Data = nil) then begin
    DeleteItem.Enabled := False;
  end else
    DeleteItem.Enabled := True;
end;

procedure TMainForm.DeleteItemClick(Sender: TObject);
var
  Node: TTreeNode;
  rRet: Boolean;
begin
  Node := TreeView.Selected;
  rRet := False;
  case PByte(Node.Data)^ of
    PSTORAGE_TYPE:
      rRet := FStorage.DeleteType(PStorageType(Node.Data).pType);
    PSTORAGE_SUBTYPE:
      rRet := FStorage.DeleteSubtype(PStorageSubtype(Node.Data).pType, PStorageSubtype(Node.Data).pSubtype);
    PSTORAGE_ITEM:
      rRet := FStorage.DeleteItem(PStorageItem(Node.Data).pType, PStorageItem(Node.Data).pSubtype, PStorageItem(Node.Data).pItem);
  end;
  if rRet then begin
    Dispose(Node.Data);
    Node.Free;
    LogMemo.Text := 'Item deleted successfully'
  end else
    LogMemo.Text := 'Could not delete item';
end;

procedure TMainForm.TreeViewKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
const
  VK_DEL = 56;
begin
  if Key = 46 then
    DeleteItemClick(Self);
end;

end.
