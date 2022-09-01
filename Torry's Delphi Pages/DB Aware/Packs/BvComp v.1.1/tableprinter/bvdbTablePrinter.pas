unit bvdbTablePrinter;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  DBGrids,IniFiles,bvGridPopupUnit,bvLocalization,bvTabPrintPreview;

const MaxColCount=40;

type
    TbvTabPrinterMenuItem = class(tbvcommongridmenuitem);
    TbvTabPrinterPopupMenu = class(TbvcommongridPopupMenu);


type
  TDBTablePrinter = class(TComponent)
  private
    { Private declarations }
    IsLoaded:boolean;
    FThisGrid:TDBGrid;
    FEnabled:boolean;
    FAutoPopup:boolean;

    procedure SetThisGrid(Value:TDBGrid);
    procedure MenuClick(Sender:TObject);
  protected
    { Protected declarations }
    procedure Notification(AComponent: TComponent;Operation: TOperation); override;
  public
    { Public declarations }

    constructor Create(AOwner:TComponent); override;
    destructor Destroy; override;
    procedure Loaded;override;
    procedure PrintTable;
  published
    { Published declarations }
    property ThisGrid:TDBGrid read FThisGrid write SetThisGrid;
    property Enabled:boolean read FEnabled write FEnabled;
    property AutoPopup:boolean read FAutoPopup write FAutoPopup default true;
  end;

var quickPrintEnabled:boolean;

implementation

uses bvDBGrid,bvMessageUnit; //,Forms;

{$R bvRSPRINT.RES}


procedure TDBTablePrinter.PrintTable;
begin
  if not Assigned(FThisGrid) then bvMessage(StrErrorNotDefinedGrid)
  else  begin
     // здесь ожидаетс€ много работы
     with TdbPrintPreviewForm.create(Self) do
     try
        Grid:=ThisGrid;
        Show;
        PaintPreview;

     finally
        //free
     end;
  end;
end;


procedure TDBTablePrinter.SetThisGrid(Value:TDBGrid);
var ThItem:TbvTabPrinterMenuItem;
    i:integer;
begin
  if (Value is TDBGrid) then begin
    if (Value<>FThisGrid) and not (Owner=Value) then begin
////
        if QuickPrintEnabled and FAutoPopup and  isLoaded and not (csdesigning in componentState) then begin
            if Assigned(FThisGrid) and Assigned(FThisGrid.PopupMenu) then begin
               if FThisGrid.PopupMenu is TbvTabPrinterPopupMenu then begin
                 FThisGrid.PopupMenu.Free;
                 FThisGrid.Popupmenu:=nil;
               end
               else begin
                 i:=0;
                 while i<FThisGrid.PopupMenu.Items.Count do
                   if (FThisGrid.PopupMenu.Items[i] is TbvTabPrinterMenuItem)
                      and (FThisGrid.PopupMenu.Items[i].Owner=Self)
                   then FThisGrid.PopupMenu.Items.Delete(i)
                   else inc(i);
               end;
            end;
            if (Self.Enabled) and Assigned(Value) then begin
              if (Value.PopupMenu=nil) then begin
                  Value.PopupMenu:=TbvTabPrinterPopupMenu.create(Self);
              end
              else if
                   not (Value.PopupMenu is TbvCommonGridPopupMenu)
                   and (Value.popupmenu.items.count>0) and
                   not ((Value.popupmenu.Items[Value.popupmenu.items.count-1] is TbvCommonGridMenuItem)
                       )
              then begin
                ThItem:=tbvTabPrinterMenuItem.Create(self);
                ThItem.Caption:='-';
                Value.PopupMenu.items.Add(ThItem);
              end;
              ThItem:=tbvTabPrinterMenuItem.Create(self);
              ThItem.Caption:=StrQuickPrint;
              ThItem.OnClick:=MenuClick;
              ThItem.Bitmap.Handle:=LoadBitmap(Hinstance,'rsprint');
//              thItem.Enabled:=AllTableSAversEnabled;
              Value.PopupMenu.items.Add(ThItem);
//              SetMenuItemBitmaps(FThisGrid.popupmenu.Handle,ThItem.Command,MF_BYCOMMAND,LoadBitmap(Hinstance,'bvGridSaver'),LoadBitmap(Hinstance,'DBTabSavImage'));
            end;
        end;

/////
        FThisGrid:=Value;
    end;
  end
  else {if Value=nil then} FThisGrid:=nil
//  else ShowMessage('“олько дл€ Tdbgrid');
end;

constructor TDBTablePrinter.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  IsLoaded:=false;
  FAutoPopup:=true;
  if AOwner is TbvDBGrid then FThisGrid:=TbvdbGrid(AOwner)
  else FThisGrid:=nil;
  Enabled:=true;
end;

destructor TDBTablePrinter.destroy;
var i:integer;
begin
    if Assigned(FThisGrid)
       and Assigned(Owner)
       and ( (Owner=FThisGrid)
             or
             (Owner is tForm)
             and (Owner.FindComponent(FThisGrid.Name)<>nil)
           )
       and Assigned(FThisGrid.PopupMenu)
    then  begin
      if FThisGrid.PopupMenu is TbvTabPrinterPopupMenu
      then FThisGrid.PopupMenu.free
      else begin
         i:=0;
         while i<FThisGrid.PopupMenu.Items.Count do
           if (FThisGrid.PopupMenu.Items[i] is TbvTabPrinterMenuItem)
              and (FThisGrid.PopupMenu.Items[i].Owner=Self)
           then FThisGrid.PopupMenu.Items.Delete(i)
           else inc(i);
      end;
    end;
    inherited Destroy;
end;

procedure TDBTablePrinter.MenuClick(Sender:tobject);
begin
  Self.PrintTable;
end;


procedure TDBTablePrinter.Loaded;
var ThItem:TbvTabPrinterMenuItem;
begin
  inherited;
//  FOldOnFormDestroy:=nil;

  if not IsLoaded then begin
      IsLoaded:=true;
      if Self.Enabled
         and Assigned(FThisGrid)
         and (FThisGrid is tbvDBGrid)
         and (Owner<>FThisGrid)
         and (FThisGrid as TbvDBGrid).Finder.AutoPopup
      then exit;

      if (Self.Enabled) and Assigned(ThisGrid) then begin
        if quickPrintEnabled and FAutoPopup and not (csDesigning in ComponentState) then  begin
            if (ThisGrid.PopupMenu=nil) then begin
               ThisGrid.PopupMenu:=tbvTabPrinterpopupmenu.Create(Self);
            end
            else if
                 not (ThisGrid.PopupMenu is TbvCommonGridPopupMenu)
                 and (ThisGrid.popupmenu.items.count>0) and
                 not ((ThisGrid.popupmenu.items[ThisGrid.popupmenu.items.count-1] is TbvCommonGridMenuItem)
                     )
            then begin
               ThItem:=tbvTabPrinterMenuItem.Create(self);
               ThItem.Caption:='-';
               ThisGrid.PopupMenu.items.Add(ThItem);
            end;
            ThItem:=tbvTabPrinterMenuItem.Create(self);
            ThItem.Caption:=StrQuickPrint;
            ThItem.OnClick:=MenuClick;
            ThItem.Bitmap.LoadFromResourceName(Hinstance,'rsprint');
//            thItem.Enabled:=AllTableSAversEnabled;
            // Handle:=LoadBitmap(Hinstance,'SPAGENTS');
            ThisGrid.PopupMenu.items.Add(ThItem);   //resource discarded
//            SetMenuItemBitmaps(FThisGrid.popupmenu.Handle,ThItem.Command,MF_BYCOMMAND,LoadBitmap(Hinstance,'bvGridSaver'),LoadBitmap(Hinstance,'bvGridsaver'));
        end;
      end;
  end;
end;

procedure TDBTablePrinter.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if (Operation = opRemove) then
  begin
    if (AComponent=ThisGrid) then ThisGrid:=nil;
  end;
end;

initialization
  quickPrintEnabled:=true;
end.
