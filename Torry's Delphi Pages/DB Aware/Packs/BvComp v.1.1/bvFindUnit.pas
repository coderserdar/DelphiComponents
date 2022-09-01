unit bvFindUnit;

interface

uses
{$ifndef LINUX}
  Windows, Messages, SysUtils, Graphics, Controls, Forms, Dialogs,
  DBGrids,Grids, StdCtrls, Buttons,ExtCtrls,  Menus, ComCtrls,
{$else}
  QForms,
  QStdCtrls,
  QButtons,
  QExtCtrls,
  QDBGrids,
  QMenus,
{$endif}
  Classes,
  bvFindDialogUnit,bvGridPopupUnit,
  bvLocalization;

type
    TbvFinderMenuItem = class(tbvCommonGridmenuitem);
    TbvFinderPopupMenu = class(TbvCommonGridPopupMenu);

type
  bv_Find = class(TComponent)
  private
    IsLoaded:boolean;
    FThisGrid :TDBGrid;
    OldOptions :TDBGridOptions;
    FAutoPopup:boolean;

//    procedure ThFind(Sender: TObject);
    procedure SetThisGrid(Value:TDBGrid);
    { Private declarations }
    procedure MenuClick(Sender:TObject);

  protected
    { Protected declarations }
    procedure Notification(AComponent: TComponent;Operation: TOperation); override;
  public
    { Public declarations }
    FDialog:TbvFindDialogForm;

    procedure Loaded; override;
    procedure Execute;
    constructor Create(AOwner:TComponent); override;
    destructor  Destroy; override;
//    function MessageHook(var Msg: TMessage): Boolean; override;
  published
    { Published declarations }
    property ThisGrid:TDBGrid read FThisGrid write SetThisGrid;
//    property DialogOnOk :boolean read ShowOk write ShowOk ;
//  property DisableWhileFind :boolean read DisableWhFind write DisableWhFind;
//  property ShowWaitForm:boolean read ShowWaitF write ShowWaitF;
    property AutoPopup:boolean read FAutoPopup write FAutoPopup default true;
end;


implementation

{$R bv_find.res}

uses bvDBGrid,DB,
     bvMessageUnit;


constructor bv_Find.Create(AOwner:TComponent);
Begin
    inherited Create(AOwner);
    IsLoaded:=false;
//    ShowOk :=false;
    FDialog:=nil;
//    DisableWhFind :=True;
//    OnFind:=ThFind;
//    ShowWaitF:=false;
    FAutoPopup:=True;
    if AOwner is TbvDBGrid then FThisGrid:=TbvdbGrid(AOwner)
    else FThisGrid:=nil;
end;


procedure bv_Find.SetThisGrid(Value:TDBGrid);
var ThItem:TbvFinderMenuItem;
    i:integer;
begin
  if (Value is TDBGrid) then begin
    if (Value<>FThisGrid) and not (Owner=Value) then begin
////
        if FAutoPopup and IsLoaded and not (csdesigning in componentState) then begin
            if Assigned(FThisGrid) and Assigned(FThisGrid.PopupMenu) then begin
               if FThisGrid.PopupMenu is TbvFinderPopupMenu then begin
                 FThisGrid.PopupMenu.Free;
                 FThisGrid.Popupmenu:=nil;
               end
               else begin
                 i:=0;
                 while i<FThisGrid.PopupMenu.Items.Count do
                   if (FThisGrid.PopupMenu.Items[i] is TbvFinderMenuItem)
                      and (FThisGrid.PopupMenu.Items[i].Owner=Self)
                   then FThisGrid.PopupMenu.Items.Delete(i)
                   else inc(i);
               end;
            end;
            if Assigned(Value) then begin
              if (Value.PopupMenu=nil) then begin
                  Value.PopupMenu:=TbvFinderPopupMenu.create(Self);
//                  (Value.PopupMenu as tbvFinderPopupMenu).DrawText:='Таблица';
              end
              else if
                   not (Value.PopupMenu is TbvCommonGridPopupMenu)
                   and (Value.popupmenu.items.count>0) and
                   not ((Value.popupmenu.items[Value.popupmenu.items.count-1] is TbvcommonGridMenuItem)
                       )
              then begin
                ThItem:=tbvFinderMenuItem.Create(self);
                ThItem.Caption:='-';
                Value.PopupMenu.items.Add(ThItem);
              end;
              ThItem:=tbvFinderMenuItem.Create(self);
              ThItem.Caption:=StrFinderCaption;
              thItem.ShortCut:=ShortCut(word('F'),[ssCtrl]);
              ThItem.OnClick:=MenuClick;
              Value.PopupMenu.items.Add(ThItem);
              //ThItem.Bitmap.Handle:=LoadBitmap(Hinstance,'bv_Finder');
              ThItem.Bitmap.LoadFromResourceName(Hinstance,'bv_Finder');
              ThItem.HelpContext:=10100;
            end;
        end;

/////
        FThisGrid:=Value;
    end;
  end
  else FThisGrid:=nil
//  else ShowMessage('Только для Tdbgrid');
end;

{
function bv_find.MessageHook(var Msg: TMessage): Boolean;
begin
  Result:= inherited MessageHook(Msg);
  if (Msg.Msg=WM_DESTROY) and (Assigned(ThisGrid)) then begin
      (ThisGrid as TDBGrid).Options:=OldOptions;
  end;
end;
}

procedure bv_Find.Execute;
var DATA:TDataSource;
begin
// Result:=false;
 if (ThisGrid=nil) then begin
   bvMessageError(StrErrorNotDefinedGrid);
   exit;
 end;

 if (ThisGrid is TDBGrid) then DATA:=(ThisGrid as TDBGrid).DataSource
 else Data:=nil;

 if DATA=nil then begin
   bvMessageError(StrErrorNotDefinedObject);
   exit;
 end;

 if Data.DataSet=nil then begin
      bvMessageError(StrErrorNotDefinedTAble);
      exit;
 end;

 if Data.DataSet.Active=false then begin
      bvMessageError(StrErrorTableIsclosed);
      exit;
 end;

 if ThisGrid is TDBGrid then begin
    OldOptions:=(ThisGrid as TDBGrid).Options;
    (ThisGrid as TDBGrid).Options:=(ThisGrid as TDBGrid).Options+[dgAlwaysShowSelection];
 end;

 if not Assigned(FDialog) then FDialog:=TbvFindDialogForm.Create(Application);
 FDialog.finder:=self;
 FDialog.Show;
end;


procedure bv_Find.MenuClick(Sender:tobject);
begin
  Execute;
end;


procedure bv_Find.Loaded;
var ThItem:TbvFinderMenuItem;
begin
  inherited;

  if not IsLoaded then begin
      IsLoaded:=true;
      if Assigned(FThisGrid)
         and (FThisGrid is tbvDBGrid)
         and (Owner<>FThisGrid)
         and (FThisGrid as TbvDBGrid).Finder.AutoPopup
      then exit;


      if Assigned(FThisGrid) then begin
        if FAutoPopup and not (csDesigning in ComponentState) then  begin
            if (ThisGrid.PopupMenu=nil) then begin
               ThisGrid.PopupMenu:=tbvFinderpopupmenu.Create(Self);
//               (ThisGrid.PopupMenu as tbvFinderPopupMenu).DrawText:='Таблица';
            end
            else if
                 not (ThisGrid.PopupMenu is TbvCommonGridPopupMenu)
                 and (ThisGrid.popupmenu.items.count>0) and
                 not ((ThisGrid.popupmenu.items[ThisGrid.popupmenu.items.count-1] is TbvCommonGridMenuItem)
                     )
            then begin
               ThItem:=tbvFinderMenuItem.Create(self);
               ThItem.Caption:='-';
               ThisGrid.PopupMenu.items.Add(ThItem);
            end;
            ThItem:=tbvFinderMenuItem.Create(self);
            ThItem.Caption:=StrFinderCaption;
            ThItem.OnClick:=MenuClick;
            thItem.ShortCut:=ShortCut(word('F'),[ssCtrl]);
            //ThItem.bitmap.Handle:=LoadBitmap(Hinstance,'bv_Finder');
            ThItem.bitmap.LoadFromResourcename(Hinstance,'bv_Finder');
            ThItem.HelpContext:=10100;
            ThisGrid.PopupMenu.items.Add(ThItem);   //resource discarded
//            SetMenuItemBitmaps(FThisGrid.popupmenu.Handle,ThItem.Command,MF_BYCOMMAND,LoadBitmap(Hinstance,'bv_Finder'),LoadBitmap(Hinstance,'bv_Finder'));
        end;
      end;
  end;
end;

destructor bv_find.destroy;
var i:integer;
begin
    if Assigned(FDialog) then begin
      if FDialog.InExecProc then  FDialog.NeedDestroy:=true
      else FDialog.free;
    end;

    if Assigned(FThisGrid)
       and Assigned(Owner)
       and ( (Owner=FThisGrid)
             or
             (Owner is tForm)
             and (Owner.FindComponent(FThisGrid.Name)<>nil)
           )
       and Assigned(FThisGrid.PopupMenu)
    then  begin
      if FThisGrid.PopupMenu is TbvFinderPopupMenu
      then FThisGrid.PopupMenu.free
      else begin
         i:=0;
         while i<FThisGrid.PopupMenu.Items.Count do
           if (FThisGrid.PopupMenu.Items[i] is TbvFinderMenuItem)
              and (FThisGrid.PopupMenu.Items[i].Owner=Self)
           then FThisGrid.PopupMenu.Items.Delete(i)
           else inc(i);
      end;
    end;
//    DoClose;
    inherited Destroy;
end;

procedure bv_Find.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if (Operation = opRemove) then
  begin
    if (AComponent=ThisGrid) then ThisGrid:=nil
    else if AComponent=FDialog then begin
       if Assigned(ThisGrid) then begin
           (ThisGrid as TDBGrid).Options:=OldOptions;
       end;
       FDialog:=nil;
    end;

  end;
end;

end.
