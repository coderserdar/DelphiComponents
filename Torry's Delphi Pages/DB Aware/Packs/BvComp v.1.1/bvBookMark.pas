unit bvBookMark;

interface

uses
{$ifndef LINUX}
  Windows, Messages, Graphics, Controls, Forms, Dialogs,
  DBGrids,Grids, StdCtrls, Buttons,ExtCtrls,  Menus, ComCtrls,
{$else}
  QForms,
  QStdCtrls,
  QButtons,
  QExtCtrls,
  QDBGrids,
  QMenus,
{$endif}
  SysUtils,
  DB,
  Classes,
  bvFindDialogUnit,bvfindUnit,bvGridPopupUnit,bvLocalization;

type
    TbvCommonBookMarkMenuItem = class(tbvCommonGridMenuItem);
    TbvBookMarkMenuItem = class(tbvCommonGridMenuItem);
    TbvBookMark1MenuItem = class(tbvCommonGridMenuItem);
    TbvBookMarkPopupMenu = class(TbvCommonGridPopupMenu);

type
  TbvBookMark = class(TComponent)
  private
    IsLoaded:boolean;
    FThisGrid :TDBGrid;
    FAutoPopup:boolean;
    FBookMark:Array[0..9] of TBookMark;

//    procedure ThFind(Sender: TObject);
    procedure SetThisGrid(Value:TDBGrid);
    { Private declarations }
    procedure MenuClick(Sender:TObject);
    procedure MenuGotoClick(Sender:TObject);

  protected
    { Protected declarations }
    procedure Notification(AComponent: TComponent;Operation: TOperation); override;
  public
    { Public declarations }
    procedure Loaded; override;
    constructor Create(AOwner:TComponent); override;
    destructor  Destroy; override;
    function Check:boolean;
//    function MessageHook(var Msg: TMessage): Boolean; override;
  published
    { Published declarations }
    property ThisGrid:TDBGrid read FThisGrid write SetThisGrid;
    property AutoPopup:boolean read FAutoPopup write FAutoPopup default true;
end;


implementation

{$R bvBookM.res}


uses bvMessageUnit,bvDBGrid;

const
   CharArr :array[0..9] of char = '0123456789';

constructor TbvBookMark.Create(AOwner:TComponent);
var i:integer;
Begin
    inherited Create(AOwner);
    IsLoaded:=false;
    FAutoPopup:=True;
    if AOwner is TbvDBGrid then FThisGrid:=TbvdbGrid(AOwner)
    else FThisGrid:=nil;

    for i:=low(FBookMark) to high(FBookMark) do
      FBookmark[i]:=nil;
end;


procedure TbvBookMark.SetThisGrid(Value:TDBGrid);
var ThItem,thItem1:TMenuItem;
    i:integer;
begin
  if (Value is TDBGrid) then begin
    if (Value<>FThisGrid) and not (Owner=Value) then begin
////
        if FAutoPopup and IsLoaded and not (csdesigning in componentState) then begin
            if Assigned(FThisGrid) and Assigned(FThisGrid.PopupMenu) then begin
               if FThisGrid.PopupMenu is TbvBookMarkPopupMenu then begin
//                 FThisGrid.PopupMenu.Items.Items.free;
                 FThisGrid.PopupMenu.Free;
                 FThisGrid.Popupmenu:=nil;
               end
               else begin
                 i:=0;
                 while i<FThisGrid.PopupMenu.Items.Count do
                   if ((FThisGrid.PopupMenu.Items[i] is TbvCommonBookMarkMenuItem)
                      )
                      and (FThisGrid.PopupMenu.Items[i].Owner=Self)
                   then begin
                      //if assigned( FThisGrid.PopupMenu.Items.Items)
                      //then FThisGrid.PopupMenu.Items.Items.free;

                      FThisGrid.PopupMenu.Items.Delete(i);
                   end
                   else inc(i);
               end;
            end;
            if Assigned(Value) then begin
              if (Value.PopupMenu=nil) then begin
                  Value.PopupMenu:=TbvBookMarkPopupMenu.create(Self);
//                  (Value.PopupMenu as tbvFinderPopupMenu).DrawText:='Таблица';
              end
              else if
                   not (Value.PopupMenu is TbvCommonGridPopupMenu)
                   and (Value.popupmenu.items.count>0) and
                   not ((Value.popupmenu.items[Value.popupmenu.items.count-1] is tbvCommonGridMenuItem)
                       )
              then begin
                ThItem:=tbvCommonBookMarkMenuItem.Create(self);
                ThItem.Caption:='-';
                Value.PopupMenu.items.Add(ThItem);
              end;

              thItem1:=TbvCommonBookMarkMenuItem.create(Self);
              thItem1.caption:=StrBookmarks;

              //ThItem1.Bitmap.Handle:=LoadBitmap(Hinstance,'bvBookMark');
              ThItem1.Bitmap.LoadFromResourceName(hInstance, 'bvBookMark');
              Value.PopupMenu.items.Add(ThItem1);

              for i:=0 to 9 do begin
                ThItem:=tbvBookMarkMenuItem.Create(self);
                ThItem.Caption:=inttostr(i);
                thItem.ShortCut:=ShortCut(word(chararr[i]),[ssAlt]);
                ThItem.OnClick:=MenuClick;
                if i=0 then
                  //ThItem.Bitmap.Handle:=LoadBitmap(Hinstance,'bvBookMark');
                  ThItem.Bitmap.LoadFromResourceName(Hinstance,'bvBookMark');

                thItem1.add(ThItem);
              end;
              for i:=0 to 9 do begin
                ThItem:=tbvBookMark1MenuItem.Create(self);
                ThItem.Caption:=inttostr(i);
                thItem.ShortCut:=ShortCut(word(chararr[i]),[ssctrl]);
                ThItem.OnClick:=MenuGotoClick;
                if i=0 then
                  //ThItem.Bitmap.Handle:=LoadBitmap(Hinstance,'bvBookMarkGOTO');
                  ThItem.Bitmap.loadfromresourcename(Hinstance,'bvBookMarkGOTO');
                //Value.PopupMenu.items.Add(ThItem);
                thItem1.add(ThItem);
              end;
              //thItem2:=NewSubMenu( '',0,'SubMenu'+Value.Name,ARR);
              //thItem1.Add(thItem2);

              //SetLength(Arr,0);
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

function TbvBookMark.Check:boolean;
var DATA:TDataSource;
begin
 Result:=false;
 if (ThisGrid=nil) then begin
   bvMessageError(StrErrorNotDefinedGrid);
   exit;
 end;

 if (ThisGrid is TdbGrid) then DATA:=(ThisGrid as TDBGrid).DataSource
 else Data:=nil;

 if DATA=nil then begin
   bvMessageError(strErrorNotDefinedObject);
   exit;
 end;

 if Data.DataSet=nil then begin
      bvMessageError(StrErrorNotDefinedGrid);
      exit;
 end;

 if Data.DataSet.Active=false then begin
      bvMessageError(StrErrorNotDefinedTable);
      exit;
 end;

 Result:=true
end;

procedure TbvBookMark.MenuClick(Sender:tobject);
var i:integer;
    str:string;
begin
   if Check then begin
        str:=ShortCutToText((Sender as TMenuItem).ShortCut);
        if pos('0',str)>0 then i:=0
        else if pos('1',str)>0 then i:=1
        else if pos('2',str)>0 then i:=2
        else if pos('3',str)>0 then i:=3
        else if pos('4',str)>0 then i:=4
        else if pos('5',str)>0 then i:=5
        else if pos('6',str)>0 then i:=6
        else if pos('7',str)>0 then i:=7
        else if pos('8',str)>0 then i:=8
        else if pos('9',str)>0 then i:=9
        else i:=10;

        if i in [0..9]
        then begin
          if Assigned(FBookMark[i]) then FreeMem(FBookMark[i]);
          FBookMark[i]:=FThisGrid.datasource.dataset.GetBookMark;
        end
   end;
end;

procedure TbvBookMark.MenuGOTOClick(Sender:tobject);
var str:string;
    i:integer;
begin
   if Check then begin
        str:=ShortCutToText((Sender as TMenuItem).ShortCut);
        if pos('0',str)>0 then i:=0
        else if pos('1',str)>0 then i:=1
        else if pos('2',str)>0 then i:=2
        else if pos('3',str)>0 then i:=3
        else if pos('4',str)>0 then i:=4
        else if pos('5',str)>0 then i:=5
        else if pos('6',str)>0 then i:=6
        else if pos('7',str)>0 then i:=7
        else if pos('8',str)>0 then i:=8
        else if pos('9',str)>0 then i:=9
        else i:=10;

        if i in [0..9]
        then begin
            if Assigned(FBookMark[i])
               and FThisGrid.datasource.dataset.BookmarkValid(FBookMark[i])
            then FThisGrid.datasource.dataset.GotoBookMark(FBookMark[i]);
        end
   end;
end;


procedure tbvBookMark.Loaded;
var ThItem,thItem1:TMenuItem;
    i:integer;
begin
  inherited;

  if not IsLoaded then begin
      IsLoaded:=true;
      if Assigned(FThisGrid)
         and (FThisGrid is tbvDBGrid)
         and (Owner<>FThisGrid)
         and (FThisGrid as TbvDBGrid).bvBookMark.AutoPopup
      then exit;


      if Assigned(FThisGrid) then begin
        if FAutoPopup and not (csDesigning in ComponentState) then  begin
            if (ThisGrid.PopupMenu=nil) then begin
               ThisGrid.PopupMenu:=tbvBookMarkpopupmenu.Create(Self);
//               (ThisGrid.PopupMenu as tbvFinderPopupMenu).DrawText:='Таблица';
            end
            else if
                 not (ThisGrid.PopupMenu is TbvCommonGridPopupMenu)
                 and (ThisGrid.popupmenu.items.count>0) and
                 not ((ThisGrid.popupmenu.items[ThisGrid.popupmenu.items.count-1] is tbvCommonGridMenuItem)
                     )
            then begin
               ThItem:=tbvBookMarkMenuItem.Create(self);
               ThItem.Caption:='-';
               ThisGrid.PopupMenu.items.Add(ThItem);
            end;

            thItem1:=Tbvcommonbookmarkmenuitem.create(Self);
            thItem1.caption:=StrBookmarks;
               //ThItem1.Bitmap.Handle:=LoadBitmap(Hinstance,'bvBookMark');
            ThItem1.Bitmap.LoadFromResourceName(Hinstance,'bvBookMark');

            ThisGrid.PopupMenu.items.Add(ThItem1);

            //SEtLength(Arr,20);
            for i:=0 to 9 do begin
              ThItem:=tbvBookMarkMenuItem.Create(self);
              ThItem.Caption:=inttostr(i);
              thItem.ShortCut:=ShortCut(word(chararr[i]),[ssAlt]);
              ThItem.OnClick:=MenuClick;
              if i=0 then
                //ThItem.Bitmap.Handle:=LoadBitmap(Hinstance,'bvBookMark');
                ThItem.Bitmap.LoadFromResourceName(Hinstance,'bvBookMark');

              thItem1.add(ThItem);
            end;

            for i:=0 to 9 do begin
              ThItem:=tbvBookMark1MenuItem.Create(self);
              ThItem.Caption:=inttostr(i);
              thItem.ShortCut:=ShortCut(word(chararr[i]),[ssctrl]);
              ThItem.OnClick:=MenuGotoClick;
              if i=0 then
                //ThItem.Bitmap.Handle:=LoadBitmap(Hinstance,'bvBookMarkGOTO');
                ThItem.Bitmap.LoadFromResourceName(Hinstance,'bvBookMarkGOTO');
              //ThisGrid.PopupMenu.items.Add(ThItem);
              thItem1.add(ThItem);
            end;
            //thItem1:=NewSubMenu( '',0,'SubMenu'+ThisGrid.Name,ARR);

            //thItem1:=TbvCommonBookMarkMenuItem.create(Self);

            //SetLength(Arr,0);

            {ThItem:=tbvBookMarkMenuItem.Create(self);
            ThItem.Caption:='Закладка';
            thItem.ShortCut:=ShortCut(word('1'),[ssAlt]);
            ThItem.OnClick:=MenuClick;
            ThItem.Bitmap.Handle:=LoadBitmap(Hinstance,'bvBookMark');
            ThisGrid.PopupMenu.items.Add(ThItem);   //resource discarded

            ThItem:=tbvBookMark1MenuItem.Create(self);
            ThItem.Caption:='Перейти к закладке';
            thItem.ShortCut:=ShortCut(word('1'),[ssctrl]);
            ThItem.OnClick:=MenuGotoClick;
            ThItem.Bitmap.Handle:=LoadBitmap(Hinstance,'bvBookMarkGOTO');
            ThisGrid.PopupMenu.items.Add(ThItem);   //resource discarded
            }
//            SetMenuItemBitmaps(FThisGrid.popupmenu.Handle,ThItem.Command,MF_BYCOMMAND,LoadBitmap(Hinstance,'bv_Finder'),LoadBitmap(Hinstance,'bv_Finder'));
        end;
      end;
  end;
end;

destructor tbvBookMark.destroy;
var i:integer;
begin
    for i:=low(FBookMark) to high(FBookMark) do
        if Assigned(FBookMark[i]) then freemem(FBookMark[i]);

    if Assigned(FThisGrid)
       and Assigned(Owner)
       and ( (Owner=FThisGrid)
             or
             (Owner is tForm)
             and (Owner.FindComponent(FThisGrid.Name)<>nil)
           )
       and Assigned(FThisGrid.PopupMenu)
    then  begin
      if FThisGrid.PopupMenu is TbvBookMarkPopupMenu
      then FThisGrid.PopupMenu.free
      else begin
         i:=0;
         while i<FThisGrid.PopupMenu.Items.Count do
           if (
               (FThisGrid.PopupMenu.Items[i] is TbvCommonBookMarkMenuItem)
              )
              and (FThisGrid.PopupMenu.Items[i].Owner=Self)
           then FThisGrid.PopupMenu.Items.Delete(i)
           else inc(i);
      end;
    end;
//    DoClose;
    inherited Destroy;
end;

procedure TbvBookMark.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if (Operation = opRemove) then
  begin
    if (AComponent=ThisGrid) then ThisGrid:=nil
  end;
end;

end.
