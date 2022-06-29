unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, dxsbar, dximctrl, Menus, ComCtrls, ImgList,
  users_basic, users_cs, Db, DBTables;

type
  TForm1 = class(TForm)
    SideBar: TdxSideBar;
    SideBarStore: TdxSideBarStore;
    imgSmall: TImageList;
    imgLarge: TImageList;
    SideBarStoreItem1: TdxStoredSideItem;
    SideBarStoreItem2: TdxStoredSideItem;
    SideBarStoreItem3: TdxStoredSideItem;
    SideBarStoreItem4: TdxStoredSideItem;
    SideBarStoreItem5: TdxStoredSideItem;
    SideBarStoreItem6: TdxStoredSideItem;
    SideBarStoreItem7: TdxStoredSideItem;
    SideBarStoreItem8: TdxStoredSideItem;
    SideBarStoreItem9: TdxStoredSideItem;
    SideBarStoreItem10: TdxStoredSideItem;
    MainMenu: TMainMenu;
    sec1: TMenuItem;
    adm1: TMenuItem;
    login1: TMenuItem;
    Database1: TDatabase;
    UsersCS1: TUsersCS;
    UsersCSReg1: TUsersCSReg;
    SideBarStoreItem11: TdxStoredSideItem;
    Memo1: TMemo;
    procedure adm1Click(Sender: TObject);
    procedure login1Click(Sender: TObject);
    procedure Database1BeforeConnect(Sender: TObject);
    procedure UsersCSReg1ApplySecurity(Sender: TObject;
      ComponentInfo: TComponentInfo; var Handled: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.adm1Click(Sender: TObject);
begin
  userscs1.usersadm;
end;

procedure TForm1.login1Click(Sender: TObject);
begin
  userscs1.login;
end;

procedure TForm1.Database1BeforeConnect(Sender: TObject);
begin
  Database1.Params.Values['PATH']:=ExtractFilePath(Application.ExeName)+'..\Data';
end;

procedure TForm1.UsersCSReg1ApplySecurity(Sender: TObject;
  ComponentInfo: TComponentInfo; var Handled: Boolean);
begin
  // how to secure a TdxSideBarItem
  if Sender is TdxSideBarItem then
    begin
        if ComponentInfo.ComponentStatus in [csDisabledVisible]  then
          begin
            (Sender as TdxSideBarItem).Enabled:=False;
            Handled:=True;
          end;
        if ComponentInfo.ComponentStatus in [csInvisible]  then
          begin
            (Sender as TdxSideBarItem).Enabled:=False;
            Handled:=True;
          end;
        if ComponentInfo.ComponentStatus in [csEnabledvisible]  then
          begin
            (Sender as TdxSideBarItem).Enabled:=True;
            Handled:=True;
          end;
    end;
end;

end.
