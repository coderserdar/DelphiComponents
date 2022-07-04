unit ComponentSelectD6;

interface

uses
  Windows, ExptIntf,Dialogs,Classes,ToolIntf, ShellApi,Forms,ComCtrls
  ,Menus,ExtCtrls,Controls,ImgList,Graphics,Messages,ToolsApi;

type
  TLHToolBar = class(TToolBar)
  public
      destructor Destroy;override;
  end;


  TComponentSelect = class(TIExpert)
  private
        FPopup : TPopupMenu;
        FTool:TLHToolBar;
        procedure CreateToolBar;
        procedure BtnClick(Sender:TObject);
        procedure Btn1Click(Sender:TObject);
        procedure ResizeTab(Sender:TObject);
        procedure CreatePalette(button: TToolButton);
        procedure TabClick(Sender: TObject);
        procedure GetComponentList(Items:TStrings;ActiveOnly:Boolean);
  public
    constructor Create;
    destructor destroy;override;
    function GetName: string; override; stdcall;
    function GetAuthor: string; override; stdcall;
    function GetStyle: TExpertStyle; override; stdcall;
    function GetIDString: string; override; stdcall;
  end;
  function MyHook(code: Integer; wparam: WPARAM; lparam: LPARAM): LRESULT stdcall;
procedure Register;

implementation

uses SearchComponent_u;
var
  FHook : hHook;
  MySender:TObject;
  FBusy:boolean;


{$r ComponentSelectD6.dcr}
{************************************************************}

function MyHook(code: Integer; wparam: WPARAM;lparam: LPARAM): LRESULT;
begin
    with PCWPStruct(lParam)^ do
       if (Message = WM_PARENTNOTIFY) and (wParam = WM_CREATE)  then
       begin
          if not(FBusy) then
          begin
             FBusy:=true;
             TComponentSelect(Mysender).CreateToolBar();
             FBusy:=false;
          end;
       end;
    Result:=CallNextHookEx(FHook, Code, wParam, lParam);
end;



function FindCompP(nom:string;par:string):TComponent;
begin
   if application.FindComponent(par)<>nil then
        with application.Findcomponent(par) do
             result:=FindComponent(nom)
   else
       result:=nil;
end;


procedure Register;
begin
  RegisterLibraryExpert(TComponentSelect.Create)
end;
{************************************************************}
function TComponentSelect.GetName: string;
begin
  result:='Component Search';
end;
{************************************************************}
function TComponentSelect.GetAuthor: string;
begin
  result:='YuanHe Zhong';
end;
{************************************************************}
function TComponentSelect.GetStyle: TExpertStyle;
begin
  result:=esAddIn;
end;
{************************************************************}
function TComponentSelect.GetIDString: string;
begin
  result:='_TComponentSelect';
end;
{************************************************************}
constructor TComponentSelect.Create;
var
        i:integer;
begin
  inherited;
  FBusy:=false;
  MySender:=self;
  i:=0;
  FHook:=SetWindowsHookEx(WH_CALLWNDPROC,MyHook,i,GetCurrentThreadID);

//  CreateToolBar;
//  CreatePalette(Ftool.Buttons[1]);
end;

destructor TComponentSelect.destroy;
var
   i:integer;
   FForm:TForm;
   Tab:TTabControl;
begin
  inherited;
   FForm:=TForm(Application.FindComponent('AppBuilder'));
   UnhookWindowsHookEx(FHook);
   if FForm<>nil then
   begin
      Tab:=TTabControl(FForm.FindComponent('TabControl'));
      if Tab<>nil then
          Tab.OnResize:=nil
   end;
   try
      if FTool<>nil then
      begin
         FTool.Free;
         FTool:=nil;
      end;
      if Assigned(Form1) then
      begin
        Form1.Visible:=false;
        Form1.Free;
        Form1:=nil;
      end;
   except
        ShowMessage('Error Except');
   end;
end;


destructor TLHToolBar.Destroy;
begin
   self.Parent:=nil;
   inherited;
end;


procedure TComponentSelect.CreateToolBar;
var
   but:TToolButton;
   FToolBar:TLHToolBar;
   bmp:TBitmap;
   i:integer;
begin
      if FTool=nil then
      begin
         FToolBar:=TLHToolBar.Create(nil);
         FToolBar.parent:=TControlBar(FindCompP('ControlBar1','AppBuilder'));
         FToolBar.Align:=alTop;
         FToolBar.AutoSize:=true;
         FToolBar.Wrapable:=false;
         FToolBar.EdgeBorders:=[];
         FToolBar.DragMode:=dmAutomatic;
         FToolBar.Flat:=true;
         FToolbar.Width:=30;
         FTool:=FToolBar;
//         FToolBar.OnEndDrag:=OnTabDrag;

         but:=TToolButton.Create(FToolBar);
         but.parent:=FToolBar;
         but.hint:='Search Component';
         but.showhint:=true;
         but.OnClick:=BtnClick;

         but:=TToolButton.Create(FToolBar);
         but.parent:=FToolBar;
         but.hint:='Search Component';
         but.showhint:=true;
         but.OnClick:=Btn1Click;


         Ftoolbar.images:=TCustomImageList.Create(Ftoolbar);
         Ftoolbar.images.DrawingStyle:=dsNormal;
         FToolBar.images.Masked:=false;
         FToolBar.images.Width:=16;
         FToolBar.images.height:=16;

         bmp:=TBitmap.create;
         bmp.width:=16;
         bmp.height:=16;
         bmp.PixelFormat:=pf8bit;
         bmp.LoadFromResourceName(hInstance,'BTN2');
         FToolBar.Images.Add(bmp,nil);
         bmp.LoadFromResourceName(hInstance,'BTN1');
         FToolBar.Images.Add(bmp,nil);

         bmp.Free;
         for i:=0 to 1 do
                 FtoolBar.Buttons[i].ImageIndex:=i;
         CreatePalette(Ftool.Buttons[1]);
      end;
end;

procedure TComponentSelect.BtnClick(Sender: TObject);
var
   t:TPoint;
begin
   GetCursorPos(t);
   try
     if (Sender as TToolButton).PopupMenu<>nil then
       (Sender as TToolButton).PopupMenu.popup(t.x,t.y);
   except
   end;
end;

procedure TComponentSelect.Btn1Click(Sender: TObject);
var
  FForm:TForm;
begin
   FForm:=TForm(Application.FindComponent('AppBuilder'));
   if FForm<>nil then
   begin
    if not Assigned(Form1) then
      Form1:=TForm1.Create(FForm);
    GetComponentList(Form1.FComponentList,true);
    Form1.Show;
   end;
end;


procedure TComponentSelect.ResizeTab(Sender:TObject);
var
   FHeight:integer;
begin
   if Sender is TTabControl then
   begin
      with (Sender as TTabControl) do
      begin
         FHeight:=Height-(DisplayRect.Bottom-DisplayRect.top)+30;
         Constraints.MinHeight:=FHeight;
         (parent as TWinControl).Constraints.MaxHeight:=FHeight;
      end;
   end;
end;

procedure TComponentSelect.CreatePalette(button: TToolButton);
var
  ts:TstringList;
  it:TMenuItem;
  pop:TPopupMenu;
  FForm:TForm;
  Tab:TTabControl;
  i:integer;
begin
   if button.PopupMenu<>nil then
      button.PopupMenu.free;
   ts:=TstringList.create;
   Pop:=nil;
   FForm:=TForm(Application.FindComponent('AppBuilder'));
   if FForm<>nil then
   begin
      Tab:=TTabControl(FForm.FindComponent('TabControl'));
      if Tab<>nil then
      begin
         ts.Text:=Tab.Tabs.Text;
         ts.Sort;
         pop:=TPopupMenu.create(Button);
         i:=-1;
         while ts.Count>0 do
         begin
            it:=TmenuItem.create(pop);
            if i=20 then
            begin
               i:=0;
               it.Break:=mbBarBreak;
            end
            else inc(i);
            it.caption:=ts[0];
            it.Hint:=ts[0];
            it.onclick:=TabClick;
            ts.delete(0);
            pop.Items.add(it);
         end;
      end;
   end;
   ts.free;
   button.PopupMenu:=pop;
end;

procedure TComponentSelect.TabClick(Sender: TObject);
var
  FForm:TForm;
  Tab:TTabControl;
begin
   FForm:=TForm(Application.FindComponent('AppBuilder'));
   if FForm<>nil then
   begin
      Tab:=TTabControl(FForm.FindComponent('TabControl'));
      if Tab<>nil then
      begin
         Tab.TabIndex:=Tab.Tabs.IndexOf((Sender as TMenuItem).Hint);
         Tab.OnChange(Tab);
      end;
   end;
end;


procedure TComponentSelect.GetComponentList(Items: TStrings;
  ActiveOnly: Boolean);
var
  PackageServices: IOTAPackageServices;
  PackageCounter: Integer;
  ComponentCounter: Integer;
  InstalledComponentName: string;
begin
  PackageServices := BorlandIDEServices as IOTAPackageServices;
  Assert(Assigned(PackageServices));

  for PackageCounter := 0 to PackageServices.PackageCount-1 do
  begin
    for ComponentCounter := 0 to PackageServices.GetComponentCount(PackageCounter)-1 do
    begin
      InstalledComponentName := PackageServices.ComponentNames[PackageCounter, ComponentCounter];
      if (items.IndexOf(InstalledComponentName) = -1) then
      begin
        if (not ActiveOnly) or (GetClass(InstalledComponentName) <> nil) then
          Items.AddObject(InstalledComponentName,TObject(PackageCounter));
      end;
    end;
  end;
end;

end.
