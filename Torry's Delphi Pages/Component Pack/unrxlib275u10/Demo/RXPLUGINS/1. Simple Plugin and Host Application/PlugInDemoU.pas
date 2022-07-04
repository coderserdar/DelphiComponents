unit PlugInDemoU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, Menus, ExtCtrls, ImgList, RxPluginManager;

type
  TForm1 = class(TForm)
    clbPlugins: TListBox;
    MainMenu1: TMainMenu;
    Plugin1: TMenuItem;
    lbStatus: TListBox;
    Panel1: TPanel;
    Panel2: TPanel;
    Label1: TLabel;
    labAuthor: TLabel;
    Label2: TLabel;
    labDescription: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    File1: TMenuItem;
    Exit1: TMenuItem;
    Help1: TMenuItem;
    About1: TMenuItem;
    SendMessagetoPlugins1: TMenuItem;
    N1: TMenuItem;
    RxPluginManager1: TRxPluginManager;
    procedure FormCreate(Sender: TObject);
    procedure uilPluginManagerBeforeLoading(Sender: TObject);
    procedure clbPluginsClick(Sender: TObject);
    procedure clbPluginsDblClick(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure About1Click(Sender: TObject);
    procedure SendMessagetoPlugins1Click(Sender: TObject);
    procedure RxPluginManager1AfterLoad(Sender: TObject; FileName: string;
      const ALibHandle: Cardinal; var AllowLoad: Boolean);
    procedure RxPluginManager1BeforeLoad(Sender: TObject; FileName: string;
      var AllowLoad: Boolean);
    procedure RxPluginManager1NewCommand(Sender: TObject; ACaption, AHint,
      AData: string; AShortCut: TShortCut; ABitmap: TBitmap;
      AEvent: TNotifyEvent);
  private
    { Private declarations }
  public
    NumButtons : integer;
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.FormCreate(Sender: TObject);
begin
   RxPluginManager1.LoadPlugins;
   RxPluginManager1.GetLoadedPlugins(clbPlugins.Items);
end;

procedure TForm1.RxPluginManager1AfterLoad(Sender: TObject; FileName: string;
  const ALibHandle: Cardinal; var AllowLoad: Boolean);
begin
  lbStatus.Items.Add('Finished loading Plug-in: ' + Filename);
end;

procedure TForm1.RxPluginManager1BeforeLoad(Sender: TObject; FileName: string;
  var AllowLoad: Boolean);
begin
  lbStatus.Items.Add('Loading Plug-in: ' + Filename);
end;

procedure TForm1.RxPluginManager1NewCommand(Sender: TObject; ACaption, AHint,
  AData: string; AShortCut: TShortCut; ABitmap: TBitmap; AEvent: TNotifyEvent);
var
   Item : TMenuItem;
begin
   lbStatus.Items.Add('Adding command: ' + ACaption);
   Item := NewItem(ACaption, scNone, False, True, AEvent, 0, '');
   MainMenu1.Items[1].Add(Item);
   with TSpeedButton.Create(Panel1) do
   begin
      Top := 4;
      Left := 4+(NumButtons * Width);
      Parent := Panel1;
      Hint := AHint;
      if ABitmap <> nil then
         Glyph.Handle := ABitmap.Handle;
      try
         NumGlyphs := Glyph.Width div Glyph.Height;
      except
      end;
      OnClick := AEvent;
   end;    // with
   Inc(NumButtons);
end;

procedure TForm1.uilPluginManagerBeforeLoading(Sender: TObject);
begin
   lbStatus.Items.Add('Starting to load Plug-ins');
end;

procedure TForm1.clbPluginsClick(Sender: TObject);
begin
   if clbPlugins.ItemIndex = -1 then Exit;
   labAuthor.Caption := RxPluginManager1.Plugins[clbPlugins.ItemIndex].Author;
   labDescription.Caption := RxPluginManager1.Plugins[clbPlugins.ItemIndex].Description;
end;

procedure TForm1.clbPluginsDblClick(Sender: TObject);
begin
   RxPluginManager1.Plugins[clbPlugins.ItemIndex].Configure;
end;

procedure TForm1.Exit1Click(Sender: TObject);
begin
   Close;
end;

procedure TForm1.About1Click(Sender: TObject);
begin
   ShowMessage('A simple host application for demoing UIL plug-ins.'#13#10#13#10'(c) 1999, Unlimited Intelligence Limited.');
end;

procedure TForm1.SendMessagetoPlugins1Click(Sender: TObject);
begin
   RxPluginManager1.SendMessage(1000, InputBox('Enter message to send to plugin', 'Message', 'Your message here'));
end;

end.
