unit MainWebModule;

interface

uses
  SysUtils, Classes, HTTPApp, plsLangMan;

type
  TfrmWM = class(TWebModule)
    procedure wHomePageAction(Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
    procedure wSetLangAction(Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
    procedure WebModuleCreate(Sender: TObject);
    procedure WebModuleDestroy(Sender: TObject);
  private
    { Private declarations }
    myLM:TLanguageManager;
  public
    { Public declarations }
    procedure LanguageChanged(Sender: TObject);
  end;

var
  frmWM: TfrmWM;

implementation

{$R *.dfm}

procedure TfrmWM.wHomePageAction(Sender: TObject; Request: TWebRequest; Response: TWebResponse;
  var Handled: Boolean);
begin
  Response.ContentType:='text/html';
  Response.Content:=
    '<html><body>'+
      '<p>'+
      myLM.LangText('str_sel_lang') + ' ' +
      '<a href="setlang?langid=en">' + myLM.LangText('lng_english')+'</a> | '+
      '<a href="setlang?langid=cs">' + myLM.LangText('lng_czech')+'</a>'+
      '</p>'+
    '</body></html>';
  Handled:=True;
end;

procedure TfrmWM.wSetLangAction(Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
begin
  myLM.LanguageCode:=Request.QueryFields.Values['langid'];
  wHomePageAction(Sender,Request,Response,Handled);
end;

procedure TfrmWM.LanguageChanged(Sender: TObject);
begin
  if not myLM.LangVCL(Self) then
  begin
    // Handle the language setup error. You can use myLM.LastError info.
  end;
  // inform other dependent objects/modules that language has changed (directly)
  // WebModule2.LanguageChanged(Sender);
  // WebModule3.LanguageChanged(Sender);
  // ...
end;

procedure TfrmWM.WebModuleCreate(Sender: TObject);
begin
  myLM:=TLanguageManager.Create;
  myLM.Folder:='Langs';
  myLM.OnLanguageChanged:=LanguageChanged;

  myLM.LanguageCode:=GetDefaultLangCode;
  if myLM.LanguageCode='' then
    myLM.LanguageCode:='en';
end;

procedure TfrmWM.WebModuleDestroy(Sender: TObject);
begin
  myLM.Free;
end;

end.
