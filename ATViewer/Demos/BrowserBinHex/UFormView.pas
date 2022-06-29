//{$define STREAMS} //See Readme.txt

unit UFormView;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  VirtualExplorerTree, VirtualTrees, StdCtrls, ExtCtrls,
  {$ifdef STREAMS}TntClasses,{$endif}
  ATBinHex;

type
  TFormView = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    TreeEx: TVirtualExplorerTreeview;
    LVEx: TVirtualExplorerListview;
    Viewer: TATBinHex;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    chkModeText: TRadioButton;
    chkModeBinary: TRadioButton;
    chkModeHex: TRadioButton;
    chkModeUnicode: TRadioButton;
    chkModeUHex: TRadioButton;
    Label2: TLabel;
    chkOEM: TCheckBox;
    chkWordWrap: TCheckBox;
    chkEnabled: TCheckBox;
    chkNonPrint: TCheckBox;
    procedure LVExChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure chkModeBinaryClick(Sender: TObject);
    procedure chkModeHexClick(Sender: TObject);
    procedure chkModeUnicodeClick(Sender: TObject);
    procedure chkModeTextClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure chkWordWrapClick(Sender: TObject);
    procedure chkOEMClick(Sender: TObject);
    procedure chkEnabledClick(Sender: TObject);
    procedure chkModeUHexClick(Sender: TObject);
    procedure chkNonPrintClick(Sender: TObject);
  private
    { Private declarations }
    {$ifdef STREAMS}
    FStream: TTntFileStream;
    {$endif}
    FLoad: boolean;
  public
    { Public declarations }
    procedure UpdateView(en: boolean);
  end;

var
  FormView: TFormView;

implementation

uses
  ATxSProc, ATxFProc, ATxCodepages,
  VirtualShellUtilities;

{$R *.DFM}

procedure TFormView.LVExChange(
  Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  NS: TNamespace;
  S: WideString;
  OK: boolean;
begin
  with LVEx do
    if ValidateNameSpace(GetFirstSelected, NS) then
      begin
      S:= NS.NameForParsing;
      OK:= IsFileExist(S);

      if OK then
        {$ifdef STREAMS}
        begin
        //Free current stream
        if Assigned(FStream) then
          begin
          Viewer.OpenStream(nil, false);
          FStream.Free;
          FStream:= nil;
          end;
        //Create and open new stream
        try
          FStream:= TTntFileStream.Create(S, fmOpenRead or fmShareDenyNone);
          OK:= Viewer.OpenStream(FStream);
        except
          if Assigned(FStream) then
            begin
            FStream.Free;
            FStream:= nil;
            end;
          OK:= false;
        end;
        end;

        {$else}
        OK:= Viewer.Open(S);
        {$endif}
       
      Viewer.Visible:= OK;
      UpdateView(OK);
      end;
end;

procedure TFormView.chkModeTextClick(Sender: TObject);
begin
  if not FLoad then
    begin
    Viewer.Mode:= vbmodeText;
    UpdateView(true);
    end;
end;

procedure TFormView.chkModeBinaryClick(Sender: TObject);
begin
  if not FLoad then
    begin
    Viewer.Mode:= vbmodeBinary;
    UpdateView(true);
    end;
end;

procedure TFormView.chkModeHexClick(Sender: TObject);
begin
  if not FLoad then
    begin
    Viewer.Mode:= vbmodeHex;
    UpdateView(true);
    end;
end;

procedure TFormView.chkModeUnicodeClick(Sender: TObject);
begin
  if not FLoad then
    begin
    Viewer.Mode:= vbmodeUnicode;
    UpdateView(true);
    end;
end;

procedure TFormView.chkModeUHexClick(Sender: TObject);
begin
  if not FLoad then
    begin
    Viewer.Mode:= vbmodeUHex;
    UpdateView(true);
    end;
end;

procedure TFormView.UpdateView(en: boolean);
begin
  FLoad:= true;

  chkModeText.Checked:= Viewer.Mode = vbmodeText;
  chkModeBinary.Checked:= Viewer.Mode = vbmodeBinary;
  chkModeHex.Checked:= Viewer.Mode = vbmodeHex;
  chkModeUnicode.Checked:= Viewer.Mode = vbmodeUnicode;
  chkModeUHex.Checked:= Viewer.Mode = vbmodeUHex;

  chkModeText.Enabled:= en;
  chkModeBinary.Enabled:= en;
  chkModeHex.Enabled:= en;
  chkModeUnicode.Enabled:= en;
  chkModeUHex.Enabled:= en;

  chkWordWrap.Enabled:= en and (Viewer.Mode in [vbmodeText, vbmodeUnicode]);
  chkOEM.Enabled:= en and not (Viewer.Mode in [vbmodeUnicode, vbmodeUHex]);
  chkEnabled.Enabled:= en;
  chkNonPrint.Enabled:= en;

  FLoad:= false;
end;

procedure TFormView.FormShow(Sender: TObject);
var
  fn: WideString;
begin
  UpdateView(false);

  fn:= SExtractFileDir(ParamStr(0));
  if IsDirExist(fn) then
    begin
    LVEx.RootFolder:= rfCustom;
    LVEx.RootFolderCustomPath:= fn;
    end;
end;

procedure TFormView.FormCreate(Sender: TObject);
begin
  {$ifdef STREAMS}
  FStream:= nil;
  {$endif}
  FLoad:= false;
  TreeEx.VirtualExplorerListview:= LVEx; //For compatability with VST2.0 installed in IDE
end;

procedure TFormView.FormDestroy(Sender: TObject);
begin
  {$ifdef STREAMS}
  if Assigned(FStream) then
    begin
    Viewer.OpenStream(nil);
    FStream.Free;
    FStream:= nil;
    end;
  {$endif}
end;

procedure TFormView.chkOEMClick(Sender: TObject);
begin
  if chkOEM.Checked then
    Viewer.TextEncoding:= vencOEM
  else
    Viewer.TextEncoding:= vencANSI;
end;

procedure TFormView.chkWordWrapClick(Sender: TObject);
begin
  Viewer.TextWrap:= chkWordWrap.Checked;
end;

procedure TFormView.chkEnabledClick(Sender: TObject);
begin
  Viewer.Enabled:= chkEnabled.Checked;
end;

procedure TFormView.chkNonPrintClick(Sender: TObject);
begin
  Viewer.TextNonPrintable:= chkNonPrint.Checked;
end;

end.
