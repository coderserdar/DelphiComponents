unit OverbyteIcsMailQuTstView;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, ComCtrls, StdCtrls,
{$IF CompilerVersion > 23}
  System.UITypes,
{$IFEND}
  OverbyteIcsMailQueue, OverbyteIcsIniFiles, OverbyteIcsUtils;

type
  TViewQuForm = class(TForm)
    QuListView: TListView;
    Panel1: TPanel;
    doUpdate: TButton;
    doClose: TButton;
    LabelState: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure doUpdateClick(Sender: TObject);
    procedure doCloseClick(Sender: TObject);
    procedure QuListViewDblClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure UpdateList;
  end;

var
  ViewQuForm: TViewQuForm;

implementation

{$R *.dfm}

Uses OverbyteIcsMailQuTst1 ;

procedure TViewQuForm.doCloseClick(Sender: TObject);
begin
    Close ;
end;

procedure TViewQuForm.doUpdateClick(Sender: TObject);
begin
    UpdateList;
end;

procedure TViewQuForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
    IniFile : TIcsIniFile;
    section: string ;
begin
    ViewQuWinFlag := false ;
    IniFile := TIcsIniFile.Create(FIniFileName);
    with IniFile do
    begin
        section := 'ViewQuWindow' ;
        WriteInteger (section, 'Top', Top);
        WriteInteger (section, 'Left', Left);
        WriteInteger (section, 'Width', Width);
        WriteInteger (section, 'Height', Height);
    end ;
    IniFile.UpdateFile;
    IniFile.Free;

end;

procedure TViewQuForm.FormCreate(Sender: TObject);
var
    IniFile : TIcsIniFile;
    section: string ;
begin
    IniFile := TIcsIniFile.Create(FIniFileName);
    with IniFile do
    begin
        section := 'ViewQuWindow' ;
        Top := ReadInteger (section, 'Top', (Screen.Height - Height) div 2);
        Left := ReadInteger (section, 'Left', (Screen.Width - Width) div 2);
        Width := ReadInteger (section, 'Width', Width);
        Height := ReadInteger (section, 'Height', Height);
    end;
    IniFile.Free;
    ViewQuWinFlag := true ;

end;

procedure TViewQuForm.FormDestroy(Sender: TObject);
begin
    ViewQuWinFlag := false ;

end;

procedure TViewQuForm.QuListViewDblClick(Sender: TObject);
var
    item: string ;
    key: integer ;
begin
    if NOT DemoForm.IcsMailQueue1.Active then exit ;
    if QuListView.ItemIndex < 0 then exit ;
    item := QuListView.Items [QuListView.ItemIndex].Caption ;
    key := MessageDlg ('Confirm Delete Item ' + item +
                        ' From Mail Queue?', mtConfirmation, mbYesNoCancel, 0) ;
    if key <> mrYes then exit ;
    if DemoForm.IcsMailQueue1.UnQueueMail (atoi (item)) then
        LabelState.Caption := 'Queue State: Removed Item OK'
    else
        LabelState.Caption := 'Queue State: Failed to Remove Item' ;
end;

procedure TViewQuForm.UpdateList;
var
    I, J: integer ;
    MailQuItem: TMailQuItem ;
    S: string ;
begin
    if NOT DemoForm.IcsMailQueue1.Active then
    begin
        LabelState.Caption := 'Queue State: Not Running' ;
        exit ;
    end;
    QuListView.Items.Clear ;
    if DemoForm.IcsMailQueue1.MailTotItems = 0 then
    begin
        LabelState.Caption := 'Queue State: Empty' ;
        exit ;
    end;
    LabelState.Caption := 'Queue State: ' + IntToStr (DemoForm.IcsMailQueue1.MailTotItems) + ' Waiting Mail Items' ;
    for I := 0 to DemoForm.IcsMailQueue1.MailTotItems - 1 do
    begin
        MailQuItem := PMailQuItem (DemoForm.IcsMailQueue1.MailQuIdx [I])^ ;
        with QuListView.Items.Add, MailQuItem do
        begin
            Caption := IntToStr (ItemNr) ;
            SubItems.Add (LastResp) ;
            SubItems.Add (DateTimeToStr (NextAttemptDT)) ;
            SubItems.Add (XReceivers) ;
            SubItems.Add (XSender) ;
            SubItems.Add (Subject) ;
            SubItems.Add (IntToKbyte (BodySize)) ;
            SubItems.Add (ExtractFileName (FName)) ;
            SubItems.Add (DateTimeToStr (QueuedDT)) ;
            SubItems.Add (DateTimeToStr (LastAttemptDT)) ;
            SubItems.Add (IntToStr (AttemptNr)) ;
            SubItems.Add (IntToStr(Ord (Priority))) ;
            SubItems.Add (MailSmtpMethNames [SmtpMeth]) ;
            S := '' ;
            for J := 0 to MaxSmtpSrv - 1 do
            begin
                if J > 0 then S := S  + ', ' ;
                S := S + SmtpSrvs [J] ;
            end;
            SubItems.Add (S) ;
        end;
    end;


end;


end.
