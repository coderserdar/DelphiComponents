{*******************************************************}
{                                                       }
{       Extension Library example of                    }
{       TELEvent, TELEventSender                        }
{                                                       }
{       (c) 2001, Balabuyev Yevgeny                     }
{       E-mail: stalcer@rambler.ru                      }
{                                                       }
{*******************************************************}

unit Unit2;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ELControls;

type
  TForm2 = class(TForm)
    Memo1: TMemo;
    ELEvent1: TELEvent;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Memo1Change(Sender: TObject);
    procedure ELEvent1Event(Sender: TObject; AEventSender: TELEventSender;
      AParam: Pointer);
  private
    FFileName: string;
    FModified: Boolean;
    FModifiedEx: Boolean;
    procedure UpdateOptions;
  public
    constructor CreateDocument(AOwner: TComponent; AFileName: string);
    procedure Save;
    procedure SaveAs(AFileName: string);
    procedure ResetModified;
    property FileName: string read FFileName;
    property Modified: Boolean read FModified;
  end;

var
  DocumentNum: Integer;
  FormWasClosed: Boolean;

implementation

uses Unit1, Unit3;

{$R *.dfm}

constructor TForm2.CreateDocument(AOwner: TComponent; AFileName: string);
begin
  Create(AOwner);
  Memo1.Lines.LoadFromFile(AFileName);
  FFileName := AFileName;
  FModified := False;
  FModifiedEx := False;
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  if FFileName <> '' then
    Caption := ExtractFileName(FFileName)
  else
  begin
    Caption := 'Document' + IntToStr(DocumentNum);
    Inc(DocumentNum);
    FModified := True;
  end;
  UpdateOptions;
end;

procedure TForm2.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
  if FModifiedEx then
    case MessageDlg('Save document ' + Caption, mtConfirmation,
      [mbYes, mbNo, mbCancel], 0) of
      mrYes:
        if FFileName <> '' then
          Form1.Save(Self)
        else
          if not Form1.SaveAs(Self) then Action := caNone;
      mrCancel:
        Action := caNone;
    end;
  FormWasClosed := Action = caFree;
end;

procedure TForm2.Save;
begin
  Memo1.Lines.SaveToFile(FFileName);
end;

procedure TForm2.SaveAs(AFileName: string);
begin
  Memo1.Lines.SaveToFile(AFileName);
  FFileName := AFileName;
end;

procedure TForm2.Memo1Change(Sender: TObject);
begin
  FModified := True;
  FModifiedEx := True;
end;

procedure TForm2.ResetModified;
begin
  FModified := False;
  FModifiedEx := False;
end;

procedure TForm2.UpdateOptions;
begin
  Memo1.Font.Assign(Form3.MemoFont);
  Memo1.Color := Form3.MemoBackgroundColor;
end;

procedure TForm2.ELEvent1Event(Sender: TObject;
  AEventSender: TELEventSender; AParam: Pointer);
begin
  { This procedure will be executed when event source component
    send events }
  UpdateOptions;
end;

initialization
  DocumentNum := 1;

end.
