{**********************************************************}
{                                                          }
{  Devrace Extension Library example of                    }
{  TELEvent, TELEventSender                                }
{                                                          }
{  Copyright (c) 2001, Balabuyev Yevgeny                   }
{  Contact: yebalabuyev@devrace.com                        }
{                                                          }
{ -------------------------------------------------------- }
{  ExtLib home page      : http://www.devrace.com/extlib   }
{  ExtLib support e-mail : extlib@devrace.com              }
{ -------------------------------------------------------- }
{                                                          }
{  Please see the file License.txt for full license        }
{  information                                             }
{                                                          }
{**********************************************************}

unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ELControls, ImgList, Menus, ExtCtrls, StdActns,
  ActnList;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Bevel2: TBevel;
    Label1: TLabel;
    ActionList1: TActionList;
    actNew: TAction;
    actSave: TAction;
    actOpen: TAction;
    actClose: TAction;
    actCloseAll: TAction;
    actExit: TAction;
    actOptions: TAction;
    actWindowCascade: TWindowCascade;
    actWindowTileHorizontal: TWindowTileHorizontal;
    actWindowTileVertical: TWindowTileVertical;
    actWindowMinimizeAll: TWindowMinimizeAll;
    actWindowArrange: TWindowArrange;
    actSaveAs: TAction;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    New1: TMenuItem;
    Open1: TMenuItem;
    Close1: TMenuItem;
    SaveAs1: TMenuItem;
    Close2: TMenuItem;
    CloseAll1: TMenuItem;
    N1: TMenuItem;
    Exit1: TMenuItem;
    Window1: TMenuItem;
    Cascade1: TMenuItem;
    ileHorizontally1: TMenuItem;
    ileVertically1: TMenuItem;
    Arrange1: TMenuItem;
    MinimizeAll1: TMenuItem;
    N2: TMenuItem;
    actOptions1: TMenuItem;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    ELStringList1: TELStringList;
    ELEvent1: TELEvent;
    procedure actNewExecute(Sender: TObject);
    procedure actOpenExecute(Sender: TObject);
    procedure actSaveUpdate(Sender: TObject);
    procedure actSaveExecute(Sender: TObject);
    procedure actSaveAsUpdate(Sender: TObject);
    procedure actSaveAsExecute(Sender: TObject);
    procedure actCloseUpdate(Sender: TObject);
    procedure actCloseExecute(Sender: TObject);
    procedure actCloseAllUpdate(Sender: TObject);
    procedure actCloseAllExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure actExitExecute(Sender: TObject);
    procedure actOptionsExecute(Sender: TObject);
    procedure ELEvent1Event(Sender: TObject; AEventSender: TELEventSender;
      AParam: Pointer);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure Save(AForm: TForm);
    function SaveAs(AForm: TForm): Boolean;
    function CloseAll: Boolean;
    procedure Start;
  end;

var
  Form1: TForm1;

implementation

uses Unit2, Unit3;

{$R *.dfm}

procedure TForm1.actNewExecute(Sender: TObject);
begin
  TForm2.Create(Application);
end;

procedure TForm1.actOpenExecute(Sender: TObject);
begin
  if OpenDialog1.Execute then
    TForm2.CreateDocument(Application, OpenDialog1.FileName);
end;

procedure TForm1.actSaveUpdate(Sender: TObject);
begin
  actSave.Enabled := (ActiveMDIChild <> nil) and
    (TForm2(ActiveMDIChild).FileName <> '') and TForm2(ActiveMDIChild).Modified;
end;

procedure TForm1.actSaveExecute(Sender: TObject);
begin
  Save(ActiveMDIChild);
end;

procedure TForm1.actSaveAsUpdate(Sender: TObject);
begin
  actSaveAs.Enabled := (ActiveMDIChild <> nil) and
    TForm2(ActiveMDIChild).Modified;
end;

procedure TForm1.actSaveAsExecute(Sender: TObject);
begin
  SaveAs(ActiveMDIChild);
end;

procedure TForm1.actCloseUpdate(Sender: TObject);
begin
  actClose.Enabled := (ActiveMDIChild <> nil);
end;

procedure TForm1.actCloseExecute(Sender: TObject);
begin
  ActiveMDIChild.Close;
end;

procedure TForm1.actCloseAllUpdate(Sender: TObject);
begin
  actCloseAll.Enabled := (MDIChildCount > 0);
end;

procedure TForm1.actCloseAllExecute(Sender: TObject);
begin
  CloseAll;
end;

procedure TForm1.Save(AForm: TForm);
begin
  TForm2(AForm).Save;
end;

function TForm1.SaveAs(AForm: TForm): Boolean;
begin
  if TForm2(AForm).FileName <> '' then
    SaveDialog1.FileName := TForm2(AForm).FileName
  else
    SaveDialog1.FileName := TForm2(AForm).Caption;
  Result := SaveDialog1.Execute;
  if Result then
    TForm2(AForm).SaveAs(SaveDialog1.FileName);
end;

function TForm1.CloseAll: Boolean;
var
  LI: Integer;
begin
  Result := True;
  for LI := MDIChildCount - 1 downto 0 do
  begin
    MDIChildren[LI].Close;
    if not FormWasClosed then
    begin
      Result := False;
      Break;
    end;
  end;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if not CloseAll then Action := caNone;
end;

procedure TForm1.actExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TForm1.actOptionsExecute(Sender: TObject);
begin
  Form3.ShowModal;
end;

procedure TForm1.Start;
var
  LF: TForm2;
begin
  if Form3.AutoOpenNewDoc then
  begin
    LF := TForm2.Create(Application);
    if Form3.ShowAnnotation then
    begin
      LF.Memo1.Lines.Assign(ELStringList1.Lines);
      LF.ResetModified;
      LF.Caption := 'Demo annotation';
    end;
  end;
  Color := Form3.MainFormBackColor;
end;

procedure TForm1.ELEvent1Event(Sender: TObject;
  AEventSender: TELEventSender; AParam: Pointer);
begin
  { This procedure will be executed when event source component
    send events }
  Color := Form3.MainFormBackColor;
end;

initialization
  ForceCurrentDirectory := False;

end.
