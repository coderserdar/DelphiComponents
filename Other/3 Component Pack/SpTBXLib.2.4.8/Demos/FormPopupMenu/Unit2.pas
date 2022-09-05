unit Unit2;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, CommCtrl, ExtCtrls, SpTBXFormPopupMenu, StdCtrls;

type
  TForm2 = class(TForm)
    MonthCalendar1: TMonthCalendar;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    FOldWndProc: TWndMethod;
    procedure NewWndProc(var Message: TMessage);
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

procedure TForm2.FormCreate(Sender: TObject);
begin
  // We can't use the MonthCalendar.OnClick event because
  // it gets fired even when clicking on the Prev/Next month
  // buttons. We have to subclass it and handle the
  // MCN_SELECT notification
  MonthCalendar1.DoubleBuffered := True;
  FOldWndProc :=  MonthCalendar1.WindowProc;
  MonthCalendar1.WindowProc := NewWndProc;
end;

procedure TForm2.NewWndProc(var Message: TMessage);
begin
  FOldWndProc(Message); // default WndProc
  if Message.Msg = CN_NOTIFY then
    if TWMNotify(Message).NMHdr^.code = MCN_SELECT then begin
      // Inform the ActiveFormPopupMenu that a selection was made.
      if Assigned(ActiveFormPopupMenu) then
        ActiveFormPopupMenu.ClosePopup(True);
    end;
end;

end.
