{
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 1997-2012 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium.
              <francois.piette@overbyte.be>
}
unit IcsMailRcv2;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, 
  FMX.StdCtrls,
  FMX.Forms, FMX.Dialogs, FMX.Memo, FMX.Layouts, FMX.Memo.Types, FMX.Controls.Presentation, FMX.ScrollBox;

type
  TMessageForm = class(TForm)
    DisplayMemo: TMemo;
  end;

var
  MessageForm: TMessageForm;

implementation

{$R *.fmx}

end.
