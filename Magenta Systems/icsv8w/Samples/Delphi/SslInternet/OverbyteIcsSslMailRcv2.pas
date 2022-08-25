{
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 1997-2010 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium. Fax: +32-4-365.74.56
              <francois.piette@overbyte.be>
}
unit OverbyteIcsSslMailRcv2;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
{$IFDEF CLR}
  System.ComponentModel,
{$ENDIF}
  StdCtrls;

type
  TMessageForm = class(TForm)
    DisplayMemo: TMemo;
  end;

var
  MessageForm: TMessageForm;

implementation

{$R *.DFM}

end.
