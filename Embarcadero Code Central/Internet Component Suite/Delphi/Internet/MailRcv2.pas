{
EMail:        francois.piette@pophost.eunet.be    francois.piette@rtfm.be
              http://www.rtfm.be/fpiette
Support:      Use the mailing list twsocket@rtfm.be See website for details.
Legal issues: Copyright (C) 1997, 1998 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium. Fax: +32-4-365.74.56
              <francois.piette@pophost.eunet.be>
}
unit MailRcv2;

interface

uses
  WinTypes, WinProcs, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TMessageForm = class(TForm)
    DisplayMemo: TMemo;
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  MessageForm: TMessageForm;

implementation

{$R *.DFM}

end.
