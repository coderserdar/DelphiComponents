{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE. Based on work given by Louis S. Berman from
              BrainTree Ltd, lsb@braintree.com
Description:  MD5 is an implmentation for the MD5 Message-Digest Algorithm
              as described in RFC-1321
EMail:        francois.piette@pophost.eunet.be
              francois.piette@rtfm.be             http://www.rtfm.be/fpiette
Creation:     October 1997
Version:      1.01
Support:      Use the mailing list twsocket@rtfm.be See website for details.
Legal issues: Copyright (C) 1997, 1998 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium. Fax: +32-4-365.74.56
              <francois.piette@pophost.eunet.be>

              This software is provided 'as-is', without any express or
              implied warranty.  In no event will the author be held liable
              for any  damages arising from the use of this software.

              Permission is granted to anyone to use this software for any
              purpose, including commercial applications, and to alter it
              and redistribute it freely, subject to the following
              restrictions:

              1. The origin of this software must not be misrepresented,
                 you must not claim that you wrote the original software.
                 If you use this software in a product, an acknowledgment
                 in the product documentation would be appreciated but is
                 not required.

              2. Altered source versions must be plainly marked as such, and
                 must not be misrepresented as being the original software.

              3. This notice may not be removed or altered from any source
                 distribution.

Updates:
Sep 12, 1998 V1.01 Beautified source

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit MD5Test1;

interface

uses
  WinTypes, WinProcs, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, MD5, StdCtrls;

type
  TMD5TestForm = class(TForm)
    DataEdit: TEdit;
    MD5Button: TButton;
    MD5ResultLabel: TLabel;
    procedure MD5ButtonClick(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  MD5TestForm: TMD5TestForm;

implementation

{$R *.DFM}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMD5TestForm.MD5ButtonClick(Sender: TObject);
var
    Buffer : String;
begin
    Buffer := DataEdit.Text + #0;
    MD5ResultLabel.Caption := GetMD5(@Buffer[1], Length(Buffer) - 1);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.
