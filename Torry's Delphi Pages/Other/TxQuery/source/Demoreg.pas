{**************************************************************************}
{   TxQuery DataSet                                                        }
{                                                                          }
{   The contents of this file are subject to the Mozilla Public License    }
{   Version 1.1 (the "License"); you may not use this file except in       }
{   compliance with the License. You may obtain a copy of the License at   }
{   http://www.mozilla.org/MPL/                                            }
{                                                                          }
{   Software distributed under the License is distributed on an "AS IS"    }
{   basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the}
{   License for the specific language governing rights and limitations     }
{   under the License.                                                     }
{                                                                          }
{   The Original Code is DemoReg.pas                                       }
{                                                                          }
{   The Initial Developer of the Original Code is Alfonso Moreno.          }
{   Portions created by Alfonso Moreno are Copyright (C) Alfonso Moreno.   }
{   All Rights Reserved.                                                   }
{                                                                          }
{   Alfonso Moreno (Hermosillo, Sonora, Mexico)                            }
{   email: luisarvayo@yahoo.com                                            }
{     url: http://www.ezsoft.com                                           }
{          http://www.sigmap.com/txquery.htm                               }
{                                                                          }
{   Contributor(s): Chee-Yang, CHAU (Malaysia) <cychau@gmail.com>          }
{                   Sherlyn CHEW (Malaysia)                                }
{              url: http://code.google.com/p/txquery/                      }
{                   http://groups.google.com/group/txquery                 }
{                                                                          }
{**************************************************************************}

unit DemoReg;

{$I XQ_FLAG.INC}
interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls, 
  Buttons, shellapi, ExtCtrls;

type
  TfrmRegister = class(TForm)
    OKBtn: TButton;
    Bevel1: TBevel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label7: TLabel;
    Memo1: TMemo;
    procedure Label2Click(Sender: TObject);
    procedure Label4Click(Sender: TObject);
    procedure Label5Click(Sender: TObject);
    procedure Label6Click(Sender: TObject);
    procedure Label7Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  procedure WriteToUs;
  procedure WriteToUs2;
  procedure WriteToUs3;
  procedure HomePage;

var
  frmRegister: TfrmRegister;

implementation

{$R *.DFM}

const
  RegistrationURL = 'https://secure.element5.com/register.html?productid=143123&language=English';
  WriteToUsURL='mailto: amoreno@sigmap.com';
  WriteToUsURL2='mailto: luisarvayo@yahoo.com';
  WriteToUsURL3='mailto: gismap@hmo.megared.net.mx';
  HomePageURL= 'http://www.sigmap.com/txquery.htm';

procedure JumpToURL(const s : string);
begin
  ShellExecute(Application.Handle, nil, PChar(s), nil, nil, SW_SHOW);
end;

procedure WriteToUs;
begin
  JumpToURL(WriteToUsURL);
end;

procedure WriteToUs2;
begin
  JumpToURL(WriteToUsURL2);
end;

procedure WriteToUs3;
begin
  JumpToURL(WriteToUsURL3);
end;

procedure HomePage;
begin
  JumpToURL(HomePageURL);
end;

procedure OnlineRegistration;
begin
  JumpToURL(RegistrationURL);
end;

procedure TfrmRegister.Label2Click(Sender: TObject);
begin
   OnlineRegistration;
end;

procedure TfrmRegister.Label4Click(Sender: TObject);
begin
   WriteToUs;
end;

procedure TfrmRegister.Label5Click(Sender: TObject);
begin
   WriteToUs2;
end;

procedure TfrmRegister.Label6Click(Sender: TObject);
begin
   WriteToUs3;
end;

procedure TfrmRegister.Label7Click(Sender: TObject);
begin
  HomePage;
end;

end.
