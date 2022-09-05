{
 SXMedia  Components - Beta 1
 --------------------------------
 Copyright 1999 Dean Ellis
 http://www.sillex.freeserve.co.uk

 This unit is part of the SXMedia Component Set. This code is
 supplied as is with no guarantees and must be used at your own
 risk.

 No modifications to this code must be made without the express
 permission of the author. Please report any problems to
 support@sillex.freeserve.co.uk

 You may use these components to create any freeware/shareware
 applications that you wish. If the components are to be used in
 a commercail product then credit for developement of these components
 should be given.

 Credits :

 Developer : Dean Ellis
}
unit sxabout;

interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls;

type
  TAboutBox = class(TForm)
    InfoContainer: TPanel;
    Version: TLabel;
    Copyright: TLabel;
    OKButton: TButton;
    Author: TLabel;
    WebSite: TLabel;
    Credits: TLabel;
    LogoContainer: TPanel;
    ProgramIcon: TImage;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.DFM}

end.

