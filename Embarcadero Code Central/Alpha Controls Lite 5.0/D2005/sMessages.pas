unit sMessages;
{$I sDefs.inc}
interface

uses Windows, Messages, Graphics, Controls, ExtCtrls, sConst;

var
  SM_ALPHACMD                   : integer;

const
  AC_SETNEWSKIN                 = 1;
  AC_REMOVESKIN                 = 2;
  AC_REFRESH                    = 3;
  AC_GETPROVIDER                = 4;
  AC_GETCACHE                   = 5;
  AC_ENDPARENTUPDATE            = 6;
  AC_CTRLHANDLED                = 7;
  AC_UPDATING                   = 8;
  AC_URGENTPAINT                = 9;
  AC_PREPARING                  = 10;
  AC_GETHALFVISIBLE             = 11;
  AC_SETTRANSBGCHANGED          = 12;  // Can be removed?
  AC_UPDATESECTION              = 13;
  AC_DROPPEDDOWN                = 14;
  AC_SETGRAPHCONTROL            = 15;  // Can be removed?
  AC_STOPFADING                 = 16;
  AC_SETBGCHANGED               = 17;  // Can be removed?
  AC_INVALIDATE                 = 18;
  AC_CHILDCHANGED               = 19;
  AC_SETCHANGEDIFNECESSARY      = 20;  // Defines BgChanged to True if required, with repainting if WParamLo = 1
  AC_GETCONTROLCOLOR            = 21;  // Returns color for the filling of children

implementation

uses sUtils, Dialogs, SysUtils;

initialization
  SM_ALPHACMD := RegisterWindowMessage('SM_ALPHACMD');
  if SM_ALPHACMD < $C000 then begin
//    ShowError('Window Message Registration failed!');
    SM_ALPHACMD := $A100;
  end;

finalization

end.
