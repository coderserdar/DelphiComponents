unit Unit_Constants;

interface

uses
    Windows;

const
    REG_WINDOWS_COUNT = 'Count';
    REG_INTERVAL      = 'TimerInterval';
    REG_MODALRESTORE  = 'RestoreOnModal';
    REG_CAPTIONBUTTON = 'CaptionButton';
    REG_DELPHILOADINIT  = 'InitOnDelphiLoad';
    REG_DELPHICLOSEKILL = 'KillOnDelphiClose';
    REG_XPSTYLE       = 'XPStyle';
    REG_SOUND         = 'Sound';

    REG_DEFVAL_INTERVAL      = 750;
    REG_DEFVAL_MODALRESTORE  = TRUE;
    REG_DEFVAL_CAPTIONBUTTON = TRUE;
    REG_DEFVAL_DELPHILOADINIT  = FALSE;
    REG_DEFVAL_DELPHICLOSEKILL = TRUE;
    REG_DEFVAL_XPSTYLE       = FALSE;
    REG_DEFVAL_SOUND         = FALSE;

    RES_TEMPLATE_COUNT   = 100;
    RES_TEMPLATE_DEFAULT = 1000;
    RES_TEMPLATE_BASE    = 101;
    RES_TEMPLATE_ITEM_BASE = 1001;
    RES_MENUITEMS        = 80;

    MENU_LOCKEXPAND = 1;
    MENU_LOCKSHRINK = 2;
    MENU_UNLOCK     = 3;
    MENU_TYPECAPTION    = 4;
    MENU_TYPESHORTCAPTION = 5;
    MENU_TYPEULTRATHIN  = 6;
    MENU_UTTOP        = 7;
    MENU_UTLEFT       = 8;
    MENU_UTBOTTOM     = 9;
    MENU_UTRIGHT      = 10;
    MENU_UTSEPARATOR  = 11;
    MENU_PROPERTY   = 12;
    MENU_UNLOAD     = 13;

    TIMER_ID        = 1;
    TIMER_MIN_VALUE = 250;
    MIN_HEIGHT      = 5;
    ULTRATHIN_MINWEIGHT = 1;
    ULTRATHIN_DEFWEIGHT = 2;

    AH_NAME         = 'AutoHide';
    AH_AUTHOR       = 'Ahmoy_Law';
    AH_WIZARD       = AH_NAME + 'Wizard';
    AH_DESCRIPTIONS = AH_WIZARD + ' for Delphi IDE by ' + AH_AUTHOR;
    AH_SIGNATURE    = AH_AUTHOR + '.' + AH_WIZARD;

    ROOT_KEY        = '\SOFTWARE\' + AH_AUTHOR + '\AutoHide\';
    ROOT_KEYINCLUDE = ROOT_KEY + 'Windows';

    ULTRATHINSHRINKLOCK_ERROR = AH_NAME + ': Cannot shrink lock on a ultra thin type!';

    SIGNATURE_HEAD  = '[';
    SIGNATURE_TAIL  = ']';

    // this chras must be an unique char!
    CHAR_LT_UNLOCK  = 'N';
    CHAR_LT_EXPAND  = 'E';
    CHAR_LT_SHRINK  = 'S';
    CHAR_ST_CAPTION = 'c';
    CHAR_ST_SHORTCAPTION = 's';
    CHAR_ST_ULTRATHIN = 'u';
    CHAR_UTD_TOP    = 't';
    CHAR_UTD_LEFT   = 'l';
    CHAR_UTD_BOTTOM = 'b';
    CHAR_UTD_RIGHT  = 'r';
    CHAR_SA_NONE    = 'D';
    CHAR_SA_TOP     = 'P';
    CHAR_SA_TOPMOST = 'M';
    CHAR_UTSA_NONE  = 'd';
    CHAR_UTSA_TOP   = 'p';
    CHAR_UTSA_TOPMOST = 'm';
    CHAR_IGNORE     = 'X';

    EXECUTE_SOUND   = MB_OK;
    CAP_BUTTON_SEPARATOR = 2;

implementation

end.
