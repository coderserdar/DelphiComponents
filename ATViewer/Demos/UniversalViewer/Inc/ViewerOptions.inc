//Universal Viewer options:

{$define CMDLINE} //Enable handling of command line parameters.
                  //It enables:
                  // 1) Command line parsing
                  // 2) WM_COPYDATA interface
                  // 3) Options "Explorer context menu item" and "Run single instance only"
                  //Should be disabled, if UV form is used as additional modaless form.

{$define MEMINI}  //Enables TMemIniFile class usage, when appropriate.
                  //Should be enabled.

{$define HELP}    //Enables CHM help usage.
                  //This links additional "HHCtrl.ocx".
                  //Should be enabled.

//{$define MENX}  //Enables translation of key names ('Ctrl+', 'Alt+' etc);
                  //requires modification of Menus.pas:
                  //   type
                  //     TMenuKeyCap = ..
                  //   var
                  //     MenuKeyCaps: array[TMenuKeyCap] of string = ...
                  //Should be moved to "interface"

//{$define EXIF}  //Enables Exif library usage (Mitec Exif Reader)
                  //and command "View -- Image -- Show EXIF".
