Keyboard Macro Manager v1.0 for Delphi 5/6/7

By Daniel Cunningham (aka X42 @ EFNet #Delphi)
4/5/2003


DESCRIPTION

Delphi IDE Enhancement for saving / restoring keyboard macros.

This enhancement is totally free and I disclaim any responsibility
for any damages it might cause.

Enjoy!  :-)


OPERATION

Very simple to use.  Record a macro as usual with Ctrl+Alt+R to start 
and again Ctrl+Alt+R to stop.  Press Ctrl+Alt+S to save the macro to 
disk.  I suggest creating a folder specifically for your macros.  

Press Ctrl+Alt+O to load a macro you have previously saved and then
you can play as usual by pressing Ctrl+Alt+P.

Here's a neat trick for your macros:  Use the incremental search 
feature in your macros.  Press Ctrl+E (in default keyboard mapping), 
type what you're looking for, and press Esc to end the search.  Unlike
the standard search, this does not cause the recording to abort.  By
using incremental search and Cut/Copy/Paste keyboard commands you 
can make pretty powerful macros.

Also, the last folder you saved to or opened from will be saved in 
your registry and used for future save/load operations.


INSTALLATION

Open the appropriate package for your version of Delphi.  On the 
Package dialog, click Compile and then click Install and away you go!

If you want to use different shortcut keys, edit KeyMacroMgr.pas
in the method TKeyboardMacroManager.BindKeyboard.  Very simple.


REMOVAL

You should now see a package named "Keyboard Macro Manager" in the
packages list under Component|Install Packages.  Click Remove to
uninstall.