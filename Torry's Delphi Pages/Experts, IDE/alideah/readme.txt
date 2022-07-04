This software is provided 'as-is', without any express or implied warranty.
In no event shall the author be held liable for any damages arising from the use
of this software.

License: Freeware, free to use and distribute as long as the original source
stays intact. free to modify as long as all modifications are sent back to me.

IDEAutohide
Copyright (c)  2002 ahmoy law
e-mail:  ahmoy_law@hotmail.com
         ahmoy_law@yahoo.com
Version: 1.00a

Any suggestions, modifications, bugs or anything! kindly please send an email to me :)

p/s: hidup oghe kelate!!!




Installation Notes
------------------
1. Open the IDEAutohide.dpk, compile and install it. For the C++ Builder users, please go to step 3.
2. For Delphi 6+ compiler may produces error like "Required package 'vcl50' not found" when compiling the package. Your compiler may offers an automatic translation or you can change it manually by replacing the vcl50 in the IDEAutohide.dpk with your current Vcl version (i.e.: Delphi 6 - vcl60, Delphi 7 - vcl).  
3. If you decided to create the package from scratch (from the Component menu, select Install Component. Then from the Install Package dialog box, select the Into New Package option), do not name it the new package as "Autohide" since this will makes Delphi auto creates a "Autohide.res" resource file and overwrites the original resource file.  Files needed in the package are 'main.pas', 'Unit_AutoHide.pas', 'Unit_WndProp.pas', 'Unit_FormMainClass.pas', 'Unit_FormAbout.pas', 'Unit_FormAddClass.pas', 'Unit_Misc.pas' and 'Unit_Constants.pas'.
4. If you managed to install the IDEAutohide package, you should spot Autohide option  under your Delphi Help menu. 


User Interface Tips
-------------------
1. Left click on the caption button to bring up the options menu. Double clicking it to toggle the window lock option (from Unlock to ExpandLock and vice versa).
2. If your Ultrathin window positioned at the screen edge is hidden when you changed the Windows Desktop Appearance (for example: from Window Standard to High Contrast Extra Large), set the UltraThin width property to a higher value.
3. If you encountered a window size problem on the Object Inspector window (or any windows) after restarting Delphi, enable the "Shutdown engine on Delphi closing" option in Autohide properties. Delphi IDE saves the windows size and position as well as on WM_DESTROY. As a result, the size and position recorded is on a shrunk window, not the actual size before it shutdowns. "Shutdown engine on Delphi closing" option will restore all windows size to its original size on WM_CLOSE.  The limitation on this option is that it wills unload Autohide no matter whether user selects Yes, No or Cancel options on receiving WM_CLOSE.
4. To restore all window size to its original size on a specific modal dialog box (for example: you open Editor Options and at the same time you want to see the Delphi Editor window), turn on the "Restore window size on a modal dialog box" option in the Autohide's properties.


Bug List
--------
1. Caption button on the Delphi Main Editor form is removed when user selected a different tab (dunno what windows message is triggered)
2. Caption button state is not updated if mouse moved too fast (WM_NCHITTEST problem)
3. Delphi could not correctly position the form during startup if " Initialize engine on Delphi loading" option is enabled
