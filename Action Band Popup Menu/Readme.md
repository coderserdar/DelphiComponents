
Delphi 7 (only)
TActionPopupMenu Component
Release Notes	September 2002
All software included with this update is subject to the Borland Software Corporation EULA as provided with Delphi 7.0. s
Delphi 7.0 did not ship with popup menu support for ActionBands and this component serves to compliment the menu functionality by implementing ActionBand style popup menus. TActionPopupMenu can be used as a replacement for TPopupMenu when a consistent menu style is desired. With this component and using ActionBands in Delphi 7 it is possible to develop applications that have a complete Microsoft Office XP menu style.

HOW TO INSTALL THE COMPONENT
Unzip the included files into a directory of your choice
Launch Delphi
Choose File|Open and select dclABPopup.dpk
From the package manager window click the install button to compile and install the component
You should now have successfully installed the TActionPopupMenu component onto the Additional tab of the component palette.
USAGE
TActionPopupMenu is a descendant of TPopupMenu so using this component is very similar to using a TPopupMenu.
From the component drop a TActionPopupMenu onto your form
Add either XPStyleActnCtrlsEx or StdStyleActnCtrlsEx to your uses clause depending on which style you would like to use in your application. If you want to support both styles then you need to add both units to your uses clause.
(The reason for this step is because the TActionPopupMenu is itself style independent and adding one of these units allows you to control which style popup menus you would like in your application)
Populate and use the menu as you would a normal TPopupMenu
NOTE: It is not necessary for the items in your menu be linked an action.

What controls the style that the TActionPopupMenuBar uses?
TActionPopupMenu is a style independent ActionBand popup menu. It has been implemented so that it can utilize any of the ActionBand menu styles. The component works specifically with the ActionBands features of Delphi 7.0 available in the Professional and Enterprise level products. If you do not use the ActionBands features of VCL then this component offers no additional benefit.

The popup menu will try to get its style from its ActionManager property if it is assigned, otherwise it will use the default (first registered) ActionBand style from the ActionBarStyles variable located in ActnMan.pas. Additionally, this component uses an internal ActionManager so that it can support visual form inheritance.

MORE INFORMATION
For more information on ActionBands and other useful Delphi tips make sure to take a look at http://www.geocities.com/delphihelp or the mirror site at http://homepages.borland.com/strefethen.

Copyright (c) Borland Software Corporation 2002