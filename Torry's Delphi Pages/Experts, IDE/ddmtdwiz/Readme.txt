Macro Toolbar Display Wizard for Delphi 7
Information Sheet
Contents
Description
Compatibility
Installation
Update History
License and Disclaimer
About the Author
» Contents
Description
Delphi 7 introduced a macro recording toolbar that can be used to record and play back macros. The toolbar was to be displayed in the bottom left of the editor's status bar. However, the toolbar was hidden in the release version, leaving an empty space in the first panel of the status bar.

This wizard causes the hidden toolbar to be displayed in each open edit window. It also adds a new menu item to Delphi's View menu that toggles the toolbar on and off.

» Contents
Compatibility
The wizard works only with Delphi 7. The macro toolbar was not available on earlier versions of Delphi and later versions have a totally revised IDE.

» Contents
Installation
NOTE: This wizard will only work with Delphi 7. It will fail to compile / install on any other version of Delphi.

The wizard source files along with supporting documentation are supplied in a zip file. Before installing you need to extract all the files from the zip file. The following files will be extracted:

ChangeLog.txt – change log.
DDabMacroTBarDisplay.dpk – package file that can be used to install the wizard.
DDabMacroTBarDisplayWiz.pas – wizard source code.
MPL.txt – the Mozilla public license.
ReadMe.htm – this file.
VDDabMacroTBarDisplay.res – binary resource file containing version information for the package.
VDDabMacroTBarDisplay.vi – version information source for compilation using the DelphiDabbler Version Information Editor.
Emphasised files may be used in the installation, others are source or documentation files.

You can now proceed to install the wizard. There are two possible ways to do this:

By compiling and installing the provided package.
By adding the wizard source code to a pre-existing design package.
Installation using the provided package
If necessary copy DDabMacroTBarDisplay.dpk, DDabMacroTBarDisplayWiz.pas and VDDabMacroTBarDisplay.res to the folder where you wish to keep them.
Start Delphi and open DDabMacroTBarDisplay.dpk.
Compile DDabMacroTBarDisplay.dpk. In the package window the Install button should now be enabled. Click this button to install the package. Dismiss the message that informs you that the package has been installed.
Close the package window. Do not permit Delphi to save any changes.
If the package has installed successfully you should see the macro recording toolbar in the editor window's status bar. If there is no editor window open, open a file in Delphi to display the window. In addition, a Display Macro Toolbar menu item should have appeared at the foot of the View menu.
To uninstall the wizard choose the Components | Install Packages menu option. In the resulting dialog box, scroll down the list of design packages until you find "DelphiDabbler Macro Toolbar Display Wizard". Select the package, click the Remove button and give permission to remove when prompted. Click OK. The macro toolbar will disappear from editor windows and the menu option will be removed. If you have configured any custom tool buttons to work with the wizard, they too will be removed.

Installation into a pre-existing package
If you don't want to install the provided package, you can add the wizard to an existing design package, possibly the "user" package named dclusr.dpk. This package can be found in $(DELPHI)\Lib, where $(DELPHI) represents the path where Delphi 7 is installed. Ensure that the package "requires" designide.pas, which is also found in $(DELPHI)\Lib.

Decide upon a suitable location into which to install the wizard. This may be a subdirectory of your $(DELPHI)\lib folder.
Copy DDabMacroTBarDisplayWiz.pas to the chosen installation folder. Note that this is the only file you need when installing into an existing package.
Start Delphi then:
Choose the Component | Install Component menu option to display the Install Component dialog box. Ensure the Into existing package tab is selected. Enter the full path to DDabMacroTBarDisplayWiz.pas in the Unit file name edit box (use the adjacent Browse button to navigate to the file). Choose the required package from the Package description combo box, or by navigating to the package using Browse. Click OK to confirm your selections.
A confirmation dialog box may displayed, querying whether it is OK to rebuild and reinstall the package. Click Yes to proceed.
The package will now be built. If this succeeds the wizard will now be ready for use and editor windows will display the macro toolbar in the status bar. A Display Macro Toolbar menu item should have appeared at the foot of the View menu.
If you wish you can now delete DDabMacroTBarDisplayWiz.pas from the chosen installation folder.
To remove the wizard, simply open the package into which you installed the wizard, remove DDabMacroTBarDisplayWiz.pas and recompile the package. The macro toolbar will disappear from editor windows and the wizard's menu item and any custom tool buttons you added will be removed. Close the package, accepting that changes should be saved.

» Contents
Update History
A complete change log is provided in ChangeLog.txt.

» Contents
License and Disclaimer
This product, Macro Toolbar Display Wizard for Delphi 7, is subject to the Mozilla Public License Version 1.1 (the "License"). You may not use this Product except in compliance with the License. You may obtain a copy of the License at http://www.mozilla.org/MPL/ or see MPL.txt included in the download.

Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for the specific language governing rights and limitations under the License.

The Initial Developer of Macro Toolbar Display Wizard for Delphi 7 is Peter D Johnson (www.delphidabbler.com). Portions created by the Initial Developer are Copyright © 2007 Peter Johnson. All Rights Reserved.

All relevant trademarks are acknowledged.

» Contents
About the Author
I'm Peter Johnson – a hobbyist programmer living in Ceredigion in West Wales, UK, writing write mainly in Delphi. My programs are available for download from: http://www.delphidabbler.com/.

Please let me know if you have any comments about this wizard, if you have found a bug, or you want to suggest any updates.
This info sheet is copyright © 2007, P D Johnson, www.delphidabbler.com