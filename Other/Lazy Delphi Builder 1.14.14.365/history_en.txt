Changelog:

Please report bugs, ask questions, share your experience via e-mail LazyDelphiBuilder@gmail.com (or use menu item: Help->Email bug report to author...). 

08.01.2017. Lazy Delphi Builder 1.14.14.365:
* support: Delphi version after 10.1 (dcc v 32.0, IDE - 25, wild guess)
* support: added Delphi Berlin support
* bugfix: drag and drop Single file to a tree cause error (file is not a folder)
* feature: during the build, support environment variable $(config) that will be replaced with 'debug' or 'release' (overwriting predefined value)
* bugfix: for projects build both release & debug version (same as for packages). It fixes errors, if compiling projects and packages and only "Build debug version" is checked: 
  *.1) Fatal: F2051 Unit UnitName.pas was compiled with a different version of some class 
  *.2) Cannot create UnitName.dcu in ReleaseDcu directory, because directory does not exist (was not created)
  *.3) Cannot find UnitNameFromPackage.dcu, because for projects this file was searched in non-existing ReleaseDcu directory
* bugfix: shell context menu does not work for paths containing \..\
* support: Custom installation editor - added Registry ids for Delphi 2009, 2010, XE8, 10 Seattle, 10.1 Berlin
* bugifx: portable installation editor: fixed exception dialog if rsvars.bat file was not found
* bugfix: fixed EListError List index out of bounds (0) if installation loaded from LazyDBP was not found (0 installations)
* support: set cursor busy when loading Installed packages preset

06.04.2016. Lazy Delphi Builder 1.14.14.364 alpha (untested):
* support: updated JCL
** fixed bug with MS Build variables (merge instead of overwrite)
** fixed bug with -NSnamespace being passed to dcc32 as -N"Snamespace"
** use short path as current when launch dcc32
** better support for Seattle 10
** rename RS 10 to 10

06.04.2016. Lazy Delphi Builder 1.14.14.362:
* fixed critical: range check error when adding new path to Lazy paths (thanks to Pavel Labutin for report)
* fixed: Lazy Paths editor: non-existing directories were not detected after item edit or add
* feature: Lazy Paths editor: added command  Replace selected with env var (auto) - shortens path by replacing it's part with Lazy or IDE environment variable
* support: lazy ide paths editor: added popup menu command Edit (same as double click)
* support: Library paths editor: disable drag for Lazy Paths
* support: IDE paths editor: disable double click for IDE & Windows paths

01.04.2016. Lazy Delphi Builder 1.14.14.360:
* fixed: BUILTIN_PROFILE_DIR did not worked if Lazy profile was loaded as relative path
* fixed: relative paths (to current directory) might not work as expected if path was empty
* fixed: console version fail to return ERRORLEVEL if LDB failed to delete directory
* fixed: GUI: MRU profiles. Loaded profile was not moved to beginning of list.
* fixed: GUI: terribly slow packages Tree update after delete
* feature: new cmdParam /MaxProblemCountToStop number
* support: cmdParam alias /cod for /CleanOutDirs
* support: output BUILTIN_PROFILE_DIR env var name in /debug mode

26.03.2016. Lazy Delphi Builder 1.14.14.356:
* critical: /CompilerOptions command line switch was broken in previous builds (passed value to dcc32 using semicolon as separator instead of space)
* fixed: warning about Files not found was not shown when user loaded profile
* fixed: .. was stripped from relative paths
* fixed: Summary message about Bpl files: %s - was not expanded
* fixed: Build dialog: Show params for custom Dcc32 if provided instead of default
* feature: avoid UI blocking. replace FTHread.WaitFor with MsgWaitForMultipleObjects
* feature: added /noop switch that bypasses all actual operations with files
* feature: Preferences dialog with Checkbox "Use recycle bin for deleted files" (MainMenu -> File -> Preferences..)
* feature: Presets for Installed packages
* feature: ui: support drag and drop for .LazyDBP file (only 1 single .LazyDbp file can be dropped)
* feature: add selected source files to IDE Browsing path (paths are replaced with env vars) via MainMenu -> Edit -> Add to IDE Library Browsing Path
* feature: add selected source files to IDE Library Search path (paths are replaced with env vars) via MainMenu -> Edit -> Add to IDE Library Search Path
* feature: relative paths support: added internal variable $(BUILTIN_PROFILE_DIR) which always points to current LazyDBP file location
* feature: added cmdParam /buildReleaseDcu
* feature: Console: added cmdparam  /ResFileMasks to allow override resource file masks from LazyDelphiBuilderGUI.ini file
* feature: for release build add DEFINE RELEASE, for Debug build add define DEBUG
* support: ui: show non-existing Lazy/IDE paths in gray Text (Library paths dialog)
* support: ui: hotkeys for Preset Load/Save on Build dialog
* support: ui: main menu -> Tools -> Portable installations.. new command
* support: ui: Scan files - Search folders ui updated
* support: add dragged and dropped folders to the list of Folders to scan
* support: ignore empty presets on load
* support: hotkeys Ctrl+O & Ctrl+S in presets combobox will load/save preset, not the profile!
* support: added short alias /urb for /UseRecycleBin
* support: Set default monitor to dmMainForm
* support: Print Loading Profile "real file path" instead of Loading profile "" on startup

28.09.2015. Lazy Delphi Builder 1.12.12.337:
* fixed: blocker - console version ignored environment variables provided via /ev cmdparam

23.09.2015. Lazy Delphi Builder 1.12.12.333:
* feature: Relative directories support (simply remove all parts with c:\Directory1\Directory2)
* feature: in-place rename in tree (no Undo support)
* feature: build log: added hotkeys Alt+Up and Alt+Down for errors navigation
* feature: MainMenu -> Help -> added command "Enable debug mode" with hotkey Ctrl+Alt+D
* feature: UI: main menu -> Build: added menu items "Continue build", "Test build", "Undo build"
* feature: UI: hotkeys: Removed Alt+C for Compile... (use F9). Added F8 for Continue. F11 for Test build (instant)
* feature: when Dragging file to Lazy Delphi Builder, automatically activate tab with Tree
* feature: UI: Tree popup menu item "Replace...". added hotkeys Ctrl+R, Ctrl+H
* fixed: "Build Release Dcu" option was not loaded from LazyDBP file. (thanks to Labutin Pavel for report)
* fixed: critical bug "Lazy Delphi Builder search paths" were not loaded from LazyDBP profile
* fixed: /baseOutDir cmdparam was ignored (actually, it was used only when .lazyDBP file contained empty BaseOutDir key)
* fixed: rsvars variables sometime cannot be read. replace rsvars.bat run with rsvars.bat parsing
* fixed: ensure that User-specified compiler switches (-$Letter+-) will override any values provided by Lazy Builder (f.e. for Debug mode -$D+ and $L+) are used. Before - these switches were duplicated.
* fixed: unknown /verbose parameter in console version
* fixed: Console: can corrupt active Cmd window (when run in /tb mode with invalid profile) and leave temporary created directory undeleted
* fixed: cmdparam /q was not supported 
* fixed: ReplaceDialog did not closed with Escape if focus in JvComboEdit
* fixed: UI: when user cancels build that contain any errors, progress bar is painted in RED, but must be YELLOW.
* fixed: Installation selection combo is painted incorrectly after Installations filter is changed
* fixed: clear filter on Clear profile/Load profile
* support: added /v cmdparam alias for /verbose
* support: gui: Edit environment variables & search paths from main menu. (mainmenu -> Tools -> ...)
* support: ignore Delphi edition when looking for Installation defined in LazyDBP profile. (4 words are ignored: Enterprise, Architect, Professional, Portable), which means that Delphi XE Architect will match "Delphi XE Professional" key
* support: breaking change: changed 64bit win installation name. From: Rad studio xeX Edition 64 To Rad studio xeX Edition win64
* support: removed BallonHint because of multiple problems with it in XE (slow drawing, no hint is shown in VirtualTreeView header etc)
* support: after adding new directory to treeview (drag-n-drop included) set focus to treeview
* support: UI: rename Scan button to Search(clear), updated hint for that button, added groupbox to keep together file masks sections
* support: UI: main form -> removed 2nd log control (hidden mini log at the bottom of main form)
* support: UI: change cursor to Hourglass on Check/Uncheck files in tree as it may take some time when there are lot files
* support: guess files for Delphi 10 Seattle
* support: Delphi version guess for files: support D3, D4, Kylix
* support: ui Build dialog: updated Caption & Hint for Ignore .dof, .dproj settings checkbox
* support: ui: packages tree: don't automatically show info panel when focus is on erroneous packages/project in tree
* support: UI: help on parameters - add note about different parameters supported in GUI & Console versions
* support: in /verbose mode, if some files/packages were not found, these file will be printed (only first 4 files with ...)
* support: /OutDirBase renamed to /BaseOutDir to match value specified in LazyDBP file. Older version (/OutDirBase) is still supported
* support: remove /baseOutDir cmdparam from GUI version, as there's no use for it in GUI and it only causes problems
* support: UI: Tree popup menu: removed item Toggle check/uncheck selected
* support: UI: Tree popup menu: renamed menu item from "Substitute path (replace).." to "Replace..." 
* support: UI: MainMenu->Edit: added "Replace..." for Tree (2nd) tab
* support: UI: on the startup after loading profile, set focus to Tree

12.09.2015. Lazy Delphi Builder 1.11.11.317:
* Support: add -$L+ parameter to dcc32 when build Debug dcus
* Fixed: breaking bug Console version does not substitute Env variables provided from Command line
* Fixed: ReloadAll button multiply Required packages (does not clear Required list)
* Fixed: error parsing Dcc32 options (the very last symbol could be lost)
* Support: display more details about error when loading Custom Delphi installation on start (use the same validator that is used in editor)
* Support: delphi xe9 renamed to Delphi RS 10

14.03.2015. Lazy Delphi Builder 1.11.10.314:
* Big thanks to Suri for donation.
I.	Features:
I.1.	Lazy Delphi Builder can now use "portable/custom" Delphi installations. You can specify a directory with dcc32 and Lib, Sources files are located and Lazy Delphi Builder will use these files to compile. Or you can specify custom registry key (the same that you use with -r Delphi command line switch) and all the paths will be read from there.
I.2.	Added XE8 and XE9 support. (untested)
I.3.	New: Build log: added toolbar to navigate between keywords in build log, save build log to file, etc [usability, ui]
I.4.	New: F3 to Find next [Hotkeys, Usability]
I.5.	Optimize log output speed in GUI mode (replaced line-by-line output with bulk output of multiple lines)
I.6.	New: added drop down menu to Compile button with commands:
	* Run test build (instant)
	* Continue previous build
I.7.	New: Undo "build" (alpha) - let you restore your Output folders from backups after compile.
II.	Bug fixes:
II.1.	fixed CRITICAL bug: Lazy Delphi Builder Search paths were duplicated each time the Compile button on build dialog was hit. Which lead to huge performance degradation and huge LazyDBP files. (thanks to Sandris Blumbergs for report). Duplicate paths will be removed when Lazy Profile will be loaded (don't forget to re-save your profiles). 
II.2.	fixed: Alt+buttons do not work in Tree view (Alt+T, Alt+L, Alt+k, Alt+r)
II.3.	fixed: cursor will remain Hourglass during long-time operations
II.4.	fixed: drag'n'drop did not work in IDE paths frame
II.5.	fixed: focused node was lost after:
	* Refresh in tree, if focused node was not folder
	* refresh in List tree view mode
	* delete selected node (cursor jumps to the top)
II.6.	fixed: Installation info: fixed Range check error for unsupported Delphi version
II.7.	fixed: don't print to log "Compilation successful" when user canceled build. [ui]
II.8.	fixed: "Test" build actually used some settings (like create/clean directories) from the profile itself.
II.9.	fixed: Continue build mode is still available after loading a different profile (thanks, Sandris)
II.10.	fixed: Delphi version autodetection for XE5, XE6 files detect them as D5, D6
II.11.	fixed: possible assertion failed on loading configs without "+/-" in the end
II.12.	fixed: recents list was loaded in reversed order each time the Lazy Delphi Builder was started
II.13.	fixed: List index out of bounds (-1) when save Environment variables Preset. (thanks to Сафин Марат for reporting)
III.	Small improvements:
III.1.	feature: Build dialog: Run immediately button for Custom Build mode
III.2.	feature: ide paths editor: added Menu item "Shell context menu..."
III.3.	feature: ide paths editor: added Menu item "Copy path to clipboard"
III.4.	Installation info: add scrollbars to Globals and Environment variables memos
III.5.	Installation info: dialog can be closed with 2x Escape or Close
III.6.	Installation info: improved UI a little bit (allow to copy values)
III.7.	Installation select menu: paint caption for platforms + add menu item Reload installations list
III.8.	Presets: disable/enable buttons depending on current Preset text
III.9.	Minor change: installation log formatting: don't use line breaks between timestamp & text
III.10.	fix for ExtraWide monitors: show toolbar on the left instead of right
III.11.	feature: MainMenu->Edit added menu item Empty Recycle Bin (active only on Select files to compile tab)
III.12.	internal: Save LazyDelphiBuilder version to all ini files
III.13.	feature: Check Wizard: added 2 new selection modes for "all installed IDEs" - packages and files
III.14.	added rules to autodetect Delphi version for XE7
III.15.	feature: show/hide supported BDS platforms (OSX, Win64, aarm etc) [usability, ui]
III.16.	feature: Show text "no packages found" if... no packages were found
III.17.	feature: if some files were not found, then show messageDlg after (was before) the MainForm become fully visible and have button on the taskbar.
III.18.	internal: BDS-specific environment variables are now reinitialized always even if they were reinitialized before (via %path%) (support to run from IDE) and for every installation (support for portable installations).
III.19.	feature: Find and Ignore duplicate Lazy Search paths on LazyDBP load
III.20.	feature: show hint with erroneous packages/projects in the statusbar during compile process [ui, usability]
III.21.	change: increase step only after package/project was compiled

15.03.2014. Lazy Delphi Builder 1.10.9.269: 
0. Fixed: autoguess Delphi version did not worked for XE5, XE6
1. Fixed: XE6 version name
2. Changed: reset build status should work for selected nodes
3. Fixed: if lazyDbp file is loaded via Recent, then it's filename is not set for Open/Save dialogs
4. Changed: added more rules to Delphi version autoguess (did not worked for VirtualTreeview)
5. New: generate C++ Builder files (checkbox in Build dialog). NB: will not work if C++ Builder is not installed!

27.02.2014. Lazy Delphi Builder 1.10.8.268 Alpha: 
Many changes. Huge parts of the core were rewritten. Experimental deprecated TasksEx that was used for running jobs in background was replaced with Awaitable class from Roman Yankovski. 
If you’ll meet a bug, please report it.
I. New: 
0. dccaarm, dcciosarm, dccios32 can be used to build projects 
1. You can build only debug dcus now. Build settings dialog now have Build release dcus checkbox. 
2. Presets were added to scan file masks 
3. Added built-in "default" preset to allow reset modified settings to default values. Implemented for: output directories, file scan masks 
4. New page “Search Paths” was added to Build settings dialog 
5. Total build time is printed in the end of the build 
6. Files tree have new feature - Check Wizard (Ctrl+W). Check wizard allows to select/check files that are suited for currently selected version of Delphi. 
7. Menu -> File –> added new command “Merge profile” that allows to update current profile with data from another LazyDBP file. 
8. Base out dir is stored in LazyDBP file. 
9. Speed optimizations in file scan classification routines 
10. File tree for projects and packages: 
10.a. Added new column “build status” (?? – was not built, ok – built successfully, er – was not built due to error) 
10.b. Filter hides folders that don’t have any visible file 
10.c. New commands in popup menu: 
10.c.1. Reset build status (useful when you want to rebuild project in Continue Build mode) 
10.c.2. Copy build log to clipboard 
10.c.3. Find a project in Build Log 
10.d. Show guessed Delphi version for selected file in Info panel 
II. Fixes: 
0. IDE version was ignored loading LazyDBP file 
1. Build settings dialog: presets were not locked on test build mode 
2. Ignored Break after N errors value (Build settings) 
3. If profile had XE 3 Architect installation, but the machine didn’t have that Delphi version, then pressing Compile button the error was shown: Assertion failure (uLazyIdeProxy.pas, line 886) LazyInstaller.CurrentInstallation not assigned. 
4. Clear profile did not cleared Environment variables and Lazy Search paths 
5. Build settings dialog: namespaces editbox had incorrect Recent(MRU) list 
6. Build settings dialog: wizard background painting issue 
III. UI: 
0. Installation log was renamed to Build log 
1. Lock menu item Edit –> Find if current tab does not support search 
2. Save as dialog, Filename value is set from the loaded profile file name 
3. Build dialog: added edit for CustomCompiler (it was available from 28.04.2013. Lazy Delphi Builder 1.8.6.240 version but only via editing LazyDBP file). 
4. Status bar now shows number of projects built with error. 
5. File tree: speed optimization for switching from List view mode to Tree view mode (about 46% faster)

15.09.2013. Lazy Delphi Builder 1.9.7.251:
0. Feature: allow to select platform. Win32, Win64 and OSX are supported for now. Implemented as dcc32 replacement with dcc64/dccOSX. Other platforms can be used by specifying custom dcc32 in LazyDbp file(see #9 in release notes for version 1.8.6.240)
image
1. Feature: You can change the amount of problems, that might happen before Lazy Builder will decide to stop build (default 2)
* 
2. FIXED: Access Violation when press "Load preset" button without any preset selected (thanks for reporting it) 
3. FIXED: can't use Unicode (non-ansi) characters in output paths. Details: OEM conversion for paths is applied only for versions prior to D2009. 
4. FIXED: Removing package from IDE in Delphi version prior to XE2 did not worked. 
5. FIXED: Packages tree: Program stops responding after Info panel is opened and erroneous package is selected 
6. FIXED (unsure): frozen Delphi version change after resources are copied 
7. Minor: fixed hints on frame "Scan settings" 
8. Minor: dcc32 was renamed to dccXX in all the hints and messages
9. Added support for Delphi XE6 =)

28.04.2013. Lazy Delphi Builder 1.8.6.240:
0. Added support of Delphi XE4 и XE5 (not tested, please let me know if it works).
1. .ini and .LazyDbp file format changed: Don't store row numbers.
2. Console help: view examples.
3. Fixed error if BaseDir was set to relative path (as parameter /OutputDirBase). F.e. it was impossible to pass there TmpFolder1 - fixed now.
4. Do not allow change Delphi version while compiling.
5. Fixed Assertion Failed on selected Delphi version change.
6. Fixed AV when Lazy Delphi Builder was run without Delphi installed.
7. If Delphi is not installed, lock controls that need it.
8. ProgressBar: use pstMarquee mode instead of pstProgress while scanning for files.
9. Allow to specify custom command line compiler. Or run custom .bat instead of dcc32. To do so, open LazyDBP file in notepad and add new key CustomCompiler=FullPathToMyCustomCompilerExeOrBatchFile to the section [[Build Settings]]. It is possible to call any other custom exe or batch file instead of dcc32 compiler. I tried to replace dcc32 with dcc64 and it worked pretty well for me.
10. Build dialog: add button with dropdown menu to the Base Folder edit control with following items:
	1. New folder in the %temp% folder
	2. Current folder
	3. Open Base-folder in Explorer
11. Fixed: dragged and dropped file/folder during the operation stopped current operation.
12. Fixed: Empty Recycle bin didn't worked sometimes (if files were just deleted)
13. Fixed: Check All required in the files tree did not work in case if there were several packages with the same name, but deleted to Recycle Bin. From now, deleted files are ignored and LibSuffix is used.
14. Feature: Increased size for defines edit in the Build.. dialog
15. CheckListBox editor for DEFINES  in the Build.. dialog
16. Build dialog: defines editor reworked - all the defines list is stored, but you can simply check and uncheck needed defines.
17. Fixed bug: Ctrl+A in the files tree selects even invisible (filtered) files
18. Tree: added button to clear Filter
19. Tree: Filter - allow multiple expressions using ";" (thanks Nashev for idea)
20. Build process: show "compiling N of total M packages" (thanks Nashev for idea)
21. Save form's sizes for
	1. Environment options
	2. Build dialog
	3. Main Form
22. Presets (allow save/load set of settings) support (killer feature!): 
	1. for Environment variables 
	2. Lazy include Paths
	3. for Scan Folders
	4. for Exclude extensions
	5. in the "Build.." dialog:
		1. for Build Options
		2. for Directories
23. About window does not have email for feedback.
24. About window: links now can be copied to the clipboard (via right click on link -> Copy)
25. fixed: Hint stays visible on the top of all windows even if Lazy Delphi Builder is in the background
26. internal: JCL and JVCL were updated
27. Feature: Installed Packages tab: system (Delphi) packages can be hidden
28. Alpha feature: Check Wizard (Ctrl+W) in the tree on the tab "2 Select Files to compile"
29. Tree: new menu item Check Packages for Current IDE
30. fixed: Instlled packages tab: packages list was not refreshed on Delphi version change.
31. All settings now, are stored in the "Data" subfolder

13.09.2012. Lazy Delphi Builder 1.7.5.232
1. GUI: Fixed "endless" List index out of bounds(-1) on 4th tab. (thanks to Vladimir Misjuk, Nashev)
2. Console: Don't print "Compiling Packages" if no packages were selected. Same for "Compiling Projects"
3. Console: Print build mode (normal, continue, test) on compile
4. GUI: Rename "Export to .bpg" to "Export to .bpg (makefile)".
5. GUI: Help: added menu item: Email bug report to author, which opens eMail Client and adds as attachments:
  * Active LazyDBP profile (list of selected files and packages)
  * Build/Installation log
  * Settings file

12.09.2012. Lazy Delphi Builder 1.7.4.230
1. Fix: LazyDBP file was saved in Ansi encoding. Changed to UTF8.
2. New: Console version now has parameter /quiet, which enabled only errors output.
3. Fix: Previous version has no "Show stack trace dialog on build". Fixed
4. Minor: Fix: Saved profile was not added to Recent menu.
5. New: Build dialog: allow to start build immediately (without displaying all Wizard pages), via DropDownMenu
6. Minor: In tree view: In list mode, trim beginning of path, but not the file name.
7. Fix: Summary message ("Your Bpl out folder is set as default BPL path in IDE and it will be deleted before compile!") was shown even when "Clean output folders before compile" checkbox was not checked. (Thanks to Perry Kappetein for report).
8. New: Build dialog: show selected build mode (test, continue, normal)
9. Minor: New: Build dialog: show hints on first page in separate control
10. Minor: New: Build dialog: show Packages count, Projects count in summary
11. Minor: New: Build dialog: show "Packages count, Projects count" on first page.
12. New: Installed packages: add Popup Menu items: 
	1. Copy file name
	2. Show Shell Menu
13. Minor: Fix: Stop button was too colorful in disabled state (thanks Nashev for report)
14. Minor: Fix: Sorting Found files freezes interface.
15. New: In treeview added filter to show only packages that were compiled with error.
16. Minor: Fix: after file scan button Stop was enabled and could be clicked.
17. Minor: In treeview: Show additional info for packages in Info:
	1. Description
	2. LibSuffix
	3. Defines
	4. Caption: Requires
18. Minor: On problematic package select and show Info-memo, do change cursor to crHourGlass and load erroneous command to Memo in separate thread to avoid interface freezing and prevent messages like "program is not responding". (actual for builds with huge amount of directories).
19. Minor: Faster display of the packages list in their compile order on Build in /debug mode
20. Fix: In tree view Check All required did not worked correcly, if there were packages with the same name in different folders.
	* Added LibSuffix check
	* [ ] There are still situations when the bug could be reproduced. F.e. if there are packages with the same name without LibSuffix, or with the same LibSuffix. F.e. for Delphi and C++ Builder in LMD full set.
21. New: Install log: Add menu item Copy Full Log
22. New: Install log: Ctrl+Up and Ctrl+Down search now finds tokens:
	1. Compiling
	2. Fatal:
	3. Error:
	4. Successfully compiled
	5. Problematic Packages
23. Fix: Console version: parameter /tb had wrong hint

05.09.2012. Lazy Delphi Builder 1.7.3.217
1.	New: Delphi XE3 version detect(I don't have yet XE3 with working dcc32, so I couldn't verify how will it compile, but it should work).
2.	New: Packages and projects compilations is moved to separate thread, so GUI is not frozen anymore (experimental).
3.	New: Export selected packages/projects to .bpg-ôàéë (Borland Project Group, old format – could be used with Delphi 5 - Delphi XE3 as well) (experimental)
4.	I've rewritten code for packages tree:
	a.	New: As alternative to Scan and Scan New Folder, I added Drag'n'drop support directly to the packages/files tree. It you can drop folders and files (.pas, .dcu, .dpr, .res, .dfm) instead of using Scan. Drag'n'Drop uses same file masks rules from the Search Folders tab.
	b.	Fix: Fixed several bugs (EListError, Access Violation) appeared after deleting files from Recycle Bin and scroll.
	c.	Sorting support in tree
	d.	Added menu item to Clear Recycle Bin in tree
	e.	Automatic column width change on resize
	f.	For folders: show information about children
	g.	Fix: Don't trim text for long paths in tree.
	h.	For tree: Added Undo menu item to Edit menu
5.	Fix: Fixed several memory leaks
6.	Open Build dialog via F9, Ctrl+F9, Alt+F9, Shift+F9
7.	Added link to site in About menu
8.	Longer time for Hints
9.	Fix: On 2 monitor system forms were showed between 2 monitors.
10.	Fix: Internal AV on exit
11.	Search (Ctrl+F) for Installed packages
12.	New: Added NameSpaces support for XE2: editor, key -NS. Saved in .LazyDBP.

14.05.2012. Lazy Delphi Builder 1.6.2.200
1. New: Always store lists of files in the same order in .LazyDbp file, to ease comparison of different .LazyDbp versions.
2. New: Hotkeys: Ctrl+Up and Ctrl+Down in Installation Log, to move between "Compiling ..." messages.
3. New: Allow to stop file deletion on Build.
4. New: Added JCL Exception Dialog for easier error tracing.
5. New: Installed Packages Tab: Allow to remove or disable packages from Delphi.
6. Fix: Sometime profile can't be compiled without reload.
7. Select files to compile dialog:
	1. New: Set focus to newly added folders.
	2. Fix: Few fixes in Substitute path dialog
8. Fix: Backup and delete files in separate thread. Show progress bar.
9. Fix: Feature #29024: Network paths support.
10. Fix: Bug #53575 (http://www.hostedredmine.com/issues/53575): Go to the "Environment Variables" window and select "Library paths". Right click -> Add new... and add 1 path. After that if you don't click existing item and try to add another path the "List index out of bounds (-1)" is generated. 
11. Fix: !Don't run CnDebugViewer!
12. Many fixes and improvements in Installation Log.
13. Many minor GUI changes and improvements.


11.09.2011. Lazy Delphi Builder 1.5.1.185
* Added main menu. 
* Added build mode: “Continue build”. If build process stopped with error, you can resolve problem and continue to compile only packages that were not built. 
* Added search (Ctrl+F) for packages tree and installation log.
* Added: “Recently opened files” support
* Added: in /debug mode, print packages list in the same order they will be compiled.
* Fixed: Access Violation on start on the computer where Delphi XE2/Pulsar installed.
* New: If build process stopped with error, ProgressBar color will be changed to red.
* Fixed: in packages tree selected record is lost on refresh.
* Display time in Installation Log
* New: in packages tree option Check all required packages.
* Build settings Dialog:
* Show Warning if checkbox Clear out dirs is checked, but output dir for BPL files is default for IDE.
* Added Hints
* Event handlers for buttons near Edits
* On Directories Tab, button Save to profile was added to save settings to current profile. (in case of need to change settings without running build).
* Fixed: Environment and custom variables were processed with errors when used in relative paths.
* Fixed: no need to click in Log to enable scrolling.
* Added: Remove files from Recycle Bin in packages tree.
* Changed: settings .ini filename does not depends on the name of exe file. Always used: LazyDelphiBuilderGUI.ini.
Note: to compile FireMonkey project with Pulsar beta, there should be defined dcc param --default-namespace:system in Build Settings dialog.

12.04.2011. Lazy Delphi Builder 1.4.0.175
* Console version: All errors should be handled properly now. In case of error in program, %ERRORLEVEL% will be set to 1. Improved error description.
* Added build modes. New build mode: TestBuild. It allows to quickly run build process with all output files created in Temporary directory (%TEMP%). These files will be deleted on Lazy Delphi Builder exit. This mode allows to ensure, that current config can be compiled. For console version there is new cmd switch /tb. For GUI version there is a button in Build settings dialog. Settings for TestBuild mode will not be saved in profile. 
* Added verification of command line switches on program start. If LazyDB will not recognize any param it will show error message (console version will stop). But you can bypass that check by specifying /by cmd parameter.
* Now paths shortening (replace absolute paths with relative, and long file/folder names with short) is enabled only if Delphi version prior to 2005 is used. Also, added cmd param /sp - (Short Paths) that allows to enable/disable use of short paths for any Delphi version.
* Fixed: don’t work buttons in Build settings dialog.
* Fixed: Scan New Folder.. error in packages tree for virtual folders “Resources”, “Sources”.
* Fixed: hanging when open “Browse for Folder” dialog.
* Packages tree: show Shell Context menu for any file or folder.
* Packages tree: fixed errors in popup menu for items in Recycle Bin.
* For /verbose and /debug modes, full list of specified command line parameters (and values) is displayed now. Added to ease debugging when program run from scripts.
* Console version: updated /help command output.
* Console version: Allow define/redefine environment options from command line using /ev <EnvVarName=Value[;EnvVar2=Valu2]> switch. It can override variables stored in Lazy Profile, Delphi environment variables, and also system environment variables in paths. Works only for variables, that were defined in format $(EnvVar).

04.04.2011. Lazy Delphi Builder 1.3.0.163 (stable)
* Added Delphi XE support.
* Fixed bug that allowed randomly to get compilation error without any details in error log.
* More debug messages in /debug mode.
* Check that dcc32.exe is functional (not trial) before compile. In earlier versions, Lazy Delphi Builder did nothing, displaying text: Compiled success.
* Build settings dialog completely reworked. Now with wizard. Before running build process it displays short summary with full description of jobs to be done (which folders will be used, what will be compiled, what not).
* Fixed: “Ignore IDE Paths” value was not saved.
* Files tree: Scan new folder action now changes $(variable) to actual folder.
* Debug output text is displayed in different color (gray in GUI, cyan in console).
* Additional log in main window is now hidden.
* Changed icon and logo.
* Added option to ignore settings from .dproj, .bdsproj, .dof files. (.cfg were ignored in all versions).
Known issue: Lazy Delphi Builder incorrectly parses .dproj files. All settings for every configuration but the Base are ignored.

02.09.2009. Lazy Delphi Builder 1.2.8.140 (stable)
* Changed the way, relative paths were used. 
* Added support of custom environment variables. $(syntax). It’s possible now to override Delphi and system environment variables in Lazy Delphi Builder.
* Added support to override Delphi Library Search Paths.
* Fixed known issues. Some improvements.
* New homepage: www.lazyproject.info.

13.04.2009. Lazy Delphi Builder 1.1.7.117
* Fixed: Impossible to change IDE version.
* Fixed: display wrong numbers of selected packages before compile on 1st scan.
* Fixed: Info-panel is not displayed automatically for projects that were compiled with errors.
* Fixed: Delphi environment variables (f.e. $(BDS), $(DELPHI7)) were not processed.
* Unused output folders are not processed (cleaned and/or created). F.e. DebugDcu output folder  will be cleaned/created only if flag “Build debug Dcu” was enabled. Res output folder will be created only if "Copy Resources" flag was set.
* Windows opened in center of screen now.

23.03.2009. Lazy Delphi Builder 1.1.6.115
* Added “New profile” button.
* Exclude masks now work only for files.
* Fixed: Library Search Path was ignored. Only LibPath/DebugLibPath folders were used.
* Fixed error with deleting .bak folders.
* Fixed: update status bar on cancelled operations.
* Lazy Delphi Builder now stores settings in 2 files: scanning options and histories in .ini file. Files lists and build configuration in .LazyDBP file.
* Added commands Check all and Uncheck All for check list boxes popup menus.

13.11.2008. Lazy Delphi Builder 0.9.4.100 beta4
* Copy resource files by mask after build
* Added command line parameter /CopyResources+-
* Fixed: Cannot terminate an externally created thread.
* Allow to compile Debug version of DCU files. This allowed to fix error with  CodeCompletion in IDE editor. Debug Dcu are compiled without optimization (-$O-) and enabled(-$D+)
* Use short paths (8 symbols)

30.10.2008. Lazy Delphi Builder 0.9.4.97(beta3)
* Fixed CheckBox draw in Exclude Masks.
* 2 versions LazyDelphiBuilder now supplied:
  * LazyDelphiBuilder.exe - console version to run saved .LazyDBP profiles.
  * LazyDelphiBuilderGUI.exe - GUI version to create/edit/run LazyDBP profiles.
* Added tab on main form to display list of packages installed in IDE
* Added support of Ctrl+Z in packages tree.
* Allow to add new folders from packages tree tab.
* Backup folders now deleted to Recycle Bin. Can be disabled with “/UseRecycleBin-”. On build, old output folders are not deleted, but renamed to Bin.bak, bpl.bak and so on. This can be disabled with “/BackupOutFolders-” command line switch.
* + Many bugs fixed.

21.10.2008. Lazy Delphi Builder 0.8.3.53 (beta)
* Compile projects (.dpr).
* Added command line switch “/noAutoSave” to disable autosave of default profile on exit.
* Added Debug tab on main form.
* Added check for empty output folders.
* Changed profile extension. Now it’s: .LazyDBP
* + Many bugs fixed.

17.10.2008. Lazy Delphi Builder 0.8.2.52(beta)
* First public release.