CodeWarp 1.22 for Delphi
Copyright 1998-2002 Creative IT - Eric Grange

Contact/Support : egrange@creative-it.net

----------

0. What's CodeWarp ?
1. Compatibility
2. Installation
3. Using CodeWarp
4. Registering
5. History

----------

0. What's CodeWarp ?

CodeWarp is a little expert for Borland Delphi 3/4/5/6/7, allowing quick code navigation
in your source. It displays classes, funcs & procs in a popup tree view, and brings
the editor to those spots in a click. This is a real productivity-oriented tool
and not a put-all-the-classes-from-every-source-in-an-over-packed-tree-window
like class browsers generally are.
The aim is to warp the cursor where you want it to go and not to display a clean,
object-oriented, but barely useable view. CodeWarp opens a huge TreeView with
tons of shortcuts, but it's a popup, it is not crowding your desktop space
but making use of all the screen, it's loading instantaneously, always up-to-date,
working on buggy or incomplete code, and can be fully operated without the mouse 
(all keys are below/within right hand).
Practice proves CodeWarp gets you to the right position in a few keyboard clicks.


1. Compatibility

Requires Borland Delphi 3-7, should although work with other Borland 
OpenToolsAPI compatible software like C++B and JB, but hasn't been tested 
(I don't have them...), and the CodeWarp parser doesn't understand those 
languages anyway. :-(


2. Installation

The help file should be copied to your windows\help directory.
Place the package file ("CodeWarp-D3.dpl" for Delphi 3, "CodeWarp-D4.bpl" for Delphi 4,
and guess it, "CodeWarp-D5.bpl" for Delphi 5, "CodeWarp-D6.bpl" for Delphi 6
and "CodeWarp-D7.bpl" for Delphi 7) anywhere you see fit 
(delphi\bin is a good choice).

Start Delphi and Install CodeWarp as a standard design-time package.
Remove it the same way.


3. Using CodeWarp

You get two new Menu Items, 'About Code Warp', under the Help menu, allowing
to reach the About Box (you wouldn't have guessed, would you ?), and a
'Code Warp' under the Search Menu, calling CodeWarp panel (you will for sure
prefer the shortcut : Ctrl+W, that can be used from the editor window).

The CodeWarp panel is mainly a tree representation of the active unit in the
editor windows, with 9 root nodes :
- classes : shortcuts to class declarations and member implementation for each class
- interface : shortcuts to interface items (classes, procs & funcs prototypes...)
- implementation : shortcuts to implementation stuff (classes, init/finalization...)
- procedures : shortcuts to procedure implementation
- functions : shortcuts to function implementation
- project : shortcuts to units & forms in your project
- components : shortcuts to known component packages
- local librairies : shortcuts to your local libraries units (directory & file tree)
- VCL Source : shortcuts to VCL unit files (this requires you to have the VCL
   source files, they come with Client-Server versions)

All entries are identified by a quick number (1 to 9), typing that number selects and
opens/closes the node, keypad is top for this. By default, the first node (classes)
is opened. You can change this from the options window.

You can either double-click the items or use the space bar/enter key to warp the
editor position to the selected spot in the code. When units get longer than
three or four screen pages, you will quickly get addicted to warping, believe me.

The User Interface is highly customizable, just reach "About CodeWarp" then
"Options" (either from the Delphi Help Menu or by pressing F1 with CodeWarp
window open). All options are described with hints.

Note on "local libraries" and "VCL Source" : these entries actually work as a small 
   file explorer, displaying all ".pas" files in folders under a base directory. 
   Like the project and component entries, these entries are filled only when expanded, 
   so you may experience a 1 or 2 seconds wait if you have hundreds of files...


4. Registering

CodeWarp v1.xx is now distributed as free software, so there is no warranty 
of any kind (see standard disclaimer below). However, Creative IT retains 
copyright all aspects to the extent permitted by the law, by "free software", 
it is only and solely meant that users do not need a licence to use the software.
For Commercial/Business or Enthousiasmed users, please send a PostCard or even
a Donation, it will always be welcome ;-).


5. Known Issues

0) Help file is that of old v1.19...
1) Warping to a form method, with the form unit closed, brings the form to front,
   not the code view. However, the cursor in the code view is positionned Ok.
   If anybody knows how to get rid of that behaviour...
2) Delphi 4, 5 : support for project features.
3) Parser does NOT actually cares about comments (whatever their styles), in most
   cases, this is no problem for general use (commented out procs can still be
   reached, etc.) but this causes some mess up with commented out language tags
   (you will see them along with other tags).
4) CodeWarp as static window rather than popup. Could prove useful as Inprise
   did not activated the code explorer in Delphi 4 Standard.
5) Search does not activates parsing of units, only already parsed areas of
   the tree (classes, functions...) and manually opened units are parsed.
6) <Your suggestions here>...


6. History

   1.22  Delphi 7 version kindly recompiled by Arnd Schilling, shouldn't expire!
   1.22  Recompiled "as was" for Delphi 7
   1.22  Fixes : Removed D5 bug-workaround from the D6 versions where it would cause
                     intermittent issues when closing D6
   1.21  New features : Delphi 6 support
   1.20  New features : Support for non-English Delphi 5 versions
   1.19  New features : Delphi 5 support, thanks to Rob Gilbert (rgilbert@bigpond.com)
                            for compiling, trials and fixes.
                        "Warp Preview" to preview the position you'll warp to. Needs to
                            be activated in the options screen.
         Fixes : Fixed some false 'class', 'record', etc... recognitions (but some of the
                     incomplete declarations will now be missed).
                 Shortcuts and toolbuttons to "Local Libs" and "VCL Source" now behave
                     correctly when one or both of these are disabled.
                 VCL Source directory autodetection improved.
                 Search & Browse Path retrieval enhanced. 
                     (CodeWarp is no longer lured by GpProfile under Delphi4)
                 Fixed nested "uses" warping (could fail in some cases).
   1.18  New features : Search now remembers recent searches and option settings.
                        Ability & Option to consolidate method declarations.
                        Ability to move the main window by dragging its caption bar.
         Fixes : Local libs filters would not be applied to sub-directories, and the default
                     "*.pas;*.inc" used instead (Thanks T. Roettgers for reporting).
                 Fixed & enhanced keyboard navigation in the "Options" screen (tab order,
                     escape key, shortcuts... thanks again T.Roettgers)
                 Icon legend in the "About Box" would be displayed with scrollbars under
                     Win95/98 (not NT). Replaced with an ownerdraw listbox.
         CodeWarp is now property of Creative IT, the new name of the development
         branch of Noval Conseil.
   1.17  New features : Added support for COM entries (declared with 'interface'
                            or 'dispinterface'), they are now listed along with
                            classes, but with a different icon.
			    Search in sub-branches (activate with '0' or CTRL+F),
                            tree can be dived or traversed.
         Fixes : Better support for tabs and spaces (non std-formatted code).
                 A minor memory leak when unloading codewarp.dpl package.
   1.16  New features : Added optional filemask to local libs, so that files other
                            than '*.pas' could be listed (thanks goes to Thomas
                            Roettgers for the idea).
                        '*.inc' files will be listed along '*.pas' in local libs
                            and vcl path entries as a default behaviour.
         Fixes : ToolButtons reordered (will be right only with D4sp2 or higher)
                 New Delphi4 component packages or now parsed ok.
                 "Fixed" a Delphi bug with deleting then expanding huge branches
                     in a treeview, causing the treeview to scroll in a weird way.
   1.15  New features : Added a shortcut toolbar for mouse-addicts. Works the same
                            way as regular numpad shorcuts.
                        If 'Classes' contains only one class, it is expanded right
                            from start (that's one click less).
         Fixes : Standard VCL path now uses registry entries for D3 & D4
                 Fixed multiple-opening bug, CodeWarp would sometimes re-open an
                     already opened file in the IDE.
                 Fixed Search Path (D4), due to changed registry key name
                 Many spelling fixes (thanks Hendy Irawan)
                 Fixed a bug concerning missed methods & class declarations in
                     the implementation section (thanks again Hendy Irawan)
   1.14  New feature :  A custom invokation shortcut can now be defined
         Fixes : Some misspellings. Yet zillions remains, Help !
   1.13  New features : Delphi 4 compatibility.
                        Local Libs entry editing enhanced a little.
                        Package Files are now sorted.
         Fixes : 'uses' parsing now allows for comments in between units, however
                     this 'uses'/comments stuff is not over yet (see KI 3).
                 Fixed double '\' bug for local libs/vcl subdir browsing.
   1.12  STATUS CHANGE : CodeWarp v1.xx is now distributed as Copyrighted FreeWare.
         New feature :  Multiple Local Libraries Entries.
         Fixes : Invoking CodeWarp with no opened form/source is no Ok.
                 Last unit in a DPK was shown with a semicolon ';', and CodeWarp
                     failed warping on this file. Now Fixed.
         	 'uses' parsing used to mess up with 'in' statements. Fixed.
                 'File not found' error now displays the missing file's name.
   1.11  New features : CodeWarp can now focus the procedure you're in, remember
                            the expanded nodes and the last focused node.
                        'uses' clause now parsed (referenced units are warpable).
                        Added a help file.
         Fixed Direct File Warping, now works under components entry.
         Default Entry is consistent with picked option (it misbehaved in v1.10).
         Fixed Bug when DPK file could not be found.
   1.10  Major features added : project files, components, local libraries 
                                and VCL Source entries (as well as options...)
         You can now warp to files or to special positions in your files
             (they are parsed only when necessary)
         Added Parse Cache, big units are re-parsed only if changed, now the
             final cpu-cycle-eater is the treeview (80-90%). 
         Fixed bug with class declarations in implementation part of units
             (CodeWarp would miss some class methods and class scopes)
         Fixed/Added 'packed record' (now listed along with standard records)
         Show Class Method Under "Procedures" & "Function" now saved properly
   1.03  CodeWarp now correctly finds procs & funcs with left tabs & spaces
             (was working in some alpha, but got lost in between.. :( )
         Gradient caption bar is no longer filled with a gradient when in
             low-color modes (less than 15 bits/pixels)
   1.02  Added Class Scope entries (and accompanying options)
   1.01  Added Record support, Implementation entries
         Minor cosmetic fixes & enhancements
   1.00  First Public Release Version
