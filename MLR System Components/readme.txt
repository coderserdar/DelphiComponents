Disclaimer: By using this sample, you agree not to hold me liable for anything concerning it. If you do not agree, do not use it, read it, or distribute it. If you do agree, you can do anything you want with it, taking full responsability.

This is a package of system components I find very useful. It includes the following:

+ TButtonBrowse: connect it to a TEdit or other TWinControl descendant, and get automatic one-click functionality of folder or file browsing.
+ TConfiguration: saves control information to the Registry or an INI file (you must indicate the controls yourself, though).
+ TFileOperation: performs a file operation through the shell, allowing undo's, renaming, etc.
+ Two new Edit Actions: Select All and Print.
+ Progmngr: Used to interface with the Program Manager.
+ TBroadCaster: Used to broadcast a message in your LAN.
+ TDriveFormatter: Used to format a drive (diskettes, please! :-))
+ TTAPIService: Used to use the simple TAPI services.
+ TDragDropManager: Manages OLE drag&drop (automatically for edits, if available).
+ TFileIterator: Builds a list of files from a folder, allows you to sort them and/or add/remove, and iterates over them.
+ TImpersonator: Allows you to log on a user and impersonate him (not finished). It could be useful for ISAPI applications.
+ TThreadedAction: This is like a normal TAction, but it will execute (if you desire) in a separate thread (up to a number of specified times before disabling).
+ TWizardManager: Manages controls (typically TPanels or similar), to build a Wizard-like interface at run-time; allows you to keep your control within grasp at design time.
+ TTelnetClient: Manages a Telnet connection (the RetrieveList procedure needs to be finished).
+ TPanelSizable: This is a panel which be resized at run-time.
+ TUsersListBox: A listbox which can be populated with the user accounts in a domain (NT only).
+ TTrayIcon: A component to manager a tray icon.
+ TThreadedAction: Similar to TAction, this one runs on its own thread, and will disable visually when a given number of threads are running (just one, if you want to avoid re-entrancy).
+ TRASPhoneBookDialog, TTAPIService: Ignore, please, until it gets a bit more fleshed out.
+ TMLRRecentDocs: A component that connects to a menu item, and manages recently used documents from there.
+ TMLRIntfTreeView: Used to keep an interfaces associated with each node. Everything gets free up when deleted.
+ TMLRObjTreeView: Keeps objects instead of pointer associated with each node. Optionally, it frees the objects when deleting them.

Some of these components are not ready for a "release". I mean to have these as "Open Source" - if you want to change anything, do it, then mail me, and if I OK the changes, I'll put them here. I do not have the time to perfect all the components, but I would be glad if someone could work on any.

1999.09.08
Marcelo Lopez Ruiz
marcelo.lopezruiz@xlnet.com.ar

History
1999.09.20
+ TThreadedAction now uses a TThread object, and now supports raising exceptions.

A long time ago
+ First release
