Universal Viewer application interface
======================================

1. WM_COPYDATA message: pass data to Viewer.

  wParam: handle of sender window
  lParam: pointer to COPYDATASCRUCT record:

  a) "Open ANSI filename" command:
  dwData = 100
  cbData: ignored
  lpData: pointer to ANSI filename (zero-terminated)

  b) "Open Unicode filename" command:
  dwData = 101
  cbData: ignored
  lpData: pointer to Unicode filename (Unicode zero-terminated)


2. Unofficial command line parameters:

  /Q              - Start Viewer in "Quick View" mode:
                    menu is hidden, hotkeys and message boxes are disabled.

  /QB             - Hide window border+caption.
                    Useful in addition to /Q parameter.
                    If both /Q and /QB params are specified, then only Alt+F4 key can close Viewer.

  /QT             - Hide taskbar icon.
                    Useful in addition to /Q parameter.

  Extended parameters are intended for third-party tools to work with, such as "Quick View script"
  by majkinetor.
