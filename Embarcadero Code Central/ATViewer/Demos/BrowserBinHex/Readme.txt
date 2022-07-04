Demo notes:

1. To compile this demo, you need additional components:

- VirtualShellTools: http://mustangpeak.net
- VirtualTreeview: http://www.soft-gems.net
- ThemeManager (for Delphi 5-6): http://www.soft-gems.net


2. There is conditional define in UFormView.pas: {$define STREAMS}.

It is for debugging purposes: if you uncomment it, program will use
TATBinHex.OpenStream method instead of TATBinHex.Open.

It doesn't affect demo functionality, only internal ATBinHex work is changed.


3. Use included AT*.inc files, which can save about 70 Kb
of final exe size. Place these files into component's Source
folder, over default ones.
