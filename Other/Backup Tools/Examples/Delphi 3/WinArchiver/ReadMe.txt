This is the Read-Me file for WinAchiver.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Last updated on 6 may 1999.

WinAchiver supports drag and drop operations :
you can drop files on an archive or drop files from the archive
on a folder.
If no archive is opened, you may drop an archive on WinAchiver and
it will be opened.

If you're in TreeView mode, then any added file will be added to
the current selected folder. So, select the right place before
adding any file, or in the "Add" dialog box, uncheck the option
"Add to current folder ?".

There's two popup menus : one in the Tree view and the other in the
List view.

You can sort the List view by clicking on any column header. If you
click a second time on it, it will reverse the sorting order.
If you want to remove the sorting, then click on
"Options/Sort/Original order".

If you want to go at my site, then select "Help/About" and click on
my link.

If you want to check what operations where performed, then select
"Options/View the output of the last operation".

When you create an archive, it is automatically segmented when needed,
but if you want to build segments of a specific size, then go in
the "Options/Configuration" and select a segment size in the
segmentation page.
If you want to change the selected language, go in the "Options/Configuration"
and select the "Options" page, then select the expected language.
The language "Automatic" will automatically select the user's language if
it is known.

You can adjust some options for the creation of an archive
(in "Options/Configuration", page "Archive creation") :
  - Compress archive : you can forbid compression if the files to be added
    are already compressed. But TArchiver will always check if a compressed
    block is bigger than the uncompressed one, and it will choose the
    most interesting one.

  - Crypt archive : will activate the encryption of the whole archive.
    You may encrypt some files only, without encrypting the whole archive,
    by selecting the "Encrypt files" in the "Add" dialog box.

  - Solid archive : when activating this option, all files are added to
    an uncompressed archive, that is then compressed when closing the
    archive. It is interesting if you've got a lot of small files, because
    the file entries will be compressed too. The disadvantage, is the
    the archive must be uncompressed/compressed each time it is opened/closed.

  - ReadOnly : if true, then it will be impossible to change the content
    of the archive after its creation (when it is closed and reopened).

  - Create SFX archive : if true, WinArchiver will create an SFX archive.
    It is especially important if you want to create SFX segmented archives.
    Note, that you should define the options of the SFX in
    "Options/SFX Configuration", before you create the new archive.

  - Block size : you may choose the size of a block of data that is compressed.

  - Reserve space : you may reserve a specific amount of bytes in the first
    disk that will contain the first segment of a segmented archive.
    It is useful if you want to add a readme file or any other file.

  - Compression level : you may select a compression level, but you'll be able
    to change it in the "Add" dialog box.

You can define the SFX options in the "Options/SFX Configuration" Dialog box:
  - Execute file after extract : you can allow the execution of a file after
    the extraction.

  - User chooses files to extract : you can allow the user to select which
    file to extract.

  - User chooses overwrite mode : you can allow the user to choose the
    overwrite mode.

  - User allowed not to run the file : you can constrain the execution
    of the file after the extraction.

  - Caption : you can define a specific caption displayed in the SFX window.

  - Command line : you can specify a file that will be executed after
    the extraction.

  - Arguments : you can sepecify arguments to the executed file.

  - Default extract path : you can specify where the files will be extracted
    be default.

  - Overwrite mode : you can select a default overwrite mode.

  - Comments : you can specify comments that will be shown before/after
    the extraction.

  - Language : you can specify the language used in the SFX archive.
    The language "Automatic" will automatically select the user's language
    if it is known.

  Click on "Help" to obtain help.


When you create a new archive, you can define a comment that will be shown
each time the archive is opened. Note, that you can define it only when
the archive is empty.

If you want to create an SFX archive from an existing archive, open it
then select "Make a Self-Extracting archive" and adjust the options.

If you want to clear the content of an archive, then select
"Files/Reset archive". It will delete the current archive, then recreate
a new one with the same name.

Note that WinArchiver doesn't check if a file already exists when you
add it.

You can filter files to be added :
Go to Options\Filters and add your filters. You can define the kind of
filter:
  - if it's required, it means that a file can be added only if it matches
    one of the filters.
  - if it's excluded, it means that a file can be added only if it matches
    none of the filters.
Then, when you add files to the archive, don't forget to check the option
"Filter files ?".


