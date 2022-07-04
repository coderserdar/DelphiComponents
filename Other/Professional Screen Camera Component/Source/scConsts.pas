{
Professional Screen Camera Component (Delphi 7 to above)
Developed 2008 by Mohammad Reza Hanifeh Pour (MRH Software Co.)
Author E-Mail: mrh.info2007@gmail.com
Centeral Office Tel: +98(21)(7764)(4130).   Everyday 9AM ~ 4PM.
Office Address: F2 29 Rezai St. Namjo Av. Tehran-Iran.
................................................................................
Version history:

v4.7.1.0: Updated 01/01/2009
    New features:
      1) Add unit hightimar.pas for a threaded timer in preview or recording.
      2) Add canvas.trylock and canvas.unlock for all parts of image processing.
      3) Included all necessary units of Wave Audio Package and TEffect in my component. 
    Modify features:
      1) Fixed some routines in function PowerDeleteFile, Because long time waiting for deleting a file.
    Remove features:
      No thing
 
v4.4.1.1: Updated 12/11/2008
    New features:
      1) Screen Camera Unit converted to component packege (Delphi 7 to above)
      2) Add info frame rate to preview routine
    Modify features:
      1) Replaced PreviewScreenFrame routine with CaptureScreenFrame routine in preview mode
    Remove features:
      1) Delete PreviewScreenFrame routine, Because between record and preview
         eventuate to memory stack overflow

v4.2.2.1: Updated 12/03/2008
    New features:
      1) Add recording from multi monitor
      2) Add Noise effect to image effects
    Modify features:
      1) Fixed some errors
      2) Fixed memory overflow in low frame rate
    Remove features:
      1) Remove solarize filter effect from image effects

v4.0.1.0: Updated 11/18/2008
    New features:
      1) Add grayscale drawing (Capture And Preview)
      2) Add some image effects (Rotation, Brightness, Contrast, Color Adjusting, Saturation, Solarize)
    Modify features:
      1) Fixed some errors
    Remove features:
      No thing

v3.8.2.0: Updated 04/03/2008
    New features:
      No thing
    Modify features:
      1) Fixed error on selecting audio input.
    Remove features:
      No thing

v3.8.1.0: Updated 03/18/2008
    New features:
      1) Add overlay event for draw objects, picture, text and more over image.
      2) Add deleting event.
      3) Add correct frame rate info.
    Modify features:
      1) correction elapsed timer.
    Remove features:
      No thing

v3.5.3.2: Updated 03/07/2008
    New features:
      No thing
    Modify features:
      1) Canceling select region from object and windows on start record, that correct.
      2) Not synchronized record time with play time in full auto mode, that correct.
      3) Corrected some internal errors.
    Remove features:
      1) Remove capture timer and elapsed timer and add into record routin.
      2) Remove sleep timer on record (For full motion).

v3.5.0.1: Updated 02/28/2008
    New features:
      1) Upper interval TTimer (Because, sometimes system error).
      2) Lower sleep on upper frame rate during record (Softer motion).
      3) Not delete already temp audio/video files from temp directory, But can now.
      4) Add freehand window for free select region.
      5) Add select object window for select region from object or
          windows under mouse pointer.
    Modify features:
      No thing
    Remove features:
      1) Remove recompressing after record (Because, Some codecs, more the size of file).

v3.0.0.0: Released 11/20/2007
    First release.
................................................................................
}

unit scConsts;

interface

const
  isComponentName = 'Professional Screen Camera Component';
  isVersion = '4.7.1.0';
  isHistory = 'Release 01/01/2009';
  DeletingMsg = 'Deleting Temp File';
  PreviewMsg = 'Preview... Cancel: Esc';
  RecordingMsg = 'Recording... Cancel: Esc -- Stop: Shift+Esc';
  SavingMsg = 'Saving Video...';
  SavingSuccessMsg = 'Saving Success...';
  //error massages
  ErrorMsg1 = 'Unable to allocate memory for image';
  ErrorMsg2 = 'Unable to lock image handle at memory';
  ErrorMsg3 = 'Error retrieving image bits';
  ErrorMsg4 = 'Failure: Video for windows version too old!';
  ErrorMsg5 = 'Error capturing a frame from screen.';
  ErrorMsg6 = 'Error capturing a frame from screen.';
  ErrorMsg7 = 'Error: This compressor cann`t work if the dimensions is adjusted slightly'
                       + #13#10 + 'Use default compressor?';
  ErrorMsg8 = 'User aborted!';
  ErrorMsg9 = 'Error: Not recording AVI file using current compressor.'
                     + #13#10 + 'Use default compressor?';
  ErrorMsg10 = 'You no selected compressor, use default compressor?';
  ErrorMsg11 = 'Can not delete video temp file.';
  ErrorMsg12 = 'Error on create AVI file.';
  ErrorMsg13 = 'Error on create AVI stream.';
  ErrorMsg14 = 'Error on compress AVI stream.';
  ErrorMsg16 = 'Error on set AVI stream format.';
  ErrorMsg17 = 'Can not delete audio temp file.';
  ErrorMsg18 = 'Error capturing a frame from screen.';
  ErrorMsg19 = 'Error on write data to AVI stream.';
  ErrorMsg20 = 'Error creating AVI file with "MS Video Codec".';
  ErrorMsg21 = 'Can not delete existing video file.';
  ErrorMsg22 = 'Error: unsupported format!';
  ErrorMsg23 = 'Error: bad format!';
  ErrorMsg24 = 'Error: reading files!';
  ErrorMsg25 = 'Error: writing files!';
  ErrorMsg26 = 'Error: out of memory!';
  ErrorMsg27 = 'Error: internal error!';
  ErrorMsg28 = 'Error: bad flags!';
  ErrorMsg29 = 'Error: bad params!';
  ErrorMsg30 = 'Error: bad size!';
  ErrorMsg31 = 'Error: bad handle!';
  ErrorMsg32 = 'Error: opening files!';
  ErrorMsg33 = 'Error: in compressor!';
  ErrorMsg34 = 'Error: no compressor!';
  ErrorMsg35 = 'Error: read-only files!';
  ErrorMsg36 = 'Error: no data!';
  ErrorMsg37 = 'Error: buffer too small!';
  ErrorMsg38 = 'Error: cannot compress!';
  ErrorMsg39 = 'Error: user abort!';
  ErrorMsg40 = 'Error: generic!';
  ErrorMsg41 = 'Avi error number: ';
  ErrorMsg42 = 'The file could not be read, indicating a corrupt file or an unrecognized format.';
  ErrorMsg43 = 'The file could not be opened because of insufficient memory.';
  ErrorMsg44 = 'A disk error occurred while reading the stream file.';
  ErrorMsg45 = 'A disk error occurred while opening the stream file.';
  ErrorMsg46 = 'According to the registry, the type of stream file specified in AVIFileOpen does not have a handler to process it.';
  ErrorMsg47 = 'Unknown error opening stream file';
  ErrorMsg48 = 'Unable to get stream';
  ErrorMsg49 = 'Not exist file path.';

implementation

end.


