unit RegisterArchiver;

interface

procedure Register;

implementation
uses Extractor, Archiver, SFXGenerator;

procedure Register;
begin
  Extractor.Register;
  Archiver.Register;
  SFXGenerator.Register;
end;

end.
