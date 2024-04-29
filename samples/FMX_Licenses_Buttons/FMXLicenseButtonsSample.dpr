program FMXLicenseButtonsSample;

uses
  System.StartUpCopy,
  FMX.Forms,
  fMain in 'fMain.pas' {Form4},
  Olf.FMX.AboutDialog in '..\..\src\Olf.FMX.AboutDialog.pas',
  Olf.FMX.AboutDialogForm in '..\..\src\Olf.FMX.AboutDialogForm.pas' {OlfAboutDialogForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm4, Form4);
  Application.Run;
end.
