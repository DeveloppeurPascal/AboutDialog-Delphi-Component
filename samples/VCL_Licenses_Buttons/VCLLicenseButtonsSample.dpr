program VCLLicenseButtonsSample;

uses
  Vcl.Forms,
  fMain in 'fMain.pas' {Form3},
  Olf.VCL.AboutDialog in '..\..\src\Olf.VCL.AboutDialog.pas',
  Olf.VCL.AboutDialogForm in '..\..\src\Olf.VCL.AboutDialogForm.pas' {OlfAboutDialogForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm3, Form3);
  Application.Run;
end.
