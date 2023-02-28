// ********************************************************************************
// *
// * About Dialog component
// *
// * (c) 2022 Patrick Premartin
// *
// ********************************************************************************
// *
// * Project website :
// *	https://dialogueapropos.developpeur-pascal.fr/
// *
// * You have bugs, requests or questions, use GitHub issues :
// *	https://github.com/DeveloppeurPascal/AboutDialog-Delphi-Component/issues
// *
// ********************************************************************************
// *
// * This is a commercial project.
// * If you don't have a license please buy one.
// *
// ********************************************************************************

// ********************************************************************************
// * 
// * Some images from this project come from Adobe Stock. Don't use them outside 
// * this program without buying a license from them.
// * 
// ********************************************************************************

program AboutDialogVCLSample;

{$R *.dres}

uses
  Vcl.Forms,
  Unit1 in 'Unit1.pas' {Form1},
  Olf.VCL.AboutDialogForm in '..\..\sources\Olf.VCL.AboutDialogForm.pas' {OlfAboutDialogForm},
  Olf.VCL.AboutDialog in '..\..\sources\Olf.VCL.AboutDialog.pas',
  u_urlOpen in '..\lib-externes\librairies\u_urlOpen.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TOlfAboutDialogForm, OlfAboutDialogForm);
  Application.Run;
end.
