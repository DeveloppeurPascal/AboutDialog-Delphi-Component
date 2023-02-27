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

program AboutDialogFMXSample;

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit2 in 'Unit2.pas' {Form2},
  u_urlOpen in '..\u_urlOpen.pas',
  Olf.FMX.AboutDialogForm in '..\..\sources\Olf.FMX.AboutDialogForm.pas' {OlfAboutDialogForm},
  Olf.FMX.AboutDialog in '..\..\sources\Olf.FMX.AboutDialog.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
