﻿/// <summary>
/// ***************************************************************************
///
/// "About" dialog box as a Delphi Component
///
/// Copyright 2022-2024 Patrick Prémartin under AGPL 3.0 license.
///
/// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
/// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
/// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
/// THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
/// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
/// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
/// DEALINGS IN THE SOFTWARE.
///
/// ***************************************************************************
///
/// An "about" dialog box component for VCL and FireMonkey Delphi projects.
///
/// ***************************************************************************
///
/// Author(s) :
///      Patrick PREMARTIN
///
/// Site :
///      https://dialogueapropos.developpeur-pascal.fr/
///
/// Project site :
///      https://github.com/DeveloppeurPascal/AboutDialog-Delphi-Component
///
/// ***************************************************************************
/// File last update : 28/05/2024 12:18:39
/// Signature : a5620a273fc4c018522a80f313425b4a8b180a87
/// ***************************************************************************
/// </summary>

program AboutDialogVCLSample;

// ********************************************************************************
// *
// * Some images from this project come from Adobe Stock. Don't use them outside
// * this program without buying a license from them.
// *
// ********************************************************************************

{$R *.dres}

uses
  Vcl.Forms,
  Unit1 in 'Unit1.pas' {Form1},
  Olf.VCL.AboutDialog in '..\..\src\Olf.VCL.AboutDialog.pas',
  Olf.VCL.AboutDialogForm in '..\..\src\Olf.VCL.AboutDialogForm.pas' {OlfAboutDialogForm},
  u_urlOpen in '..\lib-externes\librairies\src\u_urlOpen.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
