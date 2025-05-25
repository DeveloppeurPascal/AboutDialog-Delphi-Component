(* C2PP
  ***************************************************************************

  "About" dialog box as a Delphi Component

  Copyright 2022-2025 Patrick Prémartin under AGPL 3.0 license.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
  THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
  DEALINGS IN THE SOFTWARE.

  ***************************************************************************

  An "about" dialog box component for VCL and FireMonkey Delphi projects.

  ***************************************************************************

  Author(s) :
  Patrick PREMARTIN

  Site :
  https://dialogueapropos.developpeur-pascal.fr/

  Project site :
  https://github.com/DeveloppeurPascal/AboutDialog-Delphi-Component

  ***************************************************************************
  File last update : 2025-02-09T11:03:22.772+01:00
  Signature : 1b91f50e0546412082a52c7d094d744a230208bd
  ***************************************************************************
*)

program AboutDialogFMXSample;

// ********************************************************************************
// *
// * Some images from this project come from Adobe Stock. Don't use them outside
// * this program without buying a license from them.
// *
// ********************************************************************************

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit2 in 'Unit2.pas' {Form2},
  Olf.FMX.AboutDialog in '..\..\src\Olf.FMX.AboutDialog.pas',
  Olf.FMX.AboutDialogForm in '..\..\src\Olf.FMX.AboutDialogForm.pas' {OlfAboutDialogForm},
  u_urlOpen in '..\lib-externes\librairies\src\u_urlOpen.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
