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
  File last update : 2025-02-09T11:03:22.756+01:00
  Signature : 182c34425a7bd648aa2453a48c35a77383594297
  ***************************************************************************
*)

unit fMain;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.Controls.Presentation,
  FMX.StdCtrls,
  Olf.FMX.AboutDialog,
  Olf.FMX.AboutDialogForm,
  FMX.Edit;

type
  TForm4 = class(TForm)
    OlfAboutDialog1: TOlfAboutDialog;
    Button1: TButton;
    edtCaptionSuffix: TEdit;
    procedure Button1Click(Sender: TObject);
    function OlfAboutDialog1GetText(const ALang: TOlfAboutDialogLang;
      const ATxtID: TOlfAboutDialogTxtID): string;
    procedure OlfAboutDialog1ButtonBuyClick(Sender: TObject);
    procedure OlfAboutDialog1ButtonLicenseClick(Sender: TObject);
    procedure OlfAboutDialog1ButtonRegisterClick(Sender: TObject);
    function OlfAboutDialog1GetFooterText(const ALang: TOlfAboutDialogLang;
      const ATxtID: TOlfAboutDialogTxtID): string;
    procedure FormCreate(Sender: TObject);
    procedure edtCaptionSuffixChange(Sender: TObject);
    procedure edtCaptionSuffixChangeTracking(Sender: TObject);
  private
  public
  end;

var
  Form4: TForm4;

implementation

{$R *.fmx}

procedure TForm4.Button1Click(Sender: TObject);
begin
  OlfAboutDialog1.Execute;
end;

procedure TForm4.edtCaptionSuffixChange(Sender: TObject);
begin
  OlfAboutDialog1.MainFormCaptionSuffix := '- ' + edtCaptionSuffix.Text;
end;

procedure TForm4.edtCaptionSuffixChangeTracking(Sender: TObject);
begin
  OlfAboutDialog1.MainFormCaptionSuffix := '- CT ' + edtCaptionSuffix.Text;
end;

procedure TForm4.FormCreate(Sender: TObject);
begin
  OlfAboutDialog1.MainFormCaptionPrefix := '';
end;

procedure TForm4.OlfAboutDialog1ButtonBuyClick(Sender: TObject);
begin
  ((Sender as TCustomButton).Owner as TOlfAboutDialogForm).Langue :=
    TOlfAboutDialogLang.fr;
  showmessage('buy');
end;

procedure TForm4.OlfAboutDialog1ButtonLicenseClick(Sender: TObject);
begin
  ((Sender as TCustomButton).Owner as TOlfAboutDialogForm).Langue :=
    TOlfAboutDialogLang.de;
  showmessage('license');
end;

procedure TForm4.OlfAboutDialog1ButtonRegisterClick(Sender: TObject);
begin
  ((Sender as TCustomButton).Owner as TOlfAboutDialogForm).Langue :=
    TOlfAboutDialogLang.it;
  showmessage('register');
end;

function TForm4.OlfAboutDialog1GetFooterText(const ALang: TOlfAboutDialogLang;
  const ATxtID: TOlfAboutDialogTxtID): string;
begin
  if random(100) > 50 then
    result := 'coucou ça va ?';
end;

function TForm4.OlfAboutDialog1GetText(const ALang: TOlfAboutDialogLang;
  const ATxtID: TOlfAboutDialogTxtID): string;
begin
  if random(100) > 50 then
    result := ord(ATxtID).tostring;
end;

initialization

randomize;

{$IFDEF DEBUG}
ReportMemoryLeaksOnShutdown := true;
{$ENDIF}

end.
