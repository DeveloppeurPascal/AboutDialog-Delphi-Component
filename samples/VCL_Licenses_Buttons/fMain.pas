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
  Signature : d4d295dcc7dffeec37b7a03b96ce878a6d65d504
  ***************************************************************************
*)

unit fMain;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Olf.Vcl.AboutDialog,
  Vcl.StdCtrls,
  Olf.Vcl.AboutDialogForm,
  Vcl.Imaging.jpeg;

type
  TForm3 = class(TForm)
    OlfAboutDialog1: TOlfAboutDialog;
    Button1: TButton;
    edtCaptionSuffix: TEdit;
    procedure Button1Click(Sender: TObject);
    function OlfAboutDialog1GetText(const ALang: TOlfAboutDialogLang;
      const ATxtID: TOlfAboutDialogTxtID): string;
    function OlfAboutDialog1GetFooterText(const ALang: TOlfAboutDialogLang;
      const ATxtID: TOlfAboutDialogTxtID): string;
    procedure OlfAboutDialog1ButtonRegisterClick(Sender: TObject);
    procedure OlfAboutDialog1ButtonLicenseClick(Sender: TObject);
    procedure OlfAboutDialog1ButtonBuyClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure edtCaptionSuffixChange(Sender: TObject);
    procedure edtCaptionSuffixExit(Sender: TObject);
  private
  public
  end;

var
  Form3: TForm3;

implementation

{$R *.dfm}

procedure TForm3.Button1Click(Sender: TObject);
begin
  OlfAboutDialog1.Execute;
end;

procedure TForm3.edtCaptionSuffixChange(Sender: TObject);
begin
  OlfAboutDialog1.MainFormCaptionSuffix := '- CT ' + edtCaptionSuffix.Text;
end;

procedure TForm3.edtCaptionSuffixExit(Sender: TObject);
begin
  OlfAboutDialog1.MainFormCaptionSuffix := '- ' + edtCaptionSuffix.Text;
end;

procedure TForm3.FormCreate(Sender: TObject);
begin
  OlfAboutDialog1.MainFormCaptionPrefix := '';
end;

procedure TForm3.OlfAboutDialog1ButtonBuyClick(Sender: TObject);
begin
  ((Sender as TCustomButton).Owner as TOlfAboutDialogForm).Langue :=
    TOlfAboutDialogLang.fr;
  showmessage('buy button');
end;

procedure TForm3.OlfAboutDialog1ButtonLicenseClick(Sender: TObject);
begin
  ((Sender as TCustomButton).Owner as TOlfAboutDialogForm).Langue :=
    TOlfAboutDialogLang.de;
  showmessage('license button');
end;

procedure TForm3.OlfAboutDialog1ButtonRegisterClick(Sender: TObject);
begin
  ((Sender as TCustomButton).Owner as TOlfAboutDialogForm).Langue :=
    TOlfAboutDialogLang.it;
  showmessage('register button');
end;

function TForm3.OlfAboutDialog1GetFooterText(const ALang: TOlfAboutDialogLang;
  const ATxtID: TOlfAboutDialogTxtID): string;
begin
  result := 'hello world';
end;

function TForm3.OlfAboutDialog1GetText(const ALang: TOlfAboutDialogLang;
  const ATxtID: TOlfAboutDialogTxtID): string;
begin
  result := 'coucou';
end;

initialization

{$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := true;
{$ENDIF}

end.
