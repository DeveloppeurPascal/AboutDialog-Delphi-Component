(* C2PP
  ***************************************************************************

  "About" dialog box as a Delphi Component
  Copyright (c) 2022-2026 Patrick PREMARTIN

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU Affero General Public License as
  published by the Free Software Foundation, either version 3 of the
  License, or (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU Affero General Public License for more details.

  You should have received a copy of the GNU Affero General Public License
  along with this program.  If not, see <https://www.gnu.org/licenses/>.

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
  File last update : 2025-05-25T16:14:43.889+02:00
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
