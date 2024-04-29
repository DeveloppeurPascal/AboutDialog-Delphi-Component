unit fMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, Olf.FMX.AboutDialog,
  Olf.FMX.AboutDialogForm;

type
  TForm4 = class(TForm)
    OlfAboutDialog1: TOlfAboutDialog;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
    function OlfAboutDialog1GetText(const ALang: TOlfAboutDialogLang;
      const ATxtID: TOlfAboutDialogTxtID): string;
    procedure OlfAboutDialog1ButtonBuyClick(Sender: TObject);
    procedure OlfAboutDialog1ButtonLicenseClick(Sender: TObject);
    procedure OlfAboutDialog1ButtonRegisterClick(Sender: TObject);
    function OlfAboutDialog1GetFooterText(const ALang: TOlfAboutDialogLang;
      const ATxtID: TOlfAboutDialogTxtID): string;
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form4: TForm4;

implementation

{$R *.fmx}

procedure TForm4.Button1Click(Sender: TObject);
begin
  OlfAboutDialog1.Execute;
end;

procedure TForm4.OlfAboutDialog1ButtonBuyClick(Sender: TObject);
begin
  showmessage('buy');
end;

procedure TForm4.OlfAboutDialog1ButtonLicenseClick(Sender: TObject);
begin
  showmessage('license');
end;

procedure TForm4.OlfAboutDialog1ButtonRegisterClick(Sender: TObject);
begin
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
