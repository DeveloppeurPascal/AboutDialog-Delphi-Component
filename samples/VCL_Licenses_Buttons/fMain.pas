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
    procedure Button1Click(Sender: TObject);
    function OlfAboutDialog1GetText(const ALang: TOlfAboutDialogLang;
      const ATxtID: TOlfAboutDialogTxtID): string;
    function OlfAboutDialog1GetFooterText(const ALang: TOlfAboutDialogLang;
      const ATxtID: TOlfAboutDialogTxtID): string;
    procedure OlfAboutDialog1ButtonRegisterClick(Sender: TObject);
    procedure OlfAboutDialog1ButtonLicenseClick(Sender: TObject);
    procedure OlfAboutDialog1ButtonBuyClick(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form3: TForm3;

implementation

{$R *.dfm}

procedure TForm3.Button1Click(Sender: TObject);
begin
  OlfAboutDialog1.Execute;
end;

procedure TForm3.OlfAboutDialog1ButtonBuyClick(Sender: TObject);
begin
  showmessage('buy button');
end;

procedure TForm3.OlfAboutDialog1ButtonLicenseClick(Sender: TObject);
begin
  showmessage('license button');
end;

procedure TForm3.OlfAboutDialog1ButtonRegisterClick(Sender: TObject);
begin
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
