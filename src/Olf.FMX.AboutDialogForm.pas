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

unit Olf.FMX.AboutDialogForm;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.MultiResBitmap,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Objects,
  FMX.ImgList,
  FMX.Layouts,
  FMX.ExtCtrls,
  FMX.Controls.Presentation,
  FMX.StdCtrls;

type
{$SCOPEDENUMS on}
  TOlfAboutDialogLang = (FR, EN, IT, PT, SP, DE);

  TOlfAboutDialogURLClickEvent = procedure(const AURL: string) of object;
  TOlfAboutDialogCloseEvent = procedure of object;

  TOlfAboutDialogForm = class(TForm)
    zoneLogo: TLayout;
    LogoGlyph: TGlyph;
    LogoImage: TImage;
    lblTitre: TLabel;
    lblVersion: TLabel;
    lblURL: TLabel;
    btnClose: TButton;
    zoneTextes: TLayout;
    zoneBouton: TLayout;
    pnlDescription: TRectangle;
    VertScrollBox1: TVertScrollBox;
    lblDescription: TLabel;
    pnlLicence: TRectangle;
    VertScrollBox2: TVertScrollBox;
    lblLicence: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure lblURLClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnCloseClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure zoneTextesResize(Sender: TObject);
  private type
    TTxtID = (APropos, Version, Du, VersionDu, BoutonFermer);

  var
    FTitre: string;
    FVersionDate: string;
    FVersionNumero: string;
    FDescription: string;
    FLicence: string;
    FURL: string;
    FonCloseDialog: TOlfAboutDialogCloseEvent;
    FonURLClick: TOlfAboutDialogURLClickEvent;
    FLangue: TOlfAboutDialogLang;
    procedure SetonURLClick(const Value: TOlfAboutDialogURLClickEvent);
    procedure SetDescription(const Value: string);
    procedure SetImage(const Value: TImage);
    procedure SetLicence(const Value: string);
    procedure SetMultiResBitmap(const Value: TFixedMultiResBitmap);
    procedure SetTitre(const Value: string);
    procedure SetURL(const Value: string);
    procedure SetVersionDate(const Value: string);
    procedure SetVersionNumero(const Value: string);
    { Déclarations privées }
    procedure AfficheVersionEtVersionDate;
    procedure AfficheZoneLogo;
    procedure SetonCloseDialog(const Value: TOlfAboutDialogCloseEvent);
    procedure SetLangue(const Value: TOlfAboutDialogLang);
    function getTraduction(TxtID: TTxtID): string;
  public
    { Déclarations publiques }
    property Titre: string read FTitre write SetTitre;
    property VersionNumero: string read FVersionNumero write SetVersionNumero;
    property VersionDate: string read FVersionDate write SetVersionDate;
    property Image: TImage write SetImage;
    property MultiResBitmap: TFixedMultiResBitmap write SetMultiResBitmap;
    property URL: string read FURL write SetURL;
    property Licence: string read FLicence write SetLicence;
    property Description: string read FDescription write SetDescription;
    property onCloseDialog: TOlfAboutDialogCloseEvent read FonCloseDialog
      write SetonCloseDialog;
    property onURLClick: TOlfAboutDialogURLClickEvent read FonURLClick
      write SetonURLClick;
    property Langue: TOlfAboutDialogLang read FLangue write SetLangue;
    procedure SetImageList(ImageList: TCustomImageList;
      ImageListIndex: System.UITypes.TImageIndex = -1);
  end;

implementation

{$R *.fmx}

procedure TOlfAboutDialogForm.AfficheVersionEtVersionDate;
begin
  if not FVersionNumero.IsEmpty then
  begin
    lblVersion.Text := getTraduction(TTxtID.Version) + FVersionNumero;
    if not FVersionDate.IsEmpty then
      lblVersion.Text := lblVersion.Text + getTraduction(TTxtID.Du) +
        FVersionDate;
  end
  else if not FVersionDate.IsEmpty then
    lblVersion.Text := getTraduction(TTxtID.VersionDu) + FVersionDate
  else
    lblVersion.Text := '';
end;

procedure TOlfAboutDialogForm.AfficheZoneLogo;
begin
  zoneLogo.Visible := LogoGlyph.Visible or LogoImage.Visible;
  if zoneLogo.Visible then
    if (width < height) then
    begin
      zoneLogo.Align := TAlignLayout.MostTop;
      zoneLogo.height := 200;
    end
    else
    begin
      zoneLogo.Align := TAlignLayout.MostLeft;
      zoneLogo.width := 200;
    end;
end;

procedure TOlfAboutDialogForm.btnCloseClick(Sender: TObject);
begin
  if assigned(FonCloseDialog) then
    FonCloseDialog;
  close;
end;

procedure TOlfAboutDialogForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
{$IF Defined(IOS) or Defined(ANDROID)}
  TThread.ForceQueue(nil,
    procedure
    begin
      self.Free;
    end);
{$ENDIF}
end;

procedure TOlfAboutDialogForm.FormCreate(Sender: TObject);
begin
  Titre := 'titre du logiciel';
  VersionNumero := '';
  VersionDate := '';
  Image := nil;
  MultiResBitmap := nil;
  LogoGlyph.Visible := false;
  LogoImage.Visible := false;
  zoneLogo.Visible := false;
  URL := '';
  Licence := '';
  Description := '';
  Langue := TOlfAboutDialogLang.FR;
end;

procedure TOlfAboutDialogForm.FormResize(Sender: TObject);
begin
  AfficheZoneLogo;
end;

function TOlfAboutDialogForm.getTraduction(TxtID: TTxtID): string;
begin
{$I traduction_textes.inc}
end;

procedure TOlfAboutDialogForm.lblURLClick(Sender: TObject);
begin
  if assigned(FonURLClick) then
    FonURLClick(lblURL.Text);
end;

procedure TOlfAboutDialogForm.SetDescription(const Value: string);
begin
  FDescription := Value;
  if FDescription.IsEmpty then
  begin
    pnlDescription.Visible := false;
    if pnlLicence.Visible then
      pnlLicence.Align := TAlignLayout.client;
  end
  else
  begin
    pnlDescription.Visible := true;
    lblDescription.Text := FDescription;
    if pnlLicence.Visible then
    begin
      pnlDescription.Align := TAlignLayout.top;
      pnlLicence.Align := TAlignLayout.client;
      pnlDescription.height := (zoneTextes.height / 2) -
        pnlDescription.margins.top - pnlDescription.margins.bottom;
    end
    else
      pnlDescription.Align := TAlignLayout.client;
  end;
end;

procedure TOlfAboutDialogForm.SetImage(const Value: TImage);
begin
  if assigned(Value) then
    SetMultiResBitmap(Value.MultiResBitmap)
  else
    LogoImage.Visible := false;
  AfficheZoneLogo;
end;

procedure TOlfAboutDialogForm.SetImageList(ImageList: TCustomImageList;
ImageListIndex: System.UITypes.TImageIndex);
begin
  if assigned(ImageList) and (ImageListIndex > -1) then
  begin
    LogoImage.Visible := false;
    LogoGlyph.Visible := true;
    LogoGlyph.Images := ImageList;
    LogoGlyph.ImageIndex := ImageListIndex;
  end
  else
    LogoGlyph.Visible := false;
  AfficheZoneLogo;
end;

procedure TOlfAboutDialogForm.SetLangue(const Value: TOlfAboutDialogLang);
begin
  FLangue := Value;
  btnClose.Text := getTraduction(TTxtID.BoutonFermer);
end;

procedure TOlfAboutDialogForm.SetLicence(const Value: string);
begin
  FLicence := Value;
  if FLicence.IsEmpty then
  begin
    pnlLicence.Visible := false;
    if pnlDescription.Visible then
      pnlDescription.Align := TAlignLayout.client;
  end
  else
  begin
    pnlLicence.Visible := true;
    lblLicence.Text := FLicence;
    if pnlDescription.Visible then
    begin
      pnlDescription.Align := TAlignLayout.top;
      pnlLicence.Align := TAlignLayout.client;
      pnlDescription.height := (zoneTextes.height / 2) -
        pnlDescription.margins.top - pnlDescription.margins.bottom;
    end
    else
      pnlLicence.Align := TAlignLayout.client;
  end;
end;

procedure TOlfAboutDialogForm.SetMultiResBitmap(const Value
  : TFixedMultiResBitmap);
begin
  if assigned(Value) then
  begin
    LogoImage.Visible := true;
    LogoGlyph.Visible := false;
    LogoImage.MultiResBitmap.Assign(Value);
  end
  else
    LogoImage.Visible := false;
  AfficheZoneLogo;
end;

procedure TOlfAboutDialogForm.SetonCloseDialog(const Value
  : TOlfAboutDialogCloseEvent);
begin
  FonCloseDialog := Value;
end;

procedure TOlfAboutDialogForm.SetonURLClick(const Value
  : TOlfAboutDialogURLClickEvent);
begin
  FonURLClick := Value;

  if assigned(FonURLClick) then
    lblURL.Cursor := crHandPoint
  else
    lblURL.Cursor := crDefault;
end;

procedure TOlfAboutDialogForm.SetTitre(const Value: string);
begin
  FTitre := Value;
  lblTitre.Text := FTitre;
  Caption := getTraduction(TTxtID.APropos) + FTitre;
end;

procedure TOlfAboutDialogForm.SetURL(const Value: string);
begin
  FURL := Value;
  if FURL.IsEmpty then
    lblURL.Visible := false
  else
  begin
    lblURL.Text := FURL;
    lblURL.Visible := true;
  end;
end;

procedure TOlfAboutDialogForm.SetVersionDate(const Value: string);
begin
  FVersionDate := Value;
  AfficheVersionEtVersionDate;
end;

procedure TOlfAboutDialogForm.SetVersionNumero(const Value: string);
begin
  FVersionNumero := Value;
  AfficheVersionEtVersionDate;
end;

procedure TOlfAboutDialogForm.zoneTextesResize(Sender: TObject);
begin
  if pnlDescription.Visible and pnlLicence.Visible then
    pnlDescription.height := (zoneTextes.height / 2) -
      pnlDescription.margins.top - pnlDescription.margins.bottom;
end;

end.
