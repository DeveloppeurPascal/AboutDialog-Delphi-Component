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

unit Olf.VCL.AboutDialogForm;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  VCL.Graphics,
  System.UITypes,
  VCL.ImgList,
  VCL.Controls,
  VCL.Forms,
  VCL.Dialogs,
  VCL.ExtCtrls,
  VCL.StdCtrls,
  VCL.Buttons;

type
{$SCOPEDENUMS on}
  TOlfAboutDialogLang = (FR, EN, IT, PT, SP, DE);

  TOlfAboutDialogURLClickEvent = procedure(const AURL: string) of object;
  TOlfAboutDialogCloseEvent = procedure of object;

  TOlfAboutDialogForm = class(TForm)
    imgLogo: TImage;
    Panel1: TPanel;
    lblTitre: TStaticText;
    lblVersion: TStaticText;
    lblURL: TLabel;
    pnlDescription: TPanel;
    pnlLicence: TPanel;
    lblDescription: TLabel;
    lblLicence: TLabel;
    btnClose: TBitBtn;
    ScrollBox1: TScrollBox;
    ScrollBox2: TScrollBox;
    pnlTop: TPanel;
    pnlclient: TPanel;
    pnlBottom: TPanel;
    lblCopyright: TStaticText;
    procedure lblURLClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnCloseKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure btnCloseClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormActivate(Sender: TObject);
  private

    type
    TTxtID = (APropos, Version, Du, VersionDu, BoutonFermer);

  var
    FVersionNumero: string;
    FTitre: string;
    FVersionDate: string;
    FURL: string;
    FDescription: string;
    FLicence: string;
    FImage: TImage;
    FPicture: TPicture;
    FonCloseDialog: TOlfAboutDialogCloseEvent;
    FonURLClick: TOlfAboutDialogURLClickEvent;
    FLangue: TOlfAboutDialogLang;
    FCopyright: string;
    FonFormClose: TNotifyEvent;
    FonFormShow: TNotifyEvent;
    FonFormActivate: TNotifyEvent;
    procedure SetonFormActivate(const Value: TNotifyEvent);
    procedure SetonFormClose(const Value: TNotifyEvent);
    procedure SetonFormShow(const Value: TNotifyEvent);
    procedure SetCopyright(const Value: string);
    procedure SetVersionDate(const Value: string);
    procedure SetDescription(const Value: string);
    procedure SetLicence(const Value: string);
    procedure SetTitre(const Value: string);
    procedure SetURL(const Value: string);
    procedure SetVersionNumero(const Value: string);
    procedure SetImage(const Value: TImage);
    procedure AfficheVersionEtVersionDate;
    procedure CalculeHauteurEntete;
    procedure SetPicture(const Value: TPicture);
    procedure SetonCloseDialog(const Value: TOlfAboutDialogCloseEvent);
    procedure SetonURLClick(const Value: TOlfAboutDialogURLClickEvent);
    procedure SetLangue(const Value: TOlfAboutDialogLang);
    function getTraduction(TxtID: TTxtID): string;
  public
    property Titre: string read FTitre write SetTitre;
    property VersionNumero: string read FVersionNumero write SetVersionNumero;
    property VersionDate: string read FVersionDate write SetVersionDate;
    property Image: TImage read FImage write SetImage;
    property Picture: TPicture read FPicture write SetPicture;
    property URL: string read FURL write SetURL;
    property Licence: string read FLicence write SetLicence;
    property Description: string read FDescription write SetDescription;
    property onCloseDialog: TOlfAboutDialogCloseEvent read FonCloseDialog
      write SetonCloseDialog;
    property onURLClick: TOlfAboutDialogURLClickEvent read FonURLClick
      write SetonURLClick;
    property Langue: TOlfAboutDialogLang read FLangue write SetLangue;
    property Copyright: string read FCopyright write SetCopyright;

    property onFormActivate: TNotifyEvent read FonFormActivate
      write SetonFormActivate;
    property onFormShow: TNotifyEvent read FonFormShow write SetonFormShow;
    property onFormClose: TNotifyEvent read FonFormClose write SetonFormClose;

{$IF CompilerVersion >= 34.0}
    procedure SetImageList(ImageList: TCustomImageList;
      ImageListIndex: System.UITypes.TImageIndex = -1;
      ImageListName: TImageName = ''); overload;
{$ENDIF}
    procedure SetImageList(ImageList: TCustomImageList;
      ImageListIndex: System.UITypes.TImageIndex = -1); overload;
  end;

var
  OlfAboutDialogForm: TOlfAboutDialogForm;

implementation

{$R *.dfm}
{ TOlfAboutDialog }

procedure TOlfAboutDialogForm.btnCloseClick(Sender: TObject);
begin
  if assigned(FonCloseDialog) then
    FonCloseDialog;
  close;
end;

procedure TOlfAboutDialogForm.btnCloseKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
  begin
    Key := 0;
    close;
  end;
end;

procedure TOlfAboutDialogForm.CalculeHauteurEntete;
var
  hauteur: integer;
begin
  hauteur := 0;
  if lblTitre.visible then
    hauteur := hauteur + lblTitre.Margins.Top + lblTitre.Height +
      lblTitre.Margins.Bottom;
  if lblVersion.visible then
    hauteur := hauteur + lblVersion.Margins.Top + lblVersion.Height +
      lblVersion.Margins.Bottom;
  if lblURL.visible then
    hauteur := hauteur + lblURL.Margins.Top + lblURL.Height +
      lblURL.Margins.Bottom;
  if lblCopyright.visible then
    hauteur := hauteur + lblCopyright.Margins.Top + lblCopyright.Height +
      lblCopyright.Margins.Bottom;
  pnlTop.Height := hauteur;
end;

procedure TOlfAboutDialogForm.FormActivate(Sender: TObject);
begin
  if assigned(FonFormActivate) then
    FonFormActivate(self);
end;

procedure TOlfAboutDialogForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if assigned(FonFormClose) then
    FonFormClose(self);
end;

procedure TOlfAboutDialogForm.FormCreate(Sender: TObject);
begin
  Titre := 'titre du logiciel';
  VersionNumero := '';
  VersionDate := '';
  Image := nil;
  Picture := nil;
  imgLogo.visible := false;
  URL := '';
  Licence := '';
  Description := '';
  Langue := TOlfAboutDialogLang.FR;
  Copyright := '';
  FonCloseDialog := nil;
  FonURLClick := nil;
  FonFormActivate := nil;
  FonFormShow := nil;
  FonFormClose := nil;
end;

procedure TOlfAboutDialogForm.FormShow(Sender: TObject);
begin
  if assigned(FonFormShow) then
    FonFormShow(self);
end;

function TOlfAboutDialogForm.getTraduction(TxtID: TTxtID): string;
begin
{$I traduction_textes.inc}
end;

procedure TOlfAboutDialogForm.lblURLClick(Sender: TObject);
begin
  if assigned(FonURLClick) then
    FonURLClick(lblURL.caption);
end;

procedure TOlfAboutDialogForm.SetVersionDate(const Value: string);
begin
  FVersionDate := Value;
  AfficheVersionEtVersionDate;
end;

procedure TOlfAboutDialogForm.SetImage(const Value: TImage);
{$IF CompilerVersion >= 32.0}
// Delphi 10.2 Tokyo and after
var
  ms: TMemoryStream;
{$ENDIF}
begin
  if assigned(Value) then
  begin
    FImage := Value;
{$IF CompilerVersion >= 32.0}
    ms := TMemoryStream.Create;
    try
      FImage.Picture.SaveToStream(ms);
      ms.Position := 0;
      imgLogo.Picture.loadfromstream(ms);
    finally
      ms.Free;
    end;
{$ELSE}
    imgLogo.Picture.Assign(FImage.Picture);
{$ENDIF}
    imgLogo.visible := true;
  end
  else
    FImage := nil;
end;

{$IF CompilerVersion >= 34.0}

procedure TOlfAboutDialogForm.SetImageList(ImageList: TCustomImageList;
  ImageListIndex: System.UITypes.TImageIndex; ImageListName: TImageName);
begin
  if assigned(ImageList) then
  begin
    ImageList.CheckIndexAndName(ImageListIndex, ImageListName);
    if (ImageListIndex > -1) then
    begin
      ImageList.GetBitmap(ImageListIndex, imgLogo.Picture.Bitmap);
      imgLogo.visible := true;
    end;
  end;
end;
{$ENDIF}

procedure TOlfAboutDialogForm.SetImageList(ImageList: TCustomImageList;
  ImageListIndex: System.UITypes.TImageIndex);
begin
  if assigned(ImageList) and (ImageListIndex > -1) then
  begin
    ImageList.GetBitmap(ImageListIndex, imgLogo.Picture.Bitmap);
    imgLogo.visible := true;
  end;
end;

procedure TOlfAboutDialogForm.SetPicture(const Value: TPicture);
begin
  if assigned(Value) then
  begin
    FPicture := Value;
    imgLogo.Picture.Assign(FPicture);
    imgLogo.visible := true;
  end
  else
    FPicture := nil;
end;

procedure TOlfAboutDialogForm.AfficheVersionEtVersionDate;
begin
  if not FVersionNumero.IsEmpty then
  begin
    lblVersion.caption := getTraduction(TOlfAboutDialogForm.TTxtID.Version) +
      FVersionNumero;
    if not FVersionDate.IsEmpty then
      lblVersion.caption := lblVersion.caption +
        getTraduction(TOlfAboutDialogForm.TTxtID.Du) + FVersionDate;
  end
  else if not FVersionDate.IsEmpty then
    lblVersion.caption := getTraduction(TOlfAboutDialogForm.TTxtID.VersionDu) +
      FVersionDate
  else
    lblVersion.caption := '';
  CalculeHauteurEntete;
end;

procedure TOlfAboutDialogForm.SetCopyright(const Value: string);
begin
  FCopyright := Value;
  if FCopyright.IsEmpty then
    lblCopyright.visible := false
  else
  begin
    lblCopyright.caption := FCopyright;
    lblCopyright.visible := true;
  end;
  CalculeHauteurEntete;
end;

procedure TOlfAboutDialogForm.SetDescription(const Value: string);
begin
  FDescription := Value;
  if FDescription.IsEmpty then
  begin
    pnlDescription.visible := false;
    if pnlLicence.visible then
      pnlLicence.Align := alclient;
  end
  else
  begin
    pnlDescription.visible := true;
    lblDescription.caption := FDescription;
    if pnlLicence.visible then
    begin
      pnlDescription.Align := altop;
      pnlLicence.Align := alclient;
      pnlDescription.Height := pnlclient.Height div 2;
    end
    else
      pnlDescription.Align := alclient;
  end;
end;

procedure TOlfAboutDialogForm.SetLangue(const Value: TOlfAboutDialogLang);
begin
  FLangue := Value;
  btnClose.caption := getTraduction(TOlfAboutDialogForm.TTxtID.BoutonFermer);
end;

procedure TOlfAboutDialogForm.SetLicence(const Value: string);
begin
  FLicence := Value;
  if FLicence.IsEmpty then
  begin
    pnlLicence.visible := false;
    if pnlDescription.visible then
      pnlDescription.Align := alclient;
  end
  else
  begin
    pnlLicence.visible := true;
    lblLicence.caption := FLicence;
    if pnlDescription.visible then
    begin
      pnlDescription.Align := altop;
      pnlLicence.Align := alclient;
      pnlDescription.Height := pnlclient.Height div 2;
    end
    else
      pnlLicence.Align := alclient;
  end;
end;

procedure TOlfAboutDialogForm.SetonCloseDialog(const Value
  : TOlfAboutDialogCloseEvent);
begin
  FonCloseDialog := Value;
end;

procedure TOlfAboutDialogForm.SetonFormActivate(const Value: TNotifyEvent);
begin
  FonFormActivate := Value;
end;

procedure TOlfAboutDialogForm.SetonFormClose(const Value: TNotifyEvent);
begin
  FonFormClose := Value;
end;

procedure TOlfAboutDialogForm.SetonFormShow(const Value: TNotifyEvent);
begin
  FonFormShow := Value;
end;

procedure TOlfAboutDialogForm.SetonURLClick(const Value
  : TOlfAboutDialogURLClickEvent);
begin
  FonURLClick := Value;

  if assigned(FonURLClick) then
  begin
    lblURL.Cursor := crHandPoint;
    lblURL.Font.Color := clHighlight;
    lblURL.Font.Style := [TFontStyle.fsUnderline];
  end
  else
  begin
    lblURL.Cursor := crDefault;
    lblURL.Font.Color := clWindowText;
    lblURL.Font.Style := [];
  end;
end;

procedure TOlfAboutDialogForm.SetTitre(const Value: string);
begin
  FTitre := Value;
  lblTitre.caption := FTitre;
  caption := getTraduction(TOlfAboutDialogForm.TTxtID.APropos) + FTitre;
  CalculeHauteurEntete;
end;

procedure TOlfAboutDialogForm.SetURL(const Value: string);
begin
  FURL := Value;
  if FURL.IsEmpty then
    lblURL.visible := false
  else
  begin
    lblURL.caption := FURL;
    lblURL.visible := true;
  end;
  CalculeHauteurEntete;
end;

procedure TOlfAboutDialogForm.SetVersionNumero(const Value: string);
begin
  FVersionNumero := Value;
  AfficheVersionEtVersionDate;
end;

end.
