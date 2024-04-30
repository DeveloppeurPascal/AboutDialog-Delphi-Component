// ********************************************************************************
// *
// * About Dialog component
// *
// * (c) 2022-2024 Patrick Premartin
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
  TOlfAboutDialogLang = (FR, EN, IT, PT, SP, DE, Manual, Auto);
  TOlfAboutDialogTxtID = (About, Version, Date, VersionDate, CloseButton,
    Footer, LicenseInfoButton, BuyButton, RegisterButton);

  TNotifyProc = reference to procedure(Sender: TObject);

  TOlfAboutDialogGetTextEvent = function(Const ALang: TOlfAboutDialogLang;
    Const ATxtID: TOlfAboutDialogTxtID): string of object;
  TOlfAboutDialogGetTextProc = reference to function
    (Const ALang: TOlfAboutDialogLang;
    Const ATxtID: TOlfAboutDialogTxtID): string;

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
    btnLicenseInfo: TBitBtn;
    btnBuy: TBitBtn;
    btnRegister: TBitBtn;
    lblFooter: TLabel;
    procedure lblURLClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnCloseKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure btnCloseClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormActivate(Sender: TObject);
    procedure btnLicenseInfoClick(Sender: TObject);
    procedure btnBuyClick(Sender: TObject);
    procedure btnRegisterClick(Sender: TObject);
  private
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
    FLangue, FInternalLangue: TOlfAboutDialogLang;
    FCopyright: string;
    FonFormClose: TNotifyEvent;
    FonFormShow: TNotifyEvent;
    FonFormActivate: TNotifyEvent;
    FonButtonLicenseClickProc: TNotifyEvent;
    FonGetFooterTextProc: TOlfAboutDialogGetTextProc;
    FonButtonLicenseClick: TNotifyEvent;
    FonGetFooterText: TOlfAboutDialogGetTextEvent;
    FonButtonRegisterClickProc: TNotifyEvent;
    FonButtonRegisterClick: TNotifyEvent;
    FonGetTextProc: TOlfAboutDialogGetTextProc;
    FonGetText: TOlfAboutDialogGetTextEvent;
    FonButtonBuyClickProc: TNotifyEvent;
    FonButtonBuyClick: TNotifyEvent;
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
    function getTraduction(TxtID: TOlfAboutDialogTxtID): string;
    procedure SetonButtonBuyClick(const Value: TNotifyEvent);
    procedure SetonButtonBuyClickProc(const Value: TNotifyEvent);
    procedure SetonButtonLicenseClick(const Value: TNotifyEvent);
    procedure SetonButtonLicenseClickProc(const Value: TNotifyEvent);
    procedure SetonButtonRegisterClick(const Value: TNotifyEvent);
    procedure SetonButtonRegisterClickProc(const Value: TNotifyEvent);
    procedure SetonGetFooterText(const Value: TOlfAboutDialogGetTextEvent);
    procedure SetonGetFooterTextProc(const Value: TOlfAboutDialogGetTextProc);
    procedure SetonGetText(const Value: TOlfAboutDialogGetTextEvent);
    procedure SetonGetTextProc(const Value: TOlfAboutDialogGetTextProc);
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

    property onButtonLicenseClick: TNotifyEvent read FonButtonLicenseClick
      write SetonButtonLicenseClick;
    property onButtonLicenseClickProc: TNotifyEvent
      read FonButtonLicenseClickProc write SetonButtonLicenseClickProc;
    property onButtonBuyClick: TNotifyEvent read FonButtonBuyClick
      write SetonButtonBuyClick;
    property onButtonBuyClickProc: TNotifyEvent read FonButtonBuyClickProc
      write SetonButtonBuyClickProc;
    property onButtonRegisterClick: TNotifyEvent read FonButtonRegisterClick
      write SetonButtonRegisterClick;
    property onButtonRegisterClickProc: TNotifyEvent
      read FonButtonRegisterClickProc write SetonButtonRegisterClickProc;
    property onGetText: TOlfAboutDialogGetTextEvent read FonGetText
      write SetonGetText;
    property onGetTextProc: TOlfAboutDialogGetTextProc read FonGetTextProc
      write SetonGetTextProc;
    property onGetFooterText: TOlfAboutDialogGetTextEvent read FonGetFooterText
      write SetonGetFooterText;
    property onGetFooterTextProc: TOlfAboutDialogGetTextProc
      read FonGetFooterTextProc write SetonGetFooterTextProc;
  end;

implementation

{$R *.dfm}

function GetCurrentLanguageCode: String;
// copied from https://github.com/DeveloppeurPascal/librairies/blob/master/src/Olf.RTL.Language.pas
var
  buffer: PWideChar;
  UserLCID: LCID;
  BufLen: Integer;
begin
  // defaults
  UserLCID := GetUserDefaultLCID;
  BufLen := GetLocaleInfo(UserLCID, LOCALE_SISO639LANGNAME, nil, 0);
  buffer := StrAlloc(BufLen);
  if GetLocaleInfo(UserLCID, LOCALE_SISO639LANGNAME, buffer, BufLen) <> 0 then
    Result := lowercase(buffer)
  else
    Result := 'en';
  StrDispose(buffer);
end;

function GetCurrentLanguageISOCode: String;
// copied from https://github.com/DeveloppeurPascal/librairies/blob/master/src/Olf.RTL.Language.pas
begin
  Result := GetCurrentLanguageCode.Substring(0, 2);
end;

procedure TOlfAboutDialogForm.btnBuyClick(Sender: TObject);
begin
  if assigned(FonButtonBuyClickProc) then
    FonButtonBuyClickProc(Sender)
  else if assigned(FonButtonBuyClick) then
    FonButtonBuyClick(Sender);
end;

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

procedure TOlfAboutDialogForm.btnLicenseInfoClick(Sender: TObject);
begin
  if assigned(FonButtonLicenseClickProc) then
    FonButtonLicenseClickProc(Sender)
  else if assigned(FonButtonLicenseClick) then
    FonButtonLicenseClick(Sender);
end;

procedure TOlfAboutDialogForm.btnRegisterClick(Sender: TObject);
begin
  if assigned(FonButtonRegisterClickProc) then
    FonButtonRegisterClickProc(Sender)
  else if assigned(FonButtonRegisterClick) then
    FonButtonRegisterClick(Sender);
end;

procedure TOlfAboutDialogForm.CalculeHauteurEntete;
var
  hauteur: Integer;
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
  FonButtonLicenseClickProc := nil;
  FonButtonLicenseClick := nil;
  FonButtonRegisterClickProc := nil;
  FonButtonRegisterClick := nil;
  FonButtonBuyClickProc := nil;
  FonButtonBuyClick := nil;
  FonGetFooterTextProc := nil;
  FonGetFooterText := nil;
  FonGetTextProc := nil;
  FonGetText := nil;
end;

procedure TOlfAboutDialogForm.FormShow(Sender: TObject);
begin
  btnLicenseInfo.Caption :=
    getTraduction(TOlfAboutDialogTxtID.LicenseInfoButton);
  btnBuy.Caption := getTraduction(TOlfAboutDialogTxtID.BuyButton);
  btnRegister.Caption := getTraduction(TOlfAboutDialogTxtID.RegisterButton);
  btnClose.Caption := getTraduction(TOlfAboutDialogTxtID.CloseButton);

  btnLicenseInfo.visible := assigned(onButtonLicenseClick) or
    assigned(onButtonLicenseClickProc);
  btnBuy.visible := assigned(onButtonBuyClick) or
    assigned(onButtonBuyClickProc);
  btnRegister.visible := assigned(onButtonRegisterClick) or
    assigned(onButtonRegisterClickProc);

  if assigned(onGetFooterTextProc) then
    lblFooter.Caption := onGetFooterTextProc(FInternalLangue,
      TOlfAboutDialogTxtID.Footer)
  else if assigned(onGetFooterText) then
    lblFooter.Caption := onGetFooterText(FInternalLangue,
      TOlfAboutDialogTxtID.Footer)
  else
    lblFooter.Caption := '';
  lblFooter.visible := lblFooter.Caption <> '';

  if assigned(FonFormShow) then
    FonFormShow(self);
end;

function TOlfAboutDialogForm.getTraduction(TxtID: TOlfAboutDialogTxtID): string;
begin
{$I traduction_textes.inc}
end;

procedure TOlfAboutDialogForm.lblURLClick(Sender: TObject);
begin
  if assigned(FonURLClick) then
    FonURLClick(lblURL.Caption);
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
    lblVersion.Caption := getTraduction(TOlfAboutDialogTxtID.Version) +
      FVersionNumero;
    if not FVersionDate.IsEmpty then
      lblVersion.Caption := lblVersion.Caption +
        getTraduction(TOlfAboutDialogTxtID.Date) + FVersionDate;
  end
  else if not FVersionDate.IsEmpty then
    lblVersion.Caption := getTraduction(TOlfAboutDialogTxtID.VersionDate) +
      FVersionDate
  else
    lblVersion.Caption := '';
  CalculeHauteurEntete;
end;

procedure TOlfAboutDialogForm.SetCopyright(const Value: string);
begin
  FCopyright := Value;
  if FCopyright.IsEmpty then
    lblCopyright.visible := false
  else
  begin
    lblCopyright.Caption := FCopyright;
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
    lblDescription.Caption := FDescription;
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
var
  ISO: string;
begin
  FLangue := Value;
  if FLangue = TOlfAboutDialogLang.Auto then
  begin
    ISO := GetCurrentLanguageISOCode;
    if ISO = 'fr' then
      FInternalLangue := TOlfAboutDialogLang.FR
    else if ISO = 'en' then
      FInternalLangue := TOlfAboutDialogLang.EN
    else if ISO = 'it' then
      FInternalLangue := TOlfAboutDialogLang.IT
    else if ISO = 'pt' then
      FInternalLangue := TOlfAboutDialogLang.PT
    else if ISO = 'sp' then
      FInternalLangue := TOlfAboutDialogLang.SP
    else if ISO = 'de' then
      FInternalLangue := TOlfAboutDialogLang.DE
    else
      FInternalLangue := TOlfAboutDialogLang.EN;
  end
  else
    FInternalLangue := FLangue;
  // TODO : add a global translation event
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
    lblLicence.Caption := FLicence;
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

procedure TOlfAboutDialogForm.SetonButtonBuyClick(const Value: TNotifyEvent);
begin
  FonButtonBuyClick := Value;
end;

procedure TOlfAboutDialogForm.SetonButtonBuyClickProc
  (const Value: TNotifyEvent);
begin
  FonButtonBuyClickProc := Value;
end;

procedure TOlfAboutDialogForm.SetonButtonLicenseClick
  (const Value: TNotifyEvent);
begin
  FonButtonLicenseClick := Value;
end;

procedure TOlfAboutDialogForm.SetonButtonLicenseClickProc
  (const Value: TNotifyEvent);
begin
  FonButtonLicenseClickProc := Value;
end;

procedure TOlfAboutDialogForm.SetonButtonRegisterClick
  (const Value: TNotifyEvent);
begin
  FonButtonRegisterClick := Value;
end;

procedure TOlfAboutDialogForm.SetonButtonRegisterClickProc
  (const Value: TNotifyEvent);
begin
  FonButtonRegisterClickProc := Value;
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

procedure TOlfAboutDialogForm.SetonGetFooterText(const Value
  : TOlfAboutDialogGetTextEvent);
begin
  FonGetFooterText := Value;
end;

procedure TOlfAboutDialogForm.SetonGetFooterTextProc
  (const Value: TOlfAboutDialogGetTextProc);
begin
  FonGetFooterTextProc := Value;
end;

procedure TOlfAboutDialogForm.SetonGetText(const Value
  : TOlfAboutDialogGetTextEvent);
begin
  FonGetText := Value;
end;

procedure TOlfAboutDialogForm.SetonGetTextProc(const Value
  : TOlfAboutDialogGetTextProc);
begin
  FonGetTextProc := Value;
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
  lblTitre.Caption := FTitre;
  Caption := getTraduction(TOlfAboutDialogTxtID.About) + FTitre;
  CalculeHauteurEntete;
end;

procedure TOlfAboutDialogForm.SetURL(const Value: string);
begin
  FURL := Value;
  if FURL.IsEmpty then
    lblURL.visible := false
  else
  begin
    lblURL.Caption := FURL;
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
