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
  TOlfAboutDialogLang = (FR, EN, IT, PT, SP, DE, Manual, Auto);
  TOlfAboutDialogTxtID = (About, Version, Date, VersionDate, CloseButton,
    Footer, LicenseInfoButton, BuyButton, RegisterButton, LicenseText,
    DescriptionText, TitleText);

  TNotifyProc = reference to procedure(Sender: TObject);

  TOlfAboutDialogGetTextEvent = function(Const ALang: TOlfAboutDialogLang;
    Const ATxtID: TOlfAboutDialogTxtID): string of object;
  TOlfAboutDialogGetTextProc = reference to function
    (Const ALang: TOlfAboutDialogLang;
    Const ATxtID: TOlfAboutDialogTxtID): string;

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
    lblCopyright: TLabel;
    btnLicenseInfo: TButton;
    btnBuy: TButton;
    btnRegister: TButton;
    lblFooter: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure lblURLClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnCloseClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure zoneTextesResize(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnLicenseInfoClick(Sender: TObject);
    procedure btnBuyClick(Sender: TObject);
    procedure btnRegisterClick(Sender: TObject);
  private
    FTitre: string;
    FVersionDate: string;
    FVersionNumero: string;
    FDescription: string;
    FLicence: string;
    FURL: string;
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
    procedure SetonURLClick(const Value: TOlfAboutDialogURLClickEvent);
    procedure SetDescription(const Value: string);
    procedure SetImage(const Value: TImage);
    procedure SetLicence(const Value: string);
    procedure SetMultiResBitmap(const Value: TFixedMultiResBitmap);
    procedure SetTitre(const Value: string);
    procedure SetURL(const Value: string);
    procedure SetVersionDate(const Value: string);
    procedure SetVersionNumero(const Value: string);
    procedure AfficheVersionEtVersionDate;
    procedure AfficheZoneLogo;
    procedure SetonCloseDialog(const Value: TOlfAboutDialogCloseEvent);
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
  protected
    procedure UpdateTextFields;
  public
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
    property Copyright: string read FCopyright write SetCopyright;

    property onFormActivate: TNotifyEvent read FonFormActivate
      write SetonFormActivate;
    property onFormShow: TNotifyEvent read FonFormShow write SetonFormShow;
    property onFormClose: TNotifyEvent read FonFormClose write SetonFormClose;

    procedure SetImageList(ImageList: TCustomImageList;
      ImageListIndex: System.UITypes.TImageIndex = -1);

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

{$R *.fmx}
{$IF Defined(MSWINDOWS)}

uses
  Winapi.Windows;
{$ELSEIF Defined(IOS)}

uses
  MacAPI.ObjectiveC,
  iOSapi.Foundation;
{$ELSEIF Defined(MACOS)}

uses
  MacAPI.ObjectiveC,
  MacAPI.Foundation;
{$ELSE}

// Android + Linux + others
uses
  FMX.Platform;
{$ENDIF}

function GetCurrentLanguageCode: String;
// copied from https://github.com/DeveloppeurPascal/librairies/blob/master/src/Olf.RTL.Language.pas
{$IF Defined(MSWINDOWS)}
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
{$ELSEIF Defined(MACOS) or Defined(IOS)}

var
  Languages: NSArray;
begin
  Languages := TNSLocale.OCClass.preferredLanguages;
  Result := lowercase(TNSString.Wrap(Languages.objectAtIndex(0)).UTF8String);
end;
{$ELSE}

var
  LocServ: IFMXLocaleService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXLocaleService,
    IInterface(LocServ)) then
    Result := LocServ.GetCurrentLangID;
end;
{$ENDIF}

function GetCurrentLanguageISOCode: String;
// copied from https://github.com/DeveloppeurPascal/librairies/blob/master/src/Olf.RTL.Language.pas
begin
  Result := GetCurrentLanguageCode.Substring(0, 2);
end;

procedure TOlfAboutDialogForm.AfficheVersionEtVersionDate;
begin
  if not FVersionNumero.IsEmpty then
  begin
    lblVersion.Text := getTraduction(TOlfAboutDialogTxtID.Version) +
      FVersionNumero;
    if not FVersionDate.IsEmpty then
      lblVersion.Text := lblVersion.Text +
        getTraduction(TOlfAboutDialogTxtID.Date) + FVersionDate;
  end
  else if not FVersionDate.IsEmpty then
    lblVersion.Text := getTraduction(TOlfAboutDialogTxtID.VersionDate) +
      FVersionDate
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

procedure TOlfAboutDialogForm.FormResize(Sender: TObject);
begin
  AfficheZoneLogo;
end;

procedure TOlfAboutDialogForm.FormShow(Sender: TObject);
begin
  UpdateTextFields;

  btnLicenseInfo.Visible := assigned(onButtonLicenseClick) or
    assigned(onButtonLicenseClickProc);
  btnBuy.Visible := assigned(onButtonBuyClick) or
    assigned(onButtonBuyClickProc);
  btnRegister.Visible := assigned(onButtonRegisterClick) or
    assigned(onButtonRegisterClickProc);

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
    FonURLClick(lblURL.Text);
end;

procedure TOlfAboutDialogForm.SetCopyright(const Value: string);
begin
  FCopyright := Value;
  if FCopyright.IsEmpty then
    lblCopyright.Visible := false
  else
  begin
    lblCopyright.Text := FCopyright;
    lblCopyright.Visible := true;
  end;
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

  UpdateTextFields;
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
    lblURL.TextSettings.Font.Style := [TFontStyle.fsUnderline];
    lblURL.TextSettings.FontColor := talphacolors.Blue;
  end
  else
  begin
    lblURL.Cursor := crDefault;
    lblURL.TextSettings.Font.Style := [];
    lblURL.TextSettings.FontColor := talphacolors.Black;
  end;
end;

procedure TOlfAboutDialogForm.SetTitre(const Value: string);
begin
  FTitre := Value;
  lblTitre.Text := FTitre;
  UpdateTextFields;
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
  UpdateTextFields;
end;

procedure TOlfAboutDialogForm.SetVersionNumero(const Value: string);
begin
  FVersionNumero := Value;
  UpdateTextFields;
end;

procedure TOlfAboutDialogForm.UpdateTextFields;
begin
  FTitre := getTraduction(TOlfAboutDialogTxtID.TitleText);
  FLicence := getTraduction(TOlfAboutDialogTxtID.LicenseText);
  FDescription := getTraduction(TOlfAboutDialogTxtID.DescriptionText);

  caption := getTraduction(TOlfAboutDialogTxtID.About) + FTitre;

  AfficheVersionEtVersionDate;

  if assigned(onGetFooterTextProc) then
    lblFooter.Text := onGetFooterTextProc(FInternalLangue,
      TOlfAboutDialogTxtID.Footer)
  else if assigned(onGetFooterText) then
    lblFooter.Text := onGetFooterText(FInternalLangue,
      TOlfAboutDialogTxtID.Footer)
  else
    lblFooter.Text := '';
  lblFooter.Visible := not lblFooter.Text.IsEmpty;

  btnLicenseInfo.Text := getTraduction(TOlfAboutDialogTxtID.LicenseInfoButton);
  btnBuy.Text := getTraduction(TOlfAboutDialogTxtID.BuyButton);
  btnRegister.Text := getTraduction(TOlfAboutDialogTxtID.RegisterButton);
  btnClose.Text := getTraduction(TOlfAboutDialogTxtID.CloseButton);
end;

procedure TOlfAboutDialogForm.zoneTextesResize(Sender: TObject);
begin
  if pnlDescription.Visible and pnlLicence.Visible then
    pnlDescription.height := (zoneTextes.height / 2) -
      pnlDescription.margins.top - pnlDescription.margins.bottom;
end;

end.
