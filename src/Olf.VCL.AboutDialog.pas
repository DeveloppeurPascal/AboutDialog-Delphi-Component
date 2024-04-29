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

unit Olf.VCL.AboutDialog;

interface

uses
  system.UITypes,
  system.SysUtils,
  system.Classes,
  VCL.ExtCtrls,
  VCL.ImgList,
  VCL.Graphics,
  Olf.VCL.AboutDialogForm;

type

{$IF CompilerVersion >= 33.0}
  // Delphi 10.3 Rio and after
  [ComponentPlatformsAttribute(pfidWindows)]
{$ELSE}
  [ComponentPlatformsAttribute(pidWin32 + pidWin64)]
{$ENDIF}

  TOlfAboutDialog = class(TComponent)
  private
    FVersionNumero: string;
    FTitre: string;
    FVersionDate: string;
    FDescription: tstrings;
    FLicence: tstrings;
    FImage: TImage;
    FURL: string;
    FPicture: TPicture;
    FImageList: TCustomImageList;
{$IF CompilerVersion >= 34.0}
    // Delphi 10.4 Sydney and after
    FImageListName: TImageName;
{$ENDIF}
    FImageListIndex: system.UITypes.TImageIndex;
    FonCloseDialog: TOlfAboutDialogCloseEvent;
    FonURLClick: TOlfAboutDialogURLClickEvent;
    FLangue: TOlfAboutDialogLang;
    FCopyright: string;
    FonFormClose: TNotifyEvent;
    FonFormShow: TNotifyEvent;
    FonBeforeExecute: TNotifyEvent;
    FonFormActivate: TNotifyEvent;
    FonFormCreate: TNotifyEvent;
    FonAfterExecute: TNotifyEvent;
    FonButtonLicenseClickProc: TNotifyEvent;
    FonButtonLicenseClick: TNotifyEvent;
    FonGetFooterText: TOlfAboutDialogGetTextEvent;
    FonButtonRegisterClickProc: TNotifyEvent;
    FonButtonRegisterClick: TNotifyEvent;
    FonGetText: TOlfAboutDialogGetTextEvent;
    FonButtonBuyClickProc: TNotifyEvent;
    FonButtonBuyClick: TNotifyEvent;
    FonGetFooterTextProc: TOlfAboutDialogGetTextProc;
    FonGetTextProc: TOlfAboutDialogGetTextProc;
    procedure SetVersionDate(const Value: string);
    procedure SetImage(const Value: TImage);
    procedure SetDescription(const Value: tstrings);
    procedure SetLicence(const Value: tstrings);
    procedure SetTitre(const Value: string);
    procedure SetURL(const Value: string);
    procedure SetVersionNumero(const Value: string);
    procedure SetPicture(const Value: TPicture);
    procedure SetImageList(const Value: TCustomImageList);
    procedure SetImageListIndex(const Value: system.UITypes.TImageIndex);
{$IF CompilerVersion >= 34.0}
    procedure SetImageListName(const Value: TImageName);
{$ENDIF}
    procedure SetonCloseDialog(const Value: TOlfAboutDialogCloseEvent);
    procedure SetonURLClick(const Value: TOlfAboutDialogURLClickEvent);
    procedure SetLangue(const Value: TOlfAboutDialogLang);
    procedure SetCopyright(const Value: string);
    procedure SetonAfterExecute(const Value: TNotifyEvent);
    procedure SetonBeforeExecute(const Value: TNotifyEvent);
    procedure SetonFormActivate(const Value: TNotifyEvent);
    procedure SetonFormClose(const Value: TNotifyEvent);
    procedure SetonFormCreate(const Value: TNotifyEvent);
    procedure SetonFormShow(const Value: TNotifyEvent);
    procedure SetonButtonBuyClick(const Value: TNotifyEvent);
    procedure SetonButtonBuyClickProc(const Value: TNotifyEvent);
    procedure SetonButtonLicenseClick(const Value: TNotifyEvent);
    procedure SetonButtonLicenseClickProc(const Value: TNotifyEvent);
    procedure SetonButtonRegisterClick(const Value: TNotifyEvent);
    procedure SetonButtonRegisterClickProc(const Value: TNotifyEvent);
    procedure SetonGetFooterText(const Value: TOlfAboutDialogGetTextEvent);
    procedure SetonGetText(const Value: TOlfAboutDialogGetTextEvent);
    procedure SetonGetFooterTextProc(const Value: TOlfAboutDialogGetTextProc);
    procedure SetonGetTextProc(const Value: TOlfAboutDialogGetTextProc);
    { Déclarations privées }
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    { Déclarations protégées }
  public
    { Déclarations publiques }
    /// <summary>
    /// Création d'une instance du composant
    /// </summary>
    constructor Create(AOwner: TComponent); override;
    /// <summary>
    /// Suppression des données de l'instance du composant
    /// </summary>
    destructor Destroy; override;
    /// <summary>
    /// A appeler pour afficher la boite de dialogue
    /// </summary>
    function Execute: boolean;

    property onButtonLicenseClickProc: TNotifyEvent
      read FonButtonLicenseClickProc write SetonButtonLicenseClickProc;
    property onButtonBuyClickProc: TNotifyEvent read FonButtonBuyClickProc
      write SetonButtonBuyClickProc;
    property onButtonRegisterClickProc: TNotifyEvent
      read FonButtonRegisterClickProc write SetonButtonRegisterClickProc;
    property onGetTextProc: TOlfAboutDialogGetTextProc read FonGetTextProc
      write SetonGetTextProc;
    property onGetFooterTextProc: TOlfAboutDialogGetTextProc
      read FonGetFooterTextProc write SetonGetFooterTextProc;
  published
    { Déclarations publiées }
    /// <summary>
    /// Titre affiché dans la barre de titre de la fenêtre "à propos" et en premier sur la boite de dialogue
    /// </summary>
    property Titre: string read FTitre write SetTitre;
    /// <summary>
    /// Affiché sous le titre sur la forme "version XXX" ou "Version XXX du JJ/MM/AAAA"
    /// </summary>
    property VersionNumero: string read FVersionNumero write SetVersionNumero;
    /// <summary>
    /// Affiché sous le titre sur la forme "Version du JJ/MM/AAAA" ou "Version XXX du JJ/MM/AAAA"
    /// </summary>
    property VersionDate: string read FVersionDate write SetVersionDate;
    /// <summary>
    /// Image utilisée comme logo dans la boite de dialogue (pris en compte si pas d'ImageList)
    /// </summary>
    property Image: TImage read FImage write SetImage;
    /// <summary>
    /// Bitmap utilisé par l'image servant de logo dans la boite de dialogue (pris en compte si pas d'ImageList ni d'Image)
    /// </summary>
    property Picture: TPicture read FPicture write SetPicture;
    /// <summary>
    /// Liste d'images contenant le logo à utiliser dans la boite de dialogue
    /// </summary>
    property Images: TCustomImageList read FImageList write SetImageList;
    /// <summary>
    /// Index de l'image provenant de la liste d'images correspondant au logo à utiliser dans la boite de dialogue
    /// </summary>
    property ImageIndex: system.UITypes.TImageIndex read FImageListIndex
      write SetImageListIndex default -1;
{$IF CompilerVersion >= 34.0}
    /// <summary>
    /// Nom de l'image provenant de la liste d'images correspondant au logo à utiliser dans la boite de dialogue
    /// (le nom est prioritaire sur l'index s'il est renseigné et géré par la liste d'images)
    /// </summary>
    property ImageName: TImageName read FImageListName write SetImageListName;
{$ENDIF}
    /// <summary>
    /// Adresse du site pour lequel on affiche un lien sous le numéro de version/date
    /// </summary>
    property URL: string read FURL write SetURL;
    /// <summary>
    /// Texte de description à afficher dans la boite de dialogue
    /// S'il est vide, son pavé n'est pas affiché.
    /// S'il est indiqué et qu'une licence est aussi présente, ils se partagent l'espace disponible.
    /// </summary>
    property Description: tstrings read FDescription write SetDescription;
    /// <summary>
    /// Texte de la licence à afficher dans la boite de dialogue
    /// S'il est vide, son pavé n'est pas affiché.
    /// S'il est indiqué et qu'une description est aussi présente, ils se partagent l'espace disponible.
    /// </summary>
    property Licence: tstrings read FLicence write SetLicence;
    /// <summary>
    /// Evenement appelé lorsdu click sur le bouton de fermeture de la boite de dialogue
    /// </summary>
    property onCloseDialog: TOlfAboutDialogCloseEvent read FonCloseDialog
      write SetonCloseDialog;
    /// <summary>
    /// Evenement appelé lorsqu'on clique sur l'URL (si URL affichée).
    /// </summary>
    property onURLClick: TOlfAboutDialogURLClickEvent read FonURLClick
      write SetonURLClick;
    /// <summary>
    /// Langue des textes affichés par la boite de dialogue
    /// </summary>
    property Langue: TOlfAboutDialogLang read FLangue write SetLangue;
    /// <summary>
    /// Copyright du projet, affiché sous le numéro de version
    /// </summary>
    property Copyright: string read FCopyright write SetCopyright;

    property onBeforeExecute: TNotifyEvent read FonBeforeExecute
      write SetonBeforeExecute;
    property onAfterExecute: TNotifyEvent read FonAfterExecute
      write SetonAfterExecute;
    property onFormCreate: TNotifyEvent read FonFormCreate
      write SetonFormCreate;
    property onFormActivate: TNotifyEvent read FonFormActivate
      write SetonFormActivate;
    property onFormShow: TNotifyEvent read FonFormShow write SetonFormShow;
    property onFormClose: TNotifyEvent read FonFormClose write SetonFormClose;

    property onButtonLicenseClick: TNotifyEvent read FonButtonLicenseClick
      write SetonButtonLicenseClick;
    property onButtonBuyClick: TNotifyEvent read FonButtonBuyClick
      write SetonButtonBuyClick;
    property onButtonRegisterClick: TNotifyEvent read FonButtonRegisterClick
      write SetonButtonRegisterClick;
    property onGetText: TOlfAboutDialogGetTextEvent read FonGetText
      write SetonGetText;
    property onGetFooterText: TOlfAboutDialogGetTextEvent read FonGetFooterText
      write SetonGetFooterText;
  end;

procedure Register;

implementation

uses
  VCL.Forms,
  VCL.Controls;

procedure Register;
begin
  RegisterComponents('OlfSoftware', [TOlfAboutDialog]);
end;

{ TOlfAboutDialog }

constructor TOlfAboutDialog.Create(AOwner: TComponent);
begin
  inherited;
  FDescription := tstringlist.Create;
  FLicence := tstringlist.Create;
  FPicture := TPicture.Create;
  FImage := nil;
  FImageList := nil;
{$IF CompilerVersion >= 34.0}
  FImageListName := '';
{$ENDIF}
  FImageListIndex := -1;
  FVersionNumero := '';
  FTitre := '';
  FVersionDate := '';
  FURL := '';
  FLangue := TOlfAboutDialogLang.EN;
  FonCloseDialog := nil;
  FonURLClick := nil;
  FCopyright := '';
  FonBeforeExecute := nil;
  FonFormCreate := nil;
  FonFormActivate := nil;
  FonFormShow := nil;
  FonFormClose := nil;
  FonAfterExecute := nil;
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

destructor TOlfAboutDialog.Destroy;
begin
  FDescription.Free;
  FLicence.Free;
  FPicture.Free;
  inherited;
end;

function TOlfAboutDialog.Execute: boolean;
var
  frmAboutDialog: TOlfAboutDialogForm;
begin
  if assigned(FonBeforeExecute) then
    FonBeforeExecute(self);
  try
    try
      frmAboutDialog := TOlfAboutDialogForm.Create(owner);
      try
        frmAboutDialog.Langue := Langue;
        if assigned(Images) then
{$IF CompilerVersion >= 34.0}
          frmAboutDialog.SetImageList(Images, ImageIndex, ImageName)
{$ELSE}
          frmAboutDialog.SetImageList(Images, ImageIndex)
{$ENDIF}
        else if assigned(Image) then
          frmAboutDialog.Image := Image
        else
          frmAboutDialog.Picture := Picture;
        frmAboutDialog.Titre := Titre;
        if frmAboutDialog.Titre.IsEmpty and (owner is TCustomForm) then
          frmAboutDialog.Titre := (owner as TCustomForm).Caption;
        frmAboutDialog.VersionNumero := VersionNumero;
        frmAboutDialog.VersionDate := VersionDate;
        frmAboutDialog.URL := URL;
        frmAboutDialog.Description := Description.text;
        frmAboutDialog.Licence := Licence.text;
        frmAboutDialog.Copyright := Copyright;
        frmAboutDialog.onCloseDialog := onCloseDialog;
        frmAboutDialog.onURLClick := onURLClick;
        frmAboutDialog.onFormActivate := onFormActivate;
        frmAboutDialog.onFormShow := onFormShow;
        frmAboutDialog.onFormClose := onFormClose;
        frmAboutDialog.onButtonLicenseClickProc := onButtonLicenseClickProc;
        frmAboutDialog.onButtonLicenseClick := onButtonLicenseClick;
        frmAboutDialog.onButtonRegisterClickProc := onButtonRegisterClickProc;
        frmAboutDialog.onButtonRegisterClick := onButtonRegisterClick;
        frmAboutDialog.onButtonBuyClickProc := onButtonBuyClickProc;
        frmAboutDialog.onButtonBuyClick := onButtonBuyClick;
        frmAboutDialog.onGetFooterTextProc := onGetFooterTextProc;
        frmAboutDialog.onGetFooterText := onGetFooterText;
        frmAboutDialog.onGetTextProc := onGetTextProc;
        frmAboutDialog.onGetText := onGetText;
        if assigned(FonFormCreate) then
          FonFormCreate(frmAboutDialog);
        frmAboutDialog.showmodal;
      finally
        frmAboutDialog.Free;
      end;
      result := true;
    except
      result := false;
    end;
  finally
    if assigned(FonAfterExecute) then
      FonAfterExecute(self);
  end;
end;

procedure TOlfAboutDialog.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = TOperation.opRemove) then
    if AComponent = FImage then
      FImage := nil
    else if AComponent = FImageList then
    begin
      FImageList := nil;
{$IF CompilerVersion >= 34.0}
      FImageListName := '';
{$ENDIF}
      FImageListIndex := -1;
    end;
end;

procedure TOlfAboutDialog.SetVersionDate(const Value: string);
begin
  FVersionDate := Value;
end;

procedure TOlfAboutDialog.SetImage(const Value: TImage);
begin
  if assigned(FImage) then
    FImage.RemoveFreeNotification(self);
  FImage := Value;
  if assigned(FImage) then
    FImage.FreeNotification(self);
end;

procedure TOlfAboutDialog.SetImageList(const Value: TCustomImageList);
begin
  if assigned(FImageList) then
    FImageList.RemoveFreeNotification(self);
  FImageList := Value;
  if assigned(FImageList) then
    FImageList.FreeNotification(self);
end;

procedure TOlfAboutDialog.SetImageListIndex(const Value
  : system.UITypes.TImageIndex);
begin
  FImageListIndex := Value;
end;

{$IF CompilerVersion >= 34.0}

procedure TOlfAboutDialog.SetImageListName(const Value: TImageName);
begin
  FImageListName := Value;
end;
{$ENDIF}

procedure TOlfAboutDialog.SetPicture(const Value: TPicture);
begin
  FPicture.Assign(Value);
end;

procedure TOlfAboutDialog.SetCopyright(const Value: string);
begin
  FCopyright := Value;
end;

procedure TOlfAboutDialog.SetDescription(const Value: tstrings);
begin
  FDescription.Assign(Value);
end;

procedure TOlfAboutDialog.SetLangue(const Value: TOlfAboutDialogLang);
begin
  FLangue := Value;
end;

procedure TOlfAboutDialog.SetLicence(const Value: tstrings);
begin
  FLicence.Assign(Value);
end;

procedure TOlfAboutDialog.SetonAfterExecute(const Value: TNotifyEvent);
begin
  FonAfterExecute := Value;
end;

procedure TOlfAboutDialog.SetonBeforeExecute(const Value: TNotifyEvent);
begin
  FonBeforeExecute := Value;
end;

procedure TOlfAboutDialog.SetonButtonBuyClick(const Value: TNotifyEvent);
begin
  FonButtonBuyClick := Value;
end;

procedure TOlfAboutDialog.SetonButtonBuyClickProc(const Value: TNotifyEvent);
begin
  FonButtonBuyClickProc := Value;
end;

procedure TOlfAboutDialog.SetonButtonLicenseClick(const Value: TNotifyEvent);
begin
  FonButtonLicenseClick := Value;
end;

procedure TOlfAboutDialog.SetonButtonLicenseClickProc
  (const Value: TNotifyEvent);
begin
  FonButtonLicenseClickProc := Value;
end;

procedure TOlfAboutDialog.SetonButtonRegisterClick(const Value: TNotifyEvent);
begin
  FonButtonRegisterClick := Value;
end;

procedure TOlfAboutDialog.SetonButtonRegisterClickProc
  (const Value: TNotifyEvent);
begin
  FonButtonRegisterClickProc := Value;
end;

procedure TOlfAboutDialog.SetonCloseDialog(const Value
  : TOlfAboutDialogCloseEvent);
begin
  FonCloseDialog := Value;
end;

procedure TOlfAboutDialog.SetonFormActivate(const Value: TNotifyEvent);
begin
  FonFormActivate := Value;
end;

procedure TOlfAboutDialog.SetonFormClose(const Value: TNotifyEvent);
begin
  FonFormClose := Value;
end;

procedure TOlfAboutDialog.SetonFormCreate(const Value: TNotifyEvent);
begin
  FonFormCreate := Value;
end;

procedure TOlfAboutDialog.SetonFormShow(const Value: TNotifyEvent);
begin
  FonFormShow := Value;
end;

procedure TOlfAboutDialog.SetonGetFooterText(const Value
  : TOlfAboutDialogGetTextEvent);
begin
  FonGetFooterText := Value;
end;

procedure TOlfAboutDialog.SetonGetFooterTextProc(const Value
  : TOlfAboutDialogGetTextProc);
begin
  FonGetFooterTextProc := Value;
end;

procedure TOlfAboutDialog.SetonGetText(const Value
  : TOlfAboutDialogGetTextEvent);
begin
  FonGetText := Value;
end;

procedure TOlfAboutDialog.SetonGetTextProc(const Value
  : TOlfAboutDialogGetTextProc);
begin
  FonGetTextProc := Value;
end;

procedure TOlfAboutDialog.SetonURLClick(const Value
  : TOlfAboutDialogURLClickEvent);
begin
  FonURLClick := Value;
end;

procedure TOlfAboutDialog.SetTitre(const Value: string);
begin
  FTitre := Value;
end;

procedure TOlfAboutDialog.SetURL(const Value: string);
begin
  FURL := Value;
end;

procedure TOlfAboutDialog.SetVersionNumero(const Value: string);
begin
  FVersionNumero := Value;
end;

initialization

{$IF NOT DEFINED(CLR)}
  StartClassGroup(VCL.Controls.TControl);
ActivateClassGroup(VCL.Controls.TControl);
GroupDescendentsWith(TOlfAboutDialog, VCL.Controls.TControl);
{$ENDIF}

end.
