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

unit Olf.FMX.AboutDialog;

interface

uses
  FMX.Types,
  System.SysUtils,
  System.Classes,
  FMX.ImgList,
  System.UITypes,
  FMX.Objects,
  FMX.MultiResBitmap,
  Olf.FMX.AboutDialogForm;

type

{$IF CompilerVersion >= 33.0}
  [ComponentPlatformsAttribute(pfidWindows or pfidOSX or pfidLinux or
    pfidAndroid or pfidiOS)]
{$ELSE}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32 or pidOSX64 or
    pidiOSSimulator or pidiOSDevice32 or pidiOSDevice64 or pidAndroid or
    pidAndroid64 or pidLinux64)]
{$ENDIF}

  TOlfAboutDialog = class(TFMXObject)
  private
    FTitre: string;
    FMultiResBitmap: TFixedMultiResBitmap;
    FVersionDate: string;
    FImageList: TCustomImageList;
    FImage: TImage;
    FVersionNumero: string;
    FDescription: tstrings;
    FLicence: tstrings;
    FImageListIndex: TImageIndex;
    FURL: string;
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
    FonGetFooterTextProc: TOlfAboutDialogGetTextProc;
    FonButtonLicenseClick: TNotifyEvent;
    FonGetFooterText: TOlfAboutDialogGetTextEvent;
    FonButtonRegisterClickProc: TNotifyEvent;
    FonButtonRegisterClick: TNotifyEvent;
    FonGetTextProc: TOlfAboutDialogGetTextProc;
    FonGetText: TOlfAboutDialogGetTextEvent;
    FonButtonBuyClickProc: TNotifyEvent;
    FonButtonBuyClick: TNotifyEvent;
    procedure SetDescription(const Value: tstrings);
    procedure SetImage(const Value: TImage);
    procedure SetImageList(const Value: TCustomImageList);
    procedure SetImageListIndex(const Value: TImageIndex);
    procedure SetLicence(const Value: tstrings);
    procedure SeTFixedMultiResBitmap(const Value: TFixedMultiResBitmap);
    procedure SetTitre(const Value: string);
    procedure SetURL(const Value: string);
    procedure SetVersionDate(const Value: string);
    procedure SetVersionNumero(const Value: string);
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
    procedure SetonGetFooterTextProc(const Value: TOlfAboutDialogGetTextProc);
    procedure SetonGetText(const Value: TOlfAboutDialogGetTextEvent);
    procedure SetonGetTextProc(const Value: TOlfAboutDialogGetTextProc);
    { D�clarations priv�es }
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    { D�clarations prot�g�es }
  public
    { D�clarations publiques }
    /// <summary>
    /// Cr�ation d'une instance du composant
    /// </summary>
    constructor Create(AOwner: TComponent); override;
    /// <summary>
    /// Suppression des donn�es de l'instance du composant
    /// </summary>
    destructor Destroy; override;
    /// <summary>
    /// A appeler pour afficher la boite de dialog
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
    { D�clarations publi�es }
    /// <summary>
    /// Titre affich� dans la barre de titre de la fen�tre "� propos" et en premier sur la boite de dialog
    /// </summary>
    property Titre: string read FTitre write SetTitre;
    /// <summary>
    /// Affich� sous le titre sur la forme "version XXX" ou "Version XXX du JJ/MM/AAAA"
    /// </summary>
    property VersionNumero: string read FVersionNumero write SetVersionNumero;
    /// <summary>
    /// Affich� sous le titre sur la forme "Version du JJ/MM/AAAA" ou "Version XXX du JJ/MM/AAAA"
    /// </summary>
    property VersionDate: string read FVersionDate write SetVersionDate;
    /// <summary>
    /// Image utilis�e comme logo dans la boite de dialog (pris en compte si pas d'ImageList)
    /// </summary>
    property Image: TImage read FImage write SetImage;
    /// <summary>
    /// Bitmap utilis� par l'image servant de logo dans la boite de dialog (pris en compte si pas d'ImageList ni d'Image)
    /// </summary>
    property MultiResBitmap: TFixedMultiResBitmap read FMultiResBitmap
      write SeTFixedMultiResBitmap;
    /// <summary>
    /// Liste d'images contenant le logo � utiliser dans la boite de dialog
    /// </summary>
    property Images: TCustomImageList read FImageList write SetImageList;
    /// <summary>
    /// Index de l'image provenant de la liste d'images correspondant au logo � utiliser dans la boite de dialog
    /// </summary>
    property ImageIndex: TImageIndex read FImageListIndex
      write SetImageListIndex default -1;
    /// <summary>
    /// Adresse du site pour lequel on affiche un lien sous le num�ro de version/date
    /// </summary>
    property URL: string read FURL write SetURL;
    /// <summary>
    /// Texte de description � afficher dans la boite de dialog
    /// S'il est vide, son pav� n'est pas affich�.
    /// S'il est indiqu� et qu'une licence est aussi pr�sente, ils se partagent l'espace disponible.
    /// </summary>
    property Description: tstrings read FDescription write SetDescription;
    /// <summary>
    /// Texte de la licence � afficher dans la boite de dialog
    /// S'il est vide, son pav� n'est pas affich�.
    /// S'il est indiqu� et qu'une description est aussi pr�sente, ils se partagent l'espace disponible.
    /// </summary>
    property Licence: tstrings read FLicence write SetLicence;
    /// <summary>
    /// Evenement appel� lorsdu click sur le bouton de fermeture de la boite de dialog
    /// </summary>
    property onCloseDialog: TOlfAboutDialogCloseEvent read FonCloseDialog
      write SetonCloseDialog;
    /// <summary>
    /// Evenement appel� lorsqu'on clique sur l'URL (si URL affich�e).
    /// </summary>
    property onURLClick: TOlfAboutDialogURLClickEvent read FonURLClick
      write SetonURLClick;
    /// <summary>
    /// Langue des textes affich�s par la boite de dialog
    /// </summary>
    property Langue: TOlfAboutDialogLang read FLangue write SetLangue
      default TOlfAboutDialogLang.Auto;
    /// <summary>
    /// Copyright du projet, affich� sous le num�ro de version
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
  FMX.Forms;

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
  FMultiResBitmap := TImageMultiResBitmap.Create(Self, TFixedBitmapItem);
  FImage := nil;
  FImageList := nil;
  FImageListIndex := -1;
  FVersionNumero := '';
  FTitre := '';
  FVersionDate := '';
  FURL := '';
  FLangue := TOlfAboutDialogLang.Auto;
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
  FMultiResBitmap.Free;
  inherited;
end;

function TOlfAboutDialog.Execute: boolean;
var
  frmAboutBox: TOlfAboutDialogForm;
begin
  if assigned(FonBeforeExecute) then
    FonBeforeExecute(Self);
  try
    try
      frmAboutBox := TOlfAboutDialogForm.Create(owner);
      try
        frmAboutBox.Langue := Langue;
        if assigned(Images) then
          frmAboutBox.SetImageList(Images, ImageIndex)
        else if assigned(Image) then
          frmAboutBox.Image := Image
        else
          frmAboutBox.MultiResBitmap := MultiResBitmap;
        frmAboutBox.Titre := Titre;
        if frmAboutBox.Titre.IsEmpty and (owner is TCustomForm) then
          frmAboutBox.Titre := (owner as TCustomForm).Caption;
        frmAboutBox.VersionNumero := VersionNumero;
        frmAboutBox.VersionDate := VersionDate;
        frmAboutBox.URL := URL;
        frmAboutBox.Description := Description.text;
        frmAboutBox.Licence := Licence.text;
        frmAboutBox.Copyright := Copyright;
        frmAboutBox.onCloseDialog := onCloseDialog;
        frmAboutBox.onURLClick := onURLClick;
        frmAboutBox.onFormActivate := onFormActivate;
        frmAboutBox.onFormShow := onFormShow;
        frmAboutBox.onFormClose := onFormClose;
        frmAboutBox.onButtonLicenseClickProc := onButtonLicenseClickProc;
        frmAboutBox.onButtonLicenseClick := onButtonLicenseClick;
        frmAboutBox.onButtonRegisterClickProc := onButtonRegisterClickProc;
        frmAboutBox.onButtonRegisterClick := onButtonRegisterClick;
        frmAboutBox.onButtonBuyClickProc := onButtonBuyClickProc;
        frmAboutBox.onButtonBuyClick := onButtonBuyClick;
        frmAboutBox.onGetFooterTextProc := onGetFooterTextProc;
        frmAboutBox.onGetFooterText := onGetFooterText;
        frmAboutBox.onGetTextProc := onGetTextProc;
        frmAboutBox.onGetText := onGetText;
        if assigned(FonFormCreate) then
          FonFormCreate(frmAboutBox);
{$IF Defined(IOS) or Defined(ANDROID)}
        frmAboutBox.Show;
{$ELSE}
        frmAboutBox.showmodal;
{$ENDIF}
      finally
{$IF Defined(IOS) or Defined(ANDROID)}
{$ELSE}
        frmAboutBox.Free;
{$ENDIF}
      end;
      result := true;
    except
      result := false;
    end;
  finally
    if assigned(FonAfterExecute) then
      FonAfterExecute(Self);
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
      FImageListIndex := -1;
    end;
end;

procedure TOlfAboutDialog.SetCopyright(const Value: string);
begin
  FCopyright := Value;
end;

procedure TOlfAboutDialog.SetDescription(const Value: tstrings);
begin
  FDescription.Assign(Value);
end;

procedure TOlfAboutDialog.SetImage(const Value: TImage);
begin
  if assigned(FImage) then
    FImage.RemoveFreeNotification(Self);
  FImage := Value;
  if assigned(FImage) then
    FImage.FreeNotification(Self);
end;

procedure TOlfAboutDialog.SetImageList(const Value: TCustomImageList);
begin
  if assigned(FImageList) then
    FImageList.RemoveFreeNotification(Self);
  FImageList := Value;
  if assigned(FImageList) then
    FImageList.FreeNotification(Self);
end;

procedure TOlfAboutDialog.SetImageListIndex(const Value: TImageIndex);
begin
  FImageListIndex := Value;
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

procedure TOlfAboutDialog.SeTFixedMultiResBitmap(const Value
  : TFixedMultiResBitmap);
begin
  FMultiResBitmap.Assign(Value);
end;

procedure TOlfAboutDialog.SetTitre(const Value: string);
begin
  FTitre := Value;
end;

procedure TOlfAboutDialog.SetURL(const Value: string);
begin
  FURL := Value;
end;

procedure TOlfAboutDialog.SetVersionDate(const Value: string);
begin
  FVersionDate := Value;
end;

procedure TOlfAboutDialog.SetVersionNumero(const Value: string);
begin
  FVersionNumero := Value;
end;

initialization

StartClassGroup(TFMXObject);
ActivateClassGroup(TFMXObject);
GroupDescendentsWith(TOlfAboutDialog, TFMXObject);

end.
