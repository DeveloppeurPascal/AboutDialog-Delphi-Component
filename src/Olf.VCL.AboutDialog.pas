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

  [ComponentPlatformsAttribute(pfidWindows)]
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
    FImageListName: TImageName;
    FImageListIndex: system.UITypes.TImageIndex;
    FonCloseDialog: TOlfAboutDialogCloseEvent;
    FonURLClick: TOlfAboutDialogURLClickEvent;
    FLangue: TOlfAboutDialogLang;
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
    procedure SetImageListName(const Value: TImageName);
    procedure SetonCloseDialog(const Value: TOlfAboutDialogCloseEvent);
    procedure SetonURLClick(const Value: TOlfAboutDialogURLClickEvent);
    procedure SetLangue(const Value: TOlfAboutDialogLang);
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
    /// A appeler pour afficher la boite de dialogue
    /// </summary>
    function Execute: boolean;
  published
    { D�clarations publi�es }
    /// <summary>
    /// Titre affich� dans la barre de titre de la fen�tre "� propos" et en premier sur la boite de dialogue
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
    /// Image utilis�e comme logo dans la boite de dialogue (pris en compte si pas d'ImageList)
    /// </summary>
    property Image: TImage read FImage write SetImage;
    /// <summary>
    /// Bitmap utilis� par l'image servant de logo dans la boite de dialogue (pris en compte si pas d'ImageList ni d'Image)
    /// </summary>
    property Picture: TPicture read FPicture write SetPicture;
    /// <summary>
    /// Liste d'images contenant le logo � utiliser dans la boite de dialogue
    /// </summary>
    property Images: TCustomImageList read FImageList write SetImageList;
    /// <summary>
    /// Index de l'image provenant de la liste d'images correspondant au logo � utiliser dans la boite de dialogue
    /// </summary>
    property ImageIndex: system.UITypes.TImageIndex read FImageListIndex
      write SetImageListIndex default -1;
    /// <summary>
    /// Nom de l'image provenant de la liste d'images correspondant au logo � utiliser dans la boite de dialogue
    /// (le nom est prioritaire sur l'index s'il est renseign� et g�r� par la liste d'images)
    /// </summary>
    property ImageName: TImageName read FImageListName write SetImageListName;
    /// <summary>
    /// Adresse du site pour lequel on affiche un lien sous le num�ro de version/date
    /// </summary>
    property URL: string read FURL write SetURL;
    /// <summary>
    /// Texte de description � afficher dans la boite de dialogue
    /// S'il est vide, son pav� n'est pas affich�.
    /// S'il est indiqu� et qu'une licence est aussi pr�sente, ils se partagent l'espace disponible.
    /// </summary>
    property Description: tstrings read FDescription write SetDescription;
    /// <summary>
    /// Texte de la licence � afficher dans la boite de dialogue
    /// S'il est vide, son pav� n'est pas affich�.
    /// S'il est indiqu� et qu'une description est aussi pr�sente, ils se partagent l'espace disponible.
    /// </summary>
    property Licence: tstrings read FLicence write SetLicence;
    /// <summary>
    /// Evenement appel� lorsdu click sur le bouton de fermeture de la boite de dialogue
    /// </summary>
    property onCloseDialog: TOlfAboutDialogCloseEvent read FonCloseDialog
      write SetonCloseDialog;
    /// <summary>
    /// Evenement appel� lorsqu'on clique sur l'URL (si URL affich�e).
    /// </summary>
    property onURLClick: TOlfAboutDialogURLClickEvent read FonURLClick
      write SetonURLClick;
    /// <summary>
    /// Langue des textes affich�s par la boite de dialogue
    /// </summary>
    property Langue: TOlfAboutDialogLang read FLangue write SetLangue;
  end;

procedure Register;

implementation

uses
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
  FImageListName := '';
  FImageListIndex := -1;
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
  try
    frmAboutDialog := TOlfAboutDialogForm.Create(owner);
    try
      frmAboutDialog.Langue := Langue;
      if assigned(Images) then
        frmAboutDialog.SetImageList(Images, ImageIndex, ImageName)
      else if assigned(Image) then
        frmAboutDialog.Image := Image
      else
        frmAboutDialog.Picture := Picture;
      frmAboutDialog.Titre := Titre;
      frmAboutDialog.VersionNumero := VersionNumero;
      frmAboutDialog.VersionDate := VersionDate;
      frmAboutDialog.URL := URL;
      frmAboutDialog.Description := Description.text;
      frmAboutDialog.Licence := Licence.text;
      frmAboutDialog.onCloseDialog := onCloseDialog;
      frmAboutDialog.onURLClick := onURLClick;
      frmAboutDialog.showmodal;
    finally
      frmAboutDialog.Free;
    end;
    result := true;
  except
    result := false;
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
      FImageListName := '';
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

procedure TOlfAboutDialog.SetImageListName(const Value: TImageName);
begin
  FImageListName := Value;
end;

procedure TOlfAboutDialog.SetPicture(const Value: TPicture);
begin
  FPicture.Assign(Value);
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

procedure TOlfAboutDialog.SetonCloseDialog(const Value
  : TOlfAboutDialogCloseEvent);
begin
  FonCloseDialog := Value;
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
