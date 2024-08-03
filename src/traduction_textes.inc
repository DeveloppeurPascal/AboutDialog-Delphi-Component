if (TxtID = TOlfAboutDialogTxtID.Footer) then
begin
  if assigned(onGetFooterTextProc) then
    Result := onGetFooterTextProc(FInternalLangue, TxtID)
  else if assigned(onGetFooterText) then
    Result := onGetFooterText(FInternalLangue, TxtID)
  else
    Result := '';
end
else
begin
  if assigned(onGetTextProc) then
    Result := onGetTextProc(FInternalLangue, TxtID)
  else if assigned(onGetText) then
    Result := onGetText(FInternalLangue, TxtID)
  else
    Result := '';

  if Result.IsEmpty and not(FInternalLangue = TOlfAboutDialogLang.Manual) then
    case TxtID of
      TOlfAboutDialogTxtID.LicenseText:
        Result := FLicence;
      TOlfAboutDialogTxtID.DescriptionText:
        Result := FDescription;
      TOlfAboutDialogTxtID.TitleText:
        Result := FTitre;
    else
      case FInternalLangue of
        TOlfAboutDialogLang.FR:
          case TxtID of
            TOlfAboutDialogTxtID.About:
              Result := 'A propos de ';
            TOlfAboutDialogTxtID.Version:
              Result := 'Version ';
            TOlfAboutDialogTxtID.Date:
              Result := ' du ';
            TOlfAboutDialogTxtID.VersionDate:
              Result := 'Version du ';
            TOlfAboutDialogTxtID.CloseButton:
              Result := 'Fermer';
            TOlfAboutDialogTxtID.LicenseInfoButton:
              Result := 'Licence';
            TOlfAboutDialogTxtID.BuyButton:
              Result := 'Acheter';
            TOlfAboutDialogTxtID.RegisterButton:
              Result := 'Enregistrer';
          else
            raise exception.Create('Texte non gérée');
          end;
        TOlfAboutDialogLang.EN:
          case TxtID of
            TOlfAboutDialogTxtID.About:
              Result := 'About ';
            TOlfAboutDialogTxtID.Version:
              Result := 'Release ';
            TOlfAboutDialogTxtID.Date:
              Result := ' on ';
            TOlfAboutDialogTxtID.VersionDate:
              Result := 'Release date ';
            TOlfAboutDialogTxtID.CloseButton:
              Result := 'Close';
            TOlfAboutDialogTxtID.LicenseInfoButton:
              Result := 'License';
            TOlfAboutDialogTxtID.BuyButton:
              Result := 'Buy';
            TOlfAboutDialogTxtID.RegisterButton:
              Result := 'Register';
          else
            raise exception.Create('Texte non gérée');
          end;
        TOlfAboutDialogLang.IT:
          case TxtID of
            TOlfAboutDialogTxtID.About:
              Result := 'Chi siamo ';
            TOlfAboutDialogTxtID.Version:
              Result := 'Versione ';
            TOlfAboutDialogTxtID.Date:
              Result := ' di ';
            TOlfAboutDialogTxtID.VersionDate:
              Result := 'Versione di ';
            TOlfAboutDialogTxtID.CloseButton:
              Result := 'Chiudere';
            TOlfAboutDialogTxtID.LicenseInfoButton:
              Result := 'Licenza';
            TOlfAboutDialogTxtID.BuyButton:
              Result := 'Acquistare';
            TOlfAboutDialogTxtID.RegisterButton:
              Result := 'Registro';
          else
            raise exception.Create('Texte non gérée');
          end;
        TOlfAboutDialogLang.PT:
          case TxtID of
            TOlfAboutDialogTxtID.About:
              Result := 'Sobre ';
            TOlfAboutDialogTxtID.Version:
              Result := 'Versão ';
            TOlfAboutDialogTxtID.Date:
              Result := ' de ';
            TOlfAboutDialogTxtID.VersionDate:
              Result := 'Versão de ';
            TOlfAboutDialogTxtID.CloseButton:
              Result := 'Fechar';
            TOlfAboutDialogTxtID.LicenseInfoButton:
              Result := 'Licença';
            TOlfAboutDialogTxtID.BuyButton:
              Result := 'Comprar';
            TOlfAboutDialogTxtID.RegisterButton:
              Result := 'Registar';
          else
            raise exception.Create('Texte non gérée');
          end;
        TOlfAboutDialogLang.SP:
          case TxtID of
            TOlfAboutDialogTxtID.About:
              Result := 'Sobre nosotros ';
            TOlfAboutDialogTxtID.Version:
              Result := 'Versión ';
            TOlfAboutDialogTxtID.Date:
              Result := ' al ';
            TOlfAboutDialogTxtID.VersionDate:
              Result := 'Versión al ';
            TOlfAboutDialogTxtID.CloseButton:
              Result := 'Cerrar';
            TOlfAboutDialogTxtID.LicenseInfoButton:
              Result := 'Licencia';
            TOlfAboutDialogTxtID.BuyButton:
              Result := 'Comprar';
            TOlfAboutDialogTxtID.RegisterButton:
              Result := 'Registro';
          else
            raise exception.Create('Texte non gérée');
          end;
        TOlfAboutDialogLang.DE:
          case TxtID of
            TOlfAboutDialogTxtID.About:
              Result := 'Über ';
            TOlfAboutDialogTxtID.Version:
              Result := 'Version ';
            TOlfAboutDialogTxtID.Date:
              Result := ' des ';
            TOlfAboutDialogTxtID.VersionDate:
              Result := 'Version des ';
            TOlfAboutDialogTxtID.CloseButton:
              Result := 'Schließen';
            TOlfAboutDialogTxtID.LicenseInfoButton:
              Result := 'Lizenz';
            TOlfAboutDialogTxtID.BuyButton:
              Result := 'Kaufen';
            TOlfAboutDialogTxtID.RegisterButton:
              Result := 'Registrieren';
          else
            raise exception.Create('Texte non gérée');
          end;
      else
        raise exception.Create('Langue non gérée');
      end;
    end;
end;
