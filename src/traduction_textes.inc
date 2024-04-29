if (TxtID = TOlfAboutDialogTxtID.Footer) then
begin
  if assigned(onGetFooterTextProc) then
    result := onGetFooterTextProc(FLangue, TxtID)
  else if assigned(onGetFooterText) then
    result := onGetFooterText(FLangue, TxtID)
  else
    result := '';
end
else
begin
  if assigned(onGetTextProc) then
    result := onGetTextProc(FLangue, TxtID)
  else if assigned(onGetText) then
    result := onGetText(FLangue, TxtID)
  else
    result := '';

  if result.IsEmpty and not(FLangue = TOlfAboutDialogLang.Manual) then
    case FLangue of
      TOlfAboutDialogLang.FR:
        case TxtID of
          TOlfAboutDialogTxtID.About:
            result := 'A propos de ';
          TOlfAboutDialogTxtID.Version:
            result := 'Version ';
          TOlfAboutDialogTxtID.Date:
            result := ' du ';
          TOlfAboutDialogTxtID.VersionDate:
            result := 'Version du ';
          TOlfAboutDialogTxtID.CloseButton:
            result := 'Fermer';
          TOlfAboutDialogTxtID.LicenseInfoButton:
            result := 'Licence';
          TOlfAboutDialogTxtID.BuyButton:
            result := 'Acheter';
          TOlfAboutDialogTxtID.RegisterButton:
            result := 'Enregistrer';
        else
          raise exception.Create('Texte non gérée');
        end;
      TOlfAboutDialogLang.EN:
        case TxtID of
          TOlfAboutDialogTxtID.About:
            result := 'About ';
          TOlfAboutDialogTxtID.Version:
            result := 'Release ';
          TOlfAboutDialogTxtID.Date:
            result := ' on ';
          TOlfAboutDialogTxtID.VersionDate:
            result := 'Release date ';
          TOlfAboutDialogTxtID.CloseButton:
            result := 'Close';
          TOlfAboutDialogTxtID.LicenseInfoButton:
            result := 'License';
          TOlfAboutDialogTxtID.BuyButton:
            result := 'Buy';
          TOlfAboutDialogTxtID.RegisterButton:
            result := 'Register';
        else
          raise exception.Create('Texte non gérée');
        end;
      TOlfAboutDialogLang.IT:
        case TxtID of
          TOlfAboutDialogTxtID.About:
            result := 'Chi siamo ';
          TOlfAboutDialogTxtID.Version:
            result := 'Versione ';
          TOlfAboutDialogTxtID.Date:
            result := ' di ';
          TOlfAboutDialogTxtID.VersionDate:
            result := 'Versione di ';
          TOlfAboutDialogTxtID.CloseButton:
            result := 'Chiudere';
          TOlfAboutDialogTxtID.LicenseInfoButton:
            result := 'Licenza';
          TOlfAboutDialogTxtID.BuyButton:
            result := 'Acquistare';
          TOlfAboutDialogTxtID.RegisterButton:
            result := 'Registro';
        else
          raise exception.Create('Texte non gérée');
        end;
      TOlfAboutDialogLang.PT:
        case TxtID of
          TOlfAboutDialogTxtID.About:
            result := 'Sobre ';
          TOlfAboutDialogTxtID.Version:
            result := 'Versão ';
          TOlfAboutDialogTxtID.Date:
            result := ' de ';
          TOlfAboutDialogTxtID.VersionDate:
            result := 'Versão de ';
          TOlfAboutDialogTxtID.CloseButton:
            result := 'Fechar';
          TOlfAboutDialogTxtID.LicenseInfoButton:
            result := 'Licença';
          TOlfAboutDialogTxtID.BuyButton:
            result := 'Comprar';
          TOlfAboutDialogTxtID.RegisterButton:
            result := 'Registar';
        else
          raise exception.Create('Texte non gérée');
        end;
      TOlfAboutDialogLang.SP:
        case TxtID of
          TOlfAboutDialogTxtID.About:
            result := 'Sobre nosotros ';
          TOlfAboutDialogTxtID.Version:
            result := 'Versión ';
          TOlfAboutDialogTxtID.Date:
            result := ' al ';
          TOlfAboutDialogTxtID.VersionDate:
            result := 'Versión al ';
          TOlfAboutDialogTxtID.CloseButton:
            result := 'Cerrar';
          TOlfAboutDialogTxtID.LicenseInfoButton:
            result := 'Licencia';
          TOlfAboutDialogTxtID.BuyButton:
            result := 'Comprar';
          TOlfAboutDialogTxtID.RegisterButton:
            result := 'Registro';
        else
          raise exception.Create('Texte non gérée');
        end;
      TOlfAboutDialogLang.DE:
        case TxtID of
          TOlfAboutDialogTxtID.About:
            result := 'Über ';
          TOlfAboutDialogTxtID.Version:
            result := 'Version ';
          TOlfAboutDialogTxtID.Date:
            result := ' des ';
          TOlfAboutDialogTxtID.VersionDate:
            result := 'Version des ';
          TOlfAboutDialogTxtID.CloseButton:
            result := 'Schließen';
          TOlfAboutDialogTxtID.LicenseInfoButton:
            result := 'Lizenz';
          TOlfAboutDialogTxtID.BuyButton:
            result := 'Kaufen';
          TOlfAboutDialogTxtID.RegisterButton:
            result := 'Registrieren';
        else
          raise exception.Create('Texte non gérée');
        end;
    else
      raise exception.Create('Langue non gérée');
    end;
end;
