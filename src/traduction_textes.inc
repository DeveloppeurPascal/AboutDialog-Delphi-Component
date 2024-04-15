  case FLangue of
    TOlfAboutDialogLang.FR:
      case TxtID of
        TTxtID.APropos:
          result := 'A propos de ';
        TTxtID.Version:
          result := 'Version ';
        TTxtID.Du:
          result := ' du ';
        TTxtID.VersionDu:
          result := 'Version du ';
        TTxtID.BoutonFermer:
          result := 'Fermer';
      else
        raise exception.Create('Texte non gérée');
      end;
    TOlfAboutDialogLang.EN:
      case TxtID of
        TTxtID.APropos:
          result := 'About ';
        TTxtID.Version:
          result := 'Release ';
        TTxtID.Du:
          result := ' on ';
        TTxtID.VersionDu:
          result := 'Release date ';
        TTxtID.BoutonFermer:
          result := 'Close';
      else
        raise exception.Create('Texte non gérée');
      end;
    TOlfAboutDialogLang.IT:
      case TxtID of
        TTxtID.APropos:
          result := 'Chi siamo ';
        TTxtID.Version:
          result := 'Versione ';
        TTxtID.Du:
          result := ' di ';
        TTxtID.VersionDu:
          result := 'Versione di ';
        TTxtID.BoutonFermer:
          result := 'Chiudere';
      else
        raise exception.Create('Texte non gérée');
      end;
    TOlfAboutDialogLang.PT:
      case TxtID of
        TTxtID.APropos:
          result := 'Sobre ';
        TTxtID.Version:
          result := 'Versão ';
        TTxtID.Du:
          result := ' de ';
        TTxtID.VersionDu:
          result := 'Versão de ';
        TTxtID.BoutonFermer:
          result := 'Fechar';
      else
        raise exception.Create('Texte non gérée');
      end;
    TOlfAboutDialogLang.SP:
      case TxtID of
        TTxtID.APropos:
          result := 'Sobre nosotros ';
        TTxtID.Version:
          result := 'Versión ';
        TTxtID.Du:
          result := ' al ';
        TTxtID.VersionDu:
          result := 'Versión al ';
        TTxtID.BoutonFermer:
          result := 'Cerrar';
      else
        raise exception.Create('Texte non gérée');
      end;
    TOlfAboutDialogLang.DE:
      case TxtID of
        TTxtID.APropos:
          result := 'Über ';
        TTxtID.Version:
          result := 'Version ';
        TTxtID.Du:
          result := ' des ';
        TTxtID.VersionDu:
          result := 'Version des ';
        TTxtID.BoutonFermer:
          result := 'Schließen';
      else
        raise exception.Create('Texte non gérée');
      end;
  else
    raise exception.Create('Langue non gérée');
  end;
