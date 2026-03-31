(* C2PP
  ***************************************************************************

  "About" dialog box as a Delphi Component
  Copyright (c) 2022-2026 Patrick PREMARTIN

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU Affero General Public License as
  published by the Free Software Foundation, either version 3 of the
  License, or (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU Affero General Public License for more details.

  You should have received a copy of the GNU Affero General Public License
  along with this program.  If not, see <https://www.gnu.org/licenses/>.

  ***************************************************************************

  An "about" dialog box component for VCL and FireMonkey Delphi projects.

  ***************************************************************************

  Author(s) :
  Patrick PREMARTIN

  Site :
  https://dialogueapropos.developpeur-pascal.fr/

  Project site :
  https://github.com/DeveloppeurPascal/AboutDialog-Delphi-Component

  ***************************************************************************
  File last update : 2025-05-25T16:14:43.872+02:00
  Signature : f13144d355dd2e651975039ea5e9999a5138b266
  ***************************************************************************
*)

unit Unit2;

// ********************************************************************************
// *
// * Some images from this project come from Adobe Stock. Don't use them outside
// * this program without buying a license from them.
// *
// ********************************************************************************

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.Controls.Presentation,
  FMX.StdCtrls,
  System.ImageList,
  FMX.ImgList,
  FMX.Objects,
  FMX.Layouts,
  Olf.FMX.AboutDialog;

type
  TForm2 = class(TForm)
    Button1: TButton;
    Image1: TImage;
    ImageList1: TImageList;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Layout1: TLayout;
    OlfAboutDialog1: TOlfAboutDialog;
    Button5: TButton;
    OlfAboutDialog2: TOlfAboutDialog;
    Button6: TButton;
    Button7: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure OlfAboutDialog1CloseDialog;
    procedure OlfAboutDialog1URLClick(const AURL: string);
    procedure Button6Click(Sender: TObject);
    procedure OlfAboutDialog2AfterExecute(Sender: TObject);
    procedure OlfAboutDialog2BeforeExecute(Sender: TObject);
    procedure OlfAboutDialog2CloseDialog;
    procedure OlfAboutDialog2FormActivate(Sender: TObject);
    procedure OlfAboutDialog2FormClose(Sender: TObject);
    procedure OlfAboutDialog2FormCreate(Sender: TObject);
    procedure OlfAboutDialog2FormShow(Sender: TObject);
    procedure OlfAboutDialog2URLClick(const AURL: string);
    procedure Button7Click(Sender: TObject);
  private
  public
  end;

var
  Form2: TForm2;

implementation

{$R *.fmx}

uses
  System.IOUtils,
  u_urlOpen,
  Olf.FMX.AboutDialogForm;

procedure TForm2.Button1Click(Sender: TObject);
begin
  TOlfAboutDialog.Create(self).Execute;
end;

procedure TForm2.Button2Click(Sender: TObject);
begin
  with TOlfAboutDialog.Create(self) do
  begin
    Titre := 'TITRE DU LOGICIEL';
    VersionNumero := '8964';
    versiondate := '12/03/2022';
    url := 'https://developpeur-pascal.fr';
    description.text := 'description du logiciel';
    licence.Clear;
    for var i := 1 to 10 do
      licence.Add('licence du logiciel ligne ' + i.ToString);
    image := Image1;
    onURLClick := OlfAboutDialog1URLClick;
    Execute;
  end;
end;

procedure TForm2.Button3Click(Sender: TObject);
begin
  with TOlfAboutDialog.Create(self) do
  begin
    Titre := 'TITRE DU LOGICIEL';
    VersionNumero := '8964';
    versiondate := '12/03/2022';
    url := 'https://developpeur-pascal.fr';
    description.Clear;
    for var i := 1 to 10 do
      description.Add('licence du logiciel ligne ' + i.ToString);
    MultiResBitmap := Image1.MultiResBitmap;
    onURLClick := OlfAboutDialog1URLClick;
    Execute;
  end;
end;

procedure TForm2.Button4Click(Sender: TObject);
var
  dlg: TOlfAboutDialog;
begin
  dlg := TOlfAboutDialog.Create(self);
  try
    dlg.Titre := 'TITRE DU LOGICIEL';
    dlg.VersionNumero := '8964';
    dlg.versiondate := '12/03/2022';
    dlg.url := 'https://developpeur-pascal.fr';
    dlg.description.Clear;
    for var i := 1 to 10 do
      dlg.description.Add('licence du logiciel ligne ' + i.ToString);
    dlg.Images := ImageList1;
    dlg.ImageIndex := 0;
    dlg.onURLClick := OlfAboutDialog1URLClick;
    dlg.Execute;
  finally
    dlg.free;
  end;
end;

procedure TForm2.Button5Click(Sender: TObject);
begin
  OlfAboutDialog1.Langue := TOlfAboutDialogLang.de;
  OlfAboutDialog1.Execute;
end;

procedure TForm2.Button6Click(Sender: TObject);
begin
  OlfAboutDialog2.Execute;
end;

procedure TForm2.Button7Click(Sender: TObject);
begin
  OlfAboutDialog1.Langue := TOlfAboutDialogLang.Auto;
  OlfAboutDialog1.Execute;
end;

procedure TForm2.OlfAboutDialog1CloseDialog;
begin
  ShowMessage('About Dialog Closed');
end;

procedure TForm2.OlfAboutDialog1URLClick(const AURL: string);
begin
  if not AURL.isEmpty then
    url_Open_In_Browser(AURL);
end;

procedure TForm2.OlfAboutDialog2AfterExecute(Sender: TObject);
begin
  ShowMessage('OlfAboutDialog2AfterExecute');
end;

procedure TForm2.OlfAboutDialog2BeforeExecute(Sender: TObject);
begin
  ShowMessage('OlfAboutDialog2BeforeExecute');
end;

procedure TForm2.OlfAboutDialog2CloseDialog;
begin
  ShowMessage('OlfAboutDialog2CloseDialog');
end;

procedure TForm2.OlfAboutDialog2FormActivate(Sender: TObject);
begin
  ShowMessage('OlfAboutDialog2FormActivate');
end;

procedure TForm2.OlfAboutDialog2FormClose(Sender: TObject);
begin
  ShowMessage('OlfAboutDialog2FormClose');
end;

procedure TForm2.OlfAboutDialog2FormCreate(Sender: TObject);
begin
  ShowMessage('OlfAboutDialog2FormCreate');
end;

procedure TForm2.OlfAboutDialog2FormShow(Sender: TObject);
begin
  ShowMessage('OlfAboutDialog2FormShow');
end;

procedure TForm2.OlfAboutDialog2URLClick(const AURL: string);
begin
  ShowMessage(AURL);
end;

initialization

{$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := true;
{$ENDIF}

end.
