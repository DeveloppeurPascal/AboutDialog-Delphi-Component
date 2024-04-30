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

// ********************************************************************************
// *
// * Some images from this project come from Adobe Stock. Don't use them outside
// * this program without buying a license from them.
// *
// ********************************************************************************

unit Unit1;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.Imaging.pngimage,
  Vcl.ExtCtrls,
  Vcl.Imaging.jpeg,
  System.ImageList,
  Vcl.ImgList,
  Vcl.VirtualImageList,
  Vcl.BaseImageCollection,
  Vcl.ImageCollection,
  Olf.Vcl.AboutDialog;

type
  TForm1 = class(TForm)
    Button1: TButton;
    OlfAboutDialog1: TOlfAboutDialog;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    OlfAboutDialog2: TOlfAboutDialog;
    Button6: TButton;
    Image1: TImage;
    OlfAboutDialog3: TOlfAboutDialog;
    Button7: TButton;
    ImageList1: TImageList;
    Button8: TButton;
    OlfAboutDialog4: TOlfAboutDialog;
    ImageCollection1: TImageCollection;
    VirtualImageList1: TVirtualImageList;
    Label1: TLabel;
    OlfAboutDialog5: TOlfAboutDialog;
    Button9: TButton;
    Button10: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure OlfAboutDialog1URLClick(const AURL: string);
    procedure OlfAboutDialog1CloseDialog;
    procedure Button9Click(Sender: TObject);
    procedure OlfAboutDialog5AfterExecute(Sender: TObject);
    procedure OlfAboutDialog5BeforeExecute(Sender: TObject);
    procedure OlfAboutDialog5CloseDialog;
    procedure OlfAboutDialog5FormActivate(Sender: TObject);
    procedure OlfAboutDialog5FormClose(Sender: TObject);
    procedure OlfAboutDialog5FormCreate(Sender: TObject);
    procedure OlfAboutDialog5FormShow(Sender: TObject);
    procedure OlfAboutDialog5URLClick(const AURL: string);
    procedure Button10Click(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  System.ioutils,
  u_urlOpen,
  Olf.Vcl.AboutDialogForm;

var
  FrmAboutDialog: TOlfAboutDialogForm;

procedure TForm1.OlfAboutDialog1CloseDialog;
begin
  ShowMessage('About Dialog Closed');
end;

procedure TForm1.OlfAboutDialog1URLClick(const AURL: string);
begin
  if not AURL.isEmpty then
    url_Open_In_Browser(AURL);
end;

procedure TForm1.OlfAboutDialog5AfterExecute(Sender: TObject);
begin
  ShowMessage('OlfAboutDialog5AfterExecute');
end;

procedure TForm1.OlfAboutDialog5BeforeExecute(Sender: TObject);
begin
  ShowMessage('OlfAboutDialog5BeforeExecute');
end;

procedure TForm1.OlfAboutDialog5CloseDialog;
begin
  ShowMessage('OlfAboutDialog5CloseDialog');
end;

procedure TForm1.OlfAboutDialog5FormActivate(Sender: TObject);
begin
  ShowMessage('OlfAboutDialog5FormActivate');
end;

procedure TForm1.OlfAboutDialog5FormClose(Sender: TObject);
begin
  ShowMessage('OlfAboutDialog5FormClose');
end;

procedure TForm1.OlfAboutDialog5FormCreate(Sender: TObject);
begin
  ShowMessage('OlfAboutDialog5FormCreate');
end;

procedure TForm1.OlfAboutDialog5FormShow(Sender: TObject);
begin
  ShowMessage('OlfAboutDialog5FormShow');
end;

procedure TForm1.OlfAboutDialog5URLClick(const AURL: string);
begin
  ShowMessage(AURL);
end;

procedure TForm1.Button10Click(Sender: TObject);
begin
  OlfAboutDialog3.Langue := TOlfAboutDialogLang.auto;
  OlfAboutDialog3.Execute;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  rs: TResourceStream;
  st: tstringstream;
begin
  if not assigned(FrmAboutDialog) then
    FrmAboutDialog := TOlfAboutDialogForm.Create(self);
  FrmAboutDialog.Description :=
    tfile.ReadAllText('..\..\..\ex-texte-description.txt');
  rs := TResourceStream.Create(MainInstance, 'Licence', RT_RCDATA);
  try
    rs.Position := 0;
    st := tstringstream.Create;
    try
      st.CopyFrom(rs);
      st.Position := 0;
      FrmAboutDialog.Licence := st.ReadString(st.Size);
    finally
      st.Free;
    end;
  finally
    rs.Free;
  end;
  FrmAboutDialog.ShowModal;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  rs: TResourceStream;
  st: tstringstream;
begin
  OlfAboutDialog1.Description.LoadFromFile('..\..\..\ex-texte-description.txt',
    tencoding.utf8);
  rs := TResourceStream.Create(MainInstance, 'Licence', RT_RCDATA);
  try
    rs.Position := 0;
    st := tstringstream.Create;
    try
      st.CopyFrom(rs);
      st.Position := 0;
      OlfAboutDialog1.Licence.loadfromstream(st, tencoding.utf8);
    finally
      st.Free;
    end;
  finally
    rs.Free;
  end;
  OlfAboutDialog1.Execute;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  OlfAboutDialog1.Description.text :=
    tfile.ReadAllText('..\..\..\ex-texte-description.txt');
  OlfAboutDialog1.Licence.clear;
  OlfAboutDialog1.Execute;
end;

procedure TForm1.Button4Click(Sender: TObject);
var
  rs: TResourceStream;
  st: tstringstream;
begin
  OlfAboutDialog1.Description.clear;
  rs := TResourceStream.Create(MainInstance, 'Licence', RT_RCDATA);
  try
    rs.Position := 0;
    st := tstringstream.Create;
    try
      st.CopyFrom(rs);
      st.Position := 0;
      OlfAboutDialog1.Licence.text := st.ReadString(st.Size);
    finally
      st.Free;
    end;
  finally
    rs.Free;
  end;
  OlfAboutDialog1.Execute;
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  OlfAboutDialog1.Description.clear;
  OlfAboutDialog1.Licence.clear;
  OlfAboutDialog1.Execute;
end;

procedure TForm1.Button6Click(Sender: TObject);
begin
  OlfAboutDialog2.Execute;
end;

procedure TForm1.Button7Click(Sender: TObject);
begin
  OlfAboutDialog3.Langue := TOlfAboutDialogLang.it;
  OlfAboutDialog3.Execute;
end;

procedure TForm1.Button8Click(Sender: TObject);
begin
  OlfAboutDialog4.Execute;
end;

procedure TForm1.Button9Click(Sender: TObject);
begin
  OlfAboutDialog5.Execute;
end;

initialization

FrmAboutDialog := nil;
{$IFDEF DEBUG}
ReportMemoryLeaksOnShutdown := true;
{$ENDIF}

finalization

// The form is created with main form as owner.
// if assigned(FrmAboutDialog) then
// FrmAboutDialog.Free;

end.
