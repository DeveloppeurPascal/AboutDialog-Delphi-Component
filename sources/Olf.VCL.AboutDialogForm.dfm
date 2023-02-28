object OlfAboutDialogForm: TOlfAboutDialogForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'A propos'
  ClientHeight = 283
  ClientWidth = 580
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  TextHeight = 15
  object imgLogo: TImage
    Left = 0
    Top = 0
    Width = 200
    Height = 283
    Align = alLeft
    Center = True
    Proportional = True
    Stretch = True
    Transparent = True
  end
  object Panel1: TPanel
    Left = 200
    Top = 0
    Width = 380
    Height = 283
    Align = alClient
    BevelEdges = []
    BevelOuter = bvNone
    TabOrder = 0
    object pnlTop: TPanel
      Left = 0
      Top = 0
      Width = 380
      Height = 110
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 0
      object lblURL: TLabel
        AlignWithMargins = True
        Left = 5
        Top = 85
        Width = 370
        Height = 15
        Cursor = crHandPoint
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Align = alTop
        Caption = 'lblURL'
        Color = clDefault
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clHighlight
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = [fsUnderline]
        ParentColor = False
        ParentFont = False
        OnClick = lblURLClick
      end
      object lblVersion: TStaticText
        AlignWithMargins = True
        Left = 5
        Top = 56
        Width = 370
        Height = 19
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Align = alTop
        Caption = 'lblVersion'
        TabOrder = 0
      end
      object lblTitre: TStaticText
        AlignWithMargins = True
        Left = 5
        Top = 5
        Width = 370
        Height = 41
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Align = alTop
        Caption = 'lblTitre'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -27
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold]
        ParentFont = False
        ShowAccelChar = False
        TabOrder = 1
      end
    end
    object pnlclient: TPanel
      Left = 0
      Top = 110
      Width = 380
      Height = 132
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      object pnlDescription: TPanel
        AlignWithMargins = True
        Left = 5
        Top = 5
        Width = 370
        Height = 41
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Align = alTop
        BevelInner = bvLowered
        TabOrder = 0
        object ScrollBox1: TScrollBox
          Left = 2
          Top = 2
          Width = 366
          Height = 37
          Align = alClient
          TabOrder = 0
          object lblDescription: TLabel
            AlignWithMargins = True
            Left = 5
            Top = 5
            Width = 352
            Height = 15
            Margins.Left = 5
            Margins.Top = 5
            Margins.Right = 5
            Margins.Bottom = 5
            Align = alTop
            Caption = 'lblDescription'
            WordWrap = True
          end
        end
      end
      object pnlLicence: TPanel
        AlignWithMargins = True
        Left = 5
        Top = 56
        Width = 370
        Height = 71
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Align = alClient
        BevelInner = bvLowered
        TabOrder = 1
        object ScrollBox2: TScrollBox
          Left = 2
          Top = 2
          Width = 366
          Height = 67
          Align = alClient
          TabOrder = 0
          object lblLicence: TLabel
            AlignWithMargins = True
            Left = 5
            Top = 5
            Width = 352
            Height = 15
            Margins.Left = 5
            Margins.Top = 5
            Margins.Right = 5
            Margins.Bottom = 5
            Align = alTop
            Caption = 'lblLicence'
            WordWrap = True
          end
        end
      end
    end
    object pnlBottom: TPanel
      Left = 0
      Top = 242
      Width = 380
      Height = 41
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 2
      DesignSize = (
        380
        41)
      object btnClose: TBitBtn
        Left = 248
        Top = 6
        Width = 120
        Height = 25
        Anchors = [akRight, akBottom]
        Kind = bkClose
        NumGlyphs = 2
        TabOrder = 0
        OnClick = btnCloseClick
        OnKeyDown = btnCloseKeyDown
      end
    end
  end
end
