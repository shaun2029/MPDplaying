unit Settings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Buttons,
  StdCtrls, IniPropStorage;

type

  { TfrmSettings }

  TfrmSettings = class(TForm)
    btnPrev: TBitBtn;
    edtHost: TEdit;
    edtPort: TEdit;
    IniPropStorage1: TIniPropStorage;
    lblHost: TLabel;
    lblHost1: TLabel;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frmSettings: TfrmSettings;

implementation

{$R *.lfm}

end.

