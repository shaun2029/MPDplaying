unit Search;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, MPC;

type

  { TfrmSearch }

  TfrmSearch = class(TForm)
    btnClear: TButton;
    btnSearch: TButton;
    Button2: TButton;
    btnAdd: TButton;
    edtArtist: TEdit;
    edtTitle: TEdit;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    lstResults: TListBox;
    procedure btnAddClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure btnSearchClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure edtArtistKeyPress(Sender: TObject; var Key: char);
    procedure lstResultsDblClick(Sender: TObject);
    procedure lstResultsSelectionChange(Sender: TObject; User: boolean);
  private

  public
    Host, Port: string;
  end;

var
  frmSearch: TfrmSearch;

implementation

{$R *.lfm}

{ TfrmSearch }

procedure TfrmSearch.btnSearchClick(Sender: TObject);
var
  Results: TStringList;
  i: integer;
begin
  Results := TStringList.Create;
  Results.Text := MpcSearch(Host, Port, edtArtist.Text, edtTitle.Text);
  lstResults.Clear;
  for i := 0 to Results.Count - 1 do
  begin
    lstResults.Items.Add(Results.Strings[i]);
  end;
  Results.Free;
end;

procedure TfrmSearch.Button2Click(Sender: TObject);
begin

end;

procedure TfrmSearch.edtArtistKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then
  begin
    btnSearch.Click;
  end;
end;

procedure TfrmSearch.lstResultsDblClick(Sender: TObject);
begin
  btnAdd.Click;
end;

procedure TfrmSearch.lstResultsSelectionChange(Sender: TObject; User: boolean);
begin
  btnAdd.Enabled := lstResults.SelCount > 0;
end;

procedure TfrmSearch.btnAddClick(Sender: TObject);
var
  i, Count: Integer;
begin
  Count := 0;

  if lstResults.SelCount > 0 then
  begin
    for i := lstResults.Count -1 downto 0 do
    begin
      if lstResults.Selected[i] then
      begin
        if MpcAddTrackToPlaylist(Host, Port, lstResults.Items.Strings[i]) then
        begin
          Inc(Count);
        end
        else Break;
      end;
    end;
  end;

  if (lstResults.SelCount = Count) and (lstResults.SelCount = 1) then
    MessageDlg('Added: ' + lstResults.GetSelectedText, mtInformation, [mbOK], 0)
  else
    MessageDlg('Added: ' + IntToStr(Count) + ' tracks.', mtInformation, [mbOK], 0)
end;

procedure TfrmSearch.btnClearClick(Sender: TObject);
begin
  edtArtist.Text := '';
  edtTitle.Text := '';
end;

end.

