unit PlayList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Menus, MPC;

type

  { TfrmSearch }

  TfrmSearch = class(TForm)
    btnAdd: TButton;
    btnImportList: TButton;
    btnSave: TButton;
    btnLoad: TButton;
    btnClear: TButton;
    btnClearPlaylist: TButton;
    btnShuffle: TButton;
    btnSearch: TButton;
    btnQueuePlaylist: TButton;
    edtArtist: TEdit;
    edtTitle: TEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    lstPlaylist: TListBox;
    lstResults: TListBox;
    dlgOpen: TOpenDialog;
    mitmShuffle: TMenuItem;
    mitmSort: TMenuItem;
    N1: TMenuItem;
    mitmDelete: TMenuItem;
    Playlist: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    dlgSave: TSaveDialog;
    mnuPlayList: TPopupMenu;
    procedure btnAddClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure btnClearPlaylistClick(Sender: TObject);
    procedure btnLoadClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnSearchClick(Sender: TObject);
    procedure btnQueuePlaylistClick(Sender: TObject);
    procedure btnShuffleClick(Sender: TObject);
    procedure btnImportListClick(Sender: TObject);
    procedure edtArtistKeyPress(Sender: TObject; var Key: char);
    procedure lstResultsDblClick(Sender: TObject);
    procedure lstResultsSelectionChange(Sender: TObject; User: boolean);
    procedure mitmDeleteClick(Sender: TObject);
    procedure mitmSortClick(Sender: TObject);
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

procedure TfrmSearch.btnQueuePlaylistClick(Sender: TObject);
var
  i, Count: Integer;
begin
  Count := 0;

  if lstPlaylist.Count > 0 then
  begin
    for i := lstPlaylist.Count - 1 downto 0 do
    begin
      if MpcAddTrackToPlaylist(Host, Port, lstPlaylist.Items.Strings[i]) then
      begin
        Inc(Count);
      end
      else Break;
    end;
  end;

  if (lstResults.SelCount = Count) and (lstResults.SelCount = 1) then
    MessageDlg('Added: ' + lstResults.GetSelectedText, mtInformation, [mbOK], 0)
  else
    MessageDlg('Added: ' + IntToStr(Count) + ' tracks.', mtInformation, [mbOK], 0)
end;

procedure TfrmSearch.btnShuffleClick(Sender: TObject);
var
  i, j, k: integer;
begin
  Self.Enabled := False;
  btnShuffle.Enabled := False;
  Application.ProcessMessages;

  Randomize;
  for i := 0 to 10 do
  begin
    for j := 0 to lstPlayList.Count -1 do
    begin
      k := Random(lstPlayList.Count);
      lstPlaylist.Items.Add(lstPlayList.Items.Strings[k]);
      lstPlaylist.Items.Delete(k);
    end;
  end;

  btnShuffle.Enabled := True;
  Self.Enabled := True;
end;

procedure TfrmSearch.btnImportListClick(Sender: TObject);
var
  SongList: TStringList;
  Artist, Title: string;
  i: integer;
begin
  if dlgOpen.Execute then
  begin
    SongList := TStringList.Create;
    SongList.LoadFromFile(dlgOpen.FileName);

    for i := 0 to SongList.Count - 1 do
    begin
      Title := '';
      Artist := SongList.Strings[i];
      Artist := Copy(Artist, 1, Pos('-', Artist) - 1);
      if Length(Artist) > 0 then
      begin
         Title := Copy(SongList.Strings[i], Length(Artist) + 2, Length(SongList.Strings[i]));
      end;
      Artist := Trim(Artist);
      Title := Trim(Title);

      if (Length(Artist) > 1) and (Length(Title) > 0) then
      begin
        edtArtist.Text := Artist;
        edtTitle.Text := Title;
        btnSearch.Click;

        btnAdd.Tag := 1;

        while btnAdd.Tag > 0 do
        begin
          Sleep(20);
          Application.ProcessMessages;
        end;
      end;
    end;

    SongList.Free;
  end;
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

procedure TfrmSearch.mitmDeleteClick(Sender: TObject);
var
  i: Integer;
begin
  if lstPlaylist.SelCount > 0 then
  begin
    for i := lstPlaylist.Count -1 downto 0 do
    begin
      if lstPlaylist.Selected[i] then
      begin
        lstPlaylist.Items.Delete(i);
      end;
    end;
  end;
end;

procedure TfrmSearch.mitmSortClick(Sender: TObject);
begin
  lstPlaylist.Sorted := True;
  lstPlaylist.Sorted := False;
end;

procedure TfrmSearch.btnAddClick(Sender: TObject);
var
  i: Integer;
begin
  if lstResults.SelCount > 0 then
  begin
    for i := 0 to lstResults.Count -1 do
    begin
      if lstResults.Selected[i] then
      begin
        lstPlaylist.Items.Add(lstResults.Items.Strings[i]);
      end;
    end;
  end;

  btnAdd.Tag := 0;
end;

procedure TfrmSearch.btnClearClick(Sender: TObject);
begin
  edtArtist.Text := '';
  edtTitle.Text := '';
end;

procedure TfrmSearch.btnClearPlaylistClick(Sender: TObject);
begin
  if lstPlaylist.Count > 0 then
  begin
    if MessageDlg ('Delete Playlist', 'Do you wish to continue?', mtConfirmation,
      [mbYes, mbNo],0) = mrYes then
    begin
      lstPlaylist.Clear;
    end;
  end;
end;

procedure TfrmSearch.btnLoadClick(Sender: TObject);
begin
  if (dlgOpen.Execute) then
    lstPlaylist.Items.LoadFromFile(dlgOpen.Filename);
end;

procedure TfrmSearch.btnSaveClick(Sender: TObject);
begin
  if (dlgSave.Execute) then
    lstPlaylist.Items.SaveToFile(dlgSave.Filename);
end;

end.

