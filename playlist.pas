unit PlayList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Menus, MPC,
  LCLType;

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
    MenuItem1: TMenuItem;
    mitmQueueSelectedPlaylistTracks: TMenuItem;
    N2: TMenuItem;
    mitmQueueTrack: TMenuItem;
    mitmAddToPlaylist: TMenuItem;
    mitmShuffle: TMenuItem;
    mitmSort: TMenuItem;
    N1: TMenuItem;
    mitmDelete: TMenuItem;
    Playlist: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    dlgSave: TSaveDialog;
    mnuPlayList: TPopupMenu;
    mnuSearchResults: TPopupMenu;
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
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure lstResultsDblClick(Sender: TObject);
    procedure lstResultsKeyPress(Sender: TObject; var Key: char);
    procedure lstResultsSelectionChange(Sender: TObject; User: boolean);
    procedure mitmAddToPlaylistClick(Sender: TObject);
    procedure mitmDeleteClick(Sender: TObject);
    procedure mitmQueueSelectedPlaylistTracksClick(Sender: TObject);
    procedure mitmQueueTrackClick(Sender: TObject);
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

  if (lstPlaylist.SelCount = Count) and (lstResults.SelCount = 1) then
    MessageDlg('Added: ' + lstResults.GetSelectedText, mtInformation, [mbOK], 0)
  else
    MessageDlg('Queued: ' + IntToStr(Count) + ' tracks.', mtInformation, [mbOK], 0)
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

    if lstResults.Items.Count > 0 then
      lstResults.SetFocus;

    Key := #0;
  end;
end;

procedure TfrmSearch.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
  begin
    Key := 0;
    Close;
  end;
end;

procedure TfrmSearch.lstResultsDblClick(Sender: TObject);
begin
  btnAdd.Click;
end;

procedure TfrmSearch.lstResultsKeyPress(Sender: TObject; var Key: char);
begin
  if (Key = #32) then
  begin
    Key := #0;
    mitmQueueTrackClick(Self);
  end
  else if (Key = #13) then
  begin
    Key := #0;
    mitmQueueTrackClick(Self);
    Close;
  end;
end;

procedure TfrmSearch.lstResultsSelectionChange(Sender: TObject; User: boolean);
begin
  btnAdd.Enabled := lstResults.SelCount > 0;
end;

procedure TfrmSearch.mitmAddToPlaylistClick(Sender: TObject);
var
  i: Integer;
begin
  if lstResults.SelCount > 0 then
  begin
    for i := 0 to lstResults.Count -1 do
    begin
      if lstResults.Selected[i] then
      begin
        lstPlaylist.Items.Append(lstResults.Items[i]);
      end;
    end;
  end;
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

procedure TfrmSearch.mitmQueueSelectedPlaylistTracksClick(Sender: TObject);
var
  i, Count: Integer;
begin
  Count := 0;

  if lstPlayList.Count > 0 then
  begin
    for i := lstPlayList.Count - 1 downto 0 do
    begin
      if lstPlayList.Selected[i] then
      begin
        if MpcAddTrackToPlaylist(Host, Port, lstPlayList.Items.Strings[i]) then
        begin
          Inc(Count);
        end
        else Break;
      end;
    end;
  end;

  if (lstPlayList.SelCount <> Count) then
    MessageDlg('Not All Tracks Queued: ' + IntToStr(Count) + ' tracks.', mtInformation, [mbOK], 0)
end;

procedure TfrmSearch.mitmQueueTrackClick(Sender: TObject);
var
  i, Count: Integer;
begin
  Count := 0;

  if lstResults.Count > 0 then
  begin
    for i := lstResults.Count - 1 downto 0 do
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

  if (lstResults.SelCount <> Count) then
    MessageDlg('Not All Tracks Queued: ' + IntToStr(Count) + ' tracks.', mtInformation, [mbOK], 0)
  else
  begin
    lstResults.Enabled := False;
    Application.ProcessMessages;
    Sleep(500);
    lstResults.Enabled := True;
    lstResults.SetFocus;
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

