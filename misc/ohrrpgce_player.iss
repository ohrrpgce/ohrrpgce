; This script is used by Inno Setup to create a Windows Installer.
; see http://www.jrsoftware.org/isinfo.php to download Inno Setup

[Setup]
AppName=Official Hamster Republic RPG Construction Engine
#include "iver.txt"
AppPublisher=Hamster Republic Productions
AppPublisherURL=http://HamsterRepublic.com/ohrrpgce/
AppSupportURL=http://HamsterRepublic.com/ohrrpgce/docs.php
AppUpdatesURL=http://HamsterRepublic.com/ohrrpgce/download.php
AppReadmeFile={app}\README-game.txt
DefaultDirName={pf}\Hamster Republic\OHRRPGCE
DefaultGroupName=OHRRPGCE
DisableProgramGroupPage=yes
AllowNoIcons=yes
AllowUNCPath=no
LicenseFile=LICENSE-binary.txt
InfoAfterFile=README-game.txt
OutputBaseFilename=ohrrpgce_play
Compression=bzip
SolidCompression=yes
ChangesAssociations=yes
UninstallDisplayIcon={app}\game.ico

[Languages]
Name: "eng"; MessagesFile: "compiler:Default.isl"

[Tasks]
Name: "desktopicon"; Description: "{cm:CreateDesktopIcon}"
Name: "associate"; Description: "{cm:AssocFileExtension,GAME.EXE,RPG}"

[Files]
Source: "game.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: "ohrrpgce.fnt"; DestDir: "{app}"; Flags: ignoreversion
Source: "game.ico"; DestDir: "{app}"; Flags: ignoreversion
Source: "README-game.txt"; DestDir: "{app}"; Flags: ignoreversion
Source: "LICENSE-binary.txt"; DestDir: "{app}"; Flags: ignoreversion
; NOTE: Don't use "Flags: ignoreversion" on any shared system files

[Icons]
Name: "{group}\OHRRPGCE Game Player"; Filename: "{app}\game.exe"; Flags: closeonexit
Name: "{userdesktop}\OHRRPGCE Game Player"; Filename: "{app}\game.exe"; Flags: closeonexit; Tasks: desktopicon

[Registry]
Root: HKCR; Subkey: ".rpg"; ValueType: string; ValueName: ""; ValueData: "OHRRPGCE_Game"; Flags: uninsdeletevalue; Tasks: associate
Root: HKCR; Subkey: "OHRRPGCE_Game"; ValueType: string; ValueName: ""; ValueData: "OHRRPGCE Game"; Flags: uninsdeletekey; Tasks: associate
Root: HKCR; Subkey: "OHRRPGCE_Game\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "{app}\game.ico"; Tasks: associate
Root: HKCR; Subkey: "OHRRPGCE_Game\shell\open\command"; ValueType: string; ValueName: ""; ValueData: """{app}\game.exe"" %1"; Tasks: associate

[Run]

