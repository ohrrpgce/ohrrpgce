; This script is used by Inno Setup to create a Windows Installer.
; You can use this as an example for packaging your own game.
; see http://www.jrsoftware.org/isinfo.php to download Inno Setup

[Setup]
AppName=Wandering Hamster
AppVerName=Wandering Hamster (serendipity+) 20060217
VersionInfoVersion=2006.02.17.0
AppPublisher=Hamster Republic Productions
AppPublisherURL=http://HamsterRepublic.com/ohrrpgce/
AppSupportURL=http://HamsterRepublic.com/ohrrpgce/docs.php
AppUpdatesURL=http://HamsterRepublic.com/ohrrpgce/download.php
AppReadmeFile={app}\readme-wander.txt
DefaultDirName={pf}\Hamster Republic\Wandering Hamster
DefaultGroupName=Wandering Hamster
DisableProgramGroupPage=yes
AllowNoIcons=yes
AllowUNCPath=no
LicenseFile=LICENSE-binary.txt
InfoAfterFile=misc\readme-wander.txt
OutputBaseFilename=wandering-hamster
Compression=bzip
SolidCompression=yes
ChangesAssociations=no
UninstallDisplayIcon={app}\wander.ico

[Languages]
Name: "eng"; MessagesFile: "compiler:Default.isl"

[Tasks]
Name: "desktopicon"; Description: "{cm:CreateDesktopIcon}"

[Files]
Source: "wander.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: "wander.rpg"; DestDir: "{app}"; Flags: ignoreversion
Source: "misc\wanderp.hss"; DestDir: "{app}"; Flags: ignoreversion
Source: "misc\wander.ico"; DestDir: "{app}"; Flags: ignoreversion
Source: "misc\readme-wander.txt"; DestDir: "{app}"; Flags: ignoreversion
Source: "LICENSE-binary.txt"; DestDir: "{app}"; Flags: ignoreversion
; NOTE: Don't use "Flags: ignoreversion" on any shared system files

[Icons]
Name: "{group}\OHRRPGCE Game Player"; Filename: "{app}\wander.exe"; Flags: closeonexit
Name: "{userdesktop}\OHRRPGCE Game Player"; Filename: "{app}\wander.exe"; Flags: closeonexit; Tasks: desktopicon
