; This script is used by Inno Setup to create a Windows Installer.
; see http://www.jrsoftware.org/isinfo.php to download Inno Setup

[Setup]
AppName=Official Hamster Republic RPG Construction Engine
#include "iver.txt"
AppPublisher=Hamster Republic Productions
AppPublisherURL=http://HamsterRepublic.com/ohrrpgce/
AppSupportURL=http://HamsterRepublic.com/ohrrpgce/docs.php
AppUpdatesURL=http://HamsterRepublic.com/ohrrpgce/download.php
AppReadmeFile={app}\README-custom.txt
DefaultDirName=\OHRRPGCE
DefaultGroupName=OHRRPGCE
DisableProgramGroupPage=yes
AllowNoIcons=yes
AllowUNCPath=no
LicenseFile=LICENSE-binary.txt
#include "iextratxt.txt"
InfoAfterFile=whatsnew.txt
OutputBaseFilename=ohrrpgce
SolidCompression=yes
ChangesAssociations=yes
UninstallDisplayIcon={app}\game.ico

[Languages]
Name: "eng"; MessagesFile: "compiler:Default.isl"

[Tasks]
Name: "desktopicon"; Description: "{cm:CreateDesktopIcon}"
Name: "associate"; Description: "{cm:AssocFileExtension,the OHRRPGCE,RPG}"

[Files]
Source: "game.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: "custom.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: "SDL.dll"; DestDir: "{app}"; Flags: ignoreversion
Source: "SDL_mixer.dll"; DestDir: "{app}"; Flags: ignoreversion
Source: "gfx_directx.dll"; DestDir: "{app}"; Flags: ignoreversion
Source: "hspeak.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: "ohrrpgce.new"; DestDir: "{app}"; Flags: ignoreversion
Source: "game.ico"; DestDir: "{app}"; Flags: ignoreversion
Source: "README-game.txt"; DestDir: "{app}"; Flags: ignoreversion
Source: "README-custom.txt"; DestDir: "{app}"; Flags: ignoreversion
Source: "IMPORTANT-nightly.txt"; DestDir: "{app}"; Flags: ignoreversion
Source: "LICENSE-binary.txt"; DestDir: "{app}"; Flags: ignoreversion
Source: "whatsnew.txt"; DestDir: "{app}"; Flags: ignoreversion
Source: "plotscr.hsd"; DestDir: "{app}"; Flags: ignoreversion
Source: "scancode.hsi"; DestDir: "{app}"; Flags: ignoreversion
Source: "ohrhelp\*"; DestDir: "{app}\ohrhelp\"; Flags: ignoreversion
Source: "docs\*"; DestDir: "{app}\docs\"; Flags: ignoreversion
Source: "support\madplay.exe"; DestDir: "{app}\support\"; Flags: ignoreversion
Source: "support\oggenc.exe"; DestDir: "{app}\support\"; Flags: ignoreversion
Source: "support\LICENSE-*.txt"; DestDir: "{app}\support\"; Flags: ignoreversion
Source: "support\wget.exe"; DestDir: "{app}\support\"; Flags: ignoreversion
Source: "support\wget.hlp"; DestDir: "{app}\support\"; Flags: ignoreversion
Source: "support\zip.exe"; DestDir: "{app}\support\"; Flags: ignoreversion
Source: "support\unzip.exe"; DestDir: "{app}\support\"; Flags: ignoreversion
Source: "support\rcedit.exe"; DestDir: "{app}\support\"; Flags: ignoreversion
Source: "relump.exe"; DestDir: "{app}\support\"; Flags: ignoreversion
Source: "unlump.exe"; DestDir: "{app}\support\"; Flags: ignoreversion
Source: "vikings.rpg"; DestDir: "{app}"; Flags: ignoreversion
Source: "vikings\Vikings script files\viking.txt"; DestDir: "{app}\Vikings script files\"; Flags: ignoreversion
Source: "vikings\Vikings script files\viking.hsi"; DestDir: "{app}\Vikings script files\"; Flags: ignoreversion
Source: "vikings\Vikings script files\utility.lib"; DestDir: "{app}\Vikings script files\"; Flags: ignoreversion
Source: "vikings\README-vikings.txt"; DestDir: "{app}"; Flags: ignoreversion
Source: "import\background\*"; DestDir: "{app}\import\background"; Flags: ignoreversion
Source: "import\fonts\*"; DestDir: "{app}\import\fonts"; Flags: ignoreversion
Source: "import\Music\*"; DestDir: "{app}\import\Music"; Flags: ignoreversion
Source: "import\Sound Effects\*"; DestDir: "{app}\import\Sound Effects"; Flags: ignoreversion
Source: "import\Master Palettes\*"; DestDir: "{app}\import\Master Palettes"; Flags: ignoreversion
; NOTE: Don't use "Flags: ignoreversion" on any shared system files

[Icons]
Name: "{userdesktop}\OHRRPGCE Game Player"; Filename: "{app}\game.exe"; WorkingDir: "{app}"; Flags: closeonexit; Tasks: desktopicon
Name: "{userdesktop}\OHRRPGCE Custom Editor"; Filename: "{app}\custom.exe"; WorkingDir: "{app}"; Flags: closeonexit; Tasks: desktopicon
Name: "{userdesktop}\OHRRPGCE Folder (install games here)"; Filename: "{app}"; WorkingDir: "{app}"; Tasks: desktopicon
Name: "{group}\OHRRPGCE Game Player"; Filename: "{app}\game.exe"; WorkingDir: "{app}"; Flags: closeonexit
Name: "{group}\OHRRPGCE Custom Editor"; Filename: "{app}\custom.exe"; WorkingDir: "{app}"; Flags: closeonexit
Name: "{group}\OHRRPGCE Folder (install games here)"; Filename: "{app}"; WorkingDir: "{app}";
Name: "{group}\Website (Help, HOWTO, FAQ)"; Filename: "http://HamsterRepublic.com/ohrrpgce/";
Name: "{group}\Download RPG Games"; Filename: "http://HamsterRepublic.com/ohrrpgce/index.php/Games.html";
Name: "{group}\Plotscripting Dictionary"; Filename: "{app}\docs\plotdictionary.html";

[Registry]
Root: HKCR; Subkey: ".rpg"; ValueType: string; ValueName: ""; ValueData: "OHRRPGCE_Game"; Flags: uninsdeletevalue; Tasks: associate
Root: HKCR; Subkey: "OHRRPGCE_Game"; ValueType: string; ValueName: ""; ValueData: "OHRRPGCE Game"; Flags: uninsdeletekey; Tasks: associate
Root: HKCR; Subkey: "OHRRPGCE_Game\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "{app}\game.ico"; Flags: uninsdeletekey; Tasks: associate
Root: HKCR; Subkey: "OHRRPGCE_Game\shell\open\command"; ValueType: string; ValueName: ""; ValueData: """{app}\game.exe"" ""%1"""; Flags: uninsdeletekey; Tasks: associate
Root: HKCR; Subkey: "OHRRPGCE_Game\shell\edit\command"; ValueType: string; ValueName: ""; ValueData: """{app}\custom.exe"" ""%1"""; Flags: uninsdeletekey; Tasks: associate

[Run]

