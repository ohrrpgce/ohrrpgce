import version

values = {"name":     version.name,
          "longname": version.app_name,
          "exename":  "%s.exe" % (version.name),
          "xmlfile":  "%s.xml" % (version.name),
          "version":  version.version,
          "publisher":version.publisher,
          "website":  version.website}
template = """
; This script is used by Inno Setup to create a Windows Installer.
; see http://www.jrsoftware.org/isinfo.php to download Inno Setup

[Setup]
AppName=%(longname)s
AppVerName=%(longname)s %(version)s
VersionInfoVersion=%(version)s.0
AppPublisher=%(publisher)s
AppPublisherURL=%(website)s
AppSupportURL=%(website)s
AppUpdatesURL=%(website)s
AppReadmeFile={app}\README.txt
DefaultDirName={pf}\%(longname)s
DefaultGroupName=%(longname)s
DisableProgramGroupPage=yes
AllowNoIcons=yes
AllowUNCPath=yes
LicenseFile=LICENSE.txt
InfoAfterFile=README.txt
OutputBaseFilename=%(name)s-setup
Compression=bzip
SolidCompression=yes
ChangesAssociations=yes
UninstallDisplayIcon={app}\%(exename)s

[Languages]
Name: "eng"; MessagesFile: "compiler:Default.isl"

[Tasks]
Name: "desktopicon"; Description: "{cm:CreateDesktopIcon}"

[Files]
Source: "dist\%(exename)s"; DestDir: "{app}"; Flags: ignoreversion
Source: "dist\MSVCR71.dll"; DestDir: "{app}"; Flags: ignoreversion
Source: "dist\python25.dll"; DestDir: "{app}"; Flags: ignoreversion
Source: "dist\w9xpopen.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: "dist\library.zip"; DestDir: "{app}"; Flags: ignoreversion
Source: "dist\%(xmlfile)s"; DestDir: "{app}"; Flags: ignoreversion
Source: "dist\*.pyd"; DestDir: "{app}"; Flags: ignoreversion
Source: "dist\*.dll"; DestDir: "{app}"; Flags: ignoreversion
Source: "dist\etc\*"; DestDir: "{app}\etc\"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "dist\lib\*"; DestDir: "{app}\lib\"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "dist\share\*"; DestDir: "{app}\share\"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "README.txt"; DestDir: "{app}"; Flags: ignoreversion
Source: "LICENSE.txt"; DestDir: "{app}"; Flags: ignoreversion
; NOTE: Don't use "Flags: ignoreversion" on any shared system files

[Icons]
Name: "{group}\%(longname)s"; Filename: "{app}\%(exename)s"; WorkingDir: "{app}"; Flags: closeonexit
Name: "{userdesktop}\%(longname)s"; Filename: "{app}\%(exename)s"; WorkingDir: "{app}"; Flags: closeonexit; Tasks: desktopicon
Name: "{group}\Website (Help, Updates, Source Code)"; Filename: "%(website)s";

[Registry]

[Run]
"""

f = open("win-installer.iss", "w")
f.write(template % values)
f.close()
