' Some defines and typedefs for the Win32Driver structure
#define WINDOW_TITLE_SIZE   128
#define WINDOW_CLASS_PREFIX "fbgfxclass_"

' Structure containing GfxLib's internal Windows info
Type WIN32DRIVER
    Version             As Integer
    hInstance           As HINSTANCE
    WndClass            As WNDCLASS
    Wnd                 As HWND   
    Palette(0 To 255)   As PaletteEntry
    Blitter             As Sub(ByVal As uByte Ptr, ByVal As Integer)
    Is_Running          As Integer
    Is_Palette_Changed  As Integer
    Is_Active           As Integer
    w                   As Integer
    h                   As Integer
    Depth               As Integer
    FullScreen          As Integer
    Refresh_Rate        As Integer
    Window_Title        As uByte Ptr
    WindowClass(0 To WINDOW_TITLE_SIZE + Len(WINDOW_CLASS_PREFIX)-1) As uByte
    Init As Function()  As Integer
    Exit As Function()  As Integer
    Paint As Function() As Integer
    Thread              As Sub(ByVal Running_Event As HANDLE)
End Type

' Structures Containing GfxLib font info
Type FONTTYPE
    w As Integer
    DataPtr As Any Ptr
End Type

' Structure containing palette info
Type PALETTE_
    Colors As Integer
    Data As uByte Ptr
End Type

' Structure containing the GfxLib driver
Type GFXDRIVER
    Name              As Byte Ptr
    Init              As Function(ByVal Title As Byte Ptr, ByVal w As Integer, ByVal h As Integer, ByVal depth As Integer, ByVal Refresh_Rate As Integer, ByVal flags As Integer) As Integer
    Exit              As Sub()
    Lock              As Sub()
    Unlock            As Sub()
    Set_Palette       As Sub     (ByVal Index As Integer, ByVal r As Integer, ByVal g As Integer, ByVal b As Integer)
    Wait_VSync        As Sub()
    Get_Mouse         As Function(ByVal x As Integer Ptr, ByVal y As Integer Ptr, ByVal z As Integer Ptr, ByVal Buttons As Integer Ptr) As Integer
    Set_Mouse         As Sub     (ByVal x As Integer, ByVal y As Integer, ByVal Cursor As Integer)
    Set_Windows_Title As Sub     (ByVal Title As Integer Ptr)
    Fetch_Modes       As Function(ByVal Depth As Integer, ByVal Size As Integer Ptr) As Integer Ptr
    Flip              As Sub()
End Type

Type MODE
    Mode_Num          As Integer        ' Current mode number
    Page              As uByte Ptr Ptr  ' Pages memory
    Num_Pages         As Integer        ' Number of requested pages
    Work_Page         As Integer        ' Current work page number
    FrameBuffer       As uByte Ptr      ' Our current visible framebuffer
    Line              As uByte Ptr Ptr  ' Line pointers into current active framebuffer
    Pitch             As Integer        ' Width of a framebuffer line in bytes
    Target_Pitch      As Integer        ' Width of current target buffer line in bytes
    Last_Target       As Any Ptr        ' Last target buffer set
    Max_h             As Integer        ' Max registered height of target buffer
    Bpp               As Integer        ' Bytes per pixel
    Palette           As uInteger Ptr   ' Current RGB color values for each palette index */
    Device_Palette    As uInteger Ptr   ' Current RGB color values of visible device palette
    Color_Association As uByte Ptr      ' Palette color index associations for CGA/EGA emulation
    Dirty             As Byte Ptr       ' Dirty lines buffer
    Driver            As GFXDRIVER Ptr  ' Gfx driver in use
    Width             As Integer        ' Current mode width
    Height            As Integer        ' Current mode height
    Depth             As Integer        ' Current mode depth
    Color_mask        As Integer        ' Color bit mask for colordepth emulation
    Default_Palette_  As PALETTE_ Ptr   ' Default palette for current mode
    Scanline_Size     As Integer        ' Vertical size of a single scanline in pixels
    Fg_Color          As uInteger       ' Current foreground color
    Bg_Color          As uInteger       ' Current background color
    Last_x            As Single         ' Last pen 'x' position
    Last_y            As Single         ' Last pen 'y' position
    Cursor_x          As Integer        ' Current graphical text cursor 'x' position (in chars, 0 based)
    Cursor_y          As Integer        ' Current graphical text cursor 'y' position (in chars, 0 based)
    Font              As FONTTYPE Ptr   ' Current font
    View_x            As Integer        ' Current VIEW 'x' coordinate
    View_y            As Integer        ' Current VIEW 'y' coordinate
    View_w            As Integer        ' Current VIEW width
    View_h            As Integer        ' Current VIEW height
    Win_x             As Single         ' Current WINDOW 'x' coordinate
    Win_y             As Single         ' Current WINDOW 'y' coordinate
    Win_w             As Single         ' Current WINDOW width
    Win_h             As Single         ' Current WINDOW height
    Text_w            As Integer        ' Graphical text console width in characters
    Text_h            As Integer        ' Graphical text console height in characters
    Key               As Byte Ptr       ' Keyboard states
    Refresh_Rate      As Integer        ' Driver refresh rate
    Flags             As Integer        ' Status flags
End Type

Extern FB_Mode Alias "fb_mode" As MODE Ptr
Extern FB_Win32 Alias "fb_win32" As WIN32DRIVER 