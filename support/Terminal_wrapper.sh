#!/bin/sh
#APP=$0
#shift 1
osascript <<EOF
tell application "Terminal"
    activate
    --There are many different ways to quote, which probably aren't equivalent:
    --set mytab to do script quoted form of "$@"
    set mytab to do script "'$@'"
    --set mytab to do script "\"$@\""
    delay 0.5
    repeat while mytab is busy
        delay 1
    end repeat
    --display dialog "fin"
    close window 1
    --close (first window whose selected tab is mytab) saving no
end tell
EOF
#tell application "OHRRPGCE-Custom"
#    activate
#end tell
