#!/bin/sh
APP=$0
shift 1
osascript <<EOF
tell application "Terminal"
    activate
    tell window 1
        do script "$@"
        repeat while busy
            delay 1
        end repeat
        close
    end tell
end tell
tell application "OHRRPGCE-Custom"
    activate
end tell
EOF
