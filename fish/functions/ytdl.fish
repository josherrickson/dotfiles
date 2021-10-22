# Defined in - @ line 1
function ytdl --wraps='youtube-dl -f bestvideo+bestaudio --write-sub --write-auto-sub --sub-format srt/best --sub-lang en --embed-subs' --description 'alias ytdl=youtube-dl -f bestvideo+bestaudio --write-sub --write-auto-sub --sub-format srt/best --sub-lang en --embed-subs'
 command yt-dlp -f bestvideo+bestaudio --write-sub --write-auto-sub --sub-format srt/best --sub-lang en --embed-subs $argv;
end
