function fish_right_prompt
    # If there are background jobs, prints that out
    set_color green
    printf '%s [%s]' (prompt_pwd) (date "+%H:%M")
    set_color normal
end
