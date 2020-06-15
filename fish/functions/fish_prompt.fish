function fish_prompt
    # If there are background jobs, prints that out
    jobs -q; and set_color red; and echo -n "("(jobs | wc -l | sed 's/\ //g')")"; and set_color normal
    set_color green
    echo "><> "
end
