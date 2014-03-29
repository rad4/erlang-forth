
tmux new-window -t erldev:1 -n 'Erlang' 'tail -f erlpipe'
tmux new-window -t erldev:2 -n  'Serial' 'echo picocom -b 57600 /dev/ttyUSB0'
tmux select-window -t erldev:1 
tmux split-window -v  'echo 3'
tmux select-pane -t 1

