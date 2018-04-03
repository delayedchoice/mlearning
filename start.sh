export TERM="xterm-256color"
tmux new-session -d -s mlearning
tmux split-window -t mlearning:1 -v
tmux rename-window main
tmux send-keys -t mlearning:1.1 "nvim src/mlearning/core.clj" "Enter"
tmux send-keys -t mlearning:1.2 "lein repl" "Enter"
tmux new-window -t mlearning:2
tmux select-window -t mlearning:2
tmux rename-window shell
tmux select-window -t mlearning:1
tmux attach -t mlearning
