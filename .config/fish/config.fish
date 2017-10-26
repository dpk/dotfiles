if test (uname) = "Darwin"
    setenv EDITOR "bbedit -w"
end

if test -d /usr/local/opt/go/libexec/bin
    set -gx PATH $PATH /usr/local/opt/go/libexec/bin
    set -gx PATH $PATH $HOME/.go/bin
    setenv GOPATH $HOME/.go
end

set -gx PATH $PATH $HOME/bin

# i have no idea if i still need this
status --is-interactive; and . (rbenv init -|psub)

alias ec emacsclient
alias ecn "emacsclient -n"

