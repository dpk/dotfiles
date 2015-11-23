if test (uname) = "Darwin"
    set -gx PATH $PATH /Users/dpk/Library/Scripts/UNIX
    setenv EDITOR "bbedit -w"
end

if test -d /usr/local/opt/go/libexec/bin
    set -gx PATH $PATH /usr/local/opt/go/libexec/bin
    set -gx PATH $PATH /Users/dpk/.go/bin
    setenv GOPATH $HOME/.go
end
