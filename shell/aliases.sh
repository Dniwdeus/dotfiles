# default aliases provided by fasd. kept here for reference, override by redefining (shell-default-fasd-aliases)
alias a='fasd -a'        # any
alias s='fasd -si'       # show / search / select
alias d='fasd -d'        # directory
alias f='fasd -f'        # file
alias sd='fasd -sid'     # interactive directory selection
alias sf='fasd -sif'     # interactive file selection
alias z='fasd_cd -d'     # cd, same functionality as j in autojump
# alias zz='fasd_cd -d -i' # cd with interactive selection (shell-alias-fasd-interactive-cd)
alias j='fasd_cd -d -i' # cd with interactive selection
alias myp='dig +short txt ch whoami.cloudflare @1.0.0.1' # show me my IP using cloudflare
alias pcr='pre-commit run -a' # run all installed pre-commit hooks

#######
# Git #
#######

alias gll='git log --pretty="- %s"'
# alias glb='git log --graph --abbrev-commit --decorate --format=format:'%C(bold blue)%h%C(reset) - %C(bold cyan)%aD%C(reset) %C(bold green)(%ar)%C(reset)%C(bold yellow)%d%C(reset)%n''%C(white)%s%C(reset) %C(dim white)- %an%C(reset)' --all'
# alias glp='git log --graph --abbrev-commit --decorate --format=format:'%C(bold blue)%h%C(reset) - %C(bold green)(%ar)%C(reset) %C(white)%s%C(reset) %C(dim white)- %an%C(reset)%C(bold yellow)%d%C(reset)' --all'
# alias gla='git log --graph --pretty='%Cred%h%Creset -%C(auto)%d%Creset %s %Cgreen(%ar) %C(bold blue)<%an>%Creset' --all'
alias glba='git for-each-ref --format=" %(authorname) [%(committerdate:relative)] %09 %(refname)" --sort=committerdate --sort=authorname | sed "/refs\/tags\//d"' #sorted list of branches per author and date

#prune/delete merged, deleted branches locally
alias gpm='git fetch -p && git branch -vv | awk "/: gone]/{print $1}" | xargs git branch -d'

#prune/delete deleted branches locally - even umerged
alias gpum='git fetch -p && git branch -vv | awk "/: gone]/{print $1}" | xargs git branch -D'

#status for whoami whereami
alias gst='git config --list'

###########
# kubectl #
###########

# standard cmds

alias kgp='kubectl get pod'

alias kga='kubectl get pod --all-namespaces'

alias kg='kubectl get pod | grep'

alias kgg='kubectl get pod --all-namespaces | grep'

alias kdp='kubectl delete pod'

alias kn='kubectl get namespaces'

alias kl='kubectl logs -f'

alias kgd='kubectl get deployment'

alias krd='kubectl rollout restart deployment'

#show requests and limits for CPU and memory in current cluster context

## some former attempt (without using kubectl plugins!)
# alias nodestats='kubectl get nodes | grep ip- | awk '\''{print $1}'\'' | xargs -I {} sh -c '\''echo {} ; kubectl describe node {} | grep Allocated -A 5 | grep -ve Event -ve Allocated -ve percent -ve -- ; echo '\'''

## using the resource-capacity plugin via krew:

alias nsmu='kubectl resource-capacity --sort mem.util --util'

alias nsmup='kubectl resource-capacity --sort mem.util --util -p'

alias nscu='kubectl resource-capacity --sort cpu.util --util'

alias nscup='kubectl resource-capacity --sort cpu.util --util -p'

# kill evicted pods to free displayed output from unnecessary noise:
alias kille='kubectl get pod | grep Evicted | awk '\''{print $1}'\'' | xargs kubectl delete pod'

# kill pods which are kept in "Error" state to silence slack noise:
alias killa='kubectl get pod | grep Error | awk '\''{print $1}'\'' | xargs kubectl delete pod'

# kill all pods marked as "Completed"
alias killc='kubectl get pod | grep Completed | awk '\''{print $1}'\'' | xargs kubectl delete pod'

# have a quick way to orientate to which cluster / namespace I'm connected to
alias wai='kubectx -c && kubens -c'

#############
# terraform #
#############

# terraform plan
alias tp='terraform plan'

# terraform apply
alias ta='terraform apply'

# show current workspace
alias tw='terraform workspace show'

# select workspace
alias tws='terraform workspace select'

# list available workspaces
alias twl='terraform workspace list'

# create new workspace
alias twc='terraform workspace new'

# empties the entire state of selected workspace
alias tfk='terraform state list | xargs terraform state rm'
