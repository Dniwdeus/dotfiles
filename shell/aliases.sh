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

# connect to Tsetinis VPN
alias zvpn='sudo openvpn --config /run/media/dniwdeus/20b914b0-837a-46d5-ab7f-4f01aff92f03/assets/openVPN/openvpn_cavgulas.ovpn'
