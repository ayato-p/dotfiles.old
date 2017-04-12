# envfile="$HOME/.gnupg/gpg-agent.env"
# if [[ -e "$envfile" ]] && kill -0 $(grep GPG_AGENT_INFO "$envfile" | cut -d: -f 2) 2>/dev/null; then
#     eval "$(cat "$envfile")"
# else
#     eval "$(gpg-agent --daemon --enable-ssh-support --write-env-file "$envfile")"
# fi

# export GPG_AGENT_INFO  # the env file does not contain the export statement
# export SSH_AUTH_SOCK   # enable gpg-agent for ssh

# keychain --nogui --quiet ~/.ssh/id_rsa
# source ~/.keychain/$HOST-sh

# keychain -q --agents gpg
# [ -z "$HOSTNAME" ] && HOSTNAME=`uname -n`
# [ -f $HOME/.keychain/$HOSTNAME-sh-gpg ] && \
#     . $HOME/.keychain/$HOSTNAME-sh-gpg



# Start vmware-user
start-vmware-user
