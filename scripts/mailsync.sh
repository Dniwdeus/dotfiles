MBSYNC=$(pgrep mbsync)
NOTMUCH=$(pgrep notmuch)
STATE=`nmcli networking connectivity`

if [ $STATE = 'full' ]
then
  if [ -n "$MBSYNC" -o -n "$NOTMUCH" ]; then
    echo "Already running one instance of mail-sync. Exiting..."
    exit 0
  fi

  MDIR="$(ls $MAILDIR/)"
  /usr/bin/mbsync -Va
  /usr/bin/notmuch new

  for mdir in $MDIR; do
    echo "Processing $mdir"
    for fdir in $(ls -d ${MAILDIR}$mdir/*); do
      if [ $(basename $fdir) != "INBOX" ] && [ $(basename $fdir) != "new" ]; then
        echo "Tagging for $(basename $fdir)"
        notmuch tag +$(basename $fdir) -inbox -- folder:$mdir/$(basename $fdir)
      fi
    done
  done
  exit 0
fi
echo "No Internets!"
exit 0
