#rsync -avruz --delete --exclude-from 'exclude-from-sync2dropbox.txt' ~/Desktop/radiant_dev/ ~/Dropbox/radiant/
rsync -avruz --exclude-from 'radiant-sync/exclude-from-sync2dropbox.txt' ~/Desktop/radiant_dev/ ~/Dropbox/radiant/

