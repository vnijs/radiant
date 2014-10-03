#rsync -avruz --delete --exclude-from 'exclude-from-sync2dropbox.txt' ~/Desktop/radiant_dev/ ~/Dropbox/radiant/
rsync -avruz --exclude-from 'radiant-sync/exclude-from-sync2dropbox.txt' ~/Desktop/radiant_dev/ ~/Dropbox/radiant/

# rsync -avruz --delete ~/Desktop/radiant_dev/launchers/ ~/Dropbox/radiant/launchers/
#rsync -avruz --delete ~/Desktop/radiant_dev/inst/ ~/Dropbox/radiant/inst/
#rsync -avruz --delete ~/Desktop/radiant_dev/radiant-miniCRAN/ ~/Dropbox/radiant/radiant-miniCRAN/

