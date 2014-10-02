# check for folder differences without making any changes
rsync -avruz --dry-run --delete --exclude-from 'radiant-sync/exclude-from-sync2dropbox.txt' ~/Desktop/radiant_dev/ ~/Dropbox/"radiant (1)"/

